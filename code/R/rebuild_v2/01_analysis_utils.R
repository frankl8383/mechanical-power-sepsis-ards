# ARDS mechanical-power rebuild v2: outcome-source-agnostic utilities
#
# This module defines the locked ventilator representations, same-patient model
# designs, nested constraint tests, external performance metrics, and hospital
# resampling primitives. It opens no database or project artifact.

v2_require_columns <- function(x, columns, label = "data") {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(label, " lacks required column(s): ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

v2_assert_binary_outcome <- function(y) {
  if (!is.numeric(y) && !is.integer(y) && !is.logical(y)) {
    stop("Outcome must be numeric, integer, or logical.")
  }
  y <- as.integer(y)
  if (anyNA(y) || !all(y %in% c(0L, 1L))) {
    stop("Outcome must be complete and coded exactly 0/1.")
  }
  if (length(unique(y)) != 2L) {
    stop("Both outcome classes are required.")
  }
  invisible(TRUE)
}

v2_clip_probability <- function(p, eps = 1e-6) {
  if (!is.numeric(p) || length(eps) != 1L || !is.finite(eps) ||
      eps <= 0 || eps >= 0.5 || anyNA(p) || any(!is.finite(p)) ||
      any(p < 0 | p > 1)) {
    stop("Invalid probability vector or clipping constant.")
  }
  pmin(pmax(p, eps), 1 - eps)
}

v2_derive_ventilator_representations <- function(
    frame,
    plateau = "plateau_cmH2O",
    peak = "peak_cmH2O",
    peep = "peep_cmH2O",
    tidal_volume = "tidal_volume_mL",
    respiratory_rate = "rr_per_min",
    tolerance = 1e-10) {
  required <- c(plateau, peak, peep, tidal_volume, respiratory_rate)
  v2_require_columns(frame, required, "ventilator frame")
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance <= 0) {
    stop("Algebraic-identity tolerance must be one positive finite number.")
  }
  if (any(!vapply(frame[required], is.numeric, logical(1L)))) {
    stop("All ventilator components must be numeric.")
  }

  out <- as.data.frame(frame, stringsAsFactors = FALSE)
  pplat <- out[[plateau]]
  ppeak <- out[[peak]]
  peep_value <- out[[peep]]
  vt_mL <- out[[tidal_volume]]
  rr <- out[[respiratory_rate]]

  complete_finite <- !is.na(pplat) & is.finite(pplat) &
    !is.na(ppeak) & is.finite(ppeak) &
    !is.na(peep_value) & is.finite(peep_value) &
    !is.na(vt_mL) & is.finite(vt_mL) &
    !is.na(rr) & is.finite(rr)

  in_range <- complete_finite &
    pplat >= 5 & pplat <= 60 &
    ppeak >= 5 & ppeak <= 80 &
    peep_value >= 5 & peep_value <= 30 &
    vt_mL >= 100 & vt_mL <= 1500 &
    rr >= 5 & rr <= 60
  ordered <- complete_finite & ppeak >= pplat & pplat >= peep_value
  driving_pressure <- pplat - peep_value
  driving_in_range <- complete_finite &
    driving_pressure >= 0 & driving_pressure <= 40
  tuple_valid <- in_range & ordered & driving_in_range

  invalid_reason <- rep("", nrow(out))
  append_reason <- function(flag, reason) {
    target <- which(flag)
    if (!length(target)) return(invisible(NULL))
    invalid_reason[target] <<- ifelse(
      nzchar(invalid_reason[target]),
      paste(invalid_reason[target], reason, sep = ";"),
      reason
    )
    invisible(NULL)
  }
  append_reason(!complete_finite, "missing_or_nonfinite_component")
  append_reason(complete_finite & !in_range, "component_out_of_range")
  append_reason(complete_finite & !ordered, "pressure_ordering_failure")
  append_reason(
    complete_finite & !driving_in_range,
    "driving_pressure_out_of_range"
  )

  vt_L <- vt_mL / 1000
  resistive_pressure <- ppeak - pplat
  static_power <- 0.098 * rr * vt_L * peep_value
  dynamic_power <- 0.098 * rr * vt_L * 0.5 * driving_pressure
  resistive_power <- 0.098 * rr * vt_L * resistive_pressure
  smp <- 0.098 * rr * vt_L *
    (ppeak - 0.5 * (pplat - peep_value))
  four_dprr <- 4 * driving_pressure + rr
  smp_in_range <- is.finite(smp) & smp >= 0 & smp <= 100
  append_reason(tuple_valid & !smp_in_range, "smp_out_of_range")
  tuple_valid <- tuple_valid & smp_in_range
  invalid_reason[tuple_valid] <- ""
  identity_error <- smp - (static_power + dynamic_power + resistive_power)
  identity_pass <- tuple_valid & is.finite(identity_error) &
    abs(identity_error) <= tolerance

  derived <- list(
    vt_L = vt_L,
    driving_pressure = driving_pressure,
    resistive_pressure = resistive_pressure,
    smp = smp,
    four_dprr = four_dprr,
    static_power = static_power,
    dynamic_power = dynamic_power,
    resistive_power = resistive_power,
    energy_identity_error = identity_error,
    energy_identity_pass = identity_pass,
    compliance_L_per_cmH2O = ifelse(
      tuple_valid & driving_pressure > 0,
      vt_L / driving_pressure,
      NA_real_
    ),
    smp_per_compliance = ifelse(
      tuple_valid & driving_pressure > 0,
      smp / (vt_L / driving_pressure),
      NA_real_
    )
  )
  for (name in names(derived)) {
    if (!name %in% c("energy_identity_pass")) {
      derived[[name]][!tuple_valid] <- NA
    }
    out[[name]] <- derived[[name]]
  }
  out$tuple_valid <- tuple_valid
  out$tuple_invalid_reason <- invalid_reason

  if (any(tuple_valid & !identity_pass)) {
    stop(
      "Exact algebraic decomposition of the surrogate equation failed ",
      "its numerical identity check."
    )
  }
  out
}

v2_rate_concordance <- function(set_rr, total_rr, maximum_difference = 2) {
  if (!is.numeric(set_rr) || !is.numeric(total_rr) ||
      length(set_rr) != length(total_rr) ||
      length(maximum_difference) != 1L || !is.finite(maximum_difference) ||
      maximum_difference < 0) {
    stop("Invalid respiratory-rate concordance inputs.")
  }
  paired <- !is.na(set_rr) & is.finite(set_rr) &
    !is.na(total_rr) & is.finite(total_rr)
  difference <- total_rr - set_rr
  data.frame(
    rr_pair_available = paired,
    rr_total_minus_set = ifelse(paired, difference, NA_real_),
    rr_absolute_difference = ifelse(paired, abs(difference), NA_real_),
    rate_concordant = paired & abs(difference) <= maximum_difference
  )
}

v2_validate_knots <- function(knots, expected_length, variable) {
  if (!is.numeric(knots) || length(knots) != expected_length ||
      anyNA(knots) || any(!is.finite(knots)) || any(diff(knots) <= 0)) {
    stop(
      expected_length, " strictly increasing finite knots required for ",
      variable, "."
    )
  }
  invisible(TRUE)
}

v2_quantile_knots <- function(x, probabilities, variable, type = 2L) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop("Complete finite values are required to derive knots for ", variable)
  }
  knots <- as.numeric(stats::quantile(
    x, probs = probabilities, names = FALSE, type = type
  ))
  v2_validate_knots(knots, length(probabilities), variable)
  knots
}

v2_natural_spline_basis <- function(x, knots, prefix) {
  v2_validate_knots(knots, length(knots), prefix)
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop("Complete finite values are required for spline variable ", prefix)
  }
  internal <- knots[seq.int(2L, length(knots) - 1L)]
  basis <- splines::ns(
    x,
    knots = internal,
    Boundary.knots = knots[c(1L, length(knots))],
    intercept = FALSE
  )
  basis <- as.matrix(basis)
  colnames(basis) <- paste0(prefix, "_rcs", seq_len(ncol(basis)))
  basis
}

v2_baseline_continuous_variables <- c(
  "age", "pf_ratio", "map", "platelet", "creatinine"
)

v2_derive_transform_bundle <- function(frame) {
  required <- c(
    v2_baseline_continuous_variables,
    "smp", "four_dprr", "driving_pressure", "rr"
  )
  v2_require_columns(frame, required, "MIMIC derivation frame")
  if (any(!vapply(frame[required], is.numeric, logical(1L)))) {
    stop("Transformation variables must be numeric.")
  }
  if (any(!stats::complete.cases(frame[required]))) {
    stop("Transformation derivation frame must be a complete common set.")
  }
  list(
    baseline_three_knots = setNames(lapply(
      v2_baseline_continuous_variables,
      function(variable) {
        v2_quantile_knots(
          frame[[variable]], c(0.10, 0.50, 0.90), variable, type = 2L
        )
      }
    ), v2_baseline_continuous_variables),
    nonlinear_four_knots = list(
      smp = v2_quantile_knots(
        frame$smp, c(0.05, 0.35, 0.65, 0.95), "smp", type = 2L
      ),
      four_dprr = v2_quantile_knots(
        frame$four_dprr, c(0.05, 0.35, 0.65, 0.95),
        "four_dprr", type = 2L
      ),
      driving_pressure = v2_quantile_knots(
        frame$driving_pressure, c(0.05, 0.35, 0.65, 0.95),
        "driving_pressure", type = 2L
      ),
      rr = v2_quantile_knots(
        frame$rr, c(0.05, 0.35, 0.65, 0.95), "rr", type = 2L
      )
    ),
    quantile_type = 2L,
    derivation_database = "MIMIC-IV"
  )
}

v2_named_column <- function(x, name) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop("Complete finite numeric values required for ", name)
  }
  out <- matrix(as.numeric(x), ncol = 1L)
  colnames(out) <- name
  out
}

v2_build_baseline_design <- function(frame, bundle) {
  required <- c(
    "age", "sex_female", "pf_ratio", "map", "vasopressor",
    "platelet", "creatinine"
  )
  v2_require_columns(frame, required, "model frame")
  if (!all(v2_baseline_continuous_variables %in%
           names(bundle$baseline_three_knots))) {
    stop("Malformed baseline transformation bundle.")
  }
  design <- cbind(
    v2_natural_spline_basis(
      frame$age, bundle$baseline_three_knots$age, "age"
    ),
    v2_named_column(frame$sex_female, "sex_female"),
    v2_natural_spline_basis(
      frame$pf_ratio, bundle$baseline_three_knots$pf_ratio, "pf_ratio"
    ),
    v2_natural_spline_basis(
      frame$map, bundle$baseline_three_knots$map, "map"
    ),
    v2_named_column(frame$vasopressor, "vasopressor"),
    v2_natural_spline_basis(
      frame$platelet, bundle$baseline_three_knots$platelet, "platelet"
    ),
    v2_natural_spline_basis(
      frame$creatinine, bundle$baseline_three_knots$creatinine, "creatinine"
    )
  )
  storage.mode(design) <- "double"
  design
}

v2_model_specification <- function() {
  data.frame(
    model_id = c("M0", "M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY"),
    added_terms = c(
      "", "smp", "four_dprr", "driving_pressure + rr",
      "static_power + dynamic_power + resistive_power"
    ),
    incremental_df = c(0L, 1L, 1L, 2L, 3L),
    role = c(
      "severity baseline",
      "formula-based surrogate mechanical power",
      "one-df pressure-rate composite",
      "free pressure-rate weights",
      "free static-elastic/dynamic-elastic/resistive algebraic-term weights"
    ),
    stringsAsFactors = FALSE
  )
}

v2_build_design <- function(frame, model_id, bundle) {
  valid_ids <- v2_model_specification()$model_id
  if (!model_id %in% valid_ids) stop("Unknown model ID: ", model_id)
  baseline <- v2_build_baseline_design(frame, bundle)
  add <- switch(
    model_id,
    M0 = NULL,
    M_MP = v2_named_column(frame$smp, "smp"),
    M_4DPRR = v2_named_column(frame$four_dprr, "four_dprr"),
    M_DPRR = cbind(
      v2_named_column(frame$driving_pressure, "driving_pressure"),
      v2_named_column(frame$rr, "rr")
    ),
    M_ENERGY = cbind(
      v2_named_column(frame$static_power, "static_power"),
      v2_named_column(frame$dynamic_power, "dynamic_power"),
      v2_named_column(frame$resistive_power, "resistive_power")
    )
  )
  design <- if (is.null(add)) baseline else cbind(baseline, add)
  if (anyNA(design) || any(!is.finite(design)) ||
      is.null(colnames(design)) || anyDuplicated(colnames(design))) {
    stop("Invalid design matrix for ", model_id)
  }
  design
}

v2_build_nonlinear_design <- function(frame, model_id, bundle) {
  if (!model_id %in% c("M_MP_NL", "M_4DPRR_NL", "M_DPRR_NL")) {
    stop("Unknown nonlinear sensitivity model: ", model_id)
  }
  baseline <- v2_build_baseline_design(frame, bundle)
  knots <- bundle$nonlinear_four_knots
  add <- switch(
    model_id,
    M_MP_NL = v2_natural_spline_basis(frame$smp, knots$smp, "smp"),
    M_4DPRR_NL = v2_natural_spline_basis(
      frame$four_dprr, knots$four_dprr, "four_dprr"
    ),
    M_DPRR_NL = cbind(
      v2_natural_spline_basis(
        frame$driving_pressure, knots$driving_pressure, "driving_pressure"
      ),
      v2_natural_spline_basis(frame$rr, knots$rr, "rr")
    )
  )
  design <- cbind(baseline, add)
  if (anyNA(design) || any(!is.finite(design)) ||
      anyDuplicated(colnames(design))) {
    stop("Invalid nonlinear design matrix for ", model_id)
  }
  design
}

v2_fit_logistic <- function(design, y, model_id, row_ids = seq_along(y)) {
  v2_assert_binary_outcome(y)
  design <- as.matrix(design)
  if (!is.numeric(design) || nrow(design) != length(y) ||
      anyNA(design) || any(!is.finite(design)) ||
      is.null(colnames(design)) || anyDuplicated(colnames(design))) {
    stop("Invalid design matrix for ", model_id)
  }
  if (length(row_ids) != length(y) || anyNA(row_ids) ||
      anyDuplicated(row_ids)) {
    stop("Unique complete row identifiers are required.")
  }
  x <- cbind(`(Intercept)` = 1, design)
  fit <- suppressWarnings(stats::glm.fit(
    x = x,
    y = as.integer(y),
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  if (!fit$converged || fit$rank != ncol(x) ||
      anyNA(fit$coefficients) || any(!is.finite(fit$coefficients))) {
    stop("Logistic model failed or was rank deficient: ", model_id)
  }
  probability <- v2_clip_probability(
    stats::plogis(as.numeric(x %*% fit$coefficients))
  )
  log_likelihood <- sum(
    y * log(probability) + (1 - y) * log1p(-probability)
  )
  structure(
    list(
      model_id = model_id,
      coefficients = setNames(as.numeric(fit$coefficients), colnames(x)),
      design_columns = colnames(design),
      n = length(y),
      events = sum(y),
      rank = fit$rank,
      log_likelihood = log_likelihood,
      row_ids = as.character(row_ids),
      converged = fit$converged
    ),
    class = "ards_v2_logistic"
  )
}

predict.ards_v2_logistic <- function(
    object, newdata, type = c("response", "link"), ...) {
  type <- match.arg(type)
  design <- as.matrix(newdata)
  if (is.null(colnames(design)) ||
      !identical(colnames(design), object$design_columns) ||
      anyNA(design) || any(!is.finite(design))) {
    stop("Prediction design differs from the locked model.")
  }
  linear_predictor <- as.numeric(
    cbind(`(Intercept)` = 1, design) %*% object$coefficients
  )
  if (type == "link") linear_predictor else stats::plogis(linear_predictor)
}

v2_increment_collinearity_audit <- function(
    frame,
    columns,
    audit_id = paste(columns, collapse = "_"),
    condition_number_warning = 30,
    vif_warning = 5) {
  if (!is.character(columns) || length(columns) < 2L ||
      anyNA(columns) || any(!nzchar(columns)) || anyDuplicated(columns)) {
    stop("At least two unique non-empty column names are required.")
  }
  v2_require_columns(frame, columns, "collinearity audit frame")
  if (any(!vapply(frame[columns], is.numeric, logical(1L))) ||
      any(!stats::complete.cases(frame[columns])) ||
      any(!vapply(frame[columns], function(x) all(is.finite(x)), logical(1L)))) {
    stop("Collinearity audit variables must be complete finite numerics.")
  }
  x <- as.matrix(frame[columns])
  standard_deviations <- apply(x, 2L, stats::sd)
  if (any(!is.finite(standard_deviations)) ||
      any(standard_deviations <= .Machine$double.eps^0.5)) {
    stop("Collinearity audit cannot include constant variables.")
  }
  z <- scale(x, center = TRUE, scale = TRUE)
  singular_values <- svd(z, nu = 0L, nv = 0L)$d
  if (!length(singular_values) ||
      min(singular_values) <= .Machine$double.eps^0.5) {
    condition_number <- Inf
  } else {
    condition_number <- max(singular_values) / min(singular_values)
  }
  correlation <- stats::cor(x)
  vif <- vapply(seq_along(columns), function(j) {
    response <- z[, j]
    predictors <- z[, -j, drop = FALSE]
    fit <- stats::lm.fit(
      x = cbind(`(Intercept)` = 1, predictors),
      y = response
    )
    residual_sum_squares <- sum(fit$residuals^2)
    total_sum_squares <- sum((response - mean(response))^2)
    r_squared <- 1 - residual_sum_squares / total_sum_squares
    if (!is.finite(r_squared) || r_squared >= 1) Inf else 1 / (1 - r_squared)
  }, numeric(1L))
  names(vif) <- columns
  summary <- data.frame(
    audit_id = audit_id,
    n = nrow(x),
    predictors = length(columns),
    condition_number = condition_number,
    maximum_absolute_pairwise_correlation =
      max(abs(correlation[upper.tri(correlation)])),
    maximum_vif = max(vif),
    condition_number_warning =
      !is.finite(condition_number) ||
      condition_number >= condition_number_warning,
    vif_warning = any(!is.finite(vif)) || max(vif) >= vif_warning,
    stringsAsFactors = FALSE
  )
  list(
    summary = summary,
    correlation = correlation,
    vif = data.frame(
      term = names(vif),
      vif = unname(vif),
      stringsAsFactors = FALSE
    )
  )
}

v2_constraint_lrt <- function(restricted_fit, unrestricted_fit) {
  allowed <- list(
    four_dprr_weight_constraint = c("M_4DPRR", "M_DPRR"),
    equal_algebraic_term_weight_constraint = c("M_MP", "M_ENERGY")
  )
  pair <- c(restricted_fit$model_id, unrestricted_fit$model_id)
  test_id <- names(allowed)[vapply(
    allowed, identical, logical(1L), pair
  )]
  if (length(test_id) != 1L) {
    stop("The requested pair is not a locked v2 nested constraint test.")
  }
  if (!identical(restricted_fit$row_ids, unrestricted_fit$row_ids) ||
      restricted_fit$n != unrestricted_fit$n) {
    stop("Nested models must use exactly the same rows in the same order.")
  }
  df <- length(unrestricted_fit$coefficients) -
    length(restricted_fit$coefficients)
  statistic <- 2 * (
    unrestricted_fit$log_likelihood - restricted_fit$log_likelihood
  )
  if (df <= 0L || !is.finite(statistic) || statistic < -1e-8) {
    stop("Invalid nested likelihood-ratio result.")
  }
  statistic <- max(0, statistic)
  data.frame(
    test_id = test_id,
    restricted_model = restricted_fit$model_id,
    unrestricted_model = unrestricted_fit$model_id,
    chi_square = statistic,
    df = df,
    p_value = stats::pchisq(statistic, df = df, lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
}

v2_auc_rank <- function(y, p) {
  v2_assert_binary_outcome(y)
  if (length(y) != length(p) || anyNA(p) || any(!is.finite(p))) {
    stop("AUC inputs are incomplete or non-finite.")
  }
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

v2_calibration_coefficients <- function(y, p, eps = 1e-6) {
  v2_assert_binary_outcome(y)
  p <- v2_clip_probability(p, eps)
  linear_predictor <- qlogis(p)
  intercept_fit <- suppressWarnings(stats::glm.fit(
    x = matrix(1, nrow = length(y), ncol = 1L),
    y = y,
    offset = linear_predictor,
    family = stats::binomial()
  ))
  slope_fit <- suppressWarnings(stats::glm.fit(
    x = cbind(intercept = 1, linear_predictor = linear_predictor),
    y = y,
    family = stats::binomial()
  ))
  c(
    calibration_in_the_large =
      unname(intercept_fit$coefficients[[1L]]),
    calibration_intercept =
      unname(slope_fit$coefficients[[1L]]),
    calibration_slope =
      unname(slope_fit$coefficients[[2L]])
  )
}

v2_binary_performance <- function(y, p, eps = 1e-6) {
  v2_assert_binary_outcome(y)
  if (length(y) != length(p) || anyNA(p) || any(!is.finite(p))) {
    stop("Performance inputs are incomplete or non-finite.")
  }
  p <- v2_clip_probability(p, eps)
  calibration <- v2_calibration_coefficients(y, p, eps)
  c(
    n = length(y),
    events = sum(y),
    event_rate = mean(y),
    brier = mean((y - p)^2),
    log_loss = -mean(y * log(p) + (1 - y) * log1p(-p)),
    c_statistic = v2_auc_rank(y, p),
    calibration,
    observed_expected_ratio = sum(y) / sum(p)
  )
}

v2_paired_metric_difference <- function(
    y, p_new, p_reference, eps = 1e-6) {
  if (length(p_new) != length(p_reference)) {
    stop("Paired predictions must have equal length.")
  }
  new <- v2_binary_performance(y, p_new, eps)
  reference <- v2_binary_performance(y, p_reference, eps)
  metrics <- c("brier", "log_loss", "c_statistic")
  out <- new[metrics] - reference[metrics]
  names(out) <- paste0("delta_", metrics)
  out
}

v2_cluster_bootstrap_indices <- function(cluster) {
  if (length(cluster) < 2L || anyNA(cluster)) {
    stop("Complete cluster labels are required.")
  }
  cluster <- as.character(cluster)
  unique_clusters <- unique(cluster)
  if (length(unique_clusters) < 2L) {
    stop("At least two clusters are required.")
  }
  sampled <- sample(
    unique_clusters, length(unique_clusters), replace = TRUE
  )
  unlist(lapply(sampled, function(value) which(cluster == value)),
         use.names = FALSE)
}

v2_equal_center_performance <- function(
    y, p, center, minimum_center_n = 10L, eps = 1e-6) {
  if (length(y) != length(p) || length(y) != length(center) ||
      anyNA(center) || minimum_center_n < 1L) {
    stop("Invalid equal-center performance inputs.")
  }
  v2_assert_binary_outcome(y)
  p <- v2_clip_probability(p, eps)
  center <- as.character(center)
  counts <- table(center)
  eligible <- names(counts)[counts >= minimum_center_n]
  if (length(eligible) < 2L) {
    stop("At least two eligible centers are required.")
  }
  rows <- lapply(eligible, function(center_id) {
    i <- which(center == center_id)
    data.frame(
      center = center_id,
      n = length(i),
      events = sum(y[i]),
      observed_rate = mean(y[i]),
      expected_rate = mean(p[i]),
      brier = mean((y[i] - p[i])^2),
      log_loss = -mean(
        y[i] * log(p[i]) + (1 - y[i]) * log1p(-p[i])
      ),
      stringsAsFactors = FALSE
    )
  })
  detail <- do.call(rbind, rows)
  summary <- c(
    eligible_centers = nrow(detail),
    patients_in_eligible_centers = sum(detail$n),
    events_in_eligible_centers = sum(detail$events),
    equal_center_brier = mean(detail$brier),
    equal_center_log_loss = mean(detail$log_loss),
    equal_center_observed_rate = mean(detail$observed_rate),
    equal_center_expected_rate = mean(detail$expected_rate),
    equal_center_observed_expected_ratio =
      mean(detail$observed_rate) / mean(detail$expected_rate)
  )
  list(summary = summary, center_detail = detail)
}

v2_percentile_interval <- function(
    bootstrap_values, probabilities = c(0.025, 0.975)) {
  if (!is.numeric(bootstrap_values) || anyNA(bootstrap_values) ||
      any(!is.finite(bootstrap_values)) || length(bootstrap_values) < 20L) {
    stop("At least 20 complete finite bootstrap values are required.")
  }
  if (!is.numeric(probabilities) || length(probabilities) != 2L ||
      anyNA(probabilities) || probabilities[1L] <= 0 ||
      probabilities[2L] >= 1 ||
      probabilities[1L] >= probabilities[2L]) {
    stop("Invalid interval probabilities.")
  }
  stats::quantile(
    bootstrap_values, probs = probabilities,
    names = FALSE, type = 6L
  )
}
