# ARDS mechanical-power rebuild v1: reusable locked-analysis utilities
#
# Source 08_model_utils.R before this file. This module is outcome-source
# agnostic: it contains the prespecified model manifest, deterministic design
# construction, generic binary-model inference, recalibration, and resampling
# helpers. It opens no project artifact and performs no analysis on source data.

required_base_locked_utils <- c(
  "binary_performance", "fit_locked_logistic", "three_knot_rcs_basis",
  "four_knot_rcs_basis", "quantile_knots", "clip_probability",
  "validate_three_knots", "validate_knots", "assert_binary_outcome"
)
if (!all(vapply(required_base_locked_utils, function(name) {
  exists(name, mode = "function", inherits = TRUE)
}, logical(1L)))) {
  stop("Source 08_model_utils.R before 08a_locked_analysis_utils.R.")
}

MIMIC_BOOTSTRAP_REPS <- 1000L
EICU_CLUSTER_BOOTSTRAP_REPS <- 2000L
MIMIC_BOOTSTRAP_SEED <- 20260715L
EICU_BOOTSTRAP_SEED <- 20260716L
BOOTSTRAP_SUCCESS_THRESHOLD <- 0.95
PROBABILITY_CLIP_EPS <- 1e-6
BOOTSTRAP_CI_PROBS <- c(0.025, 0.975)
FLEXIBLE_CALIBRATION_KNOT_PROBS <- c(0.05, 0.35, 0.65, 0.95)
FLEXIBLE_CALIBRATION_GRID_N <- 101L

metric_names <- c(
  "brier", "scaled_brier", "log_loss",
  "calibration_in_the_large", "calibration_slope", "c_statistic"
)
continuous_s0_variables <- c(
  "age", "pf_ratio", "gcs", "map", "platelet", "creatinine"
)
no_gcs_complete_variables <- c(
  "age", "sex_female", "pf_ratio", "map", "vasopressor",
  "platelet", "creatinine", "delta_p", "rr", "smp"
)

locked_model_specification <- function() {
  data.table::data.table(
    model_id = c(
      "S0", "S1", "S2", "S3", "S2M",
      "S3NL", "S3c", "S4", "S5", "N3_abs", "N3_pbw", "R2", "R3"
    ),
    analysis_set = c(
      rep("primary_common", 6L), rep("component_common", 3L),
      rep("normalized_common", 2L), rep("no_gcs_common", 2L)
    ),
    include_gcs = c(rep(TRUE, 11L), FALSE, FALSE),
    design_type = c(
      "s0", "s0_delta_p", "s0_delta_p_rr", "s0_smp_per_5",
      "s0_delta_p_rr_smp_per_5", "s0_smp_rcs4", "s0_smp_per_5",
      "s0_full_components", "s0_full_components_smp_per_5",
      "s0_smp_z", "s0_smp_per_pbw_z", "s0_delta_p_rr", "s0_smp_per_5"
    ),
    role = c(
      "baseline", "driving_pressure", "simple_components", "primary_smp",
      "nested_smp_extension", "secondary_four_knot_smp", "component_set_smp",
      "full_components", "mathematical_stress_test",
      "normalized_common_absolute_smp", "normalized_smp_per_pbw",
      "D057_no_GCS_simple_components", "D057_no_GCS_smp"
    ),
    allow_nonestimable = c(rep(FALSE, 8L), TRUE, rep(FALSE, 4L)),
    reporting_order = seq_len(13L)
  )
}

locked_comparison_specification <- function() {
  data.table::data.table(
    comparison_id = c(
      "S1_minus_S0", "S2_minus_S1", "S3_minus_S2", "S2M_minus_S2",
      "S3NL_minus_S3", "S4_minus_S3c", "S5_minus_S4",
      "N3_pbw_minus_N3_abs", "R3_minus_R2"
    ),
    analysis_set = c(
      rep("primary_common", 5L), rep("component_common", 2L),
      "normalized_common", "no_gcs_common"
    ),
    new_model = c(
      "S1", "S2", "S3", "S2M", "S3NL", "S4", "S5", "N3_pbw", "R3"
    ),
    reference_model = c(
      "S0", "S1", "S2", "S2", "S3", "S3c", "S4", "N3_abs", "R2"
    ),
    primary_comparison = c(FALSE, FALSE, TRUE, rep(FALSE, 6L)),
    likelihood_ratio_allowed = c(rep(FALSE, 3L), TRUE, rep(FALSE, 5L))
  )
}

complete_finite <- function(x, columns) {
  if (!all(columns %in% names(x))) {
    stop("Missing complete-case field(s): ", paste(
      setdiff(columns, names(x)), collapse = ", "
    ))
  }
  Reduce(`&`, lapply(columns, function(column) {
    value <- x[[column]]
    !is.na(value) & is.finite(value)
  }))
}

validate_transform_bundle <- function(bundle) {
  if (!is.list(bundle) ||
      !setequal(names(bundle$three_knots), continuous_s0_variables) ||
      any(vapply(bundle$three_knots, length, integer(1L)) != 3L) ||
      length(bundle$smp_knots) != 4L ||
      length(bundle$smp_center_scale) != 2L ||
      length(bundle$smp_per_pbw_center_scale) != 2L) {
    stop("Malformed locked transformation bundle.")
  }
  for (variable in continuous_s0_variables) {
    validate_three_knots(bundle$three_knots[[variable]], variable)
  }
  validate_knots(bundle$smp_knots, "smp", minimum_n = 4L)
  for (scale in list(bundle$smp_center_scale, bundle$smp_per_pbw_center_scale)) {
    if (anyNA(scale) || any(!is.finite(scale)) || scale[["sd"]] <= 0) {
      stop("Invalid frozen exposure center/scale.")
    }
  }
  invisible(TRUE)
}

parameter_to_transform_bundle <- function(parameters) {
  bundle <- list(
    three_knots = parameters$three_knot_values,
    smp_knots = parameters$smp_knot_values,
    smp_center_scale = parameters$smp_center_scale,
    smp_per_pbw_center_scale = parameters$smp_per_pbw_center_scale,
    quantile_type = parameters$quantile_type
  )
  validate_transform_bundle(bundle)
  bundle
}

derive_bootstrap_transform_bundle <- function(frame) {
  derivation <- frame[frame$primary_predictor_complete %in% TRUE, , drop = FALSE]
  normalized <- frame[
    frame$component_predictor_complete %in% TRUE &
      frame$normalized_exposure_complete %in% TRUE,
    , drop = FALSE
  ]
  if (nrow(derivation) < 20L || nrow(normalized) < 20L) {
    stop("Bootstrap parameter-derivation population is too small.")
  }
  three <- setNames(lapply(continuous_s0_variables, function(variable) {
    quantile_knots(
      derivation[[variable]], c(0.10, 0.50, 0.90),
      variable = variable, type = 2L
    )
  }), continuous_s0_variables)
  bundle <- list(
    three_knots = three,
    smp_knots = quantile_knots(
      derivation$smp, c(0.05, 0.35, 0.65, 0.95),
      variable = "smp", type = 2L
    ),
    smp_center_scale = c(
      mean = mean(derivation$smp), sd = stats::sd(derivation$smp)
    ),
    smp_per_pbw_center_scale = c(
      mean = mean(normalized$smp_per_pbw),
      sd = stats::sd(normalized$smp_per_pbw)
    ),
    quantile_type = 2L
  )
  validate_transform_bundle(bundle)
  bundle
}

named_column <- function(x, name) {
  out <- matrix(as.numeric(x), ncol = 1L)
  colnames(out) <- name
  out
}

build_s0_design <- function(frame, bundle, include_gcs = TRUE) {
  validate_transform_bundle(bundle)
  pieces <- list(
    three_knot_rcs_basis(frame$age, bundle$three_knots$age, "age"),
    named_column(frame$sex_female, "sex_female"),
    three_knot_rcs_basis(
      frame$pf_ratio, bundle$three_knots$pf_ratio, "pf_ratio"
    )
  )
  if (include_gcs) {
    pieces <- c(pieces, list(three_knot_rcs_basis(
      frame$gcs, bundle$three_knots$gcs, "gcs"
    )))
  }
  pieces <- c(pieces, list(
    three_knot_rcs_basis(frame$map, bundle$three_knots$map, "map"),
    named_column(frame$vasopressor, "vasopressor"),
    three_knot_rcs_basis(
      frame$platelet, bundle$three_knots$platelet, "platelet"
    ),
    three_knot_rcs_basis(
      frame$creatinine, bundle$three_knots$creatinine, "creatinine"
    )
  ))
  design <- do.call(cbind, pieces)
  storage.mode(design) <- "double"
  design
}

build_design_matrix <- function(frame, model_id, bundle) {
  specs <- locked_model_specification()
  target_model_id <- model_id
  spec <- specs[specs[["model_id"]] == target_model_id]
  if (nrow(spec) != 1L) stop("Unknown or duplicate model ID: ", model_id)
  design <- build_s0_design(frame, bundle, include_gcs = spec$include_gcs[[1L]])
  add <- switch(
    spec$design_type[[1L]],
    s0 = NULL,
    s0_delta_p = named_column(frame$delta_p, "delta_p"),
    s0_delta_p_rr = cbind(
      named_column(frame$delta_p, "delta_p"), named_column(frame$rr, "rr")
    ),
    s0_smp_per_5 = named_column(frame$smp / 5, "smp_per_5"),
    s0_smp_rcs4 = four_knot_rcs_basis(frame$smp, bundle$smp_knots, "smp"),
    s0_delta_p_rr_smp_per_5 = cbind(
      named_column(frame$delta_p, "delta_p"),
      named_column(frame$rr, "rr"),
      named_column(frame$smp / 5, "smp_per_5")
    ),
    s0_full_components = cbind(
      named_column(frame$delta_p, "delta_p"),
      named_column(frame$rr, "rr"),
      named_column(frame$vt_per_pbw, "vt_per_pbw"),
      named_column(frame$peep, "peep"),
      named_column(frame$resistive_pressure, "resistive_pressure")
    ),
    s0_full_components_smp_per_5 = cbind(
      named_column(frame$delta_p, "delta_p"),
      named_column(frame$rr, "rr"),
      named_column(frame$vt_per_pbw, "vt_per_pbw"),
      named_column(frame$peep, "peep"),
      named_column(frame$resistive_pressure, "resistive_pressure"),
      named_column(frame$smp / 5, "smp_per_5")
    ),
    s0_smp_z = named_column(
      (frame$smp - bundle$smp_center_scale[["mean"]]) /
        bundle$smp_center_scale[["sd"]], "smp_z"
    ),
    s0_smp_per_pbw_z = named_column(
      (frame$smp_per_pbw - bundle$smp_per_pbw_center_scale[["mean"]]) /
        bundle$smp_per_pbw_center_scale[["sd"]], "smp_per_pbw_z"
    ),
    stop("Unknown design type: ", spec$design_type[[1L]])
  )
  if (!is.null(add)) design <- cbind(design, add)
  if (anyNA(design) || any(!is.finite(design)) || anyDuplicated(colnames(design))) {
    stop("Invalid design matrix for ", model_id, ".")
  }
  design
}

fit_model <- function(design, y, model_id, allow_nonestimable = FALSE) {
  failure <- function(reason) {
    if (!allow_nonestimable) stop(reason)
    structure(list(
      model_id = model_id, status = "NON_ESTIMABLE", reason = reason,
      coefficients = setNames(numeric(), character()),
      vcov = matrix(numeric(), 0L, 0L), design_columns = colnames(design),
      vcov_model_based = matrix(numeric(), 0L, 0L),
      vcov_type = "HC0_sandwich",
      n = length(y), events = sum(y), rank = NA_integer_,
      condition_number = NA_real_, loglik = NA_real_
    ), class = "ards_locked_model_inference")
  }
  tryCatch({
    base_fit <- fit_locked_logistic(design, y, model_name = model_id)
    x <- cbind(`(Intercept)` = 1, as.matrix(design))
    probability <- predict(base_fit, design, type = "response")
    weight <- probability * (1 - probability)
    information <- crossprod(x * sqrt(weight))
    covariance <- solve(information)
    dimnames(covariance) <- list(colnames(x), colnames(x))
    condition_number <- kappa(information, exact = TRUE)
    if (anyNA(covariance) || any(!is.finite(covariance)) ||
        !is.finite(condition_number)) {
      return(failure(paste0("Non-finite covariance for ", model_id)))
    }
    structure(list(
      model_id = model_id, status = "ESTIMABLE", reason = "",
      coefficients = base_fit$coefficients, vcov = covariance,
      design_columns = base_fit$design_columns,
      n = base_fit$n, events = base_fit$events, rank = base_fit$rank,
      condition_number = condition_number, loglik = base_fit$loglik
    ), class = "ards_locked_model_inference")
  }, error = function(e) failure(paste0(model_id, ": ", conditionMessage(e))))
}

normalize_analysis_weights <- function(weights, n) {
  if (!is.numeric(weights) || length(weights) != n || anyNA(weights) ||
      any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Analysis weights must be positive, finite, complete, and row-aligned.")
  }
  normalized <- as.numeric(weights) / mean(weights)
  if (any(!is.finite(normalized)) || any(normalized <= 0) ||
      abs(mean(normalized) - 1) > 1e-12) {
    stop("Internal weight normalization failed.")
  }
  normalized
}

fit_weighted_model <- function(
    design, y, weights, model_id, allow_nonestimable = FALSE) {
  failure <- function(reason) {
    if (!allow_nonestimable) stop(reason)
    structure(list(
      model_id = model_id, status = "NON_ESTIMABLE", reason = reason,
      coefficients = setNames(numeric(), character()),
      vcov = matrix(numeric(), 0L, 0L), design_columns = colnames(design),
      vcov_model_based = matrix(numeric(), 0L, 0L),
      vcov_type = "HC0_sandwich",
      n = length(y), events = sum(y), rank = NA_integer_,
      condition_number = NA_real_, loglik = NA_real_,
      weights_normalized_to_mean_one = TRUE
    ), class = "ards_locked_model_inference")
  }
  tryCatch({
    assert_binary_outcome(y)
    design <- as.matrix(design)
    if (!is.numeric(design) || nrow(design) != length(y) || anyNA(design) ||
        any(!is.finite(design)) || is.null(colnames(design)) ||
        anyDuplicated(colnames(design))) {
      stop("Invalid weighted design matrix for ", model_id)
    }
    normalized_weights <- normalize_analysis_weights(weights, length(y))
    x <- cbind(`(Intercept)` = 1, design)
    fitted <- suppressWarnings(stats::glm.fit(
      x = x, y = y, weights = normalized_weights,
      family = stats::binomial(), control = stats::glm.control(maxit = 100L)
    ))
    if (!fitted$converged || fitted$rank != ncol(x) ||
        anyNA(fitted$coefficients) || any(!is.finite(fitted$coefficients))) {
      stop("Weighted logistic model failed or is rank deficient: ", model_id)
    }
    probability <- stats::plogis(as.numeric(x %*% fitted$coefficients))
    information <- crossprod(
      x * sqrt(normalized_weights * probability * (1 - probability))
    )
    bread <- solve(information)
    score_matrix <- x * (normalized_weights * (y - probability))
    meat <- crossprod(score_matrix)
    covariance_robust <- bread %*% meat %*% bread
    dimnames(bread) <- list(colnames(x), colnames(x))
    dimnames(covariance_robust) <- list(colnames(x), colnames(x))
    condition_number <- kappa(information, exact = TRUE)
    if (anyNA(bread) || any(!is.finite(bread)) ||
        anyNA(covariance_robust) || any(!is.finite(covariance_robust)) ||
        !is.finite(condition_number)) {
      stop("Weighted covariance is non-finite: ", model_id)
    }
    # Weighted Bernoulli log likelihood under the internally normalized weights.
    pp <- clip_probability(probability, PROBABILITY_CLIP_EPS)
    loglik <- sum(normalized_weights * (
      y * log(pp) + (1 - y) * log1p(-pp)
    ))
    structure(list(
      model_id = model_id, status = "ESTIMABLE", reason = "",
      coefficients = setNames(as.numeric(fitted$coefficients), colnames(x)),
      vcov = covariance_robust, vcov_model_based = bread,
      vcov_type = "HC0_sandwich", design_columns = colnames(design),
      n = length(y), events = sum(y), rank = fitted$rank,
      condition_number = condition_number, loglik = loglik,
      weights_normalized_to_mean_one = TRUE
    ), class = "ards_locked_model_inference")
  }, error = function(e) failure(paste0(model_id, ": ", conditionMessage(e))))
}

predict_model <- function(fit, design) {
  if (!identical(fit$status, "ESTIMABLE")) {
    stop("Prediction requested from non-estimable model: ", fit$model_id)
  }
  design <- as.matrix(design)
  if (!identical(colnames(design), fit$design_columns) ||
      anyNA(design) || any(!is.finite(design))) {
    stop("Prediction design differs from locked model: ", fit$model_id)
  }
  as.numeric(stats::plogis(
    cbind(`(Intercept)` = 1, design) %*% fit$coefficients
  ))
}

performance_vector <- function(y, probability) {
  metrics <- binary_performance(y, probability, eps = PROBABILITY_CLIP_EPS)
  out <- metrics[metric_names]
  if (anyNA(out) || any(!is.finite(out))) {
    stop("A required performance metric is non-finite.")
  }
  out
}

weighted_auc_rank <- function(y, probability, weights) {
  assert_binary_outcome(y)
  weights <- normalize_analysis_weights(weights, length(y))
  if (length(probability) != length(y) || anyNA(probability) ||
      any(!is.finite(probability))) {
    stop("Weighted AUC inputs are incomplete or non-finite.")
  }
  z <- data.table::data.table(y = y, p = probability, w = weights)
  grouped <- z[, .(
    event_weight = sum(w[y == 1L]),
    nonevent_weight = sum(w[y == 0L])
  ), by = p]
  data.table::setorder(grouped, p)
  grouped[, prior_nonevent_weight :=
    data.table::shift(cumsum(nonevent_weight), fill = 0)]
  numerator <- grouped[, sum(
    event_weight * (prior_nonevent_weight + 0.5 * nonevent_weight)
  )]
  denominator <- sum(weights[y == 1L]) * sum(weights[y == 0L])
  numerator / denominator
}

weighted_performance_vector <- function(y, probability, weights) {
  assert_binary_outcome(y)
  if (length(probability) != length(y) || anyNA(probability) ||
      any(!is.finite(probability)) || any(probability < 0 | probability > 1)) {
    stop("Weighted performance probabilities are invalid.")
  }
  weights <- normalize_analysis_weights(weights, length(y))
  pp <- clip_probability(probability, PROBABILITY_CLIP_EPS)
  weighted_mean <- function(x) sum(weights * x) / sum(weights)
  prevalence <- weighted_mean(y)
  brier <- weighted_mean((y - probability)^2)
  scaled_brier <- 1 - brier / (prevalence * (1 - prevalence))
  log_loss <- -weighted_mean(y * log(pp) + (1 - y) * log1p(-pp))
  lp <- qlogis(pp)
  intercept_fit <- suppressWarnings(stats::glm.fit(
    x = matrix(1, nrow = length(y), ncol = 1L), y = y, offset = lp,
    weights = weights, family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  slope_fit <- suppressWarnings(stats::glm.fit(
    x = cbind(intercept = 1, locked_lp = lp), y = y, weights = weights,
    family = stats::binomial(), control = stats::glm.control(maxit = 100L)
  ))
  if (!intercept_fit$converged || !slope_fit$converged ||
      anyNA(intercept_fit$coefficients) || anyNA(slope_fit$coefficients) ||
      any(!is.finite(c(intercept_fit$coefficients, slope_fit$coefficients)))) {
    stop("Weighted calibration fit failed.")
  }
  out <- c(
    brier = brier,
    scaled_brier = scaled_brier,
    log_loss = log_loss,
    calibration_in_the_large = unname(intercept_fit$coefficients[[1L]]),
    calibration_slope = unname(slope_fit$coefficients[[2L]]),
    c_statistic = weighted_auc_rank(y, pp, weights)
  )
  if (anyNA(out) || any(!is.finite(out))) {
    stop("A required weighted performance metric is non-finite.")
  }
  out[metric_names]
}

fit_recalibration <- function(y, probability) {
  assert_binary_outcome(y)
  lp <- qlogis(clip_probability(probability, PROBABILITY_CLIP_EPS))
  intercept_fit <- suppressWarnings(stats::glm.fit(
    x = matrix(1, nrow = length(y), ncol = 1L), y = y, offset = lp,
    family = stats::binomial(), control = stats::glm.control(maxit = 100L)
  ))
  slope_fit <- suppressWarnings(stats::glm.fit(
    x = cbind(intercept = 1, locked_lp = lp), y = y,
    family = stats::binomial(), control = stats::glm.control(maxit = 100L)
  ))
  if (!intercept_fit$converged || !slope_fit$converged ||
      anyNA(intercept_fit$coefficients) || anyNA(slope_fit$coefficients) ||
      any(!is.finite(c(intercept_fit$coefficients, slope_fit$coefficients)))) {
    stop("External recalibration failed.")
  }
  alpha_only <- unname(intercept_fit$coefficients[[1L]])
  alpha_slope <- unname(slope_fit$coefficients[[1L]])
  beta_slope <- unname(slope_fit$coefficients[[2L]])
  list(
    intercept_only = c(intercept = alpha_only, slope = 1),
    intercept_and_slope = c(intercept = alpha_slope, slope = beta_slope),
    probability_intercept_only = stats::plogis(alpha_only + lp),
    probability_intercept_and_slope = stats::plogis(alpha_slope + beta_slope * lp)
  )
}

flexible_calibration_curve <- function(y, probability) {
  assert_binary_outcome(y)
  if (!is.numeric(probability) || length(probability) != length(y) ||
      anyNA(probability) || any(!is.finite(probability)) ||
      any(probability < 0 | probability > 1)) {
    stop("Flexible-calibration inputs are invalid or not row-aligned.")
  }

  # The smoothing specification is fixed. Knot locations depend on predictions
  # alone (never on outcomes), and this descriptive curve is not used to choose
  # a model or transformation.
  locked_lp <- stats::qlogis(clip_probability(
    probability, PROBABILITY_CLIP_EPS
  ))
  knots <- quantile_knots(
    locked_lp, FLEXIBLE_CALIBRATION_KNOT_PROBS,
    variable = "locked_model_linear_predictor", type = 2L
  )
  design <- four_knot_rcs_basis(locked_lp, knots, "locked_lp_calibration")
  fit <- fit_locked_logistic(
    design, y, model_name = "descriptive_flexible_calibration"
  )
  grid_lp <- seq(
    min(locked_lp), max(locked_lp), length.out = FLEXIBLE_CALIBRATION_GRID_N
  )
  grid_design <- four_knot_rcs_basis(
    grid_lp, knots, "locked_lp_calibration"
  )
  calibrated <- stats::predict(fit, grid_design, type = "response")
  if (length(calibrated) != FLEXIBLE_CALIBRATION_GRID_N ||
      anyNA(calibrated) || any(!is.finite(calibrated))) {
    stop("Flexible-calibration prediction failed.")
  }

  data.table::data.table(
    grid_index = seq_len(FLEXIBLE_CALIBRATION_GRID_N),
    predicted_probability = stats::plogis(grid_lp),
    calibrated_observed_probability = as.numeric(calibrated),
    locked_linear_predictor = grid_lp,
    knot_lp_05 = knots[[1L]],
    knot_lp_35 = knots[[2L]],
    knot_lp_65 = knots[[3L]],
    knot_lp_95 = knots[[4L]],
    sample_n = length(y),
    events = sum(y),
    method = paste0(
      "logistic_natural_spline_of_locked_lp;prediction_quantile_knots=",
      "0.05,0.35,0.65,0.95;grid=101_observed_lp_range"
    ),
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_status = "POINT_ESTIMATE_ONLY_NO_CI",
    used_for_model_selection = FALSE
  )
}

cluster_bootstrap_indices <- function(cluster) {
  if (anyNA(cluster)) stop("Hospital cluster is missing.")
  clusters <- unique(as.character(cluster))
  draw <- sample(clusters, length(clusters), replace = TRUE)
  unlist(lapply(draw, function(value) which(as.character(cluster) == value)),
    use.names = FALSE
  )
}

percentile_interval <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(lower = NA_real_, upper = NA_real_))
  setNames(as.numeric(stats::quantile(
    x, probs = BOOTSTRAP_CI_PROBS, names = FALSE, type = 2L
  )), c("lower", "upper"))
}

wald_contrast <- function(fit, contrast, label, from_value, to_value, unit) {
  if (!identical(fit$status, "ESTIMABLE")) {
    return(data.table::data.table(
      model_id = fit$model_id, contrast = label, from_value = from_value,
      to_value = to_value, unit = unit, log_odds_difference = NA_real_,
      standard_error = NA_real_, odds_ratio = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_, status = "NON_ESTIMABLE"
    ))
  }
  full <- setNames(rep(0, length(fit$coefficients)), names(fit$coefficients))
  if (length(setdiff(names(contrast), names(full)))) {
    stop("Contrast contains a term absent from ", fit$model_id)
  }
  full[names(contrast)] <- contrast
  estimate <- sum(full * fit$coefficients)
  standard_error <- sqrt(as.numeric(t(full) %*% fit$vcov %*% full))
  data.table::data.table(
    model_id = fit$model_id, contrast = label, from_value = from_value,
    to_value = to_value, unit = unit, log_odds_difference = estimate,
    standard_error = standard_error, odds_ratio = exp(estimate),
    ci_lower = exp(estimate - 1.96 * standard_error),
    ci_upper = exp(estimate + 1.96 * standard_error), status = "ESTIMABLE"
  )
}
