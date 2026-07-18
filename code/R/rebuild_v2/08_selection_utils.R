# ARDS mechanical-power rebuild v2: outcome-blind inclusion-weight utilities
#
# These functions model measurement/common-set inclusion among patients who
# are alive and still hospitalized at the fixed 6-hour landmark. They do not
# open, accept, or use a clinical outcome. The resulting weights are a
# sensitivity under a measured inclusion model, not proof that selection bias
# has been removed.

v2_selection_require_binary <- function(x, label = "inclusion") {
  if ((!is.numeric(x) && !is.integer(x) && !is.logical(x)) ||
      anyNA(x) || !all(as.integer(x) %in% c(0L, 1L)) ||
      length(unique(as.integer(x))) != 2L) {
    stop(label, " must be complete, binary, and contain both classes.")
  }
  invisible(TRUE)
}

v2_selection_weighted_mean <- function(x, w) {
  if (!is.numeric(x) || !is.numeric(w) || length(x) != length(w) ||
      anyNA(x) || anyNA(w) || any(!is.finite(x)) ||
      any(!is.finite(w)) || any(w < 0) || sum(w) <= 0) {
    stop("Invalid weighted-mean inputs.")
  }
  sum(x * w) / sum(w)
}

v2_selection_weighted_variance <- function(x, w) {
  mean_x <- v2_selection_weighted_mean(x, w)
  sum(w * (x - mean_x)^2) / sum(w)
}

v2_selection_effective_sample_size <- function(w) {
  if (!is.numeric(w) || !length(w) || anyNA(w) ||
      any(!is.finite(w)) || any(w < 0) || sum(w) <= 0) {
    stop("Invalid weights for effective sample size.")
  }
  sum(w)^2 / sum(w^2)
}

v2_selection_common_support <- function(frame, inclusion, cluster) {
  v2_require_columns(frame, c(inclusion, cluster), "selection support frame")
  v2_selection_require_binary(frame[[inclusion]], inclusion)
  if (anyNA(frame[[cluster]])) stop("Selection-support clusters are incomplete.")
  cluster_value <- as.character(frame[[cluster]])
  included <- as.integer(frame[[inclusion]]) == 1L
  support <- tapply(included, cluster_value, any)
  supported_clusters <- names(support)[support]
  unsupported_clusters <- names(support)[!support]
  counts <- data.frame(
    cluster = names(support),
    n = as.integer(table(cluster_value)[names(support)]),
    included_n = as.integer(tapply(
      included, cluster_value, sum
    )[names(support)]),
    supported = as.logical(support),
    stringsAsFactors = FALSE
  )
  list(
    keep = cluster_value %in% supported_clusters,
    supported_clusters = supported_clusters,
    unsupported_clusters = unsupported_clusters,
    cluster_counts = counts
  )
}

v2_selection_derive_bundle <- function(
    frame,
    always_observed_continuous,
    possibly_missing_continuous,
    binary_variables,
    knot_probabilities = c(0.10, 0.50, 0.90),
    minimum_unique_for_spline = 5L) {
  columns <- c(
    always_observed_continuous,
    possibly_missing_continuous,
    binary_variables
  )
  if (!is.character(columns) || !length(columns) || anyDuplicated(columns)) {
    stop("Selection variables must be uniquely named.")
  }
  v2_require_columns(frame, columns, "selection derivation frame")
  if (any(!vapply(
    columns,
    function(variable) is.numeric(frame[[variable]]),
    logical(1L)
  ))) {
    stop("Selection variables must be numeric.")
  }
  if (!is.numeric(minimum_unique_for_spline) ||
      length(minimum_unique_for_spline) != 1L ||
      is.na(minimum_unique_for_spline) ||
      !is.finite(minimum_unique_for_spline) ||
      minimum_unique_for_spline != as.integer(minimum_unique_for_spline) ||
      minimum_unique_for_spline < length(knot_probabilities)) {
    stop(
      "minimum_unique_for_spline must be one integer at least as large as ",
      "the number of knot probabilities."
    )
  }
  for (variable in always_observed_continuous) {
    x <- frame[[variable]]
    if (anyNA(x) || any(!is.finite(x))) {
      stop("Always-observed selection variable is incomplete: ", variable)
    }
  }
  for (variable in binary_variables) {
    x <- frame[[variable]]
    if (anyNA(x) || any(!is.finite(x)) || !all(x %in% c(0, 1))) {
      stop("Binary selection variable is invalid: ", variable)
    }
  }
  medians <- setNames(vapply(
    possibly_missing_continuous,
    function(variable) {
      x <- frame[[variable]]
      observed <- x[!is.na(x) & is.finite(x)]
      if (!length(observed)) {
        stop("No observed value for selection variable: ", variable)
      }
      stats::median(observed)
    },
    numeric(1L)
  ), possibly_missing_continuous)
  transformed <- as.data.frame(frame, stringsAsFactors = FALSE)
  for (variable in possibly_missing_continuous) {
    missing <- is.na(transformed[[variable]]) |
      !is.finite(transformed[[variable]])
    transformed[[variable]][missing] <- medians[[variable]]
  }
  spline_variables <- c(
    always_observed_continuous,
    possibly_missing_continuous
  )
  transformations <- setNames(lapply(spline_variables, function(variable) {
    values <- transformed[[variable]]
    candidate <- as.numeric(stats::quantile(
      values,
      probs = knot_probabilities,
      names = FALSE,
      type = 2L
    ))
    unique_n <- length(unique(values))
    if (unique_n >= minimum_unique_for_spline &&
        length(unique(candidate)) == length(candidate)) {
      return(list(
        type = "restricted_cubic_spline",
        knots = candidate,
        center = NA_real_,
        scale = NA_real_,
        unique_n = unique_n,
        fallback_reason = ""
      ))
    }
    center <- stats::median(values)
    scale <- stats::IQR(values, type = 2L) / 1.349
    if (!is.finite(scale) || scale <= .Machine$double.eps^0.5) {
      scale <- stats::sd(values)
    }
    if (!is.finite(scale) || scale <= .Machine$double.eps^0.5) {
      stop(
        "Selection variable is constant and cannot be modeled: ", variable
      )
    }
    list(
      type = "robust_scaled_linear",
      knots = numeric(),
      center = center,
      scale = scale,
      unique_n = unique_n,
      fallback_reason = if (unique_n < minimum_unique_for_spline) {
        "fewer_unique_values_than_prespecified_minimum"
      } else {
        "non_unique_prespecified_quantile_knots"
      }
    )
  }), spline_variables)
  transformation_audit <- do.call(rbind, lapply(
    names(transformations),
    function(variable) {
      specification <- transformations[[variable]]
      data.frame(
        variable = variable,
        transformation = specification$type,
        unique_n = specification$unique_n,
        knot_probabilities = if (
          identical(specification$type, "restricted_cubic_spline")
        ) {
          paste(knot_probabilities, collapse = ";")
        } else {
          ""
        },
        knots = if (length(specification$knots)) {
          paste(format(specification$knots, digits = 16), collapse = ";")
        } else {
          ""
        },
        center = specification$center,
        scale = specification$scale,
        fallback_reason = specification$fallback_reason,
        stringsAsFactors = FALSE
      )
    }
  ))
  list(
    always_observed_continuous = always_observed_continuous,
    possibly_missing_continuous = possibly_missing_continuous,
    binary_variables = binary_variables,
    medians = medians,
    transformations = transformations,
    transformation_audit = transformation_audit,
    knot_probabilities = knot_probabilities,
    quantile_type = 2L,
    minimum_unique_for_spline = as.integer(minimum_unique_for_spline),
    transformation_rule = paste(
      "Use the prespecified restricted cubic spline only when all requested",
      "quantile knots are unique and the variable has the prespecified",
      "minimum number of unique values; otherwise use a median-centered",
      "robust-scaled linear term. This rule is outcome-blind."
    )
  )
}

v2_selection_build_design <- function(frame, bundle) {
  required <- c(
    bundle$always_observed_continuous,
    bundle$possibly_missing_continuous,
    bundle$binary_variables
  )
  v2_require_columns(frame, required, "selection design frame")
  pieces <- list()
  audit_values <- list()
  position <- 1L
  for (variable in c(
    bundle$always_observed_continuous,
    bundle$possibly_missing_continuous
  )) {
    values <- as.numeric(frame[[variable]])
    missing <- is.na(values) | !is.finite(values)
    if (variable %in% bundle$always_observed_continuous && any(missing)) {
      stop("Always-observed selection variable is incomplete: ", variable)
    }
    if (variable %in% bundle$possibly_missing_continuous) {
      values[missing] <- bundle$medians[[variable]]
    }
    specification <- bundle$transformations[[variable]]
    if (is.null(specification) ||
        !specification$type %in%
          c("restricted_cubic_spline", "robust_scaled_linear")) {
      stop("Malformed selection transformation for ", variable)
    }
    basis <- if (identical(
      specification$type, "restricted_cubic_spline"
    )) {
      v2_natural_spline_basis(
        values,
        specification$knots,
        paste0("sel_", variable)
      )
    } else {
      out <- matrix(
        (values - specification$center) / specification$scale,
        ncol = 1L
      )
      colnames(out) <- paste0("sel_", variable, "_linear_scaled")
      out
    }
    pieces[[position]] <- basis
    position <- position + 1L
    audit_values[[variable]] <- values
    if (variable %in% bundle$possibly_missing_continuous) {
      indicator <- matrix(as.numeric(missing), ncol = 1L)
      colnames(indicator) <- paste0("sel_", variable, "_missing")
      pieces[[position]] <- indicator
      position <- position + 1L
      audit_values[[paste0(variable, "_missing")]] <- as.numeric(missing)
    }
  }
  for (variable in bundle$binary_variables) {
    values <- as.numeric(frame[[variable]])
    if (anyNA(values) || any(!is.finite(values)) ||
        !all(values %in% c(0, 1))) {
      stop("Invalid binary selection variable: ", variable)
    }
    piece <- matrix(values, ncol = 1L)
    colnames(piece) <- paste0("sel_", variable)
    pieces[[position]] <- piece
    position <- position + 1L
    audit_values[[variable]] <- values
  }
  raw_design <- do.call(cbind, pieces)
  storage.mode(raw_design) <- "double"
  if (anyNA(raw_design) || any(!is.finite(raw_design)) ||
      anyDuplicated(colnames(raw_design))) {
    stop("Invalid selection design matrix.")
  }
  # Deterministic, outcome-blind estimability gate. Columns are considered in
  # the prespecified order and retained only if they increase the rank of the
  # matrix that already contains an intercept. This removes constant
  # missingness indicators, constant binary terms, and exact linear
  # dependencies without inspecting inclusion or any clinical outcome.
  accepted <- matrix(1, nrow = nrow(raw_design), ncol = 1L)
  keep <- rep(FALSE, ncol(raw_design))
  rank_before <- 1L
  for (j in seq_len(ncol(raw_design))) {
    candidate <- cbind(accepted, raw_design[, j, drop = FALSE])
    rank_after <- qr(candidate, tol = 1e-10)$rank
    if (rank_after > rank_before) {
      keep[[j]] <- TRUE
      accepted <- candidate
      rank_before <- rank_after
    }
  }
  design_audit <- data.frame(
    design_column = colnames(raw_design),
    retained = keep,
    reason = ifelse(
      keep,
      "retained_prespecified_order",
      ifelse(
        vapply(
          seq_len(ncol(raw_design)),
          function(j) {
            stats::sd(raw_design[, j]) <= .Machine$double.eps^0.5
          },
          logical(1L)
        ),
        "constant_or_intercept_collinear",
        "exact_linear_dependency"
      )
    ),
    stringsAsFactors = FALSE
  )
  design <- raw_design[, keep, drop = FALSE]
  if (!ncol(design)) {
    stop("No estimable selection-design columns remained.")
  }
  list(
    design = design,
    design_audit = design_audit,
    balance_values = as.data.frame(
      audit_values,
      stringsAsFactors = FALSE
    )
  )
}

v2_selection_balance <- function(values, inclusion, weights) {
  v2_selection_require_binary(inclusion)
  if (!is.data.frame(values) || nrow(values) != length(inclusion) ||
      length(weights) != sum(inclusion == 1L)) {
    stop("Invalid selection-balance inputs.")
  }
  included <- inclusion == 1L
  rows <- lapply(names(values), function(variable) {
    x <- values[[variable]]
    if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
      stop("Balance variable must be complete finite numeric: ", variable)
    }
    target_mean <- mean(x)
    target_sd <- stats::sd(x)
    unweighted_mean <- mean(x[included])
    included_unweighted_sd <- stats::sd(x[included])
    weighted_mean <- v2_selection_weighted_mean(x[included], weights)
    denominator <- if (
      is.finite(target_sd) && target_sd > .Machine$double.eps^0.5
    ) target_sd else 1
    data.frame(
      variable = variable,
      target_mean = target_mean,
      target_sd = target_sd,
      included_unweighted_mean = unweighted_mean,
      included_unweighted_sd = included_unweighted_sd,
      included_weighted_mean = weighted_mean,
      smd_unweighted = (unweighted_mean - target_mean) / denominator,
      smd_weighted = (weighted_mean - target_mean) / denominator,
      structurally_nonreweightable =
        is.finite(target_sd) &&
          target_sd > .Machine$double.eps^0.5 &&
          (
            !is.finite(included_unweighted_sd) ||
              included_unweighted_sd <= .Machine$double.eps^0.5
          ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_fit_selection_weights <- function(
    frame,
    inclusion,
    row_id,
    bundle,
    truncation_quantiles = c(0.01, 0.99),
    probability_clip = 1e-4,
    model_id = "outcome_blind_joint_inclusion") {
  v2_require_columns(frame, c(inclusion, row_id), "selection frame")
  y <- as.integer(frame[[inclusion]])
  v2_selection_require_binary(y, inclusion)
  ids <- as.character(frame[[row_id]])
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Selection row IDs must be complete and unique.")
  }
  if (!is.numeric(truncation_quantiles) ||
      length(truncation_quantiles) != 2L ||
      anyNA(truncation_quantiles) ||
      truncation_quantiles[[1L]] <= 0 ||
      truncation_quantiles[[2L]] >= 1 ||
      truncation_quantiles[[1L]] >= truncation_quantiles[[2L]]) {
    stop("Invalid selection-weight truncation quantiles.")
  }
  built <- v2_selection_build_design(frame, bundle)
  x <- cbind(`(Intercept)` = 1, built$design)
  fit <- suppressWarnings(stats::glm.fit(
    x = x,
    y = y,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  if (!fit$converged || fit$rank != ncol(x) ||
      anyNA(fit$coefficients) || any(!is.finite(fit$coefficients))) {
    stop("Selection model failed or was rank deficient: ", model_id)
  }
  probability_unclipped <- stats::plogis(
    as.numeric(x %*% fit$coefficients)
  )
  probability_clipped_low <- probability_unclipped < probability_clip
  probability_clipped_high <-
    probability_unclipped > 1 - probability_clip
  probability <- pmin(
    pmax(probability_unclipped, probability_clip),
    1 - probability_clip
  )
  prevalence <- mean(y)
  included <- y == 1L
  stabilized <- prevalence / probability[included]
  limits <- as.numeric(stats::quantile(
    stabilized,
    probs = truncation_quantiles,
    names = FALSE,
    type = 2L
  ))
  truncated <- pmin(pmax(stabilized, limits[[1L]]), limits[[2L]])
  balance <- v2_selection_balance(
    built$balance_values,
    y,
    truncated
  )
  probability_summary <- function(values, group) {
    q <- as.numeric(stats::quantile(
      values,
      probs = c(0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1),
      names = FALSE,
      type = 2L
    ))
    data.frame(
      group = group,
      n = length(values),
      minimum = q[[1L]],
      q01 = q[[2L]],
      q05 = q[[3L]],
      q25 = q[[4L]],
      median = q[[5L]],
      q75 = q[[6L]],
      q95 = q[[7L]],
      q99 = q[[8L]],
      maximum = q[[9L]],
      stringsAsFactors = FALSE
    )
  }
  probability_distribution <- rbind(
    probability_summary(probability[included], "included"),
    probability_summary(probability[!included], "not_included")
  )
  weight_distribution <- probability_summary(
    stabilized,
    "stabilized_weight_raw"
  )
  weight_distribution <- rbind(
    weight_distribution,
    probability_summary(truncated, "stabilized_weight_truncated")
  )
  reweightable_balance <- !balance$structurally_nonreweightable
  summary <- data.frame(
    model_id = model_id,
    target_n = length(y),
    included_n = sum(included),
    inclusion_prevalence = prevalence,
    model_c_statistic = v2_auc_rank(y, probability),
    probability_clip = probability_clip,
    probability_clipped_low_n = sum(probability_clipped_low),
    probability_clipped_high_n = sum(probability_clipped_high),
    truncation_lower_quantile = truncation_quantiles[[1L]],
    truncation_upper_quantile = truncation_quantiles[[2L]],
    truncation_lower_value = limits[[1L]],
    truncation_upper_value = limits[[2L]],
    truncated_low_n = sum(stabilized < limits[[1L]]),
    truncated_high_n = sum(stabilized > limits[[2L]]),
    effective_sample_size_raw =
      v2_selection_effective_sample_size(stabilized),
    effective_sample_size_truncated =
      v2_selection_effective_sample_size(truncated),
    maximum_absolute_smd_unweighted =
      max(abs(balance$smd_unweighted)),
    maximum_absolute_smd_weighted =
      max(abs(balance$smd_weighted)),
    maximum_absolute_smd_weighted_reweightable = if (
      any(reweightable_balance)
    ) {
      max(abs(balance$smd_weighted[reweightable_balance]))
    } else {
      NA_real_
    },
    nonreweightable_balance_variable_n =
      sum(balance$structurally_nonreweightable),
    maximum_absolute_smd_nonreweightable = if (
      any(balance$structurally_nonreweightable)
    ) {
      max(abs(
        balance$smd_weighted[balance$structurally_nonreweightable]
      ))
    } else {
      NA_real_
    },
    prespecified_design_column_n = nrow(built$design_audit),
    retained_design_column_n = sum(built$design_audit$retained),
    omitted_nonestimable_design_column_n =
      sum(!built$design_audit$retained),
    stringsAsFactors = FALSE
  )
  list(
    model_id = model_id,
    coefficients = setNames(as.numeric(fit$coefficients), colnames(x)),
    bundle = bundle,
    design_audit = built$design_audit,
    summary = summary,
    probability_distribution = probability_distribution,
    weight_distribution = weight_distribution,
    balance = balance,
    included_weights = data.frame(
      row_id = ids[included],
      inclusion_probability = probability[included],
      stabilized_weight_raw = stabilized,
      stabilized_weight_truncated = truncated,
      stringsAsFactors = FALSE
    ),
    all_probabilities = data.frame(
      row_id = ids,
      included = y,
      inclusion_probability = probability,
      probability_was_clipped_low = probability_clipped_low,
      probability_was_clipped_high = probability_clipped_high,
      stringsAsFactors = FALSE
    ),
    interpretation = paste(
      "Outcome-blind measured-selection sensitivity only;",
      "does not establish elimination of selection bias."
    )
  )
}
