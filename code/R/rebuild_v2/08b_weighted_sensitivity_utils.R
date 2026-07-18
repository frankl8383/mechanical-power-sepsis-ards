# ARDS mechanical-power rebuild v2: weighted outcome-sensitivity utilities.
#
# This module does not open any artifact. It is intended only after
# outcome-blind selection weights have been frozen and checksum-gated.

v2_attach_frozen_selection_weights <- function(
    frame,
    frozen_weight_table,
    id_column,
    weight_id_column = "row_id",
    weight_column = "stabilized_weight_truncated",
    eligibility_column = "permitted_for_outcome_weighting",
    output_column = "selection_weight",
    require_all_rows = TRUE) {
  if (!is.data.frame(frame) || !is.data.frame(frozen_weight_table)) {
    stop("Frame and frozen weights must be data frames.")
  }
  v2_require_columns(frame, id_column, "weighted model frame")
  v2_require_columns(
    frozen_weight_table,
    c(weight_id_column, weight_column, eligibility_column),
    "frozen selection-weight table"
  )
  eligibility <- frozen_weight_table[[eligibility_column]]
  if (!is.logical(eligibility) || anyNA(eligibility) ||
      !all(eligibility)) {
    stop(
      "Frozen table is diagnostic-only or lacks permission for outcome ",
      "weighting."
    )
  }
  if (output_column %in% names(frame)) {
    stop("Output weight column already exists: ", output_column)
  }
  frame_id <- as.character(frame[[id_column]])
  weight_id <- as.character(frozen_weight_table[[weight_id_column]])
  if (anyNA(frame_id) || any(!nzchar(frame_id)) || anyDuplicated(frame_id) ||
      anyNA(weight_id) || any(!nzchar(weight_id)) ||
      anyDuplicated(weight_id)) {
    stop("Model-frame and frozen-weight IDs must be complete and unique.")
  }
  matched <- match(frame_id, weight_id)
  if (require_all_rows && anyNA(matched)) {
    stop(
      "Frozen selection weights are absent for ",
      sum(is.na(matched)), " model-frame row(s)."
    )
  }
  out <- frame[!is.na(matched), , drop = FALSE]
  selected <- matched[!is.na(matched)]
  weight <- as.numeric(frozen_weight_table[[weight_column]][selected])
  if (anyNA(weight) || any(!is.finite(weight)) || any(weight <= 0)) {
    stop("Attached frozen weights are not complete positive finite values.")
  }
  out[[output_column]] <- weight
  out
}

v2_fit_weighted_logistic <- function(
    design,
    y,
    weights,
    model_id,
    row_ids = seq_along(y)) {
  v2_assert_binary_outcome(y)
  design <- as.matrix(design)
  if (!is.numeric(design) || nrow(design) != length(y) ||
      anyNA(design) || any(!is.finite(design)) ||
      is.null(colnames(design)) || anyDuplicated(colnames(design))) {
    stop("Invalid weighted design matrix for ", model_id)
  }
  if (!is.numeric(weights) || length(weights) != length(y) ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Weighted logistic regression requires complete positive weights.")
  }
  if (length(row_ids) != length(y) || anyNA(row_ids) ||
      anyDuplicated(row_ids)) {
    stop("Unique complete row identifiers are required.")
  }
  # Scaling all case weights by one constant does not change coefficients.
  # Mean-one normalization improves numerical comparability across analyses.
  analysis_weights <- weights / mean(weights)
  x <- cbind(`(Intercept)` = 1, design)
  fit <- suppressWarnings(stats::glm.fit(
    x = x,
    y = as.integer(y),
    weights = analysis_weights,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  if (!fit$converged || fit$rank != ncol(x) ||
      anyNA(fit$coefficients) || any(!is.finite(fit$coefficients))) {
    stop("Weighted logistic model failed or was rank deficient: ", model_id)
  }
  probability <- v2_clip_probability(
    stats::plogis(as.numeric(x %*% fit$coefficients))
  )
  weighted_log_likelihood <- sum(
    analysis_weights *
      (y * log(probability) + (1 - y) * log1p(-probability))
  )
  structure(
    list(
      model_id = model_id,
      coefficients = setNames(as.numeric(fit$coefficients), colnames(x)),
      design_columns = colnames(design),
      n = length(y),
      events = sum(y),
      analysis_weight_sum = sum(analysis_weights),
      effective_sample_size =
        sum(analysis_weights)^2 / sum(analysis_weights^2),
      minimum_analysis_weight = min(analysis_weights),
      maximum_analysis_weight = max(analysis_weights),
      weighted_log_likelihood = weighted_log_likelihood,
      row_ids = as.character(row_ids),
      converged = fit$converged
    ),
    class = "ards_v2_weighted_logistic"
  )
}

predict.ards_v2_weighted_logistic <- function(
    object, newdata, type = c("response", "link"), ...) {
  type <- match.arg(type)
  design <- as.matrix(newdata)
  if (is.null(colnames(design)) ||
      !identical(colnames(design), object$design_columns) ||
      anyNA(design) || any(!is.finite(design))) {
    stop("Prediction design differs from the locked weighted model.")
  }
  linear_predictor <- as.numeric(
    cbind(`(Intercept)` = 1, design) %*% object$coefficients
  )
  if (type == "link") linear_predictor else stats::plogis(linear_predictor)
}

v2_weighted_auc <- function(y, p, weights) {
  v2_assert_binary_outcome(y)
  if (length(y) != length(p) || length(y) != length(weights) ||
      anyNA(p) || any(!is.finite(p)) ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Invalid weighted AUC inputs.")
  }
  positive_weight <- sum(weights[y == 1L])
  negative_weight <- sum(weights[y == 0L])
  if (positive_weight <= 0 || negative_weight <= 0) {
    stop("Weighted AUC requires positive weight in both outcome classes.")
  }
  # Exact weighted Mann-Whitney statistic in O(n log n), with half credit for
  # ties. This avoids the quadratic event-by-nonevent loop when the weighted
  # sensitivity is evaluated in the larger MIMIC common set.
  order_index <- order(p)
  ordered_p <- p[order_index]
  ordered_y <- as.integer(y[order_index])
  ordered_weight <- weights[order_index]
  groups <- match(ordered_p, unique(ordered_p))
  positive_by_group <- as.numeric(rowsum(
    ordered_weight * (ordered_y == 1L),
    groups,
    reorder = FALSE
  ))
  negative_by_group <- as.numeric(rowsum(
    ordered_weight * (ordered_y == 0L),
    groups,
    reorder = FALSE
  ))
  negative_before <- c(0, head(cumsum(negative_by_group), -1L))
  numerator <- 0
  numerator <- sum(
    positive_by_group *
      (negative_before + 0.5 * negative_by_group)
  )
  numerator / (positive_weight * negative_weight)
}

v2_weighted_performance <- function(y, p, weights, eps = 1e-6) {
  v2_assert_binary_outcome(y)
  p <- v2_clip_probability(p, eps)
  if (length(y) != length(p) || length(y) != length(weights) ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Invalid weighted performance inputs.")
  }
  w <- weights / sum(weights)
  lp <- stats::qlogis(p)
  intercept_fit <- suppressWarnings(stats::glm.fit(
    x = matrix(1, nrow = length(y), ncol = 1L),
    y = y,
    weights = w,
    offset = lp,
    family = stats::binomial()
  ))
  slope_fit <- suppressWarnings(stats::glm.fit(
    x = cbind(`(Intercept)` = 1, linear_predictor = lp),
    y = y,
    weights = w,
    family = stats::binomial()
  ))
  if (!intercept_fit$converged || !slope_fit$converged ||
      slope_fit$rank != 2L ||
      anyNA(c(intercept_fit$coefficients, slope_fit$coefficients))) {
    stop("Weighted calibration model failed.")
  }
  c(
    n = length(y),
    events = sum(y),
    effective_sample_size = sum(weights)^2 / sum(weights^2),
    weighted_event_rate = sum(w * y),
    brier = sum(w * (y - p)^2),
    log_loss = -sum(
      w * (y * log(p) + (1 - y) * log1p(-p))
    ),
    c_statistic = v2_weighted_auc(y, p, w),
    calibration_in_the_large =
      unname(intercept_fit$coefficients[[1L]]),
    calibration_intercept =
      unname(slope_fit$coefficients[[1L]]),
    calibration_slope =
      unname(slope_fit$coefficients[[2L]]),
    observed_expected_ratio = sum(w * y) / sum(w * p)
  )
}
