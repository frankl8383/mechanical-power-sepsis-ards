# ARDS mechanical-power rebuild v1: locked-model utility functions
#
# These functions are outcome-agnostic infrastructure. They contain no source
# data paths and never select a model, transformation, threshold, or variable.
# The analysis driver must provide the pre-outcome frozen specification.

clip_probability <- function(p, eps = 1e-6) {
  if (!is.numeric(p) || length(eps) != 1L || eps <= 0 || eps >= 0.5) {
    stop("Invalid probability vector or clipping constant.")
  }
  if (anyNA(p) || any(!is.finite(p)) || any(p < 0 | p > 1)) {
    stop("Probabilities must be complete, finite, and within [0, 1].")
  }
  pmin(pmax(p, eps), 1 - eps)
}

assert_binary_outcome <- function(y) {
  if (anyNA(y) || !all(y %in% c(0, 1))) {
    stop("Outcome must be complete and coded exactly 0/1.")
  }
  if (length(unique(y)) != 2L) {
    stop("Both outcome classes are required.")
  }
  invisible(TRUE)
}

auc_rank <- function(y, p) {
  assert_binary_outcome(y)
  if (length(y) != length(p) || anyNA(p) || any(!is.finite(p))) {
    stop("AUC inputs are incomplete or non-finite.")
  }
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

calibration_coefficients <- function(y, p, eps = 1e-6) {
  assert_binary_outcome(y)
  lp <- qlogis(clip_probability(p, eps))

  intercept_fit <- suppressWarnings(stats::glm.fit(
    x = matrix(1, nrow = length(y), ncol = 1L),
    y = y,
    offset = lp,
    family = stats::binomial()
  ))
  slope_fit <- suppressWarnings(stats::glm.fit(
    x = cbind(intercept = 1, linear_predictor = lp),
    y = y,
    family = stats::binomial()
  ))

  c(
    calibration_in_the_large = unname(intercept_fit$coefficients[[1L]]),
    calibration_intercept = unname(slope_fit$coefficients[[1L]]),
    calibration_slope = unname(slope_fit$coefficients[[2L]])
  )
}

binary_performance <- function(y, p, eps = 1e-6) {
  assert_binary_outcome(y)
  if (length(y) != length(p) || anyNA(p) || any(!is.finite(p))) {
    stop("Performance inputs are incomplete or non-finite.")
  }
  pp <- clip_probability(p, eps)
  prevalence <- mean(y)
  brier <- mean((y - p)^2)
  reference_brier <- prevalence * (1 - prevalence)
  calibration <- calibration_coefficients(y, pp, eps = eps)

  c(
    n = length(y),
    events = sum(y),
    prevalence = prevalence,
    brier = brier,
    scaled_brier = 1 - brier / reference_brier,
    log_loss = -mean(y * log(pp) + (1 - y) * log1p(-pp)),
    c_statistic = auc_rank(y, pp),
    calibration
  )
}

validate_three_knots <- function(knots, variable = "variable") {
  if (!is.numeric(knots) || length(knots) != 3L || anyNA(knots) ||
      any(!is.finite(knots)) || any(diff(knots) <= 0)) {
    stop("Three strictly increasing finite knots required for ", variable, ".")
  }
  invisible(TRUE)
}

validate_knots <- function(knots, variable = "variable", minimum_n = 3L) {
  if (!is.numeric(knots) || length(knots) < minimum_n || anyNA(knots) ||
      any(!is.finite(knots)) || any(diff(knots) <= 0)) {
    stop(
      "At least ", minimum_n,
      " strictly increasing finite knots required for ", variable, "."
    )
  }
  invisible(TRUE)
}

natural_spline_basis <- function(x, knots, prefix) {
  validate_knots(knots, prefix, minimum_n = 3L)
  if (!is.numeric(x)) stop(prefix, " must be numeric.")
  internal_knots <- if (length(knots) > 2L) {
    knots[seq.int(2L, length(knots) - 1L)]
  } else {
    NULL
  }
  basis <- splines::ns(
    x,
    knots = internal_knots,
    Boundary.knots = knots[c(1L, length(knots))],
    intercept = FALSE
  )
  basis <- as.matrix(basis)
  colnames(basis) <- paste0(prefix, "_rcs", seq_len(ncol(basis)))
  basis
}

three_knot_rcs_basis <- function(x, knots, prefix) {
  validate_three_knots(knots, prefix)
  natural_spline_basis(x, knots, prefix)
}

four_knot_rcs_basis <- function(x, knots, prefix) {
  if (length(knots) != 4L) {
    stop("Exactly four knots required for ", prefix, ".")
  }
  validate_knots(knots, prefix, minimum_n = 4L)
  natural_spline_basis(x, knots, prefix)
}

quantile_knots <- function(x, probs, variable = "variable", type = 2L) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop("Complete finite values are required to derive knots for ", variable, ".")
  }
  knots <- as.numeric(stats::quantile(
    x, probs = probs, names = FALSE, type = type
  ))
  if (length(probs) == 3L) validate_three_knots(knots, variable)
  if (any(diff(knots) <= 0)) {
    stop("Quantile knots are not strictly increasing for ", variable, ".")
  }
  knots
}

fit_locked_logistic <- function(design, y, model_name = "model") {
  assert_binary_outcome(y)
  design <- as.matrix(design)
  if (!is.numeric(design) || nrow(design) != length(y) || anyNA(design) ||
      any(!is.finite(design))) {
    stop("Incomplete or invalid design matrix for ", model_name, ".")
  }
  if (is.null(colnames(design)) || anyDuplicated(colnames(design))) {
    stop("Unique design-matrix column names are required for ", model_name, ".")
  }
  x <- cbind(`(Intercept)` = 1, design)
  fit <- suppressWarnings(stats::glm.fit(
    x = x, y = y, family = stats::binomial()
  ))
  if (!fit$converged || anyNA(fit$coefficients)) {
    stop("Logistic model failed or is rank deficient: ", model_name)
  }
  structure(
    list(
      model_name = model_name,
      coefficients = setNames(as.numeric(fit$coefficients), colnames(x)),
      design_columns = colnames(design),
      converged = fit$converged,
      rank = fit$rank,
      n = length(y),
      events = sum(y),
      loglik = -fit$deviance / 2
    ),
    class = "ards_locked_logistic"
  )
}

predict.ards_locked_logistic <- function(object, newdata, type = c("response", "link"), ...) {
  type <- match.arg(type)
  x <- as.matrix(newdata)
  if (is.null(colnames(x)) || !identical(colnames(x), object$design_columns)) {
    stop("External design columns/order differ from the locked model.")
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop("External design matrix is incomplete or non-finite.")
  }
  lp <- as.numeric(cbind(`(Intercept)` = 1, x) %*% object$coefficients)
  if (type == "link") lp else stats::plogis(lp)
}

paired_metric_difference <- function(y, p_new, p_reference, eps = 1e-6) {
  new <- binary_performance(y, p_new, eps = eps)
  reference <- binary_performance(y, p_reference, eps = eps)
  common <- intersect(names(new), names(reference))
  out <- new[common] - reference[common]
  names(out) <- paste0("delta_", common)
  out
}
