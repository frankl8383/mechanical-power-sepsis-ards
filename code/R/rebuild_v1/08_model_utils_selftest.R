#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/08_model_utils_selftest.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "08_model_utils.R"))

set.seed(20260715)
n <- 500L
x <- stats::rnorm(n)
z <- stats::runif(n)
y <- stats::rbinom(n, 1, stats::plogis(-0.2 + 0.8 * x - 0.4 * z))
knots <- quantile_knots(x, c(0.1, 0.5, 0.9), "synthetic_x")
design <- cbind(three_knot_rcs_basis(x, knots, "x"), z = z)
fit <- fit_locked_logistic(design, y, "synthetic_model")
probability <- predict(fit, design)
metrics <- binary_performance(y, probability)

stopifnot(
  length(probability) == n,
  all(probability > 0 & probability < 1),
  fit$converged,
  abs(metrics[["calibration_in_the_large"]]) < 1e-6,
  abs(metrics[["calibration_slope"]] - 1) < 1e-6,
  metrics[["c_statistic"]] > 0.65
)

x_spline <- seq(1, 100, length.out = 50)
b3 <- three_knot_rcs_basis(x_spline, c(10, 50, 90), "x3")
b4 <- four_knot_rcs_basis(x_spline, c(5, 35, 65, 95), "x4")
stopifnot(
  is.matrix(b3), nrow(b3) == length(x_spline), ncol(b3) == 2L,
  identical(colnames(b3), c("x3_rcs1", "x3_rcs2")),
  is.matrix(b4), nrow(b4) == length(x_spline), ncol(b4) == 3L,
  identical(colnames(b4), c("x4_rcs1", "x4_rcs2", "x4_rcs3")),
  all(is.finite(b3)), all(is.finite(b4))
)

cat("MODEL_UTILS_SYNTHETIC_PASS\n")
