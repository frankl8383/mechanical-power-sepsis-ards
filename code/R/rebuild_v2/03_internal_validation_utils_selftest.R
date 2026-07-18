#!/usr/bin/env Rscript

# Synthetic-only tests. No project dataset or outcome artifact is opened.

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/03_internal_validation_utils_selftest.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "01_analysis_utils.R"))
source(file.path(dirname(script_path), "03_internal_validation_utils.R"))

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop("Synthetic self-test failed: ", label)
  invisible(TRUE)
}

set.seed(2026071604L)
n <- 600L
synthetic <- data.frame(
  x1 = rnorm(n),
  x2 = rbinom(n, 1, 0.45)
)
synthetic$y <- rbinom(
  n,
  1,
  plogis(-1.25 + 0.75 * synthetic$x1 + 0.55 * synthetic$x2)
)
assert_true(length(unique(synthetic$y)) == 2L, "binary outcome")

fit_pipeline <- function(data) {
  fit <- suppressWarnings(stats::glm(
    y ~ x1 + x2,
    data = data,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  if (!fit$converged || anyNA(stats::coef(fit)) ||
      any(!is.finite(stats::coef(fit)))) {
    stop("synthetic logistic fit failed")
  }
  fit
}

predict_pipeline <- function(model, data) {
  stats::predict(model, newdata = data, type = "response")
}

one_stage <- v2_harrell_internal_validation(
  data = synthetic,
  outcome = "y",
  fit_pipeline = fit_pipeline,
  predict_pipeline = predict_pipeline,
  repetitions = 140L,
  seed = 2026071605L,
  minimum_success_fraction = 0.90,
  pipeline_id = "synthetic_fixed_logistic"
)
v2_iv_assert_reportable(one_stage)

assert_true(
  identical(names(one_stage$corrected), v2_iv_default_metrics) &&
    all(is.finite(one_stage$corrected)),
  "finite optimism-corrected metrics"
)
assert_true(
  one_stage$successful_replicates == 140L &&
    one_stage$failed_replicates == 0L,
  "one-stage completion audit"
)

# Noma Algorithm 1 uses the percentile interval of bootstrap-sample apparent
# performance and shifts both bounds by the mean optimism.
brier_rows <- one_stage$replicates[
  one_stage$replicates$metric == "brier",
  ,
  drop = FALSE
]
expected_brier_ci <- as.numeric(stats::quantile(
  brier_rows$train_estimate,
  probs = c(0.025, 0.975),
  names = FALSE,
  type = 7L
)) - mean(brier_rows$optimism)
reported_brier_ci <- one_stage$location_shifted_ci[
  one_stage$location_shifted_ci$metric == "brier",
  c("lower", "upper")
]
assert_true(
  isTRUE(all.equal(
    as.numeric(reported_brier_ci),
    expected_brier_ci,
    tolerance = 1e-12
  )),
  "location-shifted CI formula"
)

# For unpenalized logistic regression, apparent calibration slope in every
# training sample is one. The location-shifted interval must therefore be
# refused rather than presented as a zero-width sampling interval.
slope_ls <- one_stage$location_shifted_ci[
  one_stage$location_shifted_ci$metric == "calibration_slope",
  ,
  drop = FALSE
]
slope_rows <- one_stage$replicates[
  one_stage$replicates$metric == "calibration_slope",
  ,
  drop = FALSE
]
assert_true(
  isTRUE(all.equal(
    unname(one_stage$corrected[["calibration_slope"]]),
    mean(slope_rows$test_estimate),
    tolerance = 1e-6
  )),
  "Harrell-corrected calibration-slope point estimate"
)
assert_true(
  !slope_ls$supported && is.na(slope_ls$lower) &&
    grepl("use_two_stage_bootstrap", slope_ls$reason, fixed = TRUE),
  "degenerate calibration-slope LS interval blocked"
)

two_stage <- v2_two_stage_internal_validation(
  data = synthetic,
  outcome = "y",
  fit_pipeline = fit_pipeline,
  predict_pipeline = predict_pipeline,
  outer_repetitions = 32L,
  inner_repetitions = 32L,
  seed = 2026071606L,
  minimum_inner_success_fraction = 0.80,
  minimum_outer_success_fraction = 0.80,
  pipeline_id = "synthetic_fixed_logistic",
  point_validation = one_stage
)
v2_iv_assert_reportable(two_stage)
slope_ts <- two_stage$confidence_interval[
  two_stage$confidence_interval$metric == "calibration_slope",
  ,
  drop = FALSE
]
assert_true(
  slope_ts$supported && is.finite(slope_ts$lower) &&
    is.finite(slope_ts$upper) && slope_ts$lower < slope_ts$upper,
  "two-stage calibration-slope interval"
)
assert_true(
  nrow(two_stage$outer_estimates) ==
    32L * length(v2_iv_default_metrics),
  "two-stage outer estimate count"
)

# Deliberately make a fixed pipeline fail when the single marker row is absent
# from a bootstrap sample, verifying that failure rates and reasons are kept.
flaky <- synthetic[seq_len(120L), , drop = FALSE]
flaky$must_include <- 0L
flaky$must_include[[1L]] <- 1L
fit_flaky <- function(data) {
  if (!any(data$must_include == 1L)) {
    stop("synthetic_intentional_marker_omission")
  }
  fit_pipeline(data)
}
failed_validation <- v2_harrell_internal_validation(
  data = flaky,
  outcome = "y",
  fit_pipeline = fit_flaky,
  predict_pipeline = predict_pipeline,
  repetitions = 60L,
  seed = 2026071607L,
  minimum_success_fraction = 0.90,
  pipeline_id = "synthetic_flaky_pipeline"
)
assert_true(
  !failed_validation$reportable &&
    failed_validation$failed_replicates > 0L &&
    any(grepl(
      "synthetic_intentional_marker_omission",
      failed_validation$failure_summary$reason,
      fixed = TRUE
    )),
  "failure-rate audit"
)
assert_true(
  all(!failed_validation$location_shifted_ci$supported),
  "non-reportable validation cannot emit CIs"
)

cat("REBUILD_V2_INTERNAL_VALIDATION_SYNTHETIC_PASS\n")
