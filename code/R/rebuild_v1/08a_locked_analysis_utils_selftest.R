#!/usr/bin/env Rscript

# Synthetic-only self-test for 08a_locked_analysis_utils.R. This script opens
# no project data, gate, checkpoint, model artifact, or outcome artifact.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/08a_locked_analysis_utils_selftest.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "08_model_utils.R"))
source(file.path(script_dir, "08a_locked_analysis_utils.R"))

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop("Synthetic self-test failed: ", label)
  invisible(TRUE)
}

expect_error <- function(expression, label) {
  failed <- FALSE
  tryCatch(
    force(expression),
    error = function(e) failed <<- TRUE
  )
  assert_true(failed, label)
}

set.seed(20260717L)
n <- 1500L
frame <- data.table(
  analysis_id = seq_len(n),
  patient_cluster_id = sprintf("synthetic_patient_%04d", seq_len(n)),
  age = runif(n, 20, 90),
  sex_female = rbinom(n, 1, 0.46),
  pf_ratio = runif(n, 70, 295),
  gcs = runif(n, 3, 15),
  map = runif(n, 45, 115),
  vasopressor = rbinom(n, 1, 0.34),
  platelet = runif(n, 45, 480),
  creatinine = runif(n, 0.35, 4.8),
  delta_p = runif(n, 5, 28),
  rr = runif(n, 10, 36),
  smp = runif(n, 5, 42),
  vt_per_pbw = runif(n, 4.2, 10.5),
  peep = runif(n, 4, 18),
  resistive_pressure = runif(n, 0.5, 14),
  pbw = runif(n, 42, 105),
  primary_predictor_complete = TRUE,
  component_predictor_complete = TRUE,
  normalized_exposure_complete = TRUE
)
frame[, smp_per_pbw := smp / pbw]
linear_predictor <- with(
  frame,
  -3.2 + 0.018 * (age - 55) + 0.055 * (smp - 20) +
    0.035 * (delta_p - 15) + 0.45 * vasopressor -
    0.002 * (pf_ratio - 170)
)
y <- rbinom(n, 1, plogis(linear_predictor))
assert_true(length(unique(y)) == 2L, "binary synthetic outcome")

bundle <- derive_bootstrap_transform_bundle(frame)
specs <- locked_model_specification()
assert_true(
  identical(
    specs$model_id,
    c("S0", "S1", "S2", "S3", "S2M", "S3NL", "S3c", "S4", "S5",
      "N3_abs", "N3_pbw", "R2", "R3")
  ),
  "thirteen-model order"
)
expected_columns <- c(
  S0 = 14L, S1 = 15L, S2 = 16L, S3 = 15L, S2M = 17L,
  S3NL = 17L, S3c = 15L, S4 = 19L, S5 = 20L,
  N3_abs = 15L, N3_pbw = 15L, R2 = 14L, R3 = 13L
)

fits <- list()
designs <- list()
for (model_id in specs$model_id) {
  design <- build_design_matrix(frame, model_id, bundle)
  assert_true(
    ncol(design) == expected_columns[[model_id]],
    paste0(model_id, " design-column count")
  )
  fit <- fit_model(design, y, model_id, allow_nonestimable = FALSE)
  probability <- predict_model(fit, design)
  metrics <- performance_vector(y, probability)
  assert_true(identical(fit$status, "ESTIMABLE"), paste0(model_id, " fit"))
  assert_true(
    length(probability) == n && all(is.finite(probability)) &&
      all(probability > 0 & probability < 1),
    paste0(model_id, " prediction")
  )
  assert_true(
    identical(names(metrics), metric_names) && all(is.finite(metrics)),
    paste0(model_id, " performance")
  )
  designs[[model_id]] <- design
  fits[[model_id]] <- fit
}

assert_true(
  identical(tail(colnames(designs$S3), 1L), "smp_per_5") &&
    !any(grepl("smp_rcs", colnames(designs$S3), fixed = TRUE)),
  "S3 uses only linear sMP per 5 J/min"
)
s3nl_smp_columns <- grep("^smp_rcs[123]$", colnames(designs$S3NL), value = TRUE)
assert_true(
  identical(s3nl_smp_columns, paste0("smp_rcs", 1:3)) &&
    !"smp_per_5" %in% colnames(designs$S3NL),
  "S3NL alone carries the three-column four-knot sMP spline"
)

unweighted <- fits$S3
all_one_weighted <- fit_weighted_model(
  designs$S3, y, rep(1, n), "S3_all_one_weight"
)
assert_true(
  isTRUE(all.equal(
    unweighted$coefficients, all_one_weighted$coefficients,
    tolerance = 1e-10, check.attributes = TRUE
  )),
  "all-one weighted coefficients equal unweighted coefficients"
)
unweighted_probability <- predict_model(unweighted, designs$S3)
assert_true(
  isTRUE(all.equal(
    performance_vector(y, unweighted_probability),
    weighted_performance_vector(y, unweighted_probability, rep(1, n)),
    tolerance = 1e-10, check.attributes = TRUE
  )),
  "all-one weighted performance equals unweighted performance"
)
assert_true(
  identical(all_one_weighted$vcov_type, "HC0_sandwich") &&
    all(is.finite(all_one_weighted$vcov)) &&
    all(diag(all_one_weighted$vcov) > 0) &&
    all(is.finite(all_one_weighted$vcov_model_based)) &&
    all(diag(all_one_weighted$vcov_model_based) > 0),
  "finite positive HC0 and model-based covariance"
)
expect_error(
  fit_weighted_model(designs$S3, y, replace(rep(1, n), 1L, 0), "bad_zero"),
  "zero weight rejected"
)
expect_error(
  fit_weighted_model(designs$S3, y, replace(rep(1, n), 1L, NA), "bad_na"),
  "missing weight rejected"
)
expect_error(
  fit_weighted_model(designs$S3, y, replace(rep(1, n), 1L, Inf), "bad_inf"),
  "infinite weight rejected"
)

curve <- flexible_calibration_curve(y, unweighted_probability)
assert_true(nrow(curve) == 101L, "flexible calibration has 101 grid points")
assert_true(
  all(diff(curve$predicted_probability) > 0) &&
    all(is.finite(curve$calibrated_observed_probability)) &&
    all(curve$calibrated_observed_probability >= 0 &
      curve$calibrated_observed_probability <= 1) &&
    all(is.na(curve$ci_lower)) && all(is.na(curve$ci_upper)) &&
    all(curve$ci_status == "POINT_ESTIMATE_ONLY_NO_CI") &&
    all(curve$used_for_model_selection == FALSE),
  "flexible calibration fixed point-estimate-only contract"
)

cluster <- rep(sprintf("hospital_%02d", 1:50), each = 3L)
set.seed(20260718L)
cluster_index <- cluster_bootstrap_indices(cluster)
original_cluster_size <- table(cluster)
resampled_cluster_size <- table(factor(cluster[cluster_index], levels = names(
  original_cluster_size
)))
assert_true(
  length(cluster_index) == length(cluster) &&
    all(cluster_index >= 1L & cluster_index <= length(cluster)) &&
    all(as.integer(resampled_cluster_size) %%
      as.integer(original_cluster_size) == 0L) &&
    sum(as.integer(resampled_cluster_size) /
      as.integer(original_cluster_size)) == length(original_cluster_size),
  "hospital cluster bootstrap returns whole-cluster draws"
)

cat("LOCKED_ANALYSIS_UTILS_SYNTHETIC_PASS\n")
