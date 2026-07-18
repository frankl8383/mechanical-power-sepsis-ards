#!/usr/bin/env Rscript

# Synthetic-only tests. No project dataset or outcome artifact is opened.

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/01_analysis_utils_selftest.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "01_analysis_utils.R"))

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop("Synthetic self-test failed: ", label)
  invisible(TRUE)
}

set.seed(2026071601L)
n <- 3000L
raw <- data.frame(
  plateau_cmH2O = runif(n, 17, 34),
  peep_cmH2O = runif(n, 5, 15),
  tidal_volume_mL = runif(n, 300, 650),
  rr_per_min = runif(n, 10, 35)
)
raw$plateau_cmH2O <- pmax(
  raw$plateau_cmH2O,
  raw$peep_cmH2O + runif(n, 4, 16)
)
raw$peak_cmH2O <- raw$plateau_cmH2O + runif(n, 1, 12)
vent <- v2_derive_ventilator_representations(raw)

assert_true(all(vent$tuple_valid), "all synthetic tuples valid")
assert_true(
  max(abs(vent$energy_identity_error)) < 1e-10,
  "exact surrogate-equation algebraic identity"
)
assert_true(
  isTRUE(all.equal(
    vent$smp,
    vent$static_power + vent$dynamic_power + vent$resistive_power,
    tolerance = 1e-12
  )),
  "component sum equals sMP"
)

bad <- raw[1:3, ]
bad$peak_cmH2O[1] <- bad$plateau_cmH2O[1] - 1
bad$tidal_volume_mL[2] <- 50
bad$rr_per_min[3] <- NA_real_
bad_derived <- v2_derive_ventilator_representations(bad)
assert_true(
  identical(bad_derived$tuple_valid, c(FALSE, FALSE, FALSE)),
  "invalid tuples rejected"
)
assert_true(
  all(nzchar(bad_derived$tuple_invalid_reason)),
  "invalid reasons populated"
)

rate <- v2_rate_concordance(
  set_rr = c(20, 20, 20, NA),
  total_rr = c(20, 22, 23, 20),
  maximum_difference = 2
)
assert_true(
  identical(rate$rate_concordant, c(TRUE, TRUE, FALSE, FALSE)),
  "rate concordance rule"
)

frame <- data.frame(
  analysis_id = sprintf("synthetic_%04d", seq_len(n)),
  age = runif(n, 20, 90),
  sex_female = rbinom(n, 1, 0.46),
  pf_ratio = runif(n, 70, 300),
  map = runif(n, 45, 115),
  vasopressor = rbinom(n, 1, 0.32),
  platelet = runif(n, 40, 480),
  creatinine = runif(n, 0.3, 5),
  smp = vent$smp,
  four_dprr = vent$four_dprr,
  driving_pressure = vent$driving_pressure,
  rr = raw$rr_per_min,
  static_power = vent$static_power,
  dynamic_power = vent$dynamic_power,
  resistive_power = vent$resistive_power
)
linear_predictor <- with(
  frame,
  -3.3 + 0.018 * (age - 55) - 0.0025 * (pf_ratio - 170) +
    0.35 * vasopressor + 0.012 * static_power +
    0.070 * dynamic_power + 0.020 * resistive_power
)
y <- rbinom(n, 1, plogis(linear_predictor))
assert_true(length(unique(y)) == 2L, "binary outcome generated")

bundle <- v2_derive_transform_bundle(frame)
spec <- v2_model_specification()
assert_true(
  identical(
    spec$model_id,
    c("M0", "M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY")
  ),
  "locked model order"
)

fits <- list()
designs <- list()
for (model_id in spec$model_id) {
  design <- v2_build_design(frame, model_id, bundle)
  fit <- v2_fit_logistic(
    design, y, model_id, row_ids = frame$analysis_id
  )
  probability <- predict(fit, design)
  performance <- v2_binary_performance(y, probability)
  assert_true(
    fit$converged && all(probability > 0 & probability < 1),
    paste0(model_id, " converged prediction")
  )
  assert_true(
    abs(performance[["calibration_in_the_large"]]) < 1e-6 &&
      abs(performance[["calibration_slope"]] - 1) < 1e-6,
    paste0(model_id, " apparent calibration")
  )
  designs[[model_id]] <- design
  fits[[model_id]] <- fit
}

lrt_four <- v2_constraint_lrt(fits$M_4DPRR, fits$M_DPRR)
lrt_energy <- v2_constraint_lrt(fits$M_MP, fits$M_ENERGY)
assert_true(
  lrt_four$df == 1L && lrt_four$chi_square >= 0 &&
    lrt_energy$df == 2L && lrt_energy$chi_square >= 0,
  "nested constraint tests"
)

pressure_rate_collinearity <- v2_increment_collinearity_audit(
  frame,
  c("driving_pressure", "rr"),
  audit_id = "synthetic_pressure_rate"
)
algebraic_term_collinearity <- v2_increment_collinearity_audit(
  frame,
  c("static_power", "dynamic_power", "resistive_power"),
  audit_id = "synthetic_algebraic_terms"
)
assert_true(
  all(is.finite(c(
    pressure_rate_collinearity$summary$condition_number,
    pressure_rate_collinearity$summary$maximum_vif,
    algebraic_term_collinearity$summary$condition_number,
    algebraic_term_collinearity$summary$maximum_vif
  ))) &&
    nrow(algebraic_term_collinearity$vif) == 3L,
  "increment collinearity audit"
)

nonlinear_expected_columns <- c(
  M_MP_NL = ncol(v2_build_baseline_design(frame, bundle)) + 3L,
  M_4DPRR_NL = ncol(v2_build_baseline_design(frame, bundle)) + 3L,
  M_DPRR_NL = ncol(v2_build_baseline_design(frame, bundle)) + 6L
)
for (model_id in names(nonlinear_expected_columns)) {
  design <- v2_build_nonlinear_design(frame, model_id, bundle)
  assert_true(
    ncol(design) == nonlinear_expected_columns[[model_id]],
    paste0(model_id, " symmetric spline design")
  )
}

comparison <- v2_paired_metric_difference(
  y,
  predict(fits$M_ENERGY, designs$M_ENERGY),
  predict(fits$M_MP, designs$M_MP)
)
assert_true(
  identical(
    names(comparison),
    c("delta_brier", "delta_log_loss", "delta_c_statistic")
  ) && all(is.finite(comparison)),
  "paired metric difference"
)

center <- rep(sprintf("hospital_%02d", 1:30), length.out = n)
set.seed(2026071602L)
indices <- v2_cluster_bootstrap_indices(center)
assert_true(
  length(indices) == n && all(indices >= 1L & indices <= n),
  "whole-hospital bootstrap indices"
)
equal_center <- v2_equal_center_performance(
  y,
  predict(fits$M_MP, designs$M_MP),
  center,
  minimum_center_n = 10L
)
assert_true(
  equal_center$summary[["eligible_centers"]] == 30 &&
    all(is.finite(equal_center$summary)),
  "equal-center performance"
)

interval <- v2_percentile_interval(rnorm(1000))
assert_true(
  length(interval) == 2L && interval[1L] < interval[2L],
  "bootstrap percentile interval"
)

cat("REBUILD_V2_ANALYSIS_UTILS_SYNTHETIC_PASS\n")
