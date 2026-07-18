#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/12_secondary_sensitivity_utils_selftest.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "08b_weighted_sensitivity_utils.R"))
source(file.path(script_dir, "12_secondary_sensitivity_utils.R"))

set.seed(2026071701L)
n_mimic <- 800L
n_eicu <- 260L
make_frame <- function(n, prefix) {
  data.frame(
    analysis_id = sprintf("%s_%04d", prefix, seq_len(n)),
    age = runif(n, 18, 95),
    sex_female = rbinom(n, 1, 0.45),
    pf_ratio = runif(n, 45, 300),
    map = runif(n, 35, 110),
    vasopressor = rbinom(n, 1, 0.35),
    platelet = runif(n, 30, 500),
    creatinine = runif(n, 0.3, 6),
    smp = runif(n, 4, 45),
    four_dprr = runif(n, 35, 140),
    driving_pressure = runif(n, 2, 28),
    rr = runif(n, 8, 40),
    static_power = runif(n, 1, 15),
    dynamic_power = runif(n, 1, 15),
    resistive_power = runif(n, 1, 15),
    stringsAsFactors = FALSE
  )
}
mimic <- make_frame(n_mimic, "m")
eicu <- make_frame(n_eicu, "e")
mimic$smp <- mimic$static_power + mimic$dynamic_power + mimic$resistive_power
eicu$smp <- eicu$static_power + eicu$dynamic_power + eicu$resistive_power
mimic$four_dprr <- 4 * mimic$driving_pressure + mimic$rr
eicu$four_dprr <- 4 * eicu$driving_pressure + eicu$rr
mimic$outcome <- rbinom(
  n_mimic, 1,
  plogis(-2 + 0.02 * mimic$age + 0.015 * mimic$driving_pressure)
)
eicu$outcome <- rbinom(
  n_eicu, 1,
  plogis(-2 + 0.02 * eicu$age + 0.015 * eicu$driving_pressure)
)
bundle <- v2_derive_transform_bundle(mimic)

designers <- list(
  M0 = function(x) v2_build_baseline_design(x, bundle),
  M_MP_NL = function(x) v2_build_nonlinear_design(x, "M_MP_NL", bundle),
  M_4DPRR_NL =
    function(x) v2_build_nonlinear_design(x, "M_4DPRR_NL", bundle),
  M_DPRR_NL =
    function(x) v2_build_nonlinear_design(x, "M_DPRR_NL", bundle),
  M_ENERGY_LINEAR_ANCHOR =
    function(x) v2_build_design(x, "M_ENERGY", bundle)
)
roles <- c(
  M0 = "baseline",
  M_MP_NL = "nonlinear sMP",
  M_4DPRR_NL = "nonlinear 4DPRR",
  M_DPRR_NL = "symmetric nonlinear DP and RR",
  M_ENERGY_LINEAR_ANCHOR = "linear energy anchor"
)
fit <- v2_ss_fit_apply(mimic, eicu, designers, roles)
stopifnot(
  identical(colnames(fit$mimic_predictions), names(designers)),
  identical(colnames(fit$eicu_predictions), names(designers)),
  all(fit$design_audit$converged),
  fit$design_audit$incremental_parameter_n[
    fit$design_audit$model_id == "M_MP_NL"
  ] == 3L,
  fit$design_audit$incremental_parameter_n[
    fit$design_audit$model_id == "M_DPRR_NL"
  ] == 6L
)

comparisons <- data.frame(
  candidate_model = c("M_MP_NL", "M_4DPRR_NL"),
  reference_model = c("M0", "M_MP_NL"),
  comparison_role = c("increment", "fair representation"),
  stringsAsFactors = FALSE
)
performance <- v2_ss_model_performance(
  eicu$outcome, fit$eicu_predictions, "eICU-CRD", "synthetic", roles
)
paired <- v2_ss_paired_differences(
  eicu$outcome, fit$eicu_predictions, comparisons,
  "eICU-CRD", "synthetic"
)
stopifnot(nrow(performance) > 0L, nrow(paired) == 6L)

weight_table <- data.frame(
  row_id = mimic$analysis_id,
  stabilized_weight_truncated = runif(n_mimic, 0.7, 1.4),
  model_role = "joint_always_observed_ipw",
  covariate_specification = "always_observed_only",
  permitted_for_outcome_weighting = TRUE,
  stringsAsFactors = FALSE
)
mimic_weighted <- v2_attach_frozen_selection_weights(
  mimic, weight_table, "analysis_id"
)
weighted_fit <- v2_ss_fit_apply(
  mimic_weighted, eicu, designers[1:2], roles[1:2],
  weighted = TRUE
)
stopifnot(all(vapply(weighted_fit$fits, function(x) {
  inherits(x, "ards_v2_weighted_logistic") && isTRUE(x$converged)
}, logical(1L))))

flags <- data.frame(
  analysis_id = mimic$analysis_id,
  preferred_source_primary_tuple = TRUE,
  rate_concordant = rep(c(TRUE, FALSE), length.out = n_mimic),
  rate_concordant_preferred_source =
    rep(c(TRUE, FALSE), length.out = n_mimic),
  selected_total_rr_reproduced = TRUE,
  stringsAsFactors = FALSE
)
attr(flags, "rebuild_metadata") <- list(
  outcome_blind = TRUE,
  tuple_reselection = FALSE
)
flagged <- v2_ss_attach_rate_quality(mimic, flags, "MIMIC-IV")
stopifnot(
  nrow(flagged) == nrow(mimic),
  sum(flagged$rate_concordant_preferred_source) == n_mimic / 2
)

message("REBUILD_V2_SECONDARY_SENSITIVITY_UTILS_SYNTHETIC_PASS")
