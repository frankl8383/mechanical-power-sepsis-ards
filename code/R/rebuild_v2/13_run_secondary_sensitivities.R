#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2: prespecified secondary/sensitivity analyses
#
# This driver produces real point estimates only. It runs no bootstrap and does
# not create manuscript-ready confidence intervals. The analysis deliberately
# stops a harmonized infection-restricted external validation because the
# locked source constructs differ (MIMIC antibiotic/culture vs eICU diagnosis).

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/13_run_secondary_sensitivities.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "08b_weighted_sensitivity_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "12_secondary_sensitivity_utils.R"))

stopifnot(identical(LOCKED_V2$version, "2.0.0"))
set.seed(LOCKED_V2$bootstrap$seed_sensitivity)

private_out <- file.path(PRIVATE_ROOT, "secondary_sensitivities")
aggregate_out <- file.path(AGGREGATE_ROOT, "secondary_sensitivities")
qc_out <- file.path(QC_ROOT, "secondary_sensitivities")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(
  qc_out, "phase5_secondary_sensitivities_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

read_gate <- function(path, label) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (identical(names(gate), c("field", "value"))) {
    if (anyDuplicated(gate$field)) stop("Duplicate field in ", label, ".")
    return(setNames(as.character(gate$value), gate$field))
  }
  if (nrow(gate) != 1L) stop("Malformed gate: ", label)
  setNames(
    vapply(gate, function(x) as.character(x[[1L]]), character(1L)),
    names(gate)
  )
}

require_gate <- function(gate, field, expected, label) {
  observed <- unname(gate[field])
  if (length(observed) != 1L || is.na(observed) ||
      !identical(observed, as.character(expected))) {
    stop(
      label, " mismatch for ", field, ": ",
      ifelse(length(observed) == 1L, observed, "<missing>"),
      " != ", as.character(expected)
    )
  }
  invisible(observed)
}

paths <- list(
  freeze_gate = file.path(
    QC_ROOT, "primary_model_freeze",
    "phase3_primary_model_freeze_complete_v2.csv"
  ),
  landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  rate_gate = file.path(
    QC_ROOT, "construct_quality",
    "primary_tuple_rate_quality_complete_v2.csv"
  ),
  selection_gate = file.path(
    QC_ROOT, "selection_weights", "selection_weights_complete_v2.csv"
  ),
  mimic_predictor = file.path(
    PRIVATE_ROOT, "model_ready",
    "mimic_primary_predictor_common_set_v2.rds"
  ),
  eicu_predictor = file.path(
    PRIVATE_ROOT, "model_ready",
    "eicu_primary_predictor_common_set_v2.rds"
  ),
  transform_bundle = file.path(
    PRIVATE_ROOT, "model_ready", "frozen_transform_bundle_v2.rds"
  ),
  mimic_outcome = file.path(
    PRIVATE_ROOT, "mimic", "mimic_fixed6h_landmark_outcomes_v2.rds"
  ),
  eicu_outcome = file.path(
    PRIVATE_ROOT, "eicu", "eicu_fixed6h_landmark_outcomes_v2.rds"
  ),
  mimic_exposure = file.path(
    PRIVATE_ROOT, "mimic",
    "mimic_paired_exposure_primary_60min_v2.rds"
  ),
  eicu_exposure = file.path(
    PRIVATE_ROOT, "eicu",
    "eicu_paired_exposure_primary_60min_v2.rds"
  ),
  mimic_rate = file.path(
    PRIVATE_ROOT, "construct_quality",
    "mimic_primary_tuple_rate_quality_flags_v2.rds"
  ),
  eicu_rate = file.path(
    PRIVATE_ROOT, "construct_quality",
    "eicu_primary_tuple_rate_quality_flags_v2.rds"
  ),
  mimic_weights = file.path(
    PRIVATE_ROOT, "selection_weights", "mimic_selection_weights_v2.rds"
  ),
  eicu_weights = file.path(
    PRIVATE_ROOT, "selection_weights", "eicu_selection_weights_v2.rds"
  ),
  mimic_broad_index = file.path(
    PRIVATE_ROOT, "mimic", "mimic_index_cohort_v2.rds"
  ),
  eicu_broad_index = file.path(
    PRIVATE_ROOT, "eicu", "eicu_index_cohort_v2.rds"
  ),
  mimic_infection = file.path(
    PRIVATE_ROOT, "mimic",
    "mimic_index_cohort_infection_sensitivity_v2.rds"
  ),
  eicu_infection = file.path(
    PRIVATE_ROOT, "eicu",
    "eicu_index_cohort_infection_sensitivity_v2.rds"
  )
)
missing_paths <- names(paths)[!vapply(paths, file.exists, logical(1L))]
if (length(missing_paths)) {
  stop("Missing secondary-sensitivity input(s): ", paste(
    missing_paths, collapse = ", "
  ))
}

freeze_gate <- read_gate(paths$freeze_gate, "primary predictor freeze gate")
require_gate(freeze_gate, "status", "PASS", "primary predictor freeze gate")
require_gate(
  freeze_gate, "locked_config_version", LOCKED_V2$version,
  "primary predictor freeze gate"
)
require_gate(
  freeze_gate, "mimic_common_set_sha256",
  v2_pm_sha256_file(paths$mimic_predictor),
  "primary predictor freeze gate"
)
require_gate(
  freeze_gate, "eicu_common_set_sha256",
  v2_pm_sha256_file(paths$eicu_predictor),
  "primary predictor freeze gate"
)
require_gate(
  freeze_gate, "transform_bundle_sha256",
  v2_pm_sha256_file(paths$transform_bundle),
  "primary predictor freeze gate"
)
require_gate(
  freeze_gate, "parameter_derivation_database", "MIMIC-IV only",
  "primary predictor freeze gate"
)

landmark_gate <- read_gate(paths$landmark_gate, "fixed landmark gate")
require_gate(
  landmark_gate, "mimic_outcome_sha256",
  v2_pm_sha256_file(paths$mimic_outcome), "fixed landmark gate"
)
require_gate(
  landmark_gate, "eicu_outcome_sha256",
  v2_pm_sha256_file(paths$eicu_outcome), "fixed landmark gate"
)

rate_gate <- read_gate(paths$rate_gate, "rate-quality gate")
for (pair in list(
  c("status", "PASS"),
  c("outcome_artifacts_opened", "FALSE"),
  c("tuple_reselection", "FALSE"),
  c("all_invariants_pass", "TRUE")
)) {
  require_gate(rate_gate, pair[[1L]], pair[[2L]], "rate-quality gate")
}
require_gate(
  rate_gate, "mimic_flags_sha256",
  v2_pm_sha256_file(paths$mimic_rate), "rate-quality gate"
)
require_gate(
  rate_gate, "eicu_flags_sha256",
  v2_pm_sha256_file(paths$eicu_rate), "rate-quality gate"
)

selection_gate <- read_gate(paths$selection_gate, "selection-weight gate")
for (pair in list(
  c("status", "PASS"),
  c("outcome_artifacts_opened", "FALSE"),
  c("all_checks_pass", "TRUE")
)) {
  require_gate(selection_gate, pair[[1L]], pair[[2L]], "selection-weight gate")
}
require_gate(
  selection_gate, "mimic_selection_weights_sha256",
  v2_pm_sha256_file(paths$mimic_weights), "selection-weight gate"
)
require_gate(
  selection_gate, "eicu_selection_weights_sha256",
  v2_pm_sha256_file(paths$eicu_weights), "selection-weight gate"
)

mimic_predictors <- as.data.frame(readRDS(paths$mimic_predictor))
eicu_predictors <- as.data.frame(readRDS(paths$eicu_predictor))
bundle <- readRDS(paths$transform_bundle)
if (!identical(
  attr(bundle, "freeze_metadata")$derivation_database,
  "MIMIC-IV only"
)) {
  stop("The transformation bundle is not MIMIC-derived and frozen.")
}
v2_pm_validate_predictor_frame(
  transform(mimic_predictors, core_complete = TRUE),
  "MIMIC-IV", require_complete = TRUE
)
v2_pm_validate_predictor_frame(
  transform(eicu_predictors, core_complete = TRUE),
  "eICU-CRD", require_complete = TRUE
)

# Infection construct audit is completed before opening outcomes. The source
# definitions are retained exactly as implemented; no common flag is invented.
mimic_broad_index <- readRDS(paths$mimic_broad_index)
eicu_broad_index <- readRDS(paths$eicu_broad_index)
mimic_infection <- readRDS(paths$mimic_infection)
eicu_infection <- readRDS(paths$eicu_infection)
mimic_infection_metadata <- attr(mimic_infection, "rebuild_metadata")
eicu_infection_metadata <- attr(eicu_infection, "rebuild_metadata")
if (!is.list(mimic_infection_metadata) ||
    !is.list(eicu_infection_metadata) ||
    !identical(mimic_infection_metadata$role, "clinical-context sensitivity only") ||
    !identical(eicu_infection_metadata$role, "clinical-context sensitivity only") ||
    !grepl(
      "not equivalent to MIMIC",
      eicu_infection_metadata$infection_definition,
      fixed = TRUE
    )) {
  stop("Locked infection metadata does not support the planned stop decision.")
}

infection_comparability <- data.frame(
  audit_dimension = c(
    "clinical_construct",
    "evidence_sources",
    "time_window",
    "information_availability",
    "person_or_stay_selection",
    "cross_database_harmonization",
    "endpoint_model_action"
  ),
  mimic_iv = c(
    "suspected infection",
    "paired antibiotic and microbiologic culture evidence",
    "suspected-infection onset from index-48 h through index",
    "paired evidence required available by index",
    "first infection-supported qualifying stay per subject",
    "not equivalent to eICU diagnosis ascertainment",
    "no harmonized infection-restricted endpoint model run"
  ),
  eicu_crd = c(
    "diagnosis-supported infection",
    "time-stamped diagnosis and admission diagnosis text",
    "infection diagnosis from index-48 h through index",
    "diagnosis evidence required available by index",
    "first infection-supported qualifying encounter per person",
    "not equivalent to MIMIC antibiotic/culture ascertainment",
    "no harmonized infection-restricted endpoint model run"
  ),
  comparable = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
  consequence = c(
    "constructs cannot be treated as one common infection phenotype",
    "source-specific misclassification mechanisms differ",
    "timing alone does not harmonize the clinical construct",
    "both avoid post-index leakage under their own source rules",
    "selection ordering is conceptually aligned but source-specific",
    "stop unified external validation rather than invent a definition",
    "report only source-specific descriptive coverage"
  ),
  stringsAsFactors = FALSE
)

infection_coverage <- rbind(
  data.frame(
    database = "MIMIC-IV",
    broad_index_n = nrow(mimic_broad_index),
    source_specific_infection_index_n = nrow(mimic_infection),
    primary_common_set_n = nrow(mimic_predictors),
    same_stay_or_encounter_overlap_n = sum(
      mimic_predictors$analysis_id %in% as.character(mimic_infection$stay_id)
    ),
    source_specific_definition =
      "antibiotic/culture suspected infection; index-48 h to index",
    harmonized_external_validation_permitted = FALSE,
    outcome_model_run = FALSE,
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    broad_index_n = nrow(eicu_broad_index),
    source_specific_infection_index_n = nrow(eicu_infection),
    primary_common_set_n = nrow(eicu_predictors),
    same_stay_or_encounter_overlap_n = sum(
      eicu_predictors$analysis_id %in%
        as.character(eicu_infection$patientunitstayid)
    ),
    source_specific_definition =
      "diagnosis/admissionDx-supported infection; index-48 h to index",
    harmonized_external_validation_permitted = FALSE,
    outcome_model_run = FALSE,
    stringsAsFactors = FALSE
  )
)
infection_coverage$same_stay_or_encounter_overlap_fraction <-
  infection_coverage$same_stay_or_encounter_overlap_n /
  infection_coverage$primary_common_set_n

# Outcomes are opened only after every outcome-free gate and the infection stop
# decision have passed.
mimic_analysis <- v2_pm_join_outcome(
  mimic_predictors, as.data.frame(readRDS(paths$mimic_outcome)), "MIMIC-IV"
)
eicu_analysis <- v2_pm_join_outcome(
  eicu_predictors, as.data.frame(readRDS(paths$eicu_outcome)), "eICU-CRD"
)

primary_roles <- c(
  M0 = "no-GCS baseline",
  M_MP = "linear sMP",
  M_4DPRR = "linear 4DPRR",
  M_DPRR = "linear free driving-pressure and respiratory-rate weights",
  M_ENERGY = "linear free algebraic-term weights"
)
primary_designers <- list(
  M0 = function(x) v2_build_design(x, "M0", bundle),
  M_MP = function(x) v2_build_design(x, "M_MP", bundle),
  M_4DPRR = function(x) v2_build_design(x, "M_4DPRR", bundle),
  M_DPRR = function(x) v2_build_design(x, "M_DPRR", bundle),
  M_ENERGY = function(x) v2_build_design(x, "M_ENERGY", bundle)
)
primary_comparisons <- data.frame(
  candidate_model = c(
    "M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY",
    "M_4DPRR", "M_DPRR", "M_ENERGY"
  ),
  reference_model = c(
    "M0", "M0", "M0", "M0", "M_MP", "M_MP", "M_MP"
  ),
  comparison_role = c(
    rep("increment_over_baseline", 4L),
    rep("representation_comparison_to_smp", 3L)
  ),
  stringsAsFactors = FALSE
)

message("Running frozen-knot nonlinear fairness point estimates ...")
nonlinear_roles <- c(
  M0 = "no-GCS baseline",
  M_MP_NL = "sMP with frozen four-knot spline",
  M_4DPRR_NL = "4DPRR with frozen four-knot spline",
  M_DPRR_NL = "driving pressure and RR with symmetric frozen splines",
  M_ENERGY_LINEAR_ANCHOR =
    "prespecified linear algebraic-energy anchor; not a nonlinear expansion"
)
nonlinear_designers <- list(
  M0 = function(x) v2_build_design(x, "M0", bundle),
  M_MP_NL =
    function(x) v2_build_nonlinear_design(x, "M_MP_NL", bundle),
  M_4DPRR_NL =
    function(x) v2_build_nonlinear_design(x, "M_4DPRR_NL", bundle),
  M_DPRR_NL =
    function(x) v2_build_nonlinear_design(x, "M_DPRR_NL", bundle),
  M_ENERGY_LINEAR_ANCHOR =
    function(x) v2_build_design(x, "M_ENERGY", bundle)
)
nonlinear_fit <- v2_ss_fit_apply(
  mimic_analysis, eicu_analysis, nonlinear_designers, nonlinear_roles
)
nonlinear_comparisons <- data.frame(
  candidate_model = c(
    "M_MP_NL", "M_4DPRR_NL", "M_DPRR_NL",
    "M_ENERGY_LINEAR_ANCHOR",
    "M_4DPRR_NL", "M_DPRR_NL", "M_ENERGY_LINEAR_ANCHOR"
  ),
  reference_model = c(
    "M0", "M0", "M0", "M0",
    "M_MP_NL", "M_MP_NL", "M_MP_NL"
  ),
  comparison_role = c(
    "increment_over_baseline",
    "increment_over_baseline",
    "increment_over_baseline",
    "linear_energy_anchor_increment",
    "equal_flexibility_one_index_comparison",
    "symmetric_component_flexibility_comparison",
    "linear_energy_anchor_not_nonlinear_fairness"
  ),
  stringsAsFactors = FALSE
)
nonlinear_performance <- rbind(
  v2_ss_model_performance(
    mimic_analysis$outcome, nonlinear_fit$mimic_predictions,
    "MIMIC-IV", "nonlinear_fairness_apparent", nonlinear_roles
  ),
  v2_ss_model_performance(
    eicu_analysis$outcome, nonlinear_fit$eicu_predictions,
    "eICU-CRD", "nonlinear_fairness_external", nonlinear_roles
  )
)
nonlinear_paired <- rbind(
  v2_ss_paired_differences(
    mimic_analysis$outcome, nonlinear_fit$mimic_predictions,
    nonlinear_comparisons, "MIMIC-IV", "nonlinear_fairness_apparent"
  ),
  v2_ss_paired_differences(
    eicu_analysis$outcome, nonlinear_fit$eicu_predictions,
    nonlinear_comparisons, "eICU-CRD", "nonlinear_fairness_external"
  )
)
nonlinear_specification <- merge(
  nonlinear_fit$design_audit,
  data.frame(
    model_id = names(nonlinear_roles),
    nonlinear_fairness_tier = c(
      "baseline", "prespecified_nonlinear", "prespecified_nonlinear",
      "prespecified_nonlinear", "linear_anchor_only"
    ),
    knots_derived_in = c(
      "MIMIC-IV", "MIMIC-IV", "MIMIC-IV", "MIMIC-IV", "not applicable"
    ),
    applied_to_eicu_unchanged = TRUE,
    stringsAsFactors = FALSE
  ),
  by = "model_id", all.x = TRUE, sort = FALSE
)

message("Running compliance-normalized sMP point estimates ...")
mimic_compliance <- v2_ss_attach_compliance_normalization(
  mimic_analysis, readRDS(paths$mimic_exposure), "MIMIC-IV"
)
eicu_compliance <- v2_ss_attach_compliance_normalization(
  eicu_analysis, readRDS(paths$eicu_exposure), "eICU-CRD"
)
scaled_compliance <- v2_ss_scale_compliance_normalization(
  mimic_compliance$frame, eicu_compliance$frame
)
compliance_roles <- c(
  M0 = "no-GCS baseline on positive-driving-pressure common set",
  M_MP = "absolute sMP on the same positive-driving-pressure patients",
  M_CN_SMP =
    "compliance-normalized sMP, MIMIC-IQR scaled and externally frozen"
)
compliance_designers <- list(
  M0 = function(x) v2_build_design(x, "M0", bundle),
  M_MP = function(x) v2_build_design(x, "M_MP", bundle),
  M_CN_SMP = function(x) cbind(
    v2_build_design(x, "M0", bundle),
    v2_named_column(
      x$compliance_normalized_smp_scaled,
      "compliance_normalized_smp_scaled"
    )
  )
)
compliance_fit <- v2_ss_fit_apply(
  scaled_compliance$mimic, scaled_compliance$eicu,
  compliance_designers, compliance_roles
)
compliance_comparisons <- data.frame(
  candidate_model = c("M_MP", "M_CN_SMP", "M_CN_SMP"),
  reference_model = c("M0", "M0", "M_MP"),
  comparison_role = c(
    "absolute_smp_increment",
    "compliance_normalized_increment",
    "same_patient_normalization_comparison"
  ),
  stringsAsFactors = FALSE
)
compliance_performance <- rbind(
  v2_ss_model_performance(
    scaled_compliance$mimic$outcome,
    compliance_fit$mimic_predictions,
    "MIMIC-IV", "compliance_normalization_apparent", compliance_roles
  ),
  v2_ss_model_performance(
    scaled_compliance$eicu$outcome,
    compliance_fit$eicu_predictions,
    "eICU-CRD", "compliance_normalization_external", compliance_roles
  )
)
compliance_paired <- rbind(
  v2_ss_paired_differences(
    scaled_compliance$mimic$outcome,
    compliance_fit$mimic_predictions,
    compliance_comparisons, "MIMIC-IV",
    "compliance_normalization_apparent"
  ),
  v2_ss_paired_differences(
    scaled_compliance$eicu$outcome,
    compliance_fit$eicu_predictions,
    compliance_comparisons, "eICU-CRD",
    "compliance_normalization_external"
  )
)
compliance_distribution <- rbind(
  v2_ss_distribution_summary(
    scaled_compliance$mimic$compliance_L_per_cmH2O,
    "MIMIC-IV", "compliance_L_per_cmH2O", "compliance_normalization"
  ),
  v2_ss_distribution_summary(
    scaled_compliance$eicu$compliance_L_per_cmH2O,
    "eICU-CRD", "compliance_L_per_cmH2O", "compliance_normalization"
  ),
  v2_ss_distribution_summary(
    scaled_compliance$mimic$compliance_normalized_smp_raw,
    "MIMIC-IV", "compliance_normalized_smp_raw", "compliance_normalization"
  ),
  v2_ss_distribution_summary(
    scaled_compliance$eicu$compliance_normalized_smp_raw,
    "eICU-CRD", "compliance_normalized_smp_raw", "compliance_normalization"
  )
)

message("Running rate-concordant plus preferred-source restriction ...")
mimic_rate_frame <- v2_ss_attach_rate_quality(
  mimic_analysis, readRDS(paths$mimic_rate), "MIMIC-IV"
)
eicu_rate_frame <- v2_ss_attach_rate_quality(
  eicu_analysis, readRDS(paths$eicu_rate), "eICU-CRD"
)
mimic_rate_subset <- mimic_rate_frame[
  mimic_rate_frame$rate_concordant_preferred_source, , drop = FALSE
]
eicu_rate_subset <- eicu_rate_frame[
  eicu_rate_frame$rate_concordant_preferred_source, , drop = FALSE
]
if (nrow(mimic_rate_subset) < 100L || nrow(eicu_rate_subset) < 100L ||
    length(unique(eicu_rate_subset$hospital_id)) < 2L) {
  stop("Rate-concordant/preferred restriction has inadequate support.")
}
rate_fit <- v2_ss_fit_apply(
  mimic_rate_subset, eicu_rate_subset,
  primary_designers, primary_roles
)
rate_performance <- rbind(
  v2_ss_model_performance(
    mimic_rate_subset$outcome, rate_fit$mimic_predictions,
    "MIMIC-IV", "rate_concordant_preferred_apparent", primary_roles
  ),
  v2_ss_model_performance(
    eicu_rate_subset$outcome, rate_fit$eicu_predictions,
    "eICU-CRD", "rate_concordant_preferred_external", primary_roles
  )
)
rate_paired <- rbind(
  v2_ss_paired_differences(
    mimic_rate_subset$outcome, rate_fit$mimic_predictions,
    primary_comparisons, "MIMIC-IV",
    "rate_concordant_preferred_apparent"
  ),
  v2_ss_paired_differences(
    eicu_rate_subset$outcome, rate_fit$eicu_predictions,
    primary_comparisons, "eICU-CRD",
    "rate_concordant_preferred_external"
  )
)
rate_sample <- rbind(
  data.frame(
    database = "MIMIC-IV",
    input_primary_common_set_n = nrow(mimic_analysis),
    restricted_n = nrow(mimic_rate_subset),
    events = sum(mimic_rate_subset$outcome),
    event_rate = mean(mimic_rate_subset$outcome),
    hospitals = length(unique(mimic_rate_subset$hospital_id)),
    primary_tuple_reselected = FALSE,
    all_restricted_rows_rate_concordant = all(
      mimic_rate_subset$rate_concordant
    ),
    all_restricted_rows_preferred_source = all(
      mimic_rate_subset$preferred_source_primary_tuple
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    input_primary_common_set_n = nrow(eicu_analysis),
    restricted_n = nrow(eicu_rate_subset),
    events = sum(eicu_rate_subset$outcome),
    event_rate = mean(eicu_rate_subset$outcome),
    hospitals = length(unique(eicu_rate_subset$hospital_id)),
    primary_tuple_reselected = FALSE,
    all_restricted_rows_rate_concordant = all(
      eicu_rate_subset$rate_concordant
    ),
    all_restricted_rows_preferred_source = all(
      eicu_rate_subset$preferred_source_primary_tuple
    ),
    stringsAsFactors = FALSE
  )
)

message("Running permitted joint always-observed IPW endpoint sensitivity ...")
mimic_selection <- v2_ss_extract_joint_ipw_table(
  readRDS(paths$mimic_weights), "MIMIC-IV"
)
eicu_selection <- v2_ss_extract_joint_ipw_table(
  readRDS(paths$eicu_weights), "eICU-CRD"
)
mimic_weighted <- v2_attach_frozen_selection_weights(
  mimic_analysis,
  mimic_selection$table,
  id_column = "analysis_id",
  weight_id_column = "row_id",
  output_column = "selection_weight"
)
eicu_weighted <- v2_attach_frozen_selection_weights(
  eicu_analysis,
  eicu_selection$table,
  id_column = "analysis_id",
  weight_id_column = "row_id",
  output_column = "selection_weight"
)
if (nrow(mimic_weighted) != nrow(mimic_analysis) ||
    nrow(eicu_weighted) != nrow(eicu_analysis)) {
  stop("Eligible joint IPW tables do not exactly cover the primary common set.")
}
weighted_fit <- v2_ss_fit_apply(
  mimic_weighted, eicu_weighted,
  primary_designers, primary_roles,
  weighted = TRUE, mimic_weight_column = "selection_weight"
)
weighted_performance <- rbind(
  v2_ss_model_performance(
    mimic_weighted$outcome, weighted_fit$mimic_predictions,
    "MIMIC-IV", "joint_always_observed_ipw_weighted_fit",
    primary_roles, mimic_weighted$selection_weight
  ),
  v2_ss_model_performance(
    eicu_weighted$outcome, weighted_fit$eicu_predictions,
    "eICU-CRD", "joint_always_observed_ipw_external",
    primary_roles, eicu_weighted$selection_weight
  )
)
weighted_paired <- rbind(
  v2_ss_paired_differences(
    mimic_weighted$outcome, weighted_fit$mimic_predictions,
    primary_comparisons, "MIMIC-IV",
    "joint_always_observed_ipw_weighted_fit",
    mimic_weighted$selection_weight
  ),
  v2_ss_paired_differences(
    eicu_weighted$outcome, weighted_fit$eicu_predictions,
    primary_comparisons, "eICU-CRD",
    "joint_always_observed_ipw_external",
    eicu_weighted$selection_weight
  )
)
weight_provenance <- rbind(
  data.frame(
    database = "MIMIC-IV",
    model_role = mimic_selection$model$model_role,
    covariate_specification =
      mimic_selection$model$covariate_specification,
    selection_target = mimic_selection$model$selection_target,
    permitted_for_outcome_weighting = all(
      mimic_selection$table$permitted_for_outcome_weighting
    ),
    n = nrow(mimic_weighted),
    events = sum(mimic_weighted$outcome),
    effective_sample_size =
      sum(mimic_weighted$selection_weight)^2 /
      sum(mimic_weighted$selection_weight^2),
    minimum_weight = min(mimic_weighted$selection_weight),
    maximum_weight = max(mimic_weighted$selection_weight),
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    model_role = eicu_selection$model$model_role,
    covariate_specification =
      eicu_selection$model$covariate_specification,
    selection_target = eicu_selection$model$selection_target,
    permitted_for_outcome_weighting = all(
      eicu_selection$table$permitted_for_outcome_weighting
    ),
    n = nrow(eicu_weighted),
    events = sum(eicu_weighted$outcome),
    effective_sample_size =
      sum(eicu_weighted$selection_weight)^2 /
      sum(eicu_weighted$selection_weight^2),
    minimum_weight = min(eicu_weighted$selection_weight),
    maximum_weight = max(eicu_weighted$selection_weight),
    stringsAsFactors = FALSE
  )
)

sample_summary <- rbind(
  data.frame(
    analysis = "nonlinear_fairness",
    database = "MIMIC-IV",
    n = nrow(mimic_analysis),
    events = sum(mimic_analysis$outcome),
    hospitals = length(unique(mimic_analysis$hospital_id)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    analysis = "nonlinear_fairness",
    database = "eICU-CRD",
    n = nrow(eicu_analysis),
    events = sum(eicu_analysis$outcome),
    hospitals = length(unique(eicu_analysis$hospital_id)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    analysis = "compliance_normalized_smp",
    database = "MIMIC-IV",
    n = nrow(scaled_compliance$mimic),
    events = sum(scaled_compliance$mimic$outcome),
    hospitals = length(unique(scaled_compliance$mimic$hospital_id)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    analysis = "compliance_normalized_smp",
    database = "eICU-CRD",
    n = nrow(scaled_compliance$eicu),
    events = sum(scaled_compliance$eicu$outcome),
    hospitals = length(unique(scaled_compliance$eicu$hospital_id)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    analysis = "rate_concordant_preferred",
    database = "MIMIC-IV",
    n = nrow(mimic_rate_subset),
    events = sum(mimic_rate_subset$outcome),
    hospitals = length(unique(mimic_rate_subset$hospital_id)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    analysis = "rate_concordant_preferred",
    database = "eICU-CRD",
    n = nrow(eicu_rate_subset),
    events = sum(eicu_rate_subset$outcome),
    hospitals = length(unique(eicu_rate_subset$hospital_id)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    analysis = "joint_always_observed_ipw",
    database = "MIMIC-IV",
    n = nrow(mimic_weighted),
    events = sum(mimic_weighted$outcome),
    hospitals = length(unique(mimic_weighted$hospital_id)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    analysis = "joint_always_observed_ipw",
    database = "eICU-CRD",
    n = nrow(eicu_weighted),
    events = sum(eicu_weighted$outcome),
    hospitals = length(unique(eicu_weighted$hospital_id)),
    stringsAsFactors = FALSE
  )
)

private_result <- list(
  version = "secondary_sensitivity_point_estimates_v2",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED_V2$version,
  bootstrap_replicates = 0L,
  manuscript_ci_ready = FALSE,
  nonlinear = list(
    fits = nonlinear_fit$fits,
    mimic_analysis_id = mimic_analysis$analysis_id,
    eicu_analysis_id = eicu_analysis$analysis_id,
    mimic_predictions = nonlinear_fit$mimic_predictions,
    eicu_predictions = nonlinear_fit$eicu_predictions,
    energy_nonlinear_model_added = FALSE
  ),
  compliance_normalized = list(
    fits = compliance_fit$fits,
    transform = scaled_compliance$parameters,
    mimic_analysis_id = scaled_compliance$mimic$analysis_id,
    eicu_analysis_id = scaled_compliance$eicu$analysis_id,
    mimic_predictions = compliance_fit$mimic_predictions,
    eicu_predictions = compliance_fit$eicu_predictions
  ),
  rate_concordant_preferred = list(
    fits = rate_fit$fits,
    mimic_analysis_id = mimic_rate_subset$analysis_id,
    eicu_analysis_id = eicu_rate_subset$analysis_id,
    mimic_predictions = rate_fit$mimic_predictions,
    eicu_predictions = rate_fit$eicu_predictions,
    primary_tuple_reselected = FALSE
  ),
  selection_weighted = list(
    fits = weighted_fit$fits,
    mimic_analysis_id = mimic_weighted$analysis_id,
    eicu_analysis_id = eicu_weighted$analysis_id,
    mimic_predictions = weighted_fit$mimic_predictions,
    eicu_predictions = weighted_fit$eicu_predictions,
    mimic_weight = mimic_weighted$selection_weight,
    eicu_weight = eicu_weighted$selection_weight,
    permitted_model_role = "joint_always_observed_ipw"
  ),
  infection = list(
    harmonized_external_validation_permitted = FALSE,
    endpoint_model_run = FALSE,
    reason = paste(
      "MIMIC antibiotic/culture suspected infection is not equivalent to",
      "eICU diagnosis-supported infection."
    )
  ),
  input_hashes = lapply(paths, v2_pm_sha256_file)
)
private_path <- file.path(
  private_out, "secondary_sensitivity_point_estimates_v2.rds"
)
private_hash <- v2_pm_atomic_save_rds(private_result, private_path)

aggregate_outputs <- list(
  "secondary_analysis_sample_summary_v2.csv" = sample_summary,
  "nonlinear_model_specification_v2.csv" = nonlinear_specification,
  "nonlinear_point_performance_v2.csv" = nonlinear_performance,
  "nonlinear_paired_differences_v2.csv" = nonlinear_paired,
  "compliance_normalization_sample_qc_v2.csv" = rbind(
    mimic_compliance$qc, eicu_compliance$qc
  ),
  "compliance_normalization_transform_v2.csv" =
    scaled_compliance$parameters,
  "compliance_normalization_distributions_v2.csv" =
    compliance_distribution,
  "compliance_normalization_point_performance_v2.csv" =
    compliance_performance,
  "compliance_normalization_paired_differences_v2.csv" =
    compliance_paired,
  "rate_concordant_preferred_sample_v2.csv" = rate_sample,
  "rate_concordant_preferred_point_performance_v2.csv" =
    rate_performance,
  "rate_concordant_preferred_paired_differences_v2.csv" =
    rate_paired,
  "infection_definition_comparability_audit_v2.csv" =
    infection_comparability,
  "infection_source_specific_coverage_v2.csv" = infection_coverage,
  "selection_weight_provenance_v2.csv" = weight_provenance,
  "selection_weighted_point_performance_v2.csv" =
    weighted_performance,
  "selection_weighted_paired_differences_v2.csv" =
    weighted_paired
)
for (name in names(aggregate_outputs)) {
  v2_pm_atomic_write_csv(
    aggregate_outputs[[name]], file.path(aggregate_out, name)
  )
}

infection_stop <- data.frame(
  check = c(
    "mimic_definition_locked",
    "eicu_definition_locked",
    "same_timing_window",
    "clinical_construct_equivalent",
    "harmonized_external_validation_permitted",
    "infection_endpoint_model_run",
    "invented_common_definition"
  ),
  value = c(
    TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE
  ),
  detail = c(
    "antibiotic/culture suspected infection",
    "diagnosis/admissionDx-supported infection",
    "both index-48 h through index",
    "FALSE: evidence sources and misclassification mechanisms differ",
    "FALSE: SAP clinical-context sensitivity boundary enforced",
    "FALSE: stopped before any infection-restricted outcome fit",
    "FALSE: no post hoc harmonization"
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  infection_stop,
  file.path(qc_out, "infection_harmonization_stop_v2.csv")
)

invariants <- data.frame(
  check = c(
    "frozen_mimic_transform_bundle_applied_to_eicu",
    "nonlinear_smp_and_4dprr_have_equal_incremental_df",
    "nonlinear_dp_and_rr_have_symmetric_spline_df",
    "nonlinear_energy_expansion_not_added",
    "linear_energy_retained_as_anchor_only",
    "compliance_normalization_same_patient_comparison",
    "compliance_normalization_transform_mimic_derived",
    "rate_restriction_reused_primary_tuple_without_reselection",
    "rate_restriction_all_rows_concordant_and_preferred",
    "infection_definitions_not_claimed_harmonized",
    "infection_restricted_endpoint_model_not_run",
    "selection_weight_model_role_joint_always_observed_only",
    "all_endpoint_weights_explicitly_permitted",
    "diagnostic_joint_weight_not_used",
    "bootstrap_replicates_zero",
    "all_point_estimate_fits_converged"
  ),
  pass = c(
    identical(
      attr(bundle, "freeze_metadata")$derivation_database,
      "MIMIC-IV only"
    ),
    nonlinear_fit$design_audit$incremental_parameter_n[
      nonlinear_fit$design_audit$model_id == "M_MP_NL"
    ] ==
      nonlinear_fit$design_audit$incremental_parameter_n[
        nonlinear_fit$design_audit$model_id == "M_4DPRR_NL"
      ],
    nonlinear_fit$design_audit$incremental_parameter_n[
      nonlinear_fit$design_audit$model_id == "M_DPRR_NL"
    ] == 2L * nonlinear_fit$design_audit$incremental_parameter_n[
      nonlinear_fit$design_audit$model_id == "M_MP_NL"
    ],
    !"M_ENERGY_NL" %in% names(nonlinear_fit$fits),
    "M_ENERGY_LINEAR_ANCHOR" %in% names(nonlinear_fit$fits),
    all(c("M_MP", "M_CN_SMP") %in% names(compliance_fit$fits)),
    scaled_compliance$parameters$derivation_database == "MIMIC-IV only",
    all(!rate_sample$primary_tuple_reselected),
    all(
      rate_sample$all_restricted_rows_rate_concordant &
        rate_sample$all_restricted_rows_preferred_source
    ),
    all(!infection_coverage$harmonized_external_validation_permitted),
    all(!infection_coverage$outcome_model_run),
    all(weight_provenance$model_role == "joint_always_observed_ipw") &&
      all(
        weight_provenance$covariate_specification ==
          "always_observed_only"
      ),
    all(weight_provenance$permitted_for_outcome_weighting),
    !any(grepl(
      "diagnostic",
      weight_provenance$model_role,
      ignore.case = TRUE
    )),
    TRUE,
    all(c(
      nonlinear_fit$design_audit$converged,
      compliance_fit$design_audit$converged,
      rate_fit$design_audit$converged,
      weighted_fit$design_audit$converged
    ))
  ),
  stringsAsFactors = FALSE
)
if (!all(invariants$pass)) {
  stop(
    "Secondary-sensitivity invariant failure: ",
    paste(invariants$check[!invariants$pass], collapse = ", ")
  )
}
v2_pm_atomic_write_csv(
  invariants,
  file.path(qc_out, "secondary_sensitivity_invariants_v2.csv")
)

input_manifest <- data.frame(
  artifact = names(paths),
  path = unname(unlist(paths, use.names = FALSE)),
  sha256 = vapply(paths, v2_pm_sha256_file, character(1L)),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  input_manifest,
  file.path(qc_out, "secondary_sensitivity_input_manifest_v2.csv")
)

aggregate_manifest <- data.frame(
  artifact = names(aggregate_outputs),
  sha256 = vapply(
    names(aggregate_outputs),
    function(name) v2_pm_sha256_file(file.path(aggregate_out, name)),
    character(1L)
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  aggregate_manifest,
  file.path(qc_out, "secondary_sensitivity_aggregate_manifest_v2.csv")
)

gate <- data.frame(
  field = c(
    "status",
    "locked_config_version",
    "script_sha256",
    "utils_sha256",
    "private_result_sha256",
    "bootstrap_replicates",
    "manuscript_ci_ready",
    "nonlinear_models",
    "nonlinear_energy_model_added",
    "energy_anchor_role",
    "mimic_nonlinear_n",
    "eicu_nonlinear_n",
    "mimic_compliance_n",
    "eicu_compliance_n",
    "mimic_rate_preferred_n",
    "eicu_rate_preferred_n",
    "eicu_rate_preferred_hospitals",
    "primary_tuple_reselection",
    "infection_constructs_equivalent",
    "harmonized_infection_external_validation_run",
    "infection_outcome_model_run",
    "selection_weight_model_role",
    "selection_weight_covariate_specification",
    "all_endpoint_weights_permitted",
    "all_invariants_pass",
    "completed_at"
  ),
  value = c(
    "PASS",
    LOCKED_V2$version,
    v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(
      file.path(script_dir, "12_secondary_sensitivity_utils.R")
    ),
    private_hash,
    "0",
    "FALSE",
    paste(names(nonlinear_fit$fits), collapse = ";"),
    "FALSE",
    "M_ENERGY_LINEAR_ANCHOR_ONLY",
    as.character(nrow(mimic_analysis)),
    as.character(nrow(eicu_analysis)),
    as.character(nrow(scaled_compliance$mimic)),
    as.character(nrow(scaled_compliance$eicu)),
    as.character(nrow(mimic_rate_subset)),
    as.character(nrow(eicu_rate_subset)),
    as.character(length(unique(eicu_rate_subset$hospital_id))),
    "FALSE",
    "FALSE",
    "FALSE",
    "FALSE",
    "joint_always_observed_ipw",
    "always_observed_only",
    "TRUE",
    "TRUE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

message("REBUILD_V2_SECONDARY_SENSITIVITY_POINT_ESTIMATES_PASS")
message(
  "  Nonlinear common sets: MIMIC ", nrow(mimic_analysis),
  "; eICU ", nrow(eicu_analysis)
)
message(
  "  Compliance-positive: MIMIC ", nrow(scaled_compliance$mimic),
  "; eICU ", nrow(scaled_compliance$eicu)
)
message(
  "  Rate-concordant preferred: MIMIC ", nrow(mimic_rate_subset),
  "; eICU ", nrow(eicu_rate_subset), " across ",
  length(unique(eicu_rate_subset$hospital_id)), " hospitals"
)
message(
  "  Infection external model stopped: source constructs are not equivalent"
)
