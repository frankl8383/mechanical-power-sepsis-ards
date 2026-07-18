#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: locked complete-case model fitting,
# internal bootstrap validation, and locked external validation.
#
# GOVERNANCE WARNING
# ------------------
# This script is outcome-bearing. It may be executed only after the formal
# authorization checkpoint and the Phase 3a outcome-extraction PASS gate exist.
# Syntax checking must use parse(file=...). Synthetic tests must evaluate only
# selected function definitions; sourcing this complete script is prohibited.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/10_fit_locked_models.R", mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
project_from_script <- normalizePath(
  file.path(script_dir, "..", "..", ".."), mustWork = TRUE
)
expected_config_version <- "1.0.1"

sha256_file <- function(path) {
  out <- system2(
    "shasum", c("-a", "256", shQuote(path)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA256 failed for ", path, ": ", paste(out, collapse = " "))
  }
  hash <- strsplit(out[[1L]], "[[:space:]]+")[[1L]][1L]
  if (!grepl("^[0-9a-f]{64}$", hash)) stop("Invalid SHA256 for ", path)
  hash
}

read_field_value_gate <- function(path, label) {
  z <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(z), c("field", "value")) ||
      anyDuplicated(z$field) || anyNA(z$field) || any(!nzchar(z$field))) {
    stop("Malformed field/value ", label, ": ", path)
  }
  setNames(as.character(z$value), z$field)
}

read_completion_gate <- function(path, label) {
  z <- fread(path, colClasses = "character", showProgress = FALSE)
  if (identical(names(z), c("field", "value"))) {
    if (anyDuplicated(z$field) || anyNA(z$field) || any(!nzchar(z$field))) {
      stop("Malformed field/value ", label, ": ", path)
    }
    return(setNames(as.character(z$value), z$field))
  }
  if (nrow(z) != 1L || anyDuplicated(names(z))) {
    stop("Malformed one-row ", label, ": ", path)
  }
  setNames(vapply(z, function(v) as.character(v[[1L]]), character(1)), names(z))
}

require_map_value <- function(x, field, expected = NULL, label = "gate") {
  value <- unname(x[field])
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(label, " lacks a non-empty field: ", field)
  }
  if (!is.null(expected) && !identical(value, as.character(expected))) {
    stop(label, " mismatch for ", field, ": ", value, " != ", expected)
  }
  value
}

resolve_project_path <- function(path, label, require_relative = FALSE) {
  if (is.na(path) || !nzchar(path)) stop(label, " is empty.")
  absolute <- grepl("^/", path)
  if (require_relative && absolute) stop(label, " must be project-relative.")
  if (!absolute && grepl("(^|/)\\.\\.(/|$)", path)) {
    stop(label, " contains path traversal.")
  }
  candidate <- if (absolute) path else file.path(project_from_script, path)
  if (!file.exists(candidate)) stop("Missing ", label, ": ", candidate)
  resolved <- normalizePath(candidate, mustWork = TRUE)
  prefix <- paste0(project_from_script, .Platform$file.sep)
  if (!startsWith(resolved, prefix)) stop(label, " escapes the project root.")
  resolved
}

# The authorization checkpoint is intentionally the first project artifact
# opened. No config, parameter frame, outcome gate, or outcome RDS is inspected
# before these checks pass.
checkpoint_path <- file.path(
  project_from_script, "analysis_rebuild_v1", "qc", "unblinding",
  "outcome_unblinding_checkpoint_v1.csv"
)
if (!file.exists(checkpoint_path)) {
  stop(
    "Locked modeling is not authorized: missing checkpoint ", checkpoint_path,
    ". No outcome artifact was opened."
  )
}
checkpoint <- read_field_value_gate(checkpoint_path, "authorization checkpoint")
require_map_value(checkpoint, "status", "AUTHORIZED", "authorization checkpoint")
require_map_value(
  checkpoint, "config_version", expected_config_version,
  "authorization checkpoint"
)
for (field in c("authorized_at", "authorized_by", "authorization_basis")) {
  require_map_value(checkpoint, field, label = "authorization checkpoint")
}

config_path <- file.path(script_dir, "00_config.R")
outcome_script_path <- file.path(script_dir, "09_extract_rebuilt_outcomes.R")
parameter_script_path <- file.path(script_dir, "07_freeze_predictor_parameters.R")
model_utils_path <- file.path(script_dir, "08_model_utils.R")
analysis_utils_path <- file.path(script_dir, "08a_locked_analysis_utils.R")
decision_log_path <- file.path(
  project_from_script, "docs", "rebuild_v1", "analysis_decision_log.md"
)
locked_scripts <- c(
  config_script_sha256 = config_path,
  outcome_extraction_script_sha256 = outcome_script_path,
  parameter_freeze_script_sha256 = parameter_script_path,
  model_utils_script_sha256 = model_utils_path,
  model_analysis_utils_script_sha256 = analysis_utils_path,
  model_analysis_script_sha256 = script_path,
  analysis_decision_log_sha256 = decision_log_path
)
if (any(!file.exists(locked_scripts))) {
  stop(
    "Authorized locked file is missing: ",
    paste(locked_scripts[!file.exists(locked_scripts)], collapse = ", ")
  )
}
for (field in names(locked_scripts)) {
  require_map_value(
    checkpoint, field, sha256_file(locked_scripts[[field]]),
    "authorization checkpoint"
  )
}

source(config_path)
if (!identical(LOCKED$version, expected_config_version) ||
    !identical(normalizePath(PROJECT_ROOT, mustWork = TRUE), project_from_script)) {
  stop("Loaded config differs from the authorized project/configuration.")
}

analysis_manifest_relative <- require_map_value(
  checkpoint, "analysis_script_manifest_path",
  label = "authorization checkpoint"
)
analysis_manifest_path <- resolve_project_path(
  analysis_manifest_relative, "analysis script manifest", require_relative = TRUE
)
require_map_value(
  checkpoint, "analysis_script_manifest_sha256",
  sha256_file(analysis_manifest_path), "authorization checkpoint"
)
analysis_manifest <- fread(analysis_manifest_path, showProgress = FALSE)
if (!all(c("file", "sha256") %in% names(analysis_manifest)) ||
    !nrow(analysis_manifest) || anyDuplicated(analysis_manifest$file) ||
    anyNA(analysis_manifest$file) || anyNA(analysis_manifest$sha256)) {
  stop("Malformed analysis script manifest.")
}
analysis_paths <- unname(vapply(
  as.character(analysis_manifest$file), resolve_project_path,
  character(1), label = "manifested analysis script"
))
analysis_hashes <- tolower(as.character(analysis_manifest$sha256))
if (anyDuplicated(analysis_paths) ||
    any(!grepl("^[0-9a-f]{64}$", analysis_hashes)) ||
    any(!startsWith(
      analysis_paths,
      paste0(normalizePath(script_dir), .Platform$file.sep)
    )) || any(!grepl("\\.(R|r|py)$", analysis_paths))) {
  stop("Analysis script manifest path/hash invariant failed.")
}
current_analysis_hashes <- unname(vapply(
  analysis_paths, sha256_file, character(1)
))
if (!identical(analysis_hashes, current_analysis_hashes)) {
  stop(
    "Current analysis script differs from the authorized manifest: ",
    paste(analysis_paths[analysis_hashes != current_analysis_hashes], collapse = ", ")
  )
}
must_manifest <- normalizePath(
  c(model_utils_path, analysis_utils_path, outcome_script_path, script_path),
  mustWork = TRUE
)
if (length(setdiff(must_manifest, analysis_paths))) {
  stop("Authorized manifest must include the exact 08, 08a, 09, and 10 scripts.")
}

parameter_gate_path <- file.path(
  QC_ROOT, "parameter_freeze", "phase2e_parameter_freeze_complete_v1.csv"
)
outcome_gate_path <- file.path(
  QC_ROOT, "outcomes", "phase3a_rebuilt_outcomes_complete_v1.csv"
)
access_receipt_path <- file.path(
  dirname(checkpoint_path), "outcome_access_receipt_v1.csv"
)
if (any(!file.exists(c(parameter_gate_path, outcome_gate_path, access_receipt_path)))) {
  stop("Parameter, formal-outcome, or access-receipt gate is missing.")
}
require_map_value(
  checkpoint, "parameter_freeze_gate_sha256",
  sha256_file(parameter_gate_path), "authorization checkpoint"
)

parameter_gate <- read_completion_gate(parameter_gate_path, "parameter-freeze gate")
outcome_gate <- read_completion_gate(outcome_gate_path, "outcome-extraction gate")
access_receipt <- read_completion_gate(access_receipt_path, "outcome-access receipt")

require_map_value(parameter_gate, "status", "PASS", "parameter-freeze gate")
require_map_value(
  parameter_gate, "locked_config_version", LOCKED$version,
  "parameter-freeze gate"
)
for (field in c(
  "all_tests_pass", "outcome_leakage_guard_pass", "all_required_qc_present"
)) {
  require_map_value(parameter_gate, field, "TRUE", "parameter-freeze gate")
}
require_map_value(
  parameter_gate, "summary_sentinel", "BUILD_COMPLETE",
  "parameter-freeze gate"
)
require_map_value(
  parameter_gate, "script_sha256", sha256_file(parameter_script_path),
  "parameter-freeze gate"
)
require_map_value(
  parameter_gate, "model_utils_sha256", sha256_file(model_utils_path),
  "parameter-freeze gate"
)
require_map_value(
  parameter_gate, "parameter_derivation_database", "MIMIC-IV v3.1 only",
  "parameter-freeze gate"
)
require_map_value(parameter_gate, "quantile_type", "2", "parameter-freeze gate")

require_map_value(outcome_gate, "status", "PASS", "outcome-extraction gate")
require_map_value(
  outcome_gate, "config_version", LOCKED$version, "outcome-extraction gate"
)
require_map_value(
  outcome_gate, "outcome_access_status", "FORMALLY_UNBLINDED",
  "outcome-extraction gate"
)
require_map_value(
  outcome_gate, "all_accounting_invariants_pass", "TRUE",
  "outcome-extraction gate"
)
require_map_value(
  outcome_gate, "script_sha256", sha256_file(outcome_script_path),
  "outcome-extraction gate"
)
require_map_value(
  outcome_gate, "checkpoint_sha256", sha256_file(checkpoint_path),
  "outcome-extraction gate"
)
require_map_value(
  outcome_gate, "access_receipt_sha256", sha256_file(access_receipt_path),
  "outcome-extraction gate"
)
require_map_value(
  outcome_gate, "analysis_script_manifest_sha256",
  sha256_file(analysis_manifest_path), "outcome-extraction gate"
)
require_map_value(
  outcome_gate, "parameter_freeze_gate_sha256",
  sha256_file(parameter_gate_path), "outcome-extraction gate"
)
require_map_value(
  access_receipt, "status", "OUTCOME_ACCESS_INITIATED",
  "outcome-access receipt"
)
require_map_value(
  access_receipt, "config_version", LOCKED$version, "outcome-access receipt"
)
require_map_value(
  access_receipt, "checkpoint_sha256", sha256_file(checkpoint_path),
  "outcome-access receipt"
)
require_map_value(
  access_receipt, "script_sha256", sha256_file(outcome_script_path),
  "outcome-access receipt"
)

parameter_artifact_fields <- list(
  parameter = c("parameter_rds_path", "parameter_rds_sha256"),
  mimic_frame = c("mimic_model_frame_rds_path", "mimic_model_frame_rds_sha256"),
  eicu_frame = c("eicu_model_frame_rds_path", "eicu_model_frame_rds_sha256")
)
parameter_artifact_paths <- lapply(parameter_artifact_fields, function(pair) {
  path <- resolve_project_path(
    require_map_value(parameter_gate, pair[[1L]], label = "parameter-freeze gate"),
    pair[[1L]], require_relative = TRUE
  )
  require_map_value(
    parameter_gate, pair[[2L]], sha256_file(path), "parameter-freeze gate"
  )
  path
})

mimic_outcome_path <- file.path(
  PRIVATE_ROOT, "outcomes", "mimic_rebuilt_outcomes_v1.rds"
)
eicu_outcome_path <- file.path(
  PRIVATE_ROOT, "outcomes", "eicu_rebuilt_outcomes_v1.rds"
)
if (any(!file.exists(c(mimic_outcome_path, eicu_outcome_path)))) {
  stop("Outcome gate exists but a private outcome artifact is missing.")
}
require_map_value(
  outcome_gate, "mimic_outcome_rds_sha256", sha256_file(mimic_outcome_path),
  "outcome-extraction gate"
)
require_map_value(
  outcome_gate, "eicu_outcome_rds_sha256", sha256_file(eicu_outcome_path),
  "outcome-extraction gate"
)

decision_text <- paste(readLines(decision_log_path, warn = FALSE), collapse = "\n")
if (!grepl("\\| D054 \\|", decision_text) ||
    !grepl("\\| D057 \\|", decision_text)) {
  stop("Locked modeling requires both D054 and D057 in the authorized log.")
}

private_out <- file.path(PRIVATE_ROOT, "locked_models")
aggregate_out <- file.path(AGGREGATE_ROOT, "locked_models")
qc_out <- file.path(QC_ROOT, "locked_models")
completion_gate <- file.path(qc_out, "phase3b_locked_models_complete_v1.csv")
model_rds_path <- file.path(private_out, "mimic_locked_models_v1.rds")
prediction_rds_path <- file.path(private_out, "locked_model_predictions_v1.rds")
bootstrap_rds_path <- file.path(
  private_out, "locked_model_validation_bootstrap_v1.rds"
)
published_private <- c(model_rds_path, prediction_rds_path, bootstrap_rds_path)
if (file.exists(completion_gate)) {
  stop("Locked-model analysis is already complete; refusing to overwrite it.")
}
if (any(file.exists(published_private))) {
  stop(
    "Private model output exists without a PASS gate. Resolve the interrupted ",
    "run explicitly; automatic overwrite is prohibited."
  )
}

source(model_utils_path)
source(analysis_utils_path)
required_utils <- c(
  "binary_performance", "paired_metric_difference", "fit_locked_logistic",
  "three_knot_rcs_basis", "four_knot_rcs_basis", "quantile_knots",
  "clip_probability", "locked_model_specification",
  "locked_comparison_specification", "build_design_matrix", "fit_model",
  "predict_model", "performance_vector", "fit_recalibration",
  "flexible_calibration_curve",
  "cluster_bootstrap_indices", "percentile_interval", "wald_contrast",
  "fit_weighted_model", "weighted_performance_vector"
)
if (!all(vapply(required_utils, exists, logical(1L), mode = "function"))) {
  stop("Locked model utility interface is incomplete.")
}

analysis_population_mask <- function(x, analysis_set) {
  outcome_known <- x$hospital_mortality_eligible %in% TRUE &
    !is.na(x$hospital_mortality) & x$hospital_mortality %in% c(0L, 1L)
  switch(
    analysis_set,
    primary_common = x$primary_predictor_complete %in% TRUE & outcome_known,
    component_common = x$component_predictor_complete %in% TRUE & outcome_known,
    normalized_common = x$component_predictor_complete %in% TRUE &
      x$normalized_exposure_complete %in% TRUE & outcome_known,
    no_gcs_common = complete_finite(x, no_gcs_complete_variables) & outcome_known,
    stop("Unknown analysis set: ", analysis_set)
  )
}

# ---------------------------------------------------------------------------
# Deep-read the checksum-gated parameter frames and formally extracted outcomes.
# ---------------------------------------------------------------------------

parameters <- readRDS(parameter_artifact_paths$parameter)
mimic_frame <- as.data.table(readRDS(parameter_artifact_paths$mimic_frame))
eicu_frame <- as.data.table(readRDS(parameter_artifact_paths$eicu_frame))
mimic_outcomes <- as.data.table(readRDS(mimic_outcome_path))
eicu_outcomes <- as.data.table(readRDS(eicu_outcome_path))

required_parameter_fields <- c(
  "artifact_version", "decision_id", "locked_config_version",
  "derivation_database", "derivation_population_n", "quantile_type",
  "primary_predictors", "component_predictors",
  "normalized_exposure_predictors", "three_knot_values",
  "smp_knot_values", "smp_center_scale", "smp_per_pbw_center_scale",
  "canonical_model_frame_schema", "model_utils_sha256",
  "mimic_severity_gate_sha256", "mimic_input_rds_sha256"
)
if (!is.list(parameters) ||
    length(setdiff(required_parameter_fields, names(parameters))) ||
    !identical(parameters$artifact_version, "frozen_predictor_parameters_v1") ||
    !identical(parameters$decision_id, "D054") ||
    !identical(parameters$locked_config_version, LOCKED$version) ||
    !identical(parameters$derivation_database, "MIMIC-IV v3.1 only") ||
    !identical(as.integer(parameters$quantile_type), 2L) ||
    !identical(parameters$model_utils_sha256, sha256_file(model_utils_path))) {
  stop("Frozen parameter RDS failed its internal schema/provenance checks.")
}
transform_bundle <- parameter_to_transform_bundle(parameters)

canonical_schema <- as.character(parameters$canonical_model_frame_schema)
if (!identical(names(mimic_frame), canonical_schema) ||
    !identical(names(eicu_frame), canonical_schema) ||
    anyDuplicated(mimic_frame$analysis_id) ||
    anyDuplicated(eicu_frame$analysis_id) ||
    anyNA(mimic_frame$analysis_id) || anyNA(eicu_frame$analysis_id)) {
  stop("Canonical model-frame schema/key invariant failed.")
}
if (nrow(mimic_frame) != as.integer(require_map_value(
  parameter_gate, "mimic_frame_n", label = "parameter-freeze gate"
)) || nrow(eicu_frame) != as.integer(require_map_value(
  parameter_gate, "eicu_frame_n", label = "parameter-freeze gate"
))) {
  stop("Canonical model-frame count differs from the parameter gate.")
}
if (sum(mimic_frame$primary_predictor_complete) != as.integer(
  require_map_value(
    parameter_gate, "mimic_primary_predictor_complete_n",
    label = "parameter-freeze gate"
  )
) || sum(eicu_frame$primary_predictor_complete) != as.integer(
  require_map_value(
    parameter_gate, "eicu_primary_predictor_complete_n",
    label = "parameter-freeze gate"
  )
)) {
  stop("Primary completeness flag/count differs from the parameter gate.")
}

required_mimic_outcome <- c(
  "stay_id", "subject_id", "hospital_mortality",
  "hospital_mortality_eligible"
)
required_eicu_outcome <- c(
  "patientunitstayid", "person_key", "hospitalid", "hospital_mortality",
  "hospital_mortality_eligible"
)
if (length(setdiff(required_mimic_outcome, names(mimic_outcomes))) ||
    length(setdiff(required_eicu_outcome, names(eicu_outcomes)))) {
  stop("A formally extracted outcome artifact lacks its exact join/outcome fields.")
}
if (nrow(mimic_outcomes) != as.integer(require_map_value(
  outcome_gate, "mimic_prediction_n", label = "outcome-extraction gate"
)) || nrow(eicu_outcomes) != as.integer(require_map_value(
  outcome_gate, "eicu_prediction_n", label = "outcome-extraction gate"
))) {
  stop("Outcome artifact count differs from the outcome gate.")
}
if (anyDuplicated(mimic_outcomes$stay_id) ||
    anyDuplicated(mimic_outcomes$subject_id) ||
    anyDuplicated(eicu_outcomes$patientunitstayid) ||
    anyDuplicated(eicu_outcomes$person_key) ||
    anyNA(mimic_outcomes$stay_id) || anyNA(mimic_outcomes$subject_id) ||
    anyNA(eicu_outcomes$patientunitstayid) ||
    anyNA(eicu_outcomes$person_key) ||
    anyNA(eicu_outcomes$hospitalid)) {
  stop("Outcome join key/patient/hospital cluster invariant failed.")
}
meta_mimic_outcome <- attr(mimic_outcomes, "rebuild_metadata")
meta_eicu_outcome <- attr(eicu_outcomes, "rebuild_metadata")
if (!isTRUE(meta_mimic_outcome$formally_unblinded) ||
    !isTRUE(meta_eicu_outcome$formally_unblinded) ||
    !identical(meta_mimic_outcome$checkpoint_sha256, sha256_file(checkpoint_path)) ||
    !identical(meta_eicu_outcome$checkpoint_sha256, sha256_file(checkpoint_path))) {
  stop("Outcome RDS metadata does not match the formal checkpoint.")
}

mimic_outcome_link <- mimic_outcomes[, .(
  analysis_id = as.integer(stay_id),
  patient_cluster_id = as.character(subject_id),
  hospital_mortality = as.integer(hospital_mortality),
  hospital_mortality_eligible = as.logical(hospital_mortality_eligible)
)]
eicu_outcome_link <- eicu_outcomes[, .(
  analysis_id = as.integer(patientunitstayid),
  hospital_id = as.character(hospitalid),
  hospital_mortality = as.integer(hospital_mortality),
  hospital_mortality_eligible = as.logical(hospital_mortality_eligible)
)]
if (!setequal(mimic_frame$analysis_id, mimic_outcome_link$analysis_id) ||
    !setequal(eicu_frame$analysis_id, eicu_outcome_link$analysis_id)) {
  stop("Canonical predictor and outcome analysis-ID sets are not identical.")
}

mimic_analysis <- merge(
  mimic_frame, mimic_outcome_link, by = "analysis_id",
  all = FALSE, sort = FALSE
)
eicu_analysis <- merge(
  eicu_frame, eicu_outcome_link, by = "analysis_id",
  all = FALSE, sort = FALSE
)
setorder(mimic_analysis, analysis_id)
setorder(eicu_analysis, analysis_id)
if (nrow(mimic_analysis) != nrow(mimic_frame) ||
    nrow(eicu_analysis) != nrow(eicu_frame) ||
    anyDuplicated(mimic_analysis$analysis_id) ||
    anyDuplicated(eicu_analysis$analysis_id)) {
  stop("Exact analysis-ID join changed cardinality.")
}
if (anyNA(mimic_analysis$patient_cluster_id) ||
    anyDuplicated(mimic_analysis$patient_cluster_id)) {
  stop("MIMIC primary cohort must contain exactly one stay per patient.")
}
for (x in list(mimic_analysis, eicu_analysis)) {
  if (anyNA(x$hospital_mortality_eligible) ||
      any(!is.na(x$hospital_mortality) &
        !x$hospital_mortality %in% c(0L, 1L)) ||
      any(x$hospital_mortality_eligible & is.na(x$hospital_mortality)) ||
      any(!x$hospital_mortality_eligible & !is.na(x$hospital_mortality))) {
    stop("Primary-outcome eligibility invariant failed after exact join.")
  }
}

model_specs <- locked_model_specification()
comparison_specs <- locked_comparison_specification()
if (anyDuplicated(model_specs$model_id) ||
    !setequal(
      model_specs$model_id,
      c("S0", "S1", "S2", "S3", "S2M", "S3NL", "S3c", "S4", "S5",
        "N3_abs", "N3_pbw", "R2", "R3")
    ) || sum(comparison_specs$likelihood_ratio_allowed) != 1L ||
    comparison_specs[likelihood_ratio_allowed == TRUE, comparison_id] !=
      "S2M_minus_S2") {
  stop("Locked model/comparison manifest changed unexpectedly.")
}

population_counts <- rbindlist(lapply(
  list(`MIMIC-IV_v3.1` = mimic_analysis, `eICU-CRD_v2.0` = eicu_analysis),
  function(frame) {
    rbindlist(lapply(unique(model_specs$analysis_set), function(set_id) {
      keep <- analysis_population_mask(frame, set_id)
      data.table(
        analysis_set = set_id,
        source_tuple_n = nrow(frame),
        eligible_complete_n = sum(keep),
        event_n = sum(frame$hospital_mortality[keep] == 1L),
        nonevent_n = sum(frame$hospital_mortality[keep] == 0L),
        excluded_n = nrow(frame) - sum(keep)
      )
    }))
  }
), idcol = "database")
if (any(population_counts$eligible_complete_n !=
    population_counts$event_n + population_counts$nonevent_n) ||
    any(population_counts$event_n == 0L) ||
    any(population_counts$nonevent_n == 0L)) {
  stop("A locked analysis set is empty or contains only one outcome class.")
}

# SAP section 14.3 evidence tier is a descriptive precision label only. It
# neither removes a model nor changes any analysis population or estimator.
external_evidence_tier <- copy(population_counts[database == "eICU-CRD_v2.0"])
external_evidence_tier[, evidence_tier := fcase(
  event_n >= 200L & nonevent_n >= 200L, "FULL",
  event_n >= 100L & nonevent_n >= 100L, "VALIDATION_IMPRECISE",
  default = "EXPLORATORY_REPLICATION"
)]
external_evidence_tier[, `:=`(
  evidence_tier_order = match(
    evidence_tier,
    c("FULL", "VALIDATION_IMPRECISE", "EXPLORATORY_REPLICATION")
  ),
  threshold_rule = paste0(
    "FULL:events_and_nonevents>=200;VALIDATION_IMPRECISE:",
    "events_and_nonevents>=100_but_either<200;",
    "EXPLORATORY_REPLICATION:either<100"
  ),
  label_only_no_model_change = TRUE
)]
evidence_tier_qc <- copy(external_evidence_tier)
evidence_tier_qc[, expected_evidence_tier := fcase(
  event_n >= 200L & nonevent_n >= 200L, "FULL",
  event_n >= 100L & nonevent_n >= 100L, "VALIDATION_IMPRECISE",
  default = "EXPLORATORY_REPLICATION"
)]
evidence_tier_qc[, `:=`(
  rule_recomputed_pass = evidence_tier == expected_evidence_tier,
  population_or_model_changed = FALSE
)]
if (any(evidence_tier_qc$rule_recomputed_pass != TRUE) ||
    any(evidence_tier_qc$population_or_model_changed != FALSE)) {
  stop("External evidence-tier labeling invariant failed.")
}

input_gate_qc <- data.table(
  check = c(
    "authorization_checkpoint_PASS",
    "analysis_manifest_all_hashes_match",
    "manifest_includes_08_08a_09_10",
    "checkpoint_directly_hash_locks_08a",
    "parameter_gate_PASS_and_deep_hashes",
    "three_parameter_RDS_hashes_match",
    "outcome_gate_formally_unblinded_PASS",
    "outcome_RDS_hashes_match",
    "access_receipt_hash_chain_matches",
    "D054_and_D057_authorized",
    "MIMIC_one_patient_one_stay",
    "MIMIC_exact_analysis_id_join",
    "eICU_exact_analysis_id_join"
  ),
  pass = TRUE
)

# ---------------------------------------------------------------------------
# Fit every locked development model on its prespecified common set.
# ---------------------------------------------------------------------------

development_fits <- list()
development_predictions <- list()
development_performance <- list()
model_status <- list()
model_matrix_qc <- list()

for (row_index in seq_len(nrow(model_specs))) {
  spec <- model_specs[row_index]
  keep <- analysis_population_mask(mimic_analysis, spec$analysis_set)
  data <- mimic_analysis[keep]
  design <- build_design_matrix(data, spec$model_id, transform_bundle)
  fit <- fit_model(
    design, data$hospital_mortality, spec$model_id,
    allow_nonestimable = spec$allow_nonestimable
  )
  development_fits[[spec$model_id]] <- fit
  model_status[[spec$model_id]] <- data.table(
    model_id = spec$model_id,
    analysis_set = spec$analysis_set,
    role = spec$role,
    status = fit$status,
    reason = fit$reason,
    n = fit$n,
    events = fit$events,
    design_columns_n = ncol(design),
    fitted_rank = fit$rank,
    information_condition_number = fit$condition_number,
    nonestimable_allowed = spec$allow_nonestimable
  )
  model_matrix_qc[[spec$model_id]] <- data.table(
    database = "MIMIC-IV_v3.1",
    model_id = spec$model_id,
    analysis_set = spec$analysis_set,
    n = nrow(design),
    columns_n = ncol(design),
    unique_columns = !anyDuplicated(colnames(design)),
    complete_finite = !anyNA(design) && all(is.finite(design)),
    column_signature = paste(colnames(design), collapse = ";")
  )
  if (identical(fit$status, "ESTIMABLE")) {
    probability <- predict_model(fit, design)
    metrics <- performance_vector(data$hospital_mortality, probability)
    development_predictions[[spec$model_id]] <- data.table(
      database = "MIMIC-IV_v3.1",
      analysis_set = spec$analysis_set,
      model_id = spec$model_id,
      analysis_id = data$analysis_id,
      patient_cluster_id = data$patient_cluster_id,
      hospital_id = NA_character_,
      outcome = data$hospital_mortality,
      probability_raw = probability,
      probability_recal_intercept = NA_real_,
      probability_recal_intercept_slope = NA_real_
    )
    development_performance[[spec$model_id]] <- data.table(
      database = "MIMIC-IV_v3.1",
      analysis_set = spec$analysis_set,
      model_id = spec$model_id,
      stage = "apparent",
      metric = names(metrics),
      estimate = as.numeric(metrics),
      n = nrow(data), events = sum(data$hospital_mortality)
    )
  }
}

model_status <- rbindlist(model_status, use.names = TRUE)
model_matrix_qc <- rbindlist(model_matrix_qc, use.names = TRUE)
development_prediction_long <- rbindlist(
  development_predictions, use.names = TRUE, fill = TRUE
)
development_performance <- rbindlist(
  development_performance, use.names = TRUE, fill = TRUE
)
if (any(model_status$model_id != "S5" & model_status$status != "ESTIMABLE")) {
  stop(
    "A required locked development model is non-estimable: ",
    paste(model_status[model_id != "S5" & status != "ESTIMABLE", model_id],
      collapse = ", "
    )
  )
}

coefficient_results <- rbindlist(lapply(names(development_fits), function(model_id) {
  fit <- development_fits[[model_id]]
  if (!identical(fit$status, "ESTIMABLE")) {
    return(data.table(
      model_id = model_id, term = NA_character_, coefficient = NA_real_,
      standard_error = NA_real_, odds_ratio = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_,
      interpretation = "model_nonestimable"
    ))
  }
  standard_error <- sqrt(diag(fit$vcov))
  data.table(
    model_id = model_id,
    term = names(fit$coefficients),
    coefficient = as.numeric(fit$coefficients),
    standard_error = as.numeric(standard_error),
    odds_ratio = exp(as.numeric(fit$coefficients)),
    ci_lower = exp(as.numeric(fit$coefficients) - 1.96 * standard_error),
    ci_upper = exp(as.numeric(fit$coefficients) + 1.96 * standard_error),
    interpretation = fifelse(
      grepl("_rcs", names(fit$coefficients)),
      "spline_basis_coefficient_not_a_clinical_unit_OR",
      "one_design_unit_Wald_OR"
    )
  )
}), use.names = TRUE, fill = TRUE)

clinical_contrasts <- list()
for (model_id in c("S3", "S2M", "S3c", "S5", "R3")) {
  fit <- development_fits[[model_id]]
  clinical_contrasts[[paste0(model_id, "_smp5")]] <- wald_contrast(
    fit, c(smp_per_5 = 1), "absolute_sMP_per_5_J_min_linear_primary",
    0, LOCKED$mp_effect_unit_J_per_min, "J/min"
  )
}
reference <- unname(transform_bundle$smp_center_scale[["mean"]])
nonlinear_basis <- four_knot_rcs_basis(
  c(reference, reference + LOCKED$mp_effect_unit_J_per_min),
  transform_bundle$smp_knots, "smp"
)
nonlinear_contrast <- nonlinear_basis[2L, ] - nonlinear_basis[1L, ]
names(nonlinear_contrast) <- colnames(nonlinear_basis)
clinical_contrasts$S3NL_smp5_at_frozen_mean <- wald_contrast(
  development_fits$S3NL, nonlinear_contrast,
  "secondary_four_knot_sMP_frozen_mean_to_mean_plus_5",
  reference, reference + LOCKED$mp_effect_unit_J_per_min, "J/min"
)
clinical_contrasts$N3_abs_sd <- wald_contrast(
  development_fits$N3_abs, c(smp_z = 1),
  "absolute_sMP_per_frozen_MIMIC_SD", 0, 1, "MIMIC SD"
)
clinical_contrasts$N3_abs_5 <- wald_contrast(
  development_fits$N3_abs,
  c(smp_z = LOCKED$mp_effect_unit_J_per_min /
      transform_bundle$smp_center_scale[["sd"]]),
  "absolute_sMP_per_5_J_min_linear_normalized_comparison",
  0, LOCKED$mp_effect_unit_J_per_min, "J/min"
)
clinical_contrasts$N3_pbw_sd <- wald_contrast(
  development_fits$N3_pbw, c(smp_per_pbw_z = 1),
  "sMP_per_PBW_per_frozen_MIMIC_SD", 0, 1, "MIMIC SD"
)
clinical_contrasts <- rbindlist(clinical_contrasts, use.names = TRUE, fill = TRUE)

fit_s2 <- development_fits$S2
fit_s2m <- development_fits$S2M
if (!identical(fit_s2$n, fit_s2m$n) ||
    !identical(fit_s2$events, fit_s2m$events)) {
  stop("S2 and S2M were not fitted on the identical primary common set.")
}
lr_statistic <- 2 * (fit_s2m$loglik - fit_s2$loglik)
lr_df <- length(fit_s2m$coefficients) - length(fit_s2$coefficients)
if (!is.finite(lr_statistic) || lr_statistic < -1e-8 || lr_df <= 0L) {
  stop("S2-versus-S2M likelihood-ratio invariant failed.")
}
likelihood_ratio_result <- data.table(
  comparison_id = "S2M_minus_S2",
  reference_model = "S2", new_model = "S2M",
  n = fit_s2$n, events = fit_s2$events,
  likelihood_ratio_chisq = max(0, lr_statistic),
  degrees_of_freedom = lr_df,
  p_value = stats::pchisq(max(0, lr_statistic), df = lr_df, lower.tail = FALSE),
  only_permitted_LR_test = TRUE
)

run_mimic_bootstrap <- function(
    original_frame, model_specs, apparent_status,
    repetitions = MIMIC_BOOTSTRAP_REPS,
    seed = MIMIC_BOOTSTRAP_SEED) {
  if (!all(c("analysis_id", "patient_cluster_id") %in% names(original_frame)) ||
      anyNA(original_frame$patient_cluster_id) ||
      anyDuplicated(original_frame$analysis_id) ||
      anyDuplicated(original_frame$patient_cluster_id)) {
    stop(paste0(
      "MIMIC patient bootstrap requires nonmissing patient_cluster_id and ",
      "exactly one analysis stay per patient."
    ))
  }
  active <- model_specs[
    model_id %in% apparent_status[status == "ESTIMABLE", model_id]
  ]
  set.seed(seed)
  output <- vector("list", repetitions * nrow(active))
  position <- 0L
  patient_ids <- as.character(original_frame$patient_cluster_id)
  for (replicate_id in seq_len(repetitions)) {
    sampled_patient_ids <- sample(
      patient_ids, length(patient_ids), replace = TRUE
    )
    sampled <- original_frame[match(sampled_patient_ids, patient_cluster_id)]
    bundle_result <- tryCatch(
      list(ok = TRUE, value = derive_bootstrap_transform_bundle(sampled)),
      error = function(e) list(ok = FALSE, reason = conditionMessage(e))
    )
    for (row_index in seq_len(nrow(active))) {
      position <- position + 1L
      spec <- active[row_index]
      failure_row <- function(reason) data.table(
        replicate = replicate_id, model_id = spec$model_id,
        analysis_set = spec$analysis_set, success = FALSE,
        reason = reason, metric = NA_character_,
        train_estimate = NA_real_, test_estimate = NA_real_,
        optimism = NA_real_, train_n = NA_integer_, train_events = NA_integer_,
        bootstrap_unit = "MIMIC_subject_id_patient_cluster"
      )
      if (!bundle_result$ok) {
        output[[position]] <- failure_row(bundle_result$reason)
        next
      }
      result <- tryCatch({
        train_keep <- analysis_population_mask(sampled, spec$analysis_set)
        test_keep <- analysis_population_mask(original_frame, spec$analysis_set)
        train <- sampled[train_keep]
        test <- original_frame[test_keep]
        assert_binary_outcome(train$hospital_mortality)
        assert_binary_outcome(test$hospital_mortality)
        train_design <- build_design_matrix(
          train, spec$model_id, bundle_result$value
        )
        test_design <- build_design_matrix(
          test, spec$model_id, bundle_result$value
        )
        fit <- fit_model(
          train_design, train$hospital_mortality, spec$model_id,
          allow_nonestimable = TRUE
        )
        if (!identical(fit$status, "ESTIMABLE")) stop(fit$reason)
        train_metric <- performance_vector(
          train$hospital_mortality, predict_model(fit, train_design)
        )
        test_metric <- performance_vector(
          test$hospital_mortality, predict_model(fit, test_design)
        )
        data.table(
          replicate = replicate_id, model_id = spec$model_id,
          analysis_set = spec$analysis_set, success = TRUE, reason = "",
          metric = metric_names,
          train_estimate = as.numeric(train_metric[metric_names]),
          test_estimate = as.numeric(test_metric[metric_names]),
          optimism = as.numeric(train_metric[metric_names] - test_metric[metric_names]),
          train_n = nrow(train), train_events = sum(train$hospital_mortality),
          bootstrap_unit = "MIMIC_subject_id_patient_cluster"
        )
      }, error = function(e) failure_row(conditionMessage(e)))
      output[[position]] <- result
    }
  }
  rbindlist(output, use.names = TRUE, fill = TRUE)
}

mimic_bootstrap <- run_mimic_bootstrap(
  mimic_analysis, model_specs, model_status,
  repetitions = MIMIC_BOOTSTRAP_REPS, seed = MIMIC_BOOTSTRAP_SEED
)
mimic_bootstrap_success <- mimic_bootstrap[, .(
  successful_replicates = uniqueN(replicate[success == TRUE]),
  failed_replicates = MIMIC_BOOTSTRAP_REPS - uniqueN(replicate[success == TRUE]),
  success_rate = uniqueN(replicate[success == TRUE]) / MIMIC_BOOTSTRAP_REPS,
  first_failure_reason = {
    z <- reason[success == FALSE & !is.na(reason) & nzchar(reason)]
    if (length(z)) z[[1L]] else ""
  }
), by = .(model_id, analysis_set)]

not_run_s5 <- model_status[model_id == "S5" & status != "ESTIMABLE"]
if (nrow(not_run_s5)) {
  mimic_bootstrap_success <- rbindlist(list(
    mimic_bootstrap_success,
    data.table(
      model_id = "S5", analysis_set = "component_common",
      successful_replicates = NA_integer_, failed_replicates = NA_integer_,
      success_rate = NA_real_,
      first_failure_reason = "NOT_RUN_APPARENT_S5_NON_ESTIMABLE"
    )
  ), use.names = TRUE)
}
if (any(mimic_bootstrap_success[
  !is.na(success_rate), success_rate < BOOTSTRAP_SUCCESS_THRESHOLD
])) {
  stop(
    "MIMIC patient-bootstrap success below the prefrozen 95% rule: ",
    paste(mimic_bootstrap_success[
      !is.na(success_rate) & success_rate < BOOTSTRAP_SUCCESS_THRESHOLD,
      model_id
    ], collapse = ", ")
  )
}

mimic_optimism_summary <- mimic_bootstrap[success == TRUE, {
  interval <- percentile_interval(optimism)
  list(
    bootstrap_success_n = .N,
    mean_optimism = mean(optimism),
    optimism_ci_lower = interval[["lower"]],
    optimism_ci_upper = interval[["upper"]]
  )
}, by = .(model_id, analysis_set, metric)]
mimic_optimism_summary <- merge(
  mimic_optimism_summary,
  development_performance[, .(
    model_id, analysis_set, metric, apparent = estimate, n, events
  )],
  by = c("model_id", "analysis_set", "metric"), all.x = TRUE, sort = FALSE
)
mimic_optimism_summary[, `:=`(
  optimism_corrected = apparent - mean_optimism,
  optimism_corrected_ci_lower = apparent - optimism_ci_upper,
  optimism_corrected_ci_upper = apparent - optimism_ci_lower,
  interval_interpretation = paste(
    "apparent estimate minus percentile optimism distribution interval;",
    "not a full sampling confidence interval"
  ),
  requested_replicates = MIMIC_BOOTSTRAP_REPS,
  bootstrap_unit = "MIMIC_subject_id_patient_cluster_one_stay_per_patient"
)]

# ---------------------------------------------------------------------------
# Apply the locked MIMIC models once to eICU, then label recalibration as model
# updating. Raw external predictions are created before any updating fit.
# ---------------------------------------------------------------------------

external_predictions <- list()
external_performance_point <- list()
recalibration_parameters <- list()
external_matrix_qc <- list()

for (row_index in seq_len(nrow(model_specs))) {
  spec <- model_specs[row_index]
  fit <- development_fits[[spec$model_id]]
  if (!identical(fit$status, "ESTIMABLE")) next
  keep <- analysis_population_mask(eicu_analysis, spec$analysis_set)
  data <- eicu_analysis[keep]
  design <- build_design_matrix(data, spec$model_id, transform_bundle)
  if (!identical(colnames(design), fit$design_columns)) {
    stop("External design columns differ from locked MIMIC model: ", spec$model_id)
  }
  raw_probability <- predict_model(fit, design)
  # Only after the raw locked prediction is fixed do eICU outcomes enter the
  # two explicitly labelled updating fits.
  recalibration <- fit_recalibration(data$hospital_mortality, raw_probability)
  stage_probabilities <- list(
    original_locked = raw_probability,
    recalibration_intercept_only = recalibration$probability_intercept_only,
    recalibration_intercept_and_slope =
      recalibration$probability_intercept_and_slope
  )
  external_predictions[[spec$model_id]] <- data.table(
    database = "eICU-CRD_v2.0",
    analysis_set = spec$analysis_set,
    model_id = spec$model_id,
    analysis_id = data$analysis_id,
    patient_cluster_id = NA_character_,
    hospital_id = data$hospital_id,
    outcome = data$hospital_mortality,
    probability_raw = raw_probability,
    probability_recal_intercept =
      recalibration$probability_intercept_only,
    probability_recal_intercept_slope =
      recalibration$probability_intercept_and_slope
  )
  external_performance_point[[spec$model_id]] <- rbindlist(lapply(
    names(stage_probabilities), function(stage_name) {
      metrics <- performance_vector(
        data$hospital_mortality, stage_probabilities[[stage_name]]
      )
      data.table(
        database = "eICU-CRD_v2.0", analysis_set = spec$analysis_set,
        model_id = spec$model_id, stage = stage_name,
        stage_order = match(stage_name, c(
          "original_locked", "recalibration_intercept_only",
          "recalibration_intercept_and_slope"
        )),
        metric = metric_names, estimate = as.numeric(metrics[metric_names]),
        n = nrow(data), events = sum(data$hospital_mortality)
      )
    }
  ))
  recalibration_parameters[[spec$model_id]] <- rbindlist(list(
    data.table(
      model_id = spec$model_id, analysis_set = spec$analysis_set,
      updating_stage = "recalibration_intercept_only",
      intercept = recalibration$intercept_only[["intercept"]],
      slope = recalibration$intercept_only[["slope"]],
      interpretation = "eICU_model_updating_not_original_validation"
    ),
    data.table(
      model_id = spec$model_id, analysis_set = spec$analysis_set,
      updating_stage = "recalibration_intercept_and_slope",
      intercept = recalibration$intercept_and_slope[["intercept"]],
      slope = recalibration$intercept_and_slope[["slope"]],
      interpretation = "eICU_model_updating_not_original_validation"
    )
  ))
  external_matrix_qc[[spec$model_id]] <- data.table(
    database = "eICU-CRD_v2.0", model_id = spec$model_id,
    analysis_set = spec$analysis_set, n = nrow(design),
    columns_n = ncol(design), unique_columns = !anyDuplicated(colnames(design)),
    complete_finite = !anyNA(design) && all(is.finite(design)),
    column_signature = paste(colnames(design), collapse = ";")
  )
}

external_prediction_long <- rbindlist(
  external_predictions, use.names = TRUE, fill = TRUE
)
external_performance_point <- rbindlist(
  external_performance_point, use.names = TRUE, fill = TRUE
)
recalibration_parameters <- rbindlist(
  recalibration_parameters, use.names = TRUE, fill = TRUE
)

# Descriptive flexible calibration uses one fixed natural-spline recipe. The
# curves are point estimates only and never feed model selection or updating.
flexible_calibration_curve_list <- list()
curve_position <- 0L
for (model_id in names(development_predictions)) {
  prediction <- development_predictions[[model_id]]
  curve_position <- curve_position + 1L
  curve <- flexible_calibration_curve(
    prediction$outcome, prediction$probability_raw
  )
  curve[, `:=`(
    database = "MIMIC-IV_v3.1",
    analysis_set = unique(prediction$analysis_set),
    model_id = model_id,
    stage = "apparent",
    stage_order = 1L
  )]
  flexible_calibration_curve_list[[curve_position]] <- curve
}
external_curve_stages <- c(
  original_locked = "probability_raw",
  recalibration_intercept_only = "probability_recal_intercept",
  recalibration_intercept_and_slope = "probability_recal_intercept_slope"
)
for (model_id in names(external_predictions)) {
  prediction <- external_predictions[[model_id]]
  for (stage_name in names(external_curve_stages)) {
    curve_position <- curve_position + 1L
    curve <- flexible_calibration_curve(
      prediction$outcome,
      prediction[[external_curve_stages[[stage_name]]]]
    )
    curve[, `:=`(
      database = "eICU-CRD_v2.0",
      analysis_set = unique(prediction$analysis_set),
      model_id = model_id,
      stage = stage_name,
      stage_order = match(stage_name, names(external_curve_stages))
    )]
    flexible_calibration_curve_list[[curve_position]] <- curve
  }
}
flexible_calibration_curves <- rbindlist(
  flexible_calibration_curve_list, use.names = TRUE, fill = TRUE
)
setcolorder(flexible_calibration_curves, c(
  "database", "analysis_set", "model_id", "stage", "stage_order",
  setdiff(
    names(flexible_calibration_curves),
    c("database", "analysis_set", "model_id", "stage", "stage_order")
  )
))
setorder(
  flexible_calibration_curves,
  database, analysis_set, model_id, stage_order, grid_index
)
flexible_calibration_qc <- flexible_calibration_curves[, .(
  grid_n = .N,
  grid_is_strictly_increasing = all(diff(predicted_probability) > 0),
  point_estimates_complete = all(
    is.finite(predicted_probability) &
      is.finite(calibrated_observed_probability)
  ),
  confidence_interval_absent = all(is.na(ci_lower)) & all(is.na(ci_upper)),
  point_estimate_label_pass = all(ci_status == "POINT_ESTIMATE_ONLY_NO_CI"),
  excluded_from_model_selection = all(used_for_model_selection == FALSE)
), by = .(database, analysis_set, model_id, stage)]
if (any(flexible_calibration_qc$grid_n != FLEXIBLE_CALIBRATION_GRID_N) ||
    any(!flexible_calibration_qc$grid_is_strictly_increasing) ||
    any(!flexible_calibration_qc$point_estimates_complete) ||
    any(!flexible_calibration_qc$confidence_interval_absent) ||
    any(!flexible_calibration_qc$point_estimate_label_pass) ||
    any(!flexible_calibration_qc$excluded_from_model_selection)) {
  stop("Flexible-calibration curve QC failed.")
}

model_matrix_qc <- rbindlist(list(
  model_matrix_qc,
  rbindlist(external_matrix_qc, use.names = TRUE, fill = TRUE)
), use.names = TRUE, fill = TRUE)
if (any(model_matrix_qc$unique_columns != TRUE) ||
    any(model_matrix_qc$complete_finite != TRUE)) {
  stop("A development/external model-matrix QC check failed.")
}

run_eicu_cluster_bootstrap <- function(
    original_frame, model_specs, comparison_specs, development_fits,
    bundle, repetitions = EICU_CLUSTER_BOOTSTRAP_REPS,
    seed = EICU_BOOTSTRAP_SEED) {
  metric_output <- list()
  difference_output <- list()
  metric_position <- 0L
  difference_position <- 0L
  analysis_sets <- unique(model_specs$analysis_set)
  for (set_index in seq_along(analysis_sets)) {
    set_id <- analysis_sets[[set_index]]
    set_specs <- model_specs[
      analysis_set == set_id &
        model_id %in% names(development_fits)[vapply(
          development_fits, function(x) identical(x$status, "ESTIMABLE"),
          logical(1L)
        )]
    ]
    keep <- analysis_population_mask(original_frame, set_id)
    data <- original_frame[keep]
    if (anyNA(data$hospital_id) || uniqueN(data$hospital_id) < 2L) {
      stop("External cluster bootstrap needs at least two nonmissing hospitals.")
    }
    raw_probability <- setNames(lapply(set_specs$model_id, function(model_id) {
      design <- build_design_matrix(data, model_id, bundle)
      predict_model(development_fits[[model_id]], design)
    }), set_specs$model_id)
    set_comparisons <- comparison_specs[
      analysis_set == set_id & new_model %in% names(raw_probability) &
        reference_model %in% names(raw_probability)
    ]
    set.seed(seed + set_index - 1L)
    for (replicate_id in seq_len(repetitions)) {
      index <- cluster_bootstrap_indices(data$hospital_id)
      y <- data$hospital_mortality[index]
      class_ok <- length(unique(y)) == 2L
      for (model_id in names(raw_probability)) {
        failure <- function(stage, reason) data.table(
          replicate = replicate_id, seed = seed + set_index - 1L,
          analysis_set = set_id, model_id = model_id, stage = stage,
          success = FALSE, reason = reason, metric = NA_character_,
          estimate = NA_real_, resample_n = length(index),
          resample_events = sum(y)
        )
        if (!class_ok) {
          for (stage_name in c(
            "original_locked", "recalibration_intercept_only",
            "recalibration_intercept_and_slope"
          )) {
            metric_position <- metric_position + 1L
            metric_output[[metric_position]] <- failure(
              stage_name, "single_outcome_class"
            )
          }
          next
        }
        raw <- raw_probability[[model_id]][index]
        raw_result <- tryCatch({
          metric <- performance_vector(y, raw)
          data.table(
            replicate = replicate_id, seed = seed + set_index - 1L,
            analysis_set = set_id, model_id = model_id,
            stage = "original_locked", success = TRUE, reason = "",
            metric = metric_names, estimate = as.numeric(metric[metric_names]),
            resample_n = length(index), resample_events = sum(y)
          )
        }, error = function(e) failure("original_locked", conditionMessage(e)))
        metric_position <- metric_position + 1L
        metric_output[[metric_position]] <- raw_result

        recal_result <- tryCatch(fit_recalibration(y, raw), error = identity)
        if (inherits(recal_result, "error")) {
          for (stage_name in c(
            "recalibration_intercept_only",
            "recalibration_intercept_and_slope"
          )) {
            metric_position <- metric_position + 1L
            metric_output[[metric_position]] <- failure(
              stage_name, conditionMessage(recal_result)
            )
          }
        } else {
          updated <- list(
            recalibration_intercept_only =
              recal_result$probability_intercept_only,
            recalibration_intercept_and_slope =
              recal_result$probability_intercept_and_slope
          )
          for (stage_name in names(updated)) {
            stage_result <- tryCatch({
              metric <- performance_vector(y, updated[[stage_name]])
              data.table(
                replicate = replicate_id, seed = seed + set_index - 1L,
                analysis_set = set_id, model_id = model_id,
                stage = stage_name, success = TRUE, reason = "",
                metric = metric_names,
                estimate = as.numeric(metric[metric_names]),
                resample_n = length(index), resample_events = sum(y)
              )
            }, error = function(e) failure(stage_name, conditionMessage(e)))
            metric_position <- metric_position + 1L
            metric_output[[metric_position]] <- stage_result
          }
        }
      }

      for (comparison_index in seq_len(nrow(set_comparisons))) {
        comparison <- set_comparisons[comparison_index]
        failure_difference <- function(reason) data.table(
          replicate = replicate_id, seed = seed + set_index - 1L,
          analysis_set = set_id,
          comparison_id = comparison$comparison_id,
          new_model = comparison$new_model,
          reference_model = comparison$reference_model,
          success = FALSE, reason = reason, metric = NA_character_,
          difference_new_minus_reference = NA_real_,
          resample_n = length(index), resample_events = sum(y)
        )
        if (!class_ok) {
          difference_position <- difference_position + 1L
          difference_output[[difference_position]] <- failure_difference(
            "single_outcome_class"
          )
          next
        }
        difference_result <- tryCatch({
          new_metric <- performance_vector(
            y, raw_probability[[comparison$new_model]][index]
          )
          reference_metric <- performance_vector(
            y, raw_probability[[comparison$reference_model]][index]
          )
          data.table(
            replicate = replicate_id, seed = seed + set_index - 1L,
            analysis_set = set_id,
            comparison_id = comparison$comparison_id,
            new_model = comparison$new_model,
            reference_model = comparison$reference_model,
            success = TRUE, reason = "", metric = metric_names,
            difference_new_minus_reference = as.numeric(
              new_metric[metric_names] - reference_metric[metric_names]
            ),
            resample_n = length(index), resample_events = sum(y)
          )
        }, error = function(e) failure_difference(conditionMessage(e)))
        difference_position <- difference_position + 1L
        difference_output[[difference_position]] <- difference_result
      }
    }
  }
  list(
    metric = rbindlist(metric_output, use.names = TRUE, fill = TRUE),
    difference = rbindlist(difference_output, use.names = TRUE, fill = TRUE)
  )
}

eicu_bootstrap <- run_eicu_cluster_bootstrap(
  eicu_analysis, model_specs, comparison_specs, development_fits,
  transform_bundle, repetitions = EICU_CLUSTER_BOOTSTRAP_REPS,
  seed = EICU_BOOTSTRAP_SEED
)
eicu_bootstrap_success <- eicu_bootstrap$metric[, .(
  successful_replicates = uniqueN(replicate[success == TRUE]),
  failed_replicates = EICU_CLUSTER_BOOTSTRAP_REPS -
    uniqueN(replicate[success == TRUE]),
  success_rate = uniqueN(replicate[success == TRUE]) /
    EICU_CLUSTER_BOOTSTRAP_REPS,
  first_failure_reason = {
    z <- reason[success == FALSE & !is.na(reason) & nzchar(reason)]
    if (length(z)) z[[1L]] else ""
  }
), by = .(model_id, analysis_set, stage)]
eicu_difference_success <- eicu_bootstrap$difference[, .(
  successful_replicates = uniqueN(replicate[success == TRUE]),
  failed_replicates = EICU_CLUSTER_BOOTSTRAP_REPS -
    uniqueN(replicate[success == TRUE]),
  success_rate = uniqueN(replicate[success == TRUE]) /
    EICU_CLUSTER_BOOTSTRAP_REPS,
  first_failure_reason = {
    z <- reason[success == FALSE & !is.na(reason) & nzchar(reason)]
    if (length(z)) z[[1L]] else ""
  }
), by = .(comparison_id, analysis_set, new_model, reference_model)]
if (any(eicu_bootstrap_success$success_rate < BOOTSTRAP_SUCCESS_THRESHOLD) ||
    any(eicu_difference_success$success_rate < BOOTSTRAP_SUCCESS_THRESHOLD)) {
  stop("An eICU hospital-cluster bootstrap fell below the prefrozen 95% rule.")
}

external_ci <- eicu_bootstrap$metric[success == TRUE, {
  interval <- percentile_interval(estimate)
  list(
    ci_lower = interval[["lower"]], ci_upper = interval[["upper"]],
    successful_replicates = uniqueN(replicate)
  )
}, by = .(model_id, analysis_set, stage, metric)]
external_performance <- merge(
  external_performance_point, external_ci,
  by = c("model_id", "analysis_set", "stage", "metric"),
  all.x = TRUE, sort = FALSE
)
external_performance[, metric_order := match(metric, metric_names)]
setorder(external_performance, stage_order, model_id, metric_order)
external_performance[, metric_order := NULL]

external_difference_point <- rbindlist(lapply(
  seq_len(nrow(comparison_specs)), function(i) {
    comparison <- comparison_specs[i]
    new <- external_predictions[[comparison$new_model]]
    reference <- external_predictions[[comparison$reference_model]]
    if (is.null(new) || is.null(reference)) {
      return(data.table(
        comparison_id = comparison$comparison_id,
        analysis_set = comparison$analysis_set,
        new_model = comparison$new_model,
        reference_model = comparison$reference_model,
        metric = metric_names, estimate_new_minus_reference = NA_real_,
        status = "NOT_ESTIMABLE"
      ))
    }
    if (!identical(new$analysis_id, reference$analysis_id) ||
        !identical(new$outcome, reference$outcome)) {
      stop("External paired comparison ID/outcome ordering differs: ",
        comparison$comparison_id
      )
    }
    new_metric <- performance_vector(new$outcome, new$probability_raw)
    reference_metric <- performance_vector(
      reference$outcome, reference$probability_raw
    )
    data.table(
      comparison_id = comparison$comparison_id,
      analysis_set = comparison$analysis_set,
      new_model = comparison$new_model,
      reference_model = comparison$reference_model,
      metric = metric_names,
      estimate_new_minus_reference = as.numeric(
        new_metric[metric_names] - reference_metric[metric_names]
      ),
      status = "ESTIMABLE"
    )
  }
), use.names = TRUE, fill = TRUE)
external_difference_ci <- eicu_bootstrap$difference[success == TRUE, {
  interval <- percentile_interval(difference_new_minus_reference)
  list(
    ci_lower = interval[["lower"]], ci_upper = interval[["upper"]],
    successful_replicates = uniqueN(replicate)
  )
}, by = .(
  comparison_id, analysis_set, new_model, reference_model, metric
)]
external_differences <- merge(
  external_difference_point, external_difference_ci,
  by = c(
    "comparison_id", "analysis_set", "new_model", "reference_model", "metric"
  ), all.x = TRUE, sort = FALSE
)

# ---------------------------------------------------------------------------
# Private row-level/model artifacts, aggregate results, QC, and atomic PASS gate.
# ---------------------------------------------------------------------------

bootstrap_success_qc <- rbindlist(list(
  mimic_bootstrap_success[, .(
    validation = "MIMIC_patient_optimism", model_id, analysis_set,
    stage_or_comparison = "refit_pipeline",
    requested_replicates = MIMIC_BOOTSTRAP_REPS,
    successful_replicates, failed_replicates, success_rate,
    threshold = BOOTSTRAP_SUCCESS_THRESHOLD,
    rule_applicable = !is.na(success_rate), first_failure_reason
  )],
  eicu_bootstrap_success[, .(
    validation = "eICU_hospital_cluster_metric", model_id, analysis_set,
    stage_or_comparison = stage,
    requested_replicates = EICU_CLUSTER_BOOTSTRAP_REPS,
    successful_replicates, failed_replicates, success_rate,
    threshold = BOOTSTRAP_SUCCESS_THRESHOLD,
    rule_applicable = TRUE, first_failure_reason
  )],
  eicu_difference_success[, .(
    validation = "eICU_hospital_cluster_paired_difference",
    model_id = paste0(new_model, "_vs_", reference_model), analysis_set,
    stage_or_comparison = comparison_id,
    requested_replicates = EICU_CLUSTER_BOOTSTRAP_REPS,
    successful_replicates, failed_replicates, success_rate,
    threshold = BOOTSTRAP_SUCCESS_THRESHOLD,
    rule_applicable = TRUE, first_failure_reason
  )]
), use.names = TRUE, fill = TRUE)
if (any(bootstrap_success_qc[
  rule_applicable == TRUE,
  is.na(success_rate) | success_rate < BOOTSTRAP_SUCCESS_THRESHOLD
])) {
  stop("Final bootstrap success QC failed the prefrozen >=95% rule.")
}

join_qc <- data.table(
  database = c("MIMIC-IV_v3.1", "eICU-CRD_v2.0"),
  predictor_frame_n = c(nrow(mimic_frame), nrow(eicu_frame)),
  outcome_frame_n = c(nrow(mimic_outcomes), nrow(eicu_outcomes)),
  joined_n = c(nrow(mimic_analysis), nrow(eicu_analysis)),
  predictor_unique_analysis_id = c(
    !anyDuplicated(mimic_frame$analysis_id),
    !anyDuplicated(eicu_frame$analysis_id)
  ),
  outcome_unique_analysis_id = c(
    !anyDuplicated(mimic_outcome_link$analysis_id),
    !anyDuplicated(eicu_outcome_link$analysis_id)
  ),
  exact_id_set_equality = TRUE,
  cardinality_preserved = TRUE,
  patient_cluster_nonmissing = c(
    !anyNA(mimic_analysis$patient_cluster_id), NA
  ),
  one_stay_per_patient = c(
    !anyDuplicated(mimic_analysis$patient_cluster_id), NA
  ),
  join_rule = c(
    "analysis_id exactly equals MIMIC stay_id",
    "analysis_id exactly equals eICU patientunitstayid"
  )
)

scope_qc <- data.table(
  analysis_extension = c(
    "multiple_imputation", "selection_weighting", "center_heterogeneity",
    "random_split", "stepwise_selection", "outcome_driven_recalibration_of_raw"
  ),
  implemented_in_phase3b = FALSE,
  note = c(
    "deferred_to_separately_locked_sensitivity",
    "deferred_to_separately_locked_sensitivity",
    "deferred_to_separately_locked_center_analysis",
    "bootstrap_validation_only",
    "all_model_forms_locked_pre_outcome",
    "raw_external_predictions_fixed_before_separate_labeled_updating"
  )
)

design_column_manifest <- unique(model_matrix_qc[, .(
  model_id, analysis_set, columns_n, column_signature
)])
if (design_column_manifest[, uniqueN(column_signature), by = model_id][
  , any(V1 != 1L)
]) {
  stop("Development/external design-column signature mismatch.")
}

private_predictions <- rbindlist(list(
  development_prediction_long, external_prediction_long
), use.names = TRUE, fill = TRUE)
if (anyDuplicated(private_predictions[, .(database, model_id, analysis_id)])) {
  stop("Private prediction artifact is not unique by database/model/analysis ID.")
}

model_bundle <- list(
  artifact_version = "mimic_locked_models_v1",
  config_version = LOCKED$version,
  checkpoint_sha256 = sha256_file(checkpoint_path),
  parameter_gate_sha256 = sha256_file(parameter_gate_path),
  outcome_gate_sha256 = sha256_file(outcome_gate_path),
  analysis_manifest_sha256 = sha256_file(analysis_manifest_path),
  model_utils_sha256 = sha256_file(model_utils_path),
  analysis_utils_sha256 = sha256_file(analysis_utils_path),
  frozen_parameter_rds_sha256 = sha256_file(parameter_artifact_paths$parameter),
  model_specification = model_specs,
  comparison_specification = comparison_specs,
  design_column_manifest = design_column_manifest,
  development_fits = development_fits,
  fits = development_fits,
  transform_bundle = transform_bundle,
  recalibration_parameters = recalibration_parameters,
  flexible_calibration_rule = list(
    method = "logistic_natural_spline_of_locked_model_linear_predictor",
    prediction_only_knot_probs = FLEXIBLE_CALIBRATION_KNOT_PROBS,
    grid_n = FLEXIBLE_CALIBRATION_GRID_N,
    grid_range = "observed_locked_linear_predictor_range",
    confidence_intervals = FALSE,
    used_for_model_selection = FALSE
  ),
  external_evidence_tier = external_evidence_tier,
  likelihood_ratio_test = likelihood_ratio_result,
  bootstrap_rules = list(
    mimic_repetitions = MIMIC_BOOTSTRAP_REPS,
    mimic_seed = MIMIC_BOOTSTRAP_SEED,
    mimic_sampling_unit = "MIMIC_subject_id_patient_cluster",
    mimic_one_stay_per_patient_required = TRUE,
    eicu_repetitions = EICU_CLUSTER_BOOTSTRAP_REPS,
    eicu_seed = EICU_BOOTSTRAP_SEED,
    success_threshold = BOOTSTRAP_SUCCESS_THRESHOLD,
    percentile_ci = BOOTSTRAP_CI_PROBS
  )
)
attr(private_predictions, "rebuild_metadata") <- list(
  artifact_version = "locked_model_predictions_v1",
  private_row_level = TRUE,
  exact_analysis_id_join = TRUE,
  mimic_patient_cluster_id_derived_from_subject_id = TRUE,
  raw_external_predictions_precede_recalibration = TRUE
)
bootstrap_bundle <- list(
  artifact_version = "locked_model_validation_bootstrap_v1",
  mimic_patient_bootstrap = mimic_bootstrap,
  eicu_hospital_cluster_metric_bootstrap = eicu_bootstrap$metric,
  eicu_hospital_cluster_paired_difference_bootstrap =
    eicu_bootstrap$difference,
  success_qc = bootstrap_success_qc
)

aggregate_paths <- c(
  model_specification = file.path(aggregate_out, "locked_model_specification_v1.csv"),
  population_counts = file.path(aggregate_out, "analysis_population_counts_v1.csv"),
  external_evidence_tier = file.path(
    aggregate_out, "eicu_external_evidence_tier_v1.csv"
  ),
  model_status = file.path(aggregate_out, "model_status_v1.csv"),
  coefficients = file.path(aggregate_out, "model_coefficients_OR_v1.csv"),
  contrasts = file.path(aggregate_out, "clinical_effect_contrasts_OR_v1.csv"),
  mimic_performance = file.path(
    aggregate_out, "mimic_apparent_optimism_corrected_performance_v1.csv"
  ),
  eicu_performance = file.path(
    aggregate_out, "eicu_external_validation_performance_v1.csv"
  ),
  eicu_differences = file.path(
    aggregate_out, "eicu_paired_metric_differences_v1.csv"
  ),
  recalibration = file.path(
    aggregate_out, "eicu_recalibration_parameters_v1.csv"
  ),
  flexible_calibration = file.path(
    aggregate_out, "flexible_calibration_curves_v1.csv"
  ),
  lr_test = file.path(aggregate_out, "likelihood_ratio_test_v1.csv"),
  design_manifest = file.path(aggregate_out, "design_column_manifest_v1.csv")
)
qc_paths <- c(
  input_gate = file.path(qc_out, "input_gate_hash_validation.csv"),
  exact_join = file.path(qc_out, "exact_analysis_id_join_QC.csv"),
  model_matrix = file.path(qc_out, "model_matrix_QC.csv"),
  bootstrap = file.path(qc_out, "bootstrap_success_QC.csv"),
  external_evidence_tier = file.path(
    qc_out, "eicu_external_evidence_tier_QC.csv"
  ),
  flexible_calibration = file.path(
    qc_out, "flexible_calibration_curve_QC.csv"
  ),
  scope = file.path(qc_out, "analysis_scope_QC.csv"),
  summary = file.path(qc_out, "locked_model_QC.md")
)
all_planned_outputs <- c(
  published_private, aggregate_paths, qc_paths,
  file.path(qc_out, "aggregate_output_manifest_v1.csv")
)
if (any(file.exists(all_planned_outputs))) {
  stop("A planned Phase 3b output already exists without its PASS gate.")
}
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

atomic_fwrite_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  fwrite(object, tmp)
  if (!file.rename(tmp, path)) stop("Could not atomically publish ", path)
  invisible(path)
}

atomic_save_rds_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  saveRDS(object, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically publish ", path)
  invisible(path)
}

atomic_write_lines_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(object, tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) stop("Could not atomically publish ", path)
  invisible(path)
}

atomic_save_rds_new(model_bundle, model_rds_path)
atomic_save_rds_new(private_predictions, prediction_rds_path)
atomic_save_rds_new(bootstrap_bundle, bootstrap_rds_path)

atomic_fwrite_new(model_specs, aggregate_paths[["model_specification"]])
atomic_fwrite_new(population_counts, aggregate_paths[["population_counts"]])
atomic_fwrite_new(
  external_evidence_tier, aggregate_paths[["external_evidence_tier"]]
)
atomic_fwrite_new(model_status, aggregate_paths[["model_status"]])
atomic_fwrite_new(coefficient_results, aggregate_paths[["coefficients"]])
atomic_fwrite_new(clinical_contrasts, aggregate_paths[["contrasts"]])
atomic_fwrite_new(mimic_optimism_summary, aggregate_paths[["mimic_performance"]])
atomic_fwrite_new(external_performance, aggregate_paths[["eicu_performance"]])
atomic_fwrite_new(external_differences, aggregate_paths[["eicu_differences"]])
atomic_fwrite_new(recalibration_parameters, aggregate_paths[["recalibration"]])
atomic_fwrite_new(
  flexible_calibration_curves, aggregate_paths[["flexible_calibration"]]
)
atomic_fwrite_new(likelihood_ratio_result, aggregate_paths[["lr_test"]])
atomic_fwrite_new(design_column_manifest, aggregate_paths[["design_manifest"]])

atomic_fwrite_new(input_gate_qc, qc_paths[["input_gate"]])
atomic_fwrite_new(join_qc, qc_paths[["exact_join"]])
atomic_fwrite_new(model_matrix_qc, qc_paths[["model_matrix"]])
atomic_fwrite_new(bootstrap_success_qc, qc_paths[["bootstrap"]])
atomic_fwrite_new(
  evidence_tier_qc, qc_paths[["external_evidence_tier"]]
)
atomic_fwrite_new(
  flexible_calibration_qc, qc_paths[["flexible_calibration"]]
)
atomic_fwrite_new(scope_qc, qc_paths[["scope"]])

summary_lines <- c(
  "# Locked complete-case model analysis QC",
  "",
  paste0("- Configuration: ", LOCKED$version),
  paste0("- MIMIC patient bootstrap: ", MIMIC_BOOTSTRAP_REPS,
    " replicates; seed ", MIMIC_BOOTSTRAP_SEED),
  "- MIMIC bootstrap samples patient_cluster_id derived from subject_id; exactly one analysis stay per patient is required.",
  paste0("- eICU hospital-cluster bootstrap: ", EICU_CLUSTER_BOOTSTRAP_REPS,
    " replicates; base seed ", EICU_BOOTSTRAP_SEED),
  paste0("- Prefrozen minimum bootstrap success: ",
    100 * BOOTSTRAP_SUCCESS_THRESHOLD, "%"),
  "- Primary common set: S0/S1/S2/S3/S2M.",
  "- Secondary S3NL uses the same primary common set and the frozen four-knot sMP spline; it never replaces linear S3.",
  "- Component common set: S3c/S4/S5; S5 non-estimability is reported, not rescued.",
  "- Normalized common set: linear frozen-SD absolute sMP versus sMP/PBW.",
  "- D057 common set: R2/R3 omit GCS and change no other model rule.",
  "- Six S0 continuous variables use frozen three-knot bases; primary/component/D057 sMP terms are linear per 5 J/min.",
  "- Original locked eICU validation is listed before intercept-only and intercept+slope updating.",
  paste0(
    "- eICU evidence tiers (precision labels only): ",
    paste(
      paste(external_evidence_tier$analysis_set,
        external_evidence_tier$evidence_tier, sep = "="),
      collapse = "; "
    ), "."
  ),
  "- Flexible calibration uses prediction-only 5/35/65/95% LP knots and a 101-point observed-range grid; curves are point estimates without confidence intervals and are not used for model selection.",
  "- MIMIC optimism-corrected interval columns are apparent-minus-percentile-optimism intervals, not full sampling confidence intervals.",
  "- Likelihood-ratio testing is restricted to S2 versus S2M.",
  "- MI, selection weighting, and center heterogeneity are not implemented here.",
  paste0("- S5 status: ", model_status[model_id == "S5", status]),
  "",
  "BUILD_COMPLETE"
)
atomic_write_lines_new(summary_lines, qc_paths[["summary"]])

aggregate_manifest <- data.table(
  output_name = names(aggregate_paths),
  path = vapply(aggregate_paths, function(path) {
    substring(path, nchar(project_from_script) + 2L)
  }, character(1L)),
  sha256 = vapply(aggregate_paths, sha256_file, character(1L)),
  row_level_identifier_columns = FALSE
)
aggregate_manifest_path <- file.path(qc_out, "aggregate_output_manifest_v1.csv")
atomic_fwrite_new(aggregate_manifest, aggregate_manifest_path)

identifier_headers <- c(
  "analysis_id", "patient_cluster_id", "stay_id", "subject_id", "hadm_id",
  "patientunitstayid",
  "patienthealthsystemstayid", "person_key", "hospital_id", "hospitalid"
)
public_csv_paths <- c(aggregate_paths, qc_paths[names(qc_paths) != "summary"],
  aggregate_manifest_path
)
public_headers <- unique(unlist(lapply(public_csv_paths, function(path) {
  names(fread(path, nrows = 0L, showProgress = FALSE))
})))
if (any(public_headers %in% identifier_headers)) {
  stop("A row-level identifier header entered an aggregate/QC CSV.")
}
if (!identical(tail(readLines(qc_paths[["summary"]], warn = FALSE), 1L),
  "BUILD_COMPLETE")) {
  stop("Phase 3b summary sentinel is missing.")
}

completion <- data.table(
  field = c(
    "status", "config_version", "completed_at", "script_sha256",
    "model_utils_sha256", "analysis_utils_sha256",
    "checkpoint_sha256", "analysis_script_manifest_sha256",
    "parameter_freeze_gate_sha256", "outcome_gate_sha256",
    "parameter_rds_sha256", "mimic_model_frame_rds_sha256",
    "eicu_model_frame_rds_sha256", "mimic_outcome_rds_sha256",
    "eicu_outcome_rds_sha256", "model_rds_sha256",
    "prediction_rds_sha256", "bootstrap_rds_sha256",
    "aggregate_manifest_sha256", "mimic_bootstrap_repetitions",
    "mimic_bootstrap_seed", "eicu_cluster_bootstrap_repetitions",
    "eicu_bootstrap_base_seed", "bootstrap_success_threshold",
    "all_applicable_bootstrap_success_rates_pass",
    "mimic_patient_cluster_key", "mimic_one_stay_per_patient",
    "flexible_calibration_method", "flexible_calibration_grid_n",
    "flexible_calibration_point_estimates_only",
    "external_evidence_tier_rule_pass", "external_evidence_tiers",
    "raw_external_reported_before_recalibration", "likelihood_ratio_test_count",
    "likelihood_ratio_test_only_S2_vs_S2M", "S5_status",
    "multiple_imputation_implemented", "selection_weighting_implemented",
    "center_heterogeneity_implemented", "all_input_gate_checks_pass",
    "all_exact_join_checks_pass", "all_required_models_or_allowed_S5_pass",
    "all_required_outputs_present", "summary_sentinel"
  ),
  value = c(
    "PASS", LOCKED$version, format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    sha256_file(script_path), sha256_file(model_utils_path),
    sha256_file(analysis_utils_path), sha256_file(checkpoint_path),
    sha256_file(analysis_manifest_path), sha256_file(parameter_gate_path),
    sha256_file(outcome_gate_path),
    sha256_file(parameter_artifact_paths$parameter),
    sha256_file(parameter_artifact_paths$mimic_frame),
    sha256_file(parameter_artifact_paths$eicu_frame),
    sha256_file(mimic_outcome_path), sha256_file(eicu_outcome_path),
    sha256_file(model_rds_path), sha256_file(prediction_rds_path),
    sha256_file(bootstrap_rds_path), sha256_file(aggregate_manifest_path),
    MIMIC_BOOTSTRAP_REPS, MIMIC_BOOTSTRAP_SEED,
    EICU_CLUSTER_BOOTSTRAP_REPS, EICU_BOOTSTRAP_SEED,
    BOOTSTRAP_SUCCESS_THRESHOLD,
    all(bootstrap_success_qc[
      rule_applicable == TRUE, success_rate >= BOOTSTRAP_SUCCESS_THRESHOLD
    ]),
    "subject_id_as_patient_cluster_id",
    !anyNA(mimic_analysis$patient_cluster_id) &&
      !anyDuplicated(mimic_analysis$patient_cluster_id),
    "logistic_natural_spline_locked_lp_prediction_knots_05_35_65_95",
    FLEXIBLE_CALIBRATION_GRID_N,
    all(flexible_calibration_qc$confidence_interval_absent) &&
      all(flexible_calibration_qc$point_estimate_label_pass) &&
      all(flexible_calibration_qc$excluded_from_model_selection),
    all(evidence_tier_qc$rule_recomputed_pass) &&
      all(!evidence_tier_qc$population_or_model_changed),
    paste(
      paste(external_evidence_tier$analysis_set,
        external_evidence_tier$evidence_tier, sep = "="),
      collapse = ";"
    ),
    TRUE, nrow(likelihood_ratio_result),
    identical(likelihood_ratio_result$comparison_id, "S2M_minus_S2"),
    model_status[model_id == "S5", status], FALSE, FALSE, FALSE,
    all(input_gate_qc$pass),
    all(join_qc$cardinality_preserved) &&
      all(join_qc$patient_cluster_nonmissing, na.rm = TRUE) &&
      all(join_qc$one_stay_per_patient, na.rm = TRUE),
    all(model_status$model_id == "S5" |
      model_status$status == "ESTIMABLE"),
    all(file.exists(c(
      published_private, aggregate_paths, qc_paths, aggregate_manifest_path
    ))),
    "BUILD_COMPLETE"
  )
)
if (anyDuplicated(completion$field)) stop("Duplicate Phase 3b gate field.")
completion_tmp <- paste0(completion_gate, ".tmp.", Sys.getpid())
unlink(completion_tmp, force = TRUE)
fwrite(completion, completion_tmp)
if (!file.rename(completion_tmp, completion_gate)) {
  unlink(completion_tmp, force = TRUE)
  stop("Could not atomically publish Phase 3b locked-model PASS gate.")
}

message("Locked complete-case model analysis complete.")
message("  MIMIC bootstrap replicates: ", MIMIC_BOOTSTRAP_REPS)
message("  eICU hospital-cluster bootstrap replicates: ",
  EICU_CLUSTER_BOOTSTRAP_REPS)
message("  S5 status: ", model_status[model_id == "S5", status])
message("  gate: ", completion_gate)
