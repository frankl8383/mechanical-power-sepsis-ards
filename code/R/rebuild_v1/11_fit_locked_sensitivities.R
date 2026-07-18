#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: locked main-tuple sensitivity analyses
#
# GOVERNANCE WARNING
# ------------------
# This script is outcome-bearing. It must be executed only after the formal
# outcome-unblinding checkpoint, Phase 3a outcome PASS gate, and Phase 3b locked
# main-model PASS gate exist. Before authorization use parse(file=...) only.
# Never source this complete script for testing.
#
# In-scope analyses are deliberately limited to main-tuple sensitivities that
# can be implemented without rebuilding a cohort: MIMIC warning-free primary
# tuples; the D058 primary-tuple preferred-source restriction; eICU age-topcode
# exclusion; full and supported-hospital selection-weighted targets; secondary
# mortality endpoints; and the three prespecified MIMIC PBW definitions.
# MI, center heterogeneity, native scores, +/-30-minute tuples, and infection
# window variants are not implemented here.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/11_fit_locked_sensitivities.R", mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
project_from_script <- normalizePath(
  file.path(script_dir, "..", "..", ".."), mustWork = TRUE
)
expected_config_version <- "1.0.1"

sha256_file <- function(path) {
  out <- system2(
    "shasum", c("-a", "256", shQuote(path)), stdout = TRUE, stderr = TRUE
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

strict_integer_key <- function(x, label) {
  z <- as.character(x)
  if (anyNA(z) || any(!grepl("^[0-9]+$", z))) {
    stop("Non-integer or missing linkage key in ", label)
  }
  value <- suppressWarnings(as.integer(z))
  if (anyNA(value) || any(as.character(value) != z)) {
    stop("Lossy integer linkage conversion in ", label)
  }
  value
}

same_numeric <- function(x, y, tolerance = 1e-12) {
  length(x) == length(y) && all(
    (is.na(x) & is.na(y)) |
      (!is.na(x) & !is.na(y) & abs(as.numeric(x) - as.numeric(y)) <= tolerance)
  )
}

# The authorization checkpoint is intentionally the first project artifact
# opened. No config, predictor RDS, model RDS, or outcome RDS precedes it.
checkpoint_path <- file.path(
  project_from_script, "analysis_rebuild_v1", "qc", "unblinding",
  "outcome_unblinding_checkpoint_v1.csv"
)
if (!file.exists(checkpoint_path)) {
  stop(
    "Locked sensitivities are not authorized: missing checkpoint ",
    checkpoint_path, ". No outcome artifact was opened."
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
model_utils_path <- file.path(script_dir, "08_model_utils.R")
analysis_utils_path <- file.path(script_dir, "08a_locked_analysis_utils.R")
outcome_script_path <- file.path(script_dir, "09_extract_rebuilt_outcomes.R")
main_model_script_path <- file.path(script_dir, "10_fit_locked_models.R")
parameter_script_path <- file.path(script_dir, "07_freeze_predictor_parameters.R")
selection_script_path <- file.path(script_dir, "07b_build_selection_weights.R")
warning_script_path <- file.path(
  script_dir, "03b_build_mimic_warning_free_sensitivity.R"
)
mimic_exposure_script_path <- file.path(
  script_dir, "03_build_mimic_paired_exposure.R"
)
eicu_exposure_script_path <- file.path(
  script_dir, "04_build_eicu_paired_exposure.R"
)
mimic_severity_script_path <- file.path(
  script_dir, "05_build_mimic_severity_core.R"
)
eicu_severity_script_path <- file.path(
  script_dir, "06_build_eicu_severity_core.R"
)
decision_log_path <- file.path(
  project_from_script, "docs", "rebuild_v1", "analysis_decision_log.md"
)

require_map_value(
  checkpoint, "config_script_sha256", sha256_file(config_path),
  "authorization checkpoint"
)
source(config_path)
if (!identical(LOCKED$version, expected_config_version) ||
    !identical(normalizePath(PROJECT_ROOT, mustWork = TRUE), project_from_script)) {
  stop("Loaded config differs from the authorized project/configuration.")
}

locked_checkpoint_files <- c(
  outcome_extraction_script_sha256 = outcome_script_path,
  parameter_freeze_script_sha256 = parameter_script_path,
  selection_weights_script_sha256 = selection_script_path,
  mimic_severity_script_sha256 = mimic_severity_script_path,
  eicu_severity_script_sha256 = eicu_severity_script_path,
  model_utils_script_sha256 = model_utils_path,
  model_analysis_utils_script_sha256 = analysis_utils_path,
  model_analysis_script_sha256 = main_model_script_path,
  locked_sensitivities_script_sha256 = script_path,
  analysis_decision_log_sha256 = decision_log_path
)
if (any(!file.exists(locked_checkpoint_files))) {
  stop("An authorized frozen file is missing.")
}
for (field in names(locked_checkpoint_files)) {
  require_map_value(
    checkpoint, field, sha256_file(locked_checkpoint_files[[field]]),
    "authorization checkpoint"
  )
}

analysis_manifest_relative <- require_map_value(
  checkpoint, "analysis_script_manifest_path", label = "authorization checkpoint"
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
script_prefix <- paste0(normalizePath(script_dir), .Platform$file.sep)
if (anyDuplicated(analysis_paths) ||
    any(!grepl("^[0-9a-f]{64}$", analysis_hashes)) ||
    any(!startsWith(analysis_paths, script_prefix)) ||
    any(!grepl("\\.(R|r|py)$", analysis_paths))) {
  stop("Analysis manifest path/hash invariant failed.")
}
current_analysis_hashes <- unname(vapply(
  analysis_paths, sha256_file, character(1)
))
if (!identical(analysis_hashes, current_analysis_hashes)) {
  stop("A current analysis script differs from the authorized manifest.")
}
must_manifest <- normalizePath(c(
  model_utils_path, analysis_utils_path, outcome_script_path,
  main_model_script_path, script_path
), mustWork = TRUE)
if (length(setdiff(must_manifest, analysis_paths))) {
  stop("Authorized manifest must include the exact 08, 08a, 09, 10, and 11 scripts.")
}

# ---------------------------------------------------------------------------
# Deep upstream gate and artifact hash chain. No row-level artifact is read yet.
# ---------------------------------------------------------------------------

parameter_gate_path <- file.path(
  QC_ROOT, "parameter_freeze", "phase2e_parameter_freeze_complete_v1.csv"
)
selection_gate_path <- file.path(
  QC_ROOT, "selection_weights", "phase2d_selection_weights_complete_v1.csv"
)
mimic_severity_gate_path <- file.path(
  QC_ROOT, "mimic_severity", "phase2b_mimic_severity_complete_v1.csv"
)
eicu_severity_gate_path <- file.path(
  QC_ROOT, "eicu_severity", "phase2b_complete_v1.csv"
)
mimic_phase2_gate_path <- file.path(
  QC_ROOT, "mimic_exposure", "phase2_mimic_exposure_complete_v1.csv"
)
eicu_phase2_gate_path <- file.path(
  QC_ROOT, "eicu_exposure", "phase2_eicu_exposure_complete_v1.csv"
)
warning_gate_path <- file.path(
  QC_ROOT, "mimic_warning_sensitivity",
  "phase2c_mimic_warning_sensitivity_complete_v1.csv"
)
outcome_gate_path <- file.path(
  QC_ROOT, "outcomes", "phase3a_rebuilt_outcomes_complete_v1.csv"
)
main_model_gate_path <- file.path(
  QC_ROOT, "locked_models", "phase3b_locked_models_complete_v1.csv"
)
access_receipt_path <- file.path(
  dirname(checkpoint_path), "outcome_access_receipt_v1.csv"
)
gate_paths <- c(
  parameter_gate_path, selection_gate_path, mimic_severity_gate_path,
  eicu_severity_gate_path, mimic_phase2_gate_path, eicu_phase2_gate_path,
  warning_gate_path, outcome_gate_path, main_model_gate_path,
  access_receipt_path
)
if (any(!file.exists(gate_paths))) {
  stop("A required upstream PASS gate/access receipt is missing.")
}

parameter_gate <- read_completion_gate(parameter_gate_path, "parameter gate")
selection_gate <- read_completion_gate(selection_gate_path, "selection gate")
mimic_severity_gate <- read_completion_gate(
  mimic_severity_gate_path, "MIMIC severity gate"
)
eicu_severity_gate <- read_completion_gate(
  eicu_severity_gate_path, "eICU severity gate"
)
mimic_phase2_gate <- read_completion_gate(
  mimic_phase2_gate_path, "MIMIC Phase 2 gate"
)
eicu_phase2_gate <- read_completion_gate(eicu_phase2_gate_path, "eICU Phase 2 gate")
warning_gate <- read_completion_gate(warning_gate_path, "warning-free gate")
outcome_gate <- read_completion_gate(outcome_gate_path, "outcome gate")
main_model_gate <- read_completion_gate(main_model_gate_path, "main-model gate")
access_receipt <- read_completion_gate(access_receipt_path, "outcome receipt")

require_map_value(parameter_gate, "status", "PASS", "parameter gate")
require_map_value(
  parameter_gate, "locked_config_version", LOCKED$version, "parameter gate"
)
for (field in c(
  "all_tests_pass", "outcome_leakage_guard_pass", "all_required_qc_present"
)) require_map_value(parameter_gate, field, "TRUE", "parameter gate")
require_map_value(selection_gate, "status", "PASS", "selection gate")
require_map_value(selection_gate, "config_version", LOCKED$version, "selection gate")
require_map_value(
  selection_gate, "all_leakage_checks_pass", "TRUE", "selection gate"
)
require_map_value(
  selection_gate, "all_required_qc_present", "TRUE", "selection gate"
)
for (z in list(
  list(mimic_severity_gate, "MIMIC severity gate"),
  list(eicu_severity_gate, "eICU severity gate")
)) {
  require_map_value(z[[1L]], "status", "PASS", z[[2L]])
  require_map_value(z[[1L]], "config_version", LOCKED$version, z[[2L]])
}
for (z in list(
  list(mimic_phase2_gate, "MIMIC Phase 2 gate"),
  list(eicu_phase2_gate, "eICU Phase 2 gate")
)) {
  require_map_value(z[[1L]], "locked_config_version", LOCKED$version, z[[2L]])
  require_map_value(z[[1L]], "all_invariants_pass", "TRUE", z[[2L]])
  require_map_value(z[[1L]], "outcome_leakage_guard_pass", "TRUE", z[[2L]])
}
require_map_value(warning_gate, "status", "PASS", "warning-free gate")
require_map_value(
  warning_gate, "config_version", LOCKED$version, "warning-free gate"
)
require_map_value(outcome_gate, "status", "PASS", "outcome gate")
require_map_value(outcome_gate, "config_version", LOCKED$version, "outcome gate")
require_map_value(
  outcome_gate, "outcome_access_status", "FORMALLY_UNBLINDED", "outcome gate"
)
require_map_value(
  outcome_gate, "all_accounting_invariants_pass", "TRUE", "outcome gate"
)
require_map_value(main_model_gate, "status", "PASS", "main-model gate")
require_map_value(
  main_model_gate, "config_version", LOCKED$version, "main-model gate"
)
for (field in c(
  "all_input_gate_checks_pass", "all_exact_join_checks_pass",
  "all_required_models_or_allowed_S5_pass", "all_required_outputs_present"
)) require_map_value(main_model_gate, field, "TRUE", "main-model gate")
require_map_value(
  main_model_gate, "selection_weighting_implemented", "FALSE", "main-model gate"
)
require_map_value(
  main_model_gate, "summary_sentinel", "BUILD_COMPLETE", "main-model gate"
)
require_map_value(
  access_receipt, "status", "OUTCOME_ACCESS_INITIATED", "outcome receipt"
)
require_map_value(
  access_receipt, "config_version", LOCKED$version, "outcome receipt"
)

require_map_value(
  checkpoint, "parameter_freeze_gate_sha256", sha256_file(parameter_gate_path),
  "authorization checkpoint"
)
require_map_value(
  checkpoint, "selection_weights_gate_sha256", sha256_file(selection_gate_path),
  "authorization checkpoint"
)
require_map_value(
  outcome_gate, "checkpoint_sha256", sha256_file(checkpoint_path), "outcome gate"
)
require_map_value(
  outcome_gate, "access_receipt_sha256", sha256_file(access_receipt_path),
  "outcome gate"
)
require_map_value(
  outcome_gate, "analysis_script_manifest_sha256",
  sha256_file(analysis_manifest_path), "outcome gate"
)
require_map_value(
  outcome_gate, "parameter_freeze_gate_sha256",
  sha256_file(parameter_gate_path), "outcome gate"
)
require_map_value(
  outcome_gate, "selection_weights_gate_sha256",
  sha256_file(selection_gate_path), "outcome gate"
)
require_map_value(
  main_model_gate, "script_sha256", sha256_file(main_model_script_path),
  "main-model gate"
)
require_map_value(
  main_model_gate, "model_utils_sha256", sha256_file(model_utils_path),
  "main-model gate"
)
require_map_value(
  main_model_gate, "analysis_utils_sha256", sha256_file(analysis_utils_path),
  "main-model gate"
)
require_map_value(
  main_model_gate, "checkpoint_sha256", sha256_file(checkpoint_path),
  "main-model gate"
)
require_map_value(
  main_model_gate, "analysis_script_manifest_sha256",
  sha256_file(analysis_manifest_path), "main-model gate"
)
require_map_value(
  main_model_gate, "parameter_freeze_gate_sha256",
  sha256_file(parameter_gate_path),
  "main-model gate"
)
require_map_value(
  main_model_gate, "outcome_gate_sha256", sha256_file(outcome_gate_path),
  "main-model gate"
)
require_map_value(
  access_receipt, "checkpoint_sha256", sha256_file(checkpoint_path),
  "outcome receipt"
)
require_map_value(
  access_receipt, "script_sha256", sha256_file(outcome_script_path),
  "outcome receipt"
)

script_gate_checks <- list(
  list(parameter_gate, parameter_script_path, "parameter gate"),
  list(selection_gate, selection_script_path, "selection gate"),
  list(mimic_severity_gate, mimic_severity_script_path, "MIMIC severity gate"),
  list(eicu_severity_gate, eicu_severity_script_path, "eICU severity gate"),
  list(mimic_phase2_gate, mimic_exposure_script_path, "MIMIC Phase 2 gate"),
  list(eicu_phase2_gate, eicu_exposure_script_path, "eICU Phase 2 gate"),
  list(warning_gate, warning_script_path, "warning-free gate"),
  list(outcome_gate, outcome_script_path, "outcome gate")
)
for (z in script_gate_checks) {
  require_map_value(z[[1L]], "script_sha256", sha256_file(z[[2L]]), z[[3L]])
}

decision_lines <- readLines(decision_log_path, warn = FALSE)
decision_text <- paste(decision_lines, collapse = "\n")
required_decision_status <- c(
  D013 = "LOCKED", D039 = "LOCKED", D051 = "SECONDARY-LOCKED",
  D053 = "LOCKED-CORRECTION", D054 = "LOCKED",
  D055 = "SECONDARY-LOCKED", D058 = "SECONDARY-LOCKED"
)
for (decision_id in names(required_decision_status)) {
  if (!grepl(paste0("\\| ", decision_id, " \\|"), decision_text)) {
    stop("Authorized decision log lacks ", decision_id)
  }
  line <- grep(
    paste0("^\\| ", decision_id, " \\|"), decision_lines, value = TRUE
  )
  fields <- if (length(line) == 1L) {
    trimws(strsplit(line, "|", fixed = TRUE)[[1L]])
  } else character()
  if (length(fields) < 4L ||
      !identical(fields[[4L]], required_decision_status[[decision_id]])) {
    stop("Authorized decision status mismatch for ", decision_id)
  }
}

source(model_utils_path)
source(analysis_utils_path)
required_analysis_utils <- c(
  "parameter_to_transform_bundle", "build_design_matrix", "fit_model",
  "fit_weighted_model", "predict_model", "performance_vector",
  "weighted_performance_vector", "wald_contrast", "complete_finite"
)
if (!all(vapply(
  required_analysis_utils, exists, logical(1L), mode = "function"
))) stop("Locked 08a analysis utility interface is incomplete.")
if (!identical(
  locked_model_specification()[model_id == "S3", design_type],
  "s0_smp_per_5"
) || !identical(
  locked_model_specification()[model_id == "S3NL", design_type],
  "s0_smp_rcs4"
)) {
  stop("S3 must remain linear per 5 J/min and S3NL must remain secondary.")
}

# ---------------------------------------------------------------------------
# Resolve and hash every predictor-only sensitivity artifact before opening any
# outcome RDS. These checks also freeze exact ID linkage and D058 semantics.
# ---------------------------------------------------------------------------

parameter_artifact_fields <- list(
  parameter = c("parameter_rds_path", "parameter_rds_sha256"),
  mimic_frame = c("mimic_model_frame_rds_path", "mimic_model_frame_rds_sha256"),
  eicu_frame = c("eicu_model_frame_rds_path", "eicu_model_frame_rds_sha256")
)
parameter_artifact_paths <- lapply(parameter_artifact_fields, function(pair) {
  path <- resolve_project_path(
    require_map_value(parameter_gate, pair[[1L]], label = "parameter gate"),
    pair[[1L]], require_relative = TRUE
  )
  require_map_value(
    parameter_gate, pair[[2L]], sha256_file(path), "parameter gate"
  )
  path
})

mimic_severity_rds_path <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_paired_exposure_with_severity_core_v1.rds"
)
eicu_severity_rds_path <- file.path(
  PRIVATE_ROOT, "eicu", "eicu_paired_exposure_with_severity_core_v1.rds"
)
warning_free_rds_path <- file.path(
  PRIVATE_ROOT, "mimic",
  "mimic_paired_exposure_sensitivity_warning_free_selected_v1.rds"
)
mimic_preferred_reselected_path <- file.path(
  PRIVATE_ROOT, "mimic",
  "mimic_paired_exposure_sensitivity_preferred_60min_v1.rds"
)
eicu_preferred_reselected_path <- file.path(
  PRIVATE_ROOT, "eicu",
  "eicu_paired_exposure_sensitivity_preferred_60min_v1.rds"
)
mimic_weight_rds_path <- file.path(
  PRIVATE_ROOT, "selection_weights", "mimic_tuple_observation_weights_v1.rds"
)
eicu_weight_rds_path <- file.path(
  PRIVATE_ROOT, "selection_weights", "eicu_tuple_observation_weights_v1.rds"
)
eicu_support_weight_rds_path <- file.path(
  PRIVATE_ROOT, "selection_weights",
  "eicu_tuple_observation_weights_support_hospitals_v1.rds"
)
predictor_only_paths <- c(
  mimic_severity_rds_path, eicu_severity_rds_path, warning_free_rds_path,
  mimic_preferred_reselected_path, eicu_preferred_reselected_path,
  mimic_weight_rds_path, eicu_weight_rds_path, eicu_support_weight_rds_path
)
if (any(!file.exists(predictor_only_paths))) {
  stop("A required outcome-free sensitivity artifact is missing.")
}

require_map_value(
  mimic_severity_gate, "prediction_hsc_rds_sha256",
  sha256_file(mimic_severity_rds_path), "MIMIC severity gate"
)
require_map_value(
  eicu_severity_gate, "prediction_hsc_rds_sha256",
  sha256_file(eicu_severity_rds_path), "eICU severity gate"
)
require_map_value(
  warning_gate, "warning_free_rds_sha256", sha256_file(warning_free_rds_path),
  "warning-free gate"
)
require_map_value(
  warning_gate, "phase2_gate_sha256", sha256_file(mimic_phase2_gate_path),
  "warning-free gate"
)
require_map_value(
  mimic_phase2_gate, "sensitivity_preferred_60min_rds_sha256",
  sha256_file(mimic_preferred_reselected_path), "MIMIC Phase 2 gate"
)
require_map_value(
  eicu_phase2_gate, "sensitivity_preferred_60min_rds_sha256",
  sha256_file(eicu_preferred_reselected_path), "eICU Phase 2 gate"
)
for (z in list(
  list("mimic_output_rds_sha256", mimic_weight_rds_path),
  list("eicu_output_rds_sha256", eicu_weight_rds_path),
  list("eicu_support_output_rds_sha256", eicu_support_weight_rds_path)
)) {
  require_map_value(selection_gate, z[[1L]], sha256_file(z[[2L]]), "selection gate")
}

parameters <- readRDS(parameter_artifact_paths$parameter)
mimic_frame <- as.data.table(readRDS(parameter_artifact_paths$mimic_frame))
eicu_frame <- as.data.table(readRDS(parameter_artifact_paths$eicu_frame))
mimic_severity <- as.data.table(readRDS(mimic_severity_rds_path))
eicu_severity <- as.data.table(readRDS(eicu_severity_rds_path))
warning_free <- as.data.table(readRDS(warning_free_rds_path))
mimic_preferred_reselected <- as.data.table(
  readRDS(mimic_preferred_reselected_path)
)
eicu_preferred_reselected <- as.data.table(
  readRDS(eicu_preferred_reselected_path)
)
mimic_weight_source <- as.data.table(readRDS(mimic_weight_rds_path))
eicu_weight_source <- as.data.table(readRDS(eicu_weight_rds_path))
eicu_support_weight_source <- as.data.table(readRDS(eicu_support_weight_rds_path))

required_parameter_fields <- c(
  "artifact_version", "decision_id", "locked_config_version",
  "derivation_database", "quantile_type", "primary_predictors",
  "component_predictors", "normalized_exposure_predictors",
  "three_knot_values", "smp_knot_values", "smp_center_scale",
  "smp_per_pbw_center_scale", "canonical_model_frame_schema",
  "model_utils_sha256"
)
if (!is.list(parameters) ||
    length(setdiff(required_parameter_fields, names(parameters))) ||
    !identical(parameters$artifact_version, "frozen_predictor_parameters_v1") ||
    !identical(parameters$decision_id, "D054") ||
    !identical(parameters$locked_config_version, LOCKED$version) ||
    !identical(parameters$derivation_database, "MIMIC-IV v3.1 only") ||
    !identical(as.integer(parameters$quantile_type), 2L) ||
    !identical(parameters$model_utils_sha256, sha256_file(model_utils_path))) {
  stop("Frozen parameter artifact failed its internal provenance checks.")
}
transform_bundle <- parameter_to_transform_bundle(parameters)

canonical_schema <- as.character(parameters$canonical_model_frame_schema)
if (!identical(names(mimic_frame), canonical_schema) ||
    !identical(names(eicu_frame), canonical_schema) ||
    anyDuplicated(mimic_frame$analysis_id) || anyDuplicated(eicu_frame$analysis_id) ||
    anyNA(mimic_frame$analysis_id) || anyNA(eicu_frame$analysis_id)) {
  stop("Canonical predictor-frame schema/key invariant failed.")
}
if (nrow(mimic_frame) != as.integer(require_map_value(
  parameter_gate, "mimic_frame_n", label = "parameter gate"
)) || nrow(eicu_frame) != as.integer(require_map_value(
  parameter_gate, "eicu_frame_n", label = "parameter gate"
))) stop("Canonical frame count differs from the parameter gate.")

required_mimic_severity <- c(
  "stay_id", "prediction_time", "pplat_itemid", "peep_itemid", "vt_itemid",
  "rr_itemid", "delta_p", "rr_value", "smp",
  "vt_per_pbw_mL_per_kg", "smp_per_pbw_J_per_min_per_kg",
  "vt_per_pbw_omr_1y_fallback_mL_per_kg",
  "smp_per_pbw_omr_1y_fallback_J_per_min_per_kg",
  "vt_per_pbw_chartevents_only_mL_per_kg",
  "smp_per_pbw_chartevents_only_J_per_min_per_kg"
)
required_eicu_severity <- c(
  "patientunitstayid", "hospitalid", "prediction_time", "peep_source",
  "vt_source", "rr_source", "delta_p", "rr_value", "smp",
  "age_topcoded_gt89", "age_topcode_sensitivity_exclude",
  "vt_per_pbw_mL_per_kg", "smp_per_pbw_J_per_min_per_kg"
)
if (length(setdiff(required_mimic_severity, names(mimic_severity))) ||
    length(setdiff(required_eicu_severity, names(eicu_severity)))) {
  stop("A severity artifact lacks a required sensitivity field.")
}
if (anyDuplicated(mimic_severity$stay_id) ||
    anyDuplicated(eicu_severity$patientunitstayid) ||
    !setequal(mimic_frame$analysis_id, mimic_severity$stay_id) ||
    !setequal(eicu_frame$analysis_id, eicu_severity$patientunitstayid)) {
  stop("Severity/canonical analysis-ID invariant failed.")
}

# Canonical model-frame variables must remain exact projections of the final
# severity artifacts before any alternate PBW field is substituted.
mimic_projection <- mimic_severity[, .(
  analysis_id = as.integer(stay_id), delta_p_source = as.numeric(delta_p),
  rr_source = as.numeric(rr_value), smp_source = as.numeric(smp),
  vt_pbw_source = as.numeric(vt_per_pbw_mL_per_kg),
  smp_pbw_source = as.numeric(smp_per_pbw_J_per_min_per_kg)
)]
eicu_projection <- eicu_severity[, .(
  analysis_id = as.integer(patientunitstayid), delta_p_source = as.numeric(delta_p),
  rr_source = as.numeric(rr_value), smp_source = as.numeric(smp),
  vt_pbw_source = as.numeric(vt_per_pbw_mL_per_kg),
  smp_pbw_source = as.numeric(smp_per_pbw_J_per_min_per_kg)
)]
for (z in list(
  list(mimic_frame, mimic_projection, "MIMIC"),
  list(eicu_frame, eicu_projection, "eICU")
)) {
  joined <- merge(z[[1L]], z[[2L]], by = "analysis_id", all = FALSE, sort = TRUE)
  if (nrow(joined) != nrow(z[[1L]]) ||
      !same_numeric(joined$delta_p, joined$delta_p_source) ||
      !same_numeric(joined$rr, joined$rr_source) ||
      !same_numeric(joined$smp, joined$smp_source) ||
      !same_numeric(joined$vt_per_pbw, joined$vt_pbw_source) ||
      !same_numeric(joined$smp_per_pbw, joined$smp_pbw_source)) {
    stop(z[[3L]], " canonical exposure projection differs from severity source.")
  }
}

# D051: warning-free is a pure ID restriction of the already selected primary
# tuple. It must never reselect a later tuple or change its prediction time.
required_warning <- c(
  "stay_id", "prediction_time", "delta_p", "rr_value", "smp",
  "selected_tuple_any_warning", "selected_tuple_warning_count"
)
if (length(setdiff(required_warning, names(warning_free))) ||
    anyDuplicated(warning_free$stay_id) ||
    any(warning_free$selected_tuple_any_warning) ||
    nrow(warning_free) != as.integer(require_map_value(
      warning_gate, "warning_free_selected_tuple_n", label = "warning-free gate"
    ))) stop("Warning-free artifact invariant failed.")
warning_alignment <- merge(
  warning_free[, .(
    analysis_id = as.integer(stay_id), warning_prediction_time = prediction_time,
    warning_delta_p = delta_p, warning_rr = rr_value, warning_smp = smp
  )],
  mimic_severity[, .(
    analysis_id = as.integer(stay_id), primary_prediction_time = prediction_time,
    primary_delta_p = delta_p, primary_rr = rr_value, primary_smp = smp
  )],
  by = "analysis_id", all = FALSE, sort = TRUE
)
if (nrow(warning_alignment) != nrow(warning_free) ||
    !all(warning_alignment$warning_prediction_time ==
      warning_alignment$primary_prediction_time) ||
    !same_numeric(warning_alignment$warning_delta_p,
      warning_alignment$primary_delta_p) ||
    !same_numeric(warning_alignment$warning_rr, warning_alignment$primary_rr) ||
    !same_numeric(warning_alignment$warning_smp, warning_alignment$primary_smp)) {
  stop("D051 warning-free restriction changed the primary tuple/HSC time.")
}
warning_free_ids <- sort(as.integer(warning_free$stay_id))
if (length(warning_free_ids) != 6121L) stop("Unexpected D051 restriction count.")

# D058: outcome-modeled preferred-source sensitivity restricts the already
# selected primary tuple. The independently reselected Phase-2 artifacts are
# checked and explicitly marked blocked because their HSC time may differ.
mimic_preferred_ids <- sort(mimic_severity[
  pplat_itemid == 224696L & peep_itemid == 220339L &
    vt_itemid == 224685L & rr_itemid == 224690L,
  as.integer(stay_id)
])
eicu_preferred_ids <- sort(eicu_severity[
  peep_source == "PEEP" &
    vt_source %chin% c(
      "Tidal Volume Observed (VT)", "Exhaled TV (patient)",
      "Exhaled TV (machine)"
    ) & rr_source == "Total RR",
  as.integer(patientunitstayid)
])
if (length(mimic_preferred_ids) != 6578L ||
    length(eicu_preferred_ids) != 586L) {
  stop("D058 primary-tuple preferred-source counts changed.")
}

mimic_reselected <- mimic_preferred_reselected[tuple_observed == TRUE]
eicu_reselected <- eicu_preferred_reselected[tuple_observed == TRUE]
if (nrow(mimic_reselected) != as.integer(require_map_value(
  mimic_phase2_gate, "sensitivity_preferred_60min_n",
  label = "MIMIC Phase 2 gate"
)) || nrow(eicu_reselected) != as.integer(require_map_value(
  eicu_phase2_gate, "sensitivity_preferred_60min_n",
  label = "eICU Phase 2 gate"
))) stop("Reselected preferred feasibility count differs from Phase 2.")

audit_reselected <- function(
    preferred, primary, id, modeled_ids, database, expected_mismatch) {
  required <- c(id, "prediction_time", "delta_p", "rr_value", "smp")
  if (length(setdiff(required, names(preferred))) ||
      length(setdiff(required, names(primary))) ||
      anyDuplicated(preferred[[id]]) || anyNA(preferred[[id]]) ||
      anyDuplicated(primary[[id]]) || anyNA(primary[[id]]) ||
      anyDuplicated(modeled_ids) || anyNA(modeled_ids) ||
      anyNA(preferred$prediction_time) || anyNA(primary$prediction_time)) {
    stop(database, " malformed D058 preferred-source audit input.")
  }
  p <- preferred[, c(
    id, "prediction_time", "delta_p", "rr_value", "smp"
  ), with = FALSE]
  s <- primary[, c(
    id, "prediction_time", "delta_p", "rr_value", "smp"
  ), with = FALSE]
  setnames(
    p, c("prediction_time", "delta_p", "rr_value", "smp"),
    paste0(c("prediction_time", "delta_p", "rr", "smp"), "_reselected")
  )
  setnames(
    s, c("prediction_time", "delta_p", "rr_value", "smp"),
    paste0(c("prediction_time", "delta_p", "rr", "smp"), "_primary")
  )
  joined <- merge(p, s, by = id, all = FALSE, sort = TRUE)
  mismatch_flag <-
    joined$prediction_time_reselected != joined$prediction_time_primary
  if (anyNA(mismatch_flag)) {
    stop(database, " has indeterminate D058 prediction-time comparisons.")
  }
  mismatch <- sum(mismatch_flag)
  modeled_rows <- which(joined[[id]] %in% modeled_ids)
  modeled <- joined[modeled_rows]
  if (nrow(joined) != nrow(preferred) || mismatch != expected_mismatch ||
      nrow(modeled) != length(modeled_ids) ||
      any(modeled$prediction_time_reselected != modeled$prediction_time_primary) ||
      !same_numeric(modeled$delta_p_reselected, modeled$delta_p_primary) ||
      !same_numeric(modeled$rr_reselected, modeled$rr_primary) ||
      !same_numeric(modeled$smp_reselected, modeled$smp_primary)) {
    stop(database, " D058 source-restriction/reselection audit failed.")
  }
  data.table(
    database = database,
    primary_tuple_n = nrow(primary),
    modeled_primary_tuple_preferred_source_n = length(modeled_ids),
    reselected_preferred_tuple_n = nrow(preferred),
    reselected_prediction_time_mismatch_n = mismatch,
    modeled_restriction_prediction_time_mismatch_n = 0L,
    modeled_estimand = "primary_tuple_preferred_source_restriction",
    reselected_outcome_status = "BLOCKED_REQUIRES_PREFERRED_TIME_HSC_REBUILD"
  )
}
preferred_source_qc <- rbindlist(list(
  audit_reselected(
    mimic_reselected, mimic_severity, "stay_id", mimic_preferred_ids,
    "MIMIC-IV_v3.1", 57L
  ),
  audit_reselected(
    eicu_reselected, eicu_severity, "patientunitstayid", eicu_preferred_ids,
    "eICU-CRD_v2.0", 2L
  )
))

# D039 is a pure eICU restriction and must match the predictor age mapping.
if (anyNA(eicu_severity$age_topcoded_gt89) ||
    anyNA(eicu_severity$age_topcode_sensitivity_exclude) ||
    !identical(
      as.logical(eicu_severity$age_topcoded_gt89),
      as.logical(eicu_severity$age_topcode_sensitivity_exclude)
    ) || sum(eicu_severity$age_topcode_sensitivity_exclude) != 10L) {
  stop("D039 eICU age-topcode restriction invariant failed.")
}
eicu_non_topcoded_ids <- sort(eicu_severity[
  age_topcode_sensitivity_exclude == FALSE, as.integer(patientunitstayid)
])

# D055: independently recheck the stabilized-weight formula, observed-record
# 1st/99th percentile truncation, and exact observed-tuple ID link. This script
# consumes only the published truncated weights and never calls them corrected.
audit_weight_artifact <- function(
    x, database_label, expected_artifact_database,
    expected_metadata_database, expected_strict_n, expected_observed_n) {
  required <- c(
    "database", "source_stay_id", "source_hospital_id", "tuple_observed",
    "selection_probability",
    "stabilized_weight_untruncated", "stabilized_weight_truncated_1_99"
  )
  metadata <- attr(x, "rebuild_metadata")
  if (length(setdiff(required, names(x))) ||
      !identical(unique(as.character(x$database)), expected_artifact_database) ||
      !is.list(metadata) ||
      !identical(metadata$version, "tuple_observation_weights_v1") ||
      !identical(metadata$database, expected_metadata_database) ||
      !isTRUE(metadata$outcome_blind) ||
      nrow(x) != expected_strict_n ||
      sum(x$tuple_observed) != expected_observed_n ||
      anyDuplicated(x$source_stay_id) || anyNA(x$source_stay_id) ||
      anyNA(x$tuple_observed) || !all(x$tuple_observed %in% c(0L, 1L)) ||
      anyNA(x$selection_probability) ||
      any(x$selection_probability <= 0 | x$selection_probability >= 1)) {
    stop("Malformed selection-weight artifact: ", database_label)
  }
  observed <- x$tuple_observed == 1L
  if (!all(is.na(x$stabilized_weight_untruncated) == !observed) ||
      !all(is.na(x$stabilized_weight_truncated_1_99) == !observed)) {
    stop("Selection weights are not confined to observed tuples: ", database_label)
  }
  expected_untruncated <- mean(x$tuple_observed) /
    x$selection_probability[observed]
  cut <- as.numeric(quantile(
    expected_untruncated, c(0.01, 0.99), type = 2L, names = FALSE
  ))
  expected_truncated <- pmin(
    pmax(expected_untruncated, cut[[1L]]), cut[[2L]]
  )
  if (max(abs(
    expected_untruncated - x$stabilized_weight_untruncated[observed]
  )) > 2e-12 || max(abs(
    expected_truncated - x$stabilized_weight_truncated_1_99[observed]
  )) > 2e-12) {
    stop("D055 stabilized-weight/truncation formula mismatch: ", database_label)
  }
  observed_weights <- x[observed, .(
    analysis_id = strict_integer_key(source_stay_id, database_label),
    selection_probability,
    selection_weight = stabilized_weight_truncated_1_99
  )]
  if (anyDuplicated(observed_weights$analysis_id) ||
      anyNA(observed_weights$selection_weight) ||
      any(observed_weights$selection_weight <= 0) ||
      any(!is.finite(observed_weights$selection_weight))) {
    stop("Invalid linked observed weights: ", database_label)
  }
  ess <- sum(observed_weights$selection_weight)^2 /
    sum(observed_weights$selection_weight^2)
  list(
    observed = observed_weights,
    qc = data.table(
      database = database_label, strict_n = nrow(x),
      tuple_observed_n = sum(observed), marginal_observed = mean(observed),
      truncation_p01 = cut[[1L]], truncation_p99 = cut[[2L]],
      effective_sample_size = ess,
      ess_fraction = ess / sum(observed)
    )
  )
}

mimic_weight_audit <- audit_weight_artifact(
  mimic_weight_source, "MIMIC-IV_v3.1", "MIMIC-IV_v3.1",
  "MIMIC-IV_v3.1",
  as.integer(require_map_value(selection_gate, "mimic_strict_n",
    label = "selection gate")),
  as.integer(require_map_value(selection_gate, "mimic_tuple_observed_n",
    label = "selection gate"))
)
eicu_weight_audit <- audit_weight_artifact(
  eicu_weight_source, "eICU-CRD_v2.0_full_target", "eICU-CRD_v2.0",
  "eICU-CRD_v2.0",
  as.integer(require_map_value(selection_gate, "eicu_strict_n",
    label = "selection gate")),
  as.integer(require_map_value(selection_gate, "eicu_tuple_observed_n",
    label = "selection gate"))
)
eicu_support_weight_audit <- audit_weight_artifact(
  eicu_support_weight_source, "eICU-CRD_v2.0_supported_hospital_target",
  "eICU-CRD_v2.0", "eICU-CRD_v2.0_supported_hospitals",
  as.integer(require_map_value(
    selection_gate, "eicu_supported_hospital_strict_n", label = "selection gate"
  )),
  as.integer(require_map_value(
    selection_gate, "eicu_tuple_observed_n", label = "selection gate"
  ))
)
selection_weight_qc <- rbindlist(list(
  mimic_weight_audit$qc, eicu_weight_audit$qc,
  eicu_support_weight_audit$qc
))
if (!setequal(
  mimic_weight_audit$observed$analysis_id, mimic_frame$analysis_id
) || !setequal(
  eicu_weight_audit$observed$analysis_id, eicu_frame$analysis_id
) || !setequal(
  eicu_support_weight_audit$observed$analysis_id, eicu_frame$analysis_id
)) stop("D055 observed-weight IDs differ from the canonical tuple frames.")

eicu_support_hospitals <- unique(
  eicu_support_weight_source$source_hospital_id
)
eicu_support_hospitals <- eicu_support_hospitals[!is.na(eicu_support_hospitals)]
if (length(eicu_support_hospitals) != 33L ||
    uniqueN(eicu_weight_source$source_hospital_id) != 68L ||
    uniqueN(eicu_weight_source[
      tuple_observed == 0L &
        !source_hospital_id %chin% eicu_support_hospitals,
      source_hospital_id
    ]) != 35L ||
    sum(eicu_weight_source[
      !source_hospital_id %chin% eicu_support_hospitals, .N
    ]) != 585L ||
    any(eicu_weight_source[
      !source_hospital_id %chin% eicu_support_hospitals,
      tuple_observed != 0L
    ])) {
  stop("D055 eICU structural-support classification changed.")
}

# Predictor-side linkage fields are added before any outcome join.
mimic_frame <- merge(
  mimic_frame, mimic_weight_audit$observed,
  by = "analysis_id", all = FALSE, sort = FALSE
)
eicu_frame <- merge(
  eicu_frame,
  eicu_severity[, .(
    analysis_id = as.integer(patientunitstayid),
    hospital_id = as.character(hospitalid),
    age_topcode_sensitivity_exclude =
      as.logical(age_topcode_sensitivity_exclude)
  )],
  by = "analysis_id", all = FALSE, sort = FALSE
)
eicu_frame <- merge(
  eicu_frame,
  eicu_weight_audit$observed[, .(
    analysis_id, selection_probability_full = selection_probability,
    selection_weight_full = selection_weight
  )],
  by = "analysis_id", all = FALSE, sort = FALSE
)
eicu_frame <- merge(
  eicu_frame,
  eicu_support_weight_audit$observed[, .(
    analysis_id, selection_probability_supported = selection_probability,
    selection_weight_supported = selection_weight
  )],
  by = "analysis_id", all = FALSE, sort = FALSE
)
if (nrow(mimic_frame) != as.integer(require_map_value(
  parameter_gate, "mimic_frame_n", label = "parameter gate"
)) || nrow(eicu_frame) != as.integer(require_map_value(
  parameter_gate, "eicu_frame_n", label = "parameter gate"
)) || anyDuplicated(mimic_frame$analysis_id) ||
    anyDuplicated(eicu_frame$analysis_id) || anyNA(eicu_frame$hospital_id)) {
  stop("Predictor-side sensitivity linkage changed frame cardinality.")
}

# D053: create three exact MIMIC PBW definitions from the already time-aligned
# primary severity artifact. Alternative center/scale constants are derived
# only from outcome-free complete predictor sets before an outcome RDS is read.
pbw_source <- mimic_severity[, .(
  analysis_id = as.integer(stay_id),
  vt_pbw_5y = as.numeric(vt_per_pbw_mL_per_kg),
  smp_pbw_5y = as.numeric(smp_per_pbw_J_per_min_per_kg),
  vt_pbw_1y = as.numeric(vt_per_pbw_omr_1y_fallback_mL_per_kg),
  smp_pbw_1y = as.numeric(smp_per_pbw_omr_1y_fallback_J_per_min_per_kg),
  vt_pbw_chart = as.numeric(vt_per_pbw_chartevents_only_mL_per_kg),
  smp_pbw_chart = as.numeric(
    smp_per_pbw_chartevents_only_J_per_min_per_kg
  )
)]
mimic_frame_pbw <- merge(
  mimic_frame, pbw_source, by = "analysis_id", all = FALSE, sort = FALSE
)
if (nrow(mimic_frame_pbw) != nrow(mimic_frame) ||
    !same_numeric(mimic_frame_pbw$vt_per_pbw, mimic_frame_pbw$vt_pbw_5y) ||
    !same_numeric(mimic_frame_pbw$smp_per_pbw, mimic_frame_pbw$smp_pbw_5y)) {
  stop("Primary 5-year PBW fields differ from the canonical frame.")
}

make_pbw_definition <- function(frame, definition, vt_field, smp_field) {
  out <- copy(frame)
  out[, `:=`(
    vt_per_pbw = as.numeric(get(vt_field)),
    smp_per_pbw = as.numeric(get(smp_field))
  )]
  component_complete <- complete_finite(out, parameters$component_predictors)
  normalized_complete <- complete_finite(
    out, parameters$normalized_exposure_predictors
  )
  out[, `:=`(
    component_predictor_complete = component_complete,
    normalized_exposure_complete = normalized_complete,
    pbw_definition = definition
  )]
  common <- component_complete & normalized_complete
  if (!any(common)) stop("No PBW-complete rows for ", definition)
  scale <- c(
    mean = mean(out$smp_per_pbw[common]),
    sd = stats::sd(out$smp_per_pbw[common])
  )
  if (anyNA(scale) || any(!is.finite(scale)) || scale[["sd"]] <= 0) {
    stop("Invalid outcome-free sMP/PBW scale for ", definition)
  }
  bundle <- transform_bundle
  bundle$smp_per_pbw_center_scale <- scale
  validate_transform_bundle(bundle)
  list(
    frame = out,
    bundle = bundle,
    qc = data.table(
      pbw_definition = definition,
      tuple_n = nrow(out),
      component_complete_n = sum(component_complete),
      normalized_complete_n = sum(normalized_complete),
      component_normalized_common_n = sum(common),
      smp_per_pbw_mean = scale[["mean"]],
      smp_per_pbw_sd = scale[["sd"]],
      scale_derivation = paste(
        "outcome-free intersection of component-complete and",
        "normalized-exposure-complete MIMIC primary tuples"
      )
    )
  )
}

pbw_definitions <- list(
  omr_5y_primary = make_pbw_definition(
    mimic_frame_pbw, "omr_5y_primary", "vt_pbw_5y", "smp_pbw_5y"
  ),
  omr_1y_fallback = make_pbw_definition(
    mimic_frame_pbw, "omr_1y_fallback", "vt_pbw_1y", "smp_pbw_1y"
  ),
  chartevents_only = make_pbw_definition(
    mimic_frame_pbw, "chartevents_only", "vt_pbw_chart", "smp_pbw_chart"
  )
)
pbw_scale_qc <- rbindlist(lapply(pbw_definitions, `[[`, "qc"))
if (!same_numeric(
  pbw_definitions$omr_5y_primary$bundle$smp_per_pbw_center_scale,
  transform_bundle$smp_per_pbw_center_scale
) || !identical(
  pbw_scale_qc$component_normalized_common_n,
  c(737L, 579L, 33L)
)) stop("D053 PBW scale/count invariant failed.")

predictor_feasibility_qc <- data.table(
  check = c(
    "warning_free_primary_tuple_restriction_n",
    "mimic_primary_tuple_preferred_source_restriction_n",
    "eicu_primary_tuple_preferred_source_restriction_n",
    "mimic_reselected_preferred_prediction_time_mismatch_n",
    "eicu_reselected_preferred_prediction_time_mismatch_n",
    "eicu_topcoded_tuple_exclusion_n",
    "eicu_structural_zero_hospital_n",
    "eicu_structural_zero_patient_n",
    "pbw_5y_component_common_n", "pbw_1y_component_common_n",
    "pbw_chartevents_only_component_common_n"
  ),
  observed = c(
    length(warning_free_ids), length(mimic_preferred_ids),
    length(eicu_preferred_ids),
    preferred_source_qc[database == "MIMIC-IV_v3.1",
      reselected_prediction_time_mismatch_n],
    preferred_source_qc[database == "eICU-CRD_v2.0",
      reselected_prediction_time_mismatch_n],
    sum(eicu_severity$age_topcode_sensitivity_exclude),
    35L, 585L,
    pbw_scale_qc[pbw_definition == "omr_5y_primary",
      component_normalized_common_n],
    pbw_scale_qc[pbw_definition == "omr_1y_fallback",
      component_normalized_common_n],
    pbw_scale_qc[pbw_definition == "chartevents_only",
      component_normalized_common_n]
  ),
  expected = c(6121L, 6578L, 586L, 57L, 2L, 10L, 35L, 585L, 737L, 579L, 33L)
)
predictor_feasibility_qc[, pass := observed == expected]
if (any(!predictor_feasibility_qc$pass)) {
  stop("An outcome-free sensitivity feasibility invariant changed.")
}

# ---------------------------------------------------------------------------
# Only after every predictor-side and gate check passes may the outcome-bearing
# Phase 3b model bundle and Phase 3a outcome artifacts be opened.
# ---------------------------------------------------------------------------

main_model_rds_path <- file.path(
  PRIVATE_ROOT, "locked_models", "mimic_locked_models_v1.rds"
)
mimic_outcome_rds_path <- file.path(
  PRIVATE_ROOT, "outcomes", "mimic_rebuilt_outcomes_v1.rds"
)
eicu_outcome_rds_path <- file.path(
  PRIVATE_ROOT, "outcomes", "eicu_rebuilt_outcomes_v1.rds"
)
if (any(!file.exists(c(
  main_model_rds_path, mimic_outcome_rds_path, eicu_outcome_rds_path
)))) stop("A checksum-gated outcome/model RDS is missing.")
require_map_value(
  main_model_gate, "model_rds_sha256", sha256_file(main_model_rds_path),
  "main-model gate"
)
require_map_value(
  main_model_gate, "parameter_rds_sha256",
  sha256_file(parameter_artifact_paths$parameter), "main-model gate"
)
require_map_value(
  main_model_gate, "mimic_model_frame_rds_sha256",
  sha256_file(parameter_artifact_paths$mimic_frame), "main-model gate"
)
require_map_value(
  main_model_gate, "eicu_model_frame_rds_sha256",
  sha256_file(parameter_artifact_paths$eicu_frame), "main-model gate"
)
require_map_value(
  outcome_gate, "mimic_outcome_rds_sha256",
  sha256_file(mimic_outcome_rds_path), "outcome gate"
)
require_map_value(
  outcome_gate, "eicu_outcome_rds_sha256",
  sha256_file(eicu_outcome_rds_path), "outcome gate"
)
require_map_value(
  main_model_gate, "mimic_outcome_rds_sha256",
  sha256_file(mimic_outcome_rds_path), "main-model gate"
)
require_map_value(
  main_model_gate, "eicu_outcome_rds_sha256",
  sha256_file(eicu_outcome_rds_path), "main-model gate"
)

main_model_bundle <- readRDS(main_model_rds_path)
mimic_outcomes <- as.data.table(readRDS(mimic_outcome_rds_path))
eicu_outcomes <- as.data.table(readRDS(eicu_outcome_rds_path))

required_bundle_fields <- c(
  "artifact_version", "config_version", "checkpoint_sha256",
  "parameter_gate_sha256", "outcome_gate_sha256",
  "analysis_manifest_sha256", "model_utils_sha256",
  "analysis_utils_sha256", "frozen_parameter_rds_sha256",
  "model_specification", "design_column_manifest", "transform_bundle", "fits"
)
if (!is.list(main_model_bundle) ||
    length(setdiff(required_bundle_fields, names(main_model_bundle))) ||
    !identical(main_model_bundle$artifact_version, "mimic_locked_models_v1") ||
    !identical(main_model_bundle$config_version, LOCKED$version) ||
    !identical(main_model_bundle$checkpoint_sha256, sha256_file(checkpoint_path)) ||
    !identical(
      main_model_bundle$parameter_gate_sha256, sha256_file(parameter_gate_path)
    ) || !identical(
      main_model_bundle$outcome_gate_sha256, sha256_file(outcome_gate_path)
    ) || !identical(
      main_model_bundle$analysis_manifest_sha256,
      sha256_file(analysis_manifest_path)
    ) || !identical(
      main_model_bundle$model_utils_sha256, sha256_file(model_utils_path)
    ) || !identical(
      main_model_bundle$analysis_utils_sha256, sha256_file(analysis_utils_path)
    ) || !identical(
      main_model_bundle$frozen_parameter_rds_sha256,
      sha256_file(parameter_artifact_paths$parameter)
    ) || !identical(
      main_model_bundle$transform_bundle, transform_bundle
    )) stop("Main-model bundle provenance/schema check failed.")

main_fits <- main_model_bundle$fits
if (!is.list(main_fits) ||
    length(setdiff(c("S2", "S3"), names(main_fits))) ||
    !all(vapply(main_fits[c("S2", "S3")], function(x) {
      identical(x$status, "ESTIMABLE")
    }, logical(1L)))) stop("Main Phase 3b S2/S3 fits are unavailable.")
if (!identical(
  main_model_bundle$model_specification[model_id == "S3", design_type],
  "s0_smp_per_5"
)) stop("Phase 3b main S3 is not the locked linear per-5-J/min model.")

# Confirm the saved main fit design columns against the current frozen utility
# using predictor-only complete rows, not outcome-dependent subsets.
for (model_id in c("S2", "S3")) {
  design_m <- build_design_matrix(
    mimic_frame[primary_predictor_complete == TRUE], model_id, transform_bundle
  )
  design_e <- build_design_matrix(
    eicu_frame[primary_predictor_complete == TRUE], model_id, transform_bundle
  )
  if (!identical(colnames(design_m), main_fits[[model_id]]$design_columns) ||
      !identical(colnames(design_e), main_fits[[model_id]]$design_columns)) {
    stop("Main S2/S3 design signature differs from 08a: ", model_id)
  }
}

required_mimic_outcomes <- c(
  "stay_id", "hospital_mortality", "hospital_mortality_eligible",
  "mortality_28d", "mortality_28d_eligible",
  "icu_mortality", "icu_mortality_eligible"
)
required_eicu_outcomes <- c(
  "patientunitstayid", "hospitalid", "hospital_mortality",
  "hospital_mortality_eligible", "icu_mortality", "icu_mortality_eligible"
)
if (length(setdiff(required_mimic_outcomes, names(mimic_outcomes))) ||
    length(setdiff(required_eicu_outcomes, names(eicu_outcomes))) ||
    anyDuplicated(mimic_outcomes$stay_id) ||
    anyDuplicated(eicu_outcomes$patientunitstayid) ||
    anyNA(mimic_outcomes$stay_id) || anyNA(eicu_outcomes$patientunitstayid) ||
    anyNA(eicu_outcomes$hospitalid)) {
  stop("Formal outcome artifact schema/key invariant failed.")
}
if (nrow(mimic_outcomes) != as.integer(require_map_value(
  outcome_gate, "mimic_prediction_n", label = "outcome gate"
)) || nrow(eicu_outcomes) != as.integer(require_map_value(
  outcome_gate, "eicu_prediction_n", label = "outcome gate"
))) stop("Outcome RDS count differs from the Phase 3a gate.")
meta_mimic_outcome <- attr(mimic_outcomes, "rebuild_metadata")
meta_eicu_outcome <- attr(eicu_outcomes, "rebuild_metadata")
if (!isTRUE(meta_mimic_outcome$formally_unblinded) ||
    !isTRUE(meta_eicu_outcome$formally_unblinded) ||
    !identical(meta_mimic_outcome$checkpoint_sha256, sha256_file(checkpoint_path)) ||
    !identical(meta_eicu_outcome$checkpoint_sha256, sha256_file(checkpoint_path))) {
  stop("Outcome metadata does not match the formal checkpoint.")
}

validate_endpoint <- function(x, outcome, eligible, label) {
  y <- x[[outcome]]
  e <- x[[eligible]]
  if (anyNA(e) || any(!is.na(y) & !y %in% c(0L, 1L)) ||
      any(e & is.na(y)) || any(!e & !is.na(y))) {
    stop("Endpoint eligibility invariant failed: ", label)
  }
  invisible(TRUE)
}
validate_endpoint(
  mimic_outcomes, "hospital_mortality", "hospital_mortality_eligible",
  "MIMIC hospital"
)
validate_endpoint(
  mimic_outcomes, "mortality_28d", "mortality_28d_eligible", "MIMIC 28-day"
)
validate_endpoint(
  mimic_outcomes, "icu_mortality", "icu_mortality_eligible", "MIMIC ICU"
)
validate_endpoint(
  eicu_outcomes, "hospital_mortality", "hospital_mortality_eligible",
  "eICU hospital"
)
validate_endpoint(
  eicu_outcomes, "icu_mortality", "icu_mortality_eligible", "eICU ICU"
)

mimic_outcome_link <- mimic_outcomes[, .(
  analysis_id = as.integer(stay_id),
  hospital_mortality = as.integer(hospital_mortality),
  hospital_mortality_eligible = as.logical(hospital_mortality_eligible),
  mortality_28d = as.integer(mortality_28d),
  mortality_28d_eligible = as.logical(mortality_28d_eligible),
  icu_mortality = as.integer(icu_mortality),
  icu_mortality_eligible = as.logical(icu_mortality_eligible)
)]
eicu_outcome_link <- eicu_outcomes[, .(
  analysis_id = as.integer(patientunitstayid),
  outcome_hospital_id = as.character(hospitalid),
  hospital_mortality = as.integer(hospital_mortality),
  hospital_mortality_eligible = as.logical(hospital_mortality_eligible),
  icu_mortality = as.integer(icu_mortality),
  icu_mortality_eligible = as.logical(icu_mortality_eligible)
)]
if (!setequal(mimic_frame$analysis_id, mimic_outcome_link$analysis_id) ||
    !setequal(eicu_frame$analysis_id, eicu_outcome_link$analysis_id)) {
  stop("Predictor and outcome analysis-ID sets are not identical.")
}
mimic_analysis <- merge(
  mimic_frame, mimic_outcome_link, by = "analysis_id", all = FALSE, sort = FALSE
)
eicu_analysis <- merge(
  eicu_frame, eicu_outcome_link, by = "analysis_id", all = FALSE, sort = FALSE
)
setorder(mimic_analysis, analysis_id)
setorder(eicu_analysis, analysis_id)
if (nrow(mimic_analysis) != nrow(mimic_frame) ||
    nrow(eicu_analysis) != nrow(eicu_frame) ||
    anyDuplicated(mimic_analysis$analysis_id) ||
    anyDuplicated(eicu_analysis$analysis_id) ||
    any(eicu_analysis$hospital_id != eicu_analysis$outcome_hospital_id)) {
  stop("Exact sensitivity predictor/outcome join invariant failed.")
}
eicu_analysis[, outcome_hospital_id := NULL]

pbw_analysis <- lapply(pbw_definitions, function(z) {
  out <- merge(
    z$frame, mimic_outcome_link, by = "analysis_id", all = FALSE, sort = FALSE
  )
  setorder(out, analysis_id)
  if (nrow(out) != nrow(z$frame) || anyDuplicated(out$analysis_id)) {
    stop("PBW/outcome exact join invariant failed.")
  }
  list(frame = out, bundle = z$bundle, qc = z$qc)
})

# ---------------------------------------------------------------------------
# Generic locked S2/S3 sensitivity helpers. Every comparison is constructed on
# one identical outcome/predictor-complete row set, with unchanged 08a columns.
# ---------------------------------------------------------------------------

make_nonestimable_fit <- function(model_id, reason, design_columns = character(),
                                  n = 0L, events = 0L) {
  structure(list(
    model_id = model_id, status = "NON_ESTIMABLE", reason = reason,
    coefficients = setNames(numeric(), character()),
    vcov = matrix(numeric(), 0L, 0L), design_columns = design_columns,
    n = as.integer(n), events = as.integer(events), rank = NA_integer_,
    condition_number = NA_real_, loglik = NA_real_
  ), class = "ards_locked_model_inference")
}

coefficient_table_one <- function(
    fit, sensitivity_id, database, endpoint, stage, weighted) {
  if (!identical(fit$status, "ESTIMABLE")) {
    return(data.table(
      sensitivity_id, database, endpoint, stage, weighted,
      model_id = fit$model_id, term = NA_character_, coefficient = NA_real_,
      standard_error = NA_real_, odds_ratio = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_,
      vcov_type = if (weighted) "HC0_sandwich" else "model_based",
      status = "NON_ESTIMABLE", reason = fit$reason
    ))
  }
  se <- sqrt(diag(fit$vcov))
  data.table(
    sensitivity_id, database, endpoint, stage, weighted,
    model_id = fit$model_id, term = names(fit$coefficients),
    coefficient = as.numeric(fit$coefficients),
    standard_error = as.numeric(se),
    odds_ratio = exp(as.numeric(fit$coefficients)),
    ci_lower = exp(as.numeric(fit$coefficients) - 1.96 * se),
    ci_upper = exp(as.numeric(fit$coefficients) + 1.96 * se),
    vcov_type = if (weighted) "HC0_sandwich" else "model_based",
    status = "ESTIMABLE", reason = ""
  )
}

clinical_contrast_one <- function(
    fit, sensitivity_id, database, endpoint, stage, weighted) {
  contrast <- NULL
  label <- unit <- ""
  if ("smp_per_5" %in% names(fit$coefficients) ||
      identical(fit$model_id, "S3") || identical(fit$model_id, "S3c") ||
      identical(fit$model_id, "S5") || identical(fit$model_id, "R3")) {
    contrast <- c(smp_per_5 = 1)
    label <- "absolute_sMP_per_5_J_min_linear"
    unit <- "5 J/min"
  } else if ("smp_z" %in% names(fit$coefficients) ||
      identical(fit$model_id, "N3_abs")) {
    contrast <- c(smp_z = 1)
    label <- "absolute_sMP_per_frozen_MIMIC_SD"
    unit <- "MIMIC SD"
  } else if ("smp_per_pbw_z" %in% names(fit$coefficients) ||
      identical(fit$model_id, "N3_pbw")) {
    contrast <- c(smp_per_pbw_z = 1)
    label <- "sMP_per_PBW_per_definition_specific_MIMIC_SD"
    unit <- "definition-specific MIMIC SD"
  }
  if (is.null(contrast)) return(NULL)
  result <- wald_contrast(fit, contrast, label, 0, 1, unit)
  result[, `:=`(
    sensitivity_id = sensitivity_id, database = database, endpoint = endpoint,
    stage = stage, weighted = weighted
  )]
  setcolorder(result, c(
    "sensitivity_id", "database", "endpoint", "stage", "weighted",
    setdiff(names(result), c(
      "sensitivity_id", "database", "endpoint", "stage", "weighted"
    ))
  ))
  result
}

effective_sample_size <- function(weights) {
  if (is.null(weights)) return(NA_real_)
  if (anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Invalid analysis weights.")
  }
  sum(weights)^2 / sum(weights^2)
}

fit_development_pair <- function(
    frame, subset_mask, outcome_col, eligible_col, sensitivity_id,
    database, endpoint, target_population, bundle = transform_bundle,
    weight_col = NULL, allow_nonestimable = FALSE) {
  if (length(subset_mask) != nrow(frame) || anyNA(subset_mask)) {
    stop("Invalid subset mask: ", sensitivity_id)
  }
  eligible <- frame[[eligible_col]] %in% TRUE &
    !is.na(frame[[outcome_col]]) & frame[[outcome_col]] %in% c(0L, 1L)
  keep <- subset_mask & frame$primary_predictor_complete %in% TRUE & eligible
  data <- frame[keep]
  y <- as.integer(data[[outcome_col]])
  weights <- if (is.null(weight_col)) NULL else as.numeric(data[[weight_col]])
  weighted <- !is.null(weight_col)
  if (weighted && (anyNA(weights) || any(!is.finite(weights)) ||
      any(weights <= 0))) stop("Invalid row-aligned weights: ", sensitivity_id)

  fits <- list()
  predictions <- list()
  performance <- list()
  statuses <- list()
  coefficients <- list()
  contrasts <- list()
  matrices <- list()
  for (model_id in c("S2", "S3")) {
    fit_result <- tryCatch({
      design <- build_design_matrix(data, model_id, bundle)
      fit <- if (weighted) {
        fit_weighted_model(
          design, y, weights, model_id,
          allow_nonestimable = allow_nonestimable
        )
      } else {
        fit_model(
          design, y, model_id, allow_nonestimable = allow_nonestimable
        )
      }
      list(fit = fit, design = design)
    }, error = function(e) {
      if (!allow_nonestimable) stop(e)
      list(
        fit = make_nonestimable_fit(
          model_id, conditionMessage(e), n = nrow(data), events = sum(y)
        ),
        design = NULL
      )
    })
    fit <- fit_result$fit
    fits[[model_id]] <- fit
    design <- fit_result$design
    statuses[[model_id]] <- data.table(
      sensitivity_id, database, endpoint, stage = "sensitivity_refit",
      target_population, weighted, model_id, status = fit$status,
      reason = fit$reason, n = nrow(data), events = sum(y),
      weight_sum = if (weighted) sum(weights) else NA_real_,
      effective_sample_size = effective_sample_size(weights)
    )
    matrices[[model_id]] <- data.table(
      sensitivity_id, database, endpoint, model_id,
      n = nrow(data),
      columns_n = if (is.null(design)) NA_integer_ else ncol(design),
      column_signature = if (is.null(design)) NA_character_ else
        paste(colnames(design), collapse = ";"),
      expected_signature = paste(main_fits[[model_id]]$design_columns,
        collapse = ";"),
      signature_matches_main = if (is.null(design)) NA else
        identical(colnames(design), main_fits[[model_id]]$design_columns)
    )
    coefficients[[model_id]] <- coefficient_table_one(
      fit, sensitivity_id, database, endpoint, "sensitivity_refit", weighted
    )
    contrasts[[model_id]] <- clinical_contrast_one(
      fit, sensitivity_id, database, endpoint, "sensitivity_refit", weighted
    )
    if (identical(fit$status, "ESTIMABLE")) {
      probability <- predict_model(fit, design)
      metric <- if (weighted) {
        weighted_performance_vector(y, probability, weights)
      } else {
        performance_vector(y, probability)
      }
      predictions[[model_id]] <- data.table(
        sensitivity_id, database, endpoint, stage = "sensitivity_refit_apparent",
        target_population, weighted, model_id, analysis_id = data$analysis_id,
        hospital_id = if ("hospital_id" %in% names(data)) {
          as.character(data$hospital_id)
        } else NA_character_,
        outcome = y, analysis_weight = if (weighted) weights else NA_real_,
        probability = probability
      )
      performance[[model_id]] <- data.table(
        sensitivity_id, database, endpoint, stage = "sensitivity_refit_apparent",
        target_population, weighted, model_id, metric = names(metric),
        estimate = as.numeric(metric), n = nrow(data), events = sum(y),
        weight_sum = if (weighted) sum(weights) else NA_real_,
        effective_sample_size = effective_sample_size(weights),
        interpretation = if (weighted) {
          "selection-weighted sensitivity under specified observation model"
        } else "unweighted restricted-sample sensitivity"
      )
    }
  }
  if (!allow_nonestimable &&
      any(vapply(fits, function(x) x$status != "ESTIMABLE", logical(1L)))) {
    stop("A required S2/S3 sensitivity refit failed: ", sensitivity_id)
  }
  differences <- data.table(
    sensitivity_id, database, endpoint, stage = "sensitivity_refit_apparent",
    target_population, weighted, comparison_id = "S3_minus_S2",
    metric = metric_names, estimate_new_minus_reference = NA_real_,
    n = nrow(data), events = sum(y), status = "NON_ESTIMABLE"
  )
  if (all(vapply(fits, function(x) x$status == "ESTIMABLE", logical(1L)))) {
    p2 <- predictions$S2$probability
    p3 <- predictions$S3$probability
    m2 <- if (weighted) weighted_performance_vector(y, p2, weights) else
      performance_vector(y, p2)
    m3 <- if (weighted) weighted_performance_vector(y, p3, weights) else
      performance_vector(y, p3)
    differences[, `:=`(
      estimate_new_minus_reference = as.numeric(m3[metric_names] - m2[metric_names]),
      status = "ESTIMABLE"
    )]
  }
  list(
    fits = fits,
    predictions = rbindlist(predictions, use.names = TRUE, fill = TRUE),
    performance = rbindlist(performance, use.names = TRUE, fill = TRUE),
    differences = differences,
    status = rbindlist(statuses, use.names = TRUE, fill = TRUE),
    coefficients = rbindlist(coefficients, use.names = TRUE, fill = TRUE),
    contrasts = rbindlist(contrasts, use.names = TRUE, fill = TRUE),
    matrices = rbindlist(matrices, use.names = TRUE, fill = TRUE),
    analysis_ids = data$analysis_id
  )
}

apply_external_pair <- function(
    fits, frame, subset_mask, outcome_col, eligible_col, sensitivity_id,
    database, endpoint, target_population, bundle = transform_bundle,
    weight_col = NULL) {
  if (length(subset_mask) != nrow(frame) || anyNA(subset_mask)) {
    stop("Invalid external subset mask: ", sensitivity_id)
  }
  eligible <- frame[[eligible_col]] %in% TRUE &
    !is.na(frame[[outcome_col]]) & frame[[outcome_col]] %in% c(0L, 1L)
  keep <- subset_mask & frame$primary_predictor_complete %in% TRUE & eligible
  data <- frame[keep]
  y <- as.integer(data[[outcome_col]])
  weights <- if (is.null(weight_col)) NULL else as.numeric(data[[weight_col]])
  weighted <- !is.null(weight_col)
  if (weighted && (anyNA(weights) || any(!is.finite(weights)) ||
      any(weights <= 0))) stop("Invalid external weights: ", sensitivity_id)
  assert_binary_outcome(y)

  predictions <- list()
  performance <- list()
  matrices <- list()
  statuses <- list()
  for (model_id in c("S2", "S3")) {
    fit <- fits[[model_id]]
    if (is.null(fit) || !identical(fit$status, "ESTIMABLE")) {
      stop("External sensitivity requires an estimable ", model_id, " fit.")
    }
    design <- build_design_matrix(data, model_id, bundle)
    if (!identical(colnames(design), fit$design_columns)) {
      stop("External sensitivity design differs from fitted model: ", sensitivity_id)
    }
    probability <- predict_model(fit, design)
    metric <- if (weighted) {
      weighted_performance_vector(y, probability, weights)
    } else performance_vector(y, probability)
    predictions[[model_id]] <- data.table(
      sensitivity_id, database, endpoint, stage = "locked_external",
      target_population, weighted, model_id, analysis_id = data$analysis_id,
      hospital_id = as.character(data$hospital_id), outcome = y,
      analysis_weight = if (weighted) weights else NA_real_,
      probability = probability
    )
    performance[[model_id]] <- data.table(
      sensitivity_id, database, endpoint, stage = "locked_external",
      target_population, weighted, model_id, metric = names(metric),
      estimate = as.numeric(metric), n = nrow(data), events = sum(y),
      weight_sum = if (weighted) sum(weights) else NA_real_,
      effective_sample_size = effective_sample_size(weights),
      interpretation = if (weighted) {
        "selection-weighted external sensitivity under specified observation model"
      } else "original locked-model restricted external sensitivity"
    )
    matrices[[model_id]] <- data.table(
      sensitivity_id, database, endpoint, model_id, n = nrow(data),
      columns_n = ncol(design), column_signature = paste(colnames(design),
        collapse = ";"),
      expected_signature = paste(fit$design_columns, collapse = ";"),
      signature_matches_main = identical(colnames(design), fit$design_columns)
    )
    statuses[[model_id]] <- data.table(
      sensitivity_id, database, endpoint, stage = "locked_external",
      target_population, weighted, model_id, status = "ESTIMABLE",
      reason = "", n = nrow(data), events = sum(y),
      weight_sum = if (weighted) sum(weights) else NA_real_,
      effective_sample_size = effective_sample_size(weights)
    )
  }
  p2 <- predictions$S2$probability
  p3 <- predictions$S3$probability
  m2 <- if (weighted) weighted_performance_vector(y, p2, weights) else
    performance_vector(y, p2)
  m3 <- if (weighted) weighted_performance_vector(y, p3, weights) else
    performance_vector(y, p3)
  differences <- data.table(
    sensitivity_id, database, endpoint, stage = "locked_external",
    target_population, weighted, comparison_id = "S3_minus_S2",
    metric = metric_names,
    estimate_new_minus_reference = as.numeric(m3[metric_names] - m2[metric_names]),
    n = nrow(data), events = sum(y), status = "ESTIMABLE"
  )
  list(
    predictions = rbindlist(predictions, use.names = TRUE),
    performance = rbindlist(performance, use.names = TRUE),
    differences = differences,
    status = rbindlist(statuses, use.names = TRUE),
    matrices = rbindlist(matrices, use.names = TRUE),
    analysis_ids = data$analysis_id
  )
}

# ---------------------------------------------------------------------------
# A-F: primary-tuple restrictions, D055 weighted targets, and secondary outcomes.
# ---------------------------------------------------------------------------

analysis_results <- list()

analysis_results$warning_free_mimic <- fit_development_pair(
  mimic_analysis, mimic_analysis$analysis_id %in% warning_free_ids,
  "hospital_mortality", "hospital_mortality_eligible",
  "A_warning_free_primary_tuple", "MIMIC-IV_v3.1",
  "in_hospital_mortality", "D051 warning-free primary selected tuple"
)

analysis_results$preferred_source_mimic <- fit_development_pair(
  mimic_analysis, mimic_analysis$analysis_id %in% mimic_preferred_ids,
  "hospital_mortality", "hospital_mortality_eligible",
  "B_primary_tuple_preferred_source", "MIMIC-IV_v3.1",
  "in_hospital_mortality",
  "D058 primary tuple itself uses preferred source tiers"
)
analysis_results$preferred_source_eicu <- apply_external_pair(
  analysis_results$preferred_source_mimic$fits,
  eicu_analysis, eicu_analysis$analysis_id %in% eicu_preferred_ids,
  "hospital_mortality", "hospital_mortality_eligible",
  "B_primary_tuple_preferred_source", "eICU-CRD_v2.0",
  "in_hospital_mortality",
  "D058 primary tuple itself uses preferred source tiers"
)

analysis_results$eicu_age_topcode_excluded <- apply_external_pair(
  main_fits, eicu_analysis,
  eicu_analysis$age_topcode_sensitivity_exclude == FALSE,
  "hospital_mortality", "hospital_mortality_eligible",
  "C_eicu_age_topcode_excluded", "eICU-CRD_v2.0",
  "in_hospital_mortality", "D039 exclude literal age >89 records"
)

analysis_results$selection_weighted_mimic <- fit_development_pair(
  mimic_analysis, rep(TRUE, nrow(mimic_analysis)),
  "hospital_mortality", "hospital_mortality_eligible",
  "E_selection_weighted_full_target", "MIMIC-IV_v3.1",
  "in_hospital_mortality", "MIMIC strict-cohort tuple-observation target",
  weight_col = "selection_weight"
)
analysis_results$selection_weighted_eicu_full <- apply_external_pair(
  analysis_results$selection_weighted_mimic$fits,
  eicu_analysis, rep(TRUE, nrow(eicu_analysis)),
  "hospital_mortality", "hospital_mortality_eligible",
  "E_selection_weighted_full_target", "eICU-CRD_v2.0",
  "in_hospital_mortality",
  paste(
    "eICU full strict-cohort target; structurally positivity-sensitive because",
    "35 hospitals have zero observed tuple"
  ),
  weight_col = "selection_weight_full"
)
analysis_results$selection_weighted_eicu_supported <- apply_external_pair(
  analysis_results$selection_weighted_mimic$fits,
  eicu_analysis, eicu_analysis$hospital_id %chin% eicu_support_hospitals,
  "hospital_mortality", "hospital_mortality_eligible",
  "D_eicu_selection_weighted_supported_hospital_target", "eICU-CRD_v2.0",
  "in_hospital_mortality",
  "D055 target restricted to hospitals with at least one observed tuple",
  weight_col = "selection_weight_supported"
)
if (!setequal(
  analysis_results$selection_weighted_eicu_full$analysis_ids,
  analysis_results$selection_weighted_eicu_supported$analysis_ids
)) {
  stop("Supported-hospital target must retain every observed eligible tuple.")
}

analysis_results$mimic_28d <- fit_development_pair(
  mimic_analysis, rep(TRUE, nrow(mimic_analysis)),
  "mortality_28d", "mortality_28d_eligible",
  "F_mimic_28_day_mortality", "MIMIC-IV_v3.1", "28_day_mortality",
  "primary tuple with known MIMIC 28-day endpoint"
)
analysis_results$mimic_icu <- fit_development_pair(
  mimic_analysis, rep(TRUE, nrow(mimic_analysis)),
  "icu_mortality", "icu_mortality_eligible",
  "F_icu_mortality", "MIMIC-IV_v3.1", "icu_mortality",
  "primary tuple with known ICU endpoint"
)
analysis_results$eicu_icu <- apply_external_pair(
  analysis_results$mimic_icu$fits,
  eicu_analysis, rep(TRUE, nrow(eicu_analysis)),
  "icu_mortality", "icu_mortality_eligible",
  "F_icu_mortality", "eICU-CRD_v2.0", "icu_mortality",
  "locked MIMIC ICU-endpoint model applied to eICU ICU endpoint"
)

# ---------------------------------------------------------------------------
# G: prespecified D053 PBW-definition robustness analyses. For each PBW
# definition, all four models use the exact same component/normalized/outcome
# complete MIMIC rows. Alternative definitions change only the two PBW-derived
# fields and their outcome-free MIMIC sMP/PBW center/scale. Small samples may be
# reported NON_ESTIMABLE; no rescue model or new PBW definition is introduced.
# ---------------------------------------------------------------------------

main_design_manifest <- as.data.table(main_model_bundle$design_column_manifest)
if (length(setdiff(
  c("model_id", "columns_n", "column_signature"),
  names(main_design_manifest)
)) || anyDuplicated(main_design_manifest$model_id)) {
  stop("Malformed Phase 3b design-column manifest.")
}

expected_design_columns <- function(model_id) {
  target_model_id <- model_id
  row <- main_design_manifest[model_id == target_model_id]
  if (nrow(row) != 1L || is.na(row$column_signature[[1L]]) ||
      !nzchar(row$column_signature[[1L]])) {
    stop("Missing unique Phase 3b design signature for ", model_id)
  }
  columns <- strsplit(row$column_signature[[1L]], ";", fixed = TRUE)[[1L]]
  if (length(columns) != as.integer(row$columns_n[[1L]]) ||
      anyDuplicated(columns)) {
    stop("Invalid Phase 3b design signature for ", model_id)
  }
  columns
}

fit_pbw_definition <- function(definition, analysis_object) {
  frame <- analysis_object$frame
  bundle <- analysis_object$bundle
  keep <- frame$component_predictor_complete %in% TRUE &
    frame$normalized_exposure_complete %in% TRUE &
    frame$hospital_mortality_eligible %in% TRUE &
    !is.na(frame$hospital_mortality) &
    frame$hospital_mortality %in% c(0L, 1L)
  data <- frame[keep]
  y <- as.integer(data$hospital_mortality)
  sensitivity_id <- paste0("G_pbw_", definition)
  target_population <- paste0(
    "MIMIC primary tuples complete for component and normalized models under ",
    definition
  )
  model_ids <- c("S3c", "S4", "N3_abs", "N3_pbw")
  fits <- predictions <- performance <- statuses <- coefficients <-
    contrasts <- matrices <- setNames(vector("list", length(model_ids)), model_ids)

  for (model_id in model_ids) {
    design <- build_design_matrix(data, model_id, bundle)
    expected <- expected_design_columns(model_id)
    if (!identical(colnames(design), expected)) {
      stop("D053 design signature changed for ", definition, "/", model_id)
    }
    fit <- fit_model(design, y, model_id, allow_nonestimable = TRUE)
    fits[[model_id]] <- fit
    statuses[[model_id]] <- data.table(
      sensitivity_id, database = "MIMIC-IV_v3.1",
      endpoint = "in_hospital_mortality", stage = "pbw_definition_refit",
      target_population, weighted = FALSE, pbw_definition = definition,
      model_id, status = fit$status, reason = fit$reason, n = nrow(data),
      events = sum(y), weight_sum = NA_real_, effective_sample_size = NA_real_
    )
    matrices[[model_id]] <- data.table(
      sensitivity_id, database = "MIMIC-IV_v3.1",
      endpoint = "in_hospital_mortality", pbw_definition = definition,
      model_id, n = nrow(data), columns_n = ncol(design),
      column_signature = paste(colnames(design), collapse = ";"),
      expected_signature = paste(expected, collapse = ";"),
      signature_matches_main = identical(colnames(design), expected)
    )
    coefficients[[model_id]] <- coefficient_table_one(
      fit, sensitivity_id, "MIMIC-IV_v3.1", "in_hospital_mortality",
      "pbw_definition_refit", FALSE
    )[, pbw_definition := definition]
    contrast <- clinical_contrast_one(
      fit, sensitivity_id, "MIMIC-IV_v3.1", "in_hospital_mortality",
      "pbw_definition_refit", FALSE
    )
    if (!is.null(contrast)) contrast[, pbw_definition := definition]
    contrasts[[model_id]] <- contrast

    if (identical(fit$status, "ESTIMABLE")) {
      probability <- predict_model(fit, design)
      metric <- performance_vector(y, probability)
      predictions[[model_id]] <- data.table(
        sensitivity_id, database = "MIMIC-IV_v3.1",
        endpoint = "in_hospital_mortality",
        stage = "pbw_definition_refit_apparent", target_population,
        weighted = FALSE, pbw_definition = definition, model_id,
        analysis_id = data$analysis_id, hospital_id = NA_character_,
        outcome = y, analysis_weight = NA_real_, probability
      )
      performance[[model_id]] <- data.table(
        sensitivity_id, database = "MIMIC-IV_v3.1",
        endpoint = "in_hospital_mortality",
        stage = "pbw_definition_refit_apparent", target_population,
        weighted = FALSE, pbw_definition = definition, model_id,
        metric = names(metric), estimate = as.numeric(metric),
        n = nrow(data), events = sum(y), weight_sum = NA_real_,
        effective_sample_size = NA_real_,
        interpretation = "unweighted PBW-definition common-sample sensitivity"
      )
    }
  }

  comparison_rows <- lapply(list(
    list(id = "S4_minus_S3c", new = "S4", reference = "S3c"),
    list(
      id = "N3_pbw_minus_N3_abs", new = "N3_pbw", reference = "N3_abs"
    )
  ), function(comparison) {
    estimable <- identical(fits[[comparison$new]]$status, "ESTIMABLE") &&
      identical(fits[[comparison$reference]]$status, "ESTIMABLE")
    estimate <- rep(NA_real_, length(metric_names))
    reason <- "one or both prespecified models are NON_ESTIMABLE"
    if (estimable) {
      p_new <- predictions[[comparison$new]]$probability
      p_reference <- predictions[[comparison$reference]]$probability
      estimate <- as.numeric(
        performance_vector(y, p_new)[metric_names] -
          performance_vector(y, p_reference)[metric_names]
      )
      reason <- ""
    }
    data.table(
      sensitivity_id, database = "MIMIC-IV_v3.1",
      endpoint = "in_hospital_mortality",
      stage = "pbw_definition_refit_apparent", target_population,
      weighted = FALSE, pbw_definition = definition,
      comparison_id = comparison$id, metric = metric_names,
      estimate_new_minus_reference = estimate, n = nrow(data),
      events = sum(y),
      status = if (estimable) "ESTIMABLE" else "NON_ESTIMABLE",
      reason = reason
    )
  })
  list(
    fits = fits,
    predictions = rbindlist(predictions, use.names = TRUE, fill = TRUE),
    performance = rbindlist(performance, use.names = TRUE, fill = TRUE),
    differences = rbindlist(comparison_rows, use.names = TRUE),
    status = rbindlist(statuses, use.names = TRUE),
    coefficients = rbindlist(coefficients, use.names = TRUE, fill = TRUE),
    contrasts = rbindlist(contrasts, use.names = TRUE, fill = TRUE),
    matrices = rbindlist(matrices, use.names = TRUE),
    analysis_ids = data$analysis_id
  )
}

pbw_results <- lapply(names(pbw_analysis), function(definition) {
  fit_pbw_definition(definition, pbw_analysis[[definition]])
})
names(pbw_results) <- paste0("pbw_", names(pbw_analysis))
analysis_results <- c(analysis_results, pbw_results)

# ---------------------------------------------------------------------------
# Aggregate/private result assembly and invariant checks.
# ---------------------------------------------------------------------------

bind_result_field <- function(results, field) {
  values <- lapply(results, function(x) x[[field]])
  values <- Filter(function(x) !is.null(x) && nrow(x) > 0L, values)
  if (!length(values)) return(data.table())
  rbindlist(values, use.names = TRUE, fill = TRUE)
}

private_predictions <- bind_result_field(analysis_results, "predictions")
performance_results <- bind_result_field(analysis_results, "performance")
metric_differences <- bind_result_field(analysis_results, "differences")
model_status <- bind_result_field(analysis_results, "status")
coefficient_results <- bind_result_field(analysis_results, "coefficients")
clinical_contrasts <- bind_result_field(analysis_results, "contrasts")
model_matrix_qc <- bind_result_field(analysis_results, "matrices")

if (!nrow(private_predictions) || !nrow(performance_results) ||
    !nrow(metric_differences) || !nrow(model_status) ||
    !nrow(coefficient_results) || !nrow(clinical_contrasts) ||
    !nrow(model_matrix_qc)) {
  stop("A required sensitivity result family is empty.")
}
if (anyDuplicated(private_predictions[, .(
  sensitivity_id, database, endpoint, stage, model_id, analysis_id
)]) || anyDuplicated(performance_results[, .(
  sensitivity_id, database, endpoint, stage, model_id, metric
)]) || anyDuplicated(metric_differences[, .(
  sensitivity_id, database, endpoint, stage, comparison_id, metric
)]) || anyDuplicated(model_status[, .(
  sensitivity_id, database, endpoint, stage, model_id
)])) stop("A sensitivity output key is duplicated.")
if (anyNA(model_matrix_qc$signature_matches_main) ||
    any(!model_matrix_qc$signature_matches_main)) {
  stop("At least one sensitivity design signature differs from Phase 3b.")
}
if (any(model_status[
  !grepl("^G_pbw_", sensitivity_id), status != "ESTIMABLE"
])) stop("A required non-PBW sensitivity model is not estimable.")

population_counts <- unique(metric_differences[, .(
  sensitivity_id, database, endpoint, stage, target_population, weighted,
  pbw_definition, analysis_n = n, events
)])
setorder(population_counts, sensitivity_id, database, endpoint)

spec_row <- function(
    sensitivity_id, database, decision_id, endpoint, analysis_stage,
    model_ids, subset_rule, common_sample_rule, weighting, target_population,
    interpretation, limitation) {
  data.table(
    sensitivity_id, database, decision_id, endpoint, analysis_stage,
    model_ids, subset_rule, common_sample_rule, weighting, target_population,
    interpretation, limitation
  )
}

sensitivity_specification <- rbindlist(list(
  spec_row(
    "A_warning_free_primary_tuple", "MIMIC-IV_v3.1", "D051",
    "in_hospital_mortality", "MIMIC refit", "S2;S3",
    "restrict already selected primary tuple to warning-free records",
    "same primary-predictor complete rows for S2 and S3", "none",
    "warning-free primary selected tuples",
    "measurement-QC restriction", "restriction may change case mix"
  ),
  spec_row(
    "B_primary_tuple_preferred_source", "MIMIC-IV_v3.1", "D058",
    "in_hospital_mortality", "MIMIC refit", "S2;S3",
    "restrict the already selected primary tuple to preferred source tiers",
    "same primary-predictor complete rows for S2 and S3", "none",
    "primary tuples already using preferred source tiers",
    "source-restricted estimand; no tuple reselection",
    "full reselected variant blocked pending preferred-time HSC rebuild"
  ),
  spec_row(
    "B_primary_tuple_preferred_source", "eICU-CRD_v2.0", "D058",
    "in_hospital_mortality", "locked external", "S2;S3",
    "restrict the already selected primary tuple to preferred source tiers",
    "same primary-predictor complete rows for S2 and S3", "none",
    "primary tuples already using preferred source tiers",
    "external source-restricted estimand; no tuple reselection",
    "full reselected variant blocked pending preferred-time HSC rebuild"
  ),
  spec_row(
    "C_eicu_age_topcode_excluded", "eICU-CRD_v2.0", "D039",
    "in_hospital_mortality", "locked external", "S2;S3",
    "exclude literal eICU age >89 records", "same rows for S2 and S3",
    "none", "eICU primary tuples without age-topcoded records",
    "age-topcode robustness restriction", "restriction changes case mix"
  ),
  spec_row(
    "D_eicu_selection_weighted_supported_hospital_target",
    "eICU-CRD_v2.0", "D055", "in_hospital_mortality",
    "locked external", "S2;S3",
    "restrict target to hospitals with at least one observed tuple",
    "same observed eligible rows and supported-target weights for S2 and S3",
    "stabilized observation weights truncated at observed-record p01/p99",
    "supported-hospital strict-cohort target",
    "selection-weighted under the specified observation model",
    "supported-hospital restriction changes the target population"
  ),
  spec_row(
    "E_selection_weighted_full_target", "MIMIC-IV_v3.1", "D055",
    "in_hospital_mortality", "MIMIC weighted refit", "S2;S3",
    "all observed primary tuples",
    "same eligible rows and weights for S2 and S3",
    "stabilized observation weights truncated at observed-record p01/p99",
    "MIMIC strict-cohort tuple-observation target",
    "selection-weighted under the specified observation model",
    "depends on the frozen observation model"
  ),
  spec_row(
    "E_selection_weighted_full_target", "eICU-CRD_v2.0", "D055",
    "in_hospital_mortality", "locked external", "S2;S3",
    "all observed primary tuples",
    "same eligible rows and full-target weights for S2 and S3",
    "stabilized observation weights truncated at observed-record p01/p99",
    "eICU full strict-cohort target",
    "selection-weighted under the specified observation model",
    "structurally positivity-sensitive: 35 hospitals have no observed tuple"
  ),
  spec_row(
    "F_mimic_28_day_mortality", "MIMIC-IV_v3.1", "D013",
    "28_day_mortality", "MIMIC endpoint-specific refit", "S2;S3",
    "known 28-day endpoint", "same endpoint-eligible rows for S2 and S3",
    "none", "MIMIC primary tuples with known 28-day mortality",
    "secondary endpoint sensitivity", "not available in eICU"
  ),
  spec_row(
    "F_icu_mortality", "MIMIC-IV_v3.1", "D013", "icu_mortality",
    "MIMIC endpoint-specific refit", "S2;S3", "known ICU endpoint",
    "same endpoint-eligible rows for S2 and S3", "none",
    "MIMIC primary tuples with known ICU mortality",
    "secondary endpoint sensitivity", "endpoint differs from hospital mortality"
  ),
  spec_row(
    "F_icu_mortality", "eICU-CRD_v2.0", "D013", "icu_mortality",
    "locked external", "S2;S3", "known ICU endpoint",
    "same endpoint-eligible rows for S2 and S3", "none",
    "eICU primary tuples with known ICU mortality",
    "MIMIC ICU-endpoint fits applied externally",
    "endpoint differs from hospital mortality"
  )
))
sensitivity_specification <- rbindlist(c(
  list(sensitivity_specification),
  lapply(names(pbw_analysis), function(definition) spec_row(
    paste0("G_pbw_", definition), "MIMIC-IV_v3.1", "D053",
    "in_hospital_mortality", "MIMIC PBW-definition refit",
    "S3c;S4;N3_abs;N3_pbw",
    paste0("use only frozen PBW definition: ", definition),
    paste(
      "identical component-complete AND normalized-exposure-complete",
      "endpoint-eligible rows for all four models"
    ), "none", paste0("MIMIC common sample under ", definition),
    "S4-S3c component comparison and N3_pbw-N3_abs normalized comparison",
    "NON_ESTIMABLE is reported without rescue; no external alternate-PBW analysis"
  ))
), use.names = TRUE)

# Aggregate-only QC tables contain counts/hashes and never row-level IDs.
project_relative <- function(path) {
  resolved <- normalizePath(path, mustWork = TRUE)
  prefix <- paste0(project_from_script, .Platform$file.sep)
  if (!startsWith(resolved, prefix)) stop("Output/input path escapes project root.")
  substring(resolved, nchar(prefix) + 1L)
}
input_hash_row <- function(type, name, path, validation_chain) {
  data.table(
    input_type = type, artifact_name = name,
    project_relative_path = project_relative(path),
    sha256 = sha256_file(path), validation_chain, pass = TRUE
  )
}
input_hash_qc <- rbindlist(list(
  input_hash_row("checkpoint", "outcome_unblinding_checkpoint", checkpoint_path,
    "AUTHORIZED checkpoint read first"),
  input_hash_row("manifest", "analysis_script_manifest", analysis_manifest_path,
    "checkpoint hash plus every manifested script hash"),
  input_hash_row("gate", "parameter_freeze", parameter_gate_path,
    "checkpoint hash and PASS fields"),
  input_hash_row("gate", "selection_weights", selection_gate_path,
    "checkpoint hash, PASS, leakage checks"),
  input_hash_row("gate", "mimic_severity", mimic_severity_gate_path,
    "PASS plus severity artifact hash"),
  input_hash_row("gate", "eicu_severity", eicu_severity_gate_path,
    "PASS plus severity artifact hash"),
  input_hash_row("gate", "mimic_phase2", mimic_phase2_gate_path,
    "PASS invariants plus preferred-source artifact hash"),
  input_hash_row("gate", "eicu_phase2", eicu_phase2_gate_path,
    "PASS invariants plus preferred-source artifact hash"),
  input_hash_row("gate", "warning_free", warning_gate_path,
    "PASS plus warning-free artifact hash"),
  input_hash_row("gate", "outcomes", outcome_gate_path,
    "PASS plus formal unblinding and outcome hashes"),
  input_hash_row("gate", "main_models", main_model_gate_path,
    "PASS plus exact utility/model/outcome hashes"),
  input_hash_row("receipt", "outcome_access_receipt", access_receipt_path,
    "checkpoint and outcome-gate chain"),
  input_hash_row("artifact", "frozen_parameters",
    parameter_artifact_paths$parameter, "parameter gate"),
  input_hash_row("artifact", "mimic_canonical_frame",
    parameter_artifact_paths$mimic_frame, "parameter gate"),
  input_hash_row("artifact", "eicu_canonical_frame",
    parameter_artifact_paths$eicu_frame, "parameter gate"),
  input_hash_row("artifact", "mimic_severity", mimic_severity_rds_path,
    "MIMIC severity gate"),
  input_hash_row("artifact", "eicu_severity", eicu_severity_rds_path,
    "eICU severity gate"),
  input_hash_row("artifact", "warning_free", warning_free_rds_path,
    "warning-free gate"),
  input_hash_row("artifact", "mimic_preferred_reselected",
    mimic_preferred_reselected_path, "MIMIC Phase 2 gate; audit only"),
  input_hash_row("artifact", "eicu_preferred_reselected",
    eicu_preferred_reselected_path, "eICU Phase 2 gate; audit only"),
  input_hash_row("artifact", "mimic_selection_weights", mimic_weight_rds_path,
    "selection-weight gate plus independent formula audit"),
  input_hash_row("artifact", "eicu_selection_weights", eicu_weight_rds_path,
    "selection-weight gate plus independent formula audit"),
  input_hash_row("artifact", "eicu_supported_selection_weights",
    eicu_support_weight_rds_path,
    "selection-weight gate plus independent formula audit"),
  input_hash_row("artifact", "main_model_bundle", main_model_rds_path,
    "Phase 3b gate plus saved transform/design validation"),
  input_hash_row("artifact", "mimic_outcomes", mimic_outcome_rds_path,
    "Phase 3a and Phase 3b hashes after authorization"),
  input_hash_row("artifact", "eicu_outcomes", eicu_outcome_rds_path,
    "Phase 3a and Phase 3b hashes after authorization")
))

exact_join_qc <- rbindlist(c(
  list(
    data.table(
      database = "MIMIC-IV_v3.1", analysis_variant = "canonical",
      predictor_n = nrow(mimic_frame), outcome_n = nrow(mimic_outcome_link),
      joined_n = nrow(mimic_analysis), predictor_key_unique = TRUE,
      outcome_key_unique = TRUE, exact_id_set_equality = setequal(
        mimic_frame$analysis_id, mimic_outcome_link$analysis_id
      ), cardinality_preserved = nrow(mimic_analysis) == nrow(mimic_frame)
    ),
    data.table(
      database = "eICU-CRD_v2.0", analysis_variant = "canonical",
      predictor_n = nrow(eicu_frame), outcome_n = nrow(eicu_outcome_link),
      joined_n = nrow(eicu_analysis), predictor_key_unique = TRUE,
      outcome_key_unique = TRUE, exact_id_set_equality = setequal(
        eicu_frame$analysis_id, eicu_outcome_link$analysis_id
      ), cardinality_preserved = nrow(eicu_analysis) == nrow(eicu_frame)
    )
  ),
  lapply(names(pbw_analysis), function(definition) {
    z <- pbw_analysis[[definition]]$frame
    data.table(
      database = "MIMIC-IV_v3.1", analysis_variant = paste0("PBW_", definition),
      predictor_n = nrow(pbw_definitions[[definition]]$frame),
      outcome_n = nrow(mimic_outcome_link), joined_n = nrow(z),
      predictor_key_unique = !anyDuplicated(
        pbw_definitions[[definition]]$frame$analysis_id
      ), outcome_key_unique = !anyDuplicated(mimic_outcome_link$analysis_id),
      exact_id_set_equality = setequal(
        pbw_definitions[[definition]]$frame$analysis_id,
        mimic_outcome_link$analysis_id
      ), cardinality_preserved =
        nrow(z) == nrow(pbw_definitions[[definition]]$frame)
    )
  })
), use.names = TRUE)
if (any(!exact_join_qc$predictor_key_unique) ||
    any(!exact_join_qc$outcome_key_unique) ||
    any(!exact_join_qc$exact_id_set_equality) ||
    any(!exact_join_qc$cardinality_preserved)) {
  stop("An exact analysis-ID join QC check failed.")
}

scope_qc <- data.table(
  analysis_component = c(
    "warning_free_primary_tuple", "preferred_source_primary_tuple_restriction",
    "eicu_age_topcode_exclusion", "selection_weighting_full_target",
    "selection_weighting_supported_hospital_target", "secondary_endpoints",
    "three_frozen_pbw_definitions", "multiple_imputation",
    "center_heterogeneity", "OASIS_modeling", "APACHE_modeling",
    "infection_window_plus_minus_30_minutes", "infection_window_plus_24_hours",
    "preferred_source_tuple_reselection_outcome_modeling",
    "external_alternate_pbw_modeling"
  ),
  in_scope = c(rep(TRUE, 7L), rep(FALSE, 8L)),
  implemented = c(rep(TRUE, 7L), rep(FALSE, 8L)),
  note = c(
    "D051 exact restriction", "D058 exact restriction only",
    "D039 exact restriction", "D055 specified observation model",
    "D055 target changes to supported hospitals", "28-day and ICU mortality",
    "D053 5y, 1y, and chartevents-only",
    "not in locked sensitivity scope", "separate locked center work",
    "not in locked sensitivity scope", "not in locked sensitivity scope",
    "not in locked sensitivity scope", "not in locked sensitivity scope",
    "blocked pending preferred-time HSC rebuild", "not prespecified"
  )
)
if (any(scope_qc$implemented != scope_qc$in_scope)) {
  stop("Sensitivity implementation scope differs from the lock.")
}

# ---------------------------------------------------------------------------
# New-output-only publication. Row-level predictions and fitted objects stay in
# private RDS files; aggregate/QC CSVs are identifier-free. The PASS gate is
# atomically published last.
# ---------------------------------------------------------------------------

private_out <- file.path(PRIVATE_ROOT, "locked_sensitivities")
aggregate_out <- file.path(AGGREGATE_ROOT, "locked_sensitivities")
qc_out <- file.path(QC_ROOT, "locked_sensitivities")
completion_gate <- file.path(
  qc_out, "phase3c_locked_sensitivities_complete_v1.csv"
)
private_paths <- c(
  models = file.path(private_out, "locked_sensitivity_models_v1.rds"),
  predictions = file.path(private_out, "locked_sensitivity_predictions_v1.rds")
)
aggregate_paths <- c(
  specification = file.path(
    aggregate_out, "locked_sensitivity_specification_v1.csv"
  ),
  population_counts = file.path(
    aggregate_out, "locked_sensitivity_population_counts_v1.csv"
  ),
  model_status = file.path(
    aggregate_out, "locked_sensitivity_model_status_v1.csv"
  ),
  coefficients = file.path(
    aggregate_out, "locked_sensitivity_coefficients_OR_v1.csv"
  ),
  contrasts = file.path(
    aggregate_out, "locked_sensitivity_clinical_contrasts_OR_v1.csv"
  ),
  performance = file.path(
    aggregate_out, "locked_sensitivity_performance_v1.csv"
  ),
  metric_differences = file.path(
    aggregate_out, "locked_sensitivity_metric_differences_v1.csv"
  ),
  pbw_scales = file.path(aggregate_out, "pbw_definition_scales_v1.csv"),
  preferred_source = file.path(
    aggregate_out, "preferred_source_estimand_status_v1.csv"
  ),
  selection_weights = file.path(
    aggregate_out, "selection_weight_diagnostics_v1.csv"
  )
)
qc_paths <- c(
  input_hash = file.path(qc_out, "input_hash_validation_QC.csv"),
  predictor_feasibility = file.path(qc_out, "predictor_feasibility_QC.csv"),
  exact_join = file.path(qc_out, "exact_analysis_id_join_QC.csv"),
  model_matrix = file.path(qc_out, "model_matrix_QC.csv"),
  scope = file.path(qc_out, "analysis_scope_QC.csv"),
  summary = file.path(qc_out, "locked_sensitivity_QC.md")
)
output_manifest_path <- file.path(qc_out, "aggregate_output_manifest_v1.csv")
all_planned_outputs <- c(
  private_paths, aggregate_paths, qc_paths, output_manifest_path,
  completion_gate
)
if (any(file.exists(all_planned_outputs))) {
  stop("A planned Phase 3c output already exists; refusing partial overwrite.")
}
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

atomic_fwrite_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  unlink(tmp, force = TRUE)
  fwrite(object, tmp)
  if (!file.link(tmp, path)) {
    stop("Could not atomically create new output (possibly exists): ", path)
  }
  unlink(tmp, force = TRUE)
  invisible(path)
}
atomic_save_rds_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  unlink(tmp, force = TRUE)
  saveRDS(object, tmp, version = 3L, compress = "xz")
  if (!file.link(tmp, path)) {
    stop("Could not atomically create new output (possibly exists): ", path)
  }
  unlink(tmp, force = TRUE)
  invisible(path)
}
atomic_write_lines_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  unlink(tmp, force = TRUE)
  writeLines(object, tmp, useBytes = TRUE)
  if (!file.link(tmp, path)) {
    stop("Could not atomically create new output (possibly exists): ", path)
  }
  unlink(tmp, force = TRUE)
  invisible(path)
}

result_fits <- lapply(analysis_results, function(x) x$fits)
result_fits <- result_fits[!vapply(result_fits, is.null, logical(1L))]
private_model_bundle <- list(
  artifact_version = "locked_sensitivity_models_v1",
  config_version = LOCKED$version,
  checkpoint_sha256 = sha256_file(checkpoint_path),
  analysis_manifest_sha256 = sha256_file(analysis_manifest_path),
  parameter_gate_sha256 = sha256_file(parameter_gate_path),
  selection_gate_sha256 = sha256_file(selection_gate_path),
  outcome_gate_sha256 = sha256_file(outcome_gate_path),
  main_model_gate_sha256 = sha256_file(main_model_gate_path),
  model_utils_sha256 = sha256_file(model_utils_path),
  analysis_utils_sha256 = sha256_file(analysis_utils_path),
  frozen_transform_bundle = transform_bundle,
  pbw_transform_bundles = lapply(pbw_analysis, `[[`, "bundle"),
  sensitivity_specification = sensitivity_specification,
  model_status = model_status,
  fits = result_fits,
  interpretation_rules = list(
    selection_weighting =
      "selection-weighted under the specified observation model",
    full_eicu_target = "structurally positivity-sensitive",
    supported_hospital_target = "changes target population",
    preferred_source_modeled_estimand =
      "already selected primary tuple preferred-source restriction",
    preferred_source_reselected_status =
      "BLOCKED_REQUIRES_PREFERRED_TIME_HSC_REBUILD"
  )
)
attr(private_predictions, "rebuild_metadata") <- list(
  artifact_version = "locked_sensitivity_predictions_v1",
  private_row_level = TRUE, exact_analysis_id_join = TRUE,
  selection_weighting_label =
    "selection-weighted under the specified observation model",
  preferred_source_reselection_used_for_outcome_modeling = FALSE
)

atomic_save_rds_new(private_model_bundle, private_paths[["models"]])
atomic_save_rds_new(private_predictions, private_paths[["predictions"]])
atomic_fwrite_new(
  sensitivity_specification, aggregate_paths[["specification"]]
)
atomic_fwrite_new(population_counts, aggregate_paths[["population_counts"]])
atomic_fwrite_new(model_status, aggregate_paths[["model_status"]])
atomic_fwrite_new(coefficient_results, aggregate_paths[["coefficients"]])
atomic_fwrite_new(clinical_contrasts, aggregate_paths[["contrasts"]])
atomic_fwrite_new(performance_results, aggregate_paths[["performance"]])
atomic_fwrite_new(
  metric_differences, aggregate_paths[["metric_differences"]]
)
atomic_fwrite_new(pbw_scale_qc, aggregate_paths[["pbw_scales"]])
atomic_fwrite_new(preferred_source_qc, aggregate_paths[["preferred_source"]])
atomic_fwrite_new(selection_weight_qc, aggregate_paths[["selection_weights"]])
atomic_fwrite_new(input_hash_qc, qc_paths[["input_hash"]])
atomic_fwrite_new(
  predictor_feasibility_qc, qc_paths[["predictor_feasibility"]]
)
atomic_fwrite_new(exact_join_qc, qc_paths[["exact_join"]])
atomic_fwrite_new(model_matrix_qc, qc_paths[["model_matrix"]])
atomic_fwrite_new(scope_qc, qc_paths[["scope"]])

summary_lines <- c(
  "# Locked sensitivity analysis QC",
  "",
  paste0("- Configuration: ", LOCKED$version),
  "- S3 remains the locked linear per-5-J/min model; S3NL is not rerun here.",
  "- D058 outcome modeling restricts the already selected primary tuple to preferred sources.",
  "- D058 reselected preferred tuples are blocked pending preferred-time HSC rebuild.",
  "- Selection-weighted estimates are labeled under the specified observation model.",
  "- The eICU full target is structurally positivity-sensitive; the supported-hospital analysis changes the target population.",
  "- PBW analyses use only the frozen 5-year, 1-year, and chartevents-only definitions on definition-specific common samples.",
  "- PBW NON_ESTIMABLE results are reported without a rescue model.",
  "- MI, center heterogeneity, native-score modeling, infection-window variants, and external alternate-PBW modeling are absent.",
  "",
  "BUILD_COMPLETE"
)
atomic_write_lines_new(summary_lines, qc_paths[["summary"]])

manifested_public_paths <- c(aggregate_paths, qc_paths)
output_manifest <- data.table(
  output_kind = c(
    rep("aggregate", length(aggregate_paths)), rep("qc", length(qc_paths))
  ),
  output_name = names(manifested_public_paths),
  project_relative_path = vapply(
    manifested_public_paths, project_relative, character(1L)
  ),
  sha256 = vapply(manifested_public_paths, sha256_file, character(1L)),
  row_level_identifier_columns = FALSE
)
atomic_fwrite_new(output_manifest, output_manifest_path)

identifier_headers <- c(
  "analysis_id", "patient_cluster_id", "stay_id", "subject_id", "hadm_id",
  "patientunitstayid", "patienthealthsystemstayid", "person_key",
  "hospital_id", "hospitalid", "source_stay_id", "source_patient_id",
  "source_hospital_id", "patient_id"
)
public_csv_paths <- c(
  aggregate_paths, qc_paths[names(qc_paths) != "summary"], output_manifest_path
)
public_headers <- unique(unlist(lapply(public_csv_paths, function(path) {
  names(fread(path, nrows = 0L, showProgress = FALSE))
})))
if (any(public_headers %in% identifier_headers)) {
  stop("A row-level identifier header entered an aggregate/QC CSV.")
}
if (!identical(
  tail(readLines(qc_paths[["summary"]], warn = FALSE), 1L),
  "BUILD_COMPLETE"
)) stop("Phase 3c summary sentinel is missing.")

completion <- data.table(
  field = c(
    "status", "config_version", "completed_at", "script_sha256",
    "checkpoint_sha256", "analysis_script_manifest_sha256",
    "model_utils_sha256", "analysis_utils_sha256", "parameter_gate_sha256",
    "selection_gate_sha256", "mimic_severity_gate_sha256",
    "eicu_severity_gate_sha256", "mimic_phase2_gate_sha256",
    "eicu_phase2_gate_sha256", "warning_gate_sha256",
    "outcome_gate_sha256", "main_model_gate_sha256",
    "access_receipt_sha256", "parameter_rds_sha256",
    "mimic_model_frame_rds_sha256", "eicu_model_frame_rds_sha256",
    "mimic_severity_rds_sha256", "eicu_severity_rds_sha256",
    "warning_free_rds_sha256", "mimic_preferred_reselected_rds_sha256",
    "eicu_preferred_reselected_rds_sha256", "mimic_weight_rds_sha256",
    "eicu_weight_rds_sha256", "eicu_support_weight_rds_sha256",
    "mimic_outcome_rds_sha256", "eicu_outcome_rds_sha256",
    "main_model_rds_sha256", "sensitivity_model_rds_sha256",
    "sensitivity_prediction_rds_sha256", "aggregate_manifest_sha256",
    "all_input_gate_checks_pass", "all_exact_join_checks_pass",
    "all_design_column_checks_pass", "all_required_outputs_present",
    "warning_free_restriction_n", "mimic_preferred_restriction_n",
    "eicu_preferred_restriction_n", "preferred_reselected_outcome_status",
    "preferred_modeled_estimand", "selection_weighting_implemented",
    "selection_weighting_interpretation", "full_target_positivity_sensitive",
    "supported_hospital_target_changes_target", "secondary_endpoints_implemented",
    "pbw_5y_common_predictor_n", "pbw_1y_common_predictor_n",
    "pbw_chartevents_only_common_predictor_n",
    "pbw_nonestimability_reported_without_rescue",
    "multiple_imputation_implemented", "center_heterogeneity_implemented",
    "native_score_modeling_implemented", "infection_window_variants_implemented",
    "external_alternate_pbw_implemented", "summary_sentinel"
  ),
  value = as.character(c(
    "PASS", LOCKED$version, format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    sha256_file(script_path), sha256_file(checkpoint_path),
    sha256_file(analysis_manifest_path), sha256_file(model_utils_path),
    sha256_file(analysis_utils_path), sha256_file(parameter_gate_path),
    sha256_file(selection_gate_path), sha256_file(mimic_severity_gate_path),
    sha256_file(eicu_severity_gate_path), sha256_file(mimic_phase2_gate_path),
    sha256_file(eicu_phase2_gate_path), sha256_file(warning_gate_path),
    sha256_file(outcome_gate_path), sha256_file(main_model_gate_path),
    sha256_file(access_receipt_path),
    sha256_file(parameter_artifact_paths$parameter),
    sha256_file(parameter_artifact_paths$mimic_frame),
    sha256_file(parameter_artifact_paths$eicu_frame),
    sha256_file(mimic_severity_rds_path), sha256_file(eicu_severity_rds_path),
    sha256_file(warning_free_rds_path),
    sha256_file(mimic_preferred_reselected_path),
    sha256_file(eicu_preferred_reselected_path),
    sha256_file(mimic_weight_rds_path), sha256_file(eicu_weight_rds_path),
    sha256_file(eicu_support_weight_rds_path),
    sha256_file(mimic_outcome_rds_path), sha256_file(eicu_outcome_rds_path),
    sha256_file(main_model_rds_path), sha256_file(private_paths[["models"]]),
    sha256_file(private_paths[["predictions"]]), sha256_file(output_manifest_path),
    all(input_hash_qc$pass),
    all(exact_join_qc$exact_id_set_equality & exact_join_qc$cardinality_preserved),
    all(model_matrix_qc$signature_matches_main),
    all(file.exists(c(
      private_paths, aggregate_paths, qc_paths, output_manifest_path
    ))),
    length(warning_free_ids), length(mimic_preferred_ids),
    length(eicu_preferred_ids),
    "BLOCKED_REQUIRES_PREFERRED_TIME_HSC_REBUILD",
    "primary_tuple_preferred_source_restriction", TRUE,
    "selection-weighted under the specified observation model", TRUE, TRUE,
    TRUE,
    pbw_scale_qc[pbw_definition == "omr_5y_primary",
      component_normalized_common_n],
    pbw_scale_qc[pbw_definition == "omr_1y_fallback",
      component_normalized_common_n],
    pbw_scale_qc[pbw_definition == "chartevents_only",
      component_normalized_common_n],
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, "BUILD_COMPLETE"
  ))
)
if (nrow(completion) != length(completion$value) ||
    anyDuplicated(completion$field)) stop("Malformed Phase 3c completion gate.")
completion_tmp <- paste0(completion_gate, ".tmp.", Sys.getpid())
unlink(completion_tmp, force = TRUE)
fwrite(completion, completion_tmp)
if (!file.link(completion_tmp, completion_gate)) {
  unlink(completion_tmp, force = TRUE)
  stop(paste(
    "Could not atomically create the Phase 3c sensitivity PASS gate",
    "without replacing an existing file."
  ))
}
unlink(completion_tmp, force = TRUE)

message("Locked sensitivity analysis complete.")
message("  gate: ", completion_gate)
