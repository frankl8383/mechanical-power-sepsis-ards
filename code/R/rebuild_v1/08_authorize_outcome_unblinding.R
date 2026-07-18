#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: final outcome-unblinding authorization.
#
# GOVERNANCE BOUNDARY
# -------------------
# This script is outcome-free. It must never open, hash, or inspect a raw
# outcome-bearing table. It verifies only project-local scripts, design
# documents, outcome-free gates/QC, and checksum-gated private predictor-side
# artifacts. The checkpoint is created once, atomically, only after every check
# and the synthetic model-utility self-test pass.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/08_authorize_outcome_unblinding.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
project_root <- normalizePath(
  file.path(script_dir, "..", "..", ".."), mustWork = TRUE
)
expected_config_version <- "1.0.1"

unblinding_dir <- file.path(
  project_root, "analysis_rebuild_v1", "qc", "unblinding"
)
checkpoint_path <- file.path(
  unblinding_dir, "outcome_unblinding_checkpoint_v1.csv"
)
analysis_manifest_path <- file.path(
  unblinding_dir, "analysis_script_manifest_v1.csv"
)
preauthorization_qc_path <- file.path(
  unblinding_dir, "preauthorization_checks_v1.csv"
)
preauthorization_qc_relative <- file.path(
  "analysis_rebuild_v1", "qc", "unblinding", "preauthorization_checks_v1.csv"
)
analysis_manifest_relative <- file.path(
  "analysis_rebuild_v1", "qc", "unblinding",
  "analysis_script_manifest_v1.csv"
)

# A published authorization is immutable. Do not even refresh its manifest.
if (file.exists(checkpoint_path)) {
  stop(
    "Authorization checkpoint already exists; refusing to overwrite it: ",
    checkpoint_path
  )
}

# A clean preauthorization boundary is mandatory. These are only the formal
# outcome products/receipt, not their raw sources; no raw outcome table is
# defined, opened, or hashed here.
outcome_access_receipt_path <- file.path(
  unblinding_dir, "outcome_access_receipt_v1.csv"
)
outcome_private_dir <- file.path(
  project_root, "analysis_rebuild_v1", "private", "outcomes"
)
outcome_qc_dir <- file.path(
  project_root, "analysis_rebuild_v1", "qc", "outcomes"
)
phase3_component_names <- c(
  "locked_models", "locked_sensitivities", "missing_data",
  "center_heterogeneity", "native_benchmarks"
)
forbidden_phase3_dirs <- c(
  file.path(
    project_root, "analysis_rebuild_v1", "private", phase3_component_names
  ),
  file.path(
    project_root, "analysis_rebuild_v1", "aggregate", phase3_component_names
  ),
  file.path(
    project_root, "analysis_rebuild_v1", "qc", phase3_component_names
  )
)
forbidden_preexisting_outcome_products <- c(
  outcome_access_receipt_path,
  file.path(outcome_qc_dir, "phase3a_rebuilt_outcomes_complete_v1.csv"),
  file.path(outcome_private_dir, "mimic_rebuilt_outcomes_v1.rds"),
  file.path(outcome_private_dir, "eicu_rebuilt_outcomes_v1.rds"),
  file.path(outcome_qc_dir, "rebuilt_outcome_summary_v1.csv"),
  file.path(outcome_qc_dir, "rebuilt_outcome_ineligibility_reasons_v1.csv"),
  file.path(outcome_qc_dir, "rebuilt_outcome_timing_audit_v1.csv")
)
if (dir.exists(outcome_private_dir) ||
    any(dir.exists(forbidden_phase3_dirs)) ||
    any(file.exists(forbidden_preexisting_outcome_products)) ||
    (dir.exists(outcome_qc_dir) &&
       length(list.files(outcome_qc_dir, all.files = TRUE, no.. = TRUE)))) {
  stop(
    "Preauthorization boundary is not clean: an outcome receipt, Phase-3a ",
    "gate, private outcome directory, or Phase-3 analysis output already exists."
  )
}

project_file <- function(path, label = "project file") {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop(label, " path is empty.")
  }
  candidate <- if (grepl("^/", path)) path else file.path(project_root, path)
  if (!file.exists(candidate)) stop("Missing ", label, ": ", candidate)
  resolved <- normalizePath(candidate, mustWork = TRUE)
  prefix <- paste0(project_root, .Platform$file.sep)
  if (!startsWith(resolved, prefix)) {
    stop(label, " is outside the project; it will not be opened or hashed: ", path)
  }
  resolved
}

project_relative <- function(path) {
  resolved <- project_file(path)
  substring(resolved, nchar(project_root) + 2L)
}

# Deliberately restricted to project-local files. This makes it impossible for
# this authorization script to hash a raw database table outside the project.
sha256_file <- function(path) {
  resolved <- project_file(path, "hash target")
  out <- system2(
    "shasum", c("-a", "256", shQuote(resolved)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA256 failed for ", resolved, ": ", paste(out, collapse = " "))
  }
  hash <- strsplit(out[[1L]], "[[:space:]]+")[[1L]][1L]
  if (!grepl("^[0-9a-f]{64}$", hash)) stop("Invalid SHA256 for ", resolved)
  hash
}

read_completion_gate <- function(path, label) {
  resolved <- project_file(path, label)
  z <- fread(resolved, colClasses = "character", showProgress = FALSE)
  if (identical(names(z), c("field", "value"))) {
    if (!nrow(z) || anyDuplicated(z$field) || anyNA(z$field) ||
        any(!nzchar(z$field))) {
      stop("Malformed field/value ", label, ": ", resolved)
    }
    return(setNames(as.character(z$value), z$field))
  }
  if (nrow(z) != 1L || anyDuplicated(names(z)) || anyNA(names(z)) ||
      any(!nzchar(names(z)))) {
    stop("Malformed one-row ", label, ": ", resolved)
  }
  setNames(vapply(z, function(x) as.character(x[[1L]]), character(1L)), names(z))
}

require_value <- function(gate, field, expected = NULL, label = "gate") {
  value <- unname(gate[field])
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(label, " lacks a non-empty field: ", field)
  }
  if (!is.null(expected) && !identical(value, as.character(expected))) {
    stop(label, " mismatch for ", field, ": ", value, " != ", expected)
  }
  value
}

require_hash <- function(gate, field, path, label = "gate") {
  require_value(gate, field, sha256_file(path), label)
}

require_all_true_csv <- function(path, field_candidates, label) {
  z <- fread(project_file(path, label), colClasses = "character", showProgress = FALSE)
  field <- intersect(field_candidates, names(z))
  if (!nrow(z) || length(field) != 1L) {
    stop(label, " must contain one recognized non-empty pass column.")
  }
  value <- tolower(trimws(as.character(z[[field]])))
  if (anyNA(value) || !all(value %in% c("true", "pass", "1"))) {
    stop(label, " contains a failed or missing check.")
  }
  invisible(TRUE)
}

require_summary_sentinel <- function(path, label) {
  lines <- readLines(project_file(path, label), warn = FALSE)
  if (!length(lines) || !identical(tail(lines, 1L), "BUILD_COMPLETE")) {
    stop(label, " lacks BUILD_COMPLETE sentinel.")
  }
  invisible(TRUE)
}

require_fresh_outputs <- function(outputs, inputs, label) {
  output_paths <- vapply(outputs, project_file, character(1L), label = label)
  input_paths <- vapply(inputs, project_file, character(1L), label = label)
  output_time <- file.info(output_paths)$mtime
  input_time <- file.info(input_paths)$mtime
  if (anyNA(output_time) || anyNA(input_time) || min(output_time) < max(input_time)) {
    stop(label, " is older than its current script/configuration input.")
  }
  invisible(TRUE)
}

verify_artifact_hashes <- function(gate, artifacts, label) {
  if (length(artifacts)) {
    for (field in names(artifacts)) require_hash(gate, field, artifacts[[field]], label)
  }
  invisible(TRUE)
}

verify_cache_bundle <- function(
    gate_path, manifest_path, helper_path, expected_rows,
    require_official_sha = FALSE, label = "cache") {
  gate <- read_completion_gate(gate_path, paste(label, "gate"))
  require_value(gate, "status", "PASS", paste(label, "gate"))
  require_hash(gate, "helper_sha256", helper_path, paste(label, "gate"))
  require_hash(gate, "manifest_sha256", manifest_path, paste(label, "gate"))
  require_value(gate, "spec_count", expected_rows, paste(label, "gate"))
  if ("all_sources_reached_eof" %in% names(gate)) {
    require_value(gate, "all_sources_reached_eof", "TRUE", paste(label, "gate"))
  }
  if (require_official_sha) {
    require_value(
      gate, "all_official_sha256_match", "TRUE", paste(label, "gate")
    )
  }

  manifest <- fread(
    project_file(manifest_path, paste(label, "manifest")),
    colClasses = "character", showProgress = FALSE
  )
  required <- c(
    "source_name", "output_path", "output_sha256", "reached_eof",
    "helper_sha256", "status"
  )
  if (nrow(manifest) != expected_rows ||
      length(setdiff(required, names(manifest))) ||
      anyDuplicated(manifest$source_name) || anyDuplicated(manifest$output_path) ||
      any(toupper(manifest$status) != "PASS") ||
      any(toupper(manifest$reached_eof) != "TRUE") ||
      any(manifest$helper_sha256 != sha256_file(helper_path))) {
    stop(label, " manifest invariant failed.")
  }
  if (require_official_sha && (
    !"official_sha256_match" %in% names(manifest) ||
      any(toupper(manifest$official_sha256_match) != "TRUE")
  )) {
    stop(label, " manifest does not attest official raw-file hashes.")
  }
  for (i in seq_len(nrow(manifest))) {
    output <- project_file(manifest$output_path[[i]], paste(label, "cache output"))
    if (!identical(tolower(manifest$output_sha256[[i]]), sha256_file(output))) {
      stop(label, " cache-output hash mismatch: ", output)
    }
  }
  invisible(gate)
}

# ---------------------------------------------------------------------------
# Locked project paths. No raw outcome-source path is defined in this script.
# ---------------------------------------------------------------------------

scripts <- list(
  config = file.path(script_dir, "00_config.R"),
  preflight = file.path(script_dir, "00_preflight.R"),
  phase0 = file.path(script_dir, "00_phase0_lock.R"),
  core_integrity = file.path(script_dir, "00_core_integrity.R"),
  mimic_phase1 = file.path(script_dir, "01_build_mimic_index_cohort.R"),
  eicu_phase1 = file.path(script_dir, "02_build_eicu_index_cohort.R"),
  mimic_phase2 = file.path(script_dir, "03_build_mimic_paired_exposure.R"),
  warning = file.path(script_dir, "03b_build_mimic_warning_free_sensitivity.R"),
  eicu_phase2 = file.path(script_dir, "04_build_eicu_paired_exposure.R"),
  mimic_severity = file.path(script_dir, "05_build_mimic_severity_core.R"),
  mimic_filter = file.path(script_dir, "05a_filter_mimic_severity_inputs.py"),
  oasis = file.path(script_dir, "05c_build_mimic_native_oasis.R"),
  oasis_filter = file.path(script_dir, "05d_filter_mimic_oasis_inputs.py"),
  eicu_severity = file.path(script_dir, "06_build_eicu_severity_core.R"),
  eicu_filter = file.path(script_dir, "06a_filter_eicu_severity_inputs.py"),
  parameter = file.path(script_dir, "07_freeze_predictor_parameters.R"),
  selection = file.path(script_dir, "07b_build_selection_weights.R"),
  authorization = script_path,
  model_utils = file.path(script_dir, "08_model_utils.R"),
  model_utils_selftest = file.path(script_dir, "08_model_utils_selftest.R"),
  locked_analysis_utils = file.path(script_dir, "08a_locked_analysis_utils.R"),
  locked_analysis_utils_selftest = file.path(
    script_dir, "08a_locked_analysis_utils_selftest.R"
  ),
  outcomes = file.path(script_dir, "09_extract_rebuilt_outcomes.R"),
  models = file.path(script_dir, "10_fit_locked_models.R"),
  locked_sensitivities = file.path(
    script_dir, "11_fit_locked_sensitivities.R"
  ),
  missing_data_sensitivities = file.path(
    script_dir, "12_fit_missing_data_sensitivities.R"
  ),
  center_heterogeneity = file.path(
    script_dir, "13_fit_center_heterogeneity.R"
  ),
  native_benchmark = file.path(
    script_dir, "14_fit_native_benchmarks.R"
  )
)
missing_scripts <- names(scripts)[!file.exists(unlist(scripts, use.names = FALSE))]
if (length(missing_scripts)) {
  stop("Required locked script(s) missing: ", paste(missing_scripts, collapse = ", "))
}

source(scripts$config)
if (!identical(LOCKED$version, expected_config_version) ||
    !identical(normalizePath(PROJECT_ROOT, mustWork = TRUE), project_root)) {
  stop("Configuration/project root differs from the authorization target.")
}

docs <- list(
  sap = file.path(project_root, "docs", "rebuild_v1", "SAP_v1_0.md"),
  dictionary = file.path(
    project_root, "docs", "rebuild_v1", "data_dictionary_v1.md"
  ),
  decision_log = file.path(
    project_root, "docs", "rebuild_v1", "analysis_decision_log.md"
  ),
  terminology = file.path(
    project_root, "docs", "rebuild_v1", "terminology_ledger.md"
  )
)
if (any(!file.exists(unlist(docs, use.names = FALSE)))) {
  stop("A required design-lock document is missing.")
}

qc_root <- file.path(project_root, "analysis_rebuild_v1", "qc")
private_root <- file.path(project_root, "analysis_rebuild_v1", "private")

paths <- list(
  preflight_inventory = file.path(qc_root, "preflight_file_inventory.csv"),
  preflight_manifest = file.path(qc_root, "preflight_run_manifest.csv"),
  preflight_summary = file.path(qc_root, "preflight_QC.md"),
  phase0_checks = file.path(qc_root, "phase0_consistency_checks.csv"),
  phase0_manifest = file.path(qc_root, "phase0_lock_manifest.csv"),
  phase0_summary = file.path(qc_root, "phase0_lock_QC.md"),
  core_integrity = file.path(qc_root, "core_file_integrity.csv"),
  core_integrity_summary = file.path(qc_root, "core_file_integrity_QC.md"),
  mimic_phase1_gate = file.path(qc_root, "mimic", "phase1_complete_v1.csv"),
  eicu_phase1_gate = file.path(qc_root, "eicu", "phase1_eicu_complete_v1.csv"),
  mimic_phase2_gate = file.path(
    qc_root, "mimic_exposure", "phase2_mimic_exposure_complete_v1.csv"
  ),
  eicu_phase2_gate = file.path(
    qc_root, "eicu_exposure", "phase2_eicu_exposure_complete_v1.csv"
  ),
  warning_gate = file.path(
    qc_root, "mimic_warning_sensitivity",
    "phase2c_mimic_warning_sensitivity_complete_v1.csv"
  ),
  oasis_gate = file.path(
    qc_root, "mimic_native_oasis",
    "phase2c_mimic_native_oasis_complete_v1.csv"
  ),
  mimic_severity_gate = file.path(
    qc_root, "mimic_severity", "phase2b_mimic_severity_complete_v1.csv"
  ),
  eicu_severity_gate = file.path(
    qc_root, "eicu_severity", "phase2b_complete_v1.csv"
  ),
  parameter_gate = file.path(
    qc_root, "parameter_freeze", "phase2e_parameter_freeze_complete_v1.csv"
  ),
  selection_gate = file.path(
    qc_root, "selection_weights", "phase2d_selection_weights_complete_v1.csv"
  )
)
if (any(!file.exists(unlist(paths, use.names = FALSE)))) {
  stop("A required pre-outcome gate/QC artifact is missing.")
}

# ---------------------------------------------------------------------------
# Latest outcome-free preflight, Phase-0 lock, and core-integrity PASS.
# ---------------------------------------------------------------------------

preflight_inventory <- fread(
  paths$preflight_inventory, colClasses = "character", showProgress = FALSE
)
expected_preflight <- c(
  "hosp/patients.csv.gz", "hosp/admissions.csv.gz", "hosp/omr.csv.gz",
  "hosp/labevents.csv.gz", "hosp/d_labitems.csv.gz",
  "hosp/microbiologyevents.csv.gz", "hosp/prescriptions.csv.gz",
  "icu/icustays.csv.gz", "icu/chartevents.csv.gz", "icu/d_items.csv.gz",
  "icu/procedureevents.csv.gz", "icu/inputevents.csv.gz",
  "icu/outputevents.csv.gz", "patient.csv.gz",
  "respiratoryCharting.csv.gz", "respiratoryCare.csv.gz", "lab.csv.gz",
  "diagnosis.csv.gz", "admissionDx.csv.gz", "apacheApsVar.csv.gz",
  "apachePatientResult.csv.gz", "hospital.csv.gz", "nurseCharting.csv.gz",
  "infusionDrug.csv.gz", "medication.csv.gz", "intakeOutput.csv.gz",
  "final_manuscript_package/ards_mp_FINAL_submission.zip"
)
required_preflight_columns <- c(
  "database", "relative_path", "exists", "size_bytes", "readable"
)
if (nrow(preflight_inventory) != 27L ||
    length(setdiff(required_preflight_columns, names(preflight_inventory))) ||
    anyDuplicated(preflight_inventory[, paste(database, relative_path)]) ||
    !setequal(preflight_inventory$relative_path, expected_preflight) ||
    any(toupper(preflight_inventory$exists) != "TRUE") ||
    any(toupper(preflight_inventory$readable) != "TRUE") ||
    any(as.numeric(preflight_inventory$size_bytes) <= 0)) {
  stop("Latest 27-input preflight inventory is incomplete or not PASS.")
}
preflight_manifest <- read_completion_gate(
  paths$preflight_manifest, "preflight run manifest"
)
require_value(
  preflight_manifest, "config_version", LOCKED$version,
  "preflight run manifest"
)
require_value(preflight_manifest, "run_time", label = "preflight run manifest")
require_fresh_outputs(
  c(paths$preflight_inventory, paths$preflight_manifest, paths$preflight_summary),
  c(scripts$preflight, scripts$config), "preflight outputs"
)

require_all_true_csv(paths$phase0_checks, "passed", "Phase-0 consistency checks")
phase0_manifest <- fread(
  paths$phase0_manifest, colClasses = "character", showProgress = FALSE
)
if (!all(c("file", "bytes", "sha256", "config_version") %in%
         names(phase0_manifest)) || nrow(phase0_manifest) != 5L ||
    anyDuplicated(phase0_manifest$file) ||
    any(phase0_manifest$config_version != LOCKED$version)) {
  stop("Phase-0 manifest schema/version invariant failed.")
}
phase0_expected <- unlist(c(list(scripts$config), docs), use.names = FALSE)
phase0_paths <- vapply(
  phase0_manifest$file, project_file, character(1L), label = "Phase-0 file"
)
if (!setequal(phase0_paths, normalizePath(phase0_expected, mustWork = TRUE))) {
  stop("Phase-0 manifest does not contain exactly config plus four design documents.")
}
for (i in seq_len(nrow(phase0_manifest))) {
  if (!identical(tolower(phase0_manifest$sha256[[i]]), sha256_file(phase0_paths[[i]])) ||
      as.numeric(phase0_manifest$bytes[[i]]) != file.info(phase0_paths[[i]])$size) {
    stop("Phase-0 manifest is stale for: ", phase0_paths[[i]])
  }
}
require_fresh_outputs(
  c(paths$phase0_checks, paths$phase0_manifest, paths$phase0_summary),
  c(scripts$phase0, phase0_expected), "Phase-0 lock outputs"
)

core_qc <- fread(
  paths$core_integrity, colClasses = "character", showProgress = FALSE
)
expected_core_files <- c(
  "chartevents.csv.gz", "labevents.csv.gz", "patient.csv.gz",
  "respiratoryCharting.csv.gz", "respiratoryCare.csv.gz", "lab.csv.gz",
  "diagnosis.csv.gz", "admissionDx.csv.gz", "apacheApsVar.csv.gz",
  "apachePatientResult.csv.gz", "hospital.csv.gz"
)
if (nrow(core_qc) != length(expected_core_files) ||
    !all(c("file", "pass") %in% names(core_qc)) ||
    anyDuplicated(core_qc$file) || !setequal(core_qc$file, expected_core_files) ||
    any(toupper(core_qc$pass) != "TRUE")) {
  stop("Latest core-file integrity QC is not a complete PASS.")
}
require_fresh_outputs(
  c(paths$core_integrity, paths$core_integrity_summary),
  c(scripts$core_integrity, scripts$config), "core-integrity outputs"
)

# ---------------------------------------------------------------------------
# Phase 1 and Phase 2 gates, scripts, invariants, leakage guards, and RDSs.
# ---------------------------------------------------------------------------

mimic_p1 <- read_completion_gate(paths$mimic_phase1_gate, "MIMIC Phase-1 gate")
eicu_p1 <- read_completion_gate(paths$eicu_phase1_gate, "eICU Phase-1 gate")
for (z in list(mimic_p1, eicu_p1)) {
  require_value(z, "locked_config_version", LOCKED$version, "Phase-1 gate")
  require_value(z, "all_invariants_pass", "TRUE", "Phase-1 gate")
  require_value(z, "outcome_leakage_guard_pass", "TRUE", "Phase-1 gate")
}
require_hash(mimic_p1, "script_sha256", scripts$mimic_phase1, "MIMIC Phase-1 gate")
require_hash(eicu_p1, "script_sha256", scripts$eicu_phase1, "eICU Phase-1 gate")
if ("all_required_qc_present" %in% names(eicu_p1)) {
  require_value(eicu_p1, "all_required_qc_present", "TRUE", "eICU Phase-1 gate")
}

mimic_p1_artifacts <- c(
  stay_candidates_rds_sha256 = file.path(
    private_root, "mimic", "mimic_index_stay_candidates_v1.rds"
  ),
  primary_cohort_rds_sha256 = file.path(
    private_root, "mimic", "mimic_index_cohort_v1.rds"
  ),
  infection_plus24_sensitivity_rds_sha256 = file.path(
    private_root, "mimic", "mimic_index_cohort_infection_plus24_sensitivity_v1.rds"
  ),
  exact_culture_time_sensitivity_rds_sha256 = file.path(
    private_root, "mimic", "mimic_index_cohort_exact_culture_time_sensitivity_v1.rds"
  )
)
eicu_p1_artifacts <- c(
  stay_candidates_rds_sha256 = file.path(
    private_root, "eicu", "eicu_index_stay_candidates_v1.rds"
  ),
  primary_cohort_rds_sha256 = file.path(
    private_root, "eicu", "eicu_index_cohort_v1.rds"
  ),
  infection_plus24_sensitivity_rds_sha256 = file.path(
    private_root, "eicu", "eicu_index_cohort_infection_plus24_sensitivity_v1.rds"
  ),
  strict120_sensitivity_rds_sha256 = file.path(
    private_root, "eicu", "eicu_index_cohort_strict120_sensitivity_v1.rds"
  )
)
verify_artifact_hashes(mimic_p1, mimic_p1_artifacts, "MIMIC Phase-1 gate")
verify_artifact_hashes(eicu_p1, eicu_p1_artifacts, "eICU Phase-1 gate")
require_all_true_csv(
  file.path(qc_root, "mimic", "qc_invariants_v1.csv"),
  "passed", "MIMIC Phase-1 invariants"
)
require_all_true_csv(
  file.path(qc_root, "mimic", "qc_outcome_leakage_guard_v1.csv"),
  "passed", "MIMIC Phase-1 leakage guard"
)
require_all_true_csv(
  file.path(qc_root, "eicu", "qc_invariants_v1.csv"),
  "passed", "eICU Phase-1 invariants"
)
require_all_true_csv(
  file.path(qc_root, "eicu", "qc_outcome_leakage_guard_v1.csv"),
  "passed", "eICU Phase-1 leakage guard"
)

mimic_p2 <- read_completion_gate(paths$mimic_phase2_gate, "MIMIC Phase-2 gate")
eicu_p2 <- read_completion_gate(paths$eicu_phase2_gate, "eICU Phase-2 gate")
for (z in list(mimic_p2, eicu_p2)) {
  require_value(z, "locked_config_version", LOCKED$version, "Phase-2 gate")
  require_value(z, "all_invariants_pass", "TRUE", "Phase-2 gate")
  require_value(z, "outcome_leakage_guard_pass", "TRUE", "Phase-2 gate")
  require_value(z, "all_required_qc_present", "TRUE", "Phase-2 gate")
}
require_hash(mimic_p2, "script_sha256", scripts$mimic_phase2, "MIMIC Phase-2 gate")
require_hash(eicu_p2, "script_sha256", scripts$eicu_phase2, "eICU Phase-2 gate")
require_hash(mimic_p2, "phase1_gate_sha256", paths$mimic_phase1_gate, "MIMIC Phase-2 gate")
require_hash(eicu_p2, "phase1_gate_sha256", paths$eicu_phase1_gate, "eICU Phase-2 gate")
require_hash(
  mimic_p2, "input_primary_cohort_sha256",
  mimic_p1_artifacts[["primary_cohort_rds_sha256"]], "MIMIC Phase-2 gate"
)
require_hash(
  eicu_p2, "input_primary_cohort_sha256",
  eicu_p1_artifacts[["primary_cohort_rds_sha256"]], "eICU Phase-2 gate"
)
mimic_p2_artifacts <- c(
  primary_60min_rds_sha256 = file.path(
    private_root, "mimic", "mimic_paired_exposure_primary_60min_v1.rds"
  ),
  sensitivity_30min_rds_sha256 = file.path(
    private_root, "mimic", "mimic_paired_exposure_sensitivity_30min_v1.rds"
  ),
  sensitivity_preferred_60min_rds_sha256 = file.path(
    private_root, "mimic", "mimic_paired_exposure_sensitivity_preferred_60min_v1.rds"
  ),
  all_valid_primary_60min_rds_sha256 = file.path(
    private_root, "mimic", "mimic_paired_exposure_all_valid_primary_60min_v1.rds"
  )
)
eicu_p2_artifacts <- c(
  primary_60min_rds_sha256 = file.path(
    private_root, "eicu", "eicu_paired_exposure_primary_60min_v1.rds"
  ),
  sensitivity_30min_rds_sha256 = file.path(
    private_root, "eicu", "eicu_paired_exposure_sensitivity_30min_v1.rds"
  ),
  sensitivity_preferred_60min_rds_sha256 = file.path(
    private_root, "eicu", "eicu_paired_exposure_sensitivity_preferred_60min_v1.rds"
  ),
  all_valid_primary_60min_rds_sha256 = file.path(
    private_root, "eicu", "eicu_paired_exposure_all_valid_primary_60min_v1.rds"
  )
)
verify_artifact_hashes(mimic_p2, mimic_p2_artifacts, "MIMIC Phase-2 gate")
verify_artifact_hashes(eicu_p2, eicu_p2_artifacts, "eICU Phase-2 gate")
for (database in c("mimic", "eicu")) {
  require_all_true_csv(
    file.path(qc_root, paste0(database, "_exposure"),
      "paired_exposure_invariant_tests.csv"),
    "pass", paste(database, "Phase-2 invariants")
  )
  require_all_true_csv(
    file.path(qc_root, paste0(database, "_exposure"),
      "outcome_leakage_guard.csv"),
    "pass", paste(database, "Phase-2 leakage guard")
  )
}

# Warning-free selected-tuple sensitivity is a formal outcome-free Phase-2c gate.
warning_gate <- read_completion_gate(paths$warning_gate, "warning-sensitivity gate")
require_value(warning_gate, "status", "PASS", "warning-sensitivity gate")
require_value(warning_gate, "config_version", LOCKED$version, "warning-sensitivity gate")
require_hash(warning_gate, "script_sha256", scripts$warning, "warning-sensitivity gate")
require_hash(
  warning_gate, "phase2_gate_sha256", paths$mimic_phase2_gate,
  "warning-sensitivity gate"
)
warning_artifacts <- c(
  source_cache_sha256 = file.path(
    private_root, "mimic", "cache_v1", "selected_paired_exposure_chartevents_v1.rds"
  ),
  annotated_rds_sha256 = file.path(
    private_root, "mimic", "mimic_primary_selected_tuple_warning_flags_v1.rds"
  ),
  warning_free_rds_sha256 = file.path(
    private_root, "mimic",
    "mimic_paired_exposure_sensitivity_warning_free_selected_v1.rds"
  )
)
verify_artifact_hashes(warning_gate, warning_artifacts, "warning-sensitivity gate")
require_all_true_csv(
  file.path(qc_root, "mimic_warning_sensitivity", "warning_sensitivity_guard.csv"),
  "pass", "warning-sensitivity guard"
)
require_summary_sentinel(
  file.path(qc_root, "mimic_warning_sensitivity", "mimic_warning_sensitivity_QC.md"),
  "warning-sensitivity summary"
)

# ---------------------------------------------------------------------------
# Cache-gated severity and source-faithful native OASIS.
# ---------------------------------------------------------------------------

mimic_cache_gate <- file.path(
  private_root, "mimic", "cache_v1", "mimic_severity",
  "severity_input_cache_complete_v1.csv"
)
mimic_cache_manifest <- file.path(
  private_root, "mimic", "cache_v1", "mimic_severity", "filter_manifest_v1.csv"
)
eicu_cache_gate <- file.path(
  private_root, "eicu", "cache_v1", "eicu_severity",
  "severity_input_cache_complete_v1.csv"
)
eicu_cache_manifest <- file.path(
  private_root, "eicu", "cache_v1", "eicu_severity", "filter_manifest_v1.csv"
)
oasis_cache_gate <- file.path(
  private_root, "mimic", "cache_v1", "mimic_native_oasis",
  "oasis_input_cache_complete_v1.csv"
)
oasis_cache_manifest <- file.path(
  private_root, "mimic", "cache_v1", "mimic_native_oasis", "filter_manifest_v1.csv"
)
verify_cache_bundle(
  mimic_cache_gate, mimic_cache_manifest, scripts$mimic_filter, 4L,
  require_official_sha = TRUE, label = "MIMIC severity cache"
)
verify_cache_bundle(
  eicu_cache_gate, eicu_cache_manifest, scripts$eicu_filter, 4L,
  require_official_sha = FALSE, label = "eICU severity cache"
)
verify_cache_bundle(
  oasis_cache_gate, oasis_cache_manifest, scripts$oasis_filter, 2L,
  require_official_sha = TRUE, label = "native-OASIS cache"
)

oasis_gate <- read_completion_gate(paths$oasis_gate, "native-OASIS gate")
require_value(oasis_gate, "status", "PASS", "native-OASIS gate")
require_value(
  oasis_gate, "locked_config_version", LOCKED$version, "native-OASIS gate"
)
for (field in c(
  "all_invariants_pass", "synthetic_rule_tests_pass",
  "outcome_leakage_guard_pass", "all_event_sources_reached_eof",
  "all_raw_sha256_match_official", "all_required_qc_present"
)) require_value(oasis_gate, field, "TRUE", "native-OASIS gate")
for (field in c(
  "actual_outcome_fields_read", "predicted_probability_executed",
  "hsc_substitute_allowed"
)) require_value(oasis_gate, field, "FALSE", "native-OASIS gate")
require_hash(oasis_gate, "script_sha256", scripts$oasis, "native-OASIS gate")
require_hash(oasis_gate, "helper_sha256", scripts$oasis_filter, "native-OASIS gate")
require_hash(oasis_gate, "phase1_gate_sha256", paths$mimic_phase1_gate, "native-OASIS gate")
require_hash(
  oasis_gate, "input_strict_cohort_sha256",
  mimic_p1_artifacts[["primary_cohort_rds_sha256"]], "native-OASIS gate"
)
require_hash(oasis_gate, "input_cache_gate_sha256", oasis_cache_gate, "native-OASIS gate")
require_hash(
  oasis_gate, "input_cache_manifest_sha256", oasis_cache_manifest,
  "native-OASIS gate"
)
oasis_rds <- file.path(private_root, "mimic", "mimic_native_oasis_benchmark_v1.rds")
require_hash(oasis_gate, "native_oasis_rds_sha256", oasis_rds, "native-OASIS gate")
require_all_true_csv(
  file.path(qc_root, "mimic_native_oasis", "native_oasis_invariant_tests.csv"),
  "pass", "native-OASIS invariants"
)
require_all_true_csv(
  file.path(qc_root, "mimic_native_oasis", "native_oasis_synthetic_rule_tests.csv"),
  "pass", "native-OASIS synthetic tests"
)
require_all_true_csv(
  file.path(qc_root, "mimic_native_oasis", "outcome_leakage_guard.csv"),
  "pass", "native-OASIS leakage guard"
)
require_summary_sentinel(
  file.path(qc_root, "mimic_native_oasis", "mimic_native_oasis_QC.md"),
  "native-OASIS summary"
)

mimic_severity <- read_completion_gate(
  paths$mimic_severity_gate, "MIMIC severity gate"
)
eicu_severity <- read_completion_gate(
  paths$eicu_severity_gate, "eICU severity gate"
)
for (z in list(mimic_severity, eicu_severity)) {
  require_value(z, "status", "PASS", "severity gate")
  require_value(z, "config_version", LOCKED$version, "severity gate")
}
for (field in c(
  "all_invariants_pass", "outcome_leakage_guard_pass",
  "cache_all_reached_eof", "cache_all_official_sha256_match"
)) require_value(mimic_severity, field, "TRUE", "MIMIC severity gate")
require_hash(
  mimic_severity, "script_sha256", scripts$mimic_severity,
  "MIMIC severity gate"
)
require_hash(
  eicu_severity, "script_sha256", scripts$eicu_severity,
  "eICU severity gate"
)
require_hash(mimic_severity, "helper_sha256", scripts$mimic_filter, "MIMIC severity gate")
require_hash(
  eicu_severity, "filter_helper_sha256", scripts$eicu_filter,
  "eICU severity gate"
)
for (z in list(
  list(mimic_severity, paths$mimic_phase1_gate, "phase1_gate_sha256", "MIMIC"),
  list(mimic_severity, paths$mimic_phase2_gate, "phase2_gate_sha256", "MIMIC"),
  list(eicu_severity, paths$eicu_phase1_gate, "phase1_gate_sha256", "eICU"),
  list(eicu_severity, paths$eicu_phase2_gate, "phase2_gate_sha256", "eICU")
)) require_hash(z[[1L]], z[[3L]], z[[2L]], paste(z[[4L]], "severity gate"))
require_hash(
  mimic_severity, "preflight_inventory_sha256", paths$preflight_inventory,
  "MIMIC severity gate"
)
require_hash(
  mimic_severity, "native_oasis_gate_sha256", paths$oasis_gate,
  "MIMIC severity gate"
)
require_hash(
  mimic_severity, "input_cache_gate_sha256", mimic_cache_gate,
  "MIMIC severity gate"
)
require_hash(
  mimic_severity, "input_cache_manifest_sha256", mimic_cache_manifest,
  "MIMIC severity gate"
)
require_hash(
  eicu_severity, "input_cache_gate_sha256", eicu_cache_gate,
  "eICU severity gate"
)
require_hash(
  eicu_severity, "input_cache_manifest_sha256", eicu_cache_manifest,
  "eICU severity gate"
)

mimic_severity_artifacts <- c(
  input_index_rds_sha256 = mimic_p1_artifacts[["primary_cohort_rds_sha256"]],
  input_exposure_rds_sha256 = mimic_p2_artifacts[["primary_60min_rds_sha256"]],
  prediction_hsc_rds_sha256 = file.path(
    private_root, "mimic", "mimic_paired_exposure_with_severity_core_v1.rds"
  ),
  index_selection_rds_sha256 = file.path(
    private_root, "mimic", "mimic_index_known_selection_core_v1.rds"
  ),
  native_feasibility_rds_sha256 = file.path(
    private_root, "mimic", "mimic_native_oasis_feasibility_v1.rds"
  )
)
eicu_severity_artifacts <- c(
  input_exposure_rds_sha256 = eicu_p2_artifacts[["primary_60min_rds_sha256"]],
  prediction_hsc_rds_sha256 = file.path(
    private_root, "eicu", "eicu_paired_exposure_with_severity_core_v1.rds"
  ),
  index_selection_rds_sha256 = file.path(
    private_root, "eicu", "eicu_index_known_selection_core_v1.rds"
  ),
  apache_benchmark_rds_sha256 = file.path(
    private_root, "eicu", "eicu_native_apache_iva_benchmark_v1.rds"
  )
)
verify_artifact_hashes(
  mimic_severity, mimic_severity_artifacts, "MIMIC severity gate"
)
verify_artifact_hashes(eicu_severity, eicu_severity_artifacts, "eICU severity gate")
for (database in c("mimic", "eicu")) {
  require_all_true_csv(
    file.path(qc_root, paste0(database, "_severity"),
      "severity_core_invariant_tests.csv"),
    "pass", paste(database, "severity invariants")
  )
  require_all_true_csv(
    file.path(qc_root, paste0(database, "_severity"),
      "outcome_leakage_guard.csv"),
    "pass", paste(database, "severity leakage guard")
  )
  require_summary_sentinel(
    file.path(qc_root, paste0(database, "_severity"),
      paste0(database, "_severity_core_QC.md")),
    paste(database, "severity summary")
  )
}

# ---------------------------------------------------------------------------
# Frozen predictor parameters and outcome-blind selection weights.
# ---------------------------------------------------------------------------

parameter_gate <- read_completion_gate(paths$parameter_gate, "parameter-freeze gate")
require_value(parameter_gate, "status", "PASS", "parameter-freeze gate")
require_value(
  parameter_gate, "locked_config_version", LOCKED$version,
  "parameter-freeze gate"
)
for (field in c(
  "all_tests_pass", "outcome_leakage_guard_pass", "all_required_qc_present"
)) require_value(parameter_gate, field, "TRUE", "parameter-freeze gate")
require_value(
  parameter_gate, "summary_sentinel", "BUILD_COMPLETE", "parameter-freeze gate"
)
require_value(
  parameter_gate, "parameter_derivation_database", "MIMIC-IV v3.1 only",
  "parameter-freeze gate"
)
require_value(parameter_gate, "quantile_type", "2", "parameter-freeze gate")
require_hash(parameter_gate, "script_sha256", scripts$parameter, "parameter-freeze gate")
require_hash(
  parameter_gate, "model_utils_sha256", scripts$model_utils,
  "parameter-freeze gate"
)
require_hash(
  parameter_gate, "mimic_severity_gate_sha256", paths$mimic_severity_gate,
  "parameter-freeze gate"
)
require_hash(
  parameter_gate, "eicu_severity_gate_sha256", paths$eicu_severity_gate,
  "parameter-freeze gate"
)
require_hash(
  parameter_gate, "mimic_input_rds_sha256",
  mimic_severity_artifacts[["prediction_hsc_rds_sha256"]], "parameter-freeze gate"
)
require_hash(
  parameter_gate, "eicu_input_rds_sha256",
  eicu_severity_artifacts[["prediction_hsc_rds_sha256"]], "parameter-freeze gate"
)
parameter_artifacts <- list()
for (pair in list(
  c("parameter_rds_path", "parameter_rds_sha256"),
  c("mimic_model_frame_rds_path", "mimic_model_frame_rds_sha256"),
  c("eicu_model_frame_rds_path", "eicu_model_frame_rds_sha256")
)) {
  artifact_path <- project_file(
    require_value(parameter_gate, pair[[1L]], label = "parameter-freeze gate"),
    pair[[1L]]
  )
  require_hash(parameter_gate, pair[[2L]], artifact_path, "parameter-freeze gate")
  parameter_artifacts[[pair[[2L]]]] <- artifact_path
}
require_all_true_csv(
  file.path(qc_root, "parameter_freeze", "transformation_tests.csv"),
  "pass", "parameter transformation tests"
)
require_all_true_csv(
  file.path(qc_root, "parameter_freeze", "outcome_leakage_guard.csv"),
  "pass", "parameter leakage guard"
)
require_summary_sentinel(
  file.path(qc_root, "parameter_freeze", "parameter_freeze_QC.md"),
  "parameter-freeze summary"
)

selection_gate <- read_completion_gate(paths$selection_gate, "selection-weight gate")
require_value(selection_gate, "status", "PASS", "selection-weight gate")
require_value(selection_gate, "config_version", LOCKED$version, "selection-weight gate")
require_value(selection_gate, "decision_id", "D055", "selection-weight gate")
for (field in c("all_leakage_checks_pass", "all_required_qc_present")) {
  require_value(selection_gate, field, "TRUE", "selection-weight gate")
}
require_value(
  selection_gate, "summary_sentinel", "BUILD_COMPLETE", "selection-weight gate"
)
require_hash(selection_gate, "script_sha256", scripts$selection, "selection-weight gate")
require_hash(
  selection_gate, "mimic_severity_gate_sha256", paths$mimic_severity_gate,
  "selection-weight gate"
)
require_hash(
  selection_gate, "eicu_severity_gate_sha256", paths$eicu_severity_gate,
  "selection-weight gate"
)
require_hash(
  selection_gate, "mimic_phase2_gate_sha256", paths$mimic_phase2_gate,
  "selection-weight gate"
)
require_hash(
  selection_gate, "eicu_phase2_gate_sha256", paths$eicu_phase2_gate,
  "selection-weight gate"
)
selection_artifacts <- c(
  mimic_input_rds_sha256 = mimic_severity_artifacts[["index_selection_rds_sha256"]],
  eicu_input_rds_sha256 = eicu_severity_artifacts[["index_selection_rds_sha256"]],
  mimic_output_rds_sha256 = file.path(
    private_root, "selection_weights", "mimic_tuple_observation_weights_v1.rds"
  ),
  eicu_output_rds_sha256 = file.path(
    private_root, "selection_weights", "eicu_tuple_observation_weights_v1.rds"
  ),
  eicu_support_output_rds_sha256 = file.path(
    private_root, "selection_weights",
    "eicu_tuple_observation_weights_support_hospitals_v1.rds"
  )
)
verify_artifact_hashes(selection_gate, selection_artifacts, "selection-weight gate")
require_all_true_csv(
  file.path(qc_root, "selection_weights", "selection_weight_leakage_guard.csv"),
  "pass", "selection-weight leakage guard"
)
require_summary_sentinel(
  file.path(qc_root, "selection_weights", "selection_weights_QC.md"),
  "selection-weight summary"
)

# ---------------------------------------------------------------------------
# Decision-log resolution, D031 disclosure, D058, and model-code lock.
# ---------------------------------------------------------------------------

decision_lines <- readLines(docs$decision_log, warn = FALSE)
decision_row <- function(id) {
  row <- grep(paste0("^\\| ", id, " \\|"), decision_lines, value = TRUE)
  if (length(row) != 1L) stop("Decision log must contain exactly one row for ", id)
  row
}
decision_status <- function(id) {
  pieces <- strsplit(decision_row(id), "|", fixed = TRUE)[[1L]]
  if (length(pieces) < 5L) stop("Malformed decision-log row for ", id)
  trimws(pieces[[4L]])
}
required_decisions <- c(
  D031 = "LOCKED", D034 = "LOCKED", D037 = "LOCKED",
  D038 = "LOCKED-WITH-QC", D039 = "LOCKED",
  D048 = "LOCKED-WITH-QC", D050 = "LOCKED-CORRECTION",
  D053 = "LOCKED-CORRECTION", D054 = "LOCKED",
  D055 = "SECONDARY-LOCKED", D056 = "LOCKED-CLARIFICATION",
  D057 = "SECONDARY-LOCKED", D058 = "SECONDARY-LOCKED",
  D059 = "LOCKED-CLARIFICATION", D060 = "SECONDARY-LOCKED",
  D061 = "SECONDARY-LOCKED", D062 = "SECONDARY-LOCKED"
)
actual_decision_status <- vapply(names(required_decisions), decision_status, character(1L))
if (!identical(unname(actual_decision_status), unname(required_decisions))) {
  bad <- names(required_decisions)[actual_decision_status != required_decisions]
  stop("Required locked decision status mismatch: ", paste(bad, collapse = ", "))
}
for (id in sprintf("U%03d", 1:7)) {
  if (!grepl("^RESOLVED-", decision_status(id))) {
    stop("Blocking item is not resolved before unblinding: ", id)
  }
}
d031 <- decision_row("D031")
if (!grepl("governance deviation", d031, ignore.case = TRUE) ||
    !grepl("712/2,136", d031, fixed = TRUE) ||
    !grepl("prohibit use", d031, ignore.case = TRUE)) {
  stop("D031 governance deviation is not fully disclosed in the locked log.")
}
d054 <- decision_row("D054")
for (hash in unlist(parameter_artifacts, use.names = FALSE)) {
  artifact_hash <- sha256_file(hash)
  if (!grepl(artifact_hash, d054, fixed = TRUE)) {
    stop("D054 does not record a frozen parameter/frame hash: ", artifact_hash)
  }
}

model_text <- paste(readLines(scripts$models, warn = FALSE), collapse = "\n")
if (!grepl("08a_locked_analysis_utils\\.R", model_text) ||
    !grepl("D057", model_text, fixed = TRUE) ||
    !grepl('"R2"', model_text, fixed = TRUE) ||
    !grepl('"R3"', model_text, fixed = TRUE)) {
  stop("Model script does not statically expose the locked 08a/D057 R2-R3 design.")
}

# ---------------------------------------------------------------------------
# Static parse, synthetic utility self-test, environment lock, and manifest.
# No outcome-bearing script is sourced.
# ---------------------------------------------------------------------------

local_r_library <- file.path(project_root, "analysis_rebuild_v1", "r_library")
if (!dir.exists(local_r_library)) stop("Locked local R library is missing: ", local_r_library)
locked_local_r_library <- normalizePath(local_r_library, mustWork = TRUE)
.libPaths(c(locked_local_r_library, .libPaths()))
expected_packages <- c(mice = "3.19.0", sandwich = "3.1.2", geepack = "1.3.13")
observed_packages <- vapply(names(expected_packages), function(package) {
  package_path <- find.package(
    package, lib.loc = locked_local_r_library, quiet = TRUE
  )
  if (!length(package_path)) {
    stop("Required package is missing from the locked local R library: ", package)
  }
  as.character(utils::packageVersion(package, lib.loc = locked_local_r_library))
}, character(1L))
if (!identical(unname(observed_packages), unname(expected_packages))) {
  stop("Locked package-version mismatch: ", paste(
    names(observed_packages), observed_packages, sep = "=", collapse = ", "
  ))
}
expected_system_packages <- c(lme4 = "2.0.1", metafor = "5.0.1")
observed_system_packages <- vapply(names(expected_system_packages), function(package) {
  package_path <- find.package(package, quiet = TRUE)
  if (!length(package_path)) stop("Required system R package is missing: ", package)
  as.character(utils::packageVersion(package))
}, character(1L))
if (!identical(
  unname(observed_system_packages), unname(expected_system_packages)
)) {
  stop("System package-version mismatch: ", paste(
    names(observed_system_packages), observed_system_packages,
    sep = "=", collapse = ", "
  ))
}

# Construct the complete manifest in memory first, then syntax-check every one
# of its scripts. Python uses compile() directly and therefore creates no
# __pycache__ or other project artifact.
analysis_scripts <- sort(list.files(
  script_dir, pattern = "\\.(R|r|py)$", full.names = TRUE, recursive = TRUE
))
analysis_scripts <- vapply(
  analysis_scripts, project_file, character(1L), label = "analysis script"
)
required_manifest_scripts <- normalizePath(c(
  scripts$authorization, scripts$model_utils, scripts$locked_analysis_utils,
  scripts$model_utils_selftest, scripts$locked_analysis_utils_selftest,
  scripts$outcomes, scripts$models, scripts$locked_sensitivities,
  scripts$missing_data_sensitivities, scripts$center_heterogeneity,
  scripts$native_benchmark
), mustWork = TRUE)
if (!length(analysis_scripts) || anyDuplicated(analysis_scripts) ||
    length(setdiff(required_manifest_scripts, analysis_scripts))) {
  stop("Complete rebuild_v1 script manifest invariant failed.")
}
analysis_manifest <- data.table(
  file = vapply(analysis_scripts, project_relative, character(1L)),
  sha256 = vapply(analysis_scripts, sha256_file, character(1L)),
  bytes = as.numeric(file.info(analysis_scripts)$size)
)

r_manifest_scripts <- analysis_scripts[grepl("\\.(R|r)$", analysis_scripts)]
for (path in r_manifest_scripts) {
  tryCatch(
    parse(file = path, keep.source = FALSE),
    error = function(e) stop(
      "Static parse failed for manifested R script ", path, ": ",
      conditionMessage(e)
    )
  )
}
python_manifest_scripts <- analysis_scripts[grepl("\\.py$", analysis_scripts)]
python_compile_code <- paste(
  "import pathlib,sys", "p=pathlib.Path(sys.argv[1])",
  "compile(p.read_text(encoding='utf-8'),str(p),'exec')", sep = ";"
)
for (path in python_manifest_scripts) {
  output <- system2(
    "python3",
    c("-c", shQuote(python_compile_code), shQuote(path)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("Static Python compile failed for ", path, ": ", paste(output, collapse = " | "))
  }
}

# Evaluate only the outcome-source-agnostic utility modules in an isolated
# environment and verify the prefrozen model contract explicitly.
contract_env <- new.env(parent = baseenv())
sys.source(scripts$model_utils, envir = contract_env)
sys.source(scripts$locked_analysis_utils, envir = contract_env)
model_spec <- contract_env$locked_model_specification()
comparison_spec <- contract_env$locked_comparison_specification()
design_by_model <- setNames(model_spec$design_type, model_spec$model_id)
role_by_model <- setNames(model_spec$role, model_spec$model_id)
s3nl_comparison <- comparison_spec[
  comparison_spec$comparison_id == "S3NL_minus_S3"
]
utils_text <- paste(
  readLines(scripts$locked_analysis_utils, warn = FALSE), collapse = "\n"
)
if (!identical(contract_env$MIMIC_BOOTSTRAP_REPS, 1000L) ||
    !identical(contract_env$EICU_CLUSTER_BOOTSTRAP_REPS, 2000L) ||
    !identical(contract_env$BOOTSTRAP_SUCCESS_THRESHOLD, 0.95) ||
    sum(model_spec$model_id == "S3") != 1L ||
    sum(model_spec$model_id == "S3NL") != 1L ||
    !identical(unname(design_by_model[["S3"]]), "s0_smp_per_5") ||
    !identical(unname(design_by_model[["S3NL"]]), "s0_smp_rcs4") ||
    !identical(
      unname(role_by_model[["S3NL"]]), "secondary_four_knot_smp"
    ) ||
    nrow(s3nl_comparison) != 1L ||
    !identical(s3nl_comparison$new_model[[1L]], "S3NL") ||
    !identical(s3nl_comparison$reference_model[[1L]], "S3") ||
    !identical(s3nl_comparison$likelihood_ratio_allowed[[1L]], FALSE) ||
    !grepl('named_column(frame$smp / 5, "smp_per_5")', utils_text, fixed = TRUE)) {
  stop(
    "Locked model contract failed: require linear smp_per_5 S3, independent ",
    "S3NL, 1000/2000 bootstraps, and >=0.95 success."
  )
}
model_analysis_utils_contract_version <- "rebuild_v1_D054_D057_D058_D059_D060_D061_D062"

run_synthetic_selftest <- function(path, sentinel, label) {
  output <- system2(
    file.path(R.home("bin"), "Rscript"), shQuote(path),
    stdout = TRUE, stderr = TRUE,
    env = paste0(
      "R_LIBS_USER=", shQuote(normalizePath(local_r_library, mustWork = TRUE))
    )
  )
  status <- attr(output, "status")
  if ((!is.null(status) && status != 0L) || !any(output == sentinel)) {
    stop(label, " failed: ", paste(output, collapse = " | "))
  }
  invisible(sentinel)
}
run_synthetic_selftest(
  scripts$model_utils_selftest, "MODEL_UTILS_SYNTHETIC_PASS",
  "Outcome-free model-utils self-test"
)
run_synthetic_selftest(
  scripts$locked_analysis_utils_selftest,
  "LOCKED_ANALYSIS_UTILS_SYNTHETIC_PASS",
  "Outcome-free locked-analysis-utils self-test"
)

atomic_fwrite_immutable <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  fwrite(object, tmp)
  new_hash <- sha256_file(tmp)
  if (file.exists(path)) {
    if (!identical(sha256_file(path), new_hash)) {
      stop("Existing immutable preauthorization artifact differs: ", path)
    }
    return(new_hash)
  }
  # Same-directory hard-link creation is atomic and never replaces an existing
  # target. If an identical writer won the race, accept that immutable result.
  if (!file.link(tmp, path)) {
    if (file.exists(path) && identical(sha256_file(path), new_hash)) {
      return(new_hash)
    }
    stop("Could not atomically publish immutable preauthorization artifact: ", path)
  }
  sha256_file(path)
}

atomic_fwrite_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to overwrite existing checkpoint: ", path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  fwrite(object, tmp)
  if (file.exists(path)) stop("Checkpoint appeared concurrently; refusing overwrite.")
  # link(2) semantics provide atomic create-if-absent publication; unlike
  # rename, this cannot replace a concurrently published authorization.
  if (!file.link(tmp, path)) {
    stop("Could not atomically publish new checkpoint (target may exist): ", path)
  }
  invisible(path)
}

# No output is written before every scientific, provenance, parse, environment,
# and synthetic test above has passed.
analysis_manifest_sha256 <- atomic_fwrite_immutable(
  analysis_manifest, analysis_manifest_path
)

preauthorization_status <- "PRECHECK_PASS_NOT_AUTHORIZATION"
preauthorization_qc <- data.table(
  check = c(
    "authorization_semantics",
    "clean_outcome_product_boundary",
    "upstream_outcome_free_provenance",
    "locked_decisions_and_blockers",
    "complete_rebuild_v1_script_manifest",
    "manifested_R_static_parse",
    "manifested_Python_builtin_compile",
    "locked_model_contract",
    "model_utils_synthetic_selftest",
    "locked_analysis_utils_synthetic_selftest",
    "locked_R_environment",
    "D059_integrity_boundary"
  ),
  status = rep(preauthorization_status, 12L),
  detail = c(
    paste(
      "Prechecks passed only; outcome access requires the separate atomic",
      "AUTHORIZED checkpoint."
    ),
    paste(
      "Outcome receipt, Phase-3a gate, private outcome directory/products,",
      "and all Phase-3 model/sensitivity output directories were absent."
    ),
    paste(
      "All required outcome-free gates, QC, manifests, freshness checks,",
      "leakage guards, and predictor-side artifact hashes passed."
    ),
    paste0(
      paste(names(required_decisions), required_decisions, sep = "=", collapse = ";"),
      ";U001-U007=RESOLVED"
    ),
    paste0(
      nrow(analysis_manifest),
      " rebuild_v1 R/Python scripts inventoried with project-local SHA256."
    ),
    paste0(length(r_manifest_scripts), " manifested R scripts parsed successfully."),
    paste0(
      length(python_manifest_scripts),
      " manifested Python scripts passed built-in compile() without pycache."
    ),
    paste(
      "S3=linear smp_per_5; S3NL=independent four-knot spline secondary model;",
      "MIMIC bootstrap=1000; eICU cluster bootstrap=2000; success threshold=0.95."
    ),
    "MODEL_UTILS_SYNTHETIC_PASS",
    "LOCKED_ANALYSIS_UTILS_SYNTHETIC_PASS",
    paste0(
      "R=", R.version.string, ";library=", locked_local_r_library,
      ";mice=", observed_packages[["mice"]],
      ";sandwich=", observed_packages[["sandwich"]],
      ";geepack=", observed_packages[["geepack"]],
      ";lme4=", observed_system_packages[["lme4"]],
      ";metafor=", observed_system_packages[["metafor"]]
    ),
    paste(
      "No row-level outcome was read; only project-local integrity metadata",
      "and outcome-free artifacts were hashed."
    )
  )
)
if (anyDuplicated(preauthorization_qc$check) ||
    any(preauthorization_qc$status != preauthorization_status)) {
  stop("Preauthorization QC schema/status invariant failed.")
}
preauthorization_qc_sha256 <- atomic_fwrite_immutable(
  preauthorization_qc, preauthorization_qc_path
)

checkpoint <- data.table(
  field = c(
    "status", "config_version", "authorized_at", "authorized_by",
    "authorization_basis", "authorization_script_sha256",
    "config_script_sha256", "preflight_script_sha256",
    "phase0_lock_script_sha256", "core_integrity_script_sha256",
    "mimic_severity_script_sha256", "eicu_severity_script_sha256",
    "parameter_freeze_script_sha256", "selection_weights_script_sha256",
    "model_utils_script_sha256", "model_utils_selftest_sha256",
    "model_analysis_utils_script_sha256",
    "model_analysis_utils_selftest_sha256",
    "model_analysis_utils_contract_version",
    "outcome_extraction_script_sha256", "model_analysis_script_sha256",
    "locked_sensitivities_script_sha256",
    "missing_data_sensitivities_script_sha256",
    "center_heterogeneity_script_sha256",
    "native_benchmark_script_sha256",
    "sap_sha256", "data_dictionary_sha256",
    "analysis_decision_log_sha256", "terminology_ledger_sha256",
    "phase0_lock_manifest_sha256", "phase0_consistency_checks_sha256",
    "preflight_inventory_sha256", "preflight_run_manifest_sha256",
    "core_file_integrity_sha256",
    "mimic_phase1_gate_sha256", "eicu_phase1_gate_sha256",
    "mimic_phase2_gate_sha256", "eicu_phase2_gate_sha256",
    "mimic_warning_sensitivity_gate_sha256",
    "mimic_native_oasis_gate_sha256",
    "parameter_freeze_gate_sha256", "selection_weights_gate_sha256",
    "mimic_severity_gate_sha256", "eicu_severity_gate_sha256",
    "analysis_script_manifest_path", "analysis_script_manifest_sha256",
    "preauthorization_qc_path", "preauthorization_qc_sha256",
    "preauthorization_outcome_products_absent",
    "required_decisions_confirmed", "blocking_items_U001_U007_resolved",
    "D031_governance_deviation_acknowledged",
    "D059_integrity_boundary_acknowledged",
    "external_outcome_used_for_form_or_variable_selection",
    "model_utils_selftest_status", "model_analysis_utils_selftest_status",
    "locked_model_contract_status", "R_version", "R_library_path",
    "mice_version", "sandwich_version", "geepack_version",
    "lme4_version", "metafor_version"
  ),
  value = c(
    "AUTHORIZED", LOCKED$version,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    "doctorliu (user authorization recorded by Codex)",
    paste(
      "User explicitly directed: “根据计划，开始进行吧”；",
      "“继续接着原计划吧，刚刚中止了”"
    ),
    sha256_file(scripts$authorization), sha256_file(scripts$config),
    sha256_file(scripts$preflight), sha256_file(scripts$phase0),
    sha256_file(scripts$core_integrity), sha256_file(scripts$mimic_severity),
    sha256_file(scripts$eicu_severity), sha256_file(scripts$parameter),
    sha256_file(scripts$selection), sha256_file(scripts$model_utils),
    sha256_file(scripts$model_utils_selftest),
    sha256_file(scripts$locked_analysis_utils),
    sha256_file(scripts$locked_analysis_utils_selftest),
    model_analysis_utils_contract_version, sha256_file(scripts$outcomes),
    sha256_file(scripts$models),
    sha256_file(scripts$locked_sensitivities),
    sha256_file(scripts$missing_data_sensitivities),
    sha256_file(scripts$center_heterogeneity),
    sha256_file(scripts$native_benchmark),
    sha256_file(docs$sap),
    sha256_file(docs$dictionary), sha256_file(docs$decision_log),
    sha256_file(docs$terminology), sha256_file(paths$phase0_manifest),
    sha256_file(paths$phase0_checks), sha256_file(paths$preflight_inventory),
    sha256_file(paths$preflight_manifest), sha256_file(paths$core_integrity),
    sha256_file(paths$mimic_phase1_gate), sha256_file(paths$eicu_phase1_gate),
    sha256_file(paths$mimic_phase2_gate), sha256_file(paths$eicu_phase2_gate),
    sha256_file(paths$warning_gate), sha256_file(paths$oasis_gate),
    sha256_file(paths$parameter_gate), sha256_file(paths$selection_gate),
    sha256_file(paths$mimic_severity_gate), sha256_file(paths$eicu_severity_gate),
    analysis_manifest_relative, analysis_manifest_sha256,
    preauthorization_qc_relative, preauthorization_qc_sha256, "TRUE",
    paste(names(required_decisions), collapse = ","), "TRUE", "TRUE", "TRUE",
    "FALSE", "MODEL_UTILS_SYNTHETIC_PASS",
    "LOCKED_ANALYSIS_UTILS_SYNTHETIC_PASS", "PASS", R.version.string,
    locked_local_r_library,
    observed_packages[["mice"]], observed_packages[["sandwich"]],
    observed_packages[["geepack"]], observed_system_packages[["lme4"]],
    observed_system_packages[["metafor"]]
  )
)
if (anyDuplicated(checkpoint$field) || nrow(checkpoint) != length(checkpoint$value)) {
  stop("Authorization checkpoint schema invariant failed.")
}
atomic_fwrite_new(checkpoint, checkpoint_path)

message(
  "Authorization checkpoint published; no row-level outcome was read. ",
  "Only project-local integrity metadata and outcome-free artifacts were hashed."
)
message("  checkpoint: ", checkpoint_path)
message("  analysis manifest SHA256: ", analysis_manifest_sha256)
message("  preauthorization QC SHA256: ", preauthorization_qc_sha256)
