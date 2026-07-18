#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: timing-compatible native severity
# benchmarks under the D062 locked secondary specification.
#
# GOVERNANCE WARNING
# ------------------
# This script is outcome-bearing and must never be sourced. It may be executed
# only after the formal authorization checkpoint, the Phase 3a outcome PASS
# gate, the Phase 3b locked-model PASS gate, and both outcome-free native-score
# gates have passed their complete SHA256 chains. Before formal execution, the
# only permitted check is parse(file=...). This file must not be run during its
# static freeze/audit turn.

suppressPackageStartupMessages(library(data.table))
options(warn = 1)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/14_fit_native_benchmarks.R", mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
project_from_script <- normalizePath(
  file.path(script_dir, "..", "..", ".."), mustWork = TRUE
)
expected_config_version <- "1.0.1"

DECISION_ID <- "D062"
OASIS_NATIVE_INTERCEPT <- -6.1746
OASIS_NATIVE_SLOPE <- 0.1275
MIMIC_NATIVE_WINDOW_SECONDS <- 24 * 60 * 60
EICU_NATIVE_WINDOW_MINUTES <- 1440
EXPECTED_MIMIC_OUTCOME_FREE_NATIVE_N <- 1665L
EXPECTED_MIMIC_OUTCOME_FREE_ALL10_N <- 1538L
EXPECTED_EICU_OUTCOME_FREE_NATIVE_N <- 211L
NATIVE_MODEL_IDS <- c("N0", "N1", "N2", "N3")
NATIVE_MODEL_ORDER <- setNames(seq_along(NATIVE_MODEL_IDS), NATIVE_MODEL_IDS)
ONLY_LRT_COMPARISON <- "N3_minus_N2"

sha256_file <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path)) {
    stop("Missing SHA256 target: ", path)
  }
  resolved <- normalizePath(path, mustWork = TRUE)
  project_prefix <- paste0(project_from_script, .Platform$file.sep)
  if (!startsWith(resolved, project_prefix)) {
    stop("SHA256 target escapes the project root: ", resolved)
  }
  output <- system2(
    "shasum", c("-a", "256", shQuote(resolved)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA256 failed for ", resolved, ": ", paste(output, collapse = " "))
  }
  hash <- strsplit(output[[1L]], "[[:space:]]+")[[1L]][1L]
  if (!grepl("^[0-9a-f]{64}$", hash)) stop("Invalid SHA256 for ", resolved)
  hash
}

read_field_value_gate <- function(path, label) {
  z <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(z), c("field", "value")) || !nrow(z) ||
      anyDuplicated(z$field) || anyNA(z$field) || any(!nzchar(z$field))) {
    stop("Malformed field/value ", label, ": ", path)
  }
  setNames(as.character(z$value), z$field)
}

read_completion_gate <- function(path, label) {
  z <- fread(path, colClasses = "character", showProgress = FALSE)
  if (identical(names(z), c("field", "value"))) {
    if (!nrow(z) || anyDuplicated(z$field) || anyNA(z$field) ||
        any(!nzchar(z$field))) {
      stop("Malformed field/value ", label, ": ", path)
    }
    return(setNames(as.character(z$value), z$field))
  }
  if (nrow(z) != 1L || anyDuplicated(names(z)) || anyNA(names(z)) ||
      any(!nzchar(names(z)))) {
    stop("Malformed one-row ", label, ": ", path)
  }
  setNames(vapply(z, function(value) {
    as.character(value[[1L]])
  }, character(1L)), names(z))
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
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop(label, " is empty.")
  }
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

project_relative <- function(path) {
  resolved <- normalizePath(path, mustWork = TRUE)
  prefix <- paste0(project_from_script, .Platform$file.sep)
  if (!startsWith(resolved, prefix)) stop("Output escapes the project root.")
  substring(resolved, nchar(project_from_script) + 2L)
}

atomic_publish_tmp <- function(tmp, path, label) {
  if (file.exists(path)) stop("Refusing to replace existing ", label, ": ", path)
  if (!isTRUE(file.link(tmp, path))) {
    stop("Could not atomically publish new ", label, " (target may exist): ", path)
  }
  invisible(path)
}

atomic_fwrite_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  fwrite(object, tmp)
  atomic_publish_tmp(tmp, path, "CSV")
}

atomic_save_rds_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  saveRDS(object, tmp, version = 3, compress = "xz")
  atomic_publish_tmp(tmp, path, "private RDS")
}

atomic_write_lines_new <- function(object, path) {
  if (file.exists(path)) stop("Refusing to replace existing file: ", path)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(object, tmp, useBytes = TRUE)
  atomic_publish_tmp(tmp, path, "text file")
}

safe_reason <- function(x) {
  z <- unique(trimws(as.character(x)))
  z <- z[!is.na(z) & nzchar(z)]
  if (!length(z)) return("")
  paste(substr(z, 1L, 500L), collapse = " | ")
}

to_epoch <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(x))
  z <- trimws(as.character(x))
  output <- rep(NA_real_, length(z))
  keep <- !is.na(z) & nzchar(z)
  if (any(keep)) {
    parsed <- as.POSIXct(
      z[keep], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    )
    output[keep] <- as.numeric(parsed)
  }
  output
}

same_numeric <- function(x, y, tolerance = 1e-10) {
  if (length(x) != length(y)) return(FALSE)
  same_missing <- identical(is.na(x), is.na(y))
  if (!same_missing) return(FALSE)
  keep <- !is.na(x)
  !any(keep) || all(abs(as.numeric(x[keep]) - as.numeric(y[keep])) <= tolerance)
}

# ---------------------------------------------------------------------------
# The authorization checkpoint is intentionally the first project artifact
# opened. No config, gate, manifest, predictor, model, or outcome object is
# read before this checkpoint exists and declares formal authorization.
# ---------------------------------------------------------------------------

checkpoint_path <- file.path(
  project_from_script, "analysis_rebuild_v1", "qc", "unblinding",
  "outcome_unblinding_checkpoint_v1.csv"
)
if (!file.exists(checkpoint_path)) {
  stop(
    "Native-benchmark modeling is not authorized: missing checkpoint ",
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
require_map_value(
  checkpoint, "preauthorization_outcome_products_absent", "TRUE",
  "authorization checkpoint"
)
require_map_value(
  checkpoint, "external_outcome_used_for_form_or_variable_selection", "FALSE",
  "authorization checkpoint"
)
require_map_value(
  checkpoint, "model_utils_selftest_status", "MODEL_UTILS_SYNTHETIC_PASS",
  "authorization checkpoint"
)
require_map_value(
  checkpoint, "model_analysis_utils_selftest_status",
  "LOCKED_ANALYSIS_UTILS_SYNTHETIC_PASS", "authorization checkpoint"
)
require_map_value(
  checkpoint, "locked_model_contract_status", "PASS",
  "authorization checkpoint"
)
for (field in c(
  "blocking_items_U001_U007_resolved",
  "D031_governance_deviation_acknowledged",
  "D059_integrity_boundary_acknowledged"
)) require_map_value(checkpoint, field, "TRUE", "authorization checkpoint")
confirmed_decisions <- strsplit(
  require_map_value(
    checkpoint, "required_decisions_confirmed", label = "authorization checkpoint"
  ), ",", fixed = TRUE
)[[1L]]
if (!DECISION_ID %in% confirmed_decisions) {
  stop("Authorization checkpoint does not confirm D062.")
}

script_paths <- list(
  authorization = file.path(script_dir, "08_authorize_outcome_unblinding.R"),
  config = file.path(script_dir, "00_config.R"),
  preflight = file.path(script_dir, "00_preflight.R"),
  phase0 = file.path(script_dir, "00_phase0_lock.R"),
  core_integrity = file.path(script_dir, "00_core_integrity.R"),
  mimic_severity = file.path(script_dir, "05_build_mimic_severity_core.R"),
  mimic_oasis = file.path(script_dir, "05c_build_mimic_native_oasis.R"),
  mimic_oasis_helper = file.path(script_dir, "05d_filter_mimic_oasis_inputs.py"),
  eicu_severity = file.path(script_dir, "06_build_eicu_severity_core.R"),
  parameter = file.path(script_dir, "07_freeze_predictor_parameters.R"),
  selection = file.path(script_dir, "07b_build_selection_weights.R"),
  model_utils = file.path(script_dir, "08_model_utils.R"),
  model_utils_selftest = file.path(script_dir, "08_model_utils_selftest.R"),
  analysis_utils = file.path(script_dir, "08a_locked_analysis_utils.R"),
  analysis_utils_selftest = file.path(
    script_dir, "08a_locked_analysis_utils_selftest.R"
  ),
  outcomes = file.path(script_dir, "09_extract_rebuilt_outcomes.R"),
  main_models = file.path(script_dir, "10_fit_locked_models.R"),
  native_benchmark = script_path
)
if (any(!file.exists(unlist(script_paths, use.names = FALSE)))) {
  stop("An authorized native-benchmark script dependency is missing.")
}

docs <- list(
  sap = file.path(project_from_script, "docs", "rebuild_v1", "SAP_v1_0.md"),
  dictionary = file.path(
    project_from_script, "docs", "rebuild_v1", "data_dictionary_v1.md"
  ),
  decision_log = file.path(
    project_from_script, "docs", "rebuild_v1", "analysis_decision_log.md"
  ),
  terminology = file.path(
    project_from_script, "docs", "rebuild_v1", "terminology_ledger.md"
  )
)
if (any(!file.exists(unlist(docs, use.names = FALSE)))) {
  stop("An authorized design-lock document is missing.")
}

checkpoint_file_locks <- c(
  authorization_script_sha256 = script_paths$authorization,
  config_script_sha256 = script_paths$config,
  preflight_script_sha256 = script_paths$preflight,
  phase0_lock_script_sha256 = script_paths$phase0,
  core_integrity_script_sha256 = script_paths$core_integrity,
  mimic_severity_script_sha256 = script_paths$mimic_severity,
  eicu_severity_script_sha256 = script_paths$eicu_severity,
  parameter_freeze_script_sha256 = script_paths$parameter,
  selection_weights_script_sha256 = script_paths$selection,
  model_utils_script_sha256 = script_paths$model_utils,
  model_utils_selftest_sha256 = script_paths$model_utils_selftest,
  model_analysis_utils_script_sha256 = script_paths$analysis_utils,
  model_analysis_utils_selftest_sha256 = script_paths$analysis_utils_selftest,
  outcome_extraction_script_sha256 = script_paths$outcomes,
  model_analysis_script_sha256 = script_paths$main_models,
  native_benchmark_script_sha256 = script_paths$native_benchmark,
  sap_sha256 = docs$sap,
  data_dictionary_sha256 = docs$dictionary,
  analysis_decision_log_sha256 = docs$decision_log,
  terminology_ledger_sha256 = docs$terminology
)
for (field in names(checkpoint_file_locks)) {
  require_map_value(
    checkpoint, field, sha256_file(checkpoint_file_locks[[field]]),
    "authorization checkpoint"
  )
}

preauthorization_qc_relative <- require_map_value(
  checkpoint, "preauthorization_qc_path", label = "authorization checkpoint"
)
preauthorization_qc_path <- resolve_project_path(
  preauthorization_qc_relative, "preauthorization QC", require_relative = TRUE
)
require_map_value(
  checkpoint, "preauthorization_qc_sha256",
  sha256_file(preauthorization_qc_path), "authorization checkpoint"
)
preauthorization_qc <- fread(
  preauthorization_qc_path, colClasses = "character", showProgress = FALSE
)
if (!identical(names(preauthorization_qc), c("check", "status", "detail")) ||
    !nrow(preauthorization_qc) || anyDuplicated(preauthorization_qc$check) ||
    any(preauthorization_qc$status != "PRECHECK_PASS_NOT_AUTHORIZATION")) {
  stop("Preauthorization QC is malformed or incorrectly claims authorization.")
}

checkpoint_integrity_artifacts <- c(
  phase0_lock_manifest_sha256 = file.path(
    project_from_script, "analysis_rebuild_v1", "qc", "phase0_lock_manifest.csv"
  ),
  phase0_consistency_checks_sha256 = file.path(
    project_from_script, "analysis_rebuild_v1", "qc",
    "phase0_consistency_checks.csv"
  ),
  preflight_inventory_sha256 = file.path(
    project_from_script, "analysis_rebuild_v1", "qc",
    "preflight_file_inventory.csv"
  ),
  preflight_run_manifest_sha256 = file.path(
    project_from_script, "analysis_rebuild_v1", "qc",
    "preflight_run_manifest.csv"
  ),
  core_file_integrity_sha256 = file.path(
    project_from_script, "analysis_rebuild_v1", "qc", "core_file_integrity.csv"
  )
)
if (any(!file.exists(checkpoint_integrity_artifacts))) {
  stop("A checkpoint-locked integrity artifact is missing.")
}
for (field in names(checkpoint_integrity_artifacts)) {
  require_map_value(
    checkpoint, field, sha256_file(checkpoint_integrity_artifacts[[field]]),
    "authorization checkpoint"
  )
}

# Verify the complete authorization-time R/Python manifest and require that it
# equals the current complete rebuild_v1 script set, including this file.
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
if (!all(c("file", "sha256", "bytes") %in% names(analysis_manifest)) ||
    !nrow(analysis_manifest) || anyDuplicated(analysis_manifest$file) ||
    anyNA(analysis_manifest$file) || anyNA(analysis_manifest$sha256)) {
  stop("Malformed analysis script manifest.")
}
analysis_paths <- unname(vapply(
  as.character(analysis_manifest$file), resolve_project_path,
  character(1L), label = "manifested analysis script", require_relative = TRUE
))
analysis_hashes <- tolower(as.character(analysis_manifest$sha256))
script_prefix <- paste0(normalizePath(script_dir), .Platform$file.sep)
if (anyDuplicated(analysis_paths) ||
    any(!grepl("^[0-9a-f]{64}$", analysis_hashes)) ||
    any(!startsWith(analysis_paths, script_prefix)) ||
    any(!grepl("\\.(R|r|py)$", analysis_paths)) ||
    any(as.numeric(analysis_manifest$bytes) != file.info(analysis_paths)$size)) {
  stop("Analysis manifest path/hash/size invariant failed.")
}
current_script_set <- sort(normalizePath(list.files(
  script_dir, pattern = "\\.(R|r|py)$", full.names = TRUE, recursive = TRUE
), mustWork = TRUE))
if (!setequal(analysis_paths, current_script_set)) {
  stop("Authorized manifest is not the complete current rebuild_v1 script set.")
}
current_hashes <- unname(vapply(analysis_paths, sha256_file, character(1L)))
if (!identical(analysis_hashes, current_hashes)) {
  stop("A current analysis script differs from the authorized manifest.")
}
must_manifest <- normalizePath(c(
  script_paths$mimic_oasis, script_paths$eicu_severity,
  script_paths$parameter, script_paths$model_utils, script_paths$analysis_utils,
  script_paths$outcomes, script_paths$main_models, script_path
), mustWork = TRUE)
if (length(setdiff(must_manifest, analysis_paths))) {
  stop("Manifest lacks a required D062 dependency or this exact script.")
}
self_index <- match(normalizePath(script_path), analysis_paths)
if (is.na(self_index) ||
    !identical(analysis_hashes[[self_index]], sha256_file(script_path))) {
  stop("Script 14 is not self-hash-locked by the authorization manifest.")
}

# Only the outcome-free configuration is sourced. Neither 09 nor 10 is ever
# sourced by this script.
source(script_paths$config)
if (!identical(LOCKED$version, expected_config_version) ||
    !identical(normalizePath(PROJECT_ROOT, mustWork = TRUE), project_from_script)) {
  stop("Loaded config differs from the authorized project/configuration.")
}

# ---------------------------------------------------------------------------
# Complete upstream gate chain. Every gate and every private input hash is
# verified before any row-level RDS is opened.
# ---------------------------------------------------------------------------

gate_paths <- list(
  mimic_phase1 = file.path(QC_ROOT, "mimic", "phase1_complete_v1.csv"),
  eicu_phase1 = file.path(QC_ROOT, "eicu", "phase1_eicu_complete_v1.csv"),
  mimic_phase2 = file.path(
    QC_ROOT, "mimic_exposure", "phase2_mimic_exposure_complete_v1.csv"
  ),
  eicu_phase2 = file.path(
    QC_ROOT, "eicu_exposure", "phase2_eicu_exposure_complete_v1.csv"
  ),
  mimic_oasis = file.path(
    QC_ROOT, "mimic_native_oasis",
    "phase2c_mimic_native_oasis_complete_v1.csv"
  ),
  mimic_severity = file.path(
    QC_ROOT, "mimic_severity", "phase2b_mimic_severity_complete_v1.csv"
  ),
  eicu_severity = file.path(
    QC_ROOT, "eicu_severity", "phase2b_complete_v1.csv"
  ),
  parameter = file.path(
    QC_ROOT, "parameter_freeze", "phase2e_parameter_freeze_complete_v1.csv"
  ),
  selection = file.path(
    QC_ROOT, "selection_weights", "phase2d_selection_weights_complete_v1.csv"
  ),
  outcomes = file.path(
    QC_ROOT, "outcomes", "phase3a_rebuilt_outcomes_complete_v1.csv"
  ),
  main_models = file.path(
    QC_ROOT, "locked_models", "phase3b_locked_models_complete_v1.csv"
  ),
  access_receipt = file.path(
    dirname(checkpoint_path), "outcome_access_receipt_v1.csv"
  )
)
if (any(!file.exists(unlist(gate_paths, use.names = FALSE)))) {
  stop("A required D062 upstream PASS gate/access receipt is missing.")
}

mimic_oasis_gate <- read_completion_gate(
  gate_paths$mimic_oasis, "native-OASIS gate"
)
mimic_severity_gate <- read_completion_gate(
  gate_paths$mimic_severity, "MIMIC severity gate"
)
eicu_severity_gate <- read_completion_gate(
  gate_paths$eicu_severity, "eICU severity gate"
)
parameter_gate <- read_completion_gate(gate_paths$parameter, "parameter gate")
selection_gate <- read_completion_gate(gate_paths$selection, "selection gate")
outcome_gate <- read_completion_gate(gate_paths$outcomes, "outcome gate")
main_model_gate <- read_completion_gate(
  gate_paths$main_models, "main-model gate"
)
access_receipt <- read_completion_gate(
  gate_paths$access_receipt, "outcome-access receipt"
)

checkpoint_gate_locks <- c(
  mimic_phase1_gate_sha256 = gate_paths$mimic_phase1,
  eicu_phase1_gate_sha256 = gate_paths$eicu_phase1,
  mimic_phase2_gate_sha256 = gate_paths$mimic_phase2,
  eicu_phase2_gate_sha256 = gate_paths$eicu_phase2,
  mimic_native_oasis_gate_sha256 = gate_paths$mimic_oasis,
  mimic_severity_gate_sha256 = gate_paths$mimic_severity,
  eicu_severity_gate_sha256 = gate_paths$eicu_severity,
  parameter_freeze_gate_sha256 = gate_paths$parameter,
  selection_weights_gate_sha256 = gate_paths$selection
)
for (field in names(checkpoint_gate_locks)) {
  require_map_value(
    checkpoint, field, sha256_file(checkpoint_gate_locks[[field]]),
    "authorization checkpoint"
  )
}

require_map_value(mimic_oasis_gate, "status", "PASS", "native-OASIS gate")
require_map_value(
  mimic_oasis_gate, "locked_config_version", LOCKED$version,
  "native-OASIS gate"
)
require_map_value(
  mimic_oasis_gate, "official_benchmark", "OASIS", "native-OASIS gate"
)
for (field in c(
  "all_invariants_pass", "synthetic_rule_tests_pass",
  "outcome_leakage_guard_pass", "all_event_sources_reached_eof",
  "all_raw_sha256_match_official", "all_required_qc_present"
)) require_map_value(mimic_oasis_gate, field, "TRUE", "native-OASIS gate")
for (field in c(
  "actual_outcome_fields_read", "predicted_probability_executed",
  "hsc_substitute_allowed"
)) require_map_value(mimic_oasis_gate, field, "FALSE", "native-OASIS gate")
require_map_value(
  mimic_oasis_gate, "script_sha256", sha256_file(script_paths$mimic_oasis),
  "native-OASIS gate"
)
require_map_value(
  mimic_oasis_gate, "helper_sha256",
  sha256_file(script_paths$mimic_oasis_helper), "native-OASIS gate"
)
require_map_value(
  mimic_oasis_gate, "phase1_gate_sha256", sha256_file(gate_paths$mimic_phase1),
  "native-OASIS gate"
)

require_map_value(
  mimic_severity_gate, "status", "PASS", "MIMIC severity gate"
)
require_map_value(
  mimic_severity_gate, "config_version", LOCKED$version,
  "MIMIC severity gate"
)
for (field in c("all_invariants_pass", "outcome_leakage_guard_pass")) {
  require_map_value(mimic_severity_gate, field, "TRUE", "MIMIC severity gate")
}
require_map_value(
  mimic_severity_gate, "summary_sentinel", "BUILD_COMPLETE",
  "MIMIC severity gate"
)
require_map_value(
  mimic_severity_gate, "script_sha256",
  sha256_file(script_paths$mimic_severity), "MIMIC severity gate"
)
require_map_value(
  mimic_severity_gate, "phase1_gate_sha256",
  sha256_file(gate_paths$mimic_phase1), "MIMIC severity gate"
)
require_map_value(
  mimic_severity_gate, "phase2_gate_sha256",
  sha256_file(gate_paths$mimic_phase2), "MIMIC severity gate"
)
require_map_value(
  mimic_severity_gate, "native_oasis_gate_sha256",
  sha256_file(gate_paths$mimic_oasis), "MIMIC severity gate"
)

require_map_value(eicu_severity_gate, "status", "PASS", "eICU severity gate")
require_map_value(
  eicu_severity_gate, "config_version", LOCKED$version, "eICU severity gate"
)
require_map_value(
  eicu_severity_gate, "script_sha256",
  sha256_file(script_paths$eicu_severity), "eICU severity gate"
)
require_map_value(
  eicu_severity_gate, "phase1_gate_sha256",
  sha256_file(gate_paths$eicu_phase1), "eICU severity gate"
)
require_map_value(
  eicu_severity_gate, "phase2_gate_sha256",
  sha256_file(gate_paths$eicu_phase2), "eICU severity gate"
)

require_map_value(parameter_gate, "status", "PASS", "parameter gate")
require_map_value(
  parameter_gate, "locked_config_version", LOCKED$version, "parameter gate"
)
for (field in c(
  "all_tests_pass", "outcome_leakage_guard_pass", "all_required_qc_present"
)) require_map_value(parameter_gate, field, "TRUE", "parameter gate")
require_map_value(
  parameter_gate, "script_sha256", sha256_file(script_paths$parameter),
  "parameter gate"
)
require_map_value(
  parameter_gate, "model_utils_sha256", sha256_file(script_paths$model_utils),
  "parameter gate"
)
require_map_value(
  parameter_gate, "mimic_severity_gate_sha256",
  sha256_file(gate_paths$mimic_severity), "parameter gate"
)
require_map_value(
  parameter_gate, "eicu_severity_gate_sha256",
  sha256_file(gate_paths$eicu_severity), "parameter gate"
)

require_map_value(selection_gate, "status", "PASS", "selection gate")
require_map_value(
  selection_gate, "config_version", LOCKED$version, "selection gate"
)
require_map_value(selection_gate, "decision_id", "D055", "selection gate")
for (field in c("all_leakage_checks_pass", "all_required_qc_present")) {
  require_map_value(selection_gate, field, "TRUE", "selection gate")
}
require_map_value(
  selection_gate, "script_sha256", sha256_file(script_paths$selection),
  "selection gate"
)

require_map_value(outcome_gate, "status", "PASS", "outcome gate")
require_map_value(outcome_gate, "config_version", LOCKED$version, "outcome gate")
require_map_value(
  outcome_gate, "outcome_access_status", "FORMALLY_UNBLINDED", "outcome gate"
)
require_map_value(
  outcome_gate, "all_accounting_invariants_pass", "TRUE", "outcome gate"
)
require_map_value(
  outcome_gate, "script_sha256", sha256_file(script_paths$outcomes),
  "outcome gate"
)
require_map_value(
  outcome_gate, "checkpoint_sha256", sha256_file(checkpoint_path),
  "outcome gate"
)
require_map_value(
  outcome_gate, "access_receipt_sha256", sha256_file(gate_paths$access_receipt),
  "outcome gate"
)
require_map_value(
  outcome_gate, "analysis_script_manifest_sha256",
  sha256_file(analysis_manifest_path), "outcome gate"
)
require_map_value(
  outcome_gate, "parameter_freeze_gate_sha256",
  sha256_file(gate_paths$parameter), "outcome gate"
)
require_map_value(
  outcome_gate, "selection_weights_gate_sha256",
  sha256_file(gate_paths$selection), "outcome gate"
)
require_map_value(
  outcome_gate, "mimic_severity_gate_sha256",
  sha256_file(gate_paths$mimic_severity), "outcome gate"
)
require_map_value(
  outcome_gate, "eicu_severity_gate_sha256",
  sha256_file(gate_paths$eicu_severity), "outcome gate"
)

require_map_value(main_model_gate, "status", "PASS", "main-model gate")
require_map_value(
  main_model_gate, "config_version", LOCKED$version, "main-model gate"
)
for (field in c(
  "all_input_gate_checks_pass", "all_exact_join_checks_pass",
  "all_required_models_or_allowed_S5_pass", "all_required_outputs_present"
)) require_map_value(main_model_gate, field, "TRUE", "main-model gate")
for (field in c(
  "all_applicable_bootstrap_success_rates_pass",
  "external_evidence_tier_rule_pass",
  "raw_external_reported_before_recalibration",
  "likelihood_ratio_test_only_S2_vs_S2M"
)) require_map_value(main_model_gate, field, "TRUE", "main-model gate")
require_map_value(
  main_model_gate, "summary_sentinel", "BUILD_COMPLETE", "main-model gate"
)
require_map_value(
  main_model_gate, "script_sha256", sha256_file(script_paths$main_models),
  "main-model gate"
)
require_map_value(
  main_model_gate, "model_utils_sha256", sha256_file(script_paths$model_utils),
  "main-model gate"
)
require_map_value(
  main_model_gate, "analysis_utils_sha256",
  sha256_file(script_paths$analysis_utils), "main-model gate"
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
  sha256_file(gate_paths$parameter), "main-model gate"
)
require_map_value(
  main_model_gate, "outcome_gate_sha256", sha256_file(gate_paths$outcomes),
  "main-model gate"
)

require_map_value(
  access_receipt, "status", "OUTCOME_ACCESS_INITIATED", "outcome-access receipt"
)
require_map_value(
  access_receipt, "config_version", LOCKED$version, "outcome-access receipt"
)
require_map_value(
  access_receipt, "checkpoint_sha256", sha256_file(checkpoint_path),
  "outcome-access receipt"
)
require_map_value(
  access_receipt, "script_sha256", sha256_file(script_paths$outcomes),
  "outcome-access receipt"
)

decision_lines <- readLines(docs$decision_log, warn = FALSE)
d062_row <- grep("^\\| D062 \\|", decision_lines, value = TRUE)
if (length(d062_row) != 1L) stop("Decision log must contain exactly one D062 row.")
d062_parts <- strsplit(d062_row, "|", fixed = TRUE)[[1L]]
if (length(d062_parts) < 5L ||
    !identical(trimws(d062_parts[[4L]]), "SECONDARY-LOCKED")) {
  stop("D062 is absent, malformed, or not SECONDARY-LOCKED.")
}
required_d062_markers <- c(
  "-6.1746", "0.1275", "1,440", "1,000", "2,000", ">=95%",
  "local updating/extensions", "never external validation"
)
if (any(!vapply(required_d062_markers, function(marker) {
  grepl(marker, d062_row, fixed = TRUE)
}, logical(1L)))) {
  stop("D062 row lacks a required frozen native-benchmark contract marker.")
}

# The only sourced modeling code is the authorized outcome-source-agnostic
# 08/08a utility layer. Core performance and resampling functions come from it.
source(script_paths$model_utils)
source(script_paths$analysis_utils)
required_utils <- c(
  "assert_binary_outcome", "clip_probability", "fit_model", "predict_model",
  "performance_vector", "cluster_bootstrap_indices", "percentile_interval"
)
if (!all(vapply(required_utils, exists, logical(1L), mode = "function"))) {
  stop("Authorized 08/08a utility interface is incomplete.")
}
if (!identical(MIMIC_BOOTSTRAP_REPS, 1000L) ||
    !identical(EICU_CLUSTER_BOOTSTRAP_REPS, 2000L) ||
    !identical(MIMIC_BOOTSTRAP_SEED, 20260715L) ||
    !identical(EICU_BOOTSTRAP_SEED, 20260716L) ||
    !identical(BOOTSTRAP_SUCCESS_THRESHOLD, 0.95)) {
  stop("D062 bootstrap constants differ from the authorized 08a contract.")
}
require_map_value(
  main_model_gate, "mimic_bootstrap_repetitions", MIMIC_BOOTSTRAP_REPS,
  "main-model gate"
)
require_map_value(
  main_model_gate, "mimic_bootstrap_seed", MIMIC_BOOTSTRAP_SEED,
  "main-model gate"
)
require_map_value(
  main_model_gate, "eicu_cluster_bootstrap_repetitions",
  EICU_CLUSTER_BOOTSTRAP_REPS, "main-model gate"
)
require_map_value(
  main_model_gate, "eicu_bootstrap_base_seed", EICU_BOOTSTRAP_SEED,
  "main-model gate"
)
require_map_value(
  main_model_gate, "bootstrap_success_threshold", BOOTSTRAP_SUCCESS_THRESHOLD,
  "main-model gate"
)

parameter_artifact_fields <- list(
  mimic = c("mimic_model_frame_rds_path", "mimic_model_frame_rds_sha256"),
  eicu = c("eicu_model_frame_rds_path", "eicu_model_frame_rds_sha256")
)
model_frame_paths <- lapply(parameter_artifact_fields, function(pair) {
  path <- resolve_project_path(
    require_map_value(parameter_gate, pair[[1L]], label = "parameter gate"),
    pair[[1L]], require_relative = TRUE
  )
  require_map_value(
    parameter_gate, pair[[2L]], sha256_file(path), "parameter gate"
  )
  require_map_value(main_model_gate, pair[[2L]], sha256_file(path), "main-model gate")
  path
})

input_paths <- list(
  mimic_time = file.path(
    PRIVATE_ROOT, "mimic", "mimic_paired_exposure_with_severity_core_v1.rds"
  ),
  mimic_oasis = file.path(
    PRIVATE_ROOT, "mimic", "mimic_native_oasis_benchmark_v1.rds"
  ),
  mimic_outcome = file.path(
    PRIVATE_ROOT, "outcomes", "mimic_rebuilt_outcomes_v1.rds"
  ),
  eicu_time = file.path(
    PRIVATE_ROOT, "eicu", "eicu_paired_exposure_with_severity_core_v1.rds"
  ),
  eicu_apache = file.path(
    PRIVATE_ROOT, "eicu", "eicu_native_apache_iva_benchmark_v1.rds"
  ),
  eicu_outcome = file.path(
    PRIVATE_ROOT, "outcomes", "eicu_rebuilt_outcomes_v1.rds"
  )
)
if (any(!file.exists(unlist(input_paths, use.names = FALSE)))) {
  stop("A checksum-gated D062 row-level input is missing.")
}

require_map_value(
  mimic_severity_gate, "prediction_hsc_rds_sha256",
  sha256_file(input_paths$mimic_time), "MIMIC severity gate"
)
require_map_value(
  outcome_gate, "mimic_prediction_rds_sha256",
  sha256_file(input_paths$mimic_time), "outcome gate"
)
require_map_value(
  mimic_oasis_gate, "native_oasis_rds_sha256",
  sha256_file(input_paths$mimic_oasis), "native-OASIS gate"
)
require_map_value(
  outcome_gate, "mimic_outcome_rds_sha256",
  sha256_file(input_paths$mimic_outcome), "outcome gate"
)
require_map_value(
  main_model_gate, "mimic_outcome_rds_sha256",
  sha256_file(input_paths$mimic_outcome), "main-model gate"
)

require_map_value(
  eicu_severity_gate, "prediction_hsc_rds_sha256",
  sha256_file(input_paths$eicu_time), "eICU severity gate"
)
require_map_value(
  outcome_gate, "eicu_prediction_rds_sha256",
  sha256_file(input_paths$eicu_time), "outcome gate"
)
require_map_value(
  eicu_severity_gate, "apache_benchmark_rds_sha256",
  sha256_file(input_paths$eicu_apache), "eICU severity gate"
)
require_map_value(
  outcome_gate, "eicu_outcome_rds_sha256",
  sha256_file(input_paths$eicu_outcome), "outcome gate"
)
require_map_value(
  main_model_gate, "eicu_outcome_rds_sha256",
  sha256_file(input_paths$eicu_outcome), "main-model gate"
)

private_out <- file.path(PRIVATE_ROOT, "native_benchmarks")
aggregate_out <- file.path(AGGREGATE_ROOT, "native_benchmarks")
qc_out <- file.path(QC_ROOT, "native_benchmarks")
private_bundle_path <- file.path(
  private_out, "native_benchmark_private_v1.rds"
)
aggregate_paths <- c(
  specification = file.path(
    aggregate_out, "native_benchmark_specification_v1.csv"
  ),
  population = file.path(
    aggregate_out, "native_benchmark_population_v1.csv"
  ),
  native_distribution = file.path(
    aggregate_out, "native_probability_unclipped_distribution_v1.csv"
  ),
  performance = file.path(
    aggregate_out, "native_benchmark_performance_v1.csv"
  ),
  coefficients = file.path(
    aggregate_out, "native_benchmark_coefficients_v1.csv"
  ),
  smp_or = file.path(aggregate_out, "native_benchmark_smp_or_v1.csv"),
  paired_difference = file.path(
    aggregate_out, "native_benchmark_N3_minus_N2_v1.csv"
  ),
  likelihood_ratio = file.path(
    aggregate_out, "native_benchmark_likelihood_ratio_v1.csv"
  ),
  bootstrap = file.path(
    aggregate_out, "native_benchmark_bootstrap_summary_v1.csv"
  )
)
qc_paths <- c(
  input_gate = file.path(qc_out, "input_gate_hash_validation.csv"),
  join_timing = file.path(qc_out, "native_join_timing_QC.csv"),
  model_contract = file.path(qc_out, "native_model_contract_QC.csv"),
  bootstrap = file.path(qc_out, "native_bootstrap_success_QC.csv"),
  public_guard = file.path(qc_out, "public_identifier_guard_QC.csv"),
  summary = file.path(qc_out, "native_benchmark_QC.md")
)
aggregate_manifest_path <- file.path(qc_out, "aggregate_output_manifest_v1.csv")
completion_gate <- file.path(
  qc_out, "phase3f_native_benchmarks_complete_v1.csv"
)
planned_outputs <- c(
  private_bundle_path, aggregate_paths, qc_paths, aggregate_manifest_path,
  completion_gate
)
if (any(file.exists(planned_outputs))) {
  stop("A planned D062 output already exists; refusing every overwrite.")
}

# ---------------------------------------------------------------------------
# All authorization, manifest, gate, decision, and hash checks have passed.
# Only here are the checksum-gated predictor and outcome RDS files opened.
# ---------------------------------------------------------------------------

mimic_frame <- as.data.table(readRDS(model_frame_paths$mimic))
eicu_frame <- as.data.table(readRDS(model_frame_paths$eicu))
mimic_time_source <- as.data.table(readRDS(input_paths$mimic_time))
mimic_oasis_source <- as.data.table(readRDS(input_paths$mimic_oasis))
mimic_outcomes <- as.data.table(readRDS(input_paths$mimic_outcome))
eicu_time_source <- as.data.table(readRDS(input_paths$eicu_time))
eicu_apache_source <- as.data.table(readRDS(input_paths$eicu_apache))
eicu_outcomes <- as.data.table(readRDS(input_paths$eicu_outcome))

canonical_required <- c(
  "analysis_id", "smp", "primary_predictor_complete",
  "component_predictor_complete", "normalized_exposure_complete"
)
if (length(setdiff(canonical_required, names(mimic_frame))) ||
    length(setdiff(canonical_required, names(eicu_frame))) ||
    anyDuplicated(mimic_frame$analysis_id) || anyNA(mimic_frame$analysis_id) ||
    anyDuplicated(eicu_frame$analysis_id) || anyNA(eicu_frame$analysis_id)) {
  stop("Canonical model-frame schema/key invariant failed for D062.")
}
if (nrow(mimic_frame) != as.integer(require_map_value(
  parameter_gate, "mimic_frame_n", label = "parameter gate"
)) || nrow(eicu_frame) != as.integer(require_map_value(
  parameter_gate, "eicu_frame_n", label = "parameter gate"
))) {
  stop("Canonical model-frame count differs from the parameter gate.")
}

required_mimic_time <- c(
  "stay_id", "subject_id", "intime", "index_time", "smp", "tuple_observed"
)
required_mimic_oasis <- c(
  "stay_id", "subject_id", "oasis", "component_available_n"
)
required_mimic_outcome <- c(
  "stay_id", "subject_id", "hospital_mortality", "hospital_mortality_eligible"
)
required_eicu_time <- c(
  "patientunitstayid", "hospitalid", "index_time", "smp", "tuple_observed"
)
required_eicu_apache <- c(
  "patientunitstayid", "tuple_observed", "apacheversion",
  "apache_predicted_hospital_risk"
)
required_eicu_outcome <- c(
  "patientunitstayid", "hospitalid", "hospital_mortality",
  "hospital_mortality_eligible"
)
required_source_fields <- list(
  mimic_time = setdiff(required_mimic_time, names(mimic_time_source)),
  mimic_oasis = setdiff(required_mimic_oasis, names(mimic_oasis_source)),
  mimic_outcome = setdiff(required_mimic_outcome, names(mimic_outcomes)),
  eicu_time = setdiff(required_eicu_time, names(eicu_time_source)),
  eicu_apache = setdiff(required_eicu_apache, names(eicu_apache_source)),
  eicu_outcome = setdiff(required_eicu_outcome, names(eicu_outcomes))
)
if (any(lengths(required_source_fields))) {
  stop(
    "A D062 row-level source lacks required fields: ",
    paste(names(required_source_fields)[lengths(required_source_fields) > 0L],
      collapse = ", ")
  )
}

if (anyDuplicated(mimic_time_source$stay_id) ||
    anyDuplicated(mimic_time_source$subject_id) ||
    anyDuplicated(mimic_oasis_source$stay_id) ||
    anyDuplicated(mimic_oasis_source$subject_id) ||
    anyDuplicated(mimic_outcomes$stay_id) ||
    anyDuplicated(mimic_outcomes$subject_id) ||
    anyNA(mimic_time_source$stay_id) || anyNA(mimic_time_source$subject_id) ||
    anyNA(mimic_oasis_source$stay_id) || anyNA(mimic_oasis_source$subject_id) ||
    anyNA(mimic_outcomes$stay_id) || anyNA(mimic_outcomes$subject_id)) {
  stop("MIMIC native-benchmark join keys/patient clusters are invalid.")
}
if (anyDuplicated(eicu_time_source$patientunitstayid) ||
    anyDuplicated(eicu_apache_source$patientunitstayid) ||
    anyDuplicated(eicu_outcomes$patientunitstayid) ||
    anyNA(eicu_time_source$patientunitstayid) ||
    anyNA(eicu_time_source$hospitalid) ||
    anyNA(eicu_apache_source$patientunitstayid) ||
    anyNA(eicu_outcomes$patientunitstayid) || anyNA(eicu_outcomes$hospitalid)) {
  stop("eICU native-benchmark join keys/hospital clusters are invalid.")
}

mimic_time_metadata <- attr(mimic_time_source, "rebuild_metadata")
mimic_oasis_metadata <- attr(mimic_oasis_source, "rebuild_metadata")
mimic_outcome_metadata <- attr(mimic_outcomes, "rebuild_metadata")
eicu_time_metadata <- attr(eicu_time_source, "rebuild_metadata")
eicu_apache_metadata <- attr(eicu_apache_source, "rebuild_metadata")
eicu_outcome_metadata <- attr(eicu_outcomes, "rebuild_metadata")
if (!identical(mimic_time_metadata$version, "mimic_harmonized_severity_core_v1") ||
    !identical(mimic_time_metadata$locked_config_version, LOCKED$version) ||
    !isTRUE(mimic_time_metadata$outcome_blind) ||
    !identical(mimic_oasis_metadata$version, "mimic_native_oasis_benchmark_v1") ||
    !identical(mimic_oasis_metadata$locked_config_version, LOCKED$version) ||
    !identical(mimic_oasis_metadata$official_benchmark, "OASIS") ||
    !identical(mimic_oasis_metadata$predicted_probability_included, FALSE) ||
    !isTRUE(mimic_outcome_metadata$formally_unblinded) ||
    !identical(mimic_outcome_metadata$checkpoint_sha256, sha256_file(checkpoint_path))) {
  stop("MIMIC native predictor/outcome metadata failed provenance checks.")
}
if (!identical(eicu_time_metadata$version, "eicu_harmonized_severity_core_v1") ||
    !identical(eicu_time_metadata$locked_config_version, LOCKED$version) ||
    !isTRUE(eicu_time_metadata$outcome_blind) ||
    !identical(eicu_apache_metadata$version, "eicu_native_apache_iva_benchmark_v1") ||
    !identical(eicu_apache_metadata$locked_config_version, LOCKED$version) ||
    !isTRUE(eicu_apache_metadata$outcome_blind) ||
    !isTRUE(eicu_outcome_metadata$formally_unblinded) ||
    !identical(eicu_outcome_metadata$checkpoint_sha256, sha256_file(checkpoint_path))) {
  stop("eICU native predictor/outcome metadata failed provenance checks.")
}

if (!setequal(mimic_frame$analysis_id, as.integer(mimic_time_source$stay_id)) ||
    !setequal(mimic_frame$analysis_id, as.integer(mimic_outcomes$stay_id)) ||
    !all(mimic_frame$analysis_id %in% as.integer(mimic_oasis_source$stay_id)) ||
    !setequal(eicu_frame$analysis_id, as.integer(eicu_time_source$patientunitstayid)) ||
    !setequal(eicu_frame$analysis_id, as.integer(eicu_outcomes$patientunitstayid)) ||
    !all(eicu_frame$analysis_id %in% as.integer(eicu_apache_source$patientunitstayid))) {
  stop("Canonical tuple and native predictor/outcome analysis-ID sets differ.")
}

mimic_time_link <- mimic_time_source[, .(
  analysis_id = as.integer(stay_id),
  patient_cluster_id = as.character(subject_id),
  intime_epoch = to_epoch(intime),
  index_epoch = to_epoch(index_time),
  source_smp = as.numeric(smp),
  tuple_observed_source = as.logical(tuple_observed)
)]
mimic_oasis_link <- mimic_oasis_source[, .(
  analysis_id = as.integer(stay_id),
  oasis_patient_cluster_id = as.character(subject_id),
  oasis = as.numeric(oasis),
  component_available_n = as.integer(component_available_n)
)]
mimic_outcome_link <- mimic_outcomes[, .(
  analysis_id = as.integer(stay_id),
  outcome_patient_cluster_id = as.character(subject_id),
  mortality = as.integer(hospital_mortality),
  mortality_eligible = as.logical(hospital_mortality_eligible)
)]
mimic_merged <- merge(
  mimic_frame, mimic_time_link, by = "analysis_id", all = FALSE, sort = FALSE
)
mimic_merged <- merge(
  mimic_merged, mimic_oasis_link, by = "analysis_id", all = FALSE, sort = FALSE
)
mimic_merged <- merge(
  mimic_merged, mimic_outcome_link, by = "analysis_id", all = FALSE, sort = FALSE
)
setorder(mimic_merged, analysis_id)
if (nrow(mimic_merged) != nrow(mimic_frame) ||
    anyDuplicated(mimic_merged$analysis_id) ||
    anyNA(mimic_merged$intime_epoch) || anyNA(mimic_merged$index_epoch) ||
    any(mimic_merged$patient_cluster_id !=
      mimic_merged$oasis_patient_cluster_id) ||
    any(mimic_merged$patient_cluster_id !=
      mimic_merged$outcome_patient_cluster_id) ||
    any(!mimic_merged$tuple_observed_source) ||
    !same_numeric(mimic_merged$smp, mimic_merged$source_smp)) {
  stop("MIMIC D062 exact join/time/cluster/sMP invariant failed.")
}

eicu_time_link <- eicu_time_source[, .(
  analysis_id = as.integer(patientunitstayid),
  hospital_cluster_id = as.character(hospitalid),
  index_offset_minutes = as.numeric(index_time),
  source_smp = as.numeric(smp),
  tuple_observed_source = as.logical(tuple_observed)
)]
eicu_apache_link <- eicu_apache_source[, .(
  analysis_id = as.integer(patientunitstayid),
  apache_tuple_observed = as.logical(tuple_observed),
  apache_version = as.character(apacheversion),
  native_probability = as.numeric(apache_predicted_hospital_risk)
)]
eicu_outcome_link <- eicu_outcomes[, .(
  analysis_id = as.integer(patientunitstayid),
  outcome_hospital_cluster_id = as.character(hospitalid),
  mortality = as.integer(hospital_mortality),
  mortality_eligible = as.logical(hospital_mortality_eligible)
)]
eicu_merged <- merge(
  eicu_frame, eicu_time_link, by = "analysis_id", all = FALSE, sort = FALSE
)
eicu_merged <- merge(
  eicu_merged, eicu_apache_link, by = "analysis_id", all = FALSE, sort = FALSE
)
eicu_merged <- merge(
  eicu_merged, eicu_outcome_link, by = "analysis_id", all = FALSE, sort = FALSE
)
setorder(eicu_merged, analysis_id)
if (nrow(eicu_merged) != nrow(eicu_frame) ||
    anyDuplicated(eicu_merged$analysis_id) ||
    anyNA(eicu_merged$index_offset_minutes) ||
    any(eicu_merged$hospital_cluster_id !=
      eicu_merged$outcome_hospital_cluster_id) ||
    any(!eicu_merged$tuple_observed_source) ||
    any(!eicu_merged$apache_tuple_observed) ||
    !same_numeric(eicu_merged$smp, eicu_merged$source_smp)) {
  stop("eICU D062 exact join/time/cluster/sMP invariant failed.")
}

for (frame in list(mimic_merged, eicu_merged)) {
  if (anyNA(frame$mortality_eligible) ||
      any(!is.na(frame$mortality) & !frame$mortality %in% c(0L, 1L)) ||
      any(frame$mortality_eligible & is.na(frame$mortality)) ||
      any(!frame$mortality_eligible & !is.na(frame$mortality))) {
    stop("Primary-outcome eligibility invariant failed after D062 join.")
  }
}

mimic_timing_compatible <-
  mimic_merged$index_epoch >=
    mimic_merged$intime_epoch + MIMIC_NATIVE_WINDOW_SECONDS
mimic_native_valid <- is.finite(mimic_merged$oasis) &
  mimic_merged$oasis == as.integer(mimic_merged$oasis) &
  mimic_merged$component_available_n %in% 0:10
mimic_outcome_free_feasible <- mimic_timing_compatible & mimic_native_valid &
  is.finite(mimic_merged$smp)
mimic_outcome_free_all10 <- mimic_outcome_free_feasible &
  mimic_merged$component_available_n == 10L
if (sum(mimic_outcome_free_feasible) !=
      EXPECTED_MIMIC_OUTCOME_FREE_NATIVE_N ||
    sum(mimic_outcome_free_all10) != EXPECTED_MIMIC_OUTCOME_FREE_ALL10_N) {
  stop("MIMIC outcome-free D062 feasibility counts changed from 1665/1538.")
}
mimic_model_eligible <- mimic_outcome_free_feasible &
  mimic_merged$mortality_eligible &
  mimic_merged$mortality %in% c(0L, 1L)
mimic_primary <- copy(mimic_merged[mimic_model_eligible])
mimic_primary[, `:=`(
  database = "MIMIC-IV_v3.1",
  scenario = "OASIS_index_at_or_after_ICU_plus_24h",
  native_probability = stats::plogis(
    OASIS_NATIVE_INTERCEPT + OASIS_NATIVE_SLOPE * oasis
  ),
  native_lp = OASIS_NATIVE_INTERCEPT + OASIS_NATIVE_SLOPE * oasis,
  smp_per_5 = smp / 5,
  resample_cluster = patient_cluster_id
)]
mimic_complete <- copy(mimic_primary[component_available_n == 10L])
mimic_complete[, scenario := "OASIS_index_at_or_after_ICU_plus_24h_all10"]

eicu_timing_compatible <-
  eicu_merged$index_offset_minutes >= EICU_NATIVE_WINDOW_MINUTES
eicu_native_valid <- !is.na(eicu_merged$native_probability) &
  is.finite(eicu_merged$native_probability) &
  eicu_merged$native_probability >= 0 & eicu_merged$native_probability <= 1
eicu_outcome_free_feasible <- eicu_timing_compatible & eicu_native_valid &
  is.finite(eicu_merged$smp) & !is.na(eicu_merged$apache_version) &
  eicu_merged$apache_version == "IVa"
if (sum(eicu_outcome_free_feasible) != EXPECTED_EICU_OUTCOME_FREE_NATIVE_N) {
  stop("eICU outcome-free D062 feasibility count changed from 211.")
}
eicu_model_eligible <- eicu_outcome_free_feasible &
  eicu_merged$mortality_eligible &
  eicu_merged$mortality %in% c(0L, 1L)
eicu_primary <- copy(eicu_merged[eicu_model_eligible])
eicu_primary[, `:=`(
  database = "eICU-CRD_v2.0",
  scenario = "APACHE_IVa_index_at_or_after_1440min",
  native_lp = stats::qlogis(clip_probability(
    native_probability, PROBABILITY_CLIP_EPS
  )),
  smp_per_5 = smp / 5,
  resample_cluster = hospital_cluster_id
)]

scenario_frames <- list(
  OASIS_index_at_or_after_ICU_plus_24h = mimic_primary,
  OASIS_index_at_or_after_ICU_plus_24h_all10 = mimic_complete,
  APACHE_IVa_index_at_or_after_1440min = eicu_primary
)
for (scenario_name in names(scenario_frames)) {
  frame <- scenario_frames[[scenario_name]]
  if (!nrow(frame) || anyDuplicated(frame$analysis_id) ||
      anyNA(frame$resample_cluster) || uniqueN(frame$resample_cluster) < 2L ||
      anyNA(frame$native_probability) ||
      any(!is.finite(frame$native_probability)) ||
      any(frame$native_probability < 0 | frame$native_probability > 1) ||
      anyNA(frame$native_lp) || any(!is.finite(frame$native_lp)) ||
      anyNA(frame$smp_per_5) || any(!is.finite(frame$smp_per_5)) ||
      uniqueN(frame$mortality) != 2L) {
    stop("D062 scenario is empty, duplicated, incomplete, or one-class: ",
      scenario_name)
  }
}
if (anyDuplicated(mimic_primary$patient_cluster_id) ||
    anyDuplicated(mimic_complete$patient_cluster_id)) {
  stop("MIMIC D062 requires one primary tuple per patient.")
}
if (!setequal(mimic_complete$analysis_id,
    mimic_primary[component_available_n == 10L, analysis_id])) {
  stop("All-10 OASIS sensitivity is not the exact D062 subset.")
}
if (!identical(
  mimic_primary$native_probability,
  stats::plogis(OASIS_NATIVE_INTERCEPT + OASIS_NATIVE_SLOPE *
    mimic_primary$oasis)
)) {
  stop("MIMIC N0 does not equal the exact published OASIS probability.")
}

join_timing_qc <- data.table(
  check = c(
    "MIMIC_canonical_time_outcome_sets_exact_and_OASIS_covers",
    "MIMIC_join_cardinality_preserved",
    "MIMIC_patient_clusters_match_all_sources",
    "MIMIC_sMP_matches_canonical_and_time_source",
    "MIMIC_timing_rule_exact_index_ge_intime_plus_24h",
    "MIMIC_N0_exact_plogis_minus6.1746_plus0.1275_OASIS",
    "MIMIC_all10_exact_subset",
    "eICU_canonical_time_outcome_sets_exact_and_APACHE_covers",
    "eICU_join_cardinality_preserved",
    "eICU_hospital_clusters_match_time_and_outcome",
    "eICU_sMP_matches_canonical_and_time_source",
    "eICU_timing_rule_exact_index_offset_ge_1440min",
    "eICU_APACHE_version_exactly_IVa_when_risk_available",
    "eICU_native_probability_valid_and_unchanged"
  ),
  pass = c(
    setequal(mimic_frame$analysis_id, mimic_time_link$analysis_id) &&
      setequal(mimic_frame$analysis_id, mimic_outcome_link$analysis_id) &&
      all(mimic_frame$analysis_id %in% mimic_oasis_link$analysis_id),
    nrow(mimic_merged) == nrow(mimic_frame),
    all(mimic_merged$patient_cluster_id ==
      mimic_merged$oasis_patient_cluster_id) &&
      all(mimic_merged$patient_cluster_id ==
        mimic_merged$outcome_patient_cluster_id),
    same_numeric(mimic_merged$smp, mimic_merged$source_smp),
    all(mimic_primary$index_epoch >=
      mimic_primary$intime_epoch + MIMIC_NATIVE_WINDOW_SECONDS),
    identical(
      mimic_primary$native_probability,
      stats::plogis(OASIS_NATIVE_INTERCEPT + OASIS_NATIVE_SLOPE *
        mimic_primary$oasis)
    ),
    setequal(mimic_complete$analysis_id,
      mimic_primary[component_available_n == 10L, analysis_id]),
    setequal(eicu_frame$analysis_id, eicu_time_link$analysis_id) &&
      setequal(eicu_frame$analysis_id, eicu_outcome_link$analysis_id) &&
      all(eicu_frame$analysis_id %in% eicu_apache_link$analysis_id),
    nrow(eicu_merged) == nrow(eicu_frame),
    all(eicu_merged$hospital_cluster_id ==
      eicu_merged$outcome_hospital_cluster_id),
    same_numeric(eicu_merged$smp, eicu_merged$source_smp),
    all(eicu_primary$index_offset_minutes >= EICU_NATIVE_WINDOW_MINUTES),
    all(eicu_primary$apache_version == "IVa"),
    all(eicu_primary$native_probability >= 0 &
      eicu_primary$native_probability <= 1)
  ),
  detail = c(
    "exact integer stay analysis_id joins",
    "one primary tuple row retained per canonical MIMIC analysis_id",
    "subject_id agrees in time, OASIS, and formal outcome artifacts",
    "absolute tolerance <=1e-10",
    "no rounding or tolerance; >=86400 seconds exactly",
    "unchanged N0 probability; no clipping before performance",
    "timing-compatible primary population AND component_available_n==10",
    "exact integer patientunitstayid joins",
    "one primary tuple row retained per canonical eICU analysis_id",
    "hospitalid agrees in predictor-time and formal outcome artifacts",
    "absolute tolerance <=1e-10",
    "no rounding or tolerance; >=1440 minutes exactly",
    "all modeled native-risk records are explicitly apacheversion IVa",
    "raw APACHE IVa risk retained; clipping only constructs native_lp"
  )
)
if (any(!join_timing_qc$pass)) {
  stop("A D062 exact join/timing/native-risk QC invariant failed.")
}

# ---------------------------------------------------------------------------
# Exact N0-N3 native-score models. N0 is never fitted or changed. N1-N3 are
# local outcome-fitted updating/extensions and are never external validation.
# ---------------------------------------------------------------------------

native_model_specification <- data.table(
  model_id = NATIVE_MODEL_IDS,
  model_order = unname(NATIVE_MODEL_ORDER),
  formula_contract = c(
    "unchanged_native_probability",
    "offset(native_lp)+intercept",
    "intercept+native_lp",
    "intercept+native_lp+smp_per_5"
  ),
  outcome_refit = c(FALSE, TRUE, TRUE, TRUE),
  role = c(
    "contextual_unchanged_native_probability",
    "local_intercept_only_updating",
    "local_intercept_and_slope_updating",
    "local_native_score_plus_sMP_extension"
  ),
  external_validation_claim = FALSE
)

native_design <- function(frame, model_id) {
  switch(
    model_id,
    N2 = {
      output <- matrix(as.numeric(frame$native_lp), ncol = 1L)
      colnames(output) <- "native_lp"
      output
    },
    N3 = {
      output <- cbind(
        native_lp = as.numeric(frame$native_lp),
        smp_per_5 = as.numeric(frame$smp_per_5)
      )
      storage.mode(output) <- "double"
      output
    },
    stop("No ordinary design matrix is defined for native model ", model_id)
  )
}

fit_native_intercept_only <- function(frame) {
  assert_binary_outcome(frame$mortality)
  x <- matrix(1, nrow = nrow(frame), ncol = 1L)
  colnames(x) <- "(Intercept)"
  fit <- suppressWarnings(stats::glm.fit(
    x = x, y = frame$mortality, offset = frame$native_lp,
    family = stats::binomial(), control = stats::glm.control(maxit = 100L)
  ))
  if (!fit$converged || fit$rank != 1L || anyNA(fit$coefficients) ||
      any(!is.finite(fit$coefficients))) {
    stop("N1 intercept-only local updating failed or is rank deficient.")
  }
  probability <- stats::plogis(
    as.numeric(fit$coefficients[[1L]]) + frame$native_lp
  )
  information <- sum(probability * (1 - probability))
  if (!is.finite(information) || information <= 0) {
    stop("N1 intercept-only local updating has invalid information.")
  }
  covariance <- matrix(
    1 / information, nrow = 1L, ncol = 1L,
    dimnames = list("(Intercept)", "(Intercept)")
  )
  pp <- clip_probability(probability, PROBABILITY_CLIP_EPS)
  structure(list(
    model_id = "N1", status = "ESTIMABLE", reason = "",
    coefficients = setNames(
      as.numeric(fit$coefficients[[1L]]), "(Intercept)"
    ),
    vcov = covariance, design_columns = character(), n = nrow(frame),
    events = sum(frame$mortality), rank = fit$rank,
    loglik = sum(frame$mortality * log(pp) +
      (1 - frame$mortality) * log1p(-pp)),
    offset_contract = "native_lp_coefficient_fixed_at_1"
  ), class = "ards_native_local_model")
}

fit_native_models <- function(frame) {
  assert_binary_outcome(frame$mortality)
  fit_n1 <- fit_native_intercept_only(frame)
  fit_n2 <- fit_model(
    native_design(frame, "N2"), frame$mortality, "N2",
    allow_nonestimable = FALSE
  )
  fit_n3 <- fit_model(
    native_design(frame, "N3"), frame$mortality, "N3",
    allow_nonestimable = FALSE
  )
  if (!identical(fit_n2$status, "ESTIMABLE") ||
      !identical(fit_n3$status, "ESTIMABLE") ||
      !identical(names(fit_n2$coefficients), c("(Intercept)", "native_lp")) ||
      !identical(
        names(fit_n3$coefficients),
        c("(Intercept)", "native_lp", "smp_per_5")
      )) {
    stop("N2/N3 local updating/extension coefficient contract failed.")
  }
  list(N1 = fit_n1, N2 = fit_n2, N3 = fit_n3)
}

predict_native_model <- function(fit, frame, model_id) {
  if (identical(model_id, "N1")) {
    probability <- stats::plogis(
      fit$coefficients[["(Intercept)"]] + frame$native_lp
    )
  } else if (model_id %in% c("N2", "N3")) {
    probability <- predict_model(fit, native_design(frame, model_id))
  } else {
    stop("Prediction requested for unknown fitted native model: ", model_id)
  }
  if (length(probability) != nrow(frame) || anyNA(probability) ||
      any(!is.finite(probability)) || any(probability <= 0 | probability >= 1)) {
    stop("Invalid local native-model probability for ", model_id)
  }
  as.numeric(probability)
}

coefficient_table <- function(fits, database, scenario) {
  rbindlist(lapply(names(fits), function(model_id) {
    fit <- fits[[model_id]]
    standard_error <- sqrt(diag(fit$vcov))
    if (!identical(names(standard_error), names(fit$coefficients)) ||
        anyNA(standard_error) || any(!is.finite(standard_error)) ||
        any(standard_error <= 0)) {
      stop("Invalid coefficient covariance for ", scenario, "/", model_id)
    }
    data.table(
      database = database, scenario = scenario, model_id = model_id,
      model_order = NATIVE_MODEL_ORDER[[model_id]],
      term = names(fit$coefficients),
      estimate_log_odds = as.numeric(fit$coefficients),
      standard_error = as.numeric(standard_error),
      ci_lower_log_odds = as.numeric(fit$coefficients) - 1.96 * standard_error,
      ci_upper_log_odds = as.numeric(fit$coefficients) + 1.96 * standard_error,
      odds_ratio = exp(as.numeric(fit$coefficients)),
      odds_ratio_ci_lower = exp(
        as.numeric(fit$coefficients) - 1.96 * standard_error
      ),
      odds_ratio_ci_upper = exp(
        as.numeric(fit$coefficients) + 1.96 * standard_error
      ),
      effect_scale = fifelse(
        names(fit$coefficients) == "smp_per_5",
        "per_5_J_per_min", "model_coefficient"
      ),
      interpretation = fifelse(
        model_id == "N3", "local_extension_not_external_validation",
        "local_updating_not_external_validation"
      )
    )
  }), use.names = TRUE)
}

native_likelihood_ratio <- function(fits, database, scenario, n) {
  fit_n2 <- fits$N2
  fit_n3 <- fits$N3
  df <- length(fit_n3$coefficients) - length(fit_n2$coefficients)
  statistic <- 2 * (fit_n3$loglik - fit_n2$loglik)
  if (!identical(as.integer(df), 1L) || !is.finite(statistic) ||
      statistic < -1e-8) {
    stop("N3 versus N2 nested-likelihood contract failed for ", scenario)
  }
  statistic <- max(0, statistic)
  data.table(
    database = database, scenario = scenario,
    comparison_id = ONLY_LRT_COMPARISON,
    new_model = "N3", reference_model = "N2",
    likelihood_ratio_statistic = statistic,
    degrees_of_freedom = 1L,
    p_value = stats::pchisq(statistic, df = 1L, lower.tail = FALSE),
    n = n, same_sample = TRUE,
    only_allowed_nested_likelihood_ratio_test = TRUE,
    interpretation = "local_sMP_extension_test_not_external_validation"
  )
}

fit_native_scenario <- function(frame) {
  database <- unique(frame$database)
  scenario <- unique(frame$scenario)
  if (length(database) != 1L || length(scenario) != 1L) {
    stop("A native scenario mixes databases or scenario labels.")
  }
  fits <- fit_native_models(frame)
  probabilities <- list(
    N0 = as.numeric(frame$native_probability),
    N1 = predict_native_model(fits$N1, frame, "N1"),
    N2 = predict_native_model(fits$N2, frame, "N2"),
    N3 = predict_native_model(fits$N3, frame, "N3")
  )
  performance <- rbindlist(lapply(NATIVE_MODEL_IDS, function(model_id) {
    metric <- performance_vector(frame$mortality, probabilities[[model_id]])
    data.table(
      database = database, scenario = scenario, model_id = model_id,
      model_order = NATIVE_MODEL_ORDER[[model_id]], metric = metric_names,
      estimate = as.numeric(metric[metric_names]),
      n = nrow(frame), events = sum(frame$mortality)
    )
  }))
  difference <- performance[model_id == "N3", .(
    database, scenario, metric,
    estimate_new_minus_reference = estimate
  )]
  difference[, estimate_new_minus_reference :=
    estimate_new_minus_reference -
      performance[model_id == "N2", estimate][match(
        metric, performance[model_id == "N2", metric]
      )]]
  difference[, `:=`(
    comparison_id = ONLY_LRT_COMPARISON,
    new_model = "N3", reference_model = "N2", n = nrow(frame),
    events = sum(frame$mortality), same_sample = TRUE
  )]
  setcolorder(difference, c(
    "database", "scenario", "comparison_id", "new_model",
    "reference_model", "metric", "estimate_new_minus_reference",
    "n", "events", "same_sample"
  ))
  predictions <- data.table(
    database = database, scenario = scenario,
    analysis_id = frame$analysis_id,
    resample_cluster = frame$resample_cluster,
    mortality = frame$mortality,
    native_probability_unchanged = probabilities$N0,
    probability_N1_local_intercept = probabilities$N1,
    probability_N2_local_intercept_slope = probabilities$N2,
    probability_N3_local_native_plus_smp = probabilities$N3
  )
  list(
    fits = fits, probabilities = probabilities, performance = performance,
    coefficients = coefficient_table(fits, database, scenario),
    difference = difference,
    likelihood_ratio = native_likelihood_ratio(
      fits, database, scenario, nrow(frame)
    ),
    predictions = predictions
  )
}

point_results <- lapply(scenario_frames, fit_native_scenario)
point_performance <- rbindlist(lapply(
  point_results, `[[`, "performance"
), use.names = TRUE)
point_coefficients <- rbindlist(lapply(
  point_results, `[[`, "coefficients"
), use.names = TRUE)
point_differences <- rbindlist(lapply(
  point_results, `[[`, "difference"
), use.names = TRUE)
likelihood_ratio_results <- rbindlist(lapply(
  point_results, `[[`, "likelihood_ratio"
), use.names = TRUE)
private_predictions <- rbindlist(lapply(
  point_results, `[[`, "predictions"
), use.names = TRUE)
if (uniqueN(likelihood_ratio_results$comparison_id) != 1L ||
    !identical(unique(likelihood_ratio_results$comparison_id),
      ONLY_LRT_COMPARISON) ||
    any(!likelihood_ratio_results$only_allowed_nested_likelihood_ratio_test) ||
    any(!likelihood_ratio_results$same_sample)) {
  stop("A likelihood-ratio test other than N3 versus N2 was produced.")
}

# ---------------------------------------------------------------------------
# Locked resampling. N0 is a fixed prediction and receives percentile CIs with
# no refit/optimism step. N1-N3 are refit in every resample and evaluated on
# both the bootstrap training sample and the unchanged original test sample.
# ---------------------------------------------------------------------------

run_native_bootstrap <- function(frame, repetitions, seed, resampling_unit) {
  metric_rows <- list()
  difference_rows <- list()
  metric_position <- 0L
  difference_position <- 0L
  set.seed(seed)
  for (replicate_id in seq_len(repetitions)) {
    index <- cluster_bootstrap_indices(frame$resample_cluster)
    train <- frame[index]
    class_ok <- uniqueN(train$mortality) == 2L
    metric_failure <- function(model_id, reason, refit) data.table(
      replicate = replicate_id, seed = seed, model_id = model_id,
      success = FALSE, reason = safe_reason(reason), metric = NA_character_,
      train_estimate = NA_real_, test_original_estimate = NA_real_,
      optimism = NA_real_, refit_in_resample = refit,
      train_n = nrow(train), train_events = sum(train$mortality),
      resampling_unit = resampling_unit
    )
    difference_failure <- function(reason) data.table(
      replicate = replicate_id, seed = seed,
      comparison_id = ONLY_LRT_COMPARISON,
      success = FALSE, reason = safe_reason(reason), metric = NA_character_,
      train_difference = NA_real_, test_original_difference = NA_real_,
      optimism_difference = NA_real_, train_n = nrow(train),
      train_events = sum(train$mortality), resampling_unit = resampling_unit
    )
    if (!class_ok) {
      for (model_id in NATIVE_MODEL_IDS) {
        metric_position <- metric_position + 1L
        metric_rows[[metric_position]] <- metric_failure(
          model_id, "single_outcome_class", model_id != "N0"
        )
      }
      difference_position <- difference_position + 1L
      difference_rows[[difference_position]] <- difference_failure(
        "single_outcome_class"
      )
      next
    }

    n0_result <- tryCatch({
      metric <- performance_vector(train$mortality, train$native_probability)
      data.table(
        replicate = replicate_id, seed = seed, model_id = "N0",
        success = TRUE, reason = "", metric = metric_names,
        train_estimate = as.numeric(metric[metric_names]),
        test_original_estimate = NA_real_, optimism = NA_real_,
        refit_in_resample = FALSE, train_n = nrow(train),
        train_events = sum(train$mortality), resampling_unit = resampling_unit
      )
    }, error = function(e) metric_failure("N0", conditionMessage(e), FALSE))
    metric_position <- metric_position + 1L
    metric_rows[[metric_position]] <- n0_result

    local_result <- tryCatch({
      fits <- fit_native_models(train)
      train_probability <- list(
        N1 = predict_native_model(fits$N1, train, "N1"),
        N2 = predict_native_model(fits$N2, train, "N2"),
        N3 = predict_native_model(fits$N3, train, "N3")
      )
      test_probability <- list(
        N1 = predict_native_model(fits$N1, frame, "N1"),
        N2 = predict_native_model(fits$N2, frame, "N2"),
        N3 = predict_native_model(fits$N3, frame, "N3")
      )
      metric_tables <- lapply(c("N1", "N2", "N3"), function(model_id) {
        train_metric <- performance_vector(
          train$mortality, train_probability[[model_id]]
        )
        test_metric <- performance_vector(
          frame$mortality, test_probability[[model_id]]
        )
        data.table(
          replicate = replicate_id, seed = seed, model_id = model_id,
          success = TRUE, reason = "", metric = metric_names,
          train_estimate = as.numeric(train_metric[metric_names]),
          test_original_estimate = as.numeric(test_metric[metric_names]),
          optimism = as.numeric(
            train_metric[metric_names] - test_metric[metric_names]
          ),
          refit_in_resample = TRUE, train_n = nrow(train),
          train_events = sum(train$mortality), resampling_unit = resampling_unit
        )
      })
      train_n2 <- performance_vector(
        train$mortality, train_probability$N2
      )
      train_n3 <- performance_vector(
        train$mortality, train_probability$N3
      )
      test_n2 <- performance_vector(frame$mortality, test_probability$N2)
      test_n3 <- performance_vector(frame$mortality, test_probability$N3)
      train_difference <- train_n3[metric_names] - train_n2[metric_names]
      test_difference <- test_n3[metric_names] - test_n2[metric_names]
      difference_table <- data.table(
        replicate = replicate_id, seed = seed,
        comparison_id = ONLY_LRT_COMPARISON,
        success = TRUE, reason = "", metric = metric_names,
        train_difference = as.numeric(train_difference),
        test_original_difference = as.numeric(test_difference),
        optimism_difference = as.numeric(train_difference - test_difference),
        train_n = nrow(train), train_events = sum(train$mortality),
        resampling_unit = resampling_unit
      )
      list(metric = metric_tables, difference = difference_table)
    }, error = identity)

    if (inherits(local_result, "error")) {
      for (model_id in c("N1", "N2", "N3")) {
        metric_position <- metric_position + 1L
        metric_rows[[metric_position]] <- metric_failure(
          model_id, conditionMessage(local_result), TRUE
        )
      }
      difference_position <- difference_position + 1L
      difference_rows[[difference_position]] <- difference_failure(
        conditionMessage(local_result)
      )
    } else {
      for (table in local_result$metric) {
        metric_position <- metric_position + 1L
        metric_rows[[metric_position]] <- table
      }
      difference_position <- difference_position + 1L
      difference_rows[[difference_position]] <- local_result$difference
    }
  }
  list(
    metric = rbindlist(metric_rows, use.names = TRUE, fill = TRUE),
    difference = rbindlist(difference_rows, use.names = TRUE, fill = TRUE)
  )
}

scenario_resampling <- data.table(
  scenario = names(scenario_frames),
  requested_repetitions = c(
    MIMIC_BOOTSTRAP_REPS, MIMIC_BOOTSTRAP_REPS,
    EICU_CLUSTER_BOOTSTRAP_REPS
  ),
  seed = c(
    MIMIC_BOOTSTRAP_SEED, MIMIC_BOOTSTRAP_SEED, EICU_BOOTSTRAP_SEED
  ),
  resampling_unit = c(
    "MIMIC_subject_id_patient_cluster",
    "MIMIC_subject_id_patient_cluster",
    "eICU_hospital_cluster"
  )
)
bootstrap_results <- lapply(names(scenario_frames), function(scenario_name) {
  rule <- scenario_resampling[scenario == scenario_name]
  result <- run_native_bootstrap(
    scenario_frames[[scenario_name]], rule$requested_repetitions,
    rule$seed, rule$resampling_unit
  )
  result$metric[, `:=`(
    database = unique(scenario_frames[[scenario_name]]$database),
    scenario = scenario_name
  )]
  result$difference[, `:=`(
    database = unique(scenario_frames[[scenario_name]]$database),
    scenario = scenario_name
  )]
  result
})
names(bootstrap_results) <- names(scenario_frames)
bootstrap_metric_private <- rbindlist(lapply(
  bootstrap_results, `[[`, "metric"
), use.names = TRUE, fill = TRUE)
bootstrap_difference_private <- rbindlist(lapply(
  bootstrap_results, `[[`, "difference"
), use.names = TRUE, fill = TRUE)

bootstrap_model_success <- bootstrap_metric_private[, .(
  observed_replicates = uniqueN(replicate),
  minimum_replicate = min(replicate),
  maximum_replicate = max(replicate),
  successful_replicates = uniqueN(replicate[success == TRUE]),
  failed_replicates = uniqueN(replicate[success == FALSE]),
  replicate_status_unique_pass = {
    groups <- split(seq_along(replicate), replicate)
    all(vapply(groups, function(index) {
      uniqueN(success[index]) == 1L
    }, logical(1L)))
  },
  replicate_payload_pass = {
    groups <- split(seq_along(replicate), replicate)
    all(vapply(groups, function(index) {
      status <- unique(success[index])
      length(status) == 1L && if (isTRUE(status[[1L]])) {
        length(index) == length(metric_names) &&
          !anyDuplicated(metric[index]) && setequal(metric[index], metric_names)
      } else {
        length(index) == 1L && is.na(metric[index])
      }
    }, logical(1L)))
  },
  first_failure_reason = {
    reason_value <- reason[success == FALSE & !is.na(reason) & nzchar(reason)]
    if (length(reason_value)) reason_value[[1L]] else ""
  },
  refit_contract_pass = if (model_id[[1L]] == "N0") {
    all(refit_in_resample == FALSE)
  } else {
    all(refit_in_resample == TRUE)
  }
), by = .(database, scenario, model_id)]
bootstrap_model_success <- merge(
  bootstrap_model_success, scenario_resampling,
  by = "scenario", all.x = TRUE, sort = FALSE
)
bootstrap_model_success[, success_rate :=
  successful_replicates / requested_repetitions]
bootstrap_model_success[, `:=`(
  accounted_replicates = successful_replicates + failed_replicates,
  replicate_accounting_pass =
    observed_replicates == requested_repetitions &
    minimum_replicate == 1L & maximum_replicate == requested_repetitions &
    replicate_status_unique_pass & replicate_payload_pass &
    successful_replicates + failed_replicates == requested_repetitions
)]

bootstrap_difference_success <- bootstrap_difference_private[, .(
  observed_replicates = uniqueN(replicate),
  minimum_replicate = min(replicate),
  maximum_replicate = max(replicate),
  successful_replicates = uniqueN(replicate[success == TRUE]),
  failed_replicates = uniqueN(replicate[success == FALSE]),
  replicate_status_unique_pass = {
    groups <- split(seq_along(replicate), replicate)
    all(vapply(groups, function(index) {
      uniqueN(success[index]) == 1L
    }, logical(1L)))
  },
  replicate_payload_pass = {
    groups <- split(seq_along(replicate), replicate)
    all(vapply(groups, function(index) {
      status <- unique(success[index])
      length(status) == 1L && if (isTRUE(status[[1L]])) {
        length(index) == length(metric_names) &&
          !anyDuplicated(metric[index]) && setequal(metric[index], metric_names)
      } else {
        length(index) == 1L && is.na(metric[index])
      }
    }, logical(1L)))
  },
  first_failure_reason = {
    reason_value <- reason[success == FALSE & !is.na(reason) & nzchar(reason)]
    if (length(reason_value)) reason_value[[1L]] else ""
  }
), by = .(database, scenario, comparison_id)]
bootstrap_difference_success <- merge(
  bootstrap_difference_success, scenario_resampling,
  by = "scenario", all.x = TRUE, sort = FALSE
)
bootstrap_difference_success[, success_rate :=
  successful_replicates / requested_repetitions]
bootstrap_difference_success[, `:=`(
  accounted_replicates = successful_replicates + failed_replicates,
  replicate_accounting_pass =
    observed_replicates == requested_repetitions &
    minimum_replicate == 1L & maximum_replicate == requested_repetitions &
    replicate_status_unique_pass & replicate_payload_pass &
    successful_replicates + failed_replicates == requested_repetitions
)]

if (any(bootstrap_model_success$success_rate < BOOTSTRAP_SUCCESS_THRESHOLD) ||
    any(bootstrap_difference_success$success_rate <
      BOOTSTRAP_SUCCESS_THRESHOLD) ||
    any(!bootstrap_model_success$replicate_accounting_pass) ||
    any(!bootstrap_difference_success$replicate_accounting_pass) ||
    any(!bootstrap_model_success$refit_contract_pass)) {
  stop(paste(
    "A D062 native bootstrap failed the locked >=95%, exact-replicate",
    "accounting, or refit contract."
  ))
}
if (any(bootstrap_metric_private[
      model_id == "N0" & success == TRUE,
      refit_in_resample != FALSE | !is.na(optimism) |
        !is.na(test_original_estimate)
    ]) ||
    any(bootstrap_metric_private[
      model_id %in% c("N1", "N2", "N3") & success == TRUE,
      refit_in_resample != TRUE | !is.finite(optimism) |
        !is.finite(test_original_estimate)
    ]) ||
    any(bootstrap_difference_private[
      success == TRUE,
      !is.finite(optimism_difference) |
        !is.finite(test_original_difference)
    ])) {
  stop("N0 fixed-prediction or N1-N3 optimism-bootstrap invariant failed.")
}

n0_bootstrap_ci <- bootstrap_metric_private[
  model_id == "N0" & success == TRUE,
  {
    interval <- percentile_interval(train_estimate)
    list(
      ci_lower = interval[["lower"]], ci_upper = interval[["upper"]],
      bootstrap_successful_replicates = uniqueN(replicate)
    )
  },
  by = .(database, scenario, model_id, metric)
]
local_optimism <- bootstrap_metric_private[
  model_id %in% c("N1", "N2", "N3") & success == TRUE,
  {
    interval <- percentile_interval(optimism)
    list(
      mean_optimism = mean(optimism),
      optimism_ci_lower = interval[["lower"]],
      optimism_ci_upper = interval[["upper"]],
      bootstrap_successful_replicates = uniqueN(replicate)
    )
  },
  by = .(database, scenario, model_id, metric)
]
difference_optimism <- bootstrap_difference_private[
  success == TRUE,
  {
    interval <- percentile_interval(optimism_difference)
    list(
      mean_optimism_difference = mean(optimism_difference),
      optimism_difference_ci_lower = interval[["lower"]],
      optimism_difference_ci_upper = interval[["upper"]],
      bootstrap_successful_replicates = uniqueN(replicate)
    )
  },
  by = .(database, scenario, comparison_id, metric)
]

scenario_order <- setNames(seq_along(scenario_frames), names(scenario_frames))
n0_public <- merge(
  point_performance[model_id == "N0"], n0_bootstrap_ci,
  by = c("database", "scenario", "model_id", "metric"),
  all.x = TRUE, sort = FALSE
)
n0_public[, `:=`(
  model_order = NATIVE_MODEL_ORDER[["N0"]],
  result_stage = "unchanged_original_native_probability",
  result_stage_order = 1L,
  estimate_type = "apparent_fixed_native_probability",
  uncertainty_method = "percentile_cluster_bootstrap_fixed_prediction_no_refit",
  optimism_correction_applied = FALSE,
  interpretation = "contextual_native_benchmark_not_external_validation"
)]

local_apparent <- copy(point_performance[
  model_id %in% c("N1", "N2", "N3")
])
local_apparent[, `:=`(
  ci_lower = NA_real_, ci_upper = NA_real_,
  bootstrap_successful_replicates = NA_integer_,
  result_stage = fifelse(
    model_id == "N3", "local_extension_apparent", "local_updating_apparent"
  ),
  result_stage_order = 2L,
  estimate_type = "apparent_outcome_fitted",
  uncertainty_method = "descriptive_apparent_no_CI_primary_is_optimism_corrected",
  optimism_correction_applied = FALSE,
  interpretation = fifelse(
    model_id == "N3", "local_extension_not_external_validation",
    "local_updating_not_external_validation"
  )
)]

local_corrected <- merge(
  local_optimism,
  point_performance[
    model_id %in% c("N1", "N2", "N3"),
    .(database, scenario, model_id, model_order, metric,
      apparent_estimate = estimate, n, events)
  ],
  by = c("database", "scenario", "model_id", "metric"),
  all.x = TRUE, sort = FALSE
)
local_corrected[, `:=`(
  estimate = apparent_estimate - mean_optimism,
  ci_lower = apparent_estimate - optimism_ci_upper,
  ci_upper = apparent_estimate - optimism_ci_lower,
  result_stage = fifelse(
    model_id == "N3",
    "local_extension_optimism_corrected",
    "local_updating_optimism_corrected"
  ),
  result_stage_order = 3L,
  estimate_type = "bootstrap_optimism_corrected",
  uncertainty_method = paste0(
    "refit_bootstrap_train_minus_original_test_optimism;",
    "point=apparent-mean_optimism;CI=apparent-percentile_optimism"
  ),
  optimism_correction_applied = TRUE,
  interpretation = fifelse(
    model_id == "N3", "local_extension_not_external_validation",
    "local_updating_not_external_validation"
  )
)]

performance_public <- rbindlist(list(
  n0_public[, .(
    database, scenario, model_id, model_order, result_stage,
    result_stage_order, metric, estimate, ci_lower, ci_upper, n, events,
    bootstrap_successful_replicates, estimate_type, uncertainty_method,
    optimism_correction_applied, interpretation
  )],
  local_apparent[, .(
    database, scenario, model_id, model_order, result_stage,
    result_stage_order, metric, estimate, ci_lower, ci_upper, n, events,
    bootstrap_successful_replicates, estimate_type, uncertainty_method,
    optimism_correction_applied, interpretation
  )],
  local_corrected[, .(
    database, scenario, model_id, model_order, result_stage,
    result_stage_order, metric, estimate, ci_lower, ci_upper, n, events,
    bootstrap_successful_replicates, estimate_type, uncertainty_method,
    optimism_correction_applied, interpretation
  )]
), use.names = TRUE)
performance_public[, scenario_order := scenario_order[scenario]]
performance_public[, metric_order := match(metric, metric_names)]
setorder(
  performance_public, scenario_order, model_order, result_stage_order,
  metric_order
)
performance_public[, metric_order := NULL]

difference_apparent <- copy(point_differences)
difference_apparent[, `:=`(
  result_stage = "apparent_paired_N3_minus_N2",
  result_stage_order = 1L,
  ci_lower = NA_real_, ci_upper = NA_real_,
  bootstrap_successful_replicates = NA_integer_,
  optimism_correction_applied = FALSE,
  uncertainty_method = "descriptive_apparent_no_CI",
  interpretation = "paired_local_extension_difference_not_external_validation"
)]
difference_corrected <- merge(
  difference_optimism,
  point_differences[, .(
    database, scenario, comparison_id, new_model, reference_model, metric,
    apparent_difference = estimate_new_minus_reference, n, events, same_sample
  )],
  by = c("database", "scenario", "comparison_id", "metric"),
  all.x = TRUE, sort = FALSE
)
difference_corrected[, `:=`(
  estimate_new_minus_reference =
    apparent_difference - mean_optimism_difference,
  ci_lower = apparent_difference - optimism_difference_ci_upper,
  ci_upper = apparent_difference - optimism_difference_ci_lower,
  result_stage = "optimism_corrected_paired_N3_minus_N2",
  result_stage_order = 2L,
  optimism_correction_applied = TRUE,
  uncertainty_method = paste0(
    "paired_refit_bootstrap_train_minus_original_test_optimism;",
    "point=apparent-mean_optimism"
  ),
  interpretation = "paired_local_extension_difference_not_external_validation"
)]
paired_difference_public <- rbindlist(list(
  difference_apparent[, .(
    database, scenario, comparison_id, new_model, reference_model,
    result_stage, result_stage_order, metric,
    estimate_new_minus_reference, ci_lower, ci_upper, n, events, same_sample,
    bootstrap_successful_replicates, optimism_correction_applied,
    uncertainty_method, interpretation
  )],
  difference_corrected[, .(
    database, scenario, comparison_id, new_model, reference_model,
    result_stage, result_stage_order, metric,
    estimate_new_minus_reference, ci_lower, ci_upper, n, events, same_sample,
    bootstrap_successful_replicates, optimism_correction_applied,
    uncertainty_method, interpretation
  )]
), use.names = TRUE)
paired_difference_public[, scenario_order := scenario_order[scenario]]
paired_difference_public[, metric_order := match(metric, metric_names)]
setorder(
  paired_difference_public, scenario_order, result_stage_order, metric_order
)
paired_difference_public[, metric_order := NULL]

smp_or_public <- point_coefficients[
  model_id == "N3" & term == "smp_per_5",
  .(
    database, scenario, model_id, term, estimate_log_odds, standard_error,
    odds_ratio, odds_ratio_ci_lower, odds_ratio_ci_upper, effect_scale,
    interpretation
  )
]
if (nrow(smp_or_public) != length(scenario_frames) ||
    any(smp_or_public$effect_scale != "per_5_J_per_min") ||
    any(!is.finite(smp_or_public$odds_ratio))) {
  stop("D062 N3 linear sMP/5 odds-ratio output invariant failed.")
}

population_public <- data.table(
  database = c("MIMIC-IV_v3.1", "MIMIC-IV_v3.1", "eICU-CRD_v2.0"),
  scenario = names(scenario_frames),
  scenario_order = seq_along(scenario_frames),
  source_primary_tuple_n = c(
    nrow(mimic_frame), nrow(mimic_frame), nrow(eicu_frame)
  ),
  outcome_free_timing_native_feasible_n = c(
    sum(mimic_outcome_free_feasible), sum(mimic_outcome_free_all10),
    sum(eicu_outcome_free_feasible)
  ),
  locked_outcome_free_expected_n = c(
    EXPECTED_MIMIC_OUTCOME_FREE_NATIVE_N,
    EXPECTED_MIMIC_OUTCOME_FREE_ALL10_N,
    EXPECTED_EICU_OUTCOME_FREE_NATIVE_N
  ),
  outcome_analysis_n = vapply(scenario_frames, nrow, integer(1L)),
  excluded_for_outcome_unavailable_n = c(
    sum(mimic_outcome_free_feasible) - nrow(mimic_primary),
    sum(mimic_outcome_free_all10) - nrow(mimic_complete),
    sum(eicu_outcome_free_feasible) - nrow(eicu_primary)
  ),
  event_n = vapply(
    scenario_frames, function(frame) sum(frame$mortality), integer(1L)
  ),
  nonevent_n = vapply(
    scenario_frames, function(frame) sum(frame$mortality == 0L), integer(1L)
  ),
  resampling_cluster_n = vapply(
    scenario_frames, function(frame) uniqueN(frame$resample_cluster), integer(1L)
  ),
  same_sample_for_N0_N1_N2_N3 = TRUE,
  center_specific_rows_public = FALSE
)
if (any(population_public$outcome_analysis_n !=
      population_public$event_n + population_public$nonevent_n) ||
    any(population_public$outcome_free_timing_native_feasible_n !=
      population_public$locked_outcome_free_expected_n) ||
    any(population_public$excluded_for_outcome_unavailable_n < 0L)) {
  stop("D062 population accounting invariant failed.")
}

# The SAP requires the distribution of the original native probability to be
# reported without clipping. Clipping remains confined to construction of the
# native logit used by N1-N3. These are aggregate summaries only.
native_distribution_public <- rbindlist(lapply(
  names(scenario_frames), function(scenario_name) {
    frame <- scenario_frames[[scenario_name]]
    probability <- as.numeric(frame$native_probability)
    quantiles <- as.numeric(stats::quantile(
      probability, probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
      names = FALSE, type = 2L
    ))
    data.table(
      database = unique(frame$database), scenario = scenario_name,
      scenario_order = scenario_order[[scenario_name]], n = length(probability),
      minimum = min(probability), p05 = quantiles[[1L]],
      p25 = quantiles[[2L]], median = quantiles[[3L]],
      mean = mean(probability), standard_deviation = stats::sd(probability),
      p75 = quantiles[[4L]], p95 = quantiles[[5L]], maximum = max(probability),
      probability_equal_zero_n = sum(probability == 0),
      probability_equal_one_n = sum(probability == 1),
      probability_clipped_for_distribution = FALSE,
      clipping_used_only_to_construct_native_logit = TRUE
    )
  }
), use.names = TRUE)
if (nrow(native_distribution_public) != length(scenario_frames) ||
    any(native_distribution_public$n !=
      vapply(scenario_frames, nrow, integer(1L))) ||
    any(!is.finite(as.matrix(native_distribution_public[, .(
      minimum, p05, p25, median, mean, standard_deviation,
      p75, p95, maximum
    )]))) ||
    any(native_distribution_public$minimum < 0) ||
    any(native_distribution_public$maximum > 1) ||
    any(native_distribution_public$probability_clipped_for_distribution) ||
    any(!native_distribution_public$
      clipping_used_only_to_construct_native_logit)) {
  stop("Unclipped native-probability distribution reporting invariant failed.")
}

specification_public <- data.table(
  scenario = names(scenario_frames),
  scenario_order = seq_along(scenario_frames),
  database = c("MIMIC-IV_v3.1", "MIMIC-IV_v3.1", "eICU-CRD_v2.0"),
  native_benchmark = c("OASIS", "OASIS", "APACHE_IVa_native_risk"),
  timing_rule = c(
    "index_time >= ICU_intime + 24h",
    "index_time >= ICU_intime + 24h",
    "index_offset_minutes >= 1440"
  ),
  component_rule = c(
    "source-faithful OASIS", "component_available_n == 10",
    "valid APACHE IVa native risk in [0,1]"
  ),
  N0 = "unchanged_original_native_probability",
  N1 = "offset(native_lp)+intercept_local_updating",
  N2 = "intercept+native_lp_local_updating",
  N3 = "intercept+native_lp+smp_per_5_local_extension",
  only_likelihood_ratio_test = ONLY_LRT_COMPARISON,
  requested_bootstrap_repetitions =
    scenario_resampling$requested_repetitions,
  bootstrap_seed = scenario_resampling$seed,
  resampling_unit = scenario_resampling$resampling_unit,
  bootstrap_success_threshold = BOOTSTRAP_SUCCESS_THRESHOLD,
  external_validation_claim = FALSE
)

bootstrap_summary_public <- rbindlist(list(
  bootstrap_model_success[, .(
    database, scenario, result_type = "model_performance", model_id,
    comparison_id = NA_character_, requested_repetitions,
    observed_replicates, successful_replicates, failed_replicates,
    accounted_replicates, replicate_status_unique_pass,
    replicate_payload_pass, replicate_accounting_pass, success_rate,
    success_threshold = BOOTSTRAP_SUCCESS_THRESHOLD,
    threshold_pass = success_rate >= BOOTSTRAP_SUCCESS_THRESHOLD,
    refit_contract_pass, seed, resampling_unit, first_failure_reason
  )],
  bootstrap_difference_success[, .(
    database, scenario, result_type = "paired_metric_difference",
    model_id = NA_character_, comparison_id, requested_repetitions,
    observed_replicates, successful_replicates, failed_replicates,
    accounted_replicates, replicate_status_unique_pass,
    replicate_payload_pass, replicate_accounting_pass, success_rate,
    success_threshold = BOOTSTRAP_SUCCESS_THRESHOLD,
    threshold_pass = success_rate >= BOOTSTRAP_SUCCESS_THRESHOLD,
    refit_contract_pass = TRUE, seed, resampling_unit, first_failure_reason
  )]
), use.names = TRUE)

input_gate_qc <- data.table(
  check = c(
    "authorization_checkpoint_PASS",
    "checkpoint_directly_hash_locks_script_14",
    "checkpoint_confirms_D062",
    "analysis_manifest_complete_current_script_set",
    "analysis_manifest_all_hashes_and_sizes_match",
    "manifest_includes_05c_06_07_08_08a_09_10_14",
    "MIMIC_native_OASIS_gate_PASS",
    "MIMIC_severity_predictor_gate_PASS",
    "eICU_APACHE_predictor_gate_PASS",
    "parameter_gate_PASS",
    "selection_gate_PASS",
    "outcome_gate_formally_unblinded_PASS",
    "main_model_gate_PASS",
    "outcome_access_receipt_hash_chain_matches",
    "canonical_model_frame_hashes_match_parameter_and_10_gates",
    "prediction_time_artifact_hashes_match_severity_and_09_gates",
    "native_predictor_artifact_hashes_match_native_gates",
    "outcome_artifact_hashes_match_09_and_10_gates"
  ),
  pass = TRUE
)

model_contract_qc <- data.table(
  check = c(
    "model_order_exactly_N0_N1_N2_N3",
    "N0_unchanged_probability_and_never_refit",
    "N1_offset_native_lp_plus_intercept",
    "N2_intercept_plus_native_lp",
    "N3_intercept_plus_native_lp_plus_linear_smp_per_5",
    "same_sample_for_all_four_models_within_scenario",
    "only_nested_LRT_is_N3_minus_N2",
    "N3_smp_OR_reported_per_5_J_per_min",
    "unclipped_native_probability_distribution_reported",
    "MIMIC_patient_bootstrap_1000_seed_locked",
    "eICU_hospital_cluster_bootstrap_2000_seed_locked",
    "all_bootstrap_replicates_exactly_accounted",
    "bootstrap_success_threshold_at_least_0.95",
    "N0_CI_fixed_prediction_no_optimism",
    "N1_N3_refit_train_vs_original_test_optimism",
    "all_local_models_labeled_not_external_validation",
    "MIMIC_outcome_free_feasibility_reproduces_1665_1538",
    "eICU_outcome_free_feasibility_reproduces_211",
    "eICU_modeled_APACHE_version_all_IVa"
  ),
  pass = c(
    identical(native_model_specification$model_id, NATIVE_MODEL_IDS),
    all(bootstrap_metric_private[model_id == "N0", refit_in_resample == FALSE]),
    identical(native_model_specification[model_id == "N1", formula_contract],
      "offset(native_lp)+intercept"),
    identical(native_model_specification[model_id == "N2", formula_contract],
      "intercept+native_lp"),
    identical(native_model_specification[model_id == "N3", formula_contract],
      "intercept+native_lp+smp_per_5"),
    all(population_public$same_sample_for_N0_N1_N2_N3),
    uniqueN(likelihood_ratio_results$comparison_id) == 1L &&
      unique(likelihood_ratio_results$comparison_id) == ONLY_LRT_COMPARISON,
    nrow(smp_or_public) == length(scenario_frames) &&
      all(smp_or_public$effect_scale == "per_5_J_per_min"),
    nrow(native_distribution_public) == length(scenario_frames) &&
      all(!native_distribution_public$probability_clipped_for_distribution) &&
      all(native_distribution_public$
        clipping_used_only_to_construct_native_logit),
    all(scenario_resampling[grepl("^OASIS", scenario),
      requested_repetitions == 1000L & seed == MIMIC_BOOTSTRAP_SEED]),
    all(scenario_resampling[grepl("^APACHE", scenario),
      requested_repetitions == 2000L & seed == EICU_BOOTSTRAP_SEED]),
    all(bootstrap_summary_public$replicate_accounting_pass),
    all(bootstrap_summary_public$success_rate >=
      BOOTSTRAP_SUCCESS_THRESHOLD),
    all(bootstrap_metric_private[
      model_id == "N0" & success == TRUE,
      is.na(optimism) & is.na(test_original_estimate)
    ]),
    all(bootstrap_metric_private[
      model_id %in% c("N1", "N2", "N3") & success == TRUE,
      is.finite(optimism) & is.finite(test_original_estimate)
    ]),
    all(!native_model_specification$external_validation_claim) &&
      all(grepl("not_external_validation", performance_public$interpretation)),
    sum(mimic_outcome_free_feasible) == 1665L &&
      sum(mimic_outcome_free_all10) == 1538L,
    sum(eicu_outcome_free_feasible) == 211L,
    all(eicu_primary$apache_version == "IVa")
  )
)
if (any(!input_gate_qc$pass) || any(!model_contract_qc$pass) ||
    any(!bootstrap_summary_public$threshold_pass)) {
  stop("A final D062 input/model/bootstrap contract QC check failed.")
}

# ---------------------------------------------------------------------------
# Private row-level bundle, aggregate-only public outputs, identifier guard,
# and last-write PASS gate. No patient/stay/hospital identifier or center row is
# written to aggregate/QC/gate products.
# ---------------------------------------------------------------------------

private_bundle <- list(
  artifact_version = "native_benchmark_private_v1",
  config_version = LOCKED$version,
  decision_id = DECISION_ID,
  private_row_level = TRUE,
  center_specific_rows_private_only = TRUE,
  interpretation = paste(
    "N0 is unchanged native probability; N1-N3 are local updating/extensions",
    "and never external validation"
  ),
  script_sha256 = sha256_file(script_path),
  checkpoint_sha256 = sha256_file(checkpoint_path),
  analysis_manifest_sha256 = sha256_file(analysis_manifest_path),
  parameter_gate_sha256 = sha256_file(gate_paths$parameter),
  selection_gate_sha256 = sha256_file(gate_paths$selection),
  mimic_oasis_gate_sha256 = sha256_file(gate_paths$mimic_oasis),
  mimic_severity_gate_sha256 = sha256_file(gate_paths$mimic_severity),
  eicu_severity_gate_sha256 = sha256_file(gate_paths$eicu_severity),
  outcome_gate_sha256 = sha256_file(gate_paths$outcomes),
  main_model_gate_sha256 = sha256_file(gate_paths$main_models),
  input_sha256 = c(
    mimic_model_frame = sha256_file(model_frame_paths$mimic),
    eicu_model_frame = sha256_file(model_frame_paths$eicu),
    mimic_time_predictor = sha256_file(input_paths$mimic_time),
    mimic_oasis = sha256_file(input_paths$mimic_oasis),
    mimic_outcome = sha256_file(input_paths$mimic_outcome),
    eicu_time_predictor = sha256_file(input_paths$eicu_time),
    eicu_apache = sha256_file(input_paths$eicu_apache),
    eicu_outcome = sha256_file(input_paths$eicu_outcome)
  ),
  model_specification = native_model_specification,
  scenario_frames_with_identifiers = scenario_frames,
  point_fit_objects = lapply(point_results, `[[`, "fits"),
  private_predictions_with_identifiers = private_predictions,
  point_performance = point_performance,
  point_coefficients = point_coefficients,
  point_paired_differences = point_differences,
  likelihood_ratio_results = likelihood_ratio_results,
  bootstrap_metric_rows = bootstrap_metric_private,
  bootstrap_paired_difference_rows = bootstrap_difference_private
)

dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

atomic_save_rds_new(private_bundle, private_bundle_path)
atomic_fwrite_new(specification_public, aggregate_paths[["specification"]])
atomic_fwrite_new(population_public, aggregate_paths[["population"]])
atomic_fwrite_new(
  native_distribution_public, aggregate_paths[["native_distribution"]]
)
atomic_fwrite_new(performance_public, aggregate_paths[["performance"]])
atomic_fwrite_new(point_coefficients, aggregate_paths[["coefficients"]])
atomic_fwrite_new(smp_or_public, aggregate_paths[["smp_or"]])
atomic_fwrite_new(
  paired_difference_public, aggregate_paths[["paired_difference"]]
)
atomic_fwrite_new(
  likelihood_ratio_results, aggregate_paths[["likelihood_ratio"]]
)
atomic_fwrite_new(bootstrap_summary_public, aggregate_paths[["bootstrap"]])
atomic_fwrite_new(input_gate_qc, qc_paths[["input_gate"]])
atomic_fwrite_new(join_timing_qc, qc_paths[["join_timing"]])
atomic_fwrite_new(model_contract_qc, qc_paths[["model_contract"]])
atomic_fwrite_new(bootstrap_summary_public, qc_paths[["bootstrap"]])

summary_lines <- c(
  "# Timing-compatible native severity benchmark QC",
  "",
  paste0("- Configuration: ", LOCKED$version, "; decision: ", DECISION_ID, "."),
  paste0(
    "- MIMIC outcome-free timing/native feasibility reproduced exactly: ",
    EXPECTED_MIMIC_OUTCOME_FREE_NATIVE_N, "; all-10 components: ",
    EXPECTED_MIMIC_OUTCOME_FREE_ALL10_N, "."
  ),
  paste0(
    "- eICU outcome-free timing/APACHE-IVa-risk feasibility reproduced exactly: ",
    EXPECTED_EICU_OUTCOME_FREE_NATIVE_N, "."
  ),
  paste(
    "- The original native-probability distribution is reported without",
    "clipping; clipping is used only to construct native_lp."
  ),
  paste0(
    "- OASIS unchanged native probability: plogis(", OASIS_NATIVE_INTERCEPT,
    " + ", OASIS_NATIVE_SLOPE, " * OASIS)."
  ),
  "- N0 is the unchanged original native probability and is never refit or optimism-corrected.",
  "- N1 is offset(native_lp)+intercept local updating.",
  "- N2 is intercept+native_lp local updating.",
  "- N3 is intercept+native_lp+linear sMP/5 local extension; its sMP odds ratio is per 5 J/min.",
  "- The only nested likelihood-ratio comparison is N3 versus N2 on the same sample.",
  paste0(
    "- MIMIC uses ", MIMIC_BOOTSTRAP_REPS,
    " patient-cluster bootstrap repetitions with seed ",
    MIMIC_BOOTSTRAP_SEED, "."
  ),
  paste0(
    "- eICU uses ", EICU_CLUSTER_BOOTSTRAP_REPS,
    " hospital-cluster bootstrap repetitions with seed ",
    EICU_BOOTSTRAP_SEED, "."
  ),
  paste0(
    "- Every applicable bootstrap success rate passed the locked >=",
    100 * BOOTSTRAP_SUCCESS_THRESHOLD, "% threshold."
  ),
  "- N1-N3 optimism correction refits in each bootstrap training sample and tests on the unchanged original sample.",
  "- N1-N3 are local updating/extensions and never external validation of OASIS, APACHE IVa, or the MIMIC model.",
  "- Row identifiers, resampling-cluster values, and center-specific detail are private only.",
  "",
  "BUILD_COMPLETE"
)
atomic_write_lines_new(summary_lines, qc_paths[["summary"]])

aggregate_manifest <- data.table(
  output_name = names(aggregate_paths),
  path = vapply(aggregate_paths, project_relative, character(1L)),
  sha256 = vapply(aggregate_paths, sha256_file, character(1L)),
  row_level_identifier_columns = FALSE,
  center_specific_rows = FALSE
)
atomic_fwrite_new(aggregate_manifest, aggregate_manifest_path)

public_csv_without_guard <- c(
  aggregate_paths,
  qc_paths[names(qc_paths) %in% c(
    "input_gate", "join_timing", "model_contract", "bootstrap"
  )],
  aggregate_manifest_path
)
forbidden_identifier_headers <- c(
  "analysis_id", "resample_cluster", "patient_cluster_id",
  "oasis_patient_cluster_id", "outcome_patient_cluster_id",
  "hospital_cluster_id", "outcome_hospital_cluster_id",
  "stay_id", "subject_id", "hadm_id", "patientunitstayid",
  "patienthealthsystemstayid", "person_key", "uniquepid", "hospitalid",
  "hospital_id", "center_id", "source_hospital_id", "omitted_hospital_id"
)
headers_without_guard <- unique(tolower(unlist(lapply(
  public_csv_without_guard, function(path) {
    names(fread(path, nrows = 0L, showProgress = FALSE))
  }
))))
identifier_header_absent <- !any(
  headers_without_guard %in% forbidden_identifier_headers
)
public_guard_qc <- data.table(
  check = c(
    "no_row_level_identifier_header_in_public_CSV",
    "no_center_specific_rows_in_aggregate_manifest",
    "row_level_predictions_and_resampling_clusters_private_only",
    "completion_gate_contains_hashes_and_contract_only"
  ),
  pass = c(
    identifier_header_absent,
    all(aggregate_manifest$center_specific_rows == FALSE),
    isTRUE(private_bundle$private_row_level) &&
      isTRUE(private_bundle$center_specific_rows_private_only),
    TRUE
  ),
  detail = c(
    "aggregate/QC CSV headers scanned after publication",
    "no center-level estimate or identifier row is public",
    "identifiers and resampling cluster values exist only in the private RDS",
    "field/value PASS gate contains no row-level record"
  )
)
if (any(!public_guard_qc$pass)) {
  stop("A public identifier/center-detail guard failed.")
}
atomic_fwrite_new(public_guard_qc, qc_paths[["public_guard"]])

public_csv_paths <- c(
  aggregate_paths, qc_paths[names(qc_paths) != "summary"],
  aggregate_manifest_path
)
public_headers <- unique(tolower(unlist(lapply(public_csv_paths, function(path) {
  names(fread(path, nrows = 0L, showProgress = FALSE))
}))))
if (any(public_headers %in% forbidden_identifier_headers)) {
  stop("A row/patient/stay/hospital identifier header entered a public CSV.")
}
if (!identical(
  tail(readLines(qc_paths[["summary"]], warn = FALSE), 1L), "BUILD_COMPLETE"
)) {
  stop("D062 QC summary sentinel is missing.")
}

non_gate_outputs <- c(
  private_bundle_path, aggregate_paths, qc_paths, aggregate_manifest_path
)
if (!all(file.exists(non_gate_outputs))) {
  stop("A required D062 private/aggregate/QC output is missing.")
}

completion <- data.table(
  field = c(
    "status", "config_version", "decision_id", "completed_at",
    "script_sha256", "checkpoint_sha256",
    "analysis_script_manifest_sha256", "parameter_freeze_gate_sha256",
    "selection_weights_gate_sha256", "mimic_native_oasis_gate_sha256",
    "mimic_severity_gate_sha256", "eicu_severity_gate_sha256",
    "outcome_gate_sha256", "main_model_gate_sha256",
    "outcome_access_receipt_sha256",
    "mimic_model_frame_rds_sha256", "eicu_model_frame_rds_sha256",
    "mimic_time_predictor_rds_sha256", "mimic_oasis_rds_sha256",
    "mimic_outcome_rds_sha256", "eicu_time_predictor_rds_sha256",
    "eicu_apache_rds_sha256", "eicu_outcome_rds_sha256",
    "private_bundle_sha256", "aggregate_manifest_sha256",
    "native_distribution_sha256",
    "oasis_native_intercept", "oasis_native_slope",
    "mimic_minimum_index_after_intime_hours",
    "eicu_minimum_index_offset_minutes",
    "mimic_outcome_free_native_expected_n",
    "mimic_outcome_free_all10_expected_n",
    "eicu_outcome_free_native_expected_n",
    "all_outcome_free_feasibility_counts_reproduced",
    "unclipped_native_probability_distribution_reported",
    "native_scenarios_n", "model_order",
    "N0_contract", "N1_contract", "N2_contract", "N3_contract",
    "N0_fixed_prediction_no_refit", "N0_optimism_correction_applied",
    "N1_N3_refit_bootstrap_train_original_test",
    "mimic_patient_bootstrap_repetitions", "mimic_bootstrap_seed",
    "eicu_hospital_cluster_bootstrap_repetitions", "eicu_bootstrap_seed",
    "bootstrap_success_threshold", "all_bootstrap_success_rates_pass",
    "all_bootstrap_replicates_exactly_accounted",
    "likelihood_ratio_comparison_type_count",
    "only_likelihood_ratio_comparison", "likelihood_ratio_tests_n",
    "smp_effect_scale", "all_local_models_not_external_validation",
    "row_identifiers_private_only", "center_specific_detail_private_only",
    "public_identifier_guard_pass", "all_input_gate_checks_pass",
    "all_join_timing_checks_pass", "all_model_contract_checks_pass",
    "all_required_outputs_present", "summary_sentinel"
  ),
  value = c(
    "PASS", LOCKED$version, DECISION_ID,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    sha256_file(script_path), sha256_file(checkpoint_path),
    sha256_file(analysis_manifest_path), sha256_file(gate_paths$parameter),
    sha256_file(gate_paths$selection), sha256_file(gate_paths$mimic_oasis),
    sha256_file(gate_paths$mimic_severity),
    sha256_file(gate_paths$eicu_severity), sha256_file(gate_paths$outcomes),
    sha256_file(gate_paths$main_models),
    sha256_file(gate_paths$access_receipt),
    sha256_file(model_frame_paths$mimic), sha256_file(model_frame_paths$eicu),
    sha256_file(input_paths$mimic_time), sha256_file(input_paths$mimic_oasis),
    sha256_file(input_paths$mimic_outcome), sha256_file(input_paths$eicu_time),
    sha256_file(input_paths$eicu_apache), sha256_file(input_paths$eicu_outcome),
    sha256_file(private_bundle_path), sha256_file(aggregate_manifest_path),
    sha256_file(aggregate_paths[["native_distribution"]]),
    OASIS_NATIVE_INTERCEPT, OASIS_NATIVE_SLOPE, 24L,
    EICU_NATIVE_WINDOW_MINUTES, EXPECTED_MIMIC_OUTCOME_FREE_NATIVE_N,
    EXPECTED_MIMIC_OUTCOME_FREE_ALL10_N,
    EXPECTED_EICU_OUTCOME_FREE_NATIVE_N,
    all(population_public$outcome_free_timing_native_feasible_n ==
      population_public$locked_outcome_free_expected_n),
    nrow(native_distribution_public) == length(scenario_frames) &&
      all(!native_distribution_public$probability_clipped_for_distribution),
    length(scenario_frames), paste(NATIVE_MODEL_IDS, collapse = ";"),
    "unchanged_original_native_probability",
    "offset(native_lp)+intercept_local_updating",
    "intercept+native_lp_local_updating",
    "intercept+native_lp+smp_per_5_local_extension",
    TRUE, FALSE, TRUE, MIMIC_BOOTSTRAP_REPS, MIMIC_BOOTSTRAP_SEED,
    EICU_CLUSTER_BOOTSTRAP_REPS, EICU_BOOTSTRAP_SEED,
    BOOTSTRAP_SUCCESS_THRESHOLD,
    all(bootstrap_summary_public$threshold_pass),
    all(bootstrap_summary_public$replicate_accounting_pass),
    uniqueN(likelihood_ratio_results$comparison_id),
    ONLY_LRT_COMPARISON, nrow(likelihood_ratio_results),
    "linear_per_5_J_per_min", TRUE, TRUE, TRUE,
    all(public_guard_qc$pass), all(input_gate_qc$pass),
    all(join_timing_qc$pass), all(model_contract_qc$pass),
    all(file.exists(non_gate_outputs)), "BUILD_COMPLETE"
  )
)
if (anyDuplicated(completion$field) || anyNA(completion$value) ||
    nrow(completion) != length(completion$value)) {
  stop("Malformed D062 native-benchmark completion gate.")
}
atomic_fwrite_new(completion, completion_gate)

message("Locked D062 timing-compatible native benchmark analysis complete.")
message("  gate: ", completion_gate)
message("  private identifiers/center detail: ", private_bundle_path)
