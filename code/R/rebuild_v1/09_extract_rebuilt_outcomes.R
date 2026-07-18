#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: formally gated outcome extraction
#
# IMPORTANT GOVERNANCE BOUNDARY
# -----------------------------
# The first successful passage through the authorization checks below, followed
# by creation of outcome_access_receipt_v1.csv, is the formal unblinding event.
# This script must not be run for syntax checking or exploratory inspection.
# Use parse(file = ...) for static syntax checks. This script neither hashes nor
# reads an analytic outcome source until every frozen upstream artifact and the
# explicit authorization checkpoint have been verified. Per D059, prior
# header-only schema checks, gzip integrity tests, or opaque whole-file checksum
# comparisons are reproducibility metadata and do not expose row-level outcomes.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/09_extract_rebuilt_outcomes.R",
    mustWork = TRUE
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
  z <- fread(path, showProgress = FALSE)
  if (!identical(names(z), c("field", "value")) ||
      anyDuplicated(z$field) || anyNA(z$field) || any(!nzchar(z$field))) {
    stop("Malformed field/value ", label, ": ", path)
  }
  setNames(as.character(z$value), z$field)
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

read_completion_gate <- function(path, label) {
  z <- fread(path, showProgress = FALSE)
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

resolve_project_path <- function(path, label, require_relative = FALSE) {
  if (is.na(path) || !nzchar(path)) stop(label, " is empty.")
  is_absolute <- grepl("^/", path)
  if (require_relative && is_absolute) {
    stop(label, " must be project-relative.")
  }
  if (!is_absolute && grepl("(^|/)\\.\\.(/|$)", path)) {
    stop(label, " contains path traversal.")
  }
  candidate <- if (is_absolute) path else file.path(project_from_script, path)
  if (!file.exists(candidate)) stop("Missing ", label, ": ", candidate)
  resolved <- normalizePath(candidate, mustWork = TRUE)
  prefix <- paste0(project_from_script, .Platform$file.sep)
  if (!startsWith(resolved, prefix)) stop(label, " escapes the project root.")
  resolved
}

# ---------------------------------------------------------------------------
# Authorization checkpoint: this is intentionally the first project artifact
# read. If it is absent, malformed, or unauthorized, no config, private RDS,
# outcome table, or outcome-source hash is opened.
# ---------------------------------------------------------------------------

checkpoint_path <- file.path(
  project_from_script, "analysis_rebuild_v1", "qc", "unblinding",
  "outcome_unblinding_checkpoint_v1.csv"
)
if (!file.exists(checkpoint_path)) {
  stop(
    "Outcome access is not authorized: missing checkpoint ", checkpoint_path,
    ". No outcome table was opened."
  )
}
checkpoint <- read_field_value_gate(checkpoint_path, "authorization checkpoint")
require_map_value(checkpoint, "status", "AUTHORIZED", "authorization checkpoint")
require_map_value(
  checkpoint, "config_version", expected_config_version,
  "authorization checkpoint"
)
for (f in c("authorized_at", "authorized_by", "authorization_basis")) {
  require_map_value(checkpoint, f, label = "authorization checkpoint")
}

config_path <- file.path(script_dir, "00_config.R")
require_map_value(
  checkpoint, "config_script_sha256", sha256_file(config_path),
  "authorization checkpoint"
)
source(config_path)
if (!identical(LOCKED$version, expected_config_version) ||
    !identical(normalizePath(PROJECT_ROOT, mustWork = TRUE), project_from_script)) {
  stop("Loaded configuration does not match the authorized project/config version.")
}

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
phase0_manifest_path <- file.path(QC_ROOT, "phase0_lock_manifest.csv")

parameter_script <- file.path(script_dir, "07_freeze_predictor_parameters.R")
selection_script <- file.path(script_dir, "07b_build_selection_weights.R")
mimic_severity_script <- file.path(script_dir, "05_build_mimic_severity_core.R")
eicu_severity_script <- file.path(script_dir, "06_build_eicu_severity_core.R")
model_utils_script <- file.path(script_dir, "08_model_utils.R")
model_analysis_utils_script <- file.path(script_dir, "08a_locked_analysis_utils.R")
model_analysis_script <- file.path(script_dir, "10_fit_locked_models.R")

sap_path <- file.path(PROJECT_ROOT, "docs", "rebuild_v1", "SAP_v1_0.md")
dictionary_path <- file.path(
  PROJECT_ROOT, "docs", "rebuild_v1", "data_dictionary_v1.md"
)
decision_log_path <- file.path(
  PROJECT_ROOT, "docs", "rebuild_v1", "analysis_decision_log.md"
)
terminology_path <- file.path(
  PROJECT_ROOT, "docs", "rebuild_v1", "terminology_ledger.md"
)

locked_checkpoint_files <- c(
  outcome_extraction_script_sha256 = script_path,
  parameter_freeze_script_sha256 = parameter_script,
  selection_weights_script_sha256 = selection_script,
  mimic_severity_script_sha256 = mimic_severity_script,
  eicu_severity_script_sha256 = eicu_severity_script,
  model_utils_script_sha256 = model_utils_script,
  model_analysis_utils_script_sha256 = model_analysis_utils_script,
  model_analysis_script_sha256 = model_analysis_script,
  sap_sha256 = sap_path,
  data_dictionary_sha256 = dictionary_path,
  analysis_decision_log_sha256 = decision_log_path,
  terminology_ledger_sha256 = terminology_path,
  phase0_lock_manifest_sha256 = phase0_manifest_path
)
missing_locked <- locked_checkpoint_files[!file.exists(locked_checkpoint_files)]
if (length(missing_locked)) {
  stop(
    "Authorized frozen file(s) missing before outcome access: ",
    paste(missing_locked, collapse = ", "), ". No outcome table was opened."
  )
}
for (field in names(locked_checkpoint_files)) {
  require_map_value(
    checkpoint, field, sha256_file(locked_checkpoint_files[[field]]),
    "authorization checkpoint"
  )
}

# The phase-0 manifest must itself describe the exact current design documents.
phase0_manifest <- fread(phase0_manifest_path, showProgress = FALSE)
if (!all(c("file", "sha256", "config_version") %in% names(phase0_manifest)) ||
    anyDuplicated(phase0_manifest$file)) {
  stop("Malformed Phase-0 design-lock manifest.")
}
phase0_expected <- c(config_path, sap_path, terminology_path, dictionary_path,
  decision_log_path)
phase0_file_norm <- vapply(
  phase0_manifest$file, normalizePath, character(1), mustWork = TRUE
)
for (p in phase0_expected) {
  idx <- match(normalizePath(p, mustWork = TRUE), phase0_file_norm)
  if (is.na(idx) || !identical(
    as.character(phase0_manifest$sha256[[idx]]), sha256_file(p)
  ) || !identical(
    as.character(phase0_manifest$config_version[[idx]]), LOCKED$version
  )) {
    stop("Phase-0 manifest does not lock the current file: ", p)
  }
}

# A scalable manifest freezes every analysis script before first outcome read.
analysis_manifest_relative <- require_map_value(
  checkpoint, "analysis_script_manifest_path",
  label = "authorization checkpoint"
)
analysis_manifest_path <- resolve_project_path(
  analysis_manifest_relative, "analysis script manifest"
)
require_map_value(
  checkpoint, "analysis_script_manifest_sha256",
  sha256_file(analysis_manifest_path), "authorization checkpoint"
)
analysis_manifest <- fread(analysis_manifest_path, showProgress = FALSE)
if (!all(c("file", "sha256") %in% names(analysis_manifest)) ||
    !nrow(analysis_manifest) || anyDuplicated(analysis_manifest$file) ||
    anyNA(analysis_manifest$file) || anyNA(analysis_manifest$sha256)) {
  stop("Malformed analysis script manifest; expected unique file/sha256 rows.")
}
analysis_paths <- unname(vapply(
  as.character(analysis_manifest$file), resolve_project_path,
  character(1), label = "manifested analysis script"
))
if (anyDuplicated(analysis_paths)) stop("Analysis manifest resolves duplicate paths.")
analysis_script_prefix <- paste0(
  normalizePath(script_dir, mustWork = TRUE), .Platform$file.sep
)
if (any(!startsWith(analysis_paths, analysis_script_prefix)) ||
    any(!grepl("\\.(R|r|py)$", analysis_paths))) {
  stop(
    "Analysis manifest may contain only R/Python scripts under ", script_dir,
    "; no manifested file was hashed."
  )
}
manifest_hashes <- tolower(as.character(analysis_manifest$sha256))
if (any(!grepl("^[0-9a-f]{64}$", manifest_hashes))) {
  stop("Analysis manifest contains an invalid SHA256.")
}
current_manifest_hashes <- unname(vapply(
  analysis_paths, sha256_file, character(1)
))
if (!identical(manifest_hashes, current_manifest_hashes)) {
  bad <- analysis_paths[manifest_hashes != current_manifest_hashes]
  stop("Current analysis script differs from frozen manifest: ", paste(bad, collapse = ", "))
}
must_manifest <- normalizePath(
  c(
    script_path, model_analysis_script, model_utils_script,
    model_analysis_utils_script
  ), mustWork = TRUE
)
if (length(setdiff(must_manifest, analysis_paths))) {
  stop("Analysis script manifest must include 08, 08a, 09, and 10 locked scripts.")
}

gate_paths <- c(
  parameter_freeze_gate_sha256 = parameter_gate_path,
  selection_weights_gate_sha256 = selection_gate_path,
  mimic_severity_gate_sha256 = mimic_severity_gate_path,
  eicu_severity_gate_sha256 = eicu_severity_gate_path
)
if (any(!file.exists(gate_paths))) {
  stop(
    "Required upstream completion gate(s) missing: ",
    paste(gate_paths[!file.exists(gate_paths)], collapse = ", "),
    ". No outcome table was opened."
  )
}
for (field in names(gate_paths)) {
  require_map_value(
    checkpoint, field, sha256_file(gate_paths[[field]]),
    "authorization checkpoint"
  )
}

parameter_gate <- read_completion_gate(parameter_gate_path, "parameter-freeze gate")
selection_gate <- read_completion_gate(selection_gate_path, "selection-weight gate")
mimic_gate <- read_completion_gate(mimic_severity_gate_path, "MIMIC severity gate")
eicu_gate <- read_completion_gate(eicu_severity_gate_path, "eICU severity gate")
upstream_gates <- list(
  "parameter-freeze gate" = parameter_gate,
  "selection-weight gate" = selection_gate,
  "MIMIC severity gate" = mimic_gate,
  "eICU severity gate" = eicu_gate
)
for (gate_label in names(upstream_gates)) {
  g <- upstream_gates[[gate_label]]
  require_map_value(g, "status", "PASS", gate_label)
}
require_map_value(
  parameter_gate, "locked_config_version", LOCKED$version,
  "parameter-freeze gate"
)
require_map_value(
  selection_gate, "config_version", LOCKED$version, "selection-weight gate"
)
require_map_value(
  mimic_gate, "config_version", LOCKED$version, "MIMIC severity gate"
)
require_map_value(
  eicu_gate, "config_version", LOCKED$version, "eICU severity gate"
)
require_map_value(parameter_gate, "all_tests_pass", "TRUE", "parameter-freeze gate")
require_map_value(
  parameter_gate, "outcome_leakage_guard_pass", "TRUE", "parameter-freeze gate"
)
require_map_value(
  parameter_gate, "all_required_qc_present", "TRUE", "parameter-freeze gate"
)
require_map_value(
  selection_gate, "all_leakage_checks_pass", "TRUE", "selection-weight gate"
)
require_map_value(
  selection_gate, "all_required_qc_present", "TRUE", "selection-weight gate"
)
require_map_value(
  mimic_gate, "all_invariants_pass", "TRUE", "MIMIC severity gate"
)
require_map_value(
  mimic_gate, "outcome_leakage_guard_pass", "TRUE", "MIMIC severity gate"
)

gate_script_checks <- list(
  list(parameter_gate, parameter_script, "parameter-freeze gate"),
  list(selection_gate, selection_script, "selection-weight gate"),
  list(mimic_gate, mimic_severity_script, "MIMIC severity gate"),
  list(eicu_gate, eicu_severity_script, "eICU severity gate")
)
for (z in gate_script_checks) {
  require_map_value(z[[1L]], "script_sha256", sha256_file(z[[2L]]), z[[3L]])
}

mimic_prediction_rds <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_paired_exposure_with_severity_core_v1.rds"
)
eicu_prediction_rds <- file.path(
  PRIVATE_ROOT, "eicu", "eicu_paired_exposure_with_severity_core_v1.rds"
)
if (any(!file.exists(c(mimic_prediction_rds, eicu_prediction_rds)))) {
  stop("Final severity prediction artifact missing. No outcome table was opened.")
}
require_map_value(
  mimic_gate, "prediction_hsc_rds_sha256", sha256_file(mimic_prediction_rds),
  "MIMIC severity gate"
)
require_map_value(
  eicu_gate, "prediction_hsc_rds_sha256", sha256_file(eicu_prediction_rds),
  "eICU severity gate"
)

# Deep verification of the frozen parameter bundle and both model frames.
parameter_artifact_fields <- list(
  c("parameter_rds_path", "parameter_rds_sha256"),
  c("mimic_model_frame_rds_path", "mimic_model_frame_rds_sha256"),
  c("eicu_model_frame_rds_path", "eicu_model_frame_rds_sha256")
)
for (pair in parameter_artifact_fields) {
  p <- resolve_project_path(
    require_map_value(parameter_gate, pair[[1L]], label = "parameter-freeze gate"),
    pair[[1L]], require_relative = TRUE
  )
  require_map_value(
    parameter_gate, pair[[2L]], sha256_file(p), "parameter-freeze gate"
  )
}
require_map_value(
  parameter_gate, "mimic_severity_gate_sha256",
  sha256_file(mimic_severity_gate_path), "parameter-freeze gate"
)
require_map_value(
  parameter_gate, "eicu_severity_gate_sha256",
  sha256_file(eicu_severity_gate_path), "parameter-freeze gate"
)
require_map_value(
  parameter_gate, "mimic_input_rds_sha256", sha256_file(mimic_prediction_rds),
  "parameter-freeze gate"
)
require_map_value(
  parameter_gate, "eicu_input_rds_sha256", sha256_file(eicu_prediction_rds),
  "parameter-freeze gate"
)
require_map_value(
  parameter_gate, "model_utils_sha256", sha256_file(model_utils_script),
  "parameter-freeze gate"
)

selection_artifacts <- c(
  mimic_output_rds_sha256 = file.path(
    PRIVATE_ROOT, "selection_weights", "mimic_tuple_observation_weights_v1.rds"
  ),
  eicu_output_rds_sha256 = file.path(
    PRIVATE_ROOT, "selection_weights", "eicu_tuple_observation_weights_v1.rds"
  ),
  eicu_support_output_rds_sha256 = file.path(
    PRIVATE_ROOT, "selection_weights",
    "eicu_tuple_observation_weights_support_hospitals_v1.rds"
  )
)
if (any(!file.exists(selection_artifacts))) {
  stop("Selection-weight artifact missing. No outcome table was opened.")
}
for (field in names(selection_artifacts)) {
  require_map_value(
    selection_gate, field, sha256_file(selection_artifacts[[field]]),
    "selection-weight gate"
  )
}

# Only now may the outcome-free final prediction artifacts be read. They are
# projected to an explicit ID/time allow-list before any outcome source opens.
mimic_prediction_source <- as.data.table(readRDS(mimic_prediction_rds))
eicu_prediction_source <- as.data.table(readRDS(eicu_prediction_rds))
required_mimic_prediction <- c(
  "stay_id", "subject_id", "hadm_id", "prediction_time"
)
required_eicu_prediction <- c(
  "patientunitstayid", "patienthealthsystemstayid", "person_key",
  "hospitalid", "prediction_time"
)
if (length(setdiff(required_mimic_prediction, names(mimic_prediction_source))) ||
    length(setdiff(required_eicu_prediction, names(eicu_prediction_source)))) {
  stop("Final severity artifact lacks required ID/prediction-time fields.")
}
forbidden_outcome_name <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
if (any(grepl(forbidden_outcome_name, names(mimic_prediction_source),
  ignore.case = TRUE)) || any(grepl(
  forbidden_outcome_name, names(eicu_prediction_source), ignore.case = TRUE
))) {
  stop("Outcome-like field found in an outcome-free severity artifact.")
}
meta_mimic <- attr(mimic_prediction_source, "rebuild_metadata")
meta_eicu <- attr(eicu_prediction_source, "rebuild_metadata")
if (!isTRUE(meta_mimic$outcome_blind) || !isTRUE(meta_eicu$outcome_blind)) {
  stop("Severity artifact metadata does not attest outcome blindness.")
}
mimic_prediction <- mimic_prediction_source[, ..required_mimic_prediction]
eicu_prediction <- eicu_prediction_source[, ..required_eicu_prediction]
rm(mimic_prediction_source, eicu_prediction_source)
if (!nrow(mimic_prediction) || !nrow(eicu_prediction) ||
    anyDuplicated(mimic_prediction$stay_id) ||
    anyDuplicated(mimic_prediction$subject_id) ||
    anyDuplicated(eicu_prediction$patientunitstayid) ||
    anyDuplicated(eicu_prediction$person_key) ||
    anyNA(mimic_prediction$stay_id) || anyNA(mimic_prediction$subject_id) ||
    anyNA(mimic_prediction$hadm_id) ||
    anyNA(eicu_prediction$patientunitstayid) ||
    anyNA(eicu_prediction$patienthealthsystemstayid) ||
    anyNA(eicu_prediction$person_key) ||
    anyNA(eicu_prediction$hospitalid)) {
  stop("Prediction artifact ID/one-stay-per-patient invariant failed.")
}
if ("prediction_hsc_n" %in% names(mimic_gate) &&
    nrow(mimic_prediction) != as.integer(mimic_gate[["prediction_hsc_n"]])) {
  stop("MIMIC prediction row count disagrees with severity gate.")
}

private_out <- file.path(PRIVATE_ROOT, "outcomes")
qc_out <- file.path(QC_ROOT, "outcomes")
mimic_output <- file.path(private_out, "mimic_rebuilt_outcomes_v1.rds")
eicu_output <- file.path(private_out, "eicu_rebuilt_outcomes_v1.rds")
summary_output <- file.path(qc_out, "rebuilt_outcome_summary_v1.csv")
reason_output <- file.path(qc_out, "rebuilt_outcome_ineligibility_reasons_v1.csv")
timing_output <- file.path(qc_out, "rebuilt_outcome_timing_audit_v1.csv")
completion_gate <- file.path(qc_out, "phase3a_rebuilt_outcomes_complete_v1.csv")
published_outputs <- c(
  mimic_output, eicu_output, summary_output, reason_output, timing_output
)
if (file.exists(completion_gate)) {
  stop("Outcome extraction is already complete; refusing to reopen outcome tables.")
}
if (any(file.exists(published_outputs))) {
  stop(
    "Published outcome artifact exists without a completion gate. Resolve the ",
    "interrupted run explicitly; no outcome table was reopened."
  )
}
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
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
  saveRDS(object, tmp, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically publish ", path)
  invisible(path)
}

# Persist an irreversible access receipt immediately before this script hashes
# or opens any analytic outcome source. A matching receipt permits recovery after an interrupted
# unblinded run; it never authorizes changed code, checkpoint, or configuration.
unblinding_dir <- dirname(checkpoint_path)
access_receipt_path <- file.path(
  unblinding_dir, "outcome_access_receipt_v1.csv"
)
checkpoint_sha256 <- sha256_file(checkpoint_path)
script_sha256 <- sha256_file(script_path)
if (file.exists(access_receipt_path)) {
  access_receipt <- read_completion_gate(access_receipt_path, "outcome-access receipt")
  require_map_value(
    access_receipt, "status", "OUTCOME_ACCESS_INITIATED", "outcome-access receipt"
  )
  require_map_value(
    access_receipt, "config_version", LOCKED$version, "outcome-access receipt"
  )
  require_map_value(
    access_receipt, "checkpoint_sha256", checkpoint_sha256,
    "outcome-access receipt"
  )
  require_map_value(
    access_receipt, "script_sha256", script_sha256, "outcome-access receipt"
  )
} else {
  access_receipt_row <- data.table(
    status = "OUTCOME_ACCESS_INITIATED",
    config_version = LOCKED$version,
    first_access_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    checkpoint_sha256 = checkpoint_sha256,
    script_sha256 = script_sha256,
    authorized_at = require_map_value(
      checkpoint, "authorized_at", label = "authorization checkpoint"
    ),
    authorized_by = require_map_value(
      checkpoint, "authorized_by", label = "authorization checkpoint"
    ),
    authorization_basis = require_map_value(
      checkpoint, "authorization_basis", label = "authorization checkpoint"
    )
  )
  atomic_fwrite_new(access_receipt_row, access_receipt_path)
  access_receipt <- read_completion_gate(
    access_receipt_path, "outcome-access receipt"
  )
}
message(
  "FORMAL OUTCOME UNBLINDING ACTIVE: authorization and all frozen hashes PASS."
)

# ---------------------------------------------------------------------------
# Outcome sources: no line above this boundary opens, hashes, or inspects one.
# ---------------------------------------------------------------------------

mimic_admissions_path <- file.path(MIMIC_ROOT, "hosp", "admissions.csv.gz")
mimic_patients_path <- file.path(MIMIC_ROOT, "hosp", "patients.csv.gz")
mimic_icustays_path <- file.path(MIMIC_ROOT, "icu", "icustays.csv.gz")
eicu_patient_path <- file.path(EICU_ROOT, "patient.csv.gz")
raw_outcome_paths <- c(
  mimic_admissions_path, mimic_patients_path, mimic_icustays_path,
  eicu_patient_path
)
if (any(!file.exists(raw_outcome_paths))) {
  stop("Authorized outcome source missing: ", paste(
    raw_outcome_paths[!file.exists(raw_outcome_paths)], collapse = ", "
  ))
}
raw_outcome_sha256 <- vapply(raw_outcome_paths, sha256_file, character(1))

read_gzip_select <- function(path, columns, label) {
  z <- fread(
    cmd = sprintf("gzip -cd %s", shQuote(path)),
    select = columns, showProgress = FALSE, fill = FALSE
  )
  missing <- setdiff(columns, names(z))
  if (length(missing)) {
    stop(label, " lacks required source columns: ", paste(missing, collapse = ", "))
  }
  z
}

admissions <- read_gzip_select(
  mimic_admissions_path,
  c("subject_id", "hadm_id", "dischtime", "deathtime", "hospital_expire_flag"),
  "MIMIC admissions"
)
patients <- read_gzip_select(
  mimic_patients_path, c("subject_id", "dod"), "MIMIC patients"
)
icustays <- read_gzip_select(
  mimic_icustays_path,
  c("subject_id", "hadm_id", "stay_id", "outtime"), "MIMIC icustays"
)
eicu_patient <- read_gzip_select(
  eicu_patient_path,
  c(
    "patientunitstayid", "patienthealthsystemstayid", "hospitalid", "uniquepid",
    "hospitaldischargeoffset", "hospitaldischargestatus",
    "unitdischargeoffset", "unitdischargestatus"
  ),
  "eICU patient"
)

if (anyDuplicated(admissions$hadm_id) || anyDuplicated(patients$subject_id) ||
    anyDuplicated(icustays$stay_id) ||
    anyDuplicated(eicu_patient$patientunitstayid)) {
  stop("Outcome source is not unique by its documented key.")
}

parse_utc_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    value <- as.POSIXct(x, tz = "UTC")
    return(list(value = value, invalid = rep(FALSE, length(value))))
  }
  raw <- trimws(as.character(x))
  missing <- is.na(x) | !nzchar(raw)
  value <- as.POSIXct(raw, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  retry <- !missing & is.na(value)
  if (any(retry)) {
    value[retry] <- as.POSIXct(
      raw[retry], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
    )
  }
  list(value = value, invalid = !missing & is.na(value))
}

parse_iso_date <- function(x) {
  raw <- trimws(as.character(x))
  missing <- is.na(x) | !nzchar(raw)
  value <- as.Date(raw, format = "%Y-%m-%d")
  list(value = value, invalid = !missing & is.na(value))
}

parse_numeric_strict <- function(x) {
  if (is.numeric(x)) {
    value <- as.numeric(x)
    return(list(
      value = value,
      invalid = !is.na(value) & !is.finite(value)
    ))
  }
  raw <- trimws(as.character(x))
  missing <- is.na(x) | !nzchar(raw)
  numeric_syntax <- grepl(
    "^[+-]?((([0-9]+)(\\.[0-9]*)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)?$",
    raw
  )
  value <- rep(NA_real_, length(raw))
  value[!missing & numeric_syntax] <- as.numeric(raw[!missing & numeric_syntax])
  invalid <- !missing & (!numeric_syntax | !is.finite(value))
  value[invalid] <- NA_real_
  list(value = value, invalid = invalid)
}

normalize_status <- function(x) {
  raw <- trimws(as.character(x))
  raw[is.na(x) | !nzchar(raw)] <- NA_character_
  normalized <- tolower(raw)
  normalized[!normalized %chin% c("alive", "expired")] <- NA_character_
  list(
    raw = raw,
    value = normalized,
    invalid = !is.na(raw) & is.na(normalized)
  )
}

add_reason <- function(reason, condition, label) {
  condition[is.na(condition)] <- FALSE
  idx <- reason == "eligible" & condition
  reason[idx] <- label
  reason
}

# ---------------------------------------------------------------------------
# MIMIC-IV endpoints.
# ---------------------------------------------------------------------------

mimic <- copy(mimic_prediction)
mimic[, `:=`(
  subject_key = as.character(subject_id),
  hadm_key = as.character(hadm_id),
  stay_key = as.character(stay_id)
)]
admissions[, `:=`(
  subject_key = as.character(subject_id), hadm_key = as.character(hadm_id)
)]
patients[, subject_key := as.character(subject_id)]
icustays[, `:=`(
  subject_key = as.character(subject_id), hadm_key = as.character(hadm_id),
  stay_key = as.character(stay_id)
)]

admissions_min <- admissions[, .(
  hadm_key, admission_subject_key = subject_key,
  hospital_discharge_time_raw = dischtime,
  death_time_raw = deathtime,
  hospital_expire_flag_raw = hospital_expire_flag,
  admission_matched = TRUE
)]
patients_min <- patients[, .(
  subject_key, dod_raw = dod, patient_matched = TRUE
)]
icustays_min <- icustays[, .(
  stay_key, icu_subject_key = subject_key, icu_hadm_key = hadm_key,
  icu_outtime_raw = outtime, icustay_matched = TRUE
)]
mimic <- merge(mimic, admissions_min, by = "hadm_key", all.x = TRUE, sort = FALSE)
mimic <- merge(mimic, patients_min, by = "subject_key", all.x = TRUE, sort = FALSE)
mimic <- merge(mimic, icustays_min, by = "stay_key", all.x = TRUE, sort = FALSE)
if (nrow(mimic) != nrow(mimic_prediction) || anyDuplicated(mimic$stay_key)) {
  stop("MIMIC outcome join changed the prediction cohort cardinality.")
}

prediction_dt <- parse_utc_datetime(mimic$prediction_time)
discharge_dt <- parse_utc_datetime(mimic$hospital_discharge_time_raw)
death_dt <- parse_utc_datetime(mimic$death_time_raw)
icu_out_dt <- parse_utc_datetime(mimic$icu_outtime_raw)
dod_date <- parse_iso_date(mimic$dod_raw)
flag_num <- parse_numeric_strict(mimic$hospital_expire_flag_raw)
mimic[, `:=`(
  prediction_time_utc = prediction_dt$value,
  prediction_time_invalid = prediction_dt$invalid,
  hospital_discharge_time = discharge_dt$value,
  hospital_discharge_time_invalid = discharge_dt$invalid,
  death_time = death_dt$value,
  death_time_invalid = death_dt$invalid,
  icu_outtime = icu_out_dt$value,
  icu_outtime_invalid = icu_out_dt$invalid,
  date_of_death = dod_date$value,
  date_of_death_invalid = dod_date$invalid,
  hospital_expire_flag = as.integer(flag_num$value),
  hospital_expire_flag_invalid = flag_num$invalid |
    (!is.na(flag_num$value) & !flag_num$value %in% c(0, 1))
)]
mimic[, `:=`(
  admission_matched = admission_matched %in% TRUE,
  patient_matched = patient_matched %in% TRUE,
  icustay_matched = icustay_matched %in% TRUE,
  admission_identifier_match = admission_matched &
    !is.na(admission_subject_key) &
    admission_subject_key == subject_key,
  icu_identifier_match = icustay_matched & !is.na(icu_subject_key) &
    !is.na(icu_hadm_key) & icu_subject_key == subject_key &
    icu_hadm_key == hadm_key
)]
mimic[, `:=`(
  death_before_prediction = !is.na(death_time) &
    !is.na(prediction_time_utc) & death_time < prediction_time_utc,
  prediction_after_hospital_discharge = !is.na(hospital_discharge_time) &
    !is.na(prediction_time_utc) & prediction_time_utc > hospital_discharge_time,
  prediction_after_icu_discharge = !is.na(icu_outtime) &
    !is.na(prediction_time_utc) & prediction_time_utc > icu_outtime,
  death_after_hospital_discharge = !is.na(death_time) &
    !is.na(hospital_discharge_time) & death_time > hospital_discharge_time,
  icu_outtime_after_hospital_discharge = !is.na(icu_outtime) &
    !is.na(hospital_discharge_time) & icu_outtime > hospital_discharge_time,
  alive_flag_with_death_time = hospital_expire_flag == 0L & !is.na(death_time),
  expired_flag_without_death_time = hospital_expire_flag == 1L & is.na(death_time)
)]

hosp_reason <- rep("eligible", nrow(mimic))
hosp_reason <- add_reason(hosp_reason, !mimic$admission_matched, "admission_not_matched")
hosp_reason <- add_reason(
  hosp_reason, mimic$admission_matched & !mimic$admission_identifier_match,
  "admission_identifier_mismatch"
)
hosp_reason <- add_reason(
  hosp_reason, is.na(mimic$prediction_time_utc) | mimic$prediction_time_invalid,
  "prediction_time_missing_or_invalid"
)
hosp_reason <- add_reason(
  hosp_reason,
  is.na(mimic$hospital_discharge_time) | mimic$hospital_discharge_time_invalid,
  "hospital_discharge_time_missing_or_invalid"
)
hosp_reason <- add_reason(
  hosp_reason, mimic$prediction_after_hospital_discharge,
  "prediction_after_hospital_discharge"
)
hosp_reason <- add_reason(
  hosp_reason, is.na(mimic$hospital_expire_flag) |
    mimic$hospital_expire_flag_invalid,
  "hospital_status_unknown_or_invalid"
)
hosp_reason <- add_reason(
  hosp_reason, mimic$alive_flag_with_death_time,
  "alive_status_but_death_time_present"
)
hosp_reason <- add_reason(
  hosp_reason, mimic$death_time_invalid, "death_time_invalid"
)
hosp_reason <- add_reason(
  hosp_reason, mimic$death_before_prediction, "death_time_before_prediction"
)
hosp_reason <- add_reason(
  hosp_reason, mimic$death_after_hospital_discharge,
  "death_time_after_hospital_discharge"
)
mimic[, `:=`(
  hospital_mortality_eligible = hosp_reason == "eligible",
  hospital_mortality_ineligibility_reason = fifelse(
    hosp_reason == "eligible", NA_character_, hosp_reason
  ),
  hospital_mortality = fifelse(
    hosp_reason == "eligible", hospital_expire_flag, NA_integer_
  )
)]

day28_reason <- rep("eligible", nrow(mimic))
day28_reason <- add_reason(day28_reason, !mimic$patient_matched, "patient_not_matched")
day28_reason <- add_reason(
  day28_reason, is.na(mimic$prediction_time_utc) | mimic$prediction_time_invalid,
  "prediction_time_missing_or_invalid"
)
day28_reason <- add_reason(
  day28_reason, mimic$date_of_death_invalid, "date_of_death_invalid"
)
day28_reason <- add_reason(
  day28_reason, mimic$death_before_prediction, "death_time_before_prediction"
)
prediction_date <- as.Date(mimic$prediction_time_utc, tz = "UTC")
dod_before_prediction_date <- !is.na(mimic$date_of_death) &
  !is.na(prediction_date) & mimic$date_of_death < prediction_date
day28_reason <- add_reason(
  day28_reason, dod_before_prediction_date,
  "date_of_death_before_prediction_calendar_date"
)
mimic[, `:=`(
  prediction_calendar_date = prediction_date,
  day28_end_date = prediction_date + 28,
  mortality_28d_eligible = day28_reason == "eligible",
  mortality_28d_ineligibility_reason = fifelse(
    day28_reason == "eligible", NA_character_, day28_reason
  )
)]
mimic[, mortality_28d := fifelse(
  mortality_28d_eligible,
  as.integer(
    !is.na(date_of_death) & date_of_death >= prediction_calendar_date &
      date_of_death <= day28_end_date
  ),
  NA_integer_
)]

icu_reason <- rep("eligible", nrow(mimic))
icu_reason <- add_reason(icu_reason, !mimic$icustay_matched, "icustay_not_matched")
icu_reason <- add_reason(
  icu_reason, mimic$icustay_matched & !mimic$icu_identifier_match,
  "icustay_identifier_mismatch"
)
icu_reason <- add_reason(
  icu_reason, is.na(mimic$prediction_time_utc) | mimic$prediction_time_invalid,
  "prediction_time_missing_or_invalid"
)
icu_reason <- add_reason(
  icu_reason, is.na(mimic$icu_outtime) | mimic$icu_outtime_invalid,
  "icu_outtime_missing_or_invalid"
)
icu_reason <- add_reason(
  icu_reason, mimic$prediction_after_icu_discharge,
  "prediction_after_icu_discharge"
)
icu_reason <- add_reason(
  icu_reason, is.na(mimic$hospital_expire_flag) |
    mimic$hospital_expire_flag_invalid,
  "hospital_status_unknown_or_invalid"
)
icu_reason <- add_reason(
  icu_reason, mimic$expired_flag_without_death_time,
  "expired_status_but_death_time_missing"
)
icu_reason <- add_reason(
  icu_reason, mimic$alive_flag_with_death_time,
  "alive_status_but_death_time_present"
)
icu_reason <- add_reason(icu_reason, mimic$death_time_invalid, "death_time_invalid")
icu_reason <- add_reason(
  icu_reason, mimic$death_before_prediction, "death_time_before_prediction"
)
icu_reason <- add_reason(
  icu_reason, mimic$icu_outtime_after_hospital_discharge,
  "icu_outtime_after_hospital_discharge"
)
mimic[, `:=`(
  icu_mortality_eligible = icu_reason == "eligible",
  icu_mortality_ineligibility_reason = fifelse(
    icu_reason == "eligible", NA_character_, icu_reason
  ),
  icu_mortality = fifelse(
    icu_reason == "eligible",
    as.integer(
      !is.na(death_time) & death_time >= prediction_time_utc &
        death_time <= icu_outtime
    ),
    NA_integer_
  )
)]

mimic_outcomes <- mimic[, .(
  subject_id, hadm_id, stay_id,
  prediction_time = prediction_time_utc,
  hospital_discharge_time, death_time, icu_outtime,
  prediction_calendar_date, day28_end_date, date_of_death,
  hospital_expire_flag_source = hospital_expire_flag,
  hospital_mortality, hospital_mortality_eligible,
  hospital_mortality_ineligibility_reason,
  mortality_28d, mortality_28d_eligible,
  mortality_28d_ineligibility_reason,
  mortality_28d_dod_missing_coded_nonevent =
    mortality_28d_eligible & is.na(date_of_death),
  icu_mortality, icu_mortality_eligible,
  icu_mortality_ineligibility_reason
)]

# ---------------------------------------------------------------------------
# eICU-CRD endpoints. There is deliberately no eICU 28-day endpoint.
# ---------------------------------------------------------------------------

eicu <- copy(eicu_prediction)
eicu[, unit_key := as.character(patientunitstayid)]
eicu_patient[, unit_key := as.character(patientunitstayid)]
eicu_min <- eicu_patient[, .(
  unit_key,
  source_patienthealthsystemstayid = patienthealthsystemstayid,
  source_hospitalid = hospitalid,
  source_uniquepid = uniquepid,
  hospitaldischargeoffset_raw = hospitaldischargeoffset,
  hospitaldischargestatus_raw = hospitaldischargestatus,
  unitdischargeoffset_raw = unitdischargeoffset,
  unitdischargestatus_raw = unitdischargestatus,
  patient_row_matched = TRUE
)]
eicu <- merge(eicu, eicu_min, by = "unit_key", all.x = TRUE, sort = FALSE)
if (nrow(eicu) != nrow(eicu_prediction) || anyDuplicated(eicu$unit_key)) {
  stop("eICU outcome join changed the prediction cohort cardinality.")
}

prediction_offset <- parse_numeric_strict(eicu$prediction_time)
hospital_offset <- parse_numeric_strict(eicu$hospitaldischargeoffset_raw)
unit_offset <- parse_numeric_strict(eicu$unitdischargeoffset_raw)
hospital_status <- normalize_status(eicu$hospitaldischargestatus_raw)
unit_status <- normalize_status(eicu$unitdischargestatus_raw)
eicu[, `:=`(
  prediction_offset_min = prediction_offset$value,
  prediction_offset_invalid = prediction_offset$invalid,
  hospital_discharge_offset_min = hospital_offset$value,
  hospital_discharge_offset_invalid = hospital_offset$invalid,
  unit_discharge_offset_min = unit_offset$value,
  unit_discharge_offset_invalid = unit_offset$invalid,
  hospital_discharge_status_source = hospital_status$raw,
  hospital_discharge_status = hospital_status$value,
  hospital_discharge_status_invalid = hospital_status$invalid,
  unit_discharge_status_source = unit_status$raw,
  unit_discharge_status = unit_status$value,
  unit_discharge_status_invalid = unit_status$invalid,
  patient_row_matched = patient_row_matched %in% TRUE
)]
eicu[, identifier_match := patient_row_matched &
  !is.na(source_patienthealthsystemstayid) & !is.na(source_hospitalid) &
  as.character(source_patienthealthsystemstayid) ==
    as.character(patienthealthsystemstayid) &
  as.character(source_hospitalid) == as.character(hospitalid)]
eicu[, `:=`(
  prediction_after_hospital_discharge =
    !is.na(prediction_offset_min) & !is.na(hospital_discharge_offset_min) &
      prediction_offset_min > hospital_discharge_offset_min,
  prediction_after_unit_discharge =
    !is.na(prediction_offset_min) & !is.na(unit_discharge_offset_min) &
      prediction_offset_min > unit_discharge_offset_min,
  unit_discharge_after_hospital_discharge =
    !is.na(unit_discharge_offset_min) &
      !is.na(hospital_discharge_offset_min) &
      unit_discharge_offset_min > hospital_discharge_offset_min,
  unit_expired_hospital_alive_conflict =
    unit_discharge_status == "expired" & hospital_discharge_status == "alive"
)]

eicu_hosp_reason <- rep("eligible", nrow(eicu))
eicu_hosp_reason <- add_reason(
  eicu_hosp_reason, !eicu$patient_row_matched, "patient_row_not_matched"
)
eicu_hosp_reason <- add_reason(
  eicu_hosp_reason, eicu$patient_row_matched & !eicu$identifier_match,
  "source_identifier_mismatch"
)
eicu_hosp_reason <- add_reason(
  eicu_hosp_reason,
  is.na(eicu$prediction_offset_min) | eicu$prediction_offset_invalid,
  "prediction_time_missing_or_invalid"
)
eicu_hosp_reason <- add_reason(
  eicu_hosp_reason,
  is.na(eicu$hospital_discharge_offset_min) |
    eicu$hospital_discharge_offset_invalid,
  "hospital_discharge_offset_missing_or_invalid"
)
eicu_hosp_reason <- add_reason(
  eicu_hosp_reason, eicu$prediction_after_hospital_discharge,
  "prediction_after_hospital_discharge"
)
eicu_hosp_reason <- add_reason(
  eicu_hosp_reason,
  is.na(eicu$hospital_discharge_status) |
    eicu$hospital_discharge_status_invalid,
  "hospital_status_unknown_or_invalid"
)
eicu_hosp_reason <- add_reason(
  eicu_hosp_reason, eicu$unit_discharge_after_hospital_discharge,
  "unit_discharge_after_hospital_discharge"
)
eicu_hosp_reason <- add_reason(
  eicu_hosp_reason, eicu$unit_expired_hospital_alive_conflict,
  "unit_expired_but_hospital_alive"
)
eicu[, `:=`(
  hospital_mortality_eligible = eicu_hosp_reason == "eligible",
  hospital_mortality_ineligibility_reason = fifelse(
    eicu_hosp_reason == "eligible", NA_character_, eicu_hosp_reason
  ),
  hospital_mortality = fifelse(
    eicu_hosp_reason == "eligible",
    as.integer(hospital_discharge_status == "expired"), NA_integer_
  )
)]

eicu_icu_reason <- rep("eligible", nrow(eicu))
eicu_icu_reason <- add_reason(
  eicu_icu_reason, !eicu$patient_row_matched, "patient_row_not_matched"
)
eicu_icu_reason <- add_reason(
  eicu_icu_reason, eicu$patient_row_matched & !eicu$identifier_match,
  "source_identifier_mismatch"
)
eicu_icu_reason <- add_reason(
  eicu_icu_reason,
  is.na(eicu$prediction_offset_min) | eicu$prediction_offset_invalid,
  "prediction_time_missing_or_invalid"
)
eicu_icu_reason <- add_reason(
  eicu_icu_reason,
  is.na(eicu$unit_discharge_offset_min) | eicu$unit_discharge_offset_invalid,
  "unit_discharge_offset_missing_or_invalid"
)
eicu_icu_reason <- add_reason(
  eicu_icu_reason, eicu$prediction_after_unit_discharge,
  "prediction_after_unit_discharge"
)
eicu_icu_reason <- add_reason(
  eicu_icu_reason,
  is.na(eicu$unit_discharge_status) | eicu$unit_discharge_status_invalid,
  "unit_status_unknown_or_invalid"
)
eicu_icu_reason <- add_reason(
  eicu_icu_reason, eicu$unit_discharge_after_hospital_discharge,
  "unit_discharge_after_hospital_discharge"
)
eicu_icu_reason <- add_reason(
  eicu_icu_reason, eicu$unit_expired_hospital_alive_conflict,
  "unit_expired_but_hospital_alive"
)
eicu[, `:=`(
  icu_mortality_eligible = eicu_icu_reason == "eligible",
  icu_mortality_ineligibility_reason = fifelse(
    eicu_icu_reason == "eligible", NA_character_, eicu_icu_reason
  ),
  icu_mortality = fifelse(
    eicu_icu_reason == "eligible",
    as.integer(unit_discharge_status == "expired"), NA_integer_
  )
)]

eicu_outcomes <- eicu[, .(
  patientunitstayid, patienthealthsystemstayid, person_key, hospitalid,
  uniquepid = source_uniquepid,
  prediction_time_offset_min = prediction_offset_min,
  hospital_discharge_offset_min, unit_discharge_offset_min,
  hospital_discharge_status_source, unit_discharge_status_source,
  hospital_mortality, hospital_mortality_eligible,
  hospital_mortality_ineligibility_reason,
  icu_mortality, icu_mortality_eligible,
  icu_mortality_ineligibility_reason
)]
if (any(grepl("28", names(eicu_outcomes)))) {
  stop("Invariant failure: an eICU 28-day endpoint was constructed.")
}

assert_endpoint <- function(x, outcome, eligible, database, endpoint) {
  y <- x[[outcome]]
  e <- x[[eligible]]
  if (anyNA(e) || any(!is.na(y) & !y %in% c(0L, 1L)) ||
      any(e & is.na(y)) || any(!e & !is.na(y))) {
    stop("Endpoint invariant failed: ", database, " / ", endpoint)
  }
}
assert_endpoint(
  mimic_outcomes, "hospital_mortality", "hospital_mortality_eligible",
  "MIMIC-IV", "hospital_mortality"
)
assert_endpoint(
  mimic_outcomes, "mortality_28d", "mortality_28d_eligible",
  "MIMIC-IV", "mortality_28d"
)
assert_endpoint(
  mimic_outcomes, "icu_mortality", "icu_mortality_eligible",
  "MIMIC-IV", "icu_mortality"
)
assert_endpoint(
  eicu_outcomes, "hospital_mortality", "hospital_mortality_eligible",
  "eICU-CRD", "hospital_mortality"
)
assert_endpoint(
  eicu_outcomes, "icu_mortality", "icu_mortality_eligible",
  "eICU-CRD", "icu_mortality"
)

# ---------------------------------------------------------------------------
# Aggregate, non-identifying QC.
# ---------------------------------------------------------------------------

classify_reason <- function(reason) {
  out <- rep("eligible", length(reason))
  not_eligible <- !is.na(reason)
  out[not_eligible & grepl(
    "before_prediction|prediction_after|after_hospital_discharge",
    reason
  )] <- "time_contradiction"
  out[not_eligible & grepl(
    "unknown|missing|invalid|not_matched", reason
  )] <- "unknown_or_unverifiable"
  out[not_eligible & grepl(
    "identifier_mismatch|but_hospital_alive|alive_status_but",
    reason
  )] <- "source_contradiction"
  out[not_eligible & out == "eligible"] <- "other_ineligible"
  out
}

endpoint_summary <- function(
    x, database, endpoint, outcome_col, eligible_col, reason_col) {
  reason <- x[[reason_col]]
  reason_class <- classify_reason(reason)
  eligible <- x[[eligible_col]]
  outcome <- x[[outcome_col]]
  data.table(
    database = database,
    endpoint = endpoint,
    prediction_record_n = nrow(x),
    eligible_n = sum(eligible),
    event_n = sum(outcome == 1L, na.rm = TRUE),
    nonevent_n = sum(outcome == 0L, na.rm = TRUE),
    ineligible_n = sum(!eligible),
    unknown_or_unverifiable_n = sum(reason_class == "unknown_or_unverifiable"),
    time_contradiction_n = sum(reason_class == "time_contradiction"),
    source_contradiction_n = sum(reason_class == "source_contradiction"),
    other_ineligible_n = sum(reason_class == "other_ineligible")
  )
}

outcome_summary <- rbindlist(list(
  endpoint_summary(
    mimic_outcomes, "MIMIC-IV_v3.1", "in_hospital_mortality",
    "hospital_mortality", "hospital_mortality_eligible",
    "hospital_mortality_ineligibility_reason"
  ),
  endpoint_summary(
    mimic_outcomes, "MIMIC-IV_v3.1", "28_day_mortality",
    "mortality_28d", "mortality_28d_eligible",
    "mortality_28d_ineligibility_reason"
  ),
  endpoint_summary(
    mimic_outcomes, "MIMIC-IV_v3.1", "icu_mortality",
    "icu_mortality", "icu_mortality_eligible",
    "icu_mortality_ineligibility_reason"
  ),
  endpoint_summary(
    eicu_outcomes, "eICU-CRD_v2.0", "in_hospital_mortality",
    "hospital_mortality", "hospital_mortality_eligible",
    "hospital_mortality_ineligibility_reason"
  ),
  endpoint_summary(
    eicu_outcomes, "eICU-CRD_v2.0", "icu_mortality",
    "icu_mortality", "icu_mortality_eligible",
    "icu_mortality_ineligibility_reason"
  )
))

reason_counts_one <- function(x, database, endpoint, reason_col) {
  z <- x[!is.na(get(reason_col)), .N, by = .(reason = get(reason_col))]
  z[, `:=`(
    database = database, endpoint = endpoint,
    reason_class = classify_reason(reason)
  )]
  z[, .(database, endpoint, reason_class, reason, n = N)]
}
reason_counts <- rbindlist(list(
  reason_counts_one(
    mimic_outcomes, "MIMIC-IV_v3.1", "in_hospital_mortality",
    "hospital_mortality_ineligibility_reason"
  ),
  reason_counts_one(
    mimic_outcomes, "MIMIC-IV_v3.1", "28_day_mortality",
    "mortality_28d_ineligibility_reason"
  ),
  reason_counts_one(
    mimic_outcomes, "MIMIC-IV_v3.1", "icu_mortality",
    "icu_mortality_ineligibility_reason"
  ),
  reason_counts_one(
    eicu_outcomes, "eICU-CRD_v2.0", "in_hospital_mortality",
    "hospital_mortality_ineligibility_reason"
  ),
  reason_counts_one(
    eicu_outcomes, "eICU-CRD_v2.0", "icu_mortality",
    "icu_mortality_ineligibility_reason"
  )
), use.names = TRUE)

timing_audit <- rbindlist(list(
  data.table(
    database = "MIMIC-IV_v3.1",
    check = c(
      "death_time_before_prediction",
      "prediction_after_hospital_discharge",
      "prediction_after_icu_discharge",
      "death_time_after_hospital_discharge",
      "icu_outtime_after_hospital_discharge",
      "expired_status_without_death_time",
      "date_of_death_before_prediction_calendar_date"
    ),
    n = c(
      sum(mimic$death_before_prediction, na.rm = TRUE),
      sum(mimic$prediction_after_hospital_discharge, na.rm = TRUE),
      sum(mimic$prediction_after_icu_discharge, na.rm = TRUE),
      sum(mimic$death_after_hospital_discharge, na.rm = TRUE),
      sum(mimic$icu_outtime_after_hospital_discharge, na.rm = TRUE),
      sum(mimic$expired_flag_without_death_time, na.rm = TRUE),
      sum(dod_before_prediction_date, na.rm = TRUE)
    )
  ),
  data.table(
    database = "eICU-CRD_v2.0",
    check = c(
      "prediction_after_hospital_discharge",
      "prediction_after_unit_discharge",
      "unit_discharge_after_hospital_discharge",
      "unit_expired_but_hospital_alive",
      "source_identifier_mismatch"
    ),
    n = c(
      sum(eicu$prediction_after_hospital_discharge, na.rm = TRUE),
      sum(eicu$prediction_after_unit_discharge, na.rm = TRUE),
      sum(eicu$unit_discharge_after_hospital_discharge, na.rm = TRUE),
      sum(eicu$unit_expired_hospital_alive_conflict, na.rm = TRUE),
      sum(eicu$patient_row_matched & !eicu$identifier_match, na.rm = TRUE)
    )
  )
))

if (any(outcome_summary$eligible_n !=
    outcome_summary$event_n + outcome_summary$nonevent_n) ||
    any(outcome_summary$prediction_record_n !=
      outcome_summary$eligible_n + outcome_summary$ineligible_n) ||
    any(outcome_summary$ineligible_n !=
      outcome_summary$unknown_or_unverifiable_n +
        outcome_summary$time_contradiction_n +
        outcome_summary$source_contradiction_n +
        outcome_summary$other_ineligible_n)) {
  stop("Aggregate outcome-QC accounting invariant failed.")
}

attr(mimic_outcomes, "rebuild_metadata") <- list(
  version = "rebuilt_outcomes_v1",
  database = "MIMIC-IV_v3.1",
  config_version = LOCKED$version,
  formally_unblinded = TRUE,
  checkpoint_sha256 = checkpoint_sha256,
  source = "admissions hospital_expire_flag/deathtime; patients dod; icustays outtime"
)
attr(eicu_outcomes, "rebuild_metadata") <- list(
  version = "rebuilt_outcomes_v1",
  database = "eICU-CRD_v2.0",
  config_version = LOCKED$version,
  formally_unblinded = TRUE,
  checkpoint_sha256 = checkpoint_sha256,
  source = "patient hospital/unit discharge status and offsets",
  eicu_28_day_endpoint_constructed = FALSE
)

atomic_save_rds_new(mimic_outcomes, mimic_output)
atomic_save_rds_new(eicu_outcomes, eicu_output)
atomic_fwrite_new(outcome_summary, summary_output)
atomic_fwrite_new(reason_counts, reason_output)
atomic_fwrite_new(timing_audit, timing_output)

completion <- data.table(
  status = "PASS",
  config_version = LOCKED$version,
  outcome_access_status = "FORMALLY_UNBLINDED",
  first_access_at = require_map_value(
    access_receipt, "first_access_at", label = "outcome-access receipt"
  ),
  completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  script_sha256 = script_sha256,
  checkpoint_sha256 = checkpoint_sha256,
  access_receipt_sha256 = sha256_file(access_receipt_path),
  analysis_script_manifest_sha256 = sha256_file(analysis_manifest_path),
  parameter_freeze_gate_sha256 = sha256_file(parameter_gate_path),
  selection_weights_gate_sha256 = sha256_file(selection_gate_path),
  mimic_severity_gate_sha256 = sha256_file(mimic_severity_gate_path),
  eicu_severity_gate_sha256 = sha256_file(eicu_severity_gate_path),
  mimic_prediction_rds_sha256 = sha256_file(mimic_prediction_rds),
  eicu_prediction_rds_sha256 = sha256_file(eicu_prediction_rds),
  mimic_admissions_sha256 = raw_outcome_sha256[[mimic_admissions_path]],
  mimic_patients_sha256 = raw_outcome_sha256[[mimic_patients_path]],
  mimic_icustays_sha256 = raw_outcome_sha256[[mimic_icustays_path]],
  eicu_patient_sha256 = raw_outcome_sha256[[eicu_patient_path]],
  mimic_outcome_rds_sha256 = sha256_file(mimic_output),
  eicu_outcome_rds_sha256 = sha256_file(eicu_output),
  outcome_summary_sha256 = sha256_file(summary_output),
  ineligibility_reason_qc_sha256 = sha256_file(reason_output),
  timing_audit_qc_sha256 = sha256_file(timing_output),
  mimic_prediction_n = nrow(mimic_outcomes),
  eicu_prediction_n = nrow(eicu_outcomes),
  mimic_primary_eligible_n = mimic_outcomes[
    hospital_mortality_eligible == TRUE, .N
  ],
  mimic_primary_event_n = mimic_outcomes[
    hospital_mortality_eligible == TRUE & hospital_mortality == 1L, .N
  ],
  eicu_primary_eligible_n = eicu_outcomes[
    hospital_mortality_eligible == TRUE, .N
  ],
  eicu_primary_event_n = eicu_outcomes[
    hospital_mortality_eligible == TRUE & hospital_mortality == 1L, .N
  ],
  eicu_28_day_endpoint_constructed = FALSE,
  all_accounting_invariants_pass = TRUE
)
completion_tmp <- paste0(completion_gate, ".tmp.", Sys.getpid())
unlink(completion_tmp, force = TRUE)
fwrite(completion, completion_tmp)
if (!file.rename(completion_tmp, completion_gate)) {
  unlink(completion_tmp, force = TRUE)
  stop("Could not atomically publish the outcome-extraction completion gate.")
}

message("Rebuilt outcome extraction complete under formal authorization.")
message("  MIMIC prediction records: ", nrow(mimic_outcomes))
message("  eICU prediction records: ", nrow(eicu_outcomes))
message("  Private outcomes: ", private_out)
message("  Aggregate QC/gate: ", qc_out)
