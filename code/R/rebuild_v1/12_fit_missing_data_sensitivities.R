#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: association-focused missing-data
# sensitivities using multiple imputation and exploratory MNAR delta patterns.
#
# GOVERNANCE WARNING
# ------------------
# This script is outcome-bearing. It must never be sourced. It may be executed
# only after the formal authorization checkpoint, Phase 3a rebuilt-outcome
# PASS gate, and Phase 3b locked-main-model PASS gate exist and match their
# authorized SHA256 chain. Before authorization, syntax checking is limited to
# parse(file=...). This script does not estimate or report MI-based external
# validation performance: the outcome is used inside each database's
# association-focused imputation model, so eICU results are local replication.

suppressPackageStartupMessages(library(data.table))
options(warn = 1)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/12_fit_missing_data_sensitivities.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
project_from_script <- normalizePath(
  file.path(script_dir, "..", "..", ".."), mustWork = TRUE
)
expected_config_version <- "1.0.1"

MI_M <- 50L
MI_MAXIT <- 20L
MI_SEEDS <- c(`MIMIC-IV_v3.1` = 20260717L, `eICU-CRD_v2.0` = 20260718L)
MI_DONORS <- 5L
MI_RIDGE <- 1e-5
MI_IMPUTED_CONTINUOUS <- c("gcs", "map", "platelet", "creatinine")
MI_COMPLETE_PREDICTORS <- c(
  "age", "sex_female", "pf_ratio", "vasopressor", "delta_p", "rr", "smp",
  "peep", "resistive_pressure"
)
MI_COVARIATES <- c(
  "age", "sex_female", "pf_ratio", "gcs", "map", "vasopressor",
  "platelet", "creatinine"
)
NONIMPUTED_EXPOSURES <- c("delta_p", "rr", "smp")
NONIMPUTED_TUPLE_AUXILIARIES <- c("peep", "resistive_pressure")
MI_DATA_COLUMNS <- c(
  "hospital_mortality", MI_COVARIATES, NONIMPUTED_EXPOSURES,
  NONIMPUTED_TUPLE_AUXILIARIES
)
MNAR_SCENARIOS <- data.table(
  scenario = c("MAR", "MNAR_adverse_0.5SD", "MNAR_adverse_1.0SD"),
  delta_sd = c(0, 0.5, 1.0),
  assumption_class = c(
    "association_focused_MAR",
    "exploratory_continuous_delta_pattern",
    "exploratory_continuous_delta_pattern"
  )
)
MNAR_RULES <- data.table(
  variable = c("gcs", "map", "platelet", "creatinine"),
  mimic_observed_sd = c(
    4.6877682109, 14.5493740348, 108.8391842917, 1.5262113315
  ),
  adverse_direction = c(-1, -1, -1, 1),
  lower_bound = c(3, 1, .Machine$double.xmin, 0.1),
  upper_bound = c(15, 250, 9999, 28.28),
  boundary_rule = c(
    "truncate_to_source_validity_bounds",
    "truncate_to_source_validity_bounds",
    "truncate_to_strictly_positive_and_source_upper_bound",
    "truncate_to_source_validity_bounds"
  )
)

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

same_numeric <- function(x, y, tolerance = 1e-12) {
  length(x) == length(y) && all(
    (is.na(x) & is.na(y)) |
      (!is.na(x) & !is.na(y) &
        abs(as.numeric(x) - as.numeric(y)) <= tolerance)
  )
}

same_matrix_values <- function(x, y, require_rownames = TRUE) {
  is.matrix(x) && is.matrix(y) && identical(dim(x), dim(y)) &&
    identical(colnames(x), colnames(y)) &&
    (!require_rownames || identical(rownames(x), rownames(y))) &&
    all(as.vector(x) == as.vector(y))
}

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

# The authorization checkpoint is intentionally the first project artifact
# opened. No config, outcome gate, model gate, or row-level RDS precedes it.
checkpoint_path <- file.path(
  project_from_script, "analysis_rebuild_v1", "qc", "unblinding",
  "outcome_unblinding_checkpoint_v1.csv"
)
if (!file.exists(checkpoint_path)) {
  stop(
    "Missing-data modeling is not authorized: missing checkpoint ",
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
decision_log_path <- file.path(
  project_from_script, "docs", "rebuild_v1", "analysis_decision_log.md"
)

locked_checkpoint_files <- c(
  config_script_sha256 = config_path,
  parameter_freeze_script_sha256 = parameter_script_path,
  model_utils_script_sha256 = model_utils_path,
  model_analysis_utils_script_sha256 = analysis_utils_path,
  outcome_extraction_script_sha256 = outcome_script_path,
  model_analysis_script_sha256 = main_model_script_path,
  missing_data_sensitivities_script_sha256 = script_path,
  analysis_decision_log_sha256 = decision_log_path
)
if (any(!file.exists(locked_checkpoint_files))) {
  stop("An authorized locked file is missing.")
}
for (field in names(locked_checkpoint_files)) {
  require_map_value(
    checkpoint, field, sha256_file(locked_checkpoint_files[[field]]),
    "authorization checkpoint"
  )
}

source(config_path)
if (!identical(LOCKED$version, expected_config_version) ||
    !identical(normalizePath(PROJECT_ROOT, mustWork = TRUE), project_from_script)) {
  stop("Loaded config differs from the authorized project/configuration.")
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
  analysis_paths, sha256_file, character(1L)
))
if (!identical(analysis_hashes, current_analysis_hashes)) {
  stop("A current analysis script differs from the authorized manifest.")
}
must_manifest <- normalizePath(c(
  model_utils_path, analysis_utils_path, outcome_script_path,
  main_model_script_path, script_path
), mustWork = TRUE)
if (length(setdiff(must_manifest, analysis_paths))) {
  stop("Authorized manifest must include exact 08, 08a, 09, 10, and 12 scripts.")
}
self_manifest_index <- match(normalizePath(script_path), analysis_paths)
if (is.na(self_manifest_index) ||
    !identical(analysis_hashes[[self_manifest_index]], sha256_file(script_path))) {
  stop("This missing-data script is not self-hash-locked by the manifest.")
}

parameter_gate_path <- file.path(
  QC_ROOT, "parameter_freeze", "phase2e_parameter_freeze_complete_v1.csv"
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
if (any(!file.exists(c(
  parameter_gate_path, outcome_gate_path, main_model_gate_path,
  access_receipt_path
)))) {
  stop("A required upstream gate/access receipt is missing.")
}

parameter_gate <- read_completion_gate(parameter_gate_path, "parameter gate")
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
require_map_value(
  parameter_gate, "summary_sentinel", "BUILD_COMPLETE", "parameter gate"
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
  main_model_gate, "multiple_imputation_implemented", "FALSE", "main-model gate"
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
  sha256_file(parameter_gate_path), "main-model gate"
)
require_map_value(
  main_model_gate, "outcome_gate_sha256", sha256_file(outcome_gate_path),
  "main-model gate"
)
require_map_value(
  outcome_gate, "script_sha256", sha256_file(outcome_script_path), "outcome gate"
)
require_map_value(
  parameter_gate, "script_sha256", sha256_file(parameter_script_path),
  "parameter gate"
)

decision_text <- paste(readLines(decision_log_path, warn = FALSE), collapse = "\n")
for (decision_id in c("D020", "D054", "D060")) {
  if (!grepl(paste0("\\| ", decision_id, " \\|"), decision_text)) {
    stop("Authorized decision log lacks ", decision_id)
  }
}
d060_line <- grep(
  "^\\| D060 \\|", readLines(decision_log_path, warn = FALSE), value = TRUE
)
required_d060_tokens <- c(
  "SECONDARY-LOCKED", "50 imputations", "20 iterations", "five donors",
  "20260717/20260718", "S2, S3, and S2M", "4.6877682109",
  "14.5493740348", "108.8391842917", "1.5262113315",
  "PEEP", "resistive pressure", "PBW-derived fields",
  "Do not estimate or report MI external-validation performance"
)
if (length(d060_line) != 1L ||
    any(!vapply(required_d060_tokens, grepl, logical(1L),
      x = d060_line, fixed = TRUE))) {
  stop("D060 status/text does not match the executable MI contract.")
}

local_r_library <- file.path(project_from_script, "analysis_rebuild_v1", "r_library")
if (!dir.exists(local_r_library)) stop("Locked local R library is missing.")
.libPaths(c(normalizePath(local_r_library, mustWork = TRUE), .libPaths()))
if (!requireNamespace("mice", quietly = TRUE) ||
    as.character(utils::packageVersion("mice")) != "3.19.0") {
  stop("Locked mice version 3.19.0 is required.")
}
require_map_value(checkpoint, "mice_version", "3.19.0", "authorization checkpoint")

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
  require_map_value(parameter_gate, pair[[2L]], sha256_file(path), "parameter gate")
  path
})

mimic_outcome_path <- file.path(
  PRIVATE_ROOT, "outcomes", "mimic_rebuilt_outcomes_v1.rds"
)
eicu_outcome_path <- file.path(
  PRIVATE_ROOT, "outcomes", "eicu_rebuilt_outcomes_v1.rds"
)
main_model_rds_path <- file.path(
  PRIVATE_ROOT, "locked_models", "mimic_locked_models_v1.rds"
)
if (any(!file.exists(c(
  mimic_outcome_path, eicu_outcome_path, main_model_rds_path
)))) stop("A checksum-gated private upstream artifact is missing.")
require_map_value(
  outcome_gate, "mimic_outcome_rds_sha256", sha256_file(mimic_outcome_path),
  "outcome gate"
)
require_map_value(
  outcome_gate, "eicu_outcome_rds_sha256", sha256_file(eicu_outcome_path),
  "outcome gate"
)
require_map_value(
  main_model_gate, "model_rds_sha256", sha256_file(main_model_rds_path),
  "main-model gate"
)

# Only after all authorization/gate/hash checks are complete are row-level
# predictor and outcome artifacts opened.
source(model_utils_path)
source(analysis_utils_path)
required_utils <- c(
  "parameter_to_transform_bundle", "build_design_matrix", "fit_model",
  "validate_transform_bundle", "assert_binary_outcome"
)
if (!all(vapply(required_utils, exists, logical(1L), mode = "function"))) {
  stop("Locked model utility interface is incomplete.")
}

parameters <- readRDS(parameter_artifact_paths$parameter)
mimic_frame <- as.data.table(readRDS(parameter_artifact_paths$mimic_frame))
eicu_frame <- as.data.table(readRDS(parameter_artifact_paths$eicu_frame))
mimic_outcomes <- as.data.table(readRDS(mimic_outcome_path))
eicu_outcomes <- as.data.table(readRDS(eicu_outcome_path))
main_model_bundle <- readRDS(main_model_rds_path)

if (!is.list(parameters) || !identical(parameters$decision_id, "D054") ||
    !identical(parameters$locked_config_version, LOCKED$version) ||
    !identical(parameters$model_utils_sha256, sha256_file(model_utils_path))) {
  stop("Frozen parameter artifact provenance failed.")
}
transform_bundle <- parameter_to_transform_bundle(parameters)
canonical_schema <- as.character(parameters$canonical_model_frame_schema)
if (!identical(names(mimic_frame), canonical_schema) ||
    !identical(names(eicu_frame), canonical_schema) ||
    anyDuplicated(mimic_frame$analysis_id) ||
    anyDuplicated(eicu_frame$analysis_id)) {
  stop("Canonical predictor frame schema/key invariant failed.")
}

required_bundle_fields <- c(
  "artifact_version", "config_version", "checkpoint_sha256",
  "parameter_gate_sha256", "outcome_gate_sha256",
  "analysis_manifest_sha256", "model_utils_sha256",
  "analysis_utils_sha256", "frozen_parameter_rds_sha256",
  "model_specification", "fits", "transform_bundle"
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
    )) stop("Main-model bundle provenance/schema check failed.")
if (!identical(main_model_bundle$transform_bundle, transform_bundle) ||
    !identical(
      main_model_bundle$model_specification[model_id == "S3", design_type],
      "s0_smp_per_5"
    )) stop("MI does not inherit the exact frozen linear-S3 transformation.")

for (model_id in c("S2", "S3", "S2M")) {
  target_model_id <- model_id
  expected_design <- c(
    S2 = "s0_delta_p_rr", S3 = "s0_smp_per_5",
    S2M = "s0_delta_p_rr_smp_per_5"
  )[[model_id]]
  observed_design <- main_model_bundle$model_specification[
    model_id == target_model_id, design_type
  ]
  if (!identical(observed_design, expected_design) ||
      is.null(main_model_bundle$fits[[model_id]]) ||
      !identical(main_model_bundle$fits[[model_id]]$status, "ESTIMABLE")) {
    stop("Main Phase 3b model contract failed for ", model_id)
  }
}

required_mimic_outcomes <- c(
  "stay_id", "hospital_mortality", "hospital_mortality_eligible"
)
required_eicu_outcomes <- c(
  "patientunitstayid", "hospital_mortality", "hospital_mortality_eligible"
)
if (length(setdiff(required_mimic_outcomes, names(mimic_outcomes))) ||
    length(setdiff(required_eicu_outcomes, names(eicu_outcomes))) ||
    anyDuplicated(mimic_outcomes$stay_id) ||
    anyDuplicated(eicu_outcomes$patientunitstayid)) {
  stop("Outcome schema/linkage invariant failed.")
}
if (nrow(mimic_outcomes) != as.integer(require_map_value(
  outcome_gate, "mimic_prediction_n", label = "outcome gate"
)) || nrow(eicu_outcomes) != as.integer(require_map_value(
  outcome_gate, "eicu_prediction_n", label = "outcome gate"
))) stop("Outcome artifact count differs from the Phase 3a gate.")

mimic_outcome_link <- mimic_outcomes[, .(
  analysis_id = as.integer(stay_id),
  hospital_mortality = as.integer(hospital_mortality),
  hospital_mortality_eligible = as.logical(hospital_mortality_eligible)
)]
eicu_outcome_link <- eicu_outcomes[, .(
  analysis_id = as.integer(patientunitstayid),
  hospital_mortality = as.integer(hospital_mortality),
  hospital_mortality_eligible = as.logical(hospital_mortality_eligible)
)]
if (!setequal(mimic_frame$analysis_id, mimic_outcome_link$analysis_id) ||
    !setequal(eicu_frame$analysis_id, eicu_outcome_link$analysis_id)) {
  stop("Canonical frame and rebuilt outcome ID sets differ.")
}

prepare_mi_analysis <- function(frame, outcome_link, database) {
  joined <- merge(
    frame, outcome_link, by = "analysis_id", all = FALSE, sort = TRUE
  )
  if (nrow(joined) != nrow(frame) || anyDuplicated(joined$analysis_id)) {
    stop("Exact MI predictor/outcome join failed for ", database)
  }
  eligible <- joined$hospital_mortality_eligible %in% TRUE &
    !is.na(joined$hospital_mortality) &
    joined$hospital_mortality %in% c(0L, 1L)
  data <- joined[eligible]
  if (!nrow(data)) stop("No eligible MI records for ", database)
  assert_binary_outcome(data$hospital_mortality)
  if (anyNA(data[, ..MI_COMPLETE_PREDICTORS]) ||
      any(!vapply(data[, ..MI_COMPLETE_PREDICTORS], function(x) {
        all(is.finite(as.numeric(x)))
      }, logical(1L)))) {
    stop("A D060 complete predictor is missing or non-finite in ", database)
  }
  if (any(!data$sex_female %in% c(0L, 1L)) ||
      any(!data$vasopressor %in% c(0L, 1L))) {
    stop("A complete binary predictor is not coded 0/1 in ", database)
  }
  for (variable in MI_IMPUTED_CONTINUOUS) {
    observed <- data[[variable]][!is.na(data[[variable]])]
    if (!length(observed) || any(!is.finite(observed))) {
      stop("Invalid observed imputation target ", variable, " in ", database)
    }
  }
  data
}

mimic_analysis <- prepare_mi_analysis(
  mimic_frame, mimic_outcome_link, "MIMIC-IV_v3.1"
)
eicu_analysis <- prepare_mi_analysis(
  eicu_frame, eicu_outcome_link, "eICU-CRD_v2.0"
)

sd_reproduction_qc <- rbindlist(lapply(seq_len(nrow(MNAR_RULES)), function(i) {
  variable <- MNAR_RULES$variable[[i]]
  observed <- mimic_frame[[variable]][!is.na(mimic_frame[[variable]])]
  reproduced <- stats::sd(observed)
  frozen <- MNAR_RULES$mimic_observed_sd[[i]]
  data.table(
    variable = variable,
    derivation_population = "MIMIC canonical primary-tuple frame; observed values",
    observed_n = length(observed), reproduced_sd = reproduced,
    frozen_D060_sd = frozen, absolute_difference = abs(reproduced - frozen),
    tolerance = 1e-9 * max(1, abs(frozen)),
    pass = abs(reproduced - frozen) <= 1e-9 * max(1, abs(frozen))
  )
}))
if (any(!sd_reproduction_qc$pass)) {
  stop("D060 frozen MIMIC SD reproduction failed.")
}

fixed_mice_specification <- function() {
  method <- setNames(rep("", length(MI_DATA_COLUMNS)), MI_DATA_COLUMNS)
  method[MI_IMPUTED_CONTINUOUS] <- "pmm"
  predictor_matrix <- matrix(
    0L, nrow = length(MI_DATA_COLUMNS), ncol = length(MI_DATA_COLUMNS),
    dimnames = list(MI_DATA_COLUMNS, MI_DATA_COLUMNS)
  )
  for (target in MI_IMPUTED_CONTINUOUS) {
    predictor_matrix[target, setdiff(MI_DATA_COLUMNS, target)] <- 1L
  }
  predictor_matrix[, "hospital_mortality"] <- ifelse(
    rownames(predictor_matrix) %in% MI_IMPUTED_CONTINUOUS, 1L, 0L
  )
  predictor_matrix["hospital_mortality", ] <- 0L
  predictor_matrix[NONIMPUTED_EXPOSURES, ] <- 0L
  predictor_matrix[NONIMPUTED_TUPLE_AUXILIARIES, ] <- 0L
  predictor_matrix[MI_COMPLETE_PREDICTORS, ] <- 0L
  diag(predictor_matrix) <- 0L
  post <- setNames(rep("", length(MI_DATA_COLUMNS)), MI_DATA_COLUMNS)
  list(method = method, predictor_matrix = predictor_matrix, post = post)
}

mi_spec <- fixed_mice_specification()
if (!identical(names(mi_spec$method), MI_DATA_COLUMNS) ||
    !identical(rownames(mi_spec$predictor_matrix), MI_DATA_COLUMNS) ||
    !identical(colnames(mi_spec$predictor_matrix), MI_DATA_COLUMNS) ||
    !identical(unname(mi_spec$method[MI_IMPUTED_CONTINUOUS]), rep("pmm", 4L)) ||
    any(mi_spec$method[setdiff(MI_DATA_COLUMNS, MI_IMPUTED_CONTINUOUS)] != "") ||
    any(mi_spec$predictor_matrix[
      setdiff(MI_DATA_COLUMNS, MI_IMPUTED_CONTINUOUS), , drop = FALSE
    ] != 0L) ||
    any(mi_spec$predictor_matrix[MI_IMPUTED_CONTINUOUS,
      "hospital_mortality", drop = FALSE
    ] != 1L)) {
  stop("Fixed D060 mice method/predictor-matrix contract failed.")
}

method_specification <- data.table(
  variable = MI_DATA_COLUMNS,
  method = unname(mi_spec$method[MI_DATA_COLUMNS]),
  imputed = MI_DATA_COLUMNS %in% MI_IMPUTED_CONTINUOUS,
  role = fcase(
    MI_DATA_COLUMNS == "hospital_mortality",
    "complete_outcome_predictor_never_imputed",
    MI_DATA_COLUMNS %in% MI_IMPUTED_CONTINUOUS,
    "incomplete_continuous_covariate_PMM",
    MI_DATA_COLUMNS %in% NONIMPUTED_EXPOSURES,
    "complete_ventilator_exposure_predictor_never_imputed",
    MI_DATA_COLUMNS %in% NONIMPUTED_TUPLE_AUXILIARIES,
    "complete_tuple_auxiliary_predictor_never_imputed",
    default = "complete_covariate_predictor_never_imputed"
  ),
  donors = ifelse(MI_DATA_COLUMNS %in% MI_IMPUTED_CONTINUOUS, MI_DONORS, NA_integer_),
  ridge = ifelse(MI_DATA_COLUMNS %in% MI_IMPUTED_CONTINUOUS, MI_RIDGE, NA_real_)
)
predictor_matrix_specification <- as.data.table(as.table(
  mi_spec$predictor_matrix
))
setnames(
  predictor_matrix_specification,
  c("target_variable", "predictor_variable", "included")
)
predictor_matrix_specification[, included := as.integer(as.character(included))]
predictor_matrix_specification[, `:=`(
  outcome_as_predictor = predictor_variable == "hospital_mortality" & included == 1L,
  matrix_frozen_identically_across_databases = TRUE
)]

summarize_values <- function(
    values, database, scenario, variable, value_source, variable_type) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  q <- if (length(values)) {
    as.numeric(stats::quantile(
      values, c(0, 0.05, 0.50, 0.95, 1), names = FALSE, type = 2L
    ))
  } else rep(NA_real_, 5L)
  data.table(
    database = database, scenario = scenario, variable = variable,
    value_source = value_source, variable_type = variable_type,
    n = length(values), mean = if (length(values)) mean(values) else NA_real_,
    sd = if (length(values) > 1L) stats::sd(values) else NA_real_,
    min = q[[1L]], p05 = q[[2L]], median = q[[3L]], p95 = q[[4L]],
    max = q[[5L]],
    proportion_one = if (identical(variable_type, "binary") && length(values)) {
      mean(values == 1)
    } else NA_real_
  )
}

apply_delta_pattern <- function(completed, original, delta_sd, database, scenario) {
  shifted <- copy(completed)
  qc <- rbindlist(lapply(seq_len(nrow(MNAR_RULES)), function(i) {
    rule <- MNAR_RULES[i]
    variable <- rule$variable[[1L]]
    missing <- is.na(original[[variable]])
    before <- as.numeric(shifted[[variable]][missing])
    proposed <- before + delta_sd * rule$mimic_observed_sd[[1L]] *
      rule$adverse_direction[[1L]]
    after <- pmin(
      pmax(proposed, rule$lower_bound[[1L]]), rule$upper_bound[[1L]]
    )
    # This assignment occurs inside the lapply closure. A nested replacement
    # such as shifted[[variable]][missing] <- after would create a closure-local
    # copy of `shifted` and leave the object returned by this function unchanged.
    # Use data.table's explicit by-reference setter so only the originally
    # missing (therefore imputed) cells in the outer object are shifted.
    data.table::set(shifted, which(missing), variable, after)
    observed <- !missing
    observed_before <- as.numeric(completed[[variable]][observed])
    observed_after <- as.numeric(shifted[[variable]][observed])
    if (!same_numeric(shifted[[variable]][missing], after) ||
        !same_numeric(observed_after, observed_before)) {
      stop("MNAR delta assignment altered the wrong cells for ", variable)
    }
    data.table(
      database = database, scenario = scenario, variable = variable,
      missing_cells_in_one_imputation = sum(missing),
      delta_sd = delta_sd,
      frozen_mimic_sd = rule$mimic_observed_sd[[1L]],
      signed_absolute_shift = delta_sd * rule$mimic_observed_sd[[1L]] *
        rule$adverse_direction[[1L]],
      lower_bound = rule$lower_bound[[1L]],
      upper_bound = rule$upper_bound[[1L]],
      proposed_below_lower_n = sum(proposed < rule$lower_bound[[1L]]),
      proposed_above_upper_n = sum(proposed > rule$upper_bound[[1L]]),
      boundary_truncated_n = sum(after != proposed),
      observed_cells_shifted_n = sum(observed_after != observed_before)
    )
  }))
  list(data = shifted, qc = qc)
}

rubin_pool <- function(coefficient_draws, database, scenario, model_id) {
  target_database <- database
  target_scenario <- scenario
  target_model_id <- model_id
  target <- coefficient_draws[
    database == target_database & scenario == target_scenario &
      model_id == target_model_id
  ]
  if (!nrow(target) || uniqueN(target$imputation) != MI_M ||
      anyDuplicated(target[, .(imputation, term)])) {
    stop("Incomplete per-imputation coefficient table for ", database, "/",
      scenario, "/", model_id)
  }
  rbindlist(lapply(unique(target$term), function(term_name) {
    z <- target[term == term_name][order(imputation)]
    if (nrow(z) != MI_M || any(!is.finite(z$estimate)) ||
        any(!is.finite(z$within_variance)) || any(z$within_variance < 0)) {
      stop("Invalid Rubin inputs for term ", term_name)
    }
    qbar <- mean(z$estimate)
    ubar <- mean(z$within_variance)
    between <- stats::var(z$estimate)
    total <- ubar + (1 + 1 / MI_M) * between
    relative_increase <- if (ubar > 0) {
      (1 + 1 / MI_M) * between / ubar
    } else if (between > 0) Inf else 0
    df <- if (between <= .Machine$double.eps) {
      Inf
    } else {
      (MI_M - 1) * (1 + ubar / ((1 + 1 / MI_M) * between))^2
    }
    lambda_missing_information <- if (total > 0) {
      ((1 + 1 / MI_M) * between) / total
    } else 0
    fraction_missing_information <- if (is.infinite(relative_increase)) {
      1
    } else if (is.finite(df)) {
      (relative_increase + 2 / (df + 3)) / (relative_increase + 1)
    } else lambda_missing_information
    relative_efficiency <- 1 / (1 + fraction_missing_information / MI_M)
    standard_error <- sqrt(total)
    critical <- if (is.finite(df)) stats::qt(0.975, df) else stats::qnorm(0.975)
    statistic <- qbar / standard_error
    p_value <- if (is.finite(df)) {
      2 * stats::pt(-abs(statistic), df)
    } else 2 * stats::pnorm(-abs(statistic))
    data.table(
      database = database, scenario = scenario, model_id = model_id,
      term = term_name, imputations = MI_M, estimate = qbar,
      within_imputation_variance = ubar,
      between_imputation_variance = between,
      total_variance = total, standard_error = standard_error,
      degrees_freedom = df, statistic = statistic, p_value = p_value,
      ci_lower = qbar - critical * standard_error,
      ci_upper = qbar + critical * standard_error,
      odds_ratio = exp(qbar),
      odds_ratio_ci_lower = exp(qbar - critical * standard_error),
      odds_ratio_ci_upper = exp(qbar + critical * standard_error),
      relative_increase_in_variance = relative_increase,
      lambda_missing_information = lambda_missing_information,
      fraction_missing_information = fraction_missing_information,
      relative_efficiency = relative_efficiency,
      monte_carlo_error_qbar = sqrt(between / MI_M),
      pooling_rule = "manual_Rubin_scalar"
    )
  }))
}

extract_chain_summary <- function(mids, database, missing_counts) {
  chain_mean <- mids$chainMean
  chain_var <- mids$chainVar
  if (length(dim(chain_mean)) != 3L ||
      !identical(dim(chain_mean), dim(chain_var)) ||
      dim(chain_mean)[[2L]] != MI_MAXIT || dim(chain_mean)[[3L]] != MI_M) {
    stop("Unexpected mice chain array dimensions for ", database)
  }
  # mice 3.19.0 stores chainMean/chainVar as
  # variable x iteration x chain arrays.
  variable_names <- dimnames(chain_mean)[[1L]]
  if (is.null(variable_names) ||
      length(setdiff(MI_IMPUTED_CONTINUOUS, variable_names))) {
    stop("mice chain array lacks a D060 target for ", database)
  }
  rbindlist(lapply(MI_IMPUTED_CONTINUOUS, function(variable) {
    variable_index <- match(variable, variable_names)
    rbindlist(lapply(seq_len(MI_M), function(chain) {
      mean_trace <- as.numeric(chain_mean[variable_index, , chain])
      var_trace <- as.numeric(chain_var[variable_index, , chain])
      final_window <- seq.int(max(1L, MI_MAXIT - 4L), MI_MAXIT)
      data.table(
        database = database, variable = variable, chain = chain,
        original_missing_n = unname(missing_counts[[variable]]),
        iterations = MI_MAXIT,
        final_chain_mean = mean_trace[[MI_MAXIT]],
        final_chain_variance = var_trace[[MI_MAXIT]],
        last5_mean_of_chain_means = if (any(is.finite(mean_trace[final_window]))) {
          mean(mean_trace[final_window], na.rm = TRUE)
        } else NA_real_,
        last5_range_of_chain_means = if (any(is.finite(mean_trace[final_window]))) {
          diff(range(mean_trace[final_window], na.rm = TRUE))
        } else NA_real_,
        last5_mean_of_chain_variances = if (any(is.finite(var_trace[final_window]))) {
          mean(var_trace[final_window], na.rm = TRUE)
        } else NA_real_
      )
    }))
  }))
}

extract_chain_trace <- function(mids, database, missing_counts) {
  chain_mean <- mids$chainMean
  chain_var <- mids$chainVar
  if (length(dim(chain_mean)) != 3L ||
      !identical(dim(chain_mean), dim(chain_var)) ||
      dim(chain_mean)[[2L]] != MI_MAXIT || dim(chain_mean)[[3L]] != MI_M) {
    stop("Unexpected mice chain array dimensions for ", database)
  }
  variable_names <- dimnames(chain_mean)[[1L]]
  if (is.null(variable_names) ||
      length(setdiff(MI_IMPUTED_CONTINUOUS, variable_names))) {
    stop("mice chain array lacks a D060 target for ", database)
  }
  trace <- rbindlist(lapply(MI_IMPUTED_CONTINUOUS, function(variable) {
    variable_index <- match(variable, variable_names)
    rbindlist(lapply(seq_len(MI_M), function(chain) {
      data.table(
        database = database,
        variable = variable,
        chain = chain,
        iteration = seq_len(MI_MAXIT),
        original_missing_n = unname(missing_counts[[variable]]),
        chain_mean = as.numeric(chain_mean[variable_index, , chain]),
        chain_variance = as.numeric(chain_var[variable_index, , chain])
      )
    }))
  }))
  expected_n <- length(MI_IMPUTED_CONTINUOUS) * MI_M * MI_MAXIT
  if (nrow(trace) != expected_n ||
      anyDuplicated(trace[, .(database, variable, chain, iteration)]) ||
      any(trace$original_missing_n <= 0L)) {
    stop("Iteration-level mice chain trace invariant failed for ", database)
  }
  trace
}

extract_logged_events <- function(mids, database) {
  events <- mids$loggedEvents
  if (is.null(events) || !nrow(events)) {
    return(data.table(
      database = database, iteration = NA_integer_, imputation = NA_integer_,
      dependent_variable = NA_character_, method = NA_character_,
      event = NA_character_, event_count = 0L, status = "NO_LOGGED_EVENTS"
    ))
  }
  events <- as.data.table(events)
  expected <- c("it", "im", "dep", "meth", "out")
  if (length(setdiff(expected, names(events)))) {
    stop("Unexpected mice loggedEvents schema for ", database)
  }
  events[, .(
    event_count = .N
  ), by = .(
    database = database, iteration = as.integer(it),
    imputation = as.integer(im), dependent_variable = as.character(dep),
    method = as.character(meth), event = as.character(out),
    status = "LOGGED_EVENT_REPORTED"
  )]
}

run_database_mi <- function(frame, database, seed) {
  original <- copy(frame[, c("analysis_id", MI_DATA_COLUMNS), with = FALSE])
  mice_data <- as.data.frame(original[, ..MI_DATA_COLUMNS])
  if (!identical(names(mice_data), MI_DATA_COLUMNS) ||
      anyNA(mice_data[, MI_COMPLETE_PREDICTORS, drop = FALSE]) ||
      anyNA(mice_data[, NONIMPUTED_EXPOSURES, drop = FALSE]) ||
      anyNA(mice_data[, NONIMPUTED_TUPLE_AUXILIARIES, drop = FALSE]) ||
      anyNA(mice_data$hospital_mortality)) {
    stop("D060 MI input completeness invariant failed for ", database)
  }
  missing_counts <- vapply(
    MI_IMPUTED_CONTINUOUS,
    function(variable) sum(is.na(mice_data[[variable]])), integer(1L)
  )
  if (any(missing_counts <= 0L)) {
    stop("Every D060 imputation target must have missing values in ", database)
  }
  where <- matrix(
    FALSE, nrow = nrow(mice_data), ncol = ncol(mice_data),
    dimnames = list(NULL, names(mice_data))
  )
  where[, MI_IMPUTED_CONTINUOUS] <- is.na(
    as.matrix(mice_data[, MI_IMPUTED_CONTINUOUS, drop = FALSE])
  )
  if (any(where[, setdiff(MI_DATA_COLUMNS, MI_IMPUTED_CONTINUOUS), drop = FALSE])) {
    stop("where matrix attempts to impute a D060 non-imputed variable.")
  }

  set.seed(seed)
  mids <- mice::mice(
    data = mice_data, m = MI_M, maxit = MI_MAXIT,
    method = mi_spec$method,
    predictorMatrix = mi_spec$predictor_matrix,
    post = mi_spec$post,
    where = where,
    visitSequence = MI_IMPUTED_CONTINUOUS,
    printFlag = FALSE,
    seed = seed,
    donors = MI_DONORS,
    ridge = MI_RIDGE,
    remove.constant = FALSE,
    remove.collinear = FALSE
  )
  if (!inherits(mids, "mids") || mids$m != MI_M || mids$iteration != MI_MAXIT ||
      !identical(unname(mids$method[MI_DATA_COLUMNS]),
        unname(mi_spec$method[MI_DATA_COLUMNS])) ||
      !same_matrix_values(
        mids$predictorMatrix, mi_spec$predictor_matrix,
        require_rownames = TRUE
      ) || !same_matrix_values(mids$where, where, require_rownames = FALSE)) {
    stop("mice object differs from the frozen D060 specification for ", database)
  }

  chain_summary <- extract_chain_summary(mids, database, missing_counts)
  chain_trace <- extract_chain_trace(mids, database, missing_counts)
  logged_events <- extract_logged_events(mids, database)
  coefficient_draws <- list()
  completeness_qc <- list()
  shift_qc <- list()
  distributions <- list()
  draw_index <- 0L
  completeness_index <- 0L
  shift_index <- 0L
  distribution_index <- 0L

  for (scenario_index in seq_len(nrow(MNAR_SCENARIOS))) {
    scenario <- MNAR_SCENARIOS$scenario[[scenario_index]]
    delta_sd <- MNAR_SCENARIOS$delta_sd[[scenario_index]]
    imputed_values <- setNames(
      lapply(MI_IMPUTED_CONTINUOUS, function(x) numeric()),
      MI_IMPUTED_CONTINUOUS
    )

    for (imputation in seq_len(MI_M)) {
      completed <- as.data.table(mice::complete(mids, action = imputation))
      if (!identical(names(completed), MI_DATA_COLUMNS) ||
          nrow(completed) != nrow(original)) {
        stop("Completed imputation schema/cardinality changed for ", database)
      }
      shifted <- apply_delta_pattern(
        completed, original, delta_sd, database, scenario
      )
      completed <- shifted$data
      shift_index <- shift_index + 1L
      shift_qc[[shift_index]] <- cbind(
        shifted$qc, imputation = imputation
      )

      outcome_unchanged <- same_numeric(
        completed$hospital_mortality, original$hospital_mortality
      )
      nonimputed_unchanged <- all(vapply(
        c(MI_COMPLETE_PREDICTORS, "hospital_mortality"),
        function(variable) same_numeric(
          completed[[variable]], original[[variable]]
        ), logical(1L)
      ))
      observed_covariates_unchanged <- all(vapply(
        MI_IMPUTED_CONTINUOUS, function(variable) {
          observed <- !is.na(original[[variable]])
          same_numeric(
            completed[[variable]][observed], original[[variable]][observed]
          )
        }, logical(1L)
      ))
      all_model_covariates_complete <- all(vapply(
        MI_COVARIATES, function(variable) {
          !anyNA(completed[[variable]]) &&
            all(is.finite(as.numeric(completed[[variable]])))
        }, logical(1L)
      ))
      if (!outcome_unchanged || !nonimputed_unchanged ||
          !observed_covariates_unchanged || !all_model_covariates_complete) {
        stop("MI/MNAR invariants failed for ", database, "/", scenario,
          "/imputation ", imputation)
      }
      assert_binary_outcome(as.integer(completed$hospital_mortality))

      for (variable in MI_IMPUTED_CONTINUOUS) {
        missing <- is.na(original[[variable]])
        imputed_values[[variable]] <- c(
          imputed_values[[variable]], as.numeric(completed[[variable]][missing])
        )
      }

      design_signature_pass <- TRUE
      for (model_id in c("S2", "S3", "S2M")) {
        design <- build_design_matrix(completed, model_id, transform_bundle)
        expected_columns <- main_model_bundle$fits[[model_id]]$design_columns
        if (!identical(colnames(design), expected_columns)) {
          design_signature_pass <- FALSE
          stop("MI design differs from main locked model for ", model_id)
        }
        if ((model_id %in% c("S3", "S2M")) !=
            ("smp_per_5" %in% colnames(design)) ||
            (model_id == "S2" && "smp_per_5" %in% colnames(design))) {
          stop("Linear sMP/5 model-term contract failed for ", model_id)
        }
        fit <- fit_model(
          design, as.integer(completed$hospital_mortality), model_id,
          allow_nonestimable = FALSE
        )
        if (!identical(fit$status, "ESTIMABLE") ||
            any(!is.finite(fit$coefficients)) ||
            any(!is.finite(fit$vcov))) {
          stop("An MI association model is not estimable: ", database, "/",
            scenario, "/", model_id, "/", imputation)
        }
        variance <- diag(fit$vcov)
        names(variance) <- rownames(fit$vcov)
        draw_index <- draw_index + 1L
        coefficient_draws[[draw_index]] <- data.table(
          database = database, scenario = scenario,
          imputation = imputation, model_id = model_id,
          term = names(fit$coefficients),
          estimate = as.numeric(fit$coefficients),
          within_variance = as.numeric(variance[names(fit$coefficients)]),
          n = fit$n, events = fit$events,
          model_status = fit$status
        )
      }
      completeness_index <- completeness_index + 1L
      completeness_qc[[completeness_index]] <- data.table(
        database = database, scenario = scenario, imputation = imputation,
        n = nrow(completed), events = sum(completed$hospital_mortality),
        all_imputed_covariates_complete = all_model_covariates_complete,
        outcome_never_imputed_or_changed = outcome_unchanged,
        complete_predictors_and_tuple_never_imputed_or_changed =
          nonimputed_unchanged,
        observed_covariate_cells_unchanged = observed_covariates_unchanged,
        exact_locked_design_columns = design_signature_pass,
        performance_estimated = FALSE
      )
    }

    for (variable in MI_IMPUTED_CONTINUOUS) {
      observed <- original[[variable]][!is.na(original[[variable]])]
      distribution_index <- distribution_index + 1L
      distributions[[distribution_index]] <- summarize_values(
        observed, database, scenario, variable, "observed_original",
        "continuous"
      )
      distribution_index <- distribution_index + 1L
      distributions[[distribution_index]] <- summarize_values(
        imputed_values[[variable]], database, scenario, variable,
        "imputed_cells_pooled_across_50", "continuous"
      )
    }
  }

  coefficient_draws <- rbindlist(coefficient_draws)
  pooled <- rbindlist(lapply(MNAR_SCENARIOS$scenario, function(scenario) {
    rbindlist(lapply(c("S2", "S3", "S2M"), function(model_id) {
      rubin_pool(coefficient_draws, database, scenario, model_id)
    }))
  }))
  if (anyDuplicated(pooled[, .(database, scenario, model_id, term)]) ||
      any(!is.finite(pooled$estimate)) ||
      any(!is.finite(pooled$total_variance))) {
    stop("Pooled MI coefficient invariant failed for ", database)
  }

  list(
    database = database,
    seed = seed,
    analysis_ids = original$analysis_id,
    mids = mids,
    missing_counts = missing_counts,
    coefficient_draws = coefficient_draws,
    pooled_coefficients = pooled,
    completeness_qc = rbindlist(completeness_qc),
    shift_qc = rbindlist(shift_qc),
    distributions = rbindlist(distributions),
    chain_summary = chain_summary,
    chain_trace = chain_trace,
    logged_events = logged_events
  )
}

private_out <- file.path(PRIVATE_ROOT, "missing_data")
aggregate_out <- file.path(AGGREGATE_ROOT, "missing_data")
qc_out <- file.path(QC_ROOT, "missing_data")
completion_gate <- file.path(
  qc_out, "phase3d_missing_data_sensitivities_complete_v1.csv"
)
private_rds_path <- file.path(
  private_out, "association_focused_multiple_imputation_v1.rds"
)
aggregate_paths <- c(
  specification = file.path(aggregate_out, "mi_specification_v1.csv"),
  method = file.path(aggregate_out, "mi_method_specification_v1.csv"),
  predictor_matrix = file.path(aggregate_out, "mi_predictor_matrix_v1.csv"),
  population_missingness = file.path(
    aggregate_out, "mi_population_missingness_v1.csv"
  ),
  pooled_coefficients = file.path(
    aggregate_out, "mi_Rubin_pooled_coefficients_v1.csv"
  ),
  smp_contrasts = file.path(
    aggregate_out, "mi_Rubin_pooled_smp_per5_contrasts_v1.csv"
  ),
  distributions = file.path(
    aggregate_out, "mi_observed_imputed_distributions_v1.csv"
  ),
  chain_trace = file.path(aggregate_out, "mi_chain_trace_v1.csv"),
  chain_summary = file.path(aggregate_out, "mi_chain_trace_summary_v1.csv"),
  logged_events = file.path(aggregate_out, "mi_logged_events_v1.csv")
)
qc_paths <- c(
  input_gate = file.path(qc_out, "input_gate_hash_validation.csv"),
  sd_reproduction = file.path(qc_out, "D060_mimic_sd_reproduction_QC.csv"),
  completion = file.path(qc_out, "imputation_completion_QC.csv"),
  mnar_shift = file.path(qc_out, "MNAR_delta_shift_QC.csv"),
  summary = file.path(qc_out, "missing_data_sensitivity_QC.md")
)
aggregate_manifest_path <- file.path(qc_out, "aggregate_output_manifest_v1.csv")
planned_outputs <- c(
  private_rds_path, aggregate_paths, qc_paths, aggregate_manifest_path,
  completion_gate
)
if (any(file.exists(planned_outputs))) {
  stop("A planned Phase 3d output already exists; refusing overwrite.")
}
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

mimic_mi <- run_database_mi(
  mimic_analysis, "MIMIC-IV_v3.1", unname(MI_SEEDS[["MIMIC-IV_v3.1"]])
)
eicu_mi <- run_database_mi(
  eicu_analysis, "eICU-CRD_v2.0", unname(MI_SEEDS[["eICU-CRD_v2.0"]])
)

pooled_coefficients <- rbindlist(list(
  mimic_mi$pooled_coefficients, eicu_mi$pooled_coefficients
))
smp_contrasts <- copy(pooled_coefficients[
  model_id %in% c("S3", "S2M") & term == "smp_per_5"
])
smp_contrasts[, `:=`(
  contrast_id = "smp_plus_5_J_per_min",
  from_value_J_per_min = 0,
  to_value_J_per_min = 5,
  log_odds_difference = estimate,
  contrast_odds_ratio = odds_ratio,
  contrast_ci_lower = odds_ratio_ci_lower,
  contrast_ci_upper = odds_ratio_ci_upper,
  exposure_form = "linear_smp_per_5_J_per_min",
  external_validation_interpretation = "NOT_APPLICABLE_ASSOCIATION_ONLY"
)]
setcolorder(smp_contrasts, c(
  "database", "scenario", "model_id", "contrast_id",
  "from_value_J_per_min", "to_value_J_per_min", "exposure_form",
  "log_odds_difference", "standard_error", "degrees_freedom", "p_value",
  "contrast_odds_ratio", "contrast_ci_lower", "contrast_ci_upper",
  setdiff(names(smp_contrasts), c(
    "database", "scenario", "model_id", "contrast_id",
    "from_value_J_per_min", "to_value_J_per_min", "exposure_form",
    "log_odds_difference", "standard_error", "degrees_freedom", "p_value",
    "contrast_odds_ratio", "contrast_ci_lower", "contrast_ci_upper"
  ))
))
if (nrow(smp_contrasts) != 2L * nrow(MNAR_SCENARIOS) * 2L ||
    any(!smp_contrasts$model_id %in% c("S3", "S2M"))) {
  stop("Expected one pooled sMP/5 contrast for S3 and S2M per database/scenario.")
}

population_missingness <- rbindlist(lapply(list(
  `MIMIC-IV_v3.1` = mimic_analysis,
  `eICU-CRD_v2.0` = eicu_analysis
), function(x) {
  data.table(
    variable = MI_DATA_COLUMNS,
    population_n = nrow(x),
    missing_n = vapply(MI_DATA_COLUMNS, function(variable) {
      sum(is.na(x[[variable]]))
    }, integer(1L)),
    event_n = sum(x$hospital_mortality),
    outcome_known_n = sum(!is.na(x$hospital_mortality))
  )
}), idcol = "database")
population_missingness[, `:=`(
  missing_fraction = missing_n / population_n,
  imputed_under_D060 = variable %in% MI_IMPUTED_CONTINUOUS
)]
if (any(population_missingness[
  !variable %in% MI_IMPUTED_CONTINUOUS, missing_n != 0L
])) stop("D060 non-imputed variable has missing values.")

imputation_completion_qc <- rbindlist(list(
  mimic_mi$completeness_qc, eicu_mi$completeness_qc
))
completion_flags <- unlist(imputation_completion_qc[, .(
      all_imputed_covariates_complete,
      outcome_never_imputed_or_changed,
      complete_predictors_and_tuple_never_imputed_or_changed,
      observed_covariate_cells_unchanged,
      exact_locked_design_columns
    )], use.names = FALSE)
if (nrow(imputation_completion_qc) !=
    2L * nrow(MNAR_SCENARIOS) * MI_M ||
    any(!completion_flags) || any(imputation_completion_qc$performance_estimated)) {
  stop("Final MI completion/invariance QC failed.")
}

raw_mnar_shift_qc <- rbindlist(list(mimic_mi$shift_qc, eicu_mi$shift_qc))
shift_constancy <- raw_mnar_shift_qc[, .(
  missing_count_values = uniqueN(missing_cells_in_one_imputation),
  frozen_sd_values = uniqueN(frozen_mimic_sd),
  signed_shift_values = uniqueN(signed_absolute_shift),
  lower_bound_values = uniqueN(lower_bound),
  upper_bound_values = uniqueN(upper_bound)
), by = .(database, scenario, variable, delta_sd)]
if (any(as.matrix(shift_constancy[
  , .SD, .SDcols = patterns("_values$")
]) != 1L)) {
  stop("MNAR shift constants changed across imputations.")
}
mnar_shift_qc <- raw_mnar_shift_qc[
  , .(
    imputations = uniqueN(imputation),
    missing_cells_per_imputation = first(missing_cells_in_one_imputation),
    shifted_imputed_cells_total = sum(missing_cells_in_one_imputation),
    frozen_mimic_sd = first(frozen_mimic_sd),
    signed_absolute_shift = first(signed_absolute_shift),
    lower_bound = first(lower_bound), upper_bound = first(upper_bound),
    proposed_below_lower_total = sum(proposed_below_lower_n),
    proposed_above_upper_total = sum(proposed_above_upper_n),
    boundary_truncated_total = sum(boundary_truncated_n),
    observed_cells_shifted_total = sum(observed_cells_shifted_n)
  ), by = .(database, scenario, variable, delta_sd)
]
if (any(mnar_shift_qc$imputations != MI_M) ||
    any(mnar_shift_qc$observed_cells_shifted_total != 0L)) {
  stop("MNAR shift accounting QC failed.")
}

distribution_summary <- rbindlist(list(
  mimic_mi$distributions, eicu_mi$distributions
))
chain_trace_summary <- rbindlist(list(
  mimic_mi$chain_summary, eicu_mi$chain_summary
))
iteration_level_chain_trace <- rbindlist(list(
  mimic_mi$chain_trace, eicu_mi$chain_trace
))
expected_iteration_trace_n <-
  2L * length(MI_IMPUTED_CONTINUOUS) * MI_M * MI_MAXIT
if (nrow(iteration_level_chain_trace) != expected_iteration_trace_n ||
    anyDuplicated(iteration_level_chain_trace[
      , .(database, variable, chain, iteration)
    ])) {
  stop("Combined iteration-level mice chain trace invariant failed.")
}
logged_events <- rbindlist(list(
  mimic_mi$logged_events, eicu_mi$logged_events
), use.names = TRUE, fill = TRUE)

mi_specification <- rbindlist(list(
  data.table(
    specification = c(
      "decision", "analysis_scope", "databases", "imputations", "iterations",
      "pmm_donors", "pmm_ridge", "MIMIC_seed", "eICU_seed",
      "imputed_variables", "nonimputed_complete_predictors",
      "complete_tuple_auxiliary_predictors",
      "PBW_normalized_auxiliary_rule",
      "outcome_role", "models", "transformation_rule",
      "performance_rule", "eICU_interpretation", "PF_MNAR_shift",
      "binary_MNAR_shift", "MNAR_status"
    ),
    value = c(
      "D060", "association_focused_covariate_MI_only",
      "database_specific_MIMIC_and_eICU", MI_M, MI_MAXIT,
      MI_DONORS, MI_RIDGE, MI_SEEDS[["MIMIC-IV_v3.1"]],
      MI_SEEDS[["eICU-CRD_v2.0"]],
      paste(MI_IMPUTED_CONTINUOUS, collapse = ";"),
      paste(MI_COMPLETE_PREDICTORS, collapse = ";"),
      paste(NONIMPUTED_TUPLE_AUXILIARIES, collapse = ";"),
      "vt_per_pbw_and_smp_per_pbw_excluded_because_PBW_not_complete",
      "predictor_only_never_imputed", "S2;S3;S2M",
      "D054_MIMIC_frozen_after_imputation;linear_smp_per5",
      "NO_MI_PERFORMANCE_OF_ANY_KIND",
      "database_specific_association_replication_not_external_validation",
      "FALSE", "NOT_APPLICABLE_VASOPRESSOR_COMPLETE",
      "exploratory_delta_pattern"
    )
  ),
  MNAR_SCENARIOS[, .(
    specification = paste0("scenario_", scenario),
    value = paste0("delta_sd=", delta_sd, ";class=", assumption_class)
  )],
  MNAR_RULES[, .(
    specification = paste0("MNAR_rule_", variable),
    value = paste0(
      "sd=", format(mimic_observed_sd, digits = 14),
      ";direction=", adverse_direction,
      ";bounds=", format(lower_bound, scientific = TRUE), "..",
      format(upper_bound, scientific = TRUE), ";", boundary_rule
    )
  )]
), use.names = TRUE)

input_gate_qc <- data.table(
  check = c(
    "authorization_checkpoint_PASS",
    "complete_analysis_manifest_hashes_match",
    "self_script_hash_matches_manifest",
    "parameter_gate_PASS_and_hash_chain",
    "phase3a_outcome_gate_formally_unblinded_PASS",
    "phase3b_main_model_gate_PASS",
    "outcome_RDS_hashes_match",
    "main_model_RDS_hash_matches",
    "D020_D054_D060_authorized",
    "mice_3.19.0_locked",
    "exact_predictor_outcome_ID_sets",
    "D060_complete_predictors_have_no_missing",
    "complete_tuple_auxiliaries_predictor_only",
    "PBW_normalized_auxiliaries_excluded",
    "D060_MIMIC_SDs_reproduced",
    "S2_S3_S2M_locked_design_contract",
    "outcome_predictor_never_imputed",
    "iteration_level_chain_trace_complete",
    "no_MI_performance_estimated"
  ),
  pass = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    all(population_missingness[
      !variable %in% MI_IMPUTED_CONTINUOUS, missing_n == 0L
    ]),
    all(NONIMPUTED_TUPLE_AUXILIARIES %in% MI_DATA_COLUMNS) &&
      !any(NONIMPUTED_TUPLE_AUXILIARIES %in% MI_COVARIATES),
    !any(c("vt_per_pbw", "smp_per_pbw") %in% MI_DATA_COLUMNS),
    all(sd_reproduction_qc$pass), TRUE,
    all(imputation_completion_qc$outcome_never_imputed_or_changed),
    nrow(iteration_level_chain_trace) == expected_iteration_trace_n,
    all(!imputation_completion_qc$performance_estimated)
  )
)
if (any(!input_gate_qc$pass)) stop("Final Phase 3d input-gate QC failed.")

private_bundle <- list(
  artifact_version = "association_focused_multiple_imputation_v1",
  config_version = LOCKED$version,
  decision_id = "D060",
  checkpoint_sha256 = sha256_file(checkpoint_path),
  analysis_manifest_sha256 = sha256_file(analysis_manifest_path),
  parameter_gate_sha256 = sha256_file(parameter_gate_path),
  outcome_gate_sha256 = sha256_file(outcome_gate_path),
  main_model_gate_sha256 = sha256_file(main_model_gate_path),
  script_sha256 = sha256_file(script_path),
  model_utils_sha256 = sha256_file(model_utils_path),
  analysis_utils_sha256 = sha256_file(analysis_utils_path),
  transform_bundle = transform_bundle,
  method = mi_spec$method,
  predictor_matrix = mi_spec$predictor_matrix,
  scenarios = MNAR_SCENARIOS,
  mnar_rules = MNAR_RULES,
  complete_tuple_auxiliary_predictors = NONIMPUTED_TUPLE_AUXILIARIES,
  PBW_normalized_auxiliary_predictors_used = FALSE,
  PBW_normalized_auxiliary_exclusion_reason =
    "PBW_not_complete_in_primary_tuple_population",
  mimic = list(
    seed = mimic_mi$seed, analysis_ids = mimic_mi$analysis_ids,
    mids = mimic_mi$mids, coefficient_draws = mimic_mi$coefficient_draws
  ),
  eicu = list(
    seed = eicu_mi$seed, analysis_ids = eicu_mi$analysis_ids,
    mids = eicu_mi$mids, coefficient_draws = eicu_mi$coefficient_draws
  ),
  pooled_coefficients = pooled_coefficients,
  association_only = TRUE,
  eicu_role = "database_specific_association_replication",
  mi_discrimination_computed = FALSE,
  mi_calibration_computed = FALSE,
  mi_overall_performance_computed = FALSE,
  mi_external_validation_computed = FALSE
)
attr(private_bundle, "rebuild_metadata") <- list(
  private_row_level = TRUE,
  contains_outcome_in_mids_predictor = TRUE,
  outcome_imputed = FALSE,
  exposure_tuple_imputed = FALSE,
  mnar_shift_observed_cells = FALSE,
  mnar_scenarios_exploratory = TRUE
)

atomic_save_rds_new(private_bundle, private_rds_path)
atomic_fwrite_new(mi_specification, aggregate_paths[["specification"]])
atomic_fwrite_new(method_specification, aggregate_paths[["method"]])
atomic_fwrite_new(
  predictor_matrix_specification, aggregate_paths[["predictor_matrix"]]
)
atomic_fwrite_new(
  population_missingness, aggregate_paths[["population_missingness"]]
)
atomic_fwrite_new(
  pooled_coefficients, aggregate_paths[["pooled_coefficients"]]
)
atomic_fwrite_new(smp_contrasts, aggregate_paths[["smp_contrasts"]])
atomic_fwrite_new(distribution_summary, aggregate_paths[["distributions"]])
atomic_fwrite_new(
  iteration_level_chain_trace, aggregate_paths[["chain_trace"]]
)
atomic_fwrite_new(chain_trace_summary, aggregate_paths[["chain_summary"]])
atomic_fwrite_new(logged_events, aggregate_paths[["logged_events"]])
atomic_fwrite_new(input_gate_qc, qc_paths[["input_gate"]])
atomic_fwrite_new(sd_reproduction_qc, qc_paths[["sd_reproduction"]])
atomic_fwrite_new(imputation_completion_qc, qc_paths[["completion"]])
atomic_fwrite_new(mnar_shift_qc, qc_paths[["mnar_shift"]])

summary_lines <- c(
  "# Association-focused missing-data sensitivity QC",
  "",
  paste0("- Decision: D060; configuration: ", LOCKED$version),
  paste0("- Imputation: m=", MI_M, ", maxit=", MI_MAXIT,
    ", PMM donors=", MI_DONORS, "."),
  paste0("- Seeds: MIMIC=", MI_SEEDS[["MIMIC-IV_v3.1"]],
    "; eICU=", MI_SEEDS[["eICU-CRD_v2.0"]], "."),
  paste0("- Imputed only: ", paste(MI_IMPUTED_CONTINUOUS, collapse = ", "), "."),
  "- Outcome, age, sex, P/F, vasopressor, delta pressure, RR, sMP, PEEP, and resistive pressure were predictors and were never imputed.",
  "- PEEP and resistive pressure were complete-tuple auxiliary predictors only and never entered the S2/S3/S2M outcome-model design.",
  "- VT/PBW and sMP/PBW were excluded as imputation auxiliaries because PBW was not complete in the primary tuple population.",
  "- S2, S3, and S2M use the D054 frozen transformations; S3/S2M sMP is linear per 5 J/min.",
  "- Rubin pooling includes all coefficients plus the S3 and S2M sMP/5 clinical contrasts.",
  "- Iteration-level chain means/variances and the last-five-iteration summaries were exported without row identifiers.",
  "- The 0.5-SD and 1.0-SD adverse delta-pattern scenarios shift imputed cells only and are exploratory.",
  "- P/F was not shifted; a binary adverse-state shift was inapplicable because vasopressor was complete.",
  "- No MI discrimination, calibration, overall performance, or external-validation estimate was computed.",
  "- eICU MI results are database-specific association replication, not locked external performance.",
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
  row_level_identifier_columns = FALSE,
  performance_output = FALSE
)
atomic_fwrite_new(aggregate_manifest, aggregate_manifest_path)

identifier_headers <- c(
  "analysis_id", "patient_cluster_id", "stay_id", "subject_id", "hadm_id",
  "patientunitstayid", "patienthealthsystemstayid", "person_key", "hospitalid"
)
public_csv_paths <- c(
  aggregate_paths, qc_paths[names(qc_paths) != "summary"],
  aggregate_manifest_path
)
public_headers <- unique(unlist(lapply(public_csv_paths, function(path) {
  names(fread(path, nrows = 0L, showProgress = FALSE))
})))
if (any(public_headers %in% identifier_headers)) {
  stop("A row-level identifier header entered an aggregate/QC CSV.")
}
if (!identical(tail(readLines(qc_paths[["summary"]], warn = FALSE), 1L),
  "BUILD_COMPLETE")) stop("Phase 3d summary sentinel is missing.")

required_output_paths <- c(
  private_rds_path, aggregate_paths, qc_paths, aggregate_manifest_path
)
completion <- data.table(
  field = c(
    "status", "config_version", "completed_at", "script_sha256",
    "checkpoint_sha256", "analysis_script_manifest_sha256",
    "parameter_freeze_gate_sha256", "outcome_gate_sha256",
    "main_model_gate_sha256", "parameter_rds_sha256",
    "mimic_model_frame_rds_sha256", "eicu_model_frame_rds_sha256",
    "mimic_outcome_rds_sha256", "eicu_outcome_rds_sha256",
    "main_model_rds_sha256", "private_MI_rds_sha256",
    "aggregate_manifest_sha256", "decision_id", "MI_scope",
    "imputations", "iterations", "PMM_donors", "PMM_ridge",
    "MIMIC_seed", "eICU_seed", "imputed_variables",
    "complete_tuple_auxiliary_predictors",
    "PBW_normalized_auxiliary_predictors_used",
    "PBW_normalized_auxiliary_exclusion_reason",
    "outcome_used_as_predictor", "outcome_imputed",
    "ventilator_tuple_imputed", "models_pooled",
    "smp_contrast_models", "smp_form", "frozen_transformations_used",
    "MNAR_scenarios", "MNAR_shifted_cells", "PF_shifted",
    "binary_adverse_shift_applied", "MNAR_interpretation",
    "MI_discrimination_reported", "MI_calibration_reported",
    "MI_overall_performance_reported", "MI_external_validation_reported",
    "eICU_interpretation", "iteration_level_chain_trace_exported",
    "all_input_gate_checks_pass",
    "all_imputation_invariants_pass", "all_D060_SDs_reproduced",
    "all_required_outputs_present", "summary_sentinel"
  ),
  value = c(
    "PASS", LOCKED$version, format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    sha256_file(script_path), sha256_file(checkpoint_path),
    sha256_file(analysis_manifest_path), sha256_file(parameter_gate_path),
    sha256_file(outcome_gate_path), sha256_file(main_model_gate_path),
    sha256_file(parameter_artifact_paths$parameter),
    sha256_file(parameter_artifact_paths$mimic_frame),
    sha256_file(parameter_artifact_paths$eicu_frame),
    sha256_file(mimic_outcome_path), sha256_file(eicu_outcome_path),
    sha256_file(main_model_rds_path), sha256_file(private_rds_path),
    sha256_file(aggregate_manifest_path), "D060",
    "association_focused_database_specific_only", MI_M, MI_MAXIT,
    MI_DONORS, MI_RIDGE, MI_SEEDS[["MIMIC-IV_v3.1"]],
    MI_SEEDS[["eICU-CRD_v2.0"]],
    paste(MI_IMPUTED_CONTINUOUS, collapse = ";"),
    paste(NONIMPUTED_TUPLE_AUXILIARIES, collapse = ";"), FALSE,
    "PBW_not_complete_in_primary_tuple_population", TRUE, FALSE, FALSE,
    "S2;S3;S2M", "S3;S2M", "linear_per_5_J_per_min", TRUE,
    "MAR;MNAR_adverse_0.5SD;MNAR_adverse_1.0SD",
    "imputed_GCS_MAP_platelet_creatinine_cells_only", FALSE, FALSE,
    "EXPLORATORY", FALSE, FALSE, FALSE, FALSE,
    "database_specific_association_replication_not_external_validation",
    TRUE,
    all(input_gate_qc$pass),
    all(completion_flags),
    all(sd_reproduction_qc$pass), all(file.exists(required_output_paths)),
    "BUILD_COMPLETE"
  )
)
if (anyDuplicated(completion$field)) stop("Duplicate Phase 3d gate field.")
completion_tmp <- paste0(completion_gate, ".tmp.", Sys.getpid())
unlink(completion_tmp, force = TRUE)
fwrite(completion, completion_tmp)
if (file.exists(completion_gate) ||
    !file.rename(completion_tmp, completion_gate)) {
  unlink(completion_tmp, force = TRUE)
  stop("Could not atomically publish Phase 3d missing-data PASS gate.")
}

message("Association-focused missing-data sensitivities complete.")
message("  m/maxit: ", MI_M, "/", MI_MAXIT)
message("  models: S2, S3, S2M")
message("  MI performance reported: FALSE")
message("  gate: ", completion_gate)
