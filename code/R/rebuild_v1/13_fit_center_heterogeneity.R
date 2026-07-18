#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: locked eICU center heterogeneity and
# leave-one-hospital-out influence analyses.
#
# GOVERNANCE WARNING
# ------------------
# This script is outcome-bearing and must never be sourced. It may be executed
# only after the formal outcome-unblinding checkpoint, the Phase 3a rebuilt-
# outcome PASS gate, and the Phase 3b locked-model PASS gate all exist and match
# the authorized SHA256 chain. Before authorization, syntax checking is limited
# to parse(file=...). No model form, threshold, fallback, or reporting rule may
# be changed after outcome access.

suppressPackageStartupMessages(library(data.table))
options(warn = 1)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/13_fit_center_heterogeneity.R", mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
project_from_script <- normalizePath(
  file.path(script_dir, "..", "..", ".."), mustWork = TRUE
)
expected_config_version <- "1.0.1"

DECISION_ID <- "D061"
GLMER_OPTIMIZER <- "bobyqa"
GLMER_NAGQ <- 1L
GLMER_MAXFUN <- 200000L
SINGULAR_TOLERANCE <- 1e-4
FALLBACK_MIN_N <- 30L
FALLBACK_MIN_EVENTS <- 5L
FALLBACK_MIN_NONEVENTS <- 5L
FALLBACK_MIN_META_HOSPITALS <- 5L
INFERENTIAL_MIN_HOSPITALS <- 20L
LOHO_MODELS <- c("S2", "S3")
LOHO_COMPARISON <- "S3_minus_S2"

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
  setNames(vapply(z, function(v) as.character(v[[1L]]), character(1L)), names(z))
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
  if (!startsWith(resolved, prefix)) stop("Output is outside the project root.")
  substring(resolved, nchar(project_from_script) + 2L)
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

quantile_summary <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(
      minimum = NA_real_, q1 = NA_real_, median = NA_real_,
      q3 = NA_real_, maximum = NA_real_
    ))
  }
  q <- as.numeric(stats::quantile(
    x, probs = c(0, 0.25, 0.5, 0.75, 1),
    names = FALSE, type = 2L
  ))
  setNames(q, c("minimum", "q1", "median", "q3", "maximum"))
}

safe_reason <- function(x) {
  x <- unique(trimws(as.character(x)))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return("")
  paste(substr(x, 1L, 500L), collapse = " | ")
}

# ---------------------------------------------------------------------------
# Authorization is the first project artifact opened. No config, model gate,
# predictor artifact, private model bundle, prediction bundle, or outcome RDS
# is read before this checkpoint and its direct hash locks are verified.
# ---------------------------------------------------------------------------

checkpoint_path <- file.path(
  project_from_script, "analysis_rebuild_v1", "qc", "unblinding",
  "outcome_unblinding_checkpoint_v1.csv"
)
if (!file.exists(checkpoint_path)) {
  stop(
    "Center modeling is not authorized: missing checkpoint ", checkpoint_path,
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
parameter_script_path <- file.path(
  script_dir, "07_freeze_predictor_parameters.R"
)
selection_script_path <- file.path(script_dir, "07b_build_selection_weights.R")
model_utils_path <- file.path(script_dir, "08_model_utils.R")
analysis_utils_path <- file.path(script_dir, "08a_locked_analysis_utils.R")
outcome_script_path <- file.path(script_dir, "09_extract_rebuilt_outcomes.R")
main_model_script_path <- file.path(script_dir, "10_fit_locked_models.R")
decision_log_path <- file.path(
  project_from_script, "docs", "rebuild_v1", "analysis_decision_log.md"
)
locked_checkpoint_files <- c(
  config_script_sha256 = config_path,
  parameter_freeze_script_sha256 = parameter_script_path,
  selection_weights_script_sha256 = selection_script_path,
  model_utils_script_sha256 = model_utils_path,
  model_analysis_utils_script_sha256 = analysis_utils_path,
  outcome_extraction_script_sha256 = outcome_script_path,
  model_analysis_script_sha256 = main_model_script_path,
  center_heterogeneity_script_sha256 = script_path,
  analysis_decision_log_sha256 = decision_log_path
)
if (any(!file.exists(locked_checkpoint_files))) {
  stop("An authorized center-analysis dependency is missing.")
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
  character(1L), label = "manifested analysis script"
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
  stop("Authorized manifest must include exact 08, 08a, 09, 10, and 13 scripts.")
}
self_manifest_index <- match(normalizePath(script_path), analysis_paths)
if (is.na(self_manifest_index) ||
    !identical(analysis_hashes[[self_manifest_index]], sha256_file(script_path))) {
  stop("This center-analysis script is not self-hash-locked by the manifest.")
}

# ---------------------------------------------------------------------------
# Verify every upstream gate and artifact hash before any row-level RDS read.
# ---------------------------------------------------------------------------

parameter_gate_path <- file.path(
  QC_ROOT, "parameter_freeze", "phase2e_parameter_freeze_complete_v1.csv"
)
selection_gate_path <- file.path(
  QC_ROOT, "selection_weights", "phase2d_selection_weights_complete_v1.csv"
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
  parameter_gate_path, selection_gate_path, outcome_gate_path,
  main_model_gate_path, access_receipt_path
)
if (any(!file.exists(gate_paths))) {
  stop("A required upstream PASS gate/access receipt is missing.")
}

parameter_gate <- read_completion_gate(parameter_gate_path, "parameter gate")
selection_gate <- read_completion_gate(selection_gate_path, "selection gate")
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
  parameter_gate, "script_sha256", sha256_file(parameter_script_path),
  "parameter gate"
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
  selection_gate, "script_sha256", sha256_file(selection_script_path),
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
  outcome_gate, "script_sha256", sha256_file(outcome_script_path), "outcome gate"
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

require_map_value(main_model_gate, "status", "PASS", "main-model gate")
require_map_value(
  main_model_gate, "config_version", LOCKED$version, "main-model gate"
)
for (field in c(
  "all_input_gate_checks_pass", "all_exact_join_checks_pass",
  "all_required_models_or_allowed_S5_pass", "all_required_outputs_present"
)) require_map_value(main_model_gate, field, "TRUE", "main-model gate")
require_map_value(
  main_model_gate, "summary_sentinel", "BUILD_COMPLETE", "main-model gate"
)
require_map_value(
  main_model_gate, "center_heterogeneity_implemented", "FALSE",
  "main-model gate"
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
  access_receipt, "status", "OUTCOME_ACCESS_INITIATED", "outcome receipt"
)
require_map_value(
  access_receipt, "config_version", LOCKED$version, "outcome receipt"
)
require_map_value(
  access_receipt, "checkpoint_sha256", sha256_file(checkpoint_path),
  "outcome receipt"
)
require_map_value(
  access_receipt, "script_sha256", sha256_file(outcome_script_path),
  "outcome receipt"
)

require_map_value(
  checkpoint, "parameter_freeze_gate_sha256", sha256_file(parameter_gate_path),
  "authorization checkpoint"
)
require_map_value(
  checkpoint, "selection_weights_gate_sha256", sha256_file(selection_gate_path),
  "authorization checkpoint"
)

decision_text <- paste(readLines(decision_log_path, warn = FALSE), collapse = "\n")
if (!grepl("\\| D061 \\|", decision_text)) {
  stop("Authorized decision log lacks D061.")
}

if (!requireNamespace("lme4", quietly = TRUE)) {
  stop("The locked center analysis requires the lme4 package.")
}
if (!requireNamespace("metafor", quietly = TRUE)) {
  stop("The locked center analysis requires the metafor package.")
}
lme4_version <- as.character(utils::packageVersion("lme4"))
metafor_version <- as.character(utils::packageVersion("metafor"))
require_map_value(
  checkpoint, "lme4_version", lme4_version, "authorization checkpoint"
)
require_map_value(
  checkpoint, "metafor_version", metafor_version, "authorization checkpoint"
)

source(model_utils_path)
source(analysis_utils_path)
required_utils <- c(
  "locked_model_specification", "build_design_matrix", "performance_vector",
  "assert_binary_outcome"
)
if (!all(vapply(required_utils, exists, logical(1L), mode = "function"))) {
  stop("Locked 08/08a utility interface is incomplete.")
}
model_spec <- locked_model_specification()
if (nrow(model_spec[model_id == "S3"]) != 1L ||
    !identical(model_spec[model_id == "S3", analysis_set], "primary_common") ||
    !identical(model_spec[model_id == "S3", design_type], "s0_smp_per_5")) {
  stop("D061 requires primary-common S3 with a linear sMP/5 term.")
}

eicu_frame_path <- resolve_project_path(
  require_map_value(
    parameter_gate, "eicu_model_frame_rds_path", label = "parameter gate"
  ), "eICU canonical model frame", require_relative = TRUE
)
require_map_value(
  parameter_gate, "eicu_model_frame_rds_sha256", sha256_file(eicu_frame_path),
  "parameter gate"
)
require_map_value(
  main_model_gate, "eicu_model_frame_rds_sha256", sha256_file(eicu_frame_path),
  "main-model gate"
)

model_bundle_path <- file.path(
  PRIVATE_ROOT, "locked_models", "mimic_locked_models_v1.rds"
)
prediction_bundle_path <- file.path(
  PRIVATE_ROOT, "locked_models", "locked_model_predictions_v1.rds"
)
eicu_outcome_path <- file.path(
  PRIVATE_ROOT, "outcomes", "eicu_rebuilt_outcomes_v1.rds"
)
if (any(!file.exists(c(
  model_bundle_path, prediction_bundle_path, eicu_outcome_path
)))) {
  stop("A checksum-gated private model/prediction/outcome artifact is missing.")
}
require_map_value(
  main_model_gate, "model_rds_sha256", sha256_file(model_bundle_path),
  "main-model gate"
)
require_map_value(
  main_model_gate, "prediction_rds_sha256", sha256_file(prediction_bundle_path),
  "main-model gate"
)
require_map_value(
  outcome_gate, "eicu_outcome_rds_sha256", sha256_file(eicu_outcome_path),
  "outcome gate"
)
require_map_value(
  main_model_gate, "eicu_outcome_rds_sha256", sha256_file(eicu_outcome_path),
  "main-model gate"
)

private_out <- file.path(PRIVATE_ROOT, "center_heterogeneity")
aggregate_out <- file.path(AGGREGATE_ROOT, "center_heterogeneity")
qc_out <- file.path(QC_ROOT, "center_heterogeneity")
completion_gate <- file.path(
  qc_out, "phase3e_center_heterogeneity_complete_v1.csv"
)
private_bundle_path <- file.path(
  private_out, "eicu_center_heterogeneity_private_v1.rds"
)
aggregate_paths <- c(
  specification = file.path(
    aggregate_out, "center_heterogeneity_specification_v1.csv"
  ),
  support = file.path(aggregate_out, "center_support_summary_v1.csv"),
  mixed_models = file.path(
    aggregate_out, "mixed_effects_heterogeneity_v1.csv"
  ),
  two_stage = file.path(
    aggregate_out, "two_stage_heterogeneity_v1.csv"
  ),
  loho_models = file.path(
    aggregate_out, "loho_model_influence_summary_v1.csv"
  ),
  loho_comparison = file.path(
    aggregate_out, "loho_comparison_influence_summary_v1.csv"
  )
)
qc_paths <- c(
  input_gate = file.path(qc_out, "input_gate_hash_validation.csv"),
  mixed_stability = file.path(qc_out, "mixed_model_stability_QC.csv"),
  fallback = file.path(qc_out, "two_stage_fallback_QC.csv"),
  loho = file.path(qc_out, "loho_influence_QC.csv"),
  summary = file.path(qc_out, "center_heterogeneity_QC.md")
)
aggregate_manifest_path <- file.path(qc_out, "aggregate_output_manifest_v1.csv")
planned_outputs <- c(
  private_bundle_path, aggregate_paths, qc_paths, aggregate_manifest_path,
  completion_gate
)
if (any(file.exists(planned_outputs))) {
  stop(
    "A planned center-analysis output already exists; refusing to overwrite it."
  )
}

# ---------------------------------------------------------------------------
# All checkpoint/manifest/upstream-gate/self-hash checks have now passed.
# Row-level predictor/model/prediction/outcome artifacts are opened only here.
# ---------------------------------------------------------------------------

model_bundle <- readRDS(model_bundle_path)
private_predictions <- as.data.table(readRDS(prediction_bundle_path))
eicu_frame <- as.data.table(readRDS(eicu_frame_path))
eicu_outcomes <- as.data.table(readRDS(eicu_outcome_path))

required_model_bundle <- c(
  "artifact_version", "config_version", "checkpoint_sha256",
  "parameter_gate_sha256", "outcome_gate_sha256",
  "analysis_manifest_sha256", "model_utils_sha256", "analysis_utils_sha256",
  "development_fits", "transform_bundle"
)
if (!is.list(model_bundle) ||
    length(setdiff(required_model_bundle, names(model_bundle))) ||
    !identical(model_bundle$artifact_version, "mimic_locked_models_v1") ||
    !identical(model_bundle$config_version, LOCKED$version) ||
    !identical(model_bundle$checkpoint_sha256, sha256_file(checkpoint_path)) ||
    !identical(
      model_bundle$parameter_gate_sha256, sha256_file(parameter_gate_path)
    ) ||
    !identical(model_bundle$outcome_gate_sha256, sha256_file(outcome_gate_path)) ||
    !identical(
      model_bundle$analysis_manifest_sha256,
      sha256_file(analysis_manifest_path)
    ) ||
    !identical(model_bundle$model_utils_sha256, sha256_file(model_utils_path)) ||
    !identical(
      model_bundle$analysis_utils_sha256, sha256_file(analysis_utils_path)
    )) {
  stop("Private locked-model bundle failed provenance/schema checks.")
}
if (!all(c("S0", "S3") %in% names(model_bundle$development_fits)) ||
    !identical(model_bundle$development_fits$S0$status, "ESTIMABLE") ||
    !identical(model_bundle$development_fits$S3$status, "ESTIMABLE")) {
  stop("Locked MIMIC S0/S3 fits are absent or non-estimable.")
}

prediction_metadata <- attr(private_predictions, "rebuild_metadata")
if (!isTRUE(prediction_metadata$private_row_level) ||
    !isTRUE(prediction_metadata$exact_analysis_id_join) ||
    !isTRUE(prediction_metadata$raw_external_predictions_precede_recalibration)) {
  stop("Private prediction bundle metadata failed D061 provenance checks.")
}
outcome_metadata <- attr(eicu_outcomes, "rebuild_metadata")
if (!isTRUE(outcome_metadata$formally_unblinded) ||
    !identical(outcome_metadata$checkpoint_sha256, sha256_file(checkpoint_path))) {
  stop("Formal eICU outcome metadata does not match the checkpoint.")
}

required_frame <- c(
  "analysis_id", "primary_predictor_complete", "age", "sex_female",
  "pf_ratio", "gcs", "map", "vasopressor", "platelet", "creatinine",
  "delta_p", "rr", "smp"
)
required_outcome <- c(
  "patientunitstayid", "hospitalid", "hospital_mortality",
  "hospital_mortality_eligible"
)
required_prediction <- c(
  "database", "analysis_set", "model_id", "analysis_id", "hospital_id",
  "probability_raw"
)
if (length(setdiff(required_frame, names(eicu_frame))) ||
    length(setdiff(required_outcome, names(eicu_outcomes))) ||
    length(setdiff(required_prediction, names(private_predictions)))) {
  stop("A row-level center-analysis input lacks required columns.")
}
if (anyDuplicated(eicu_frame$analysis_id) || anyNA(eicu_frame$analysis_id) ||
    anyDuplicated(eicu_outcomes$patientunitstayid) ||
    anyNA(eicu_outcomes$patientunitstayid) || anyNA(eicu_outcomes$hospitalid)) {
  stop("eICU predictor/outcome key invariant failed.")
}

outcome_link <- eicu_outcomes[, .(
  analysis_id = as.integer(patientunitstayid),
  hospital_id = as.character(hospitalid),
  mortality = as.integer(hospital_mortality),
  outcome_eligible = as.logical(hospital_mortality_eligible)
)]
if (!setequal(eicu_frame$analysis_id, outcome_link$analysis_id)) {
  stop("Canonical eICU predictor and formal outcome ID sets are not identical.")
}
center_all <- merge(
  eicu_frame, outcome_link, by = "analysis_id", all = FALSE, sort = FALSE
)
setorder(center_all, analysis_id)
if (nrow(center_all) != nrow(eicu_frame) ||
    anyDuplicated(center_all$analysis_id) || anyNA(center_all$hospital_id) ||
    anyNA(center_all$outcome_eligible) ||
    any(center_all$outcome_eligible & is.na(center_all$mortality)) ||
    any(!center_all$outcome_eligible & !is.na(center_all$mortality)) ||
    any(!is.na(center_all$mortality) & !center_all$mortality %in% c(0L, 1L))) {
  stop("Exact eICU outcome join or eligibility invariant failed.")
}
center_frame <- center_all[
  primary_predictor_complete %in% TRUE & outcome_eligible %in% TRUE &
    !is.na(mortality)
]
if (!nrow(center_frame) || uniqueN(center_frame$mortality) != 2L ||
    anyDuplicated(center_frame$analysis_id) || anyNA(center_frame$hospital_id)) {
  stop("D061 primary-common center frame is empty, duplicated, or one-class.")
}
assert_binary_outcome(center_frame$mortality)

s3_design <- build_design_matrix(
  center_frame, "S3", model_bundle$transform_bundle
)
s0_design <- build_design_matrix(
  center_frame, "S0", model_bundle$transform_bundle
)
if (!identical(
  colnames(s3_design), model_bundle$development_fits$S3$design_columns
) || !identical(
  colnames(s0_design), model_bundle$development_fits$S0$design_columns
) || !identical(tail(colnames(s3_design), 1L), "smp_per_5") ||
    !identical(
      colnames(s3_design)[seq_len(ncol(s0_design))], colnames(s0_design)
    )) {
  stop("eICU mixed-model design differs from locked MIMIC S3/S0 design.")
}
if (!identical(
  names(model_bundle$development_fits$S0$coefficients),
  c("(Intercept)", colnames(s0_design))
) || !identical(
  names(model_bundle$development_fits$S3$coefficients),
  c("(Intercept)", colnames(s3_design))
)) {
  stop("Locked MIMIC S0/S3 coefficient names differ from frozen designs.")
}
locked_s0_lp <- as.numeric(
  cbind(`(Intercept)` = 1, s0_design) %*%
    model_bundle$development_fits$S0$coefficients
)
if (anyNA(locked_s0_lp) || any(!is.finite(locked_s0_lp))) {
  stop("Locked MIMIC S0 linear predictor is non-finite in eICU.")
}

mixed_data <- data.frame(
  mortality = center_frame$mortality,
  hospital_factor = factor(center_frame$hospital_id),
  s3_design,
  check.names = FALSE
)
fixed_rhs <- paste(sprintf("`%s`", colnames(s3_design)), collapse = " + ")
random_intercept_formula <- stats::as.formula(paste0(
  "mortality ~ ", fixed_rhs, " + (1 | hospital_factor)"
))
random_slope_formula <- stats::as.formula(paste0(
  "mortality ~ ", fixed_rhs,
  " + (1 + smp_per_5 | hospital_factor)"
))
glmer_control <- lme4::glmerControl(
  optimizer = GLMER_OPTIMIZER,
  optCtrl = list(maxfun = GLMER_MAXFUN),
  calc.derivs = TRUE
)

fit_glmer_locked <- function(formula, label) {
  captured_warnings <- character()
  error_message <- ""
  fit <- tryCatch(
    withCallingHandlers(
      lme4::glmer(
        formula = formula, data = mixed_data, family = stats::binomial(),
        control = glmer_control, nAGQ = GLMER_NAGQ
      ),
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error_message <<- conditionMessage(e)
      NULL
    }
  )
  if (is.null(fit)) {
    return(list(
      label = label, fit = NULL, stable = FALSE, fit_error = TRUE,
      optimizer_clean = FALSE, lme4_messages_clean = FALSE,
      warning_free = length(captured_warnings) == 0L,
      fixed_design_complete = FALSE, finite_estimates = FALSE, singular = NA,
      warnings = captured_warnings, error = error_message,
      reason = safe_reason(c("fit_error", error_message, captured_warnings))
    ))
  }
  optimizer_code <- fit@optinfo$conv$opt
  optimizer_clean <- length(optimizer_code) == 1L &&
    !is.na(optimizer_code) && as.integer(optimizer_code) == 0L
  lme4_messages <- unlist(
    fit@optinfo$conv$lme4$messages, use.names = FALSE
  )
  lme4_messages_clean <- !length(lme4_messages)
  fixed_design_complete <- identical(
    names(lme4::fixef(fit)), c("(Intercept)", colnames(s3_design))
  )
  vc <- tryCatch(
    withCallingHandlers(
      as.matrix(stats::vcov(fit)),
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  warning_free <- !length(captured_warnings)
  random_effect_modes <- unlist(lme4::ranef(fit), use.names = FALSE)
  finite_estimates <- all(is.finite(lme4::fixef(fit))) &&
    all(is.finite(lme4::getME(fit, "theta"))) &&
    length(random_effect_modes) > 0L && all(is.finite(random_effect_modes)) &&
    is.finite(as.numeric(stats::logLik(fit))) && !is.null(vc) &&
    all(is.finite(vc))
  singular <- lme4::isSingular(fit, tol = SINGULAR_TOLERANCE)
  stable <- optimizer_clean && lme4_messages_clean && warning_free &&
    fixed_design_complete && finite_estimates && !singular
  reason <- safe_reason(c(
    if (!optimizer_clean) "optimizer_convergence_code_nonzero" else "",
    if (!lme4_messages_clean) lme4_messages else "",
    if (!warning_free) captured_warnings else "",
    if (!fixed_design_complete) "fixed_S3_design_column_dropped_or_changed" else "",
    if (!finite_estimates) "nonfinite_estimate_or_covariance" else "",
    if (singular) "singular_fit_tol_1e-4" else ""
  ))
  list(
    label = label, fit = fit, stable = stable, fit_error = FALSE,
    optimizer_clean = optimizer_clean,
    lme4_messages_clean = lme4_messages_clean,
    warning_free = warning_free, fixed_design_complete = fixed_design_complete,
    finite_estimates = finite_estimates,
    singular = singular, warnings = captured_warnings,
    lme4_messages = lme4_messages, error = "", reason = reason
  )
}

random_intercept_result <- fit_glmer_locked(
  random_intercept_formula, "random_intercept"
)
random_slope_result <- fit_glmer_locked(
  random_slope_formula, "correlated_random_intercept_smp_slope"
)

extract_mixed_summary <- function(result, model_id) {
  reportable <- isTRUE(result$stable)
  beta <- se <- intercept_sd <- slope_sd <- correlation <- NA_real_
  if (reportable) {
    coefficient_table <- coef(summary(result$fit))
    if (!"smp_per_5" %in% rownames(coefficient_table)) {
      stop("A stable mixed model lacks the fixed sMP/5 coefficient.")
    }
    beta <- unname(coefficient_table["smp_per_5", "Estimate"])
    se <- unname(coefficient_table["smp_per_5", "Std. Error"])
    vc <- as.data.frame(lme4::VarCorr(result$fit))
    intercept_row <- vc$grp == "hospital_factor" &
      vc$var1 == "(Intercept)" & is.na(vc$var2)
    if (sum(intercept_row) == 1L) {
      intercept_sd <- vc$sdcor[intercept_row]
    }
    slope_row <- vc$grp == "hospital_factor" &
      vc$var1 == "smp_per_5" & is.na(vc$var2)
    if (sum(slope_row) == 1L) slope_sd <- vc$sdcor[slope_row]
    correlation_row <- vc$grp == "hospital_factor" &
      vc$var1 == "(Intercept)" & !is.na(vc$var2) &
      vc$var2 == "smp_per_5"
    if (sum(correlation_row) == 1L) {
      correlation <- vc$sdcor[correlation_row]
    }
    if (any(!is.finite(c(beta, se, intercept_sd))) || se <= 0) {
      stop("A model marked stable yielded an invalid public summary.")
    }
    if (model_id == "correlated_random_slope" &&
        any(!is.finite(c(slope_sd, correlation)))) {
      stop("A stable random-slope model lacks finite slope heterogeneity.")
    }
  }
  data.table(
    model_id = model_id,
    fixed_design = "frozen_D054_S3_primary_common_linear_sMP_per_5",
    random_effects = if (model_id == "random_intercept") {
      "hospital_random_intercept"
    } else {
      "correlated_hospital_random_intercept_and_sMP_per_5_slope"
    },
    n = nrow(center_frame), events = sum(center_frame$mortality),
    nonevents = sum(center_frame$mortality == 0L),
    contributing_hospital_count = uniqueN(center_frame$hospital_id),
    status = if (reportable) "REPORTABLE" else if (result$fit_error) {
      "FIT_ERROR_NOT_REPORTABLE"
    } else {
      "UNSTABLE_NOT_REPORTABLE"
    },
    fixed_smp_per_5_log_or = beta,
    fixed_smp_per_5_standard_error = se,
    fixed_smp_per_5_or = exp(beta),
    fixed_smp_per_5_or_ci_lower = exp(beta - 1.96 * se),
    fixed_smp_per_5_or_ci_upper = exp(beta + 1.96 * se),
    random_intercept_sd = intercept_sd,
    random_smp_per_5_slope_sd = slope_sd,
    intercept_slope_correlation = correlation,
    optimizer = GLMER_OPTIMIZER, nAGQ = GLMER_NAGQ,
    maxfun = GLMER_MAXFUN, singular_tolerance = SINGULAR_TOLERANCE,
    stability_rule_pass = reportable,
    reporting_scope = NA_character_
  )
}

mixed_summary <- rbindlist(list(
  extract_mixed_summary(random_intercept_result, "random_intercept"),
  extract_mixed_summary(random_slope_result, "correlated_random_slope")
), use.names = TRUE, fill = TRUE)

center_counts <- center_frame[, .(
  complete_n = .N,
  event_n = sum(mortality == 1L),
  nonevent_n = sum(mortality == 0L),
  event_proportion = mean(mortality)
), by = hospital_id]
setorder(center_counts, hospital_id)
hospital_count <- nrow(center_counts)
sparse_center_majority <- sum(center_counts$complete_n < 5L) > hospital_count / 2
mixed_reporting_scope <- if (
  hospital_count < INFERENTIAL_MIN_HOSPITALS || sparse_center_majority
) "DESCRIPTIVE_CENTER_HETEROGENEITY" else "INFERENTIAL_SECONDARY"
mixed_summary[, reporting_scope := mixed_reporting_scope]

support_metric_summary <- rbindlist(lapply(
  c("complete_n", "event_n", "nonevent_n", "event_proportion"),
  function(variable) {
    q <- quantile_summary(center_counts[[variable]])
    data.table(
      support_metric = variable,
      hospital_count = hospital_count,
      total = if (variable == "event_proportion") NA_real_ else
        sum(center_counts[[variable]]),
      minimum = q[["minimum"]], q1 = q[["q1"]],
      median = q[["median"]], q3 = q[["q3"]],
      maximum = q[["maximum"]]
    )
  }
))
support_context <- data.table(
  source_strict_cohort_n = as.integer(require_map_value(
    selection_gate, "eicu_strict_n", label = "selection gate"
  )),
  source_strict_hospital_count = as.integer(require_map_value(
    selection_gate, "eicu_hospital_n", label = "selection gate"
  )),
  structural_zero_tuple_hospital_count = as.integer(require_map_value(
    selection_gate, "eicu_zero_observed_hospital_n", label = "selection gate"
  )),
  observed_tuple_n = as.integer(require_map_value(
    selection_gate, "eicu_tuple_observed_n", label = "selection gate"
  )),
  observed_tuple_hospital_count = as.integer(require_map_value(
    selection_gate, "eicu_hospital_n", label = "selection gate"
  )) - as.integer(require_map_value(
    selection_gate, "eicu_zero_observed_hospital_n", label = "selection gate"
  )),
  primary_common_n = nrow(center_frame),
  primary_common_hospital_count = hospital_count,
  primary_common_event_n = sum(center_frame$mortality),
  primary_common_nonevent_n = sum(center_frame$mortality == 0L),
  primary_common_hospitals_below_5_records = sum(center_counts$complete_n < 5L),
  sparse_center_majority = sparse_center_majority,
  zero_tuple_hospitals_entered_outcome_model = FALSE,
  reporting_scope = mixed_reporting_scope
)
support_summary <- cbind(
  support_context[rep(1L, nrow(support_metric_summary))],
  support_metric_summary
)

# ---------------------------------------------------------------------------
# Prespecified two-stage fallback, triggered only if the correlated random-slope
# model is unstable. No penalized or separation-rescue estimator is permitted.
# ---------------------------------------------------------------------------

fallback_triggered <- !isTRUE(random_slope_result$stable)
fallback_center_rows <- copy(center_counts)
fallback_center_rows[, `:=`(
  threshold_eligible = complete_n >= FALLBACK_MIN_N &
    event_n >= FALLBACK_MIN_EVENTS & nonevent_n >= FALLBACK_MIN_NONEVENTS,
  fit_attempted = FALSE,
  fit_success = FALSE,
  exclusion_reason = "fallback_not_triggered",
  smp_per_5_log_or = NA_real_, standard_error = NA_real_,
  smp_per_5_or = NA_real_, warning_text = ""
)]

fit_two_stage_center <- function(center_id) {
  data <- center_frame[hospital_id == center_id]
  row_index <- match(data$analysis_id, center_frame$analysis_id)
  model_data <- data.frame(
    mortality = data$mortality,
    locked_mimic_s0_lp = locked_s0_lp[row_index],
    smp_per_5 = data$smp / 5
  )
  warnings <- character()
  error_message <- ""
  fit <- tryCatch(
    withCallingHandlers(
      stats::glm(
        mortality ~ locked_mimic_s0_lp + smp_per_5,
        data = model_data, family = stats::binomial(),
        control = stats::glm.control(maxit = 100L)
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error_message <<- conditionMessage(e)
      NULL
    }
  )
  if (is.null(fit)) {
    return(list(
      success = FALSE, reason = safe_reason(c("fit_error", error_message)),
      warnings = warnings, beta = NA_real_, se = NA_real_, fit = NULL
    ))
  }
  covariance <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  coefficient <- stats::coef(fit)
  standard_error <- if (is.null(covariance)) {
    rep(NA_real_, length(coefficient))
  } else {
    sqrt(diag(covariance))
  }
  names(standard_error) <- names(coefficient)
  warning_separation <- any(grepl(
    "did not converge|fitted probabilities numerically 0 or 1|separation",
    warnings, ignore.case = TRUE
  ))
  estimable <- isTRUE(fit$converged) && fit$rank == 3L &&
    identical(names(coefficient), c(
      "(Intercept)", "locked_mimic_s0_lp", "smp_per_5"
    )) && all(is.finite(coefficient)) && all(is.finite(standard_error)) &&
    standard_error[["smp_per_5"]] > 0 && !warning_separation &&
    !length(warnings)
  reason <- safe_reason(c(
    if (!isTRUE(fit$converged)) "glm_not_converged" else "",
    if (fit$rank != 3L) "rank_deficient" else "",
    if (warning_separation) "separation_or_boundary_warning" else "",
    if (length(warnings)) warnings else "",
    if (!all(is.finite(coefficient))) "nonfinite_coefficient" else "",
    if (!all(is.finite(standard_error))) "nonfinite_standard_error" else ""
  ))
  list(
    success = estimable,
    reason = if (estimable) "" else reason,
    warnings = warnings,
    beta = if (estimable) unname(coefficient[["smp_per_5"]]) else NA_real_,
    se = if (estimable) unname(standard_error[["smp_per_5"]]) else NA_real_,
    fit = fit
  )
}

fallback_fit_objects <- list()
if (fallback_triggered) {
  fallback_center_rows[, exclusion_reason := fifelse(
    threshold_eligible, "eligible_not_yet_fitted", "threshold_ineligible"
  )]
  candidate_ids <- fallback_center_rows[
    threshold_eligible == TRUE, hospital_id
  ]
  for (center_id in candidate_ids) {
    fitted <- fit_two_stage_center(center_id)
    fallback_fit_objects[[center_id]] <- fitted$fit
    fallback_center_rows[hospital_id == center_id, `:=`(
      fit_attempted = TRUE,
      fit_success = fitted$success,
      exclusion_reason = if (fitted$success) "" else fitted$reason,
      smp_per_5_log_or = fitted$beta,
      standard_error = fitted$se,
      smp_per_5_or = exp(fitted$beta),
      warning_text = safe_reason(fitted$warnings)
    )]
  }
}

successful_fallback <- fallback_center_rows[fit_success == TRUE]
meta_fit <- NULL
meta_prediction <- NULL
meta_warning <- character()
meta_error <- ""
meta_status <- if (!fallback_triggered) {
  "NOT_TRIGGERED_RANDOM_SLOPE_STABLE"
} else if (nrow(successful_fallback) < FALLBACK_MIN_META_HOSPITALS) {
  "NOT_ESTIMABLE_FEWER_THAN_5_SUCCESSFUL_HOSPITALS"
} else {
  "PENDING"
}
if (fallback_triggered &&
    nrow(successful_fallback) >= FALLBACK_MIN_META_HOSPITALS) {
  meta_fit <- tryCatch(
    withCallingHandlers(
      metafor::rma.uni(
        yi = successful_fallback$smp_per_5_log_or,
        sei = successful_fallback$standard_error,
        method = "REML", test = "knha"
      ),
      warning = function(w) {
        meta_warning <<- c(meta_warning, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      meta_error <<- conditionMessage(e)
      NULL
    }
  )
  if (!is.null(meta_fit) && !length(meta_warning) &&
      all(is.finite(c(
        as.numeric(meta_fit$b), meta_fit$se, meta_fit$ci.lb, meta_fit$ci.ub,
        meta_fit$tau2, meta_fit$I2
      ))) && length(meta_fit$se) == 1L && meta_fit$se > 0) {
    meta_prediction <- tryCatch(
      stats::predict(meta_fit),
      error = function(e) {
        meta_error <<- conditionMessage(e)
        NULL
      }
    )
    required_prediction_fields <- c(
      "pred", "ci.lb", "ci.ub", "pi.lb", "pi.ub"
    )
    prediction_fields_complete <- !is.null(meta_prediction) &&
      all(vapply(required_prediction_fields, function(field) {
        value <- meta_prediction[[field]]
        length(value) == 1L && !is.na(value) && is.finite(value)
      }, logical(1L)))
    if (prediction_fields_complete) {
      meta_status <- "ESTIMABLE_REML_HARTUNG_KNAPP"
    } else {
      meta_status <- "NOT_ESTIMABLE_PREDICTION_INTERVAL_FAILURE"
    }
  } else {
    meta_status <- "NOT_ESTIMABLE_REML_FAILURE"
  }
}

meta_estimable <- identical(meta_status, "ESTIMABLE_REML_HARTUNG_KNAPP")
pooled_beta <- if (meta_estimable) as.numeric(meta_fit$b) else NA_real_
pooled_se <- if (meta_estimable) as.numeric(meta_fit$se) else NA_real_
pooled_ci_lower <- if (meta_estimable) as.numeric(meta_fit$ci.lb) else NA_real_
pooled_ci_upper <- if (meta_estimable) as.numeric(meta_fit$ci.ub) else NA_real_
prediction_lower <- if (meta_estimable) {
  as.numeric(meta_prediction$pi.lb)
} else NA_real_
prediction_upper <- if (meta_estimable) {
  as.numeric(meta_prediction$pi.ub)
} else NA_real_
fallback_reporting_scope <- if (
  nrow(successful_fallback) < INFERENTIAL_MIN_HOSPITALS ||
    sparse_center_majority
) "DESCRIPTIVE_CENTER_HETEROGENEITY" else "INFERENTIAL_SECONDARY"
two_stage_summary <- data.table(
  fallback_triggered = fallback_triggered,
  trigger_rule = "correlated_random_slope_not_stable",
  eligible_hospital_count = sum(fallback_center_rows$threshold_eligible),
  attempted_hospital_count = sum(fallback_center_rows$fit_attempted),
  successful_hospital_count = nrow(successful_fallback),
  excluded_after_attempt_count = sum(
    fallback_center_rows$fit_attempted & !fallback_center_rows$fit_success
  ),
  minimum_hospitals_for_pooling = FALLBACK_MIN_META_HOSPITALS,
  pooling_status = meta_status,
  pooling_method = "REML",
  inference_method = "Hartung_Knapp",
  pooled_smp_per_5_log_or = pooled_beta,
  pooled_standard_error = pooled_se,
  pooled_log_or_ci_lower = pooled_ci_lower,
  pooled_log_or_ci_upper = pooled_ci_upper,
  pooled_smp_per_5_or = exp(pooled_beta),
  pooled_or_ci_lower = exp(pooled_ci_lower),
  pooled_or_ci_upper = exp(pooled_ci_upper),
  tau_squared = if (meta_estimable) as.numeric(meta_fit$tau2) else NA_real_,
  I_squared_percent = if (meta_estimable) as.numeric(meta_fit$I2) else NA_real_,
  prediction_interval_log_or_lower = prediction_lower,
  prediction_interval_log_or_upper = prediction_upper,
  prediction_interval_or_lower = exp(prediction_lower),
  prediction_interval_or_upper = exp(prediction_upper),
  reporting_scope = fallback_reporting_scope,
  penalized_rescue_used = FALSE
)

# ---------------------------------------------------------------------------
# LOHO influence: fixed original S2/S3 raw predictions from Phase 3b only.
# Each hospital is omitted once; no model refit and no recalibration occur.
# ---------------------------------------------------------------------------

loho_input <- private_predictions[
  database == "eICU-CRD_v2.0" & analysis_set == "primary_common" &
    model_id %in% LOHO_MODELS,
  .(analysis_id = as.integer(analysis_id), model_id, probability_raw)
]
if (nrow(loho_input) != 2L * nrow(center_frame) ||
    anyDuplicated(loho_input[, .(analysis_id, model_id)]) ||
    anyNA(loho_input$probability_raw) ||
    any(!is.finite(loho_input$probability_raw)) ||
    any(loho_input$probability_raw < 0 | loho_input$probability_raw > 1) ||
    !setequal(loho_input$analysis_id, center_frame$analysis_id)) {
  stop("Locked raw S2/S3 prediction rows do not match the D061 center frame.")
}
prediction_hospital_check <- unique(private_predictions[
  database == "eICU-CRD_v2.0" & analysis_set == "primary_common" &
    model_id %in% LOHO_MODELS,
  .(
    analysis_id = as.integer(analysis_id),
    prediction_hospital_id = as.character(hospital_id)
  )
])
if (nrow(prediction_hospital_check) != nrow(center_frame) ||
    anyDuplicated(prediction_hospital_check$analysis_id) ||
    anyNA(prediction_hospital_check$prediction_hospital_id)) {
  stop("S2/S3 prediction bundles disagree on the hospital linkage key.")
}
prediction_hospital_check <- merge(
  prediction_hospital_check,
  center_frame[, .(analysis_id, formal_hospital_id = hospital_id)],
  by = "analysis_id", all = FALSE, sort = FALSE
)
if (nrow(prediction_hospital_check) != nrow(center_frame) ||
    any(prediction_hospital_check$prediction_hospital_id !=
      prediction_hospital_check$formal_hospital_id)) {
  stop("Prediction-bundle hospital linkage differs from formal outcomes.")
}
loho_wide <- dcast(
  loho_input, analysis_id ~ model_id, value.var = "probability_raw"
)
if (!all(LOHO_MODELS %in% names(loho_wide))) {
  stop("Locked raw S2/S3 predictions could not be reshaped exactly.")
}
loho_frame <- merge(
  center_frame[, .(analysis_id, hospital_id, mortality)],
  loho_wide, by = "analysis_id", all = FALSE, sort = FALSE
)
setorder(loho_frame, analysis_id)
if (nrow(loho_frame) != nrow(center_frame) ||
    anyDuplicated(loho_frame$analysis_id) ||
    anyNA(loho_frame[, ..LOHO_MODELS])) {
  stop("LOHO exact join/cardinality invariant failed.")
}

full_metrics <- lapply(LOHO_MODELS, function(model_id) {
  performance_vector(loho_frame$mortality, loho_frame[[model_id]])
})
names(full_metrics) <- LOHO_MODELS
if (!identical(names(full_metrics$S2), names(full_metrics$S3))) {
  stop("S2/S3 performance metric definitions differ.")
}
loho_metric_names <- names(full_metrics$S2)
full_difference <- full_metrics$S3 - full_metrics$S2

loho_model_rows <- list()
loho_difference_rows <- list()
loho_position <- 0L
for (omitted_id in sort(unique(loho_frame$hospital_id))) {
  keep <- loho_frame$hospital_id != omitted_id
  remaining <- loho_frame[keep]
  success <- nrow(remaining) > 0L && uniqueN(remaining$mortality) == 2L &&
    uniqueN(remaining$hospital_id) >= 1L
  reason <- if (success) "" else "remaining_sample_empty_or_one_class"
  omitted_metrics <- setNames(vector("list", length(LOHO_MODELS)), LOHO_MODELS)
  if (success) {
    metric_error <- ""
    omitted_metrics <- tryCatch(
      setNames(lapply(LOHO_MODELS, function(model_id) {
        performance_vector(remaining$mortality, remaining[[model_id]])
      }), LOHO_MODELS),
      error = function(e) {
        metric_error <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(omitted_metrics)) {
      success <- FALSE
      reason <- safe_reason(c("performance_failure", metric_error))
    }
  }
  loho_position <- loho_position + 1L
  if (success) {
    loho_model_rows[[loho_position]] <- rbindlist(lapply(
      LOHO_MODELS, function(model_id) data.table(
        omitted_hospital_id = omitted_id,
        remaining_n = nrow(remaining),
        remaining_events = sum(remaining$mortality),
        remaining_hospital_count = uniqueN(remaining$hospital_id),
        model_id = model_id,
        metric = loho_metric_names,
        full_estimate = as.numeric(full_metrics[[model_id]][loho_metric_names]),
        loho_estimate = as.numeric(omitted_metrics[[model_id]][loho_metric_names]),
        change_from_full = as.numeric(
          omitted_metrics[[model_id]][loho_metric_names] -
            full_metrics[[model_id]][loho_metric_names]
        ),
        success = TRUE, reason = ""
      )
    ))
    omitted_difference <- omitted_metrics$S3 - omitted_metrics$S2
    loho_difference_rows[[loho_position]] <- data.table(
      omitted_hospital_id = omitted_id,
      remaining_n = nrow(remaining),
      remaining_events = sum(remaining$mortality),
      remaining_hospital_count = uniqueN(remaining$hospital_id),
      comparison_id = LOHO_COMPARISON,
      metric = loho_metric_names,
      full_difference_s3_minus_s2 = as.numeric(
        full_difference[loho_metric_names]
      ),
      loho_difference_s3_minus_s2 = as.numeric(
        omitted_difference[loho_metric_names]
      ),
      change_from_full_difference = as.numeric(
        omitted_difference[loho_metric_names] -
          full_difference[loho_metric_names]
      ),
      success = TRUE, reason = ""
    )
  } else {
    loho_model_rows[[loho_position]] <- rbindlist(lapply(
      LOHO_MODELS, function(model_id) data.table(
        omitted_hospital_id = omitted_id,
        remaining_n = nrow(remaining),
        remaining_events = sum(remaining$mortality),
        remaining_hospital_count = uniqueN(remaining$hospital_id),
        model_id = model_id,
        metric = loho_metric_names,
        full_estimate = as.numeric(full_metrics[[model_id]][loho_metric_names]),
        loho_estimate = NA_real_, change_from_full = NA_real_,
        success = FALSE, reason = reason
      )
    ))
    loho_difference_rows[[loho_position]] <- data.table(
      omitted_hospital_id = omitted_id,
      remaining_n = nrow(remaining),
      remaining_events = sum(remaining$mortality),
      remaining_hospital_count = uniqueN(remaining$hospital_id),
      comparison_id = LOHO_COMPARISON,
      metric = loho_metric_names,
      full_difference_s3_minus_s2 = as.numeric(
        full_difference[loho_metric_names]
      ),
      loho_difference_s3_minus_s2 = NA_real_,
      change_from_full_difference = NA_real_,
      success = FALSE, reason = reason
    )
  }
}
private_loho_models <- rbindlist(loho_model_rows, use.names = TRUE, fill = TRUE)
private_loho_differences <- rbindlist(
  loho_difference_rows, use.names = TRUE, fill = TRUE
)
if (uniqueN(private_loho_models$omitted_hospital_id) != hospital_count ||
    uniqueN(private_loho_differences$omitted_hospital_id) != hospital_count ||
    nrow(private_loho_models) !=
      hospital_count * length(LOHO_MODELS) * length(loho_metric_names) ||
    nrow(private_loho_differences) !=
      hospital_count * length(loho_metric_names)) {
  stop("LOHO did not attempt every contributing hospital exactly once.")
}

loho_model_summary <- private_loho_models[, {
  q_estimate <- quantile_summary(loho_estimate[success])
  q_change <- quantile_summary(change_from_full[success])
  list(
    full_estimate = unique(full_estimate),
    attempted_hospital_count = uniqueN(omitted_hospital_id),
    successful_omission_count = uniqueN(omitted_hospital_id[success]),
    failed_omission_count = uniqueN(omitted_hospital_id[!success]),
    loho_estimate_minimum = q_estimate[["minimum"]],
    loho_estimate_q1 = q_estimate[["q1"]],
    loho_estimate_median = q_estimate[["median"]],
    loho_estimate_q3 = q_estimate[["q3"]],
    loho_estimate_maximum = q_estimate[["maximum"]],
    change_from_full_minimum = q_change[["minimum"]],
    change_from_full_maximum = q_change[["maximum"]],
    maximum_absolute_change_from_full = if (any(success)) {
      max(abs(change_from_full[success]))
    } else NA_real_,
    method = "fixed_original_raw_prediction_leave_one_hospital_out",
    refit = FALSE, recalibration = FALSE,
    interpretation = "influence_not_transportability"
  )
}, by = .(model_id, metric)]

loho_difference_summary <- private_loho_differences[, {
  q_estimate <- quantile_summary(loho_difference_s3_minus_s2[success])
  q_change <- quantile_summary(change_from_full_difference[success])
  list(
    full_difference_s3_minus_s2 = unique(full_difference_s3_minus_s2),
    attempted_hospital_count = uniqueN(omitted_hospital_id),
    successful_omission_count = uniqueN(omitted_hospital_id[success]),
    failed_omission_count = uniqueN(omitted_hospital_id[!success]),
    loho_difference_minimum = q_estimate[["minimum"]],
    loho_difference_q1 = q_estimate[["q1"]],
    loho_difference_median = q_estimate[["median"]],
    loho_difference_q3 = q_estimate[["q3"]],
    loho_difference_maximum = q_estimate[["maximum"]],
    change_from_full_minimum = q_change[["minimum"]],
    change_from_full_maximum = q_change[["maximum"]],
    maximum_absolute_change_from_full = if (any(success)) {
      max(abs(change_from_full_difference[success]))
    } else NA_real_,
    method = "paired_fixed_original_raw_prediction_LOHO",
    refit = FALSE, recalibration = FALSE,
    interpretation = "influence_not_transportability"
  )
}, by = .(comparison_id, metric)]

# ---------------------------------------------------------------------------
# Aggregate/QC outputs. All hospital-specific rows, identifiers, coefficients,
# and LOHO omissions remain in one private RDS. Public CSVs are aggregate only.
# ---------------------------------------------------------------------------

specification <- data.table(
  decision_id = DECISION_ID,
  analysis_population = "eICU_primary_common_S3",
  fixed_effect_design = "frozen_D054_S3",
  exposure_term = "linear_sMP_per_5_J_min",
  random_intercept_model = TRUE,
  correlated_random_slope_model = TRUE,
  optimizer = GLMER_OPTIMIZER,
  nAGQ = GLMER_NAGQ,
  maxfun = GLMER_MAXFUN,
  singular_tolerance = SINGULAR_TOLERANCE,
  fallback_min_n = FALLBACK_MIN_N,
  fallback_min_events = FALLBACK_MIN_EVENTS,
  fallback_min_nonevents = FALLBACK_MIN_NONEVENTS,
  fallback_min_hospitals_for_pooling = FALLBACK_MIN_META_HOSPITALS,
  fallback_pooling = "REML_Hartung_Knapp",
  fallback_penalized_rescue = FALSE,
  loho_prediction_stage = "original_locked_raw",
  loho_refit = FALSE,
  loho_recalibration = FALSE
)

mixed_stability_qc <- rbindlist(lapply(
  list(random_intercept_result, random_slope_result), function(result) {
    data.table(
      model_id = result$label,
      fit_object_created = !is.null(result$fit),
      optimizer_clean = result$optimizer_clean,
      lme4_messages_clean = result$lme4_messages_clean,
      warning_free = result$warning_free,
      fixed_design_complete = result$fixed_design_complete,
      finite_estimates = result$finite_estimates,
      singular = result$singular,
      singular_tolerance = SINGULAR_TOLERANCE,
      stability_rule_pass = result$stable,
      reportable = result$stable,
      failure_reason = result$reason
    )
  }
))
fallback_qc <- data.table(
  check = c(
    "fallback_trigger_equals_random_slope_instability",
    "eligibility_n_at_least_30",
    "eligibility_events_at_least_5",
    "eligibility_nonevents_at_least_5",
    "conventional_glm_only",
    "penalized_rescue_absent",
    "pooling_requires_at_least_5_successful_hospitals",
    "pooling_method_REML",
    "inference_Hartung_Knapp",
    "hospital_specific_rows_private"
  ),
  pass = c(
    identical(fallback_triggered, !isTRUE(random_slope_result$stable)),
    all(fallback_center_rows[threshold_eligible, complete_n >= FALLBACK_MIN_N]),
    all(fallback_center_rows[threshold_eligible, event_n >= FALLBACK_MIN_EVENTS]),
    all(fallback_center_rows[
      threshold_eligible, nonevent_n >= FALLBACK_MIN_NONEVENTS
    ]),
    TRUE, TRUE,
    !meta_estimable || nrow(successful_fallback) >= FALLBACK_MIN_META_HOSPITALS,
    !meta_estimable || identical(meta_fit$method, "REML"),
    !meta_estimable || identical(meta_fit$test, "knha"),
    TRUE
  )
)
loho_qc <- data.table(
  check = c(
    "models_exactly_S2_S3",
    "analysis_set_primary_common",
    "prediction_stage_original_raw",
    "exact_analysis_id_match",
    "each_hospital_omitted_once",
    "model_refit_absent",
    "recalibration_absent",
    "paired_difference_direction_S3_minus_S2",
    "hospital_rows_private",
    "interpretation_influence_not_transportability"
  ),
  pass = c(
    setequal(unique(loho_input$model_id), LOHO_MODELS),
    TRUE, TRUE,
    setequal(loho_frame$analysis_id, center_frame$analysis_id),
    uniqueN(private_loho_models$omitted_hospital_id) == hospital_count &&
      uniqueN(private_loho_differences$omitted_hospital_id) == hospital_count,
    TRUE, TRUE, TRUE, TRUE, TRUE
  )
)
input_gate_qc <- data.table(
  check = c(
    "authorization_checkpoint_PASS",
    "checkpoint_directly_hash_locks_script_13",
    "analysis_manifest_all_hashes_match",
    "manifest_includes_08_08a_09_10_13",
    "parameter_gate_PASS",
    "selection_gate_D055_PASS",
    "outcome_gate_formally_unblinded_PASS",
    "main_model_gate_PASS",
    "access_receipt_hash_chain_matches",
    "private_model_bundle_hash_matches",
    "private_prediction_bundle_hash_matches",
    "formal_eICU_outcome_hash_matches",
    "canonical_eICU_frame_hash_matches",
    "D061_authorized",
    "S3_design_exactly_frozen_and_linear_sMP_per_5"
  ),
  pass = TRUE
)

private_bundle <- list(
  artifact_version = "eicu_center_heterogeneity_private_v1",
  config_version = LOCKED$version,
  decision_id = DECISION_ID,
  private_row_level = TRUE,
  checkpoint_sha256 = sha256_file(checkpoint_path),
  analysis_manifest_sha256 = sha256_file(analysis_manifest_path),
  outcome_gate_sha256 = sha256_file(outcome_gate_path),
  main_model_gate_sha256 = sha256_file(main_model_gate_path),
  script_sha256 = sha256_file(script_path),
  package_versions = c(
    lme4 = lme4_version,
    metafor = metafor_version
  ),
  center_frame = center_frame[, .(
    analysis_id, hospital_id, mortality, smp_per_5 = smp / 5,
    locked_mimic_s0_lp = locked_s0_lp
  )],
  center_counts = center_counts,
  s3_design_columns = colnames(s3_design),
  random_intercept = random_intercept_result,
  correlated_random_slope = random_slope_result,
  fallback_triggered = fallback_triggered,
  fallback_hospital_results = fallback_center_rows,
  fallback_fit_objects = fallback_fit_objects,
  fallback_meta_fit = meta_fit,
  fallback_meta_prediction = meta_prediction,
  fallback_meta_warnings = meta_warning,
  fallback_meta_error = meta_error,
  loho_model_rows = private_loho_models,
  loho_paired_difference_rows = private_loho_differences
)

forbidden_headers <- c(
  "analysis_id", "patientunitstayid", "hospitalid", "hospital_id",
  "source_hospital_id", "omitted_hospital_id", "center_id"
)
identifier_pattern <- "(^|_)(hospital|center)(_?id|identifier)(_|$)"
prepublication_public_tables <- list(
  specification, support_summary, mixed_summary, two_stage_summary,
  loho_model_summary, loho_difference_summary, input_gate_qc,
  mixed_stability_qc, fallback_qc, loho_qc
)
prepublication_headers <- unique(tolower(unlist(lapply(
  prepublication_public_tables, names
))))
if (any(prepublication_headers %in% forbidden_headers) ||
    any(grepl(identifier_pattern, prepublication_headers))) {
  stop("A planned public table contains a hospital/row-level identifier header.")
}

dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

atomic_save_rds_new(private_bundle, private_bundle_path)
atomic_fwrite_new(specification, aggregate_paths[["specification"]])
atomic_fwrite_new(support_summary, aggregate_paths[["support"]])
atomic_fwrite_new(mixed_summary, aggregate_paths[["mixed_models"]])
atomic_fwrite_new(two_stage_summary, aggregate_paths[["two_stage"]])
atomic_fwrite_new(loho_model_summary, aggregate_paths[["loho_models"]])
atomic_fwrite_new(
  loho_difference_summary, aggregate_paths[["loho_comparison"]]
)
atomic_fwrite_new(input_gate_qc, qc_paths[["input_gate"]])
atomic_fwrite_new(mixed_stability_qc, qc_paths[["mixed_stability"]])
atomic_fwrite_new(fallback_qc, qc_paths[["fallback"]])
atomic_fwrite_new(loho_qc, qc_paths[["loho"]])

summary_lines <- c(
  "# eICU center heterogeneity and LOHO influence QC",
  "",
  paste0("- Configuration: ", LOCKED$version, "; decision: ", DECISION_ID),
  "- Population: eICU primary-common S3 complete set.",
  paste0(
    "- Mixed models: binomial glmer; optimizer ", GLMER_OPTIMIZER,
    "; nAGQ=", GLMER_NAGQ, "; maxfun=", GLMER_MAXFUN,
    "; singularity tolerance=", SINGULAR_TOLERANCE, "."
  ),
  paste0(
    "- Correlated random-slope stability: ",
    if (random_slope_result$stable) "PASS/reportable" else
      "FAIL/not reportable; prespecified fallback triggered", "."
  ),
  paste0("- Two-stage fallback status: ", meta_status, "."),
  "- Two-stage hospital models use only locked MIMIC S0 LP plus linear sMP/5; no penalized rescue.",
  "- LOHO uses fixed original S2/S3 raw predictions without refitting or recalibration.",
  "- LOHO is influence analysis, not transportability validation.",
  "- Hospital identifiers and hospital-specific rows are private only.",
  paste0("- Center-heterogeneity reporting scope: ", mixed_reporting_scope, "."),
  "",
  "BUILD_COMPLETE"
)
atomic_write_lines_new(summary_lines, qc_paths[["summary"]])

aggregate_manifest <- data.table(
  output_name = names(aggregate_paths),
  path = vapply(aggregate_paths, project_relative, character(1L)),
  sha256 = vapply(aggregate_paths, sha256_file, character(1L)),
  contains_site_keys = FALSE,
  hospital_specific_rows = FALSE
)
atomic_fwrite_new(aggregate_manifest, aggregate_manifest_path)

public_csv_paths <- c(
  aggregate_paths, qc_paths[names(qc_paths) != "summary"],
  aggregate_manifest_path
)
public_headers <- unique(tolower(unlist(lapply(public_csv_paths, function(path) {
  names(fread(path, nrows = 0L, showProgress = FALSE))
}))))
public_identifier_guard <- !any(public_headers %in% forbidden_headers) &&
  !any(grepl(identifier_pattern, public_headers))
if (!public_identifier_guard) {
  stop("A hospital/row-level identifier header entered a public CSV.")
}
if (!all(input_gate_qc$pass) || !all(fallback_qc$pass) ||
    !all(loho_qc$pass)) {
  stop("Final center-analysis QC contains a failed invariant.")
}
if (!identical(
  tail(readLines(qc_paths[["summary"]], warn = FALSE), 1L), "BUILD_COMPLETE"
)) {
  stop("Center-analysis QC summary sentinel is missing.")
}

completion <- data.table(
  field = c(
    "status", "config_version", "decision_id", "completed_at",
    "script_sha256", "checkpoint_sha256",
    "analysis_script_manifest_sha256", "parameter_freeze_gate_sha256",
    "selection_weights_gate_sha256", "outcome_gate_sha256",
    "main_model_gate_sha256", "model_rds_sha256", "prediction_rds_sha256",
    "eicu_model_frame_rds_sha256", "eicu_outcome_rds_sha256",
    "private_bundle_sha256", "aggregate_manifest_sha256",
    "lme4_version", "metafor_version", "analysis_population",
    "fixed_design", "smp_effect_scale", "glmer_optimizer", "glmer_nAGQ",
    "glmer_maxfun", "singular_tolerance",
    "random_intercept_stability_pass", "random_slope_stability_pass",
    "random_slope_result_reportable", "fallback_triggered",
    "fallback_successful_hospital_count", "fallback_meta_status",
    "fallback_penalized_rescue_used", "loho_models",
    "loho_prediction_stage", "loho_refit", "loho_recalibration",
    "loho_every_hospital_attempted", "loho_interpretation",
    "hospital_specific_rows_private", "public_identifier_guard_pass",
    "reporting_scope", "all_input_gate_checks_pass",
    "all_fallback_contract_checks_pass", "all_loho_contract_checks_pass",
    "all_required_outputs_present", "summary_sentinel"
  ),
  value = c(
    "PASS", LOCKED$version, DECISION_ID,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    sha256_file(script_path), sha256_file(checkpoint_path),
    sha256_file(analysis_manifest_path), sha256_file(parameter_gate_path),
    sha256_file(selection_gate_path), sha256_file(outcome_gate_path),
    sha256_file(main_model_gate_path), sha256_file(model_bundle_path),
    sha256_file(prediction_bundle_path), sha256_file(eicu_frame_path),
    sha256_file(eicu_outcome_path), sha256_file(private_bundle_path),
    sha256_file(aggregate_manifest_path),
    lme4_version, metafor_version,
    "eICU_primary_common_S3", "frozen_D054_D061_S3",
    "linear_per_5_J_min", GLMER_OPTIMIZER, GLMER_NAGQ, GLMER_MAXFUN,
    SINGULAR_TOLERANCE, random_intercept_result$stable,
    random_slope_result$stable, random_slope_result$stable,
    fallback_triggered, nrow(successful_fallback), meta_status, FALSE,
    paste(LOHO_MODELS, collapse = ";"), "original_locked_raw", FALSE, FALSE,
    uniqueN(private_loho_models$omitted_hospital_id) == hospital_count &&
      uniqueN(private_loho_differences$omitted_hospital_id) == hospital_count,
    "influence_not_transportability", TRUE, public_identifier_guard,
    mixed_reporting_scope, all(input_gate_qc$pass), all(fallback_qc$pass),
    all(loho_qc$pass), all(file.exists(c(
      private_bundle_path, aggregate_paths, qc_paths, aggregate_manifest_path
    ))), "BUILD_COMPLETE"
  )
)
if (anyDuplicated(completion$field) || anyNA(completion$value)) {
  stop("Malformed center-analysis completion gate.")
}
completion_tmp <- paste0(completion_gate, ".tmp.", Sys.getpid())
unlink(completion_tmp, force = TRUE)
fwrite(completion, completion_tmp)
if (!file.rename(completion_tmp, completion_gate)) {
  unlink(completion_tmp, force = TRUE)
  stop("Could not atomically publish Phase 3e center-analysis PASS gate.")
}

message("Locked eICU center heterogeneity analysis complete.")
message("  random-slope reportable: ", random_slope_result$stable)
message("  two-stage fallback status: ", meta_status)
message("  gate: ", completion_gate)
