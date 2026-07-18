#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2: formal resumable two-stage slope interval
#
# Run exactly one locked model per invocation.  Each completed outer replicate
# is saved atomically under a model-specific checkpoint directory.  Re-running
# the same command validates and reuses those checkpoints.
#
# This driver writes only to analysis_rebuild_v2/{private,aggregate,qc}/
# two_stage_calibration/.  It never overwrites the primary model point
# estimates, fits, predictions, or primary-model completion gate.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/12_run_two_stage_calibration_slope.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "03_internal_validation_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "12_two_stage_resume_utils.R"))

parse_model_id <- function(arguments) {
  if (length(arguments) == 1L &&
      grepl("^--model-id=", arguments[[1L]])) {
    value <- sub("^--model-id=", "", arguments[[1L]])
  } else if (length(arguments) == 2L &&
             identical(arguments[[1L]], "--model-id")) {
    value <- arguments[[2L]]
  } else {
    stop(
      "Usage: Rscript 12_run_two_stage_calibration_slope.R ",
      "--model-id <M0|M_MP|M_4DPRR|M_DPRR|M_ENERGY>"
    )
  }
  if (!value %in% LOCKED_V2$model_ids) {
    stop("Unknown locked model_id: ", value)
  }
  value
}

parse_max_new <- function() {
  value <- Sys.getenv("ARDS_V2_TWO_STAGE_MAX_NEW_REPS", unset = "")
  if (!nzchar(value)) return(Inf)
  numeric_value <- suppressWarnings(as.numeric(value))
  if (length(numeric_value) != 1L || is.na(numeric_value) ||
      !is.finite(numeric_value) ||
      numeric_value != as.integer(numeric_value) ||
      numeric_value < 1L) {
    stop("ARDS_V2_TWO_STAGE_MAX_NEW_REPS must be one positive integer.")
  }
  as.integer(numeric_value)
}

read_field_gate <- function(path) {
  gate <- fread(path, showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field)) {
    stop("Malformed field/value gate: ", path)
  }
  setNames(as.character(gate$value), gate$field)
}

assert_gate_value <- function(gate, field, expected) {
  observed <- unname(gate[[field]])
  if (is.null(observed) || !identical(observed, as.character(expected))) {
    stop(
      "Primary-model gate mismatch for ", field, ": expected ",
      expected, ", observed ",
      if (is.null(observed)) "<missing>" else observed
    )
  }
  invisible(TRUE)
}

model_id <- parse_model_id(commandArgs(trailingOnly = TRUE))
model_index <- match(model_id, LOCKED_V2$model_ids)
max_new_replicates <- parse_max_new()

outer_repetitions <-
  LOCKED_V2$bootstrap$calibration_slope_outer_replicates
inner_repetitions <-
  LOCKED_V2$bootstrap$calibration_slope_inner_replicates
master_seed <- LOCKED_V2$bootstrap$seed_sensitivity +
  model_index - 1L
minimum_success <- LOCKED_V2$bootstrap$minimum_success_fraction

stopifnot(
  identical(outer_repetitions, 1000L),
  identical(inner_repetitions, 200L),
  identical(LOCKED_V2$bootstrap$mimic_internal_replicates, 1000L),
  identical(LOCKED_V2$bootstrap$eicu_hospital_cluster_replicates, 2000L)
)

primary_private <- file.path(PRIVATE_ROOT, "primary_models")
primary_qc <- file.path(QC_ROOT, "primary_models")
primary_gate_path <- file.path(
  primary_qc, "phase4_primary_models_complete_v2.csv"
)
primary_manifest_path <- file.path(
  primary_qc, "primary_model_private_output_manifest_v2.csv"
)
analysis_path <- file.path(
  primary_private, "mimic_primary_analysis_frame_v2.rds"
)
point_validation_path <- file.path(
  primary_private, "mimic_internal_validation_v2.rds"
)
required_inputs <- c(
  primary_gate_path, primary_manifest_path,
  analysis_path, point_validation_path
)
if (any(!file.exists(required_inputs))) {
  stop(
    "Missing formal primary-model input(s): ",
    paste(required_inputs[!file.exists(required_inputs)], collapse = ", ")
  )
}

primary_gate <- read_field_gate(primary_gate_path)
for (item in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c("analysis_mode", "FINAL_LOCKED_BOOTSTRAP"),
  c(
    "internal_bootstrap_repetitions",
    LOCKED_V2$bootstrap$mimic_internal_replicates
  ),
  c("internal_bootstrap_all_success_gates_pass", "TRUE"),
  c(
    "external_hospital_bootstrap_repetitions",
    LOCKED_V2$bootstrap$eicu_hospital_cluster_replicates
  ),
  c("external_bootstrap_gate_pass", "TRUE"),
  c("calibration_bootstrap_gate_pass", "TRUE"),
  c("two_stage_executed", "FALSE"),
  c("final_manuscript_ci_ready", "FALSE")
)) {
  assert_gate_value(primary_gate, item[[1L]], item[[2L]])
}

primary_manifest <- fread(
  primary_manifest_path, showProgress = FALSE
)
required_manifest_columns <- c("role", "path", "sha256", "row_level")
if (!all(required_manifest_columns %in% names(primary_manifest)) ||
    anyDuplicated(primary_manifest$role)) {
  stop("Malformed primary private-output manifest.")
}
verify_manifest_role <- function(role_name, expected_path) {
  row <- primary_manifest[get("role") == role_name]
  if (nrow(row) != 1L) {
    stop("Primary private-output manifest lacks unique role: ", role_name)
  }
  if (!identical(
    normalizePath(row$path[[1L]], mustWork = TRUE),
    normalizePath(expected_path, mustWork = TRUE)
  )) {
    stop(
      "Primary private-output path mismatch for role ", role_name, "."
    )
  }
  observed_hash <- v2_pm_sha256_file(expected_path)
  if (!identical(row$sha256[[1L]], observed_hash)) {
    stop(
      "Primary private-output hash mismatch for role ", role_name, "."
    )
  }
  observed_hash
}
analysis_hash <- verify_manifest_role("mimic_analysis", analysis_path)
point_validation_hash <- verify_manifest_role(
  "internal_validation", point_validation_path
)

analysis <- readRDS(analysis_path)
if (!is.data.frame(analysis) || !"outcome" %in% names(analysis)) {
  stop("Malformed frozen MIMIC primary analysis frame.")
}
v2_assert_binary_outcome(analysis$outcome)
if (nrow(analysis) != as.integer(primary_gate[["mimic_n"]]) ||
    sum(analysis$outcome) !=
      as.integer(primary_gate[["mimic_events"]])) {
  stop("MIMIC analysis frame does not match the formal primary gate.")
}
refit_contract <- v2_pm_internal_refit_contract_audit(
  analysis, model_id
)
if (nrow(refit_contract) != 1L || !isTRUE(refit_contract$pass[[1L]])) {
  stop("Transformation-refit contract failed for ", model_id, ".")
}

point_validations <- readRDS(point_validation_path)
if (!is.list(point_validations) ||
    !identical(names(point_validations), LOCKED_V2$model_ids)) {
  stop("Formal internal-validation artifact has unexpected model names.")
}
point_validation <- point_validations[[model_id]]
expected_point_seed <- LOCKED_V2$bootstrap$seed_mimic +
  model_index - 1L
expected_pipeline_id <- paste0(
  model_id, "_rederive_transform_bundle_in_each_training_resample"
)
if (!inherits(point_validation, "ards_v2_harrell_validation") ||
    !isTRUE(point_validation$reportable) ||
    point_validation$repetitions_requested !=
      LOCKED_V2$bootstrap$mimic_internal_replicates ||
    point_validation$seed != expected_point_seed ||
    !identical(point_validation$metrics, v2_iv_default_metrics) ||
    !identical(point_validation$pipeline_id, expected_pipeline_id) ||
    !is.finite(point_validation$corrected[["calibration_slope"]])) {
  stop("Formal point validation is not the locked ", model_id, " object.")
}

source_paths <- c(
  driver = script_path,
  config = file.path(script_dir, "00_config.R"),
  analysis_utils = file.path(script_dir, "01_analysis_utils.R"),
  internal_validation_utils =
    file.path(script_dir, "03_internal_validation_utils.R"),
  primary_model_utils =
    file.path(script_dir, "09_primary_model_utils.R"),
  resume_utils =
    file.path(script_dir, "12_two_stage_resume_utils.R"),
  primary_completion_gate = primary_gate_path,
  primary_private_manifest = primary_manifest_path,
  mimic_analysis = analysis_path,
  point_validation = point_validation_path
)
source_hashes <- vapply(
  source_paths, v2_pm_sha256_file, character(1L)
)
if (!identical(source_hashes[["mimic_analysis"]], analysis_hash) ||
    !identical(
      source_hashes[["point_validation"]], point_validation_hash
    )) {
  stop("Input hash cross-check failed.")
}

contract <- v2_ts_make_contract(
  model_id = model_id,
  model_index = model_index,
  data_n = nrow(analysis),
  events = sum(analysis$outcome),
  outcome = "outcome",
  metrics = v2_iv_default_metrics,
  pipeline_id = expected_pipeline_id,
  outer_repetitions = outer_repetitions,
  inner_repetitions = inner_repetitions,
  seed = master_seed,
  minimum_inner_success_fraction = minimum_success,
  minimum_outer_success_fraction = minimum_success,
  level = 0.95,
  quantile_type = 7L,
  source_hashes = source_hashes
)
contract_hash <- v2_ts_contract_hash(contract)

private_out <- file.path(
  PRIVATE_ROOT, "two_stage_calibration", model_id
)
aggregate_out <- file.path(AGGREGATE_ROOT, "two_stage_calibration")
qc_out <- file.path(QC_ROOT, "two_stage_calibration", model_id)
checkpoint_dir <- file.path(private_out, "checkpoints")
for (directory in c(
  private_out, aggregate_out, qc_out, checkpoint_dir
)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

contract_path <- file.path(
  private_out,
  paste0("mimic_", model_id, "_two_stage_contract_v2.rds")
)
contract_file_hash <- v2_pm_atomic_save_rds(contract, contract_path)

resume <- v2_ts_resume(
  data = analysis,
  outcome = "outcome",
  fit_pipeline = v2_pm_internal_fit_factory(model_id),
  predict_pipeline = v2_pm_internal_predict_factory(model_id),
  score_pipeline = v2_iv_default_score,
  contract = contract,
  checkpoint_dir = checkpoint_dir,
  max_new_replicates = max_new_replicates
)

progress <- data.frame(
  status = if (resume$complete) "CHECKPOINTS_COMPLETE" else "INCOMPLETE",
  locked_config_version = LOCKED_V2$version,
  analysis_mode = "FINAL_LOCKED_TWO_STAGE_CALIBRATION_SLOPE",
  model_id = model_id,
  contract_hash = contract_hash,
  outer_repetitions = outer_repetitions,
  inner_repetitions = inner_repetitions,
  completed_before = resume$completed_before,
  new_replicates = resume$new_replicates,
  completed_after = resume$completed_after,
  pending_after = resume$pending_after,
  stringsAsFactors = FALSE
)
progress_path <- file.path(
  qc_out, paste0("mimic_", model_id, "_two_stage_progress_v2.csv")
)
v2_pm_atomic_write_csv(progress, progress_path)

completion_gate_path <- file.path(
  qc_out,
  paste0("mimic_", model_id, "_two_stage_complete_v2.csv")
)
if (!resume$complete) {
  if (file.exists(completion_gate_path)) {
    unlink(completion_gate_path, force = TRUE)
  }
  cat(
    "REBUILD_V2_TWO_STAGE_CHECKPOINT_PROGRESS\n",
    "Model: ", model_id, "\n",
    "Completed: ", resume$completed_after, "/", outer_repetitions, "\n",
    "New this invocation: ", resume$new_replicates, "\n",
    "Re-run the identical command to resume.\n",
    sep = ""
  )
  quit(save = "no", status = 0L)
}

validation <- v2_ts_collect_validation(
  contract = contract,
  checkpoint_dir = checkpoint_dir,
  point_validation = point_validation
)
v2_iv_assert_reportable(validation)
slope <- validation$confidence_interval[
  validation$confidence_interval$metric == "calibration_slope",
  ,
  drop = FALSE
]
if (nrow(slope) != 1L || !isTRUE(slope$supported[[1L]])) {
  stop("Formal two-stage calibration-slope interval is unsupported.")
}

validation_path <- file.path(
  private_out,
  paste0("mimic_", model_id, "_two_stage_validation_v2.rds")
)
validation_hash <- v2_pm_atomic_save_rds(
  validation, validation_path
)
summary <- data.frame(
  analysis_mode = "FINAL_LOCKED_TWO_STAGE_CALIBRATION_SLOPE",
  model_id = model_id,
  estimate = slope$estimate,
  lower = slope$lower,
  upper = slope$upper,
  level = slope$level,
  method = slope$method,
  outer_repetitions = validation$outer_repetitions_requested,
  inner_repetitions = validation$inner_repetitions_requested,
  successful_outer_replicates =
    validation$successful_outer_replicates,
  outer_success_fraction = validation$outer_success_fraction,
  point_validation_repetitions =
    point_validation$repetitions_requested,
  point_validation_seed = point_validation$seed,
  two_stage_seed = master_seed,
  reportable_for_manuscript = TRUE,
  stringsAsFactors = FALSE
)
summary_path <- file.path(
  aggregate_out,
  paste0("mimic_", model_id, "_two_stage_slope_summary_v2.csv")
)
v2_pm_atomic_write_csv(summary, summary_path)

outer_audit_path <- file.path(
  qc_out,
  paste0("mimic_", model_id, "_two_stage_outer_audit_v2.csv")
)
inner_failure_path <- file.path(
  qc_out,
  paste0("mimic_", model_id, "_two_stage_inner_failures_v2.csv")
)
outer_failure_summary_path <- file.path(
  qc_out,
  paste0("mimic_", model_id, "_two_stage_failure_summary_v2.csv")
)
v2_pm_atomic_write_csv(validation$outer_audit, outer_audit_path)
v2_pm_atomic_write_csv(validation$inner_failures, inner_failure_path)
v2_pm_atomic_write_csv(
  validation$outer_failure_summary, outer_failure_summary_path
)

input_manifest <- data.frame(
  role = names(source_paths),
  path = unname(source_paths),
  sha256 = unname(source_hashes),
  stringsAsFactors = FALSE
)
input_manifest_path <- file.path(
  qc_out,
  paste0("mimic_", model_id, "_two_stage_input_manifest_v2.csv")
)
v2_pm_atomic_write_csv(input_manifest, input_manifest_path)

checkpoint_manifest <- v2_ts_checkpoint_manifest(
  contract, checkpoint_dir, v2_pm_sha256_file
)
checkpoint_manifest_path <- file.path(
  qc_out,
  paste0("mimic_", model_id, "_two_stage_checkpoint_manifest_v2.csv")
)
v2_pm_atomic_write_csv(
  checkpoint_manifest, checkpoint_manifest_path
)

completion_gate <- data.frame(
  field = c(
    "status", "locked_config_version", "analysis_mode", "model_id",
    "model_index", "contract_hash", "driver_sha256",
    "resume_utils_sha256", "primary_completion_gate_sha256",
    "primary_private_manifest_sha256", "mimic_analysis_sha256",
    "point_validation_sha256", "contract_file_sha256",
    "validation_sha256", "summary_sha256",
    "checkpoint_manifest_sha256", "outer_repetitions",
    "inner_repetitions", "successful_outer_replicates",
    "outer_success_fraction", "minimum_success_fraction",
    "point_validation_repetitions", "point_validation_seed",
    "two_stage_seed", "calibration_slope_interval_supported",
    "primary_model_outputs_overwritten",
    "reportable_for_manuscript", "completed_at"
  ),
  value = c(
    "PASS", LOCKED_V2$version,
    "FINAL_LOCKED_TWO_STAGE_CALIBRATION_SLOPE", model_id,
    model_index, contract_hash,
    source_hashes[["driver"]], source_hashes[["resume_utils"]],
    source_hashes[["primary_completion_gate"]],
    source_hashes[["primary_private_manifest"]],
    source_hashes[["mimic_analysis"]],
    source_hashes[["point_validation"]],
    contract_file_hash, validation_hash,
    v2_pm_sha256_file(summary_path),
    v2_pm_sha256_file(checkpoint_manifest_path),
    outer_repetitions, inner_repetitions,
    validation$successful_outer_replicates,
    validation$outer_success_fraction, minimum_success,
    point_validation$repetitions_requested,
    point_validation$seed, master_seed, "TRUE", "FALSE", "TRUE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(completion_gate, completion_gate_path)

cat(
  "REBUILD_V2_TWO_STAGE_CALIBRATION_SLOPE_PASS\n",
  "Model: ", model_id, "\n",
  "Outer/inner: ", outer_repetitions, "/", inner_repetitions, "\n",
  "Successful outer replicates: ",
  validation$successful_outer_replicates, "\n",
  "Slope estimate (95% CI): ",
  format(slope$estimate, digits = 6), " (",
  format(slope$lower, digits = 6), ", ",
  format(slope$upper, digits = 6), ")\n",
  sep = ""
)
