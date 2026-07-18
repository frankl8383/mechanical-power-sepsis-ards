#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2: finalize the five locked two-stage
# calibration-slope intervals.
#
# This script fails closed unless every model-specific completion gate passes.
# It then publishes one combined manuscript summary and a separate global gate.
# It does not overwrite fitted models, predictions, bootstrap checkpoints, or
# the phase-4 primary-model completion gate.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/13_finalize_two_stage_calibration_slope.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))

read_field_gate <- function(path) {
  gate <- fread(path, showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field)) {
    stop("Malformed field/value gate: ", path)
  }
  setNames(as.character(gate$value), gate$field)
}

assert_gate_value <- function(gate, field, expected, label) {
  observed <- unname(gate[[field]])
  if (is.null(observed) || !identical(observed, as.character(expected))) {
    stop(
      label, " mismatch for ", field, ": expected ", expected,
      ", observed ", if (is.null(observed)) "<missing>" else observed
    )
  }
  invisible(TRUE)
}

model_ids <- LOCKED_V2$model_ids
outer_expected <- LOCKED_V2$bootstrap$calibration_slope_outer_replicates
inner_expected <- LOCKED_V2$bootstrap$calibration_slope_inner_replicates
minimum_success <- LOCKED_V2$bootstrap$minimum_success_fraction

stopifnot(
  identical(model_ids, c("M0", "M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY")),
  identical(outer_expected, 1000L),
  identical(inner_expected, 200L),
  identical(minimum_success, 0.95)
)

per_model_aggregate <- file.path(AGGREGATE_ROOT, "two_stage_calibration")
per_model_qc <- file.path(QC_ROOT, "two_stage_calibration")
combined_path <- file.path(
  AGGREGATE_ROOT, "primary_models",
  "mimic_two_stage_slope_summary_v2.csv"
)
global_gate_path <- file.path(
  per_model_qc, "two_stage_calibration_complete_v2.csv"
)
input_manifest_path <- file.path(
  per_model_qc, "two_stage_calibration_input_manifest_v2.csv"
)

summary_rows <- vector("list", length(model_ids))
manifest_rows <- list()

for (index in seq_along(model_ids)) {
  model_id <- model_ids[[index]]
  gate_path <- file.path(
    per_model_qc, model_id,
    paste0("mimic_", model_id, "_two_stage_complete_v2.csv")
  )
  summary_path <- file.path(
    per_model_aggregate,
    paste0("mimic_", model_id, "_two_stage_slope_summary_v2.csv")
  )
  if (!file.exists(gate_path) || !file.exists(summary_path)) {
    stop(
      "Two-stage finalization blocked; missing completed artifact for ",
      model_id, "."
    )
  }

  gate <- read_field_gate(gate_path)
  label <- paste0("Two-stage gate ", model_id)
  for (item in list(
    c("status", "PASS"),
    c("locked_config_version", LOCKED_V2$version),
    c("analysis_mode", "FINAL_LOCKED_TWO_STAGE_CALIBRATION_SLOPE"),
    c("model_id", model_id),
    c("model_index", index),
    c("outer_repetitions", outer_expected),
    c("inner_repetitions", inner_expected),
    c("minimum_success_fraction", minimum_success),
    c("calibration_slope_interval_supported", "TRUE"),
    c("primary_model_outputs_overwritten", "FALSE"),
    c("reportable_for_manuscript", "TRUE")
  )) {
    assert_gate_value(gate, item[[1L]], item[[2L]], label)
  }
  success_fraction <- as.numeric(gate[["outer_success_fraction"]])
  if (!is.finite(success_fraction) || success_fraction < minimum_success) {
    stop(label, " failed the minimum outer success fraction.")
  }

  summary <- fread(summary_path, showProgress = FALSE)
  required_columns <- c(
    "analysis_mode", "model_id", "estimate", "lower", "upper",
    "level", "method", "outer_repetitions", "inner_repetitions",
    "successful_outer_replicates", "outer_success_fraction",
    "point_validation_repetitions", "point_validation_seed",
    "two_stage_seed", "reportable_for_manuscript"
  )
  if (nrow(summary) != 1L ||
      !identical(names(summary), required_columns)) {
    stop("Malformed two-stage summary for ", model_id, ".")
  }
  if (!identical(summary$model_id[[1L]], model_id) ||
      !identical(
        summary$analysis_mode[[1L]],
        "FINAL_LOCKED_TWO_STAGE_CALIBRATION_SLOPE"
      ) ||
      summary$outer_repetitions[[1L]] != outer_expected ||
      summary$inner_repetitions[[1L]] != inner_expected ||
      summary$outer_success_fraction[[1L]] < minimum_success ||
      !isTRUE(summary$reportable_for_manuscript[[1L]]) ||
      any(!is.finite(unlist(
        summary[, .(estimate, lower, upper, level)]
      ))) ||
      summary$lower[[1L]] > summary$estimate[[1L]] ||
      summary$estimate[[1L]] > summary$upper[[1L]]) {
    stop("Two-stage summary invariant failed for ", model_id, ".")
  }
  observed_summary_hash <- v2_pm_sha256_file(summary_path)
  if (!identical(gate[["summary_sha256"]], observed_summary_hash)) {
    stop("Two-stage summary hash mismatch for ", model_id, ".")
  }

  summary_rows[[index]] <- as.data.frame(summary)
  manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
    model_id = model_id,
    role = "model_completion_gate",
    path = gate_path,
    sha256 = v2_pm_sha256_file(gate_path),
    stringsAsFactors = FALSE
  )
  manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
    model_id = model_id,
    role = "model_slope_summary",
    path = summary_path,
    sha256 = observed_summary_hash,
    stringsAsFactors = FALSE
  )
}

combined <- rbindlist(summary_rows, use.names = TRUE)
combined[, model_id := factor(model_id, levels = model_ids)]
setorder(combined, model_id)
combined[, model_id := as.character(model_id)]
if (!identical(combined$model_id, model_ids)) {
  stop("Combined two-stage model ordering failed.")
}

manifest <- rbindlist(manifest_rows, use.names = TRUE)
manifest <- rbind(
  manifest,
  data.frame(
    model_id = "",
    role = "finalizer_script",
    path = script_path,
    sha256 = v2_pm_sha256_file(script_path),
    stringsAsFactors = FALSE
  )
)

# Publish only after all five model artifacts and invariants have passed.
v2_pm_atomic_write_csv(as.data.frame(combined), combined_path)
v2_pm_atomic_write_csv(as.data.frame(manifest), input_manifest_path)

global_gate <- data.frame(
  field = c(
    "status", "locked_config_version", "analysis_mode",
    "models_completed", "model_ids", "outer_repetitions",
    "inner_repetitions", "minimum_success_fraction",
    "all_model_gates_pass", "all_intervals_supported",
    "all_reportable_for_manuscript",
    "primary_model_outputs_overwritten",
    "final_manuscript_ci_ready", "combined_summary_sha256",
    "input_manifest_sha256", "finalizer_script_sha256",
    "completed_at"
  ),
  value = c(
    "PASS", LOCKED_V2$version,
    "FINAL_LOCKED_TWO_STAGE_CALIBRATION_SLOPE",
    length(model_ids), paste(model_ids, collapse = "|"),
    outer_expected, inner_expected, minimum_success,
    "TRUE", "TRUE", "TRUE", "FALSE", "TRUE",
    v2_pm_sha256_file(combined_path),
    v2_pm_sha256_file(input_manifest_path),
    v2_pm_sha256_file(script_path),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(global_gate, global_gate_path)

cat(
  "REBUILD_V2_TWO_STAGE_GLOBAL_PASS\n",
  "Models: ", paste(model_ids, collapse = ", "), "\n",
  "Outer/inner per model: ", outer_expected, "/", inner_expected, "\n",
  "Combined summary: ", combined_path, "\n",
  "Global gate: ", global_gate_path, "\n",
  sep = ""
)
