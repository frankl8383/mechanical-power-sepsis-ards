#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2:
# prespecified complete-GCS sensitivity point estimates.
#
# Outcome artifacts are opened only after the complete-GCS predictor and
# transformation freeze passes. No bootstrap or manuscript-ready confidence
# interval is produced here.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/24_run_complete_gcs_sensitivity.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "12_secondary_sensitivity_utils.R"))
source(file.path(script_dir, "22_complete_gcs_utils.R"))

stopifnot(
  identical(LOCKED_V2$version, "2.0.0"),
  isTRUE(LOCKED_V2$missing_data_hierarchy$sensitivity_complete_gcs)
)

read_gate <- function(path, label) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field) || anyNA(gate$field) ||
      any(!nzchar(gate$field))) {
    stop("Malformed ", label, ": ", path)
  }
  setNames(as.character(gate$value), gate$field)
}

require_gate_value <- function(gate, field, expected, label) {
  value <- unname(gate[field])
  if (length(value) != 1L || is.na(value) || !nzchar(value) ||
      !identical(value, as.character(expected))) {
    stop(
      label, " mismatch for ", field, ": ",
      ifelse(length(value) == 1L, value, "<missing>"),
      " != ", as.character(expected)
    )
  }
  invisible(value)
}

paths <- list(
  freeze_gate = file.path(
    QC_ROOT, "complete_gcs_sensitivity",
    "complete_gcs_predictor_freeze_complete_v2.csv"
  ),
  primary_freeze_gate = file.path(
    QC_ROOT, "primary_model_freeze",
    "phase3_primary_model_freeze_complete_v2.csv"
  ),
  landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  mimic_predictors = file.path(
    PRIVATE_ROOT, "complete_gcs_sensitivity",
    "mimic_complete_gcs_predictors_v2.rds"
  ),
  eicu_predictors = file.path(
    PRIVATE_ROOT, "complete_gcs_sensitivity",
    "eicu_complete_gcs_predictors_v2.rds"
  ),
  frozen_bundle = file.path(
    PRIVATE_ROOT, "complete_gcs_sensitivity",
    "frozen_complete_gcs_transform_bundle_v2.rds"
  ),
  mimic_outcome = file.path(
    PRIVATE_ROOT, "mimic",
    "mimic_fixed6h_landmark_outcomes_v2.rds"
  ),
  eicu_outcome = file.path(
    PRIVATE_ROOT, "eicu",
    "eicu_fixed6h_landmark_outcomes_v2.rds"
  )
)
missing <- names(paths)[!vapply(paths, file.exists, logical(1L))]
if (length(missing)) {
  stop(
    "Missing complete-GCS endpoint input(s): ",
    paste(missing, collapse = ", ")
  )
}

private_out <- file.path(PRIVATE_ROOT, "complete_gcs_sensitivity")
aggregate_out <- file.path(AGGREGATE_ROOT, "complete_gcs_sensitivity")
qc_out <- file.path(QC_ROOT, "complete_gcs_sensitivity")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(
  qc_out, "complete_gcs_sensitivity_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

freeze_gate <- read_gate(
  paths$freeze_gate, "complete-GCS predictor freeze gate"
)
for (pair in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c("decision_id", "V2-D021"),
  c(
    "gcs_source_harmonization",
    "recorded source-specific total; not identical measurement"
  ),
  c("quantile_type", "2"),
  c("parameter_derivation_database", "MIMIC-IV only"),
  c("external_transform_application", "unchanged"),
  c("outcome_artifacts_opened", "FALSE"),
  c("external_outcomes_used", "FALSE"),
  c("endpoint_model_run", "FALSE"),
  c("all_invariants_pass", "TRUE"),
  c("manuscript_ci_ready", "FALSE")
)) {
  require_gate_value(
    freeze_gate, pair[[1L]], pair[[2L]],
    "complete-GCS predictor freeze gate"
  )
}
for (pair in list(
  c("mimic_predictor_sha256", paths$mimic_predictors),
  c("eicu_predictor_sha256", paths$eicu_predictors),
  c("frozen_bundle_sha256", paths$frozen_bundle)
)) {
  require_gate_value(
    freeze_gate,
    pair[[1L]],
    v2_pm_sha256_file(pair[[2L]]),
    "complete-GCS predictor freeze gate"
  )
}

primary_gate <- read_gate(
  paths$primary_freeze_gate, "primary predictor freeze gate"
)
for (pair in list(
  c("status", "PASS"),
  c("mimic_common_set_n", "9861"),
  c("eicu_common_set_n", "1211"),
  c("outcome_fields_read", "FALSE")
)) {
  require_gate_value(
    primary_gate, pair[[1L]], pair[[2L]],
    "primary predictor freeze gate"
  )
}

mimic_predictors <- as.data.frame(readRDS(paths$mimic_predictors))
eicu_predictors <- as.data.frame(readRDS(paths$eicu_predictors))
frozen_bundle <- readRDS(paths$frozen_bundle)
if (!is.list(frozen_bundle) ||
    !identical(
      frozen_bundle$artifact_version,
      "frozen_complete_gcs_bundle_v2"
    ) ||
    !identical(frozen_bundle$locked_config_version, LOCKED_V2$version) ||
    !identical(frozen_bundle$decision_id, "V2-D021") ||
    !identical(frozen_bundle$derivation_database, "MIMIC-IV only") ||
    !identical(frozen_bundle$external_outcomes_used, FALSE) ||
    !identical(frozen_bundle$manuscript_ci_ready, FALSE)) {
  stop("Malformed frozen complete-GCS bundle.")
}
transform_bundle <- frozen_bundle$transform_bundle
if (!identical(transform_bundle$quantile_type, 2L) ||
    !identical(transform_bundle$derivation_database, "MIMIC-IV") ||
    !all(v2_cg_baseline_continuous_variables %in%
         names(transform_bundle$baseline_three_knots))) {
  stop("Malformed frozen complete-GCS transformation bundle.")
}

for (entry in list(
  list(frame = mimic_predictors, database = "MIMIC-IV"),
  list(frame = eicu_predictors, database = "eICU-CRD")
)) {
  metadata <- attr(entry$frame, "freeze_metadata")
  if (!is.list(metadata) ||
      !identical(metadata$decision_id, "V2-D021") ||
      !identical(metadata$outcome_fields_read, FALSE)) {
    stop(entry$database, " complete-GCS freeze metadata is invalid.")
  }
  v2_pm_validate_predictor_frame(
    entry$frame, entry$database, require_complete = TRUE
  )
  v2_pm_assert_outcome_free(
    entry$frame,
    paste(entry$database, "frozen complete-GCS predictors")
  )
  if (anyNA(entry$frame$gcs) ||
      any(entry$frame$gcs < 3 | entry$frame$gcs > 15)) {
    stop(entry$database, " frozen complete-GCS values are invalid.")
  }
  for (model_id in LOCKED_V2$model_ids) {
    design <- v2_cg_build_design(
      entry$frame, model_id, transform_bundle
    )
    if (!all(c("gcs_rcs1", "gcs_rcs2") %in% colnames(design))) {
      stop(entry$database, " design lost the frozen GCS spline.")
    }
  }
}
if (uniqueN(eicu_predictors$hospital_id) < 2L) {
  stop("Complete-GCS external predictors have fewer than two hospitals.")
}

# No outcome artifact is opened above this line.
landmark_gate <- read_gate(paths$landmark_gate, "fixed-landmark gate")
for (pair in list(
  c("locked_config_version", LOCKED_V2$version),
  c("mimic_outcome_sha256", v2_pm_sha256_file(paths$mimic_outcome)),
  c("eicu_outcome_sha256", v2_pm_sha256_file(paths$eicu_outcome))
)) {
  require_gate_value(
    landmark_gate, pair[[1L]], pair[[2L]], "fixed-landmark gate"
  )
}
mimic_analysis <- v2_pm_join_outcome(
  mimic_predictors,
  as.data.frame(readRDS(paths$mimic_outcome)),
  "MIMIC-IV"
)
eicu_analysis <- v2_pm_join_outcome(
  eicu_predictors,
  as.data.frame(readRDS(paths$eicu_outcome)),
  "eICU-CRD"
)
if (length(unique(mimic_analysis$outcome)) != 2L ||
    length(unique(eicu_analysis$outcome)) != 2L ||
    uniqueN(eicu_analysis$hospital_id) < 2L) {
  stop("Complete-GCS endpoint classes or external hospital support failed.")
}

model_roles <- c(
  M0 = "complete-GCS severity baseline",
  M_MP = "linear sMP plus complete-GCS severity baseline",
  M_4DPRR = "linear 4DPRR plus complete-GCS severity baseline",
  M_DPRR =
    "free driving-pressure/RR weights plus complete-GCS severity baseline",
  M_ENERGY =
    "free algebraic-term weights plus complete-GCS severity baseline"
)
model_designers <- setNames(lapply(
  LOCKED_V2$model_ids,
  function(model_id) {
    force(model_id)
    function(x) v2_cg_build_design(
      x, model_id, transform_bundle
    )
  }
), LOCKED_V2$model_ids)

fit_result <- v2_ss_fit_apply(
  mimic_analysis,
  eicu_analysis,
  model_designers,
  model_roles
)
if (any(!fit_result$design_audit$converged)) {
  stop("At least one complete-GCS development model did not converge.")
}

comparisons <- data.frame(
  candidate_model = c(
    "M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY",
    "M_4DPRR", "M_DPRR", "M_ENERGY"
  ),
  reference_model = c(
    "M0", "M0", "M0", "M0",
    "M_MP", "M_MP", "M_MP"
  ),
  comparison_role = c(
    rep("increment_over_complete_gcs_baseline", 4L),
    rep("representation_comparison_to_smp", 3L)
  ),
  stringsAsFactors = FALSE
)
performance <- rbind(
  v2_ss_model_performance(
    mimic_analysis$outcome,
    fit_result$mimic_predictions,
    "MIMIC-IV",
    "complete_gcs_apparent_point_estimate",
    model_roles
  ),
  v2_ss_model_performance(
    eicu_analysis$outcome,
    fit_result$eicu_predictions,
    "eICU-CRD",
    "complete_gcs_external_point_estimate",
    model_roles
  )
)
paired <- rbind(
  v2_ss_paired_differences(
    mimic_analysis$outcome,
    fit_result$mimic_predictions,
    comparisons,
    "MIMIC-IV",
    "complete_gcs_apparent_point_estimate"
  ),
  v2_ss_paired_differences(
    eicu_analysis$outcome,
    fit_result$eicu_predictions,
    comparisons,
    "eICU-CRD",
    "complete_gcs_external_point_estimate"
  )
)

coefficient_table <- do.call(rbind, lapply(
  names(fit_result$fits),
  function(model_id) {
    estimate <- fit_result$fits[[model_id]]$coefficients
    data.frame(
      model_id = model_id,
      term = names(estimate),
      log_odds_coefficient = as.numeric(estimate),
      odds_ratio = exp(as.numeric(estimate)),
      point_estimate_only = TRUE,
      stringsAsFactors = FALSE
    )
  }
))

sample_qc <- rbind(
  data.frame(
    database = "MIMIC-IV",
    all_fixed6h_tuple_n =
      as.integer(freeze_gate[["mimic_all_tuple_n"]]),
    no_gcs_primary_common_n =
      as.integer(primary_gate[["mimic_common_set_n"]]),
    complete_gcs_common_n = nrow(mimic_analysis),
    retained_vs_no_gcs_primary_fraction =
      nrow(mimic_analysis) /
        as.integer(primary_gate[["mimic_common_set_n"]]),
    events = sum(mimic_analysis$outcome),
    non_events = sum(mimic_analysis$outcome == 0L),
    event_rate = mean(mimic_analysis$outcome),
    hospitals = uniqueN(mimic_analysis$hospital_id),
    gcs_minimum = min(mimic_analysis$gcs),
    gcs_median = median(mimic_analysis$gcs),
    gcs_maximum = max(mimic_analysis$gcs),
    bootstrap_replicates = 0L,
    manuscript_ci_ready = FALSE,
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    all_fixed6h_tuple_n =
      as.integer(freeze_gate[["eicu_all_tuple_n"]]),
    no_gcs_primary_common_n =
      as.integer(primary_gate[["eicu_common_set_n"]]),
    complete_gcs_common_n = nrow(eicu_analysis),
    retained_vs_no_gcs_primary_fraction =
      nrow(eicu_analysis) /
        as.integer(primary_gate[["eicu_common_set_n"]]),
    events = sum(eicu_analysis$outcome),
    non_events = sum(eicu_analysis$outcome == 0L),
    event_rate = mean(eicu_analysis$outcome),
    hospitals = uniqueN(eicu_analysis$hospital_id),
    gcs_minimum = min(eicu_analysis$gcs),
    gcs_median = median(eicu_analysis$gcs),
    gcs_maximum = max(eicu_analysis$gcs),
    bootstrap_replicates = 0L,
    manuscript_ci_ready = FALSE,
    stringsAsFactors = FALSE
  )
)

design_audit <- fit_result$design_audit
design_audit$mimic_eicu_design_columns_identical <- vapply(
  LOCKED_V2$model_ids,
  function(model_id) {
    identical(
      colnames(model_designers[[model_id]](mimic_analysis)),
      colnames(model_designers[[model_id]](eicu_analysis))
    )
  },
  logical(1L)
)
design_audit$gcs_spline_present <- vapply(
  LOCKED_V2$model_ids,
  function(model_id) {
    all(c("gcs_rcs1", "gcs_rcs2") %in%
          colnames(model_designers[[model_id]](mimic_analysis)))
  },
  logical(1L)
)

private_result <- list(
  artifact_version = "complete_gcs_sensitivity_point_estimates_v2",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED_V2$version,
  decision_id = "V2-D021",
  model_roles = model_roles,
  fits = fit_result$fits,
  mimic_analysis_id = mimic_analysis$analysis_id,
  eicu_analysis_id = eicu_analysis$analysis_id,
  mimic_predictions = fit_result$mimic_predictions,
  eicu_predictions = fit_result$eicu_predictions,
  source_harmonization =
    "recorded source-specific total; not identical measurement",
  external_model_application =
    "unchanged MIMIC coefficients and complete-GCS transformations",
  bootstrap_replicates = 0L,
  manuscript_ci_ready = FALSE,
  input_hashes = lapply(paths, v2_pm_sha256_file)
)
private_path <- file.path(
  private_out, "complete_gcs_sensitivity_point_estimates_v2.rds"
)
private_hash <- v2_pm_atomic_save_rds(private_result, private_path)

aggregate_outputs <- list(
  "complete_gcs_endpoint_sample_qc_v2.csv" = sample_qc,
  "complete_gcs_endpoint_design_audit_v2.csv" = design_audit,
  "complete_gcs_coefficients_v2.csv" = coefficient_table,
  "complete_gcs_point_performance_v2.csv" = performance,
  "complete_gcs_paired_differences_v2.csv" = paired
)
for (name in names(aggregate_outputs)) {
  v2_pm_atomic_write_csv(
    aggregate_outputs[[name]], file.path(aggregate_out, name)
  )
}

invariants <- data.frame(
  check = c(
    "predictor_freeze_passed_before_outcome_access",
    "complete_gcs_samples_are_subsets_of_primary",
    "both_outcome_classes_present",
    "at_least_two_eicu_hospitals",
    "all_models_include_complete_gcs_spline",
    "mimic_eicu_design_columns_identical",
    "all_models_converged",
    "same_patient_model_comparisons",
    "external_coefficients_and_transforms_not_refit",
    "bootstrap_replicates_zero",
    "manuscript_ci_ready_false"
  ),
  pass = c(
    identical(freeze_gate[["status"]], "PASS"),
    nrow(mimic_analysis) <=
      as.integer(primary_gate[["mimic_common_set_n"]]) &&
      nrow(eicu_analysis) <=
        as.integer(primary_gate[["eicu_common_set_n"]]),
    length(unique(mimic_analysis$outcome)) == 2L &&
      length(unique(eicu_analysis$outcome)) == 2L,
    uniqueN(eicu_analysis$hospital_id) >= 2L,
    all(design_audit$gcs_spline_present),
    all(design_audit$mimic_eicu_design_columns_identical),
    all(design_audit$converged),
    all(design_audit$mimic_n == nrow(mimic_analysis)) &&
      all(design_audit$eicu_n == nrow(eicu_analysis)),
    identical(
      private_result$external_model_application,
      "unchanged MIMIC coefficients and complete-GCS transformations"
    ),
    identical(private_result$bootstrap_replicates, 0L),
    identical(private_result$manuscript_ci_ready, FALSE)
  ),
  stringsAsFactors = FALSE
)
if (any(!invariants$pass)) {
  stop(
    "Complete-GCS endpoint invariant failure: ",
    paste(invariants$check[!invariants$pass], collapse = ", ")
  )
}
v2_pm_atomic_write_csv(
  invariants,
  file.path(qc_out, "complete_gcs_sensitivity_invariants_v2.csv")
)

input_manifest <- data.frame(
  artifact = names(paths),
  path = unname(unlist(paths, use.names = FALSE)),
  sha256 = vapply(paths, v2_pm_sha256_file, character(1L)),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  input_manifest,
  file.path(qc_out, "complete_gcs_sensitivity_input_manifest_v2.csv")
)
output_manifest <- data.frame(
  artifact = c("private_result", names(aggregate_outputs)),
  path = c(
    private_path,
    file.path(aggregate_out, names(aggregate_outputs))
  ),
  sha256 = c(
    private_hash,
    vapply(
      names(aggregate_outputs),
      function(name) {
        v2_pm_sha256_file(file.path(aggregate_out, name))
      },
      character(1L)
    )
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  output_manifest,
  file.path(qc_out, "complete_gcs_sensitivity_output_manifest_v2.csv")
)

gate <- data.frame(
  field = c(
    "status",
    "locked_config_version",
    "decision_id",
    "script_sha256",
    "utils_sha256",
    "secondary_utils_sha256",
    "freeze_gate_sha256",
    "private_result_sha256",
    "mimic_n",
    "mimic_events",
    "eicu_n",
    "eicu_events",
    "eicu_hospitals",
    "quantile_type",
    "parameter_derivation_database",
    "same_patient_comparisons",
    "external_coefficients_and_transforms_applied_unchanged",
    "gcs_source_harmonization",
    "bootstrap_replicates",
    "manuscript_ci_ready",
    "all_invariants_pass",
    "completed_at"
  ),
  value = c(
    "PASS",
    LOCKED_V2$version,
    "V2-D021",
    v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(
      file.path(script_dir, "22_complete_gcs_utils.R")
    ),
    v2_pm_sha256_file(
      file.path(script_dir, "12_secondary_sensitivity_utils.R")
    ),
    v2_pm_sha256_file(paths$freeze_gate),
    private_hash,
    as.character(nrow(mimic_analysis)),
    as.character(sum(mimic_analysis$outcome)),
    as.character(nrow(eicu_analysis)),
    as.character(sum(eicu_analysis$outcome)),
    as.character(uniqueN(eicu_analysis$hospital_id)),
    as.character(transform_bundle$quantile_type),
    "MIMIC-IV only",
    "TRUE",
    "TRUE",
    "recorded source-specific total; not identical measurement",
    "0",
    "FALSE",
    "TRUE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

message("REBUILD_V2_COMPLETE_GCS_SENSITIVITY_PASS")
message(
  "  MIMIC: ", nrow(mimic_analysis), " (",
  sum(mimic_analysis$outcome), " events); eICU: ",
  nrow(eicu_analysis), " (", sum(eicu_analysis$outcome),
  " events) across ", uniqueN(eicu_analysis$hospital_id),
  " hospitals"
)
