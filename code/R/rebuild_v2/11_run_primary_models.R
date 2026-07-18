#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2: primary model analysis driver
#
# The predictor common sets and transformation bundle must already be frozen by
# 10_freeze_primary_model_frames.R. This script then performs the independent
# outcome join, develops all five models in MIMIC, applies them unchanged to
# eICU, and executes internal/external validation and center-robust analyses.
# No manuscript conclusion is generated here.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/11_run_primary_models.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "03_internal_validation_utils.R"))
source(file.path(script_dir, "04_external_validation_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))

stopifnot(
  identical(LOCKED_V2$version, "2.0.0"),
  identical(LOCKED_V2$model_ids, v2_model_specification()$model_id)
)

parse_boolean <- function(x, label) {
  value <- tolower(trimws(x))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(label, " must be TRUE or FALSE.")
  }
  value %in% c("true", "1", "yes")
}

env_integer <- function(name, default, minimum = 20L) {
  value <- Sys.getenv(name, unset = as.character(default))
  numeric_value <- suppressWarnings(as.numeric(value))
  if (length(numeric_value) != 1L || is.na(numeric_value) ||
      !is.finite(numeric_value) ||
      numeric_value != as.integer(numeric_value) ||
      numeric_value < minimum) {
    stop(name, " must be one integer >= ", minimum, ".")
  }
  as.integer(numeric_value)
}

final_bootstrap <- parse_boolean(
  Sys.getenv("ARDS_V2_FINAL_BOOTSTRAP", unset = "FALSE"),
  "ARDS_V2_FINAL_BOOTSTRAP"
)
run_two_stage <- parse_boolean(
  Sys.getenv(
    "ARDS_V2_RUN_TWO_STAGE",
    unset = if (final_bootstrap) "TRUE" else "FALSE"
  ),
  "ARDS_V2_RUN_TWO_STAGE"
)
internal_repetitions <- if (final_bootstrap) {
  LOCKED_V2$bootstrap$mimic_internal_replicates
} else {
  env_integer("ARDS_V2_INTERNAL_BOOTSTRAP_REPS", 50L)
}
external_repetitions <- if (final_bootstrap) {
  LOCKED_V2$bootstrap$eicu_hospital_cluster_replicates
} else {
  env_integer("ARDS_V2_EXTERNAL_BOOTSTRAP_REPS", 100L)
}
two_stage_outer <- if (final_bootstrap) {
  LOCKED_V2$bootstrap$calibration_slope_outer_replicates
} else {
  env_integer("ARDS_V2_TWO_STAGE_OUTER_REPS", 20L)
}
two_stage_inner <- if (final_bootstrap) {
  LOCKED_V2$bootstrap$calibration_slope_inner_replicates
} else {
  env_integer("ARDS_V2_TWO_STAGE_INNER_REPS", 20L)
}
analysis_mode <- if (final_bootstrap) {
  "FINAL_LOCKED_BOOTSTRAP"
} else {
  "DRY_RUN_BOOTSTRAP_NOT_FOR_MANUSCRIPT_CI"
}

private_model_root <- file.path(PRIVATE_ROOT, "model_ready")
freeze_gate_path <- file.path(
  QC_ROOT, "primary_model_freeze",
  "phase3_primary_model_freeze_complete_v2.csv"
)
landmark_gate_path <- file.path(
  QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
)
mimic_predictor_path <- file.path(
  private_model_root, "mimic_primary_predictor_common_set_v2.rds"
)
eicu_predictor_path <- file.path(
  private_model_root, "eicu_primary_predictor_common_set_v2.rds"
)
bundle_path <- file.path(
  private_model_root, "frozen_transform_bundle_v2.rds"
)
mimic_outcome_path <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_fixed6h_landmark_outcomes_v2.rds"
)
eicu_outcome_path <- file.path(
  PRIVATE_ROOT, "eicu", "eicu_fixed6h_landmark_outcomes_v2.rds"
)

required <- c(
  freeze_gate_path, landmark_gate_path,
  mimic_predictor_path, eicu_predictor_path, bundle_path,
  mimic_outcome_path, eicu_outcome_path
)
if (any(!file.exists(required))) {
  stop(
    "Missing primary-model input(s): ",
    paste(required[!file.exists(required)], collapse = ", ")
  )
}

private_out <- file.path(PRIVATE_ROOT, "primary_models")
aggregate_out <- file.path(AGGREGATE_ROOT, "primary_models")
qc_out <- file.path(QC_ROOT, "primary_models")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(
  qc_out, "phase4_primary_models_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

read_gate <- function(path, label) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (identical(names(gate), c("field", "value"))) {
    if (anyDuplicated(gate$field) || anyNA(gate$field) ||
        any(!nzchar(gate$field))) {
      stop("Malformed field/value ", label, ": ", path)
    }
    return(setNames(as.character(gate$value), gate$field))
  }
  if (nrow(gate) != 1L || anyDuplicated(names(gate))) {
    stop("Malformed one-row ", label, ": ", path)
  }
  setNames(
    vapply(gate, function(x) as.character(x[[1L]]), character(1L)),
    names(gate)
  )
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

# Verify the entire outcome-free freeze before opening outcomes.
freeze_gate <- read_gate(freeze_gate_path, "predictor freeze gate")
require_gate_value(freeze_gate, "status", "PASS", "predictor freeze gate")
require_gate_value(
  freeze_gate,
  "locked_config_version",
  LOCKED_V2$version,
  "predictor freeze gate"
)
for (field in c(
  "outcome_fields_read", "outcome_leakage_guard_pass",
  "timing_and_range_qc_pass", "external_transform_coverage_pass",
  "same_patient_common_set_frozen"
)) {
  expected <- if (field == "outcome_fields_read") "FALSE" else "TRUE"
  require_gate_value(freeze_gate, field, expected, "predictor freeze gate")
}
require_gate_value(
  freeze_gate,
  "mimic_common_set_sha256",
  v2_pm_sha256_file(mimic_predictor_path),
  "predictor freeze gate"
)
require_gate_value(
  freeze_gate,
  "eicu_common_set_sha256",
  v2_pm_sha256_file(eicu_predictor_path),
  "predictor freeze gate"
)
require_gate_value(
  freeze_gate,
  "transform_bundle_sha256",
  v2_pm_sha256_file(bundle_path),
  "predictor freeze gate"
)
require_gate_value(
  freeze_gate,
  "parameter_derivation_database",
  "MIMIC-IV only",
  "predictor freeze gate"
)

mimic_predictors <- as.data.frame(readRDS(mimic_predictor_path))
eicu_predictors <- as.data.frame(readRDS(eicu_predictor_path))
transform_bundle <- readRDS(bundle_path)
mimic_predictor_validation <- v2_pm_validate_predictor_frame(
  transform(mimic_predictors, core_complete = TRUE),
  "MIMIC-IV",
  require_complete = TRUE
)
eicu_predictor_validation <- v2_pm_validate_predictor_frame(
  transform(eicu_predictors, core_complete = TRUE),
  "eICU-CRD",
  require_complete = TRUE
)
if (!identical(
  attr(transform_bundle, "freeze_metadata")$derivation_database,
  "MIMIC-IV only"
)) {
  stop("Transformation bundle is not the frozen MIMIC-only artifact.")
}

# Outcomes are opened only after all outcome-free artifacts pass.
landmark_gate <- read_gate(landmark_gate_path, "landmark gate")
require_gate_value(
  landmark_gate,
  "mimic_outcome_sha256",
  v2_pm_sha256_file(mimic_outcome_path),
  "landmark gate"
)
require_gate_value(
  landmark_gate,
  "eicu_outcome_sha256",
  v2_pm_sha256_file(eicu_outcome_path),
  "landmark gate"
)
mimic_outcomes <- as.data.frame(readRDS(mimic_outcome_path))
eicu_outcomes <- as.data.frame(readRDS(eicu_outcome_path))
mimic_analysis <- v2_pm_join_outcome(
  mimic_predictors, mimic_outcomes, "MIMIC-IV"
)
eicu_analysis <- v2_pm_join_outcome(
  eicu_predictors, eicu_outcomes, "eICU-CRD"
)

sample_qc <- rbind(
  data.frame(
    database = "MIMIC-IV",
    common_set_n = nrow(mimic_analysis),
    events = sum(mimic_analysis$outcome),
    non_events = sum(mimic_analysis$outcome == 0L),
    event_rate = mean(mimic_analysis$outcome),
    hospitals = length(unique(mimic_analysis$hospital_id)),
    recognized_sex_n = sum(
      mimic_analysis$sex_female %in% c(0, 1)
    ),
    complete_predictor_n = sum(stats::complete.cases(
      mimic_analysis[v2_pm_model_columns]
    )),
    unique_id_n = length(unique(mimic_analysis$analysis_id)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    common_set_n = nrow(eicu_analysis),
    events = sum(eicu_analysis$outcome),
    non_events = sum(eicu_analysis$outcome == 0L),
    event_rate = mean(eicu_analysis$outcome),
    hospitals = length(unique(eicu_analysis$hospital_id)),
    recognized_sex_n = sum(
      eicu_analysis$sex_female %in% c(0, 1)
    ),
    complete_predictor_n = sum(stats::complete.cases(
      eicu_analysis[v2_pm_model_columns]
    )),
    unique_id_n = length(unique(eicu_analysis$analysis_id)),
    stringsAsFactors = FALSE
  )
)
if (any(
  sample_qc$recognized_sex_n != sample_qc$common_set_n |
    sample_qc$complete_predictor_n != sample_qc$common_set_n |
    sample_qc$unique_id_n != sample_qc$common_set_n |
    sample_qc$events <= 0L |
    sample_qc$non_events <= 0L
)) {
  stop("Primary common-set ID, completeness, sex, or event QC failed.")
}

# Develop all models in MIMIC using the full-sample frozen bundle.
mimic_fits <- v2_pm_fit_models(mimic_analysis, transform_bundle)
fit_summary <- v2_pm_fit_summary(mimic_fits)
if (length(unique(fit_summary$n)) != 1L ||
    length(unique(fit_summary$events)) != 1L ||
    unique(fit_summary$n) != nrow(mimic_analysis) ||
    unique(fit_summary$events) != sum(mimic_analysis$outcome) ||
    any(!fit_summary$converged)) {
  stop("The five development models do not share one successful patient set.")
}
coefficient_table <- v2_pm_coefficient_table(
  mimic_fits, mimic_analysis, transform_bundle
)
constraint_tests <- v2_pm_likelihood_ratio_tests(mimic_fits)
collinearity <- v2_pm_collinearity_audits(mimic_analysis)

mimic_prediction_matrix <- v2_pm_predict_models(
  mimic_fits, mimic_predictors, transform_bundle
)
eicu_prediction_matrix <- v2_pm_predict_models(
  mimic_fits, eicu_predictors, transform_bundle
)
if (!identical(
  colnames(mimic_prediction_matrix),
  colnames(eicu_prediction_matrix)
) || !identical(
  colnames(eicu_prediction_matrix),
  LOCKED_V2$model_ids
)) {
  stop("Frozen MIMIC/eICU prediction model ordering differs.")
}

mimic_apparent <- v2_ev_performance_wide(
  mimic_analysis$outcome,
  mimic_prediction_matrix,
  "mimic_apparent_full_sample"
)

prediction_column_names <- paste0("prediction_", LOCKED_V2$model_ids)
model_column_map <- setNames(
  prediction_column_names, LOCKED_V2$model_ids
)
mimic_prediction_frame <- data.frame(
  analysis_id = mimic_analysis$analysis_id,
  hospital_id = mimic_analysis$hospital_id,
  outcome = mimic_analysis$outcome,
  mimic_prediction_matrix,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
names(mimic_prediction_frame)[
  seq.int(ncol(mimic_prediction_frame) - length(LOCKED_V2$model_ids) + 1L,
          ncol(mimic_prediction_frame))
] <- prediction_column_names
eicu_prediction_frame <- data.frame(
  analysis_id = eicu_analysis$analysis_id,
  hospital_id = eicu_analysis$hospital_id,
  outcome = eicu_analysis$outcome,
  eicu_prediction_matrix,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
names(eicu_prediction_frame)[
  seq.int(ncol(eicu_prediction_frame) - length(LOCKED_V2$model_ids) + 1L,
          ncol(eicu_prediction_frame))
] <- prediction_column_names

external_set <- v2_ev_prediction_set(
  eicu_prediction_frame,
  id_column = "analysis_id",
  outcome_column = "outcome",
  hospital_column = "hospital_id",
  model_columns = model_column_map,
  set_id = "eicu_locked_mimic_predictions_primary_common_set_v2"
)

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
    rep("increment_over_baseline", 4L),
    rep("representation_comparison_to_smp", 3L)
  ),
  stringsAsFactors = FALSE
)
comparison_core <- comparisons[c("candidate_model", "reference_model")]

external_raw <- v2_ev_raw_performance(external_set)
external_paired <- v2_ev_paired_differences(
  external_set, comparison_core
)
external_paired <- merge(
  external_paired,
  comparisons,
  by = c("candidate_model", "reference_model"),
  all.x = TRUE,
  sort = FALSE
)
external_updates <- v2_ev_external_model_updates(external_set)
largest_center <- v2_ev_largest_hospital_exclusion(
  external_set, comparison_core
)
equal_center <- v2_ev_equal_hospital_performance(
  external_set,
  minimum_hospital_n =
    LOCKED_V2$center_robustness$equal_center_minimum_n,
  comparisons = comparison_core
)
loho <- v2_ev_leave_one_hospital_out(
  external_set, comparison_core
)
if (loho$failed_hospitals > 0L) {
  stop("At least one leave-one-hospital-out analysis failed.")
}
flexible_calibration <- v2_ev_flexible_calibration_data(
  external_set
)

# Internal validation: transformations are re-derived inside every resampled
# training set. The full-sample bundle is not supplied to these callbacks.
internal_refit_contract <- do.call(rbind, lapply(
  LOCKED_V2$model_ids,
  function(model_id) {
    v2_pm_internal_refit_contract_audit(
      mimic_analysis, model_id
    )
  }
))
if (any(!internal_refit_contract$pass)) {
  stop("Internal bootstrap transformation-refit contract failed.")
}

internal_validation <- setNames(lapply(
  seq_along(LOCKED_V2$model_ids),
  function(i) {
    model_id <- LOCKED_V2$model_ids[[i]]
    result <- v2_harrell_internal_validation(
      data = mimic_analysis,
      outcome = "outcome",
      fit_pipeline = v2_pm_internal_fit_factory(model_id),
      predict_pipeline = v2_pm_internal_predict_factory(model_id),
      repetitions = internal_repetitions,
      seed = LOCKED_V2$bootstrap$seed_mimic + i - 1L,
      minimum_success_fraction =
        LOCKED_V2$bootstrap$minimum_success_fraction,
      pipeline_id = paste0(
        model_id, "_rederive_transform_bundle_in_each_training_resample"
      )
    )
    v2_iv_assert_reportable(result)
    result
  }
), LOCKED_V2$model_ids)

internal_summary <- do.call(rbind, lapply(
  names(internal_validation),
  function(model_id) {
    result <- internal_validation[[model_id]]
    ci <- result$location_shifted_ci
    data.frame(
      analysis_mode = analysis_mode,
      model_id = model_id,
      metric = result$metrics,
      apparent = as.numeric(result$apparent[result$metrics]),
      mean_optimism =
        as.numeric(result$mean_optimism[result$metrics]),
      optimism_corrected =
        as.numeric(result$corrected[result$metrics]),
      lower = ci$lower[match(result$metrics, ci$metric)],
      upper = ci$upper[match(result$metrics, ci$metric)],
      ci_supported =
        ci$supported[match(result$metrics, ci$metric)],
      ci_reason = ci$reason[match(result$metrics, ci$metric)],
      ci_method = ci$method[match(result$metrics, ci$metric)],
      repetitions_requested = result$repetitions_requested,
      successful_replicates = result$successful_replicates,
      success_fraction = result$success_fraction,
      reportable_for_manuscript =
        (
          final_bootstrap &&
          result$repetitions_requested ==
            LOCKED_V2$bootstrap$mimic_internal_replicates
        ) &
        ci$supported[match(result$metrics, ci$metric)],
      stringsAsFactors = FALSE
    )
  }
))
internal_failure_audit <- do.call(rbind, lapply(
  names(internal_validation),
  function(model_id) {
    audit <- internal_validation[[model_id]]$audit
    audit$model_id <- model_id
    audit[c("model_id", "replicate", "success", "reason")]
  }
))

two_stage_validation <- NULL
two_stage_summary <- data.frame(
  analysis_mode = analysis_mode,
  model_id = LOCKED_V2$model_ids,
  status = if (run_two_stage) "RUNNING" else "NOT_RUN_IN_THIS_DRIVER_INVOCATION",
  outer_repetitions = if (run_two_stage) two_stage_outer else NA_integer_,
  inner_repetitions = if (run_two_stage) two_stage_inner else NA_integer_,
  stringsAsFactors = FALSE
)
if (run_two_stage) {
  two_stage_validation <- setNames(lapply(
    seq_along(LOCKED_V2$model_ids),
    function(i) {
      model_id <- LOCKED_V2$model_ids[[i]]
      result <- v2_two_stage_internal_validation(
        data = mimic_analysis,
        outcome = "outcome",
        fit_pipeline = v2_pm_internal_fit_factory(model_id),
        predict_pipeline = v2_pm_internal_predict_factory(model_id),
        outer_repetitions = two_stage_outer,
        inner_repetitions = two_stage_inner,
        seed = LOCKED_V2$bootstrap$seed_sensitivity + i - 1L,
        minimum_inner_success_fraction =
          LOCKED_V2$bootstrap$minimum_success_fraction,
        minimum_outer_success_fraction =
          LOCKED_V2$bootstrap$minimum_success_fraction,
        pipeline_id = internal_validation[[model_id]]$pipeline_id,
        point_validation = internal_validation[[model_id]]
      )
      v2_iv_assert_reportable(result)
      result
    }
  ), LOCKED_V2$model_ids)
  two_stage_summary <- do.call(rbind, lapply(
    names(two_stage_validation),
    function(model_id) {
      result <- two_stage_validation[[model_id]]
      slope <- result$confidence_interval[
        result$confidence_interval$metric == "calibration_slope",
        ,
        drop = FALSE
      ]
      data.frame(
        analysis_mode = analysis_mode,
        model_id = model_id,
        status = if (slope$supported) "SUPPORTED" else "UNSUPPORTED",
        estimate = slope$estimate,
        lower = slope$lower,
        upper = slope$upper,
        method = slope$method,
        outer_repetitions = result$outer_repetitions_requested,
        inner_repetitions = result$inner_repetitions_requested,
        successful_outer_replicates =
          result$successful_outer_replicates,
        outer_success_fraction = result$outer_success_fraction,
        reportable_for_manuscript =
          final_bootstrap &&
          result$outer_repetitions_requested ==
            LOCKED_V2$bootstrap$calibration_slope_outer_replicates &&
          result$inner_repetitions_requested ==
            LOCKED_V2$bootstrap$calibration_slope_inner_replicates,
        stringsAsFactors = FALSE
      )
    }
  ))
}

external_bootstrap <- v2_ev_cluster_bootstrap(
  external_set,
  comparisons = comparison_core,
  repetitions = external_repetitions,
  seed = LOCKED_V2$bootstrap$seed_eicu,
  minimum_success_fraction =
    LOCKED_V2$bootstrap$minimum_success_fraction,
  keep_replicates = TRUE
)
v2_ev_assert_bootstrap_reportable(external_bootstrap)
external_bootstrap$model_summary$analysis_mode <- analysis_mode
external_bootstrap$model_summary$reportable_for_manuscript <-
  final_bootstrap &&
  external_bootstrap$requested_replicates ==
    LOCKED_V2$bootstrap$eicu_hospital_cluster_replicates
external_bootstrap$paired_difference_summary$analysis_mode <-
  analysis_mode
external_bootstrap$paired_difference_summary$
  reportable_for_manuscript <-
  final_bootstrap &&
  external_bootstrap$requested_replicates ==
    LOCKED_V2$bootstrap$eicu_hospital_cluster_replicates

calibration_bootstrap <- v2_ev_cluster_bootstrap_calibration_bands(
  external_set,
  repetitions = external_repetitions,
  seed = LOCKED_V2$bootstrap$seed_sensitivity,
  minimum_success_fraction =
    LOCKED_V2$bootstrap$minimum_success_fraction,
  keep_replicates = TRUE
)
if (!calibration_bootstrap$reportable) {
  stop("External flexible-calibration bootstrap failed its success gate.")
}
calibration_curve <- calibration_bootstrap$curve_with_pointwise_band
calibration_curve$analysis_mode <- analysis_mode
calibration_curve$reportable_for_manuscript <-
  final_bootstrap &&
  calibration_bootstrap$requested_replicates ==
    LOCKED_V2$bootstrap$eicu_hospital_cluster_replicates

# Hospital-identifiable details remain private. Aggregate outputs omit IDs.
hospital_support_private <- do.call(rbind, lapply(
  sort(unique(eicu_analysis$hospital_id)),
  function(id) {
    rows <- eicu_analysis$hospital_id == id
    data.frame(
      hospital_id = id,
      n = sum(rows),
      events = sum(eicu_analysis$outcome[rows]),
      event_rate = mean(eicu_analysis$outcome[rows]),
      stringsAsFactors = FALSE
    )
  }
))
hospital_support_summary <- data.frame(
  hospitals = nrow(hospital_support_private),
  hospitals_with_events = sum(hospital_support_private$events > 0L),
  hospitals_with_non_events = sum(
    hospital_support_private$n - hospital_support_private$events > 0L
  ),
  hospitals_with_at_least_10_patients = sum(
    hospital_support_private$n >= 10L
  ),
  minimum_n = min(hospital_support_private$n),
  median_n = stats::median(hospital_support_private$n),
  maximum_n = max(hospital_support_private$n),
  minimum_events = min(hospital_support_private$events),
  median_events = stats::median(hospital_support_private$events),
  maximum_events = max(hospital_support_private$events),
  largest_center_n = largest_center$excluded_n,
  largest_center_events = largest_center$excluded_events,
  largest_center_fraction =
    largest_center$excluded_n / nrow(eicu_analysis),
  stringsAsFactors = FALSE
)

loho_model_summary <- if (nrow(loho$model_influence)) {
  do.call(rbind, lapply(
    split(
      loho$model_influence,
      interaction(
        loho$model_influence$model_id,
        loho$model_influence$metric,
        drop = TRUE
      )
    ),
    function(x) {
      data.frame(
        model_id = x$model_id[[1L]],
        metric = x$metric[[1L]],
        omissions = nrow(x),
        minimum_estimate = min(x$estimate),
        maximum_estimate = max(x$estimate),
        maximum_absolute_change_from_full =
          max(abs(x$change_from_full)),
        stringsAsFactors = FALSE
      )
    }
  ))
} else {
  data.frame()
}
loho_difference_summary <- if (nrow(loho$paired_difference_influence)) {
  do.call(rbind, lapply(
    split(
      loho$paired_difference_influence,
      interaction(
        loho$paired_difference_influence$candidate_model,
        loho$paired_difference_influence$reference_model,
        loho$paired_difference_influence$metric,
        drop = TRUE
      )
    ),
    function(x) {
      data.frame(
        candidate_model = x$candidate_model[[1L]],
        reference_model = x$reference_model[[1L]],
        metric = x$metric[[1L]],
        omissions = nrow(x),
        minimum_estimate = min(x$estimate),
        maximum_estimate = max(x$estimate),
        maximum_absolute_change_from_full =
          max(abs(x$change_from_full)),
        stringsAsFactors = FALSE
      )
    }
  ))
} else {
  data.frame()
}

largest_center_summary <- data.frame(
  analysis = largest_center$analysis,
  excluded_n = largest_center$excluded_n,
  excluded_events = largest_center$excluded_events,
  retained_n = largest_center$retained_n,
  retained_events = largest_center$retained_events,
  retained_hospitals = largest_center$retained_hospitals,
  stringsAsFactors = FALSE
)

# Row-level and hospital-identifiable artifacts.
private_hashes <- c(
  mimic_analysis = v2_pm_atomic_save_rds(
    mimic_analysis,
    file.path(private_out, "mimic_primary_analysis_frame_v2.rds")
  ),
  eicu_analysis = v2_pm_atomic_save_rds(
    eicu_analysis,
    file.path(private_out, "eicu_primary_analysis_frame_v2.rds")
  ),
  mimic_fits = v2_pm_atomic_save_rds(
    mimic_fits,
    file.path(private_out, "mimic_primary_fits_v2.rds")
  ),
  mimic_predictions = v2_pm_atomic_save_rds(
    mimic_prediction_frame,
    file.path(private_out, "mimic_frozen_predictions_v2.rds")
  ),
  eicu_predictions = v2_pm_atomic_save_rds(
    eicu_prediction_frame,
    file.path(private_out, "eicu_frozen_predictions_v2.rds")
  ),
  internal_validation = v2_pm_atomic_save_rds(
    internal_validation,
    file.path(private_out, "mimic_internal_validation_v2.rds")
  ),
  external_bootstrap = v2_pm_atomic_save_rds(
    external_bootstrap,
    file.path(private_out, "eicu_external_cluster_bootstrap_v2.rds")
  ),
  calibration_bootstrap = v2_pm_atomic_save_rds(
    calibration_bootstrap,
    file.path(private_out, "eicu_calibration_cluster_bootstrap_v2.rds")
  ),
  center_analyses = v2_pm_atomic_save_rds(
    list(
      largest_center = largest_center,
      equal_center = equal_center,
      leave_one_hospital_out = loho,
      hospital_support = hospital_support_private
    ),
    file.path(private_out, "eicu_center_robustness_private_v2.rds")
  ),
  external_updates = v2_pm_atomic_save_rds(
    external_updates,
    file.path(private_out, "eicu_external_model_updates_private_v2.rds")
  )
)
if (run_two_stage) {
  private_hashes <- c(
    private_hashes,
    two_stage_validation = v2_pm_atomic_save_rds(
      two_stage_validation,
      file.path(
        private_out, "mimic_two_stage_internal_validation_v2.rds"
      )
    )
  )
}

# Disclosure-safe point estimates, validation summaries, and QC.
aggregate_tables <- list(
  primary_model_fit_summary_v2 = fit_summary,
  primary_model_coefficients_v2 = coefficient_table,
  primary_constraint_lrt_v2 = constraint_tests,
  mimic_apparent_performance_v2 = mimic_apparent,
  eicu_raw_external_performance_v2 = external_raw,
  eicu_raw_external_paired_differences_v2 = external_paired,
  eicu_external_bootstrap_model_summary_v2 =
    external_bootstrap$model_summary,
  eicu_external_bootstrap_paired_summary_v2 =
    external_bootstrap$paired_difference_summary,
  mimic_internal_validation_summary_v2 = internal_summary,
  mimic_two_stage_slope_summary_v2 = two_stage_summary,
  eicu_largest_center_exclusion_summary_v2 = largest_center_summary,
  eicu_largest_center_exclusion_performance_v2 =
    largest_center$model_performance,
  eicu_largest_center_exclusion_paired_v2 =
    largest_center$paired_differences,
  eicu_equal_center_performance_v2 = equal_center$performance,
  eicu_equal_center_paired_v2 = equal_center$paired_differences,
  eicu_loho_model_influence_summary_v2 = loho_model_summary,
  eicu_loho_paired_influence_summary_v2 = loho_difference_summary,
  eicu_hospital_support_summary_v2 = hospital_support_summary,
  eicu_flexible_calibration_curve_v2 = calibration_curve,
  eicu_prediction_distribution_v2 =
    calibration_bootstrap$prediction_distribution,
  eicu_external_update_performance_v2 =
    external_updates$update_performance,
  primary_collinearity_summary_v2 = collinearity$summary,
  primary_pressure_rate_vif_v2 = collinearity$pressure_rate_vif,
  primary_algebraic_vif_v2 = collinearity$algebraic_vif
)
for (name in names(aggregate_tables)) {
  table <- aggregate_tables[[name]]
  if (!is.data.frame(table)) stop("Aggregate output is not tabular: ", name)
  v2_pm_atomic_write_csv(
    table,
    file.path(aggregate_out, paste0(name, ".csv"))
  )
}

v2_pm_atomic_write_csv(
  sample_qc,
  file.path(qc_out, "primary_analysis_sample_qc_v2.csv")
)
v2_pm_atomic_write_csv(
  internal_refit_contract,
  file.path(qc_out, "internal_bootstrap_refit_contract_v2.csv")
)
v2_pm_atomic_write_csv(
  internal_failure_audit,
  file.path(qc_out, "internal_bootstrap_replicate_audit_v2.csv")
)
v2_pm_atomic_write_csv(
  external_bootstrap$audit,
  file.path(qc_out, "external_cluster_bootstrap_replicate_audit_v2.csv")
)
v2_pm_atomic_write_csv(
  calibration_bootstrap$audit,
  file.path(qc_out, "calibration_bootstrap_replicate_audit_v2.csv")
)
v2_pm_atomic_write_csv(
  as.data.frame(collinearity$pressure_rate_correlation),
  file.path(qc_out, "pressure_rate_correlation_matrix_v2.csv")
)
v2_pm_atomic_write_csv(
  as.data.frame(collinearity$algebraic_correlation),
  file.path(qc_out, "algebraic_term_correlation_matrix_v2.csv")
)

manifest_paths <- c(
  script = script_path,
  config = file.path(script_dir, "00_config.R"),
  analysis_utils = file.path(script_dir, "01_analysis_utils.R"),
  internal_validation_utils =
    file.path(script_dir, "03_internal_validation_utils.R"),
  external_validation_utils =
    file.path(script_dir, "04_external_validation_utils.R"),
  primary_model_utils =
    file.path(script_dir, "09_primary_model_utils.R"),
  predictor_freeze_gate = freeze_gate_path,
  landmark_gate = landmark_gate_path,
  mimic_predictors = mimic_predictor_path,
  eicu_predictors = eicu_predictor_path,
  transform_bundle = bundle_path,
  mimic_outcomes = mimic_outcome_path,
  eicu_outcomes = eicu_outcome_path
)
manifest <- data.frame(
  role = names(manifest_paths),
  path = unname(manifest_paths),
  sha256 = unname(vapply(
    manifest_paths, v2_pm_sha256_file, character(1L)
  )),
  row_level = names(manifest_paths) %in% c(
    "mimic_predictors", "eicu_predictors",
    "mimic_outcomes", "eicu_outcomes"
  ),
  stringsAsFactors = FALSE
)
private_manifest <- data.frame(
  role = names(private_hashes),
  path = file.path(
    private_out,
    c(
      "mimic_primary_analysis_frame_v2.rds",
      "eicu_primary_analysis_frame_v2.rds",
      "mimic_primary_fits_v2.rds",
      "mimic_frozen_predictions_v2.rds",
      "eicu_frozen_predictions_v2.rds",
      "mimic_internal_validation_v2.rds",
      "eicu_external_cluster_bootstrap_v2.rds",
      "eicu_calibration_cluster_bootstrap_v2.rds",
      "eicu_center_robustness_private_v2.rds",
      "eicu_external_model_updates_private_v2.rds",
      if (run_two_stage) {
        "mimic_two_stage_internal_validation_v2.rds"
      } else {
        character()
      }
    )
  ),
  sha256 = unname(private_hashes),
  row_level = TRUE,
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  manifest,
  file.path(qc_out, "primary_model_input_manifest_v2.csv")
)
v2_pm_atomic_write_csv(
  private_manifest,
  file.path(qc_out, "primary_model_private_output_manifest_v2.csv")
)

gate <- data.frame(
  field = c(
    "status", "locked_config_version", "analysis_mode",
    "script_sha256", "predictor_freeze_gate_sha256",
    "mimic_predictor_sha256", "eicu_predictor_sha256",
    "transform_bundle_sha256", "mimic_outcome_sha256",
    "eicu_outcome_sha256", "mimic_n", "mimic_events",
    "eicu_n", "eicu_events", "models_fit",
    "same_patient_comparison_pass", "recognized_sex_complete_pass",
    "outcome_join_exact_and_order_preserved",
    "transform_bundle_full_model_mimic_only",
    "bootstrap_transform_rederived_each_training_resample",
    "internal_bootstrap_repetitions",
    "internal_bootstrap_all_success_gates_pass",
    "external_hospital_bootstrap_repetitions",
    "external_bootstrap_success_fraction",
    "external_bootstrap_gate_pass",
    "calibration_bootstrap_success_fraction",
    "calibration_bootstrap_gate_pass",
    "two_stage_executed", "two_stage_outer_repetitions",
    "two_stage_inner_repetitions",
    "raw_external_reported_before_model_updating",
    "largest_center_exclusion_complete",
    "equal_center_complete", "loho_complete",
    "final_manuscript_ci_ready"
  ),
  value = c(
    "PASS", LOCKED_V2$version, analysis_mode,
    v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(freeze_gate_path),
    v2_pm_sha256_file(mimic_predictor_path),
    v2_pm_sha256_file(eicu_predictor_path),
    v2_pm_sha256_file(bundle_path),
    v2_pm_sha256_file(mimic_outcome_path),
    v2_pm_sha256_file(eicu_outcome_path),
    nrow(mimic_analysis), sum(mimic_analysis$outcome),
    nrow(eicu_analysis), sum(eicu_analysis$outcome),
    length(mimic_fits), "TRUE", "TRUE", "TRUE", "TRUE", "TRUE",
    internal_repetitions,
    all(vapply(
      internal_validation, function(x) x$reportable, logical(1L)
    )),
    external_repetitions,
    external_bootstrap$success_fraction,
    external_bootstrap$reportable,
    calibration_bootstrap$success_fraction,
    calibration_bootstrap$reportable,
    run_two_stage,
    if (run_two_stage) two_stage_outer else NA,
    if (run_two_stage) two_stage_inner else NA,
    "TRUE", "TRUE", "TRUE", loho$failed_hospitals == 0L,
    final_bootstrap && run_two_stage &&
      internal_repetitions ==
        LOCKED_V2$bootstrap$mimic_internal_replicates &&
      external_repetitions ==
        LOCKED_V2$bootstrap$eicu_hospital_cluster_replicates &&
      two_stage_outer ==
        LOCKED_V2$bootstrap$calibration_slope_outer_replicates &&
      two_stage_inner ==
        LOCKED_V2$bootstrap$calibration_slope_inner_replicates
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

cat(
  "REBUILD_V2_PRIMARY_MODELS_PASS\n",
  "Mode: ", analysis_mode, "\n",
  "MIMIC: n=", nrow(mimic_analysis),
  ", events=", sum(mimic_analysis$outcome), "\n",
  "eICU: n=", nrow(eicu_analysis),
  ", events=", sum(eicu_analysis$outcome),
  ", hospitals=", length(unique(eicu_analysis$hospital_id)), "\n",
  "Internal bootstrap: ", internal_repetitions, " per model\n",
  "External hospital bootstrap: ", external_repetitions, "\n",
  "Two-stage slope bootstrap executed: ", run_two_stage, "\n",
  sep = ""
)
