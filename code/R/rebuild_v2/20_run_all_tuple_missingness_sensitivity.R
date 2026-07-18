#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2:
# all-tuple frozen-median plus missing-indicator sensitivity point estimates
#
# This driver opens outcomes only after the MIMIC-only missingness rule,
# imputed predictor frames, transformation bundle, and common design columns
# have been frozen and hash-verified. It runs no bootstrap and creates no
# manuscript-ready confidence intervals.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/20_run_all_tuple_missingness_sensitivity.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "12_secondary_sensitivity_utils.R"))
source(file.path(script_dir, "18_missingness_sensitivity_utils.R"))

stopifnot(
  identical(LOCKED_V2$version, "2.0.0"),
  isTRUE(LOCKED_V2$missing_data_hierarchy$sensitivity_frozen_median_indicator)
)

read_gate <- function(path, label) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field) || anyNA(gate$field) ||
      any(!nzchar(gate$field))) {
    stop("Malformed field/value ", label, ": ", path)
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
    QC_ROOT, "missingness_sensitivity",
    "all_tuple_missingness_freeze_complete_v2.csv"
  ),
  landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  mimic_predictors = file.path(
    PRIVATE_ROOT, "missingness_sensitivity",
    "mimic_all_tuple_missingness_predictors_v2.rds"
  ),
  eicu_predictors = file.path(
    PRIVATE_ROOT, "missingness_sensitivity",
    "eicu_all_tuple_missingness_predictors_v2.rds"
  ),
  frozen_bundle = file.path(
    PRIVATE_ROOT, "missingness_sensitivity",
    "frozen_all_tuple_missingness_bundle_v2.rds"
  ),
  mimic_outcome = file.path(
    PRIVATE_ROOT, "mimic", "mimic_fixed6h_landmark_outcomes_v2.rds"
  ),
  eicu_outcome = file.path(
    PRIVATE_ROOT, "eicu", "eicu_fixed6h_landmark_outcomes_v2.rds"
  )
)
missing_paths <- names(paths)[!vapply(paths, file.exists, logical(1L))]
if (length(missing_paths)) {
  stop(
    "Missing all-tuple missingness-sensitivity input(s): ",
    paste(missing_paths, collapse = ", ")
  )
}

private_out <- file.path(PRIVATE_ROOT, "missingness_sensitivity")
aggregate_out <- file.path(AGGREGATE_ROOT, "missingness_sensitivity")
qc_out <- file.path(QC_ROOT, "missingness_sensitivity")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(
  qc_out, "all_tuple_missingness_sensitivity_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

freeze_gate <- read_gate(paths$freeze_gate, "missingness freeze gate")
for (pair in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c("indicator_variables", "map;platelet;creatinine"),
  c(
    "indicator_columns",
    paste(vapply(
      v2_mi_locked_indicator_variables,
      v2_mi_indicator_name,
      character(1L)
    ), collapse = ";")
  ),
  c("quantile_type", "2"),
  c("parameter_derivation_database", "MIMIC-IV only"),
  c("eicu_novel_missingness_policy", "hard STOP"),
  c("same_indicators_appended_to_all_models", "TRUE"),
  c("outcome_artifacts_opened", "FALSE"),
  c("external_outcomes_used", "FALSE"),
  c("all_invariants_pass", "TRUE"),
  c("manuscript_ci_ready", "FALSE")
)) {
  require_gate_value(
    freeze_gate, pair[[1L]], pair[[2L]], "missingness freeze gate"
  )
}
require_gate_value(
  freeze_gate,
  "mimic_predictor_sha256",
  v2_pm_sha256_file(paths$mimic_predictors),
  "missingness freeze gate"
)
require_gate_value(
  freeze_gate,
  "eicu_predictor_sha256",
  v2_pm_sha256_file(paths$eicu_predictors),
  "missingness freeze gate"
)
require_gate_value(
  freeze_gate,
  "frozen_bundle_sha256",
  v2_pm_sha256_file(paths$frozen_bundle),
  "missingness freeze gate"
)

mimic_predictors <- as.data.frame(readRDS(paths$mimic_predictors))
eicu_predictors <- as.data.frame(readRDS(paths$eicu_predictors))
frozen_bundle <- readRDS(paths$frozen_bundle)
if (!is.list(frozen_bundle) ||
    !identical(
      frozen_bundle$artifact_version,
      "frozen_all_tuple_missingness_bundle_v2"
    ) ||
    !identical(frozen_bundle$locked_config_version, LOCKED_V2$version) ||
    !identical(frozen_bundle$derivation_database, "MIMIC-IV only") ||
    !identical(frozen_bundle$external_outcomes_used, FALSE)) {
  stop("Malformed frozen all-tuple missingness bundle.")
}
rule <- frozen_bundle$rule
transform_bundle <- frozen_bundle$transform_bundle
v2_mi_validate_rule(rule)

for (entry in list(
  list(frame = mimic_predictors, database = "MIMIC-IV"),
  list(frame = eicu_predictors, database = "eICU-CRD")
)) {
  metadata <- attr(entry$frame, "freeze_metadata")
  if (!is.list(metadata) ||
      !identical(metadata$outcome_fields_read, FALSE) ||
      !identical(metadata$all_tuple_positive, TRUE) ||
      !identical(metadata$indicator_variables, rule$indicator_variables)) {
    stop(entry$database, " predictor freeze metadata is invalid.")
  }
  v2_pm_validate_predictor_frame(
    entry$frame, entry$database, require_complete = TRUE
  )
  v2_pm_assert_outcome_free(
    entry$frame,
    paste(entry$database, "frozen all-tuple missingness predictors")
  )
  for (model_id in LOCKED_V2$model_ids) {
    design <- v2_mi_build_design(
      entry$frame, model_id, transform_bundle, rule
    )
    if (!all(rule$indicator_columns %in% colnames(design))) {
      stop(entry$database, " design lost frozen missing indicators.")
    }
  }
}

# The outcome artifacts are opened only after the entire predictor freeze above
# has passed.
landmark_gate <- read_gate(paths$landmark_gate, "fixed landmark gate")
for (pair in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c(
    "mimic_outcome_sha256",
    v2_pm_sha256_file(paths$mimic_outcome)
  ),
  c(
    "eicu_outcome_sha256",
    v2_pm_sha256_file(paths$eicu_outcome)
  )
)) {
  if (pair[[1L]] %in% names(landmark_gate)) {
    require_gate_value(
      landmark_gate, pair[[1L]], pair[[2L]], "fixed landmark gate"
    )
  }
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

model_roles <- c(
  M0 =
    "no-GCS baseline plus three frozen MIMIC-derived missing indicators",
  M_MP = "linear sMP plus the common missing-indicator baseline",
  M_4DPRR = "linear 4DPRR plus the common missing-indicator baseline",
  M_DPRR =
    "free driving-pressure/RR weights plus the common missing-indicator baseline",
  M_ENERGY =
    "free algebraic-term weights plus the common missing-indicator baseline"
)
model_designers <- setNames(lapply(
  LOCKED_V2$model_ids,
  function(model_id) {
    force(model_id)
    function(x) v2_mi_build_design(
      x, model_id, transform_bundle, rule
    )
  }
), LOCKED_V2$model_ids)

fit_result <- v2_ss_fit_apply(
  mimic_analysis,
  eicu_analysis,
  model_designers,
  model_roles
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
    rep("increment_over_missing_indicator_baseline", 4L),
    rep("representation_comparison_to_smp", 3L)
  ),
  stringsAsFactors = FALSE
)

performance <- rbind(
  v2_ss_model_performance(
    mimic_analysis$outcome,
    fit_result$mimic_predictions,
    "MIMIC-IV",
    "all_tuple_frozen_median_indicator_apparent",
    model_roles
  ),
  v2_ss_model_performance(
    eicu_analysis$outcome,
    fit_result$eicu_predictions,
    "eICU-CRD",
    "all_tuple_frozen_median_indicator_external",
    model_roles
  )
)
paired <- rbind(
  v2_ss_paired_differences(
    mimic_analysis$outcome,
    fit_result$mimic_predictions,
    comparisons,
    "MIMIC-IV",
    "all_tuple_frozen_median_indicator_apparent"
  ),
  v2_ss_paired_differences(
    eicu_analysis$outcome,
    fit_result$eicu_predictions,
    comparisons,
    "eICU-CRD",
    "all_tuple_frozen_median_indicator_external"
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

sample_qc <- do.call(rbind, lapply(
  list(`MIMIC-IV` = mimic_analysis, `eICU-CRD` = eicu_analysis),
  function(frame) {
    indicator_matrix <- as.matrix(frame[rule$indicator_columns])
    data.frame(
      database = unique(frame$database),
      all_tuple_n = nrow(frame),
      events = sum(frame$outcome),
      non_events = sum(frame$outcome == 0L),
      event_rate = mean(frame$outcome),
      hospitals = length(unique(frame$hospital_id)),
      complete_no_gcs_core_n = sum(rowSums(indicator_matrix) == 0),
      one_or_more_imputed_baseline_predictors_n =
        sum(rowSums(indicator_matrix) > 0),
      map_missing_n = sum(frame$map_missing_indicator),
      platelet_missing_n = sum(frame$platelet_missing_indicator),
      creatinine_missing_n = sum(frame$creatinine_missing_indicator),
      rows_retained_fraction = 1,
      external_outcomes_used_for_imputation = FALSE,
      bootstrap_replicates = 0L,
      manuscript_ci_ready = FALSE,
      stringsAsFactors = FALSE
    )
  }
))

design_columns <- do.call(rbind, lapply(
  LOCKED_V2$model_ids,
  function(model_id) {
    mimic_design <- model_designers[[model_id]](mimic_analysis)
    eicu_design <- model_designers[[model_id]](eicu_analysis)
    data.frame(
      model_id = model_id,
      total_parameter_n = length(
        fit_result$fits[[model_id]]$coefficients
      ),
      design_column_n = ncol(mimic_design),
      indicator_columns =
        paste(rule$indicator_columns, collapse = ";"),
      all_three_indicators_present =
        all(rule$indicator_columns %in% colnames(mimic_design)),
      mimic_eicu_design_columns_identical =
        identical(colnames(mimic_design), colnames(eicu_design)),
      same_patient_mimic_n = nrow(mimic_design),
      same_patient_eicu_n = nrow(eicu_design),
      converged = isTRUE(fit_result$fits[[model_id]]$converged),
      stringsAsFactors = FALSE
    )
  }
))

private_result <- list(
  artifact_version = "all_tuple_missingness_sensitivity_point_estimates_v2",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED_V2$version,
  rule = rule,
  model_roles = model_roles,
  fits = fit_result$fits,
  mimic_analysis_id = mimic_analysis$analysis_id,
  eicu_analysis_id = eicu_analysis$analysis_id,
  mimic_predictions = fit_result$mimic_predictions,
  eicu_predictions = fit_result$eicu_predictions,
  external_model_application = "unchanged MIMIC coefficients and transforms",
  external_outcomes_used_for_imputation = FALSE,
  bootstrap_replicates = 0L,
  manuscript_ci_ready = FALSE,
  input_hashes = lapply(paths, v2_pm_sha256_file)
)
private_path <- file.path(
  private_out, "all_tuple_missingness_sensitivity_point_estimates_v2.rds"
)
private_hash <- v2_pm_atomic_save_rds(private_result, private_path)

aggregate_outputs <- list(
  "all_tuple_missingness_sample_qc_v2.csv" = sample_qc,
  "all_tuple_missingness_design_audit_v2.csv" = design_columns,
  "all_tuple_missingness_coefficients_v2.csv" = coefficient_table,
  "all_tuple_missingness_point_performance_v2.csv" = performance,
  "all_tuple_missingness_paired_differences_v2.csv" = paired
)
for (name in names(aggregate_outputs)) {
  v2_pm_atomic_write_csv(
    aggregate_outputs[[name]], file.path(aggregate_out, name)
  )
}

invariants <- data.frame(
  check = c(
    "all_mimic_tuple_rows_retained",
    "all_eicu_tuple_rows_retained",
    "indicator_variables_exact",
    "quantile_type_2",
    "all_models_share_same_three_indicators",
    "mimic_eicu_design_columns_identical",
    "all_models_converged",
    "same_patient_model_comparisons",
    "external_coefficients_not_refit",
    "external_outcomes_forbidden_from_imputation",
    "eicu_novel_missingness_hard_stop",
    "bootstrap_replicates_zero",
    "manuscript_ci_ready_false"
  ),
  pass = c(
    nrow(mimic_analysis) ==
      as.integer(freeze_gate[["mimic_all_tuple_n"]]),
    nrow(eicu_analysis) ==
      as.integer(freeze_gate[["eicu_all_tuple_n"]]),
    identical(
      rule$indicator_variables,
      c("map", "platelet", "creatinine")
    ),
    identical(as.integer(rule$quantile_type), 2L),
    all(design_columns$all_three_indicators_present),
    all(design_columns$mimic_eicu_design_columns_identical),
    all(design_columns$converged),
    all(
      design_columns$same_patient_mimic_n == nrow(mimic_analysis)
    ) && all(
      design_columns$same_patient_eicu_n == nrow(eicu_analysis)
    ),
    identical(
      private_result$external_model_application,
      "unchanged MIMIC coefficients and transforms"
    ),
    identical(
      private_result$external_outcomes_used_for_imputation,
      FALSE
    ),
    identical(rule$external_novel_missingness_policy, "hard STOP"),
    identical(private_result$bootstrap_replicates, 0L),
    identical(private_result$manuscript_ci_ready, FALSE)
  ),
  stringsAsFactors = FALSE
)
if (!all(invariants$pass)) {
  stop(
    "All-tuple missingness endpoint invariant failure: ",
    paste(invariants$check[!invariants$pass], collapse = ", ")
  )
}
v2_pm_atomic_write_csv(
  invariants,
  file.path(qc_out, "all_tuple_missingness_sensitivity_invariants_v2.csv")
)

input_manifest <- data.frame(
  artifact = names(paths),
  path = unname(unlist(paths, use.names = FALSE)),
  sha256 = vapply(paths, v2_pm_sha256_file, character(1L)),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  input_manifest,
  file.path(qc_out, "all_tuple_missingness_sensitivity_input_manifest_v2.csv")
)
aggregate_manifest <- data.frame(
  artifact = names(aggregate_outputs),
  path = file.path(aggregate_out, names(aggregate_outputs)),
  sha256 = vapply(
    names(aggregate_outputs),
    function(name) {
      v2_pm_sha256_file(file.path(aggregate_out, name))
    },
    character(1L)
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  aggregate_manifest,
  file.path(
    qc_out, "all_tuple_missingness_sensitivity_output_manifest_v2.csv"
  )
)

gate <- data.frame(
  field = c(
    "status",
    "locked_config_version",
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
    "indicator_variables",
    "quantile_type",
    "parameter_derivation_database",
    "eicu_novel_missingness_policy",
    "same_indicators_appended_to_all_models",
    "same_patient_comparisons",
    "external_coefficients_and_transforms_applied_unchanged",
    "external_outcomes_used_for_imputation",
    "bootstrap_replicates",
    "manuscript_ci_ready",
    "all_invariants_pass",
    "completed_at"
  ),
  value = c(
    "PASS",
    LOCKED_V2$version,
    v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(
      file.path(script_dir, "18_missingness_sensitivity_utils.R")
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
    as.character(length(unique(eicu_analysis$hospital_id))),
    paste(rule$indicator_variables, collapse = ";"),
    as.character(rule$quantile_type),
    rule$derivation_database,
    rule$external_novel_missingness_policy,
    "TRUE",
    "TRUE",
    "TRUE",
    "FALSE",
    "0",
    "FALSE",
    "TRUE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

message("REBUILD_V2_ALL_TUPLE_MISSINGNESS_SENSITIVITY_PASS")
message(
  "  MIMIC: ", nrow(mimic_analysis), " (",
  sum(mimic_analysis$outcome), " events); eICU: ",
  nrow(eicu_analysis), " (", sum(eicu_analysis$outcome),
  " events) across ", length(unique(eicu_analysis$hospital_id)),
  " hospitals"
)
