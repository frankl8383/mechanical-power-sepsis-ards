#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2:
# outcome-blind complete-GCS predictor and transformation freeze.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/23_freeze_complete_gcs_frames.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "22_complete_gcs_utils.R"))

stopifnot(
  identical(LOCKED_V2$version, "2.0.0"),
  isTRUE(LOCKED_V2$missing_data_hierarchy$sensitivity_complete_gcs)
)

read_field_gate <- function(path, label) {
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
  filter_runner = file.path(
    script_dir, "22_run_complete_gcs_filters.R"
  ),
  filter_helper = file.path(
    script_dir, "22a_filter_complete_gcs_inputs_v2.py"
  ),
  decision_log = file.path(
    PROJECT_ROOT, "docs", "rebuild_v2", "analysis_decision_log_v2.md"
  ),
  primary_freeze_gate = file.path(
    QC_ROOT, "primary_model_freeze",
    "phase3_primary_model_freeze_complete_v2.csv"
  ),
  primary_freeze_manifest = file.path(
    QC_ROOT, "primary_model_freeze",
    "primary_model_freeze_manifest_v2.csv"
  ),
  landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  mimic_raw_filter_gate = file.path(
    QC_ROOT, "complete_gcs", "raw_filters", "mimic",
    "mimic_complete_gcs_raw_filter_complete_v2.csv"
  ),
  eicu_raw_filter_gate = file.path(
    QC_ROOT, "complete_gcs", "raw_filters", "eicu",
    "eicu_complete_gcs_raw_filter_complete_v2.csv"
  ),
  mimic_candidates = file.path(
    PRIVATE_ROOT, "mimic", "cache_v2", "complete_gcs",
    "mimic_gcs_candidates_v2.csv.gz"
  ),
  eicu_candidates = file.path(
    PRIVATE_ROOT, "eicu", "cache_v2", "complete_gcs",
    "eicu_gcs_candidates_v2.csv.gz"
  ),
  mimic_target = file.path(
    PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds"
  ),
  eicu_target = file.path(
    PRIVATE_ROOT, "eicu", "eicu_no_gcs_core_targets_v2.rds"
  ),
  mimic_joined = file.path(
    PRIVATE_ROOT, "model_ready",
    "mimic_tuple_core_joined_outcome_free_v2.rds"
  ),
  eicu_joined = file.path(
    PRIVATE_ROOT, "model_ready",
    "eicu_tuple_core_joined_outcome_free_v2.rds"
  )
)
missing <- names(paths)[!vapply(paths, file.exists, logical(1L))]
if (length(missing)) {
  stop(
    "Missing complete-GCS freeze input(s): ",
    paste(missing, collapse = ", ")
  )
}

decision_text <- readLines(paths$decision_log, warn = FALSE)
if (sum(grepl("^\\| V2-D021 \\|", decision_text)) != 1L ||
    !any(grepl(
      "LOCKED BEFORE COMPLETE-GCS SOURCE EXTRACTION AND OUTCOME FIT",
      decision_text,
      fixed = TRUE
    ))) {
  stop("Complete-GCS decision V2-D021 is absent or not uniquely locked.")
}

primary_gate <- read_field_gate(
  paths$primary_freeze_gate, "primary predictor freeze gate"
)
for (pair in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c("outcome_fields_read", "FALSE"),
  c("outcome_leakage_guard_pass", "TRUE"),
  c("parameter_derivation_database", "MIMIC-IV only")
)) {
  require_gate_value(
    primary_gate, pair[[1L]], pair[[2L]],
    "primary predictor freeze gate"
  )
}

primary_manifest <- fread(
  paths$primary_freeze_manifest,
  colClasses = "character",
  showProgress = FALSE
)
v2_pm_require_columns(
  primary_manifest, c("role", "path", "sha256"),
  "primary predictor freeze manifest"
)
if (anyDuplicated(primary_manifest$role)) {
  stop("Primary predictor freeze manifest contains duplicate roles.")
}
for (pair in list(
  c("mimic_joined_output", paths$mimic_joined),
  c("eicu_joined_output", paths$eicu_joined)
)) {
  manifest_row <- primary_manifest[
    primary_manifest$role == pair[[1L]], , drop = FALSE
  ]
  if (nrow(manifest_row) != 1L ||
      !identical(
        normalizePath(manifest_row$path[[1L]], mustWork = TRUE),
        normalizePath(pair[[2L]], mustWork = TRUE)
      ) ||
      !identical(
        manifest_row$sha256[[1L]],
        v2_pm_sha256_file(pair[[2L]])
      )) {
    stop("Primary freeze provenance mismatch for ", pair[[1L]], ".")
  }
}

landmark_gate <- read_field_gate(
  paths$landmark_gate, "fixed-landmark gate"
)
for (pair in list(
  c("locked_config_version", LOCKED_V2$version),
  c("mimic_target_sha256", v2_pm_sha256_file(paths$mimic_target)),
  c("eicu_target_sha256", v2_pm_sha256_file(paths$eicu_target))
)) {
  require_gate_value(
    landmark_gate, pair[[1L]], pair[[2L]], "fixed-landmark gate"
  )
}

raw_gates <- list(
  mimic = read_field_gate(
    paths$mimic_raw_filter_gate, "MIMIC complete-GCS raw-filter gate"
  ),
  eicu = read_field_gate(
    paths$eicu_raw_filter_gate, "eICU complete-GCS raw-filter gate"
  )
)
for (database in names(raw_gates)) {
  gate <- raw_gates[[database]]
  for (pair in list(
    c("status", "PASS"),
    c("database", database),
    c("locked_config_version", LOCKED_V2$version),
    c("decision_id", "V2-D021"),
    c("raw_source_official_hash_match", "TRUE"),
    c("reached_eof", "TRUE"),
    c("outcome_artifacts_opened", "FALSE")
  )) {
    require_gate_value(
      gate, pair[[1L]], pair[[2L]],
      paste(database, "complete-GCS raw-filter gate")
    )
  }
}
require_gate_value(
  raw_gates$mimic, "target_n", "10468",
  "MIMIC complete-GCS raw-filter gate"
)
require_gate_value(
  raw_gates$eicu, "target_n", "1459",
  "eICU complete-GCS raw-filter gate"
)
require_gate_value(
  raw_gates$mimic, "target_sha256",
  v2_pm_sha256_file(paths$mimic_target),
  "MIMIC complete-GCS raw-filter gate"
)
require_gate_value(
  raw_gates$eicu, "target_sha256",
  v2_pm_sha256_file(paths$eicu_target),
  "eICU complete-GCS raw-filter gate"
)
for (database in names(raw_gates)) {
  require_gate_value(
    raw_gates[[database]], "script_sha256",
    v2_pm_sha256_file(paths$filter_runner),
    paste(database, "complete-GCS raw-filter gate")
  )
  require_gate_value(
    raw_gates[[database]], "helper_sha256",
    v2_pm_sha256_file(paths$filter_helper),
    paste(database, "complete-GCS raw-filter gate")
  )
}
require_gate_value(
  raw_gates$mimic, "candidate_output_sha256",
  v2_pm_sha256_file(paths$mimic_candidates),
  "MIMIC complete-GCS raw-filter gate"
)
require_gate_value(
  raw_gates$eicu, "candidate_output_sha256",
  v2_pm_sha256_file(paths$eicu_candidates),
  "eICU complete-GCS raw-filter gate"
)

private_out <- file.path(PRIVATE_ROOT, "complete_gcs_sensitivity")
aggregate_out <- file.path(AGGREGATE_ROOT, "complete_gcs_sensitivity")
qc_out <- file.path(QC_ROOT, "complete_gcs_sensitivity")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

output_paths <- list(
  mimic_selected = file.path(
    private_out, "mimic_selected_complete_gcs_v2.rds"
  ),
  eicu_selected = file.path(
    private_out, "eicu_selected_complete_gcs_v2.rds"
  ),
  mimic_predictors = file.path(
    private_out, "mimic_complete_gcs_predictors_v2.rds"
  ),
  eicu_predictors = file.path(
    private_out, "eicu_complete_gcs_predictors_v2.rds"
  ),
  frozen_bundle = file.path(
    private_out, "frozen_complete_gcs_transform_bundle_v2.rds"
  )
)
completion_gate <- file.path(
  qc_out, "complete_gcs_predictor_freeze_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

message("Reading outcome-blind complete-GCS candidate caches ...")
mimic_raw <- fread(
  paths$mimic_candidates,
  colClasses = "character",
  showProgress = FALSE
)
eicu_raw <- fread(
  paths$eicu_candidates,
  colClasses = "character",
  showProgress = FALSE
)
mimic_target <- as.data.frame(readRDS(paths$mimic_target))
eicu_target <- as.data.frame(readRDS(paths$eicu_target))
mimic_joined <- as.data.frame(readRDS(paths$mimic_joined))
eicu_joined <- as.data.frame(readRDS(paths$eicu_joined))

for (object in list(
  mimic_target, eicu_target, mimic_joined, eicu_joined
)) {
  v2_pm_assert_outcome_free(object, "complete-GCS freeze input")
}
if (nrow(mimic_target) != 10468L || nrow(mimic_joined) != 10468L ||
    nrow(eicu_target) != 1459L || nrow(eicu_joined) != 1459L) {
  stop("Full fixed-6h tuple target sizes changed before GCS derivation.")
}
mimic_source_validation <- v2_pm_validate_predictor_frame(
  mimic_joined, "MIMIC-IV", require_complete = FALSE
)
eicu_source_validation <- v2_pm_validate_predictor_frame(
  eicu_joined, "eICU-CRD", require_complete = FALSE
)

message("Deriving strictly selected source-specific GCS values ...")
mimic_derived <- v2_cg_derive_mimic(mimic_raw, mimic_target)
eicu_derived <- v2_cg_derive_eicu(eicu_raw, eicu_target)
if (!nrow(mimic_derived$selected) || !nrow(eicu_derived$selected)) {
  stop("At least one database yielded no selected GCS values.")
}

mimic_complete <- v2_cg_join_complete_frame(
  mimic_joined, mimic_derived$selected, "MIMIC-IV"
)
eicu_complete <- v2_cg_join_complete_frame(
  eicu_joined, eicu_derived$selected, "eICU-CRD"
)
if (data.table::uniqueN(eicu_complete$hospital_id) < 2L) {
  stop("Complete-GCS eICU frame has fewer than two hospitals.")
}

transform_bundle <- v2_cg_derive_transform_bundle(mimic_complete)
attr(transform_bundle, "freeze_metadata") <- list(
  artifact_version = "frozen_complete_gcs_transform_bundle_v2",
  decision_id = "V2-D021",
  derivation_database = "MIMIC-IV only",
  derivation_population =
    "fixed-6h complete-GCS complete common set",
  derivation_n = nrow(mimic_complete),
  baseline_knots = "type-2 quantiles at 0.10,0.50,0.90",
  external_application = "unchanged to eICU-CRD",
  external_outcomes_used = FALSE
)

design_audit <- do.call(rbind, lapply(
  list(`MIMIC-IV` = mimic_complete, `eICU-CRD` = eicu_complete),
  function(frame) {
    do.call(rbind, lapply(LOCKED_V2$model_ids, function(model_id) {
      design <- v2_cg_build_design(frame, model_id, transform_bundle)
      data.frame(
        database = unique(frame$database),
        model_id = model_id,
        n = nrow(design),
        design_columns = ncol(design),
        missing_cells = sum(is.na(design)),
        nonfinite_cells = sum(!is.finite(design)),
        unique_column_names = !anyDuplicated(colnames(design)),
        complete_gcs_spline_present =
          all(c("gcs_rcs1", "gcs_rcs2") %in% colnames(design)),
        pass = !anyNA(design) && all(is.finite(design)) &&
          !anyDuplicated(colnames(design)) &&
          all(c("gcs_rcs1", "gcs_rcs2") %in% colnames(design)),
        stringsAsFactors = FALSE
      )
    }))
  }
))
for (model_id in LOCKED_V2$model_ids) {
  if (!identical(
    colnames(v2_cg_build_design(
      mimic_complete, model_id, transform_bundle
    )),
    colnames(v2_cg_build_design(
      eicu_complete, model_id, transform_bundle
    ))
  )) {
    stop("MIMIC/eICU complete-GCS design mismatch for ", model_id, ".")
  }
}
if (any(!design_audit$pass)) {
  stop("Complete-GCS model design validation failed.")
}

attr(mimic_derived$selected, "freeze_metadata") <- list(
  artifact_version = "mimic_selected_complete_gcs_v2",
  decision_id = "V2-D021",
  source_cache_sha256 = v2_pm_sha256_file(paths$mimic_candidates),
  source_definition =
    "strict same-charttime eye/verbal/motor reconstruction",
  outcome_fields_read = FALSE
)
attr(eicu_derived$selected, "freeze_metadata") <- list(
  artifact_version = "eicu_selected_complete_gcs_v2",
  decision_id = "V2-D021",
  source_cache_sha256 = v2_pm_sha256_file(paths$eicu_candidates),
  source_definition =
    "explicit recorded total prioritized over same-offset reconstruction",
  outcome_fields_read = FALSE
)
attr(mimic_complete, "freeze_metadata") <- list(
  artifact_version = "mimic_complete_gcs_predictors_v2",
  decision_id = "V2-D021",
  source_joined_sha256 = v2_pm_sha256_file(paths$mimic_joined),
  selected_gcs_n = nrow(mimic_derived$selected),
  retained_n = nrow(mimic_complete),
  outcome_fields_read = FALSE
)
attr(eicu_complete, "freeze_metadata") <- list(
  artifact_version = "eicu_complete_gcs_predictors_v2",
  decision_id = "V2-D021",
  source_joined_sha256 = v2_pm_sha256_file(paths$eicu_joined),
  selected_gcs_n = nrow(eicu_derived$selected),
  retained_n = nrow(eicu_complete),
  outcome_fields_read = FALSE
)
frozen_bundle <- list(
  artifact_version = "frozen_complete_gcs_bundle_v2",
  locked_config_version = LOCKED_V2$version,
  decision_id = "V2-D021",
  transform_bundle = transform_bundle,
  derivation_database = "MIMIC-IV only",
  derivation_n = nrow(mimic_complete),
  external_outcomes_used = FALSE,
  manuscript_ci_ready = FALSE
)

output_hashes <- c(
  mimic_selected = v2_pm_atomic_save_rds(
    mimic_derived$selected, output_paths$mimic_selected
  ),
  eicu_selected = v2_pm_atomic_save_rds(
    eicu_derived$selected, output_paths$eicu_selected
  ),
  mimic_predictors = v2_pm_atomic_save_rds(
    mimic_complete, output_paths$mimic_predictors
  ),
  eicu_predictors = v2_pm_atomic_save_rds(
    eicu_complete, output_paths$eicu_predictors
  ),
  frozen_bundle = v2_pm_atomic_save_rds(
    frozen_bundle, output_paths$frozen_bundle
  )
)

mimic_mapping <- mimic_derived$parsed[, .(
  raw_rows = .N,
  target_stays = uniqueN(stay_id),
  storetime_missing_rows = sum(storetime_missing),
  airway_unscorable_rows = sum(unscorable_airway_text),
  text_internal_conflict_rows = sum(text_internal_conflict),
  value_text_conflict_rows = sum(value_text_conflict),
  accepted_component_rows = sum(component_valid)
), by = .(itemid = itemid_numeric, component)]
eicu_mapping <- eicu_derived$parsed[, .(
  raw_rows = .N,
  target_stays = uniqueN(patientunitstayid),
  entry_offset_missing_rows = sum(is.na(entry_time)),
  strict_numeric_rows = sum(!is.na(value_numeric))
), by = .(
  nursingchartcelltypevallabel,
  nursingchartcelltypevalname,
  mapping
)]
v2_pm_atomic_write_csv(
  as.data.frame(mimic_mapping),
  file.path(qc_out, "mimic_complete_gcs_mapping_qc_v2.csv")
)
v2_pm_atomic_write_csv(
  as.data.frame(eicu_mapping),
  file.path(qc_out, "eicu_complete_gcs_mapping_qc_v2.csv")
)
v2_pm_atomic_write_csv(
  rbind(mimic_derived$timing_qc, eicu_derived$timing_qc),
  file.path(qc_out, "complete_gcs_timing_selection_qc_v2.csv")
)

sample_flow <- rbind(
  data.frame(
    database = "MIMIC-IV",
    all_fixed6h_tuple_n = nrow(mimic_joined),
    selected_gcs_n = nrow(mimic_derived$selected),
    no_gcs_core_complete_n = sum(mimic_joined$core_complete),
    complete_gcs_common_n = nrow(mimic_complete),
    complete_gcs_fraction_of_all_tuple =
      nrow(mimic_complete) / nrow(mimic_joined),
    hospital_n = uniqueN(mimic_complete$hospital_id),
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    all_fixed6h_tuple_n = nrow(eicu_joined),
    selected_gcs_n = nrow(eicu_derived$selected),
    no_gcs_core_complete_n = sum(eicu_joined$core_complete),
    complete_gcs_common_n = nrow(eicu_complete),
    complete_gcs_fraction_of_all_tuple =
      nrow(eicu_complete) / nrow(eicu_joined),
    hospital_n = uniqueN(eicu_complete$hospital_id),
    stringsAsFactors = FALSE
  )
)
v2_pm_atomic_write_csv(
  sample_flow,
  file.path(aggregate_out, "complete_gcs_sample_flow_v2.csv")
)

selected_by_database <- list(
  `MIMIC-IV` = as.data.frame(mimic_derived$selected),
  `eICU-CRD` = as.data.frame(eicu_derived$selected)
)
source_rows <- Map(
  function(database, selected) {
    counts <- table(selected$gcs_source, useNA = "no")
    data.frame(
      database = database,
      gcs_source = names(counts),
      selected_n = as.integer(counts),
      stringsAsFactors = FALSE
    )
  },
  names(selected_by_database),
  selected_by_database
)
source_distribution <- do.call(rbind, source_rows)
v2_pm_atomic_write_csv(
  source_distribution,
  file.path(aggregate_out, "complete_gcs_source_distribution_v2.csv")
)

transform_rows <- list()
counter <- 0L
for (group in c("baseline_three_knots", "nonlinear_four_knots")) {
  for (variable in names(transform_bundle[[group]])) {
    values <- transform_bundle[[group]][[variable]]
    for (i in seq_along(values)) {
      counter <- counter + 1L
      transform_rows[[counter]] <- data.frame(
        parameter_group = group,
        variable = variable,
        parameter_index = i,
        value = values[[i]],
        derivation_database = "MIMIC-IV",
        derivation_n = nrow(mimic_complete),
        quantile_type = transform_bundle$quantile_type,
        external_application = "unchanged",
        stringsAsFactors = FALSE
      )
    }
  }
}
transform_parameters <- do.call(rbind, transform_rows)
v2_pm_atomic_write_csv(
  transform_parameters,
  file.path(
    aggregate_out, "complete_gcs_transform_parameters_v2.csv"
  )
)
v2_pm_atomic_write_csv(
  design_audit,
  file.path(aggregate_out, "complete_gcs_model_design_v2.csv")
)

gcs_range_qc <- do.call(rbind, lapply(
  list(`MIMIC-IV` = mimic_complete, `eICU-CRD` = eicu_complete),
  function(frame) {
    quantiles <- as.numeric(quantile(
      frame$gcs, c(0, 0.05, 0.5, 0.95, 1),
      type = 2L, names = FALSE
    ))
    data.frame(
      database = unique(frame$database),
      variable = "gcs",
      total_n = nrow(frame),
      available_n = sum(!is.na(frame$gcs)),
      missing_n = sum(is.na(frame$gcs)),
      invalid_n = sum(frame$gcs < 3 | frame$gcs > 15),
      minimum = quantiles[[1L]],
      p05 = quantiles[[2L]],
      median = quantiles[[3L]],
      p95 = quantiles[[4L]],
      maximum = quantiles[[5L]],
      stringsAsFactors = FALSE
    )
  }
))
v2_pm_atomic_write_csv(
  rbind(
    mimic_source_validation$range_qc,
    eicu_source_validation$range_qc,
    gcs_range_qc
  ),
  file.path(qc_out, "complete_gcs_range_qc_v2.csv")
)

leakage <- rbind(
  v2_pm_predictor_leakage_audit(
    mimic_complete, "MIMIC complete-GCS predictor frame"
  ),
  v2_pm_predictor_leakage_audit(
    eicu_complete, "eICU complete-GCS predictor frame"
  )
)
if (any(!leakage$pass)) {
  stop("Outcome-like fields entered the complete-GCS predictor freeze.")
}
v2_pm_atomic_write_csv(
  leakage,
  file.path(qc_out, "complete_gcs_outcome_leakage_guard_v2.csv")
)

invariants <- data.frame(
  check = c(
    "decision_locked_before_extraction",
    "raw_sources_reached_eof",
    "raw_source_official_hashes_match",
    "full_fixed6h_target_sizes",
    "selected_gcs_range_valid",
    "selected_gcs_available_by_landmark",
    "complete_case_no_imputation",
    "mimic_gcs_is_same_charttime_reconstruction",
    "eicu_explicit_total_priority_implemented",
    "source_harmonized_not_measurement_identical",
    "mimic_only_type2_transform_derivation",
    "strictly_increasing_baseline_knots",
    "external_design_applied_unchanged",
    "at_least_two_eicu_hospitals",
    "outcome_leakage_guard_pass",
    "outcome_artifacts_not_opened",
    "point_estimate_only_contract"
  ),
  pass = c(
    TRUE,
    all(vapply(raw_gates, function(x) {
      identical(x[["reached_eof"]], "TRUE")
    }, logical(1L))),
    all(vapply(raw_gates, function(x) {
      identical(x[["raw_source_official_hash_match"]], "TRUE")
    }, logical(1L))),
    nrow(mimic_joined) == 10468L && nrow(eicu_joined) == 1459L,
    all(mimic_complete$gcs >= 3 & mimic_complete$gcs <= 15) &&
      all(eicu_complete$gcs >= 3 & eicu_complete$gcs <= 15),
    all(mimic_complete$gcs_available_time_value <=
          mimic_complete$landmark_time_value) &&
      all(eicu_complete$gcs_available_time_value <=
            eicu_complete$landmark_time_value),
    all(mimic_complete$core_complete) &&
      all(eicu_complete$core_complete) &&
      all(stats::complete.cases(
        mimic_complete[c(v2_pm_model_columns, "gcs")]
      )) &&
      all(stats::complete.cases(
        eicu_complete[c(v2_pm_model_columns, "gcs")]
      )),
    all(mimic_derived$selected$gcs_source ==
          "same_charttime_eye_verbal_motor_strict_reconstruction"),
    all(
      grepl("^explicit_total:", eicu_derived$selected$gcs_source) |
        eicu_derived$selected$gcs_source ==
          "same_time_eye_verbal_motor_reconstruction"
    ),
    TRUE,
    identical(transform_bundle$quantile_type, 2L) &&
      identical(transform_bundle$derivation_database, "MIMIC-IV"),
    all(vapply(
      transform_bundle$baseline_three_knots,
      function(x) length(x) == 3L && all(diff(x) > 0),
      logical(1L)
    )),
    all(design_audit$pass),
    uniqueN(eicu_complete$hospital_id) >= 2L,
    all(leakage$pass),
    TRUE,
    TRUE
  ),
  stringsAsFactors = FALSE
)
if (any(!invariants$pass)) {
  stop(
    "Complete-GCS freeze invariant failure: ",
    paste(invariants$check[!invariants$pass], collapse = ", ")
  )
}
v2_pm_atomic_write_csv(
  invariants,
  file.path(qc_out, "complete_gcs_freeze_invariants_v2.csv")
)

input_manifest <- data.frame(
  artifact = names(paths),
  path = unname(unlist(paths, use.names = FALSE)),
  sha256 = vapply(paths, v2_pm_sha256_file, character(1L)),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  input_manifest,
  file.path(qc_out, "complete_gcs_freeze_input_manifest_v2.csv")
)
output_manifest <- data.frame(
  artifact = names(output_paths),
  path = unname(unlist(output_paths, use.names = FALSE)),
  sha256 = unname(output_hashes[names(output_paths)]),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  output_manifest,
  file.path(qc_out, "complete_gcs_freeze_output_manifest_v2.csv")
)

gate <- data.frame(
  field = c(
    "status",
    "locked_config_version",
    "decision_id",
    "script_sha256",
    "utils_sha256",
    "decision_log_sha256",
    "mimic_candidate_sha256",
    "eicu_candidate_sha256",
    "mimic_predictor_sha256",
    "eicu_predictor_sha256",
    "frozen_bundle_sha256",
    "mimic_all_tuple_n",
    "eicu_all_tuple_n",
    "mimic_selected_gcs_n",
    "eicu_selected_gcs_n",
    "mimic_complete_gcs_n",
    "eicu_complete_gcs_n",
    "eicu_complete_gcs_hospital_n",
    "gcs_source_harmonization",
    "quantile_type",
    "parameter_derivation_database",
    "external_transform_application",
    "outcome_artifacts_opened",
    "external_outcomes_used",
    "endpoint_model_run",
    "all_invariants_pass",
    "manuscript_ci_ready",
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
    v2_pm_sha256_file(paths$decision_log),
    v2_pm_sha256_file(paths$mimic_candidates),
    v2_pm_sha256_file(paths$eicu_candidates),
    output_hashes[["mimic_predictors"]],
    output_hashes[["eicu_predictors"]],
    output_hashes[["frozen_bundle"]],
    as.character(nrow(mimic_joined)),
    as.character(nrow(eicu_joined)),
    as.character(nrow(mimic_derived$selected)),
    as.character(nrow(eicu_derived$selected)),
    as.character(nrow(mimic_complete)),
    as.character(nrow(eicu_complete)),
    as.character(uniqueN(eicu_complete$hospital_id)),
    "recorded source-specific total; not identical measurement",
    as.character(transform_bundle$quantile_type),
    "MIMIC-IV only",
    "unchanged",
    "FALSE",
    "FALSE",
    "FALSE",
    "TRUE",
    "FALSE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

message("REBUILD_V2_COMPLETE_GCS_PREDICTOR_FREEZE_PASS")
message(
  "  MIMIC complete-GCS n=", nrow(mimic_complete),
  "; eICU n=", nrow(eicu_complete),
  "; eICU hospitals=", uniqueN(eicu_complete$hospital_id)
)
