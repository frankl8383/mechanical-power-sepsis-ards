#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2: outcome-blind primary frame freeze
#
# This script joins the fixed-landmark ventilator representations to the
# no-GCS severity core, freezes one complete common set per database, and
# derives all transformation parameters from MIMIC-IV only. It never opens an
# outcome artifact.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/10_freeze_primary_model_frames.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))

stopifnot(
  identical(LOCKED_V2$version, "2.0.0"),
  identical(
    LOCKED_V2$model_ids,
    v2_model_specification()$model_id
  )
)

path_from_env <- function(name, default) {
  value <- Sys.getenv(name, unset = default)
  normalizePath(value, mustWork = TRUE)
}

mimic_tuple_path <- path_from_env(
  "ARDS_V2_MIMIC_TUPLE_TARGET_PATH",
  file.path(PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds")
)
eicu_tuple_path <- path_from_env(
  "ARDS_V2_EICU_TUPLE_TARGET_PATH",
  file.path(PRIVATE_ROOT, "eicu", "eicu_no_gcs_core_targets_v2.rds")
)
mimic_core_path <- path_from_env(
  "ARDS_V2_MIMIC_NO_GCS_CORE_PATH",
  file.path(
    PRIVATE_ROOT, "mimic",
    "mimic_fixed6h_tuple_no_gcs_core_v2.rds"
  )
)
eicu_core_path <- path_from_env(
  "ARDS_V2_EICU_NO_GCS_CORE_PATH",
  file.path(
    PRIVATE_ROOT, "eicu",
    "eicu_fixed6h_tuple_no_gcs_core_v2.rds"
  )
)
landmark_gate_path <- path_from_env(
  "ARDS_V2_LANDMARK_GATE_PATH",
  file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  )
)
severity_gate_path <- path_from_env(
  "ARDS_V2_NO_GCS_CORE_GATE_PATH",
  file.path(
    QC_ROOT, "no_gcs_core", "phase2b_no_gcs_core_complete_v2.csv"
  )
)

private_out <- file.path(PRIVATE_ROOT, "model_ready")
qc_out <- file.path(QC_ROOT, "primary_model_freeze")
aggregate_out <- file.path(AGGREGATE_ROOT, "primary_model_freeze")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)

mimic_joined_path <- file.path(
  private_out, "mimic_tuple_core_joined_outcome_free_v2.rds"
)
eicu_joined_path <- file.path(
  private_out, "eicu_tuple_core_joined_outcome_free_v2.rds"
)
mimic_common_path <- file.path(
  private_out, "mimic_primary_predictor_common_set_v2.rds"
)
eicu_common_path <- file.path(
  private_out, "eicu_primary_predictor_common_set_v2.rds"
)
bundle_path <- file.path(
  private_out, "frozen_transform_bundle_v2.rds"
)
completion_gate <- file.path(
  qc_out, "phase3_primary_model_freeze_complete_v2.csv"
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

landmark_gate <- read_gate(landmark_gate_path, "landmark gate")
require_gate_value(
  landmark_gate,
  "locked_config_version",
  LOCKED_V2$version,
  "landmark gate"
)
require_gate_value(
  landmark_gate,
  "mimic_target_sha256",
  v2_pm_sha256_file(mimic_tuple_path),
  "landmark gate"
)
require_gate_value(
  landmark_gate,
  "eicu_target_sha256",
  v2_pm_sha256_file(eicu_tuple_path),
  "landmark gate"
)
require_gate_value(
  landmark_gate,
  "all_energy_identities_pass",
  "TRUE",
  "landmark gate"
)

severity_gate <- read_gate(severity_gate_path, "no-GCS severity gate")
if (!LOCKED_V2$version %in% unname(severity_gate)) {
  stop("No-GCS severity gate does not contain the locked config version.")
}
require_gate_value(
  severity_gate,
  "mimic_tuple_output_sha256",
  v2_pm_sha256_file(mimic_core_path),
  "no-GCS severity gate"
)
require_gate_value(
  severity_gate,
  "eicu_tuple_output_sha256",
  v2_pm_sha256_file(eicu_core_path),
  "no-GCS severity gate"
)
for (field in c("all_invariants_pass", "outcome_leakage_guard_pass")) {
  require_gate_value(severity_gate, field, "TRUE", "no-GCS severity gate")
}
if ("status" %in% names(severity_gate)) {
  require_gate_value(severity_gate, "status", "PASS", "no-GCS severity gate")
}

mimic_tuple <- as.data.frame(readRDS(mimic_tuple_path))
eicu_tuple <- as.data.frame(readRDS(eicu_tuple_path))
mimic_core <- as.data.frame(readRDS(mimic_core_path))
eicu_core <- as.data.frame(readRDS(eicu_core_path))

leakage_qc <- rbind(
  v2_pm_predictor_leakage_audit(mimic_tuple, "MIMIC tuple target"),
  v2_pm_predictor_leakage_audit(mimic_core, "MIMIC no-GCS core"),
  v2_pm_predictor_leakage_audit(eicu_tuple, "eICU tuple target"),
  v2_pm_predictor_leakage_audit(eicu_core, "eICU no-GCS core")
)
if (any(!leakage_qc$pass)) {
  stop("An outcome-like field was found before outcome-free frame freeze.")
}

mimic_tuple_key <- v2_pm_key(
  mimic_tuple,
  c("subject_id", "hadm_id", "stay_id"),
  "MIMIC tuple target"
)
mimic_core_key <- v2_pm_key(
  mimic_core,
  c("subject_id", "hadm_id", "stay_id"),
  "MIMIC no-GCS core"
)
eicu_tuple_key <- v2_pm_key(
  eicu_tuple,
  c("patientunitstayid"),
  "eICU tuple target"
)
eicu_core_key <- v2_pm_key(
  eicu_core,
  c("patientunitstayid"),
  "eICU no-GCS core"
)
if (!setequal(mimic_tuple_key, mimic_core_key) ||
    !setequal(eicu_tuple_key, eicu_core_key)) {
  stop("Tuple targets and tuple no-GCS cores must have identical patient sets.")
}

mimic_joined <- v2_pm_build_predictor_frame(
  mimic_tuple, mimic_core, "MIMIC-IV"
)
eicu_joined <- v2_pm_build_predictor_frame(
  eicu_tuple, eicu_core, "eICU-CRD"
)
mimic_validation <- v2_pm_validate_predictor_frame(
  mimic_joined, "MIMIC-IV", require_complete = FALSE
)
eicu_validation <- v2_pm_validate_predictor_frame(
  eicu_joined, "eICU-CRD", require_complete = FALSE
)
mimic_common <- v2_pm_complete_common_set(
  mimic_joined, "MIMIC-IV"
)
eicu_common <- v2_pm_complete_common_set(
  eicu_joined, "eICU-CRD"
)

if (nrow(mimic_common) < 200L || nrow(eicu_common) < 100L) {
  stop("A primary complete common set is unexpectedly small.")
}

transform_bundle <- v2_derive_transform_bundle(mimic_common)
attr(transform_bundle, "freeze_metadata") <- list(
  artifact_version = "frozen_transform_bundle_v2",
  locked_config_version = LOCKED_V2$version,
  derivation_database = "MIMIC-IV only",
  derivation_population =
    "MIMIC fixed-6h no-GCS complete common set",
  derivation_n = nrow(mimic_common),
  quantile_type = 2L,
  mimic_common_set_source_sha256 = NA_character_,
  external_application =
    "Apply unchanged to the eICU complete common set"
)

design_coverage <- do.call(rbind, lapply(
  list(`MIMIC-IV` = mimic_common, `eICU-CRD` = eicu_common),
  function(frame) {
    do.call(rbind, lapply(LOCKED_V2$model_ids, function(model_id) {
      design <- v2_build_design(frame, model_id, transform_bundle)
      data.frame(
        model_id = model_id,
        n = nrow(design),
        design_columns = ncol(design),
        missing_n = sum(is.na(design)),
        nonfinite_n = sum(!is.finite(design)),
        column_names_unique = !anyDuplicated(colnames(design)),
        pass = !anyNA(design) && all(is.finite(design)) &&
          !anyDuplicated(colnames(design)),
        stringsAsFactors = FALSE
      )
    }))
  }
))
design_coverage$database <- rep(
  c("MIMIC-IV", "eICU-CRD"),
  each = length(LOCKED_V2$model_ids)
)
design_coverage <- design_coverage[
  c("database", setdiff(names(design_coverage), "database"))
]
if (any(!design_coverage$pass)) {
  stop("Frozen MIMIC transformations failed design coverage.")
}

attr(mimic_joined, "freeze_metadata") <- list(
  artifact_version = "mimic_tuple_core_joined_outcome_free_v2",
  database = "MIMIC-IV",
  outcome_fields_read = FALSE,
  tuple_source_sha256 = v2_pm_sha256_file(mimic_tuple_path),
  core_source_sha256 = v2_pm_sha256_file(mimic_core_path)
)
attr(eicu_joined, "freeze_metadata") <- list(
  artifact_version = "eicu_tuple_core_joined_outcome_free_v2",
  database = "eICU-CRD",
  outcome_fields_read = FALSE,
  tuple_source_sha256 = v2_pm_sha256_file(eicu_tuple_path),
  core_source_sha256 = v2_pm_sha256_file(eicu_core_path)
)
attr(mimic_common, "freeze_metadata") <- list(
  artifact_version = "mimic_primary_predictor_common_set_v2",
  database = "MIMIC-IV",
  outcome_fields_read = FALSE,
  complete_common_set = TRUE
)
attr(eicu_common, "freeze_metadata") <- list(
  artifact_version = "eicu_primary_predictor_common_set_v2",
  database = "eICU-CRD",
  outcome_fields_read = FALSE,
  complete_common_set = TRUE
)

mimic_joined_hash <- v2_pm_atomic_save_rds(
  mimic_joined, mimic_joined_path
)
eicu_joined_hash <- v2_pm_atomic_save_rds(
  eicu_joined, eicu_joined_path
)
mimic_common_hash <- v2_pm_atomic_save_rds(
  mimic_common, mimic_common_path
)
eicu_common_hash <- v2_pm_atomic_save_rds(
  eicu_common, eicu_common_path
)
attr(transform_bundle, "freeze_metadata")$
  mimic_common_set_source_sha256 <- mimic_common_hash
bundle_hash <- v2_pm_atomic_save_rds(transform_bundle, bundle_path)

completeness_qc <- rbind(
  data.frame(
    database = "MIMIC-IV",
    tuple_target_n = nrow(mimic_tuple),
    no_gcs_core_n = nrow(mimic_core),
    joined_n = nrow(mimic_joined),
    complete_common_set_n = nrow(mimic_common),
    incomplete_core_n = nrow(mimic_joined) - nrow(mimic_common),
    complete_proportion = nrow(mimic_common) / nrow(mimic_joined),
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    tuple_target_n = nrow(eicu_tuple),
    no_gcs_core_n = nrow(eicu_core),
    joined_n = nrow(eicu_joined),
    complete_common_set_n = nrow(eicu_common),
    incomplete_core_n = nrow(eicu_joined) - nrow(eicu_common),
    complete_proportion = nrow(eicu_common) / nrow(eicu_joined),
    stringsAsFactors = FALSE
  )
)

missingness_qc <- do.call(rbind, lapply(
  list(`MIMIC-IV` = mimic_joined, `eICU-CRD` = eicu_joined),
  function(frame) {
    do.call(rbind, lapply(v2_pm_model_columns, function(variable) {
      data.frame(
        variable = variable,
        total_n = nrow(frame),
        available_n = sum(!is.na(frame[[variable]])),
        missing_n = sum(is.na(frame[[variable]])),
        available_proportion = mean(!is.na(frame[[variable]])),
        stringsAsFactors = FALSE
      )
    }))
  }
))
missingness_qc$database <- rep(
  c("MIMIC-IV", "eICU-CRD"),
  each = length(v2_pm_model_columns)
)
missingness_qc <- missingness_qc[
  c("database", setdiff(names(missingness_qc), "database"))
]

parameter_rows <- list()
counter <- 0L
for (group in c("baseline_three_knots", "nonlinear_four_knots")) {
  for (variable in names(transform_bundle[[group]])) {
    values <- transform_bundle[[group]][[variable]]
    for (i in seq_along(values)) {
      counter <- counter + 1L
      parameter_rows[[counter]] <- data.frame(
        parameter_group = group,
        variable = variable,
        parameter_index = i,
        value = values[[i]],
        derivation_database = "MIMIC-IV",
        derivation_n = nrow(mimic_common),
        quantile_type = transform_bundle$quantile_type,
        stringsAsFactors = FALSE
      )
    }
  }
}
parameter_table <- do.call(rbind, parameter_rows)

manifest <- data.frame(
  role = c(
    "script", "analysis_utils", "primary_model_utils",
    "landmark_gate", "no_gcs_core_gate",
    "mimic_tuple_input", "eicu_tuple_input",
    "mimic_core_input", "eicu_core_input",
    "mimic_joined_output", "eicu_joined_output",
    "mimic_common_output", "eicu_common_output",
    "transform_bundle_output"
  ),
  path = c(
    script_path,
    file.path(script_dir, "01_analysis_utils.R"),
    file.path(script_dir, "09_primary_model_utils.R"),
    landmark_gate_path, severity_gate_path,
    mimic_tuple_path, eicu_tuple_path,
    mimic_core_path, eicu_core_path,
    mimic_joined_path, eicu_joined_path,
    mimic_common_path, eicu_common_path, bundle_path
  ),
  sha256 = c(
    v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(file.path(script_dir, "01_analysis_utils.R")),
    v2_pm_sha256_file(file.path(script_dir, "09_primary_model_utils.R")),
    v2_pm_sha256_file(landmark_gate_path),
    v2_pm_sha256_file(severity_gate_path),
    v2_pm_sha256_file(mimic_tuple_path),
    v2_pm_sha256_file(eicu_tuple_path),
    v2_pm_sha256_file(mimic_core_path),
    v2_pm_sha256_file(eicu_core_path),
    mimic_joined_hash, eicu_joined_hash,
    mimic_common_hash, eicu_common_hash, bundle_hash
  ),
  row_level = c(
    FALSE, FALSE, FALSE, FALSE, FALSE,
    TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE
  ),
  stringsAsFactors = FALSE
)

v2_pm_atomic_write_csv(
  rbind(mimic_validation$range_qc, eicu_validation$range_qc),
  file.path(qc_out, "primary_predictor_range_qc_v2.csv")
)
v2_pm_atomic_write_csv(
  rbind(mimic_validation$timing_qc, eicu_validation$timing_qc),
  file.path(qc_out, "primary_predictor_timing_qc_v2.csv")
)
v2_pm_atomic_write_csv(
  leakage_qc,
  file.path(qc_out, "outcome_leakage_guard_v2.csv")
)
v2_pm_atomic_write_csv(
  completeness_qc,
  file.path(qc_out, "primary_common_set_completeness_v2.csv")
)
v2_pm_atomic_write_csv(
  missingness_qc,
  file.path(qc_out, "primary_predictor_missingness_v2.csv")
)
v2_pm_atomic_write_csv(
  design_coverage,
  file.path(qc_out, "frozen_transform_external_coverage_v2.csv")
)
v2_pm_atomic_write_csv(
  parameter_table,
  file.path(aggregate_out, "frozen_transform_parameters_v2.csv")
)
v2_pm_atomic_write_csv(
  v2_model_specification(),
  file.path(aggregate_out, "locked_model_specification_v2.csv")
)
v2_pm_atomic_write_csv(
  manifest,
  file.path(qc_out, "primary_model_freeze_manifest_v2.csv")
)

gate <- data.frame(
  field = c(
    "status", "locked_config_version", "script_sha256",
    "landmark_gate_sha256", "no_gcs_core_gate_sha256",
    "mimic_tuple_input_sha256", "eicu_tuple_input_sha256",
    "mimic_core_input_sha256", "eicu_core_input_sha256",
    "mimic_joined_n", "eicu_joined_n",
    "mimic_common_set_n", "eicu_common_set_n",
    "mimic_common_set_sha256", "eicu_common_set_sha256",
    "transform_bundle_sha256", "parameter_derivation_database",
    "quantile_type", "outcome_fields_read",
    "outcome_leakage_guard_pass", "timing_and_range_qc_pass",
    "external_transform_coverage_pass",
    "same_patient_common_set_frozen"
  ),
  value = c(
    "PASS", LOCKED_V2$version, v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(landmark_gate_path),
    v2_pm_sha256_file(severity_gate_path),
    v2_pm_sha256_file(mimic_tuple_path),
    v2_pm_sha256_file(eicu_tuple_path),
    v2_pm_sha256_file(mimic_core_path),
    v2_pm_sha256_file(eicu_core_path),
    nrow(mimic_joined), nrow(eicu_joined),
    nrow(mimic_common), nrow(eicu_common),
    mimic_common_hash, eicu_common_hash, bundle_hash,
    "MIMIC-IV only", transform_bundle$quantile_type,
    "FALSE", "TRUE", "TRUE", "TRUE", "TRUE"
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

cat(
  "REBUILD_V2_PRIMARY_MODEL_FREEZE_PASS\n",
  "MIMIC common set: ", nrow(mimic_common), "\n",
  "eICU common set: ", nrow(eicu_common), "\n",
  sep = ""
)
