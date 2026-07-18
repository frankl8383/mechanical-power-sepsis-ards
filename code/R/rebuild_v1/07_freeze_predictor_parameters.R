#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: outcome-blind predictor-frame and
# transformation-parameter freeze.
#
# This script may run only after both database-specific Phase 2b severity gates
# pass. It reads no outcome, death, discharge, follow-up, or performance field.
# Numeric transformations are derived only from the MIMIC primary
# predictor-complete population and are then applied unchanged to eICU for
# coverage and transformation smoke tests.

suppressPackageStartupMessages(library(data.table))

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/07_freeze_predictor_parameters.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))

stopifnot(identical(LOCKED$version, "1.0.1"))

# ---------------------------------------------------------------------------
# Fixed paths. Phase 09 depends on the gate and artifact paths below.
# ---------------------------------------------------------------------------

mimic_severity_gate_path <- file.path(
  QC_ROOT, "mimic_severity", "phase2b_mimic_severity_complete_v1.csv"
)
eicu_severity_gate_path <- file.path(
  QC_ROOT, "eicu_severity", "phase2b_complete_v1.csv"
)
mimic_phase1_gate_path <- file.path(QC_ROOT, "mimic", "phase1_complete_v1.csv")
mimic_phase2_gate_path <- file.path(
  QC_ROOT, "mimic_exposure", "phase2_mimic_exposure_complete_v1.csv"
)
eicu_phase1_gate_path <- file.path(
  QC_ROOT, "eicu", "phase1_eicu_complete_v1.csv"
)
eicu_phase2_gate_path <- file.path(
  QC_ROOT, "eicu_exposure", "phase2_eicu_exposure_complete_v1.csv"
)

mimic_severity_script <- file.path(script_dir, "05_build_mimic_severity_core.R")
eicu_severity_script <- file.path(script_dir, "06_build_eicu_severity_core.R")
model_utils_path <- file.path(script_dir, "08_model_utils.R")
mimic_phase1_script <- file.path(script_dir, "01_build_mimic_index_cohort.R")
mimic_phase2_script <- file.path(script_dir, "03_build_mimic_paired_exposure.R")
eicu_phase1_script <- file.path(script_dir, "02_build_eicu_index_cohort.R")
eicu_phase2_script <- file.path(script_dir, "04_build_eicu_paired_exposure.R")

mimic_index_path <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_index_cohort_v1.rds"
)
mimic_exposure_path <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_paired_exposure_primary_60min_v1.rds"
)
eicu_exposure_path <- file.path(
  PRIVATE_ROOT, "eicu", "eicu_paired_exposure_primary_60min_v1.rds"
)
mimic_input_path <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_paired_exposure_with_severity_core_v1.rds"
)
eicu_input_path <- file.path(
  PRIVATE_ROOT, "eicu", "eicu_paired_exposure_with_severity_core_v1.rds"
)

private_out <- file.path(PRIVATE_ROOT, "model_ready")
qc_out <- file.path(QC_ROOT, "parameter_freeze")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

parameter_path <- file.path(private_out, "frozen_predictor_parameters_v1.rds")
mimic_frame_path <- file.path(private_out, "mimic_canonical_model_frame_v1.rds")
eicu_frame_path <- file.path(private_out, "eicu_canonical_model_frame_v1.rds")
completion_gate <- file.path(qc_out, "phase2e_parameter_freeze_complete_v1.csv")
completion_gate_tmp <- paste0(completion_gate, ".tmp")

# An interrupted or invalid rerun must never leave a stale PASS gate, even if
# an upstream input or the required decision-log amendment is now missing.
unlink(c(completion_gate, completion_gate_tmp), force = TRUE)

relative_parameter_path <- file.path(
  "analysis_rebuild_v1", "private", "model_ready",
  "frozen_predictor_parameters_v1.rds"
)
relative_mimic_frame_path <- file.path(
  "analysis_rebuild_v1", "private", "model_ready",
  "mimic_canonical_model_frame_v1.rds"
)
relative_eicu_frame_path <- file.path(
  "analysis_rebuild_v1", "private", "model_ready",
  "eicu_canonical_model_frame_v1.rds"
)

decision_log_path <- file.path(
  PROJECT_ROOT, "docs", "rebuild_v1", "analysis_decision_log.md"
)

required_files <- c(
  mimic_severity_gate_path, eicu_severity_gate_path,
  mimic_phase1_gate_path, mimic_phase2_gate_path,
  eicu_phase1_gate_path, eicu_phase2_gate_path,
  mimic_severity_script, eicu_severity_script, model_utils_path,
  mimic_phase1_script, mimic_phase2_script, eicu_phase1_script,
  eicu_phase2_script, mimic_index_path, mimic_exposure_path,
  eicu_exposure_path, mimic_input_path, eicu_input_path,
  decision_log_path
)
if (any(!file.exists(required_files))) {
  stop(
    "Missing required input(s): ",
    paste(required_files[!file.exists(required_files)], collapse = ", ")
  )
}

decision_text <- paste(readLines(decision_log_path, warn = FALSE), collapse = "\n")
if (!grepl("\\| D053 \\|", decision_text)) {
  stop("D053 height-source amendment must be logged before parameter freeze.")
}

# ---------------------------------------------------------------------------
# Hash, gate, and atomic-publication helpers.
# ---------------------------------------------------------------------------

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

read_wide_gate <- function(path) {
  x <- fread(path, colClasses = "character", showProgress = FALSE)
  if (nrow(x) != 1L || anyDuplicated(names(x))) {
    stop("Malformed one-row completion gate: ", path)
  }
  setNames(as.character(unlist(x[1L], use.names = FALSE)), names(x))
}

gate_value <- function(gate, field, expected = NULL) {
  value <- unname(gate[field])
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("Completion gate missing field: ", field)
  }
  if (!is.null(expected) && !identical(value, as.character(expected))) {
    stop(
      "Completion-gate mismatch for ", field, ": ", value,
      " != ", as.character(expected)
    )
  }
  invisible(value)
}

atomic_fwrite <- function(x, path) {
  tmp <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path), fileext = ".tmp"
  )
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  fwrite(x, tmp)
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(tmp, path)) stop("Could not publish ", path)
  invisible(path)
}

atomic_write_lines <- function(x, path) {
  tmp <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path), fileext = ".tmp"
  )
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(x, tmp, useBytes = TRUE)
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(tmp, path)) stop("Could not publish ", path)
  invisible(path)
}

immutable_save_rds <- function(object, path) {
  tmp <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path), fileext = ".tmp"
  )
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  saveRDS(object, tmp, version = 3, compress = "xz")
  new_hash <- sha256_file(tmp)
  if (file.exists(path)) {
    old_hash <- sha256_file(path)
    if (!identical(old_hash, new_hash)) {
      stop(
        "Immutable artifact differs from the existing version: ", path,
        ". A new version and decision-log amendment are required."
      )
    }
    return(old_hash)
  }
  if (!file.rename(tmp, path)) stop("Could not publish immutable RDS: ", path)
  sha256_file(path)
}

# ---------------------------------------------------------------------------
# Verify both immutable severity chains before opening row-level inputs.
# ---------------------------------------------------------------------------

mimic_gate <- read_wide_gate(mimic_severity_gate_path)
eicu_gate <- read_wide_gate(eicu_severity_gate_path)

gate_value(mimic_gate, "status", "PASS")
gate_value(mimic_gate, "config_version", LOCKED$version)
gate_value(mimic_gate, "all_invariants_pass", "TRUE")
gate_value(mimic_gate, "outcome_leakage_guard_pass", "TRUE")
gate_value(mimic_gate, "cache_all_reached_eof", "TRUE")
gate_value(mimic_gate, "cache_all_official_sha256_match", "TRUE")
gate_value(
  mimic_gate, "script_sha256", sha256_file(mimic_severity_script)
)
gate_value(
  mimic_gate, "phase1_gate_sha256", sha256_file(mimic_phase1_gate_path)
)
gate_value(
  mimic_gate, "phase2_gate_sha256", sha256_file(mimic_phase2_gate_path)
)
gate_value(
  mimic_gate, "input_index_rds_sha256", sha256_file(mimic_index_path)
)
gate_value(
  mimic_gate, "input_exposure_rds_sha256", sha256_file(mimic_exposure_path)
)
gate_value(
  mimic_gate, "prediction_hsc_rds_sha256", sha256_file(mimic_input_path)
)

gate_value(eicu_gate, "status", "PASS")
gate_value(eicu_gate, "config_version", LOCKED$version)
gate_value(
  eicu_gate, "script_sha256", sha256_file(eicu_severity_script)
)
gate_value(
  eicu_gate, "phase1_gate_sha256", sha256_file(eicu_phase1_gate_path)
)
gate_value(
  eicu_gate, "phase2_gate_sha256", sha256_file(eicu_phase2_gate_path)
)
gate_value(
  eicu_gate, "input_exposure_rds_sha256", sha256_file(eicu_exposure_path)
)
gate_value(
  eicu_gate, "prediction_hsc_rds_sha256", sha256_file(eicu_input_path)
)

# Verify that the Phase 1/2 scripts currently on disk still match their gates.
mimic_phase1_gate <- fread(mimic_phase1_gate_path, colClasses = "character")
mimic_phase2_gate <- fread(mimic_phase2_gate_path, colClasses = "character")
eicu_phase1_gate <- fread(eicu_phase1_gate_path, colClasses = "character")
eicu_phase2_gate <- fread(eicu_phase2_gate_path, colClasses = "character")
for (z in list(
  mimic_phase1_gate, mimic_phase2_gate, eicu_phase1_gate, eicu_phase2_gate
)) {
  if (!identical(names(z), c("field", "value")) || anyDuplicated(z$field)) {
    stop("Malformed upstream field/value gate.")
  }
}
field_value <- function(x, field_name) {
  value <- x[x$field == field_name, value]
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("Upstream gate missing field: ", field_name)
  }
  value
}
for (z in list(
  mimic_phase1_gate, mimic_phase2_gate, eicu_phase1_gate, eicu_phase2_gate
)) {
  if (!identical(field_value(z, "locked_config_version"), LOCKED$version) ||
      !identical(field_value(z, "all_invariants_pass"), "TRUE") ||
      !identical(field_value(z, "outcome_leakage_guard_pass"), "TRUE")) {
    stop("An upstream Phase 1/2 gate is not locked, invariant-clean, and outcome-blind.")
  }
  if ("all_required_qc_present" %chin% z$field &&
      !identical(field_value(z, "all_required_qc_present"), "TRUE")) {
    stop("An upstream Phase 1/2 gate reports incomplete QC artifacts.")
  }
}
if (!identical(
  field_value(mimic_phase1_gate, "script_sha256"),
  sha256_file(mimic_phase1_script)
) || !identical(
  field_value(mimic_phase2_gate, "script_sha256"),
  sha256_file(mimic_phase2_script)
) || !identical(
  field_value(eicu_phase1_gate, "script_sha256"),
  sha256_file(eicu_phase1_script)
) || !identical(
  field_value(eicu_phase2_gate, "script_sha256"),
  sha256_file(eicu_phase2_script)
)) {
  stop("An upstream Phase 1/2 script differs from its completion gate.")
}

source(model_utils_path)
required_model_functions <- c(
  "quantile_knots", "three_knot_rcs_basis", "four_knot_rcs_basis"
)
if (!all(vapply(
  required_model_functions, exists, logical(1L), mode = "function"
))) {
  stop("Required locked model-utility functions are unavailable.")
}

input_gate_validation <- data.table(
  test = c(
    "mimic_severity_gate_PASS",
    "eicu_severity_gate_PASS",
    "mimic_severity_script_hash_matches",
    "eicu_severity_script_hash_matches",
    "mimic_phase1_phase2_gate_hashes_match",
    "eicu_phase1_phase2_gate_hashes_match",
    "mimic_prediction_HSC_hash_matches",
    "eicu_prediction_HSC_hash_matches",
    "locked_model_utils_loaded",
    "D053_height_amendment_logged"
  ),
  pass = TRUE
)

# ---------------------------------------------------------------------------
# Canonical, outcome-free model frames.
# ---------------------------------------------------------------------------

mimic_source <- as.data.table(readRDS(mimic_input_path))
eicu_source <- as.data.table(readRDS(eicu_input_path))

required_mimic <- c(
  "stay_id", "age_at_admission", "gender", "pf_ratio", "gcs_worst",
  "map_min", "vasopressor_any", "platelet_min", "creatinine_max",
  "delta_p", "rr_value", "smp", "vt_per_pbw_mL_per_kg", "peep_value",
  "resistive_pressure", "smp_per_pbw_J_per_min_per_kg"
)
required_eicu <- c(
  "patientunitstayid", "age_num_harmonized", "gender", "pf_ratio",
  "gcs_worst", "map_min", "vasopressor_any", "platelet_min",
  "creatinine_max", "delta_p", "rr_value", "smp",
  "vt_per_pbw_mL_per_kg", "peep_value", "resistive_pressure",
  "smp_per_pbw_J_per_min_per_kg"
)
missing_mimic <- setdiff(required_mimic, names(mimic_source))
missing_eicu <- setdiff(required_eicu, names(eicu_source))
if (length(missing_mimic) || length(missing_eicu)) {
  stop(
    "Missing canonical predictor source field(s). MIMIC: ",
    paste(missing_mimic, collapse = ", "), "; eICU: ",
    paste(missing_eicu, collapse = ", ")
  )
}

if (nrow(mimic_source) != as.integer(gate_value(
  mimic_gate, "prediction_hsc_n"
)) || nrow(mimic_source) != as.integer(field_value(
  mimic_phase2_gate, "primary_60min_n"
))) {
  stop("MIMIC prediction-HSC row count disagrees with an upstream gate.")
}
if (nrow(eicu_source) != as.integer(field_value(
  eicu_phase2_gate, "primary_60min_n"
))) {
  stop("eICU prediction-HSC row count disagrees with the Phase 2 gate.")
}
if (anyDuplicated(mimic_source$stay_id) ||
    anyDuplicated(eicu_source$patientunitstayid) ||
    anyNA(mimic_source$stay_id) || anyNA(eicu_source$patientunitstayid)) {
  stop("Prediction-HSC analysis identifiers must be nonmissing and unique.")
}
if ("tuple_observed" %in% names(mimic_source) &&
    !all(mimic_source$tuple_observed == TRUE)) {
  stop("MIMIC prediction-HSC artifact includes a non-observed tuple.")
}
if ("tuple_observed" %in% names(eicu_source) &&
    !all(eicu_source$tuple_observed == TRUE)) {
  stop("eICU prediction-HSC artifact includes a non-observed tuple.")
}

sex_mimic <- fifelse(
  mimic_source$gender == "M", 0L,
  fifelse(mimic_source$gender == "F", 1L, NA_integer_)
)
sex_eicu <- fifelse(
  eicu_source$gender == "Male", 0L,
  fifelse(eicu_source$gender == "Female", 1L, NA_integer_)
)

mimic_frame <- data.table(
  analysis_id = as.integer(mimic_source$stay_id),
  age = as.numeric(mimic_source$age_at_admission),
  sex_female = as.integer(sex_mimic),
  pf_ratio = as.numeric(mimic_source$pf_ratio),
  gcs = as.numeric(mimic_source$gcs_worst),
  map = as.numeric(mimic_source$map_min),
  vasopressor = as.integer(mimic_source$vasopressor_any),
  platelet = as.numeric(mimic_source$platelet_min),
  creatinine = as.numeric(mimic_source$creatinine_max),
  delta_p = as.numeric(mimic_source$delta_p),
  rr = as.numeric(mimic_source$rr_value),
  smp = as.numeric(mimic_source$smp),
  vt_per_pbw = as.numeric(mimic_source$vt_per_pbw_mL_per_kg),
  peep = as.numeric(mimic_source$peep_value),
  resistive_pressure = as.numeric(mimic_source$resistive_pressure),
  smp_per_pbw = as.numeric(mimic_source$smp_per_pbw_J_per_min_per_kg)
)
eicu_frame <- data.table(
  analysis_id = as.integer(eicu_source$patientunitstayid),
  age = as.numeric(eicu_source$age_num_harmonized),
  sex_female = as.integer(sex_eicu),
  pf_ratio = as.numeric(eicu_source$pf_ratio),
  gcs = as.numeric(eicu_source$gcs_worst),
  map = as.numeric(eicu_source$map_min),
  vasopressor = as.integer(eicu_source$vasopressor_any),
  platelet = as.numeric(eicu_source$platelet_min),
  creatinine = as.numeric(eicu_source$creatinine_max),
  delta_p = as.numeric(eicu_source$delta_p),
  rr = as.numeric(eicu_source$rr_value),
  smp = as.numeric(eicu_source$smp),
  vt_per_pbw = as.numeric(eicu_source$vt_per_pbw_mL_per_kg),
  peep = as.numeric(eicu_source$peep_value),
  resistive_pressure = as.numeric(eicu_source$resistive_pressure),
  smp_per_pbw = as.numeric(eicu_source$smp_per_pbw_J_per_min_per_kg)
)

s0_vars <- c(
  "age", "sex_female", "pf_ratio", "gcs", "map", "vasopressor",
  "platelet", "creatinine"
)
primary_vars <- c(s0_vars, "delta_p", "rr", "smp")
component_vars <- c(
  primary_vars, "vt_per_pbw", "peep", "resistive_pressure"
)
normalized_vars <- c(s0_vars, "smp_per_pbw")

complete_finite <- function(x, columns) {
  Reduce(
    `&`,
    lapply(columns, function(column) {
      value <- x[[column]]
      !is.na(value) & is.finite(value)
    })
  )
}

for (x_name in c("mimic_frame", "eicu_frame")) {
  x <- get(x_name)
  x[, primary_predictor_complete := complete_finite(x, primary_vars)]
  x[, component_predictor_complete := complete_finite(x, component_vars)]
  x[, normalized_exposure_complete := complete_finite(x, normalized_vars)]
  setorder(x, analysis_id)
  assign(x_name, x)
}

canonical_predictors <- c(
  "age", "sex_female", "pf_ratio", "gcs", "map", "vasopressor",
  "platelet", "creatinine", "delta_p", "rr", "smp", "vt_per_pbw",
  "peep", "resistive_pressure", "smp_per_pbw"
)
canonical_schema <- c(
  "analysis_id", canonical_predictors,
  "primary_predictor_complete", "component_predictor_complete",
  "normalized_exposure_complete"
)
if (!identical(names(mimic_frame), canonical_schema) ||
    !identical(names(eicu_frame), canonical_schema)) {
  stop("Canonical model-frame schema changed unexpectedly.")
}

forbidden_output_pattern <- paste(
  c(
    "mort", "death", "dead", "expire", "surviv", "outcome", "discharg",
    "outtime", "icu_end", "unit_end", "hospital_end", "length_of_stay",
    "future", "admin", "time", "offset"
  ),
  collapse = "|"
)
if (any(grepl(
  forbidden_output_pattern, canonical_schema, ignore.case = TRUE
))) {
  stop("Outcome/follow-up/administrative field entered a canonical frame.")
}

same_with_na <- function(x, y, tolerance = 0) {
  if (length(x) != length(y) || !identical(is.na(x), is.na(y))) return(FALSE)
  keep <- !is.na(x)
  if (!any(keep)) return(TRUE)
  all(abs(as.numeric(x[keep]) - as.numeric(y[keep])) <= tolerance)
}

source_mapping_tests <- data.table(
  test = c(
    "mimic_age_exact_source_mapping",
    "eicu_age_exact_source_mapping",
    "mimic_analysis_id_exact_stay_id",
    "eicu_analysis_id_exact_patientunitstayid",
    "mimic_sex_M0_F1_other_missing",
    "eicu_sex_Male0_Female1_other_missing",
    "mimic_sex_exact_source_mapping_after_sort",
    "eicu_sex_exact_source_mapping_after_sort",
    "mimic_smp_per_pbw_exact_source_mapping_after_sort",
    "eicu_smp_per_pbw_exact_source_mapping_after_sort"
  ),
  pass = c(
    same_with_na(
      mimic_frame$age,
      mimic_source$age_at_admission[
        match(mimic_frame$analysis_id, mimic_source$stay_id)
      ]
    ),
    same_with_na(
      eicu_frame$age,
      eicu_source$age_num_harmonized[
        match(eicu_frame$analysis_id, eicu_source$patientunitstayid)
      ]
    ),
    identical(mimic_frame$analysis_id, sort(as.integer(mimic_source$stay_id))),
    identical(
      eicu_frame$analysis_id,
      sort(as.integer(eicu_source$patientunitstayid))
    ),
    all(sex_mimic[mimic_source$gender %chin% "M"] == 0L) &&
      all(sex_mimic[mimic_source$gender %chin% "F"] == 1L) &&
      all(is.na(sex_mimic[!mimic_source$gender %chin% c("M", "F")])),
    all(sex_eicu[eicu_source$gender %chin% "Male"] == 0L) &&
      all(sex_eicu[eicu_source$gender %chin% "Female"] == 1L) &&
    all(is.na(
      sex_eicu[!eicu_source$gender %chin% c("Male", "Female")]
    )),
    same_with_na(
      mimic_frame$sex_female,
      sex_mimic[match(mimic_frame$analysis_id, mimic_source$stay_id)]
    ),
    same_with_na(
      eicu_frame$sex_female,
      sex_eicu[match(eicu_frame$analysis_id, eicu_source$patientunitstayid)]
    ),
    same_with_na(
      mimic_frame$smp_per_pbw,
      mimic_source$smp_per_pbw_J_per_min_per_kg[
        match(mimic_frame$analysis_id, mimic_source$stay_id)
      ]
    ),
    same_with_na(
      eicu_frame$smp_per_pbw,
      eicu_source$smp_per_pbw_J_per_min_per_kg[
        match(eicu_frame$analysis_id, eicu_source$patientunitstayid)
      ]
    )
  )
)
if (any(!source_mapping_tests$pass)) {
  stop(
    "Canonical source-mapping test failed: ",
    paste(source_mapping_tests[pass == FALSE, test], collapse = ", ")
  )
}

# Add deterministic private metadata without adding noncanonical columns.
mimic_metadata <- list(
  artifact_version = "canonical_model_frame_v1",
  database = "MIMIC-IV v3.1",
  analysis_id_source = "stay_id",
  analysis_id_definition = "analysis_id is the exact original stay_id",
  linkage_rule = "Exact analysis_id to MIMIC stay_id; no fuzzy or person-only join",
  source_input_sha256 = sha256_file(mimic_input_path),
  canonical_schema = canonical_schema
)
eicu_metadata <- list(
  artifact_version = "canonical_model_frame_v1",
  database = "eICU-CRD v2.0",
  analysis_id_source = "patientunitstayid",
  analysis_id_definition = "analysis_id is the exact original patientunitstayid",
  linkage_rule = paste(
    "Exact analysis_id to eICU patientunitstayid; no fuzzy, person-only, or",
    "health-system-stay-only join"
  ),
  source_input_sha256 = sha256_file(eicu_input_path),
  canonical_schema = canonical_schema
)

mimic_frame <- as.data.frame(mimic_frame, stringsAsFactors = FALSE)
eicu_frame <- as.data.frame(eicu_frame, stringsAsFactors = FALSE)
attr(mimic_frame, "model_frame_metadata") <- mimic_metadata
attr(eicu_frame, "model_frame_metadata") <- eicu_metadata

# ---------------------------------------------------------------------------
# Range, missingness, and predictor-completeness QC.
# ---------------------------------------------------------------------------

range_rules <- data.table(
  variable = canonical_predictors,
  lower = c(
    18, 0, 0, 3, 1, 0, 0, 0.1, 0, 5, 0, 0, 5, 0, 0
  ),
  upper = c(
    Inf, 1, 300, 15, 250, 1, 9999, 28.28, 40, 60, 100,
    Inf, 30, 75, Inf
  ),
  lower_inclusive = c(
    TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
    TRUE, FALSE, TRUE, TRUE, FALSE
  ),
  upper_inclusive = TRUE
)

range_row <- function(database, frame, rule) {
  value <- frame[[rule$variable]]
  observed <- value[!is.na(value)]
  lower_ok <- if (rule$lower_inclusive) {
    observed >= rule$lower
  } else {
    observed > rule$lower
  }
  upper_ok <- if (rule$upper_inclusive) {
    observed <= rule$upper
  } else {
    observed < rule$upper
  }
  valid <- is.finite(observed) & lower_ok & upper_ok
  q <- if (length(observed)) {
    as.numeric(quantile(
      observed, c(0, .05, .5, .95, 1), names = FALSE, type = 2
    ))
  } else {
    rep(NA_real_, 5L)
  }
  data.table(
    database = database,
    variable = rule$variable,
    total_n = length(value),
    nonmissing_n = length(observed),
    missing_n = sum(is.na(value)),
    invalid_n = sum(!valid),
    min = q[1L], p05 = q[2L], median = q[3L], p95 = q[4L], max = q[5L]
  )
}

range_qc <- rbindlist(lapply(seq_len(nrow(range_rules)), function(i) {
  rbindlist(list(
    range_row("MIMIC-IV", mimic_frame, range_rules[i]),
    range_row("eICU-CRD", eicu_frame, range_rules[i])
  ))
}))
if (any(range_qc$invalid_n > 0L)) {
  stop(
    "Canonical predictor range failure: ",
    paste(
      range_qc[invalid_n > 0, paste(database, variable, invalid_n, sep = ":")],
      collapse = ", "
    )
  )
}

missingness_qc <- rbindlist(lapply(
  list(`MIMIC-IV` = mimic_frame, `eICU-CRD` = eicu_frame),
  function(frame) {
    rbindlist(lapply(canonical_predictors, function(variable) {
      data.table(
        variable = variable,
        total_n = nrow(frame),
        available_n = sum(!is.na(frame[[variable]])),
        missing_n = sum(is.na(frame[[variable]])),
        available_proportion = mean(!is.na(frame[[variable]]))
      )
    }))
  }
), idcol = "database")

completeness_qc <- rbindlist(lapply(
  list(`MIMIC-IV` = mimic_frame, `eICU-CRD` = eicu_frame),
  function(frame) {
    data.table(
      total_n = nrow(frame),
      primary_predictor_complete_n = sum(frame$primary_predictor_complete),
      primary_predictor_complete_proportion = mean(
        frame$primary_predictor_complete
      ),
      component_predictor_complete_n = sum(
        frame$component_predictor_complete
      ),
      component_predictor_complete_proportion = mean(
        frame$component_predictor_complete
      ),
      normalized_exposure_complete_n = sum(
        frame$normalized_exposure_complete
      ),
      normalized_exposure_complete_proportion = mean(
        frame$normalized_exposure_complete
      ),
      component_and_normalized_complete_n = sum(
        frame$component_predictor_complete &
          frame$normalized_exposure_complete
      )
    )
  }
), idcol = "database")

if (sum(mimic_frame$primary_predictor_complete) < 200L) {
  stop("MIMIC primary predictor-complete population is unexpectedly small.")
}
if (sum(
  mimic_frame$component_predictor_complete &
    mimic_frame$normalized_exposure_complete
) < 100L) {
  stop(
    "MIMIC component/normalized common population is too small to freeze ",
    "the prespecified PBW-normalized exposure scale."
  )
}
if (!any(eicu_frame$primary_predictor_complete) ||
    !any(eicu_frame$component_predictor_complete) ||
    !any(eicu_frame$normalized_exposure_complete) ||
    !any(
      eicu_frame$component_predictor_complete &
        eicu_frame$normalized_exposure_complete
    )) {
  stop("An eICU model-comparison population is empty.")
}

# ---------------------------------------------------------------------------
# MIMIC-only numeric parameter derivation (U006 / D054).
# ---------------------------------------------------------------------------

mimic_primary <- mimic_frame[mimic_frame$primary_predictor_complete, , drop = FALSE]
mimic_normalized_scaling <- mimic_frame[
  mimic_frame$component_predictor_complete &
    mimic_frame$normalized_exposure_complete,
  , drop = FALSE
]

three_knot_variables <- c(
  "age", "pf_ratio", "gcs", "map", "platelet", "creatinine"
)
three_probs <- c(0.10, 0.50, 0.90)
smp_probs <- c(0.05, 0.35, 0.65, 0.95)
three_knots <- setNames(lapply(three_knot_variables, function(variable) {
  setNames(
    quantile_knots(
      mimic_primary[[variable]], three_probs,
      variable = variable, type = 2L
    ),
    c("p10", "p50", "p90")
  )
}), three_knot_variables)
smp_knots <- setNames(
  quantile_knots(
    mimic_primary$smp, smp_probs, variable = "smp", type = 2L
  ),
  c("p05", "p35", "p65", "p95")
)
smp_center_scale <- c(
  mean = mean(mimic_primary$smp),
  sd = stats::sd(mimic_primary$smp)
)
smp_per_pbw_center_scale <- c(
  mean = mean(mimic_normalized_scaling$smp_per_pbw),
  sd = stats::sd(mimic_normalized_scaling$smp_per_pbw)
)
if (any(!is.finite(c(smp_center_scale, smp_per_pbw_center_scale))) ||
    smp_center_scale[["sd"]] <= 0 ||
    smp_per_pbw_center_scale[["sd"]] <= 0) {
  stop("A frozen exposure SD is non-finite or non-positive.")
}

model_utils_sha256 <- sha256_file(model_utils_path)
mimic_input_sha256 <- sha256_file(mimic_input_path)
eicu_input_sha256 <- sha256_file(eicu_input_path)

parameters <- list(
  artifact_version = "frozen_predictor_parameters_v1",
  decision_id = "D054",
  locked_config_version = LOCKED$version,
  derivation_database = "MIMIC-IV v3.1 only",
  derivation_population = "primary_predictor_complete",
  derivation_population_n = nrow(mimic_primary),
  normalized_exposure_scale_population = paste(
    "component_predictor_complete AND normalized_exposure_complete"
  ),
  normalized_exposure_scale_population_n = nrow(mimic_normalized_scaling),
  quantile_type = 2L,
  sex_coding = c(male = 0L, female = 1L),
  primary_predictors = primary_vars,
  component_predictors = component_vars,
  normalized_exposure_predictors = normalized_vars,
  three_knot_probabilities = setNames(three_probs, c("p10", "p50", "p90")),
  three_knot_values = three_knots,
  smp_knot_probabilities = setNames(
    smp_probs, c("p05", "p35", "p65", "p95")
  ),
  smp_knot_values = smp_knots,
  smp_center_scale = smp_center_scale,
  smp_per_pbw_center_scale = smp_per_pbw_center_scale,
  canonical_model_frame_schema = canonical_schema,
  model_utils_sha256 = model_utils_sha256,
  mimic_severity_gate_sha256 = sha256_file(mimic_severity_gate_path),
  mimic_input_rds_sha256 = mimic_input_sha256
)

# Human-readable parameter table.
parameter_csv <- rbindlist(c(
  lapply(three_knot_variables, function(variable) {
    data.table(
      variable = variable,
      parameter = names(three_knots[[variable]]),
      probability = three_probs,
      value = as.numeric(three_knots[[variable]]),
      quantile_type = 2L,
      derivation_population = "MIMIC primary predictor-complete",
      derivation_n = nrow(mimic_primary)
    )
  }),
  list(
    data.table(
      variable = "smp",
      parameter = names(smp_knots),
      probability = smp_probs,
      value = as.numeric(smp_knots),
      quantile_type = 2L,
      derivation_population = "MIMIC primary predictor-complete",
      derivation_n = nrow(mimic_primary)
    ),
    data.table(
      variable = "smp",
      parameter = c("mean", "sd"),
      probability = NA_real_,
      value = as.numeric(smp_center_scale),
      quantile_type = NA_integer_,
      derivation_population = "MIMIC primary predictor-complete",
      derivation_n = nrow(mimic_primary)
    ),
    data.table(
      variable = "smp_per_pbw",
      parameter = c("mean", "sd"),
      probability = NA_real_,
      value = as.numeric(smp_per_pbw_center_scale),
      quantile_type = NA_integer_,
      derivation_population = paste(
        "MIMIC component-complete AND normalized-complete"
      ),
      derivation_n = nrow(mimic_normalized_scaling)
    )
  )
), use.names = TRUE)

# ---------------------------------------------------------------------------
# Apply MIMIC parameters unchanged to both databases: transformation tests.
# ---------------------------------------------------------------------------

eicu_primary <- eicu_frame[eicu_frame$primary_predictor_complete, , drop = FALSE]
eicu_normalized_scaling <- eicu_frame[
  eicu_frame$component_predictor_complete &
    eicu_frame$normalized_exposure_complete,
  , drop = FALSE
]

transformation_tests <- rbindlist(c(
  lapply(three_knot_variables, function(variable) {
    mimic_basis <- three_knot_rcs_basis(
      mimic_primary[[variable]], three_knots[[variable]], variable
    )
    eicu_basis <- three_knot_rcs_basis(
      eicu_primary[[variable]], three_knots[[variable]], variable
    )
    data.table(
      test = c(
        paste0(variable, "_type2_knots_exact"),
        paste0(variable, "_mimic_basis_finite_2_columns"),
        paste0(variable, "_eicu_same_knots_basis_finite_2_columns")
      ),
      pass = c(
        identical(
          unname(three_knots[[variable]]),
          as.numeric(quantile(
            mimic_primary[[variable]], three_probs,
            names = FALSE, type = 2
          ))
        ),
        ncol(mimic_basis) == 2L && all(is.finite(mimic_basis)),
        ncol(eicu_basis) == 2L && all(is.finite(eicu_basis)) &&
          identical(colnames(mimic_basis), colnames(eicu_basis))
      ),
      detail = c(
        paste(format(three_knots[[variable]], digits = 15), collapse = ";"),
        paste0("MIMIC n=", nrow(mimic_basis)),
        paste0("eICU n=", nrow(eicu_basis), "; MIMIC knots unchanged")
      )
    )
  }),
  list(local({
    mimic_smp_basis <- four_knot_rcs_basis(
      mimic_primary$smp, smp_knots, "smp"
    )
    eicu_smp_basis <- four_knot_rcs_basis(
      eicu_primary$smp, smp_knots, "smp"
    )
    mimic_smp_z <- (
      mimic_primary$smp - smp_center_scale[["mean"]]
    ) / smp_center_scale[["sd"]]
    eicu_smp_z <- (
      eicu_primary$smp - smp_center_scale[["mean"]]
    ) / smp_center_scale[["sd"]]
    mimic_pbw_z <- (
      mimic_normalized_scaling$smp_per_pbw -
        smp_per_pbw_center_scale[["mean"]]
    ) / smp_per_pbw_center_scale[["sd"]]
    eicu_pbw_z <- (
      eicu_normalized_scaling$smp_per_pbw -
        smp_per_pbw_center_scale[["mean"]]
    ) / smp_per_pbw_center_scale[["sd"]]
    data.table(
      test = c(
        "smp_type2_four_knots_exact",
        "smp_mimic_basis_finite_3_columns",
        "smp_eicu_same_knots_basis_finite_3_columns",
        "smp_mimic_mean_sd_exact",
        "smp_eicu_standardization_uses_mimic_mean_sd",
        "smp_per_pbw_mimic_mean_sd_exact_common_population",
        "smp_per_pbw_eicu_standardization_uses_mimic_mean_sd"
      ),
      pass = c(
        identical(
          unname(smp_knots),
          as.numeric(quantile(
            mimic_primary$smp, smp_probs, names = FALSE, type = 2
          ))
        ),
        ncol(mimic_smp_basis) == 3L && all(is.finite(mimic_smp_basis)),
        ncol(eicu_smp_basis) == 3L && all(is.finite(eicu_smp_basis)) &&
          identical(colnames(mimic_smp_basis), colnames(eicu_smp_basis)),
        abs(mean(mimic_smp_z)) < 1e-12 &&
          abs(stats::sd(mimic_smp_z) - 1) < 1e-12,
        all(is.finite(eicu_smp_z)) && all(abs(
          eicu_smp_z * smp_center_scale[["sd"]] +
            smp_center_scale[["mean"]] - eicu_primary$smp
        ) < 1e-10),
        abs(mean(mimic_pbw_z)) < 1e-12 &&
          abs(stats::sd(mimic_pbw_z) - 1) < 1e-12,
        all(is.finite(eicu_pbw_z)) && all(abs(
          eicu_pbw_z * smp_per_pbw_center_scale[["sd"]] +
            smp_per_pbw_center_scale[["mean"]] -
            eicu_normalized_scaling$smp_per_pbw
        ) < 1e-10)
      ),
      detail = c(
        paste(format(smp_knots, digits = 15), collapse = ";"),
        paste0("MIMIC n=", nrow(mimic_smp_basis)),
        paste0("eICU n=", nrow(eicu_smp_basis), "; MIMIC knots unchanged"),
        paste(format(smp_center_scale, digits = 15), collapse = ";"),
        paste0("eICU n=", length(eicu_smp_z)),
        paste(
          format(smp_per_pbw_center_scale, digits = 15), collapse = ";"
        ),
        paste0("eICU n=", length(eicu_pbw_z))
      )
    )
  }))
), use.names = TRUE)

coverage_qc <- rbindlist(lapply(
  c(three_knot_variables, "smp"),
  function(variable) {
    knots <- if (variable == "smp") smp_knots else three_knots[[variable]]
    rbindlist(lapply(
      list(`MIMIC-IV` = mimic_frame, `eICU-CRD` = eicu_frame),
      function(frame) {
        value <- frame[[variable]]
        value <- value[!is.na(value)]
        data.table(
          variable = variable,
          nonmissing_n = length(value),
          below_mimic_lower_knot_n = sum(value < min(knots)),
          within_mimic_boundary_knots_n = sum(
            value >= min(knots) & value <= max(knots)
          ),
          above_mimic_upper_knot_n = sum(value > max(knots)),
          mimic_lower_knot = min(knots),
          mimic_upper_knot = max(knots),
          clipped_or_reestimated = FALSE
        )
      }
    ), idcol = "database")
  }
))

frame_tests <- rbindlist(list(
  input_gate_validation,
  source_mapping_tests,
  data.table(
    test = c(
      "mimic_frame_exact_canonical_schema",
      "eicu_frame_exact_canonical_schema",
      "mimic_analysis_id_unique_nonmissing",
      "eicu_analysis_id_unique_nonmissing",
      "primary_complete_recomputes_MIMIC",
      "primary_complete_recomputes_eICU",
      "component_complete_recomputes_MIMIC",
      "component_complete_recomputes_eICU",
      "normalized_complete_recomputes_MIMIC",
      "normalized_complete_recomputes_eICU",
      "canonical_ranges_pass",
      "no_eicu_parameter_reestimation"
    ),
    pass = c(
      identical(names(mimic_frame), canonical_schema),
      identical(names(eicu_frame), canonical_schema),
      !anyDuplicated(mimic_frame$analysis_id) && !anyNA(mimic_frame$analysis_id),
      !anyDuplicated(eicu_frame$analysis_id) && !anyNA(eicu_frame$analysis_id),
      identical(
        mimic_frame$primary_predictor_complete,
        complete_finite(mimic_frame, primary_vars)
      ),
      identical(
        eicu_frame$primary_predictor_complete,
        complete_finite(eicu_frame, primary_vars)
      ),
      identical(
        mimic_frame$component_predictor_complete,
        complete_finite(mimic_frame, component_vars)
      ),
      identical(
        eicu_frame$component_predictor_complete,
        complete_finite(eicu_frame, component_vars)
      ),
      identical(
        mimic_frame$normalized_exposure_complete,
        complete_finite(mimic_frame, normalized_vars)
      ),
      identical(
        eicu_frame$normalized_exposure_complete,
        complete_finite(eicu_frame, normalized_vars)
      ),
      all(range_qc$invalid_n == 0L),
      identical(parameters$derivation_database, "MIMIC-IV v3.1 only") &&
        !any(grepl("eicu", names(parameters), ignore.case = TRUE))
    )
  )
), use.names = TRUE, fill = TRUE)
frame_tests[, detail := ""]
all_tests <- rbindlist(list(frame_tests, transformation_tests), fill = TRUE)
all_tests[is.na(detail), detail := ""]
if (any(!all_tests$pass)) {
  stop(
    "Predictor-freeze test failure(s): ",
    paste(all_tests[pass == FALSE, test], collapse = ", ")
  )
}

# Exact allow-list is the primary leakage protection. No administrative or
# outcome field is permitted in either frame or in parameter names.
parameter_recursive_names <- unique(unlist(lapply(parameters, names)))
parameter_recursive_names <- parameter_recursive_names[
  !is.na(parameter_recursive_names)
]
outcome_leakage_guard <- data.table(
  check = c(
    "mimic_frame_exact_allowlist",
    "eicu_frame_exact_allowlist",
    "mimic_frame_has_no_outcome_followup_admin_column",
    "eicu_frame_has_no_outcome_followup_admin_column",
    "parameter_artifact_has_no_outcome_named_field",
    "frames_contain_no_source_time_or_discharge_field",
    "eicu_used_only_for_coverage_and_transform_smoke_tests"
  ),
  pass = c(
    identical(names(mimic_frame), canonical_schema),
    identical(names(eicu_frame), canonical_schema),
    !any(grepl(
      forbidden_output_pattern, names(mimic_frame), ignore.case = TRUE
    )),
    !any(grepl(
      forbidden_output_pattern, names(eicu_frame), ignore.case = TRUE
    )),
    !any(grepl(
      "mort|death|dead|expire|surviv|outcome|discharg",
      c(names(parameters), parameter_recursive_names), ignore.case = TRUE
    )),
    !any(c(
      "prediction_time", "index_time", "outtime", "icu_end_offset",
      "hospitaldischargeoffset", "unitdischargeoffset", "hospitalid"
    ) %in% c(names(mimic_frame), names(eicu_frame))),
    identical(parameters$derivation_database, "MIMIC-IV v3.1 only")
  )
)
if (any(!outcome_leakage_guard$pass)) {
  stop(
    "Outcome/administrative leakage guard failed: ",
    paste(outcome_leakage_guard[pass == FALSE, check], collapse = ", ")
  )
}

# ---------------------------------------------------------------------------
# Immutable private artifacts and aggregate human-readable QC.
# ---------------------------------------------------------------------------

parameter_rds_sha256 <- immutable_save_rds(parameters, parameter_path)
mimic_model_frame_rds_sha256 <- immutable_save_rds(
  mimic_frame, mimic_frame_path
)
eicu_model_frame_rds_sha256 <- immutable_save_rds(eicu_frame, eicu_frame_path)

schema_qc <- data.table(
  position = seq_along(canonical_schema),
  field = canonical_schema,
  role = c(
    "linkage_key", rep("canonical_raw_predictor", length(canonical_predictors)),
    rep("complete_population_flag", 3L)
  ),
  mimic_storage_type = vapply(mimic_frame, typeof, character(1L)),
  eicu_storage_type = vapply(eicu_frame, typeof, character(1L))
)

sex_qc <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    source_value = as.character(mimic_source$gender[
      match(mimic_frame$analysis_id, mimic_source$stay_id)
    ]),
    coded_value = mimic_frame$sex_female
  )[, .N, by = .(database, source_value, coded_value)],
  data.table(
    database = "eICU-CRD",
    source_value = as.character(eicu_source$gender[
      match(eicu_frame$analysis_id, eicu_source$patientunitstayid)
    ]),
    coded_value = eicu_frame$sex_female
  )[, .N, by = .(database, source_value, coded_value)]
), use.names = TRUE)

atomic_fwrite(
  input_gate_validation,
  file.path(qc_out, "input_gate_hash_validation.csv")
)
atomic_fwrite(schema_qc, file.path(qc_out, "canonical_frame_schema_QC.csv"))
atomic_fwrite(sex_qc, file.path(qc_out, "sex_coding_QC.csv"))
atomic_fwrite(completeness_qc, file.path(qc_out, "predictor_completeness_QC.csv"))
atomic_fwrite(missingness_qc, file.path(qc_out, "predictor_missingness_QC.csv"))
atomic_fwrite(range_qc, file.path(qc_out, "predictor_range_QC.csv"))
atomic_fwrite(
  parameter_csv,
  file.path(qc_out, "frozen_predictor_parameters_v1.csv")
)
atomic_fwrite(
  coverage_qc,
  file.path(qc_out, "external_parameter_coverage_QC.csv")
)
atomic_fwrite(all_tests, file.path(qc_out, "transformation_tests.csv"))
atomic_fwrite(
  outcome_leakage_guard,
  file.path(qc_out, "outcome_leakage_guard.csv")
)

summary_path <- file.path(qc_out, "parameter_freeze_QC.md")
summary_lines <- c(
  "# Outcome-blind canonical predictor and parameter freeze QC",
  "",
  paste0("- Locked configuration: ", LOCKED$version),
  paste0("- MIMIC source rows: ", nrow(mimic_frame)),
  paste0("- eICU source rows: ", nrow(eicu_frame)),
  paste0(
    "- MIMIC primary predictor-complete: ", nrow(mimic_primary)
  ),
  paste0(
    "- eICU primary predictor-complete (parameters unchanged): ",
    nrow(eicu_primary)
  ),
  paste0(
    "- MIMIC component/normalized common scale population: ",
    nrow(mimic_normalized_scaling)
  ),
  paste0(
    "- eICU component/normalized common population: ",
    nrow(eicu_normalized_scaling)
  ),
  paste0("- Quantile algorithm: type=2"),
  paste0(
    "- Three-knot variables: ", paste(three_knot_variables, collapse = ", ")
  ),
  paste0(
    "- sMP four knots: ",
    paste(format(smp_knots, digits = 8), collapse = ", ")
  ),
  paste0(
    "- sMP mean/SD: ",
    paste(format(smp_center_scale, digits = 8), collapse = ", ")
  ),
  paste0(
    "- sMP/PBW mean/SD: ",
    paste(format(smp_per_pbw_center_scale, digits = 8), collapse = ", ")
  ),
  "- Sex coding: male=0, female=1, other/unknown=missing.",
  paste0(
    "- Canonical frames contain one exact source-ID linkage key, 15 raw ",
    "predictors, and three completeness flags only."
  ),
  paste0(
    "- sMP/PBW scaling supports prespecified P1-C secondary analysis and ",
    "does not alter the primary S2-versus-S3 parameter population."
  ),
  paste0(
    "- eICU supplied no knot, mean, SD, model-form, performance, or ",
    "outcome information to this freeze."
  ),
  "- No mortality, discharge, follow-up, effect, or performance field was read.",
  "",
  "BUILD_COMPLETE"
)
atomic_write_lines(summary_lines, summary_path)

required_qc <- file.path(qc_out, c(
  "input_gate_hash_validation.csv", "canonical_frame_schema_QC.csv",
  "sex_coding_QC.csv", "predictor_completeness_QC.csv",
  "predictor_missingness_QC.csv", "predictor_range_QC.csv",
  "frozen_predictor_parameters_v1.csv",
  "external_parameter_coverage_QC.csv", "transformation_tests.csv",
  "outcome_leakage_guard.csv", "parameter_freeze_QC.md"
))
if (!all(file.exists(required_qc)) ||
    !all(file.exists(c(parameter_path, mimic_frame_path, eicu_frame_path)))) {
  stop("A required parameter-freeze artifact was not published.")
}

completion <- data.table(
  field = c(
    "status", "locked_config_version", "completed_at", "script_sha256",
    "mimic_severity_gate_sha256", "eicu_severity_gate_sha256",
    "mimic_severity_script_sha256", "eicu_severity_script_sha256",
    "mimic_input_rds_sha256", "eicu_input_rds_sha256",
    "model_utils_path", "model_utils_sha256",
    "parameter_rds_path", "parameter_rds_sha256",
    "mimic_model_frame_rds_path", "mimic_model_frame_rds_sha256",
    "eicu_model_frame_rds_path", "eicu_model_frame_rds_sha256",
    "mimic_frame_n", "eicu_frame_n",
    "mimic_primary_predictor_complete_n",
    "eicu_primary_predictor_complete_n",
    "mimic_component_predictor_complete_n",
    "eicu_component_predictor_complete_n",
    "mimic_normalized_exposure_complete_n",
    "eicu_normalized_exposure_complete_n",
    "mimic_component_normalized_common_n",
    "eicu_component_normalized_common_n",
    "parameter_derivation_database", "quantile_type",
    "all_tests_pass", "outcome_leakage_guard_pass",
    "all_required_qc_present", "summary_sentinel"
  ),
  value = c(
    "PASS", LOCKED$version, format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    sha256_file(script_path),
    sha256_file(mimic_severity_gate_path),
    sha256_file(eicu_severity_gate_path),
    sha256_file(mimic_severity_script),
    sha256_file(eicu_severity_script),
    mimic_input_sha256, eicu_input_sha256,
    file.path("code", "R", "rebuild_v1", "08_model_utils.R"),
    model_utils_sha256,
    relative_parameter_path, parameter_rds_sha256,
    relative_mimic_frame_path, mimic_model_frame_rds_sha256,
    relative_eicu_frame_path, eicu_model_frame_rds_sha256,
    nrow(mimic_frame), nrow(eicu_frame),
    sum(mimic_frame$primary_predictor_complete),
    sum(eicu_frame$primary_predictor_complete),
    sum(mimic_frame$component_predictor_complete),
    sum(eicu_frame$component_predictor_complete),
    sum(mimic_frame$normalized_exposure_complete),
    sum(eicu_frame$normalized_exposure_complete),
    sum(
      mimic_frame$component_predictor_complete &
        mimic_frame$normalized_exposure_complete
    ),
    sum(
      eicu_frame$component_predictor_complete &
        eicu_frame$normalized_exposure_complete
    ),
    "MIMIC-IV v3.1 only", 2L,
    all(all_tests$pass), all(outcome_leakage_guard$pass),
    all(file.exists(required_qc)), "BUILD_COMPLETE"
  )
)
if (anyDuplicated(completion$field)) stop("Duplicate completion-gate field.")
fwrite(completion, completion_gate_tmp)
if (!file.rename(completion_gate_tmp, completion_gate)) {
  stop("Could not atomically publish parameter-freeze completion gate.")
}

message("Predictor parameter freeze complete.")
message("  MIMIC primary complete: ", nrow(mimic_primary))
message("  eICU primary complete: ", nrow(eicu_primary))
message("  parameter SHA256: ", parameter_rds_sha256)
message("  gate: ", completion_gate)
