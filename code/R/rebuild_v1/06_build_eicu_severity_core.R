#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: eICU harmonized severity core
#
# Outcome-blind Phase 2b extraction. Two products are deliberately separated:
#   1) prediction-time HSC for the complete-tuple population; and
#   2) index-known HSC for every strict-cohort patient, for tuple-observation
#      selection/IPW work only.
#
# No mortality, discharge, survival, or outcome field is read. APACHE IVa is
# written to a separate native-benchmark artifact and is never substituted for
# a time-aligned HSC variable.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("code/R/rebuild_v1/06_build_eicu_severity_core.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "00_config.R"))

stopifnot(
  identical(LOCKED$version, "1.0.1"),
  LOCKED$primary_exposure_window_hours_after_index == 6
)

input_exposure <- file.path(
  PRIVATE_ROOT, "eicu", "eicu_paired_exposure_primary_60min_v1.rds"
)
phase1_gate_path <- file.path(QC_ROOT, "eicu", "phase1_eicu_complete_v1.csv")
phase2_gate_path <- file.path(
  QC_ROOT, "eicu_exposure", "phase2_eicu_exposure_complete_v1.csv"
)
phase1_script <- file.path(dirname(script_path), "02_build_eicu_index_cohort.R")
phase2_script <- file.path(dirname(script_path), "04_build_eicu_paired_exposure.R")
raw_patient <- file.path(EICU_ROOT, "patient.csv.gz")
raw_nurse <- file.path(EICU_ROOT, "nurseCharting.csv.gz")
raw_lab <- file.path(EICU_ROOT, "lab.csv.gz")
raw_infusion <- file.path(EICU_ROOT, "infusionDrug.csv.gz")
raw_medication <- file.path(EICU_ROOT, "medication.csv.gz")
raw_apache <- file.path(EICU_ROOT, "apachePatientResult.csv.gz")

private_out <- file.path(PRIVATE_ROOT, "eicu")
qc_out <- file.path(QC_ROOT, "eicu_severity")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(qc_out, "phase2b_complete_v1.csv")
completion_gate_tmp <- paste0(completion_gate, ".tmp")
unlink(c(completion_gate, completion_gate_tmp), force = TRUE)

required_files <- c(
  input_exposure, phase1_gate_path, phase2_gate_path, phase1_script,
  phase2_script, raw_patient, raw_nurse, raw_lab, raw_infusion,
  raw_medication, raw_apache
)
if (any(!file.exists(required_files))) {
  stop("Missing required input(s): ", paste(required_files[!file.exists(required_files)], collapse = ", "))
}

forbidden_pattern <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)

strict_numeric <- function(x) {
  z <- trimws(as.character(x))
  ok <- grepl("^[+-]?((([0-9]+)(\\.[0-9]*)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)?$", z)
  out <- rep(NA_real_, length(z))
  out[ok] <- suppressWarnings(as.numeric(z[ok]))
  out[!is.finite(out)] <- NA_real_
  out
}

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

read_gate_map <- function(path) {
  x <- fread(path, showProgress = FALSE)
  if (!identical(names(x), c("field", "value")) || anyDuplicated(x$field)) {
    stop("Malformed field/value completion gate: ", path)
  }
  setNames(x$value, x$field)
}

require_gate_value <- function(gate, field, expected = NULL) {
  value <- unname(gate[field])
  if (length(value) != 1L || is.na(value)) {
    stop("Completion gate missing field: ", field)
  }
  if (!is.null(expected) && !identical(value, as.character(expected))) {
    stop(
      "Completion-gate mismatch for ", field, ": ", value,
      " != ", as.character(expected)
    )
  }
  value
}

# Immutable upstream chain: no raw severity source is opened until both cohort
# and exposure gates, scripts, and row-level hashes agree.
phase1_gate <- read_gate_map(phase1_gate_path)
phase2_gate <- read_gate_map(phase2_gate_path)
require_gate_value(phase1_gate, "locked_config_version", LOCKED$version)
require_gate_value(phase1_gate, "all_invariants_pass", "TRUE")
require_gate_value(phase1_gate, "outcome_leakage_guard_pass", "TRUE")
require_gate_value(phase1_gate, "all_required_qc_present", "TRUE")
require_gate_value(phase2_gate, "locked_config_version", LOCKED$version)
require_gate_value(phase2_gate, "all_invariants_pass", "TRUE")
require_gate_value(phase2_gate, "outcome_leakage_guard_pass", "TRUE")
require_gate_value(phase2_gate, "all_required_qc_present", "TRUE")
require_gate_value(
  phase1_gate, "script_sha256", sha256_file(phase1_script)
)
require_gate_value(
  phase2_gate, "script_sha256", sha256_file(phase2_script)
)
require_gate_value(
  phase2_gate, "phase1_gate_sha256", sha256_file(phase1_gate_path)
)
input_exposure_sha256 <- sha256_file(input_exposure)
require_gate_value(
  phase2_gate, "primary_60min_rds_sha256", input_exposure_sha256
)

quantile_safe <- function(x, probs = c(0, .05, .25, .5, .75, .95, 1)) {
  if (!length(x) || all(is.na(x))) return(rep(NA_real_, length(probs)))
  as.numeric(quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 2))
}

distribution_row <- function(window_type, variable, x) {
  q <- quantile_safe(x)
  data.table(
    window_type = window_type, variable = variable,
    n = sum(!is.na(x)), missing_n = sum(is.na(x)),
    min = q[1L], q05 = q[2L], q25 = q[3L], median = q[4L],
    q75 = q[5L], q95 = q[6L], max = q[7L],
    mean = if (any(!is.na(x))) mean(x, na.rm = TRUE) else NA_real_,
    sd = if (sum(!is.na(x)) > 1L) sd(x, na.rm = TRUE) else NA_real_
  )
}

read_filtered_cache <- function(path, source_key, required_columns, manifest) {
  if (!file.exists(path)) stop("Missing CSV-aware cache: ", path)
  out <- fread(
    cmd = sprintf("gzip -cd %s", shQuote(path)),
    showProgress = interactive(), fill = FALSE
  )
  missing <- setdiff(required_columns, names(out))
  if (length(missing)) {
    stop("Filtered cache ", source_key, " is missing: ", paste(missing, collapse = ", "))
  }
  # `source_key` is deliberately distinct from the manifest column name;
  # otherwise data.table resolves both sides of `source_name == source_name`
  # inside `i` to the column and silently selects every manifest row.
  expected <- manifest[source_name == source_key, kept_rows]
  if (length(expected) != 1L || nrow(out) != expected) {
    stop(
      "Filtered-cache row-count mismatch for ", source_key,
      ": R=", nrow(out), ", manifest=", paste(expected, collapse = ",")
    )
  }
  out
}

# ---------------------------------------------------------------------------
# Outcome-free base cohort and the two non-overlapping analytic windows.
# ---------------------------------------------------------------------------

exposure_source <- as.data.table(readRDS(input_exposure))
required_exposure <- c(
  "patientunitstayid", "patienthealthsystemstayid", "person_key", "uniquepid",
  "hospitalid", "age_num", "gender", "index_time", "pf_ratio",
  "prediction_time", "tuple_observed"
)
missing_exposure <- setdiff(required_exposure, names(exposure_source))
if (length(missing_exposure)) {
  stop("Paired-exposure artifact is missing: ", paste(missing_exposure, collapse = ", "))
}
if (any(grepl(forbidden_pattern, names(exposure_source), ignore.case = TRUE))) {
  stop("Outcome-like field found in paired-exposure source.")
}
if (anyDuplicated(exposure_source$patientunitstayid)) {
  stop("Paired-exposure artifact must contain one row per strict-cohort stay.")
}
if (nrow(exposure_source) != as.integer(require_gate_value(
  phase2_gate, "strict_cohort_n"
))) {
  stop("Paired-exposure row count disagrees with the Phase-2 gate.")
}
if (sum(exposure_source$tuple_observed, na.rm = TRUE) != as.integer(
  require_gate_value(phase2_gate, "primary_60min_n")
)) {
  stop("Observed-tuple count disagrees with the Phase-2 gate.")
}

strict_base <- copy(exposure_source)
strict_base[, index_window_start := pmax(0, index_time - 1440)]
strict_base[, index_window_end := index_time]

tuple_base <- strict_base[tuple_observed == TRUE & !is.na(prediction_time)]
tuple_base[, prediction_window_start := pmax(0, index_time - 1440)]
tuple_base[, prediction_window_end := prediction_time]
if (!nrow(tuple_base)) stop("No complete-tuple patient has a prediction time.")

if (any(strict_base$index_window_start > strict_base$index_window_end)) {
  stop("Invalid index-known severity window.")
}
if (any(tuple_base$prediction_window_start > tuple_base$prediction_window_end)) {
  stop("Invalid prediction-time severity window.")
}

all_id_file <- tempfile("eicu_strict_ids_", fileext = ".txt")
on.exit(unlink(all_id_file), add = TRUE)
fwrite(strict_base[, .(patientunitstayid)], all_id_file, col.names = FALSE)

# Quoted commas in eICU CSV fields make delimiter-naive shell filtering unsafe.
# A Python stdlib csv+gzip helper scans every logical record to EOF and
# atomically publishes canonical caches plus a checksum-bearing gate.
filter_helper <- file.path(
  dirname(script_path), "06a_filter_eicu_severity_inputs.py"
)
cache_dir <- file.path(private_out, "cache_v1", "eicu_severity")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(filter_helper)) stop("Missing CSV-aware filter helper.")
filter_output <- system2(
  "python3",
  c(
    shQuote(filter_helper), "--ids", shQuote(all_id_file),
    "--eicu-root", shQuote(EICU_ROOT),
    "--cache-dir", shQuote(cache_dir)
  ),
  stdout = TRUE, stderr = TRUE
)
filter_status <- attr(filter_output, "status")
if (!is.null(filter_status) && filter_status != 0L) {
  stop("CSV-aware filter failed: ", paste(filter_output, collapse = "\n"))
}
message(paste(filter_output, collapse = "\n"))

cache_gate_path <- file.path(cache_dir, "severity_input_cache_complete_v1.csv")
cache_manifest_path <- file.path(cache_dir, "filter_manifest_v1.csv")
if (!file.exists(cache_gate_path) || !file.exists(cache_manifest_path)) {
  stop("CSV-aware cache has no completion gate/manifest.")
}
cache_gate <- fread(cache_gate_path)
cache_manifest <- fread(cache_manifest_path)
if (nrow(cache_gate) != 1L || cache_gate$status[[1L]] != "PASS") {
  stop("CSV-aware cache gate is not PASS.")
}
if (nrow(cache_manifest) != 4L || any(cache_manifest$status != "PASS") ||
    any(cache_manifest$reached_eof != TRUE)) {
  stop("CSV-aware cache manifest did not record four complete EOF scans.")
}
if (cache_gate$strict_id_count[[1L]] != nrow(strict_base) ||
    any(cache_manifest$strict_id_count != nrow(strict_base))) {
  stop("CSV-aware cache strict-ID count mismatch.")
}
helper_hash <- sha256_file(filter_helper)
manifest_hash <- sha256_file(cache_manifest_path)
if (cache_gate$helper_sha256[[1L]] != helper_hash ||
    any(cache_manifest$helper_sha256 != helper_hash)) {
  stop("CSV-aware cache helper hash mismatch.")
}
if (cache_gate$manifest_sha256[[1L]] != manifest_hash) {
  stop("CSV-aware cache manifest hash mismatch.")
}
cache_output_hashes <- vapply(
  cache_manifest$output_path, sha256_file, character(1L)
)
if (any(cache_manifest$output_sha256 != cache_output_hashes)) {
  stop("CSV-aware cache output hash mismatch.")
}

cache_paths <- c(
  nurseCharting = file.path(cache_dir, "nurse_severity_candidates_v1.csv.gz"),
  lab = file.path(cache_dir, "lab_severity_candidates_v1.csv.gz"),
  infusionDrug = file.path(cache_dir, "infusion_severity_candidates_v1.csv.gz"),
  medication = file.path(cache_dir, "medication_severity_candidates_v1.csv.gz")
)

make_bounds <- function(window_type) {
  if (identical(window_type, "prediction_time_hsc")) {
    tuple_base[, .(
      patientunitstayid,
      window_start = prediction_window_start,
      window_end = prediction_window_end
    )]
  } else if (identical(window_type, "index_known_selection")) {
    strict_base[, .(
      patientunitstayid,
      window_start = index_window_start,
      window_end = index_window_end
    )]
  } else {
    stop("Unknown window type: ", window_type)
  }
}

window_types <- c("prediction_time_hsc", "index_known_selection")

# ---------------------------------------------------------------------------
# Admission-known demographics/height. eICU documents age >89 as the literal
# top-code "> 89". Primary numeric mapping is 90, the lowest integer compatible
# with that category; every such record receives a sensitivity/exclusion flag.
# admissionheight is documented in cm and is never unit-guessed.
# ---------------------------------------------------------------------------

patient_age <- fread(
  cmd = sprintf("gzip -cd %s", shQuote(raw_patient)),
  select = c("patientunitstayid", "age", "gender", "admissionheight"),
  showProgress = FALSE
)
patient_age <- patient_age[patientunitstayid %in% strict_base$patientunitstayid]
if (nrow(patient_age) != nrow(strict_base) || anyDuplicated(patient_age$patientunitstayid)) {
  stop("Patient age did not join one-to-one to the strict cohort.")
}
patient_age[, age_topcoded_gt89 := age == "> 89"]
patient_age[, age_num_harmonized := fifelse(
  age_topcoded_gt89, 90, suppressWarnings(as.numeric(age))
)]
patient_age[, age_topcode_sensitivity_exclude := age_topcoded_gt89]
if (anyNA(patient_age$age_num_harmonized)) {
  stop("An eICU age could not be mapped without guessing.")
}
patient_age[, height_raw := suppressWarnings(as.numeric(admissionheight))]
patient_age[, height_cm := fifelse(
  !is.na(height_raw) & height_raw >= 120 & height_raw <= 230,
  height_raw, NA_real_
)]
patient_age[, height_valid := !is.na(height_cm)]
patient_age[, sex_binary_for_pbw := fcase(
  gender == "Male", "male",
  gender == "Female", "female",
  default = NA_character_
)]
patient_age[, pbw_kg := fcase(
  height_valid & sex_binary_for_pbw == "male",
  50 + 0.91 * (height_cm - 152.4),
  height_valid & sex_binary_for_pbw == "female",
  45.5 + 0.91 * (height_cm - 152.4),
  default = NA_real_
)]
if (any(!is.na(patient_age$pbw_kg) & patient_age$pbw_kg <= 0)) {
  stop("Non-positive eICU predicted body weight.")
}

age_check <- patient_age[
  strict_base[, .(patientunitstayid, age_num_existing = age_num)],
  on = "patientunitstayid"
]
if (any(abs(age_check$age_num_harmonized - age_check$age_num_existing) > 1e-12)) {
  stop("Harmonized age disagrees with the outcome-blind cohort builder.")
}
gender_check <- patient_age[
  strict_base[, .(patientunitstayid, gender_existing = gender)],
  on = "patientunitstayid"
]
if (any(gender_check$gender != gender_check$gender_existing, na.rm = TRUE)) {
  stop("Patient gender disagrees with the outcome-blind cohort builder.")
}

# ---------------------------------------------------------------------------
# nurseCharting: exact official/local GCS and MAP mappings.
#
# Provenance: official MIT-LCP/eicu-code commit
# 34cece8c70771a3fab48da84d4c47f0e133ca021, pivoted-gcs.sql and
# pivoted-vital.sql. Candidate label rows are retained for aggregate mapping QC;
# only exact label/name pairs below can enter the HSC.
# ---------------------------------------------------------------------------

message("Reading CSV-aware eICU GCS/MAP cache ...")
nurse <- read_filtered_cache(
  cache_paths[["nurseCharting"]], "nurseCharting",
  c(
    "nursingchartid", "patientunitstayid", "nursingchartoffset",
    "nursingchartentryoffset", "nursingchartcelltypecat",
    "nursingchartcelltypevallabel", "nursingchartcelltypevalname",
    "nursingchartvalue"
  ), cache_manifest
)
if (!nrow(nurse)) stop("No GCS/MAP candidate nurseCharting rows were found.")

nurse[, mapping := fcase(
  nursingchartcelltypevallabel == "Glasgow coma score" &
    nursingchartcelltypevalname == "GCS Total", "gcs_total",
  nursingchartcelltypevallabel == "Score (Glasgow Coma Scale)" &
    nursingchartcelltypevalname == "Value", "gcs_total",
  nursingchartcelltypevallabel == "Glasgow coma score" &
    nursingchartcelltypevalname == "Eyes", "gcs_eye",
  nursingchartcelltypevallabel == "Glasgow coma score" &
    nursingchartcelltypevalname == "Verbal", "gcs_verbal",
  nursingchartcelltypevallabel == "Glasgow coma score" &
    nursingchartcelltypevalname == "Motor", "gcs_motor",
  nursingchartcelltypevallabel == "Non-Invasive BP" &
    nursingchartcelltypevalname == "Non-Invasive BP Mean", "map_noninvasive",
  nursingchartcelltypevallabel == "Invasive BP" &
    nursingchartcelltypevalname == "Invasive BP Mean", "map_invasive",
  nursingchartcelltypevallabel == "MAP (mmHg)" &
    nursingchartcelltypevalname == "Value", "map_invasive",
  nursingchartcelltypevallabel == "Arterial Line MAP (mmHg)" &
    nursingchartcelltypevalname == "Value", "map_invasive",
  default = "candidate_not_used"
)]
nurse[, value_num := strict_numeric(nursingchartvalue)]
nurse[, measurement_time := as.numeric(nursingchartoffset)]
nurse[, available_time := fifelse(
  is.na(nursingchartentryoffset),
  measurement_time,
  pmax(measurement_time, as.numeric(nursingchartentryoffset))
)]

nurse_mapping_qc <- nurse[, .(
  raw_rows = .N,
  strict_stays = uniqueN(patientunitstayid),
  numeric_rows = sum(!is.na(value_num)),
  nonnumeric_rows = sum(is.na(value_num)),
  entry_offset_missing_rows = sum(is.na(nursingchartentryoffset))
), by = .(
  nursingchartcelltypecat, nursingchartcelltypevallabel,
  nursingchartcelltypevalname, mapping
)]
setorder(
  nurse_mapping_qc, nursingchartcelltypecat,
  nursingchartcelltypevallabel, nursingchartcelltypevalname
)
fwrite(nurse_mapping_qc, file.path(qc_out, "nurse_mapping_frequency.csv"))

gcs_nonnumeric_qc <- nurse[
  mapping %chin% c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor") &
    is.na(value_num),
  .(raw_rows = .N, strict_stays = uniqueN(patientunitstayid)),
  by = .(mapping, raw_value = trimws(as.character(nursingchartvalue)))
]
setorder(gcs_nonnumeric_qc, mapping, -raw_rows, raw_value)
fwrite(gcs_nonnumeric_qc, file.path(qc_out, "gcs_nonnumeric_value_frequency.csv"))

derive_gcs <- function(bounds, window_type) {
  z <- merge(nurse, bounds, by = "patientunitstayid", all = FALSE, sort = FALSE)
  z[, in_measurement_window :=
      measurement_time >= window_start & measurement_time <= window_end]
  z[, available_by_window_end :=
      !is.na(available_time) & available_time <= window_end]
  z <- z[in_measurement_window & available_by_window_end]

  explicit <- z[mapping == "gcs_total"]
  explicit[, valid_gcs :=
    !is.na(value_num) & abs(value_num - round(value_num)) < 1e-10 &
      value_num >= 3 & value_num <= 15]
  explicit_valid <- explicit[valid_gcs == TRUE]
  explicit_reduced <- explicit_valid[, {
    vals <- unique(value_num)
    list(
      value_conflict = length(vals) > 1L,
      gcs_value = if (length(vals) == 1L) vals[[1L]] else NA_real_,
      available_time = if (length(vals) == 1L) min(available_time) else NA_real_,
      source_label = paste(sort(unique(nursingchartcelltypevallabel)), collapse = ";")
    )
  }, by = .(patientunitstayid, measurement_time)]
  explicit_reduced <- explicit_reduced[value_conflict == FALSE & !is.na(gcs_value)]
  explicit_reduced[, `:=`(
    gcs_source = paste0("explicit_total:", source_label),
    source_priority = 1L
  )]

  component_ranges <- data.table(
    mapping = c("gcs_eye", "gcs_verbal", "gcs_motor"),
    lower = c(1, 1, 1), upper = c(4, 5, 6)
  )
  components <- component_ranges[z, on = "mapping", nomatch = 0L]
  components[, valid_component :=
    !is.na(value_num) & abs(value_num - round(value_num)) < 1e-10 &
      value_num >= lower & value_num <= upper]
  components <- components[valid_component == TRUE]
  component_reduced <- components[, {
    vals <- unique(value_num)
    list(
      value_conflict = length(vals) > 1L,
      component_value = if (length(vals) == 1L) vals[[1L]] else NA_real_,
      component_available_time = if (length(vals) == 1L) {
        min(available_time)
      } else {
        NA_real_
      }
    )
  }, by = .(patientunitstayid, measurement_time, mapping)]
  component_reduced <- component_reduced[
    value_conflict == FALSE & !is.na(component_value)
  ]
  component_wide <- dcast(
    component_reduced,
    patientunitstayid + measurement_time ~ mapping,
    value.var = c("component_value", "component_available_time")
  )
  needed_component_cols <- paste0(
    "component_value_", c("gcs_eye", "gcs_verbal", "gcs_motor")
  )
  needed_available_cols <- paste0(
    "component_available_time_", c("gcs_eye", "gcs_verbal", "gcs_motor")
  )
  for (nm in c(needed_component_cols, needed_available_cols)) {
    if (!nm %in% names(component_wide)) component_wide[, (nm) := NA_real_]
  }
  component_wide[, complete_components :=
    !is.na(component_value_gcs_eye) &
      !is.na(component_value_gcs_verbal) &
      !is.na(component_value_gcs_motor)]
  reconstructed <- component_wide[complete_components == TRUE]
  reconstructed[, gcs_value :=
    component_value_gcs_eye + component_value_gcs_verbal +
      component_value_gcs_motor]
  reconstructed[, available_time := pmax(
    component_available_time_gcs_eye,
    component_available_time_gcs_verbal,
    component_available_time_gcs_motor
  )]
  reconstructed[, `:=`(
    gcs_source = "same_time_eye_verbal_motor_reconstruction",
    source_priority = 2L
  )]

  candidates <- rbindlist(list(
    explicit_reduced[, .(
      patientunitstayid, measurement_time, available_time,
      gcs_value, gcs_source, source_priority
    )],
    reconstructed[, .(
      patientunitstayid, measurement_time, available_time,
      gcs_value, gcs_source, source_priority
    )]
  ), use.names = TRUE, fill = TRUE)
  # Source priority is ordered before score: if any valid explicit total exists
  # in the window, component reconstruction is used only for QC, not selection.
  setorder(
    candidates, patientunitstayid, source_priority, gcs_value,
    measurement_time, available_time
  )
  selected <- candidates[, .SD[1L], by = patientunitstayid]
  setnames(
    selected,
    c("measurement_time", "available_time", "gcs_value"),
    c("gcs_time", "gcs_available_time", "gcs_worst")
  )
  selected[, source_priority := NULL]

  timing <- data.table(
    window_type = window_type,
    component = "gcs",
    candidate_rows = nrow(nurse[
      mapping %chin% c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor")
    ]),
    rows_measurement_in_window = sum(
      merge(
        nurse[mapping %chin% c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor")],
        bounds, by = "patientunitstayid"
      )[, measurement_time >= window_start & measurement_time <= window_end]
    ),
    rows_available_by_window_end = nrow(z[
      mapping %chin% c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor")
    ]),
    selected_patients = nrow(selected),
    explicit_selected = sum(grepl("^explicit_total", selected$gcs_source)),
    reconstructed_selected = sum(selected$gcs_source ==
      "same_time_eye_verbal_motor_reconstruction")
  )
  list(selected = selected, timing = timing)
}

derive_map <- function(bounds, window_type) {
  z <- merge(nurse, bounds, by = "patientunitstayid", all = FALSE, sort = FALSE)
  z <- z[mapping %chin% c("map_invasive", "map_noninvasive")]
  z[, in_measurement_window :=
      measurement_time >= window_start & measurement_time <= window_end]
  z[, available_by_window_end :=
      !is.na(available_time) & available_time <= window_end]
  z[, valid_map := !is.na(value_num) & value_num >= 1 & value_num <= 250]
  eligible <- z[in_measurement_window & available_by_window_end & valid_map]
  reduced <- eligible[, .(
    map_value = median(value_num),
    map_available_time = max(available_time),
    duplicate_rows = .N
  ), by = .(
    patientunitstayid, map_time = measurement_time,
    map_source = mapping,
    map_source_label = nursingchartcelltypevallabel,
    map_source_name = nursingchartcelltypevalname
  )]
  reduced[, source_rank := fifelse(map_source == "map_invasive", 1L, 2L)]
  # All sources can supply the minimum. Invasive wins an exact minimum tie.
  setorder(
    reduced, patientunitstayid, map_value, source_rank,
    map_time, map_available_time
  )
  selected <- reduced[, .SD[1L], by = patientunitstayid]
  setnames(selected, "map_value", "map_min")
  selected[, c("source_rank", "duplicate_rows") := NULL]

  timing <- data.table(
    window_type = window_type,
    component = "map",
    candidate_rows = nrow(z),
    rows_measurement_in_window = sum(z$in_measurement_window),
    rows_available_by_window_end = sum(
      z$in_measurement_window & z$available_by_window_end
    ),
    valid_rows_in_window = nrow(eligible),
    selected_patients = nrow(selected)
  )
  list(selected = selected, timing = timing)
}

# ---------------------------------------------------------------------------
# Laboratory values: exact eICU names and interface/system units. The last
# revision available by the relevant window end is used. A revision timestamp
# after prediction/index is not allowed to leak backward.
# ---------------------------------------------------------------------------

message("Reading CSV-aware eICU platelet/creatinine cache ...")
lab <- read_filtered_cache(
  cache_paths[["lab"]], "lab",
  c(
    "labid", "patientunitstayid", "labresultoffset", "labname",
    "labresult", "labmeasurenamesystem", "labmeasurenameinterface",
    "labresultrevisedoffset"
  ), cache_manifest
)
lab[, value_num := strict_numeric(labresult)]
lab[, measurement_time := as.numeric(labresultoffset)]
lab[, available_time := fifelse(
  is.na(labresultrevisedoffset),
  measurement_time,
  pmax(measurement_time, as.numeric(labresultrevisedoffset))
)]
lab[, unit_valid := fcase(
  labname == "creatinine", tolower(trimws(labmeasurenamesystem)) == "mg/dl",
  labname == "platelets x 1000", tolower(trimws(labmeasurenamesystem)) == "k/mcl",
  default = FALSE
)]
lab[is.na(unit_valid), unit_valid := FALSE]
lab[, value_valid := fcase(
  labname == "creatinine", !is.na(value_num) & value_num >= 0.1 & value_num <= 28.28,
  labname == "platelets x 1000", !is.na(value_num) & value_num > 0 & value_num <= 9999,
  default = FALSE
)]

lab_mapping_qc <- lab[, .(
  raw_rows = .N,
  strict_stays = uniqueN(patientunitstayid),
  numeric_rows = sum(!is.na(value_num)),
  unit_valid_rows = sum(unit_valid),
  value_valid_rows = sum(value_valid),
  revised_offset_missing_rows = sum(is.na(labresultrevisedoffset))
), by = .(labname, labmeasurenamesystem, labmeasurenameinterface)]
setorder(lab_mapping_qc, labname, -raw_rows)
fwrite(lab_mapping_qc, file.path(qc_out, "lab_mapping_unit_frequency.csv"))

derive_labs <- function(bounds, window_type) {
  z <- merge(lab, bounds, by = "patientunitstayid", all = FALSE, sort = FALSE)
  z[, in_measurement_window :=
      measurement_time >= window_start & measurement_time <= window_end]
  z[, available_by_window_end :=
      !is.na(available_time) & available_time <= window_end]
  eligible <- z[
    in_measurement_window & available_by_window_end & unit_valid & value_valid
  ]
  latest <- eligible[, .SD[available_time == max(available_time)],
                     by = .(patientunitstayid, labname, measurement_time)]
  reduced <- latest[, {
    vals <- unique(value_num)
    list(
      value_conflict = length(vals) > 1L,
      lab_value = if (length(vals) == 1L) vals[[1L]] else NA_real_,
      lab_available_time = if (length(vals) == 1L) {
        available_time[[1L]]
      } else {
        NA_real_
      }
    )
  }, by = .(patientunitstayid, labname, lab_time = measurement_time)]
  reduced <- reduced[value_conflict == FALSE & !is.na(lab_value)]

  platelet <- reduced[labname == "platelets x 1000"]
  setorder(platelet, patientunitstayid, lab_value, lab_time, lab_available_time)
  platelet <- platelet[, .SD[1L], by = patientunitstayid]
  setnames(
    platelet,
    c("lab_value", "lab_time", "lab_available_time"),
    c("platelet_min", "platelet_time", "platelet_available_time")
  )
  platelet <- platelet[, .(
    patientunitstayid, platelet_min, platelet_time, platelet_available_time
  )]

  creatinine <- reduced[labname == "creatinine"]
  setorder(
    creatinine, patientunitstayid, -lab_value,
    lab_time, lab_available_time
  )
  creatinine <- creatinine[, .SD[1L], by = patientunitstayid]
  setnames(
    creatinine,
    c("lab_value", "lab_time", "lab_available_time"),
    c("creatinine_max", "creatinine_time", "creatinine_available_time")
  )
  creatinine <- creatinine[, .(
    patientunitstayid, creatinine_max, creatinine_time,
    creatinine_available_time
  )]

  timing <- rbindlist(lapply(c("platelets x 1000", "creatinine"), function(nm) {
    zz <- z[labname == nm]
    data.table(
      window_type = window_type,
      component = if (nm == "platelets x 1000") "platelet" else "creatinine",
      candidate_rows = nrow(zz),
      rows_measurement_in_window = sum(zz$in_measurement_window),
      rows_available_by_window_end = sum(
        zz$in_measurement_window & zz$available_by_window_end
      ),
      valid_rows_in_window = nrow(eligible[labname == nm]),
      selected_patients = if (nm == "platelets x 1000") {
        nrow(platelet)
      } else {
        nrow(creatinine)
      }
    )
  }))
  list(platelet = platelet, creatinine = creatinine, timing = timing)
}

# ---------------------------------------------------------------------------
# Vasopressors/inotropes. Drug identities follow official eicu-code mappings.
# Primary active exposure requires either a positive infusionDrug drugrate or a
# non-cancelled, non-PRN, parenteral medication order whose active interval
# overlaps the window. Name-only infusion rows with missing rate are retained
# as a documented-only sensitivity flag rather than silently called active.
# ---------------------------------------------------------------------------

message("Reading CSV-aware eICU infusionDrug cache ...")
infusion <- read_filtered_cache(
  cache_paths[["infusionDrug"]], "infusionDrug",
  c(
    "infusiondrugid", "patientunitstayid", "infusionoffset",
    "drugname", "drugrate", "infusionrate", "drugamount",
    "volumeoffluid"
  ), cache_manifest
)
infusion[, drugname_normalized := tolower(trimws(drugname))]
infusion[, drug_class := fcase(
  grepl("norepinephrine|levophed|^nss with levo|^nss w/ levo/vaso", drugname_normalized),
  "norepinephrine",
  grepl("epineph|adrenalin|^epi \\(", drugname_normalized), "epinephrine",
  grepl("vasopressin", drugname_normalized), "vasopressin",
  grepl("dopamine|inotropin", drugname_normalized), "dopamine",
  grepl("dobu", drugname_normalized), "dobutamine",
  grepl(
    "phenylephrine|neo[- ]?synephrine|neosynsprine",
    drugname_normalized
  ), "phenylephrine",
  default = NA_character_
)]
infusion[, rate_num := strict_numeric(drugrate)]
infusion[, rate_status := fcase(
  !is.na(rate_num) & rate_num > 0, "positive",
  !is.na(rate_num) & rate_num == 0, "zero",
  !is.na(rate_num) & rate_num < 0, "negative",
  default = "missing_or_nonnumeric"
)]
infusion[, measurement_time := as.numeric(infusionoffset)]

infusion_mapping_qc <- infusion[!is.na(drug_class), .(
  raw_rows = .N,
  strict_stays = uniqueN(patientunitstayid),
  positive_rate_rows = sum(rate_status == "positive"),
  zero_rate_rows = sum(rate_status == "zero"),
  negative_rate_rows = sum(rate_status == "negative"),
  missing_or_nonnumeric_rate_rows = sum(rate_status == "missing_or_nonnumeric")
), by = .(drug_class, drugname)]
setorder(infusion_mapping_qc, drug_class, -raw_rows, drugname)
fwrite(
  infusion_mapping_qc,
  file.path(qc_out, "infusion_drug_mapping_frequency.csv")
)

message("Reading CSV-aware eICU medication cache ...")
medication <- read_filtered_cache(
  cache_paths[["medication"]], "medication",
  c(
    "medicationid", "patientunitstayid", "drugorderoffset",
    "drugstartoffset", "drugivadmixture", "drugordercancelled",
    "drugname", "drughiclseqno", "dosage", "routeadmin", "prn",
    "drugstopoffset"
  ), cache_manifest
)

hicl_map <- list(
  norepinephrine = c(37410L, 36346L, 2051L),
  epinephrine = c(37407L, 39089L, 36437L, 34361L, 2050L),
  dobutamine = c(8777L, 40L),
  dopamine = c(2060L, 2059L),
  vasopressin = c(38884L, 38883L, 2839L),
  phenylephrine = c(37028L, 35517L, 35587L, 2087L)
)
medication[, hicl_num := suppressWarnings(as.integer(drughiclseqno))]
medication[, drugname_normalized := tolower(trimws(drugname))]
medication[, drug_class := fcase(
  hicl_num %in% hicl_map$norepinephrine, "norepinephrine",
  hicl_num %in% hicl_map$epinephrine, "epinephrine",
  hicl_num %in% hicl_map$dobutamine, "dobutamine",
  hicl_num %in% hicl_map$dopamine, "dopamine",
  hicl_num %in% hicl_map$vasopressin, "vasopressin",
  hicl_num %in% hicl_map$phenylephrine, "phenylephrine",
  is.na(hicl_num) & grepl("norepinephrine|levophed", drugname_normalized),
  "norepinephrine",
  is.na(hicl_num) & grepl("^epinephrine|adrenalin", drugname_normalized),
  "epinephrine",
  is.na(hicl_num) & grepl("vasopressin", drugname_normalized), "vasopressin",
  is.na(hicl_num) & grepl("dopamine|inotropin", drugname_normalized), "dopamine",
  is.na(hicl_num) & grepl("dobutamine|dobutrex", drugname_normalized), "dobutamine",
  is.na(hicl_num) & grepl(
    "phenylephrine|neo[- ]?synephrine|neosynsprine",
    drugname_normalized
  ), "phenylephrine",
  default = NA_character_
)]
medication[, start_time_raw := as.numeric(drugstartoffset)]
medication[, stop_time_raw := as.numeric(drugstopoffset)]
medication[, order_time_raw := as.numeric(drugorderoffset)]
medication[, start_offset_zero_raw := !is.na(start_time_raw) & start_time_raw == 0]
medication[, stop_offset_zero_raw := !is.na(stop_time_raw) & stop_time_raw == 0]
medication[, order_offset_zero_raw := !is.na(order_time_raw) & order_time_raw == 0]
# Official eicu-code pivoted-med treats zero offsets as ETL missingness.
medication[, start_time := fifelse(start_offset_zero_raw, NA_real_, start_time_raw)]
medication[, stop_time := fifelse(stop_offset_zero_raw, NA_real_, stop_time_raw)]
medication[, order_time := fifelse(order_offset_zero_raw, NA_real_, order_time_raw)]
medication[, order_available_time := fifelse(
  is.na(order_time), start_time, pmax(order_time, start_time, na.rm = TRUE)
)]
medication[, order_available_time_zero_retained := fifelse(
  is.na(order_time_raw),
  start_time_raw,
  pmax(order_time_raw, start_time_raw, na.rm = TRUE)
)]
medication[, route_normalized := toupper(trimws(routeadmin))]
medication[, parenteral_route :=
  drugivadmixture == "Yes" |
    route_normalized %chin% c("IV", "IVPB", "CENTRAL IV", "ZPYXVEND") |
    grepl("^INTRAVEN", route_normalized)]
medication[, noncancelled := drugordercancelled == "No"]
medication[, nonprn := is.na(prn) | prn != "Yes"]
medication[, dosage_present := !is.na(dosage) & nzchar(trimws(as.character(dosage)))]
medication[, interval_valid :=
  !is.na(start_time) & (is.na(stop_time) | stop_time >= start_time)]
medication[, interval_valid_zero_retained :=
  !is.na(start_time_raw) &
    (is.na(stop_time_raw) | stop_time_raw >= start_time_raw)]
medication[is.na(parenteral_route), parenteral_route := FALSE]
medication[is.na(noncancelled), noncancelled := FALSE]
medication[is.na(nonprn), nonprn := FALSE]
medication[is.na(dosage_present), dosage_present := FALSE]
medication[is.na(interval_valid), interval_valid := FALSE]
medication[is.na(interval_valid_zero_retained), interval_valid_zero_retained := FALSE]

medication_mapping_qc <- medication[!is.na(drug_class), .(
  raw_rows = .N,
  strict_stays = uniqueN(patientunitstayid),
  noncancelled_rows = sum(noncancelled),
  parenteral_rows = sum(parenteral_route),
  interval_valid_rows = sum(interval_valid),
  interval_valid_zero_retained_rows = sum(interval_valid_zero_retained),
  start_offset_zero_raw_rows = sum(start_offset_zero_raw),
  stop_offset_zero_raw_rows = sum(stop_offset_zero_raw),
  order_offset_zero_raw_rows = sum(order_offset_zero_raw),
  nonprn_rows = sum(nonprn)
), by = .(
  drug_class, drugname, hicl_num, drugivadmixture,
  drugordercancelled, routeadmin, prn
)]
setorder(medication_mapping_qc, drug_class, -raw_rows, drugname)
fwrite(
  medication_mapping_qc,
  file.path(qc_out, "medication_drug_mapping_frequency.csv")
)

derive_pressor <- function(bounds, window_type) {
  inf <- merge(
    infusion[!is.na(drug_class)], bounds,
    by = "patientunitstayid", all = FALSE, sort = FALSE
  )
  inf[, in_window :=
      measurement_time >= window_start & measurement_time <= window_end]
  inf_window <- inf[in_window == TRUE]
  inf_patient <- inf_window[, .(
    pressor_positive_infusion = any(rate_status == "positive"),
    pressor_infusion_missing_rate_documented = any(
      rate_status == "missing_or_nonnumeric"
    ),
    pressor_infusion_zero_only =
      !any(rate_status == "positive") & any(rate_status == "zero"),
    pressor_classes_infusion = paste(
      sort(unique(drug_class[rate_status == "positive"])), collapse = ";"
    )
  ), by = patientunitstayid]

  med <- merge(
    medication[!is.na(drug_class)], bounds,
    by = "patientunitstayid", all = FALSE, sort = FALSE
  )
  med[, interval_overlaps_window :=
    start_time <= window_end & (is.na(stop_time) | stop_time >= window_start)]
  med[, available_by_window_end :=
      !is.na(order_available_time) & order_available_time <= window_end]
  med[, active_order :=
    noncancelled & nonprn & dosage_present & parenteral_route & interval_valid &
      interval_overlaps_window & available_by_window_end]
  med[is.na(active_order), active_order := FALSE]
  med[, interval_overlaps_window_zero_retained :=
    start_time_raw <= window_end &
      (is.na(stop_time_raw) | stop_time_raw >= window_start)]
  med[, available_by_window_end_zero_retained :=
    !is.na(order_available_time_zero_retained) &
      order_available_time_zero_retained <= window_end]
  med[, active_order_zero_retained_sensitivity :=
    noncancelled & nonprn & dosage_present & parenteral_route &
      interval_valid_zero_retained & interval_overlaps_window_zero_retained &
      available_by_window_end_zero_retained]
  med[is.na(active_order_zero_retained_sensitivity),
      active_order_zero_retained_sensitivity := FALSE]
  med_patient <- med[, .(
    pressor_active_medication_order = any(active_order),
    pressor_active_medication_order_zero_retained_sensitivity = any(
      active_order_zero_retained_sensitivity
    ),
    pressor_classes_medication = paste(
      sort(unique(drug_class[active_order == TRUE])), collapse = ";"
    ),
    pressor_classes_medication_zero_retained_sensitivity = paste(
      sort(unique(drug_class[active_order_zero_retained_sensitivity == TRUE])),
      collapse = ";"
    )
  ), by = patientunitstayid]

  out <- merge(
    bounds[, .(patientunitstayid)], inf_patient,
    by = "patientunitstayid", all.x = TRUE, sort = FALSE
  )
  out <- merge(out, med_patient, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  logical_fields <- c(
    "pressor_positive_infusion", "pressor_infusion_missing_rate_documented",
    "pressor_infusion_zero_only", "pressor_active_medication_order",
    "pressor_active_medication_order_zero_retained_sensitivity"
  )
  for (v in logical_fields) set(out, which(is.na(out[[v]])), v, FALSE)
  for (v in c(
    "pressor_classes_infusion", "pressor_classes_medication",
    "pressor_classes_medication_zero_retained_sensitivity"
  )) {
    set(out, which(is.na(out[[v]])), v, "")
  }
  out[, vasopressor_any :=
    pressor_positive_infusion | pressor_active_medication_order]
  out[, vasopressor_documented_sensitivity :=
    vasopressor_any | pressor_infusion_missing_rate_documented]
  out[, vasopressor_zero_offset_retained_sensitivity :=
    pressor_positive_infusion |
      pressor_active_medication_order_zero_retained_sensitivity]
  out[, vasopressor_source := fcase(
    pressor_positive_infusion & pressor_active_medication_order, "both",
    pressor_positive_infusion, "positive_infusion",
    pressor_active_medication_order, "active_medication_order",
    pressor_infusion_missing_rate_documented, "missing_rate_infusion_only",
    pressor_infusion_zero_only, "zero_rate_infusion_only",
    default = "none"
  )]

  timing <- data.table(
    window_type = window_type,
    component = "vasopressor",
    candidate_infusion_rows = nrow(inf),
    infusion_rows_in_window = nrow(inf_window),
    positive_infusion_rows_in_window = sum(inf_window$rate_status == "positive"),
    missing_rate_infusion_rows_in_window = sum(
      inf_window$rate_status == "missing_or_nonnumeric"
    ),
    candidate_medication_rows = nrow(med),
    active_medication_order_rows = sum(med$active_order),
    active_medication_order_zero_retained_rows = sum(
      med$active_order_zero_retained_sensitivity
    ),
    medication_start_offset_zero_raw_rows = sum(med$start_offset_zero_raw),
    medication_stop_offset_zero_raw_rows = sum(med$stop_offset_zero_raw),
    medication_order_offset_zero_raw_rows = sum(med$order_offset_zero_raw),
    selected_patients = sum(out$vasopressor_any),
    documented_sensitivity_patients = sum(out$vasopressor_documented_sensitivity),
    zero_offset_retained_sensitivity_patients = sum(
      out$vasopressor_zero_offset_retained_sensitivity
    )
  )
  list(selected = out, timing = timing)
}

# ---------------------------------------------------------------------------
# Build each HSC from the same raw, outcome-blind mappings.
# ---------------------------------------------------------------------------

build_core <- function(window_type) {
  bounds <- make_bounds(window_type)
  gcs <- derive_gcs(bounds, window_type)
  map <- derive_map(bounds, window_type)
  labs <- derive_labs(bounds, window_type)
  pressor <- derive_pressor(bounds, window_type)

  out <- copy(bounds)
  out <- merge(out, gcs$selected, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  out <- merge(out, map$selected, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  out <- merge(out, pressor$selected, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  out <- merge(out, labs$platelet, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  out <- merge(out, labs$creatinine, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  out <- merge(
    out,
    patient_age[, .(
      patientunitstayid, age_num_harmonized, age_topcoded_gt89,
      age_topcode_sensitivity_exclude, height_raw, height_cm, height_valid,
      sex_binary_for_pbw, pbw_kg
    )],
    by = "patientunitstayid", all.x = TRUE, sort = FALSE
  )
  if (nrow(out) != nrow(bounds) || anyDuplicated(out$patientunitstayid)) {
    stop("HSC is not one row per intended patient: ", window_type)
  }

  timing <- rbindlist(
    list(gcs$timing, map$timing, labs$timing, pressor$timing),
    fill = TRUE, use.names = TRUE
  )
  list(core = out, timing = timing)
}

prediction_result <- build_core("prediction_time_hsc")
index_result <- build_core("index_known_selection")
prediction_core <- prediction_result$core
index_core <- index_result$core

# ---------------------------------------------------------------------------
# Invariants: temporal availability, ranges, and no post-window predictors.
# ---------------------------------------------------------------------------

check_core <- function(x, window_type) {
  checks <- list(
    one_row_per_patient = !anyDuplicated(x$patientunitstayid),
    nonnegative_window_start = all(x$window_start >= 0),
    ordered_window = all(x$window_start <= x$window_end),
    gcs_range = all(is.na(x$gcs_worst) | (x$gcs_worst >= 3 & x$gcs_worst <= 15)),
    map_range = all(is.na(x$map_min) | (x$map_min >= 1 & x$map_min <= 250)),
    platelet_range = all(is.na(x$platelet_min) | (x$platelet_min > 0 & x$platelet_min <= 9999)),
    creatinine_range = all(is.na(x$creatinine_max) | (x$creatinine_max >= 0.1 & x$creatinine_max <= 28.28)),
    gcs_measurement_in_window = all(
      is.na(x$gcs_time) | (x$gcs_time >= x$window_start & x$gcs_time <= x$window_end)
    ),
    gcs_available_by_end = all(is.na(x$gcs_available_time) | x$gcs_available_time <= x$window_end),
    map_measurement_in_window = all(
      is.na(x$map_time) | (x$map_time >= x$window_start & x$map_time <= x$window_end)
    ),
    map_available_by_end = all(is.na(x$map_available_time) | x$map_available_time <= x$window_end),
    platelet_measurement_in_window = all(
      is.na(x$platelet_time) |
        (x$platelet_time >= x$window_start & x$platelet_time <= x$window_end)
    ),
    platelet_available_by_end = all(
      is.na(x$platelet_available_time) | x$platelet_available_time <= x$window_end
    ),
    creatinine_measurement_in_window = all(
      is.na(x$creatinine_time) |
        (x$creatinine_time >= x$window_start & x$creatinine_time <= x$window_end)
    ),
    creatinine_available_by_end = all(
      is.na(x$creatinine_available_time) | x$creatinine_available_time <= x$window_end
    ),
    no_outcome_like_columns = !any(grepl(forbidden_pattern, names(x), ignore.case = TRUE))
  )
  data.table(
    window_type = window_type,
    check = names(checks),
    pass = unlist(checks, use.names = FALSE)
  )
}

invariants <- rbindlist(list(
  check_core(prediction_core, "prediction_time_hsc"),
  check_core(index_core, "index_known_selection"),
  data.table(
    window_type = "csv_aware_input_cache",
    check = c(
      "four_raw_streams_reached_logical_eof",
      "r_rows_match_cache_manifest",
      "gcs_total_strict_stay_coverage_not_truncated",
      "creatinine_strict_stay_coverage_not_truncated",
      "platelet_strict_stay_coverage_not_truncated"
    ),
    pass = c(
      nrow(cache_manifest) == 4L & all(cache_manifest$reached_eof == TRUE),
      nrow(nurse) == cache_manifest[source_name == "nurseCharting", kept_rows] &
        nrow(lab) == cache_manifest[source_name == "lab", kept_rows] &
        nrow(infusion) == cache_manifest[source_name == "infusionDrug", kept_rows] &
        nrow(medication) == cache_manifest[source_name == "medication", kept_rows],
      uniqueN(nurse[mapping == "gcs_total"]$patientunitstayid) >= 1800L,
      uniqueN(lab[labname == "creatinine"]$patientunitstayid) >= 1800L,
      uniqueN(lab[labname == "platelets x 1000"]$patientunitstayid) >= 1200L
    )
  )
))
if (any(!invariants$pass)) {
  stop(
    "Severity-core invariant failure(s): ",
    paste(invariants[pass == FALSE, paste(window_type, check, sep = ":")], collapse = ", ")
  )
}
fwrite(invariants, file.path(qc_out, "severity_core_invariant_tests.csv"))

# ---------------------------------------------------------------------------
# Private row-level artifacts. Prediction-time and index-known HSCs remain
# explicitly separate so no post-index value can enter the selection model.
# ---------------------------------------------------------------------------

prediction_attached <- merge(
  tuple_base, prediction_core,
  by = "patientunitstayid", all.x = TRUE, sort = FALSE
)
prediction_attached[, vt_per_pbw_mL_per_kg := fifelse(
  !is.na(pbw_kg), vt_value / pbw_kg, NA_real_
)]
prediction_attached[, smp_per_pbw_J_per_min_per_kg := fifelse(
  !is.na(pbw_kg), smp / pbw_kg, NA_real_
)]
index_attached <- merge(
  strict_base, index_core,
  by = "patientunitstayid", all.x = TRUE, sort = FALSE
)

normalization_invariants <- data.table(
  window_type = "prediction_time_hsc",
  check = c(
    "pbw_requires_valid_height_and_binary_sex",
    "vt_per_pbw_formula",
    "smp_per_pbw_formula"
  ),
  pass = c(
    all(
      is.na(prediction_attached$pbw_kg) |
        (prediction_attached$height_cm >= 120 &
           prediction_attached$height_cm <= 230 &
           prediction_attached$sex_binary_for_pbw %chin% c("male", "female"))
    ),
    all(
      is.na(prediction_attached$vt_per_pbw_mL_per_kg) |
        abs(
          prediction_attached$vt_per_pbw_mL_per_kg -
            prediction_attached$vt_value / prediction_attached$pbw_kg
        ) < 1e-10
    ),
    all(
      is.na(prediction_attached$smp_per_pbw_J_per_min_per_kg) |
        abs(
          prediction_attached$smp_per_pbw_J_per_min_per_kg -
            prediction_attached$smp / prediction_attached$pbw_kg
        ) < 1e-10
    )
  )
)
invariants <- rbindlist(list(invariants, normalization_invariants), fill = TRUE)
if (any(!invariants$pass)) stop("PBW/normalization invariant failed.")
fwrite(invariants, file.path(qc_out, "severity_core_invariant_tests.csv"))

if (any(grepl(forbidden_pattern, names(prediction_attached), ignore.case = TRUE)) ||
    any(grepl(forbidden_pattern, names(index_attached), ignore.case = TRUE))) {
  stop("Outcome-like field entered a private HSC artifact.")
}

common_metadata <- list(
  version = "eicu_harmonized_severity_core_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED$version,
  script = script_path,
  outcome_blind = TRUE,
  official_mapping_provenance = paste0(
    "MIT-LCP/eicu-code commit 34cece8c70771a3fab48da84d4c47f0e133ca021; ",
    "pivoted-gcs.sql, pivoted-vital.sql, pivoted-lab.sql, ",
    "pivoted-infusion.sql, pivoted-med.sql"
  ),
  apache_apsvar_used_for_hsc = FALSE
)
attr(prediction_attached, "rebuild_metadata") <- c(
  common_metadata,
  list(
    artifact = "eicu_paired_exposure_with_severity_core_v1.rds",
    window = "max(ICU offset 0, index-1440 min) through tuple prediction_time"
  )
)
attr(index_attached, "rebuild_metadata") <- c(
  common_metadata,
  list(
    artifact = "eicu_index_known_selection_core_v1.rds",
    window = "max(ICU offset 0, index-1440 min) through index",
    intended_use = "complete-tuple observation model/IPW only"
  )
)

prediction_rds <- file.path(
  private_out, "eicu_paired_exposure_with_severity_core_v1.rds"
)
selection_rds <- file.path(
  private_out, "eicu_index_known_selection_core_v1.rds"
)
apache_rds <- file.path(
  private_out, "eicu_native_apache_iva_benchmark_v1.rds"
)
saveRDS(prediction_attached, prediction_rds, compress = "xz")
saveRDS(index_attached, selection_rds, compress = "xz")

# ---------------------------------------------------------------------------
# APACHE IVa native benchmark: separate artifact, no actual-outcome fields.
# Negative sentinel values are set missing and counted. This artifact never
# supplies a time-aligned S0 variable.
# ---------------------------------------------------------------------------

apache <- fread(
  cmd = sprintf("gzip -cd %s", shQuote(raw_apache)),
  select = c(
    "patientunitstayid", "acutephysiologyscore", "apachescore",
    "apacheversion", "predictedhospitalmortality"
  ),
  showProgress = FALSE
)
apache <- apache[
  apacheversion == "IVa" & patientunitstayid %in% strict_base$patientunitstayid
]
if (anyDuplicated(apache$patientunitstayid)) {
  stop("More than one APACHE IVa row exists for a strict-cohort stay.")
}
setnames(
  apache,
  c("acutephysiologyscore", "apachescore", "predictedhospitalmortality"),
  c("apache_iva_acute_physiology_score", "apache_iva_score", "apache_predicted_hospital_risk")
)
apache[, apache_score_negative_sentinel := apache_iva_score < 0]
apache[, apache_risk_negative_sentinel := apache_predicted_hospital_risk < 0]
apache[apache_iva_score < 0, apache_iva_score := NA_real_]
apache[apache_iva_acute_physiology_score < 0, apache_iva_acute_physiology_score := NA_real_]
apache[apache_predicted_hospital_risk < 0, apache_predicted_hospital_risk := NA_real_]
apache[apache_predicted_hospital_risk > 1, apache_predicted_hospital_risk := NA_real_]

apache_artifact <- merge(
  strict_base[, .(patientunitstayid, tuple_observed)],
  apache[, .(
    patientunitstayid, apacheversion,
    apache_iva_acute_physiology_score, apache_iva_score,
    apache_predicted_hospital_risk,
    apache_score_negative_sentinel, apache_risk_negative_sentinel
  )],
  by = "patientunitstayid", all.x = TRUE, sort = FALSE
)
if (any(grepl("actual|death|expire|discharge|outcome|surviv", names(apache_artifact),
              ignore.case = TRUE))) {
  stop("Actual-outcome field entered the APACHE benchmark artifact.")
}
attr(apache_artifact, "rebuild_metadata") <- list(
  version = "eicu_native_apache_iva_benchmark_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED$version,
  script = script_path,
  outcome_blind = TRUE,
  role = "separate database-native benchmark; never an S0 HSC source",
  apache_apsvar_used_for_hsc = FALSE
)
saveRDS(
  apache_artifact,
  apache_rds,
  compress = "xz"
)

# ---------------------------------------------------------------------------
# Aggregate-only availability, timing, source, unit, top-code, and value QC.
# ---------------------------------------------------------------------------

missingness_qc <- rbindlist(lapply(list(
  prediction_time_hsc = prediction_core,
  index_known_selection = index_core
), function(x) {
  rbindlist(lapply(
    c("gcs_worst", "map_min", "vasopressor_any", "platelet_min", "creatinine_max"),
    function(v) data.table(
      denominator = nrow(x),
      variable = v,
      available_n = sum(!is.na(x[[v]])),
      missing_n = sum(is.na(x[[v]])),
      available_proportion = mean(!is.na(x[[v]]))
    )
  ))
}), idcol = "window_type")
fwrite(missingness_qc, file.path(qc_out, "severity_core_missingness.csv"))

pressor_coverage_qc <- rbindlist(lapply(list(
  prediction_time_hsc = prediction_core,
  index_known_selection = index_core
), function(x) {
  data.table(
    denominator = nrow(x),
    positive_infusion_n = sum(x$pressor_positive_infusion),
    active_medication_order_n = sum(x$pressor_active_medication_order),
    both_n = sum(
      x$pressor_positive_infusion & x$pressor_active_medication_order
    ),
    positive_infusion_only_n = sum(
      x$pressor_positive_infusion & !x$pressor_active_medication_order
    ),
    active_medication_order_only_n = sum(
      !x$pressor_positive_infusion & x$pressor_active_medication_order
    ),
    primary_union_n = sum(x$vasopressor_any),
    missing_rate_infusion_documented_n = sum(
      x$pressor_infusion_missing_rate_documented
    ),
    documented_sensitivity_union_n = sum(
      x$vasopressor_documented_sensitivity
    ),
    active_medication_order_zero_retained_n = sum(
      x$pressor_active_medication_order_zero_retained_sensitivity
    ),
    zero_offset_retained_union_n = sum(
      x$vasopressor_zero_offset_retained_sensitivity
    )
  )
}), idcol = "window_type")
fwrite(
  pressor_coverage_qc,
  file.path(qc_out, "vasopressor_source_overlap_QC.csv")
)

value_qc <- rbindlist(lapply(names(list(
  prediction_time_hsc = prediction_core,
  index_known_selection = index_core
)), function(nm) {
  x <- list(
    prediction_time_hsc = prediction_core,
    index_known_selection = index_core
  )[[nm]]
  rbindlist(list(
    distribution_row(nm, "gcs_worst", x$gcs_worst),
    distribution_row(nm, "map_min", x$map_min),
    distribution_row(nm, "platelet_min", x$platelet_min),
    distribution_row(nm, "creatinine_max", x$creatinine_max)
  ))
}))
value_qc <- rbindlist(list(
  value_qc,
  distribution_row(
    "prediction_time_hsc", "pbw_kg", prediction_attached$pbw_kg
  ),
  distribution_row(
    "prediction_time_hsc", "vt_per_pbw_mL_per_kg",
    prediction_attached$vt_per_pbw_mL_per_kg
  ),
  distribution_row(
    "prediction_time_hsc", "smp_per_pbw_J_per_min_per_kg",
    prediction_attached$smp_per_pbw_J_per_min_per_kg
  )
), fill = TRUE)
fwrite(value_qc, file.path(qc_out, "severity_core_value_distribution.csv"))

timing_qc <- rbindlist(list(prediction_result$timing, index_result$timing), fill = TRUE)
fwrite(timing_qc, file.path(qc_out, "severity_core_timing_QC.csv"))

source_counts <- function(x) {
  g <- x[, .(source = fifelse(is.na(gcs_source), "missing", gcs_source))]
  g <- g[, .N, by = source]
  g[, component := "gcs"]
  m <- x[, .(source = fifelse(is.na(map_source), "missing", map_source))]
  m <- m[, .N, by = source]
  m[, component := "map"]
  v <- x[, .(source = vasopressor_source)]
  v <- v[, .N, by = source]
  v[, component := "vasopressor"]
  rbindlist(list(g, m, v), use.names = TRUE, fill = TRUE)
}
source_qc <- rbindlist(lapply(list(
  prediction_time_hsc = prediction_core,
  index_known_selection = index_core
), source_counts), idcol = "window_type")
source_qc[, denominator := ifelse(
  window_type == "prediction_time_hsc", nrow(prediction_core), nrow(index_core)
)]
source_qc[, proportion := N / denominator]
fwrite(source_qc, file.path(qc_out, "severity_core_source_frequency.csv"))

age_topcode_qc <- rbindlist(list(
  data.table(
    population = "strict_index_cohort",
    denominator = nrow(strict_base),
    topcoded_gt89_n = sum(patient_age$age_topcoded_gt89),
    mapped_to_90_n = sum(patient_age$age_num_harmonized == 90 & patient_age$age_topcoded_gt89)
  ),
  data.table(
    population = "complete_tuple_prediction_hsc",
    denominator = nrow(tuple_base),
    topcoded_gt89_n = sum(
      patient_age[patientunitstayid %in% tuple_base$patientunitstayid]$age_topcoded_gt89
    ),
    mapped_to_90_n = sum(
      patient_age[
        patientunitstayid %in% tuple_base$patientunitstayid
      ]$age_num_harmonized == 90 &
        patient_age[
          patientunitstayid %in% tuple_base$patientunitstayid
        ]$age_topcoded_gt89
    )
  )
))
fwrite(age_topcode_qc, file.path(qc_out, "age_topcode_QC.csv"))

height_pbw_qc <- rbindlist(lapply(list(
  strict_index_cohort = patient_age,
  complete_tuple_prediction_hsc = patient_age[
    patientunitstayid %in% tuple_base$patientunitstayid
  ]
), function(x) {
  hq <- quantile_safe(x$height_raw)
  pq <- quantile_safe(x$pbw_kg)
  data.table(
    denominator = nrow(x),
    source_field = "patient.admissionheight",
    source_unit = "cm (eICU data dictionary; no conversion)",
    raw_height_available_n = sum(!is.na(x$height_raw)),
    below_120_n = sum(x$height_raw < 120, na.rm = TRUE),
    valid_120_230_n = sum(x$height_valid),
    above_230_n = sum(x$height_raw > 230, na.rm = TRUE),
    binary_sex_n = sum(!is.na(x$sex_binary_for_pbw)),
    pbw_available_n = sum(!is.na(x$pbw_kg)),
    height_raw_min = hq[1L], height_raw_median = hq[4L],
    height_raw_max = hq[7L],
    pbw_min = pq[1L], pbw_median = pq[4L], pbw_max = pq[7L]
  )
}), idcol = "population")
fwrite(height_pbw_qc, file.path(qc_out, "height_pbw_QC.csv"))

apache_qc <- data.table(
  population = c("strict_index_cohort", "complete_tuple_population"),
  denominator = c(nrow(strict_base), nrow(tuple_base)),
  apache_iva_row_available_n = c(
    sum(!is.na(apache_artifact$apacheversion)),
    apache_artifact[tuple_observed == TRUE, sum(!is.na(apacheversion))]
  ),
  apache_iva_score_available_n = c(
    sum(!is.na(apache_artifact$apache_iva_score)),
    apache_artifact[tuple_observed == TRUE, sum(!is.na(apache_iva_score))]
  ),
  apache_predicted_risk_available_n = c(
    sum(!is.na(apache_artifact$apache_predicted_hospital_risk)),
    apache_artifact[tuple_observed == TRUE, sum(!is.na(apache_predicted_hospital_risk))]
  ),
  apache_negative_score_sentinel_n = c(
    sum(apache_artifact$apache_score_negative_sentinel, na.rm = TRUE),
    apache_artifact[tuple_observed == TRUE, sum(apache_score_negative_sentinel, na.rm = TRUE)]
  ),
  apache_negative_risk_sentinel_n = c(
    sum(apache_artifact$apache_risk_negative_sentinel, na.rm = TRUE),
    apache_artifact[tuple_observed == TRUE, sum(apache_risk_negative_sentinel, na.rm = TRUE)]
  )
)
fwrite(apache_qc, file.path(qc_out, "apache_iva_native_benchmark_QC.csv"))

# Final aggregate-output leakage guard. Identifiers are forbidden in QC
# headers; predicted APACHE risk is permitted, actual outcomes are not.
qc_csv <- list.files(qc_out, pattern = "\\.csv$", full.names = TRUE)
qc_headers <- rbindlist(lapply(qc_csv, function(f) {
  data.table(file = basename(f), column = names(fread(f, nrows = 0L)))
}))
identifier_headers <- c(
  "patientunitstayid", "patienthealthsystemstayid", "person_key", "uniquepid",
  "hospitalid", "nursingchartid", "labid", "infusiondrugid", "medicationid"
)
leakage_guard <- data.table(
  check = c(
    "paired_source_has_no_outcome_like_columns",
    "prediction_hsc_has_no_outcome_like_columns",
    "index_selection_core_has_no_outcome_like_columns",
    "apache_artifact_has_no_actual_outcome_columns",
    "aggregate_qc_has_no_identifier_columns",
    "apacheApsVar_not_used_for_hsc"
  ),
  pass = c(
    !any(grepl(forbidden_pattern, names(exposure_source), ignore.case = TRUE)),
    !any(grepl(forbidden_pattern, names(prediction_attached), ignore.case = TRUE)),
    !any(grepl(forbidden_pattern, names(index_attached), ignore.case = TRUE)),
    !any(grepl("actual|death|expire|discharge|outcome|surviv", names(apache_artifact), ignore.case = TRUE)),
    !any(qc_headers$column %chin% identifier_headers),
    TRUE
  )
)
if (any(!leakage_guard$pass)) stop("Final severity leakage guard failed.")
fwrite(leakage_guard, file.path(qc_out, "outcome_leakage_guard.csv"))

summary_lines <- c(
  "# eICU harmonized pre-prediction severity-core QC",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("- Locked configuration: ", LOCKED$version),
  "- CSV-aware raw-input scans reached logical EOF: 4/4",
  paste0("  - nurseCharting logical rows scanned/kept: ",
         cache_manifest[source_name == "nurseCharting", scanned_rows], "/",
         cache_manifest[source_name == "nurseCharting", kept_rows]),
  paste0("  - lab logical rows scanned/kept: ",
         cache_manifest[source_name == "lab", scanned_rows], "/",
         cache_manifest[source_name == "lab", kept_rows]),
  paste0("  - infusionDrug logical rows scanned/kept: ",
         cache_manifest[source_name == "infusionDrug", scanned_rows], "/",
         cache_manifest[source_name == "infusionDrug", kept_rows]),
  paste0("  - medication logical rows scanned/kept: ",
         cache_manifest[source_name == "medication", scanned_rows], "/",
         cache_manifest[source_name == "medication", kept_rows]),
  paste0("- Outcome-blind invariant checks passed: ",
         sum(invariants$pass), "/", nrow(invariants)),
  paste0("- Strict index cohort / index-known selection core: ", nrow(index_core)),
  paste0("- Complete-tuple prediction-time HSC: ", nrow(prediction_core)),
  paste0("- GCS available at prediction time: ", sum(!is.na(prediction_core$gcs_worst))),
  paste0("- MAP available at prediction time: ", sum(!is.na(prediction_core$map_min))),
  paste0("- Active vasopressor/inotrope exposure at prediction time: ", sum(prediction_core$vasopressor_any)),
  paste0("  - positive infusion: ", sum(prediction_core$pressor_positive_infusion)),
  paste0("  - active medication order: ", sum(prediction_core$pressor_active_medication_order)),
  paste0("  - both sources: ", sum(
    prediction_core$pressor_positive_infusion &
      prediction_core$pressor_active_medication_order
  )),
  paste0("  - medication-order only: ", sum(
    !prediction_core$pressor_positive_infusion &
      prediction_core$pressor_active_medication_order
  )),
  paste0("  - zero-offset-retained medication sensitivity: ", sum(
    prediction_core$pressor_active_medication_order_zero_retained_sensitivity
  )),
  paste0("- Platelet available at prediction time: ", sum(!is.na(prediction_core$platelet_min))),
  paste0("- Creatinine available at prediction time: ", sum(!is.na(prediction_core$creatinine_max))),
  paste0("- Valid admission height/PBW at prediction time: ", sum(
    !is.na(prediction_attached$pbw_kg)
  )),
  paste0("- eICU age >89 top-code mapped to 90 (strict cohort): ", sum(patient_age$age_topcoded_gt89)),
  "- GCS preference: valid explicit total first; same-time eye+verbal+motor reconstruction only when every component is a unique valid numeric value.",
  "- Intubation, medication, and other text was never normalized into a GCS value.",
  "- MAP sources: invasive BP mean, arterial-line MAP, generic MAP (official invasive mapping), and non-invasive BP mean; invasive wins an exact minimum tie.",
  "- Lab revisions and nurse entry times after the relevant window end were excluded.",
  "- Primary medication mapping follows official eicu-code and treats order/start/stop offset 0 as ETL missingness; raw-zero counts and a zero-retained sensitivity feasibility flag are preserved.",
  "- Primary pressor flag covers norepinephrine, epinephrine, vasopressin, dopamine, dobutamine, and phenylephrine, and requires positive infusion rate or an active non-cancelled parenteral medication order; missing-rate infusion documentation is retained only as a sensitivity flag.",
  "- APACHE IVa score/risk is stored separately and apacheApsVar admission-day values were not used for HSC variables.",
  "- No mortality, discharge, survival, effect estimate, or model-performance result was read or summarized.",
  "- Row-level outputs are confined to analysis_rebuild_v1/private/eicu.",
  "",
  "Mapping provenance: official MIT-LCP/eicu-code commit 34cece8c70771a3fab48da84d4c47f0e133ca021 plus local label, unit, frequency, and timing audit.",
  "",
  "BUILD_COMPLETE"
)
writeLines(
  summary_lines,
  file.path(qc_out, "eicu_severity_core_QC.md"),
  useBytes = TRUE
)

# Atomic completion gate. Downstream scripts must require this exact PASS file;
# private RDS files from an interrupted run are otherwise considered invalid.
completion <- data.table(
  status = "PASS",
  config_version = LOCKED$version,
  completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  script_sha256 = sha256_file(script_path),
  phase1_gate_sha256 = sha256_file(phase1_gate_path),
  phase2_gate_sha256 = sha256_file(phase2_gate_path),
  input_exposure_rds_sha256 = input_exposure_sha256,
  filter_helper_sha256 = sha256_file(filter_helper),
  input_cache_gate_sha256 = sha256_file(cache_gate_path),
  input_cache_manifest_sha256 = sha256_file(cache_manifest_path),
  input_cache_strict_ids_sha256 = cache_gate$strict_ids_sha256[[1L]],
  prediction_hsc_rds_sha256 = sha256_file(prediction_rds),
  index_selection_rds_sha256 = sha256_file(selection_rds),
  apache_benchmark_rds_sha256 = sha256_file(apache_rds)
)
fwrite(completion, completion_gate_tmp)
if (!file.rename(completion_gate_tmp, completion_gate)) {
  stop("Could not atomically publish the Phase 2b completion gate.")
}

message("eICU harmonized severity build complete (outcome-blind).")
message("  prediction-time HSC: ", nrow(prediction_core))
message("  index-known selection core: ", nrow(index_core))
message("  private outputs: ", private_out)
message("  aggregate QC: ", qc_out)
