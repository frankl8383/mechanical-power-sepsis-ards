#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: MIMIC-IV harmonized severity core
#
# Outcome-blind Phase 2b extraction. Two row-level products are kept strictly
# separate:
#   A) prediction-time HSC for patients with a valid 0-6 h tuple; and
#   B) index-known HSC for every strict-cohort patient, for the tuple-
#      observation selection model only.
#
# This script never opens admissions, death, discharge, or outcome data.

suppressPackageStartupMessages(library(data.table))

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("code/R/rebuild_v1/05_build_mimic_severity_core.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "00_config.R"))

stopifnot(
  identical(LOCKED$version, "1.0.1"),
  LOCKED$primary_exposure_window_hours_after_index == 6
)

phase1_gate_path <- file.path(QC_ROOT, "mimic", "phase1_complete_v1.csv")
phase2_gate_path <- file.path(
  QC_ROOT, "mimic_exposure", "phase2_mimic_exposure_complete_v1.csv"
)
preflight_inventory_path <- file.path(QC_ROOT, "preflight_file_inventory.csv")
native_oasis_gate_path <- file.path(
  QC_ROOT, "mimic_native_oasis",
  "phase2c_mimic_native_oasis_complete_v1.csv"
)
input_index <- file.path(PRIVATE_ROOT, "mimic", "mimic_index_cohort_v1.rds")
input_exposure <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_paired_exposure_primary_60min_v1.rds"
)

raw_chart <- file.path(MIMIC_ROOT, "icu", "chartevents.csv.gz")
raw_lab <- file.path(MIMIC_ROOT, "hosp", "labevents.csv.gz")
raw_omr <- file.path(MIMIC_ROOT, "hosp", "omr.csv.gz")
raw_input <- file.path(MIMIC_ROOT, "icu", "inputevents.csv.gz")
raw_d_items <- file.path(MIMIC_ROOT, "icu", "d_items.csv.gz")
raw_d_labitems <- file.path(MIMIC_ROOT, "hosp", "d_labitems.csv.gz")

private_out <- file.path(PRIVATE_ROOT, "mimic")
qc_out <- file.path(QC_ROOT, "mimic_severity")
cache_dir <- file.path(private_out, "cache_v1", "mimic_severity")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

completion_gate <- file.path(qc_out, "phase2b_mimic_severity_complete_v1.csv")
completion_gate_tmp <- paste0(completion_gate, ".tmp")

required_files <- c(
  phase1_gate_path, phase2_gate_path, preflight_inventory_path,
  native_oasis_gate_path,
  input_index, input_exposure,
  raw_chart, raw_lab, raw_omr, raw_input, raw_d_items, raw_d_labitems
)
if (any(!file.exists(required_files))) {
  stop(
    "Missing required input(s): ",
    paste(required_files[!file.exists(required_files)], collapse = ", ")
  )
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
  z <- fread(path)
  if (!identical(names(z), c("field", "value")) || anyDuplicated(z$field)) {
    stop("Malformed field/value completion gate: ", path)
  }
  setNames(as.character(z$value), z$field)
}

require_gate_value <- function(gate, field, expected = NULL) {
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
  value
}

# ---------------------------------------------------------------------------
# Immutable upstream completion gates. No large raw table is opened before
# every gate, script hash, and row-level input hash agrees.
# ---------------------------------------------------------------------------

phase1_gate <- read_gate_map(phase1_gate_path)
phase2_gate <- read_gate_map(phase2_gate_path)
native_oasis_gate <- read_gate_map(native_oasis_gate_path)
preflight_inventory <- fread(preflight_inventory_path)
preflight_omr <- preflight_inventory[
  database == "MIMIC-IV" & relative_path == "hosp/omr.csv.gz"
]
if (nrow(preflight_inventory) != 27L || nrow(preflight_omr) != 1L ||
    preflight_omr$exists[[1L]] != TRUE ||
    preflight_omr$readable[[1L]] != TRUE ||
    normalizePath(preflight_omr$absolute_path[[1L]], mustWork = TRUE) !=
      normalizePath(raw_omr, mustWork = TRUE)) {
  stop("D053 requires a readable OMR row in the 27-file preflight inventory.")
}
require_gate_value(phase1_gate, "locked_config_version", LOCKED$version)
require_gate_value(phase1_gate, "all_invariants_pass", "TRUE")
require_gate_value(phase1_gate, "outcome_leakage_guard_pass", "TRUE")
require_gate_value(phase2_gate, "locked_config_version", LOCKED$version)
require_gate_value(phase2_gate, "all_invariants_pass", "TRUE")
require_gate_value(phase2_gate, "outcome_leakage_guard_pass", "TRUE")
require_gate_value(phase2_gate, "all_required_qc_present", "TRUE")
require_gate_value(native_oasis_gate, "status", "PASS")
require_gate_value(native_oasis_gate, "locked_config_version", LOCKED$version)
require_gate_value(native_oasis_gate, "all_invariants_pass", "TRUE")
require_gate_value(native_oasis_gate, "outcome_leakage_guard_pass", "TRUE")
require_gate_value(native_oasis_gate, "actual_outcome_fields_read", "FALSE")
require_gate_value(native_oasis_gate, "predicted_probability_executed", "FALSE")
require_gate_value(native_oasis_gate, "hsc_substitute_allowed", "FALSE")

phase1_script <- file.path(dirname(script_path), "01_build_mimic_index_cohort.R")
phase2_script <- file.path(dirname(script_path), "03_build_mimic_paired_exposure.R")
native_oasis_script <- file.path(
  dirname(script_path), "05c_build_mimic_native_oasis.R"
)
native_oasis_helper <- file.path(
  dirname(script_path), "05d_filter_mimic_oasis_inputs.py"
)
require_gate_value(
  phase1_gate, "script_sha256", sha256_file(phase1_script)
)
require_gate_value(
  phase2_gate, "script_sha256", sha256_file(phase2_script)
)
require_gate_value(
  phase2_gate, "phase1_gate_sha256", sha256_file(phase1_gate_path)
)
require_gate_value(
  native_oasis_gate, "script_sha256", sha256_file(native_oasis_script)
)
require_gate_value(
  native_oasis_gate, "helper_sha256", sha256_file(native_oasis_helper)
)

index_sha256 <- sha256_file(input_index)
exposure_sha256 <- sha256_file(input_exposure)
require_gate_value(phase1_gate, "primary_cohort_rds_sha256", index_sha256)
require_gate_value(phase2_gate, "input_primary_cohort_sha256", index_sha256)
require_gate_value(phase2_gate, "primary_60min_rds_sha256", exposure_sha256)

# An interrupted rerun must never leave a stale PASS gate.
unlink(c(completion_gate, completion_gate_tmp), force = TRUE)

index_source <- as.data.table(readRDS(input_index))
exposure_source <- as.data.table(readRDS(input_exposure))
if (nrow(index_source) != as.integer(require_gate_value(
  phase2_gate, "strict_cohort_n"
))) {
  stop("Strict cohort row count disagrees with Phase 2 gate.")
}
if (nrow(exposure_source) != nrow(index_source)) {
  stop("Paired-exposure artifact must retain every strict-cohort row.")
}
if (sum(exposure_source$tuple_observed == TRUE) != as.integer(
  require_gate_value(phase2_gate, "primary_60min_n")
)) {
  stop("Tuple count disagrees with Phase 2 gate.")
}
if (anyDuplicated(exposure_source$stay_id) ||
    anyDuplicated(index_source$stay_id)) {
  stop("Upstream artifacts must have one row per strict stay.")
}
if (!setequal(exposure_source$stay_id, index_source$stay_id)) {
  stop("Phase 1 and Phase 2 strict-stay sets differ.")
}

forbidden_source_pattern <- paste(
  c("mort", "death", "dead", "expire", "hospital.*discharge", "outcome", "surviv"),
  collapse = "|"
)
if (any(grepl(
  forbidden_source_pattern, names(index_source), ignore.case = TRUE
)) || any(grepl(
  forbidden_source_pattern, names(exposure_source), ignore.case = TRUE
))) {
  stop("Outcome-like field found in an upstream outcome-blind artifact.")
}

required_exposure <- c(
  "stay_id", "subject_id", "hadm_id", "intime", "outtime",
  "age_at_admission", "gender", "index_time", "pf_ratio",
  "prediction_time", "tuple_observed", "vt_value", "smp"
)
missing_exposure <- setdiff(required_exposure, names(exposure_source))
if (length(missing_exposure)) {
  stop("Paired-exposure artifact is missing: ", paste(missing_exposure, collapse = ", "))
}

strict_base <- copy(exposure_source)
tuple_base <- strict_base[tuple_observed == TRUE & !is.na(prediction_time)]
if (nrow(tuple_base) != as.integer(phase2_gate[["primary_60min_n"]])) {
  stop("Complete-tuple population is not the locked Phase 2 population.")
}
if (anyDuplicated(strict_base$subject_id)) {
  stop("First-stay strict cohort unexpectedly contains repeated subjects.")
}
if (anyDuplicated(strict_base[, paste(subject_id, hadm_id, sep = ":")])) {
  stop("Strict cohort unexpectedly repeats a subject/admission key.")
}

to_epoch <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(x))
  z <- trimws(as.character(x))
  out <- rep(NA_real_, length(z))
  ok <- !is.na(z) & nzchar(z)
  if (any(ok)) {
    parsed <- as.POSIXct(
      z[ok], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    )
    out[ok] <- as.numeric(parsed)
  }
  out
}
from_epoch <- function(x) as.POSIXct(x, origin = "1970-01-01", tz = "UTC")

strict_base[, `:=`(
  intime_epoch = to_epoch(intime),
  index_epoch = to_epoch(index_time)
)]
tuple_base[, `:=`(
  intime_epoch = to_epoch(intime),
  index_epoch = to_epoch(index_time),
  prediction_epoch = to_epoch(prediction_time)
)]
if (anyNA(strict_base$intime_epoch) || anyNA(strict_base$index_epoch) ||
    anyNA(tuple_base$prediction_epoch)) {
  stop("Missing required ICU/index/prediction timestamp.")
}

prediction_bounds <- tuple_base[, .(
  stay_id,
  window_start_epoch = pmax(intime_epoch, index_epoch - 24 * 3600),
  window_end_epoch = prediction_epoch,
  icu_intime_epoch = intime_epoch
)]
index_bounds <- strict_base[, .(
  stay_id,
  window_start_epoch = pmax(intime_epoch, index_epoch - 24 * 3600),
  window_end_epoch = index_epoch,
  icu_intime_epoch = intime_epoch
)]
if (any(prediction_bounds$window_start_epoch > prediction_bounds$window_end_epoch) ||
    any(index_bounds$window_start_epoch > index_bounds$window_end_epoch)) {
  stop("Invalid severity window.")
}

# ---------------------------------------------------------------------------
# Full-EOF source filtering and checksum-bearing cache gate.
# ---------------------------------------------------------------------------

key_path <- file.path(cache_dir, "target_keys_v1.csv")
fwrite(
  strict_base[, .(subject_id, hadm_id, stay_id)],
  key_path
)
filter_helper <- file.path(
  dirname(script_path), "05a_filter_mimic_severity_inputs.py"
)
if (!file.exists(filter_helper)) stop("Missing MIMIC severity filter helper.")
filter_output <- system2(
  "python3",
  c(
    shQuote(filter_helper), "--keys", shQuote(key_path),
    "--mimic-root", shQuote(MIMIC_ROOT),
    "--cache-dir", shQuote(cache_dir)
  ),
  stdout = TRUE, stderr = TRUE
)
filter_status <- attr(filter_output, "status")
if (!is.null(filter_status) && filter_status != 0L) {
  stop("MIMIC target filter failed: ", paste(filter_output, collapse = "\n"))
}
message(paste(filter_output, collapse = "\n"))

cache_gate_path <- file.path(cache_dir, "severity_input_cache_complete_v1.csv")
cache_manifest_path <- file.path(cache_dir, "filter_manifest_v1.csv")
if (!file.exists(cache_gate_path) || !file.exists(cache_manifest_path)) {
  stop("Severity-input cache has no gate/manifest.")
}
cache_gate <- fread(cache_gate_path)
cache_manifest <- fread(cache_manifest_path)
if (nrow(cache_gate) != 1L || cache_gate$status[[1L]] != "PASS" ||
    cache_gate$all_sources_reached_eof[[1L]] != TRUE ||
    cache_gate$all_official_sha256_match[[1L]] != TRUE) {
  stop("Severity-input cache gate is not a complete PASS.")
}
if (nrow(cache_manifest) != 4L || any(cache_manifest$status != "PASS") ||
    any(cache_manifest$reached_eof != TRUE) ||
    any(cache_manifest$official_sha256_match != TRUE) ||
    any(cache_manifest$target_stay_count != nrow(strict_base))) {
  stop("Severity-input cache manifest is incomplete or inconsistent.")
}

cache_paths <- c(
  chartevents = file.path(cache_dir, "chartevents_severity_candidates_v1.csv.gz"),
  labevents = file.path(cache_dir, "labevents_severity_candidates_v1.csv.gz"),
  inputevents = file.path(cache_dir, "inputevents_severity_candidates_v1.csv.gz"),
  omr = file.path(cache_dir, "omr_height_candidates_v1.csv.gz")
)

read_filtered_cache <- function(path, source_name, required_columns) {
  if (!file.exists(path)) stop("Missing severity cache: ", path)
  out <- fread(
    cmd = sprintf("gzip -cd %s", shQuote(path)),
    showProgress = interactive(), fill = FALSE
  )
  missing <- setdiff(required_columns, names(out))
  if (length(missing)) {
    stop("Cache ", source_name, " is missing: ", paste(missing, collapse = ", "))
  }
  # Avoid data.table's column scoping entirely: the manifest also contains a
  # `source_name` column, which would otherwise mask this function argument.
  target_source_name <- source_name
  expected <- cache_manifest[["kept_rows"]][
    cache_manifest[["source_name"]] == target_source_name
  ]
  if (length(expected) != 1L || nrow(out) != expected) {
    stop(
      "Cache row count mismatch for ", source_name, ": ", nrow(out),
      " != ", paste(expected, collapse = ",")
    )
  }
  out
}

strict_numeric <- function(x) {
  z <- trimws(as.character(x))
  ok <- grepl("^[+-]?((([0-9]+)(\\.[0-9]*)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)?$", z)
  out <- rep(NA_real_, length(z))
  out[ok] <- suppressWarnings(as.numeric(z[ok]))
  out[!is.finite(out)] <- NA_real_
  out
}

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

# ---------------------------------------------------------------------------
# Verify exact item metadata before any clinical extraction.
# ---------------------------------------------------------------------------

d_items <- fread(cmd = sprintf("gzip -cd %s", shQuote(raw_d_items)))
d_labitems <- fread(cmd = sprintf("gzip -cd %s", shQuote(raw_d_labitems)))
expected_items <- data.table(
  itemid = c(
    220052L, 220181L, 220739L, 223900L, 223901L, 226730L, 226707L,
    221906L, 221289L, 222315L, 221662L, 221653L, 221749L
  ),
  expected_label = c(
    "Arterial Blood Pressure mean", "Non Invasive Blood Pressure mean",
    "GCS - Eye Opening", "GCS - Verbal Response", "GCS - Motor Response",
    "Height (cm)", "Height", "Norepinephrine", "Epinephrine", "Vasopressin",
    "Dopamine", "Dobutamine", "Phenylephrine"
  ),
  expected_linksto = c(rep("chartevents", 7L), rep("inputevents", 6L))
)
item_metadata <- merge(
  expected_items,
  d_items[, .(itemid, label, abbreviation, linksto, category, unitname, param_type)],
  by = "itemid", all.x = TRUE, sort = FALSE
)
item_metadata[, pass := label == expected_label & linksto == expected_linksto]

expected_labs <- data.table(
  itemid = c(51265L, 50912L),
  expected_label = c("Platelet Count", "Creatinine")
)
lab_metadata <- merge(
  expected_labs,
  d_labitems[, .(itemid, label, fluid, category)],
  by = "itemid", all.x = TRUE, sort = FALSE
)
lab_metadata[, pass := label == expected_label & fluid == "Blood"]
if (any(item_metadata$pass != TRUE) || any(lab_metadata$pass != TRUE)) {
  stop("Locked MIMIC severity item metadata mismatch.")
}
fwrite(item_metadata, file.path(qc_out, "locked_item_metadata_QC.csv"))
fwrite(lab_metadata, file.path(qc_out, "locked_labitem_metadata_QC.csv"))

# ---------------------------------------------------------------------------
# Chart events: GCS, MAP, and height.
# ---------------------------------------------------------------------------

message("Reading MIMIC GCS/MAP/height cache ...")
chart <- read_filtered_cache(
  cache_paths[["chartevents"]], "chartevents",
  c(
    "subject_id", "hadm_id", "stay_id", "charttime", "storetime",
    "itemid", "value", "valuenum", "valueuom", "warning"
  )
)
chart[, `:=`(
  charttime_epoch = to_epoch(charttime),
  storetime_epoch = to_epoch(storetime),
  value_num = strict_numeric(valuenum),
  value_text = trimws(as.character(value)),
  unit_normalized = tolower(trimws(as.character(valueuom))),
  warning_num = suppressWarnings(as.integer(warning))
)]
if (anyNA(chart$charttime_epoch)) stop("Unparseable charttime in retained chartevents.")
chart[, mapping := fcase(
  itemid == 220052L, "map_invasive",
  itemid == 220181L, "map_noninvasive",
  itemid == 220739L, "gcs_eye",
  itemid == 223900L, "gcs_verbal",
  itemid == 223901L, "gcs_motor",
  itemid == 226730L, "height_cm",
  itemid == 226707L, "height_inch",
  default = NA_character_
)]
if (anyNA(chart$mapping)) stop("Unmapped item entered chartevents severity cache.")

chart_mapping_qc <- chart[, .(
  raw_rows = .N,
  strict_stays = uniqueN(stay_id),
  numeric_rows = sum(!is.na(value_num)),
  storetime_missing_rows = sum(is.na(storetime_epoch)),
  warning_1_rows = sum(warning_num == 1L, na.rm = TRUE),
  warning_missing_rows = sum(is.na(warning_num))
), by = .(itemid, mapping, valueuom)]
setorder(chart_mapping_qc, itemid, valueuom)
fwrite(chart_mapping_qc, file.path(qc_out, "chartevents_mapping_unit_QC.csv"))

# Strict GCS text/valuenum reconciliation. ET/Trach/intubation text remains
# unscorable even when an extract supplies a numeric-looking code.
gcs_rows <- chart[grepl("^gcs_", mapping)]
gcs_rows[, component_upper := fcase(
  mapping == "gcs_eye", 4,
  mapping == "gcs_verbal", 5,
  mapping == "gcs_motor", 6,
  default = NA_real_
)]
gcs_rows[, text_norm := tolower(trimws(value_text))]
gcs_rows[, unscorable_airway_text := grepl(
  "ett|et[/ -]?trach|trache|intubat|unable to score", text_norm
)]

extract_prefix_score <- function(x) {
  m <- regexec("^([1-6])(?:\\.0)?(?:[[:space:]]|$)", x, perl = TRUE)
  pieces <- regmatches(x, m)
  vapply(
    pieces,
    function(z) if (length(z) >= 2L) as.numeric(z[[2L]]) else NA_real_,
    numeric(1L)
  )
}

gcs_rows[, text_prefix_score := extract_prefix_score(text_norm)]
gcs_rows[, text_category_score := fcase(
  mapping == "gcs_eye" & grepl("spont", text_norm), 4,
  mapping == "gcs_eye" & grepl("to speech|speech", text_norm), 3,
  mapping == "gcs_eye" & grepl("to pain|pain", text_norm), 2,
  mapping == "gcs_eye" & grepl("no response|none", text_norm), 1,
  mapping == "gcs_verbal" & grepl("orient", text_norm), 5,
  mapping == "gcs_verbal" & grepl("confus", text_norm), 4,
  mapping == "gcs_verbal" & grepl("inappropriate", text_norm), 3,
  mapping == "gcs_verbal" & grepl("incomprehensible", text_norm), 2,
  mapping == "gcs_verbal" & grepl("no response|none", text_norm), 1,
  mapping == "gcs_motor" & grepl("obeys", text_norm), 6,
  mapping == "gcs_motor" & grepl("localiz", text_norm), 5,
  mapping == "gcs_motor" & grepl("withdraw|flex-withdraw", text_norm), 4,
  mapping == "gcs_motor" & grepl("abnormal flex|flexion", text_norm), 3,
  mapping == "gcs_motor" & grepl("abnormal extens|extension", text_norm), 2,
  mapping == "gcs_motor" & grepl("no response|none", text_norm), 1,
  default = NA_real_
)]
gcs_rows[, text_internal_conflict :=
  !is.na(text_prefix_score) & !is.na(text_category_score) &
    text_prefix_score != text_category_score]
gcs_rows[, text_score := fcase(
  text_internal_conflict, NA_real_,
  !is.na(text_prefix_score), text_prefix_score,
  default = text_category_score
)]
gcs_rows[, valuenum_valid :=
  !is.na(value_num) & abs(value_num - round(value_num)) < 1e-10 &
    value_num >= 1 & value_num <= component_upper]
gcs_rows[, text_score_valid :=
  !is.na(text_score) & abs(text_score - round(text_score)) < 1e-10 &
    text_score >= 1 & text_score <= component_upper]
gcs_rows[, value_valuenum_conflict :=
  valuenum_valid & text_score_valid & value_num != text_score]
gcs_rows[, gcs_component_value := fcase(
  unscorable_airway_text | text_internal_conflict | value_valuenum_conflict,
  NA_real_,
  valuenum_valid, value_num,
  text_score_valid, text_score,
  default = NA_real_
)]
gcs_rows[, gcs_component_valid := !is.na(gcs_component_value)]

gcs_text_qc <- gcs_rows[, .(
  raw_rows = .N,
  strict_stays = uniqueN(stay_id),
  airway_unscorable_rows = sum(unscorable_airway_text),
  text_internal_conflict_rows = sum(text_internal_conflict),
  value_valuenum_conflict_rows = sum(value_valuenum_conflict),
  accepted_rows = sum(gcs_component_valid),
  storetime_missing_rows = sum(is.na(storetime_epoch))
), by = .(
  itemid, mapping, raw_value = value_text, raw_valuenum = valuenum
)]
setorder(gcs_text_qc, itemid, -raw_rows, raw_value)
fwrite(gcs_text_qc, file.path(qc_out, "gcs_value_valuenum_QC.csv"))

derive_gcs <- function(bounds, window_type) {
  z <- merge(gcs_rows, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  z[, measurement_in_window :=
    charttime_epoch >= window_start_epoch & charttime_epoch <= window_end_epoch]
  z[, storetime_available :=
    !is.na(storetime_epoch) & storetime_epoch <= window_end_epoch]
  eligible <- z[
    measurement_in_window & storetime_available & gcs_component_valid
  ]
  reduced <- eligible[, {
    vals <- unique(gcs_component_value)
    list(
      value_conflict = length(vals) > 1L,
      component_value = if (length(vals) == 1L) vals[[1L]] else NA_real_,
      component_available_epoch = if (length(vals) == 1L) {
        max(storetime_epoch)
      } else {
        NA_real_
      },
      warning_any = any(warning_num == 1L, na.rm = TRUE),
      duplicate_rows = .N
    )
  }, by = .(stay_id, gcs_time_epoch = charttime_epoch, component = mapping)]
  component_time_conflict_groups <- sum(reduced$value_conflict)
  reduced <- reduced[value_conflict == FALSE & !is.na(component_value)]
  wide <- dcast(
    reduced,
    stay_id + gcs_time_epoch ~ component,
    value.var = c("component_value", "component_available_epoch", "warning_any")
  )
  needed <- as.vector(outer(
    c("component_value", "component_available_epoch", "warning_any"),
    c("gcs_eye", "gcs_verbal", "gcs_motor"),
    paste, sep = "_"
  ))
  for (nm in needed) {
    if (!nm %in% names(wide)) {
      if (grepl("warning", nm)) wide[, (nm) := FALSE] else wide[, (nm) := NA_real_]
    }
  }
  complete <- wide[
    !is.na(component_value_gcs_eye) &
      !is.na(component_value_gcs_verbal) &
      !is.na(component_value_gcs_motor)
  ]
  complete[, gcs_worst :=
    component_value_gcs_eye + component_value_gcs_verbal +
      component_value_gcs_motor]
  complete[, gcs_available_epoch := pmax(
    component_available_epoch_gcs_eye,
    component_available_epoch_gcs_verbal,
    component_available_epoch_gcs_motor
  )]
  complete[, gcs_warning_any :=
    warning_any_gcs_eye | warning_any_gcs_verbal | warning_any_gcs_motor]
  setorder(complete, stay_id, gcs_worst, gcs_time_epoch, gcs_available_epoch)
  selected <- complete[, .SD[1L], by = stay_id]
  selected <- selected[, .(
    stay_id, gcs_worst, gcs_time_epoch, gcs_available_epoch,
    gcs_eye = component_value_gcs_eye,
    gcs_verbal = component_value_gcs_verbal,
    gcs_motor = component_value_gcs_motor,
    gcs_warning_any,
    gcs_source = "same_charttime_eye_verbal_motor_strict_reconstruction"
  )]
  timing <- data.table(
    window_type = window_type,
    component = "gcs",
    candidate_rows = nrow(z),
    rows_measurement_in_window = sum(z$measurement_in_window),
    rows_storetime_available = sum(
      z$measurement_in_window & z$storetime_available
    ),
    valid_component_rows = nrow(eligible),
    duplicate_component_time_conflict_groups = component_time_conflict_groups,
    complete_same_time_candidate_rows = nrow(complete),
    selected_patients = nrow(selected),
    airway_unscorable_rows_in_window = sum(
      z$measurement_in_window & z$storetime_available &
        z$unscorable_airway_text
    ),
    value_valuenum_conflict_rows_in_window = sum(
      z$measurement_in_window & z$storetime_available &
        z$value_valuenum_conflict
    )
  )
  list(selected = selected, timing = timing)
}

map_rows <- chart[mapping %chin% c("map_invasive", "map_noninvasive")]
map_rows[, unit_valid := unit_normalized == "mmhg"]
map_rows[, value_valid :=
  !is.na(value_num) & value_num >= 1 & value_num <= 250]
map_rows[, official_wide_range_251_299 :=
  !is.na(value_num) & value_num > 250 & value_num < 300]

derive_map <- function(bounds, window_type) {
  z <- merge(map_rows, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  z[, measurement_in_window :=
    charttime_epoch >= window_start_epoch & charttime_epoch <= window_end_epoch]
  z[, storetime_available :=
    !is.na(storetime_epoch) & storetime_epoch <= window_end_epoch]
  eligible <- z[
    measurement_in_window & storetime_available & unit_valid & value_valid
  ]
  reduced <- eligible[, .(
    map_value = median(value_num),
    map_available_epoch = max(storetime_epoch),
    warning_any = any(warning_num == 1L, na.rm = TRUE),
    duplicate_rows = .N
  ), by = .(
    stay_id, map_time_epoch = charttime_epoch, map_source = mapping,
    map_itemid = itemid
  )]
  reduced[, source_rank := fifelse(map_source == "map_invasive", 1L, 2L)]
  # The minimum value is selected across the window. At an exact value tie,
  # invasive MAP is preferred before time tie-breaks.
  setorder(
    reduced, stay_id, map_value, source_rank,
    map_time_epoch, map_available_epoch
  )
  selected <- reduced[, .SD[1L], by = stay_id]
  setnames(selected, "map_value", "map_min")
  selected <- selected[, .(
    stay_id, map_min, map_time_epoch, map_available_epoch,
    map_source, map_itemid, map_warning_any = warning_any
  )]
  timing <- data.table(
    window_type = window_type,
    component = "map",
    candidate_rows = nrow(z),
    rows_measurement_in_window = sum(z$measurement_in_window),
    rows_storetime_available = sum(
      z$measurement_in_window & z$storetime_available
    ),
    unit_valid_rows_in_window = sum(
      z$measurement_in_window & z$storetime_available & z$unit_valid
    ),
    excluded_251_299_rows_in_window = sum(
      z$measurement_in_window & z$storetime_available & z$unit_valid &
        z$official_wide_range_251_299
    ),
    valid_rows_in_window = nrow(eligible),
    selected_patients = nrow(selected)
  )
  list(selected = selected, timing = timing)
}

# ---------------------------------------------------------------------------
# Laboratory values: exact blood platelet/creatinine items, exact units, and
# storetime-based information availability. No missing storetime is backfilled.
# ---------------------------------------------------------------------------

message("Reading MIMIC platelet/creatinine cache ...")
lab_raw <- read_filtered_cache(
  cache_paths[["labevents"]], "labevents",
  c(
    "labevent_id", "subject_id", "hadm_id", "charttime", "storetime",
    "itemid", "value", "valuenum", "valueuom", "flag", "priority"
  )
)
lab_key <- strict_base[, .(subject_id, hadm_id, stay_id)]
lab <- merge(
  lab_raw, lab_key,
  by = c("subject_id", "hadm_id"), all = FALSE, sort = FALSE
)
if (nrow(lab) != nrow(lab_raw)) {
  stop("A retained laboratory row did not map one-to-one to a strict stay.")
}
lab[, `:=`(
  charttime_epoch = to_epoch(charttime),
  storetime_epoch = to_epoch(storetime),
  value_num = strict_numeric(valuenum),
  unit_normalized = tolower(trimws(as.character(valueuom)))
)]
if (anyNA(lab$charttime_epoch)) stop("Unparseable charttime in retained labevents.")
lab[, lab_mapping := fcase(
  itemid == 51265L, "platelet",
  itemid == 50912L, "creatinine",
  default = NA_character_
)]
if (anyNA(lab$lab_mapping)) stop("Unmapped item entered laboratory cache.")
lab[, unit_valid := fcase(
  lab_mapping == "platelet", unit_normalized == "k/ul",
  lab_mapping == "creatinine", unit_normalized == "mg/dl",
  default = FALSE
)]
lab[, value_valid := fcase(
  lab_mapping == "platelet",
  !is.na(value_num) & value_num > 0 & value_num <= 9999,
  lab_mapping == "creatinine",
  !is.na(value_num) & value_num >= 0.1 & value_num <= 28.28,
  default = FALSE
)]

lab_mapping_qc <- lab[, .(
  raw_rows = .N,
  strict_stays = uniqueN(stay_id),
  numeric_rows = sum(!is.na(value_num)),
  unit_valid_rows = sum(unit_valid),
  value_valid_rows = sum(value_valid),
  storetime_missing_rows = sum(is.na(storetime_epoch)),
  abnormal_flag_rows = sum(!is.na(flag) & nzchar(trimws(as.character(flag))))
), by = .(itemid, lab_mapping, valueuom, priority)]
setorder(lab_mapping_qc, itemid, valueuom, priority)
fwrite(lab_mapping_qc, file.path(qc_out, "labevents_mapping_unit_QC.csv"))

derive_labs <- function(bounds, window_type) {
  z <- merge(lab, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  z[, measurement_in_window :=
    charttime_epoch >= window_start_epoch & charttime_epoch <= window_end_epoch]
  z[, storetime_available :=
    !is.na(storetime_epoch) & storetime_epoch <= window_end_epoch]
  eligible <- z[
    measurement_in_window & storetime_available & unit_valid & value_valid
  ]
  reduced <- eligible[, .(
    lab_value = median(value_num),
    lab_available_epoch = max(storetime_epoch),
    duplicate_rows = .N,
    value_conflict = uniqueN(value_num) > 1L
  ), by = .(
    stay_id, lab_time_epoch = charttime_epoch, lab_mapping, itemid
  )]

  platelet <- reduced[lab_mapping == "platelet"]
  setorder(
    platelet, stay_id, lab_value, lab_time_epoch, lab_available_epoch
  )
  platelet <- platelet[, .SD[1L], by = stay_id]
  setnames(
    platelet,
    c("lab_value", "lab_time_epoch", "lab_available_epoch"),
    c("platelet_min", "platelet_time_epoch", "platelet_available_epoch")
  )
  platelet <- platelet[, .(
    stay_id, platelet_min, platelet_time_epoch, platelet_available_epoch,
    platelet_duplicate_value_conflict = value_conflict
  )]

  creatinine <- reduced[lab_mapping == "creatinine"]
  setorder(
    creatinine, stay_id, -lab_value, lab_time_epoch, lab_available_epoch
  )
  creatinine <- creatinine[, .SD[1L], by = stay_id]
  setnames(
    creatinine,
    c("lab_value", "lab_time_epoch", "lab_available_epoch"),
    c("creatinine_max", "creatinine_time_epoch", "creatinine_available_epoch")
  )
  creatinine <- creatinine[, .(
    stay_id, creatinine_max, creatinine_time_epoch,
    creatinine_available_epoch,
    creatinine_duplicate_value_conflict = value_conflict
  )]

  timing <- rbindlist(lapply(c("platelet", "creatinine"), function(nm) {
    zz <- z[lab_mapping == nm]
    data.table(
      window_type = window_type,
      component = nm,
      candidate_rows = nrow(zz),
      rows_measurement_in_window = sum(zz$measurement_in_window),
      rows_storetime_available = sum(
        zz$measurement_in_window & zz$storetime_available
      ),
      unit_valid_rows_in_window = sum(
        zz$measurement_in_window & zz$storetime_available & zz$unit_valid
      ),
      valid_rows_in_window = nrow(eligible[lab_mapping == nm]),
      selected_patients = if (nm == "platelet") nrow(platelet) else nrow(creatinine)
    )
  }))
  list(platelet = platelet, creatinine = creatinine, timing = timing)
}

# ---------------------------------------------------------------------------
# Actual vasoactive input intervals. The primary binary indicator requires a
# positive documented rate, a valid active interval overlapping the HSC
# window, storetime no later than the window endpoint, and a non-cancelled /
# non-rewritten status. Doses are not harmonized across databases.
# ---------------------------------------------------------------------------

message("Reading MIMIC six-drug inputevents cache ...")
input <- read_filtered_cache(
  cache_paths[["inputevents"]], "inputevents",
  c(
    "subject_id", "hadm_id", "stay_id", "starttime", "endtime", "storetime",
    "itemid", "amount", "amountuom", "rate", "rateuom", "orderid",
    "statusdescription", "originalamount", "originalrate"
  )
)
input[, drug_class := fcase(
  itemid == 221906L, "norepinephrine",
  itemid == 221289L, "epinephrine",
  itemid == 222315L, "vasopressin",
  itemid == 221662L, "dopamine",
  itemid == 221653L, "dobutamine",
  itemid == 221749L, "phenylephrine",
  default = NA_character_
)]
if (anyNA(input$drug_class)) stop("Unmapped item entered inputevents cache.")
input[, `:=`(
  start_epoch = to_epoch(starttime),
  end_epoch = to_epoch(endtime),
  storetime_epoch = to_epoch(storetime),
  rate_num = strict_numeric(rate),
  original_rate_num = strict_numeric(originalrate),
  status_normalized = tolower(trimws(as.character(statusdescription)))
)]
input[, status_non_cancelled :=
  !is.na(status_normalized) & nzchar(status_normalized) &
    !grepl("rewritten|cancel", status_normalized)]
input[, interval_valid :=
  !is.na(start_epoch) & !is.na(end_epoch) & end_epoch >= start_epoch]
input[, positive_rate := !is.na(rate_num) & rate_num > 0]

pressor_mapping_qc <- input[, .(
  raw_rows = .N,
  strict_stays = uniqueN(stay_id),
  positive_rate_rows = sum(positive_rate),
  zero_rate_rows = sum(!is.na(rate_num) & rate_num == 0),
  negative_rate_rows = sum(!is.na(rate_num) & rate_num < 0),
  rate_missing_rows = sum(is.na(rate_num)),
  interval_valid_rows = sum(interval_valid),
  storetime_missing_rows = sum(is.na(storetime_epoch)),
  non_cancelled_rows = sum(status_non_cancelled)
), by = .(itemid, drug_class, statusdescription, rateuom, amountuom)]
setorder(pressor_mapping_qc, drug_class, -raw_rows, statusdescription, rateuom)
fwrite(
  pressor_mapping_qc,
  file.path(qc_out, "vasoactive_inputevents_mapping_QC.csv")
)
pressor_rate_qc <- input[positive_rate == TRUE, {
  q <- quantile_safe(rate_num)
  list(
    positive_rate_rows = .N,
    strict_stays = uniqueN(stay_id),
    min = q[1L], q05 = q[2L], q25 = q[3L], median = q[4L],
    q75 = q[5L], q95 = q[6L], max = q[7L]
  )
}, by = .(itemid, drug_class, rateuom, statusdescription)]
setorder(pressor_rate_qc, drug_class, rateuom, statusdescription)
fwrite(
  pressor_rate_qc,
  file.path(qc_out, "vasoactive_rate_distribution_QC.csv")
)

# ---------------------------------------------------------------------------
# Historical OMR height fallback (D053). OMR provides date precision only, so
# the measurement is conservatively considered available at 23:59:59 on its
# chart date. Same-day/future values cannot establish pre-index availability.
# ---------------------------------------------------------------------------

message("Reading MIMIC OMR height cache ...")
omr_raw <- read_filtered_cache(
  cache_paths[["omr"]], "omr",
  c("subject_id", "chartdate", "seq_num", "result_name", "result_value")
)
if (any(omr_raw$result_name != "Height (Inches)")) {
  stop("Non-height result entered the OMR severity cache.")
}
omr_raw[, `:=`(
  chart_date = as.IDate(chartdate),
  height_omr_inches_raw = strict_numeric(result_value)
)]
if (anyNA(omr_raw$chart_date)) stop("Unparseable OMR height chartdate.")
omr_raw[, height_omr_cm_raw := height_omr_inches_raw * 2.54]
omr_raw[, value_valid :=
  !is.na(height_omr_cm_raw) &
    height_omr_cm_raw >= 120 & height_omr_cm_raw <= 230]

omr_key <- strict_base[, .(
  subject_id, stay_id,
  index_date = as.IDate(from_epoch(index_epoch))
)]
omr_linked <- merge(
  omr_raw, omr_key,
  by = "subject_id", all = FALSE, sort = FALSE, allow.cartesian = TRUE
)
if (nrow(omr_linked) != nrow(omr_raw)) {
  stop("A retained OMR row did not map one-to-one to a strict-cohort subject.")
}
omr_linked[, lookback_days := as.integer(index_date - chart_date)]
omr_linked[, eligible_5y :=
  value_valid & !is.na(lookback_days) &
    lookback_days >= 1L & lookback_days <= 1826L]

omr_daily <- omr_linked[eligible_5y == TRUE, .(
  height_omr_cm = median(height_omr_cm_raw),
  omr_daily_valid_value_n = .N,
  omr_daily_min_cm = min(height_omr_cm_raw),
  omr_daily_max_cm = max(height_omr_cm_raw),
  omr_daily_range_cm = max(height_omr_cm_raw) - min(height_omr_cm_raw)
), by = .(stay_id, chart_date, index_date, lookback_days)]
setorder(omr_daily, stay_id, -chart_date)
omr_selected <- omr_daily[, .SD[1L], by = stay_id]
omr_selected[, `:=`(
  omr_available_epoch = as.numeric(chart_date) * 86400 + 86399,
  omr_date_precision = "date_eod_conservative",
  omr_within_366d = lookback_days <= 366L
)]
setnames(omr_selected, "lookback_days", "omr_lookback_days")

omr_mapping_qc <- data.table(
  raw_height_rows = nrow(omr_linked),
  strict_subjects_with_raw_height = uniqueN(omr_linked$subject_id),
  numeric_rows = sum(!is.na(omr_linked$height_omr_inches_raw)),
  range_valid_rows = sum(omr_linked$value_valid),
  same_day_or_future_rows = sum(
    omr_linked$value_valid & omr_linked$lookback_days <= 0L,
    na.rm = TRUE
  ),
  older_than_1826d_rows = sum(
    omr_linked$value_valid & omr_linked$lookback_days > 1826L,
    na.rm = TRUE
  ),
  eligible_5y_rows = sum(omr_linked$eligible_5y),
  eligible_5y_daily_groups = nrow(omr_daily),
  daily_groups_with_multiple_valid_values = sum(
    omr_daily$omr_daily_valid_value_n > 1L
  ),
  daily_groups_range_gt5cm = sum(omr_daily$omr_daily_range_cm > 5),
  selected_5y_stays = nrow(omr_selected),
  selected_1y_stays = sum(omr_selected$omr_within_366d)
)
fwrite(omr_mapping_qc, file.path(qc_out, "omr_height_mapping_QC.csv"))

derive_pressor <- function(bounds, window_type) {
  z <- merge(input, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  z[, interval_overlaps_window :=
    interval_valid & start_epoch <= window_end_epoch &
      end_epoch >= window_start_epoch]
  z[, storetime_available :=
    !is.na(storetime_epoch) & storetime_epoch <= window_end_epoch]
  z[, active_row :=
    interval_overlaps_window & storetime_available & positive_rate &
      status_non_cancelled]
  z[is.na(active_row), active_row := FALSE]

  active <- z[active_row == TRUE]
  by_patient <- active[, .(
    vasopressor_any = TRUE,
    vasoactive_active_row_n = .N,
    vasoactive_drugs = paste(sort(unique(drug_class)), collapse = ";"),
    vasoactive_first_start_epoch = min(start_epoch),
    vasoactive_last_end_epoch = max(end_epoch),
    vasoactive_latest_available_epoch = max(storetime_epoch),
    norepinephrine_any = any(drug_class == "norepinephrine"),
    epinephrine_any = any(drug_class == "epinephrine"),
    vasopressin_any = any(drug_class == "vasopressin"),
    dopamine_any = any(drug_class == "dopamine"),
    dobutamine_any = any(drug_class == "dobutamine"),
    phenylephrine_any = any(drug_class == "phenylephrine")
  ), by = stay_id]
  out <- merge(
    bounds[, .(stay_id)], by_patient,
    by = "stay_id", all.x = TRUE, sort = FALSE
  )
  logical_fields <- c(
    "vasopressor_any", "norepinephrine_any", "epinephrine_any",
    "vasopressin_any", "dopamine_any", "dobutamine_any",
    "phenylephrine_any"
  )
  for (v in logical_fields) set(out, which(is.na(out[[v]])), v, FALSE)
  out[is.na(vasoactive_active_row_n), vasoactive_active_row_n := 0L]
  out[is.na(vasoactive_drugs), vasoactive_drugs := ""]

  timing <- data.table(
    window_type = window_type,
    component = "six_drug_vasoactive_exposure",
    candidate_rows = nrow(z),
    interval_overlap_rows = sum(z$interval_overlaps_window),
    storetime_available_overlap_rows = sum(
      z$interval_overlaps_window & z$storetime_available
    ),
    positive_rate_overlap_rows = sum(
      z$interval_overlaps_window & z$storetime_available & z$positive_rate
    ),
    active_rows = nrow(active),
    selected_patients = nrow(by_patient),
    rewritten_or_cancelled_overlap_rows = sum(
      z$interval_overlaps_window & z$storetime_available &
        !z$status_non_cancelled
    )
  )
  list(selected = out, timing = timing)
}

# Height/PBW support is defined before build_core is invoked. D053 keeps
# endpoint-available chartevents height first and uses only conservatively
# pre-index OMR height as a fallback.
height_rows <- chart[mapping %chin% c("height_cm", "height_inch")]
height_rows[, unit_valid := fcase(
  mapping == "height_cm", unit_normalized == "cm",
  mapping == "height_inch", unit_normalized == "inch",
  default = FALSE
)]
height_rows[, height_cm_value := fcase(
  mapping == "height_cm", value_num,
  mapping == "height_inch", value_num * 2.54,
  default = NA_real_
)]
height_rows[, value_valid :=
  !is.na(height_cm_value) & height_cm_value >= 120 & height_cm_value <= 230]
height_rows[, source_rank := fifelse(mapping == "height_cm", 1L, 2L)]

derive_height <- function(bounds, window_type) {
  z <- merge(height_rows, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  z[, measurement_available :=
    charttime_epoch >= icu_intime_epoch & charttime_epoch <= window_end_epoch]
  z[, storetime_available :=
    !is.na(storetime_epoch) & storetime_epoch <= window_end_epoch]
  eligible <- z[
    measurement_available & storetime_available & unit_valid & value_valid
  ]
  setorder(
    eligible, stay_id, source_rank, charttime_epoch, storetime_epoch, itemid
  )
  char_selected <- eligible[, .SD[1L], by = stay_id]
  char_selected <- char_selected[, .(
    stay_id,
    height_chartevents_cm = height_cm_value,
    height_chartevents_time_epoch = charttime_epoch,
    height_chartevents_available_epoch = storetime_epoch,
    height_chartevents_source = mapping,
    height_chartevents_itemid = itemid,
    height_chartevents_warning = warning_num %in% 1L
  )]

  combined <- merge(
    bounds[, .(stay_id)], char_selected,
    by = "stay_id", all.x = TRUE, sort = FALSE
  )
  combined <- merge(
    combined,
    omr_selected[, .(
      stay_id,
      height_omr_cm,
      height_omr_chartdate = chart_date,
      height_omr_available_epoch = omr_available_epoch,
      height_omr_lookback_days = omr_lookback_days,
      height_omr_date_precision = omr_date_precision,
      height_omr_within_366d = omr_within_366d,
      height_omr_daily_valid_value_n = omr_daily_valid_value_n,
      height_omr_daily_range_cm = omr_daily_range_cm
    )],
    by = "stay_id", all.x = TRUE, sort = FALSE
  )
  combined[, height_cm_chartevents_only := height_chartevents_cm]
  combined[, height_cm_omr_1y_fallback := fcase(
    !is.na(height_chartevents_cm), height_chartevents_cm,
    is.na(height_chartevents_cm) & !is.na(height_omr_cm) &
      height_omr_within_366d == TRUE, height_omr_cm,
    default = NA_real_
  )]
  combined[, height_cm := fcase(
    !is.na(height_chartevents_cm), height_chartevents_cm,
    is.na(height_chartevents_cm) & !is.na(height_omr_cm), height_omr_cm,
    default = NA_real_
  )]
  combined[, height_source := fcase(
    !is.na(height_chartevents_cm), height_chartevents_source,
    is.na(height_chartevents_cm) & !is.na(height_omr_cm),
    "omr_height_inches_5y_fallback",
    default = NA_character_
  )]
  combined[, `:=`(
    height_time_epoch = fcase(
      !is.na(height_chartevents_cm), height_chartevents_time_epoch,
      is.na(height_chartevents_cm) & !is.na(height_omr_cm),
      height_omr_available_epoch,
      default = NA_real_
    ),
    height_available_epoch = fcase(
      !is.na(height_chartevents_cm), height_chartevents_available_epoch,
      is.na(height_chartevents_cm) & !is.na(height_omr_cm),
      height_omr_available_epoch,
      default = NA_real_
    ),
    height_itemid = fifelse(
      !is.na(height_chartevents_cm), height_chartevents_itemid, NA_integer_
    ),
    height_warning = fifelse(
      !is.na(height_chartevents_cm), height_chartevents_warning, FALSE
    ),
    height_date_precision = fcase(
      !is.na(height_chartevents_cm), "exact_datetime",
      is.na(height_chartevents_cm) & !is.na(height_omr_cm),
      "date_eod_conservative",
      default = NA_character_
    ),
    height_lookback_days = fifelse(
      is.na(height_chartevents_cm) & !is.na(height_omr_cm),
      height_omr_lookback_days, NA_integer_
    )
  )]
  timing <- data.table(
    window_type = window_type,
    component = "height_pbw_support",
    candidate_rows = nrow(z),
    rows_measurement_by_endpoint = sum(z$measurement_available),
    rows_storetime_available = sum(
      z$measurement_available & z$storetime_available
    ),
    unit_valid_rows = sum(
      z$measurement_available & z$storetime_available & z$unit_valid
    ),
    valid_rows = nrow(eligible),
    chartevents_selected = nrow(char_selected),
    omr_5y_available = sum(!is.na(combined$height_omr_cm)),
    omr_5y_fallback_selected = sum(
      is.na(combined$height_chartevents_cm) &
        !is.na(combined$height_omr_cm)
    ),
    omr_1y_fallback_selected = sum(
      is.na(combined$height_chartevents_cm) &
        !is.na(combined$height_omr_cm) &
        combined$height_omr_within_366d == TRUE
    ),
    selected_patients = sum(!is.na(combined$height_cm)),
    cm_source_selected = sum(
      combined$height_chartevents_source == "height_cm", na.rm = TRUE
    ),
    inch_source_selected = sum(
      combined$height_chartevents_source == "height_inch", na.rm = TRUE
    )
  )
  list(selected = combined, timing = timing)
}

# ---------------------------------------------------------------------------
# Build the prediction-time and index-known HSCs from identical mappings.
# ---------------------------------------------------------------------------

build_core <- function(bounds, window_type) {
  gcs <- derive_gcs(bounds, window_type)
  map <- derive_map(bounds, window_type)
  height <- derive_height(bounds, window_type)
  labs <- derive_labs(bounds, window_type)
  pressor <- derive_pressor(bounds, window_type)

  out <- copy(bounds)
  out <- merge(out, gcs$selected, by = "stay_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, map$selected, by = "stay_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, pressor$selected, by = "stay_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, labs$platelet, by = "stay_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, labs$creatinine, by = "stay_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, height$selected, by = "stay_id", all.x = TRUE, sort = FALSE)
  demographics <- strict_base[, .(stay_id, age_at_admission, gender)]
  out <- merge(out, demographics, by = "stay_id", all.x = TRUE, sort = FALSE)

  out[, sex_binary_for_pbw := fcase(
    gender == "M", "male",
    gender == "F", "female",
    default = NA_character_
  )]
  calculate_pbw <- function(height) fcase(
    !is.na(height) & out$sex_binary_for_pbw == "male",
    50 + 0.91 * (height - 152.4),
    !is.na(height) & out$sex_binary_for_pbw == "female",
    45.5 + 0.91 * (height - 152.4),
    default = NA_real_
  )
  out[, `:=`(
    pbw_kg = calculate_pbw(height_cm),
    pbw_kg_chartevents_only = calculate_pbw(height_cm_chartevents_only),
    pbw_kg_omr_1y_fallback = calculate_pbw(height_cm_omr_1y_fallback)
  )]
  pbw_fields <- c(
    "pbw_kg", "pbw_kg_chartevents_only", "pbw_kg_omr_1y_fallback"
  )
  if (any(vapply(pbw_fields, function(v) {
    any(!is.na(out[[v]]) & out[[v]] <= 0)
  }, logical(1L)))) {
    stop("Non-positive MIMIC predicted body weight.")
  }
  out[, complete_hsc :=
    !is.na(gcs_worst) & !is.na(map_min) &
      !is.na(platelet_min) & !is.na(creatinine_max) &
      !is.na(vasopressor_any)]

  if (nrow(out) != nrow(bounds) || anyDuplicated(out$stay_id)) {
    stop("HSC is not one row per intended stay: ", window_type)
  }
  timing <- rbindlist(
    list(
      gcs$timing, map$timing, pressor$timing,
      labs$timing, height$timing
    ),
    fill = TRUE, use.names = TRUE
  )
  list(core = out, timing = timing)
}

prediction_result <- build_core(prediction_bounds, "prediction_time_hsc")
index_result <- build_core(index_bounds, "index_known_selection")
prediction_core <- prediction_result$core
index_core <- index_result$core

# Small deterministic rule tests complement the real-data invariants. They do
# not read or encode any patient/outcome information.
synthetic_map_tie <- data.table(
  map_value = c(40, 40),
  source_rank = c(2L, 1L),
  source = c("map_noninvasive", "map_invasive")
)
setorder(synthetic_map_tie, map_value, source_rank)
synthetic_pressor <- data.table(
  start = c(-10, -10, -10), end = c(10, 10, 10),
  store = c(0, 0, 20), rate = c(1, 0, 1),
  status = c("finishedrunning", "finishedrunning", "finishedrunning")
)
synthetic_pressor[, active :=
  start <= 0 & end >= -1440 & store <= 0 & rate > 0 &
    !grepl("rewritten|cancel", status)]
synthetic_rule_tests <- data.table(
  check = c(
    "gcs_component_sum_4_5_6_is_15",
    "airway_text_is_unscorable",
    "map_exact_minimum_tie_prefers_invasive",
    "pressor_requires_positive_rate_and_availability",
    "male_pbw_at_152_4cm_is_50",
    "female_pbw_at_152_4cm_is_45_5",
    "omr_same_date_values_use_median",
    "omr_prior_calendar_day_lookback_is_one"
  ),
  pass = c(
    sum(c(4, 5, 6)) == 15,
    grepl("ett|et[/ -]?trach|trache|intubat", "1.0 et/trach"),
    synthetic_map_tie$source[[1L]] == "map_invasive",
    identical(synthetic_pressor$active, c(TRUE, FALSE, FALSE)),
    abs((50 + 0.91 * (152.4 - 152.4)) - 50) < 1e-12,
    abs((45.5 + 0.91 * (152.4 - 152.4)) - 45.5) < 1e-12,
    median(c(160, 164)) == 162,
    as.integer(as.IDate("2020-01-02") - as.IDate("2020-01-01")) == 1L
  )
)
if (any(!synthetic_rule_tests$pass)) stop("Synthetic HSC rule test failed.")
fwrite(
  synthetic_rule_tests,
  file.path(qc_out, "severity_core_synthetic_rule_tests.csv")
)

# Retain only map_rows for the prespecified 251-299 range feasibility audit;
# the other multi-million-row candidate tables are no longer needed.
rm(
  chart, gcs_rows, height_rows, lab, lab_raw, input,
  omr_raw, omr_linked, omr_daily, omr_selected
)
invisible(gc())

# ---------------------------------------------------------------------------
# Invariants: ranges, window membership, information availability, and the
# strict separation between prediction-time and index-known products.
# ---------------------------------------------------------------------------

check_core <- function(x, window_type) {
  pbw_expected <- function(height) fcase(
    !is.na(height) & x$sex_binary_for_pbw == "male",
    50 + 0.91 * (height - 152.4),
    !is.na(height) & x$sex_binary_for_pbw == "female",
    45.5 + 0.91 * (height - 152.4),
    default = NA_real_
  )
  pbw_matches <- function(observed, height) {
    expected <- pbw_expected(height)
    all((is.na(observed) & is.na(expected)) |
          (!is.na(observed) & !is.na(expected) &
             abs(observed - expected) < 1e-10))
  }
  checks <- list(
    one_row_per_stay = !anyDuplicated(x$stay_id),
    nonnegative_window_length = all(x$window_start_epoch <= x$window_end_epoch),
    gcs_range = all(is.na(x$gcs_worst) | (x$gcs_worst >= 3 & x$gcs_worst <= 15)),
    map_range_harmonized = all(is.na(x$map_min) | (x$map_min >= 1 & x$map_min <= 250)),
    platelet_range = all(
      is.na(x$platelet_min) | (x$platelet_min > 0 & x$platelet_min <= 9999)
    ),
    creatinine_range = all(
      is.na(x$creatinine_max) |
        (x$creatinine_max >= 0.1 & x$creatinine_max <= 28.28)
    ),
    height_range = all(
      is.na(x$height_cm) | (x$height_cm >= 120 & x$height_cm <= 230)
    ),
    height_chartevents_only_range = all(
      is.na(x$height_cm_chartevents_only) |
        (x$height_cm_chartevents_only >= 120 &
           x$height_cm_chartevents_only <= 230)
    ),
    height_omr_1y_fallback_range = all(
      is.na(x$height_cm_omr_1y_fallback) |
        (x$height_cm_omr_1y_fallback >= 120 &
           x$height_cm_omr_1y_fallback <= 230)
    ),
    pbw_positive = all(is.na(x$pbw_kg) | x$pbw_kg > 0) &&
      all(is.na(x$pbw_kg_chartevents_only) |
            x$pbw_kg_chartevents_only > 0) &&
      all(is.na(x$pbw_kg_omr_1y_fallback) |
            x$pbw_kg_omr_1y_fallback > 0),
    pbw_formula_primary = pbw_matches(x$pbw_kg, x$height_cm),
    pbw_formula_chartevents_only = pbw_matches(
      x$pbw_kg_chartevents_only, x$height_cm_chartevents_only
    ),
    pbw_formula_omr_1y = pbw_matches(
      x$pbw_kg_omr_1y_fallback, x$height_cm_omr_1y_fallback
    ),
    height_primary_source_precedence = all(
      (is.na(x$height_chartevents_cm) |
         x$height_cm == x$height_chartevents_cm) &
        (!is.na(x$height_chartevents_cm) |
           is.na(x$height_omr_cm) | x$height_cm == x$height_omr_cm)
    ),
    omr_5y_lookback = all(
      is.na(x$height_omr_cm) |
        (x$height_omr_lookback_days >= 1L &
           x$height_omr_lookback_days <= 1826L)
    ),
    omr_1y_sensitivity_limit = all(
      is.na(x$height_cm_omr_1y_fallback) |
        !is.na(x$height_chartevents_cm) |
        (x$height_omr_within_366d == TRUE &
           x$height_omr_lookback_days <= 366L)
    ),
    pressor_binary_known = all(!is.na(x$vasopressor_any)),
    gcs_measurement_in_window = all(
      is.na(x$gcs_time_epoch) |
        (x$gcs_time_epoch >= x$window_start_epoch &
          x$gcs_time_epoch <= x$window_end_epoch)
    ),
    gcs_available_by_end = all(
      is.na(x$gcs_available_epoch) |
        x$gcs_available_epoch <= x$window_end_epoch
    ),
    map_measurement_in_window = all(
      is.na(x$map_time_epoch) |
        (x$map_time_epoch >= x$window_start_epoch &
          x$map_time_epoch <= x$window_end_epoch)
    ),
    map_available_by_end = all(
      is.na(x$map_available_epoch) |
        x$map_available_epoch <= x$window_end_epoch
    ),
    platelet_measurement_in_window = all(
      is.na(x$platelet_time_epoch) |
        (x$platelet_time_epoch >= x$window_start_epoch &
          x$platelet_time_epoch <= x$window_end_epoch)
    ),
    platelet_available_by_end = all(
      is.na(x$platelet_available_epoch) |
        x$platelet_available_epoch <= x$window_end_epoch
    ),
    creatinine_measurement_in_window = all(
      is.na(x$creatinine_time_epoch) |
        (x$creatinine_time_epoch >= x$window_start_epoch &
          x$creatinine_time_epoch <= x$window_end_epoch)
    ),
    creatinine_available_by_end = all(
      is.na(x$creatinine_available_epoch) |
        x$creatinine_available_epoch <= x$window_end_epoch
    ),
    height_measurement_by_end = all(
      is.na(x$height_time_epoch) |
        x$height_time_epoch <= x$window_end_epoch
    ),
    chartevents_height_during_icu_by_end = all(
      is.na(x$height_chartevents_time_epoch) |
        (x$height_chartevents_time_epoch >= x$icu_intime_epoch &
           x$height_chartevents_time_epoch <= x$window_end_epoch)
    ),
    height_available_by_end = all(
      is.na(x$height_available_epoch) |
        x$height_available_epoch <= x$window_end_epoch
    ),
    active_pressor_available_by_end = all(
      is.na(x$vasoactive_latest_available_epoch) |
        x$vasoactive_latest_available_epoch <= x$window_end_epoch
    )
  )
  data.table(
    window_type = window_type,
    check = names(checks),
    pass = unlist(checks, use.names = FALSE)
  )
}

invariants <- rbindlist(list(
  check_core(prediction_core, "prediction_time_hsc"),
  check_core(index_core, "index_known_selection")
))
if (any(!invariants$pass)) {
  stop(
    "Severity-core invariant failure(s): ",
    paste(
      invariants[pass == FALSE, paste(window_type, check, sep = ":")],
      collapse = ", "
    )
  )
}
fwrite(invariants, file.path(qc_out, "severity_core_invariant_tests.csv"))

finalize_core_times <- function(x) {
  out <- copy(x)
  epoch_map <- c(
    window_start_epoch = "hsc_window_start",
    window_end_epoch = "hsc_window_end",
    gcs_time_epoch = "gcs_time",
    gcs_available_epoch = "gcs_available_time",
    map_time_epoch = "map_time",
    map_available_epoch = "map_available_time",
    platelet_time_epoch = "platelet_time",
    platelet_available_epoch = "platelet_available_time",
    creatinine_time_epoch = "creatinine_time",
    creatinine_available_epoch = "creatinine_available_time",
    height_time_epoch = "height_time",
    height_available_epoch = "height_available_time",
    height_chartevents_time_epoch = "height_chartevents_time",
    height_chartevents_available_epoch = "height_chartevents_available_time",
    height_omr_available_epoch = "height_omr_available_time",
    vasoactive_first_start_epoch = "vasoactive_first_start_time",
    vasoactive_last_end_epoch = "vasoactive_last_end_time",
    vasoactive_latest_available_epoch = "vasoactive_latest_available_time"
  )
  for (old in names(epoch_map)) {
    if (old %in% names(out)) out[, (epoch_map[[old]]) := from_epoch(get(old))]
  }
  drop <- intersect(
    c(names(epoch_map), "icu_intime_epoch"), names(out)
  )
  out[, (drop) := NULL]
  out
}

prediction_core_final <- finalize_core_times(prediction_core)
index_core_final <- finalize_core_times(index_core)

# Remove ICU-outcome-proxy/future administrative fields from model-ready
# artifacts even though upstream tuple construction legitimately used outtime
# only to bound observation.
prediction_drop <- intersect(
  c(
    "outtime", "last_careunit", "observable_exposure_end",
    "observable_window_minutes", "protocol_exposure_end", "n_valid_tuples",
    "last_valid_anchor_time", "intime_epoch", "index_epoch",
    "prediction_epoch", "age_at_admission", "gender"
  ),
  names(tuple_base)
)
prediction_model_base <- copy(tuple_base)
prediction_model_base[, (prediction_drop) := NULL]

selection_keep <- intersect(c(
  "stay_id", "subject_id", "hadm_id", "intime", "age_at_admission",
  "gender", "index_time", "pao2", "fio2_near_value", "peep_near_value",
  "pf_ratio", "invasive_evidence_type", "infection_direction",
  "first_careunit", "admission_type", "pao2_source", "fio2_near_time",
  "fio2_near_source", "fio2_signed_gap_min", "fio2_abs_gap_min",
  "peep_near_time", "peep_near_source", "peep_near_label",
  "peep_signed_gap_min", "peep_abs_gap_min", "infection_time",
  "infection_gap_h", "infection_evidence_time",
  "infection_culture_time_precision", "infection_available_by_index",
  "tuple_observed"
), names(strict_base))
selection_model_base <- strict_base[, ..selection_keep]
selection_model_base[, c("age_at_admission", "gender") := NULL]

prediction_attached <- merge(
  prediction_model_base, prediction_core_final,
  by = "stay_id", all.x = TRUE, sort = FALSE
)
prediction_attached[, vt_per_pbw_mL_per_kg := fifelse(
  !is.na(pbw_kg), vt_value / pbw_kg, NA_real_
)]
prediction_attached[, smp_per_pbw_J_per_min_per_kg := fifelse(
  !is.na(pbw_kg), smp / pbw_kg, NA_real_
)]
prediction_attached[, vt_per_pbw_chartevents_only_mL_per_kg := fifelse(
  !is.na(pbw_kg_chartevents_only),
  vt_value / pbw_kg_chartevents_only, NA_real_
)]
prediction_attached[, smp_per_pbw_chartevents_only_J_per_min_per_kg := fifelse(
  !is.na(pbw_kg_chartevents_only),
  smp / pbw_kg_chartevents_only, NA_real_
)]
prediction_attached[, vt_per_pbw_omr_1y_fallback_mL_per_kg := fifelse(
  !is.na(pbw_kg_omr_1y_fallback),
  vt_value / pbw_kg_omr_1y_fallback, NA_real_
)]
prediction_attached[, smp_per_pbw_omr_1y_fallback_J_per_min_per_kg := fifelse(
  !is.na(pbw_kg_omr_1y_fallback),
  smp / pbw_kg_omr_1y_fallback, NA_real_
)]

# D053 requires the absolute-versus-PBW-normalized secondary comparison to be
# frozen without altering absolute sMP.  Audit every saved normalized field
# against its numerator and the corresponding PBW definition before publish.
ratio_matches <- function(observed, numerator, denominator) {
  expected <- fifelse(
    !is.na(denominator), numerator / denominator, NA_real_
  )
  all(
    (is.na(observed) & is.na(expected)) |
      (!is.na(observed) & !is.na(expected) &
         abs(observed - expected) < 1e-12)
  )
}
normalized_formula_invariants <- data.table(
  window_type = "prediction_time_hsc",
  check = c(
    "absolute_smp_unchanged_by_normalization",
    "vt_per_pbw_formula_primary_omr_5y",
    "smp_per_pbw_formula_primary_omr_5y",
    "vt_per_pbw_formula_chartevents_only",
    "smp_per_pbw_formula_chartevents_only",
    "vt_per_pbw_formula_omr_1y_fallback",
    "smp_per_pbw_formula_omr_1y_fallback"
  ),
  pass = c(
    isTRUE(all.equal(
      prediction_attached[
        match(prediction_model_base$stay_id, stay_id), smp
      ],
      prediction_model_base$smp,
      check.attributes = FALSE
    )),
    ratio_matches(
      prediction_attached$vt_per_pbw_mL_per_kg,
      prediction_attached$vt_value,
      prediction_attached$pbw_kg
    ),
    ratio_matches(
      prediction_attached$smp_per_pbw_J_per_min_per_kg,
      prediction_attached$smp,
      prediction_attached$pbw_kg
    ),
    ratio_matches(
      prediction_attached$vt_per_pbw_chartevents_only_mL_per_kg,
      prediction_attached$vt_value,
      prediction_attached$pbw_kg_chartevents_only
    ),
    ratio_matches(
      prediction_attached$smp_per_pbw_chartevents_only_J_per_min_per_kg,
      prediction_attached$smp,
      prediction_attached$pbw_kg_chartevents_only
    ),
    ratio_matches(
      prediction_attached$vt_per_pbw_omr_1y_fallback_mL_per_kg,
      prediction_attached$vt_value,
      prediction_attached$pbw_kg_omr_1y_fallback
    ),
    ratio_matches(
      prediction_attached$smp_per_pbw_omr_1y_fallback_J_per_min_per_kg,
      prediction_attached$smp,
      prediction_attached$pbw_kg_omr_1y_fallback
    )
  )
)
if (any(!normalized_formula_invariants$pass)) {
  stop(
    "PBW-normalized exposure invariant failure(s): ",
    paste(
      normalized_formula_invariants[pass == FALSE, check],
      collapse = ", "
    )
  )
}
invariants <- rbindlist(list(invariants, normalized_formula_invariants))
fwrite(invariants, file.path(qc_out, "severity_core_invariant_tests.csv"))

index_attached <- merge(
  selection_model_base, index_core_final,
  by = "stay_id", all.x = TRUE, sort = FALSE
)

if (nrow(prediction_attached) != nrow(tuple_base) ||
    anyDuplicated(prediction_attached$stay_id)) {
  stop("Prediction-time artifact row invariant failed.")
}
if (nrow(index_attached) != nrow(strict_base) ||
    anyDuplicated(index_attached$stay_id)) {
  stop("Index-known selection artifact row invariant failed.")
}
if ("prediction_time" %in% names(index_attached) ||
    any(c(
      "anchor_time", "pplat", "ppeak_value", "vt_value", "smp",
      "outtime", "last_careunit", "observable_exposure_end",
      "n_valid_tuples", "last_valid_anchor_time"
    ) %in% names(index_attached))) {
  stop("Post-index/future field entered index-known selection artifact.")
}

forbidden_output_pattern <- paste(
  c(
    "mort", "death", "dead", "expire", "discharge", "outcome", "surviv",
    "outtime", "last_careunit"
  ),
  collapse = "|"
)
if (any(grepl(
  forbidden_output_pattern, names(prediction_attached), ignore.case = TRUE
)) || any(grepl(
  forbidden_output_pattern, names(index_attached), ignore.case = TRUE
))) {
  stop("Outcome-like/future administrative field entered an HSC artifact.")
}

prediction_rds <- file.path(
  private_out, "mimic_paired_exposure_with_severity_core_v1.rds"
)
selection_rds <- file.path(
  private_out, "mimic_index_known_selection_core_v1.rds"
)
native_rds <- file.path(
  private_out, "mimic_native_oasis_feasibility_v1.rds"
)

common_metadata <- list(
  version = "mimic_harmonized_severity_core_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED$version,
  script = script_path,
  helper = normalizePath(filter_helper, mustWork = TRUE),
  outcome_blind = TRUE,
  source_gate_phase1_sha256 = sha256_file(phase1_gate_path),
  source_gate_phase2_sha256 = sha256_file(phase2_gate_path),
  mapping = paste(
    "GCS 220739/223900/223901 same-charttime strict reconstruction;",
    "MAP 220052/220181 minimum 1-250 mmHg;",
    "platelet 51265 minimum K/uL; creatinine 50912 maximum mg/dL;",
    "six positive-rate active inputevent drugs including phenylephrine;",
    "height chartevents first, then pre-index OMR <=1826-day fallback"
  )
)
attr(prediction_attached, "rebuild_metadata") <- c(
  common_metadata,
  list(
    artifact = basename(prediction_rds),
    population = "locked 0-6 h primary 60-min complete-tuple cohort",
    window = "max(ICU intime, index-24 h) through tuple prediction_time"
  )
)
attr(index_attached, "rebuild_metadata") <- c(
  common_metadata,
  list(
    artifact = basename(selection_rds),
    population = "all strict index-cohort stays",
    window = "max(ICU intime, index-24 h) through index",
    intended_use = "tuple-observation selection/IPW model only"
  )
)
saveRDS(prediction_attached, prediction_rds, compress = "xz")
saveRDS(index_attached, selection_rds, compress = "xz")

# ---------------------------------------------------------------------------
# Native OASIS pointer/provenance only. D048 is resolved by the separate,
# outcome-free Phase 2c artifact. This HSC script does not execute or copy its
# row-level score and never substitutes the native +24 h score for S0.
# ---------------------------------------------------------------------------

official_repo <- normalizePath(
  file.path(PROJECT_ROOT, "..", "tmp", "mimic-code"), mustWork = TRUE
)
official_oasis_sql <- normalizePath(
  file.path(official_repo, "mimic-iv", "concepts", "score", "oasis.sql"),
  mustWork = TRUE
)
official_commit <- system2(
  "git", c("-C", shQuote(official_repo), "rev-parse", "HEAD"),
  stdout = TRUE, stderr = TRUE
)
if (length(official_commit) != 1L ||
    !grepl("^[0-9a-f]{40}$", official_commit)) {
  stop("Could not verify pinned official mimic-code commit.")
}
oasis_sql_text <- readLines(official_oasis_sql, warn = FALSE)
oasis_forbidden_refs <- c(
  "deathtime", "hospital_expire_flag", "dischtime", "discharge_location"
)
oasis_ref_present <- vapply(
  oasis_forbidden_refs,
  function(x) any(grepl(x, oasis_sql_text, fixed = TRUE)),
  logical(1L)
)
if (!all(oasis_ref_present)) {
  stop("Pinned OASIS feasibility audit did not find expected provenance markers.")
}
native_feasibility <- data.table(
  benchmark = "OASIS",
  status = "RESOLVED_SEPARATE_OUTCOME_FREE_ARTIFACT",
  official_repository_commit = official_commit,
  official_sql_path = official_oasis_sql,
  official_sql_sha256 = sha256_file(official_oasis_sql),
  official_time_origin = "first 24 hours after ICU admission",
  hsc_substitute_allowed = FALSE,
  executed_in_phase2b = FALSE,
  actual_outcome_fields_read = FALSE,
  blocking_for_hsc = FALSE,
  separate_gate_status = require_gate_value(native_oasis_gate, "status"),
  separate_gate_path = normalizePath(native_oasis_gate_path, mustWork = TRUE),
  separate_gate_sha256 = sha256_file(native_oasis_gate_path),
  separate_native_rds_sha256 = require_gate_value(
    native_oasis_gate, "native_oasis_rds_sha256"
  ),
  separate_native_n = as.integer(require_gate_value(
    native_oasis_gate, "strict_cohort_n"
  )),
  reason = paste(
    "D048 is implemented by checksum-gated 05c/05d using explicit safe-column",
    "allow-lists and no actual-outcome or predicted-probability fields.",
    "The native first-day (+24 h) OASIS remains contextual only and cannot",
    "replace the time-aligned HSC or enter the index-time S0 model."
  )
)
if (native_feasibility$separate_native_n[[1L]] != nrow(strict_base)) {
  stop("Separate native OASIS artifact does not cover the strict cohort.")
}
attr(native_feasibility, "rebuild_metadata") <- list(
  version = "mimic_native_oasis_feasibility_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  outcome_blind = TRUE,
  row_level_patient_data = FALSE
)
saveRDS(native_feasibility, native_rds, compress = "xz")
fwrite(
  native_feasibility,
  file.path(qc_out, "native_oasis_feasibility_provenance.csv")
)

# ---------------------------------------------------------------------------
# Aggregate-only mapping, timing, coverage, range, source, missingness, and
# tuple-observation selection QC.
# ---------------------------------------------------------------------------

timing_qc <- rbindlist(
  list(prediction_result$timing, index_result$timing),
  fill = TRUE, use.names = TRUE
)
fwrite(timing_qc, file.path(qc_out, "severity_core_timing_QC.csv"))

core_list <- list(
  prediction_time_hsc = prediction_core,
  index_known_selection = index_core
)
missingness_qc <- rbindlist(lapply(names(core_list), function(nm) {
  x <- core_list[[nm]]
  rbindlist(lapply(c(
    "gcs_worst", "map_min", "vasopressor_any", "platelet_min",
    "creatinine_max", "height_cm", "height_cm_chartevents_only",
    "height_cm_omr_1y_fallback", "pbw_kg",
    "pbw_kg_chartevents_only", "pbw_kg_omr_1y_fallback", "complete_hsc"
  ), function(v) {
    data.table(
      window_type = nm,
      denominator = nrow(x),
      variable = v,
      available_n = sum(!is.na(x[[v]])),
      missing_n = sum(is.na(x[[v]])),
      available_proportion = mean(!is.na(x[[v]])),
      positive_n = if (is.logical(x[[v]])) sum(x[[v]], na.rm = TRUE) else NA_integer_
    )
  }))
}))
fwrite(missingness_qc, file.path(qc_out, "severity_core_missingness.csv"))

value_qc <- rbindlist(lapply(names(core_list), function(nm) {
  x <- core_list[[nm]]
  rbindlist(list(
    distribution_row(nm, "gcs_worst", x$gcs_worst),
    distribution_row(nm, "map_min", x$map_min),
    distribution_row(nm, "platelet_min", x$platelet_min),
    distribution_row(nm, "creatinine_max", x$creatinine_max),
    distribution_row(nm, "height_cm", x$height_cm),
    distribution_row(
      nm, "height_cm_chartevents_only", x$height_cm_chartevents_only
    ),
    distribution_row(
      nm, "height_cm_omr_1y_fallback", x$height_cm_omr_1y_fallback
    ),
    distribution_row(nm, "pbw_kg", x$pbw_kg),
    distribution_row(
      nm, "pbw_kg_chartevents_only", x$pbw_kg_chartevents_only
    ),
    distribution_row(
      nm, "pbw_kg_omr_1y_fallback", x$pbw_kg_omr_1y_fallback
    )
  ))
}))
value_qc <- rbindlist(list(
  value_qc,
  distribution_row(
    "prediction_time_hsc", "vt_per_pbw_mL_per_kg",
    prediction_attached$vt_per_pbw_mL_per_kg
  ),
  distribution_row(
    "prediction_time_hsc", "smp_per_pbw_J_per_min_per_kg",
    prediction_attached$smp_per_pbw_J_per_min_per_kg
  ),
  distribution_row(
    "prediction_time_hsc", "vt_per_pbw_chartevents_only_mL_per_kg",
    prediction_attached$vt_per_pbw_chartevents_only_mL_per_kg
  ),
  distribution_row(
    "prediction_time_hsc",
    "smp_per_pbw_chartevents_only_J_per_min_per_kg",
    prediction_attached$smp_per_pbw_chartevents_only_J_per_min_per_kg
  ),
  distribution_row(
    "prediction_time_hsc", "vt_per_pbw_omr_1y_fallback_mL_per_kg",
    prediction_attached$vt_per_pbw_omr_1y_fallback_mL_per_kg
  ),
  distribution_row(
    "prediction_time_hsc",
    "smp_per_pbw_omr_1y_fallback_J_per_min_per_kg",
    prediction_attached$smp_per_pbw_omr_1y_fallback_J_per_min_per_kg
  )
))
fwrite(value_qc, file.path(qc_out, "severity_core_value_distribution.csv"))

source_counts <- function(x) {
  g <- x[, .(source = fifelse(is.na(gcs_source), "missing", gcs_source))]
  g <- g[, .N, by = source]
  g[, component := "gcs"]
  m <- x[, .(source = fifelse(is.na(map_source), "missing", map_source))]
  m <- m[, .N, by = source]
  m[, component := "map"]
  h <- x[, .(source = fifelse(is.na(height_source), "missing", height_source))]
  h <- h[, .N, by = source]
  h[, component := "height"]
  rbindlist(list(g, m, h), use.names = TRUE)
}
source_qc <- rbindlist(lapply(names(core_list), function(nm) {
  z <- source_counts(core_list[[nm]])
  z[, `:=`(window_type = nm, denominator = nrow(core_list[[nm]]))]
  z[, proportion := N / denominator]
  z
}))
fwrite(source_qc, file.path(qc_out, "severity_core_source_frequency.csv"))

height_source_coverage_qc <- rbindlist(lapply(names(core_list), function(nm) {
  x <- core_list[[nm]]
  data.table(
    window_type = nm,
    denominator = nrow(x),
    chartevents_height_n = sum(!is.na(x$height_chartevents_cm)),
    omr_5y_height_n = sum(!is.na(x$height_omr_cm)),
    both_sources_n = sum(
      !is.na(x$height_chartevents_cm) & !is.na(x$height_omr_cm)
    ),
    chartevents_primary_n = sum(
      x$height_source %chin% c("height_cm", "height_inch"), na.rm = TRUE
    ),
    omr_5y_fallback_primary_n = sum(
      x$height_source == "omr_height_inches_5y_fallback", na.rm = TRUE
    ),
    primary_height_n = sum(!is.na(x$height_cm)),
    omr_1y_fallback_height_n = sum(!is.na(x$height_cm_omr_1y_fallback)),
    chartevents_only_height_n = sum(!is.na(x$height_cm_chartevents_only))
  )
}))
fwrite(
  height_source_coverage_qc,
  file.path(qc_out, "height_source_coverage_QC.csv")
)

height_overlap_qc <- rbindlist(lapply(names(core_list), function(nm) {
  x <- core_list[[nm]]
  d <- x[
    !is.na(height_chartevents_cm) & !is.na(height_omr_cm),
    height_chartevents_cm - height_omr_cm
  ]
  q <- quantile_safe(d)
  aq <- quantile_safe(abs(d))
  data.table(
    window_type = nm,
    overlap_n = length(d),
    signed_difference_mean_cm = if (length(d)) mean(d) else NA_real_,
    signed_difference_median_cm = q[4L],
    signed_difference_q05_cm = q[2L],
    signed_difference_q95_cm = q[6L],
    absolute_difference_median_cm = aq[4L],
    absolute_difference_q95_cm = aq[6L],
    absolute_difference_gt5cm_n = sum(abs(d) > 5),
    absolute_difference_gt10cm_n = sum(abs(d) > 10)
  )
}))
fwrite(
  height_overlap_qc,
  file.path(qc_out, "height_chartevents_omr_agreement_QC.csv")
)

height_component_feasibility_qc <- rbindlist(lapply(
  names(core_list), function(nm) {
    x <- core_list[[nm]]
    data.table(
      window_type = nm,
      denominator = nrow(x),
      complete_hsc_n = sum(x$complete_hsc),
      component_complete_chartevents_only_n = sum(
        x$complete_hsc & !is.na(x$pbw_kg_chartevents_only)
      ),
      component_complete_omr_1y_fallback_n = sum(
        x$complete_hsc & !is.na(x$pbw_kg_omr_1y_fallback)
      ),
      component_complete_omr_5y_fallback_n = sum(
        x$complete_hsc & !is.na(x$pbw_kg)
      )
    )
  }
))
fwrite(
  height_component_feasibility_qc,
  file.path(qc_out, "height_component_feasibility_QC.csv")
)

selected_warning_qc <- rbindlist(lapply(names(core_list), function(nm) {
  x <- core_list[[nm]]
  data.table(
    window_type = nm,
    denominator = nrow(x),
    gcs_selected_warning_n = sum(x$gcs_warning_any, na.rm = TRUE),
    map_selected_warning_n = sum(x$map_warning_any, na.rm = TRUE),
    height_selected_warning_n = sum(x$height_warning, na.rm = TRUE)
  )
}))
fwrite(
  selected_warning_qc,
  file.path(qc_out, "selected_chartevent_warning_QC.csv")
)

pressor_coverage_qc <- rbindlist(lapply(names(core_list), function(nm) {
  x <- core_list[[nm]]
  data.table(
    window_type = nm,
    denominator = nrow(x),
    any_six_drug_n = sum(x$vasopressor_any),
    norepinephrine_n = sum(x$norepinephrine_any),
    epinephrine_n = sum(x$epinephrine_any),
    vasopressin_n = sum(x$vasopressin_any),
    dopamine_n = sum(x$dopamine_any),
    dobutamine_n = sum(x$dobutamine_any),
    phenylephrine_n = sum(x$phenylephrine_any),
    active_input_rows = sum(x$vasoactive_active_row_n)
  )
}))
fwrite(
  pressor_coverage_qc,
  file.path(qc_out, "vasoactive_six_drug_coverage_QC.csv")
)

selection_qc <- merge(
  strict_base[, .(
    stay_id, tuple_observed, age_at_admission, pf_ratio
  )],
  index_core[, .(
    stay_id, gcs_worst, map_min, vasopressor_any, platelet_min,
    creatinine_max, height_cm, pbw_kg, complete_hsc
  )],
  by = "stay_id", all.x = TRUE, sort = FALSE
)
selection_aggregate <- selection_qc[, .(
  n = .N,
  age_available_n = sum(!is.na(age_at_admission)),
  age_mean = mean(age_at_admission, na.rm = TRUE),
  pf_available_n = sum(!is.na(pf_ratio)),
  pf_mean = mean(pf_ratio, na.rm = TRUE),
  gcs_available_n = sum(!is.na(gcs_worst)),
  map_available_n = sum(!is.na(map_min)),
  pressor_any_n = sum(vasopressor_any),
  platelet_available_n = sum(!is.na(platelet_min)),
  creatinine_available_n = sum(!is.na(creatinine_max)),
  height_available_n = sum(!is.na(height_cm)),
  pbw_available_n = sum(!is.na(pbw_kg)),
  complete_hsc_n = sum(complete_hsc)
), by = tuple_observed]
fwrite(
  selection_aggregate,
  file.path(qc_out, "selection_known_core_by_tuple_observation.csv")
)

map_range_feasibility <- rbindlist(lapply(list(
  prediction_time_hsc = prediction_bounds,
  index_known_selection = index_bounds
), function(bounds) {
  z <- merge(map_rows, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  z[, in_window :=
    charttime_epoch >= window_start_epoch & charttime_epoch <= window_end_epoch &
      !is.na(storetime_epoch) & storetime_epoch <= window_end_epoch & unit_valid]
  data.table(
    candidate_rows_in_window = sum(z$in_window),
    harmonized_1_250_rows = sum(
      z$in_window & !is.na(z$value_num) & z$value_num >= 1 & z$value_num <= 250
    ),
    excluded_251_299_rows = sum(z$in_window & z$official_wide_range_251_299),
    stays_with_251_299 = uniqueN(z[in_window & official_wide_range_251_299]$stay_id)
  )
}), idcol = "window_type")
fwrite(
  map_range_feasibility,
  file.path(qc_out, "map_harmonized_range_QC.csv")
)

# Aggregate cache provenance: paths/hashes/counts only, never row identifiers.
fwrite(
  cache_manifest,
  file.path(qc_out, "severity_input_cache_manifest.csv")
)

# Final leakage guard covers private artifacts and all aggregate CSV headers.
qc_csv <- list.files(qc_out, pattern = "\\.csv$", full.names = TRUE)
qc_headers <- rbindlist(lapply(qc_csv, function(f) {
  data.table(file = basename(f), column = names(fread(f, nrows = 0L)))
}))
identifier_headers <- c(
  "subject_id", "hadm_id", "stay_id", "labevent_id", "orderid",
  "caregiver_id", "specimen_id", "patient_id", "person_id"
)
leakage_guard <- data.table(
  check = c(
    "upstream_index_has_no_outcome_like_columns",
    "upstream_exposure_has_no_outcome_like_columns",
    "prediction_hsc_has_no_outcome_or_future_admin_columns",
    "index_selection_has_no_outcome_or_post_index_exposure_columns",
    "aggregate_qc_has_no_row_identifier_columns",
    "admissions_table_never_opened",
    "native_oasis_not_executed",
    "separate_native_oasis_gate_passes_outcome_guard",
    "input_cache_all_sources_reached_eof",
    "input_cache_all_official_sha256_match",
    "input_cache_has_exact_d053_sources"
  ),
  pass = c(
    !any(grepl(forbidden_source_pattern, names(index_source), ignore.case = TRUE)),
    !any(grepl(forbidden_source_pattern, names(exposure_source), ignore.case = TRUE)),
    !any(grepl(forbidden_output_pattern, names(prediction_attached), ignore.case = TRUE)),
    !any(grepl(forbidden_output_pattern, names(index_attached), ignore.case = TRUE)) &&
      !any(c("prediction_time", "anchor_time", "pplat", "smp") %in% names(index_attached)),
    !any(qc_headers$column %chin% identifier_headers),
    TRUE,
    identical(native_feasibility$executed_in_phase2b[[1L]], FALSE),
    identical(native_feasibility$separate_gate_status[[1L]], "PASS") &&
      identical(
        require_gate_value(native_oasis_gate, "actual_outcome_fields_read"),
        "FALSE"
      ),
    all(cache_manifest$reached_eof == TRUE),
    all(cache_manifest$official_sha256_match == TRUE),
    setequal(
      cache_manifest$source_name,
      c("chartevents", "labevents", "inputevents", "omr")
    )
  )
)
if (any(!leakage_guard$pass)) stop("Final MIMIC severity leakage guard failed.")
fwrite(leakage_guard, file.path(qc_out, "outcome_leakage_guard.csv"))

summary_lines <- c(
  "# MIMIC-IV harmonized pre-prediction severity-core QC",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("- Locked configuration: ", LOCKED$version),
  paste0("- Strict index cohort / index-known selection core: ", nrow(index_core)),
  paste0("- Complete-tuple prediction-time HSC: ", nrow(prediction_core)),
  paste0("- GCS available at prediction time: ", sum(!is.na(prediction_core$gcs_worst))),
  paste0("- MAP available at prediction time: ", sum(!is.na(prediction_core$map_min))),
  paste0("- Six-drug active vasoactive exposure at prediction time: ", sum(prediction_core$vasopressor_any)),
  paste0("- Platelet available at prediction time: ", sum(!is.na(prediction_core$platelet_min))),
  paste0("- Creatinine available at prediction time: ", sum(!is.na(prediction_core$creatinine_max))),
  paste0("- Complete HSC at prediction time: ", sum(prediction_core$complete_hsc)),
  paste0("- Primary chartevents+OMR<=5y height/PBW at prediction time: ", sum(!is.na(prediction_core$pbw_kg))),
  paste0("- OMR<=1y fallback height/PBW sensitivity at prediction time: ", sum(!is.na(prediction_core$pbw_kg_omr_1y_fallback))),
  paste0("- Chartevents-only height/PBW sensitivity at prediction time: ", sum(!is.na(prediction_core$pbw_kg_chartevents_only))),
  paste0("- Component-complete primary / OMR<=1y / chartevents-only: ", paste(
    height_component_feasibility_qc[
      window_type == "prediction_time_hsc",
      c(
        component_complete_omr_5y_fallback_n,
        component_complete_omr_1y_fallback_n,
        component_complete_chartevents_only_n
      )
    ],
    collapse = " / "
  )),
  "- GCS uses only same-charttime eye/verbal/motor components; ET/Trach/intubation text and value/valuenum conflicts remain unscorable.",
  "- MAP range is harmonized to 1-250 mmHg; excluded 251-299 rows are preserved as outcome-blind feasibility counts.",
  "- Every selected clinical HSC measurement and its storetime are no later than prediction/index; missing storetime is never backfilled.",
  "- The primary vasoactive indicator requires a positive rate, active overlapping interval, available storetime, and non-cancelled/non-rewritten status for six locked drugs.",
  "- Height 226730 cm is preferred over 226707 inch; D053 then permits only an OMR height dated 1-1826 days before index. The <=366-day and chartevents-only definitions are named sensitivities.",
  "- All four HSC sources reached EOF, passed gzip/CSV checks, and matched the MIMIC-IV v3.1 official SHA256 manifest.",
  "- D048/U004 is resolved by a separate outcome-free OASIS PASS artifact; its native first-day score is never substituted for time-aligned HSC or entered into index-time S0.",
  "- No admissions, mortality, discharge, survival, effect estimate, or model-performance data were read.",
  "- Row-level outputs are confined to analysis_rebuild_v1/private/mimic.",
  "",
  "BUILD_COMPLETE"
)
summary_path <- file.path(qc_out, "mimic_severity_core_QC.md")
writeLines(summary_lines, summary_path, useBytes = TRUE)

required_qc <- c(
  "locked_item_metadata_QC.csv", "locked_labitem_metadata_QC.csv",
  "chartevents_mapping_unit_QC.csv", "gcs_value_valuenum_QC.csv",
  "labevents_mapping_unit_QC.csv", "vasoactive_inputevents_mapping_QC.csv",
  "vasoactive_rate_distribution_QC.csv", "omr_height_mapping_QC.csv",
  "selected_chartevent_warning_QC.csv",
  "severity_core_synthetic_rule_tests.csv", "severity_core_invariant_tests.csv",
  "severity_core_timing_QC.csv",
  "severity_core_missingness.csv", "severity_core_value_distribution.csv",
  "severity_core_source_frequency.csv", "vasoactive_six_drug_coverage_QC.csv",
  "height_source_coverage_QC.csv", "height_chartevents_omr_agreement_QC.csv",
  "height_component_feasibility_QC.csv",
  "selection_known_core_by_tuple_observation.csv", "map_harmonized_range_QC.csv",
  "severity_input_cache_manifest.csv", "native_oasis_feasibility_provenance.csv",
  "outcome_leakage_guard.csv", "mimic_severity_core_QC.md"
)
if (any(!file.exists(file.path(qc_out, required_qc)))) {
  stop("Required MIMIC severity QC output is missing.")
}
if (!identical(tail(readLines(summary_path, warn = FALSE), 1L), "BUILD_COMPLETE")) {
  stop("MIMIC severity QC summary lacks BUILD_COMPLETE sentinel.")
}

# Atomic final completion gate. Its absence invalidates any row-level files
# left by an interrupted run.
completion <- data.table(
  status = "PASS",
  config_version = LOCKED$version,
  completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  script_sha256 = sha256_file(script_path),
  helper_sha256 = sha256_file(filter_helper),
  phase1_gate_sha256 = sha256_file(phase1_gate_path),
  phase2_gate_sha256 = sha256_file(phase2_gate_path),
  preflight_inventory_sha256 = sha256_file(preflight_inventory_path),
  native_oasis_gate_sha256 = sha256_file(native_oasis_gate_path),
  input_index_rds_sha256 = index_sha256,
  input_exposure_rds_sha256 = exposure_sha256,
  input_cache_gate_sha256 = sha256_file(cache_gate_path),
  input_cache_manifest_sha256 = sha256_file(cache_manifest_path),
  prediction_hsc_n = nrow(prediction_attached),
  index_selection_n = nrow(index_attached),
  component_complete_omr_5y_n = height_component_feasibility_qc[
    window_type == "prediction_time_hsc",
    component_complete_omr_5y_fallback_n
  ],
  component_complete_omr_1y_n = height_component_feasibility_qc[
    window_type == "prediction_time_hsc",
    component_complete_omr_1y_fallback_n
  ],
  component_complete_chartevents_only_n = height_component_feasibility_qc[
    window_type == "prediction_time_hsc",
    component_complete_chartevents_only_n
  ],
  prediction_hsc_rds_sha256 = sha256_file(prediction_rds),
  index_selection_rds_sha256 = sha256_file(selection_rds),
  native_feasibility_rds_sha256 = sha256_file(native_rds),
  all_invariants_pass = all(invariants$pass),
  outcome_leakage_guard_pass = all(leakage_guard$pass),
  cache_all_reached_eof = all(cache_manifest$reached_eof == TRUE),
  cache_all_official_sha256_match = all(
    cache_manifest$official_sha256_match == TRUE
  ),
  native_benchmark_status = native_feasibility$status[[1L]],
  summary_sentinel = "BUILD_COMPLETE"
)
fwrite(completion, completion_gate_tmp)
if (!file.rename(completion_gate_tmp, completion_gate)) {
  stop("Could not atomically publish the MIMIC Phase 2b completion gate.")
}

message("MIMIC-IV harmonized severity build complete (outcome-blind).")
message("  prediction-time HSC: ", nrow(prediction_attached))
message("  index-known selection core: ", nrow(index_attached))
message("  private outputs: ", private_out)
message("  aggregate QC: ", qc_out)
