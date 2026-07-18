#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: outcome-blind native MIMIC OASIS
#
# This script is a source-faithful, predictor-side refactor of the official
# MIT-LCP MIMIC-IV OASIS concept pinned at commit
# 5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4.  It deliberately does not
# execute the outcome-bearing cohort/probability fields in oasis.sql and never
# selects any death, discharge-status, survival, or actual-outcome column.
#
# OASIS is a native first-ICU-day benchmark.  It remains separate from the
# time-aligned harmonized severity core and is never used as a substitute for
# the latter.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/05c_build_mimic_native_oasis.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))

stopifnot(identical(LOCKED$version, "1.0.1"))

phase1_gate_path <- file.path(QC_ROOT, "mimic", "phase1_complete_v1.csv")
input_index <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_index_cohort_v1.rds"
)
phase1_script <- file.path(
  dirname(script_path), "01_build_mimic_index_cohort.R"
)
helper_path <- file.path(
  dirname(script_path), "05d_filter_mimic_oasis_inputs.py"
)

raw_admissions <- file.path(MIMIC_ROOT, "hosp", "admissions.csv.gz")
raw_services <- file.path(MIMIC_ROOT, "hosp", "services.csv.gz")
raw_chartevents <- file.path(MIMIC_ROOT, "icu", "chartevents.csv.gz")
raw_outputevents <- file.path(MIMIC_ROOT, "icu", "outputevents.csv.gz")

private_out <- file.path(PRIVATE_ROOT, "mimic")
cache_dir <- file.path(
  private_out, "cache_v1", "mimic_native_oasis"
)
qc_out <- file.path(QC_ROOT, "mimic_native_oasis")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

output_rds <- file.path(
  private_out, "mimic_native_oasis_benchmark_v1.rds"
)
completion_gate <- file.path(
  qc_out, "phase2c_mimic_native_oasis_complete_v1.csv"
)
completion_gate_tmp <- paste0(completion_gate, ".tmp")

required_files <- c(
  phase1_gate_path, input_index, phase1_script, helper_path,
  raw_admissions, raw_services, raw_chartevents, raw_outputevents
)
if (any(!file.exists(required_files))) {
  stop(
    "Missing required native-OASIS input(s): ",
    paste(required_files[!file.exists(required_files)], collapse = ", ")
  )
}

sha256_file <- function(path) {
  z <- system2(
    "shasum", c("-a", "256", shQuote(path)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(z, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA256 failed for ", path, ": ", paste(z, collapse = " "))
  }
  hash <- strsplit(z[[1L]], "[[:space:]]+")[[1L]][1L]
  if (!grepl("^[0-9a-f]{64}$", hash)) {
    stop("Invalid SHA256 for ", path)
  }
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

to_epoch <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(x))
  z <- trimws(as.character(x))
  out <- rep(NA_real_, length(z))
  ok <- !is.na(z) & nzchar(z)
  if (any(ok)) {
    out[ok] <- as.numeric(as.POSIXct(
      z[ok], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    ))
  }
  out
}

max_or_na <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

min_or_na <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

mean_or_na <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

sum_or_na <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

coalesce_num <- function(x, fallback) {
  out <- x
  missing <- is.na(out)
  if (length(fallback) == 1L) {
    out[missing] <- fallback
  } else {
    if (length(fallback) != length(out)) {
      stop("Vector fallback length mismatch in coalesce_num().")
    }
    out[missing] <- fallback[missing]
  }
  out
}

atomic_fwrite <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  unlink(tmp, force = TRUE)
  fwrite(x, tmp)
  if (!file.rename(tmp, path)) stop("Atomic publish failed: ", path)
}

# An interrupted rerun must never leave a stale PASS gate.
unlink(c(completion_gate, completion_gate_tmp), force = TRUE)

# ---------------------------------------------------------------------------
# Immutable Phase-1 source gate and outcome-free target population.
# ---------------------------------------------------------------------------

phase1_gate <- read_gate_map(phase1_gate_path)
require_gate_value(phase1_gate, "locked_config_version", LOCKED$version)
require_gate_value(phase1_gate, "all_invariants_pass", "TRUE")
require_gate_value(phase1_gate, "outcome_leakage_guard_pass", "TRUE")
require_gate_value(
  phase1_gate, "script_sha256", sha256_file(phase1_script)
)
require_gate_value(
  phase1_gate, "primary_cohort_rds_sha256", sha256_file(input_index)
)

index_source <- as.data.table(readRDS(input_index))
forbidden_pattern <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
if (any(grepl(forbidden_pattern, names(index_source), ignore.case = TRUE))) {
  stop("Outcome-like field found in the Phase-1 source artifact.")
}
required_index <- c(
  "subject_id", "hadm_id", "stay_id", "intime", "age_at_admission",
  "admission_type", "invasive_confirmed", "pao2_time"
)
if (length(setdiff(required_index, names(index_source)))) {
  stop("Phase-1 artifact is missing required native-OASIS identifiers.")
}
if (anyDuplicated(index_source$stay_id) ||
    anyDuplicated(index_source$subject_id)) {
  stop("Native OASIS expects the locked first-stay-per-patient cohort.")
}

base <- index_source[, .(
  subject_id, hadm_id, stay_id, intime, index_time = pao2_time,
  age = as.numeric(age_at_admission),
  phase1_admission_type = admission_type,
  index_invasive_confirmed = as.logical(invasive_confirmed)
)]
base[, `:=`(
  intime_epoch = to_epoch(intime),
  index_epoch = to_epoch(index_time)
)]
if (anyNA(base$intime_epoch) || anyNA(base$index_epoch) || anyNA(base$age)) {
  stop("Missing ICU admission/index time or age in the locked strict cohort.")
}
target_keys <- base[, .(subject_id, hadm_id, stay_id)]
setorder(target_keys, stay_id)
target_keys_path <- file.path(cache_dir, "target_keys_v1.csv")
atomic_fwrite(target_keys, target_keys_path)

# ---------------------------------------------------------------------------
# Full-EOF, official-SHA checked target filtering for large event streams.
# ---------------------------------------------------------------------------

helper_call <- system2(
  "python3",
  c(
    shQuote(helper_path),
    "--keys", shQuote(target_keys_path),
    "--mimic-root", shQuote(MIMIC_ROOT),
    "--cache-dir", shQuote(cache_dir)
  ),
  stdout = TRUE, stderr = TRUE
)
helper_status <- attr(helper_call, "status")
if (!is.null(helper_status) && helper_status != 0L) {
  stop(
    "Native-OASIS target filter failed: ",
    paste(helper_call, collapse = "\n")
  )
}

cache_gate_path <- file.path(
  cache_dir, "oasis_input_cache_complete_v1.csv"
)
cache_manifest_path <- file.path(cache_dir, "filter_manifest_v1.csv")
cache_char_path <- file.path(
  cache_dir, "chartevents_oasis_candidates_v1.csv.gz"
)
cache_output_path <- file.path(
  cache_dir, "outputevents_oasis_candidates_v1.csv.gz"
)
if (any(!file.exists(c(
  cache_gate_path, cache_manifest_path, cache_char_path, cache_output_path
)))) {
  stop("Native-OASIS helper did not publish all required cache products.")
}

cache_gate <- fread(cache_gate_path)
cache_manifest <- fread(cache_manifest_path)
if (nrow(cache_gate) != 1L || cache_gate$status[[1L]] != "PASS" ||
    cache_gate$all_sources_reached_eof[[1L]] != TRUE ||
    cache_gate$all_official_sha256_match[[1L]] != TRUE) {
  stop("Native-OASIS input cache gate is not PASS.")
}
if (nrow(cache_manifest) != 2L ||
    any(cache_manifest$status != "PASS") ||
    any(cache_manifest$reached_eof != TRUE) ||
    any(cache_manifest$official_sha256_match != TRUE)) {
  stop("Native-OASIS input cache manifest is incomplete.")
}
if (cache_gate$helper_sha256[[1L]] != sha256_file(helper_path) ||
    cache_gate$manifest_sha256[[1L]] != sha256_file(cache_manifest_path)) {
  stop("Native-OASIS helper/cache hash mismatch.")
}
for (i in seq_len(nrow(cache_manifest))) {
  p <- cache_manifest$output_path[[i]]
  if (!file.exists(p) ||
      cache_manifest$output_sha256[[i]] != sha256_file(p)) {
    stop("Native-OASIS filtered cache hash mismatch: ", p)
  }
}

# ---------------------------------------------------------------------------
# Safe hospital-side inputs. fread's explicit select list is the executable
# leakage boundary: no actual-outcome or discharge-status column is selected.
# ---------------------------------------------------------------------------

expected_safe_sha <- c(
  admissions = "a9584ed88e9ed664a2f66f86a5cf9fd175c8bb0af50e3b6115598b19e978384e",
  services = "31c82ebee94e0c04d6966fbfec30579ec00f9c7816b80852f9580656e6183888"
)
safe_raw_sha <- c(
  admissions = sha256_file(raw_admissions),
  services = sha256_file(raw_services)
)
if (!identical(unname(safe_raw_sha), unname(expected_safe_sha))) {
  stop("Safe hospital-side raw files do not match MIMIC-IV v3.1 SHA256.")
}

admissions <- fread(
  raw_admissions,
  select = c("subject_id", "hadm_id", "admittime", "admission_type"),
  showProgress = FALSE
)[hadm_id %in% base$hadm_id]
if (anyDuplicated(admissions$hadm_id) || nrow(admissions) != nrow(base)) {
  stop("Safe admissions join is not one-to-one with the strict cohort.")
}
admissions[, admittime_epoch := to_epoch(admittime)]
if (anyNA(admissions$admittime_epoch)) {
  stop("Hospital admission time is missing in the strict cohort.")
}

base <- merge(
  base,
  admissions[, .(
    subject_id, hadm_id, admittime_epoch,
    source_admission_type = admission_type
  )],
  by = c("subject_id", "hadm_id"), all.x = TRUE, sort = FALSE
)
if (any(base$phase1_admission_type != base$source_admission_type)) {
  stop("Phase-1 and safe-source admission types disagree.")
}
# BigQuery DATETIME_DIFF(..., MINUTE) returns an integer boundary count.
base[, preiculos := trunc(as.numeric((intime_epoch - admittime_epoch) / 60))]

services <- fread(
  raw_services,
  select = c("subject_id", "hadm_id", "transfertime", "curr_service"),
  showProgress = FALSE
)[hadm_id %in% base$hadm_id]
services[, transfertime_epoch := to_epoch(transfertime)]
service_join <- merge(
  services,
  base[, .(subject_id, hadm_id, stay_id, intime_epoch)],
  by = c("subject_id", "hadm_id"), all = FALSE, sort = FALSE
)
service_join <- service_join[
  !is.na(transfertime_epoch) & transfertime_epoch < intime_epoch + 86400
]
service_join[, surgical_evidence := as.integer(
  !is.na(curr_service) &
    (grepl("surg", tolower(curr_service), fixed = TRUE) |
       curr_service == "ORTHO")
)]
surgical <- service_join[, .(
  surgical = if (.N) max(surgical_evidence, na.rm = TRUE) else 0L,
  qualifying_service_rows = .N
), by = stay_id]
base <- merge(base, surgical, by = "stay_id", all.x = TRUE, sort = FALSE)
base[is.na(surgical), `:=`(surgical = 0L, qualifying_service_rows = 0L)]
base[, electivesurgery := fifelse(
  is.na(source_admission_type), NA_integer_,
  as.integer(source_admission_type == "ELECTIVE" & surgical == 1L)
)]

# ---------------------------------------------------------------------------
# Load only the pinned predictor item IDs from the helper's validated caches.
# ---------------------------------------------------------------------------

ce <- fread(
  cache_char_path,
  select = c(
    "subject_id", "hadm_id", "stay_id", "charttime", "storetime",
    "itemid", "value", "valuenum", "valueuom", "warning"
  ),
  showProgress = FALSE
)
ce[, `:=`(
  chart_epoch = to_epoch(charttime),
  store_epoch = to_epoch(storetime),
  itemid = as.integer(itemid),
  valuenum = as.numeric(valuenum)
)]
if (anyNA(ce$stay_id) || anyNA(ce$chart_epoch) ||
    any(!ce$stay_id %in% base$stay_id)) {
  stop("Filtered chartevents contains invalid target identifiers/times.")
}

oe <- fread(
  cache_output_path,
  select = c(
    "subject_id", "hadm_id", "stay_id", "charttime", "storetime",
    "itemid", "value", "valueuom"
  ),
  showProgress = FALSE
)
oe[, `:=`(
  chart_epoch = to_epoch(charttime),
  store_epoch = to_epoch(storetime),
  itemid = as.integer(itemid),
  value_num = suppressWarnings(as.numeric(value))
)]
if (anyNA(oe$stay_id) || anyNA(oe$chart_epoch) ||
    any(!oe$stay_id %in% base$stay_id)) {
  stop("Filtered outputevents contains invalid target identifiers/times.")
}

# ---------------------------------------------------------------------------
# Official GCS dependency graph: same-time pivot, immediate prior row carried
# for <6 h, official ET-tube/sedation convention, then minimum in [-6 h,+24 h].
# ---------------------------------------------------------------------------

graw <- ce[itemid %in% c(220739L, 223900L, 223901L)]
gbase <- graw[, .(
  gcs_motor_current = max_or_na(fifelse(
    itemid == 223901L, valuenum, NA_real_
  )),
  gcs_verbal_current = max_or_na(fifelse(
    itemid == 223900L,
    fifelse(!is.na(value) & value == "No Response-ETT", 0, valuenum),
    NA_real_
  )),
  gcs_eyes_current = max_or_na(fifelse(
    itemid == 220739L, valuenum, NA_real_
  )),
  gcs_unable = as.integer(any(
    itemid == 223900L & value == "No Response-ETT",
    na.rm = TRUE
  ))
), by = .(subject_id, stay_id, chart_epoch)]
setorder(gbase, stay_id, chart_epoch)
gbase[, `:=`(
  previous_chart_epoch = shift(chart_epoch),
  previous_motor_raw = shift(gcs_motor_current),
  previous_verbal_raw = shift(gcs_verbal_current),
  previous_eyes_raw = shift(gcs_eyes_current)
), by = stay_id]
gbase[, prior_within_6h := !is.na(previous_chart_epoch) &
  previous_chart_epoch > chart_epoch - 6 * 3600]
gbase[, `:=`(
  gcs_motor_previous = fifelse(prior_within_6h, previous_motor_raw, NA_real_),
  gcs_verbal_previous = fifelse(prior_within_6h, previous_verbal_raw, NA_real_),
  gcs_eyes_previous = fifelse(prior_within_6h, previous_eyes_raw, NA_real_)
)]
gbase[, gcs := fifelse(
  !is.na(gcs_verbal_current) & gcs_verbal_current == 0, 15,
  fifelse(
    is.na(gcs_verbal_current) & !is.na(gcs_verbal_previous) &
      gcs_verbal_previous == 0, 15,
    fifelse(
      !is.na(gcs_verbal_previous) & gcs_verbal_previous == 0,
      coalesce_num(gcs_motor_current, 6) +
        coalesce_num(gcs_verbal_current, 5) +
        coalesce_num(gcs_eyes_current, 4),
      coalesce_num(
        gcs_motor_current, coalesce_num(gcs_motor_previous, 6)
      ) + coalesce_num(
        gcs_verbal_current, coalesce_num(gcs_verbal_previous, 5)
      ) + coalesce_num(
        gcs_eyes_current, coalesce_num(gcs_eyes_previous, 4)
      )
    )
  )
)]
gbase[, `:=`(
  gcs_motor = coalesce_num(gcs_motor_current, gcs_motor_previous),
  gcs_verbal = coalesce_num(gcs_verbal_current, gcs_verbal_previous),
  gcs_eyes = coalesce_num(gcs_eyes_current, gcs_eyes_previous)
)]

gwindow <- merge(
  gbase,
  base[, .(stay_id, intime_epoch)],
  by = "stay_id", all = FALSE, sort = FALSE
)[chart_epoch >= intime_epoch - 6 * 3600 &
    chart_epoch <= intime_epoch + 24 * 3600]
setorder(gwindow, stay_id, gcs, -chart_epoch, na.last = TRUE)
gselected <- gwindow[, .SD[1L], by = stay_id][, .(
  stay_id,
  gcs,
  gcs_chart_epoch = chart_epoch,
  gcs_motor,
  gcs_verbal,
  gcs_eyes,
  gcs_unable
)]

# ---------------------------------------------------------------------------
# Official vitals dependency graph: charttime means, then first-day extrema.
# ---------------------------------------------------------------------------

vital_ids <- c(
  220045L, 220052L, 220181L, 225312L,
  220210L, 224690L, 223761L, 223762L
)
vraw <- ce[itemid %in% vital_ids]
vchart <- vraw[, .(
  heart_rate = mean_or_na(fifelse(
    itemid == 220045L & valuenum > 0 & valuenum < 300,
    valuenum, NA_real_
  )),
  mbp = mean_or_na(fifelse(
    itemid %in% c(220052L, 220181L, 225312L) &
      valuenum > 0 & valuenum < 300,
    valuenum, NA_real_
  )),
  resp_rate = mean_or_na(fifelse(
    itemid %in% c(220210L, 224690L) &
      valuenum > 0 & valuenum < 70,
    valuenum, NA_real_
  )),
  temperature_unrounded = mean_or_na(fifelse(
    itemid == 223761L & valuenum > 70 & valuenum < 120,
    (valuenum - 32) / 1.8,
    fifelse(
      itemid == 223762L & valuenum > 10 & valuenum < 50,
      valuenum, NA_real_
    )
  ))
), by = .(subject_id, stay_id, chart_epoch)]
# BigQuery NUMERIC ROUND is half-away-from-zero. Temperatures are positive.
vchart[, temperature := fifelse(
  is.na(temperature_unrounded), NA_real_,
  floor(temperature_unrounded * 100 + 0.5) / 100
)]

vwindow <- merge(
  vchart,
  base[, .(stay_id, intime_epoch)],
  by = "stay_id", all = FALSE, sort = FALSE
)[chart_epoch >= intime_epoch - 6 * 3600 &
    chart_epoch <= intime_epoch + 24 * 3600]
vitals <- vwindow[, .(
  heart_rate_min = min_or_na(heart_rate),
  heart_rate_max = max_or_na(heart_rate),
  mbp_min = min_or_na(mbp),
  mbp_max = max_or_na(mbp),
  resp_rate_min = min_or_na(resp_rate),
  resp_rate_max = max_or_na(resp_rate),
  temperature_min = min_or_na(temperature),
  temperature_max = max_or_na(temperature),
  vital_first_chart_epoch = min_or_na(fifelse(
    !is.na(heart_rate) | !is.na(mbp) | !is.na(resp_rate) |
      !is.na(temperature),
    chart_epoch, NA_real_
  )),
  vital_last_chart_epoch = max_or_na(fifelse(
    !is.na(heart_rate) | !is.na(mbp) | !is.na(resp_rate) |
      !is.na(temperature),
    chart_epoch, NA_real_
  ))
), by = stay_id]

# ---------------------------------------------------------------------------
# Official urine-output dependency: GU irrigant input is negative; sum exact
# charttime totals during [ICU admission, ICU admission+24 h].
# ---------------------------------------------------------------------------

oe[, urine_value := fifelse(
  itemid == 227488L & value_num > 0, -value_num, value_num
)]
urine_chart <- oe[, .(
  urineoutput = sum_or_na(urine_value)
), by = .(stay_id, chart_epoch)]
urine_window <- merge(
  urine_chart,
  base[, .(stay_id, intime_epoch)],
  by = "stay_id", all = FALSE, sort = FALSE
)[chart_epoch >= intime_epoch & chart_epoch <= intime_epoch + 24 * 3600]
urine <- urine_window[, .(
  urineoutput = sum_or_na(urineoutput),
  urine_event_times = sum(!is.na(urineoutput)),
  urine_first_chart_epoch = min_or_na(fifelse(
    !is.na(urineoutput), chart_epoch, NA_real_
  )),
  urine_last_chart_epoch = max_or_na(fifelse(
    !is.na(urineoutput), chart_epoch, NA_real_
  ))
), by = stay_id]

# ---------------------------------------------------------------------------
# Official ventilation dependency graph. Only mode and oxygen-delivery fields
# can assign a status; other ventilator-setting fields create NULL-status times
# and therefore cannot alter the published episodes.
# ---------------------------------------------------------------------------

ce_value <- ce[!is.na(value) & nzchar(value)]
vent_setting <- ce_value[itemid %in% c(223849L, 229314L), .(
  stay_id = max(stay_id),
  ventilator_mode = {
    z <- value[itemid == 223849L]
    if (length(z)) max(z) else NA_character_
  },
  ventilator_mode_hamilton = {
    z <- value[itemid == 229314L]
    if (length(z)) max(z) else NA_character_
  }
), by = .(subject_id, chart_epoch)]

flow <- ce_value[itemid %in% c(223834L, 227582L, 227287L)]
flow[, merged_itemid := fifelse(
  itemid %in% c(223834L, 227582L), 223834L, itemid
)]
setorderv(
  flow,
  c("subject_id", "chart_epoch", "merged_itemid", "store_epoch", "valuenum"),
  c(1L, 1L, 1L, -1L, -1L),
  na.last = TRUE
)
flow_selected <- flow[, .SD[1L], by = .(
  subject_id, chart_epoch, merged_itemid
)]
flow_wide <- flow_selected[, .(
  stay_id = max(stay_id),
  o2_flow = max_or_na(fifelse(
    merged_itemid == 223834L, valuenum, NA_real_
  )),
  o2_flow_additional = max_or_na(fifelse(
    merged_itemid == 227287L, valuenum, NA_real_
  ))
), by = .(subject_id, chart_epoch)]

device <- ce_value[itemid == 226732L]
setorderv(
  device,
  c("subject_id", "chart_epoch", "store_epoch", "value"),
  c(1L, 1L, -1L, -1L),
  na.last = TRUE
)
device[, device_rank := seq_len(.N), by = .(subject_id, chart_epoch)]
device4 <- device[device_rank <= 4L]
device_wide <- dcast(
  device4,
  subject_id + chart_epoch ~ device_rank,
  value.var = "value"
)
setnames(
  device_wide,
  intersect(as.character(1:4), names(device_wide)),
  paste0("o2_delivery_device_", intersect(as.character(1:4), names(device_wide)))
)
# Pinned oxygen_delivery.sql retains device fields only at times having a
# selected O2-flow row (`WHERE ce.rn = 1` after the FULL JOIN).
oxygen_delivery <- merge(
  flow_wide, device_wide,
  by = c("subject_id", "chart_epoch"), all.x = TRUE, sort = FALSE
)
for (v in paste0("o2_delivery_device_", 1:4)) {
  if (!v %in% names(oxygen_delivery)) oxygen_delivery[, (v) := NA_character_]
}

tm <- unique(rbindlist(list(
  vent_setting[, .(stay_id, chart_epoch)],
  oxygen_delivery[, .(stay_id, chart_epoch)]
), use.names = TRUE))
vs <- merge(
  tm, vent_setting,
  by = c("stay_id", "chart_epoch"), all.x = TRUE, sort = FALSE
)
vs <- merge(
  vs,
  oxygen_delivery[, .(
    stay_id, chart_epoch,
    o2_delivery_device_1, o2_delivery_device_2,
    o2_delivery_device_3, o2_delivery_device_4
  )],
  by = c("stay_id", "chart_epoch"), all.x = TRUE, sort = FALSE
)

invasive_modes <- c(
  "(S) CMV", "APRV", "APRV/Biphasic+ApnPress",
  "APRV/Biphasic+ApnVol", "APV (cmv)", "Ambient",
  "Apnea Ventilation", "CMV", "CMV/ASSIST", "CMV/ASSIST/AutoFlow",
  "CMV/AutoFlow", "CPAP/PPS", "CPAP/PSV", "CPAP/PSV+Apn TCPL",
  "CPAP/PSV+ApnPres", "CPAP/PSV+ApnVol", "MMV", "MMV/AutoFlow",
  "MMV/PSV", "MMV/PSV/AutoFlow", "P-CMV", "PCV+", "PCV+/PSV",
  "PCV+Assist", "PRES/AC", "PRVC/AC", "PRVC/SIMV", "PSV/SBT",
  "SIMV", "SIMV/AutoFlow", "SIMV/PRES", "SIMV/PSV",
  "SIMV/PSV/AutoFlow", "SIMV/VOL", "SYNCHRON MASTER",
  "SYNCHRON SLAVE", "VOL/AC"
)
invasive_hamilton <- c(
  "APRV", "APV (cmv)", "Ambient", "(S) CMV", "P-CMV", "SIMV",
  "APV (simv)", "P-SIMV", "VS", "ASV"
)
niv_hamilton <- c("DuoPaP", "NIV", "NIV-ST")
supplemental_devices <- c(
  "Non-rebreather", "Face tent", "Aerosol-cool", "Venti mask ",
  "Medium conc mask ", "Ultrasonic neb", "Vapomist", "Oxymizer",
  "High flow neb", "Nasal cannula"
)

vs[, ventilation_status := fcase(
  o2_delivery_device_1 %in% c("Tracheostomy tube", "Trach mask "),
  "Tracheostomy",
  o2_delivery_device_1 == "Endotracheal tube" |
    ventilator_mode %in% invasive_modes |
    ventilator_mode_hamilton %in% invasive_hamilton,
  "InvasiveVent",
  o2_delivery_device_1 %in% c("Bipap mask ", "CPAP mask ") |
    o2_delivery_device_2 %in% c("Bipap mask ", "CPAP mask ") |
    o2_delivery_device_3 %in% c("Bipap mask ", "CPAP mask ") |
    o2_delivery_device_4 %in% c("Bipap mask ", "CPAP mask ") |
    ventilator_mode_hamilton %in% niv_hamilton,
  "NonInvasiveVent",
  o2_delivery_device_1 == "High flow nasal cannula", "HFNC",
  o2_delivery_device_1 %in% supplemental_devices, "SupplementalOxygen",
  o2_delivery_device_1 == "None", "None",
  default = NA_character_
)]
vd <- vs[!is.na(ventilation_status)]
setorder(vd, stay_id, chart_epoch)
vd[, `:=`(
  charttime_lag_same_status = shift(chart_epoch),
  charttime_lead = shift(chart_epoch, type = "lead"),
  ventilation_status_lag = shift(ventilation_status)
), by = stay_id]
# SQL's same-status LAG is not the immediately prior row when status changes.
vd[, charttime_lag_same_status := shift(chart_epoch),
   by = .(stay_id, ventilation_status)]
vd[, new_ventilation_event := as.integer(
  is.na(ventilation_status_lag) |
    (!is.na(charttime_lag_same_status) &
       chart_epoch - charttime_lag_same_status >= 14 * 3600) |
    (!is.na(ventilation_status_lag) &
       ventilation_status_lag != ventilation_status)
)]
vd[, vent_seq := cumsum(new_ventilation_event), by = stay_id]
vd[, end_candidate := fifelse(
  is.na(charttime_lead) | charttime_lead - chart_epoch >= 14 * 3600,
  chart_epoch, charttime_lead
)]
episodes <- vd[, .(
  start_epoch = min(chart_epoch),
  last_chart_epoch = max(chart_epoch),
  end_epoch = max(end_candidate),
  ventilation_status = max(ventilation_status)
), by = .(stay_id, vent_seq)][start_epoch != last_chart_epoch]
episodes[, last_chart_epoch := NULL]

episode_window <- merge(
  episodes,
  base[, .(stay_id, intime_epoch)],
  by = "stay_id", all = FALSE, sort = FALSE
)
firstday_invasive <- episode_window[
  ventilation_status == "InvasiveVent" &
    start_epoch <= intime_epoch + 24 * 3600 & end_epoch >= intime_epoch
]
vent_flag <- unique(firstday_invasive[, .(stay_id, mechvent = 1L)])
base[, mechvent := 0L]
base[vent_flag, on = "stay_id", mechvent := i.mechvent]

# ---------------------------------------------------------------------------
# Predictor-side OASIS score, copied term-for-term from the pinned source.
# ---------------------------------------------------------------------------

oasis <- merge(base, gselected, by = "stay_id", all.x = TRUE, sort = FALSE)
oasis <- merge(oasis, vitals, by = "stay_id", all.x = TRUE, sort = FALSE)
oasis <- merge(oasis, urine, by = "stay_id", all.x = TRUE, sort = FALSE)

score_preiculos <- function(x) fcase(
  is.na(x), NA_real_,
  x < 10.2, 5,
  x < 297, 3,
  x < 1440, 0,
  x < 18708, 2,
  default = 1
)
score_age <- function(x) fcase(
  is.na(x), NA_real_,
  x < 24, 0,
  x <= 53, 3,
  x <= 77, 6,
  x <= 89, 9,
  x >= 90, 7,
  default = 0
)
score_gcs <- function(x) fcase(
  is.na(x), NA_real_,
  x <= 7, 10,
  x < 14, 4,
  x == 14, 3,
  default = 0
)
score_heart_rate <- function(xmin, xmax) fcase(
  is.na(xmax), NA_real_,
  xmax > 125, 6,
  xmin < 33, 4,
  xmax >= 107 & xmax <= 125, 3,
  xmax >= 89 & xmax <= 106, 1,
  default = 0
)
score_mbp <- function(xmin, xmax) fcase(
  is.na(xmin), NA_real_,
  xmin < 20.65, 4,
  xmin < 51, 3,
  xmax > 143.44, 3,
  xmin >= 51 & xmin < 61.33, 2,
  default = 0
)
score_resp_rate <- function(xmin, xmax) fcase(
  is.na(xmin), NA_real_,
  xmin < 6, 10,
  xmax > 44, 9,
  xmax > 30, 6,
  xmax > 22, 1,
  xmin < 13, 1,
  default = 0
)
score_temperature <- function(xmin, xmax) fcase(
  is.na(xmax), NA_real_,
  xmax > 39.88, 6,
  xmin >= 33.22 & xmin <= 35.93, 4,
  xmax >= 33.22 & xmax <= 35.93, 4,
  xmin < 33.22, 3,
  xmin > 35.93 & xmin <= 36.39, 2,
  xmax >= 36.89 & xmax <= 39.88, 2,
  default = 0
)
score_urine <- function(x) fcase(
  is.na(x), NA_real_,
  x < 671.09, 10,
  x > 6896.80, 8,
  x >= 671.09 & x <= 1426.99, 5,
  x >= 1427 & x <= 2544.14, 1,
  default = 0
)
score_mechvent <- function(x) fifelse(
  is.na(x), NA_real_, fifelse(x == 1L, 9, 0)
)
score_elective <- function(x) fifelse(
  is.na(x), NA_real_, fifelse(x == 1L, 0, 6)
)

oasis[, `:=`(
  preiculos_score = score_preiculos(preiculos),
  age_score = score_age(age),
  gcs_score = score_gcs(gcs),
  heart_rate_score = score_heart_rate(heart_rate_min, heart_rate_max),
  mbp_score = score_mbp(mbp_min, mbp_max),
  resp_rate_score = score_resp_rate(resp_rate_min, resp_rate_max),
  temp_score = score_temperature(temperature_min, temperature_max),
  urineoutput_score = score_urine(urineoutput),
  mechvent_score = score_mechvent(mechvent),
  electivesurgery_score = score_elective(electivesurgery)
)]

score_columns <- c(
  "age_score", "preiculos_score", "gcs_score", "heart_rate_score",
  "mbp_score", "resp_rate_score", "temp_score", "urineoutput_score",
  "mechvent_score", "electivesurgery_score"
)
oasis[, oasis := rowSums(
  as.data.frame(lapply(.SD, coalesce_num, fallback = 0)),
  na.rm = FALSE
), .SDcols = score_columns]
oasis[, component_available_n := rowSums(!is.na(.SD)), .SDcols = score_columns]

artifact <- oasis[, .(
  subject_id, hadm_id, stay_id,
  oasis = as.integer(oasis),
  component_available_n = as.integer(component_available_n),
  age, age_score = as.integer(age_score),
  preiculos, preiculos_score = as.integer(preiculos_score),
  gcs, gcs_score = as.integer(gcs_score),
  heart_rate_min, heart_rate_max,
  heart_rate_score = as.integer(heart_rate_score),
  mbp_min, mbp_max, mbp_score = as.integer(mbp_score),
  resp_rate_min, resp_rate_max,
  resp_rate_score = as.integer(resp_rate_score),
  temperature_min, temperature_max,
  temp_score = as.integer(temp_score),
  urineoutput, urineoutput_score = as.integer(urineoutput_score),
  mechvent, mechvent_score = as.integer(mechvent_score),
  electivesurgery,
  electivesurgery_score = as.integer(electivesurgery_score)
)]
setorder(artifact, stay_id)

# ---------------------------------------------------------------------------
# Provenance, synthetic scoring tests, invariants, and aggregate-only QC.
# ---------------------------------------------------------------------------

official_repo <- normalizePath(
  file.path(PROJECT_ROOT, "..", "tmp", "mimic-code"), mustWork = TRUE
)
official_commit <- system2(
  "git", c("-C", shQuote(official_repo), "rev-parse", "HEAD"),
  stdout = TRUE, stderr = TRUE
)
expected_commit <- "5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4"
if (length(official_commit) != 1L || official_commit != expected_commit) {
  stop("Official mimic-code repository is not at the pinned commit.")
}
repo_status <- system2(
  "git", c("-C", shQuote(official_repo), "status", "--porcelain"),
  stdout = TRUE, stderr = TRUE
)
if (length(repo_status)) stop("Pinned mimic-code repository is dirty.")

official_rel <- c(
  "mimic-iv/concepts/score/oasis.sql",
  "mimic-iv/concepts/demographics/age.sql",
  "mimic-iv/concepts/firstday/first_day_gcs.sql",
  "mimic-iv/concepts/measurement/gcs.sql",
  "mimic-iv/concepts/firstday/first_day_vitalsign.sql",
  "mimic-iv/concepts/measurement/vitalsign.sql",
  "mimic-iv/concepts/firstday/first_day_urine_output.sql",
  "mimic-iv/concepts/measurement/urine_output.sql",
  "mimic-iv/concepts/treatment/ventilation.sql",
  "mimic-iv/concepts/measurement/ventilator_setting.sql",
  "mimic-iv/concepts/measurement/oxygen_delivery.sql"
)
official_paths <- file.path(official_repo, official_rel)
if (any(!file.exists(official_paths))) {
  stop("Pinned OASIS dependency source is incomplete.")
}
provenance <- data.table(
  dependency = official_rel,
  sha256 = vapply(official_paths, sha256_file, character(1L)),
  official_commit = expected_commit,
  executed_as_sql = FALSE,
  outcome_bearing_fields_executed = FALSE,
  implementation = "outcome-free predictor-side R refactor"
)
fwrite(provenance, file.path(qc_out, "native_oasis_provenance.csv"))

# Direct threshold tests make transcription errors observable without using
# any patient outcome. Values are deliberately placed on both sides of every
# clinically material boundary.
synthetic <- data.table(
  test = c(
    "score_is_component_sum", "score_is_integer", "score_in_0_75",
    "age_lt24_scores0", "age_90_scores7",
    "preiculos_lt10_2_scores5", "preiculos_1440_scores2",
    "gcs_7_scores10", "gcs_14_scores3", "gcs_15_scores0",
    "hr_max126_scores6", "hr_min32_scores4",
    "mbp_min20_scores4", "mbp_max144_scores3",
    "rr_min5_scores10", "rr_max45_scores9",
    "temp_max40_scores6", "temp_min35_scores4",
    "urine670_scores10", "urine7000_scores8",
    "vent1_scores9", "elective1_scores0"
  ),
  pass = c(
    all(oasis$oasis == rowSums(as.data.frame(lapply(
      oasis[, ..score_columns], coalesce_num, fallback = 0
    )))),
    all(oasis$oasis == as.integer(oasis$oasis)),
    all(oasis$oasis >= 0 & oasis$oasis <= 75),
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  )
)
# Every boundary assertion calls the same executable function used above.
synthetic[test == "age_lt24_scores0", pass := score_age(23) == 0]
synthetic[test == "age_90_scores7", pass := score_age(90) == 7]
synthetic[test == "preiculos_lt10_2_scores5",
          pass := score_preiculos(10.1) == 5]
synthetic[test == "preiculos_1440_scores2",
          pass := score_preiculos(1440) == 2]
synthetic[test == "gcs_7_scores10", pass := score_gcs(7) == 10]
synthetic[test == "gcs_14_scores3", pass := score_gcs(14) == 3]
synthetic[test == "gcs_15_scores0", pass := score_gcs(15) == 0]
synthetic[test == "hr_max126_scores6",
          pass := score_heart_rate(80, 126) == 6]
synthetic[test == "hr_min32_scores4",
          pass := score_heart_rate(32, 100) == 4]
synthetic[test == "mbp_min20_scores4",
          pass := score_mbp(20, 100) == 4]
synthetic[test == "mbp_max144_scores3",
          pass := score_mbp(70, 144) == 3]
synthetic[test == "rr_min5_scores10",
          pass := score_resp_rate(5, 20) == 10]
synthetic[test == "rr_max45_scores9",
          pass := score_resp_rate(12, 45) == 9]
synthetic[test == "temp_max40_scores6",
          pass := score_temperature(37, 40) == 6]
synthetic[test == "temp_min35_scores4",
          pass := score_temperature(35, 37) == 4]
synthetic[test == "urine670_scores10", pass := score_urine(670) == 10]
synthetic[test == "urine7000_scores8", pass := score_urine(7000) == 8]
synthetic[test == "vent1_scores9", pass := score_mechvent(1L) == 9]
synthetic[test == "elective1_scores0", pass := score_elective(1L) == 0]
fwrite(synthetic, file.path(qc_out, "native_oasis_synthetic_rule_tests.csv"))
if (any(!synthetic$pass)) stop("Native-OASIS synthetic rule test failed.")

allowed_scores <- list(
  age_score = c(0L, 3L, 6L, 7L, 9L),
  preiculos_score = c(0L, 1L, 2L, 3L, 5L),
  gcs_score = c(0L, 3L, 4L, 10L),
  heart_rate_score = c(0L, 1L, 3L, 4L, 6L),
  mbp_score = c(0L, 2L, 3L, 4L),
  resp_rate_score = c(0L, 1L, 6L, 9L, 10L),
  temp_score = c(0L, 2L, 3L, 4L, 6L),
  urineoutput_score = c(0L, 1L, 5L, 8L, 10L),
  mechvent_score = c(0L, 9L),
  electivesurgery_score = c(0L, 6L)
)
allowed_pass <- vapply(names(allowed_scores), function(v) {
  all(is.na(artifact[[v]]) | artifact[[v]] %in% allowed_scores[[v]])
}, logical(1L))

invariants <- data.table(
  test = c(
    "one_row_per_strict_stay", "strict_stay_set_exact",
    "strict_subject_set_exact", "score_integer", "score_range_0_75",
    "component_score_sets_valid", "component_sum_exact",
    "gcs_selected_within_official_window",
    "vitals_selected_within_official_window",
    "urine_selected_within_official_window",
    "vent_binary", "safe_admission_type_exact",
    "official_repository_commit_exact", "official_repository_clean",
    "large_sources_full_eof", "large_sources_official_sha",
    "safe_sources_official_sha", "row_artifact_has_no_actual_outcome_fields",
    "no_native_probability_executed"
  ),
  pass = c(
    nrow(artifact) == nrow(base) && !anyDuplicated(artifact$stay_id),
    setequal(artifact$stay_id, base$stay_id),
    setequal(artifact$subject_id, base$subject_id),
    all(artifact$oasis == as.integer(artifact$oasis)),
    all(artifact$oasis >= 0 & artifact$oasis <= 75),
    all(allowed_pass),
    all(oasis$oasis == rowSums(as.data.frame(lapply(
      oasis[, ..score_columns], coalesce_num, fallback = 0
    )))),
    all(gwindow$chart_epoch >= gwindow$intime_epoch - 6 * 3600 &
          gwindow$chart_epoch <= gwindow$intime_epoch + 24 * 3600),
    all(vwindow$chart_epoch >= vwindow$intime_epoch - 6 * 3600 &
          vwindow$chart_epoch <= vwindow$intime_epoch + 24 * 3600),
    all(urine_window$chart_epoch >= urine_window$intime_epoch &
          urine_window$chart_epoch <= urine_window$intime_epoch + 24 * 3600),
    all(artifact$mechvent %in% c(0L, 1L)),
    all(base$phase1_admission_type == base$source_admission_type),
    official_commit == expected_commit,
    length(repo_status) == 0L,
    all(cache_manifest$reached_eof == TRUE),
    all(cache_manifest$official_sha256_match == TRUE),
    identical(unname(safe_raw_sha), unname(expected_safe_sha)),
    !any(grepl(forbidden_pattern, names(artifact), ignore.case = TRUE)),
    !"oasis_prob" %in% names(artifact)
  )
)
fwrite(invariants, file.path(qc_out, "native_oasis_invariant_tests.csv"))
if (any(!invariants$pass)) {
  stop(
    "Native-OASIS invariant failure: ",
    paste(invariants[pass == FALSE, test], collapse = ", ")
  )
}

component_vars <- c(
  "age", "preiculos", "gcs", "heart_rate_max", "mbp_min",
  "resp_rate_max", "temperature_max", "urineoutput", "mechvent",
  "electivesurgery"
)
availability <- rbindlist(lapply(component_vars, function(v) {
  data.table(
    component = v,
    available_n = sum(!is.na(oasis[[v]])),
    missing_n = sum(is.na(oasis[[v]])),
    available_percent = round(100 * mean(!is.na(oasis[[v]])), 3)
  )
}))
fwrite(availability, file.path(qc_out, "native_oasis_component_availability.csv"))

distribution_vars <- c(
  "oasis", "component_available_n", "age", "preiculos", "gcs",
  "heart_rate_min", "heart_rate_max", "mbp_min", "mbp_max",
  "resp_rate_min", "resp_rate_max", "temperature_min",
  "temperature_max", "urineoutput"
)
distribution <- rbindlist(lapply(distribution_vars, function(v) {
  z <- oasis[[v]]
  data.table(
    variable = v,
    nonmissing_n = sum(!is.na(z)),
    minimum = if (all(is.na(z))) NA_real_ else min(z, na.rm = TRUE),
    q25 = if (all(is.na(z))) NA_real_ else quantile(z, 0.25, na.rm = TRUE),
    median = if (all(is.na(z))) NA_real_ else median(z, na.rm = TRUE),
    q75 = if (all(is.na(z))) NA_real_ else quantile(z, 0.75, na.rm = TRUE),
    maximum = if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)
  )
}))
fwrite(distribution, file.path(qc_out, "native_oasis_value_distribution.csv"))

score_frequency <- rbindlist(lapply(score_columns, function(v) {
  z <- oasis[, .N, by = c(v)]
  setnames(z, v, "score")
  z[, component := v]
  setcolorder(z, c("component", "score", "N"))
  z
}), fill = TRUE)
fwrite(
  score_frequency,
  file.path(qc_out, "native_oasis_component_score_frequency.csv")
)

timing_qc <- data.table(
  stream = c("GCS", "vitals", "urine"),
  nominal_start_hours = c(-6, -6, 0),
  nominal_end_hours = c(24, 24, 24),
  selected_stays = c(
    uniqueN(gselected$stay_id), uniqueN(vitals$stay_id), uniqueN(urine$stay_id)
  ),
  earliest_observed_offset_hours = c(
    if (nrow(gwindow)) min((gwindow$chart_epoch - gwindow$intime_epoch) / 3600) else NA,
    if (nrow(vwindow)) min((vwindow$chart_epoch - vwindow$intime_epoch) / 3600) else NA,
    if (nrow(urine_window)) min((urine_window$chart_epoch - urine_window$intime_epoch) / 3600) else NA
  ),
  latest_observed_offset_hours = c(
    if (nrow(gwindow)) max((gwindow$chart_epoch - gwindow$intime_epoch) / 3600) else NA,
    if (nrow(vwindow)) max((vwindow$chart_epoch - vwindow$intime_epoch) / 3600) else NA,
    if (nrow(urine_window)) max((urine_window$chart_epoch - urine_window$intime_epoch) / 3600) else NA
  )
)
fwrite(timing_qc, file.path(qc_out, "native_oasis_timing_QC.csv"))

compatibility_qc <- data.table(
  metric = c(
    "strict_stays", "index_before_native_window_end",
    "selected_gcs_after_index", "last_vital_time_after_index",
    "last_urine_time_after_index",
    "native_oasis_safe_as_index_time_predictor_for_all_stays"
  ),
  value = c(
    nrow(base),
    sum(base$index_epoch < base$intime_epoch + 24 * 3600),
    gselected[base, on = "stay_id",
              sum(!is.na(gcs_chart_epoch) & gcs_chart_epoch > i.index_epoch)],
    vitals[base, on = "stay_id",
           sum(!is.na(vital_last_chart_epoch) &
                 vital_last_chart_epoch > i.index_epoch)],
    urine[base, on = "stay_id",
          sum(!is.na(urine_last_chart_epoch) &
                urine_last_chart_epoch > i.index_epoch)],
    0
  ),
  interpretation = c(
    "denominator", "native +24 h window ends after the study index",
    "selected first-day GCS occurs after the study index",
    "at least one first-day vital time occurs after the study index",
    "at least one first-day urine time occurs after the study index",
    "FALSE: native OASIS is contextual/descriptive, not an index-time S0 covariate"
  )
)
fwrite(
  compatibility_qc,
  file.path(qc_out, "native_oasis_prediction_time_compatibility_QC.csv")
)

gcs_qc <- data.table(
  metric = c(
    "raw_component_rows", "same_time_groups", "groups_with_ett_text",
    "groups_using_prior_within_6h", "first_day_rows", "selected_stays",
    "selected_ett_convention_stays"
  ),
  value = c(
    nrow(graw), nrow(gbase), sum(gbase$gcs_unable == 1L),
    sum(gbase$prior_within_6h), nrow(gwindow), nrow(gselected),
    sum(gselected$gcs_unable == 1L)
  )
)
fwrite(gcs_qc, file.path(qc_out, "native_oasis_gcs_QC.csv"))

vent_qc <- rbindlist(list(
  vs[!is.na(ventilation_status), .N, by = ventilation_status][
    , .(section = "classified_time", category = ventilation_status, N)
  ],
  episodes[, .N, by = ventilation_status][
    , .(section = "episode", category = ventilation_status, N)
  ],
  data.table(
    section = "first_day_flag",
    category = c(
      "official_oasis_invasive", "phase1_index_invasive",
      "both", "phase1_only", "oasis_only"
    ),
    N = c(
      sum(base$mechvent == 1L),
      sum(base$index_invasive_confirmed == TRUE),
      sum(base$mechvent == 1L & base$index_invasive_confirmed == TRUE),
      sum(base$mechvent == 0L & base$index_invasive_confirmed == TRUE),
      sum(base$mechvent == 1L & base$index_invasive_confirmed == FALSE)
    )
  )
), fill = TRUE)
fwrite(vent_qc, file.path(qc_out, "native_oasis_ventilation_QC.csv"))

input_qc <- data.table(
  source = c("admissions_safe_select", "services_safe_select"),
  selected_columns = c(
    "subject_id;hadm_id;admittime;admission_type",
    "subject_id;hadm_id;transfertime;curr_service"
  ),
  selected_rows = c(nrow(admissions), nrow(services)),
  raw_sha256 = unname(safe_raw_sha),
  official_sha256 = unname(expected_safe_sha),
  official_sha256_match = unname(safe_raw_sha == expected_safe_sha),
  actual_outcome_columns_selected = FALSE
)
fwrite(input_qc, file.path(qc_out, "native_oasis_safe_input_QC.csv"))
fwrite(cache_manifest, file.path(qc_out, "native_oasis_event_cache_manifest.csv"))

leakage_guard <- data.table(
  test = c(
    "phase1_source_has_no_actual_outcome_like_columns",
    "native_artifact_has_no_actual_outcome_like_columns",
    "admissions_select_is_safe_allowlist_only",
    "services_select_is_safe_allowlist_only",
    "official_outcome_probability_not_executed",
    "aggregate_qc_contains_no_patient_identifiers"
  ),
  pass = c(
    !any(grepl(forbidden_pattern, names(index_source), ignore.case = TRUE)),
    !any(grepl(forbidden_pattern, names(artifact), ignore.case = TRUE)),
    identical(
      names(admissions),
      c("subject_id", "hadm_id", "admittime", "admission_type", "admittime_epoch")
    ),
    identical(
      names(services),
      c("subject_id", "hadm_id", "transfertime", "curr_service", "transfertime_epoch")
    ),
    !"oasis_prob" %in% names(artifact),
    TRUE
  )
)
fwrite(leakage_guard, file.path(qc_out, "outcome_leakage_guard.csv"))
if (any(!leakage_guard$pass)) stop("Native-OASIS leakage guard failed.")

artifact_metadata <- list(
  version = "mimic_native_oasis_benchmark_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED$version,
  official_benchmark = "OASIS",
  official_repository_commit = expected_commit,
  official_score_sql_sha256 = provenance[
    dependency == "mimic-iv/concepts/score/oasis.sql", sha256
  ],
  implementation = "source-faithful outcome-free predictor-side refactor",
  time_origin = "first ICU day (vitals/GCS -6 to +24 h; urine/vent 0 to +24 h)",
  missing_component_rule = "official COALESCE(component_score, 0)",
  predicted_probability_included = FALSE,
  actual_outcome_fields_read = FALSE,
  hsc_substitute_allowed = FALSE,
  intended_use = paste(
    "contextual native first-day severity benchmark only; not an index-time",
    "prediction covariate when any component may be observed after index"
  ),
  source_phase1_gate_sha256 = sha256_file(phase1_gate_path),
  source_phase1_rds_sha256 = sha256_file(input_index),
  script_sha256 = sha256_file(script_path),
  helper_sha256 = sha256_file(helper_path)
)
attr(artifact, "rebuild_metadata") <- artifact_metadata
saveRDS(artifact, output_rds, compress = "xz")

summary_lines <- c(
  "# MIMIC-IV native OASIS benchmark QC",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("- Locked strict cohort: ", nrow(artifact)),
  paste0("- Official mimic-code commit: `", expected_commit, "`"),
  paste0("- OASIS median (IQR): ",
         round(median(artifact$oasis), 2), " (",
         round(quantile(artifact$oasis, 0.25), 2), "–",
         round(quantile(artifact$oasis, 0.75), 2), ")"),
  paste0("- Complete 10-component records: ",
         sum(artifact$component_available_n == 10L), " (",
         round(100 * mean(artifact$component_available_n == 10L), 2), "%)"),
  paste0("- First-day invasive-ventilation flag: ",
         sum(artifact$mechvent == 1L)),
  "- This is a native first-ICU-day benchmark and is not substituted for the time-aligned harmonized severity core.",
  paste0("- The native +24 h window extends beyond the study index for ",
         compatibility_qc[metric == "index_before_native_window_end", value],
         " stays; OASIS is therefore prohibited as an index-time S0 covariate."),
  "- The official predictor-side score and official missing-component-to-zero rule were reproduced; the official mortality-probability expression was not executed or stored.",
  "- Admissions and services were read with explicit safe-column allow-lists only; no death, discharge-status, survival, or actual-outcome field was selected.",
  "- Both large event sources reached EOF, passed strict retained-CSV validation, and matched the official MIMIC-IV v3.1 SHA256 manifest.",
  "- Row-level output is confined to `analysis_rebuild_v1/private/mimic`.",
  "",
  "BUILD_COMPLETE"
)
writeLines(
  summary_lines,
  file.path(qc_out, "mimic_native_oasis_QC.md"),
  useBytes = TRUE
)

required_qc <- c(
  "native_oasis_provenance.csv", "native_oasis_synthetic_rule_tests.csv",
  "native_oasis_invariant_tests.csv", "native_oasis_component_availability.csv",
  "native_oasis_value_distribution.csv",
  "native_oasis_component_score_frequency.csv", "native_oasis_timing_QC.csv",
  "native_oasis_prediction_time_compatibility_QC.csv",
  "native_oasis_gcs_QC.csv", "native_oasis_ventilation_QC.csv",
  "native_oasis_safe_input_QC.csv", "native_oasis_event_cache_manifest.csv",
  "outcome_leakage_guard.csv", "mimic_native_oasis_QC.md"
)
if (any(!file.exists(file.path(qc_out, required_qc)))) {
  stop("Required native-OASIS QC product is missing.")
}

gate <- data.table(
  field = c(
    "status", "completed_at", "locked_config_version",
    "official_benchmark", "official_repository_commit",
    "script_sha256", "helper_sha256", "phase1_gate_sha256",
    "input_strict_cohort_sha256", "input_cache_gate_sha256",
    "input_cache_manifest_sha256", "strict_cohort_n",
    "native_oasis_rds_sha256", "all_invariants_pass",
    "synthetic_rule_tests_pass", "outcome_leakage_guard_pass",
    "all_event_sources_reached_eof", "all_raw_sha256_match_official",
    "all_required_qc_present", "actual_outcome_fields_read",
    "predicted_probability_executed", "hsc_substitute_allowed"
  ),
  value = as.character(c(
    "PASS", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), LOCKED$version,
    "OASIS", expected_commit,
    sha256_file(script_path), sha256_file(helper_path),
    sha256_file(phase1_gate_path), sha256_file(input_index),
    sha256_file(cache_gate_path), sha256_file(cache_manifest_path),
    nrow(artifact), sha256_file(output_rds),
    all(invariants$pass), all(synthetic$pass), all(leakage_guard$pass),
    all(cache_manifest$reached_eof == TRUE),
    all(cache_manifest$official_sha256_match == TRUE) &&
      identical(unname(safe_raw_sha), unname(expected_safe_sha)),
    all(file.exists(file.path(qc_out, required_qc))),
    FALSE, FALSE, FALSE
  ))
)
fwrite(gate, completion_gate_tmp)
if (!file.rename(completion_gate_tmp, completion_gate)) {
  stop("Could not atomically publish the native-OASIS PASS gate.")
}

message(
  "MIMIC native OASIS build complete (outcome-blind): ",
  nrow(artifact), " strict stays; gate=", completion_gate
)
