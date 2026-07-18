#!/usr/bin/env Rscript

## =============================================================================
## 02_build_eicu_index_cohort.R
## eICU-CRD v2.0: strict, time-aligned oxygenation-defined index cohort
##
## This is a NEW reconstruction workflow. It does not overwrite any legacy
## checkpoint or result from 08_eicu_external_validation_v1_0.R.
##
## Main phenotype (parameterized below)
##   - age >= 18 years;
##   - the first in-ICU PaO2 event that can be paired to a valid FiO2 within
##     +/-2 h and satisfies all other index criteria;
##   - P/F <= 300 and a valid PEEP >= 5 cmH2O within +/-2 h of the PaO2;
##   - explicit evidence of an artificial airway at/near that event;
##   - no explicit NIV evidence within +/-2 h of that event;
##   - primary: infection evidence available from 48 h before through index;
##     index+24 h ascertainment is saved only as a sensitivity cohort;
##   - first qualifying event per ICU stay, then first eligible ICU stay per
##     unique patient.
##
## IMPORTANT INTERPRETIVE BOUNDARIES
##   - The output is an oxygenation-defined phenotype, not imaging-adjudicated
##     ARDS. eICU has no harmonized bilateral-opacities adjudication here.
##   - eICU infection is diagnosis-based and is not equivalent to the MIMIC
##     suspected-infection antibiotic/culture rule.
##   - eICU does not provide a reliable calendar admission date for ordering all
##     repeat hospitalizations. Across multiple hospital encounters for the same
##     uniquepid, hospitaldischargeyear then patienthealthsystemstayid are used as
##     a deterministic ordering proxy; this limitation is written to QC output.
##   - A future artificial-airway record is accepted only within a short charting
##     grace period and only if a recent "No Artificial Airway" state does not
##     contradict it.
##
## PRIVACY / OUTPUT BOUNDARY
##   Row-level data, including eICU identifiers, are written only to:
##     analysis_rebuild_v1/private/eicu/
##   Aggregate QC tables containing no patient/stay identifiers are written to:
##     analysis_rebuild_v1/qc/eicu/
##
## Runtime (typical local machine): several minutes. Raw eICU files are read-only.
## =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

## ---- Configuration ----------------------------------------------------------

PROJECT_BOOTSTRAP <- Sys.getenv(
  "ARDS_MP_PROJECT_ROOT",
  Sys.getenv("ARDS_MP_PROJECT", getwd())
)
CONFIG_PATH <- file.path(PROJECT_BOOTSTRAP, "code", "R", "rebuild_v1", "00_config.R")
if (!file.exists(CONFIG_PATH)) stop("Locked configuration not found: ", CONFIG_PATH)
source(CONFIG_PATH, local = FALSE)

PROJECT <- PROJECT_ROOT
EICU <- EICU_ROOT
OUT_PRIVATE <- file.path(PRIVATE_ROOT, "eicu")
OUT_QC <- file.path(QC_ROOT, "eicu")
dir.create(OUT_PRIVATE, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_QC, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required to create the eICU Phase-1 completion gate")
}
SCRIPT_PATH <- normalizePath(file.path(
  PROJECT, "code", "R", "rebuild_v1", "02_build_eicu_index_cohort.R"
), mustWork = TRUE)
SCRIPT_SHA256 <- digest::digest(file = SCRIPT_PATH, algo = "sha256")
PHASE1_COMPLETE <- file.path(OUT_QC, "phase1_eicu_complete_v1.csv")
PHASE1_COMPLETE_TMP <- paste0(PHASE1_COMPLETE, ".tmp")
# A completion marker certifies a finished build. Remove a stale marker before
# touching any formal product so an interrupted rerun cannot be consumed.
unlink(c(PHASE1_COMPLETE, PHASE1_COMPLETE_TMP), force = TRUE)

AGE_MIN <- LOCKED$minimum_age_years
## Index is the first qualifying event during the ICU stay. It is not restricted
## to admission day 1; doing so is not part of LOCKED v1.0.1. All prediction
## variables will subsequently be collected from index forward.
INDEX_MIN_OFFSET <- 0L
PF_FIO2_WINDOW_MIN <- LOCKED$pao2_fio2_pair_window_minutes
PEEP_WINDOW_MIN <- LOCKED$pao2_peep_pair_window_minutes
PEEP_MIN <- LOCKED$minimum_index_peep_cmH2O
PF_MAX <- LOCKED$pf_threshold_mmHg
INV_AIRWAY_LOOKBACK_MIN <- as.integer(Sys.getenv("EICU_INV_LOOKBACK_MIN", "720"))
INV_AIRWAY_LOOKAHEAD_MIN <- as.integer(Sys.getenv("EICU_INV_LOOKAHEAD_MIN", "120"))
RECENT_NO_AIRWAY_MIN <- as.integer(Sys.getenv("EICU_RECENT_NO_AIRWAY_MIN", "120"))
NIV_EXCLUSION_WINDOW_MIN <- as.integer(Sys.getenv("EICU_NIV_WINDOW_MIN", "120"))
INFECTION_LOOKBACK_MIN <- LOCKED$infection_window_hours_before_index * 60L
INFECTION_LOOKAHEAD_MIN <- LOCKED$infection_window_hours_after_index * 60L
INFECTION_SENS_LOOKAHEAD_MIN <-
  LOCKED$sensitivity_infection_window_hours_after_index * 60L

log_msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
}

required_files <- c(
  "patient.csv.gz", "lab.csv.gz", "respiratoryCharting.csv.gz",
  "respiratoryCare.csv.gz", "diagnosis.csv.gz", "admissionDx.csv.gz",
  "apacheApsVar.csv.gz"
)
missing_files <- required_files[!file.exists(file.path(EICU, required_files))]
if (length(missing_files)) {
  stop("Missing required eICU files: ", paste(missing_files, collapse = ", "))
}

## Read only rows selected by an awk expression. The fields used for filtering
## below (labname and respiratory-chart label) do not contain commas in eICU.
## fread then performs standards-compliant parsing of the retained CSV rows.
read_gz_filtered <- function(path, awk_condition) {
  cmd <- sprintf(
    "gzip -cd %s | awk -F',' 'NR==1 || (%s)'",
    shQuote(path), awk_condition
  )
  fread(cmd = cmd, showProgress = FALSE)
}

## Standard schema for time-varying measurements/evidence:
## patientunitstayid, measure_time, measurement_value (numeric),
## measurement_source (character), measurement_label (character).
collapse_measurements <- function(x) {
  stopifnot(all(c(
    "patientunitstayid", "measure_time", "measurement_value",
    "measurement_source", "measurement_label"
  ) %in% names(x)))
  x <- x[!is.na(patientunitstayid) & !is.na(measure_time)]
  x[, .(
    measurement_value = if (all(is.na(measurement_value))) NA_real_ else
      median(measurement_value, na.rm = TRUE),
    measurement_source = paste(sort(unique(na.omit(measurement_source))), collapse = "+"),
    measurement_label = paste(sort(unique(na.omit(measurement_label))), collapse = "+")
  ), by = .(patientunitstayid, measure_time)]
}

## Return the closest prior or future measurement for every anchor event.
## The result always has one row per event_id; unmatched fields are NA.
nearest_side <- function(anchor, measure, direction = c("prior", "future"),
                         max_gap, prefix) {
  direction <- match.arg(direction)
  a <- unique(anchor[, .(patientunitstayid, event_id, anchor_time)])
  if (anyDuplicated(a$event_id)) stop("event_id must uniquely identify anchors")

  empty_result <- function() {
    z <- a[, .(event_id)]
    z[, c("near_time", "near_value", "near_source", "near_label",
          "signed_gap", "abs_gap") := .(
      NA_real_, NA_real_, NA_character_, NA_character_, NA_real_, NA_real_
    )]
    setnames(z, setdiff(names(z), "event_id"),
             paste0(prefix, "_", setdiff(names(z), "event_id")))
    z
  }

  if (!nrow(measure)) return(empty_result())
  m <- unique(measure[, .(
    patientunitstayid, measure_time,
    measurement_value, measurement_source, measurement_label
  )])
  m[, measure_time_keep := measure_time]
  setkey(m, patientunitstayid, measure_time)

  if (direction == "prior") {
    z <- m[a,
      on = .(patientunitstayid, measure_time <= anchor_time),
      mult = "last",
      .(
        event_id = i.event_id,
        anchor_time = i.anchor_time,
        near_time = x.measure_time_keep,
        near_value = x.measurement_value,
        near_source = x.measurement_source,
        near_label = x.measurement_label
      )
    ]
  } else {
    z <- m[a,
      on = .(patientunitstayid, measure_time >= anchor_time),
      mult = "first",
      .(
        event_id = i.event_id,
        anchor_time = i.anchor_time,
        near_time = x.measure_time_keep,
        near_value = x.measurement_value,
        near_source = x.measurement_source,
        near_label = x.measurement_label
      )
    ]
  }

  z[, `:=`(
    signed_gap = near_time - anchor_time,
    abs_gap = abs(near_time - anchor_time)
  )]
  if (direction == "prior") {
    z[is.na(near_time) | signed_gap > 0 | abs_gap > max_gap,
      c("near_time", "near_value", "near_source", "near_label",
        "signed_gap", "abs_gap") := .(
        NA_real_, NA_real_, NA_character_, NA_character_, NA_real_, NA_real_
      )]
  } else {
    z[is.na(near_time) | signed_gap < 0 | abs_gap > max_gap,
      c("near_time", "near_value", "near_source", "near_label",
        "signed_gap", "abs_gap") := .(
        NA_real_, NA_real_, NA_character_, NA_character_, NA_real_, NA_real_
      )]
  }
  z[, anchor_time := NULL]
  setnames(z, setdiff(names(z), "event_id"),
           paste0(prefix, "_", setdiff(names(z), "event_id")))
  z
}

## Closest measurement in a symmetric window. Ties prefer a prior measurement,
## avoiding use of later information when an equally close earlier value exists.
nearest_symmetric <- function(anchor, measure, max_gap, prefix) {
  b <- nearest_side(anchor, measure, "prior", max_gap, "prior")
  f <- nearest_side(anchor, measure, "future", max_gap, "future")
  bl <- b[, .(
    event_id,
    near_time = prior_near_time,
    near_value = prior_near_value,
    near_source = prior_near_source,
    near_label = prior_near_label,
    signed_gap = prior_signed_gap,
    abs_gap = prior_abs_gap,
    tie_rank = 0L
  )]
  fl <- f[, .(
    event_id,
    near_time = future_near_time,
    near_value = future_near_value,
    near_source = future_near_source,
    near_label = future_near_label,
    signed_gap = future_signed_gap,
    abs_gap = future_abs_gap,
    tie_rank = 1L
  )]
  z <- rbindlist(list(bl, fl), use.names = TRUE)
  z <- z[!is.na(near_time)]
  if (nrow(z)) {
    setorder(z, event_id, abs_gap, tie_rank, near_time)
    z <- z[, .SD[1], by = event_id]
    z[, tie_rank := NULL]
  } else {
    z <- data.table(
      event_id = integer(), near_time = numeric(), near_value = numeric(),
      near_source = character(), near_label = character(),
      signed_gap = numeric(), abs_gap = numeric()
    )
  }
  z <- merge(unique(anchor[, .(event_id)]), z, by = "event_id", all.x = TRUE)
  setnames(z, setdiff(names(z), "event_id"),
           paste0(prefix, "_", setdiff(names(z), "event_id")))
  z
}

metric_quantiles <- function(x, metric, subset_name = "final_cohort") {
  x <- x[is.finite(x)]
  probs <- c(0, .05, .25, .5, .75, .95, 1)
  q <- if (length(x)) as.numeric(quantile(x, probs, na.rm = TRUE, names = FALSE)) else
    rep(NA_real_, length(probs))
  data.table(
    subset = subset_name,
    metric = metric,
    n_nonmissing = length(x),
    min = q[1], p05 = q[2], p25 = q[3], median = q[4],
    p75 = q[5], p95 = q[6], max = q[7]
  )
}

## ---- Patient/stay denominator -----------------------------------------------

log_msg("Reading patient table")
pat <- fread(
  file.path(EICU, "patient.csv.gz"),
  select = c(
    "patientunitstayid", "patienthealthsystemstayid", "uniquepid",
    "hospitalid", "gender", "age", "unitvisitnumber", "unitstaytype",
    "hospitaldischargeyear", "unitdischargeoffset", "hospitaldischargeoffset",
    "apacheadmissiondx"
  ),
  showProgress = FALSE
)
pat[, age_num := suppressWarnings(as.numeric(age))]
pat[age == "> 89", age_num := 90]
pat[, adult := !is.na(age_num) & age_num >= AGE_MIN]
pat[, icu_end_offset := suppressWarnings(as.numeric(unitdischargeoffset))]
pat[!is.finite(icu_end_offset) | icu_end_offset < 0, icu_end_offset := NA_real_]
pat[, person_key := fifelse(
  !is.na(uniquepid) & nzchar(trimws(uniquepid)),
  paste0("pid:", uniquepid),
  paste0("missing_pid_hsp:", patienthealthsystemstayid,
         "_stay:", patientunitstayid)
)]
adult_ids <- pat[adult == TRUE, patientunitstayid]
adult_bounds <- pat[adult == TRUE & !is.na(icu_end_offset),
                    .(patientunitstayid, icu_end_offset)]

## ---- PaO2, FiO2, and PEEP extraction ---------------------------------------

log_msg("Streaming PaO2/FiO2 rows from lab.csv.gz")
lab <- read_gz_filtered(
  file.path(EICU, "lab.csv.gz"),
  '$5=="paO2" || $5=="FiO2"'
)[, .(patientunitstayid, labresultoffset, labname, labresult)]
lab <- merge(lab, adult_bounds, by = "patientunitstayid", all = FALSE)

pao2 <- lab[
  labname == "paO2" & !is.na(labresult) & labresult >= 20 & labresult <= 700 &
    labresultoffset >= INDEX_MIN_OFFSET & labresultoffset <= icu_end_offset,
  .(pao2 = median(labresult, na.rm = TRUE)),
  by = .(patientunitstayid, pao2_time = labresultoffset)
]
pao2[, event_id := .I]

fio2_lab <- lab[labname == "FiO2" & !is.na(labresult), .(
  patientunitstayid,
  measure_time = labresultoffset,
  measurement_value = as.numeric(labresult),
  measurement_source = "lab",
  measurement_label = "FiO2"
)]
fio2_lab[measurement_value > 0 & measurement_value <= 1,
         measurement_value := measurement_value * 100]
fio2_lab <- fio2_lab[
  measurement_value >= 21 & measurement_value <= 100
]
rm(lab); invisible(gc())

log_msg("Streaming selected ventilator rows from respiratoryCharting.csv.gz")
rc_condition <- paste(c(
  '$6=="FiO2"', '$6=="FIO2 (%)"', '$6=="PEEP"', '$6=="PEEP/CPAP"',
  '$6=="Endotracheal Tube Placement"', '$6=="O2 Device"',
  '$6=="RT Vent On/Off"', '$6=="Mechanical Ventilator Mode"',
  '$6=="Non-invasive Ventilation Mode"', '$6=="Ventilator Support Mode"',
  '$6=="Bipap Delivery Mode"', '$6 ~ /^NIV /'
), collapse = " || ")
rc <- read_gz_filtered(
  file.path(EICU, "respiratoryCharting.csv.gz"), rc_condition
)[, .(patientunitstayid, respchartoffset, respchartvaluelabel, respchartvalue)]
rc <- merge(rc, adult_bounds, by = "patientunitstayid", all = FALSE)
rc[, value_num := suppressWarnings(as.numeric(respchartvalue))]

fio2_rc <- rc[respchartvaluelabel %chin% c("FiO2", "FIO2 (%)") &
                 !is.na(value_num), .(
  patientunitstayid,
  measure_time = respchartoffset,
  measurement_value = value_num,
  measurement_source = "respiratoryCharting",
  measurement_label = respchartvaluelabel
)]
fio2_rc[measurement_value > 0 & measurement_value <= 1,
        measurement_value := measurement_value * 100]
fio2_rc <- fio2_rc[
  measurement_value >= 21 & measurement_value <= 100
]

fio2 <- collapse_measurements(rbindlist(list(fio2_lab, fio2_rc), use.names = TRUE))
rm(fio2_lab, fio2_rc); invisible(gc())

peep <- rc[
  respchartvaluelabel %chin% c("PEEP", "PEEP/CPAP") &
    !is.na(value_num) & value_num >= 0 & value_num <= 30 &
    respchartoffset >= INDEX_MIN_OFFSET - PEEP_WINDOW_MIN &
    respchartoffset <= icu_end_offset + PEEP_WINDOW_MIN,
  .(
    measurement_value = median(value_num, na.rm = TRUE),
    measurement_source = "respiratoryCharting",
    measurement_label = paste(sort(unique(respchartvaluelabel)), collapse = "+")
  ),
  by = .(patientunitstayid, measure_time = respchartoffset)
]

if (!nrow(pao2)) stop("No valid in-ICU PaO2 events were extracted")
if (!nrow(fio2)) stop("No valid FiO2 measurements were extracted")
if (!nrow(peep)) stop("No valid PEEP measurements were extracted")

anchors <- pao2[, .(
  patientunitstayid, event_id, anchor_time = pao2_time
)]
log_msg("Pairing PaO2 with nearest FiO2 and PEEP")
fio2_near <- nearest_symmetric(anchors, fio2, PF_FIO2_WINDOW_MIN, "fio2")
peep_near <- nearest_symmetric(anchors, peep, PEEP_WINDOW_MIN, "peep")

events <- merge(pao2, fio2_near, by = "event_id", all.x = TRUE)
events <- merge(events, peep_near, by = "event_id", all.x = TRUE)
events[, pf_ratio := pao2 / (fio2_near_value / 100)]
events[!is.finite(pf_ratio) | pf_ratio <= 0 | pf_ratio > 1000, pf_ratio := NA_real_]
events <- merge(events, pat, by = "patientunitstayid", all.x = TRUE)

## ---- Explicit invasive-airway evidence and NIV exclusion -------------------

log_msg("Reading explicit artificial-airway states")
rcare <- fread(
  file.path(EICU, "respiratoryCare.csv.gz"),
  select = c("patientunitstayid", "respcarestatusoffset", "airwaytype"),
  showProgress = FALSE
)
rcare <- rcare[
  patientunitstayid %in% adult_ids & !is.na(airwaytype) & nzchar(trimws(airwaytype)) &
    respcarestatusoffset >= INDEX_MIN_OFFSET - INV_AIRWAY_LOOKBACK_MIN
]
inv_airway_levels <- c(
  "Oral ETT", "Nasal ETT", "Tracheostomy", "Double-Lumen Tube",
  "Cricothyrotomy"
)
rcare[, inv_flag := as.numeric(airwaytype %chin% inv_airway_levels)]
## Unknown/Other airway states are not allowed to act as "No Artificial Airway".
rcare[!airwaytype %chin% c(inv_airway_levels, "No Artificial Airway"), inv_flag := NA_real_]
airway_state <- rcare[!is.na(inv_flag), .(
  measurement_value = if (all(inv_flag == 1)) 1 else
    if (all(inv_flag == 0)) 0 else NA_real_,
  measurement_source = "respiratoryCare",
  measurement_label = paste(sort(unique(airwaytype)), collapse = "+")
), by = .(patientunitstayid, measure_time = respcarestatusoffset)]
airway_inv_only <- airway_state[measurement_value == 1]

chart_airway <- rc[
  (respchartvaluelabel == "Endotracheal Tube Placement" |
     (respchartvaluelabel == "O2 Device" & respchartvalue == "ETT")) &
    respchartoffset >= INDEX_MIN_OFFSET - INV_AIRWAY_LOOKBACK_MIN &
    respchartoffset <= icu_end_offset + INV_AIRWAY_LOOKAHEAD_MIN,
  .(
    measurement_value = 1,
    measurement_source = "respiratoryCharting",
    measurement_label = paste(respchartvaluelabel, respchartvalue, sep = ":")
  ), by = .(patientunitstayid, measure_time = respchartoffset)
]
chart_airway <- collapse_measurements(chart_airway)

## A deliberately narrow "explicit invasive mode" mapping. CPAP, pressure
## support, tracheostomy mask, T-piece, and generic RT-on markers are not enough
## by themselves to establish an invasive route.
invasive_mode <- rc[
  (respchartvaluelabel == "O2 Device" &
     respchartvalue %chin% c("Ventilator", "ETT")) |
    (respchartvaluelabel == "Mechanical Ventilator Mode" &
       respchartvalue %chin% c("AC/CMV", "SIMV", "PCV w/assist", "SIMV+", "APRV")) |
    (respchartvaluelabel == "Ventilator Support Mode" &
       respchartvalue %chin% c("CMV", "SIMV", "APV", "Pressure control")),
  .(
    measurement_value = 1,
    measurement_source = "respiratoryCharting",
    measurement_label = paste(respchartvaluelabel, respchartvalue, sep = ":")
  ), by = .(patientunitstayid, measure_time = respchartoffset)
]
invasive_mode <- collapse_measurements(invasive_mode)

airway_event_any <- collapse_measurements(rbindlist(list(
  airway_inv_only[, .(
    patientunitstayid, measure_time, measurement_value,
    measurement_source, measurement_label
  )],
  chart_airway[, .(
    patientunitstayid, measure_time, measurement_value,
    measurement_source, measurement_label
  )]
), use.names = TRUE))

vent_active <- rc[
  (
    (respchartvaluelabel == "O2 Device" & respchartvalue %chin% c("Ventilator", "ETT")) |
    (respchartvaluelabel == "RT Vent On/Off" &
       tolower(respchartvalue) %chin% c("start", "continued")) |
    (respchartvaluelabel == "Mechanical Ventilator Mode" &
       !respchartvalue %chin% c("Trach mask", "T-piece")) |
    (respchartvaluelabel == "Ventilator Support Mode" &
       respchartvalue != "Documentation undone")
  ), .(
    measurement_value = 1,
    measurement_source = "respiratoryCharting",
    measurement_label = paste(respchartvaluelabel, respchartvalue, sep = ":")
  ), by = .(patientunitstayid, measure_time = respchartoffset)
]
vent_active <- collapse_measurements(vent_active)

is_niv_evidence <- function(label, value) {
  label <- as.character(label)
  value <- trimws(as.character(value))
  value_upper <- toupper(value)
  out <-
    (label == "Non-invasive Ventilation Mode" &
       value_upper %chin% c("S/T", "CPAP", "AVAPS")) |
    grepl("^NIV ", label) |
    label == "Bipap Delivery Mode" |
    (label == "O2 Device" & value_upper %chin% c("BI-PAP", "CPAP"))
  out[is.na(out)] <- FALSE
  out
}

niv_candidate_rows <- rc[
  respchartvaluelabel == "Non-invasive Ventilation Mode" |
    grepl("^NIV ", respchartvaluelabel) |
    respchartvaluelabel == "Bipap Delivery Mode" |
    respchartvaluelabel == "O2 Device"
]
niv_candidate_rows[, classified_as_niv := is_niv_evidence(
  respchartvaluelabel, respchartvalue
)]

niv_events <- rc[
  is_niv_evidence(respchartvaluelabel, respchartvalue),
  .(
    measurement_value = 1,
    measurement_source = "respiratoryCharting",
    measurement_label = paste(respchartvaluelabel, respchartvalue, sep = ":")
  ), by = .(patientunitstayid, measure_time = respchartoffset)
]
niv_events <- collapse_measurements(niv_events)

air_state_prior <- nearest_side(
  anchors, airway_state, "prior", INV_AIRWAY_LOOKBACK_MIN, "air_state_prior"
)
air_inv_future <- nearest_side(
  anchors, airway_inv_only, "future", INV_AIRWAY_LOOKAHEAD_MIN, "air_inv_future"
)
chart_air_prior <- nearest_side(
  anchors, chart_airway, "prior", INV_AIRWAY_LOOKBACK_MIN, "chart_air_prior"
)
chart_air_future <- nearest_side(
  anchors, chart_airway, "future", INV_AIRWAY_LOOKAHEAD_MIN, "chart_air_future"
)
vent_near <- nearest_symmetric(
  anchors, vent_active, PEEP_WINDOW_MIN, "vent_active"
)
niv_near <- nearest_symmetric(
  anchors, niv_events, NIV_EXCLUSION_WINDOW_MIN, "niv"
)
airway_near120 <- nearest_symmetric(
  anchors, airway_event_any, 120L, "airway_120"
)
invasive_mode_near <- nearest_symmetric(
  anchors, invasive_mode, 120L, "invasive_mode"
)

for (x in list(
  air_state_prior, air_inv_future, chart_air_prior, chart_air_future,
  vent_near, niv_near, airway_near120, invasive_mode_near
)) {
  events <- merge(events, x, by = "event_id", all.x = TRUE)
}

events[, recent_no_airway :=
  !is.na(air_state_prior_near_value) & air_state_prior_near_value == 0 &
    air_state_prior_abs_gap <= RECENT_NO_AIRWAY_MIN]
events[, airway_operational_confirmed :=
  (
    (!is.na(air_state_prior_near_value) & air_state_prior_near_value == 1) |
      !is.na(chart_air_prior_near_time) |
      (
        (!is.na(air_inv_future_near_time) | !is.na(chart_air_future_near_time)) &
          !recent_no_airway
      )
  )]
events[, explicit_invasive_mode := !is.na(invasive_mode_near_time)]
events[, airway_within_120 := !is.na(airway_120_near_time)]
events[, invasive_confirmed :=
  airway_operational_confirmed | explicit_invasive_mode]
events[, strict_invasive_120 :=
  airway_within_120 | explicit_invasive_mode]
events[, niv_near_index := !is.na(niv_near_time)]
events[, vent_marker_near_index := !is.na(vent_active_near_time)]
events[, invasive_evidence_type := fifelse(
  !is.na(air_state_prior_near_value) & air_state_prior_near_value == 1,
  "prior_artificial_airway_state",
  fifelse(
    !is.na(chart_air_prior_near_time), "prior_ETT_chart",
    fifelse(
      !is.na(air_inv_future_near_time), "future_airway_confirmation",
      fifelse(!is.na(chart_air_future_near_time),
              "future_ETT_chart_confirmation",
              fifelse(explicit_invasive_mode, "explicit_invasive_mode", NA_character_))
    )
  )
)]

## ---- Early diagnosis-based infection evidence ------------------------------

infection_regex <- paste0(
  "(sepsis|septic|pneumonia|infection|infectious|bacteremia|fungaemia|fungemia|",
  "viremia|meningitis|encephalitis|endocarditis|cholangitis|cholecystitis|",
  "pyelonephritis|urinary tract infection|peritonitis|abscess|cellulitis|empyema)"
)
infection_exclude_regex <- paste0(
  "(non[- ]?infectious|without (evidence of )?infection|",
  "no (evidence of )?infection)"
)

log_msg("Reading time-stamped infection diagnoses")
dx <- fread(
  file.path(EICU, "diagnosis.csv.gz"),
  select = c("patientunitstayid", "diagnosisoffset", "diagnosisstring"),
  showProgress = FALSE
)
dx <- dx[patientunitstayid %in% adult_ids]
dx[, text_lower := tolower(diagnosisstring)]
dx <- dx[
  grepl(infection_regex, text_lower, perl = TRUE) &
    !grepl(infection_exclude_regex, text_lower, perl = TRUE) &
    diagnosisoffset >= INDEX_MIN_OFFSET - INFECTION_LOOKBACK_MIN
]
dx_events <- dx[, .(
  patientunitstayid,
  measure_time = diagnosisoffset,
  measurement_value = 1,
  measurement_source = "diagnosis",
  measurement_label = fifelse(grepl("sepsis|septic", text_lower),
                              "sepsis_or_septic", "other_infection")
)]
sepsis_events <- dx_events[measurement_label == "sepsis_or_septic"]

## admissionDx has a real chart-entry offset. The patient.apacheadmissiondx
## field is intentionally not assigned offset 0: outcome-blind audit showed
## that doing so can make post-index documentation appear available at index.
log_msg("Reading time-stamped admission diagnoses")
admission_dx <- fread(
  file.path(EICU, "admissionDx.csv.gz"),
  select = c(
    "patientunitstayid", "admitdxenteredoffset", "admitdxpath",
    "admitdxname", "admitdxtext"
  ),
  showProgress = FALSE
)
admission_dx <- admission_dx[
  patientunitstayid %in% adult_ids & !is.na(admitdxenteredoffset)
]
admission_dx[, text_lower := tolower(paste(
  fcoalesce(admitdxpath, ""), fcoalesce(admitdxname, ""),
  fcoalesce(admitdxtext, "")
))]
admission_dx <- admission_dx[
  grepl(infection_regex, text_lower, perl = TRUE) &
    !grepl(infection_exclude_regex, text_lower, perl = TRUE) &
    admitdxenteredoffset >= INDEX_MIN_OFFSET - INFECTION_LOOKBACK_MIN
]
admission_infection <- admission_dx[, .(
  patientunitstayid,
  measure_time = admitdxenteredoffset,
  measurement_value = 1,
  measurement_source = "admissionDx",
  measurement_label = fifelse(
    grepl("sepsis|septic", text_lower),
    "sepsis_or_septic", "other_infection"
  )
)]

infection_source_mapping_qc <- rbindlist(list(
  dx_events[, .(
    matched_rows = .N,
    matched_stays = uniqueN(patientunitstayid),
    offset_min = as.numeric(min(measure_time)),
    offset_median = as.numeric(median(measure_time)),
    offset_p95 = as.numeric(quantile(measure_time, .95, names = FALSE)),
    offset_max = as.numeric(max(measure_time))
  ), by = .(source = measurement_source, infection_class = measurement_label)],
  admission_infection[, .(
    matched_rows = .N,
    matched_stays = uniqueN(patientunitstayid),
    offset_min = as.numeric(min(measure_time)),
    offset_median = as.numeric(median(measure_time)),
    offset_p95 = as.numeric(quantile(measure_time, .95, names = FALSE)),
    offset_max = as.numeric(max(measure_time))
  ), by = .(source = measurement_source, infection_class = measurement_label)]
), use.names = TRUE)

dx_events <- collapse_measurements(rbindlist(
  list(dx_events, admission_infection), use.names = TRUE
))
sepsis_events <- collapse_measurements(rbindlist(
  list(sepsis_events,
       admission_infection[measurement_label == "sepsis_or_septic"]),
  use.names = TRUE
))
rm(dx, admission_dx); invisible(gc())

infection_prior <- nearest_side(
  anchors, dx_events, "prior", INFECTION_LOOKBACK_MIN, "infection_prior"
)
infection_future <- nearest_side(
  anchors, dx_events, "future", INFECTION_LOOKAHEAD_MIN, "infection_future"
)
infection_future_sens <- nearest_side(
  anchors, dx_events, "future", INFECTION_SENS_LOOKAHEAD_MIN,
  "infection_future_sens"
)
sepsis_prior <- nearest_side(
  anchors, sepsis_events, "prior", INFECTION_LOOKBACK_MIN, "sepsis_prior"
)
sepsis_future <- nearest_side(
  anchors, sepsis_events, "future", INFECTION_LOOKAHEAD_MIN, "sepsis_future"
)
sepsis_future_sens <- nearest_side(
  anchors, sepsis_events, "future", INFECTION_SENS_LOOKAHEAD_MIN,
  "sepsis_future_sens"
)
for (x in list(
  infection_prior, infection_future, infection_future_sens,
  sepsis_prior, sepsis_future, sepsis_future_sens
)) {
  events <- merge(events, x, by = "event_id", all.x = TRUE)
}
events[, infection_early :=
  !is.na(infection_prior_near_time) | !is.na(infection_future_near_time)]
events[, infection_available_by_index := !is.na(infection_prior_near_time)]
events[, infection_plus24_sensitivity :=
  infection_early | !is.na(infection_future_sens_near_time)]
events[, sepsis_early :=
  !is.na(sepsis_prior_near_time) | !is.na(sepsis_future_near_time)]
events[, sepsis_plus24_sensitivity :=
  sepsis_early | !is.na(sepsis_future_sens_near_time)]
events[, infection_time := fifelse(
  !is.na(infection_prior_near_time), infection_prior_near_time,
  infection_future_near_time
)]
events[, infection_signed_gap := infection_time - pao2_time]
events[, infection_source := fifelse(
  !is.na(infection_prior_near_source), infection_prior_near_source,
  infection_future_near_source
)]
events[, infection_plus24_time := fifelse(
  infection_early, infection_time, infection_future_sens_near_time
)]
events[, infection_plus24_signed_gap := infection_plus24_time - pao2_time]
events[, infection_plus24_source := fifelse(
  infection_early, infection_source, infection_future_sens_near_source
)]

## APACHE ventilation flags are retained only as descriptive QC; they are not
## used to define invasive ventilation because their exact timing is unavailable.
aps <- fread(
  file.path(EICU, "apacheApsVar.csv.gz"),
  select = c("patientunitstayid", "intubated", "vent"),
  showProgress = FALSE
)
aps[, apache_inv_flag := as.integer(intubated == 1 | vent == 1)]
aps <- aps[, .(apache_inv_flag = max(apache_inv_flag, na.rm = TRUE)),
           by = patientunitstayid]
aps[!is.finite(apache_inv_flag), apache_inv_flag := NA_integer_]
events <- merge(events, aps, by = "patientunitstayid", all.x = TRUE)
events[, apache_only_flag :=
  !is.na(apache_inv_flag) & apache_inv_flag == 1 &
    !strict_invasive_120 & !airway_operational_confirmed]

## ---- Sequential eligibility, first event, and first eligible stay -----------

events[, pf_paired := !is.na(fio2_near_value) & !is.na(pf_ratio)]
events[, low_oxygen := pf_paired & pf_ratio <= PF_MAX]
events[, peep_paired_ge5 :=
  !is.na(peep_near_value) & peep_near_value >= PEEP_MIN]

stage0 <- events[adult == TRUE]
stage1 <- stage0[pf_paired == TRUE]
stage2 <- stage1[low_oxygen == TRUE]
stage3 <- stage2[peep_paired_ge5 == TRUE]
stage4 <- stage3[invasive_confirmed == TRUE]
stage5 <- stage4[niv_near_index == FALSE]
stage6 <- stage5[infection_early == TRUE]
stage6_plus24 <- stage5[infection_plus24_sensitivity == TRUE]

## More time-local eICU sensitivity: an artificial-airway event or an explicit
## invasive-mode marker must be within +/-120 min. APACHE-only cases are never
## eligible because the APACHE flags lack event-level timing.
stage4_strict120 <- stage3[strict_invasive_120 == TRUE]
stage5_strict120 <- stage4_strict120[niv_near_index == FALSE]
stage6_strict120 <- stage5_strict120[
  infection_early == TRUE & apache_only_flag == FALSE
]

setorder(stage6, patientunitstayid, pao2_time, event_id)
stay_candidates <- stage6[, .SD[1], by = patientunitstayid]

## Deterministic repeat-stay ordering. patienthealthsystemstayid is used only
## after hospitaldischargeyear because eICU lacks an analyzable calendar date.
stay_candidates[, order_year := fifelse(
  is.na(hospitaldischargeyear), 9999L, as.integer(hospitaldischargeyear)
)]
stay_candidates[, order_hsp := fifelse(
  is.na(patienthealthsystemstayid), .Machine$integer.max,
  as.integer(patienthealthsystemstayid)
)]
stay_candidates[, order_visit := fifelse(
  is.na(unitvisitnumber), .Machine$integer.max, as.integer(unitvisitnumber)
)]
setorder(
  stay_candidates, person_key, order_year, order_hsp, order_visit,
  pao2_time, patientunitstayid
)
cohort <- stay_candidates[, .SD[1], by = person_key]
cohort[, c("order_year", "order_hsp", "order_visit") := NULL]
stay_candidates[, c("order_year", "order_hsp", "order_visit") := NULL]

setorder(stage6_plus24, patientunitstayid, pao2_time, event_id)
plus24_stay_candidates <- stage6_plus24[, .SD[1], by = patientunitstayid]
plus24_stay_candidates[, order_year := fifelse(
  is.na(hospitaldischargeyear), 9999L, as.integer(hospitaldischargeyear)
)]
plus24_stay_candidates[, order_hsp := fifelse(
  is.na(patienthealthsystemstayid), .Machine$integer.max,
  as.integer(patienthealthsystemstayid)
)]
plus24_stay_candidates[, order_visit := fifelse(
  is.na(unitvisitnumber), .Machine$integer.max, as.integer(unitvisitnumber)
)]
setorder(
  plus24_stay_candidates, person_key, order_year, order_hsp, order_visit,
  pao2_time, patientunitstayid
)
plus24_cohort <- plus24_stay_candidates[, .SD[1], by = person_key]
plus24_cohort[, c("order_year", "order_hsp", "order_visit") := NULL]

setorder(stage6_strict120, patientunitstayid, pao2_time, event_id)
strict120_stay_candidates <- stage6_strict120[, .SD[1], by = patientunitstayid]
strict120_stay_candidates[, order_year := fifelse(
  is.na(hospitaldischargeyear), 9999L, as.integer(hospitaldischargeyear)
)]
strict120_stay_candidates[, order_hsp := fifelse(
  is.na(patienthealthsystemstayid), .Machine$integer.max,
  as.integer(patienthealthsystemstayid)
)]
strict120_stay_candidates[, order_visit := fifelse(
  is.na(unitvisitnumber), .Machine$integer.max, as.integer(unitvisitnumber)
)]
setorder(
  strict120_stay_candidates, person_key, order_year, order_hsp, order_visit,
  pao2_time, patientunitstayid
)
strict120_cohort <- strict120_stay_candidates[, .SD[1], by = person_key]
strict120_cohort[, c("order_year", "order_hsp", "order_visit") := NULL]

## Keep only variables needed for downstream exposure construction and audits.
restricted_keep <- c(
  "patientunitstayid", "patienthealthsystemstayid", "person_key", "uniquepid",
  "hospitalid", "hospitaldischargeyear", "unitvisitnumber", "unitstaytype",
  "age_num", "gender", "pao2_time", "pao2", "fio2_near_time",
  "fio2_near_value", "fio2_near_source", "fio2_signed_gap", "fio2_abs_gap",
  "pf_ratio", "peep_near_time", "peep_near_value", "peep_near_label",
  "peep_signed_gap", "peep_abs_gap", "invasive_confirmed",
  "airway_operational_confirmed", "airway_within_120", "explicit_invasive_mode",
  "strict_invasive_120", "apache_only_flag", "invasive_evidence_type",
  "vent_marker_near_index", "niv_near_index",
  "infection_early", "infection_available_by_index", "sepsis_early",
  "infection_time", "infection_signed_gap", "infection_source",
  "infection_plus24_sensitivity", "sepsis_plus24_sensitivity",
  "infection_plus24_time", "infection_plus24_signed_gap",
  "infection_plus24_source",
  "apache_inv_flag", "unitdischargeoffset", "icu_end_offset",
  "hospitaldischargeoffset"
)
restricted_keep <- restricted_keep[restricted_keep %in% names(cohort)]
stay_candidates_out <- stay_candidates[, ..restricted_keep]
cohort_out <- cohort[, ..restricted_keep]
plus24_cohort_out <- plus24_cohort[, ..restricted_keep]
strict120_cohort_out <- strict120_cohort[, ..restricted_keep]

metadata <- list(
  version = "eicu_index_cohort_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  raw_data = normalizePath(EICU),
  script = normalizePath(file.path(
    PROJECT, "code", "R", "rebuild_v1", "02_build_eicu_index_cohort.R"
  ), mustWork = FALSE),
  locked_config_version = LOCKED$version,
  phenotype = "oxygenation-defined; not imaging-adjudicated ARDS",
  infection_definition = paste(
    "primary diagnosis.diagnosisoffset or admissionDx.admitdxenteredoffset",
    "evidence from index-48h through index; diagnosis-based, not Seymour",
    "suspected infection; patient.apacheadmissiondx is not assigned offset 0"
  ),
  infection_plus24_sensitivity =
    "diagnosis evidence from index-48h through index+24h; sensitivity only",
  eicu_specific_operational_rule = paste(
    "Primary invasive confirmation uses explicit artificial-airway evidence",
    "with 720-min lookback/120-min documentation grace or an explicit invasive",
    "mode within 120 min; this is not a validated ventilation episode."
  ),
  strict_invasive_sensitivity = paste(
    "Artificial-airway evidence or explicit invasive mode within +/-120 min;",
    "proximal NIV and APACHE-only evidence excluded."
  ),
  parameters = list(
    age_min = AGE_MIN, index_min_offset = INDEX_MIN_OFFSET,
    index_upper_bound = "unitdischargeoffset",
    pf_fio2_window_min = PF_FIO2_WINDOW_MIN,
    peep_window_min = PEEP_WINDOW_MIN, peep_min = PEEP_MIN, pf_max = PF_MAX,
    invasive_lookback_min = INV_AIRWAY_LOOKBACK_MIN,
    invasive_lookahead_min = INV_AIRWAY_LOOKAHEAD_MIN,
    niv_exclusion_window_min = NIV_EXCLUSION_WINDOW_MIN,
    infection_lookback_min = INFECTION_LOOKBACK_MIN,
    infection_lookahead_min = INFECTION_LOOKAHEAD_MIN,
    infection_sensitivity_lookahead_min = INFECTION_SENS_LOOKAHEAD_MIN
  )
)
attr(stay_candidates_out, "rebuild_metadata") <- metadata
attr(cohort_out, "rebuild_metadata") <- metadata
attr(plus24_cohort_out, "rebuild_metadata") <- metadata
attr(strict120_cohort_out, "rebuild_metadata") <- metadata

saveRDS(
  stay_candidates_out,
  file.path(OUT_PRIVATE, "eicu_index_stay_candidates_v1.rds"),
  compress = "xz"
)
saveRDS(
  cohort_out,
  file.path(OUT_PRIVATE, "eicu_index_cohort_v1.rds"),
  compress = "xz"
)
saveRDS(
  plus24_cohort_out,
  file.path(OUT_PRIVATE, "eicu_index_cohort_infection_plus24_sensitivity_v1.rds"),
  compress = "xz"
)
saveRDS(
  strict120_cohort_out,
  file.path(OUT_PRIVATE, "eicu_index_cohort_strict120_sensitivity_v1.rds"),
  compress = "xz"
)

## ---- Aggregate QC -----------------------------------------------------------

niv_mapping_qc <- niv_candidate_rows[, .(
  raw_rows = .N,
  adult_stays_with_value = uniqueN(patientunitstayid)
), by = .(
  label = respchartvaluelabel,
  value = respchartvalue,
  classified_as_niv
)]
setorder(niv_mapping_qc, label, -raw_rows, value)
fwrite(niv_mapping_qc, file.path(OUT_QC, "qc_niv_value_mapping_v1.csv"))
setorder(infection_source_mapping_qc, source, infection_class)
fwrite(
  infection_source_mapping_qc,
  file.path(OUT_QC, "qc_infection_source_mapping_v1.csv")
)

count_stage <- function(x, step, rule) {
  data.table(
    step = step,
    rule = rule,
    n_events = nrow(x),
    n_icu_stays = uniqueN(x$patientunitstayid),
    n_unique_patients = uniqueN(x$person_key)
  )
}
funnel <- rbindlist(list(
  data.table(
    step = "L0_all_eICU_stays", rule = "all patient.csv ICU stays",
    n_events = NA_integer_, n_icu_stays = uniqueN(pat$patientunitstayid),
    n_unique_patients = uniqueN(pat$person_key)
  ),
  data.table(
    step = "L1_adult_stays", rule = paste0("age >= ", AGE_MIN),
    n_events = NA_integer_, n_icu_stays = uniqueN(pat[adult == TRUE]$patientunitstayid),
    n_unique_patients = uniqueN(pat[adult == TRUE]$person_key)
  ),
  count_stage(stage1, "L2_in_ICU_PF_pair",
              paste0("PaO2 with valid FiO2 within +/-", PF_FIO2_WINDOW_MIN, " min")),
  count_stage(stage2, "L3_low_oxygen",
              paste0("P/F <= ", PF_MAX)),
  count_stage(stage3, "L4_same_window_PEEP",
              paste0("PEEP >= ", PEEP_MIN, " within +/-", PEEP_WINDOW_MIN, " min")),
  count_stage(stage4, "L5_explicit_invasive_airway",
              "explicit artificial-airway evidence at/near index"),
  count_stage(stage5, "L6_exclude_proximal_NIV",
              paste0("no explicit NIV evidence within +/-", NIV_EXCLUSION_WINDOW_MIN, " min")),
  count_stage(stage6, "L7_early_infection_diagnosis",
              paste0("infection diagnosis from -", INFECTION_LOOKBACK_MIN,
                     " to +", INFECTION_LOOKAHEAD_MIN, " min of index")),
  data.table(
    step = "L8_first_eligible_stay_per_patient",
    rule = "first qualifying event per stay; deterministic first eligible stay per uniquepid",
    n_events = nrow(cohort_out), n_icu_stays = nrow(cohort_out),
    n_unique_patients = uniqueN(cohort_out$person_key)
  ),
  count_stage(
    stage6_plus24, "I1_infection_plus24_sensitivity_events",
    "sensitivity: infection diagnosis from index-48 h through index+24 h"
  ),
  data.table(
    step = "I2_infection_plus24_first_eligible_stay_per_patient",
    rule = "infection +24 h sensitivity after first eligible stay per uniquepid",
    n_events = nrow(plus24_cohort_out), n_icu_stays = nrow(plus24_cohort_out),
    n_unique_patients = uniqueN(plus24_cohort_out$person_key)
  ),
  count_stage(
    stage6_strict120, "S1_strict120_eligible_events",
    "sensitivity: artificial airway or explicit invasive mode within +/-120 min; no NIV/APACHE-only"
  ),
  data.table(
    step = "S2_strict120_first_eligible_stay_per_patient",
    rule = "strict120 sensitivity after first eligible stay per uniquepid",
    n_events = nrow(strict120_cohort_out), n_icu_stays = nrow(strict120_cohort_out),
    n_unique_patients = uniqueN(strict120_cohort_out$person_key)
  )
), use.names = TRUE)
fwrite(funnel, file.path(OUT_QC, "qc_funnel_v1.csv"))

pairing_qc <- rbindlist(list(
  metric_quantiles(stage3$fio2_signed_gap, "FiO2_minus_PaO2_minutes", "PEEP-qualified events"),
  metric_quantiles(stage3$peep_signed_gap, "PEEP_minus_PaO2_minutes", "PEEP-qualified events"),
  metric_quantiles(cohort_out$fio2_signed_gap, "FiO2_minus_PaO2_minutes"),
  metric_quantiles(cohort_out$peep_signed_gap, "PEEP_minus_PaO2_minutes"),
  metric_quantiles(cohort_out$pao2_time, "index_offset_from_ICU_admission_minutes"),
  metric_quantiles(cohort_out$pf_ratio, "P_F_ratio"),
  metric_quantiles(cohort_out$peep_near_value, "PEEP_cmH2O"),
  metric_quantiles(cohort_out$infection_signed_gap, "infection_dx_minus_index_minutes")
), use.names = TRUE, fill = TRUE)
fwrite(pairing_qc, file.path(OUT_QC, "qc_pairing_and_timing_v1.csv"))

source_tab <- function(x, domain_name, source_vector) {
  z <- data.table(source = fifelse(is.na(source_vector), "missing", source_vector))[
    , .N, by = source
  ]
  z[, domain := domain_name]
  setcolorder(z, c("domain", "source", "N"))
  z
}
source_qc <- rbindlist(list(
  source_tab(cohort_out, "FiO2", cohort_out$fio2_near_source),
  source_tab(cohort_out, "PEEP_label", cohort_out$peep_near_label),
  source_tab(cohort_out, "invasive_evidence", cohort_out$invasive_evidence_type),
  source_tab(cohort_out, "infection_source", cohort_out$infection_source)
), use.names = TRUE)
setorder(source_qc, domain, -N, source)
fwrite(source_qc, file.path(OUT_QC, "qc_source_coverage_v1.csv"))

evidence_count <- function(x, subset_name, evidence_name, flag_name) {
  keep <- !is.na(x[[flag_name]]) & as.logical(x[[flag_name]])
  y <- x[keep]
  data.table(
    subset = subset_name,
    evidence = evidence_name,
    n_events = nrow(y),
    n_icu_stays = uniqueN(y$patientunitstayid),
    n_unique_patients = uniqueN(y$person_key)
  )
}
evidence_qc <- rbindlist(lapply(
  list(
    list(stage3, "P/F<=300 + PEEP>=5 events", "artificial_airway_operational_720_120", "airway_operational_confirmed"),
    list(stage3, "P/F<=300 + PEEP>=5 events", "artificial_airway_within_120", "airway_within_120"),
    list(stage3, "P/F<=300 + PEEP>=5 events", "explicit_invasive_mode_within_120", "explicit_invasive_mode"),
    list(stage3, "P/F<=300 + PEEP>=5 events", "APACHE_vent_or_intubated_flag", "apache_inv_flag"),
    list(stage3, "P/F<=300 + PEEP>=5 events", "APACHE_only_no_time_local_evidence", "apache_only_flag"),
    list(stage3, "P/F<=300 + PEEP>=5 events", "primary_invasive_confirmation", "invasive_confirmed"),
    list(stage3, "P/F<=300 + PEEP>=5 events", "strict120_invasive_confirmation", "strict_invasive_120"),
    list(cohort, "final primary cohort", "artificial_airway_operational_720_120", "airway_operational_confirmed"),
    list(cohort, "final primary cohort", "artificial_airway_within_120", "airway_within_120"),
    list(cohort, "final primary cohort", "explicit_invasive_mode_within_120", "explicit_invasive_mode"),
    list(cohort, "final primary cohort", "APACHE_vent_or_intubated_flag", "apache_inv_flag"),
    list(cohort, "final primary cohort", "APACHE_only_no_time_local_evidence", "apache_only_flag"),
    list(cohort, "final primary cohort", "strict120_invasive_confirmation", "strict_invasive_120")
  ),
  function(z) evidence_count(z[[1]], z[[2]], z[[3]], z[[4]])
), use.names = TRUE)
fwrite(evidence_qc, file.path(OUT_QC, "qc_invasive_evidence_sources_v1.csv"))

phenotype_qc <- data.table(
  metric = c(
    "final_n", "final_hospitals",
    "final_infection_available_by_index_n", "final_sepsis_early_n",
    "final_vent_marker_near_index_n", "final_apache_invasive_flag_n",
    "candidate_stays_before_patient_dedup_n", "patients_with_gt1_eligible_stay_n",
    "missing_uniquepid_final_n", "PEEP_label_contains_PEEP_CPAP_n",
    "all_final_indices_within_ICU_stay_n", "final_index_within_first_24h_n",
    "strict120_sensitivity_final_n", "infection_plus24_sensitivity_final_n",
    "infection_plus24_postindex_only_n"
  ),
  value = c(
    nrow(cohort_out),
    uniqueN(cohort_out$hospitalid),
    sum(cohort_out$infection_available_by_index, na.rm = TRUE),
    sum(cohort_out$sepsis_early, na.rm = TRUE),
    sum(cohort_out$vent_marker_near_index, na.rm = TRUE),
    sum(cohort_out$apache_inv_flag == 1, na.rm = TRUE),
    nrow(stay_candidates_out),
    stay_candidates[, .N, by = person_key][N > 1, .N],
    sum(is.na(cohort_out$uniquepid) | !nzchar(trimws(cohort_out$uniquepid))),
    sum(grepl("PEEP/CPAP", cohort_out$peep_near_label, fixed = TRUE), na.rm = TRUE),
    sum(cohort_out$pao2_time >= INDEX_MIN_OFFSET &
          cohort_out$pao2_time <= cohort_out$icu_end_offset, na.rm = TRUE),
    sum(cohort_out$pao2_time <= 1440, na.rm = TRUE),
    nrow(strict120_cohort_out),
    nrow(plus24_cohort_out),
    sum(!plus24_cohort_out$infection_early, na.rm = TRUE)
  ),
  denominator = c(
    NA, nrow(cohort_out), nrow(cohort_out), nrow(cohort_out),
    nrow(cohort_out), nrow(cohort_out), NA, uniqueN(stay_candidates$person_key),
    nrow(cohort_out), nrow(cohort_out), nrow(cohort_out), nrow(cohort_out), NA,
    NA, nrow(plus24_cohort_out)
  )
)
fwrite(phenotype_qc, file.path(OUT_QC, "qc_phenotype_summary_v1.csv"))

## Aggregate overlap with the legacy cohort, if its checkpoint is present.
legacy_path <- file.path(PROJECT, "checkpoints", "eicu_analysis_master.rds")
if (file.exists(legacy_path)) {
  legacy <- as.data.table(readRDS(legacy_path))
  legacy_id_name <- if ("patientunitstayid" %in% names(legacy)) {
    "patientunitstayid"
  } else if ("stay_id" %in% names(legacy)) {
    "stay_id"
  } else {
    NA_character_
  }
  if (!is.na(legacy_id_name)) {
    old_ids <- unique(legacy[[legacy_id_name]])
    new_ids <- unique(cohort_out$patientunitstayid)
    reconciliation <- data.table(
      metric = c("legacy_stays", "strict_new_stays", "overlap", "legacy_only",
                 "strict_new_only", "jaccard"),
      value = c(
        length(old_ids), length(new_ids), length(intersect(old_ids, new_ids)),
        length(setdiff(old_ids, new_ids)), length(setdiff(new_ids, old_ids)),
        length(intersect(old_ids, new_ids)) / length(union(old_ids, new_ids))
      )
    )
    fwrite(reconciliation,
           file.path(OUT_QC, "qc_legacy_reconciliation_v1.csv"))
  }
}

parameters <- data.table(
  parameter = c(
    "locked_config_version", "age_min", "index_min_offset", "index_upper_bound",
    "pf_fio2_window_min",
    "peep_window_min", "peep_min", "pf_max", "inv_airway_lookback_min",
    "inv_airway_lookahead_min", "recent_no_airway_min",
    "niv_exclusion_window_min", "infection_lookback_min",
    "infection_lookahead_min", "infection_sensitivity_lookahead_min",
    "primary_exposure_window_hours_after_index",
    "eicu_primary_infection_sources",
    "eicu_specific_invasive_operational_rule", "strict120_sensitivity_rule",
    "phase1_outcome_blinding", "run_timestamp", "R_version",
    "repeat_hospitalization_ordering"
  ),
  value = as.character(c(
    LOCKED$version, AGE_MIN, INDEX_MIN_OFFSET, "unitdischargeoffset",
    PF_FIO2_WINDOW_MIN,
    PEEP_WINDOW_MIN, PEEP_MIN, PF_MAX, INV_AIRWAY_LOOKBACK_MIN,
    INV_AIRWAY_LOOKAHEAD_MIN, RECENT_NO_AIRWAY_MIN,
    NIV_EXCLUSION_WINDOW_MIN, INFECTION_LOOKBACK_MIN,
    INFECTION_LOOKAHEAD_MIN, INFECTION_SENS_LOOKAHEAD_MIN,
    LOCKED$primary_exposure_window_hours_after_index,
    "diagnosis.diagnosisoffset + admissionDx.admitdxenteredoffset; patient.apacheadmissiondx offset-0 disabled",
    "airway lookback 720 min / documentation grace 120 min OR explicit invasive mode within 120 min; not a validated episode",
    "airway or explicit invasive mode within +/-120 min; proximal NIV and APACHE-only excluded",
    "outcome columns and outcome counts omitted from new Phase 1 cohort/QC outputs",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    R.version.string,
    "hospitaldischargeyear_then_patienthealthsystemstayid_then_unitvisitnumber"
  ))
)
fwrite(parameters, file.path(OUT_QC, "run_parameters_v1.csv"))

## ---- Hard QC invariants -----------------------------------------------------

invariants <- data.table(
  check = c(
    "nonempty_cohort", "one_row_per_person_key", "age_ge_18", "index_within_ICU_stay",
    "FiO2_gap_within_120", "PEEP_gap_within_120", "PF_le_300",
    "PEEP_ge_5", "explicit_invasive_confirmation", "no_proximal_NIV",
    "infection_in_locked_window", "strict120_has_time_local_evidence",
    "strict120_excludes_APACHE_only", "primary_has_no_postindex_infection_only",
    "plus24_sensitivity_in_declared_window"
  ),
  passed = c(
    nrow(cohort_out) > 0,
    !anyDuplicated(cohort_out$person_key),
    all(cohort_out$age_num >= AGE_MIN),
    all(cohort_out$pao2_time >= INDEX_MIN_OFFSET &
          cohort_out$pao2_time <= cohort_out$icu_end_offset),
    all(cohort_out$fio2_abs_gap <= PF_FIO2_WINDOW_MIN),
    all(cohort_out$peep_abs_gap <= PEEP_WINDOW_MIN),
    all(cohort_out$pf_ratio <= PF_MAX),
    all(cohort_out$peep_near_value >= PEEP_MIN),
    all(cohort_out$invasive_confirmed),
    all(!cohort_out$niv_near_index),
    all(cohort_out$infection_signed_gap >= -INFECTION_LOOKBACK_MIN &
          cohort_out$infection_signed_gap <= INFECTION_LOOKAHEAD_MIN),
    all(strict120_cohort_out$strict_invasive_120),
    all(!strict120_cohort_out$apache_only_flag),
    all(cohort_out$infection_signed_gap <= 0),
    all(plus24_cohort_out$infection_plus24_signed_gap >= -INFECTION_LOOKBACK_MIN &
          plus24_cohort_out$infection_plus24_signed_gap <=
            INFECTION_SENS_LOOKAHEAD_MIN)
  )
)
fwrite(invariants, file.path(OUT_QC, "qc_invariants_v1.csv"))
if (!all(invariants$passed)) {
  stop("One or more strict-cohort QC invariants failed; see qc_invariants_v1.csv")
}

## Guard against accidental identifier leakage into aggregate CSVs.
sensitive_names <- c(
  "patientunitstayid", "patienthealthsystemstayid", "uniquepid", "person_key"
)
aggregate_csvs <- list.files(OUT_QC, pattern = "\\.csv$", full.names = TRUE)
for (f in aggregate_csvs) {
  nms <- names(fread(f, nrows = 0, showProgress = FALSE))
  if (any(nms %chin% sensitive_names)) {
    stop("Identifier-like column found in aggregate output: ", basename(f))
  }
}

## Phase 1 remains outcome-blind. Discharge offsets are observation bounds and
## discharge year is a deterministic repeat-encounter ordering proxy; neither
## is an outcome status. Explicit outcome/status columns remain prohibited.
prohibited_outcome_names <- c(
  "hospitaldischargestatus", "unitdischargestatus", "hospital_expire_flag",
  "deathtime", "dod", "died_hosp", "died_28d", "died_icu"
)
private_objects <- list(
  stay_candidates = stay_candidates_out,
  primary = cohort_out,
  infection_plus24_sensitivity = plus24_cohort_out,
  strict120_sensitivity = strict120_cohort_out
)
private_name_guard <- all(vapply(
  private_objects,
  function(x) !any(tolower(names(x)) %chin% prohibited_outcome_names),
  logical(1L)
))
aggregate_token_guard <- TRUE
outcome_token_regex <- paste0(
  "mortality|death|deathtime|died_|hospital_expire|",
  "hospitaldischargestatus|unitdischargestatus"
)
for (f in aggregate_csvs) {
  z <- fread(f, showProgress = FALSE)
  txt <- paste(unlist(z, use.names = TRUE), collapse = " ")
  if (grepl(outcome_token_regex, tolower(txt), perl = TRUE)) {
    aggregate_token_guard <- FALSE
    break
  }
}
outcome_guard <- data.table(
  check = c(
    "private_cohort_column_names_exclude_outcome_status",
    "aggregate_QC_content_excludes_outcome_tokens"
  ),
  passed = c(private_name_guard, aggregate_token_guard)
)
fwrite(outcome_guard, file.path(OUT_QC, "qc_outcome_leakage_guard_v1.csv"))
if (!all(outcome_guard$passed)) {
  stop("eICU Phase-1 outcome leakage guard failed")
}

log_msg(
  "Strict eICU index cohort complete:", nrow(cohort_out), "patients/stays;",
  uniqueN(cohort_out$hospitalid), "hospitals; outcomes not loaded/reported"
)
log_msg("Restricted outputs:", OUT_PRIVATE)
log_msg("Aggregate QC:", OUT_QC)

## Publish the downstream completion gate only after every formal product and
## required aggregate audit exists and every hard check has passed.
formal_rds <- c(
  stay_candidates = file.path(OUT_PRIVATE, "eicu_index_stay_candidates_v1.rds"),
  primary_cohort = file.path(OUT_PRIVATE, "eicu_index_cohort_v1.rds"),
  infection_plus24_sensitivity = file.path(
    OUT_PRIVATE, "eicu_index_cohort_infection_plus24_sensitivity_v1.rds"
  ),
  strict120_sensitivity = file.path(
    OUT_PRIVATE, "eicu_index_cohort_strict120_sensitivity_v1.rds"
  )
)
required_qc <- file.path(OUT_QC, c(
  "qc_funnel_v1.csv", "qc_pairing_and_timing_v1.csv",
  "qc_source_coverage_v1.csv", "qc_invasive_evidence_sources_v1.csv",
  "qc_niv_value_mapping_v1.csv", "qc_infection_source_mapping_v1.csv",
  "qc_phenotype_summary_v1.csv",
  "run_parameters_v1.csv", "qc_invariants_v1.csv",
  "qc_outcome_leakage_guard_v1.csv"
))
if (!all(file.exists(formal_rds))) {
  stop("One or more formal eICU Phase-1 RDS products are missing")
}
if (!all(file.exists(required_qc))) {
  stop("One or more required eICU Phase-1 QC products are missing")
}
formal_rds_sha256 <- vapply(
  formal_rds,
  function(path) digest::digest(file = path, algo = "sha256"),
  character(1L)
)
completion_gate <- data.table(
  field = c(
    "config_path", "locked_config_version", "script_sha256", "completed_at",
    "all_invariants_pass", "outcome_leakage_guard_pass",
    "all_required_qc_present", "primary_cohort_n",
    paste0(names(formal_rds), "_rds_sha256")
  ),
  value = as.character(c(
    normalizePath(CONFIG_PATH), LOCKED$version, SCRIPT_SHA256,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    all(invariants$passed), all(outcome_guard$passed),
    all(file.exists(required_qc)), nrow(cohort_out), formal_rds_sha256
  ))
)
fwrite(completion_gate, PHASE1_COMPLETE_TMP)
if (!file.rename(PHASE1_COMPLETE_TMP, PHASE1_COMPLETE)) {
  stop("Could not atomically publish the eICU Phase-1 completion gate")
}
log_msg("BUILD_COMPLETE | config", LOCKED$version, "| script SHA256", SCRIPT_SHA256)
