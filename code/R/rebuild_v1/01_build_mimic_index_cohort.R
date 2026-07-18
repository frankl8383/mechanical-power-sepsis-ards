#!/usr/bin/env Rscript

## =============================================================================
## 01_build_mimic_index_cohort.R
## MIMIC-IV v3.1: strict, time-aligned oxygenation-defined index cohort
##
## This is a new reconstruction workflow. It never overwrites legacy scripts,
## checkpoints, or submission outputs.
##
## Primary phenotype (LOCKED v1.0.1)
##   - age >=18 years;
##   - a known-arterial PaO2 paired with the closest valid FiO2 within +/-2 h;
##   - P/F <=300 and the closest valid PEEP >=5 cmH2O within +/-2 h;
##   - explicit invasive-ventilation evidence at that PaO2 time;
##   - no proximal explicit NIV evidence;
##   - Seymour-style suspected infection (antibiotic plus culture) whose onset
##     is in the 48 h before the respiratory index and whose paired evidence is
##     fully available by the index;
##   - first qualifying event per ICU stay, then first qualifying stay per
##     patient.
##
## Interpretive boundary: this is an oxygenation-defined acute hypoxemic
## respiratory-failure phenotype. It is not imaging-adjudicated ARDS.
##
## Privacy boundary
##   Row/stay/patient-level outputs are written only under:
##     analysis_rebuild_v1/private/mimic/
##   Disclosure-safe aggregate QC is written under:
##     analysis_rebuild_v1/qc/mimic/
##
## Official concept reference:
##   MIT-LCP mimic-code commit 5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4
##   measurement/bg.sql; measurement/ventilator_setting.sql;
##   treatment/ventilation.sql; medication/antibiotic.sql;
##   sepsis/suspicion_of_infection.sql.
## =============================================================================

suppressPackageStartupMessages(library(data.table))

PROJECT_BOOTSTRAP <- Sys.getenv(
  "ARDS_MP_PROJECT_ROOT",
  getwd()
)
CONFIG_PATH <- file.path(PROJECT_BOOTSTRAP, "code", "R", "rebuild_v1", "00_config.R")
if (!file.exists(CONFIG_PATH)) stop("Locked configuration not found: ", CONFIG_PATH)
source(CONFIG_PATH, local = FALSE)

PROJECT <- PROJECT_ROOT
MIMIC <- MIMIC_ROOT
OUT_PRIVATE <- file.path(PRIVATE_ROOT, "mimic")
OUT_QC <- file.path(QC_ROOT, "mimic")
CACHE_DIR <- file.path(OUT_PRIVATE, "cache_v1")
dir.create(OUT_PRIVATE, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_QC, recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
RUN_LOG <- file.path(OUT_QC, "run_log_v1.txt")
if (file.exists(RUN_LOG)) unlink(RUN_LOG)
PHASE1_COMPLETE <- file.path(OUT_QC, "phase1_complete_v1.csv")
PHASE1_COMPLETE_TMP <- paste0(PHASE1_COMPLETE, ".tmp")
## A completion marker is a downstream gate, not a progress file. Remove any
## marker (and interrupted temporary marker) before doing substantive work.
unlink(c(PHASE1_COMPLETE, PHASE1_COMPLETE_TMP), force = TRUE)
RUN_STARTED_AT <- Sys.time()
SCRIPT_PATH <- file.path(
  PROJECT, "code", "R", "rebuild_v1", "01_build_mimic_index_cohort.R"
)
SCRIPT_SHA256 <- if (requireNamespace("digest", quietly = TRUE)) {
  digest::digest(file = SCRIPT_PATH, algo = "sha256")
} else NA_character_

AGE_MIN <- LOCKED$minimum_age_years
PF_MAX <- LOCKED$pf_threshold_mmHg
PEEP_MIN <- LOCKED$minimum_index_peep_cmH2O
PF_WINDOW_MIN <- LOCKED$pao2_fio2_pair_window_minutes
PEEP_WINDOW_MIN <- LOCKED$pao2_peep_pair_window_minutes
INFECTION_BEFORE_H <- LOCKED$infection_window_hours_before_index
INFECTION_AFTER_H <- LOCKED$infection_window_hours_after_index
SENSITIVITY_INFECTION_AFTER_H <-
  LOCKED$sensitivity_infection_window_hours_after_index
VENT_STATUS_LOOKBACK_MIN <- 14L * 60L
NIV_EXCLUSION_WINDOW_MIN <- PF_WINDOW_MIN
REFRESH_CACHE <- identical(Sys.getenv("MIMIC_REBUILD_REFRESH_CACHE", "0"), "1")

log_msg <- function(...) {
  line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ...)
  cat(line, "\n")
  cat(line, "\n", file = RUN_LOG, append = TRUE)
  flush.console()
}

as_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = "UTC"))
  as.POSIXct(x, tz = "UTC")
}

max_num_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (any(is.finite(x))) max(x[is.finite(x)]) else NA_real_
}

first_character_or_na <- function(x) {
  x <- as.character(x)
  i <- which(!is.na(x) & nzchar(trimws(x)))
  if (length(i)) x[i[1L]] else NA_character_
}

first_numeric_or_na <- function(x) {
  i <- which(!is.na(x))
  if (length(i)) as.numeric(x[i[1L]]) else NA_real_
}

read_gz_filtered <- function(path, awk_condition) {
  cmd <- sprintf(
    "gzip -cd %s | awk -F',' 'NR==1 || (%s)'",
    shQuote(path), awk_condition
  )
  fread(cmd = cmd, showProgress = FALSE)
}

read_or_build_cache <- function(cache_path, builder) {
  if (file.exists(cache_path) && !REFRESH_CACHE) {
    log_msg("Reading private filtered cache:", basename(cache_path))
    return(readRDS(cache_path))
  }
  x <- builder()
  saveRDS(x, cache_path, compress = "gzip")
  x
}

select_measurements_by_hierarchy <- function(x) {
  stopifnot(all(c(
    "stay_id", "measure_time", "measurement_value",
    "measurement_source", "measurement_label", "source_rank",
    "record_time", "observation_id"
  ) %in% names(x)))
  x <- x[
    !is.na(stay_id) & !is.na(measure_time) & is.finite(measurement_value)
  ]
  ## Duplicate rows are collapsed only within an identical source/rank/time.
  ## Values from different sources are never averaged together.
  x <- x[, .(
    measurement_value = median(measurement_value, na.rm = TRUE),
    record_time = if (all(is.na(record_time))) as.POSIXct(NA) else
      max(record_time, na.rm = TRUE),
    observation_id = max(suppressWarnings(as.numeric(observation_id)), na.rm = TRUE)
  ), by = .(
    stay_id, measure_time, source_rank,
    measurement_source, measurement_label
  )]
  x[!is.finite(observation_id), observation_id := NA_real_]
  x[, record_time_order := as.numeric(record_time)]
  x[!is.finite(record_time_order), record_time_order := -Inf]
  x[, observation_id_order := suppressWarnings(as.numeric(observation_id))]
  x[!is.finite(observation_id_order), observation_id_order := -Inf]
  setorder(
    x, stay_id, measure_time, source_rank,
    -record_time_order, -observation_id_order
  )
  x <- x[, .SD[1L], by = .(stay_id, measure_time)]
  x[, c("record_time_order", "observation_id_order") := NULL]
  x
}

nearest_side <- function(anchor, measure, direction = c("prior", "future"),
                         max_gap_min, prefix) {
  direction <- match.arg(direction)
  a <- unique(anchor[, .(stay_id, event_id, anchor_time)])
  if (anyDuplicated(a$event_id)) stop("event_id must uniquely identify anchors")

  empty <- function() {
    z <- a[, .(event_id)]
    z[, c("near_time", "near_value", "near_source", "near_label",
          "signed_gap_min", "abs_gap_min") := .(
      as.POSIXct(NA), NA_real_, NA_character_, NA_character_, NA_real_, NA_real_
    )]
    setnames(z, setdiff(names(z), "event_id"),
             paste0(prefix, "_", setdiff(names(z), "event_id")))
    z
  }
  if (!nrow(measure)) return(empty())

  m <- unique(measure[, .(
    stay_id, measure_time, measurement_value,
    measurement_source, measurement_label
  )])
  m[, measure_time_keep := measure_time]
  setkey(m, stay_id, measure_time)

  if (direction == "prior") {
    z <- m[a,
      on = .(stay_id, measure_time <= anchor_time), mult = "last",
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
      on = .(stay_id, measure_time >= anchor_time), mult = "first",
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
  z[, signed_gap_min := as.numeric(difftime(
    near_time, anchor_time, units = "mins"
  ))]
  z[, abs_gap_min := abs(signed_gap_min)]
  z[
    is.na(near_time) | abs_gap_min > max_gap_min |
      (direction == "prior" & signed_gap_min > 0) |
      (direction == "future" & signed_gap_min < 0),
    c("near_time", "near_value", "near_source", "near_label",
      "signed_gap_min", "abs_gap_min") := .(
        as.POSIXct(NA), NA_real_, NA_character_, NA_character_, NA_real_, NA_real_
      )
  ]
  z[, anchor_time := NULL]
  setnames(z, setdiff(names(z), "event_id"),
           paste0(prefix, "_", setdiff(names(z), "event_id")))
  z
}

nearest_symmetric <- function(anchor, measure, max_gap_min, prefix) {
  b <- nearest_side(anchor, measure, "prior", max_gap_min, "prior")
  f <- nearest_side(anchor, measure, "future", max_gap_min, "future")
  bl <- b[, .(
    event_id, near_time = prior_near_time, near_value = prior_near_value,
    near_source = prior_near_source, near_label = prior_near_label,
    signed_gap_min = prior_signed_gap_min, abs_gap_min = prior_abs_gap_min,
    tie_rank = 0L
  )]
  fl <- f[, .(
    event_id, near_time = future_near_time, near_value = future_near_value,
    near_source = future_near_source, near_label = future_near_label,
    signed_gap_min = future_signed_gap_min, abs_gap_min = future_abs_gap_min,
    tie_rank = 1L
  )]
  z <- rbindlist(list(bl, fl), use.names = TRUE)
  z <- z[!is.na(near_time)]
  if (nrow(z)) {
    setorder(z, event_id, abs_gap_min, tie_rank, near_time)
    z <- z[, .SD[1L], by = event_id]
    z[, tie_rank := NULL]
  }
  z <- merge(unique(anchor[, .(event_id)]), z, by = "event_id", all.x = TRUE)
  setnames(z, setdiff(names(z), "event_id"),
           paste0(prefix, "_", setdiff(names(z), "event_id")))
  z
}

metric_quantiles <- function(x, metric, subset_name = "final_cohort") {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  probs <- c(0, .05, .25, .5, .75, .95, 1)
  q <- if (length(x)) {
    as.numeric(quantile(x, probs, names = FALSE, na.rm = TRUE))
  } else rep(NA_real_, length(probs))
  data.table(
    subset = subset_name, metric = metric, n_nonmissing = length(x),
    min = q[1], p05 = q[2], p25 = q[3], median = q[4],
    p75 = q[5], p95 = q[6], max = q[7]
  )
}

required_files <- c(
  "icu/icustays.csv.gz", "icu/chartevents.csv.gz",
  "icu/procedureevents.csv.gz", "hosp/patients.csv.gz",
  "hosp/admissions.csv.gz", "hosp/labevents.csv.gz",
  "hosp/prescriptions.csv.gz", "hosp/microbiologyevents.csv.gz"
)
missing_files <- required_files[!file.exists(file.path(MIMIC, required_files))]
if (length(missing_files)) {
  stop("Missing required MIMIC-IV files: ", paste(missing_files, collapse = ", "))
}

log_msg(
  "Starting clean MIMIC rebuild; locked config", LOCKED$version,
  "; script SHA256", SCRIPT_SHA256
)

## ---- Adult ICU-stay denominator --------------------------------------------

log_msg("Reading ICU stays, patients, and admissions")
stays <- fread(
  file.path(MIMIC, "icu", "icustays.csv.gz"),
  select = c("subject_id", "hadm_id", "stay_id", "first_careunit",
             "last_careunit", "intime", "outtime", "los"),
  showProgress = FALSE
)
patients <- fread(
  file.path(MIMIC, "hosp", "patients.csv.gz"),
  select = c("subject_id", "gender", "anchor_age", "anchor_year"),
  showProgress = FALSE
)
admissions <- fread(
  file.path(MIMIC, "hosp", "admissions.csv.gz"),
  select = c("subject_id", "hadm_id", "admittime", "admission_type"),
  showProgress = FALSE
)
for (v in c("intime", "outtime")) set(stays, j = v, value = as_utc(stays[[v]]))
admissions[, admittime := as_utc(admittime)]

stays <- merge(stays, patients, by = "subject_id", all.x = TRUE)
stays <- merge(
  stays, admissions,
  by = c("subject_id", "hadm_id"), all.x = TRUE,
  suffixes = c("", "_admission")
)
stays[, age_at_admission := anchor_age +
        as.integer(format(admittime, "%Y")) - anchor_year]
stays[, adult := !is.na(age_at_admission) & age_at_admission >= AGE_MIN]
adult_stays <- stays[
  adult == TRUE & !is.na(intime) & !is.na(outtime) & outtime > intime
]
adult_ids <- adult_stays$stay_id

## ---- Stream selected blood-gas and ventilator rows -------------------------

lab_cache <- file.path(CACHE_DIR, "selected_bg_labevents_v1.rds")
LAB_CACHE_PREEXISTED <- file.exists(lab_cache)
lab <- read_or_build_cache(lab_cache, function() {
  log_msg("Streaming selected blood-gas rows from labevents.csv.gz")
  x <- read_gz_filtered(
    file.path(MIMIC, "hosp", "labevents.csv.gz"),
    '$5==52033 || $5==50816 || $5==50819 || $5==50821'
  )
  x[, .(
    specimen_id, subject_id, hadm_id, charttime, storetime,
    itemid, value, valuenum, valueuom
  )]
})
lab[, charttime := as_utc(charttime)]
lab[, storetime := as_utc(storetime)]

bg <- lab[, .(
  subject_id = first_numeric_or_na(subject_id),
  hadm_id = first_numeric_or_na(hadm_id),
  charttime = if (all(is.na(charttime))) as.POSIXct(NA) else max(charttime, na.rm = TRUE),
  storetime = if (all(is.na(storetime))) as.POSIXct(NA) else max(storetime, na.rm = TRUE),
  specimen = first_character_or_na(value[itemid == 52033L]),
  pao2 = max_num_or_na(valuenum[itemid == 50821L]),
  fio2_lab_raw = max_num_or_na(valuenum[itemid == 50816L]),
  peep_lab_raw = max_num_or_na(valuenum[itemid == 50819L])
), by = specimen_id]
bg[, arterial_known := grepl("art", tolower(specimen))]
bg <- bg[
  arterial_known == TRUE & is.finite(pao2) &
    pao2 >= LOCKED$physiologic_ranges$pao2_mmHg[1] &
    pao2 <= LOCKED$physiologic_ranges$pao2_mmHg[2] &
    !is.na(hadm_id) & !is.na(charttime)
]
bg[, fio2_lab := fio2_lab_raw]
bg[fio2_lab > 0.2 & fio2_lab <= 1, fio2_lab := fio2_lab * 100]
bg[!is.finite(fio2_lab) | fio2_lab < 21 | fio2_lab > 100, fio2_lab := NA_real_]
bg[, peep_lab := peep_lab_raw]
bg[!is.finite(peep_lab) | peep_lab < 0 | peep_lab > 30, peep_lab := NA_real_]

bg_stay <- merge(
  bg,
  adult_stays[, .(subject_id, hadm_id, stay_id, intime, outtime)],
  by = c("subject_id", "hadm_id"), all = FALSE, allow.cartesian = TRUE
)
bg_stay <- bg_stay[charttime >= intime & charttime <= outtime]
bg_stay[, sec_from_intime := as.numeric(difftime(charttime, intime, units = "secs"))]
setorder(bg_stay, specimen_id, sec_from_intime)
bg_stay <- bg_stay[, .SD[1L], by = specimen_id]

ce_cache <- file.path(CACHE_DIR, "selected_index_chartevents_v1.rds")
CE_CACHE_PREEXISTED <- file.exists(ce_cache)
ce <- read_or_build_cache(ce_cache, function() {
  log_msg("Streaming selected respiratory rows from chartevents.csv.gz")
  x <- read_gz_filtered(
    file.path(MIMIC, "icu", "chartevents.csv.gz"),
    paste(c(
      '$7==220224', '$7==223835', '$7==220339', '$7==224700',
      '$7==223849', '$7==229314', '$7==226732', '$7==223848'
    ), collapse = " || ")
  )
  x[, .(subject_id, hadm_id, stay_id, charttime, storetime,
        itemid, value, valuenum, valueuom)]
})
ce[, charttime := as_utc(charttime)]
ce[, storetime := as_utc(storetime)]
ce <- ce[stay_id %in% adult_ids & !is.na(charttime)]
ce <- merge(
  ce,
  adult_stays[, .(stay_id, stay_subject_id = subject_id,
                  stay_hadm_id = hadm_id, intime, outtime)],
  by = "stay_id", all = FALSE
)
ce <- ce[charttime >= intime & charttime <= outtime]
ce_id_mismatch_n <- ce[
  subject_id != stay_subject_id | hadm_id != stay_hadm_id, .N
]
ce <- ce[subject_id == stay_subject_id & hadm_id == stay_hadm_id]

## PaO2: known arterial lab specimens plus the explicitly arterial chart item.
pao2_lab <- bg_stay[, .(
  stay_id, subject_id, hadm_id,
  pao2_time = charttime, pao2,
  pao2_source = "labevents_known_arterial",
  source_rank = 1L,
  source_record_time = storetime,
  pao2_observation_id = as.numeric(specimen_id),
  pao2_specimen_id = as.numeric(specimen_id),
  same_bg_fio2 = fio2_lab,
  same_bg_peep = peep_lab
)]
pao2_chart <- ce[
  itemid == 220224L & is.finite(valuenum) &
    valuenum >= LOCKED$physiologic_ranges$pao2_mmHg[1] &
    valuenum <= LOCKED$physiologic_ranges$pao2_mmHg[2], .(
      pao2 = median(as.numeric(valuenum), na.rm = TRUE),
      pao2_source = "chartevents_arterial_item_220224",
      source_rank = 2L,
      source_record_time = if (all(is.na(storetime))) as.POSIXct(NA) else
        max(storetime, na.rm = TRUE),
      pao2_observation_id = max(as.numeric(.I)),
      pao2_specimen_id = NA_real_,
      same_bg_fio2 = NA_real_,
      same_bg_peep = NA_real_
    ), by = .(
      stay_id,
      subject_id = stay_subject_id,
      hadm_id = stay_hadm_id,
      pao2_time = charttime
    )
]
pao2 <- rbindlist(list(pao2_lab, pao2_chart), use.names = TRUE)
pao2[, record_order := as.numeric(source_record_time)]
pao2[!is.finite(record_order), record_order := -Inf]
setorder(
  pao2, stay_id, pao2_time, source_rank,
  -record_order, -pao2_observation_id
)
pao2 <- pao2[, .SD[1L], by = .(stay_id, pao2_time)]
pao2[, record_order := NULL]
pao2[, event_id := .I]

fio2_lab <- bg_stay[is.finite(fio2_lab), .(
  stay_id, measure_time = charttime, measurement_value = fio2_lab,
  measurement_source = "labevents_same_blood_gas",
  measurement_label = "FiO2_item_50816", source_rank = 1L,
  record_time = storetime, observation_id = specimen_id
)]
fio2_chart <- ce[itemid == 223835L & is.finite(valuenum), .(
  stay_id, measure_time = charttime, measurement_value = as.numeric(valuenum),
  measurement_source = "chartevents",
  measurement_label = "Inspired_O2_Fraction_item_223835", source_rank = 2L,
  record_time = storetime, observation_id = .I
)]
fio2_chart[
  measurement_value >= 0.20 & measurement_value <= 1,
  measurement_value := measurement_value * 100
]
fio2_chart <- fio2_chart[
  measurement_value >= LOCKED$physiologic_ranges$fio2_percent[1] &
    measurement_value <= LOCKED$physiologic_ranges$fio2_percent[2]
]
fio2 <- select_measurements_by_hierarchy(rbindlist(
  list(fio2_lab, fio2_chart), use.names = TRUE
))

peep_lab <- bg_stay[is.finite(peep_lab), .(
  stay_id, measure_time = charttime, measurement_value = peep_lab,
  measurement_source = "labevents_same_blood_gas",
  measurement_label = "PEEP_item_50819", source_rank = 3L,
  record_time = storetime, observation_id = specimen_id
)]
peep_chart <- ce[
  itemid %in% c(220339L, 224700L) & is.finite(valuenum) &
    valuenum >= 0 & valuenum <= LOCKED$physiologic_ranges$peep_cmH2O[2], .(
      stay_id, measure_time = charttime,
      measurement_value = as.numeric(valuenum),
      measurement_source = fifelse(
        itemid == 220339L, "chartevents_PEEP_set", "chartevents_total_PEEP"
      ),
      measurement_label = fifelse(
        itemid == 220339L, "PEEP_set_item_220339", "Total_PEEP_item_224700"
      ),
      source_rank = fifelse(itemid == 220339L, 1L, 2L),
      record_time = storetime, observation_id = .I
    )
]
peep <- select_measurements_by_hierarchy(rbindlist(
  list(peep_chart, peep_lab), use.names = TRUE
))

if (!nrow(pao2)) stop("No valid known-arterial PaO2 events were extracted")
if (!nrow(fio2)) stop("No valid FiO2 measurements were extracted")
if (!nrow(peep)) stop("No valid PEEP measurements were extracted")

anchors <- pao2[, .(stay_id, event_id, anchor_time = pao2_time)]
log_msg("Pairing PaO2 with the closest FiO2 and PEEP")
fio2_near <- nearest_symmetric(anchors, fio2, PF_WINDOW_MIN, "fio2")
peep_near <- nearest_symmetric(anchors, peep, PEEP_WINDOW_MIN, "peep")
events <- merge(pao2, fio2_near, by = "event_id", all.x = TRUE)
events <- merge(events, peep_near, by = "event_id", all.x = TRUE)
## Enforce truly same-specimen blood-gas fallbacks. Generic time matching above
## cannot distinguish two arterial specimens charted at the same timestamp.
events[is.finite(same_bg_fio2), `:=`(
  fio2_near_time = pao2_time,
  fio2_near_value = same_bg_fio2,
  fio2_near_source = "labevents_same_specimen_50816",
  fio2_near_label = "FiO2_item_50816",
  fio2_signed_gap_min = 0,
  fio2_abs_gap_min = 0
)]
events[
  is.finite(same_bg_peep) &
    !is.na(peep_near_time) & peep_near_time == pao2_time &
    grepl("labevents", peep_near_source),
  `:=`(
    peep_near_time = pao2_time,
    peep_near_value = same_bg_peep,
    peep_near_source = "labevents_same_specimen_50819",
    peep_near_label = "PEEP_item_50819",
    peep_signed_gap_min = 0,
    peep_abs_gap_min = 0
  )
]
events[, pf_ratio := pao2 / (fio2_near_value / 100)]
events[!is.finite(pf_ratio) | pf_ratio <= 0 | pf_ratio > 1000, pf_ratio := NA_real_]
events <- merge(
  events,
  adult_stays[, .(
    stay_id, first_careunit, last_careunit, intime, outtime, los,
    age_at_admission, gender, admittime, admission_type
  )],
  by = "stay_id", all.x = TRUE
)

## ---- Explicit invasive-ventilation evidence --------------------------------

log_msg("Reading explicit invasive-ventilation procedure intervals")
proc <- fread(
  file.path(MIMIC, "icu", "procedureevents.csv.gz"),
  select = c("subject_id", "hadm_id", "stay_id", "starttime", "endtime",
             "itemid", "statusdescription"),
  showProgress = FALSE
)
proc <- proc[
  stay_id %in% adult_ids & itemid %in% c(225792L, 225794L) &
    statusdescription != "Paused"
]
proc[, starttime := as_utc(starttime)]
proc[, endtime := as_utc(endtime)]
proc <- proc[
  !is.na(starttime) & !is.na(endtime) & endtime >= starttime
]

invasive_proc <- proc[itemid == 225792L]
active_match <- invasive_proc[
  events,
  on = .(stay_id, starttime <= pao2_time, endtime >= pao2_time),
  nomatch = 0L,
  .(event_id = i.event_id),
  allow.cartesian = TRUE
]
active_ids <- unique(active_match$event_id)
events[, invasive_procedure_active := event_id %in% active_ids]

## Explicit NIV procedure item 225794 excludes any event whose index +/-120 min
## window overlaps the documented NIV interval. This interval-overlap rule is
## locked separately from the point-record chartevents NIV rule below.
events[, niv_window_start := pao2_time - NIV_EXCLUSION_WINDOW_MIN * 60]
events[, niv_window_end := pao2_time + NIV_EXCLUSION_WINDOW_MIN * 60]
niv_proc <- proc[itemid == 225794L]
niv_proc_match <- niv_proc[
  events,
  on = .(
    stay_id,
    starttime <= niv_window_end,
    endtime >= niv_window_start
  ),
  nomatch = 0L,
  .(event_id = i.event_id),
  allow.cartesian = TRUE
]
niv_proc_ids <- unique(niv_proc_match$event_id)
events[, niv_procedure_proximal := event_id %in% niv_proc_ids]

invasive_modes <- c(
  "(S) CMV", "APRV", "APRV/Biphasic+ApnPress", "APRV/Biphasic+ApnVol",
  "APV (cmv)", "Ambient", "Apnea Ventilation", "CMV", "CMV/ASSIST",
  "CMV/ASSIST/AutoFlow", "CMV/AutoFlow", "CPAP/PPS", "CPAP/PSV",
  "CPAP/PSV+Apn TCPL", "CPAP/PSV+ApnPres", "CPAP/PSV+ApnVol", "MMV",
  "MMV/AutoFlow", "MMV/PSV", "MMV/PSV/AutoFlow", "P-CMV", "PCV+",
  "PCV+/PSV", "PCV+Assist", "PRES/AC", "PRVC/AC", "PRVC/SIMV",
  "PSV/SBT", "SIMV", "SIMV/AutoFlow", "SIMV/PRES", "SIMV/PSV",
  "SIMV/PSV/AutoFlow", "SIMV/VOL", "SYNCHRON MASTER", "SYNCHRON SLAVE",
  "VOL/AC", "APV (simv)", "P-SIMV", "VS", "ASV"
)
niv_modes <- c("DuoPaP", "NIV", "NIV-ST")

status_rows <- ce[itemid %in% c(223849L, 229314L, 226732L) &
                    !is.na(value) & nzchar(trimws(value)), .(
  stay_id, event_time = charttime, itemid,
  status_value = trimws(as.character(value))
)]
vent_status <- status_rows[, {
  vals_mode <- status_value[itemid %in% c(223849L, 229314L)]
  vals_device <- status_value[itemid == 226732L]
  has_ett <- any(vals_device == "Endotracheal tube")
  has_trach <- any(vals_device %in% c("Tracheostomy tube", "Trach mask"))
  has_inv_mode <- any(vals_mode %in% invasive_modes)
  has_niv <- any(vals_device %in% c("Bipap mask", "CPAP mask")) ||
    any(vals_mode %in% niv_modes)
  status <- if (has_ett || has_inv_mode) {
    "InvasiveVent"
  } else if (has_niv) {
    "NonInvasiveVent"
  } else if (has_trach) {
    "Tracheostomy"
  } else {
    "Other"
  }
  .(ventilation_status = status)
}, by = .(stay_id, event_time)]

## Preserve NIV conflicts independently of priority-collapsed ventilation
## status. A timestamp with both invasive and NIV markers must still trigger the
## NIV exclusion window.
raw_niv_status <- status_rows[, {
  vals_mode <- status_value[itemid %in% c(223849L, 229314L)]
  vals_device <- status_value[itemid == 226732L]
  .(has_raw_niv =
      any(vals_device %in% c("Bipap mask", "CPAP mask")) ||
      any(vals_mode %in% niv_modes))
}, by = .(stay_id, event_time)]

vent_measure <- vent_status[ventilation_status != "Other", .(
  stay_id, measure_time = event_time, measurement_value = 1,
  measurement_source = "chartevents_mode_or_device",
  measurement_label = ventilation_status
)]
niv_measure <- raw_niv_status[has_raw_niv == TRUE, .(
  stay_id, measure_time = event_time, measurement_value = 1,
  measurement_source = "raw_chartevents_NIV_marker",
  measurement_label = "NonInvasiveVent"
)]
recent_status <- nearest_side(
  anchors, vent_measure, "prior", VENT_STATUS_LOOKBACK_MIN, "vent_status"
)
niv_near <- nearest_symmetric(
  anchors, niv_measure,
  NIV_EXCLUSION_WINDOW_MIN,
  "niv"
)
events <- merge(events, recent_status, by = "event_id", all.x = TRUE)
events <- merge(events, niv_near, by = "event_id", all.x = TRUE)
events[, recent_invasive_status :=
  !is.na(vent_status_near_time) & vent_status_near_label == "InvasiveVent"]
events[, niv_chartevents_proximal := !is.na(niv_near_time)]
events[, proximal_niv := niv_procedure_proximal | niv_chartevents_proximal]
events[, niv_evidence_type := fifelse(
  niv_procedure_proximal & niv_chartevents_proximal,
  "procedureevents_225794_and_chartevents",
  fifelse(
    niv_procedure_proximal,
    "procedureevents_225794_interval_overlap",
    fifelse(niv_chartevents_proximal, "chartevents_NIV_mode_or_mask", "none")
  )
)]
events[, invasive_confirmed := invasive_procedure_active | recent_invasive_status]
events[, invasive_evidence_type := fifelse(
  invasive_procedure_active,
  "procedureevents_225792_active",
  fifelse(recent_invasive_status, "recent_invasive_mode_or_ETT", NA_character_)
)]

## Sequential respiratory eligibility before infection ascertainment.
events[, pf_paired := !is.na(fio2_near_value) & !is.na(pf_ratio)]
events[, low_oxygen := pf_paired & pf_ratio <= PF_MAX]
events[, peep_paired_ge5 :=
  !is.na(peep_near_value) & peep_near_value >= PEEP_MIN]
stage0 <- events
stage1 <- stage0[pf_paired == TRUE]
stage2 <- stage1[low_oxygen == TRUE]
stage3 <- stage2[peep_paired_ge5 == TRUE]
stage4 <- stage3[invasive_confirmed == TRUE]
stage5 <- stage4[proximal_niv == FALSE]
if (!nrow(stage5)) stop("No respiratory index candidates remained before infection filtering")

## ---- Seymour-style suspected infection ------------------------------------

## The terms and route exclusions mirror MIT-LCP medication/antibiotic.sql.
antibiotic_terms <- c(
  "adoxa", "ala-tet", "alodox", "amikacin", "amikin", "amoxicill",
  "amphotericin", "anidulafungin", "ancef", "clavulanate", "ampicillin",
  "augmentin", "avelox", "avidoxy", "azactam", "azithromycin", "aztreonam",
  "axetil", "bactocill", "bactrim", "bactroban", "bethkis", "biaxin",
  "bicillin l-a", "cayston", "cefazolin", "cedax", "cefoxitin",
  "ceftazidime", "cefaclor", "cefadroxil", "cefdinir", "cefditoren",
  "cefepime", "cefotan", "cefotetan", "cefotaxime", "ceftaroline",
  "cefpodoxime", "cefpirome", "cefprozil", "ceftibuten", "ceftin",
  "ceftriaxone", "cefuroxime", "cephalexin", "cephalothin", "cephapririn",
  "chloramphenicol", "cipro", "ciprofloxacin", "claforan", "clarithromycin",
  "cleocin", "clindamycin", "cubicin", "dicloxacillin", "dirithromycin",
  "doryx", "doxycy", "duricef", "dynacin", "ery-tab", "eryped", "eryc",
  "erythrocin", "erythromycin", "factive", "flagyl", "fortaz", "furadantin",
  "garamycin", "gentamicin", "kanamycin", "keflex", "kefzol", "ketek",
  "levaquin", "levofloxacin", "lincocin", "linezolid", "macrobid",
  "macrodantin", "maxipime", "mefoxin", "metronidazole", "meropenem",
  "methicillin", "minocin", "minocycline", "monodox", "monurol", "morgidox",
  "moxatag", "moxifloxacin", "mupirocin", "myrac", "nafcillin", "neomycin",
  "nicazel doxy 30", "nitrofurantoin", "norfloxacin", "noroxin", "ocudox",
  "ofloxacin", "omnicef", "oracea", "oraxyl", "oxacillin", "pc pen vk",
  "pce dispertab", "panixine", "pediazole", "penicillin", "periostat",
  "pfizerpen", "piperacillin", "tazobactam", "primsol", "proquin", "raniclor",
  "rifadin", "rifampin", "rocephin", "smz-tmp", "septra", "solodyn",
  "spectracef", "streptomycin", "sulfadiazine", "sulfamethoxazole",
  "trimethoprim", "sulfatrim", "sulfisoxazole", "suprax", "synercid",
  "tazicef", "tetracycline", "timentin", "tobramycin", "unasyn", "vancocin",
  "vancomycin", "vantin", "vibativ", "vibra-tabs", "vibramycin", "zinacef",
  "zithromax", "zosyn", "zyvox"
)
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}
antibiotic_regex <- paste(escape_regex(antibiotic_terms), collapse = "|")
candidate_hadm <- unique(stage5$hadm_id)

log_msg("Reading candidate-hospitalization prescriptions")
rx <- fread(
  file.path(MIMIC, "hosp", "prescriptions.csv.gz"),
  select = c("subject_id", "hadm_id", "pharmacy_id", "starttime", "stoptime",
             "drug_type", "drug", "route"),
  showProgress = FALSE
)
rx <- rx[hadm_id %in% candidate_hadm]
rx[, starttime := as_utc(starttime)]
rx[, stoptime := as_utc(stoptime)]
rx[, drug_lower := tolower(drug)]
rx[, route_lower := tolower(route)]
rx <- rx[
  !is.na(starttime) & drug_type != "BASE" &
    !route %in% c("OU", "OS", "OD", "AU", "AS", "AD", "TP") &
    !grepl("ear|eye", route_lower) &
    !grepl("cream|desensitization|ophth oint|gel", drug_lower) &
    grepl(antibiotic_regex, drug_lower, perl = TRUE)
]
setorder(rx, subject_id, starttime, stoptime, drug, hadm_id)
rx[, ab_id := .I]
rx[, antibiotic_date := as.IDate(starttime)]
abx <- rx[, .(
  ab_id, subject_id, hadm_id, antibiotic_time = starttime,
  antibiotic_date, antibiotic = drug, route
)]
rm(rx); invisible(gc())

log_msg("Reading candidate-hospitalization microbiology specimens")
micro <- fread(
  file.path(MIMIC, "hosp", "microbiologyevents.csv.gz"),
  select = c("micro_specimen_id", "subject_id", "hadm_id", "chartdate",
             "charttime", "spec_type_desc", "org_itemid", "org_name"),
  showProgress = FALSE
)
micro <- micro[hadm_id %in% candidate_hadm & !is.na(micro_specimen_id)]
micro[, charttime := as_utc(charttime)]
micro[, chartdate := as.IDate(chartdate)]
cultures <- micro[, .(
  subject_id = first_numeric_or_na(subject_id),
  hadm_id = first_numeric_or_na(hadm_id),
  culture_date = if (all(is.na(chartdate))) as.IDate(NA) else max(chartdate, na.rm = TRUE),
  charttime_exact = if (all(is.na(charttime))) as.POSIXct(NA) else max(charttime, na.rm = TRUE),
  specimen = first_character_or_na(spec_type_desc),
  positive_culture = as.integer(any(
    !is.na(org_name) & nzchar(trimws(org_name)) &
      (is.na(org_itemid) | org_itemid != 90856L)
  ))
), by = micro_specimen_id]
cultures[, has_exact_time := !is.na(charttime_exact)]
cultures[, culture_time_precision := fifelse(
  has_exact_time, "exact_charttime", "date_only"
)]
cultures[, culture_time := charttime_exact]
cultures[is.na(culture_time) & !is.na(culture_date),
         culture_time := as.POSIXct(culture_date, tz = "UTC")]
cultures[, culture_evidence_time := charttime_exact]
cultures[!has_exact_time & !is.na(culture_date),
         culture_evidence_time :=
           as.POSIXct(culture_date, tz = "UTC") + 24 * 3600 - 1]
cultures <- cultures[
  !is.na(subject_id) & !is.na(hadm_id) & !is.na(culture_time)
]
rm(micro); invisible(gc())

if (!nrow(abx) || !nrow(cultures)) {
  stop("Antibiotic/culture extraction produced no candidate events")
}

## Broad non-equi window first; exact official time/date logic follows.
abx[, culture_lo := antibiotic_time - 96 * 3600]
abx[, culture_hi := antibiotic_time + 48 * 3600]
setkey(cultures, subject_id, hadm_id, culture_time)
matches <- cultures[
  abx,
  on = .(
    subject_id, hadm_id,
    culture_time >= culture_lo,
    culture_time <= culture_hi
  ),
  nomatch = 0L,
  allow.cartesian = TRUE,
  .(
    ab_id = i.ab_id,
    antibiotic_time = i.antibiotic_time,
    antibiotic_date = i.antibiotic_date,
    culture_time = x.culture_time,
    culture_evidence_time = x.culture_evidence_time,
    culture_date = x.culture_date,
    has_exact_time = x.has_exact_time,
    culture_time_precision = x.culture_time_precision,
    positive_culture = x.positive_culture,
    micro_specimen_id = x.micro_specimen_id
  )
]
matches[, ab_minus_culture_h := as.numeric(difftime(
  antibiotic_time, culture_time, units = "hours"
))]
matches[, valid_culture_before_abx :=
  (has_exact_time & ab_minus_culture_h > 0 & ab_minus_culture_h <= 72) |
    (!has_exact_time & antibiotic_date >= culture_date &
       antibiotic_date <= culture_date + 3L)]
matches[, valid_abx_before_culture :=
  (has_exact_time & ab_minus_culture_h < 0 & ab_minus_culture_h >= -24) |
    (!has_exact_time & antibiotic_date >= culture_date - 1L &
       antibiotic_date <= culture_date)]

pre <- matches[valid_culture_before_abx == TRUE]
setorder(pre, ab_id, culture_date, culture_time, -positive_culture, micro_specimen_id)
pre <- pre[, .SD[1L], by = ab_id][, .(
  ab_id, prior_culture_time = culture_time,
  prior_culture_evidence_time = culture_evidence_time,
  prior_culture_time_precision = culture_time_precision,
  prior_culture_positive = positive_culture
)]
post <- matches[valid_abx_before_culture == TRUE]
setorder(post, ab_id, culture_date, culture_time, -positive_culture, micro_specimen_id)
post <- post[, .SD[1L], by = ab_id][, .(
  ab_id, next_culture_time = culture_time,
  next_culture_evidence_time = culture_evidence_time,
  next_culture_time_precision = culture_time_precision,
  next_culture_positive = positive_culture
)]

suspected <- merge(
  abx[, .(ab_id, subject_id, hadm_id, antibiotic_time)],
  pre, by = "ab_id", all.x = TRUE
)
suspected <- merge(suspected, post, by = "ab_id", all.x = TRUE)
suspected <- suspected[
  !is.na(prior_culture_time) | !is.na(next_culture_time)
]
suspected[, suspected_infection_time := prior_culture_time]
suspected[
  is.na(suspected_infection_time) & !is.na(next_culture_time),
  suspected_infection_time := antibiotic_time
]
suspected[, culture_direction := fifelse(
  !is.na(prior_culture_time), "culture_before_antibiotic",
  "antibiotic_before_culture"
)]
suspected[, paired_culture_time := prior_culture_time]
suspected[is.na(paired_culture_time), paired_culture_time := next_culture_time]
suspected[, paired_culture_evidence_time := prior_culture_evidence_time]
suspected[is.na(paired_culture_evidence_time),
          paired_culture_evidence_time := next_culture_evidence_time]
suspected[, culture_time_precision := prior_culture_time_precision]
suspected[is.na(culture_time_precision),
          culture_time_precision := next_culture_time_precision]
## The paired definition is not considered available until both antibiotic and
## culture have occurred. This prevents a future culture from defining primary
## cohort membership at an earlier respiratory index.
suspected[, evidence_available_time := pmax(
  antibiotic_time, paired_culture_evidence_time, na.rm = TRUE
)]
suspected <- unique(suspected[, .(
  subject_id, hadm_id, suspected_infection_time, evidence_available_time,
  culture_direction, culture_time_precision
)])

## Primary membership is defined only with information available by index:
## suspected-infection onset in [-48, 0] h and completion of the paired
## antibiotic+culture evidence by index. The +24 h retrospective phenotype is
## built separately and never defines the primary cohort.
infection_anchors <- unique(stage5[, .(
  subject_id, hadm_id, event_id, anchor_time = pao2_time
)])
infection_anchors[, onset_lo := anchor_time - INFECTION_BEFORE_H * 3600]
infection_anchors[, onset_hi_primary := anchor_time]
infection_anchors[, onset_hi_sensitivity :=
                    anchor_time + SENSITIVITY_INFECTION_AFTER_H * 3600]

setkey(suspected, subject_id, hadm_id, suspected_infection_time)
primary_matches <- suspected[
  infection_anchors,
  on = .(
    subject_id, hadm_id,
    suspected_infection_time >= onset_lo,
    suspected_infection_time <= onset_hi_primary
  ),
  nomatch = 0L,
  allow.cartesian = TRUE,
  .(
    event_id = i.event_id,
    anchor_time = i.anchor_time,
    infection_time = x.suspected_infection_time,
    evidence_available_time = x.evidence_available_time,
    infection_direction = x.culture_direction,
    infection_culture_time_precision = x.culture_time_precision
  )
]
primary_matches <- primary_matches[evidence_available_time <= anchor_time]
primary_matches[, infection_gap_h := as.numeric(difftime(
  infection_time, anchor_time, units = "hours"
))]
primary_matches[, precision_rank := fifelse(
  infection_culture_time_precision == "exact_charttime", 1L, 2L
)]
setorder(
  primary_matches, event_id, -infection_time,
  precision_rank, -evidence_available_time
)
primary_link <- primary_matches[, .SD[1L], by = event_id]
setnames(primary_link, "evidence_available_time", "infection_evidence_time")
primary_link[, infection_available_by_index := TRUE]
primary_link[, suspected_infection_in_window := TRUE]

exact_primary_link <- primary_matches[
  infection_culture_time_precision == "exact_charttime"
]
setorder(
  exact_primary_link, event_id, -infection_time, -evidence_available_time
)
exact_primary_link <- exact_primary_link[, .SD[1L], by = event_id]
exact_primary_link <- exact_primary_link[, .(
  event_id,
  exact_infection_time = infection_time,
  exact_infection_evidence_time = evidence_available_time,
  exact_infection_direction = infection_direction,
  exact_infection_gap_h = infection_gap_h,
  exact_culture_primary_eligible = TRUE
)]

sensitivity_matches <- suspected[
  infection_anchors,
  on = .(
    subject_id, hadm_id,
    suspected_infection_time >= onset_lo,
    suspected_infection_time <= onset_hi_sensitivity
  ),
  nomatch = 0L,
  allow.cartesian = TRUE,
  .(
    event_id = i.event_id,
    anchor_time = i.anchor_time,
    sensitivity_infection_time = x.suspected_infection_time,
    sensitivity_evidence_time = x.evidence_available_time,
    sensitivity_culture_time_precision = x.culture_time_precision
  )
]
sensitivity_matches <- sensitivity_matches[
  sensitivity_evidence_time <=
    anchor_time + SENSITIVITY_INFECTION_AFTER_H * 3600
]
sensitivity_matches[, sensitivity_infection_gap_h := as.numeric(difftime(
  sensitivity_infection_time, anchor_time, units = "hours"
))]
sensitivity_matches[, precision_rank := fifelse(
  sensitivity_culture_time_precision == "exact_charttime", 1L, 2L
)]
setorder(
  sensitivity_matches, event_id,
  -sensitivity_infection_time, precision_rank, -sensitivity_evidence_time
)
sensitivity_link <- sensitivity_matches[, .SD[1L], by = event_id]
sensitivity_link[, retrospective_infection_24h := TRUE]

stage5 <- merge(
  stage5,
  primary_link[, .(
    event_id, infection_time, infection_evidence_time,
    infection_direction, infection_culture_time_precision, infection_gap_h,
    infection_available_by_index, suspected_infection_in_window
  )],
  by = "event_id", all.x = TRUE
)
stage5 <- merge(stage5, exact_primary_link, by = "event_id", all.x = TRUE)
stage5 <- merge(
  stage5,
  sensitivity_link[, .(
    event_id, retrospective_infection_24h,
    sensitivity_infection_time, sensitivity_evidence_time,
    sensitivity_infection_gap_h, sensitivity_culture_time_precision
  )],
  by = "event_id", all.x = TRUE
)
stage5[is.na(suspected_infection_in_window), suspected_infection_in_window := FALSE]
stage5[is.na(infection_available_by_index), infection_available_by_index := FALSE]
stage5[is.na(retrospective_infection_24h), retrospective_infection_24h := FALSE]
stage5[is.na(exact_culture_primary_eligible), exact_culture_primary_eligible := FALSE]
stage6 <- stage5[suspected_infection_in_window == TRUE]
if (!nrow(stage6)) stop("No events met the locked suspected-infection window")

## ---- First event per stay, then first qualifying stay per patient -----------

setorder(stage6, stay_id, pao2_time, event_id)
stay_candidates <- stage6[, .SD[1L], by = stay_id]
setorder(stay_candidates, subject_id, pao2_time, intime, stay_id)
cohort <- stay_candidates[, .SD[1L], by = subject_id]

## Retrospective -48/+24 h sensitivity cohort. This is built independently
## from stage5 and cannot alter primary membership or primary index times.
sensitivity_events <- stage5[retrospective_infection_24h == TRUE]
setorder(sensitivity_events, stay_id, pao2_time, event_id)
sensitivity_stay_candidates <- sensitivity_events[, .SD[1L], by = stay_id]
setorder(
  sensitivity_stay_candidates, subject_id, pao2_time, intime, stay_id
)
sensitivity_cohort <- sensitivity_stay_candidates[, .SD[1L], by = subject_id]

## Exact-culture-time sensitivity nested within the primary information window.
exact_culture_events <- stage5[
  exact_culture_primary_eligible == TRUE
]
setorder(exact_culture_events, stay_id, pao2_time, event_id)
exact_culture_stay_candidates <- exact_culture_events[, .SD[1L], by = stay_id]
setorder(
  exact_culture_stay_candidates, subject_id, pao2_time, intime, stay_id
)
exact_culture_cohort <- exact_culture_stay_candidates[, .SD[1L], by = subject_id]
exact_culture_cohort[, `:=`(
  infection_time = exact_infection_time,
  infection_evidence_time = exact_infection_evidence_time,
  infection_direction = exact_infection_direction,
  infection_gap_h = exact_infection_gap_h,
  infection_culture_time_precision = "exact_charttime"
)]

restricted_keep <- c(
  "subject_id", "hadm_id", "stay_id", "first_careunit", "last_careunit",
  "intime", "outtime", "age_at_admission", "gender", "admission_type",
  "pao2_time", "pao2", "pao2_source", "fio2_near_time",
  "fio2_near_value", "fio2_near_source", "fio2_signed_gap_min",
  "fio2_abs_gap_min", "pf_ratio", "peep_near_time", "peep_near_value",
  "peep_near_source", "peep_near_label", "peep_signed_gap_min",
  "peep_abs_gap_min", "invasive_confirmed", "invasive_evidence_type",
  "proximal_niv", "niv_procedure_proximal", "niv_chartevents_proximal",
  "niv_evidence_type", "infection_time", "infection_gap_h",
  "infection_evidence_time", "infection_direction",
  "infection_culture_time_precision",
  "infection_available_by_index", "retrospective_infection_24h",
  "sensitivity_infection_time", "sensitivity_evidence_time",
  "sensitivity_infection_gap_h", "sensitivity_culture_time_precision"
)
restricted_keep <- restricted_keep[restricted_keep %in% names(cohort)]
stay_out <- stay_candidates[, ..restricted_keep]
cohort_out <- cohort[, ..restricted_keep]
sensitivity_keep <- restricted_keep[restricted_keep %in% names(sensitivity_cohort)]
sensitivity_cohort_out <- sensitivity_cohort[, ..sensitivity_keep]
exact_culture_keep <- restricted_keep[
  restricted_keep %in% names(exact_culture_cohort)
]
exact_culture_cohort_out <- exact_culture_cohort[, ..exact_culture_keep]

metadata <- list(
  version = "mimic_index_cohort_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  raw_data = normalizePath(MIMIC),
  script = normalizePath(file.path(
    PROJECT, "code", "R", "rebuild_v1", "01_build_mimic_index_cohort.R"
  ), mustWork = FALSE),
  locked_config_version = LOCKED$version,
  mimic_code_reference_commit =
    "5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4",
  phenotype = "oxygenation-defined; not imaging-adjudicated ARDS",
  parameters = list(
    age_min = AGE_MIN, pf_max = PF_MAX,
    pf_fio2_window_min = PF_WINDOW_MIN,
    peep_window_min = PEEP_WINDOW_MIN, peep_min = PEEP_MIN,
    vent_status_lookback_min = VENT_STATUS_LOOKBACK_MIN,
    niv_exclusion_window_min = NIV_EXCLUSION_WINDOW_MIN,
    primary_infection_window_h = c(-INFECTION_BEFORE_H, INFECTION_AFTER_H),
    retrospective_sensitivity_infection_window_h =
      c(-INFECTION_BEFORE_H, SENSITIVITY_INFECTION_AFTER_H),
    arterial_rule = paste(
      "Known arterial specimen in labevents or explicitly arterial",
      "chartevents item 220224"
    ),
    invasive_rule = paste(
      "Active procedureevents item 225792 or most recent official-list",
      "invasive mode/ETT documentation within 14 h"
    ),
    niv_exclusion_rule = paste(
      "procedureevents item 225794 interval overlaps index +/-120 min OR",
      "explicit NIV mode/mask chartevent within index +/-120 min"
    ),
    measurement_source_hierarchy = list(
      FiO2 = "same-blood-gas 50816 > chartevents 223835",
      PEEP = "PEEP-set 220339 > total-PEEP 224700 > blood-gas 50819"
    ),
    duplicate_measurement_rule = paste(
      "Median only within identical stay/time/source/rank; never average",
      "across source ranks; retain maximum storetime for availability QC"
    ),
    date_only_culture_availability = paste(
      "Conservatively assigned to 23:59:59 on chartdate for primary",
      "information-availability filtering"
    )
  )
)
attr(stay_out, "rebuild_metadata") <- metadata
attr(cohort_out, "rebuild_metadata") <- metadata
metadata_sensitivity <- metadata
metadata_sensitivity$version <- "mimic_index_cohort_infection_plus24_sensitivity_v1"
metadata_sensitivity$primary_analysis <- FALSE
metadata_sensitivity$phenotype <- paste(
  "retrospective oxygenation-defined sensitivity; suspected-infection onset",
  "and completed paired evidence allowed through index +24 h"
)
attr(sensitivity_cohort_out, "rebuild_metadata") <- metadata_sensitivity
metadata_exact <- metadata
metadata_exact$version <- "mimic_index_cohort_exact_culture_time_sensitivity_v1"
metadata_exact$primary_analysis <- FALSE
metadata_exact$phenotype <- paste(
  "primary -48..0 h phenotype restricted to cultures with exact charttime;",
  "date-only culture records excluded"
)
attr(exact_culture_cohort_out, "rebuild_metadata") <- metadata_exact
saveRDS(stay_out, file.path(OUT_PRIVATE, "mimic_index_stay_candidates_v1.rds"),
        compress = "xz")
saveRDS(cohort_out, file.path(OUT_PRIVATE, "mimic_index_cohort_v1.rds"),
        compress = "xz")
saveRDS(
  sensitivity_cohort_out,
  file.path(
    OUT_PRIVATE,
    "mimic_index_cohort_infection_plus24_sensitivity_v1.rds"
  ),
  compress = "xz"
)
saveRDS(
  exact_culture_cohort_out,
  file.path(
    OUT_PRIVATE,
    "mimic_index_cohort_exact_culture_time_sensitivity_v1.rds"
  ),
  compress = "xz"
)

## ---- Aggregate QC -----------------------------------------------------------

count_stage <- function(x, step, rule) {
  data.table(
    step = step, rule = rule,
    n_events = nrow(x), n_icu_stays = uniqueN(x$stay_id),
    n_unique_patients = uniqueN(x$subject_id)
  )
}
funnel <- rbindlist(list(
  data.table(
    step = "L0_all_MIMIC_ICU_stays", rule = "all icustays.csv stays",
    n_events = NA_integer_, n_icu_stays = uniqueN(stays$stay_id),
    n_unique_patients = uniqueN(stays$subject_id)
  ),
  data.table(
    step = "L1_adult_stays", rule = paste0("age >= ", AGE_MIN),
    n_events = NA_integer_, n_icu_stays = uniqueN(adult_stays$stay_id),
    n_unique_patients = uniqueN(adult_stays$subject_id)
  ),
  count_stage(stage1, "L2_known_arterial_PF_pair",
              paste0("known-arterial PaO2 plus FiO2 within +/-", PF_WINDOW_MIN, " min")),
  count_stage(stage2, "L3_low_oxygen", paste0("P/F <= ", PF_MAX)),
  count_stage(stage3, "L4_same_window_PEEP",
              paste0("PEEP >= ", PEEP_MIN, " within +/-", PEEP_WINDOW_MIN, " min")),
  count_stage(stage4, "L5_explicit_invasive_ventilation",
              "active invasive procedure or recent official-list invasive status"),
  count_stage(stage5, "L6_exclude_proximal_NIV",
              paste0("no explicit NIV evidence within +/-", NIV_EXCLUSION_WINDOW_MIN, " min")),
  count_stage(stage6, "L7_suspected_infection",
              paste0("paired evidence available by index; onset from -",
                     INFECTION_BEFORE_H, " to ", INFECTION_AFTER_H, " h")),
  data.table(
    step = "L8_first_qualifying_stay_per_patient",
    rule = "first qualifying event/stay per subject_id",
    n_events = nrow(cohort_out), n_icu_stays = nrow(cohort_out),
    n_unique_patients = uniqueN(cohort_out$subject_id)
  )
), use.names = TRUE)
fwrite(funnel, file.path(OUT_QC, "qc_funnel_v1.csv"))

post_index_only_subjects <- setdiff(
  sensitivity_cohort_out$subject_id, cohort_out$subject_id
)
sensitivity_funnel <- rbindlist(list(
  count_stage(
    stage5, "S0_respiratory_candidates",
    "respiratory criteria complete before infection-window restriction"
  ),
  count_stage(
    sensitivity_events, "S1_infection_minus48_plus24_events",
    "paired suspected-infection onset/evidence allowed through index +24 h"
  ),
  count_stage(
    sensitivity_stay_candidates, "S2_first_event_per_stay",
    "first qualifying retrospective event per ICU stay"
  ),
  count_stage(
    sensitivity_cohort, "S3_first_stay_per_patient",
    "first qualifying retrospective ICU stay per patient"
  ),
  data.table(
    step = "S4_added_vs_primary_post_index_only",
    rule = "patients in +24 h cohort but absent from locked -48..0 h cohort",
    n_events = length(post_index_only_subjects),
    n_icu_stays = length(post_index_only_subjects),
    n_unique_patients = length(post_index_only_subjects)
  )
), use.names = TRUE)
fwrite(
  sensitivity_funnel,
  file.path(OUT_QC, "qc_infection_plus24_sensitivity_funnel_v1.csv")
)

exact_culture_funnel <- rbindlist(list(
  count_stage(
    stage6, "E0_primary_infection_events",
    "all locked primary events after conservative precision handling"
  ),
  count_stage(
    exact_culture_events, "E1_exact_culture_charttime_events",
    "primary events restricted to paired cultures with exact charttime"
  ),
  count_stage(
    exact_culture_stay_candidates, "E2_first_event_per_stay",
    "first exact-culture event per ICU stay"
  ),
  count_stage(
    exact_culture_cohort, "E3_first_stay_per_patient",
    "first exact-culture ICU stay per patient"
  )
), use.names = TRUE)
fwrite(
  exact_culture_funnel,
  file.path(OUT_QC, "qc_exact_culture_time_sensitivity_funnel_v1.csv")
)

pairing_qc <- rbindlist(list(
  metric_quantiles(stage3$fio2_signed_gap_min, "FiO2_minus_PaO2_minutes",
                   "PEEP-qualified events"),
  metric_quantiles(stage3$peep_signed_gap_min, "PEEP_minus_PaO2_minutes",
                   "PEEP-qualified events"),
  metric_quantiles(cohort_out$fio2_signed_gap_min, "FiO2_minus_PaO2_minutes"),
  metric_quantiles(cohort_out$peep_signed_gap_min, "PEEP_minus_PaO2_minutes"),
  metric_quantiles(as.numeric(difftime(
    cohort_out$pao2_time, cohort_out$intime, units = "hours"
  )), "index_hours_from_ICU_admission"),
  metric_quantiles(cohort_out$pf_ratio, "P_F_ratio"),
  metric_quantiles(cohort_out$peep_near_value, "PEEP_cmH2O"),
  metric_quantiles(cohort_out$infection_gap_h, "infection_minus_index_hours")
), use.names = TRUE)
fwrite(pairing_qc, file.path(OUT_QC, "qc_pairing_and_timing_v1.csv"))

source_qc <- rbindlist(list(
  cohort_out[, .N, by = .(source = pao2_source)][
    , domain := "PaO2"
  ][, .(domain, source, N)],
  cohort_out[, .N, by = .(source = fio2_near_source)][
    , domain := "FiO2"
  ][, .(domain, source, N)],
  cohort_out[, .N, by = .(source = peep_near_source)][
    , domain := "PEEP"
  ][, .(domain, source, N)],
  cohort_out[, .N, by = .(source = invasive_evidence_type)][
    , domain := "invasive_evidence"
  ][, .(domain, source, N)],
  cohort_out[, .N, by = .(source = infection_direction)][
    , domain := "infection_pair_direction"
  ][, .(domain, source, N)],
  cohort_out[, .N, by = .(source = infection_culture_time_precision)][
    , domain := "infection_culture_time_precision"
  ][, .(domain, source, N)],
  stage4[proximal_niv == TRUE, .N, by = .(source = niv_evidence_type)][
    , domain := "excluded_proximal_NIV"
  ][, .(domain, source, N)]
), use.names = TRUE)
setorder(source_qc, domain, -N, source)
fwrite(source_qc, file.path(OUT_QC, "qc_source_coverage_v1.csv"))

phenotype_qc <- data.table(
  metric = c(
    "final_n", "final_first_24h_index_n",
    "final_infection_available_by_index_n",
    "respiratory_candidates_retrospective_infection_plus24h_n",
    "sensitivity_plus24_final_n", "sensitivity_plus24_added_vs_primary_n",
    "sensitivity_selected_index_with_post_index_evidence_n",
    "respiratory_candidates_excluded_by_NIV_procedure_n",
    "respiratory_candidates_excluded_by_NIV_chart_n",
    "respiratory_candidates_excluded_by_both_NIV_sources_n",
    "primary_exact_culture_time_n", "primary_date_only_culture_n",
    "exact_culture_time_sensitivity_final_n",
    "final_procedure_based_invasive_n", "final_mode_or_ETT_invasive_n",
    "candidate_stays_before_patient_dedup_n",
    "patients_with_gt1_eligible_stay_n", "chartevents_ID_mismatch_rows_n",
    "known_arterial_lab_BG_events_n", "arterial_chart_events_n",
    "suspected_infection_events_n", "antibiotic_events_candidate_hadm_n",
    "culture_specimens_candidate_hadm_n"
  ),
  value = c(
    nrow(cohort_out),
    sum(as.numeric(difftime(cohort_out$pao2_time, cohort_out$intime,
                            units = "hours")) <= 24, na.rm = TRUE),
    sum(cohort_out$infection_available_by_index, na.rm = TRUE),
    sum(stage5$retrospective_infection_24h, na.rm = TRUE),
    nrow(sensitivity_cohort_out), length(post_index_only_subjects),
    sum(sensitivity_cohort_out$sensitivity_evidence_time >
          sensitivity_cohort_out$pao2_time, na.rm = TRUE),
    sum(stage4$niv_procedure_proximal, na.rm = TRUE),
    sum(stage4$niv_chartevents_proximal, na.rm = TRUE),
    sum(stage4$niv_procedure_proximal & stage4$niv_chartevents_proximal,
        na.rm = TRUE),
    sum(cohort_out$infection_culture_time_precision == "exact_charttime",
        na.rm = TRUE),
    sum(cohort_out$infection_culture_time_precision == "date_only",
        na.rm = TRUE),
    nrow(exact_culture_cohort_out),
    sum(cohort_out$invasive_evidence_type == "procedureevents_225792_active",
        na.rm = TRUE),
    sum(cohort_out$invasive_evidence_type == "recent_invasive_mode_or_ETT",
        na.rm = TRUE),
    nrow(stay_out),
    stay_candidates[, .N, by = subject_id][N > 1, .N],
    ce_id_mismatch_n, nrow(pao2_lab), nrow(pao2_chart), nrow(suspected),
    nrow(abx), nrow(cultures)
  ),
  denominator = c(
    NA, nrow(cohort_out), nrow(cohort_out), nrow(stage5),
    nrow(sensitivity_cohort_out), nrow(sensitivity_cohort_out),
    nrow(sensitivity_cohort_out), nrow(stage4), nrow(stage4), nrow(stage4),
    nrow(cohort_out), nrow(cohort_out), nrow(cohort_out),
    nrow(cohort_out), nrow(cohort_out), NA,
    uniqueN(stay_candidates$subject_id), nrow(ce), nrow(pao2), nrow(pao2),
    NA, NA, NA
  )
)
fwrite(phenotype_qc, file.path(OUT_QC, "qc_phenotype_summary_v1.csv"))

item_coverage <- rbindlist(list(
  ce[, .(source_table = "chartevents", n_rows = .N,
         n_stays = uniqueN(stay_id)), by = itemid],
  lab[, .(source_table = "labevents", n_rows = .N,
          n_stays = NA_integer_), by = itemid],
  proc[, .(source_table = "procedureevents", n_rows = .N,
           n_stays = uniqueN(stay_id)), by = itemid]
), use.names = TRUE, fill = TRUE)
setorder(item_coverage, source_table, itemid)
fwrite(item_coverage, file.path(OUT_QC, "qc_selected_item_coverage_v1.csv"))

legacy_path <- file.path(PROJECT, "checkpoints", "cohort_ids.rds")
if (file.exists(legacy_path)) {
  legacy <- as.data.table(readRDS(legacy_path))
  if ("stay_id" %in% names(legacy)) {
    old_ids <- unique(legacy$stay_id)
    new_ids <- unique(cohort_out$stay_id)
    reconciliation <- data.table(
      metric = c(
        "legacy_stays", "strict_new_stays", "overlap", "legacy_only",
        "strict_new_only", "jaccard"
      ),
      value = c(
        length(old_ids), length(new_ids), length(intersect(old_ids, new_ids)),
        length(setdiff(old_ids, new_ids)), length(setdiff(new_ids, old_ids)),
        length(intersect(old_ids, new_ids)) / length(union(old_ids, new_ids))
      )
    )
    fwrite(reconciliation, file.path(OUT_QC, "qc_legacy_reconciliation_v1.csv"))
  }
}

parameters <- data.table(
  parameter = c(
    "locked_config_version", "age_min", "pf_max", "pf_fio2_window_min",
    "peep_window_min", "peep_min", "vent_status_lookback_min",
    "niv_exclusion_window_min", "infection_before_index_h",
    "infection_after_index_h", "arterial_rule", "invasive_rule",
    "niv_procedure_exclusion_rule", "sensitivity_infection_after_index_h",
    "fio2_same_timestamp_hierarchy", "peep_same_timestamp_hierarchy",
    "within_source_duplicate_rule", "date_only_culture_availability",
    "suspected_infection_rule",
    "mimic_code_reference_commit",
    "script_sha256", "run_timestamp", "R_version"
  ),
  value = c(
    LOCKED$version, AGE_MIN, PF_MAX, PF_WINDOW_MIN, PEEP_WINDOW_MIN, PEEP_MIN,
    VENT_STATUS_LOOKBACK_MIN, NIV_EXCLUSION_WINDOW_MIN, INFECTION_BEFORE_H,
    INFECTION_AFTER_H,
    "known arterial lab specimen OR arterial chart item 220224",
    "active procedure 225792 OR recent official-list invasive mode/ETT",
    paste0(
      "procedure 225794 interval overlaps index +/-",
      NIV_EXCLUSION_WINDOW_MIN, " min; OR proximal chart NIV mode/mask"
    ),
    SENSITIVITY_INFECTION_AFTER_H,
    "same-blood-gas 50816 > chartevents 223835; no averaging",
    "PEEP set 220339 > total PEEP 224700 > blood-gas 50819; no averaging",
    "median within identical stay/time/source/rank only; no cross-source averaging",
    "23:59:59 on chartdate; exact-charttime-only sensitivity saved separately",
    paste(
      "MIT-LCP antibiotic+culture pairing; primary onset -48..0h and both",
      "components available by index; +24h retrospective sensitivity only"
    ),
    "5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4",
    SCRIPT_SHA256, format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    R.version.string
  )
)
fwrite(parameters, file.path(OUT_QC, "run_parameters_v1.csv"))

invariants <- data.table(
  check = c(
    "nonempty_cohort", "one_row_per_subject", "one_row_per_stay",
    "age_ge_18", "index_in_ICU", "FiO2_gap_within_120",
    "PEEP_gap_within_120", "PF_le_300", "PEEP_ge_5",
    "invasive_confirmed", "no_proximal_NIV",
    "no_procedure_225794_overlap", "no_raw_chartevents_NIV_marker",
    "infection_in_locked_window",
    "infection_evidence_available_by_index",
    "sensitivity_nonempty", "sensitivity_one_row_per_subject",
    "sensitivity_PF_and_PEEP_valid", "sensitivity_infection_window_valid",
    "sensitivity_evidence_available_by_plus24h",
    "exact_culture_sensitivity_nonempty",
    "exact_culture_sensitivity_precision_valid"
  ),
  passed = c(
    nrow(cohort_out) > 0,
    !anyDuplicated(cohort_out$subject_id), !anyDuplicated(cohort_out$stay_id),
    all(cohort_out$age_at_admission >= AGE_MIN),
    all(cohort_out$pao2_time >= cohort_out$intime &
          cohort_out$pao2_time <= cohort_out$outtime),
    all(cohort_out$fio2_abs_gap_min <= PF_WINDOW_MIN),
    all(cohort_out$peep_abs_gap_min <= PEEP_WINDOW_MIN),
    all(cohort_out$pf_ratio <= PF_MAX),
    all(cohort_out$peep_near_value >= PEEP_MIN),
    all(cohort_out$invasive_confirmed), all(!cohort_out$proximal_niv),
    all(!cohort_out$niv_procedure_proximal),
    all(!cohort_out$niv_chartevents_proximal),
    all(cohort_out$infection_gap_h >= -INFECTION_BEFORE_H &
          cohort_out$infection_gap_h <= INFECTION_AFTER_H),
    all(cohort_out$infection_evidence_time <= cohort_out$pao2_time),
    nrow(sensitivity_cohort_out) > 0,
    !anyDuplicated(sensitivity_cohort_out$subject_id),
    all(sensitivity_cohort_out$pf_ratio <= PF_MAX &
          sensitivity_cohort_out$peep_near_value >= PEEP_MIN),
    all(sensitivity_cohort_out$sensitivity_infection_gap_h >=
          -INFECTION_BEFORE_H &
          sensitivity_cohort_out$sensitivity_infection_gap_h <=
            SENSITIVITY_INFECTION_AFTER_H),
    all(sensitivity_cohort_out$sensitivity_evidence_time <=
          sensitivity_cohort_out$pao2_time +
            SENSITIVITY_INFECTION_AFTER_H * 3600),
    nrow(exact_culture_cohort_out) > 0,
    all(exact_culture_cohort_out$infection_culture_time_precision ==
          "exact_charttime")
  )
)
fwrite(invariants, file.path(OUT_QC, "qc_invariants_v1.csv"))
if (!all(invariants$passed)) {
  stop("One or more strict-cohort invariants failed; see qc_invariants_v1.csv")
}

## Aggregate-output leakage guard.
sensitive_names <- c("subject_id", "hadm_id", "stay_id")
aggregate_csvs <- list.files(OUT_QC, pattern = "\\.csv$", full.names = TRUE)
for (f in aggregate_csvs) {
  nms <- names(fread(f, nrows = 0, showProgress = FALSE))
  if (any(nms %chin% sensitive_names)) {
    stop("Identifier-like column found in aggregate output: ", basename(f))
  }
}

## Phase-1 remains blinded: neither restricted cohort contains a mortality or
## discharge-status field, and aggregate QC contains no outcome-language token.
prohibited_outcome_names <- c(
  "hospital_expire_flag", "deathtime", "dod", "died_hosp", "died_28d",
  "died_icu", "hospitaldischargestatus", "unitdischargestatus"
)
private_objects <- list(
  primary = cohort_out,
  infection_plus24_sensitivity = sensitivity_cohort_out,
  exact_culture_sensitivity = exact_culture_cohort_out
)
private_name_guard <- all(vapply(
  private_objects,
  function(x) !any(tolower(names(x)) %chin% prohibited_outcome_names),
  logical(1)
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
    "private_cohort_column_names_exclude_outcomes",
    "aggregate_QC_content_excludes_outcome_tokens"
  ),
  passed = c(private_name_guard, aggregate_token_guard)
)
fwrite(outcome_guard, file.path(OUT_QC, "qc_outcome_leakage_guard_v1.csv"))
if (!all(outcome_guard$passed)) {
  stop("Phase-1 outcome leakage guard failed")
}

run_finished_at <- Sys.time()
run_manifest <- data.table(
  field = c(
    "locked_config_version", "script_path", "script_sha256",
    "mimic_root", "mimic_code_reference_commit", "run_started_at",
    "run_finished_at", "elapsed_minutes", "R_version",
    "lab_cache_used", "chartevents_cache_used"
  ),
  value = as.character(c(
    LOCKED$version, normalizePath(SCRIPT_PATH), SCRIPT_SHA256,
    normalizePath(MIMIC),
    "5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4",
    format(RUN_STARTED_AT, "%Y-%m-%d %H:%M:%S %z"),
    format(run_finished_at, "%Y-%m-%d %H:%M:%S %z"),
    round(as.numeric(difftime(run_finished_at, RUN_STARTED_AT, units = "mins")), 3),
    R.version.string, LAB_CACHE_PREEXISTED, CE_CACHE_PREEXISTED
  ))
)
fwrite(run_manifest, file.path(OUT_QC, "run_manifest_v1.csv"))

log_msg(
  "Strict MIMIC index cohort complete:", nrow(cohort_out), "patients/stays"
)
log_msg("Restricted outputs:", OUT_PRIVATE)
log_msg("Aggregate QC:", OUT_QC)

## Write the downstream completion gate only after every invariant and leakage
## guard has passed. Hash all four restricted Phase-1 products so a consumer
## cannot silently mix files from different runs.
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required to create the Phase-1 completion gate")
}
formal_rds <- c(
  stay_candidates = file.path(
    OUT_PRIVATE, "mimic_index_stay_candidates_v1.rds"
  ),
  primary_cohort = file.path(OUT_PRIVATE, "mimic_index_cohort_v1.rds"),
  infection_plus24_sensitivity = file.path(
    OUT_PRIVATE, "mimic_index_cohort_infection_plus24_sensitivity_v1.rds"
  ),
  exact_culture_time_sensitivity = file.path(
    OUT_PRIVATE,
    "mimic_index_cohort_exact_culture_time_sensitivity_v1.rds"
  )
)
if (!all(file.exists(formal_rds))) {
  stop("One or more formal Phase-1 RDS products are missing")
}
formal_rds_sha256 <- vapply(
  formal_rds,
  function(path) digest::digest(file = path, algo = "sha256"),
  character(1)
)
completed_at <- Sys.time()
completion_gate <- data.table(
  field = c(
    "config_path", "locked_config_version", "script_sha256", "completed_at",
    "all_invariants_pass", "outcome_leakage_guard_pass",
    paste0(names(formal_rds), "_rds_sha256")
  ),
  value = as.character(c(
    normalizePath(CONFIG_PATH), LOCKED$version, SCRIPT_SHA256,
    format(completed_at, "%Y-%m-%d %H:%M:%S %z"),
    all(invariants$passed), all(outcome_guard$passed), formal_rds_sha256
  ))
)
fwrite(completion_gate, PHASE1_COMPLETE_TMP)
log_msg(
  "BUILD_COMPLETE | config", LOCKED$version, "| script SHA256", SCRIPT_SHA256
)
if (!file.rename(PHASE1_COMPLETE_TMP, PHASE1_COMPLETE)) {
  stop("Could not atomically publish Phase-1 completion gate")
}
