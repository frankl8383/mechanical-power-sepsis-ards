#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: MIMIC-IV paired ventilator exposure
#
# Outcome-blind Phase-2 extraction. The input strict cohort is required to be
# outcome-free, and this script uses an explicit allow-list before any joins or
# summaries. Row-level artifacts remain under private/mimic; only aggregate QC
# is written outside that directory.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/03_build_mimic_paired_exposure.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))

stopifnot(
  identical(LOCKED$primary_exposure_summary, "first_valid_complete_tuple"),
  LOCKED$primary_exposure_window_hours_after_index == 6,
  LOCKED$primary_ventilator_tuple_pair_window_minutes == 60,
  LOCKED$sensitivity_ventilator_tuple_pair_window_minutes == 30
)

input_cohort <- file.path(PRIVATE_ROOT, "mimic", "mimic_index_cohort_v1.rds")
raw_chart <- file.path(MIMIC_ROOT, "icu", "chartevents.csv.gz")
raw_ditems <- file.path(MIMIC_ROOT, "icu", "d_items.csv.gz")
private_out <- file.path(PRIVATE_ROOT, "mimic")
cache_out <- file.path(private_out, "cache_v1")
qc_out <- file.path(QC_ROOT, "mimic_exposure")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

phase1_complete <- file.path(QC_ROOT, "mimic", "phase1_complete_v1.csv")
phase2_complete <- file.path(qc_out, "phase2_mimic_exposure_complete_v1.csv")
phase2_complete_tmp <- paste0(phase2_complete, ".tmp")
# A completion marker is a downstream gate, never a progress indicator.
unlink(c(phase2_complete, phase2_complete_tmp), force = TRUE)

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required for completion-gate verification.")
}
script_sha256 <- digest::digest(file = script_path, algo = "sha256")
phase1_script <- file.path(dirname(script_path), "01_build_mimic_index_cohort.R")
if (!file.exists(phase1_script)) stop("Missing Phase-1 script: ", phase1_script)
if (!file.exists(phase1_complete)) {
  stop("Phase-1 completion gate is absent: ", phase1_complete)
}
phase1_gate <- fread(phase1_complete, showProgress = FALSE)
if (!identical(names(phase1_gate), c("field", "value")) ||
    anyDuplicated(phase1_gate$field)) {
  stop("Malformed Phase-1 completion gate.")
}
phase1_values <- setNames(phase1_gate$value, phase1_gate$field)
required_phase1_fields <- c(
  "locked_config_version", "script_sha256", "all_invariants_pass",
  "outcome_leakage_guard_pass", "primary_cohort_rds_sha256"
)
missing_phase1_fields <- setdiff(required_phase1_fields, names(phase1_values))
if (length(missing_phase1_fields)) {
  stop(
    "Phase-1 completion gate is missing: ",
    paste(missing_phase1_fields, collapse = ", ")
  )
}
if (!identical(phase1_values[["locked_config_version"]], LOCKED$version) ||
    tolower(phase1_values[["all_invariants_pass"]]) != "true" ||
    tolower(phase1_values[["outcome_leakage_guard_pass"]]) != "true") {
  stop("Phase-1 completion gate did not certify the locked configuration/QC.")
}
phase1_script_sha256 <- digest::digest(file = phase1_script, algo = "sha256")
if (!identical(phase1_script_sha256, phase1_values[["script_sha256"]])) {
  stop("Phase-1 script SHA differs from the published completion gate.")
}
phase1_gate_sha256 <- digest::digest(file = phase1_complete, algo = "sha256")

if (!file.exists(input_cohort)) stop("Missing strict MIMIC cohort: ", input_cohort)
if (!file.exists(raw_chart)) stop("Missing MIMIC chartevents: ", raw_chart)
if (!file.exists(raw_ditems)) stop("Missing MIMIC d_items: ", raw_ditems)
input_cohort_sha256 <- digest::digest(file = input_cohort, algo = "sha256")
if (!identical(
  input_cohort_sha256,
  phase1_values[["primary_cohort_rds_sha256"]]
)) {
  stop("Strict MIMIC cohort SHA differs from the Phase-1 completion gate.")
}

forbidden_pattern <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)

# ---------------------------------------------------------------------------
# Outcome-leakage guard and strict-cohort bounds
# ---------------------------------------------------------------------------

cohort_source <- readRDS(input_cohort)
if (any(grepl(forbidden_pattern, names(cohort_source), ignore.case = TRUE))) {
  stop(
    "Leakage guard failed: strict index artifact contains outcome-like fields. ",
    "Rebuild an outcome-free index artifact before exposure extraction."
  )
}

required_index <- c(
  "subject_id", "hadm_id", "stay_id", "intime", "outtime",
  "age_at_admission", "gender", "pao2_time", "pao2",
  "fio2_near_value", "peep_near_value", "pf_ratio",
  "invasive_evidence_type", "infection_direction"
)
optional_index <- c(
  "first_careunit", "last_careunit", "admission_type", "pao2_source",
  "fio2_near_time", "fio2_near_source", "fio2_signed_gap_min",
  "fio2_abs_gap_min", "peep_near_time", "peep_near_source",
  "peep_near_label", "peep_signed_gap_min", "peep_abs_gap_min",
  "infection_time", "infection_gap_h", "infection_evidence_time",
  "infection_culture_time_precision", "infection_available_by_index"
)
missing_index <- setdiff(required_index, names(cohort_source))
if (length(missing_index)) {
  stop("Strict MIMIC cohort is missing: ", paste(missing_index, collapse = ", "))
}
index_fields <- c(required_index, intersect(optional_index, names(cohort_source)))
index <- as.data.table(cohort_source)[, ..index_fields]
rm(cohort_source)
gc(verbose = FALSE)

if (any(grepl(forbidden_pattern, names(index), ignore.case = TRUE))) {
  stop("Leakage guard failed after strict-cohort projection.")
}
if (anyDuplicated(index$subject_id) || anyDuplicated(index$stay_id)) {
  stop("Strict index cohort must have one row per subject and stay.")
}
if (anyNA(index$subject_id) || anyNA(index$stay_id) || anyNA(index$pao2_time)) {
  stop("Strict index cohort has a missing identifier or index time.")
}

as_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = "UTC"))
  as.POSIXct(x, tz = "UTC")
}
for (v in intersect(
  c(
    "intime", "outtime", "pao2_time", "fio2_near_time",
    "peep_near_time", "infection_time", "infection_evidence_time"
  ),
  names(index)
)) set(index, j = v, value = as_utc(index[[v]]))

setnames(index, "pao2_time", "index_time")
index[, protocol_exposure_end := index_time +
        3600 * LOCKED$primary_exposure_window_hours_after_index]
index[, observable_exposure_end := pmin(protocol_exposure_end, outtime)]
if (anyNA(index$observable_exposure_end) ||
    any(index$observable_exposure_end < index$index_time)) {
  stop("Invalid observable exposure bound in strict MIMIC cohort.")
}
index[, index_from_icu_hours := as.numeric(difftime(
  index_time, intime, units = "hours"
))]
index[, observable_window_minutes := as.numeric(difftime(
  observable_exposure_end, index_time, units = "mins"
))]

# ---------------------------------------------------------------------------
# Locked item mapping and metadata verification
# ---------------------------------------------------------------------------

ranges <- LOCKED$physiologic_ranges
item_map <- data.table(
  itemid = c(
    224696L, 224695L, 220339L, 224700L, 224685L,
    224684L, 224690L, 220210L, 224688L
  ),
  component = c(
    "pplat", "ppeak", "peep", "peep", "vt",
    "vt", "rr", "rr", "rr"
  ),
  source = c(
    "Plateau Pressure", "Peak Insp. Pressure", "PEEP set",
    "Total PEEP Level", "Tidal Volume (observed)",
    "Tidal Volume (set)", "Respiratory Rate (Total)",
    "Respiratory Rate", "Respiratory Rate (Set)"
  ),
  source_rank = c(1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 3L),
  analytic_role = c(
    "anchor", "explicit", "preferred", "flagged_fallback",
    "preferred_observed", "flagged_fallback", "preferred_total",
    "fallback_measured", "flagged_fallback_set"
  )
)
item_map[, `:=`(
  lower = fcase(
    component == "pplat", ranges$plateau_cmH2O[[1L]],
    component == "ppeak", ranges$peak_cmH2O[[1L]],
    component == "peep", ranges$peep_cmH2O[[1L]],
    component == "vt", ranges$tidal_volume_mL[[1L]],
    component == "rr", ranges$respiratory_rate_per_min[[1L]]
  ),
  upper = fcase(
    component == "pplat", ranges$plateau_cmH2O[[2L]],
    component == "ppeak", ranges$peak_cmH2O[[2L]],
    component == "peep", ranges$peep_cmH2O[[2L]],
    component == "vt", ranges$tidal_volume_mL[[2L]],
    component == "rr", ranges$respiratory_rate_per_min[[2L]]
  )
)]

ditems <- fread(
  raw_ditems,
  select = c("itemid", "label", "linksto", "unitname"),
  showProgress = FALSE
)[itemid %in% item_map$itemid]
metadata_check <- merge(
  item_map[, .(itemid, locked_label = source)],
  ditems,
  by = "itemid", all.x = TRUE
)
metadata_check[, pass := !is.na(label) & label == locked_label &
                 linksto == "chartevents"]
if (any(!metadata_check$pass)) {
  stop("Locked MIMIC tuple item metadata did not match local d_items.")
}
fwrite(metadata_check, file.path(qc_out, "locked_item_metadata_QC.csv"))

# ---------------------------------------------------------------------------
# Targeted raw chartevents extraction. Filtering uses only numeric columns
# before the potentially comma-containing free-text value column.
# ---------------------------------------------------------------------------

cache_file <- file.path(cache_out, "selected_paired_exposure_chartevents_v1.rds")
refresh_cache <- identical(Sys.getenv("MIMIC_EXPOSURE_REFRESH_CACHE", "0"), "1")

read_target_chart <- function() {
  id_file <- tempfile("mimic_strict_stays_", fileext = ".txt")
  on.exit(unlink(id_file), add = TRUE)
  fwrite(index[, .(stay_id)], id_file, col.names = FALSE)
  item_tests <- paste(sprintf("$7==%d", item_map$itemid), collapse = " || ")
  rg_bin <- Sys.which("rg")
  if (nzchar(rg_bin)) {
    # Ripgrep narrows to the nine itemids before awk performs the stay lookup;
    # this preserves exact CSV rows while avoiding a full awk parse of every
    # chartevents row. The first seven MIMIC columns are comma-free.
    item_regex <- paste(item_map$itemid, collapse = "|")
    rg_pattern <- paste0(
      "^subject_id,|^[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,(?:",
      item_regex, ")(?:,|$)"
    )
    awk_program <- paste0(
      "NR==FNR { keep[$1]=1; next } ",
      "FNR==1 || ($3 in keep)"
    )
    read_cmd <- sprintf(
      "gzip -cd %s | %s --no-line-number --no-heading %s - | ",
      shQuote(raw_chart), shQuote(rg_bin), shQuote(rg_pattern)
    )
    read_cmd <- paste0(
      read_cmd,
      sprintf("LC_ALL=C awk -F',' %s %s -", shQuote(awk_program), shQuote(id_file))
    )
  } else {
    awk_program <- paste0(
      "NR==FNR { keep[$1]=1; next } ",
      "FNR==1 || (($3 in keep) && (", item_tests, "))"
    )
    read_cmd <- sprintf(
      "gzip -cd %s | LC_ALL=C awk -F',' %s %s -",
      shQuote(raw_chart), shQuote(awk_program), shQuote(id_file)
    )
  }
  message(
    "Reading locked MIMIC ventilator items for ", nrow(index),
    " strict-cohort stays ..."
  )
  x <- fread(
    cmd = read_cmd,
    select = c(
      "subject_id", "hadm_id", "stay_id", "charttime", "storetime",
      "itemid", "valuenum", "warning"
    ),
    showProgress = interactive()
  )
  attr(x, "target_stay_ids") <- sort(index$stay_id)
  attr(x, "locked_itemids") <- sort(item_map$itemid)
  attr(x, "created_at") <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  saveRDS(x, cache_file, compress = "gzip")
  x
}

resp <- if (file.exists(cache_file) && !refresh_cache) {
  z <- readRDS(cache_file)
  cache_ids <- attr(z, "target_stay_ids")
  cache_items <- attr(z, "locked_itemids")
  if (is.null(cache_ids) || !identical(sort(cache_ids), sort(index$stay_id)) ||
      is.null(cache_items) || !identical(sort(cache_items), sort(item_map$itemid))) {
    message("Exposure cache target differs from strict cohort; rebuilding cache.")
    read_target_chart()
  } else {
    message("Reading target-specific private MIMIC exposure cache.")
    z
  }
} else {
  read_target_chart()
}

if (!nrow(resp)) stop("No locked MIMIC ventilator observations were read.")
if (any(!resp$stay_id %in% index$stay_id)) {
  stop("Raw extraction admitted a stay outside the strict cohort.")
}
if (any(!resp$itemid %in% item_map$itemid)) {
  stop("Raw extraction admitted an item outside the locked allow-list.")
}

resp[, charttime := as_utc(charttime)]
resp[, storetime := as_utc(storetime)]
resp[, value_num := suppressWarnings(as.numeric(valuenum))]
resp[!is.finite(value_num), value_num := NA_real_]
resp[, storetime_missing := is.na(storetime)]
resp[, available_time := charttime]
resp[!is.na(storetime), available_time := pmax(charttime, storetime)]
resp <- merge(resp, item_map, by = "itemid", all.x = TRUE, sort = FALSE)
if (anyNA(resp$component)) stop("An extracted item failed locked mapping.")
resp[, in_component_range := !is.na(value_num) &
       value_num >= lower & value_num <= upper]
resp[, below_component_range := !is.na(value_num) & value_num < lower]
resp[, above_component_range := !is.na(value_num) & value_num > upper]

# Same-time/source numeric duplicates are reduced to their median. The derived
# median becomes available after the last numeric duplicate is documented.
obs <- resp[, .(
  raw_n = .N,
  numeric_n = sum(!is.na(value_num)),
  nonnumeric_n = sum(is.na(value_num)),
  value = if (any(!is.na(value_num))) median(value_num, na.rm = TRUE) else NA_real_,
  available_time = if (any(!is.na(value_num))) {
    max(available_time[!is.na(value_num)], na.rm = TRUE)
  } else {
    as.POSIXct(NA, tz = "UTC")
  },
  any_storetime_fallback = any(storetime_missing & !is.na(value_num)),
  any_warning = any(warning == 1L, na.rm = TRUE)
), by = .(
  stay_id, component, itemid, source, source_rank, analytic_role,
  measurement_time = charttime, lower, upper
)]
obs[, in_component_range := !is.na(value) & value >= lower & value <= upper]

bounds <- index[, .(
  subject_id, hadm_id, stay_id, index_time,
  protocol_exposure_end, observable_exposure_end
)]
obs <- bounds[obs, on = "stay_id"]
if (anyNA(obs$index_time)) stop("A tuple observation failed strict-cohort join.")

pplat_obs <- obs[component == "pplat"]
pplat_window_raw <- pplat_obs[
  measurement_time >= index_time &
    measurement_time <= observable_exposure_end
]
anchors_range_valid <- pplat_window_raw[in_component_range == TRUE]
anchors <- anchors_range_valid[
  available_time >= index_time &
    available_time <= observable_exposure_end
]
setorder(anchors, stay_id, measurement_time, available_time, itemid)
anchors[, anchor_id := .I]
setnames(
  anchors,
  c(
    "measurement_time", "available_time", "value", "source",
    "any_storetime_fallback"
  ),
  c(
    "anchor_time", "anchor_available_time", "pplat", "pplat_source",
    "pplat_storetime_fallback"
  )
)
anchors <- anchors[, .(
  subject_id, hadm_id, stay_id, anchor_id, index_time,
  protocol_exposure_end, observable_exposure_end, anchor_time,
  anchor_available_time, pplat, pplat_source,
  pplat_itemid = itemid, pplat_storetime_fallback
)]

candidates <- obs[
  component != "pplat" &
    measurement_time >= index_time &
    measurement_time <= observable_exposure_end &
    available_time >= index_time &
    available_time <= observable_exposure_end &
    in_component_range == TRUE
]

empty_pair <- function() {
  data.table(
    anchor_id = integer(), value = numeric(), time = as.POSIXct(character()),
    available_time = as.POSIXct(character()), source = character(),
    itemid = integer(), source_rank = integer(), signed_gap = numeric(),
    abs_gap = numeric(), storetime_fallback = logical()
  )
}

pair_one_component <- function(anchor_dt, candidate_dt, component_name,
                               window_minutes, preferred_only = FALSE) {
  a <- anchor_dt[, .(stay_id, anchor_id, anchor_time)]
  cdt <- candidate_dt[component == component_name]
  if (preferred_only && component_name %chin% c("peep", "vt", "rr")) {
    cdt <- cdt[source_rank == 1L]
  }
  if (!nrow(a) || !nrow(cdt)) return(empty_pair())
  cdt <- cdt[, .(
    stay_id, component_time = measurement_time,
    component_available_time = available_time,
    component_value = value, component_source = source,
    component_itemid = itemid, source_rank,
    component_storetime_fallback = any_storetime_fallback
  )]
  z <- merge(a, cdt, by = "stay_id", allow.cartesian = TRUE)
  z[, signed_gap := as.numeric(difftime(
    component_time, anchor_time, units = "mins"
  ))]
  z <- z[abs(signed_gap) <= window_minutes]
  if (!nrow(z)) return(empty_pair())
  z[, `:=`(abs_gap = abs(signed_gap), future_tie = signed_gap > 0)]
  # Locked order: source hierarchy, nearest absolute gap, prior on exact tie.
  setorder(
    z, anchor_id, source_rank, abs_gap, future_tie,
    component_time, component_available_time, component_itemid
  )
  z <- z[, .SD[1L], by = anchor_id]
  z[, .(
    anchor_id, value = component_value, time = component_time,
    available_time = component_available_time, source = component_source,
    itemid = component_itemid, source_rank, signed_gap, abs_gap,
    storetime_fallback = component_storetime_fallback
  )]
}

build_variant <- function(variant_name, window_minutes, preferred_only = FALSE) {
  t <- copy(anchors)
  for (comp in c("ppeak", "peep", "vt", "rr")) {
    paired <- pair_one_component(
      anchors, candidates, comp, window_minutes,
      preferred_only = preferred_only
    )
    setnames(
      paired,
      setdiff(names(paired), "anchor_id"),
      paste0(comp, "_", setdiff(names(paired), "anchor_id"))
    )
    t <- paired[t, on = "anchor_id"]
  }

  t[, complete_components :=
      !is.na(ppeak_value) & !is.na(peep_value) &
      !is.na(vt_value) & !is.na(rr_value)]
  t[, pressure_order_valid := complete_components &
      ppeak_value >= pplat & pplat >= peep_value]
  t[, delta_p := fifelse(complete_components, pplat - peep_value, NA_real_)]
  t[, resistive_pressure := fifelse(
    complete_components, ppeak_value - pplat, NA_real_
  )]
  t[, delta_valid := pressure_order_valid &
      delta_p >= ranges$driving_pressure_cmH2O[[1L]] &
      delta_p <= ranges$driving_pressure_cmH2O[[2L]]]
  t[, smp := fifelse(
    complete_components,
    0.098 * rr_value * (vt_value / 1000) *
      (ppeak_value - 0.5 * (pplat - peep_value)),
    NA_real_
  )]
  t[, smp_valid := delta_valid &
      smp >= ranges$surrogate_mp_J_per_min[[1L]] &
      smp <= ranges$surrogate_mp_J_per_min[[2L]]]

  available_cols <- c(
    "anchor_available_time", "ppeak_available_time", "peep_available_time",
    "vt_available_time", "rr_available_time"
  )
  measurement_cols <- c(
    "anchor_time", "ppeak_time", "peep_time", "vt_time", "rr_time"
  )
  t[, prediction_time_num := do.call(
    pmax, c(lapply(.SD, as.numeric), list(na.rm = FALSE))
  ), .SDcols = available_cols]
  t[, tuple_last_measurement_time_num := do.call(
    pmax, c(lapply(.SD, as.numeric), list(na.rm = FALSE))
  ), .SDcols = measurement_cols]
  t[, prediction_time := as.POSIXct(
    prediction_time_num, origin = "1970-01-01", tz = "UTC"
  )]
  t[, tuple_last_measurement_time := as.POSIXct(
    tuple_last_measurement_time_num, origin = "1970-01-01", tz = "UTC"
  )]
  t[, c("prediction_time_num", "tuple_last_measurement_time_num") := NULL]
  t[, availability_valid := complete_components & !is.na(prediction_time) &
      prediction_time >= anchor_time &
      prediction_time >= index_time &
      prediction_time <= observable_exposure_end]
  t[, valid_tuple := smp_valid & availability_valid]

  t[, invalid_missing_ppeak := is.na(ppeak_value)]
  t[, invalid_missing_peep := is.na(peep_value)]
  t[, invalid_missing_vt := is.na(vt_value)]
  t[, invalid_missing_rr := is.na(rr_value)]
  t[, invalid_peak_below_plateau := complete_components & ppeak_value < pplat]
  t[, invalid_plateau_below_peep := complete_components & pplat < peep_value]
  t[, invalid_delta_range := pressure_order_valid & !delta_valid]
  t[, invalid_smp_range := delta_valid & !smp_valid]
  t[, invalid_late_availability := complete_components & !availability_valid]
  t[, variant := variant_name]

  setorder(t, stay_id, anchor_time, anchor_available_time, anchor_id)
  valid <- t[valid_tuple == TRUE]
  selected <- valid[, .SD[1L], by = stay_id]
  list(all_anchors = t, valid = valid, selected = selected)
}

variants <- list(
  primary_60min = build_variant(
    "primary_60min",
    LOCKED$primary_ventilator_tuple_pair_window_minutes,
    preferred_only = FALSE
  ),
  sensitivity_30min = build_variant(
    "sensitivity_30min",
    LOCKED$sensitivity_ventilator_tuple_pair_window_minutes,
    preferred_only = FALSE
  ),
  sensitivity_preferred_60min = build_variant(
    "sensitivity_preferred_60min",
    LOCKED$primary_ventilator_tuple_pair_window_minutes,
    preferred_only = TRUE
  )
)

# ---------------------------------------------------------------------------
# Invariants: temporal scope, hierarchy, formulas, and earliest-valid selection
# ---------------------------------------------------------------------------

check_variant <- function(v, window_minutes, preferred_only) {
  a <- v$all_anchors
  s <- v$selected
  checks <- list(
    unique_anchor_id = !anyDuplicated(a$anchor_id),
    one_selected_per_stay = !anyDuplicated(s$stay_id),
    selected_is_valid = all(s$valid_tuple),
    anchor_measurement_in_exact_window = all(
      s$anchor_time >= s$index_time &
        s$anchor_time <= s$observable_exposure_end
    ),
    anchor_availability_in_exact_window = all(
      s$anchor_available_time >= s$index_time &
        s$anchor_available_time <= s$observable_exposure_end
    ),
    pairing_window = all(
      s$ppeak_abs_gap <= window_minutes & s$peep_abs_gap <= window_minutes &
        s$vt_abs_gap <= window_minutes & s$rr_abs_gap <= window_minutes
    ),
    all_component_measurements_in_exact_window = all(
      s$ppeak_time >= s$index_time &
        s$ppeak_time <= s$observable_exposure_end &
        s$peep_time >= s$index_time &
        s$peep_time <= s$observable_exposure_end &
        s$vt_time >= s$index_time &
        s$vt_time <= s$observable_exposure_end &
        s$rr_time >= s$index_time &
        s$rr_time <= s$observable_exposure_end
    ),
    all_component_availability_in_exact_window = all(
      s$ppeak_available_time >= s$index_time &
        s$ppeak_available_time <= s$observable_exposure_end &
        s$peep_available_time >= s$index_time &
        s$peep_available_time <= s$observable_exposure_end &
        s$vt_available_time >= s$index_time &
        s$vt_available_time <= s$observable_exposure_end &
        s$rr_available_time >= s$index_time &
        s$rr_available_time <= s$observable_exposure_end
    ),
    pressure_order = all(s$ppeak_value >= s$pplat & s$pplat >= s$peep_value),
    prediction_not_before_anchor = all(s$prediction_time >= s$anchor_time),
    prediction_by_observable_end = all(
      s$prediction_time <= s$observable_exposure_end
    ),
    delta_formula = all(abs(s$delta_p - (s$pplat - s$peep_value)) < 1e-10),
    resistive_formula = all(abs(
      s$resistive_pressure - (s$ppeak_value - s$pplat)
    ) < 1e-10),
    smp_formula = all(abs(
      s$smp - 0.098 * s$rr_value * (s$vt_value / 1000) *
        (s$ppeak_value - 0.5 * (s$pplat - s$peep_value))
    ) < 1e-10),
    earliest_valid_anchor = if (!nrow(s)) {
      TRUE
    } else {
      earliest <- v$valid[, .(earliest_anchor_time = min(anchor_time)), by = stay_id]
      chk <- earliest[s[, .(stay_id, anchor_time)], on = "stay_id"]
      all(chk$anchor_time == chk$earliest_anchor_time)
    }
  )

  if (preferred_only) {
    checks$preferred_peep_only <- all(s$peep_itemid == 220339L)
    checks$preferred_vt_only <- all(s$vt_itemid == 224685L)
    checks$preferred_rr_only <- all(s$rr_itemid == 224690L)
  } else {
    fallback_ok <- function(component_name) {
      rank_col <- paste0(component_name, "_source_rank")
      fb <- a[get(rank_col) > 1L & valid_tuple == TRUE]
      if (!nrow(fb)) return(TRUE)
      better_candidates <- candidates[component == component_name,
        .(
          stay_id, candidate_time = measurement_time,
          candidate_rank = source_rank
        )
      ]
      if (!nrow(better_candidates)) return(TRUE)
      z <- merge(
        fb[, .(
          stay_id, anchor_id, anchor_time,
          selected_rank = get(rank_col)
        )],
        better_candidates,
        by = "stay_id", allow.cartesian = TRUE
      )
      !nrow(z[
        candidate_rank < selected_rank &
          abs(as.numeric(difftime(
            candidate_time, anchor_time, units = "mins"
          ))) <= window_minutes
      ])
    }
    checks$fallback_peep_only_when_needed <- fallback_ok("peep")
    checks$fallback_vt_only_when_needed <- fallback_ok("vt")
    checks$fallback_rr_only_when_needed <- fallback_ok("rr")
  }
  data.table(check = names(checks), pass = unlist(checks, use.names = FALSE))
}

invariant_tables <- list(
  primary_60min = check_variant(
    variants$primary_60min,
    LOCKED$primary_ventilator_tuple_pair_window_minutes,
    FALSE
  ),
  sensitivity_30min = check_variant(
    variants$sensitivity_30min,
    LOCKED$sensitivity_ventilator_tuple_pair_window_minutes,
    FALSE
  ),
  sensitivity_preferred_60min = check_variant(
    variants$sensitivity_preferred_60min,
    LOCKED$primary_ventilator_tuple_pair_window_minutes,
    TRUE
  )
)
invariants <- rbindlist(invariant_tables, idcol = "variant")
if (any(!invariants$pass)) {
  stop(
    "Invariant failure(s): ",
    paste(invariants[pass == FALSE, paste(variant, check, sep = ":")],
          collapse = ", ")
  )
}

# ---------------------------------------------------------------------------
# Outcome-free private artifacts
# ---------------------------------------------------------------------------

tuple_fields <- c(
  "stay_id", "anchor_id", "anchor_time", "anchor_available_time",
  "pplat", "pplat_source", "pplat_itemid", "pplat_storetime_fallback",
  "ppeak_value", "ppeak_time", "ppeak_available_time", "ppeak_source",
  "ppeak_itemid", "ppeak_source_rank", "ppeak_signed_gap", "ppeak_abs_gap",
  "ppeak_storetime_fallback", "peep_value", "peep_time",
  "peep_available_time", "peep_source", "peep_itemid", "peep_source_rank",
  "peep_signed_gap", "peep_abs_gap", "peep_storetime_fallback", "vt_value",
  "vt_time", "vt_available_time", "vt_source", "vt_itemid",
  "vt_source_rank", "vt_signed_gap", "vt_abs_gap", "vt_storetime_fallback",
  "rr_value", "rr_time", "rr_available_time", "rr_source", "rr_itemid",
  "rr_source_rank", "rr_signed_gap", "rr_abs_gap", "rr_storetime_fallback",
  "delta_p", "resistive_pressure", "smp", "tuple_last_measurement_time",
  "prediction_time", "variant"
)

make_cohort_artifact <- function(v, artifact_name) {
  selected <- v$selected[, ..tuple_fields]
  counts <- if (nrow(v$valid)) {
    v$valid[, .(
      n_valid_tuples = .N,
      first_valid_anchor_time = min(anchor_time),
      last_valid_anchor_time = max(anchor_time)
    ), by = stay_id]
  } else {
    data.table(
      stay_id = integer(), n_valid_tuples = integer(),
      first_valid_anchor_time = as.POSIXct(character()),
      last_valid_anchor_time = as.POSIXct(character())
    )
  }
  out <- merge(index, counts, by = "stay_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, selected, by = "stay_id", all.x = TRUE, sort = FALSE)
  out[, tuple_observed := !is.na(anchor_time)]
  out[is.na(n_valid_tuples), n_valid_tuples := 0L]
  if (nrow(out) != nrow(index) || anyDuplicated(out$stay_id)) {
    stop("Private artifact is not one row per strict stay: ", artifact_name)
  }
  if (any(grepl(forbidden_pattern, names(out), ignore.case = TRUE))) {
    stop("Leakage guard failed in private artifact: ", artifact_name)
  }
  attr(out, "rebuild_metadata") <- list(
    version = "mimic_paired_exposure_v1",
    artifact = artifact_name,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    locked_config_version = LOCKED$version,
    script = script_path,
    outcome_blind = TRUE,
    tuple_anchor_itemid = 224696L,
    selection = "earliest physiologically valid complete plateau anchor",
    documentation_availability = paste(
      "pmax(charttime, storetime); missing storetime falls back to charttime;",
      "every measurement and availability time is bounded by index through",
      "min(index+6h, ICU outtime)"
    )
  )
  saveRDS(out, file.path(private_out, artifact_name), compress = "xz")
  out
}

artifacts <- list(
  primary_60min = make_cohort_artifact(
    variants$primary_60min,
    "mimic_paired_exposure_primary_60min_v1.rds"
  ),
  sensitivity_30min = make_cohort_artifact(
    variants$sensitivity_30min,
    "mimic_paired_exposure_sensitivity_30min_v1.rds"
  ),
  sensitivity_preferred_60min = make_cohort_artifact(
    variants$sensitivity_preferred_60min,
    "mimic_paired_exposure_sensitivity_preferred_60min_v1.rds"
  )
)

all_valid_fields <- c("subject_id", "hadm_id", tuple_fields)
primary_valid_private <- variants$primary_60min$valid[, ..all_valid_fields]
if (any(grepl(forbidden_pattern, names(primary_valid_private), ignore.case = TRUE))) {
  stop("Leakage guard failed in all-valid-tuples artifact.")
}
attr(primary_valid_private, "rebuild_metadata") <- list(
  version = "mimic_paired_exposure_all_valid_primary_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED$version,
  outcome_blind = TRUE
)
saveRDS(
  primary_valid_private,
  file.path(private_out, "mimic_paired_exposure_all_valid_primary_60min_v1.rds"),
  compress = "xz"
)

# ---------------------------------------------------------------------------
# Aggregate QC only; identifiers never leave the private directory
# ---------------------------------------------------------------------------

raw_qc <- resp[, .(
  raw_rows = .N,
  strict_stays_with_item = uniqueN(stay_id),
  numeric_rows = sum(!is.na(value_num)),
  nonnumeric_rows = sum(is.na(value_num)),
  rows_in_locked_range = sum(in_component_range),
  rows_below_locked_range = sum(below_component_range),
  rows_above_locked_range = sum(above_component_range),
  missing_storetime_rows = sum(storetime_missing),
  warning_rows = sum(warning == 1L, na.rm = TRUE)
), by = .(component, itemid, source, source_rank, analytic_role)]
setorder(raw_qc, component, source_rank, itemid)
fwrite(raw_qc, file.path(qc_out, "raw_component_item_QC.csv"))

window_qc <- obs[
  measurement_time >= index_time & measurement_time <= observable_exposure_end,
  .(
    timestamp_source_groups = .N,
    strict_stays_with_group = uniqueN(stay_id),
    groups_in_locked_range = sum(in_component_range),
    groups_outside_locked_range = sum(!is.na(value) & !in_component_range),
    groups_available_in_exact_window = sum(
      in_component_range & available_time >= index_time &
        available_time <= observable_exposure_end
    ),
    groups_available_after_observable_end = sum(
      in_component_range & available_time > observable_exposure_end
    )
  ),
  by = .(component, itemid, source, source_rank)
]
setorder(window_qc, component, source_rank, itemid)
fwrite(window_qc, file.path(qc_out, "exposure_window_component_item_QC.csv"))

hierarchy_qc <- copy(item_map)
hierarchy_qc[, same_rank_tie_break := paste(
  "nearest absolute measurement gap; prior wins an exact prior/future tie;",
  "then earlier measurement and availability times"
)]
fwrite(hierarchy_qc, file.path(qc_out, "locked_source_hierarchy_QC.csv"))

late_entries <- obs[
  measurement_time >= index_time & measurement_time <= observable_exposure_end &
    in_component_range == TRUE & available_time > observable_exposure_end
]
late_entry_qc <- if (nrow(late_entries)) {
  late_entries[, .(
    late_timestamp_source_groups = .N,
    strict_stays_with_late_entry = uniqueN(stay_id),
    median_minutes_after_observable_end = median(as.numeric(difftime(
      available_time, observable_exposure_end, units = "mins"
    ))),
    maximum_minutes_after_observable_end = max(as.numeric(difftime(
      available_time, observable_exposure_end, units = "mins"
    )))
  ), by = .(component, itemid, source)]
} else {
  data.table(
    component = character(), itemid = integer(), source = character(),
    late_timestamp_source_groups = integer(),
    strict_stays_with_late_entry = integer(),
    median_minutes_after_observable_end = numeric(),
    maximum_minutes_after_observable_end = numeric()
  )
}
setorder(late_entry_qc, component, itemid)
fwrite(late_entry_qc, file.path(qc_out, "late_entry_exclusion_QC.csv"))

duplicate_qc <- obs[, .(
  timestamp_source_groups = .N,
  groups_with_duplicates = sum(raw_n > 1L),
  maximum_duplicate_count = max(raw_n),
  groups_with_storetime_fallback = sum(any_storetime_fallback)
), by = .(component, itemid, source)]
setorder(duplicate_qc, component, itemid)
fwrite(duplicate_qc, file.path(qc_out, "same_time_source_duplicate_QC.csv"))

funnel_for_variant <- function(v, variant_name) {
  a <- v$all_anchors
  stages <- list(
    strict_index_cohort = c(nrow(index), NA_integer_),
    any_plateau_label_in_window = c(
      uniqueN(pplat_window_raw$stay_id), nrow(pplat_window_raw)
    ),
    valid_plateau_measurement = c(
      uniqueN(anchors_range_valid$stay_id), nrow(anchors_range_valid)
    ),
    plateau_available_in_exact_window = c(
      uniqueN(anchors$stay_id), nrow(anchors)
    ),
    complete_paired_components = c(
      uniqueN(a[complete_components == TRUE]$stay_id),
      nrow(a[complete_components == TRUE])
    ),
    valid_pressure_ordering = c(
      uniqueN(a[pressure_order_valid == TRUE]$stay_id),
      nrow(a[pressure_order_valid == TRUE])
    ),
    valid_driving_pressure = c(
      uniqueN(a[delta_valid == TRUE]$stay_id), nrow(a[delta_valid == TRUE])
    ),
    valid_surrogate_mechanical_power = c(
      uniqueN(a[smp_valid == TRUE]$stay_id), nrow(a[smp_valid == TRUE])
    ),
    available_in_exact_window = c(
      uniqueN(a[smp_valid & availability_valid]$stay_id),
      nrow(a[smp_valid & availability_valid])
    ),
    selected_earliest_valid_tuple = c(nrow(v$selected), nrow(v$selected))
  )
  rbindlist(lapply(names(stages), function(stage_name) {
    data.table(
      variant = variant_name, stage = stage_name,
      n_patients = stages[[stage_name]][[1L]],
      n_anchors = stages[[stage_name]][[2L]]
    )
  }))
}
funnel <- rbindlist(Map(funnel_for_variant, variants, names(variants)))
fwrite(funnel, file.path(qc_out, "paired_exposure_funnel.csv"))

invalid_flags <- c(
  "invalid_missing_ppeak", "invalid_missing_peep", "invalid_missing_vt",
  "invalid_missing_rr", "invalid_peak_below_plateau",
  "invalid_plateau_below_peep", "invalid_delta_range", "invalid_smp_range",
  "invalid_late_availability"
)
invalid_reasons <- rbindlist(lapply(names(variants), function(nm) {
  a <- variants[[nm]]$all_anchors
  rbindlist(lapply(invalid_flags, function(flag) {
    data.table(
      variant = nm, reason = sub("^invalid_", "", flag),
      n_anchors = sum(a[[flag]], na.rm = TRUE),
      n_patients = uniqueN(a[get(flag) == TRUE]$stay_id)
    )
  }))
}))
fwrite(invalid_reasons, file.path(qc_out, "anchor_invalid_reasons.csv"))

source_distribution <- rbindlist(lapply(names(variants), function(nm) {
  s <- variants[[nm]]$selected
  rbindlist(lapply(c("pplat", "ppeak", "peep", "vt", "rr"), function(comp) {
    source_col <- if (comp == "pplat") "pplat_source" else paste0(comp, "_source")
    item_col <- if (comp == "pplat") "pplat_itemid" else paste0(comp, "_itemid")
    z <- s[, .N, by = .(
      source = get(source_col), itemid = get(item_col)
    )]
    z[, `:=`(
      variant = nm, component = comp, denominator_selected = nrow(s),
      proportion = if (nrow(s)) N / nrow(s) else NA_real_
    )]
    z
  }), fill = TRUE)
}), fill = TRUE)
setcolorder(source_distribution, c(
  "variant", "component", "itemid", "source", "N",
  "denominator_selected", "proportion"
))
fwrite(
  source_distribution,
  file.path(qc_out, "selected_component_source_distribution.csv")
)

quantile_safe <- function(x, probs) {
  x <- x[is.finite(x)]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  as.numeric(quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 2))
}

gap_distribution <- rbindlist(lapply(names(variants), function(nm) {
  s <- variants[[nm]]$selected
  rbindlist(lapply(c("ppeak", "peep", "vt", "rr"), function(comp) {
    signed <- s[[paste0(comp, "_signed_gap")]]
    absolute <- s[[paste0(comp, "_abs_gap")]]
    aq <- quantile_safe(absolute, c(0, .25, .5, .75, .9, .95, 1))
    sq <- quantile_safe(signed, c(.05, .25, .5, .75, .95))
    data.table(
      variant = nm, component = comp, n = sum(!is.na(absolute)),
      abs_min = aq[1L], abs_q25 = aq[2L], abs_median = aq[3L],
      abs_q75 = aq[4L], abs_q90 = aq[5L], abs_q95 = aq[6L], abs_max = aq[7L],
      signed_q05 = sq[1L], signed_q25 = sq[2L], signed_median = sq[3L],
      signed_q75 = sq[4L], signed_q95 = sq[5L],
      prior_n = sum(signed < 0, na.rm = TRUE),
      same_time_n = sum(signed == 0, na.rm = TRUE),
      future_n = sum(signed > 0, na.rm = TRUE)
    )
  }))
}))
fwrite(gap_distribution, file.path(qc_out, "selected_pairing_gap_distribution.csv"))

tuple_count_distribution <- rbindlist(lapply(names(artifacts), function(nm) {
  x <- artifacts[[nm]]$n_valid_tuples
  q <- quantile_safe(x, c(0, .25, .5, .75, .9, .95, 1))
  data.table(
    variant = nm, strict_cohort_n = length(x), zero_valid_n = sum(x == 0L),
    min = q[1L], q25 = q[2L], median = q[3L], q75 = q[4L],
    q90 = q[5L], q95 = q[6L], max = q[7L], mean = mean(x)
  )
}))
fwrite(tuple_count_distribution, file.path(qc_out, "valid_tuple_count_distribution.csv"))

distribution_row <- function(variant, metric, x) {
  x <- as.numeric(x)
  q <- quantile_safe(x, c(0, .05, .25, .5, .75, .95, 1))
  data.table(
    variant, metric, n = sum(is.finite(x)), min = q[1L], q05 = q[2L],
    q25 = q[3L], median = q[4L], q75 = q[5L], q95 = q[6L], max = q[7L],
    mean = if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_,
    sd = if (sum(is.finite(x)) > 1L) sd(x, na.rm = TRUE) else NA_real_
  )
}

prediction_distribution <- rbindlist(lapply(names(variants), function(nm) {
  s <- variants[[nm]]$selected
  rbindlist(list(
    distribution_row(nm, "anchor_minus_index_minutes", as.numeric(difftime(
      s$anchor_time, s$index_time, units = "mins"
    ))),
    distribution_row(nm, "prediction_minus_index_minutes", as.numeric(difftime(
      s$prediction_time, s$index_time, units = "mins"
    ))),
    distribution_row(
      nm, "availability_minus_last_measurement_minutes",
      as.numeric(difftime(
        s$prediction_time, s$tuple_last_measurement_time, units = "mins"
      ))
    )
  ))
}))
fwrite(prediction_distribution, file.path(qc_out, "prediction_time_distribution.csv"))

value_distribution <- rbindlist(lapply(names(variants), function(nm) {
  s <- variants[[nm]]$selected
  value_map <- list(
    plateau_cmH2O = s$pplat, peak_cmH2O = s$ppeak_value,
    peep_cmH2O = s$peep_value, tidal_volume_mL = s$vt_value,
    respiratory_rate_per_min = s$rr_value, driving_pressure_cmH2O = s$delta_p,
    resistive_pressure_cmH2O = s$resistive_pressure,
    surrogate_mp_J_per_min = s$smp
  )
  rbindlist(lapply(names(value_map), function(metric) {
    distribution_row(nm, metric, value_map[[metric]])
  }))
}))
fwrite(
  value_distribution,
  file.path(qc_out, "selected_component_value_distribution.csv")
)

boundary_qc <- rbindlist(lapply(names(variants), function(nm) {
  s <- variants[[nm]]$selected
  data.table(
    variant = nm,
    variable = c("pplat", "ppeak", "peep", "vt", "rr", "delta_p", "smp"),
    at_lower_boundary = c(
      sum(s$pplat == ranges$plateau_cmH2O[1L]),
      sum(s$ppeak_value == ranges$peak_cmH2O[1L]),
      sum(s$peep_value == ranges$peep_cmH2O[1L]),
      sum(s$vt_value == ranges$tidal_volume_mL[1L]),
      sum(s$rr_value == ranges$respiratory_rate_per_min[1L]),
      sum(s$delta_p == ranges$driving_pressure_cmH2O[1L]),
      sum(s$smp == ranges$surrogate_mp_J_per_min[1L])
    ),
    at_upper_boundary = c(
      sum(s$pplat == ranges$plateau_cmH2O[2L]),
      sum(s$ppeak_value == ranges$peak_cmH2O[2L]),
      sum(s$peep_value == ranges$peep_cmH2O[2L]),
      sum(s$vt_value == ranges$tidal_volume_mL[2L]),
      sum(s$rr_value == ranges$respiratory_rate_per_min[2L]),
      sum(s$delta_p == ranges$driving_pressure_cmH2O[2L]),
      sum(s$smp == ranges$surrogate_mp_J_per_min[2L])
    )
  )
}))
fwrite(boundary_qc, file.path(qc_out, "selected_boundary_value_QC.csv"))

observation_bound_qc <- data.table(
  strict_cohort_n = nrow(index),
  complete_protocol_window_n = sum(index$outtime >= index$protocol_exposure_end),
  icu_end_before_protocol_end_n = sum(index$outtime < index$protocol_exposure_end),
  minimum_observable_minutes = min(index$observable_window_minutes),
  median_observable_minutes = median(index$observable_window_minutes),
  maximum_observable_minutes = max(index$observable_window_minutes)
)
fwrite(observation_bound_qc, file.path(qc_out, "observation_bound_QC.csv"))

# Selection audit uses only variables known by index. No post-index ventilator
# value (other than the binary observation indicator) enters these summaries.
selection <- copy(index)
selection <- artifacts$primary_60min[, .(
  stay_id, tuple_observed
)][selection, on = "stay_id"]
selection[, selection_group := fifelse(
  tuple_observed, "tuple_observed", "tuple_missing"
)]

continuous_selection_vars <- intersect(c(
  "age_at_admission", "index_from_icu_hours", "pao2", "fio2_near_value",
  "peep_near_value", "pf_ratio", "fio2_abs_gap_min", "peep_abs_gap_min",
  "infection_gap_h"
), names(selection))

selection_continuous <- rbindlist(lapply(continuous_selection_vars, function(v) {
  by_group <- selection[, {
    z <- as.numeric(get(v))
    q <- quantile_safe(z, c(.25, .5, .75))
    .(
      variable = v, n = sum(is.finite(z)), missing_n = sum(!is.finite(z)),
      mean = if (any(is.finite(z))) mean(z, na.rm = TRUE) else NA_real_,
      sd = if (sum(is.finite(z)) > 1L) sd(z, na.rm = TRUE) else NA_real_,
      q25 = q[1L], median = q[2L], q75 = q[3L]
    )
  }, by = selection_group]
  x1 <- as.numeric(selection[tuple_observed == TRUE][[v]])
  x0 <- as.numeric(selection[tuple_observed == FALSE][[v]])
  pooled_sd <- sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE)) / 2)
  smd <- if (is.finite(pooled_sd) && pooled_sd > 0) {
    (mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) / pooled_sd
  } else {
    NA_real_
  }
  by_group[, standardized_mean_difference_observed_minus_missing := smd]
  by_group
}))
fwrite(selection_continuous, file.path(qc_out, "selection_audit_index_continuous.csv"))

categorical_selection_vars <- intersect(c(
  "gender", "first_careunit", "admission_type", "pao2_source",
  "fio2_near_source", "peep_near_source", "invasive_evidence_type",
  "infection_direction", "infection_culture_time_precision"
), names(selection))
selection_categorical <- rbindlist(lapply(categorical_selection_vars, function(v) {
  z <- selection[, .N, by = .(
    selection_group, level = fifelse(
      is.na(get(v)) | !nzchar(as.character(get(v))),
      "<missing>", as.character(get(v))
    )
  )]
  z[, denominator := sum(N), by = selection_group]
  z[, `:=`(proportion = N / denominator, variable = v)]
  z[, .(variable, selection_group, level, N, denominator, proportion)]
}))
fwrite(selection_categorical, file.path(qc_out, "selection_audit_index_categorical.csv"))

fwrite(invariants, file.path(qc_out, "paired_exposure_invariant_tests.csv"))

# Final leakage audit checks headers only; QC contents contain aggregate counts.
qc_csv <- list.files(qc_out, pattern = "\\.csv$", full.names = TRUE)
qc_headers <- rbindlist(lapply(qc_csv, function(f) {
  data.table(file = basename(f), column = names(fread(f, nrows = 0L)))
}))
identifier_columns <- c(
  "subject_id", "hadm_id", "stay_id", "caregiver_id", "anchor_id"
)
private_objects <- c(artifacts, list(all_valid = primary_valid_private))
leakage_guard <- data.table(
  check = c(
    "strict_source_had_no_outcome_like_columns",
    "working_index_has_no_outcome_like_columns",
    "private_artifacts_have_no_outcome_like_columns",
    "aggregate_qc_headers_have_no_outcome_like_columns",
    "aggregate_qc_contains_no_identifier_columns",
    "raw_extraction_target_stays_only",
    "raw_extraction_locked_items_only"
  ),
  pass = c(
    TRUE,
    !any(grepl(forbidden_pattern, names(index), ignore.case = TRUE)),
    all(vapply(private_objects, function(x) {
      !any(grepl(forbidden_pattern, names(x), ignore.case = TRUE))
    }, logical(1L))),
    !any(grepl(forbidden_pattern, qc_headers$column, ignore.case = TRUE)),
    !any(qc_headers$column %chin% identifier_columns),
    all(resp$stay_id %in% index$stay_id),
    all(resp$itemid %in% item_map$itemid)
  )
)
if (any(!leakage_guard$pass)) stop("Final exposure leakage guard failed.")
fwrite(leakage_guard, file.path(qc_out, "outcome_leakage_guard.csv"))

run_parameters <- data.table(
  parameter = c(
    "locked_config_version", "exposure_window_hours", "primary_pair_window_min",
    "sensitivity_pair_window_min", "availability_rule", "observable_end_rule",
    "tuple_selection", "outcome_blind"
  ),
  value = c(
    LOCKED$version,
    as.character(LOCKED$primary_exposure_window_hours_after_index),
    as.character(LOCKED$primary_ventilator_tuple_pair_window_minutes),
    as.character(LOCKED$sensitivity_ventilator_tuple_pair_window_minutes),
    "pmax(charttime,storetime); missing storetime->charttime",
    "min(index+6h,ICU outtime)",
    "earliest physiologically valid complete plateau-anchored tuple",
    "TRUE"
  )
)
fwrite(run_parameters, file.path(qc_out, "run_parameters.csv"))

primary_n <- nrow(variants$primary_60min$selected)
sens30_n <- nrow(variants$sensitivity_30min$selected)
preferred_n <- nrow(variants$sensitivity_preferred_60min$selected)
summary_lines <- c(
  "# MIMIC-IV paired ventilator exposure QC",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("- Locked configuration: ", LOCKED$version),
  paste0("- Strict index cohort: ", nrow(index), " patients/stays"),
  paste0("- Primary ±60-minute first valid tuples: ", primary_n),
  paste0("- Sensitivity ±30-minute first valid tuples: ", sens30_n),
  paste0("- Preferred-source-only ±60-minute first valid tuples: ", preferred_n),
  "- Every component measurement and documentation-availability time is restricted to index through min(index+6 h, ICU outtime).",
  "- Documentation availability is max(charttime, storetime); missing storetime falls back to charttime.",
  "- Pairing hierarchy is source preference, nearest absolute gap, then prior on an exact prior/future tie.",
  "- Same-stay/time/source numeric duplicates are reduced to their median.",
  "- No mortality, death, hospital-expiry, survival, effect, or performance field was read or summarized.",
  "- Row-level outputs are confined to analysis_rebuild_v1/private/mimic.",
  "",
  "Detailed aggregate label, range, source, gap, invalid-reason, availability, selection, invariant, and leakage checks are stored in this directory."
)
writeLines(
  summary_lines,
  file.path(qc_out, "mimic_paired_exposure_QC.md"),
  useBytes = TRUE
)

# Publish an atomic downstream gate only after every private product and every
# required aggregate QC artifact exists and all invariant/leakage checks pass.
formal_rds <- c(
  primary_60min = file.path(
    private_out, "mimic_paired_exposure_primary_60min_v1.rds"
  ),
  sensitivity_30min = file.path(
    private_out, "mimic_paired_exposure_sensitivity_30min_v1.rds"
  ),
  sensitivity_preferred_60min = file.path(
    private_out,
    "mimic_paired_exposure_sensitivity_preferred_60min_v1.rds"
  ),
  all_valid_primary_60min = file.path(
    private_out,
    "mimic_paired_exposure_all_valid_primary_60min_v1.rds"
  )
)
required_qc <- file.path(qc_out, c(
  "locked_item_metadata_QC.csv", "raw_component_item_QC.csv",
  "exposure_window_component_item_QC.csv", "locked_source_hierarchy_QC.csv",
  "late_entry_exclusion_QC.csv", "same_time_source_duplicate_QC.csv",
  "paired_exposure_funnel.csv", "anchor_invalid_reasons.csv",
  "selected_component_source_distribution.csv",
  "selected_pairing_gap_distribution.csv", "valid_tuple_count_distribution.csv",
  "prediction_time_distribution.csv", "selected_component_value_distribution.csv",
  "selected_boundary_value_QC.csv", "observation_bound_QC.csv",
  "selection_audit_index_continuous.csv",
  "selection_audit_index_categorical.csv",
  "paired_exposure_invariant_tests.csv", "outcome_leakage_guard.csv",
  "run_parameters.csv", "mimic_paired_exposure_QC.md"
))
if (!all(file.exists(formal_rds))) {
  stop("One or more formal MIMIC exposure RDS products are missing.")
}
if (!all(file.exists(required_qc))) {
  stop("One or more required MIMIC exposure QC products are missing.")
}
formal_rds_sha256 <- vapply(
  formal_rds,
  function(path) digest::digest(file = path, algo = "sha256"),
  character(1L)
)
completed_at <- Sys.time()
completion_gate <- data.table(
  field = c(
    "locked_config_version", "script_sha256", "phase1_gate_sha256",
    "input_primary_cohort_sha256", "completed_at", "all_invariants_pass",
    "outcome_leakage_guard_pass", "all_required_qc_present",
    "strict_cohort_n", "primary_60min_n", "sensitivity_30min_n",
    "sensitivity_preferred_60min_n",
    paste0(names(formal_rds), "_rds_sha256")
  ),
  value = as.character(c(
    LOCKED$version, script_sha256, phase1_gate_sha256,
    input_cohort_sha256, format(completed_at, "%Y-%m-%d %H:%M:%S %z"),
    all(invariants$pass), all(leakage_guard$pass), all(file.exists(required_qc)),
    nrow(index), primary_n, sens30_n, preferred_n, formal_rds_sha256
  ))
)
fwrite(completion_gate, phase2_complete_tmp)
if (!file.rename(phase2_complete_tmp, phase2_complete)) {
  stop("Could not atomically publish MIMIC Phase-2 completion gate.")
}

message("MIMIC paired exposure build complete (outcome-blind).")
message("  BUILD_COMPLETE | script SHA256 ", script_sha256)
message("  strict cohort: ", nrow(index))
message("  primary 60-min tuple: ", primary_n)
message("  sensitivity 30-min tuple: ", sens30_n)
message("  preferred-only 60-min tuple: ", preferred_n)
message("  private outputs: ", private_out)
message("  aggregate QC: ", qc_out)
