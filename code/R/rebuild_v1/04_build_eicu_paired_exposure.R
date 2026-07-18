#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: eICU paired ventilator exposure
#
# Outcome-blind Phase 2 extraction. This script deliberately projects the
# strict index cohort onto an allow-list that excludes discharge and mortality
# fields before any joins or summaries are performed.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("code/R/rebuild_v1/04_build_eicu_paired_exposure.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "00_config.R"))

stopifnot(
  identical(LOCKED$primary_exposure_summary, "first_valid_complete_tuple"),
  LOCKED$primary_exposure_window_hours_after_index == 6,
  LOCKED$primary_ventilator_tuple_pair_window_minutes == 60,
  LOCKED$sensitivity_ventilator_tuple_pair_window_minutes == 30
)

input_cohort <- file.path(PRIVATE_ROOT, "eicu", "eicu_index_cohort_v1.rds")
raw_resp <- file.path(EICU_ROOT, "respiratoryCharting.csv.gz")
private_out <- file.path(PRIVATE_ROOT, "eicu")
qc_out <- file.path(QC_ROOT, "eicu_exposure")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required for eICU completion-gate verification.")
}
phase1_script <- file.path(
  dirname(script_path), "02_build_eicu_index_cohort.R"
)
phase1_complete <- file.path(QC_ROOT, "eicu", "phase1_eicu_complete_v1.csv")
phase2_complete <- file.path(qc_out, "phase2_eicu_exposure_complete_v1.csv")
phase2_complete_tmp <- paste0(phase2_complete, ".tmp")
# An interrupted rerun is invalid by construction.
unlink(c(phase2_complete, phase2_complete_tmp), force = TRUE)

stopifnot(
  file.exists(input_cohort), file.exists(raw_resp), file.exists(phase1_script),
  file.exists(phase1_complete)
)

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

phase1_gate <- read_gate_map(phase1_complete)
require_gate_value(phase1_gate, "locked_config_version", LOCKED$version)
require_gate_value(phase1_gate, "all_invariants_pass", "TRUE")
require_gate_value(phase1_gate, "outcome_leakage_guard_pass", "TRUE")
require_gate_value(phase1_gate, "all_required_qc_present", "TRUE")
require_gate_value(
  phase1_gate, "script_sha256",
  digest::digest(file = phase1_script, algo = "sha256")
)
input_cohort_sha256 <- digest::digest(file = input_cohort, algo = "sha256")
require_gate_value(
  phase1_gate, "primary_cohort_rds_sha256", input_cohort_sha256
)
phase1_gate_sha256 <- digest::digest(file = phase1_complete, algo = "sha256")
script_sha256 <- digest::digest(file = script_path, algo = "sha256")

# ---------------------------------------------------------------------------
# Leakage guard: outcomes are present in the source cohort for later phases,
# but are never selected into this outcome-blind exposure build.
# ---------------------------------------------------------------------------

forbidden_pattern <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)

cohort_source <- readRDS(input_cohort)
required_index <- c(
  "patientunitstayid", "patienthealthsystemstayid", "person_key", "uniquepid",
  "hospitalid", "age_num", "gender", "pao2_time", "pao2",
  "fio2_near_value", "peep_near_value", "pf_ratio", "icu_end_offset",
  "invasive_evidence_type", "infection_source"
)
missing_index <- setdiff(required_index, names(cohort_source))
if (length(missing_index)) {
  stop("Strict eICU cohort is missing required fields: ", paste(missing_index, collapse = ", "))
}

index <- as.data.table(cohort_source)[, ..required_index]
rm(cohort_source)
gc(verbose = FALSE)

if (any(grepl(forbidden_pattern, names(index), ignore.case = TRUE))) {
  stop("Leakage guard failed: a forbidden outcome-like field entered the working cohort.")
}
if (anyDuplicated(index$patientunitstayid)) {
  stop("Strict index cohort must contain one row per patientunitstayid.")
}
if (anyNA(index$patientunitstayid) || anyNA(index$pao2_time)) {
  stop("Strict index cohort has a missing stay identifier or index time.")
}

setnames(
  index,
  c("pao2_time", "fio2_near_value", "peep_near_value"),
  c("index_time", "index_fio2", "index_peep")
)
index[, protocol_exposure_end := index_time +
        60 * LOCKED$primary_exposure_window_hours_after_index]
index[, observable_exposure_end := pmin(protocol_exposure_end, icu_end_offset, na.rm = TRUE)]
if (any(!is.finite(index$observable_exposure_end))) {
  stop("Non-finite observable exposure endpoint.")
}

# ---------------------------------------------------------------------------
# Read only locked labels and only strict-cohort stays from the large gzipped
# source. The first five eICU respiratoryCharting columns are comma-free
# numeric/category fields; the locked label is column six. fread subsequently
# validates that no unexpected label was admitted.
# ---------------------------------------------------------------------------

locked_labels <- c(
  "Plateau Pressure",
  "Peak Insp. Pressure",
  "PEEP",
  "PEEP/CPAP",
  "Tidal Volume Observed (VT)",
  "Exhaled TV (patient)",
  "Exhaled TV (machine)",
  "Tidal Volume (set)",
  "Total RR",
  "Vent Rate"
)

id_file <- tempfile("eicu_strict_stays_", fileext = ".txt")
on.exit(unlink(id_file), add = TRUE)
fwrite(index[, .(patientunitstayid)], id_file, col.names = FALSE)

awk_tests <- paste(sprintf("$6==\"%s\"", locked_labels), collapse = " || ")
awk_program <- paste0(
  "NR==FNR { keep[$1]=1; next } ",
  "FNR==1 || (($2 in keep) && (", awk_tests, "))"
)
read_cmd <- sprintf(
  "gzip -cd %s | awk -F',' %s %s -",
  shQuote(raw_resp), shQuote(awk_program), shQuote(id_file)
)

message("Reading locked eICU ventilator labels for ", nrow(index), " strict-cohort stays ...")
resp <- fread(
  cmd = read_cmd,
  select = c(
    "respchartid", "patientunitstayid", "respchartoffset",
    "respchartentryoffset", "respchartvaluelabel", "respchartvalue"
  ),
  showProgress = interactive()
)

if (!nrow(resp)) stop("No locked ventilator observations were read.")
unexpected_labels <- setdiff(unique(resp$respchartvaluelabel), locked_labels)
if (length(unexpected_labels)) {
  stop("Unexpected labels passed the locked filter: ", paste(unexpected_labels, collapse = ", "))
}
if (any(!resp$patientunitstayid %in% index$patientunitstayid)) {
  stop("Raw extraction admitted a stay outside the strict cohort.")
}

# Numeric parsing is intentionally strict: units or inequality symbols are not
# guessed. Scientific notation is accepted; blank and non-numeric text are NA.
strict_numeric <- function(x) {
  z <- trimws(as.character(x))
  ok <- grepl("^[+-]?((([0-9]+)(\\.[0-9]*)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)?$", z)
  out <- rep(NA_real_, length(z))
  out[ok] <- suppressWarnings(as.numeric(z[ok]))
  out[!is.finite(out)] <- NA_real_
  out
}

resp[, value_num := strict_numeric(respchartvalue)]
resp[, entry_missing := is.na(respchartentryoffset)]
resp[, available_time := fifelse(
  is.na(respchartentryoffset),
  as.numeric(respchartoffset),
  pmax(as.numeric(respchartoffset), as.numeric(respchartentryoffset))
)]

resp[, component := fcase(
  respchartvaluelabel == "Plateau Pressure", "pplat",
  respchartvaluelabel == "Peak Insp. Pressure", "ppeak",
  respchartvaluelabel %chin% c("PEEP", "PEEP/CPAP"), "peep",
  respchartvaluelabel %chin% c(
    "Tidal Volume Observed (VT)", "Exhaled TV (patient)",
    "Exhaled TV (machine)", "Tidal Volume (set)"
  ), "vt",
  respchartvaluelabel %chin% c("Total RR", "Vent Rate"), "rr",
  default = NA_character_
)]
resp[, source_rank := fcase(
  respchartvaluelabel %chin% c(
    "Plateau Pressure", "Peak Insp. Pressure", "PEEP",
    "Tidal Volume Observed (VT)", "Exhaled TV (patient)",
    "Exhaled TV (machine)", "Total RR"
  ), 1L,
  respchartvaluelabel %chin% c("PEEP/CPAP", "Tidal Volume (set)", "Vent Rate"), 2L,
  default = NA_integer_
)]

ranges <- LOCKED$physiologic_ranges
range_lookup <- data.table(
  component = c("pplat", "ppeak", "peep", "vt", "rr"),
  lower = c(
    ranges$plateau_cmH2O[[1L]], ranges$peak_cmH2O[[1L]],
    ranges$peep_cmH2O[[1L]], ranges$tidal_volume_mL[[1L]],
    ranges$respiratory_rate_per_min[[1L]]
  ),
  upper = c(
    ranges$plateau_cmH2O[[2L]], ranges$peak_cmH2O[[2L]],
    ranges$peep_cmH2O[[2L]], ranges$tidal_volume_mL[[2L]],
    ranges$respiratory_rate_per_min[[2L]]
  )
)
resp <- range_lookup[resp, on = "component"]
resp[, in_component_range := !is.na(value_num) & value_num >= lower & value_num <= upper]

# Duplicate numeric observations sharing a stay, timestamp, and source label
# are reduced to a median. The median is available only after the last numeric
# duplicate has been entered.
obs <- resp[, .(
  raw_n = .N,
  numeric_n = sum(!is.na(value_num)),
  nonnumeric_n = sum(is.na(value_num)),
  value = if (any(!is.na(value_num))) median(value_num, na.rm = TRUE) else NA_real_,
  available_time = if (any(!is.na(value_num))) {
    max(available_time[!is.na(value_num)], na.rm = TRUE)
  } else {
    NA_real_
  },
  any_entry_fallback = any(entry_missing & !is.na(value_num))
), by = .(
  patientunitstayid, component, source = respchartvaluelabel,
  source_rank, measurement_time = respchartoffset, lower, upper
)]
obs[, in_component_range := !is.na(value) & value >= lower & value <= upper]

# Attach index bounds without ever bringing outcome fields into scope.
obs <- index[, .(
  patientunitstayid, index_time, protocol_exposure_end,
  observable_exposure_end
)][obs, on = "patientunitstayid"]
if (anyNA(obs$index_time)) stop("A ventilator observation failed to join to the strict cohort.")

# Retain all plateau records for funnel accounting. Valid anchors must be
# measured in the locked 0-6 h window and before ICU observation ends.
pplat_obs <- obs[component == "pplat"]
pplat_window_raw <- pplat_obs[
  measurement_time >= index_time & measurement_time <= observable_exposure_end
]
anchors_range_valid <- pplat_window_raw[in_component_range == TRUE]
anchors <- anchors_range_valid[available_time <= observable_exposure_end]
setorder(anchors, patientunitstayid, measurement_time, available_time, source)
anchors[, anchor_id := .I]
setnames(
  anchors,
  c("measurement_time", "available_time", "value", "source", "any_entry_fallback"),
  c("anchor_time", "anchor_available_time", "pplat", "pplat_source", "pplat_entry_fallback")
)
anchors <- anchors[, .(
  patientunitstayid, anchor_id, index_time, protocol_exposure_end,
  observable_exposure_end, anchor_time, anchor_available_time, pplat,
  pplat_source, pplat_entry_fallback
)]

# Every component must be measured in the locked index through index+6 h
# exposure window. Rows entered only after the observable exposure endpoint are
# excluded before hierarchy/proximity pairing so that a late preferred value
# cannot block a timely alternative from forming the first valid tuple.
max_pair_window <- LOCKED$primary_ventilator_tuple_pair_window_minutes
candidates <- obs[
  component != "pplat" &
    measurement_time >= index_time &
    measurement_time <= observable_exposure_end &
    in_component_range == TRUE &
    available_time <= observable_exposure_end
]

pair_one_component <- function(anchor_dt, candidate_dt, component_name, window_minutes,
                               preferred_only = FALSE) {
  a <- anchor_dt[, .(
    patientunitstayid, anchor_id, anchor_time,
    observable_exposure_end
  )]
  cdt <- candidate_dt[component == component_name]
  if (preferred_only && component_name %chin% c("peep", "vt", "rr")) {
    cdt <- cdt[source_rank == 1L]
  }
  if (!nrow(a) || !nrow(cdt)) {
    return(data.table(
      anchor_id = integer(),
      value = numeric(),
      time = numeric(),
      available_time = numeric(),
      source = character(),
      source_rank = integer(),
      signed_gap = numeric(),
      abs_gap = numeric(),
      entry_fallback = logical()
    ))
  }
  cdt <- cdt[, .(
    patientunitstayid,
    component_time = measurement_time,
    component_available_time = available_time,
    component_value = value,
    component_source = source,
    source_rank,
    component_entry_fallback = any_entry_fallback
  )]
  z <- merge(a, cdt, by = "patientunitstayid", allow.cartesian = TRUE)
  z[, signed_gap := component_time - anchor_time]
  z <- z[
    abs(signed_gap) <= window_minutes &
      component_time <= observable_exposure_end
  ]
  if (!nrow(z)) {
    return(data.table(
      anchor_id = integer(), value = numeric(), time = numeric(),
      available_time = numeric(), source = character(), source_rank = integer(),
      signed_gap = numeric(), abs_gap = numeric(), entry_fallback = logical()
    ))
  }
  z[, abs_gap := abs(signed_gap)]
  z[, future_tie := signed_gap > 0]
  # Hierarchy first, then nearest timestamp. Prior wins an exact +/- tie.
  setorder(
    z, anchor_id, source_rank, abs_gap, future_tie,
    component_time, component_available_time, component_source
  )
  z <- z[, .SD[1L], by = anchor_id]
  z[, .(
    anchor_id,
    value = component_value,
    time = component_time,
    available_time = component_available_time,
    source = component_source,
    source_rank,
    signed_gap,
    abs_gap,
    entry_fallback = component_entry_fallback
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
  t[, pressure_order_valid :=
      complete_components & ppeak_value >= pplat & pplat >= peep_value]
  t[, delta_p := fifelse(complete_components, pplat - peep_value, NA_real_)]
  t[, resistive_pressure := fifelse(
    complete_components, ppeak_value - pplat, NA_real_
  )]
  t[, delta_valid :=
      pressure_order_valid &
      delta_p >= ranges$driving_pressure_cmH2O[[1L]] &
      delta_p <= ranges$driving_pressure_cmH2O[[2L]]]
  t[, smp := fifelse(
    complete_components,
    0.098 * rr_value * (vt_value / 1000) *
      (ppeak_value - 0.5 * (pplat - peep_value)),
    NA_real_
  )]
  t[, smp_valid :=
      delta_valid &
      smp >= ranges$surrogate_mp_J_per_min[[1L]] &
      smp <= ranges$surrogate_mp_J_per_min[[2L]]]

  available_cols <- c(
    "anchor_available_time", "ppeak_available_time", "peep_available_time",
    "vt_available_time", "rr_available_time"
  )
  measurement_cols <- c(
    "anchor_time", "ppeak_time", "peep_time", "vt_time", "rr_time"
  )
  t[, prediction_time := if (complete_components) {
    max(unlist(.SD), na.rm = FALSE)
  } else {
    NA_real_
  }, by = anchor_id, .SDcols = available_cols]
  t[, tuple_last_measurement_time := if (complete_components) {
    max(unlist(.SD), na.rm = FALSE)
  } else {
    NA_real_
  }, by = anchor_id, .SDcols = measurement_cols]
  t[, availability_valid :=
      complete_components & !is.na(prediction_time) &
      prediction_time >= anchor_time &
      prediction_time <= observable_exposure_end]
  t[, valid_tuple := smp_valid & availability_valid]

  t[, invalid_missing_ppeak := is.na(ppeak_value)]
  t[, invalid_missing_peep := is.na(peep_value)]
  t[, invalid_missing_vt := is.na(vt_value)]
  t[, invalid_missing_rr := is.na(rr_value)]
  t[, invalid_peak_below_plateau :=
      complete_components & ppeak_value < pplat]
  t[, invalid_plateau_below_peep :=
      complete_components & pplat < peep_value]
  t[, invalid_delta_range := pressure_order_valid & !delta_valid]
  t[, invalid_smp_range := delta_valid & !smp_valid]
  t[, invalid_late_availability :=
      complete_components & !availability_valid]
  t[, variant := variant_name]

  setorder(t, patientunitstayid, anchor_time, anchor_available_time, anchor_id)
  valid <- t[valid_tuple == TRUE]
  selected <- valid[, .SD[1L], by = patientunitstayid]
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
# Invariants, including hierarchy and exact formula checks.
# ---------------------------------------------------------------------------

check_variant <- function(v, window_minutes, preferred_only) {
  a <- v$all_anchors
  s <- v$selected
  checks <- list(
    unique_anchor_id = !anyDuplicated(a$anchor_id),
    one_selected_per_stay = !anyDuplicated(s$patientunitstayid),
    selected_is_valid = all(s$valid_tuple),
    anchor_in_window = all(
      s$anchor_time >= s$index_time &
        s$anchor_time <= s$observable_exposure_end
    ),
    pairing_window = all(
      s$ppeak_abs_gap <= window_minutes & s$peep_abs_gap <= window_minutes &
        s$vt_abs_gap <= window_minutes & s$rr_abs_gap <= window_minutes
    ),
    all_component_measurements_within_exposure_window = all(
      s$ppeak_time >= s$index_time & s$ppeak_time <= s$observable_exposure_end &
        s$peep_time >= s$index_time & s$peep_time <= s$observable_exposure_end &
        s$vt_time >= s$index_time & s$vt_time <= s$observable_exposure_end &
        s$rr_time >= s$index_time & s$rr_time <= s$observable_exposure_end
    ),
    all_component_entries_within_exposure_window = all(
      s$anchor_available_time <= s$observable_exposure_end &
        s$ppeak_available_time <= s$observable_exposure_end &
        s$peep_available_time <= s$observable_exposure_end &
        s$vt_available_time <= s$observable_exposure_end &
        s$rr_available_time <= s$observable_exposure_end
    ),
    pressure_order = all(s$ppeak_value >= s$pplat & s$pplat >= s$peep_value),
    prediction_not_before_anchor = all(s$prediction_time >= s$anchor_time),
    prediction_by_observable_end = all(
      s$prediction_time <= s$observable_exposure_end
    ),
    delta_formula = all(abs(s$delta_p - (s$pplat - s$peep_value)) < 1e-10),
    resistive_formula = all(
      abs(s$resistive_pressure - (s$ppeak_value - s$pplat)) < 1e-10
    ),
    smp_formula = all(abs(
      s$smp - 0.098 * s$rr_value * (s$vt_value / 1000) *
        (s$ppeak_value - 0.5 * (s$pplat - s$peep_value))
    ) < 1e-10),
    earliest_valid_anchor = if (!nrow(s)) {
      TRUE
    } else {
      earliest <- v$valid[, .(earliest_anchor_time = min(anchor_time)),
                          by = patientunitstayid]
      chk <- earliest[s[, .(patientunitstayid, anchor_time)],
                      on = "patientunitstayid"]
      all(chk$anchor_time == chk$earliest_anchor_time)
    }
  )
  if (preferred_only) {
    checks$preferred_peep_only <- all(s$peep_source == "PEEP")
    checks$preferred_vt_only <- all(s$vt_source %chin% c(
      "Tidal Volume Observed (VT)", "Exhaled TV (patient)",
      "Exhaled TV (machine)"
    ))
    checks$preferred_rr_only <- all(s$rr_source == "Total RR")
  } else {
    # A fallback is legal only when no preferred source is present within the
    # pairing window for that same anchor.
    fallback_ok <- function(component_name) {
      fb <- a[
        get(paste0(component_name, "_source_rank")) > 1L & valid_tuple == TRUE
      ]
      if (!nrow(fb)) return(TRUE)
      pref <- candidates[
        component == component_name & source_rank == 1L,
        .(patientunitstayid, preferred_time = measurement_time)
      ]
      z <- merge(
        fb[, .(patientunitstayid, anchor_id, anchor_time)],
        pref,
        by = "patientunitstayid",
        allow.cartesian = TRUE
      )
      if (!nrow(z)) return(TRUE)
      !any(abs(z$preferred_time - z$anchor_time) <= window_minutes)
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
    paste(invariants[pass == FALSE, paste(variant, check, sep = ":")], collapse = ", ")
  )
}

# ---------------------------------------------------------------------------
# Row-level, outcome-free private artifacts.
# ---------------------------------------------------------------------------

tuple_fields <- c(
  "patientunitstayid", "anchor_id", "anchor_time", "anchor_available_time",
  "pplat", "pplat_source", "pplat_entry_fallback",
  "ppeak_value", "ppeak_time", "ppeak_available_time", "ppeak_source",
  "ppeak_source_rank", "ppeak_signed_gap", "ppeak_abs_gap", "ppeak_entry_fallback",
  "peep_value", "peep_time", "peep_available_time", "peep_source",
  "peep_source_rank", "peep_signed_gap", "peep_abs_gap", "peep_entry_fallback",
  "vt_value", "vt_time", "vt_available_time", "vt_source", "vt_source_rank",
  "vt_signed_gap", "vt_abs_gap", "vt_entry_fallback",
  "rr_value", "rr_time", "rr_available_time", "rr_source", "rr_source_rank",
  "rr_signed_gap", "rr_abs_gap", "rr_entry_fallback",
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
    ), by = patientunitstayid]
  } else {
    data.table(
      patientunitstayid = integer(), n_valid_tuples = integer(),
      first_valid_anchor_time = numeric(), last_valid_anchor_time = numeric()
    )
  }
  out <- merge(index, counts, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  out <- merge(out, selected, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  out[, tuple_observed := !is.na(anchor_time)]
  out[is.na(n_valid_tuples), n_valid_tuples := 0L]
  if (nrow(out) != nrow(index) || anyDuplicated(out$patientunitstayid)) {
    stop("Private artifact is not one row per strict-cohort stay: ", artifact_name)
  }
  if (any(grepl(forbidden_pattern, names(out), ignore.case = TRUE))) {
    stop("Leakage guard failed in private artifact: ", artifact_name)
  }
  attr(out, "rebuild_metadata") <- list(
    version = "eicu_paired_exposure_v1",
    artifact = artifact_name,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    locked_config_version = LOCKED$version,
    script = script_path,
    outcome_blind = TRUE,
    tuple_anchor = "Plateau Pressure",
    selection = "earliest physiologically valid complete anchor",
    prediction_time = paste(
      "maximum of measurement/entry availability times; entry missing falls",
      "back to measurement time; must be no later than observable index+6h endpoint"
    )
  )
  saveRDS(out, file.path(private_out, artifact_name), compress = "xz")
  out
}

artifacts <- list(
  primary_60min = make_cohort_artifact(
    variants$primary_60min,
    "eicu_paired_exposure_primary_60min_v1.rds"
  ),
  sensitivity_30min = make_cohort_artifact(
    variants$sensitivity_30min,
    "eicu_paired_exposure_sensitivity_30min_v1.rds"
  ),
  sensitivity_preferred_60min = make_cohort_artifact(
    variants$sensitivity_preferred_60min,
    "eicu_paired_exposure_sensitivity_preferred_60min_v1.rds"
  )
)

primary_valid_private <- variants$primary_60min$valid[, ..tuple_fields]
if (any(grepl(forbidden_pattern, names(primary_valid_private), ignore.case = TRUE))) {
  stop("Leakage guard failed in all-valid-tuples artifact.")
}
attr(primary_valid_private, "rebuild_metadata") <- list(
  version = "eicu_paired_exposure_all_valid_primary_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED$version,
  outcome_blind = TRUE
)
saveRDS(
  primary_valid_private,
  file.path(private_out, "eicu_paired_exposure_all_valid_primary_60min_v1.rds"),
  compress = "xz"
)

# ---------------------------------------------------------------------------
# Aggregate-only QC. No identifier is written to the QC directory.
# ---------------------------------------------------------------------------

raw_qc <- resp[, .(
  raw_rows = .N,
  strict_stays_with_label = uniqueN(patientunitstayid),
  numeric_rows = sum(!is.na(value_num)),
  nonnumeric_rows = sum(is.na(value_num)),
  rows_in_locked_range = sum(in_component_range),
  rows_outside_locked_range = sum(!is.na(value_num) & !in_component_range),
  entry_offset_missing_rows = sum(entry_missing)
), by = .(component, source = respchartvaluelabel)]
setorder(raw_qc, component, source)
fwrite(raw_qc, file.path(qc_out, "raw_component_label_QC.csv"))

source_hierarchy_qc <- data.table(
  component = c(
    "pplat", "ppeak", "peep", "peep", "vt", "vt", "vt", "vt", "rr", "rr"
  ),
  source = c(
    "Plateau Pressure", "Peak Insp. Pressure", "PEEP", "PEEP/CPAP",
    "Tidal Volume Observed (VT)", "Exhaled TV (patient)",
    "Exhaled TV (machine)", "Tidal Volume (set)", "Total RR", "Vent Rate"
  ),
  source_rank = c(1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L),
  analytic_role = c(
    "anchor", "explicit", "preferred", "flagged_fallback",
    "preferred_observed", "preferred_exhaled", "preferred_exhaled",
    "flagged_fallback", "preferred_total", "flagged_fallback"
  ),
  same_rank_tie_break = paste(
    "nearest absolute measurement gap; prior wins an exact prior/future tie;",
    "then earlier availability time and lexical source label"
  )
)
fwrite(source_hierarchy_qc, file.path(qc_out, "locked_source_hierarchy_QC.csv"))

preferred_feasibility <- data.table(
  component = c("peep", "vt", "rr"),
  required_preferred_source = c(
    "PEEP", "observed/exhaled VT pool", "Total RR"
  ),
  raw_rows_in_strict_cohort = c(
    nrow(resp[respchartvaluelabel == "PEEP"]),
    nrow(resp[respchartvaluelabel %chin% c(
      "Tidal Volume Observed (VT)", "Exhaled TV (patient)",
      "Exhaled TV (machine)"
    )]),
    nrow(resp[respchartvaluelabel == "Total RR"])
  ),
  strict_stays_with_source = c(
    uniqueN(resp[respchartvaluelabel == "PEEP"]$patientunitstayid),
    uniqueN(resp[respchartvaluelabel %chin% c(
      "Tidal Volume Observed (VT)", "Exhaled TV (patient)",
      "Exhaled TV (machine)"
    )]$patientunitstayid),
    uniqueN(resp[respchartvaluelabel == "Total RR"]$patientunitstayid)
  )
)
preferred_feasibility[, source_available := raw_rows_in_strict_cohort > 0L]
preferred_feasibility[, complete_preferred_tuple_sensitivity_estimable :=
                        all(source_available)]
fwrite(
  preferred_feasibility,
  file.path(qc_out, "preferred_source_sensitivity_feasibility.csv")
)

late_entry_qc <- obs[
  measurement_time >= index_time &
    measurement_time <= observable_exposure_end &
    in_component_range == TRUE &
    available_time > observable_exposure_end,
  .(
    late_timestamp_source_groups = .N,
    strict_stays_with_late_entry = uniqueN(patientunitstayid),
    median_minutes_after_observable_end = median(
      available_time - observable_exposure_end
    ),
    maximum_minutes_after_observable_end = max(
      available_time - observable_exposure_end
    )
  ),
  by = .(component, source)
]
setorder(late_entry_qc, component, source)
fwrite(late_entry_qc, file.path(qc_out, "late_entry_exclusion_QC.csv"))

duplicate_qc <- obs[, .(
  timestamp_source_groups = .N,
  groups_with_duplicates = sum(raw_n > 1L),
  maximum_duplicate_count = max(raw_n),
  groups_with_entry_fallback = sum(any_entry_fallback)
), by = .(component, source)]
setorder(duplicate_qc, component, source)
fwrite(duplicate_qc, file.path(qc_out, "same_time_duplicate_QC.csv"))

funnel_for_variant <- function(v, variant_name) {
  a <- v$all_anchors
  stage <- list(
    strict_index_cohort = list(n_patients = nrow(index), n_anchors = NA_integer_),
    any_plateau_label_in_window = list(
      n_patients = uniqueN(pplat_window_raw$patientunitstayid),
      n_anchors = nrow(pplat_window_raw)
    ),
    valid_plateau_measurement = list(
      n_patients = uniqueN(anchors_range_valid$patientunitstayid),
      n_anchors = nrow(anchors_range_valid)
    ),
    plateau_available_by_observable_exposure_end = list(
      n_patients = uniqueN(anchors$patientunitstayid),
      n_anchors = nrow(anchors)
    ),
    complete_paired_components = list(
      n_patients = uniqueN(a[complete_components == TRUE]$patientunitstayid),
      n_anchors = nrow(a[complete_components == TRUE])
    ),
    valid_pressure_ordering = list(
      n_patients = uniqueN(a[pressure_order_valid == TRUE]$patientunitstayid),
      n_anchors = nrow(a[pressure_order_valid == TRUE])
    ),
    valid_driving_pressure = list(
      n_patients = uniqueN(a[delta_valid == TRUE]$patientunitstayid),
      n_anchors = nrow(a[delta_valid == TRUE])
    ),
    valid_surrogate_mechanical_power = list(
      n_patients = uniqueN(a[smp_valid == TRUE]$patientunitstayid),
      n_anchors = nrow(a[smp_valid == TRUE])
    ),
    available_by_observable_exposure_end = list(
      n_patients = uniqueN(a[smp_valid & availability_valid]$patientunitstayid),
      n_anchors = nrow(a[smp_valid & availability_valid])
    ),
    selected_earliest_valid_tuple = list(
      n_patients = nrow(v$selected), n_anchors = nrow(v$selected)
    )
  )
  rbindlist(lapply(names(stage), function(nm) {
    data.table(
      variant = variant_name, stage = nm,
      n_patients = stage[[nm]]$n_patients,
      n_anchors = stage[[nm]]$n_anchors
    )
  }))
}
funnel <- rbindlist(Map(
  funnel_for_variant,
  variants,
  names(variants)
))
fwrite(funnel, file.path(qc_out, "paired_exposure_funnel.csv"))

invalid_flag_names <- c(
  "invalid_missing_ppeak", "invalid_missing_peep", "invalid_missing_vt",
  "invalid_missing_rr", "invalid_peak_below_plateau",
  "invalid_plateau_below_peep", "invalid_delta_range",
  "invalid_smp_range", "invalid_late_availability"
)
invalid_reasons <- rbindlist(lapply(names(variants), function(nm) {
  a <- variants[[nm]]$all_anchors
  rbindlist(lapply(invalid_flag_names, function(flag) {
    data.table(
      variant = nm,
      reason = sub("^invalid_", "", flag),
      n_anchors = sum(a[[flag]], na.rm = TRUE),
      n_patients = uniqueN(a[get(flag) == TRUE]$patientunitstayid)
    )
  }))
}))
fwrite(invalid_reasons, file.path(qc_out, "anchor_invalid_reasons.csv"))

source_distribution <- rbindlist(lapply(names(variants), function(nm) {
  s <- variants[[nm]]$selected
  rbindlist(lapply(c("pplat", "ppeak", "peep", "vt", "rr"), function(comp) {
    source_col <- if (comp == "pplat") "pplat_source" else paste0(comp, "_source")
    s[, .N, by = .(source = get(source_col))][, `:=`(
      variant = nm,
      component = comp,
      denominator_selected = nrow(s),
      proportion = N / nrow(s)
    )]
  }), fill = TRUE)
}), fill = TRUE)
setcolorder(
  source_distribution,
  c("variant", "component", "source", "N", "denominator_selected", "proportion")
)
fwrite(source_distribution, file.path(qc_out, "selected_component_source_distribution.csv"))

quantile_safe <- function(x, probs) {
  if (!length(x) || all(is.na(x))) return(rep(NA_real_, length(probs)))
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

prediction_distribution <- rbindlist(lapply(names(variants), function(nm) {
  s <- variants[[nm]]$selected
  anchor_delay <- s$anchor_time - s$index_time
  prediction_delay <- s$prediction_time - s$index_time
  entry_delay <- s$prediction_time - s$tuple_last_measurement_time
  bind_rows <- function(metric, x) {
    q <- quantile_safe(x, c(0, .05, .25, .5, .75, .95, 1))
    data.table(
      variant = nm, metric = metric, n = sum(!is.na(x)),
      min = q[1L], q05 = q[2L], q25 = q[3L], median = q[4L],
      q75 = q[5L], q95 = q[6L], max = q[7L], mean = mean(x, na.rm = TRUE)
    )
  }
  rbindlist(list(
    bind_rows("anchor_minus_index_minutes", anchor_delay),
    bind_rows("prediction_minus_index_minutes", prediction_delay),
    bind_rows("availability_minus_last_measurement_minutes", entry_delay)
  ))
}))
fwrite(prediction_distribution, file.path(qc_out, "prediction_time_distribution.csv"))

selected_value_distribution <- rbindlist(lapply(names(variants), function(nm) {
  s <- variants[[nm]]$selected
  value_map <- list(
    plateau_cmH2O = s$pplat,
    peak_cmH2O = s$ppeak_value,
    peep_cmH2O = s$peep_value,
    tidal_volume_mL = s$vt_value,
    respiratory_rate_per_min = s$rr_value,
    driving_pressure_cmH2O = s$delta_p,
    resistive_pressure_cmH2O = s$resistive_pressure,
    surrogate_mp_J_per_min = s$smp
  )
  rbindlist(lapply(names(value_map), function(metric) {
    x <- value_map[[metric]]
    q <- quantile_safe(x, c(0, .05, .25, .5, .75, .95, 1))
    data.table(
      variant = nm, metric = metric, n = sum(!is.na(x)),
      min = q[1L], q05 = q[2L], q25 = q[3L], median = q[4L],
      q75 = q[5L], q95 = q[6L], max = q[7L],
      mean = if (length(x)) mean(x, na.rm = TRUE) else NA_real_,
      sd = if (length(x) > 1L) sd(x, na.rm = TRUE) else NA_real_
    )
  }))
}))
fwrite(
  selected_value_distribution,
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

# Outcome-blind observed-versus-unobserved audit based only on index-known data.
selection <- copy(index)
selection <- artifacts$primary_60min[
  , .(patientunitstayid, tuple_observed)
][selection, on = "patientunitstayid"]
selection[, selection_group := fifelse(tuple_observed, "tuple_observed", "tuple_missing")]

continuous_selection_vars <- c(
  "age_num", "index_time", "pao2", "index_fio2", "index_peep", "pf_ratio"
)
selection_continuous <- rbindlist(lapply(continuous_selection_vars, function(v) {
  selection[, {
    z <- get(v)
    q <- quantile_safe(z, c(.25, .5, .75))
    .(
      variable = v, n = sum(!is.na(z)), missing_n = sum(is.na(z)),
      mean = mean(z, na.rm = TRUE), sd = sd(z, na.rm = TRUE),
      q25 = q[1L], median = q[2L], q75 = q[3L]
    )
  }, by = selection_group]
}))
selection_smd <- rbindlist(lapply(continuous_selection_vars, function(v) {
  x1 <- selection[tuple_observed == TRUE][[v]]
  x0 <- selection[tuple_observed == FALSE][[v]]
  pooled_sd <- sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE)) / 2)
  data.table(
    variable = v,
    standardized_mean_difference_observed_minus_missing =
      (mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) / pooled_sd
  )
}))
selection_continuous <- selection_smd[selection_continuous, on = "variable"]
fwrite(selection_continuous, file.path(qc_out, "selection_audit_index_continuous.csv"))

categorical_selection_vars <- c("gender", "invasive_evidence_type", "infection_source")
selection_categorical <- rbindlist(lapply(categorical_selection_vars, function(v) {
  z <- selection[, .N, by = .(selection_group, level = as.character(get(v)))]
  z[, denominator := sum(N), by = selection_group]
  z[, proportion := N / denominator]
  z[, variable := v]
  z[, .(variable, selection_group, level, N, denominator, proportion)]
}))
fwrite(selection_categorical, file.path(qc_out, "selection_audit_index_categorical.csv"))

fwrite(invariants, file.path(qc_out, "paired_exposure_invariant_tests.csv"))

# Final aggregate-output leakage audit. Inspect headers/column names only.
qc_csv <- list.files(qc_out, pattern = "\\.csv$", full.names = TRUE)
qc_headers <- rbindlist(lapply(qc_csv, function(f) {
  data.table(file = basename(f), column = names(fread(f, nrows = 0L)))
}))
header_leak <- qc_headers[grepl(forbidden_pattern, column, ignore.case = TRUE)]
leakage_guard <- data.table(
  check = c(
    "working_index_has_no_outcome_like_columns",
    "private_artifacts_have_no_outcome_like_columns",
    "aggregate_qc_headers_have_no_outcome_like_columns",
    "aggregate_qc_contains_no_identifier_columns"
  ),
  pass = c(
    !any(grepl(forbidden_pattern, names(index), ignore.case = TRUE)),
    all(vapply(
      artifacts,
      function(x) !any(grepl(forbidden_pattern, names(x), ignore.case = TRUE)),
      logical(1L)
    )),
    nrow(header_leak) == 0L,
    !any(qc_headers$column %chin% c(
      "patientunitstayid", "patienthealthsystemstayid", "person_key",
      "uniquepid", "respchartid"
    ))
  )
)
if (any(!leakage_guard$pass)) stop("Final leakage guard failed.")
fwrite(leakage_guard, file.path(qc_out, "outcome_leakage_guard.csv"))

primary_n <- nrow(variants$primary_60min$selected)
sens30_n <- nrow(variants$sensitivity_30min$selected)
preferred_n <- nrow(variants$sensitivity_preferred_60min$selected)

summary_lines <- c(
  "# eICU paired ventilator exposure QC",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("- Locked configuration: ", LOCKED$version),
  paste0("- Strict index cohort: ", nrow(index), " stays/patients"),
  paste0("- Primary ±60-minute first valid tuples: ", primary_n),
  paste0("- Sensitivity ±30-minute first valid tuples: ", sens30_n),
  paste0("- Preferred-source-only ±60-minute first valid tuples: ", preferred_n),
  paste0(
    "- Preferred-source-only sensitivity estimable: ",
    if (all(preferred_feasibility$source_available)) "yes" else "no",
    if (!all(preferred_feasibility$source_available)) {
      " (at least one required preferred source was absent in the strict cohort)"
    } else {
      ""
    }
  ),
  "- Pairing hierarchy: source preference, then nearest absolute gap; prior wins an exact tie.",
  "- Duplicate numeric observations at the same stay/time/source were reduced to their median.",
  "- Prediction time uses measurement/entry availability time and must be within the observable 6-hour window.",
  "- No mortality, discharge-status, survival, effect, or performance field was selected or summarized; ICU end time was used only as an observation bound.",
  "- Row-level outputs are confined to analysis_rebuild_v1/private/eicu.",
  "",
  "All detailed funnel, label, range, source, gap, invalid-reason, timing, selection, boundary, invariant, and leakage checks are in this directory as aggregate CSV files."
)
writeLines(summary_lines, file.path(qc_out, "eicu_paired_exposure_QC.md"), useBytes = TRUE)

# Publish an atomic downstream gate only after all private artifacts and
# aggregate checks have been written and verified.
formal_rds <- c(
  primary_60min = file.path(
    private_out, "eicu_paired_exposure_primary_60min_v1.rds"
  ),
  sensitivity_30min = file.path(
    private_out, "eicu_paired_exposure_sensitivity_30min_v1.rds"
  ),
  sensitivity_preferred_60min = file.path(
    private_out, "eicu_paired_exposure_sensitivity_preferred_60min_v1.rds"
  ),
  all_valid_primary_60min = file.path(
    private_out, "eicu_paired_exposure_all_valid_primary_60min_v1.rds"
  )
)
required_qc <- file.path(qc_out, c(
  "raw_component_label_QC.csv", "locked_source_hierarchy_QC.csv",
  "preferred_source_sensitivity_feasibility.csv",
  "late_entry_exclusion_QC.csv", "same_time_duplicate_QC.csv",
  "paired_exposure_funnel.csv", "anchor_invalid_reasons.csv",
  "selected_component_source_distribution.csv",
  "selected_pairing_gap_distribution.csv", "valid_tuple_count_distribution.csv",
  "prediction_time_distribution.csv", "selected_component_value_distribution.csv",
  "selected_boundary_value_QC.csv", "selection_audit_index_continuous.csv",
  "selection_audit_index_categorical.csv", "paired_exposure_invariant_tests.csv",
  "outcome_leakage_guard.csv", "eicu_paired_exposure_QC.md"
))
if (!all(file.exists(formal_rds))) {
  stop("One or more formal eICU exposure RDS products are missing.")
}
if (!all(file.exists(required_qc))) {
  stop("One or more required eICU exposure QC products are missing.")
}
formal_rds_sha256 <- vapply(
  formal_rds,
  function(path) digest::digest(file = path, algo = "sha256"),
  character(1L)
)
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
    input_cohort_sha256, format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    all(invariants$pass), all(leakage_guard$pass), all(file.exists(required_qc)),
    nrow(index), primary_n, sens30_n, preferred_n, formal_rds_sha256
  ))
)
fwrite(completion_gate, phase2_complete_tmp)
if (!file.rename(phase2_complete_tmp, phase2_complete)) {
  stop("Could not atomically publish the eICU Phase-2 completion gate.")
}
message("eICU paired exposure build complete (outcome-blind).")
message("  BUILD_COMPLETE | script SHA256 ", script_sha256)
message("  strict cohort: ", nrow(index))
message("  primary 60-min tuple: ", primary_n)
message("  sensitivity 30-min tuple: ", sens30_n)
message("  preferred-only 60-min tuple: ", preferred_n)
message("  private outputs: ", private_out)
message("  aggregate QC: ", qc_out)
