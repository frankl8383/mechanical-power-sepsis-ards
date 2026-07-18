#!/usr/bin/env Rscript

# rebuild_v2 Phase 1B: eICU-CRD broad oxygenation-defined AHRF index cohort.
# Diagnosis-based infection is not a primary eligibility criterion and remains
# a separately labelled clinical-context sensitivity.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/02_build_eicu_index_cohort.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))
source(file.path(dirname(script_path), "00_phase1_replay_utils.R"))

set.seed(LOCKED_V2$bootstrap$seed_eicu)

out_private <- file.path(PRIVATE_ROOT, "eicu")
out_qc <- file.path(QC_ROOT, "eicu")
dir.create(out_private, recursive = TRUE, showWarnings = FALSE)
dir.create(out_qc, recursive = TRUE, showWarnings = FALSE)

completion_path <- file.path(out_qc, "phase1_eicu_complete_v2.csv")
completion_tmp <- paste0(completion_path, ".tmp")
unlink(c(completion_path, completion_tmp), force = TRUE)

message("Replaying the pinned v1 eICU respiratory phenotype to stage5 ...")
replayed <- replay_v1_respiratory_stage5("eicu")
stage5 <- replayed$stage5

required_stage5 <- c(
  "patientunitstayid", "patienthealthsystemstayid", "person_key",
  "hospitalid", "age_num", "gender", "pao2_time", "pao2",
  "fio2_near_value", "peep_near_value", "pf_ratio", "icu_end_offset",
  "invasive_evidence_type", "infection_early", "infection_source"
)
missing_stage5 <- setdiff(required_stage5, names(stage5))
if (length(missing_stage5)) {
  stop("Replayed eICU stage5 is missing: ", paste(missing_stage5, collapse = ", "))
}

first_person_cohort <- function(events) {
  x <- copy(events)
  setorder(x, patientunitstayid, pao2_time, event_id)
  stays <- x[, .SD[1L], by = patientunitstayid]
  stays[, order_year := fifelse(
    is.na(hospitaldischargeyear), 9999L, as.integer(hospitaldischargeyear)
  )]
  stays[, order_hsp := fifelse(
    is.na(patienthealthsystemstayid), .Machine$integer.max,
    as.integer(patienthealthsystemstayid)
  )]
  stays[, order_visit := fifelse(
    is.na(unitvisitnumber), .Machine$integer.max, as.integer(unitvisitnumber)
  )]
  setorder(
    stays, person_key, order_year, order_hsp, order_visit,
    pao2_time, patientunitstayid
  )
  cohort <- stays[, .SD[1L], by = person_key]
  cohort[, c("order_year", "order_hsp", "order_visit") := NULL]
  stays[, c("order_year", "order_hsp", "order_visit") := NULL]
  list(stays = stays, cohort = cohort)
}

broad <- first_person_cohort(stage5)
infection_events <- stage5[infection_early == TRUE]
infection_sensitivity <- first_person_cohort(infection_events)

restricted_keep <- c(
  "patientunitstayid", "patienthealthsystemstayid", "person_key", "uniquepid",
  "hospitalid", "unitvisitnumber", "unitstaytype",
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
  "infection_plus24_source", "apache_inv_flag", "icu_end_offset"
)
restricted_keep <- restricted_keep[
  restricted_keep %in% names(broad$cohort)
]
stay_out <- broad$stays[, ..restricted_keep]
cohort_out <- broad$cohort[, ..restricted_keep]
infection_keep <- restricted_keep[
  restricted_keep %in% names(infection_sensitivity$cohort)
]
infection_out <- infection_sensitivity$cohort[, ..infection_keep]

metadata <- list(
  version = "eicu_index_cohort_v2",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED_V2$version,
  governance = LOCKED_V2$governance,
  phenotype = paste(
    "broad oxygenation-defined acute hypoxemic respiratory failure;",
    "not imaging-adjudicated ARDS; infection not required"
  ),
  replay_source = replayed$replay_manifest$source_path[[1L]],
  replay_source_sha256 = replayed$replay_manifest$source_sha256[[1L]],
  replay_boundary = replayed$replay_manifest$execution_boundary[[1L]],
  selection = paste(
    "first respiratory-eligible event per ICU stay, then deterministic first",
    "qualifying encounter per eICU person_key"
  ),
  parameters = list(
    age_min = LOCKED_V2$minimum_age_years,
    pf_max = LOCKED_V2$pf_threshold_mmHg,
    peep_min = LOCKED_V2$minimum_index_peep_cmH2O,
    fio2_pair_window_min = LOCKED_V2$pao2_fio2_pair_window_minutes,
    peep_pair_window_min = LOCKED_V2$pao2_peep_pair_window_minutes
  )
)
attr(stay_out, "rebuild_metadata") <- metadata
attr(cohort_out, "rebuild_metadata") <- metadata
attr(infection_out, "rebuild_metadata") <- modifyList(
  metadata,
  list(
    version = "eicu_index_cohort_infection_sensitivity_v2",
    role = LOCKED_V2$infection_sensitivity$role,
    infection_definition = paste(
      "eICU diagnosis/admissionDx evidence available from index-48h through",
      "index; not equivalent to MIMIC antibiotic/culture suspected infection"
    )
  )
)

stay_path <- file.path(out_private, "eicu_index_stay_candidates_v2.rds")
cohort_path <- file.path(out_private, "eicu_index_cohort_v2.rds")
saveRDS(stay_out, stay_path, compress = "xz")
saveRDS(cohort_out, cohort_path, compress = "xz")
saveRDS(
  infection_out,
  file.path(out_private, "eicu_index_cohort_infection_sensitivity_v2.rds"),
  compress = "xz"
)

funnel <- rbindlist(list(
  replayed$stage_counts[, .(
    step = paste0("respiratory_", stage),
    unit = "candidate event",
    n = event_n
  )],
  data.table(
    step = c(
      "broad_respiratory_eligible_events",
      "first_broad_event_per_icu_stay",
      "first_broad_encounter_per_person",
      "infection_restricted_sensitivity_persons"
    ),
    unit = c("candidate event", "ICU stay", "person", "person"),
    n = c(
      nrow(stage5), nrow(stay_out), nrow(cohort_out), nrow(infection_out)
    )
  )
))
fwrite(funnel, file.path(out_qc, "qc_funnel_v2.csv"))
fwrite(
  replayed$replay_manifest,
  file.path(out_qc, "controlled_replay_manifest_v2.csv")
)

forbidden <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
invariants <- data.table(
  check = c(
    "one_row_per_person",
    "one_row_per_unit_stay",
    "all_adult",
    "pf_at_or_below_threshold",
    "peep_at_or_above_threshold",
    "infection_sensitivity_nested_in_respiratory_events",
    "outcome_like_columns_absent",
    "v1_tree_unchanged"
  ),
  pass = c(
    !anyDuplicated(cohort_out$person_key),
    !anyDuplicated(stay_out$patientunitstayid),
    all(cohort_out$age_num >= LOCKED_V2$minimum_age_years),
    all(cohort_out$pf_ratio <= LOCKED_V2$pf_threshold_mmHg),
    all(cohort_out$peep_near_value >= LOCKED_V2$minimum_index_peep_cmH2O),
    all(infection_out$infection_early == TRUE),
    !any(grepl(forbidden, names(cohort_out), ignore.case = TRUE)),
    all(replayed$replay_manifest$v1_output_tree_unchanged)
  )
)
fwrite(invariants, file.path(out_qc, "qc_invariants_v2.csv"))
if (!all(invariants$pass)) {
  stop(
    "eICU broad Phase-1 invariant failure: ",
    paste(invariants[pass == FALSE, check], collapse = ", ")
  )
}

gate <- data.table(
  field = c(
    "locked_config_version", "script_sha256", "source_v1_sha256",
    "replay_stop_assignment", "all_invariants_pass",
    "outcome_leakage_guard_pass", "all_required_qc_present",
    "primary_cohort_rds_sha256", "primary_cohort_n", "completed_at"
  ),
  value = c(
    LOCKED_V2$version,
    sha256_file(script_path),
    replayed$replay_manifest$source_sha256[[1L]],
    "stage5",
    "TRUE", "TRUE", "TRUE",
    sha256_file(cohort_path),
    as.character(nrow(cohort_out)),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
)
fwrite(gate, completion_tmp)
if (!file.rename(completion_tmp, completion_path)) {
  stop("Could not atomically publish eICU Phase-1 completion gate.")
}

message("eICU broad AHRF Phase 1 complete.")
message("  respiratory-eligible events: ", nrow(stage5))
message("  first eligible ICU stays: ", nrow(stay_out))
message("  first eligible persons: ", nrow(cohort_out))
