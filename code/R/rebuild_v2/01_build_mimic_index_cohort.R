#!/usr/bin/env Rscript

# rebuild_v2 Phase 1A: MIMIC-IV broad oxygenation-defined AHRF index cohort.
# Infection is not a primary eligibility criterion. The rebuild_v1
# infection-restricted cohort is retained as a separately labelled sensitivity.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/01_build_mimic_index_cohort.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))
source(file.path(dirname(script_path), "00_phase1_replay_utils.R"))

set.seed(LOCKED_V2$bootstrap$seed_mimic)

out_private <- file.path(PRIVATE_ROOT, "mimic")
out_qc <- file.path(QC_ROOT, "mimic")
dir.create(out_private, recursive = TRUE, showWarnings = FALSE)
dir.create(out_qc, recursive = TRUE, showWarnings = FALSE)

completion_path <- file.path(out_qc, "phase1_complete_v2.csv")
completion_tmp <- paste0(completion_path, ".tmp")
unlink(c(completion_path, completion_tmp), force = TRUE)

message("Replaying the pinned v1 MIMIC respiratory phenotype to stage5 ...")
replayed <- replay_v1_respiratory_stage5("mimic")
stage5 <- replayed$stage5

required_stage5 <- c(
  "subject_id", "hadm_id", "stay_id", "intime", "outtime",
  "age_at_admission", "gender", "pao2_time", "pao2",
  "fio2_near_value", "peep_near_value", "pf_ratio",
  "invasive_evidence_type"
)
missing_stage5 <- setdiff(required_stage5, names(stage5))
if (length(missing_stage5)) {
  stop("Replayed MIMIC stage5 is missing: ", paste(missing_stage5, collapse = ", "))
}

# Preserve the v1 exposure-script schema without implying infection at the
# broad index. These fields are deliberately NA in the broad primary cohort.
infection_schema <- list(
  infection_time = as.POSIXct(NA, tz = "UTC"),
  infection_gap_h = NA_real_,
  infection_evidence_time = as.POSIXct(NA, tz = "UTC"),
  infection_direction = NA_character_,
  infection_culture_time_precision = NA_character_,
  infection_available_by_index = NA
)
for (nm in names(infection_schema)) {
  if (!nm %in% names(stage5)) {
    set(stage5, j = nm, value = rep(infection_schema[[nm]], nrow(stage5)))
  }
}

setorder(stage5, stay_id, pao2_time, event_id)
stay_candidates <- stage5[, .SD[1L], by = stay_id]
setorder(stay_candidates, subject_id, pao2_time, intime, stay_id)
cohort <- stay_candidates[, .SD[1L], by = subject_id]

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
  "infection_culture_time_precision", "infection_available_by_index"
)
restricted_keep <- restricted_keep[restricted_keep %in% names(cohort)]
stay_out <- stay_candidates[, ..restricted_keep]
cohort_out <- cohort[, ..restricted_keep]

metadata <- list(
  version = "mimic_index_cohort_v2",
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
    "first respiratory-eligible event per ICU stay, then first qualifying",
    "stay per MIMIC subject"
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

stay_path <- file.path(out_private, "mimic_index_stay_candidates_v2.rds")
cohort_path <- file.path(out_private, "mimic_index_cohort_v2.rds")
saveRDS(stay_out, stay_path, compress = "xz")
saveRDS(cohort_out, cohort_path, compress = "xz")

# Keep the already audited v1 infection-restricted cohort as a clinical-context
# sensitivity. It is not used to define the broad index or main exposure build.
v1_infection_path <- file.path(
  REBUILD_V1_ROOT, "private", "mimic", "mimic_index_cohort_v1.rds"
)
if (!file.exists(v1_infection_path)) {
  stop("Required rebuild_v1 MIMIC infection sensitivity is missing: ", v1_infection_path)
}
infection_sensitivity <- readRDS(v1_infection_path)
infection_metadata <- attr(infection_sensitivity, "rebuild_metadata")
attr(infection_sensitivity, "rebuild_metadata") <- list(
  version = "mimic_index_cohort_infection_sensitivity_v2",
  role = LOCKED_V2$infection_sensitivity$role,
  source_artifact = normalizePath(v1_infection_path),
  source_sha256 = sha256_file(v1_infection_path),
  source_metadata = infection_metadata,
  note = paste(
    "Reused unchanged because the post-review amendment moves infection",
    "from primary eligibility to a sensitivity; its v1 construction remains",
    "the audited Seymour-style antibiotic/culture implementation."
  )
)
saveRDS(
  infection_sensitivity,
  file.path(out_private, "mimic_index_cohort_infection_sensitivity_v2.rds"),
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
      "first_broad_stay_per_subject",
      "infection_restricted_sensitivity_subjects"
    ),
    unit = c("candidate event", "ICU stay", "subject", "subject"),
    n = c(
      nrow(stage5), nrow(stay_out), nrow(cohort_out),
      nrow(infection_sensitivity)
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
    "one_row_per_subject",
    "one_row_per_stay",
    "all_adult",
    "pf_at_or_below_threshold",
    "peep_at_or_above_threshold",
    "no_primary_infection_requirement",
    "outcome_like_columns_absent",
    "v1_tree_unchanged"
  ),
  pass = c(
    !anyDuplicated(cohort_out$subject_id),
    !anyDuplicated(stay_out$stay_id),
    all(cohort_out$age_at_admission >= LOCKED_V2$minimum_age_years),
    all(cohort_out$pf_ratio <= LOCKED_V2$pf_threshold_mmHg),
    all(cohort_out$peep_near_value >= LOCKED_V2$minimum_index_peep_cmH2O),
    all(is.na(cohort_out$infection_time)),
    !any(grepl(forbidden, names(cohort_out), ignore.case = TRUE)),
    all(replayed$replay_manifest$v1_output_tree_unchanged)
  )
)
fwrite(invariants, file.path(out_qc, "qc_invariants_v2.csv"))
if (!all(invariants$pass)) {
  stop(
    "MIMIC broad Phase-1 invariant failure: ",
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
  stop("Could not atomically publish MIMIC Phase-1 completion gate.")
}

message("MIMIC broad AHRF Phase 1 complete.")
message("  respiratory-eligible events: ", nrow(stage5))
message("  first eligible ICU stays: ", nrow(stay_out))
message("  first eligible subjects: ", nrow(cohort_out))
