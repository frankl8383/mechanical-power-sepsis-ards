#!/usr/bin/env Rscript

# rebuild_v2 conditional secondary analysis:
# - freeze and construct an outcome-blind 72-hour persistent-AHRF phenotype;
# - impose a separate index+72 h hospital-risk landmark;
# - audit reuse of the already frozen index+6 h tuple and no-GCS core;
# - inspect post-landmark event and hospital support only after publishing and
#   hashing the outcome-blind phenotype;
# - stop the outcome analysis unless the eICU final common set has >=100
#   post-landmark deaths and >=10 contributing hospitals.
#
# This script does not fit an outcome model or run a bootstrap.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/16_build_persistent_ahrf_feasibility.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))
source(file.path(dirname(script_path), "00_phase1_replay_utils.R"))
source(file.path(dirname(script_path), "01_analysis_utils.R"))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required.")
}

out_private <- file.path(PRIVATE_ROOT, "persistent_ahrf")
out_aggregate <- file.path(AGGREGATE_ROOT, "persistent_ahrf")
out_qc <- file.path(QC_ROOT, "persistent_ahrf")
for (d in c(out_private, out_aggregate, out_qc)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

completion_path <- file.path(
  out_qc, "persistent_ahrf_72h_feasibility_complete_v2.csv"
)
completion_tmp <- paste0(completion_path, ".tmp")
unlink(c(completion_path, completion_tmp), force = TRUE)

preread_lock_path <- normalizePath(
  file.path(
    PROJECT_ROOT, "docs", "rebuild_v2",
    "persistent_ahrf_72h_preread_lock_v2.md"
  ),
  mustWork = TRUE
)
preread_lock_sha256_expected <-
  "7c4fc1c9f77eae28672e04c7e18abc917dbbe1aed3747711f5229a8a64628c98"
if (!identical(
  sha256_file(preread_lock_path),
  preread_lock_sha256_expected
)) {
  stop("The persistent-AHRF preread lock changed; review before execution.")
}

PERSISTENCE_LOCK <- list(
  label = "persistent-AHRF-enriched sensitivity",
  index = "already frozen broad-AHRF index event",
  landmark_hours = 72,
  windows = c("0_to_lt24h", "24_to_lt48h", "48_to_le72h"),
  pf_threshold_mmHg = 300,
  peep_threshold_cmH2O = 5,
  record_rule = paste(
    "all valid PaO2/FiO2 records, including P/F >=300, paired to",
    "PEEP >=5 by the locked symmetric +/-120-minute rule"
  ),
  summary_rule = "arithmetic mean P/F within each non-overlapping 24-hour window",
  measurement_support_rule = "at least one valid record in each window",
  persistence_rule = "mean P/F <300 mm Hg in every window",
  death_exception = "none",
  tuple_rule = "reuse the frozen first valid tuple available by index+6h",
  core_rule = "reuse the frozen complete no-GCS core available by index+6h",
  endpoint = "in-hospital mortality strictly after the index+72h landmark",
  minimum_eicu_events = LOCKED_V2$persistence_sensitivity$minimum_eicu_events,
  minimum_eicu_hospitals =
    LOCKED_V2$persistence_sensitivity$minimum_eicu_hospitals
)

definition <- data.table(
  field = names(PERSISTENCE_LOCK),
  value = vapply(PERSISTENCE_LOCK, function(x) {
    paste(as.character(x), collapse = " | ")
  }, character(1))
)
fwrite(
  definition,
  file.path(out_qc, "persistent_ahrf_72h_locked_definition_v2.csv")
)

capture_preinfection_stages <- function(database = c("mimic", "eicu")) {
  database <- match.arg(database)
  spec_name <- paste0(database, "_phase1")
  source_path <- assert_pinned_source(spec_name)
  v1_before <- snapshot_v1_tree()

  scratch_root <- file.path(
    PRIVATE_ROOT, "_persistent_ahrf_replay_scratch", database
  )
  roots <- list(
    rebuild = scratch_root,
    private = file.path(scratch_root, "private"),
    aggregate = file.path(scratch_root, "aggregate"),
    qc = file.path(scratch_root, "qc")
  )
  for (d in roots) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  cache_guard <- NULL
  old_refresh <- Sys.getenv(
    "MIMIC_REBUILD_REFRESH_CACHE", unset = NA_character_
  )
  if (database == "mimic") {
    Sys.setenv(MIMIC_REBUILD_REFRESH_CACHE = "0")
    cache_guard <- seed_mimic_phase1_cache_links(roots$private)
  }
  on.exit({
    if (is.na(old_refresh)) {
      Sys.unsetenv("MIMIC_REBUILD_REFRESH_CACHE")
    } else {
      Sys.setenv(MIMIC_REBUILD_REFRESH_CACHE = old_refresh)
    }
  }, add = TRUE)

  replay_env <- new.env(parent = globalenv())
  replay_env$source <- make_config_injector(
    replay_env, roots, phase1_compatibility_lock()
  )
  expressions <- parse(file = source_path, keep.source = FALSE)
  stop_expression <- NA_integer_
  for (i in seq_along(expressions)) {
    eval(expressions[[i]], envir = replay_env)
    if (identical(assignment_name(expressions[[i]]), "stage5")) {
      stop_expression <- i
      break
    }
  }
  needed <- c("stage1", "stage5")
  missing <- needed[
    !vapply(
      needed, exists, logical(1), envir = replay_env, inherits = FALSE
    )
  ]
  if (length(missing)) {
    stop(
      database, " replay failed to expose: ",
      paste(missing, collapse = ", ")
    )
  }
  stage1 <- get("stage1", envir = replay_env, inherits = FALSE)
  stage5 <- get("stage5", envir = replay_env, inherits = FALSE)
  if (!is.data.table(stage1) || !nrow(stage1) ||
      !is.data.table(stage5) || !nrow(stage5)) {
    stop(database, " replay stages are empty or malformed.")
  }

  if (!is.null(cache_guard)) assert_cache_hashes_unchanged(cache_guard)
  assert_v1_tree_unchanged(v1_before)

  list(
    stage1 = copy(stage1),
    stage5 = copy(stage5),
    manifest = data.table(
      database = database,
      source_path = source_path,
      source_sha256 = sha256_file(source_path),
      stopped_after_expression = stop_expression,
      stop_assignment = "stage5",
      stage1_n = nrow(stage1),
      stage5_n = nrow(stage5),
      stage1_role = paste(
        "all paired P/F records before the low-P/F threshold;",
        "used so P/F >=300 records remain in 24-hour means"
      ),
      outcome_artifact_read = FALSE,
      v1_output_tree_unchanged = TRUE,
      completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    )
  )
}

window_from_elapsed_hours <- function(x) {
  fcase(
    is.finite(x) & x >= 0 & x < 24, 1L,
    is.finite(x) & x >= 24 & x < 48, 2L,
    is.finite(x) & x >= 48 & x <= 72, 3L,
    default = NA_integer_
  )
}

make_window_wide <- function(events, id_cols) {
  by_cols <- c(id_cols, "window_id")
  summary <- events[, .(
    measurement_n = .N,
    mean_pf = mean(pf_ratio),
    minimum_pf = min(pf_ratio),
    maximum_pf = max(pf_ratio),
    mean_peep = mean(peep_value)
  ), by = by_cols]

  formula <- as.formula(paste(
    paste(id_cols, collapse = " + "), "~ window_id"
  ))
  wide <- dcast(
    summary,
    formula,
    value.var = c(
      "measurement_n", "mean_pf", "minimum_pf", "maximum_pf", "mean_peep"
    )
  )
  old_names <- names(wide)
  new_names <- gsub("_1$", "_window1", old_names)
  new_names <- gsub("_2$", "_window2", new_names)
  new_names <- gsub("_3$", "_window3", new_names)
  setnames(wide, old_names, new_names)
  list(summary = summary, wide = wide)
}

build_mimic_physiology <- function(replay) {
  index_path <- file.path(
    PRIVATE_ROOT, "mimic", "mimic_index_cohort_v2.rds"
  )
  tuple_path <- file.path(
    PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds"
  )
  core_path <- file.path(
    PRIVATE_ROOT, "mimic", "mimic_fixed6h_tuple_no_gcs_core_v2.rds"
  )
  for (p in c(index_path, tuple_path, core_path)) {
    if (!file.exists(p)) stop("Required MIMIC input is missing: ", p)
  }

  index <- as.data.table(readRDS(index_path))[, .(
    subject_id, hadm_id, stay_id,
    index_time = as.POSIXct(pao2_time, tz = "UTC")
  )]
  if (anyDuplicated(index$subject_id) || anyDuplicated(index$stay_id)) {
    stop("MIMIC broad index is not unique by subject and stay.")
  }
  tuple <- as.data.table(readRDS(tuple_path))[, .(stay_id)]
  core <- as.data.table(readRDS(core_path))[, .(
    stay_id, complete_no_gcs_core_by_6h =
      as.logical(tuple_and_complete_no_gcs_core)
  )]
  if (anyDuplicated(tuple$stay_id) || anyDuplicated(core$stay_id)) {
    stop("MIMIC tuple/core inputs are not unique by stay.")
  }

  stage1 <- replay$stage1
  v2_require_columns(
    stage1,
    c("subject_id", "hadm_id", "stay_id", "pao2_time",
      "pf_ratio", "peep_near_value"),
    "MIMIC replay stage1"
  )
  events <- stage1[
    is.finite(pf_ratio) &
      is.finite(peep_near_value) &
      peep_near_value >= PERSISTENCE_LOCK$peep_threshold_cmH2O,
    .(
      subject_id, hadm_id, stay_id,
      measurement_time = as.POSIXct(pao2_time, tz = "UTC"),
      pf_ratio = as.numeric(pf_ratio),
      peep_value = as.numeric(peep_near_value)
    )
  ]
  events <- merge(
    events,
    index[, .(subject_id, hadm_id, stay_id, index_time)],
    by = c("subject_id", "hadm_id", "stay_id"),
    all = FALSE
  )
  events[, elapsed_hours := as.numeric(difftime(
    measurement_time, index_time, units = "hours"
  ))]
  events[, window_id := window_from_elapsed_hours(elapsed_hours)]
  events <- events[!is.na(window_id)]
  if (!nrow(events)) stop("MIMIC has no P/F-plus-PEEP records through 72 h.")

  windowed <- make_window_wide(
    events, c("subject_id", "hadm_id", "stay_id")
  )
  phenotype <- merge(
    index, windowed$wide,
    by = c("subject_id", "hadm_id", "stay_id"), all.x = TRUE
  )
  required_window_cols <- unlist(lapply(
    c("measurement_n", "mean_pf", "minimum_pf", "maximum_pf", "mean_peep"),
    function(prefix) paste0(prefix, "_window", 1:3)
  ))
  for (nm in required_window_cols) {
    if (!nm %in% names(phenotype)) phenotype[, (nm) := NA_real_]
  }
  phenotype[, all_three_windows_observed :=
      rowSums(!is.na(.SD)) == 3L,
    .SDcols = paste0("mean_pf_window", 1:3)
  ]
  phenotype[, persistent_ahrf :=
      all_three_windows_observed &
        mean_pf_window1 < PERSISTENCE_LOCK$pf_threshold_mmHg &
        mean_pf_window2 < PERSISTENCE_LOCK$pf_threshold_mmHg &
        mean_pf_window3 < PERSISTENCE_LOCK$pf_threshold_mmHg]
  phenotype[, frozen_tuple_available_by_6h :=
      stay_id %in% tuple$stay_id]
  phenotype <- merge(phenotype, core, by = "stay_id", all.x = TRUE)
  phenotype[is.na(complete_no_gcs_core_by_6h),
            complete_no_gcs_core_by_6h := FALSE]
  setcolorder(
    phenotype,
    c("subject_id", "hadm_id", "stay_id", "index_time",
      setdiff(names(phenotype), c(
        "subject_id", "hadm_id", "stay_id", "index_time"
      )))
  )

  metadata <- list(
    version = "mimic_persistent_ahrf_72h_physiology_v2",
    database = "MIMIC-IV 3.1",
    outcome_blind = TRUE,
    preread_lock_sha256 = sha256_file(preread_lock_path),
    phase1_source_sha256 = replay$manifest$source_sha256[[1L]],
    index_sha256 = sha256_file(index_path),
    tuple_sha256 = sha256_file(tuple_path),
    core_sha256 = sha256_file(core_path),
    phenotype = PERSISTENCE_LOCK
  )
  attr(phenotype, "rebuild_metadata") <- metadata
  attr(events, "rebuild_metadata") <- metadata

  event_path <- file.path(
    out_private, "mimic_persistent_pf_records_0_72h_v2.rds"
  )
  phenotype_path <- file.path(
    out_private, "mimic_persistent_ahrf_72h_physiology_v2.rds"
  )
  saveRDS(events, event_path, compress = "xz")
  saveRDS(phenotype, phenotype_path, compress = "xz")

  list(
    events = events,
    window_summary = windowed$summary,
    phenotype = phenotype,
    paths = c(events = event_path, phenotype = phenotype_path),
    inputs = c(index = index_path, tuple = tuple_path, core = core_path)
  )
}

build_eicu_physiology <- function(replay) {
  index_path <- file.path(
    PRIVATE_ROOT, "eicu", "eicu_index_cohort_v2.rds"
  )
  tuple_path <- file.path(
    PRIVATE_ROOT, "eicu", "eicu_no_gcs_core_targets_v2.rds"
  )
  core_path <- file.path(
    PRIVATE_ROOT, "eicu", "eicu_fixed6h_tuple_no_gcs_core_v2.rds"
  )
  for (p in c(index_path, tuple_path, core_path)) {
    if (!file.exists(p)) stop("Required eICU input is missing: ", p)
  }

  index <- as.data.table(readRDS(index_path))[, .(
    patientunitstayid, patienthealthsystemstayid, person_key,
    hospitalid, index_time = as.numeric(pao2_time)
  )]
  if (anyDuplicated(index$person_key) ||
      anyDuplicated(index$patientunitstayid)) {
    stop("eICU broad index is not unique by person and unit stay.")
  }
  tuple <- as.data.table(readRDS(tuple_path))[, .(patientunitstayid)]
  core <- as.data.table(readRDS(core_path))[, .(
    patientunitstayid, complete_no_gcs_core_by_6h =
      as.logical(tuple_and_complete_no_gcs_core)
  )]
  if (anyDuplicated(tuple$patientunitstayid) ||
      anyDuplicated(core$patientunitstayid)) {
    stop("eICU tuple/core inputs are not unique by unit stay.")
  }

  stage1 <- replay$stage1
  v2_require_columns(
    stage1,
    c("patientunitstayid", "pao2_time", "pf_ratio", "peep_near_value"),
    "eICU replay stage1"
  )
  events <- stage1[
    is.finite(pf_ratio) &
      is.finite(peep_near_value) &
      peep_near_value >= PERSISTENCE_LOCK$peep_threshold_cmH2O,
    .(
      patientunitstayid,
      measurement_time = as.numeric(pao2_time),
      pf_ratio = as.numeric(pf_ratio),
      peep_value = as.numeric(peep_near_value)
    )
  ]
  events <- merge(
    events,
    index[, .(patientunitstayid, index_time)],
    by = "patientunitstayid", all = FALSE
  )
  events[, elapsed_hours := (measurement_time - index_time) / 60]
  events[, window_id := window_from_elapsed_hours(elapsed_hours)]
  events <- events[!is.na(window_id)]
  if (!nrow(events)) stop("eICU has no P/F-plus-PEEP records through 72 h.")

  windowed <- make_window_wide(events, "patientunitstayid")
  phenotype <- merge(
    index, windowed$wide, by = "patientunitstayid", all.x = TRUE
  )
  required_window_cols <- unlist(lapply(
    c("measurement_n", "mean_pf", "minimum_pf", "maximum_pf", "mean_peep"),
    function(prefix) paste0(prefix, "_window", 1:3)
  ))
  for (nm in required_window_cols) {
    if (!nm %in% names(phenotype)) phenotype[, (nm) := NA_real_]
  }
  phenotype[, all_three_windows_observed :=
      rowSums(!is.na(.SD)) == 3L,
    .SDcols = paste0("mean_pf_window", 1:3)
  ]
  phenotype[, persistent_ahrf :=
      all_three_windows_observed &
        mean_pf_window1 < PERSISTENCE_LOCK$pf_threshold_mmHg &
        mean_pf_window2 < PERSISTENCE_LOCK$pf_threshold_mmHg &
        mean_pf_window3 < PERSISTENCE_LOCK$pf_threshold_mmHg]
  phenotype[, frozen_tuple_available_by_6h :=
      patientunitstayid %in% tuple$patientunitstayid]
  phenotype <- merge(
    phenotype, core, by = "patientunitstayid", all.x = TRUE
  )
  phenotype[is.na(complete_no_gcs_core_by_6h),
            complete_no_gcs_core_by_6h := FALSE]
  setcolorder(
    phenotype,
    c("patientunitstayid", "patienthealthsystemstayid", "person_key",
      "hospitalid", "index_time",
      setdiff(names(phenotype), c(
        "patientunitstayid", "patienthealthsystemstayid", "person_key",
        "hospitalid", "index_time"
      )))
  )

  metadata <- list(
    version = "eicu_persistent_ahrf_72h_physiology_v2",
    database = "eICU-CRD 2.0",
    outcome_blind = TRUE,
    preread_lock_sha256 = sha256_file(preread_lock_path),
    phase1_source_sha256 = replay$manifest$source_sha256[[1L]],
    index_sha256 = sha256_file(index_path),
    tuple_sha256 = sha256_file(tuple_path),
    core_sha256 = sha256_file(core_path),
    phenotype = PERSISTENCE_LOCK
  )
  attr(phenotype, "rebuild_metadata") <- metadata
  attr(events, "rebuild_metadata") <- metadata

  event_path <- file.path(
    out_private, "eicu_persistent_pf_records_0_72h_v2.rds"
  )
  phenotype_path <- file.path(
    out_private, "eicu_persistent_ahrf_72h_physiology_v2.rds"
  )
  saveRDS(events, event_path, compress = "xz")
  saveRDS(phenotype, phenotype_path, compress = "xz")

  list(
    events = events,
    window_summary = windowed$summary,
    phenotype = phenotype,
    paths = c(events = event_path, phenotype = phenotype_path),
    inputs = c(index = index_path, tuple = tuple_path, core = core_path)
  )
}

message("Replaying all paired P/F events without reading outcomes ...")
mimic_replay <- capture_preinfection_stages("mimic")
eicu_replay <- capture_preinfection_stages("eicu")
replay_manifest <- rbindlist(list(
  mimic_replay$manifest, eicu_replay$manifest
), use.names = TRUE, fill = TRUE)
fwrite(
  replay_manifest,
  file.path(out_qc, "persistent_ahrf_phase1_replay_manifest_v2.csv")
)

message("Building and publishing outcome-blind persistence phenotypes ...")
mimic <- build_mimic_physiology(mimic_replay)
rm(mimic_replay)
gc()
eicu <- build_eicu_physiology(eicu_replay)
rm(eicu_replay)
gc()

forbidden_outcome_name <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
outcome_blind_invariants <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    check = c(
      "one_row_per_subject",
      "one_row_per_index_stay",
      "all_persistence_records_in_locked_windows",
      "all_persistence_records_have_peep_ge5",
      "all_persistence_records_finite_pf",
      "phenotype_has_no_outcome_like_columns",
      "outcome_blind_metadata_true",
      "index_event_guarantees_window1_support"
    ),
    pass = c(
      !anyDuplicated(mimic$phenotype$subject_id),
      !anyDuplicated(mimic$phenotype$stay_id),
      all(mimic$events$elapsed_hours >= 0 &
            mimic$events$elapsed_hours <= 72),
      all(mimic$events$peep_value >= 5),
      all(is.finite(mimic$events$pf_ratio)),
      !any(grepl(
        forbidden_outcome_name, names(mimic$phenotype),
        ignore.case = TRUE
      )),
      isTRUE(attr(
        mimic$phenotype, "rebuild_metadata"
      )$outcome_blind),
      all(!is.na(mimic$phenotype$mean_pf_window1))
    )
  ),
  data.table(
    database = "eICU-CRD",
    check = c(
      "one_row_per_person",
      "one_row_per_index_unit_stay",
      "all_persistence_records_in_locked_windows",
      "all_persistence_records_have_peep_ge5",
      "all_persistence_records_finite_pf",
      "phenotype_has_no_outcome_like_columns",
      "outcome_blind_metadata_true",
      "index_event_guarantees_window1_support"
    ),
    pass = c(
      !anyDuplicated(eicu$phenotype$person_key),
      !anyDuplicated(eicu$phenotype$patientunitstayid),
      all(eicu$events$elapsed_hours >= 0 &
            eicu$events$elapsed_hours <= 72),
      all(eicu$events$peep_value >= 5),
      all(is.finite(eicu$events$pf_ratio)),
      !any(grepl(
        forbidden_outcome_name, names(eicu$phenotype),
        ignore.case = TRUE
      )),
      isTRUE(attr(
        eicu$phenotype, "rebuild_metadata"
      )$outcome_blind),
      all(!is.na(eicu$phenotype$mean_pf_window1))
    )
  )
))
fwrite(
  outcome_blind_invariants,
  file.path(
    out_qc, "persistent_ahrf_outcome_blind_invariants_v2.csv"
  )
)
if (!all(outcome_blind_invariants$pass)) {
  stop(
    "Outcome-blind persistent-AHRF invariant failure: ",
    paste(
      outcome_blind_invariants[
        pass == FALSE, paste(database, check, sep = ":")
      ],
      collapse = ", "
    )
  )
}

outcome_blind_manifest <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    artifact_role = names(mimic$paths),
    path = normalizePath(mimic$paths),
    sha256 = vapply(mimic$paths, sha256_file, character(1)),
    row_n = c(nrow(mimic$events), nrow(mimic$phenotype)),
    outcome_artifact_read_before_publish = FALSE
  ),
  data.table(
    database = "eICU-CRD",
    artifact_role = names(eicu$paths),
    path = normalizePath(eicu$paths),
    sha256 = vapply(eicu$paths, sha256_file, character(1)),
    row_n = c(nrow(eicu$events), nrow(eicu$phenotype)),
    outcome_artifact_read_before_publish = FALSE
  )
), use.names = TRUE)
fwrite(
  outcome_blind_manifest,
  file.path(
    out_qc, "persistent_ahrf_outcome_blind_artifact_manifest_v2.csv"
  )
)

window_coverage <- rbindlist(list(
  rbindlist(lapply(1:3, function(k) {
    col <- paste0("mean_pf_window", k)
    data.table(
      database = "MIMIC-IV", window = k,
      broad_index_n = nrow(mimic$phenotype),
      observed_n = sum(!is.na(mimic$phenotype[[col]])),
      observed_percent =
        100 * mean(!is.na(mimic$phenotype[[col]]))
    )
  })),
  rbindlist(lapply(1:3, function(k) {
    col <- paste0("mean_pf_window", k)
    data.table(
      database = "eICU-CRD", window = k,
      broad_index_n = nrow(eicu$phenotype),
      observed_n = sum(!is.na(eicu$phenotype[[col]])),
      observed_percent =
        100 * mean(!is.na(eicu$phenotype[[col]]))
    )
  }))
))
fwrite(
  window_coverage,
  file.path(
    out_aggregate, "persistent_ahrf_window_coverage_v2.csv"
  )
)

physiology_feasibility <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    broad_index_n = nrow(mimic$phenotype),
    all_three_windows_observed_n =
      sum(mimic$phenotype$all_three_windows_observed),
    persistent_ahrf_n = sum(mimic$phenotype$persistent_ahrf),
    persistent_with_frozen_tuple_n = sum(
      mimic$phenotype$persistent_ahrf &
        mimic$phenotype$frozen_tuple_available_by_6h
    ),
    persistent_with_complete_core_n = sum(
      mimic$phenotype$persistent_ahrf &
        mimic$phenotype$complete_no_gcs_core_by_6h
    )
  ),
  data.table(
    database = "eICU-CRD",
    broad_index_n = nrow(eicu$phenotype),
    all_three_windows_observed_n =
      sum(eicu$phenotype$all_three_windows_observed),
    persistent_ahrf_n = sum(eicu$phenotype$persistent_ahrf),
    persistent_with_frozen_tuple_n = sum(
      eicu$phenotype$persistent_ahrf &
        eicu$phenotype$frozen_tuple_available_by_6h
    ),
    persistent_with_complete_core_n = sum(
      eicu$phenotype$persistent_ahrf &
        eicu$phenotype$complete_no_gcs_core_by_6h
    )
  )
))
fwrite(
  physiology_feasibility,
  file.path(
    out_aggregate, "persistent_ahrf_outcome_blind_feasibility_v2.csv"
  )
)

# The two phenotype RDS files and their hashes exist before this point.
# Hospital outcomes are read only below.
message("Outcome-blind phenotypes are frozen; now applying the 72-hour landmark.")

as_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = "UTC"))
  as.POSIXct(x, tz = "UTC")
}

build_mimic_landmark <- function(phenotype) {
  admissions_path <- file.path(MIMIC_ROOT, "hosp", "admissions.csv.gz")
  admissions <- fread(
    admissions_path,
    select = c(
      "subject_id", "hadm_id", "dischtime", "deathtime",
      "hospital_expire_flag"
    ),
    showProgress = FALSE
  )
  admissions[, dischtime := as_utc(dischtime)]
  admissions[, deathtime := as_utc(deathtime)]
  if (anyDuplicated(admissions[, .(subject_id, hadm_id)])) {
    stop("MIMIC admissions is not unique by subject/admission.")
  }
  x <- merge(
    copy(phenotype), admissions,
    by = c("subject_id", "hadm_id"), all.x = TRUE
  )
  x[, landmark_72h_time := index_time + 72 * 3600]
  x[, effective_death_time := deathtime]
  x[
    hospital_expire_flag == 1L & !is.na(dischtime) &
      (is.na(effective_death_time) | dischtime < effective_death_time),
    effective_death_time := dischtime
  ]
  x[, hospital_end_known := !is.na(dischtime)]
  x[, hospital_status_known := hospital_expire_flag %in% c(0L, 1L)]
  x[, death_on_or_before_72h :=
      hospital_expire_flag == 1L & !is.na(effective_death_time) &
        effective_death_time <= landmark_72h_time]
  x[, live_discharge_on_or_before_72h :=
      hospital_expire_flag == 0L & !is.na(dischtime) &
        dischtime <= landmark_72h_time]
  x[, unknown_followup_at_72h :=
      (!hospital_end_known | !hospital_status_known) &
        !death_on_or_before_72h & !live_discharge_on_or_before_72h]
  x[, at_risk_in_hospital_at_72h :=
      hospital_end_known & hospital_status_known &
        dischtime > landmark_72h_time &
        (is.na(effective_death_time) |
           effective_death_time > landmark_72h_time)]
  x[, post_72h_hospital_death :=
      at_risk_in_hospital_at_72h & hospital_expire_flag == 1L]
  x[, persistent_landmark_tuple :=
      persistent_ahrf & at_risk_in_hospital_at_72h &
        frozen_tuple_available_by_6h]
  x[, persistent_landmark_analysis_ready :=
      persistent_landmark_tuple & complete_no_gcs_core_by_6h]
  list(x = x, outcome_source_path = admissions_path)
}

build_eicu_landmark <- function(phenotype) {
  patient_path <- file.path(EICU_ROOT, "patient.csv.gz")
  patient <- fread(
    patient_path,
    select = c(
      "patientunitstayid", "hospitaldischargeoffset",
      "hospitaldischargestatus"
    ),
    showProgress = FALSE
  )
  if (anyDuplicated(patient$patientunitstayid)) {
    stop("eICU patient is not unique by patientunitstayid.")
  }
  patient[, hospitaldischargeoffset :=
      suppressWarnings(as.numeric(hospitaldischargeoffset))]
  patient[, hospital_status_normalized :=
      tolower(trimws(as.character(hospitaldischargestatus)))]
  x <- merge(
    copy(phenotype), patient, by = "patientunitstayid", all.x = TRUE
  )
  x[, landmark_72h_time := index_time + 72 * 60]
  x[, hospital_end_known := is.finite(hospitaldischargeoffset)]
  x[, hospital_status_known :=
      hospital_status_normalized %chin% c("alive", "expired")]
  x[, death_on_or_before_72h :=
      hospital_status_normalized == "expired" & hospital_end_known &
        hospitaldischargeoffset <= landmark_72h_time]
  x[, live_discharge_on_or_before_72h :=
      hospital_status_normalized == "alive" & hospital_end_known &
        hospitaldischargeoffset <= landmark_72h_time]
  x[, unknown_followup_at_72h :=
      (!hospital_end_known | !hospital_status_known) &
        !death_on_or_before_72h & !live_discharge_on_or_before_72h]
  x[, at_risk_in_hospital_at_72h :=
      hospital_end_known & hospital_status_known &
        hospitaldischargeoffset > landmark_72h_time]
  x[, post_72h_hospital_death :=
      at_risk_in_hospital_at_72h &
        hospital_status_normalized == "expired"]
  x[, persistent_landmark_tuple :=
      persistent_ahrf & at_risk_in_hospital_at_72h &
        frozen_tuple_available_by_6h]
  x[, persistent_landmark_analysis_ready :=
      persistent_landmark_tuple & complete_no_gcs_core_by_6h]
  list(x = x, outcome_source_path = patient_path)
}

mimic_lm <- build_mimic_landmark(mimic$phenotype)
eicu_lm <- build_eicu_landmark(eicu$phenotype)

mimic_audit_path <- file.path(
  out_private, "mimic_persistent_ahrf_72h_landmark_audit_v2.rds"
)
eicu_audit_path <- file.path(
  out_private, "eicu_persistent_ahrf_72h_landmark_audit_v2.rds"
)
saveRDS(mimic_lm$x, mimic_audit_path, compress = "xz")
saveRDS(eicu_lm$x, eicu_audit_path, compress = "xz")

make_flow <- function(x, database) {
  rbindlist(list(
    data.table(
      database = database,
      population = "broad_index",
      step = c(
        "broad_index",
        "all_three_24h_windows_observed",
        "persistent_ahrf_physiology"
      ),
      n = c(
        nrow(x),
        sum(x$all_three_windows_observed),
        sum(x$persistent_ahrf)
      )
    ),
    data.table(
      database = database,
      population = "persistent_ahrf_physiology",
      step = c(
        "death_on_or_before_72h",
        "live_discharge_on_or_before_72h",
        "unknown_followup_at_72h",
        "at_risk_in_hospital_at_72h",
        "at_risk_with_frozen_tuple_by_6h",
        "at_risk_with_tuple_and_complete_no_gcs_core",
        "post_72h_in_hospital_deaths_in_analysis_ready_set"
      ),
      n = c(
        sum(x$persistent_ahrf & x$death_on_or_before_72h),
        sum(x$persistent_ahrf & x$live_discharge_on_or_before_72h),
        sum(x$persistent_ahrf & x$unknown_followup_at_72h),
        sum(x$persistent_ahrf & x$at_risk_in_hospital_at_72h),
        sum(x$persistent_landmark_tuple),
        sum(x$persistent_landmark_analysis_ready),
        sum(
          x$persistent_landmark_analysis_ready &
            x$post_72h_hospital_death
        )
      )
    )
  ))
}

flow <- rbindlist(list(
  make_flow(mimic_lm$x, "MIMIC-IV"),
  make_flow(eicu_lm$x, "eICU-CRD")
))
fwrite(
  flow,
  file.path(out_aggregate, "persistent_ahrf_72h_landmark_flow_v2.csv")
)

center_support <- rbindlist(lapply(
  c(
    "persistent_ahrf",
    "at_risk_in_hospital_at_72h",
    "persistent_landmark_tuple",
    "persistent_landmark_analysis_ready"
  ),
  function(flag) {
    eligible <- if (flag == "persistent_ahrf") {
      eicu_lm$x$persistent_ahrf
    } else if (flag == "at_risk_in_hospital_at_72h") {
      eicu_lm$x$persistent_ahrf &
        eicu_lm$x$at_risk_in_hospital_at_72h
    } else {
      eicu_lm$x[[flag]]
    }
    counts <- eicu_lm$x[
      eligible & !is.na(hospitalid), .N, by = hospitalid
    ]
    data.table(
      database = "eICU-CRD",
      tier = flag,
      patient_n = sum(eligible),
      hospital_n = nrow(counts),
      hospitals_ge5 = sum(counts$N >= 5),
      hospitals_ge10 = sum(counts$N >= 10),
      largest_hospital_n =
        if (nrow(counts)) max(counts$N) else NA_integer_,
      largest_hospital_percent =
        if (nrow(counts)) 100 * max(counts$N) / sum(counts$N) else NA_real_
    )
  }
))
fwrite(
  center_support,
  file.path(out_aggregate, "persistent_ahrf_eicu_hospital_support_v2.csv")
)

landmark_invariants <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    check = c(
      "analysis_ready_is_persistent",
      "analysis_ready_at_risk_after_72h",
      "analysis_ready_has_frozen_tuple",
      "analysis_ready_has_complete_core",
      "no_early_death_in_analysis_ready",
      "no_early_live_discharge_in_analysis_ready",
      "post_landmark_endpoint_binary",
      "phenotype_hash_unchanged_after_outcome_join"
    ),
    pass = c(
      all(mimic_lm$x[
        persistent_landmark_analysis_ready == TRUE, persistent_ahrf
      ]),
      all(mimic_lm$x[
        persistent_landmark_analysis_ready == TRUE,
        at_risk_in_hospital_at_72h
      ]),
      all(mimic_lm$x[
        persistent_landmark_analysis_ready == TRUE,
        frozen_tuple_available_by_6h
      ]),
      all(mimic_lm$x[
        persistent_landmark_analysis_ready == TRUE,
        complete_no_gcs_core_by_6h
      ]),
      !any(mimic_lm$x[
        persistent_landmark_analysis_ready == TRUE, death_on_or_before_72h
      ]),
      !any(mimic_lm$x[
        persistent_landmark_analysis_ready == TRUE,
        live_discharge_on_or_before_72h
      ]),
      all(
        as.integer(mimic_lm$x$post_72h_hospital_death) %in% c(0L, 1L)
      ),
      identical(
        sha256_file(mimic$paths[["phenotype"]]),
        outcome_blind_manifest[
          database == "MIMIC-IV" & artifact_role == "phenotype",
          sha256
        ]
      )
    )
  ),
  data.table(
    database = "eICU-CRD",
    check = c(
      "analysis_ready_is_persistent",
      "analysis_ready_at_risk_after_72h",
      "analysis_ready_has_frozen_tuple",
      "analysis_ready_has_complete_core",
      "no_early_death_in_analysis_ready",
      "no_early_live_discharge_in_analysis_ready",
      "post_landmark_endpoint_binary",
      "phenotype_hash_unchanged_after_outcome_join"
    ),
    pass = c(
      all(eicu_lm$x[
        persistent_landmark_analysis_ready == TRUE, persistent_ahrf
      ]),
      all(eicu_lm$x[
        persistent_landmark_analysis_ready == TRUE,
        at_risk_in_hospital_at_72h
      ]),
      all(eicu_lm$x[
        persistent_landmark_analysis_ready == TRUE,
        frozen_tuple_available_by_6h
      ]),
      all(eicu_lm$x[
        persistent_landmark_analysis_ready == TRUE,
        complete_no_gcs_core_by_6h
      ]),
      !any(eicu_lm$x[
        persistent_landmark_analysis_ready == TRUE, death_on_or_before_72h
      ]),
      !any(eicu_lm$x[
        persistent_landmark_analysis_ready == TRUE,
        live_discharge_on_or_before_72h
      ]),
      all(
        as.integer(eicu_lm$x$post_72h_hospital_death) %in% c(0L, 1L)
      ),
      identical(
        sha256_file(eicu$paths[["phenotype"]]),
        outcome_blind_manifest[
          database == "eICU-CRD" & artifact_role == "phenotype",
          sha256
        ]
      )
    )
  )
))
fwrite(
  landmark_invariants,
  file.path(out_qc, "persistent_ahrf_72h_landmark_invariants_v2.csv")
)
if (!all(landmark_invariants$pass)) {
  stop(
    "Persistent-AHRF landmark invariant failure: ",
    paste(
      landmark_invariants[
        pass == FALSE, paste(database, check, sep = ":")
      ],
      collapse = ", "
    )
  )
}

eicu_final <- eicu_lm$x[persistent_landmark_analysis_ready == TRUE]
eicu_event_n <- sum(eicu_final$post_72h_hospital_death)
eicu_hospital_n <- uniqueN(eicu_final$hospitalid[!is.na(
  eicu_final$hospitalid
)])
event_gate_pass <-
  eicu_event_n >= PERSISTENCE_LOCK$minimum_eicu_events
hospital_gate_pass <-
  eicu_hospital_n >= PERSISTENCE_LOCK$minimum_eicu_hospitals
analysis_permitted <- event_gate_pass && hospital_gate_pass
gate_status <- if (analysis_permitted) "PROCEED" else "STOP"

analysis_spec <- data.table(
  field = c(
    "label", "target_population", "prediction_landmark",
    "exposure_and_core", "outcome", "models_if_gate_passes",
    "uncertainty_if_eventually_run", "current_execution",
    "gate_status"
  ),
  value = c(
    PERSISTENCE_LOCK$label,
    paste(
      "patients in the frozen 6h common set who remain in hospital at",
      "index+72h and satisfy the locked three-window persistence rule"
    ),
    "index+72 hours; follow-up begins strictly after this time",
    paste(
      "reuse frozen index+6h first tuple and complete no-GCS core;",
      "no 72h tuple reselection and no 72h severity re-extraction"
    ),
    PERSISTENCE_LOCK$endpoint,
    paste(LOCKED_V2$model_ids, collapse = ", "),
    paste(
      "same primary internal/external framework only after a separate",
      "approved run; no bootstrap is run by this feasibility script"
    ),
    "feasibility, support, and stopping gate only",
    gate_status
  )
)
fwrite(
  analysis_spec,
  file.path(
    out_aggregate, "persistent_ahrf_72h_locked_analysis_spec_v2.csv"
  )
)

mimic_frame_path <- file.path(
  out_private, "mimic_persistent_ahrf_72h_analysis_frame_v2.rds"
)
eicu_frame_path <- file.path(
  out_private, "eicu_persistent_ahrf_72h_analysis_frame_v2.rds"
)
unlink(c(mimic_frame_path, eicu_frame_path), force = TRUE)

if (analysis_permitted) {
  mimic_common_path <- file.path(
    PRIVATE_ROOT, "model_ready",
    "mimic_primary_predictor_common_set_v2.rds"
  )
  eicu_common_path <- file.path(
    PRIVATE_ROOT, "model_ready",
    "eicu_primary_predictor_common_set_v2.rds"
  )
  mimic_common <- as.data.table(readRDS(mimic_common_path))
  eicu_common <- as.data.table(readRDS(eicu_common_path))

  mimic_frame <- merge(
    mimic_common,
    mimic_lm$x[
      persistent_landmark_analysis_ready == TRUE,
      .(
        subject_id, hadm_id, stay_id,
        in_hospital_mortality_after_72h_landmark =
          as.integer(post_72h_hospital_death)
      )
    ],
    by = c("subject_id", "hadm_id", "stay_id"), all = FALSE
  )
  eicu_frame <- merge(
    eicu_common,
    eicu_lm$x[
      persistent_landmark_analysis_ready == TRUE,
      .(
        patientunitstayid, hospitalid,
        in_hospital_mortality_after_72h_landmark =
          as.integer(post_72h_hospital_death)
      )
    ],
    by = c("patientunitstayid", "hospitalid"), all = FALSE
  )
  attr(mimic_frame, "rebuild_metadata") <- list(
    sensitivity = PERSISTENCE_LOCK$label,
    gate_status = gate_status,
    model_fit_performed = FALSE
  )
  attr(eicu_frame, "rebuild_metadata") <- list(
    sensitivity = PERSISTENCE_LOCK$label,
    gate_status = gate_status,
    model_fit_performed = FALSE
  )
  saveRDS(mimic_frame, mimic_frame_path, compress = "xz")
  saveRDS(eicu_frame, eicu_frame_path, compress = "xz")
}

gate_summary <- data.table(
  field = c(
    "status",
    "eicu_final_analysis_ready_n",
    "eicu_post_landmark_death_n",
    "eicu_minimum_required_post_landmark_deaths",
    "eicu_event_gate_pass",
    "eicu_contributing_hospital_n",
    "eicu_minimum_required_hospitals",
    "eicu_hospital_gate_pass",
    "outcome_model_or_bootstrap_run",
    "stop_reason"
  ),
  value = c(
    gate_status,
    as.character(nrow(eicu_final)),
    as.character(eicu_event_n),
    as.character(PERSISTENCE_LOCK$minimum_eicu_events),
    as.character(event_gate_pass),
    as.character(eicu_hospital_n),
    as.character(PERSISTENCE_LOCK$minimum_eicu_hospitals),
    as.character(hospital_gate_pass),
    "FALSE",
    if (analysis_permitted) {
      ""
    } else {
      paste(
        c(
          if (!event_gate_pass) {
            paste0(
              "post-landmark deaths ", eicu_event_n, " < ",
              PERSISTENCE_LOCK$minimum_eicu_events
            )
          },
          if (!hospital_gate_pass) {
            paste0(
              "contributing hospitals ", eicu_hospital_n, " < ",
              PERSISTENCE_LOCK$minimum_eicu_hospitals
            )
          }
        ),
        collapse = "; "
      )
    }
  )
)
fwrite(
  gate_summary,
  file.path(out_aggregate, "persistent_ahrf_72h_gate_summary_v2.csv")
)

input_paths <- c(
  script = script_path,
  config = file.path(dirname(script_path), "00_config.R"),
  replay_utils = file.path(
    dirname(script_path), "00_phase1_replay_utils.R"
  ),
  analysis_utils = file.path(dirname(script_path), "01_analysis_utils.R"),
  preread_lock = preread_lock_path,
  mimic_phase1_source = replay_manifest[
    database == "mimic", source_path
  ],
  eicu_phase1_source = replay_manifest[
    database == "eicu", source_path
  ],
  mimic_index = mimic$inputs[["index"]],
  mimic_tuple = mimic$inputs[["tuple"]],
  mimic_core = mimic$inputs[["core"]],
  eicu_index = eicu$inputs[["index"]],
  eicu_tuple = eicu$inputs[["tuple"]],
  eicu_core = eicu$inputs[["core"]],
  mimic_outcome_source = mimic_lm$outcome_source_path,
  eicu_outcome_source = eicu_lm$outcome_source_path
)
input_manifest <- data.table(
  role = names(input_paths),
  path = normalizePath(input_paths, mustWork = TRUE),
  sha256 = vapply(input_paths, sha256_file, character(1))
)
fwrite(
  input_manifest,
  file.path(out_qc, "persistent_ahrf_72h_input_manifest_v2.csv")
)

private_paths <- c(
  mimic$paths, eicu$paths,
  mimic_landmark_audit = mimic_audit_path,
  eicu_landmark_audit = eicu_audit_path
)
if (analysis_permitted) {
  private_paths <- c(
    private_paths,
    mimic_analysis_frame = mimic_frame_path,
    eicu_analysis_frame = eicu_frame_path
  )
}
private_manifest <- data.table(
  role = names(private_paths),
  path = normalizePath(private_paths, mustWork = TRUE),
  sha256 = vapply(private_paths, sha256_file, character(1))
)
fwrite(
  private_manifest,
  file.path(out_qc, "persistent_ahrf_72h_private_manifest_v2.csv")
)

gate <- data.table(
  field = c(
    "locked_config_version",
    "preread_lock_time",
    "preread_lock_sha256",
    "script_sha256",
    "v1_tree_unchanged",
    "outcome_blind_phenotype_published_before_outcome_read",
    "all_outcome_blind_invariants_pass",
    "all_landmark_invariants_pass",
    "persistent_definition_reproducible",
    "eicu_final_analysis_ready_n",
    "eicu_post_landmark_death_n",
    "eicu_event_gate_pass",
    "eicu_contributing_hospital_n",
    "eicu_hospital_gate_pass",
    "analysis_gate_status",
    "outcome_model_or_bootstrap_run",
    "completed_at"
  ),
  value = c(
    LOCKED_V2$version,
    "2026-07-17 00:59:43 +0800",
    sha256_file(preread_lock_path),
    sha256_file(script_path),
    as.character(all(replay_manifest$v1_output_tree_unchanged)),
    "TRUE",
    as.character(all(outcome_blind_invariants$pass)),
    as.character(all(landmark_invariants$pass)),
    "TRUE",
    as.character(nrow(eicu_final)),
    as.character(eicu_event_n),
    as.character(event_gate_pass),
    as.character(eicu_hospital_n),
    as.character(hospital_gate_pass),
    gate_status,
    "FALSE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
)
fwrite(gate, completion_tmp)
if (!file.rename(completion_tmp, completion_path)) {
  stop("Could not atomically publish persistent-AHRF completion gate.")
}

message("Persistent-AHRF 72-hour feasibility gate complete: ", gate_status)
message(
  "  eICU final set: ", nrow(eicu_final),
  "; post-landmark deaths: ", eicu_event_n,
  "; hospitals: ", eicu_hospital_n
)
if (!analysis_permitted) {
  message(
    "  STOP enforced. No outcome model, shorter window, or relaxed gate used."
  )
}
