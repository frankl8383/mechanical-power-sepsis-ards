#!/usr/bin/env Rscript

# rebuild_v2 Phase 1E:
# - impose a fixed index+6 h hospital-risk landmark;
# - audit early death, early live discharge, unknown follow-up, ICU exit, and
#   ventilator-tuple nonmeasurement separately;
# - create outcome-free no-GCS core target objects with the locked
#   index-24 h (bounded by ICU entry) through landmark covariate windows;
# - keep post-landmark outcomes in separate private artifacts.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/05_build_fixed_landmark_flow.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))
source(file.path(dirname(script_path), "00_phase1_replay_utils.R"))
source(file.path(dirname(script_path), "01_analysis_utils.R"))

set.seed(LOCKED_V2$bootstrap$seed_sensitivity)

out_qc <- file.path(QC_ROOT, "fixed_landmark")
dir.create(out_qc, recursive = TRUE, showWarnings = FALSE)

as_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = "UTC"))
  as.POSIXct(x, tz = "UTC")
}

add_ventilator_representations <- function(x, database) {
  observed <- x[tuple_observed == TRUE]
  if (!nrow(observed)) stop(database, " has no observed ventilator tuples.")
  source_smp <- observed$smp
  derived <- v2_derive_ventilator_representations(
    as.data.frame(observed),
    plateau = "pplat",
    peak = "ppeak_value",
    peep = "peep_value",
    tidal_volume = "vt_value",
    respiratory_rate = "rr_value"
  )
  if (!all(derived$tuple_valid)) {
    stop(database, " selected tuple failed rebuild_v2 representation QC.")
  }
  if (any(abs(source_smp - derived$smp) > 1e-8)) {
    stop(database, " v1 selected sMP differs from the exact v2 recomputation.")
  }
  as.data.table(derived)
}

make_flow <- function(x, database) {
  data.table(
    database = database,
    step = c(
      "broad_index_cohort",
      "known_hospital_end_and_status",
      "death_on_or_before_6h_landmark",
      "live_discharge_on_or_before_6h_landmark",
      "other_or_unknown_before_landmark",
      "at_risk_with_known_outcome_at_6h_landmark",
      "valid_tuple_by_6h_landmark",
      "no_valid_tuple_by_6h_landmark",
      "no_tuple_and_left_icu_before_landmark",
      "no_tuple_with_unknown_icu_end",
      "no_tuple_despite_icu_observation_through_landmark",
      "post_landmark_in_hospital_deaths_all_at_risk",
      "post_landmark_in_hospital_deaths_with_valid_tuple"
    ),
    n = c(
      nrow(x),
      sum(x$hospital_end_known & x$hospital_status_known),
      sum(x$early_death),
      sum(x$early_live_discharge),
      sum(x$early_other_or_unknown_exit),
      sum(x$landmark_analysis_eligible),
      sum(x$landmark_analysis_eligible & x$tuple_observed),
      sum(x$landmark_analysis_eligible & !x$tuple_observed),
      sum(
        x$landmark_analysis_eligible & !x$tuple_observed &
          x$icu_end_known & !x$icu_observed_through_landmark
      ),
      sum(
        x$landmark_analysis_eligible & !x$tuple_observed &
          !x$icu_end_known
      ),
      sum(
        x$landmark_analysis_eligible & !x$tuple_observed &
          x$icu_observed_through_landmark
      ),
      sum(
        x$landmark_analysis_eligible &
          x$post_landmark_hospital_death
      ),
      sum(
        x$landmark_analysis_eligible & x$tuple_observed &
          x$post_landmark_hospital_death
      )
    )
  )
}

build_mimic_landmark <- function() {
  exposure_path <- file.path(
    PRIVATE_ROOT, "mimic", "mimic_paired_exposure_primary_60min_v2.rds"
  )
  if (!file.exists(exposure_path)) {
    stop("Run 03_build_mimic_paired_exposure.R first: ", exposure_path)
  }
  exposure <- as.data.table(readRDS(exposure_path))
  required <- c(
    "subject_id", "hadm_id", "stay_id", "index_time", "outtime",
    "tuple_observed", "prediction_time", "pplat", "ppeak_value",
    "peep_value", "vt_value", "rr_value", "smp"
  )
  v2_require_columns(exposure, required, "MIMIC paired exposure")

  admissions <- fread(
    file.path(MIMIC_ROOT, "hosp", "admissions.csv.gz"),
    select = c(
      "subject_id", "hadm_id", "dischtime", "deathtime",
      "hospital_expire_flag"
    ),
    showProgress = FALSE
  )
  admissions[, dischtime := as_utc(dischtime)]
  admissions[, deathtime := as_utc(deathtime)]
  if (anyDuplicated(admissions[, .(subject_id, hadm_id)])) {
    stop("MIMIC admissions has duplicate subject_id/hadm_id rows.")
  }
  x <- merge(
    exposure, admissions, by = c("subject_id", "hadm_id"), all.x = TRUE
  )
  x[, landmark_time := index_time + LOCKED_V2$landmark_hours * 3600]
  x[, effective_death_time := deathtime]
  x[
    hospital_expire_flag == 1L & !is.na(dischtime) &
      (is.na(effective_death_time) | dischtime < effective_death_time),
    effective_death_time := dischtime
  ]
  x[, hospital_end_known := !is.na(dischtime)]
  x[, hospital_status_known := hospital_expire_flag %in% c(0L, 1L)]
  x[, early_death :=
      hospital_expire_flag == 1L & !is.na(effective_death_time) &
        effective_death_time <= landmark_time]
  x[, early_live_discharge :=
      hospital_expire_flag == 0L & !is.na(dischtime) &
        dischtime <= landmark_time]
  x[, early_other_or_unknown_exit :=
      (!hospital_end_known | !hospital_status_known |
         (!is.na(dischtime) & dischtime <= landmark_time)) &
        !early_death & !early_live_discharge]
  x[, at_risk_at_landmark :=
      hospital_end_known & dischtime > landmark_time &
        (is.na(effective_death_time) | effective_death_time > landmark_time)]
  x[, landmark_analysis_eligible :=
      at_risk_at_landmark & hospital_status_known]
  x[, icu_end_known := !is.na(outtime)]
  x[, icu_observed_through_landmark :=
      icu_end_known & outtime >= landmark_time]
  x[, post_landmark_hospital_death :=
      landmark_analysis_eligible & hospital_expire_flag == 1L]

  if (any(
    x$tuple_observed & !is.na(x$prediction_time) &
      x$prediction_time > x$landmark_time
  )) {
    stop("A MIMIC selected tuple became available after the fixed landmark.")
  }
  x[, flow_category := fcase(
    early_death, "early_death",
    early_live_discharge, "early_live_discharge",
    early_other_or_unknown_exit, "early_other_or_unknown_exit",
    landmark_analysis_eligible & tuple_observed, "landmark_with_tuple",
    landmark_analysis_eligible & !tuple_observed &
      icu_observed_through_landmark, "landmark_no_tuple_icu_observed",
    landmark_analysis_eligible & !tuple_observed &
      icu_end_known & !icu_observed_through_landmark,
    "landmark_no_tuple_left_icu",
    landmark_analysis_eligible & !tuple_observed & !icu_end_known,
    "landmark_no_tuple_unknown_icu_end",
    default = "not_analysis_eligible_other"
  )]

  final <- add_ventilator_representations(
    x[landmark_analysis_eligible & tuple_observed == TRUE],
    "MIMIC"
  )
  final[, `:=`(
    age = age_at_admission,
    sex = gender,
    index_pf = pf_ratio,
    covariate_window_start = pmax(intime, index_time - 24 * 3600),
    covariate_window_end = landmark_time,
    ventilator_tuple_available_time = prediction_time
  )]

  private_dir <- file.path(PRIVATE_ROOT, "mimic")
  audit_path <- file.path(
    private_dir, "mimic_fixed6h_landmark_eligibility_audit_v2.rds"
  )
  selection_target_path <- file.path(
    private_dir, "mimic_all_landmark_at_risk_selection_targets_v2.rds"
  )
  target_path <- file.path(
    private_dir, "mimic_no_gcs_core_targets_v2.rds"
  )
  outcome_path <- file.path(
    private_dir, "mimic_fixed6h_landmark_outcomes_v2.rds"
  )
  saveRDS(x, audit_path, compress = "xz")

  selection_targets <- x[landmark_analysis_eligible == TRUE, .(
    subject_id, hadm_id, stay_id,
    age = age_at_admission, sex = gender,
    index_pf = pf_ratio, index_peep = peep_near_value,
    icu_intime = intime, icu_outtime = outtime,
    index_time, landmark_time,
    covariate_window_start = pmax(intime, index_time - 24 * 3600),
    covariate_window_end = landmark_time,
    tuple_observed, n_valid_tuples,
    tuple_nonmeasurement_reason = fcase(
      tuple_observed, "valid_tuple",
      !icu_end_known, "unknown_icu_end_without_valid_tuple",
      !icu_observed_through_landmark,
      "left_icu_before_landmark_without_valid_tuple",
      default = "no_valid_tuple_despite_icu_observation"
    )
  )]
  attr(selection_targets, "required_no_gcs_fields_to_extract") <- c(
    "map", "vasopressor", "platelet", "creatinine"
  )
  attr(selection_targets, "selection_weight_role") <- paste(
    "Outcome-free denominator for joint inclusion modeling:",
    "valid tuple plus complete no-GCS core."
  )
  saveRDS(selection_targets, selection_target_path, compress = "xz")

  target_keep <- c(
    "subject_id", "hadm_id", "stay_id", "intime", "age", "sex", "index_pf",
    "index_time", "landmark_time", "covariate_window_start",
    "covariate_window_end", "ventilator_tuple_available_time",
    "pplat", "ppeak_value", "peep_value", "vt_value", "rr_value",
    "vt_L", "driving_pressure", "resistive_pressure", "smp", "four_dprr",
    "static_power", "dynamic_power", "resistive_power",
    "energy_identity_error", "energy_identity_pass",
    "compliance_L_per_cmH2O", "smp_per_compliance",
    "first_careunit", "last_careunit"
  )
  target_keep <- target_keep[target_keep %in% names(final)]
  targets <- final[, ..target_keep]
  attr(targets, "required_no_gcs_fields_to_extract") <- c(
    "map", "vasopressor", "platelet", "creatinine"
  )
  attr(targets, "timing_rule") <- paste(
    "All no-GCS baseline predictors must be measured/documented from",
    "max(ICU intime, index-24h) through the fixed 6-hour landmark;",
    "external outcome information is forbidden from extraction or imputation."
  )
  saveRDS(targets, target_path, compress = "xz")

  outcomes <- final[, .(
    subject_id, hadm_id, stay_id,
    in_hospital_mortality_after_6h_landmark =
      as.integer(post_landmark_hospital_death)
  )]
  saveRDS(outcomes, outcome_path, compress = "xz")

  list(
    audit = x,
    selection_targets = selection_targets,
    targets = targets,
    outcomes = outcomes,
    paths = c(
      audit = audit_path,
      selection_targets = selection_target_path,
      targets = target_path,
      outcomes = outcome_path
    )
  )
}

build_eicu_landmark <- function() {
  exposure_path <- file.path(
    PRIVATE_ROOT, "eicu", "eicu_paired_exposure_primary_60min_v2.rds"
  )
  if (!file.exists(exposure_path)) {
    stop("Run 04_build_eicu_paired_exposure.R first: ", exposure_path)
  }
  exposure <- as.data.table(readRDS(exposure_path))
  required <- c(
    "patientunitstayid", "hospitalid", "index_time", "icu_end_offset",
    "tuple_observed", "prediction_time", "pplat", "ppeak_value",
    "peep_value", "vt_value", "rr_value", "smp"
  )
  v2_require_columns(exposure, required, "eICU paired exposure")

  patient <- fread(
    file.path(EICU_ROOT, "patient.csv.gz"),
    select = c(
      "patientunitstayid", "hospitaldischargeoffset",
      "hospitaldischargestatus", "unitdischargeoffset"
    ),
    showProgress = FALSE
  )
  if (anyDuplicated(patient$patientunitstayid)) {
    stop("eICU patient table has duplicate patientunitstayid rows.")
  }
  x <- merge(exposure, patient, by = "patientunitstayid", all.x = TRUE)
  x[, hospitaldischargeoffset :=
      suppressWarnings(as.numeric(hospitaldischargeoffset))]
  x[, unitdischargeoffset := suppressWarnings(as.numeric(unitdischargeoffset))]
  x[, hospital_status_normalized :=
      tolower(trimws(as.character(hospitaldischargestatus)))]
  x[, landmark_time := index_time + LOCKED_V2$landmark_hours * 60]
  x[, hospital_end_known := is.finite(hospitaldischargeoffset)]
  x[, hospital_status_known :=
      hospital_status_normalized %chin% c("alive", "expired")]
  x[, early_death :=
      hospital_status_normalized == "expired" & hospital_end_known &
        hospitaldischargeoffset <= landmark_time]
  x[, early_live_discharge :=
      hospital_status_normalized == "alive" & hospital_end_known &
        hospitaldischargeoffset <= landmark_time]
  x[, early_other_or_unknown_exit :=
      (!hospital_end_known | !hospital_status_known |
         (hospital_end_known & hospitaldischargeoffset <= landmark_time)) &
        !early_death & !early_live_discharge]
  x[, at_risk_at_landmark :=
      hospital_end_known & hospitaldischargeoffset > landmark_time]
  x[, landmark_analysis_eligible :=
      at_risk_at_landmark & hospital_status_known]
  x[, icu_end_known := is.finite(icu_end_offset)]
  x[, icu_observed_through_landmark :=
      icu_end_known & icu_end_offset >= landmark_time]
  x[, post_landmark_hospital_death :=
      landmark_analysis_eligible & hospital_status_normalized == "expired"]

  if (any(
    x$tuple_observed & is.finite(x$prediction_time) &
      x$prediction_time > x$landmark_time
  )) {
    stop("An eICU selected tuple became available after the fixed landmark.")
  }
  x[, flow_category := fcase(
    early_death, "early_death",
    early_live_discharge, "early_live_discharge",
    early_other_or_unknown_exit, "early_other_or_unknown_exit",
    landmark_analysis_eligible & tuple_observed, "landmark_with_tuple",
    landmark_analysis_eligible & !tuple_observed &
      icu_observed_through_landmark, "landmark_no_tuple_icu_observed",
    landmark_analysis_eligible & !tuple_observed &
      icu_end_known & !icu_observed_through_landmark,
    "landmark_no_tuple_left_icu",
    landmark_analysis_eligible & !tuple_observed & !icu_end_known,
    "landmark_no_tuple_unknown_icu_end",
    default = "not_analysis_eligible_other"
  )]

  final <- add_ventilator_representations(
    x[landmark_analysis_eligible & tuple_observed == TRUE],
    "eICU"
  )
  final[, `:=`(
    age = age_num,
    sex = gender,
    index_pf = pf_ratio,
    covariate_window_start = pmax(0, index_time - 24 * 60),
    covariate_window_end = landmark_time,
    ventilator_tuple_available_time = prediction_time
  )]

  private_dir <- file.path(PRIVATE_ROOT, "eicu")
  audit_path <- file.path(
    private_dir, "eicu_fixed6h_landmark_eligibility_audit_v2.rds"
  )
  selection_target_path <- file.path(
    private_dir, "eicu_all_landmark_at_risk_selection_targets_v2.rds"
  )
  target_path <- file.path(
    private_dir, "eicu_no_gcs_core_targets_v2.rds"
  )
  outcome_path <- file.path(
    private_dir, "eicu_fixed6h_landmark_outcomes_v2.rds"
  )
  saveRDS(x, audit_path, compress = "xz")

  selection_targets <- x[landmark_analysis_eligible == TRUE, .(
    patientunitstayid, patienthealthsystemstayid, person_key, hospitalid,
    age = age_num, sex = gender,
    index_pf = pf_ratio, index_peep = index_peep,
    icu_intime_offset = 0, icu_outtime_offset = icu_end_offset,
    index_time, landmark_time,
    covariate_window_start = pmax(0, index_time - 24 * 60),
    covariate_window_end = landmark_time,
    tuple_observed, n_valid_tuples,
    tuple_nonmeasurement_reason = fcase(
      tuple_observed, "valid_tuple",
      !icu_end_known, "unknown_icu_end_without_valid_tuple",
      !icu_observed_through_landmark,
      "left_icu_before_landmark_without_valid_tuple",
      default = "no_valid_tuple_despite_icu_observation"
    )
  )]
  attr(selection_targets, "required_no_gcs_fields_to_extract") <- c(
    "map", "vasopressor", "platelet", "creatinine"
  )
  attr(selection_targets, "selection_weight_role") <- paste(
    "Outcome-free denominator for joint inclusion modeling:",
    "valid tuple plus complete no-GCS core."
  )
  saveRDS(selection_targets, selection_target_path, compress = "xz")

  target_keep <- c(
    "patientunitstayid", "patienthealthsystemstayid", "person_key",
    "hospitalid", "age", "sex", "index_pf", "index_time", "landmark_time",
    "covariate_window_start", "covariate_window_end",
    "ventilator_tuple_available_time",
    "pplat", "ppeak_value", "peep_value", "vt_value", "rr_value",
    "vt_L", "driving_pressure", "resistive_pressure", "smp", "four_dprr",
    "static_power", "dynamic_power", "resistive_power",
    "energy_identity_error", "energy_identity_pass",
    "compliance_L_per_cmH2O", "smp_per_compliance"
  )
  target_keep <- target_keep[target_keep %in% names(final)]
  targets <- final[, ..target_keep]
  attr(targets, "required_no_gcs_fields_to_extract") <- c(
    "map", "vasopressor", "platelet", "creatinine"
  )
  attr(targets, "timing_rule") <- paste(
    "All no-GCS baseline predictors must be measured/documented from",
    "max(ICU offset 0, index-1440 min) through the fixed 6-hour landmark;",
    "external outcome information is forbidden from extraction or imputation."
  )
  saveRDS(targets, target_path, compress = "xz")

  outcomes <- final[, .(
    patientunitstayid, hospitalid,
    in_hospital_mortality_after_6h_landmark =
      as.integer(post_landmark_hospital_death)
  )]
  saveRDS(outcomes, outcome_path, compress = "xz")

  list(
    audit = x,
    selection_targets = selection_targets,
    targets = targets,
    outcomes = outcomes,
    paths = c(
      audit = audit_path,
      selection_targets = selection_target_path,
      targets = target_path,
      outcomes = outcome_path
    )
  )
}

mimic <- build_mimic_landmark()
eicu <- build_eicu_landmark()

flow <- rbindlist(list(
  make_flow(mimic$audit, "MIMIC-IV"),
  make_flow(eicu$audit, "eICU-CRD")
))
fwrite(flow, file.path(out_qc, "fixed6h_landmark_flow_v2.csv"))

measurement_reasons <- rbindlist(list(
  mimic$audit[
    landmark_analysis_eligible == TRUE & tuple_observed == FALSE,
    .N,
    by = .(reason = fifelse(
        icu_observed_through_landmark,
        "no_valid_tuple_despite_icu_observation",
        fifelse(
          icu_end_known,
          "left_icu_before_landmark_without_valid_tuple",
          "unknown_icu_end_without_valid_tuple"
        )
      ))
  ][, database := "MIMIC-IV"],
  eicu$audit[
    landmark_analysis_eligible == TRUE & tuple_observed == FALSE,
    .N,
    by = .(reason = fifelse(
        icu_observed_through_landmark,
        "no_valid_tuple_despite_icu_observation",
        fifelse(
          icu_end_known,
          "left_icu_before_landmark_without_valid_tuple",
          "unknown_icu_end_without_valid_tuple"
        )
      ))
  ][, database := "eICU-CRD"]
), use.names = TRUE, fill = TRUE)
setnames(measurement_reasons, "N", "n")
fwrite(
  measurement_reasons,
  file.path(out_qc, "fixed6h_tuple_nonmeasurement_reasons_v2.csv")
)

representation_qc <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    target_n = nrow(mimic$targets),
    maximum_energy_identity_error =
      max(abs(mimic$targets$energy_identity_error), na.rm = TRUE),
    all_energy_identities_pass =
      all(mimic$targets$energy_identity_pass)
  ),
  data.table(
    database = "eICU-CRD",
    target_n = nrow(eicu$targets),
    maximum_energy_identity_error =
      max(abs(eicu$targets$energy_identity_error), na.rm = TRUE),
    all_energy_identities_pass =
      all(eicu$targets$energy_identity_pass)
  )
))
fwrite(
  representation_qc,
  file.path(out_qc, "fixed6h_representation_invariants_v2.csv")
)

window_invariants <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    check = c(
      "window_starts_no_earlier_than_icu_entry",
      "window_starts_no_later_than_index",
      "window_ends_at_fixed_landmark",
      "window_duration_at_most_30h"
    ),
    pass = c(
      all(mimic$targets$covariate_window_start >= mimic$targets$intime),
      all(mimic$targets$covariate_window_start <= mimic$targets$index_time),
      all(mimic$targets$covariate_window_end == mimic$targets$landmark_time),
      all(as.numeric(difftime(
        mimic$targets$covariate_window_end,
        mimic$targets$covariate_window_start,
        units = "hours"
      )) <= 30 + 1e-8)
    )
  ),
  data.table(
    database = "eICU-CRD",
    check = c(
      "window_starts_no_earlier_than_icu_offset_zero",
      "window_starts_no_later_than_index",
      "window_ends_at_fixed_landmark",
      "window_duration_at_most_1800min"
    ),
    pass = c(
      all(eicu$targets$covariate_window_start >= 0),
      all(eicu$targets$covariate_window_start <= eicu$targets$index_time),
      all(eicu$targets$covariate_window_end == eicu$targets$landmark_time),
      all(
        eicu$targets$covariate_window_end -
          eicu$targets$covariate_window_start <= 1800 + 1e-8
      )
    )
  )
))
fwrite(
  window_invariants,
  file.path(out_qc, "fixed6h_covariate_window_invariants_v2.csv")
)
if (!all(window_invariants$pass)) {
  stop(
    "Fixed-landmark covariate-window invariant failure: ",
    paste(
      window_invariants[pass == FALSE, paste(database, check, sep = ":")],
      collapse = ", "
    )
  )
}

forbidden_outcome_name <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
selection_invariants <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    check = c(
      "one_row_per_landmark_subject",
      "selection_target_matches_all_at_risk",
      "tuple_targets_are_selection_target_subset",
      "selection_target_has_no_outcome_like_columns",
      "tuple_target_has_no_outcome_like_columns",
      "outcomes_match_tuple_targets",
      "post_landmark_outcome_is_binary"
    ),
    pass = c(
      !anyDuplicated(mimic$selection_targets$subject_id),
      nrow(mimic$selection_targets) ==
        sum(mimic$audit$landmark_analysis_eligible),
      setequal(
        mimic$targets$subject_id,
        mimic$selection_targets[tuple_observed == TRUE, subject_id]
      ),
      !any(grepl(
        forbidden_outcome_name, names(mimic$selection_targets),
        ignore.case = TRUE
      )),
      !any(grepl(
        forbidden_outcome_name, names(mimic$targets),
        ignore.case = TRUE
      )),
      setequal(mimic$outcomes$subject_id, mimic$targets$subject_id),
      all(
        mimic$outcomes$in_hospital_mortality_after_6h_landmark %in%
          c(0L, 1L)
      )
    )
  ),
  data.table(
    database = "eICU-CRD",
    check = c(
      "one_row_per_landmark_unit_stay",
      "selection_target_matches_all_at_risk",
      "tuple_targets_are_selection_target_subset",
      "selection_target_has_no_outcome_like_columns",
      "tuple_target_has_no_outcome_like_columns",
      "outcomes_match_tuple_targets",
      "post_landmark_outcome_is_binary"
    ),
    pass = c(
      !anyDuplicated(eicu$selection_targets$patientunitstayid),
      nrow(eicu$selection_targets) ==
        sum(eicu$audit$landmark_analysis_eligible),
      setequal(
        eicu$targets$patientunitstayid,
        eicu$selection_targets[
          tuple_observed == TRUE, patientunitstayid
        ]
      ),
      !any(grepl(
        forbidden_outcome_name, names(eicu$selection_targets),
        ignore.case = TRUE
      )),
      !any(grepl(
        forbidden_outcome_name, names(eicu$targets),
        ignore.case = TRUE
      )),
      setequal(
        eicu$outcomes$patientunitstayid,
        eicu$targets$patientunitstayid
      ),
      all(
        eicu$outcomes$in_hospital_mortality_after_6h_landmark %in%
          c(0L, 1L)
      )
    )
  )
))
fwrite(
  selection_invariants,
  file.path(out_qc, "fixed6h_selection_target_invariants_v2.csv")
)
if (!all(selection_invariants$pass)) {
  stop(
    "Fixed-landmark selection-target invariant failure: ",
    paste(
      selection_invariants[
        pass == FALSE, paste(database, check, sep = ":")
      ],
      collapse = ", "
    )
  )
}

center_summary <- function(hospital, population, database) {
  counts <- data.table(hospital = hospital)[
    !is.na(hospital), .N, by = hospital
  ]
  data.table(
    database = database,
    population = population,
    n = sum(counts$N),
    hospital_n = nrow(counts),
    largest_hospital_n = if (nrow(counts)) max(counts$N) else NA_integer_,
    largest_hospital_percent = if (nrow(counts)) {
      100 * max(counts$N) / sum(counts$N)
    } else {
      NA_real_
    },
    hospitals_ge5 = sum(counts$N >= 5),
    hospitals_ge10 = sum(counts$N >= 10),
    hospitals_ge30 = sum(counts$N >= 30)
  )
}
hospital_support <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    population = c("broad_index", "landmark_at_risk", "landmark_tuple"),
    n = c(
      nrow(mimic$audit),
      nrow(mimic$selection_targets),
      nrow(mimic$targets)
    ),
    hospital_n = 1L,
    largest_hospital_n = c(
      nrow(mimic$audit),
      nrow(mimic$selection_targets),
      nrow(mimic$targets)
    ),
    largest_hospital_percent = 100,
    hospitals_ge5 = 1L,
    hospitals_ge10 = 1L,
    hospitals_ge30 = 1L
  ),
  center_summary(
    eicu$audit$hospitalid, "broad_index", "eICU-CRD"
  ),
  center_summary(
    eicu$selection_targets$hospitalid, "landmark_at_risk", "eICU-CRD"
  ),
  center_summary(
    eicu$targets$hospitalid, "landmark_tuple", "eICU-CRD"
  )
), use.names = TRUE, fill = TRUE)
fwrite(
  hospital_support,
  file.path(out_qc, "fixed6h_hospital_support_v2.csv")
)

manifest <- data.table(
  field = c(
    "locked_config_version", "script_sha256", "landmark_hours",
    "mimic_target_sha256",
    "mimic_selection_target_sha256", "mimic_outcome_sha256",
    "eicu_target_sha256", "eicu_selection_target_sha256",
    "eicu_outcome_sha256",
    "all_energy_identities_pass", "completed_at"
  ),
  value = c(
    LOCKED_V2$version,
    sha256_file(script_path),
    as.character(LOCKED_V2$landmark_hours),
    sha256_file(mimic$paths[["targets"]]),
    sha256_file(mimic$paths[["selection_targets"]]),
    sha256_file(mimic$paths[["outcomes"]]),
    sha256_file(eicu$paths[["targets"]]),
    sha256_file(eicu$paths[["selection_targets"]]),
    sha256_file(eicu$paths[["outcomes"]]),
    as.character(all(representation_qc$all_energy_identities_pass)),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
)
fwrite(manifest, file.path(out_qc, "fixed6h_landmark_complete_v2.csv"))

message("Fixed 6-hour landmark flow complete.")
message("  MIMIC no-GCS extraction targets: ", nrow(mimic$targets))
message("  eICU no-GCS extraction targets: ", nrow(eicu$targets))
