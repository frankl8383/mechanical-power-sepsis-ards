#!/usr/bin/env Rscript

# Summarize the observed prediction-to-hospital-exit interval in the two
# primary analysis cohorts. This descriptive analysis does not refit or
# evaluate any outcome model; it completes reporting of the variable
# in-hospital follow-up horizon.

suppressPackageStartupMessages({
  library(data.table)
})

project_dir <- normalizePath(
  Sys.getenv("ARDS_PROJECT_DIR", unset = getwd()),
  winslash = "/",
  mustWork = TRUE
)

private_dir <- file.path(project_dir, "analysis_rebuild_v2", "private")
aggregate_dir <- file.path(
  project_dir, "analysis_rebuild_v2", "aggregate", "followup_duration"
)
qc_dir <- file.path(
  project_dir, "analysis_rebuild_v2", "qc", "followup_duration"
)
dir.create(aggregate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)

summarize_interval <- function(x, database, group_label) {
  stopifnot(nrow(x) > 0L, all(is.finite(x$followup_days)))
  data.table(
    database = database,
    outcome_group = group_label,
    n = nrow(x),
    followup_days_median = median(x$followup_days),
    followup_days_q1 = unname(quantile(x$followup_days, 0.25)),
    followup_days_q3 = unname(quantile(x$followup_days, 0.75)),
    followup_days_minimum = min(x$followup_days),
    followup_days_maximum = max(x$followup_days)
  )
}

mimic_flow <- as.data.table(readRDS(file.path(
  private_dir, "mimic", "mimic_fixed6h_landmark_eligibility_audit_v2.rds"
)))
mimic_core <- as.data.table(readRDS(file.path(
  private_dir, "mimic", "mimic_fixed6h_tuple_no_gcs_core_v2.rds"
)))[
  complete_no_gcs_core == TRUE,
  .(subject_id, hadm_id, stay_id)
]
mimic <- merge(
  mimic_core,
  mimic_flow[, .(
    subject_id,
    hadm_id,
    stay_id,
    landmark_time,
    hospital_exit_time = dischtime,
    post_landmark_hospital_death
  )],
  by = c("subject_id", "hadm_id", "stay_id"),
  all.x = TRUE,
  sort = FALSE
)
mimic[, followup_days := as.numeric(difftime(
  hospital_exit_time, landmark_time, units = "days"
))]

eicu_flow <- as.data.table(readRDS(file.path(
  private_dir, "eicu", "eicu_fixed6h_landmark_eligibility_audit_v2.rds"
)))
eicu_core <- as.data.table(readRDS(file.path(
  private_dir, "eicu", "eicu_fixed6h_tuple_no_gcs_core_v2.rds"
)))[
  complete_no_gcs_core == TRUE,
  .(patientunitstayid, hospitalid)
]
eicu <- merge(
  eicu_core,
  eicu_flow[, .(
    patientunitstayid,
    hospitalid,
    landmark_time,
    hospital_exit_time = hospitaldischargeoffset,
    post_landmark_hospital_death
  )],
  by = c("patientunitstayid", "hospitalid"),
  all.x = TRUE,
  sort = FALSE
)
eicu[, followup_days := (hospital_exit_time - landmark_time) / 1440]

stopifnot(
  nrow(mimic) == 9861L,
  nrow(eicu) == 1211L,
  all(mimic$post_landmark_hospital_death %in% c(FALSE, TRUE)),
  all(eicu$post_landmark_hospital_death %in% c(FALSE, TRUE)),
  all(is.finite(mimic$followup_days)),
  all(is.finite(eicu$followup_days)),
  all(mimic$followup_days > 0),
  all(eicu$followup_days > 0)
)

summary_rows <- rbindlist(list(
  summarize_interval(mimic, "MIMIC-IV", "All"),
  summarize_interval(
    mimic[post_landmark_hospital_death == FALSE],
    "MIMIC-IV",
    "Discharged alive"
  ),
  summarize_interval(
    mimic[post_landmark_hospital_death == TRUE],
    "MIMIC-IV",
    "In-hospital death"
  ),
  summarize_interval(eicu, "eICU-CRD", "All"),
  summarize_interval(
    eicu[post_landmark_hospital_death == FALSE],
    "eICU-CRD",
    "Discharged alive"
  ),
  summarize_interval(
    eicu[post_landmark_hospital_death == TRUE],
    "eICU-CRD",
    "In-hospital death"
  )
))

for (column in grep("^followup_days_", names(summary_rows), value = TRUE)) {
  set(summary_rows, j = column, value = round(summary_rows[[column]], 3))
}

qc <- data.table(
  database = c("MIMIC-IV", "eICU-CRD"),
  expected_primary_n = c(9861L, 1211L),
  observed_primary_n = c(nrow(mimic), nrow(eicu)),
  missing_interval_n = c(
    sum(!is.finite(mimic$followup_days)),
    sum(!is.finite(eicu$followup_days))
  ),
  nonpositive_interval_n = c(
    sum(mimic$followup_days <= 0, na.rm = TRUE),
    sum(eicu$followup_days <= 0, na.rm = TRUE)
  )
)
qc[, pass := (
  expected_primary_n == observed_primary_n &
    missing_interval_n == 0L &
    nonpositive_interval_n == 0L
)]
stopifnot(all(qc$pass))

fwrite(
  summary_rows,
  file.path(aggregate_dir, "primary_cohort_followup_duration_v2.csv")
)
fwrite(
  qc,
  file.path(qc_dir, "primary_cohort_followup_duration_qc_v2.csv")
)

message("Follow-up duration reporting summary: PASS")
