#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: locked configuration
# Freeze date: 2026-07-15
# This file is intentionally outcome-agnostic. Changes require an entry in
# docs/rebuild_v1/analysis_decision_log.md and a version increment.

options(stringsAsFactors = FALSE)

require_env_dir <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    stop(
      "Environment variable ", name,
      " must point to an accessible local data directory."
    )
  }
  normalizePath(value, mustWork = TRUE)
}

PROJECT_ROOT <- normalizePath(
  Sys.getenv(
    "ARDS_MP_PROJECT_ROOT",
    unset = getwd()
  ),
  mustWork = TRUE
)

MIMIC_ROOT <- require_env_dir("MIMIC_IV_DIR")

EICU_ROOT <- require_env_dir("EICU_CRD_DIR")

REBUILD_ROOT <- file.path(PROJECT_ROOT, "analysis_rebuild_v1")
PRIVATE_ROOT <- file.path(REBUILD_ROOT, "private")
AGGREGATE_ROOT <- file.path(REBUILD_ROOT, "aggregate")
QC_ROOT <- file.path(REBUILD_ROOT, "qc")

for (d in c(REBUILD_ROOT, PRIVATE_ROOT, AGGREGATE_ROOT, QC_ROOT)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

LOCKED <- list(
  version = "1.0.1",
  freeze_date = as.Date("2026-07-15"),
  minimum_age_years = 18,
  first_qualifying_stay_per_patient = TRUE,
  primary_outcome = "in_hospital_mortality",
  secondary_outcomes = c("mimic_28_day_mortality", "icu_mortality"),
  pf_threshold_mmHg = 300,
  minimum_index_peep_cmH2O = 5,
  pao2_fio2_pair_window_minutes = 120,
  pao2_peep_pair_window_minutes = 120,
  infection_window_hours_before_index = 48,
  # Primary prediction cohort requires infection evidence to be available by
  # the hypoxemia index. A +24 h retrospective phenotype is sensitivity only.
  infection_window_hours_after_index = 0,
  sensitivity_infection_window_hours_after_index = 24,
  primary_exposure_window_hours_after_index = 6,
  primary_ventilator_tuple_pair_window_minutes = 60,
  sensitivity_ventilator_tuple_pair_window_minutes = 30,
  mp_effect_unit_J_per_min = 5,
  mp_formula = "0.098 * RR * (VT_mL / 1000) * (Ppeak - 0.5 * (Pplat - PEEP))",
  physiologic_ranges = list(
    pao2_mmHg = c(20, 700),
    fio2_percent = c(21, 100),
    peep_cmH2O = c(5, 30),
    plateau_cmH2O = c(5, 60),
    peak_cmH2O = c(5, 80),
    tidal_volume_mL = c(100, 1500),
    respiratory_rate_per_min = c(5, 60),
    driving_pressure_cmH2O = c(0, 40),
    surrogate_mp_J_per_min = c(0, 100)
  ),
  physiologic_ordering = "Ppeak >= Pplat >= PEEP",
  primary_exposure_summary = "first_valid_complete_tuple",
  secondary_exposure_summary = "median_valid_tuples_with_24h_landmark",
  external_validation_rule = paste(
    "All coefficients, transformations, standardization constants, knots,",
    "imputation rules, and model forms are frozen in MIMIC-IV before eICU outcome evaluation."
  )
)

stopifnot(
  LOCKED$minimum_age_years == 18,
  LOCKED$pf_threshold_mmHg == 300,
  LOCKED$minimum_index_peep_cmH2O == 5,
  LOCKED$primary_outcome == "in_hospital_mortality"
)
