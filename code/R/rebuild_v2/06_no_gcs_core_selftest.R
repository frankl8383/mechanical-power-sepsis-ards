#!/usr/bin/env Rscript

# Deterministic, patient-free rule checks for rebuild_v2 no-GCS extraction.

suppressPackageStartupMessages(library(data.table))

forbidden <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv", "status"),
  collapse = "|"
)

synthetic <- data.table(
  id = 1:4,
  age = c(60, 61, 62, 63),
  sex = c("Male", "Female", "Unknown", "Male"),
  pf_ratio = c(100, 120, 140, 160),
  map = c(40, 50, 60, NA),
  vasopressor = c(1L, 0L, 0L, 0L),
  platelet = c(100, 110, 120, 130),
  creatinine = c(1, 2, 3, 4)
)
synthetic[, sex_recognized := sex %chin% c("Male", "Female", "M", "F")]
synthetic[, sex_female := as.integer(sex %chin% c("Female", "F"))]
synthetic[, complete_no_gcs_core :=
  !is.na(age) & sex_recognized & !is.na(pf_ratio) &
    !is.na(map) & vasopressor %in% c(0L, 1L) &
    !is.na(platelet) & !is.na(creatinine)]

map_tie <- data.table(
  map_value = c(40, 40),
  source_rank = c(2L, 1L),
  source = c("map_noninvasive", "map_invasive")
)
setorder(map_tie, map_value, source_rank)

window <- data.table(
  start = c(0, 0, 0), finish = c(360, 360, 360),
  measurement = c(0, 360, 361), available = c(0, 360, 360)
)
window[, eligible :=
  measurement >= start & measurement <= finish & available <= finish]

checks <- c(
  unknown_sex_numeric_column_retained = synthetic[id == 3L, sex_female] == 0L,
  unknown_sex_excluded_from_complete_core =
    synthetic[id == 3L, complete_no_gcs_core] == FALSE,
  missing_map_excluded_from_complete_core =
    synthetic[id == 4L, complete_no_gcs_core] == FALSE,
  complete_rows_correct =
    identical(synthetic$complete_no_gcs_core, c(TRUE, TRUE, FALSE, FALSE)),
  invasive_map_wins_exact_tie = map_tie$source[[1L]] == "map_invasive",
  inclusive_window_and_availability =
    identical(window$eligible, c(TRUE, TRUE, FALSE)),
  clean_schema_passes_leakage_regex =
    !any(grepl(forbidden, names(synthetic), ignore.case = TRUE)),
  outcome_schema_fails_leakage_regex =
    any(grepl(forbidden, c(names(synthetic), "hospital_mortality"), ignore.case = TRUE))
)

if (any(checks != TRUE)) {
  stop(
    "NO_GCS_CORE_SELFTEST_FAILURE: ",
    paste(names(checks)[checks != TRUE], collapse = ", ")
  )
}
cat("REBUILD_V2_NO_GCS_CORE_SYNTHETIC_PASS\n")
