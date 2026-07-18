#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/22_complete_gcs_utils_selftest.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "22_complete_gcs_utils.R"))

base_time <- as.POSIXct("2200-01-02 00:00:00", tz = "UTC")
mimic_target <- data.frame(
  stay_id = c(1L, 2L),
  intime = base_time - 3600,
  index_time = base_time,
  landmark_time = base_time + 6 * 3600,
  covariate_window_start = base_time - 3600,
  covariate_window_end = base_time + 6 * 3600
)
mimic_raw <- data.frame(
  stay_id = c(
    rep(1L, 3L), rep(1L, 3L), rep(2L, 4L)
  ),
  charttime = format(
    c(
      rep(base_time + 600, 3L),
      rep(base_time + 1200, 3L),
      rep(base_time + 600, 4L)
    ),
    "%Y-%m-%d %H:%M:%S", tz = "UTC"
  ),
  storetime = format(
    c(
      rep(base_time + 700, 3L),
      rep(base_time + 1300, 3L),
      rep(base_time + 700, 4L)
    ),
    "%Y-%m-%d %H:%M:%S", tz = "UTC"
  ),
  itemid = c(
    220739, 223900, 223901,
    220739, 223900, 223901,
    220739, 220739, 223900, 223901
  ),
  value = c(
    "No Response", "No Response", "No Response",
    "Spontaneously", "Oriented", "Obeys Commands",
    "2 To Pain", "3 To Speech", "Confused", "Obeys Commands"
  ),
  valuenum = c(1, 1, 1, 4, 5, 6, 2, 3, 4, 6)
)
mimic <- v2_cg_derive_mimic(mimic_raw, mimic_target)
stopifnot(
  nrow(mimic$selected) == 1L,
  mimic$selected$stay_id[[1L]] == "1",
  mimic$selected$gcs[[1L]] == 3,
  mimic$timing_qc$duplicate_component_time_conflict_groups == 1L
)

parsed <- v2_cg_parse_mimic_components(data.frame(
  stay_id = 1L,
  charttime = "2200-01-02 01:00:00",
  storetime = "2200-01-02 01:01:00",
  itemid = 223900L,
  value = "ET/Trach",
  valuenum = 1
))
stopifnot(
  parsed$unscorable_airway_text,
  is.na(parsed$component_value)
)

eicu_target <- data.frame(
  patientunitstayid = c(10L, 11L),
  index_time = c(100, 100),
  landmark_time = c(460, 460),
  covariate_window_start = c(0, 0),
  covariate_window_end = c(460, 460)
)
eicu_raw <- data.frame(
  patientunitstayid = c(
    10L, 10L, 10L, 10L, 11L, 11L, 11L
  ),
  nursingchartoffset = c(120, 130, 130, 130, 120, 120, 120),
  nursingchartentryoffset = c(125, 135, 135, 135, NA, NA, NA),
  nursingchartcelltypevallabel = c(
    "Glasgow coma score",
    rep("Glasgow coma score", 6L)
  ),
  nursingchartcelltypevalname = c(
    "GCS Total", "Eyes", "Verbal", "Motor",
    "Eyes", "Verbal", "Motor"
  ),
  nursingchartvalue = c(12, 1, 1, 1, 2, 3, 4)
)
eicu <- v2_cg_derive_eicu(eicu_raw, eicu_target)
stopifnot(
  nrow(eicu$selected) == 2L,
  eicu$selected$gcs[eicu$selected$patientunitstayid == "10"] == 12,
  grepl(
    "^explicit_total:",
    eicu$selected$gcs_source[
      eicu$selected$patientunitstayid == "10"
    ]
  ),
  eicu$selected$gcs[eicu$selected$patientunitstayid == "11"] == 9
)

set.seed(20260717)
n <- 200L
frame <- data.frame(
  age = seq(20, 90, length.out = n),
  sex_female = rep(c(0, 1), length.out = n),
  pf_ratio = seq(60, 290, length.out = n),
  gcs = rep(3:15, length.out = n),
  map = seq(35, 110, length.out = n),
  vasopressor = rep(c(0, 1, 0), length.out = n),
  platelet = seq(20, 500, length.out = n),
  creatinine = seq(0.2, 8, length.out = n),
  smp = seq(2, 40, length.out = n),
  four_dprr = seq(30, 180, length.out = n),
  driving_pressure = seq(2, 35, length.out = n),
  rr = seq(6, 50, length.out = n),
  static_power = seq(1, 12, length.out = n),
  dynamic_power = seq(0.5, 18, length.out = n),
  resistive_power = seq(0.2, 10, length.out = n)
)
bundle <- v2_cg_derive_transform_bundle(frame)
designs <- lapply(
  v2_model_specification()$model_id,
  function(model_id) v2_cg_build_design(frame, model_id, bundle)
)
stopifnot(
  all(vapply(designs, function(x) !anyNA(x), logical(1L))),
  all(vapply(designs, function(x) "gcs_rcs1" %in% colnames(x), logical(1L))),
  all(vapply(
    bundle$baseline_three_knots,
    function(x) length(x) == 3L && all(diff(x) > 0),
    logical(1L)
  ))
)

message("REBUILD_V2_COMPLETE_GCS_UTILS_SELFTEST_PASS")
