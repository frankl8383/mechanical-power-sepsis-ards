#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/18_missingness_sensitivity_utils_selftest.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "18_missingness_sensitivity_utils.R"))

make_frame <- function(database, n = 60L) {
  x <- seq_len(n)
  data.frame(
    database = database,
    analysis_id = paste0(substr(database, 1, 1), x),
    hospital_id = if (database == "MIMIC-IV") {
      rep("MIMIC_IV_SINGLE_CENTER", n)
    } else {
      paste0("H", 1L + (x %% 4L))
    },
    index_time_value = x * 1000,
    landmark_time_value = x * 1000 + 21600,
    tuple_available_time_value = x * 1000 + 1800,
    covariate_window_start_value = x * 1000 - 3600,
    covariate_window_end_value = x * 1000 + 21600,
    age = 30 + (x %% 50),
    sex_female = x %% 2,
    pf_ratio = 80 + x,
    map = 50 + (x %% 40),
    vasopressor = as.numeric(x %% 5 == 0),
    platelet = 100 + x,
    creatinine = 0.5 + x / 50,
    smp = 5 + x / 10,
    four_dprr = 50 + x,
    driving_pressure = 5 + (x %% 15),
    rr = 10 + (x %% 20),
    static_power = 2 + x / 30,
    dynamic_power = 1 + x / 40,
    resistive_power = 2 + x / 24,
    core_complete = TRUE,
    stringsAsFactors = FALSE
  )
}

mimic <- make_frame("MIMIC-IV")
mimic$smp <- mimic$static_power + mimic$dynamic_power + mimic$resistive_power
mimic$four_dprr <- 4 * mimic$driving_pressure + mimic$rr
mimic$map[c(2, 9)] <- NA_real_
mimic$platelet[c(3, 10)] <- NA_real_
mimic$creatinine[c(4, 11)] <- NA_real_
mimic$core_complete <- stats::complete.cases(mimic[v2_pm_model_columns])

eicu <- make_frame("eICU-CRD")
eicu$smp <- eicu$static_power + eicu$dynamic_power + eicu$resistive_power
eicu$four_dprr <- 4 * eicu$driving_pressure + eicu$rr
eicu$map[5] <- NA_real_
eicu$platelet[6] <- NA_real_
eicu$creatinine[7] <- NA_real_
eicu$core_complete <- stats::complete.cases(eicu[v2_pm_model_columns])

rule <- v2_mi_derive_rule(mimic)
mimic_imputed <- v2_mi_apply_rule(mimic, rule, "MIMIC-IV")
eicu_imputed <- v2_mi_apply_rule(eicu, rule, "eICU-CRD")
bundle <- v2_derive_transform_bundle(mimic_imputed)
design_columns <- lapply(LOCKED_V2$model_ids, function(model_id) {
  colnames(v2_mi_build_design(
    mimic_imputed, model_id, bundle, rule
  ))
})
external_columns <- lapply(LOCKED_V2$model_ids, function(model_id) {
  colnames(v2_mi_build_design(
    eicu_imputed, model_id, bundle, rule
  ))
})

novel <- eicu
novel$age[1] <- NA_real_
novel_stop <- inherits(try(
  v2_mi_apply_rule(novel, rule, "eICU-CRD"),
  silent = TRUE
), "try-error")

stopifnot(
  identical(rule$indicator_variables, c("map", "platelet", "creatinine")),
  identical(as.integer(rule$quantile_type), 2L),
  identical(rule$external_outcomes_used, FALSE),
  all(stats::complete.cases(mimic_imputed[v2_pm_model_columns])),
  all(stats::complete.cases(eicu_imputed[v2_pm_model_columns])),
  all(vapply(
    seq_along(design_columns),
    function(i) identical(design_columns[[i]], external_columns[[i]]),
    logical(1L)
  )),
  all(vapply(
    design_columns,
    function(columns) all(rule$indicator_columns %in% columns),
    logical(1L)
  )),
  novel_stop
)

cat("REBUILD_V2_MISSINGNESS_SENSITIVITY_UTILS_SYNTHETIC_PASS\n")
