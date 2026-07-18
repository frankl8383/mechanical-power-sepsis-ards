#!/usr/bin/env Rscript

# Synthetic-only tests. No project dataset or outcome artifact is opened.

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/09_primary_model_utils_selftest.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "01_analysis_utils.R"))
source(file.path(dirname(script_path), "03_internal_validation_utils.R"))
source(file.path(dirname(script_path), "04_external_validation_utils.R"))
source(file.path(dirname(script_path), "09_primary_model_utils.R"))

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop("Synthetic self-test failed: ", label)
  invisible(TRUE)
}

assert_error <- function(expression, pattern, label) {
  message <- tryCatch(
    {
      force(expression)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
  if (is.na(message) || !grepl(pattern, message, fixed = TRUE)) {
    stop(
      "Synthetic self-test failed: ", label,
      "; observed error: ", ifelse(is.na(message), "<none>", message)
    )
  }
  invisible(TRUE)
}

make_sources <- function(n, database, seed, missing_core_n = 0L) {
  set.seed(seed)
  age <- runif(n, 18, 92)
  sex_female <- rbinom(n, 1, 0.47)
  pf_ratio <- runif(n, 45, 299)
  map <- pmin(pmax(rnorm(n, 74, 13), 35), 140)
  vasopressor <- rbinom(n, 1, plogis(-1 + 0.015 * (75 - map)))
  platelet <- exp(rnorm(n, log(190), 0.42))
  creatinine <- pmin(pmax(exp(rnorm(n, log(1.1), 0.55)), 0.2), 12)
  peep <- sample(c(5, 8, 10, 12, 15), n, replace = TRUE)
  driving <- runif(n, 5, 28)
  resistive <- runif(n, 1, 15)
  rr <- runif(n, 8, 34)
  vt_l <- runif(n, 0.28, 0.75)
  static <- 0.098 * rr * vt_l * peep
  dynamic <- 0.098 * rr * vt_l * 0.5 * driving
  resistive_power <- 0.098 * rr * vt_l * resistive
  smp <- static + dynamic + resistive_power
  four <- 4 * driving + rr

  if (database == "MIMIC-IV") {
    start <- as.POSIXct("2100-01-01 00:00:00", tz = "UTC") +
      seq_len(n) * 86400
    index <- start + 3600
    tuple_time <- index + sample(60:21000, n, replace = TRUE)
    landmark <- index + 21600
    tuple <- data.frame(
      subject_id = 10000000 + seq_len(n),
      hadm_id = 20000000 + seq_len(n),
      stay_id = 30000000 + seq_len(n),
      index_time = index,
      landmark_time = landmark,
      covariate_window_start = start,
      covariate_window_end = landmark,
      ventilator_tuple_available_time = tuple_time,
      smp = smp,
      four_dprr = four,
      driving_pressure = driving,
      rr_value = rr,
      static_power = static,
      dynamic_power = dynamic,
      resistive_power = resistive_power,
      stringsAsFactors = FALSE
    )
    core <- data.frame(
      subject_id = tuple$subject_id,
      hadm_id = tuple$hadm_id,
      stay_id = tuple$stay_id,
      age = age,
      sex = ifelse(sex_female == 1, "F", "M"),
      sex_female = sex_female,
      index_pf = pf_ratio,
      map = map,
      vasopressor = vasopressor,
      platelets = platelet,
      creatinine = creatinine,
      stringsAsFactors = FALSE
    )
    outcome <- data.frame(
      subject_id = tuple$subject_id,
      hadm_id = tuple$hadm_id,
      stay_id = tuple$stay_id,
      stringsAsFactors = FALSE
    )
  } else {
    index <- sample(0:240, n, replace = TRUE)
    landmark <- index + 360
    tuple_time <- index + sample(1:359, n, replace = TRUE)
    tuple <- data.frame(
      patientunitstayid = 400000 + seq_len(n),
      hospitalid = rep(seq_len(max(4L, ceiling(n / 80))), length.out = n),
      index_time = index,
      landmark_time = landmark,
      covariate_window_start = pmax(0, index - 1440),
      covariate_window_end = landmark,
      ventilator_tuple_available_time = tuple_time,
      smp = smp,
      four_dprr = four,
      driving_pressure = driving,
      rr_value = rr,
      static_power = static,
      dynamic_power = dynamic,
      resistive_power = resistive_power,
      stringsAsFactors = FALSE
    )
    core <- data.frame(
      patientunitstayid = tuple$patientunitstayid,
      age = age,
      gender = ifelse(sex_female == 1, "Female", "Male"),
      sex_female = sex_female,
      index_pf = pf_ratio,
      map_value = map,
      vasopressor_any = vasopressor,
      platelet = platelet,
      creatinine_value = creatinine,
      stringsAsFactors = FALSE
    )
    outcome <- data.frame(
      patientunitstayid = tuple$patientunitstayid,
      hospitalid = tuple$hospitalid,
      stringsAsFactors = FALSE
    )
  }
  if (missing_core_n > 0L) {
    missing_rows <- seq_len(missing_core_n)
    if ("map" %in% names(core)) {
      core$map[missing_rows] <- NA_real_
    }
    if ("map_value" %in% names(core)) {
      core$map_value[missing_rows] <- NA_real_
    }
  }
  linear_predictor <- -2.0 +
    0.018 * (age - 55) -
    0.004 * (pf_ratio - 150) +
    0.35 * vasopressor +
    0.035 * (driving - 14) +
    0.015 * (rr - 18)
  death <- rbinom(n, 1, plogis(linear_predictor))
  if (length(unique(death)) != 2L) {
    death[seq_len(2L)] <- c(0L, 1L)
  }
  outcome$in_hospital_mortality_after_6h_landmark <- death
  list(tuple = tuple, core = core, outcome = outcome)
}

mimic_source <- make_sources(
  900L, "MIMIC-IV", 2026071711L, missing_core_n = 35L
)
mimic_all <- v2_pm_build_predictor_frame(
  mimic_source$tuple, mimic_source$core, "MIMIC-IV"
)
mimic_qc <- v2_pm_validate_predictor_frame(
  mimic_all, "MIMIC-IV", require_complete = FALSE
)
mimic_common <- v2_pm_complete_common_set(mimic_all, "MIMIC-IV")
assert_true(
  nrow(mimic_all) == 900L &&
    nrow(mimic_common) == 865L &&
    all(mimic_qc$range_qc$invalid_n == 0L) &&
    mimic_qc$timing_qc$pass,
  "MIMIC outcome-free common-set construction"
)

mimic_analysis <- v2_pm_join_outcome(
  mimic_common, mimic_source$outcome, "MIMIC-IV"
)
assert_true(
  identical(mimic_analysis$analysis_id, mimic_common$analysis_id) &&
    sum(mimic_analysis$outcome) > 0L &&
    sum(mimic_analysis$outcome == 0L) > 0L,
  "independent MIMIC outcome join preserves rows"
)

bundle <- v2_derive_transform_bundle(mimic_common)
fits <- v2_pm_fit_models(mimic_analysis, bundle)
fit_summary <- v2_pm_fit_summary(fits)
coefficients <- v2_pm_coefficient_table(
  fits, mimic_analysis, bundle
)
lrt <- v2_pm_likelihood_ratio_tests(fits)
collinearity <- v2_pm_collinearity_audits(mimic_common)
mimic_predictions <- v2_pm_predict_models(
  fits, mimic_common, bundle
)
internal_refit_contract <- v2_pm_internal_refit_contract_audit(
  mimic_analysis, "M_MP"
)
internal_dry_run <- v2_harrell_internal_validation(
  data = mimic_analysis,
  outcome = "outcome",
  fit_pipeline = v2_pm_internal_fit_factory("M_MP"),
  predict_pipeline = v2_pm_internal_predict_factory("M_MP"),
  repetitions = 30L,
  seed = 2026071713L,
  minimum_success_fraction = 0.95,
  pipeline_id = "synthetic_M_MP_rederive_bundle_each_resample"
)
assert_true(
  identical(names(fits), v2_model_specification()$model_id) &&
    all(fit_summary$converged) &&
    nrow(lrt) == 2L &&
    all(is.finite(coefficients$standard_error)) &&
    nrow(collinearity$summary) == 2L &&
    identical(
      colnames(mimic_predictions),
      v2_model_specification()$model_id
    ),
  "locked primary model fitting outputs"
)
assert_true(
  internal_refit_contract$pass,
  "bootstrap transform bundle re-derived inside training callback"
)
if (!internal_dry_run$reportable) {
  print(internal_dry_run$failure_summary)
}
assert_true(
  internal_dry_run$reportable &&
    internal_dry_run$successful_replicates == 30L,
  "internal bootstrap callback dry-run"
)

eicu_source <- make_sources(
  480L, "eICU-CRD", 2026071712L, missing_core_n = 20L
)
eicu_all <- v2_pm_build_predictor_frame(
  eicu_source$tuple, eicu_source$core, "eICU-CRD"
)
eicu_common <- v2_pm_complete_common_set(eicu_all, "eICU-CRD")
eicu_analysis <- v2_pm_join_outcome(
  eicu_common, eicu_source$outcome, "eICU-CRD"
)
eicu_predictions <- v2_pm_predict_models(
  fits, eicu_common, bundle
)
long_predictions <- v2_pm_predictions_long(
  eicu_analysis, eicu_predictions, "eICU-CRD"
)
eicu_prediction_data <- data.frame(
  analysis_id = eicu_analysis$analysis_id,
  hospital_id = eicu_analysis$hospital_id,
  outcome = eicu_analysis$outcome,
  eicu_predictions,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
names(eicu_prediction_data)[
  seq.int(ncol(eicu_prediction_data) - ncol(eicu_predictions) + 1L,
          ncol(eicu_prediction_data))
] <- paste0("prediction_", colnames(eicu_predictions))
external_set <- v2_ev_prediction_set(
  eicu_prediction_data,
  "analysis_id",
  "outcome",
  "hospital_id",
  setNames(
    paste0("prediction_", colnames(eicu_predictions)),
    colnames(eicu_predictions)
  ),
  set_id = "synthetic_primary_driver"
)
driver_comparisons <- data.frame(
  candidate_model = c("M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY"),
  reference_model = rep("M0", 4L),
  stringsAsFactors = FALSE
)
external_dry_run <- v2_ev_cluster_bootstrap(
  external_set,
  comparisons = driver_comparisons,
  repetitions = 30L,
  seed = 2026071714L,
  minimum_success_fraction = 0.95
)
largest_dry_run <- v2_ev_largest_hospital_exclusion(
  external_set, driver_comparisons
)
equal_dry_run <- v2_ev_equal_hospital_performance(
  external_set, 10L, driver_comparisons
)
loho_dry_run <- v2_ev_leave_one_hospital_out(
  external_set, driver_comparisons
)
assert_true(
  nrow(eicu_common) == 460L &&
    nrow(eicu_predictions) == 460L &&
    nrow(long_predictions) ==
      460L * length(v2_model_specification()$model_id) &&
    identical(
      long_predictions$analysis_id[
        long_predictions$model_id == "M0"
      ],
      eicu_analysis$analysis_id
    ) &&
    external_dry_run$reportable &&
    largest_dry_run$retained_n < nrow(eicu_analysis) &&
    nrow(equal_dry_run$paired_differences) == 4L &&
    loho_dry_run$failed_hospitals == 0L,
  "unchanged MIMIC transformations and coefficients applied to eICU"
)

leaking <- mimic_source$core
leaking$mortality <- 0L
assert_error(
  v2_pm_build_predictor_frame(
    mimic_source$tuple, leaking, "MIMIC-IV"
  ),
  "Outcome-like predictor field",
  "outcome-like predictor field blocked"
)

misordered_outcome <- eicu_source$outcome
first_common_position <- match(
  eicu_common$analysis_id,
  as.character(misordered_outcome$patientunitstayid)
)[[1L]]
misordered_outcome$hospitalid[[first_common_position]] <-
  misordered_outcome$hospitalid[[first_common_position]] + 1000L
assert_error(
  v2_pm_join_outcome(
    eicu_common, misordered_outcome, "eICU-CRD"
  ),
  "hospital IDs disagree",
  "eICU outcome/hospital mismatch blocked"
)

missing_core_key <- mimic_source$core[-1L, , drop = FALSE]
assert_error(
  v2_pm_build_predictor_frame(
    mimic_source$tuple, missing_core_key, "MIMIC-IV"
  ),
  "lacks 1 required key",
  "missing no-GCS core patient blocked"
)

bad_range <- mimic_all
bad_range$smp[[1L]] <- 150
assert_error(
  v2_pm_validate_predictor_frame(
    bad_range, "MIMIC-IV", require_complete = FALSE
  ),
  "predictor range failure",
  "predictor range gate"
)

cat("REBUILD_V2_PRIMARY_MODEL_UTILS_SYNTHETIC_PASS\n")
