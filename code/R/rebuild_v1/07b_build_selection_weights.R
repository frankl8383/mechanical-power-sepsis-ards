#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: outcome-blind tuple-observation model
#
# This script estimates, separately in MIMIC-IV and eICU-CRD, the probability
# that a strict-cohort patient has a valid 0-6 h complete ventilator tuple.
# Only variables known by the hypoxemia index are used. Clinical outcomes,
# post-index ventilator components, discharge fields, and future administrative
# fields are never opened or copied into the published private artifacts.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/07b_build_selection_weights.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))

stopifnot(identical(LOCKED$version, "1.0.1"))

qc_out <- file.path(QC_ROOT, "selection_weights")
private_out <- file.path(PRIVATE_ROOT, "selection_weights")
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)

mimic_gate_path <- file.path(
  QC_ROOT, "mimic_severity", "phase2b_mimic_severity_complete_v1.csv"
)
eicu_gate_path <- file.path(
  QC_ROOT, "eicu_severity", "phase2b_complete_v1.csv"
)
mimic_phase2_path <- file.path(
  QC_ROOT, "mimic_exposure", "phase2_mimic_exposure_complete_v1.csv"
)
eicu_phase2_path <- file.path(
  QC_ROOT, "eicu_exposure", "phase2_eicu_exposure_complete_v1.csv"
)
mimic_input <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_index_known_selection_core_v1.rds"
)
eicu_input <- file.path(
  PRIVATE_ROOT, "eicu", "eicu_index_known_selection_core_v1.rds"
)

mimic_output <- file.path(
  private_out, "mimic_tuple_observation_weights_v1.rds"
)
eicu_output <- file.path(
  private_out, "eicu_tuple_observation_weights_v1.rds"
)
eicu_support_output <- file.path(
  private_out, "eicu_tuple_observation_weights_support_hospitals_v1.rds"
)
completion_gate <- file.path(qc_out, "phase2d_selection_weights_complete_v1.csv")
completion_gate_tmp <- paste0(completion_gate, ".tmp")

required_inputs <- c(
  mimic_gate_path, eicu_gate_path, mimic_phase2_path, eicu_phase2_path,
  mimic_input, eicu_input
)
if (any(!file.exists(required_inputs))) {
  stop(
    "Missing required input(s): ",
    paste(required_inputs[!file.exists(required_inputs)], collapse = ", ")
  )
}
if (file.exists(completion_gate)) {
  stop(
    "A published selection-weight gate already exists. A versioned amendment ",
    "is required before rebuilding it."
  )
}
unlink(completion_gate_tmp, force = TRUE)

sha256_file <- function(path) {
  out <- system2(
    "shasum", c("-a", "256", shQuote(path)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA256 failed for ", path, ": ", paste(out, collapse = " "))
  }
  hash <- strsplit(out[[1L]], "[[:space:]]+")[[1L]][1L]
  if (!grepl("^[0-9a-f]{64}$", hash)) stop("Invalid SHA256 for ", path)
  hash
}

read_wide_gate <- function(path) {
  z <- fread(path)
  if (nrow(z) != 1L || anyDuplicated(names(z))) {
    stop("Malformed single-row completion gate: ", path)
  }
  z
}

read_map_gate <- function(path) {
  z <- fread(path)
  if (!identical(names(z), c("field", "value")) || anyDuplicated(z$field)) {
    stop("Malformed field/value completion gate: ", path)
  }
  setNames(as.character(z$value), z$field)
}

require_wide <- function(gate, field, expected = NULL) {
  if (!field %in% names(gate)) stop("Gate lacks field: ", field)
  value <- as.character(gate[[field]][[1L]])
  if (!is.null(expected) && !identical(value, as.character(expected))) {
    stop("Gate mismatch for ", field, ": ", value, " != ", expected)
  }
  value
}

require_map <- function(gate, field, expected = NULL) {
  value <- unname(gate[field])
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("Gate lacks field: ", field)
  }
  if (!is.null(expected) && !identical(value, as.character(expected))) {
    stop("Gate mismatch for ", field, ": ", value, " != ", expected)
  }
  value
}

mimic_gate <- read_wide_gate(mimic_gate_path)
eicu_gate <- read_wide_gate(eicu_gate_path)
mimic_phase2 <- read_map_gate(mimic_phase2_path)
eicu_phase2 <- read_map_gate(eicu_phase2_path)

require_wide(mimic_gate, "status", "PASS")
require_wide(mimic_gate, "config_version", LOCKED$version)
require_wide(mimic_gate, "all_invariants_pass", "TRUE")
require_wide(mimic_gate, "outcome_leakage_guard_pass", "TRUE")
require_wide(eicu_gate, "status", "PASS")
require_wide(eicu_gate, "config_version", LOCKED$version)
require_map(mimic_phase2, "locked_config_version", LOCKED$version)
require_map(mimic_phase2, "all_invariants_pass", "TRUE")
require_map(mimic_phase2, "outcome_leakage_guard_pass", "TRUE")
require_map(eicu_phase2, "locked_config_version", LOCKED$version)
require_map(eicu_phase2, "all_invariants_pass", "TRUE")
require_map(eicu_phase2, "outcome_leakage_guard_pass", "TRUE")

mimic_severity_script <- file.path(
  dirname(script_path), "05_build_mimic_severity_core.R"
)
eicu_severity_script <- file.path(
  dirname(script_path), "06_build_eicu_severity_core.R"
)
require_wide(
  mimic_gate, "script_sha256", sha256_file(mimic_severity_script)
)
require_wide(
  eicu_gate, "script_sha256", sha256_file(eicu_severity_script)
)
require_wide(
  mimic_gate, "phase2_gate_sha256", sha256_file(mimic_phase2_path)
)
require_wide(
  eicu_gate, "phase2_gate_sha256", sha256_file(eicu_phase2_path)
)
require_wide(
  mimic_gate, "index_selection_rds_sha256", sha256_file(mimic_input)
)
require_wide(
  eicu_gate, "index_selection_rds_sha256", sha256_file(eicu_input)
)

mimic_source <- as.data.table(readRDS(mimic_input))
eicu_source <- as.data.table(readRDS(eicu_input))

required_mimic <- c(
  "stay_id", "subject_id", "intime", "index_time", "age_at_admission",
  "gender", "fio2_near_value", "peep_near_value", "pf_ratio",
  "gcs_worst", "map_min", "vasopressor_any", "platelet_min",
  "creatinine_max", "tuple_observed"
)
required_eicu <- c(
  "patientunitstayid", "person_key", "hospitalid", "index_time",
  "age_num_harmonized", "gender", "index_fio2", "index_peep",
  "pf_ratio", "gcs_worst", "map_min", "vasopressor_any",
  "platelet_min", "creatinine_max", "tuple_observed"
)
if (length(setdiff(required_mimic, names(mimic_source)))) {
  stop("MIMIC selection core lacks required canonical inputs.")
}
if (length(setdiff(required_eicu, names(eicu_source)))) {
  stop("eICU selection core lacks required canonical inputs.")
}
if (nrow(mimic_source) != as.integer(require_map(
  mimic_phase2, "strict_cohort_n"
)) || nrow(eicu_source) != as.integer(require_map(
  eicu_phase2, "strict_cohort_n"
))) {
  stop("Selection-core row count disagrees with the locked Phase 2 cohort.")
}
if (sum(mimic_source$tuple_observed == TRUE) != as.integer(require_map(
  mimic_phase2, "primary_60min_n"
)) || sum(eicu_source$tuple_observed == TRUE) != as.integer(require_map(
  eicu_phase2, "primary_60min_n"
))) {
  stop("Tuple-observation count disagrees with the locked Phase 2 cohort.")
}
if (anyDuplicated(mimic_source$stay_id) ||
    anyDuplicated(eicu_source$patientunitstayid)) {
  stop("Selection core is not unique by ICU stay.")
}

as_complete_numeric <- function(x, variable) {
  out <- suppressWarnings(as.numeric(x))
  bad <- !is.na(x) & is.na(out)
  if (any(bad)) stop("Non-numeric value in ", variable)
  if (any(!is.na(out) & !is.finite(out))) {
    stop("Non-finite value in ", variable)
  }
  out
}

make_mimic_frame <- function(x) {
  index_time_hours <- as.numeric(difftime(
    x$index_time, x$intime, units = "hours"
  ))
  out <- data.table(
    database = "MIMIC-IV_v3.1",
    source_stay_id = as.character(x$stay_id),
    source_patient_id = as.character(x$subject_id),
    source_hospital_id = NA_character_,
    tuple_observed = as.integer(x$tuple_observed),
    age_years = as_complete_numeric(x$age_at_admission, "MIMIC age"),
    sex_female = fcase(
      x$gender == "F", 1,
      x$gender == "M", 0,
      default = NA_real_
    ),
    pf_ratio = as_complete_numeric(x$pf_ratio, "MIMIC P/F"),
    index_peep = as_complete_numeric(x$peep_near_value, "MIMIC index PEEP"),
    index_fio2 = as_complete_numeric(x$fio2_near_value, "MIMIC index FiO2"),
    index_time_hours = index_time_hours,
    gcs = as_complete_numeric(x$gcs_worst, "MIMIC GCS"),
    map = as_complete_numeric(x$map_min, "MIMIC MAP"),
    platelet = as_complete_numeric(x$platelet_min, "MIMIC platelet"),
    creatinine = as_complete_numeric(x$creatinine_max, "MIMIC creatinine"),
    vasopressor = as.integer(x$vasopressor_any)
  )
  out
}

make_eicu_frame <- function(x) {
  out <- data.table(
    database = "eICU-CRD_v2.0",
    source_stay_id = as.character(x$patientunitstayid),
    source_patient_id = as.character(x$person_key),
    source_hospital_id = as.character(x$hospitalid),
    tuple_observed = as.integer(x$tuple_observed),
    age_years = as_complete_numeric(x$age_num_harmonized, "eICU age"),
    sex_female = fcase(
      x$gender == "Female", 1,
      x$gender == "Male", 0,
      default = NA_real_
    ),
    pf_ratio = as_complete_numeric(x$pf_ratio, "eICU P/F"),
    index_peep = as_complete_numeric(x$index_peep, "eICU index PEEP"),
    index_fio2 = as_complete_numeric(x$index_fio2, "eICU index FiO2"),
    index_time_hours = as_complete_numeric(x$index_time, "eICU index time") / 60,
    gcs = as_complete_numeric(x$gcs_worst, "eICU GCS"),
    map = as_complete_numeric(x$map_min, "eICU MAP"),
    platelet = as_complete_numeric(x$platelet_min, "eICU platelet"),
    creatinine = as_complete_numeric(x$creatinine_max, "eICU creatinine"),
    vasopressor = as.integer(x$vasopressor_any)
  )
  out
}

mimic <- make_mimic_frame(mimic_source)
eicu <- make_eicu_frame(eicu_source)
rm(mimic_source, eicu_source)
invisible(gc())

validate_canonical <- function(x, database) {
  if (anyNA(x$tuple_observed) || !all(x$tuple_observed %in% c(0L, 1L)) ||
      length(unique(x$tuple_observed)) != 2L) {
    stop("Invalid tuple-observation indicator in ", database)
  }
  if (anyDuplicated(x$source_stay_id) || anyNA(x$source_stay_id)) {
    stop("Invalid source ICU-stay key in ", database)
  }
  if (anyNA(x$source_patient_id)) stop("Missing patient key in ", database)
  if (anyNA(x$index_time_hours) || any(x$index_time_hours < 0)) {
    stop("Index time is missing or before ICU admission in ", database)
  }
  if (any(!is.na(x$sex_female) & !x$sex_female %in% c(0, 1)) ||
      any(!is.na(x$vasopressor) & !x$vasopressor %in% c(0, 1))) {
    stop("Binary index-known predictor is invalid in ", database)
  }
  invisible(TRUE)
}
validate_canonical(mimic, "MIMIC-IV")
validate_canonical(eicu, "eICU-CRD")

# Fixed common linear observation model. Units were fixed before fitting and
# are identical across databases. Imputation medians and coefficients are
# database-specific; no coefficient is transported between databases.
model_spec <- data.table(
  raw_variable = c(
    "age_years", "sex_female", "pf_ratio", "index_peep", "index_fio2",
    "index_time_hours", "gcs", "map", "platelet", "creatinine",
    "vasopressor"
  ),
  model_term = c(
    "age_per_10y", "sex_female", "pf_per_50", "index_peep_per_5",
    "index_fio2_per_10", "index_time_per_24h", "gcs",
    "map_per_10", "platelet_per_100", "creatinine", "vasopressor"
  ),
  divisor = c(10, 1, 50, 5, 10, 24, 1, 10, 100, 1, 1),
  imputation = "database-specific median",
  missing_indicator = TRUE,
  functional_form = "linear; no interaction"
)

weighted_mean <- function(x, w) sum(x * w) / sum(w)

fit_observation_model <- function(frame, database_label) {
  design <- data.table(row_index = seq_len(nrow(frame)))
  imputation <- vector("list", nrow(model_spec))
  missing_terms <- character()

  for (i in seq_len(nrow(model_spec))) {
    raw_name <- model_spec$raw_variable[[i]]
    term <- model_spec$model_term[[i]]
    scaled <- frame[[raw_name]] / model_spec$divisor[[i]]
    missing <- is.na(scaled)
    if (all(missing)) stop("All values missing for ", raw_name, " in ", database_label)
    median_value <- median(scaled, na.rm = TRUE)
    design[, (term) := fifelse(missing, median_value, scaled)]
    missing_term <- paste0(term, "_missing")
    design[, (missing_term) := as.integer(missing)]
    if (any(missing)) missing_terms <- c(missing_terms, missing_term)
    imputation[[i]] <- data.table(
      database = database_label,
      raw_variable = raw_name,
      model_term = term,
      divisor = model_spec$divisor[[i]],
      missing_n = sum(missing),
      missing_proportion = mean(missing),
      imputation_median_on_model_scale = median_value,
      missing_indicator_included = any(missing)
    )
  }
  design[, row_index := NULL]
  candidate_terms <- c(model_spec$model_term, missing_terms)
  nonconstant <- vapply(
    design[, ..candidate_terms],
    function(z) length(unique(z)) > 1L,
    logical(1L)
  )
  fit_terms <- candidate_terms[nonconstant]
  if (length(setdiff(model_spec$model_term, fit_terms))) {
    stop("A prespecified non-missing predictor is constant in ", database_label)
  }
  x <- as.matrix(design[, ..fit_terms])
  y <- frame$tuple_observed
  fit <- suppressWarnings(glm.fit(
    x = cbind(`(Intercept)` = 1, x),
    y = y,
    family = binomial()
  ))
  if (!fit$converged || anyNA(fit$coefficients) ||
      any(!is.finite(fit$coefficients))) {
    stop("Tuple-observation model failed in ", database_label)
  }
  probability <- as.numeric(fit$fitted.values)
  if (anyNA(probability) || any(!is.finite(probability)) ||
      any(probability <= 0 | probability >= 1)) {
    stop("Non-finite or boundary selection probability in ", database_label)
  }

  marginal_observed <- mean(y)
  weight_untruncated <- rep(NA_real_, length(y))
  weight_untruncated[y == 1L] <- marginal_observed / probability[y == 1L]
  observed_weights <- weight_untruncated[y == 1L]
  truncation <- as.numeric(quantile(
    observed_weights, probs = c(0.01, 0.99), type = 2,
    names = FALSE
  ))
  weight_truncated <- rep(NA_real_, length(y))
  weight_truncated[y == 1L] <- pmin(
    pmax(observed_weights, truncation[[1L]]), truncation[[2L]]
  )
  observed_truncated <- weight_truncated[y == 1L]
  ess <- sum(observed_truncated)^2 / sum(observed_truncated^2)

  coefficient <- data.table(
    database = database_label,
    term = names(fit$coefficients),
    coefficient = as.numeric(fit$coefficients),
    odds_ratio = exp(as.numeric(fit$coefficients))
  )

  imputation_qc <- rbindlist(imputation)
  design_qc <- rbindlist(lapply(names(design), function(term) {
    data.table(
      database = database_label,
      term = term,
      included_in_fit = term %in% fit_terms,
      minimum = min(design[[term]]),
      median = median(design[[term]]),
      maximum = max(design[[term]])
    )
  }))

  balance <- rbindlist(lapply(fit_terms, function(term) {
    z <- design[[term]]
    target_mean <- mean(z)
    target_sd <- sd(z)
    observed <- y == 1L
    unweighted_mean <- mean(z[observed])
    after_mean <- weighted_mean(z[observed], observed_truncated)
    data.table(
      database = database_label,
      term = term,
      target_strict_mean = target_mean,
      target_strict_sd = target_sd,
      observed_unweighted_mean = unweighted_mean,
      observed_weighted_mean = after_mean,
      smd_observed_vs_target_before = if (target_sd > 0) {
        (unweighted_mean - target_mean) / target_sd
      } else NA_real_,
      smd_observed_vs_target_after = if (target_sd > 0) {
        (after_mean - target_mean) / target_sd
      } else NA_real_
    )
  }))

  rank_decile <- pmin(
    10L,
    pmax(1L, ceiling(frank(probability, ties.method = "average") /
      length(probability) * 10))
  )
  probability_calibration <- data.table(
    database = database_label,
    decile = rank_decile,
    probability = probability,
    observed = y
  )[, .(
    n = .N,
    observed_n = sum(observed),
    observed_proportion = mean(observed),
    predicted_mean = mean(probability),
    predicted_minimum = min(probability),
    predicted_maximum = max(probability)
  ), by = .(database, decile)]

  q_probs <- c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)
  weight_distribution <- rbindlist(lapply(list(
    stabilized_untruncated = observed_weights,
    stabilized_truncated_1_99 = observed_truncated,
    selection_probability_all_strict = probability,
    selection_probability_observed = probability[y == 1L]
  ), function(z) {
    q <- as.numeric(quantile(z, probs = q_probs, type = 2, names = FALSE))
    data.table(
      probability = q_probs,
      value = q
    )
  }), idcol = "quantity")
  weight_distribution[, database := database_label]

  positivity <- data.table(
    database = database_label,
    strict_n = length(y),
    tuple_observed_n = sum(y),
    tuple_observed_proportion = marginal_observed,
    minimum_selection_probability = min(probability),
    maximum_selection_probability = max(probability),
    untruncated_weight_maximum = max(observed_weights),
    untruncated_weight_p99 = as.numeric(quantile(
      observed_weights, 0.99, type = 2
    )),
    truncation_p01 = truncation[[1L]],
    truncation_p99 = truncation[[2L]],
    truncated_weight_maximum = max(observed_truncated),
    effective_sample_size = ess,
    ess_fraction_of_observed = ess / sum(y),
    max_absolute_smd_before = max(
      abs(balance$smd_observed_vs_target_before), na.rm = TRUE
    ),
    max_absolute_smd_after = max(
      abs(balance$smd_observed_vs_target_after), na.rm = TRUE
    ),
    positivity_sensitive = (
      ess / sum(y) < 0.50 ||
      as.numeric(quantile(observed_weights, 0.99, type = 2)) > 10 ||
      max(observed_weights) > 20
    )
  )

  private <- copy(frame)
  private[, selection_probability := probability]
  private[, stabilized_weight_untruncated := weight_untruncated]
  private[, stabilized_weight_truncated_1_99 := weight_truncated]
  for (term in names(design)) {
    private[, (paste0("design_", term)) := design[[term]]]
  }
  attr(private, "rebuild_metadata") <- list(
    version = "tuple_observation_weights_v1",
    database = database_label,
    outcome_blind = TRUE,
    target = "valid complete ventilator tuple observed within index through 6 h",
    numerator = "database-specific marginal probability of tuple observation",
    truncation = "1st and 99th percentiles among tuple-observed records",
    model = paste(fit_terms, collapse = " + "),
    source_id_linkage = if (database_label == "MIMIC-IV_v3.1") {
      "source_stay_id is the exact character representation of stay_id"
    } else {
      paste(
        "source_stay_id is the exact character representation of",
        "patientunitstayid; source_hospital_id is hospitalid"
      )
    }
  )

  list(
    private = private,
    coefficient = coefficient,
    imputation = imputation_qc,
    design = design_qc,
    balance = balance,
    probability_calibration = probability_calibration,
    weight_distribution = weight_distribution,
    positivity = positivity,
    fit_terms = fit_terms
  )
}

mimic_fit <- fit_observation_model(mimic, "MIMIC-IV_v3.1")
eicu_fit <- fit_observation_model(eicu, "eICU-CRD_v2.0")

# eICU plateau-pressure documentation has a center-level support problem that
# a patient-level propensity model cannot repair. Preserve the full-target
# model as the common prespecified analysis, explicitly flag structural zero-
# observation hospitals, and separately refit the same covariate model in the
# empirically supported target of hospitals with at least one observed tuple.
# The supported-hospital restriction uses only the observation indicator and
# hospital ID; it never uses a clinical outcome.
eicu_hospital_support <- eicu[, .(
  strict_n = .N,
  tuple_observed_n = sum(tuple_observed),
  tuple_missing_n = sum(tuple_observed == 0L),
  tuple_observed_proportion = mean(tuple_observed)
), by = source_hospital_id]
eicu_hospital_support[, support_class := fcase(
  tuple_observed_n == 0L, "structural_zero_observed",
  tuple_missing_n == 0L, "all_observed",
  default = "mixed_support"
)]
support_hospitals <- eicu_hospital_support[
  tuple_observed_n > 0L, source_hospital_id
]
eicu_support <- eicu[source_hospital_id %chin% support_hospitals]
if (sum(eicu_support$tuple_observed) != sum(eicu$tuple_observed) ||
    any(eicu_hospital_support[
      support_class == "structural_zero_observed", tuple_observed_n
    ] != 0L)) {
  stop("eICU hospital-support restriction invariant failed.")
}
eicu_support_fit <- fit_observation_model(
  eicu_support, "eICU-CRD_v2.0_supported_hospitals"
)

mimic_fit$positivity[, `:=`(
  hospital_support_scope = "single-center; not applicable",
  hospital_n = 1L,
  structural_zero_observed_hospital_n = NA_integer_,
  structural_zero_observed_patient_n = NA_integer_
)]
eicu_fit$positivity[, `:=`(
  hospital_support_scope = "all strict-cohort hospitals",
  hospital_n = nrow(eicu_hospital_support),
  structural_zero_observed_hospital_n = eicu_hospital_support[
    support_class == "structural_zero_observed", .N
  ],
  structural_zero_observed_patient_n = eicu_hospital_support[
    support_class == "structural_zero_observed", sum(strict_n)
  ],
  positivity_sensitive = TRUE
)]
eicu_support_fit$positivity[, `:=`(
  hospital_support_scope = "hospitals with at least one observed tuple",
  hospital_n = length(support_hospitals),
  structural_zero_observed_hospital_n = 0L,
  structural_zero_observed_patient_n = 0L
)]

forbidden_private_pattern <- paste(c(
  "mort", "death", "dead", "expire", "discharge", "outcome", "surviv",
  "outtime", "end_offset", "prediction_time", "anchor", "pplat", "ppeak",
  "tidal", "respiratory_rate", "smp", "delta_p", "resistive"
), collapse = "|")
if (any(grepl(
  forbidden_private_pattern, names(mimic_fit$private), ignore.case = TRUE
)) || any(grepl(
  forbidden_private_pattern, names(eicu_fit$private), ignore.case = TRUE
))) {
  stop("Outcome-like, future, or post-index exposure field entered weight artifact.")
}

atomic_save_rds <- function(object, path) {
  tmp <- paste0(path, ".tmp")
  unlink(tmp, force = TRUE)
  saveRDS(object, tmp, compress = "xz")
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(tmp, path)) stop("Could not atomically publish ", path)
}

atomic_fwrite <- function(object, path) {
  tmp <- paste0(path, ".tmp")
  unlink(tmp, force = TRUE)
  fwrite(object, tmp)
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(tmp, path)) stop("Could not atomically publish ", path)
}

atomic_save_rds(mimic_fit$private, mimic_output)
atomic_save_rds(eicu_fit$private, eicu_output)
atomic_save_rds(eicu_support_fit$private, eicu_support_output)

model_spec_qc <- copy(model_spec)
model_spec_qc[, `:=`(
  population = "all strict index-cohort records",
  target = "tuple_observed",
  fit_scope = "separate database-specific logistic models"
)]
atomic_fwrite(model_spec_qc, file.path(qc_out, "selection_model_specification.csv"))
atomic_fwrite(
  rbindlist(list(
    mimic_fit$coefficient, eicu_fit$coefficient,
    eicu_support_fit$coefficient
  )),
  file.path(qc_out, "selection_model_coefficients.csv")
)
atomic_fwrite(
  rbindlist(list(
    mimic_fit$imputation, eicu_fit$imputation,
    eicu_support_fit$imputation
  )),
  file.path(qc_out, "selection_model_imputation_QC.csv")
)
atomic_fwrite(
  rbindlist(list(
    mimic_fit$design, eicu_fit$design,
    eicu_support_fit$design
  )),
  file.path(qc_out, "selection_model_design_QC.csv")
)
atomic_fwrite(
  rbindlist(list(
    mimic_fit$balance, eicu_fit$balance,
    eicu_support_fit$balance
  )),
  file.path(qc_out, "selection_weight_balance_QC.csv")
)
atomic_fwrite(
  rbindlist(list(
    mimic_fit$probability_calibration, eicu_fit$probability_calibration,
    eicu_support_fit$probability_calibration
  )),
  file.path(qc_out, "selection_probability_decile_QC.csv")
)
atomic_fwrite(
  rbindlist(list(
    mimic_fit$weight_distribution, eicu_fit$weight_distribution,
    eicu_support_fit$weight_distribution
  ), use.names = TRUE),
  file.path(qc_out, "selection_weight_distribution_QC.csv")
)
positivity <- rbindlist(list(
  mimic_fit$positivity, eicu_fit$positivity,
  eicu_support_fit$positivity
), fill = TRUE)
atomic_fwrite(
  positivity,
  file.path(qc_out, "selection_weight_positivity_QC.csv")
)
atomic_fwrite(
  eicu_hospital_support[order(
    support_class, -strict_n, source_hospital_id
  )],
  file.path(qc_out, "eicu_hospital_tuple_support_QC.csv")
)

leakage_guard <- data.table(
  check = c(
    "canonical_input_allowlist_only",
    "private_outputs_have_no_outcome_like_fields",
    "private_outputs_have_no_post_index_exposure_fields",
    "clinical_outcome_tables_not_opened",
    "both_models_use_identical_raw_predictor_specification",
    "both_models_are_linear_without_interactions",
    "weights_exist_only_for_tuple_observed_records",
    "all_selection_probabilities_strictly_between_zero_and_one",
    "eicu_full_target_structural_support_failure_flagged",
    "supported_hospital_refit_retains_every_observed_tuple"
  ),
  pass = c(
    TRUE,
    !any(grepl(
      paste(c("mort", "death", "expire", "discharge", "outcome", "surviv"),
        collapse = "|"),
      c(
        names(mimic_fit$private), names(eicu_fit$private),
        names(eicu_support_fit$private)
      ),
      ignore.case = TRUE
    )),
    !any(grepl(
      paste(c(
        "prediction_time", "anchor", "pplat", "ppeak", "tidal",
        "respiratory_rate", "smp", "delta_p", "resistive", "outtime",
        "end_offset"
      ), collapse = "|"),
      c(
        names(mimic_fit$private), names(eicu_fit$private),
        names(eicu_support_fit$private)
      ),
      ignore.case = TRUE
    )),
    TRUE,
    identical(model_spec$raw_variable, c(
      "age_years", "sex_female", "pf_ratio", "index_peep", "index_fio2",
      "index_time_hours", "gcs", "map", "platelet", "creatinine",
      "vasopressor"
    )),
    all(model_spec$functional_form == "linear; no interaction"),
    all(is.na(mimic_fit$private$stabilized_weight_truncated_1_99) ==
      (mimic_fit$private$tuple_observed == 0L)) &&
      all(is.na(eicu_fit$private$stabilized_weight_truncated_1_99) ==
        (eicu_fit$private$tuple_observed == 0L)) &&
      all(is.na(
        eicu_support_fit$private$stabilized_weight_truncated_1_99
      ) == (eicu_support_fit$private$tuple_observed == 0L)),
    all(mimic_fit$private$selection_probability > 0 &
      mimic_fit$private$selection_probability < 1) &&
      all(eicu_fit$private$selection_probability > 0 &
        eicu_fit$private$selection_probability < 1) &&
      all(eicu_support_fit$private$selection_probability > 0 &
        eicu_support_fit$private$selection_probability < 1),
    eicu_fit$positivity$positivity_sensitive &&
      eicu_fit$positivity$structural_zero_observed_hospital_n > 0L,
    sum(eicu_support_fit$private$tuple_observed) ==
      sum(eicu_fit$private$tuple_observed)
  )
)
if (any(!leakage_guard$pass)) stop("Selection-weight leakage/invariant guard failed.")
atomic_fwrite(
  leakage_guard,
  file.path(qc_out, "selection_weight_leakage_guard.csv")
)

summary_lines <- c(
  "# Outcome-blind complete-tuple observation weighting QC",
  "",
  paste0("- Locked configuration: ", LOCKED$version),
  paste0(
    "- MIMIC strict / tuple observed: ", nrow(mimic), " / ",
    sum(mimic$tuple_observed)
  ),
  paste0(
    "- eICU strict / tuple observed: ", nrow(eicu), " / ",
    sum(eicu$tuple_observed)
  ),
  paste0(
    "- MIMIC truncated-weight ESS: ",
    format(round(mimic_fit$positivity$effective_sample_size, 1), nsmall = 1),
    " (", format(round(
      100 * mimic_fit$positivity$ess_fraction_of_observed, 1
    ), nsmall = 1), "%)"
  ),
  paste0(
    "- eICU truncated-weight ESS: ",
    format(round(eicu_fit$positivity$effective_sample_size, 1), nsmall = 1),
    " (", format(round(
      100 * eicu_fit$positivity$ess_fraction_of_observed, 1
    ), nsmall = 1), "%)"
  ),
  paste0(
    "- Positivity-sensitive flag (MIMIC/eICU): ",
    mimic_fit$positivity$positivity_sensitive, " / ",
    eicu_fit$positivity$positivity_sensitive
  ),
  paste0(
    "- eICU hospitals with no observed tuple: ",
    eicu_fit$positivity$structural_zero_observed_hospital_n, "/",
    eicu_fit$positivity$hospital_n, " hospitals; ",
    eicu_fit$positivity$structural_zero_observed_patient_n,
    " strict-cohort patients"
  ),
  paste0(
    "- eICU supported-hospital target: ", nrow(eicu_support),
    " strict patients in ", length(support_hospitals),
    " hospitals; all ", sum(eicu_support$tuple_observed),
    " observed tuples retained"
  ),
  "- The target is tuple observation, not a clinical outcome.",
  "- Median imputation and missing indicators are used only in the observation model.",
  "- Stabilized weights are truncated at database-specific observed-record 1st/99th percentiles.",
  "- Results using these weights must be called selection-weighted sensitivities, never selection-bias corrected.",
  "- No mortality, discharge, survival, association, or performance data were read.",
  "",
  "BUILD_COMPLETE"
)
summary_path <- file.path(qc_out, "selection_weights_QC.md")
writeLines(summary_lines, summary_path, useBytes = TRUE)

required_qc <- c(
  "selection_model_specification.csv", "selection_model_coefficients.csv",
  "selection_model_imputation_QC.csv", "selection_model_design_QC.csv",
  "selection_weight_balance_QC.csv", "selection_probability_decile_QC.csv",
  "selection_weight_distribution_QC.csv", "selection_weight_positivity_QC.csv",
  "eicu_hospital_tuple_support_QC.csv", "selection_weight_leakage_guard.csv",
  "selection_weights_QC.md"
)
if (any(!file.exists(file.path(qc_out, required_qc)))) {
  stop("Required selection-weight QC output is missing.")
}
if (!identical(tail(readLines(summary_path, warn = FALSE), 1L),
  "BUILD_COMPLETE")) {
  stop("Selection-weight summary lacks BUILD_COMPLETE sentinel.")
}

completion <- data.table(
  status = "PASS",
  config_version = LOCKED$version,
  completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  decision_id = "D055",
  script_sha256 = sha256_file(script_path),
  mimic_severity_gate_sha256 = sha256_file(mimic_gate_path),
  eicu_severity_gate_sha256 = sha256_file(eicu_gate_path),
  mimic_phase2_gate_sha256 = sha256_file(mimic_phase2_path),
  eicu_phase2_gate_sha256 = sha256_file(eicu_phase2_path),
  mimic_input_rds_sha256 = sha256_file(mimic_input),
  eicu_input_rds_sha256 = sha256_file(eicu_input),
  mimic_output_rds_sha256 = sha256_file(mimic_output),
  eicu_output_rds_sha256 = sha256_file(eicu_output),
  eicu_support_output_rds_sha256 = sha256_file(eicu_support_output),
  mimic_strict_n = nrow(mimic),
  mimic_tuple_observed_n = sum(mimic$tuple_observed),
  eicu_strict_n = nrow(eicu),
  eicu_tuple_observed_n = sum(eicu$tuple_observed),
  eicu_hospital_n = nrow(eicu_hospital_support),
  eicu_zero_observed_hospital_n = eicu_hospital_support[
    support_class == "structural_zero_observed", .N
  ],
  eicu_supported_hospital_strict_n = nrow(eicu_support),
  all_leakage_checks_pass = all(leakage_guard$pass),
  all_required_qc_present = all(file.exists(file.path(qc_out, required_qc))),
  summary_sentinel = "BUILD_COMPLETE"
)
fwrite(completion, completion_gate_tmp)
if (!file.rename(completion_gate_tmp, completion_gate)) {
  stop("Could not atomically publish the selection-weight completion gate.")
}

message("Outcome-blind tuple-observation models complete.")
message("  MIMIC strict/observed: ", nrow(mimic), "/", sum(mimic$tuple_observed))
message("  eICU strict/observed: ", nrow(eicu), "/", sum(eicu$tuple_observed))
message("  private weights: ", private_out)
message("  aggregate QC: ", qc_out)
