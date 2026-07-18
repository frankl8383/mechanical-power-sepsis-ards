#!/usr/bin/env Rscript

# Synthetic-only tests. No project dataset or outcome artifact is opened.

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/04_external_validation_utils_selftest.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "01_analysis_utils.R"))
source(file.path(dirname(script_path), "04_external_validation_utils.R"))

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

assert_true(
  identical(formals(v2_ev_cluster_bootstrap)$repetitions, 2000L),
  "2,000-replicate default for metric bootstrap"
)
assert_true(
  identical(
    formals(v2_ev_cluster_bootstrap_calibration_bands)$repetitions,
    2000L
  ),
  "2,000-replicate default for calibration bands"
)

set.seed(2026071610L)
hospital_sizes <- c(80L, rep(40L, 11L))
hospital <- rep(sprintf("H%02d", seq_along(hospital_sizes)), hospital_sizes)
n <- length(hospital)
x1 <- rnorm(n)
x2 <- rbinom(n, 1, 0.45)
hospital_shift <- rep(seq(-0.35, 0.35, length.out = 12L), hospital_sizes)
true_probability <- plogis(
  -1.15 + 0.85 * x1 + 0.45 * x2 + hospital_shift
)
y <- rbinom(n, 1, true_probability)

# Guarantee both outcome classes in every hospital so ordinary synthetic
# cluster resamples are all estimable.
for (id in unique(hospital)) {
  index <- which(hospital == id)
  if (!any(y[index] == 1L)) y[index[[1L]]] <- 1L
  if (!any(y[index] == 0L)) y[index[[length(index)]]] <- 0L
}

synthetic <- data.frame(
  patient_id = sprintf("P%04d", seq_len(n)),
  hospital = hospital,
  death = y,
  prediction_m0 = plogis(-0.95 + 0.30 * x1),
  prediction_m1 = plogis(-1.05 + 0.76 * x1 + 0.38 * x2),
  prediction_m2 = plogis(-1.20 + 0.82 * x1 + 0.43 * x2),
  stringsAsFactors = FALSE
)

prediction_set <- v2_ev_prediction_set(
  synthetic,
  id_column = "patient_id",
  outcome_column = "death",
  hospital_column = "hospital",
  model_columns = c(
    M0 = "prediction_m0",
    M1 = "prediction_m1",
    M2 = "prediction_m2"
  ),
  set_id = "synthetic_external"
)
v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)

model_frames <- lapply(
  c(M0 = "prediction_m0", M1 = "prediction_m1", M2 = "prediction_m2"),
  function(column) {
    data.frame(
      patient_id = synthetic$patient_id,
      death = synthetic$death,
      hospital = synthetic$hospital,
      probability = synthetic[[column]],
      stringsAsFactors = FALSE
    )
  }
)
strict_frame_set <- v2_ev_prediction_set_from_model_frames(
  model_frames,
  id_column = "patient_id",
  outcome_column = "death",
  hospital_column = "hospital",
  probability_column = "probability",
  set_id = "synthetic_external_from_frames"
)
assert_true(
  identical(strict_frame_set$row_ids, prediction_set$row_ids) &&
    identical(strict_frame_set$predictions, prediction_set$predictions),
  "strict per-model frame constructor"
)

comparisons <- data.frame(
  candidate_model = c("M1", "M2"),
  reference_model = c("M0", "M0"),
  stringsAsFactors = FALSE
)

raw <- v2_ev_raw_performance(prediction_set)
assert_true(
  nrow(raw) == 3L &&
    all(v2_ev_metric_names %in% names(raw)) &&
    identical(raw$analysis, rep(
      "raw_frozen_external_validation", 3L
    )),
  "complete raw external performance"
)

paired <- v2_ev_paired_differences(prediction_set, comparisons)
manual_delta <- v2_paired_metric_difference(
  prediction_set$outcome,
  prediction_set$predictions[, "M1"],
  prediction_set$predictions[, "M0"]
)
assert_true(
  isTRUE(all.equal(
    as.numeric(paired[1L, c(
      "delta_brier", "delta_log_loss", "delta_c_statistic"
    )]),
    as.numeric(manual_delta),
    tolerance = 1e-12
  )),
  "same-patient paired differences"
)

updates <- v2_ev_external_model_updates(prediction_set)
assert_true(
  nrow(updates$raw_external_validation) == 3L &&
    nrow(updates$update_performance) == 6L &&
    setequal(
      updates$update_performance$update_type,
      c("intercept_only", "intercept_and_slope")
    ) &&
    all(
      updates$update_performance$analysis ==
        "external_model_update_descriptive_apparent"
    ) &&
    grepl(
      "not raw external validation",
      updates$update_warning,
      fixed = TRUE
    ),
  "raw validation and model updates strictly separated"
)

bootstrap <- v2_ev_cluster_bootstrap(
  prediction_set,
  comparisons = comparisons,
  repetitions = 120L,
  seed = 2026071611L,
  minimum_success_fraction = 0.95,
  keep_replicates = TRUE
)
v2_ev_assert_bootstrap_reportable(bootstrap)
assert_true(
  bootstrap$successful_replicates == 120L &&
    bootstrap$failed_replicates == 0L &&
    all(bootstrap$model_summary$reportable) &&
    all(is.finite(bootstrap$model_summary$lower)) &&
    nrow(bootstrap$paired_difference_summary) ==
      nrow(comparisons) * length(v2_ev_difference_metric_names) &&
    all(is.finite(bootstrap$paired_difference_summary$lower)),
  "paired hospital-cluster bootstrap and confidence intervals"
)
assert_true(
  length(unique(bootstrap$model_replicates$replicate)) == 120L &&
    length(unique(bootstrap$difference_replicates$replicate)) == 120L,
  "replicate-level estimates retained"
)

largest <- v2_ev_largest_hospital_exclusion(
  prediction_set, comparisons
)
assert_true(
  identical(largest$excluded_hospital, "H01") &&
    largest$excluded_n == 80L &&
    largest$retained_n == n - 80L &&
    nrow(largest$model_performance) == 3L,
  "largest-hospital exclusion"
)

equal_hospital <- v2_ev_equal_hospital_performance(
  prediction_set,
  minimum_hospital_n = 10L,
  comparisons = comparisons
)
assert_true(
  nrow(equal_hospital$performance) == 3L &&
    nrow(equal_hospital$paired_differences) == 2L &&
    nrow(equal_hospital$hospital_detail) == 12L &&
    all(abs(
      equal_hospital$hospital_detail$total_analysis_weight - 1
    ) < 1e-12) &&
    all(v2_ev_metric_names %in% names(equal_hospital$performance)),
  "equal-hospital-weighted analysis"
)

loho <- v2_ev_leave_one_hospital_out(prediction_set, comparisons)
assert_true(
  loho$successful_hospitals == 12L &&
    loho$failed_hospitals == 0L &&
    length(unique(loho$model_influence$omitted_hospital)) == 12L &&
    all(is.finite(loho$model_influence$change_from_full)) &&
    all(is.finite(
      loho$paired_difference_influence$change_from_full
    )),
  "leave-one-hospital-out influence analysis"
)

calibration <- v2_ev_flexible_calibration_data(
  prediction_set,
  grid_points = 61L,
  distribution_bins = 8L
)
assert_true(
  nrow(calibration$curve) == 3L * 61L &&
    all(calibration$curve$estimated_observed_probability >= 0) &&
    all(calibration$curve$estimated_observed_probability <= 1) &&
    setequal(names(calibration$specs), c("M0", "M1", "M2")),
  "flexible calibration curve data"
)

calibration_bootstrap <- v2_ev_cluster_bootstrap_calibration_bands(
  prediction_set,
  repetitions = 60L,
  seed = 2026071612L,
  minimum_success_fraction = 0.95,
  grid_points = 41L,
  distribution_bins = 8L,
  keep_replicates = TRUE
)
assert_true(
  calibration_bootstrap$reportable &&
    calibration_bootstrap$successful_replicates == 60L &&
    all(is.finite(
      calibration_bootstrap$curve_with_pointwise_band$lower
    )) &&
    nrow(calibration_bootstrap$replicate_curves) ==
      60L * 3L * 41L &&
    grepl(
      "not simultaneous",
      calibration_bootstrap$interval_note,
      fixed = TRUE
    ),
  "cluster-bootstrap pointwise calibration bands"
)

# A prediction set with reordered patients must not pass a cross-object row
# identity check.
reordered <- synthetic[rev(seq_len(nrow(synthetic))), , drop = FALSE]
reordered_set <- v2_ev_prediction_set(
  reordered,
  id_column = "patient_id",
  outcome_column = "death",
  hospital_column = "hospital",
  model_columns = c(
    M0 = "prediction_m0",
    M1 = "prediction_m1",
    M2 = "prediction_m2"
  ),
  set_id = "synthetic_reordered"
)
assert_error(
  v2_ev_assert_same_rows(prediction_set, reordered_set),
  "same patients in the same order",
  "cross-object row mismatch blocked"
)

misordered_frames <- model_frames
misordered_frames$M2 <- misordered_frames$M2[
  rev(seq_len(nrow(misordered_frames$M2))), ,
  drop = FALSE
]
assert_error(
  v2_ev_prediction_set_from_model_frames(
    misordered_frames,
    "patient_id",
    "death",
    "hospital",
    "probability"
  ),
  "same patients in the same order",
  "per-model row mismatch blocked"
)

duplicate_id <- synthetic
duplicate_id$patient_id[[2L]] <- duplicate_id$patient_id[[1L]]
assert_error(
  v2_ev_prediction_set(
    duplicate_id,
    "patient_id",
    "death",
    "hospital",
    c(M0 = "prediction_m0")
  ),
  "unique",
  "duplicate patient identifiers blocked"
)

assert_error(
  v2_ev_paired_differences(
    prediction_set,
    data.frame(
      candidate_model = "M1",
      reference_model = "UNKNOWN",
      stringsAsFactors = FALSE
    )
  ),
  "Unknown comparison model",
  "unknown model comparison blocked"
)

# Deliberately concentrate every event in one of four hospitals. Cluster
# samples omitting that hospital have one outcome class and must be retained as
# failed replicates. The <95% completion rate blocks interval reporting.
set.seed(2026071613L)
failure_data <- data.frame(
  patient_id = sprintf("F%03d", seq_len(120L)),
  hospital = rep(paste0("F", 1:4), each = 30L),
  death = c(rep(1L, 10L), rep(0L, 110L)),
  prediction = runif(120L, 0.05, 0.35),
  stringsAsFactors = FALSE
)
failure_set <- v2_ev_prediction_set(
  failure_data,
  "patient_id",
  "death",
  "hospital",
  c(M0 = "prediction"),
  set_id = "synthetic_failure_gate"
)
failure_bootstrap <- v2_ev_cluster_bootstrap(
  failure_set,
  repetitions = 80L,
  seed = 2026071614L,
  minimum_success_fraction = 0.95,
  keep_replicates = TRUE
)
assert_true(
  !failure_bootstrap$reportable &&
    failure_bootstrap$failed_replicates > 0L &&
    nrow(failure_bootstrap$failure_summary) > 0L &&
    all(is.na(failure_bootstrap$model_summary$lower)),
  "failed bootstrap audit and 95% reporting gate"
)

cat("REBUILD_V2_EXTERNAL_VALIDATION_SYNTHETIC_PASS\n")
