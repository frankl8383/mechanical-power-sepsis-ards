#!/usr/bin/env Rscript

# Synthetic-only equivalence and interruption/resume tests.
# No project dataset or outcome artifact is opened.

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/12_two_stage_resume_utils_selftest.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "03_internal_validation_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "12_two_stage_resume_utils.R"))

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop("Synthetic self-test failed: ", label)
  invisible(TRUE)
}

assert_error <- function(expression, pattern, label) {
  observed <- tryCatch(
    {
      force(expression)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
  if (is.na(observed) || !grepl(pattern, observed, fixed = TRUE)) {
    stop(
      "Synthetic self-test failed: ", label,
      "; observed error: ",
      ifelse(is.na(observed), "<none>", observed)
    )
  }
  invisible(TRUE)
}

make_analysis <- function(n = 360L, seed = 2026071721L) {
  set.seed(seed)
  age <- runif(n, 18, 92)
  sex_female <- rbinom(n, 1, 0.48)
  pf_ratio <- runif(n, 45, 299)
  map <- pmin(pmax(rnorm(n, 75, 13), 35), 140)
  vasopressor <- rbinom(
    n, 1, plogis(-1.2 + 0.018 * (75 - map))
  )
  platelet <- exp(rnorm(n, log(190), 0.42))
  creatinine <- pmin(
    pmax(exp(rnorm(n, log(1.05), 0.5)), 0.2), 12
  )
  peep <- sample(c(5, 8, 10, 12, 15), n, replace = TRUE)
  driving_pressure <- runif(n, 5, 28)
  resistive_pressure <- runif(n, 1, 15)
  rr <- runif(n, 8, 34)
  vt_l <- runif(n, 0.28, 0.75)
  static_power <- 0.098 * rr * vt_l * peep
  dynamic_power <-
    0.098 * rr * vt_l * 0.5 * driving_pressure
  resistive_power <-
    0.098 * rr * vt_l * resistive_pressure
  smp <- static_power + dynamic_power + resistive_power
  four_dprr <- 4 * driving_pressure + rr
  linear_predictor <- -1.55 +
    0.018 * (age - 55) -
    0.004 * (pf_ratio - 150) +
    0.38 * vasopressor +
    0.035 * (driving_pressure - 14) +
    0.025 * (rr - 18)
  outcome <- rbinom(n, 1, plogis(linear_predictor))
  if (length(unique(outcome)) != 2L) {
    outcome[seq_len(2L)] <- c(0L, 1L)
  }
  data.frame(
    analysis_id = paste0("synthetic_", seq_len(n)),
    outcome = outcome,
    age = age,
    sex_female = sex_female,
    pf_ratio = pf_ratio,
    map = map,
    vasopressor = vasopressor,
    platelet = platelet,
    creatinine = creatinine,
    smp = smp,
    four_dprr = four_dprr,
    driving_pressure = driving_pressure,
    rr = rr,
    static_power = static_power,
    dynamic_power = dynamic_power,
    resistive_power = resistive_power,
    stringsAsFactors = FALSE
  )
}

analysis <- make_analysis()
model_id <- "M_MP"
model_index <- match(model_id, v2_model_specification()$model_id)
pipeline_id <- paste0(
  model_id, "_rederive_transform_bundle_in_each_training_resample"
)
fit_pipeline <- v2_pm_internal_fit_factory(model_id)
predict_pipeline <- v2_pm_internal_predict_factory(model_id)
point_validation <- v2_harrell_internal_validation(
  data = analysis,
  outcome = "outcome",
  fit_pipeline = fit_pipeline,
  predict_pipeline = predict_pipeline,
  repetitions = 20L,
  seed = 2026071601L + model_index - 1L,
  minimum_success_fraction = 0.95,
  pipeline_id = pipeline_id
)
v2_iv_assert_reportable(point_validation)

outer_repetitions <- 20L
inner_repetitions <- 20L
master_seed <- 2026071603L + model_index - 1L
source_hashes <- c(
  synthetic_analysis = paste(rep("0", 64L), collapse = ""),
  synthetic_point_validation = paste(rep("1", 64L), collapse = "")
)
contract <- v2_ts_make_contract(
  model_id = model_id,
  model_index = model_index,
  data_n = nrow(analysis),
  events = sum(analysis$outcome),
  outcome = "outcome",
  metrics = v2_iv_default_metrics,
  pipeline_id = pipeline_id,
  outer_repetitions = outer_repetitions,
  inner_repetitions = inner_repetitions,
  seed = master_seed,
  minimum_inner_success_fraction = 0.95,
  minimum_outer_success_fraction = 0.95,
  source_hashes = source_hashes
)

monolithic <- v2_two_stage_internal_validation(
  data = analysis,
  outcome = "outcome",
  fit_pipeline = fit_pipeline,
  predict_pipeline = predict_pipeline,
  outer_repetitions = outer_repetitions,
  inner_repetitions = inner_repetitions,
  seed = master_seed,
  minimum_inner_success_fraction = 0.95,
  minimum_outer_success_fraction = 0.95,
  pipeline_id = pipeline_id,
  point_validation = point_validation
)
v2_iv_assert_reportable(monolithic)

checkpoint_dir <- tempfile("ards_v2_two_stage_resume_selftest_")
dir.create(checkpoint_dir, recursive = TRUE)
on.exit(unlink(checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)

partial <- v2_ts_resume(
  data = analysis,
  outcome = "outcome",
  fit_pipeline = fit_pipeline,
  predict_pipeline = predict_pipeline,
  contract = contract,
  checkpoint_dir = checkpoint_dir,
  max_new_replicates = 7L,
  progress_every = 7L
)
assert_true(
  partial$completed_before == 0L &&
    partial$new_replicates == 7L &&
    partial$completed_after == 7L &&
    partial$pending_after == 13L &&
    !partial$complete,
  "first invocation stops after the requested checkpoint batch"
)

first_paths <- partial$checkpoint_paths[file.exists(partial$checkpoint_paths)]
first_hashes <- vapply(
  first_paths,
  function(path) digest::digest(file = path, algo = "sha256"),
  character(1L)
)

resumed <- v2_ts_resume(
  data = analysis,
  outcome = "outcome",
  fit_pipeline = fit_pipeline,
  predict_pipeline = predict_pipeline,
  contract = contract,
  checkpoint_dir = checkpoint_dir,
  max_new_replicates = Inf,
  progress_every = 13L
)
assert_true(
  resumed$completed_before == 7L &&
    resumed$new_replicates == 13L &&
    resumed$completed_after == 20L &&
    resumed$pending_after == 0L &&
    resumed$complete,
  "second invocation resumes only pending outer replicates"
)
assert_true(
  identical(
    first_hashes,
    vapply(
      first_paths,
      function(path) digest::digest(file = path, algo = "sha256"),
      character(1L)
    )
  ),
  "completed checkpoint bytes remain unchanged after resume"
)

idempotent <- v2_ts_resume(
  data = analysis,
  outcome = "outcome",
  fit_pipeline = fit_pipeline,
  predict_pipeline = predict_pipeline,
  contract = contract,
  checkpoint_dir = checkpoint_dir,
  max_new_replicates = 1L
)
assert_true(
  idempotent$completed_before == 20L &&
    idempotent$new_replicates == 0L &&
    idempotent$complete,
  "fully completed rerun is idempotent"
)

resumable <- v2_ts_collect_validation(
  contract, checkpoint_dir, point_validation
)
v2_iv_assert_reportable(resumable)
assert_true(
  isTRUE(all.equal(
    monolithic$outer_audit,
    resumable$outer_audit,
    tolerance = 0,
    check.attributes = FALSE
  )) &&
    isTRUE(all.equal(
      monolithic$outer_estimates,
      resumable$outer_estimates,
      tolerance = 0,
      check.attributes = FALSE
    )) &&
    isTRUE(all.equal(
      monolithic$inner_failures,
      resumable$inner_failures,
      tolerance = 0,
      check.attributes = FALSE
    )) &&
    isTRUE(all.equal(
      monolithic$confidence_interval,
      resumable$confidence_interval,
      tolerance = 0,
      check.attributes = FALSE
    )),
  "resumable result is exactly equivalent to monolithic algorithm"
)

tampered_contract <- contract
tampered_contract$pipeline_id <- paste0(contract$pipeline_id, "_tampered")
assert_error(
  v2_ts_resume(
    data = analysis,
    outcome = "outcome",
    fit_pipeline = fit_pipeline,
    predict_pipeline = predict_pipeline,
    contract = tampered_contract,
    checkpoint_dir = checkpoint_dir,
    max_new_replicates = 1L
  ),
  "checkpoint contract mismatch",
  "contract drift blocks checkpoint reuse"
)

cat("REBUILD_V2_TWO_STAGE_RESUME_SYNTHETIC_PASS\n")
