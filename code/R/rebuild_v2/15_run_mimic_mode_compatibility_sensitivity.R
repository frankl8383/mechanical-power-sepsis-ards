#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2:
# MIMIC conventional volume-control-compatible mode sensitivity.
#
# The mode and rate flags were frozen without opening outcomes. This script
# joins them to the already frozen primary common set, fits the five locked
# linear representations on the restricted same-patient subset, and reports
# point estimates only. It does not relabel the subset as passive ventilation.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/15_run_mimic_mode_compatibility_sensitivity.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "12_secondary_sensitivity_utils.R"))

stopifnot(identical(LOCKED_V2$version, "2.0.0"))

private_out <- file.path(PRIVATE_ROOT, "mode_compatibility_sensitivity")
aggregate_out <- file.path(
  AGGREGATE_ROOT, "mode_compatibility_sensitivity"
)
qc_out <- file.path(QC_ROOT, "mode_compatibility_sensitivity")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(
  qc_out, "mimic_mode_compatibility_sensitivity_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

paths <- list(
  primary_gate = file.path(
    QC_ROOT, "primary_models", "phase4_primary_models_complete_v2.csv"
  ),
  primary_private_manifest = file.path(
    QC_ROOT, "primary_models",
    "primary_model_private_output_manifest_v2.csv"
  ),
  mode_gate = file.path(
    QC_ROOT, "construct_quality",
    "mimic_primary_tuple_mode_quality_complete_v2.csv"
  ),
  mode_flags = file.path(
    PRIVATE_ROOT, "construct_quality",
    "mimic_primary_tuple_mode_quality_flags_v2.rds"
  ),
  mimic_analysis = file.path(
    PRIVATE_ROOT, "primary_models",
    "mimic_primary_analysis_frame_v2.rds"
  ),
  transform_bundle = file.path(
    PRIVATE_ROOT, "model_ready", "frozen_transform_bundle_v2.rds"
  ),
  decision_log = file.path(
    PROJECT_ROOT, "docs", "rebuild_v2", "analysis_decision_log_v2.md"
  )
)
missing <- names(paths)[!vapply(paths, file.exists, logical(1L))]
if (length(missing)) {
  stop("Missing mode-sensitivity input(s): ", paste(missing, collapse = ", "))
}

read_field_gate <- function(path, label) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field)) {
    stop("Malformed field/value gate: ", label)
  }
  setNames(as.character(gate$value), gate$field)
}
require_field <- function(gate, field, expected, label) {
  observed <- unname(gate[field])
  if (length(observed) != 1L || is.na(observed) ||
      !identical(observed, as.character(expected))) {
    stop(
      label, " mismatch for ", field, ": ",
      ifelse(length(observed) == 1L, observed, "<missing>"),
      " != ", as.character(expected)
    )
  }
  invisible(TRUE)
}

primary_gate <- read_field_gate(paths$primary_gate, "primary model gate")
for (pair in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c("analysis_mode", "FINAL_LOCKED_BOOTSTRAP"),
  c("same_patient_comparison_pass", "TRUE"),
  c("outcome_join_exact_and_order_preserved", "TRUE")
)) {
  require_field(primary_gate, pair[[1L]], pair[[2L]], "primary model gate")
}

mode_gate <- read_field_gate(paths$mode_gate, "mode-quality gate")
for (pair in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c("outcome_artifacts_opened", "FALSE"),
  c("tuple_reselection", "FALSE"),
  c("all_invariants_pass", "TRUE")
)) {
  require_field(mode_gate, pair[[1L]], pair[[2L]], "mode-quality gate")
}
require_field(
  mode_gate, "flags_sha256", v2_pm_sha256_file(paths$mode_flags),
  "mode-quality gate"
)

decision_text <- paste(readLines(paths$decision_log, warn = FALSE), collapse = "\n")
if (!grepl(
    "V2-D017.*LOCKED BEFORE MODE-RESTRICTED OUTCOME FIT",
    decision_text
  )) {
  stop("The preread mode-compatibility decision is absent from the log.")
}

private_manifest <- fread(
  paths$primary_private_manifest,
  colClasses = "character",
  showProgress = FALSE
)
manifest_row <- private_manifest[
  role == "mimic_analysis" &
    normalizePath(path, mustWork = TRUE) ==
      normalizePath(paths$mimic_analysis, mustWork = TRUE)
]
if (nrow(manifest_row) != 1L ||
    !identical(
      manifest_row$sha256[[1L]],
      v2_pm_sha256_file(paths$mimic_analysis)
    )) {
  stop("The frozen MIMIC primary analysis frame failed manifest validation.")
}

analysis <- as.data.frame(readRDS(paths$mimic_analysis))
analysis$core_complete <- TRUE
analysis_validation <- v2_pm_validate_predictor_frame(
  analysis, "MIMIC-IV", require_complete = TRUE
)
v2_assert_binary_outcome(analysis$outcome)
if (nrow(analysis) != as.integer(primary_gate[["mimic_n"]]) ||
    sum(analysis$outcome) != as.integer(primary_gate[["mimic_events"]])) {
  stop("Primary MIMIC sample counts differ from the locked primary gate.")
}

bundle <- readRDS(paths$transform_bundle)
if (!identical(
  attr(bundle, "freeze_metadata")$derivation_database,
  "MIMIC-IV only"
)) {
  stop("The transformation bundle is not frozen from MIMIC-IV.")
}

flags <- as.data.frame(readRDS(paths$mode_flags))
required_flags <- c(
  "analysis_id", "formula_compatible_mode", "rate_concordant",
  "formula_compatible_and_rate_concordant",
  "preferred_source_primary_tuple",
  "formula_compatible_rate_and_preferred"
)
v2_require_columns(flags, required_flags, "MIMIC mode-quality flags")
flags$analysis_id <- as.character(flags$analysis_id)
if (anyNA(flags$analysis_id) || any(!nzchar(flags$analysis_id)) ||
    anyDuplicated(flags$analysis_id)) {
  stop("Mode-quality flag IDs are invalid.")
}
for (column in setdiff(required_flags, "analysis_id")) {
  if (anyNA(flags[[column]]) || !is.logical(flags[[column]])) {
    stop("Mode-quality flag is not complete logical: ", column)
  }
}
if (!identical(
  flags$formula_compatible_and_rate_concordant,
  flags$formula_compatible_mode & flags$rate_concordant
)) {
  stop("The frozen combined mode/rate flag is internally inconsistent.")
}

position <- match(as.character(analysis$analysis_id), flags$analysis_id)
if (anyNA(position)) {
  stop("Mode-quality artifact lacks primary-common-set IDs.")
}
analysis$formula_compatible_mode <-
  flags$formula_compatible_mode[position]
analysis$rate_concordant <- flags$rate_concordant[position]
analysis$preferred_source_primary_tuple <-
  flags$preferred_source_primary_tuple[position]
analysis$formula_compatible_and_rate_concordant <-
  flags$formula_compatible_and_rate_concordant[position]
analysis$formula_compatible_rate_and_preferred <-
  flags$formula_compatible_rate_and_preferred[position]

restricted <- analysis[
  analysis$formula_compatible_and_rate_concordant, ,
  drop = FALSE
]
if (nrow(restricted) < 100L || sum(restricted$outcome) < 20L ||
    sum(1L - restricted$outcome) < 20L) {
  stop("The frozen mode-compatible/rate-concordant subset lacks support.")
}
if (!all(restricted$formula_compatible_mode) ||
    !all(restricted$rate_concordant)) {
  stop("The restricted sample does not satisfy its frozen definition.")
}

model_spec <- v2_model_specification()
fits <- list()
predictions <- matrix(
  NA_real_, nrow = nrow(restricted), ncol = nrow(model_spec),
  dimnames = list(NULL, model_spec$model_id)
)
design_audit <- vector("list", nrow(model_spec))
for (i in seq_len(nrow(model_spec))) {
  model_id <- model_spec$model_id[[i]]
  design <- v2_build_design(restricted, model_id, bundle)
  fit <- v2_fit_logistic(
    design, restricted$outcome, model_id, restricted$analysis_id
  )
  fits[[model_id]] <- fit
  predictions[, model_id] <- stats::predict(
    fit, design, type = "response"
  )
  design_audit[[i]] <- data.frame(
    model_id = model_id,
    model_role = model_spec$role[[i]],
    incremental_df = model_spec$incremental_df[[i]],
    total_parameter_n = length(fit$coefficients),
    converged = isTRUE(fit$converged),
    rank_full = fit$rank == length(fit$coefficients),
    stringsAsFactors = FALSE
  )
}
design_audit <- do.call(rbind, design_audit)

model_roles <- setNames(model_spec$role, model_spec$model_id)
performance <- v2_ss_model_performance(
  restricted$outcome,
  predictions,
  "MIMIC-IV",
  "formula_compatible_mode_and_rate_concordant_apparent",
  model_roles
)
comparisons <- data.frame(
  candidate_model = c(
    "M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY",
    "M_4DPRR", "M_DPRR", "M_ENERGY"
  ),
  reference_model = c(
    "M0", "M0", "M0", "M0", "M_MP", "M_MP", "M_MP"
  ),
  comparison_role = c(
    rep("increment_over_baseline", 4L),
    rep("representation_comparison_to_smp", 3L)
  ),
  stringsAsFactors = FALSE
)
paired <- v2_ss_paired_differences(
  restricted$outcome,
  predictions,
  comparisons,
  "MIMIC-IV",
  "formula_compatible_mode_and_rate_concordant_apparent"
)

constraint_tests <- rbind(
  data.frame(
    constraint = "4DP_plus_RR_equal_weight_constraint",
    constrained_model = "M_4DPRR",
    unconstrained_model = "M_DPRR",
    likelihood_ratio_chisq =
      2 * (fits$M_DPRR$log_likelihood - fits$M_4DPRR$log_likelihood),
    degrees_of_freedom = 1L,
    stringsAsFactors = FALSE
  ),
  data.frame(
    constraint = "equal_algebraic_energy_coefficients",
    constrained_model = "M_MP",
    unconstrained_model = "M_ENERGY",
    likelihood_ratio_chisq =
      2 * (fits$M_ENERGY$log_likelihood - fits$M_MP$log_likelihood),
    degrees_of_freedom = 2L,
    stringsAsFactors = FALSE
  )
)
constraint_tests$p_value <- stats::pchisq(
  constraint_tests$likelihood_ratio_chisq,
  df = constraint_tests$degrees_of_freedom,
  lower.tail = FALSE
)
if (any(constraint_tests$likelihood_ratio_chisq < -1e-8)) {
  stop("A restricted-sample nested likelihood-ratio statistic is negative.")
}
constraint_tests$likelihood_ratio_chisq <-
  pmax(constraint_tests$likelihood_ratio_chisq, 0)

coefficients <- do.call(rbind, lapply(names(fits), function(model_id) {
  data.frame(
    model_id = model_id,
    term = names(fits[[model_id]]$coefficients),
    coefficient = as.numeric(fits[[model_id]]$coefficients),
    stringsAsFactors = FALSE
  )
}))

sample_summary <- data.frame(
  database = "MIMIC-IV",
  primary_common_set_n = nrow(analysis),
  primary_common_set_events = sum(analysis$outcome),
  compatible_mode_common_set_n = sum(analysis$formula_compatible_mode),
  compatible_mode_and_rate_concordant_n = nrow(restricted),
  restricted_events = sum(restricted$outcome),
  restricted_event_rate = mean(restricted$outcome),
  restricted_preferred_source_n =
    sum(restricted$preferred_source_primary_tuple),
  restricted_preferred_source_percent =
    100 * mean(restricted$preferred_source_primary_tuple),
  retained_from_primary_percent = 100 * nrow(restricted) / nrow(analysis),
  primary_tuple_reselected = FALSE,
  passive_ventilation_claimed = FALSE,
  bootstrap_replicates = 0L,
  manuscript_ci_ready = FALSE,
  stringsAsFactors = FALSE
)

private_result <- list(
  sample = restricted,
  fits = fits,
  predictions = predictions,
  specification = list(
    population =
      "MIMIC primary common set with conventional volume-control-compatible mode and rate concordance",
    mode_rule =
      "frozen outcome-free D017 mapping; nearest mode within +/-60 min",
    primary_tuple_reselected = FALSE,
    passive_ventilation_claimed = FALSE,
    bootstrap_replicates = 0L
  ),
  input_hashes = lapply(paths, v2_pm_sha256_file)
)
private_path <- file.path(
  private_out, "mimic_mode_compatibility_point_estimates_v2.rds"
)
private_hash <- v2_pm_atomic_save_rds(private_result, private_path)

aggregate_outputs <- list(
  "mimic_mode_compatibility_sample_v2.csv" = sample_summary,
  "mimic_mode_compatibility_model_audit_v2.csv" = design_audit,
  "mimic_mode_compatibility_point_performance_v2.csv" = performance,
  "mimic_mode_compatibility_paired_differences_v2.csv" = paired,
  "mimic_mode_compatibility_constraint_tests_v2.csv" = constraint_tests,
  "mimic_mode_compatibility_coefficients_v2.csv" = coefficients
)
for (name in names(aggregate_outputs)) {
  v2_pm_atomic_write_csv(
    aggregate_outputs[[name]], file.path(aggregate_out, name)
  )
}

input_manifest <- data.frame(
  artifact = names(paths),
  path = normalizePath(unname(unlist(paths)), mustWork = TRUE),
  sha256 = vapply(paths, v2_pm_sha256_file, character(1L)),
  stringsAsFactors = FALSE
)
aggregate_manifest <- data.frame(
  artifact = names(aggregate_outputs),
  sha256 = vapply(
    names(aggregate_outputs),
    function(name) {
      v2_pm_sha256_file(file.path(aggregate_out, name))
    },
    character(1L)
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  input_manifest,
  file.path(qc_out, "mimic_mode_compatibility_input_manifest_v2.csv")
)
v2_pm_atomic_write_csv(
  aggregate_manifest,
  file.path(qc_out, "mimic_mode_compatibility_output_manifest_v2.csv")
)

invariants <- data.frame(
  check = c(
    "mode_flags_frozen_outcome_free",
    "primary_tuple_reselected",
    "all_restricted_rows_formula_compatible",
    "all_restricted_rows_rate_concordant",
    "passive_ventilation_claimed",
    "same_patient_model_comparison",
    "all_models_converged_and_full_rank",
    "bootstrap_replicates_zero"
  ),
  expected = c(
    "TRUE", "FALSE", "TRUE", "TRUE",
    "FALSE", "TRUE", "TRUE", "TRUE"
  ),
  observed = c(
    "TRUE",
    "FALSE",
    as.character(all(restricted$formula_compatible_mode)),
    as.character(all(restricted$rate_concordant)),
    "FALSE",
    as.character(nrow(predictions) == nrow(restricted)),
    as.character(all(design_audit$converged & design_audit$rank_full)),
    "TRUE"
  ),
  stringsAsFactors = FALSE
)
invariants$pass <- invariants$expected == invariants$observed
if (!all(invariants$pass)) {
  stop(
    "Mode-sensitivity invariant failure: ",
    paste(invariants$check[!invariants$pass], collapse = ", ")
  )
}
v2_pm_atomic_write_csv(
  invariants,
  file.path(qc_out, "mimic_mode_compatibility_invariants_v2.csv")
)

gate <- data.frame(
  field = c(
    "status", "locked_config_version", "script_sha256",
    "primary_analysis_sha256", "mode_flags_sha256",
    "private_result_sha256", "primary_common_set_n",
    "restricted_n", "restricted_events",
    "restriction", "primary_tuple_reselection",
    "passive_ventilation_claimed", "bootstrap_replicates",
    "manuscript_ci_ready", "all_models_converged",
    "all_invariants_pass", "completed_at"
  ),
  value = c(
    "PASS", LOCKED_V2$version, v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(paths$mimic_analysis),
    v2_pm_sha256_file(paths$mode_flags),
    private_hash, nrow(analysis), nrow(restricted),
    sum(restricted$outcome),
    "formula_compatible_mode_and_rate_concordant",
    "FALSE", "FALSE", "0", "FALSE",
    as.character(all(design_audit$converged)),
    "TRUE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

message("REBUILD_V2_MIMIC_MODE_COMPATIBILITY_POINT_ESTIMATES_PASS")
message(
  "  Restricted n=", nrow(restricted),
  "; events=", sum(restricted$outcome),
  "; retained=", sprintf("%.1f%%", 100 * nrow(restricted) / nrow(analysis))
)
