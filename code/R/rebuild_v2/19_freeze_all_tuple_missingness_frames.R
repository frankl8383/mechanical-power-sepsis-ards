#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2:
# outcome-blind all-tuple frozen-median plus missing-indicator frame freeze

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/19_freeze_all_tuple_missingness_frames.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))
source(file.path(script_dir, "18_missingness_sensitivity_utils.R"))

stopifnot(
  identical(LOCKED_V2$version, "2.0.0"),
  isTRUE(LOCKED_V2$missing_data_hierarchy$sensitivity_frozen_median_indicator)
)

read_gate <- function(path, label) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (identical(names(gate), c("field", "value"))) {
    if (anyDuplicated(gate$field) || anyNA(gate$field) ||
        any(!nzchar(gate$field))) {
      stop("Malformed field/value ", label, ": ", path)
    }
    return(setNames(as.character(gate$value), gate$field))
  }
  if (nrow(gate) != 1L || anyDuplicated(names(gate))) {
    stop("Malformed one-row ", label, ": ", path)
  }
  setNames(
    vapply(gate, function(x) as.character(x[[1L]]), character(1L)),
    names(gate)
  )
}

require_gate_value <- function(gate, field, expected, label) {
  value <- unname(gate[field])
  if (length(value) != 1L || is.na(value) || !nzchar(value) ||
      !identical(value, as.character(expected))) {
    stop(
      label, " mismatch for ", field, ": ",
      ifelse(length(value) == 1L, value, "<missing>"),
      " != ", as.character(expected)
    )
  }
  invisible(value)
}

paths <- list(
  primary_freeze_gate = file.path(
    QC_ROOT, "primary_model_freeze",
    "phase3_primary_model_freeze_complete_v2.csv"
  ),
  primary_freeze_manifest = file.path(
    QC_ROOT, "primary_model_freeze",
    "primary_model_freeze_manifest_v2.csv"
  ),
  landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  mimic_joined = file.path(
    PRIVATE_ROOT, "model_ready",
    "mimic_tuple_core_joined_outcome_free_v2.rds"
  ),
  eicu_joined = file.path(
    PRIVATE_ROOT, "model_ready",
    "eicu_tuple_core_joined_outcome_free_v2.rds"
  )
)
missing_paths <- names(paths)[!vapply(paths, file.exists, logical(1L))]
if (length(missing_paths)) {
  stop(
    "Missing all-tuple missingness-freeze input(s): ",
    paste(missing_paths, collapse = ", ")
  )
}

private_out <- file.path(PRIVATE_ROOT, "missingness_sensitivity")
aggregate_out <- file.path(AGGREGATE_ROOT, "missingness_sensitivity")
qc_out <- file.path(QC_ROOT, "missingness_sensitivity")
dir.create(private_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)

mimic_output <- file.path(
  private_out, "mimic_all_tuple_missingness_predictors_v2.rds"
)
eicu_output <- file.path(
  private_out, "eicu_all_tuple_missingness_predictors_v2.rds"
)
bundle_output <- file.path(
  private_out, "frozen_all_tuple_missingness_bundle_v2.rds"
)
completion_gate <- file.path(
  qc_out, "all_tuple_missingness_freeze_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

primary_gate <- read_gate(
  paths$primary_freeze_gate, "primary model freeze gate"
)
for (pair in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c("outcome_fields_read", "FALSE"),
  c("outcome_leakage_guard_pass", "TRUE"),
  c("timing_and_range_qc_pass", "TRUE"),
  c("parameter_derivation_database", "MIMIC-IV only")
)) {
  require_gate_value(
    primary_gate, pair[[1L]], pair[[2L]], "primary model freeze gate"
  )
}

manifest <- fread(
  paths$primary_freeze_manifest,
  colClasses = "character",
  showProgress = FALSE
)
v2_pm_require_columns(
  manifest, c("role", "path", "sha256"), "primary model freeze manifest"
)
if (anyDuplicated(manifest$role)) {
  stop("Primary model freeze manifest has duplicate roles.")
}
for (pair in list(
  c("mimic_joined_output", paths$mimic_joined),
  c("eicu_joined_output", paths$eicu_joined)
)) {
  role_name <- pair[[1L]]
  path <- pair[[2L]]
  row <- manifest[manifest[["role"]] == role_name, , drop = FALSE]
  if (nrow(row) != 1L ||
      !identical(normalizePath(row$path[[1L]], mustWork = TRUE),
                 normalizePath(path, mustWork = TRUE)) ||
      !identical(row$sha256[[1L]], v2_pm_sha256_file(path))) {
    stop("Primary freeze manifest mismatch for ", role_name, ".")
  }
}

landmark_gate <- read_gate(paths$landmark_gate, "fixed landmark gate")
for (pair in list(
  c("status", "PASS"),
  c("locked_config_version", LOCKED_V2$version),
  c("landmark_hours", as.character(LOCKED_V2$landmark_hours))
)) {
  if (pair[[1L]] %in% names(landmark_gate)) {
    require_gate_value(
      landmark_gate, pair[[1L]], pair[[2L]], "fixed landmark gate"
    )
  }
}

mimic_original <- as.data.frame(readRDS(paths$mimic_joined))
eicu_original <- as.data.frame(readRDS(paths$eicu_joined))
leakage <- rbind(
  v2_pm_predictor_leakage_audit(
    mimic_original, "MIMIC all-tuple outcome-free frame"
  ),
  v2_pm_predictor_leakage_audit(
    eicu_original, "eICU all-tuple outcome-free frame"
  )
)
if (any(!leakage$pass)) {
  stop("Outcome-like fields were present before missingness-rule freeze.")
}

mimic_validation <- v2_pm_validate_predictor_frame(
  mimic_original, "MIMIC-IV", require_complete = FALSE
)
eicu_validation <- v2_pm_validate_predictor_frame(
  eicu_original, "eICU-CRD", require_complete = FALSE
)
if (!identical(nrow(mimic_original), 10468L) ||
    !identical(nrow(eicu_original), 1459L)) {
  stop("All-tuple frame sizes changed from the frozen fixed-6h artifacts.")
}

rule <- v2_mi_derive_rule(mimic_original)
mimic_imputed <- v2_mi_apply_rule(mimic_original, rule, "MIMIC-IV")
eicu_imputed <- v2_mi_apply_rule(eicu_original, rule, "eICU-CRD")
transform_bundle <- v2_derive_transform_bundle(mimic_imputed)
attr(transform_bundle, "freeze_metadata") <- list(
  artifact_version = "frozen_all_tuple_missingness_transform_bundle_v2",
  derivation_database = "MIMIC-IV only",
  derivation_population =
    "all fixed-6h tuple-positive MIMIC patients after frozen median imputation",
  derivation_n = nrow(mimic_imputed),
  quantile_type = 2L,
  external_application = "applied unchanged to all tuple-positive eICU rows",
  external_outcomes_used = FALSE
)

design_audit <- do.call(rbind, lapply(
  list(`MIMIC-IV` = mimic_imputed, `eICU-CRD` = eicu_imputed),
  function(frame) {
    database <- unique(frame$database)
    do.call(rbind, lapply(LOCKED_V2$model_ids, function(model_id) {
      design <- v2_mi_build_design(
        frame, model_id, transform_bundle, rule
      )
      data.frame(
        database = database,
        model_id = model_id,
        n = nrow(design),
        design_columns = ncol(design),
        indicator_columns = paste(rule$indicator_columns, collapse = ";"),
        all_indicator_columns_present =
          all(rule$indicator_columns %in% colnames(design)),
        missing_n = sum(is.na(design)),
        nonfinite_n = sum(!is.finite(design)),
        column_names_unique = !anyDuplicated(colnames(design)),
        pass = !anyNA(design) && all(is.finite(design)) &&
          !anyDuplicated(colnames(design)) &&
          all(rule$indicator_columns %in% colnames(design)),
        stringsAsFactors = FALSE
      )
    }))
  }
))
for (model_id in LOCKED_V2$model_ids) {
  columns <- lapply(
    list(mimic_imputed, eicu_imputed),
    function(frame) colnames(v2_mi_build_design(
      frame, model_id, transform_bundle, rule
    ))
  )
  if (!identical(columns[[1L]], columns[[2L]])) {
    stop("MIMIC/eICU design columns differ for ", model_id, ".")
  }
}
if (any(!design_audit$pass)) {
  stop("Missingness-sensitivity design coverage failed.")
}

attr(mimic_imputed, "freeze_metadata") <- list(
  artifact_version = "mimic_all_tuple_missingness_predictors_v2",
  database = "MIMIC-IV",
  source_sha256 = v2_pm_sha256_file(paths$mimic_joined),
  all_tuple_positive = TRUE,
  outcome_fields_read = FALSE,
  missingness_rule = rule$artifact_version,
  indicator_variables = rule$indicator_variables
)
attr(eicu_imputed, "freeze_metadata") <- list(
  artifact_version = "eicu_all_tuple_missingness_predictors_v2",
  database = "eICU-CRD",
  source_sha256 = v2_pm_sha256_file(paths$eicu_joined),
  all_tuple_positive = TRUE,
  outcome_fields_read = FALSE,
  missingness_rule = rule$artifact_version,
  indicator_variables = rule$indicator_variables
)

frozen_bundle <- list(
  artifact_version = "frozen_all_tuple_missingness_bundle_v2",
  locked_config_version = LOCKED_V2$version,
  rule = rule,
  transform_bundle = transform_bundle,
  derivation_database = "MIMIC-IV only",
  derivation_n = nrow(mimic_imputed),
  external_outcomes_used = FALSE,
  manuscript_ci_ready = FALSE
)

mimic_hash <- v2_pm_atomic_save_rds(mimic_imputed, mimic_output)
eicu_hash <- v2_pm_atomic_save_rds(eicu_imputed, eicu_output)
bundle_hash <- v2_pm_atomic_save_rds(frozen_bundle, bundle_output)

original_missingness <- do.call(rbind, lapply(
  list(`MIMIC-IV` = mimic_original, `eICU-CRD` = eicu_original),
  function(frame) {
    database <- unique(frame$database)
    do.call(rbind, lapply(v2_pm_model_columns, function(variable) {
      data.frame(
        database = database,
        variable = variable,
        n = nrow(frame),
        missing_n = sum(is.na(frame[[variable]])),
        missing_fraction = mean(is.na(frame[[variable]])),
        frozen_indicator_added =
          variable %in% rule$indicator_variables,
        representation_variable =
          variable %in% v2_pm_representation_columns,
        stringsAsFactors = FALSE
      )
    }))
  }
))
parameter_table <- v2_mi_rule_parameter_table(
  rule, mimic_original, eicu_original
)

transform_rows <- list()
counter <- 0L
for (group in c("baseline_three_knots", "nonlinear_four_knots")) {
  for (variable in names(transform_bundle[[group]])) {
    values <- transform_bundle[[group]][[variable]]
    for (i in seq_along(values)) {
      counter <- counter + 1L
      transform_rows[[counter]] <- data.frame(
        parameter_group = group,
        variable = variable,
        parameter_index = i,
        value = values[[i]],
        derivation_database = "MIMIC-IV",
        derivation_n = nrow(mimic_imputed),
        quantile_type = transform_bundle$quantile_type,
        external_application = "unchanged",
        stringsAsFactors = FALSE
      )
    }
  }
}
transform_parameters <- do.call(rbind, transform_rows)

aggregate_outputs <- list(
  "all_tuple_missingness_imputation_parameters_v2.csv" =
    parameter_table,
  "all_tuple_missingness_transform_parameters_v2.csv" =
    transform_parameters,
  "all_tuple_missingness_model_design_v2.csv" = design_audit
)
for (name in names(aggregate_outputs)) {
  v2_pm_atomic_write_csv(
    aggregate_outputs[[name]], file.path(aggregate_out, name)
  )
}

v2_pm_atomic_write_csv(
  original_missingness,
  file.path(qc_out, "all_tuple_missingness_original_missingness_v2.csv")
)
v2_pm_atomic_write_csv(
  leakage,
  file.path(qc_out, "all_tuple_missingness_outcome_leakage_guard_v2.csv")
)
v2_pm_atomic_write_csv(
  rbind(mimic_validation$range_qc, eicu_validation$range_qc),
  file.path(qc_out, "all_tuple_missingness_source_range_qc_v2.csv")
)

invariants <- data.frame(
  check = c(
    "locked_indicator_variables_exact",
    "quantile_type_2",
    "mimic_only_parameter_derivation",
    "external_outcomes_forbidden",
    "eicu_novel_missingness_hard_stop",
    "all_models_share_same_indicators",
    "all_tuple_rows_retained_mimic",
    "all_tuple_rows_retained_eicu",
    "representations_complete_before_imputation",
    "imputed_frames_complete",
    "mimic_eicu_design_columns_identical",
    "outcome_leakage_guard_pass"
  ),
  pass = c(
    identical(
      rule$indicator_variables,
      c("map", "platelet", "creatinine")
    ),
    identical(as.integer(rule$quantile_type), 2L),
    identical(rule$derivation_database, "MIMIC-IV only") &&
      identical(
        attr(transform_bundle, "freeze_metadata")$derivation_database,
        "MIMIC-IV only"
      ),
    identical(rule$external_outcomes_used, FALSE),
    identical(rule$external_novel_missingness_policy, "hard STOP"),
    all(design_audit$all_indicator_columns_present),
    nrow(mimic_imputed) == nrow(mimic_original),
    nrow(eicu_imputed) == nrow(eicu_original),
    all(stats::complete.cases(
      mimic_original[v2_pm_representation_columns]
    )) && all(stats::complete.cases(
      eicu_original[v2_pm_representation_columns]
    )),
    all(stats::complete.cases(mimic_imputed[v2_pm_model_columns])) &&
      all(stats::complete.cases(eicu_imputed[v2_pm_model_columns])),
    all(vapply(LOCKED_V2$model_ids, function(model_id) {
      identical(
        colnames(v2_mi_build_design(
          mimic_imputed, model_id, transform_bundle, rule
        )),
        colnames(v2_mi_build_design(
          eicu_imputed, model_id, transform_bundle, rule
        ))
      )
    }, logical(1L))),
    all(leakage$pass)
  ),
  stringsAsFactors = FALSE
)
if (!all(invariants$pass)) {
  stop(
    "All-tuple missingness freeze invariant failure: ",
    paste(invariants$check[!invariants$pass], collapse = ", ")
  )
}
v2_pm_atomic_write_csv(
  invariants,
  file.path(qc_out, "all_tuple_missingness_freeze_invariants_v2.csv")
)

input_manifest <- data.frame(
  artifact = names(paths),
  path = unname(unlist(paths, use.names = FALSE)),
  sha256 = vapply(paths, v2_pm_sha256_file, character(1L)),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  input_manifest,
  file.path(qc_out, "all_tuple_missingness_freeze_input_manifest_v2.csv")
)
output_paths <- c(
  mimic_predictors = mimic_output,
  eicu_predictors = eicu_output,
  frozen_bundle = bundle_output,
  setNames(
    file.path(aggregate_out, names(aggregate_outputs)),
    paste0("aggregate_", names(aggregate_outputs))
  )
)
output_manifest <- data.frame(
  artifact = names(output_paths),
  path = unname(output_paths),
  sha256 = vapply(output_paths, v2_pm_sha256_file, character(1L)),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  output_manifest,
  file.path(qc_out, "all_tuple_missingness_freeze_output_manifest_v2.csv")
)

gate <- data.frame(
  field = c(
    "status",
    "locked_config_version",
    "script_sha256",
    "utils_sha256",
    "mimic_source_sha256",
    "eicu_source_sha256",
    "mimic_predictor_sha256",
    "eicu_predictor_sha256",
    "frozen_bundle_sha256",
    "mimic_all_tuple_n",
    "eicu_all_tuple_n",
    "indicator_variables",
    "indicator_columns",
    "quantile_type",
    "parameter_derivation_database",
    "eicu_novel_missingness_policy",
    "same_indicators_appended_to_all_models",
    "outcome_artifacts_opened",
    "external_outcomes_used",
    "all_invariants_pass",
    "manuscript_ci_ready",
    "completed_at"
  ),
  value = c(
    "PASS",
    LOCKED_V2$version,
    v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(
      file.path(script_dir, "18_missingness_sensitivity_utils.R")
    ),
    v2_pm_sha256_file(paths$mimic_joined),
    v2_pm_sha256_file(paths$eicu_joined),
    mimic_hash,
    eicu_hash,
    bundle_hash,
    as.character(nrow(mimic_imputed)),
    as.character(nrow(eicu_imputed)),
    paste(rule$indicator_variables, collapse = ";"),
    paste(rule$indicator_columns, collapse = ";"),
    as.character(rule$quantile_type),
    rule$derivation_database,
    rule$external_novel_missingness_policy,
    "TRUE",
    "FALSE",
    "FALSE",
    "TRUE",
    "FALSE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

message("REBUILD_V2_ALL_TUPLE_MISSINGNESS_FREEZE_PASS")
message(
  "  MIMIC: ", nrow(mimic_imputed),
  "; eICU: ", nrow(eicu_imputed),
  "; indicators: ", paste(rule$indicator_variables, collapse = ", ")
)
