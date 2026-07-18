# ARDS mechanical-power rebuild v2:
# frozen-median plus missing-indicator sensitivity helpers
#
# This module has no top-level artifact access. Its purpose is to enforce the
# SAP v2 missing-predictor sensitivity without allowing eICU data or outcomes
# to influence imputation parameters, indicator selection, transformations, or
# model design.

v2_mi_locked_indicator_variables <- c(
  "map", "platelet", "creatinine"
)

v2_mi_indicator_name <- function(variable) {
  paste0(variable, "_missing_indicator")
}

v2_mi_validate_rule <- function(rule) {
  required <- c(
    "artifact_version", "derivation_database", "quantile_type",
    "indicator_variables", "indicator_columns", "medians",
    "external_outcomes_used", "external_novel_missingness_policy"
  )
  if (!is.list(rule) || any(!required %in% names(rule))) {
    stop("Malformed frozen missingness rule.")
  }
  if (!identical(
    as.character(rule$indicator_variables),
    v2_mi_locked_indicator_variables
  )) {
    stop("Frozen indicator-variable order is not the locked SAP order.")
  }
  expected_columns <- unname(vapply(
    v2_mi_locked_indicator_variables,
    v2_mi_indicator_name,
    character(1L)
  ))
  if (!identical(as.character(rule$indicator_columns), expected_columns)) {
    stop("Frozen missing-indicator column names are invalid.")
  }
  medians <- rule$medians
  if (!is.numeric(medians) || is.null(names(medians)) ||
      !identical(names(medians), v2_mi_locked_indicator_variables) ||
      anyNA(medians) || any(!is.finite(medians))) {
    stop("Frozen MIMIC medians are invalid.")
  }
  if (!identical(rule$derivation_database, "MIMIC-IV only") ||
      !identical(as.integer(rule$quantile_type), 2L) ||
      !identical(rule$external_outcomes_used, FALSE) ||
      !identical(
        rule$external_novel_missingness_policy,
        "hard STOP"
      )) {
    stop("Frozen missingness rule violates the external-validation contract.")
  }
  invisible(TRUE)
}

v2_mi_derive_rule <- function(mimic_frame) {
  if (!exists("v2_pm_assert_outcome_free", mode = "function", inherits = TRUE) ||
      !exists("v2_pm_baseline_columns", inherits = TRUE)) {
    stop("Source 09_primary_model_utils.R before deriving the rule.")
  }
  v2_pm_assert_outcome_free(
    mimic_frame,
    "MIMIC all-tuple missingness-rule derivation frame"
  )
  v2_pm_require_columns(
    mimic_frame,
    c(v2_pm_baseline_columns, v2_pm_representation_columns),
    "MIMIC all-tuple missingness-rule derivation frame"
  )
  baseline_missing <- v2_pm_baseline_columns[
    vapply(
      mimic_frame[v2_pm_baseline_columns],
      function(x) any(is.na(x)),
      logical(1L)
    )
  ]
  if (!identical(baseline_missing, v2_mi_locked_indicator_variables)) {
    stop(
      "MIMIC baseline missingness does not equal the locked indicator set: ",
      paste(baseline_missing, collapse = ", ")
    )
  }
  if (any(!stats::complete.cases(
    mimic_frame[v2_pm_representation_columns]
  ))) {
    stop("A tuple representation is missing in the all-tuple MIMIC frame.")
  }
  medians <- setNames(vapply(
    v2_mi_locked_indicator_variables,
    function(variable) {
      value <- mimic_frame[[variable]]
      observed <- value[!is.na(value)]
      if (!is.numeric(value) || !length(observed) ||
          any(!is.finite(observed))) {
        stop("Invalid MIMIC values for median derivation: ", variable)
      }
      as.numeric(stats::quantile(
        observed,
        probs = 0.5,
        names = FALSE,
        type = 2L
      ))
    },
    numeric(1L)
  ), v2_mi_locked_indicator_variables)
  rule <- list(
    artifact_version = "frozen_all_tuple_missingness_rule_v2",
    derivation_database = "MIMIC-IV only",
    derivation_population =
      "all fixed-6h tuple-positive MIMIC patients before outcome access",
    quantile_type = 2L,
    indicator_variables = v2_mi_locked_indicator_variables,
    indicator_columns = unname(vapply(
      v2_mi_locked_indicator_variables,
      v2_mi_indicator_name,
      character(1L)
    )),
    medians = medians,
    external_application = "apply unchanged to all tuple-positive eICU rows",
    external_outcomes_used = FALSE,
    external_novel_missingness_policy = "hard STOP",
    same_indicators_appended_to_all_models = TRUE
  )
  v2_mi_validate_rule(rule)
  rule
}

v2_mi_apply_rule <- function(
    frame,
    rule,
    database = c("MIMIC-IV", "eICU-CRD")) {
  database <- match.arg(database)
  v2_mi_validate_rule(rule)
  v2_pm_assert_outcome_free(
    frame,
    paste(database, "all-tuple missingness sensitivity input")
  )
  v2_pm_require_columns(
    frame,
    c(v2_pm_model_columns, "core_complete", "analysis_id"),
    paste(database, "all-tuple missingness sensitivity input")
  )
  original_id <- as.character(frame$analysis_id)
  if (anyNA(original_id) || any(!nzchar(original_id)) ||
      anyDuplicated(original_id)) {
    stop(database, " all-tuple IDs are invalid.")
  }
  novel_missing <- setdiff(
    v2_pm_baseline_columns[
      vapply(
        frame[v2_pm_baseline_columns],
        function(x) any(is.na(x)),
        logical(1L)
      )
    ],
    rule$indicator_variables
  )
  if (length(novel_missing)) {
    stop(
      database, " contains baseline missingness not represented by the ",
      "MIMIC-derived frozen rule: ", paste(novel_missing, collapse = ", ")
    )
  }
  if (any(!stats::complete.cases(
    frame[v2_pm_representation_columns]
  ))) {
    stop(database, " has missing tuple representations.")
  }
  out <- as.data.frame(frame, stringsAsFactors = FALSE)
  missing_counts <- setNames(integer(length(rule$indicator_variables)),
                             rule$indicator_variables)
  for (variable in rule$indicator_variables) {
    indicator <- is.na(out[[variable]])
    missing_counts[[variable]] <- sum(indicator)
    indicator_name <- v2_mi_indicator_name(variable)
    if (indicator_name %in% names(out)) {
      stop("Missing-indicator column already exists: ", indicator_name)
    }
    out[[indicator_name]] <- as.numeric(indicator)
    out[[variable]][indicator] <- rule$medians[[variable]]
  }
  if (any(!stats::complete.cases(out[v2_pm_model_columns])) ||
      any(!vapply(
        out[rule$indicator_columns],
        function(x) is.numeric(x) && !anyNA(x) &&
          all(is.finite(x)) && all(x %in% c(0, 1)),
        logical(1L)
      ))) {
    stop(database, " frozen missingness rule did not create a valid frame.")
  }
  out$core_complete <- TRUE
  if (!identical(as.character(out$analysis_id), original_id)) {
    stop(database, " row order changed while applying the missingness rule.")
  }
  attr(out, "missingness_rule_application") <- list(
    database = database,
    frozen_rule_version = rule$artifact_version,
    derivation_database = rule$derivation_database,
    indicator_variables = rule$indicator_variables,
    indicator_columns = rule$indicator_columns,
    missing_counts = missing_counts,
    external_outcomes_used = FALSE
  )
  out
}

v2_mi_build_design <- function(frame, model_id, transform_bundle, rule) {
  v2_mi_validate_rule(rule)
  base <- v2_build_design(frame, model_id, transform_bundle)
  v2_pm_require_columns(
    frame,
    rule$indicator_columns,
    paste(model_id, "missing-indicator model frame")
  )
  indicators <- as.matrix(frame[rule$indicator_columns])
  storage.mode(indicators) <- "double"
  colnames(indicators) <- rule$indicator_columns
  design <- cbind(base, indicators)
  if (anyNA(design) || any(!is.finite(design)) ||
      anyDuplicated(colnames(design))) {
    stop("Invalid missingness-sensitivity design for ", model_id)
  }
  design
}

v2_mi_rule_parameter_table <- function(rule, mimic_frame, eicu_frame) {
  v2_mi_validate_rule(rule)
  do.call(rbind, lapply(rule$indicator_variables, function(variable) {
    data.frame(
      variable = variable,
      median = unname(rule$medians[[variable]]),
      quantile_type = rule$quantile_type,
      derivation_database = rule$derivation_database,
      mimic_derivation_n = nrow(mimic_frame),
      mimic_missing_n = sum(is.na(mimic_frame[[variable]])),
      eicu_missing_n = sum(is.na(eicu_frame[[variable]])),
      indicator_column = v2_mi_indicator_name(variable),
      external_application = rule$external_application,
      external_outcomes_used = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}
