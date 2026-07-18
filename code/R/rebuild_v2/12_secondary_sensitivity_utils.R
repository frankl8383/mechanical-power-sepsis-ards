# ARDS mechanical-power rebuild v2: secondary/sensitivity point-estimate helpers
#
# This module contains no top-level artifact access. It implements only the
# limited analyses prespecified in SAP v2.0.0 and deliberately contains no
# bootstrap, outcome-selected transformation, threshold search, or nonlinear
# algebraic-energy expansion.

v2_ss_model_performance <- function(
    y,
    predictions,
    database,
    analysis,
    model_roles = NULL,
    weights = NULL) {
  v2_assert_binary_outcome(y)
  predictions <- as.matrix(predictions)
  if (!is.numeric(predictions) || nrow(predictions) != length(y) ||
      is.null(colnames(predictions)) || anyDuplicated(colnames(predictions)) ||
      anyNA(predictions) || any(!is.finite(predictions))) {
    stop("Invalid prediction matrix for ", analysis, ".")
  }
  if (!is.null(model_roles)) {
    if (is.null(names(model_roles)) ||
        !all(colnames(predictions) %in% names(model_roles))) {
      stop("Model-role dictionary is incomplete for ", analysis, ".")
    }
  }
  if (!is.null(weights) &&
      (!is.numeric(weights) || length(weights) != length(y) ||
       anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0))) {
    stop("Invalid performance weights for ", analysis, ".")
  }
  rows <- lapply(colnames(predictions), function(model_id) {
    score <- if (is.null(weights)) {
      v2_binary_performance(y, predictions[, model_id])
    } else {
      v2_weighted_performance(y, predictions[, model_id], weights)
    }
    data.frame(
      database = database,
      analysis = analysis,
      model_id = model_id,
      model_role = if (is.null(model_roles)) "" else model_roles[[model_id]],
      weighting = if (is.null(weights)) "unweighted" else "frozen_joint_ipw",
      metric = names(score),
      estimate = as.numeric(score),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_ss_paired_differences <- function(
    y,
    predictions,
    comparisons,
    database,
    analysis,
    weights = NULL) {
  v2_assert_binary_outcome(y)
  predictions <- as.matrix(predictions)
  required <- c("candidate_model", "reference_model", "comparison_role")
  v2_require_columns(comparisons, required, "secondary comparison table")
  if (any(!unique(c(
    comparisons$candidate_model,
    comparisons$reference_model
  )) %in% colnames(predictions))) {
    stop("Comparison refers to an absent prediction column.")
  }
  if (!is.null(weights) &&
      (!is.numeric(weights) || length(weights) != length(y) ||
       anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0))) {
    stop("Invalid paired-difference weights.")
  }
  rows <- lapply(seq_len(nrow(comparisons)), function(i) {
    candidate <- comparisons$candidate_model[[i]]
    reference <- comparisons$reference_model[[i]]
    if (is.null(weights)) {
      delta <- v2_paired_metric_difference(
        y, predictions[, candidate], predictions[, reference]
      )
    } else {
      candidate_score <- v2_weighted_performance(
        y, predictions[, candidate], weights
      )
      reference_score <- v2_weighted_performance(
        y, predictions[, reference], weights
      )
      metrics <- c("brier", "log_loss", "c_statistic")
      delta <- candidate_score[metrics] - reference_score[metrics]
      names(delta) <- paste0("delta_", metrics)
    }
    data.frame(
      database = database,
      analysis = analysis,
      candidate_model = candidate,
      reference_model = reference,
      comparison_role = comparisons$comparison_role[[i]],
      weighting = if (is.null(weights)) "unweighted" else "frozen_joint_ipw",
      metric = names(delta),
      estimate = as.numeric(delta),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_ss_fit_apply <- function(
    mimic_frame,
    eicu_frame,
    model_designers,
    model_roles,
    weighted = FALSE,
    mimic_weight_column = "selection_weight") {
  required <- c("analysis_id", "outcome")
  v2_require_columns(mimic_frame, required, "MIMIC sensitivity frame")
  v2_require_columns(eicu_frame, required, "eICU sensitivity frame")
  if (!is.list(model_designers) || is.null(names(model_designers)) ||
      anyDuplicated(names(model_designers)) ||
      !all(vapply(model_designers, is.function, logical(1L)))) {
    stop("Named model-designer functions are required.")
  }
  if (is.null(names(model_roles)) ||
      !identical(names(model_designers), names(model_roles))) {
    stop("Model roles must follow the model-designer order exactly.")
  }
  if (weighted) {
    v2_require_columns(
      mimic_frame, mimic_weight_column, "weighted MIMIC sensitivity frame"
    )
  }
  fits <- list()
  mimic_predictions <- matrix(
    NA_real_, nrow = nrow(mimic_frame), ncol = length(model_designers),
    dimnames = list(NULL, names(model_designers))
  )
  eicu_predictions <- matrix(
    NA_real_, nrow = nrow(eicu_frame), ncol = length(model_designers),
    dimnames = list(NULL, names(model_designers))
  )
  design_audit <- list()
  for (model_id in names(model_designers)) {
    mimic_design <- model_designers[[model_id]](mimic_frame)
    eicu_design <- model_designers[[model_id]](eicu_frame)
    if (!identical(colnames(mimic_design), colnames(eicu_design))) {
      stop("MIMIC/eICU design mismatch for ", model_id, ".")
    }
    fit <- if (weighted) {
      v2_fit_weighted_logistic(
        mimic_design,
        mimic_frame$outcome,
        mimic_frame[[mimic_weight_column]],
        model_id,
        mimic_frame$analysis_id
      )
    } else {
      v2_fit_logistic(
        mimic_design,
        mimic_frame$outcome,
        model_id,
        mimic_frame$analysis_id
      )
    }
    fits[[model_id]] <- fit
    mimic_predictions[, model_id] <- stats::predict(
      fit, mimic_design, type = "response"
    )
    eicu_predictions[, model_id] <- stats::predict(
      fit, eicu_design, type = "response"
    )
    design_audit[[model_id]] <- data.frame(
      model_id = model_id,
      model_role = model_roles[[model_id]],
      total_parameter_n = length(fit$coefficients),
      incremental_parameter_n =
        ncol(mimic_design) -
        ncol(model_designers[[names(model_designers)[[1L]]]](mimic_frame)),
      design_columns_identical_external = TRUE,
      mimic_n = nrow(mimic_frame),
      eicu_n = nrow(eicu_frame),
      converged = isTRUE(fit$converged),
      stringsAsFactors = FALSE
    )
  }
  list(
    fits = fits,
    mimic_predictions = mimic_predictions,
    eicu_predictions = eicu_predictions,
    design_audit = do.call(rbind, design_audit)
  )
}

v2_ss_attach_compliance_normalization <- function(
    frame,
    exposure,
    database,
    tolerance = 1e-8) {
  v2_require_columns(
    frame,
    c("analysis_id", "smp", "driving_pressure"),
    paste(database, "primary common set")
  )
  contract <- if (database == "MIMIC-IV") {
    list(id = "stay_id", delta_p = "delta_p", vt = "vt_value")
  } else if (database == "eICU-CRD") {
    list(
      id = "patientunitstayid", delta_p = "delta_p", vt = "vt_value"
    )
  } else {
    stop("Unsupported compliance-normalization database: ", database)
  }
  v2_require_columns(
    exposure,
    c(contract$id, contract$delta_p, contract$vt, "smp", "tuple_observed"),
    paste(database, "primary tuple artifact")
  )
  frame_id <- as.character(frame$analysis_id)
  exposure_id <- as.character(exposure[[contract$id]])
  if (anyNA(exposure_id) || any(!nzchar(exposure_id)) ||
      anyDuplicated(exposure_id)) {
    stop(database, " primary tuple IDs are invalid.")
  }
  position <- match(frame_id, exposure_id)
  if (anyNA(position)) {
    stop(database, " primary tuple artifact lacks common-set IDs.")
  }
  selected <- exposure[position, , drop = FALSE]
  if (!all(as.logical(selected$tuple_observed))) {
    stop(database, " common-set rows are not all primary tuple-positive.")
  }
  selected_delta <- as.numeric(selected[[contract$delta_p]])
  selected_smp <- as.numeric(selected$smp)
  selected_vt <- as.numeric(selected[[contract$vt]])
  if (anyNA(selected_delta) || anyNA(selected_smp) || anyNA(selected_vt) ||
      any(!is.finite(selected_delta)) || any(!is.finite(selected_smp)) ||
      any(!is.finite(selected_vt)) ||
      max(abs(selected_delta - frame$driving_pressure)) > tolerance ||
      max(abs(selected_smp - frame$smp)) > tolerance) {
    stop(database, " tuple/frame representation identity mismatch.")
  }
  if (any(selected_vt < 100 | selected_vt > 1500)) {
    stop(database, " tidal volume is outside the locked valid range.")
  }
  positive_driving_pressure <- frame$driving_pressure > 0
  compliance <- rep(NA_real_, nrow(frame))
  normalized <- rep(NA_real_, nrow(frame))
  compliance[positive_driving_pressure] <-
    (selected_vt[positive_driving_pressure] / 1000) /
    frame$driving_pressure[positive_driving_pressure]
  normalized[positive_driving_pressure] <-
    frame$smp[positive_driving_pressure] /
    compliance[positive_driving_pressure]
  valid <- positive_driving_pressure &
    is.finite(compliance) & compliance > 0 &
    is.finite(normalized) & normalized >= 0
  out <- as.data.frame(frame, stringsAsFactors = FALSE)
  out$tidal_volume_mL <- selected_vt
  out$compliance_L_per_cmH2O <- compliance
  out$compliance_normalized_smp_raw <- normalized
  qc <- data.frame(
    database = database,
    input_common_set_n = nrow(frame),
    positive_driving_pressure_n = sum(positive_driving_pressure),
    zero_driving_pressure_n = sum(frame$driving_pressure == 0),
    invalid_or_missing_normalized_n = sum(!valid),
    analysis_n = sum(valid),
    maximum_smp_identity_error =
      max(abs(selected_smp - frame$smp)),
    maximum_driving_pressure_identity_error =
      max(abs(selected_delta - frame$driving_pressure)),
    primary_tuple_reselected = FALSE,
    stringsAsFactors = FALSE
  )
  list(frame = out[valid, , drop = FALSE], qc = qc)
}

v2_ss_scale_compliance_normalization <- function(
    mimic_frame,
    eicu_frame) {
  variable <- "compliance_normalized_smp_raw"
  v2_require_columns(mimic_frame, variable, "MIMIC compliance frame")
  v2_require_columns(eicu_frame, variable, "eICU compliance frame")
  center <- stats::median(mimic_frame[[variable]])
  scale <- stats::IQR(mimic_frame[[variable]], type = 2L)
  if (!is.finite(center) || !is.finite(scale) ||
      scale <= .Machine$double.eps^0.5) {
    stop("MIMIC compliance-normalized sMP scale is invalid.")
  }
  mimic_frame$compliance_normalized_smp_scaled <-
    (mimic_frame[[variable]] - center) / scale
  eicu_frame$compliance_normalized_smp_scaled <-
    (eicu_frame[[variable]] - center) / scale
  if (anyNA(mimic_frame$compliance_normalized_smp_scaled) ||
      anyNA(eicu_frame$compliance_normalized_smp_scaled) ||
      any(!is.finite(mimic_frame$compliance_normalized_smp_scaled)) ||
      any(!is.finite(eicu_frame$compliance_normalized_smp_scaled))) {
    stop("Frozen compliance-normalization transform produced invalid values.")
  }
  list(
    mimic = mimic_frame,
    eicu = eicu_frame,
    parameters = data.frame(
      variable = variable,
      center = center,
      scale = scale,
      scale_definition = "MIMIC IQR, quantile type 2",
      derivation_database = "MIMIC-IV only",
      external_application = "applied unchanged to eICU-CRD",
      stringsAsFactors = FALSE
    )
  )
}

v2_ss_attach_rate_quality <- function(frame, flags, database) {
  v2_require_columns(
    frame, "analysis_id", paste(database, "primary common set")
  )
  v2_require_columns(
    flags,
    c(
      "analysis_id", "preferred_source_primary_tuple",
      "rate_concordant", "rate_concordant_preferred_source",
      "selected_total_rr_reproduced"
    ),
    paste(database, "rate-quality flags")
  )
  metadata <- attr(flags, "rebuild_metadata")
  if (!is.list(metadata) || !isTRUE(metadata$outcome_blind) ||
      !identical(metadata$tuple_reselection, FALSE)) {
    stop(database, " rate-quality artifact does not prove no tuple reselection.")
  }
  flag_id <- as.character(flags$analysis_id)
  if (anyNA(flag_id) || any(!nzchar(flag_id)) || anyDuplicated(flag_id)) {
    stop(database, " rate-quality IDs are invalid.")
  }
  position <- match(as.character(frame$analysis_id), flag_id)
  if (anyNA(position)) {
    stop(database, " rate-quality flags lack common-set IDs.")
  }
  out <- as.data.frame(frame, stringsAsFactors = FALSE)
  for (column in c(
    "preferred_source_primary_tuple", "rate_concordant",
    "rate_concordant_preferred_source", "selected_total_rr_reproduced"
  )) {
    out[[column]] <- flags[[column]][position]
  }
  if (anyNA(out$rate_concordant_preferred_source)) {
    stop(database, " combined rate/preferred flag is incomplete.")
  }
  out
}

v2_ss_extract_joint_ipw_table <- function(
    selection_object,
    expected_database,
    expected_model_role = "joint_always_observed_ipw") {
  if (!is.list(selection_object) ||
      !identical(selection_object$database, expected_database) ||
      !isTRUE(selection_object$outcome_blind) ||
      !is.list(selection_object$models)) {
    stop("Malformed selection-weight artifact for ", expected_database, ".")
  }
  candidates <- Filter(
    function(model) {
      is.list(model) &&
        identical(model$model_role, expected_model_role) &&
        identical(model$covariate_specification, "always_observed_only") &&
        isTRUE(model$endpoint_weight_eligible)
    },
    selection_object$models
  )
  if (length(candidates) != 1L) {
    stop(
      expected_database,
      " must contain exactly one eligible joint always-observed IPW model."
    )
  }
  selected <- candidates[[1L]]
  if (!identical(
    selected$selection_target,
    "valid_tuple_and_complete_no_gcs_core"
  )) {
    stop("Wrong selection target in ", expected_database, " weight model.")
  }
  table <- as.data.frame(selected$included_weights)
  v2_require_columns(
    table,
    c(
      "row_id", "stabilized_weight_truncated",
      "model_role", "covariate_specification",
      "permitted_for_outcome_weighting"
    ),
    paste(expected_database, " eligible joint weight table")
  )
  if (!all(table$permitted_for_outcome_weighting) ||
      !all(table$model_role == expected_model_role) ||
      !all(table$covariate_specification == "always_observed_only")) {
    stop(expected_database, " weight permission/provenance check failed.")
  }
  list(table = table, model = selected)
}

v2_ss_distribution_summary <- function(
    x, database, variable, analysis) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop("Distribution summary requires complete finite numeric values.")
  }
  q <- as.numeric(stats::quantile(
    x, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
    names = FALSE, type = 2L
  ))
  data.frame(
    database = database,
    analysis = analysis,
    variable = variable,
    n = length(x),
    minimum = q[[1L]],
    q01 = q[[2L]],
    q05 = q[[3L]],
    q25 = q[[4L]],
    median = q[[5L]],
    q75 = q[[6L]],
    q95 = q[[7L]],
    q99 = q[[8L]],
    maximum = q[[9L]],
    stringsAsFactors = FALSE
  )
}
