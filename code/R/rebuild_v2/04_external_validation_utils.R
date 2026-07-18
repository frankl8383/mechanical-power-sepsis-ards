# ARDS mechanical-power rebuild v2: external-validation utilities
#
# This module evaluates already-frozen patient-level predictions. It does not
# fit or alter the development models and it opens no project artifact.
#
# Design safeguards:
#   * every model is stored in one prediction-set object with one immutable
#     patient row order;
#   * raw external validation is reported separately from model updating;
#   * all paired model contrasts use exactly the same bootstrap rows;
#   * hospital-cluster bootstrap failures are retained and a prespecified
#     >=95% completion gate controls whether confidence intervals are emitted;
#   * flexible calibration bands use knots and grids frozen in the original
#     external sample and are pointwise, not simultaneous, intervals.

v2_ev_metric_names <- c(
  "brier",
  "log_loss",
  "c_statistic",
  "calibration_in_the_large",
  "calibration_intercept",
  "calibration_slope",
  "observed_expected_ratio"
)

v2_ev_difference_metric_names <- c(
  "brier", "log_loss", "c_statistic"
)

v2_ev_assert_scalar_integer <- function(x, label, minimum = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x != as.integer(x) || x < minimum) {
    stop(label, " must be one integer >= ", minimum, ".")
  }
  invisible(TRUE)
}

v2_ev_assert_fraction <- function(x, label, lower_open = TRUE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x)) {
    stop(label, " must be in ", if (lower_open) "(0, 1]." else "[0, 1].")
  }
  valid_lower <- if (lower_open) x > 0 else x >= 0
  if (!valid_lower || x > 1) {
    stop(label, " must be in ", if (lower_open) "(0, 1]." else "[0, 1].")
  }
  invisible(TRUE)
}

v2_ev_assert_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be in (0, 1).")
  }
  invisible(TRUE)
}

v2_ev_assert_dependencies <- function() {
  required <- c(
    "v2_assert_binary_outcome",
    "v2_clip_probability",
    "v2_binary_performance",
    "v2_paired_metric_difference"
  )
  missing <- required[!vapply(
    required, exists, logical(1L), mode = "function", inherits = TRUE
  )]
  if (length(missing)) {
    stop(
      "Source 01_analysis_utils.R before this module; missing function(s): ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

v2_ev_prediction_set <- function(
    data,
    id_column,
    outcome_column,
    hospital_column,
    model_columns,
    set_id = "external_raw_frozen_predictions") {
  v2_ev_assert_dependencies()
  if (!is.data.frame(data) || nrow(data) < 2L) {
    stop("data must be a data frame with at least two rows.")
  }
  scalar_names <- list(
    id_column = id_column,
    outcome_column = outcome_column,
    hospital_column = hospital_column,
    set_id = set_id
  )
  invalid_scalar <- names(scalar_names)[!vapply(
    scalar_names,
    function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    },
    logical(1L)
  )]
  if (length(invalid_scalar)) {
    stop(
      "The following arguments must be non-empty scalar strings: ",
      paste(invalid_scalar, collapse = ", ")
    )
  }
  if (!is.character(model_columns) || !length(model_columns) ||
      is.null(names(model_columns)) || anyNA(model_columns) ||
      any(!nzchar(model_columns)) || anyNA(names(model_columns)) ||
      any(!nzchar(names(model_columns))) ||
      anyDuplicated(model_columns) || anyDuplicated(names(model_columns))) {
    stop(
      "model_columns must be a uniquely named character vector mapping ",
      "model IDs to prediction columns."
    )
  }
  if (anyDuplicated(c(id_column, outcome_column, hospital_column)) ||
      any(unname(model_columns) %in%
          c(id_column, outcome_column, hospital_column))) {
    stop(
      "ID, outcome, hospital, and model prediction columns must be distinct."
    )
  }
  requested <- c(
    id_column, outcome_column, hospital_column, unname(model_columns)
  )
  missing <- setdiff(requested, names(data))
  if (length(missing)) {
    stop("Prediction data lack column(s): ", paste(missing, collapse = ", "))
  }

  row_id <- as.character(data[[id_column]])
  if (anyNA(row_id) || any(!nzchar(row_id)) || anyDuplicated(row_id)) {
    stop("Patient row identifiers must be complete, non-empty, and unique.")
  }
  outcome <- data[[outcome_column]]
  v2_assert_binary_outcome(outcome)
  outcome <- as.integer(outcome)
  hospital <- as.character(data[[hospital_column]])
  if (anyNA(hospital) || any(!nzchar(hospital))) {
    stop("Hospital identifiers must be complete and non-empty.")
  }
  if (length(unique(hospital)) < 2L) {
    stop("At least two hospitals are required for external validation.")
  }

  prediction_list <- lapply(unname(model_columns), function(column) {
    value <- data[[column]]
    if (!is.numeric(value) || length(value) != nrow(data) ||
        anyNA(value) || any(!is.finite(value)) ||
        any(value < 0 | value > 1)) {
      stop(
        "Prediction column ", column,
        " must contain complete finite probabilities in [0, 1]."
      )
    }
    as.numeric(value)
  })
  predictions <- do.call(cbind, prediction_list)
  colnames(predictions) <- names(model_columns)
  storage.mode(predictions) <- "double"

  structure(
    list(
      set_id = set_id,
      prediction_type = "raw_frozen_external",
      row_ids = row_id,
      outcome = outcome,
      hospital = hospital,
      predictions = predictions,
      source_columns = list(
        id = id_column,
        outcome = outcome_column,
        hospital = hospital_column,
        models = model_columns
      ),
      n = nrow(data),
      events = sum(outcome),
      hospitals = length(unique(hospital))
    ),
    class = "ards_v2_external_prediction_set"
  )
}

v2_ev_prediction_set_from_model_frames <- function(
    model_frames,
    id_column,
    outcome_column,
    hospital_column,
    probability_column,
    set_id = "external_raw_frozen_predictions") {
  if (!is.list(model_frames) || !length(model_frames) ||
      is.null(names(model_frames)) || anyNA(names(model_frames)) ||
      any(!nzchar(names(model_frames))) || anyDuplicated(names(model_frames))) {
    stop("model_frames must be a uniquely named non-empty list.")
  }
  column_arguments <- list(
    id_column = id_column,
    outcome_column = outcome_column,
    hospital_column = hospital_column,
    probability_column = probability_column
  )
  if (any(!vapply(
    column_arguments,
    function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    },
    logical(1L)
  ))) {
    stop("All model-frame column arguments must be scalar strings.")
  }
  required <- unname(unlist(column_arguments, use.names = FALSE))
  if (anyDuplicated(required)) {
    stop("Model-frame ID, outcome, hospital, and probability columns differ.")
  }
  invalid_frame <- names(model_frames)[!vapply(
    model_frames, is.data.frame, logical(1L)
  )]
  if (length(invalid_frame)) {
    stop(
      "The following model frames are not data frames: ",
      paste(invalid_frame, collapse = ", ")
    )
  }
  missing <- lapply(model_frames, function(frame) setdiff(required, names(frame)))
  if (any(lengths(missing))) {
    detail <- vapply(
      names(missing)[lengths(missing) > 0L],
      function(id) paste0(id, " [", paste(missing[[id]], collapse = ", "), "]"),
      character(1L)
    )
    stop("Model frames lack required columns: ", paste(detail, collapse = "; "))
  }

  reference <- model_frames[[1L]]
  reference_id <- as.character(reference[[id_column]])
  reference_outcome <- reference[[outcome_column]]
  reference_hospital <- as.character(reference[[hospital_column]])
  for (model_id in names(model_frames)[-1L]) {
    frame <- model_frames[[model_id]]
    if (!identical(as.character(frame[[id_column]]), reference_id) ||
        !identical(frame[[outcome_column]], reference_outcome) ||
        !identical(
          as.character(frame[[hospital_column]]), reference_hospital
        )) {
      stop(
        "Model frame ", model_id,
        " does not contain exactly the same patients in the same order with ",
        "identical outcomes and hospitals."
      )
    }
  }

  combined <- data.frame(
    .v2_ev_id = reference_id,
    .v2_ev_outcome = reference_outcome,
    .v2_ev_hospital = reference_hospital,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  internal_columns <- sprintf(".v2_ev_prediction_%03d", seq_along(model_frames))
  for (j in seq_along(model_frames)) {
    combined[[internal_columns[j]]] <-
      model_frames[[j]][[probability_column]]
  }
  model_columns <- setNames(internal_columns, names(model_frames))
  v2_ev_prediction_set(
    combined,
    id_column = ".v2_ev_id",
    outcome_column = ".v2_ev_outcome",
    hospital_column = ".v2_ev_hospital",
    model_columns = model_columns,
    set_id = set_id
  )
}

v2_ev_validate_prediction_set <- function(x, require_raw = FALSE) {
  v2_ev_assert_dependencies()
  if (!inherits(x, "ards_v2_external_prediction_set")) {
    stop("Expected an ards_v2_external_prediction_set object.")
  }
  required <- c(
    "set_id", "prediction_type", "row_ids", "outcome", "hospital",
    "predictions", "n", "events", "hospitals"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Malformed prediction set; missing: ", paste(missing, collapse = ", "))
  }
  if (require_raw && !identical(x$prediction_type, "raw_frozen_external")) {
    stop("This analysis is restricted to raw frozen external predictions.")
  }
  if (length(x$row_ids) != x$n || anyNA(x$row_ids) ||
      anyDuplicated(x$row_ids) ||
      length(x$outcome) != x$n || length(x$hospital) != x$n ||
      nrow(x$predictions) != x$n || ncol(x$predictions) < 1L ||
      is.null(colnames(x$predictions)) ||
      anyDuplicated(colnames(x$predictions))) {
    stop("Malformed prediction set dimensions or row identifiers.")
  }
  v2_assert_binary_outcome(x$outcome)
  if (anyNA(x$hospital) || any(!nzchar(x$hospital)) ||
      length(unique(x$hospital)) < 2L) {
    stop("Malformed hospital vector in prediction set.")
  }
  if (!is.numeric(x$predictions) || anyNA(x$predictions) ||
      any(!is.finite(x$predictions)) ||
      any(x$predictions < 0 | x$predictions > 1)) {
    stop("Malformed prediction matrix.")
  }
  invisible(TRUE)
}

v2_ev_assert_same_rows <- function(x, y) {
  v2_ev_validate_prediction_set(x)
  v2_ev_validate_prediction_set(y)
  if (!identical(x$row_ids, y$row_ids) ||
      !identical(x$outcome, y$outcome) ||
      !identical(x$hospital, y$hospital)) {
    stop(
      "Prediction sets do not contain exactly the same patients in the same ",
      "order with identical outcomes and hospitals."
    )
  }
  invisible(TRUE)
}

v2_ev_validate_comparisons <- function(comparisons, model_ids) {
  if (is.null(comparisons)) {
    return(data.frame(
      candidate_model = character(),
      reference_model = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(comparisons) ||
      !all(c("candidate_model", "reference_model") %in% names(comparisons))) {
    stop(
      "comparisons must contain candidate_model and reference_model columns."
    )
  }
  out <- comparisons[c("candidate_model", "reference_model")]
  out$candidate_model <- as.character(out$candidate_model)
  out$reference_model <- as.character(out$reference_model)
  if (anyNA(out$candidate_model) || anyNA(out$reference_model) ||
      any(!nzchar(out$candidate_model)) ||
      any(!nzchar(out$reference_model)) ||
      any(out$candidate_model == out$reference_model)) {
    stop("Every model comparison must name two distinct non-empty models.")
  }
  unknown <- setdiff(
    unique(c(out$candidate_model, out$reference_model)),
    model_ids
  )
  if (length(unknown)) {
    stop("Unknown comparison model(s): ", paste(unknown, collapse = ", "))
  }
  key <- paste(out$candidate_model, out$reference_model, sep = "\r")
  if (anyDuplicated(key)) stop("Duplicate paired model comparisons.")
  rownames(out) <- NULL
  out
}

v2_ev_performance_wide <- function(y, predictions, analysis_label) {
  if (!is.matrix(predictions) || is.null(colnames(predictions)) ||
      anyDuplicated(colnames(predictions))) {
    stop("predictions must be a matrix with unique model IDs.")
  }
  rows <- lapply(seq_len(ncol(predictions)), function(j) {
    performance <- v2_binary_performance(y, predictions[, j])
    if (anyNA(performance) || any(!is.finite(performance))) {
      stop(
        "Non-finite external performance for model ",
        colnames(predictions)[j], "."
      )
    }
    data.frame(
      analysis = analysis_label,
      model_id = colnames(predictions)[j],
      as.list(performance),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

v2_ev_raw_performance <- function(prediction_set) {
  v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)
  v2_ev_performance_wide(
    prediction_set$outcome,
    prediction_set$predictions,
    "raw_frozen_external_validation"
  )
}

v2_ev_paired_differences <- function(
    prediction_set,
    comparisons,
    analysis_label = "raw_frozen_external_validation") {
  v2_ev_validate_prediction_set(prediction_set)
  comparisons <- v2_ev_validate_comparisons(
    comparisons, colnames(prediction_set$predictions)
  )
  if (!nrow(comparisons)) {
    return(data.frame(
      analysis = character(),
      candidate_model = character(),
      reference_model = character(),
      delta_brier = numeric(),
      delta_log_loss = numeric(),
      delta_c_statistic = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(seq_len(nrow(comparisons)), function(i) {
    candidate <- comparisons$candidate_model[i]
    reference <- comparisons$reference_model[i]
    delta <- v2_paired_metric_difference(
      prediction_set$outcome,
      prediction_set$predictions[, candidate],
      prediction_set$predictions[, reference]
    )
    data.frame(
      analysis = analysis_label,
      candidate_model = candidate,
      reference_model = reference,
      as.list(delta),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

v2_ev_fit_recalibration <- function(y, p, eps = 1e-6) {
  v2_assert_binary_outcome(y)
  p <- v2_clip_probability(p, eps)
  lp <- stats::qlogis(p)
  intercept_fit <- suppressWarnings(stats::glm.fit(
    x = matrix(1, nrow = length(y), ncol = 1L),
    y = y,
    offset = lp,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  slope_fit <- suppressWarnings(stats::glm.fit(
    x = cbind(`(Intercept)` = 1, linear_predictor = lp),
    y = y,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  if (!intercept_fit$converged ||
      anyNA(intercept_fit$coefficients) ||
      any(!is.finite(intercept_fit$coefficients))) {
    stop("Intercept-only recalibration failed.")
  }
  if (!slope_fit$converged || slope_fit$rank != 2L ||
      anyNA(slope_fit$coefficients) ||
      any(!is.finite(slope_fit$coefficients))) {
    stop("Intercept-and-slope recalibration failed.")
  }
  list(
    intercept_only = c(
      intercept = unname(intercept_fit$coefficients[[1L]]),
      slope = 1
    ),
    intercept_and_slope = c(
      intercept = unname(slope_fit$coefficients[[1L]]),
      slope = unname(slope_fit$coefficients[[2L]])
    ),
    probabilities = list(
      intercept_only = stats::plogis(
        lp + unname(intercept_fit$coefficients[[1L]])
      ),
      intercept_and_slope = stats::plogis(
        unname(slope_fit$coefficients[[1L]]) +
          unname(slope_fit$coefficients[[2L]]) * lp
      )
    )
  )
}

v2_ev_external_model_updates <- function(prediction_set, eps = 1e-6) {
  v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)
  raw <- v2_ev_raw_performance(prediction_set)
  update_rows <- list()
  prediction_rows <- list()
  counter <- 0L
  for (model_id in colnames(prediction_set$predictions)) {
    update <- v2_ev_fit_recalibration(
      prediction_set$outcome,
      prediction_set$predictions[, model_id],
      eps
    )
    for (update_type in c("intercept_only", "intercept_and_slope")) {
      counter <- counter + 1L
      probability <- update$probabilities[[update_type]]
      performance <- v2_binary_performance(
        prediction_set$outcome, probability, eps
      )
      update_rows[[counter]] <- data.frame(
        analysis = "external_model_update_descriptive_apparent",
        update_type = update_type,
        model_id = model_id,
        update_intercept = update[[update_type]][["intercept"]],
        update_slope = update[[update_type]][["slope"]],
        as.list(performance),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      prediction_rows[[counter]] <- data.frame(
        row_id = prediction_set$row_ids,
        model_id = model_id,
        update_type = update_type,
        updated_probability = probability,
        stringsAsFactors = FALSE
      )
    }
  }
  list(
    raw_external_validation = raw,
    update_warning = paste(
      "Update parameters were estimated with external outcomes.",
      "Updated performance is descriptive apparent performance and is not",
      "raw external validation."
    ),
    update_performance = do.call(rbind, update_rows),
    updated_predictions = do.call(rbind, prediction_rows)
  )
}

v2_ev_long_model_metrics <- function(y, predictions, replicate) {
  rows <- lapply(seq_len(ncol(predictions)), function(j) {
    score <- v2_binary_performance(y, predictions[, j])
    data.frame(
      replicate = as.integer(replicate),
      model_id = colnames(predictions)[j],
      metric = v2_ev_metric_names,
      estimate = as.numeric(score[v2_ev_metric_names]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_ev_long_difference_metrics <- function(
    y, predictions, comparisons, replicate) {
  if (!nrow(comparisons)) {
    return(data.frame(
      replicate = integer(),
      candidate_model = character(),
      reference_model = character(),
      metric = character(),
      estimate = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(seq_len(nrow(comparisons)), function(i) {
    candidate <- comparisons$candidate_model[i]
    reference <- comparisons$reference_model[i]
    candidate_score <- v2_binary_performance(y, predictions[, candidate])
    reference_score <- v2_binary_performance(y, predictions[, reference])
    data.frame(
      replicate = as.integer(replicate),
      candidate_model = candidate,
      reference_model = reference,
      metric = v2_ev_difference_metric_names,
      estimate = as.numeric(
        candidate_score[v2_ev_difference_metric_names] -
          reference_score[v2_ev_difference_metric_names]
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_ev_cluster_sample_indices <- function(hospital) {
  hospital <- as.character(hospital)
  if (length(hospital) < 2L || anyNA(hospital) ||
      any(!nzchar(hospital))) {
    stop("Complete hospital labels are required.")
  }
  hospitals <- unique(hospital)
  if (length(hospitals) < 2L) {
    stop("At least two hospitals are required.")
  }
  sampled <- sample(hospitals, length(hospitals), replace = TRUE)
  indices <- unlist(
    lapply(sampled, function(id) which(hospital == id)),
    use.names = FALSE
  )
  list(
    indices = indices,
    sampled_hospitals = sampled,
    distinct_sampled_hospitals = length(unique(sampled))
  )
}

v2_ev_failure_summary <- function(audit) {
  if (!is.data.frame(audit) ||
      !all(c("replicate", "success", "reason") %in% names(audit))) {
    stop("Malformed external-validation bootstrap audit.")
  }
  failed <- audit[!audit$success, , drop = FALSE]
  if (!nrow(failed)) {
    return(data.frame(
      reason = character(),
      failed_replicates = integer(),
      stringsAsFactors = FALSE
    ))
  }
  failed$reason[is.na(failed$reason) | !nzchar(failed$reason)] <-
    "unspecified_failure"
  counts <- sort(table(failed$reason), decreasing = TRUE)
  data.frame(
    reason = names(counts),
    failed_replicates = as.integer(counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

v2_ev_percentile_ci <- function(values, level = 0.95, type = 7L) {
  if (!is.numeric(values) || length(values) < 20L ||
      anyNA(values) || any(!is.finite(values))) {
    stop("At least 20 complete finite bootstrap estimates are required.")
  }
  v2_ev_assert_level(level)
  v2_ev_assert_scalar_integer(type, "type", 1L)
  if (type > 9L) stop("type must be between 1 and 9.")
  alpha <- 1 - level
  as.numeric(stats::quantile(
    values,
    probs = c(alpha / 2, 1 - alpha / 2),
    names = FALSE,
    type = as.integer(type)
  ))
}

v2_ev_bootstrap_model_summary <- function(
    point,
    replicates,
    successful_replicates,
    requested_replicates,
    reportable,
    level,
    quantile_type) {
  rows <- list()
  counter <- 0L
  for (model_id in unique(point$model_id)) {
    for (metric in v2_ev_metric_names) {
      counter <- counter + 1L
      estimate <- point[
        point$model_id == model_id, metric, drop = TRUE
      ]
      values <- replicates$estimate[
        replicates$model_id == model_id & replicates$metric == metric
      ]
      interval <- if (reportable) {
        v2_ev_percentile_ci(values, level, quantile_type)
      } else {
        c(NA_real_, NA_real_)
      }
      rows[[counter]] <- data.frame(
        model_id = model_id,
        metric = metric,
        estimate = as.numeric(estimate),
        lower = interval[[1L]],
        upper = interval[[2L]],
        level = level,
        method = "hospital_cluster_bootstrap_percentile",
        successful_replicates = successful_replicates,
        requested_replicates = requested_replicates,
        success_fraction = successful_replicates / requested_replicates,
        reportable = reportable,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

v2_ev_bootstrap_difference_summary <- function(
    point,
    replicates,
    successful_replicates,
    requested_replicates,
    reportable,
    level,
    quantile_type) {
  if (!nrow(point)) {
    return(data.frame(
      candidate_model = character(),
      reference_model = character(),
      metric = character(),
      estimate = numeric(),
      lower = numeric(),
      upper = numeric(),
      level = numeric(),
      method = character(),
      successful_replicates = integer(),
      requested_replicates = integer(),
      success_fraction = numeric(),
      reportable = logical(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- list()
  counter <- 0L
  for (i in seq_len(nrow(point))) {
    candidate <- point$candidate_model[i]
    reference <- point$reference_model[i]
    for (metric in v2_ev_difference_metric_names) {
      counter <- counter + 1L
      column <- paste0("delta_", metric)
      estimate <- point[[column]][i]
      values <- replicates$estimate[
        replicates$candidate_model == candidate &
          replicates$reference_model == reference &
          replicates$metric == metric
      ]
      interval <- if (reportable) {
        v2_ev_percentile_ci(values, level, quantile_type)
      } else {
        c(NA_real_, NA_real_)
      }
      rows[[counter]] <- data.frame(
        candidate_model = candidate,
        reference_model = reference,
        metric = paste0("delta_", metric),
        estimate = estimate,
        lower = interval[[1L]],
        upper = interval[[2L]],
        level = level,
        method = "paired_hospital_cluster_bootstrap_percentile",
        successful_replicates = successful_replicates,
        requested_replicates = requested_replicates,
        success_fraction = successful_replicates / requested_replicates,
        reportable = reportable,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

v2_ev_cluster_bootstrap <- function(
    prediction_set,
    comparisons = NULL,
    repetitions = 2000L,
    seed = 2026071602L,
    minimum_success_fraction = 0.95,
    level = 0.95,
    quantile_type = 7L,
    keep_replicates = TRUE) {
  v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)
  v2_ev_assert_scalar_integer(repetitions, "repetitions", 20L)
  v2_ev_assert_scalar_integer(seed, "seed", 1L)
  v2_ev_assert_fraction(
    minimum_success_fraction, "minimum_success_fraction"
  )
  v2_ev_assert_level(level)
  if (minimum_success_fraction < 0.95) {
    stop("The external-validation success gate cannot be lower than 0.95.")
  }
  if (!is.logical(keep_replicates) || length(keep_replicates) != 1L ||
      is.na(keep_replicates)) {
    stop("keep_replicates must be TRUE or FALSE.")
  }
  comparisons <- v2_ev_validate_comparisons(
    comparisons, colnames(prediction_set$predictions)
  )
  point_model <- v2_ev_raw_performance(prediction_set)
  point_difference <- v2_ev_paired_differences(
    prediction_set, comparisons
  )

  audit <- data.frame(
    replicate = seq_len(repetitions),
    success = FALSE,
    reason = NA_character_,
    sampled_hospitals = NA_integer_,
    distinct_sampled_hospitals = NA_integer_,
    bootstrap_rows = NA_integer_,
    stringsAsFactors = FALSE
  )
  model_results <- vector("list", repetitions)
  difference_results <- vector("list", repetitions)

  set.seed(as.integer(seed))
  for (b in seq_len(repetitions)) {
    sampled <- v2_ev_cluster_sample_indices(prediction_set$hospital)
    audit$sampled_hospitals[b] <- length(sampled$sampled_hospitals)
    audit$distinct_sampled_hospitals[b] <-
      sampled$distinct_sampled_hospitals
    audit$bootstrap_rows[b] <- length(sampled$indices)
    result <- tryCatch({
      y_b <- prediction_set$outcome[sampled$indices]
      p_b <- prediction_set$predictions[sampled$indices, , drop = FALSE]
      model_long <- v2_ev_long_model_metrics(y_b, p_b, b)
      difference_long <- v2_ev_long_difference_metrics(
        y_b, p_b, comparisons, b
      )
      if (anyNA(model_long$estimate) ||
          any(!is.finite(model_long$estimate)) ||
          anyNA(difference_long$estimate) ||
          any(!is.finite(difference_long$estimate))) {
        stop("nonfinite_metric")
      }
      list(model = model_long, difference = difference_long)
    }, error = function(e) e)
    if (inherits(result, "error")) {
      audit$reason[b] <- conditionMessage(result)
    } else {
      audit$success[b] <- TRUE
      audit$reason[b] <- ""
      model_results[[b]] <- result$model
      difference_results[[b]] <- result$difference
    }
  }

  successful <- sum(audit$success)
  success_fraction <- successful / repetitions
  reportable <- successful >= 20L &&
    success_fraction >= minimum_success_fraction
  successful_model_results <- model_results[audit$success]
  model_replicates <- if (length(successful_model_results)) {
    do.call(rbind, successful_model_results)
  } else {
    data.frame(
      replicate = integer(),
      model_id = character(),
      metric = character(),
      estimate = numeric(),
      stringsAsFactors = FALSE
    )
  }
  successful_difference_results <- difference_results[audit$success]
  difference_replicates <- if (
    length(successful_difference_results) &&
      any(vapply(successful_difference_results, nrow, integer(1L)) > 0L)
  ) {
    do.call(rbind, successful_difference_results)
  } else {
    data.frame(
      replicate = integer(),
      candidate_model = character(),
      reference_model = character(),
      metric = character(),
      estimate = numeric(),
      stringsAsFactors = FALSE
    )
  }

  model_summary <- v2_ev_bootstrap_model_summary(
    point_model,
    model_replicates,
    successful,
    repetitions,
    reportable,
    level,
    quantile_type
  )
  difference_summary <- v2_ev_bootstrap_difference_summary(
    point_difference,
    difference_replicates,
    successful,
    repetitions,
    reportable,
    level,
    quantile_type
  )

  structure(
    list(
      analysis = "raw_frozen_external_validation",
      resampling_unit = "hospital",
      point_model_performance = point_model,
      point_paired_differences = point_difference,
      model_summary = model_summary,
      paired_difference_summary = difference_summary,
      requested_replicates = repetitions,
      successful_replicates = successful,
      failed_replicates = repetitions - successful,
      success_fraction = success_fraction,
      minimum_success_fraction = minimum_success_fraction,
      reportable = reportable,
      audit = audit,
      failure_summary = v2_ev_failure_summary(audit),
      model_replicates = if (keep_replicates) {
        model_replicates
      } else {
        NULL
      },
      difference_replicates = if (keep_replicates) {
        difference_replicates
      } else {
        NULL
      },
      seed = as.integer(seed)
    ),
    class = "ards_v2_external_cluster_bootstrap"
  )
}

v2_ev_assert_bootstrap_reportable <- function(x) {
  if (!inherits(x, "ards_v2_external_cluster_bootstrap")) {
    stop("Expected an ards_v2_external_cluster_bootstrap object.")
  }
  if (!isTRUE(x$reportable)) {
    stop(
      "External cluster bootstrap failed its completion gate: ",
      x$successful_replicates, "/", x$requested_replicates,
      " successful (", format(round(x$success_fraction, 4), nsmall = 4), ")."
    )
  }
  invisible(TRUE)
}

v2_ev_subset_prediction_set <- function(
    prediction_set, keep, set_id, prediction_type = NULL) {
  v2_ev_validate_prediction_set(prediction_set)
  if (!is.logical(keep) || length(keep) != prediction_set$n ||
      anyNA(keep) || sum(keep) < 2L) {
    stop("keep must select at least two complete logical rows.")
  }
  outcome <- prediction_set$outcome[keep]
  v2_assert_binary_outcome(outcome)
  hospital <- prediction_set$hospital[keep]
  if (length(unique(hospital)) < 2L) {
    stop("Subset must retain at least two hospitals.")
  }
  out <- prediction_set
  out$set_id <- set_id
  if (!is.null(prediction_type)) out$prediction_type <- prediction_type
  out$row_ids <- prediction_set$row_ids[keep]
  out$outcome <- outcome
  out$hospital <- hospital
  out$predictions <- prediction_set$predictions[keep, , drop = FALSE]
  out$n <- sum(keep)
  out$events <- sum(outcome)
  out$hospitals <- length(unique(hospital))
  v2_ev_validate_prediction_set(out)
  out
}

v2_ev_largest_hospital_exclusion <- function(
    prediction_set, comparisons = NULL) {
  v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)
  counts <- table(prediction_set$hospital)
  ordered <- order(
    -as.integer(counts),
    names(counts),
    method = "radix"
  )
  largest <- names(counts)[ordered[[1L]]]
  keep <- prediction_set$hospital != largest
  subset <- v2_ev_subset_prediction_set(
    prediction_set,
    keep,
    paste0(prediction_set$set_id, "_excluding_largest_hospital")
  )
  list(
    analysis = "largest_hospital_exclusion",
    excluded_hospital = largest,
    excluded_n = unname(as.integer(counts[[largest]])),
    excluded_events = sum(
      prediction_set$outcome[prediction_set$hospital == largest]
    ),
    retained_n = subset$n,
    retained_events = subset$events,
    retained_hospitals = subset$hospitals,
    model_performance = v2_ev_performance_wide(
      subset$outcome,
      subset$predictions,
      "largest_hospital_exclusion"
    ),
    paired_differences = v2_ev_paired_differences(
      subset,
      comparisons,
      "largest_hospital_exclusion"
    ),
    retained_row_ids = subset$row_ids
  )
}

v2_ev_weighted_auc <- function(y, p, weights) {
  v2_assert_binary_outcome(y)
  if (length(y) != length(p) || length(y) != length(weights) ||
      anyNA(p) || any(!is.finite(p)) ||
      anyNA(weights) || any(!is.finite(weights)) ||
      any(weights <= 0)) {
    stop("Invalid weighted AUC inputs.")
  }
  positive <- which(y == 1L)
  negative <- which(y == 0L)
  numerator <- 0
  for (i in positive) {
    comparison <- (p[i] > p[negative]) +
      0.5 * (p[i] == p[negative])
    numerator <- numerator +
      weights[i] * sum(weights[negative] * comparison)
  }
  numerator / (sum(weights[positive]) * sum(weights[negative]))
}

v2_ev_weighted_calibration <- function(y, p, weights, eps = 1e-6) {
  v2_assert_binary_outcome(y)
  p <- v2_clip_probability(p, eps)
  if (length(y) != length(weights) || anyNA(weights) ||
      any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Invalid calibration weights.")
  }
  lp <- stats::qlogis(p)
  intercept_fit <- suppressWarnings(stats::glm.fit(
    x = matrix(1, nrow = length(y), ncol = 1L),
    y = y,
    weights = weights,
    offset = lp,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  slope_fit <- suppressWarnings(stats::glm.fit(
    x = cbind(`(Intercept)` = 1, linear_predictor = lp),
    y = y,
    weights = weights,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  if (!intercept_fit$converged || !slope_fit$converged ||
      slope_fit$rank != 2L ||
      anyNA(c(intercept_fit$coefficients, slope_fit$coefficients)) ||
      any(!is.finite(c(
        intercept_fit$coefficients, slope_fit$coefficients
      )))) {
    stop("Weighted calibration model failed.")
  }
  c(
    calibration_in_the_large =
      unname(intercept_fit$coefficients[[1L]]),
    calibration_intercept =
      unname(slope_fit$coefficients[[1L]]),
    calibration_slope =
      unname(slope_fit$coefficients[[2L]])
  )
}

v2_ev_weighted_performance <- function(y, p, weights, eps = 1e-6) {
  v2_assert_binary_outcome(y)
  p <- v2_clip_probability(p, eps)
  if (length(y) != length(p) || length(y) != length(weights) ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Invalid weighted performance inputs.")
  }
  weights <- weights / sum(weights)
  calibration <- v2_ev_weighted_calibration(y, p, weights, eps)
  c(
    n = length(y),
    events = sum(y),
    weighted_event_rate = sum(weights * y),
    brier = sum(weights * (y - p)^2),
    log_loss = -sum(
      weights * (y * log(p) + (1 - y) * log1p(-p))
    ),
    c_statistic = v2_ev_weighted_auc(y, p, weights),
    calibration,
    observed_expected_ratio =
      sum(weights * y) / sum(weights * p)
  )
}

v2_ev_equal_hospital_performance <- function(
    prediction_set,
    minimum_hospital_n = 10L,
    comparisons = NULL) {
  v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)
  v2_ev_assert_scalar_integer(
    minimum_hospital_n, "minimum_hospital_n", 1L
  )
  counts <- table(prediction_set$hospital)
  eligible <- names(counts)[counts >= minimum_hospital_n]
  if (length(eligible) < 2L) {
    stop("At least two hospitals meet the minimum size threshold.")
  }
  keep <- prediction_set$hospital %in% eligible
  outcome <- prediction_set$outcome[keep]
  v2_assert_binary_outcome(outcome)
  hospital <- prediction_set$hospital[keep]
  predictions <- prediction_set$predictions[keep, , drop = FALSE]
  comparisons <- v2_ev_validate_comparisons(
    comparisons, colnames(predictions)
  )
  retained_counts <- table(hospital)
  weights <- 1 / as.numeric(retained_counts[hospital])
  rows <- lapply(seq_len(ncol(predictions)), function(j) {
    score <- v2_ev_weighted_performance(
      outcome, predictions[, j], weights
    )
    data.frame(
      analysis = "equal_hospital_weighted",
      minimum_hospital_n = as.integer(minimum_hospital_n),
      eligible_hospitals = length(eligible),
      retained_patients = length(outcome),
      retained_events = sum(outcome),
      model_id = colnames(predictions)[j],
      as.list(score),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  score_by_model <- setNames(lapply(
    seq_len(ncol(predictions)),
    function(j) {
      v2_ev_weighted_performance(
        outcome, predictions[, j], weights
      )
    }
  ), colnames(predictions))
  paired_rows <- lapply(seq_len(nrow(comparisons)), function(i) {
    candidate <- comparisons$candidate_model[i]
    reference <- comparisons$reference_model[i]
    delta <- score_by_model[[candidate]][v2_ev_difference_metric_names] -
      score_by_model[[reference]][v2_ev_difference_metric_names]
    data.frame(
      analysis = "equal_hospital_weighted",
      minimum_hospital_n = as.integer(minimum_hospital_n),
      eligible_hospitals = length(eligible),
      retained_patients = length(outcome),
      retained_events = sum(outcome),
      candidate_model = candidate,
      reference_model = reference,
      delta_brier = unname(delta[["brier"]]),
      delta_log_loss = unname(delta[["log_loss"]]),
      delta_c_statistic = unname(delta[["c_statistic"]]),
      stringsAsFactors = FALSE
    )
  })
  paired_differences <- if (length(paired_rows)) {
    do.call(rbind, paired_rows)
  } else {
    data.frame(
      analysis = character(),
      minimum_hospital_n = integer(),
      eligible_hospitals = integer(),
      retained_patients = integer(),
      retained_events = integer(),
      candidate_model = character(),
      reference_model = character(),
      delta_brier = numeric(),
      delta_log_loss = numeric(),
      delta_c_statistic = numeric(),
      stringsAsFactors = FALSE
    )
  }
  hospital_detail <- data.frame(
    hospital = names(retained_counts),
    n = as.integer(retained_counts),
    events = vapply(
      names(retained_counts),
      function(id) sum(outcome[hospital == id]),
      numeric(1L)
    ),
    total_analysis_weight = vapply(
      names(retained_counts),
      function(id) sum(weights[hospital == id]),
      numeric(1L)
    ),
    stringsAsFactors = FALSE
  )
  list(
    analysis = "equal_hospital_weighted",
    performance = do.call(rbind, rows),
    paired_differences = paired_differences,
    hospital_detail = hospital_detail,
    excluded_hospitals = setdiff(names(counts), eligible),
    retained_row_ids = prediction_set$row_ids[keep]
  )
}

v2_ev_leave_one_hospital_out <- function(
    prediction_set, comparisons = NULL) {
  v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)
  comparisons <- v2_ev_validate_comparisons(
    comparisons, colnames(prediction_set$predictions)
  )
  full_model <- v2_ev_raw_performance(prediction_set)
  full_difference <- v2_ev_paired_differences(
    prediction_set, comparisons
  )
  hospitals <- sort(unique(prediction_set$hospital))
  audit <- data.frame(
    omitted_hospital = hospitals,
    omitted_n = as.integer(table(prediction_set$hospital)[hospitals]),
    omitted_events = vapply(
      hospitals,
      function(id) sum(prediction_set$outcome[
        prediction_set$hospital == id
      ]),
      numeric(1L)
    ),
    success = FALSE,
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
  model_rows <- vector("list", length(hospitals))
  difference_rows <- vector("list", length(hospitals))

  for (i in seq_along(hospitals)) {
    omitted <- hospitals[[i]]
    keep <- prediction_set$hospital != omitted
    result <- tryCatch({
      subset <- v2_ev_subset_prediction_set(
        prediction_set,
        keep,
        paste0(prediction_set$set_id, "_omit_", omitted)
      )
      model <- v2_ev_performance_wide(
        subset$outcome,
        subset$predictions,
        "leave_one_hospital_out"
      )
      difference <- v2_ev_paired_differences(
        subset,
        comparisons,
        "leave_one_hospital_out"
      )
      model_long <- do.call(rbind, lapply(
        seq_len(nrow(model)),
        function(j) {
          data.frame(
            omitted_hospital = omitted,
            omitted_n = audit$omitted_n[i],
            omitted_events = audit$omitted_events[i],
            retained_n = subset$n,
            retained_events = subset$events,
            model_id = model$model_id[j],
            metric = v2_ev_metric_names,
            estimate = as.numeric(model[j, v2_ev_metric_names]),
            full_estimate = as.numeric(
              full_model[
                full_model$model_id == model$model_id[j],
                v2_ev_metric_names
              ]
            ),
            stringsAsFactors = FALSE
          )
        }
      ))
      model_long$change_from_full <-
        model_long$estimate - model_long$full_estimate
      difference_long <- if (nrow(difference)) {
        do.call(rbind, lapply(seq_len(nrow(difference)), function(j) {
          full_row <- full_difference[
            full_difference$candidate_model ==
              difference$candidate_model[j] &
              full_difference$reference_model ==
              difference$reference_model[j],
            ,
            drop = FALSE
          ]
          data.frame(
            omitted_hospital = omitted,
            omitted_n = audit$omitted_n[i],
            omitted_events = audit$omitted_events[i],
            retained_n = subset$n,
            retained_events = subset$events,
            candidate_model = difference$candidate_model[j],
            reference_model = difference$reference_model[j],
            metric = paste0("delta_", v2_ev_difference_metric_names),
            estimate = as.numeric(
              difference[j, paste0(
                "delta_", v2_ev_difference_metric_names
              )]
            ),
            full_estimate = as.numeric(
              full_row[1L, paste0(
                "delta_", v2_ev_difference_metric_names
              )]
            ),
            stringsAsFactors = FALSE
          )
        }))
      } else {
        data.frame(
          omitted_hospital = character(),
          omitted_n = integer(),
          omitted_events = integer(),
          retained_n = integer(),
          retained_events = integer(),
          candidate_model = character(),
          reference_model = character(),
          metric = character(),
          estimate = numeric(),
          full_estimate = numeric(),
          stringsAsFactors = FALSE
        )
      }
      if (nrow(difference_long)) {
        difference_long$change_from_full <-
          difference_long$estimate - difference_long$full_estimate
      } else {
        difference_long$change_from_full <- numeric()
      }
      list(model = model_long, difference = difference_long)
    }, error = function(e) e)
    if (inherits(result, "error")) {
      audit$reason[i] <- conditionMessage(result)
    } else {
      audit$success[i] <- TRUE
      audit$reason[i] <- ""
      model_rows[[i]] <- result$model
      difference_rows[[i]] <- result$difference
    }
  }

  successful_model_rows <- model_rows[audit$success]
  model_influence <- if (length(successful_model_rows)) {
    do.call(rbind, successful_model_rows)
  } else {
    data.frame()
  }
  successful_difference_rows <- difference_rows[audit$success]
  difference_influence <- if (
    length(successful_difference_rows) &&
      any(vapply(successful_difference_rows, nrow, integer(1L)) > 0L)
  ) {
    do.call(rbind, successful_difference_rows)
  } else {
    data.frame()
  }
  list(
    analysis = "leave_one_hospital_out_influence",
    full_model_performance = full_model,
    full_paired_differences = full_difference,
    model_influence = model_influence,
    paired_difference_influence = difference_influence,
    audit = audit,
    successful_hospitals = sum(audit$success),
    failed_hospitals = sum(!audit$success)
  )
}

v2_ev_calibration_spec <- function(
    p,
    knot_probabilities = c(0.05, 0.35, 0.65, 0.95),
    grid_points = 101L,
    eps = 1e-6) {
  if (!is.numeric(p) || length(p) < 20L || anyNA(p) ||
      any(!is.finite(p)) || any(p < 0 | p > 1)) {
    stop("Calibration predictions must be complete probabilities.")
  }
  if (!is.numeric(knot_probabilities) ||
      length(knot_probabilities) != 4L ||
      anyNA(knot_probabilities) ||
      any(diff(knot_probabilities) <= 0) ||
      knot_probabilities[[1L]] <= 0 ||
      knot_probabilities[[4L]] >= 1) {
    stop("Four strictly increasing knot probabilities in (0, 1) required.")
  }
  v2_ev_assert_scalar_integer(grid_points, "grid_points", 20L)
  lp <- stats::qlogis(v2_clip_probability(p, eps))
  knots <- as.numeric(stats::quantile(
    lp,
    probs = knot_probabilities,
    names = FALSE,
    type = 2L
  ))
  if (any(!is.finite(knots)) || any(diff(knots) <= 0)) {
    stop("Flexible calibration requires four distinct logit-scale knots.")
  }
  grid_lp <- seq(knots[[1L]], knots[[4L]], length.out = grid_points)
  list(
    boundary_knots = knots[c(1L, 4L)],
    internal_knots = knots[c(2L, 3L)],
    knot_probabilities = knot_probabilities,
    grid_linear_predictor = grid_lp,
    grid_probability = stats::plogis(grid_lp),
    eps = eps
  )
}

v2_ev_calibration_basis <- function(linear_predictor, spec) {
  if (!is.numeric(linear_predictor) || anyNA(linear_predictor) ||
      any(!is.finite(linear_predictor))) {
    stop("Calibration linear predictors must be complete and finite.")
  }
  basis <- splines::ns(
    linear_predictor,
    knots = spec$internal_knots,
    Boundary.knots = spec$boundary_knots,
    intercept = FALSE
  )
  basis <- as.matrix(basis)
  colnames(basis) <- paste0("calibration_ns", seq_len(ncol(basis)))
  basis
}

v2_ev_fit_flexible_calibration <- function(y, p, spec) {
  v2_assert_binary_outcome(y)
  if (length(y) != length(p)) {
    stop("Outcome and calibration prediction lengths differ.")
  }
  lp <- stats::qlogis(v2_clip_probability(p, spec$eps))
  basis <- v2_ev_calibration_basis(lp, spec)
  x <- cbind(`(Intercept)` = 1, basis)
  fit <- suppressWarnings(stats::glm.fit(
    x = x,
    y = y,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100L)
  ))
  if (!fit$converged || fit$rank != ncol(x) ||
      anyNA(fit$coefficients) || any(!is.finite(fit$coefficients))) {
    stop("Flexible calibration model failed or was rank deficient.")
  }
  grid_basis <- v2_ev_calibration_basis(
    spec$grid_linear_predictor, spec
  )
  grid_x <- cbind(`(Intercept)` = 1, grid_basis)
  stats::plogis(as.numeric(grid_x %*% fit$coefficients))
}

v2_ev_prediction_distribution <- function(
    p, model_id, bins = 10L, eps = 1e-6) {
  v2_ev_assert_scalar_integer(bins, "bins", 2L)
  p <- v2_clip_probability(p, eps)
  breaks <- unique(as.numeric(stats::quantile(
    p,
    probs = seq(0, 1, length.out = bins + 1L),
    names = FALSE,
    type = 2L
  )))
  if (length(breaks) < 3L) {
    return(data.frame(
      model_id = model_id,
      bin = 1L,
      lower = min(p),
      upper = max(p),
      n = length(p),
      stringsAsFactors = FALSE
    ))
  }
  group <- cut(
    p,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE,
    labels = FALSE
  )
  rows <- lapply(sort(unique(group)), function(g) {
    values <- p[group == g]
    data.frame(
      model_id = model_id,
      bin = as.integer(g),
      lower = min(values),
      upper = max(values),
      n = length(values),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_ev_flexible_calibration_data <- function(
    prediction_set,
    knot_probabilities = c(0.05, 0.35, 0.65, 0.95),
    grid_points = 101L,
    distribution_bins = 10L) {
  v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)
  curves <- list()
  distributions <- list()
  specs <- list()
  for (j in seq_len(ncol(prediction_set$predictions))) {
    model_id <- colnames(prediction_set$predictions)[j]
    p <- prediction_set$predictions[, j]
    spec <- v2_ev_calibration_spec(
      p, knot_probabilities, grid_points
    )
    calibrated <- v2_ev_fit_flexible_calibration(
      prediction_set$outcome, p, spec
    )
    curves[[model_id]] <- data.frame(
      model_id = model_id,
      grid_index = seq_along(calibrated),
      predicted_probability = spec$grid_probability,
      estimated_observed_probability = calibrated,
      identity_probability = spec$grid_probability,
      stringsAsFactors = FALSE
    )
    distributions[[model_id]] <- v2_ev_prediction_distribution(
      p, model_id, distribution_bins, spec$eps
    )
    specs[[model_id]] <- spec
  }
  list(
    analysis = "raw_frozen_external_flexible_calibration",
    curve = do.call(rbind, curves),
    prediction_distribution = do.call(rbind, distributions),
    specs = specs,
    curve_method = paste(
      "Logistic calibration model with a natural spline of the logit",
      "prediction; knots at the 5th, 35th, 65th, and 95th percentiles."
    )
  )
}

v2_ev_cluster_bootstrap_calibration_bands <- function(
    prediction_set,
    repetitions = 2000L,
    seed = 2026071603L,
    minimum_success_fraction = 0.95,
    level = 0.95,
    knot_probabilities = c(0.05, 0.35, 0.65, 0.95),
    grid_points = 101L,
    distribution_bins = 10L,
    keep_replicates = TRUE) {
  v2_ev_validate_prediction_set(prediction_set, require_raw = TRUE)
  v2_ev_assert_scalar_integer(repetitions, "repetitions", 20L)
  v2_ev_assert_scalar_integer(seed, "seed", 1L)
  v2_ev_assert_fraction(
    minimum_success_fraction, "minimum_success_fraction"
  )
  v2_ev_assert_level(level)
  if (minimum_success_fraction < 0.95) {
    stop("The calibration-band success gate cannot be lower than 0.95.")
  }
  if (!is.logical(keep_replicates) || length(keep_replicates) != 1L ||
      is.na(keep_replicates)) {
    stop("keep_replicates must be TRUE or FALSE.")
  }
  point <- v2_ev_flexible_calibration_data(
    prediction_set,
    knot_probabilities,
    grid_points,
    distribution_bins
  )
  specs <- point$specs
  audit <- data.frame(
    replicate = seq_len(repetitions),
    success = FALSE,
    reason = NA_character_,
    sampled_hospitals = NA_integer_,
    distinct_sampled_hospitals = NA_integer_,
    bootstrap_rows = NA_integer_,
    stringsAsFactors = FALSE
  )
  replicate_rows <- vector("list", repetitions)

  set.seed(as.integer(seed))
  for (b in seq_len(repetitions)) {
    sampled <- v2_ev_cluster_sample_indices(prediction_set$hospital)
    audit$sampled_hospitals[b] <- length(sampled$sampled_hospitals)
    audit$distinct_sampled_hospitals[b] <-
      sampled$distinct_sampled_hospitals
    audit$bootstrap_rows[b] <- length(sampled$indices)
    result <- tryCatch({
      y_b <- prediction_set$outcome[sampled$indices]
      p_b <- prediction_set$predictions[sampled$indices, , drop = FALSE]
      v2_assert_binary_outcome(y_b)
      rows <- lapply(seq_len(ncol(p_b)), function(j) {
        model_id <- colnames(p_b)[j]
        calibrated <- v2_ev_fit_flexible_calibration(
          y_b, p_b[, j], specs[[model_id]]
        )
        if (anyNA(calibrated) || any(!is.finite(calibrated))) {
          stop("nonfinite_flexible_calibration")
        }
        data.frame(
          replicate = b,
          model_id = model_id,
          grid_index = seq_along(calibrated),
          estimated_observed_probability = calibrated,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    }, error = function(e) e)
    if (inherits(result, "error")) {
      audit$reason[b] <- conditionMessage(result)
    } else {
      audit$success[b] <- TRUE
      audit$reason[b] <- ""
      replicate_rows[[b]] <- result
    }
  }

  successful <- sum(audit$success)
  success_fraction <- successful / repetitions
  reportable <- successful >= 20L &&
    success_fraction >= minimum_success_fraction
  successful_rows <- replicate_rows[audit$success]
  replicate_curves <- if (length(successful_rows)) {
    do.call(rbind, successful_rows)
  } else {
    data.frame(
      replicate = integer(),
      model_id = character(),
      grid_index = integer(),
      estimated_observed_probability = numeric(),
      stringsAsFactors = FALSE
    )
  }

  curve <- point$curve
  curve$lower <- NA_real_
  curve$upper <- NA_real_
  curve$level <- level
  curve$interval_method <-
    "pointwise_hospital_cluster_bootstrap_percentile"
  curve$successful_replicates <- successful
  curve$requested_replicates <- repetitions
  curve$success_fraction <- success_fraction
  curve$reportable <- reportable
  if (reportable) {
    for (i in seq_len(nrow(curve))) {
      values <- replicate_curves$estimated_observed_probability[
        replicate_curves$model_id == curve$model_id[i] &
          replicate_curves$grid_index == curve$grid_index[i]
      ]
      interval <- v2_ev_percentile_ci(values, level, 7L)
      curve$lower[i] <- interval[[1L]]
      curve$upper[i] <- interval[[2L]]
    }
  }

  structure(
    list(
      analysis = "raw_frozen_external_flexible_calibration",
      curve_with_pointwise_band = curve,
      prediction_distribution = point$prediction_distribution,
      specs = specs,
      requested_replicates = repetitions,
      successful_replicates = successful,
      failed_replicates = repetitions - successful,
      success_fraction = success_fraction,
      minimum_success_fraction = minimum_success_fraction,
      reportable = reportable,
      audit = audit,
      failure_summary = v2_ev_failure_summary(audit),
      replicate_curves = if (keep_replicates) {
        replicate_curves
      } else {
        NULL
      },
      seed = as.integer(seed),
      interval_note = paste(
        "Bands are pointwise percentile intervals from hospital-cluster",
        "resampling; they are not simultaneous confidence bands."
      )
    ),
    class = "ards_v2_external_calibration_bootstrap"
  )
}
