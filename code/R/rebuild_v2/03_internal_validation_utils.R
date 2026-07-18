# ARDS mechanical-power rebuild v2: internal-validation utilities
#
# This module implements Harrell's bootstrap optimism correction and the two
# confidence-interval algorithms described by Noma et al.:
#
# Noma H, Shinozaki T, Iba K, Teramukai S, Furukawa TA. Confidence intervals
# of prediction accuracy measures for multivariable prediction models based on
# the bootstrap-based optimism correction methods. Stat Med.
# 2021;40:5691-5701. doi:10.1002/sim.9148.
#
# For metric theta, Harrell correction is
#
#   optimism_b = theta_boot,b - theta_orig,b
#   theta_corrected = theta_apparent - mean_b(optimism_b).
#
# Noma Algorithm 1 first takes the percentile interval of theta_boot,b, the
# apparent performance of each bootstrap-fitted model in its own bootstrap
# sample, and then shifts both limits by the estimated bias:
#
#   CI_LS = quantile(theta_boot, alpha / 2, 1 - alpha / 2)
#           - mean_b(optimism_b).
#
# It is not the percentile distribution of optimism subtracted from the
# original apparent estimate.
#
# Noma Algorithm 2 resamples the development data in an outer bootstrap and
# repeats the complete optimism-correction procedure in every outer sample.
# Percentiles of the outer optimism-corrected estimates form the two-stage CI.
#
# The location-shifted method only captures variability of apparent
# performance and is a large-sample approximation. For an ordinary
# unpenalized logistic model, apparent calibration intercept and slope are
# structurally 0 and 1 in every training sample. Their apparent bootstrap
# distributions therefore collapse, so a location-shifted interval is not
# informative. This module detects that degeneration and requires the
# two-stage procedure for calibration-slope uncertainty.
#
# Required upstream utility:
#   v2_binary_performance(y, p)
#
# The fitting callback must replay the complete fixed development pipeline,
# including transformation derivation, imputation, variable selection (if
# any), and coefficient fitting. The prediction callback must use only the
# fitted training-sample object when it scores the original data.

v2_iv_default_metrics <- c(
  "brier", "log_loss", "c_statistic", "calibration_slope"
)

v2_iv_assert_scalar_integer <- function(x, label, minimum = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x != as.integer(x) || x < minimum) {
    stop(label, " must be one integer >= ", minimum, ".")
  }
  invisible(TRUE)
}

v2_iv_assert_fraction <- function(x, label) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x <= 0 || x > 1) {
    stop(label, " must be in (0, 1].")
  }
  invisible(TRUE)
}

v2_iv_validate_data <- function(data, outcome) {
  if (!is.data.frame(data) || nrow(data) < 2L) {
    stop("Internal validation requires a data frame with at least two rows.")
  }
  if (!is.character(outcome) || length(outcome) != 1L ||
      is.na(outcome) || !nzchar(outcome) || !outcome %in% names(data)) {
    stop("outcome must name one column in data.")
  }
  if (!exists("v2_binary_performance", mode = "function", inherits = TRUE)) {
    stop(
      "Source 01_analysis_utils.R before 03_internal_validation_utils.R."
    )
  }
  v2_assert_binary_outcome(data[[outcome]])
  invisible(TRUE)
}

v2_iv_validate_callbacks <- function(
    fit_pipeline, predict_pipeline, score_pipeline) {
  callbacks <- list(
    fit_pipeline = fit_pipeline,
    predict_pipeline = predict_pipeline,
    score_pipeline = score_pipeline
  )
  invalid <- names(callbacks)[!vapply(callbacks, is.function, logical(1L))]
  if (length(invalid)) {
    stop("The following callbacks are not functions: ",
         paste(invalid, collapse = ", "))
  }
  invisible(TRUE)
}

v2_iv_default_score <- function(y, p) {
  performance <- v2_binary_performance(y, p)
  performance[v2_iv_default_metrics]
}

v2_iv_validate_score <- function(score, metrics, label) {
  if (!is.numeric(score) || is.null(names(score)) ||
      anyDuplicated(names(score))) {
    stop(label, " must be a uniquely named numeric vector.")
  }
  missing <- setdiff(metrics, names(score))
  if (length(missing)) {
    stop(label, " lacks metric(s): ", paste(missing, collapse = ", "))
  }
  out <- as.numeric(score[metrics])
  names(out) <- metrics
  if (anyNA(out) || any(!is.finite(out))) {
    stop(label, " contains a missing or non-finite requested metric.")
  }
  out
}

v2_iv_score_model <- function(
    model,
    train_data,
    test_data,
    outcome,
    predict_pipeline,
    score_pipeline,
    metrics) {
  train_probability <- predict_pipeline(model, train_data)
  test_probability <- predict_pipeline(model, test_data)
  if (!is.numeric(train_probability) ||
      length(train_probability) != nrow(train_data) ||
      anyNA(train_probability) || any(!is.finite(train_probability)) ||
      any(train_probability < 0 | train_probability > 1)) {
    stop("Training predictions must be complete finite probabilities.")
  }
  if (!is.numeric(test_probability) ||
      length(test_probability) != nrow(test_data) ||
      anyNA(test_probability) || any(!is.finite(test_probability)) ||
      any(test_probability < 0 | test_probability > 1)) {
    stop("Test predictions must be complete finite probabilities.")
  }
  train_score <- v2_iv_validate_score(
    score_pipeline(train_data[[outcome]], train_probability),
    metrics,
    "Training score"
  )
  test_score <- v2_iv_validate_score(
    score_pipeline(test_data[[outcome]], test_probability),
    metrics,
    "Test score"
  )
  list(train = train_score, test = test_score)
}

v2_iv_failure_summary <- function(audit) {
  required <- c("replicate", "success", "reason")
  if (!is.data.frame(audit) || !all(required %in% names(audit))) {
    stop("Malformed internal-validation audit.")
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

v2_iv_percentile_interval <- function(
    values,
    level = 0.95,
    quantile_type = 7L) {
  if (!is.numeric(values) || length(values) < 20L ||
      anyNA(values) || any(!is.finite(values))) {
    stop("At least 20 complete finite values are required for a CI.")
  }
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be in (0, 1).")
  }
  v2_iv_assert_scalar_integer(quantile_type, "quantile_type", 1L)
  if (quantile_type > 9L) stop("quantile_type must be between 1 and 9.")
  alpha <- 1 - level
  as.numeric(stats::quantile(
    values,
    probs = c(alpha / 2, 1 - alpha / 2),
    names = FALSE,
    type = as.integer(quantile_type)
  ))
}

v2_location_shifted_ci <- function(
    validation,
    level = 0.95,
    quantile_type = 7L,
    degeneracy_tolerance = 1e-6) {
  if (!inherits(validation, "ards_v2_harrell_validation")) {
    stop("validation must be an ards_v2_harrell_validation object.")
  }
  if (!is.numeric(degeneracy_tolerance) ||
      length(degeneracy_tolerance) != 1L ||
      is.na(degeneracy_tolerance) || !is.finite(degeneracy_tolerance) ||
      degeneracy_tolerance <= 0) {
    stop("degeneracy_tolerance must be one positive finite number.")
  }

  output <- lapply(validation$metrics, function(metric) {
    metric_rows <- validation$replicates[
      validation$replicates$metric == metric,
      ,
      drop = FALSE
    ]
    estimate <- unname(validation$corrected[[metric]])
    bias <- unname(validation$mean_optimism[[metric]])
    if (!validation$reportable) {
      return(data.frame(
        metric = metric,
        estimate = estimate,
        lower = NA_real_,
        upper = NA_real_,
        level = level,
        supported = FALSE,
        reason = "bootstrap_success_below_prespecified_minimum",
        method = "Noma_location_shifted_percentile",
        quantile_type = as.integer(quantile_type),
        stringsAsFactors = FALSE
      ))
    }
    apparent_bootstrap <- metric_rows$train_estimate
    if (length(apparent_bootstrap) < 20L) {
      return(data.frame(
        metric = metric,
        estimate = estimate,
        lower = NA_real_,
        upper = NA_real_,
        level = level,
        supported = FALSE,
        reason = "fewer_than_20_successful_bootstrap_estimates",
        method = "Noma_location_shifted_percentile",
        quantile_type = as.integer(quantile_type),
        stringsAsFactors = FALSE
      ))
    }
    scale <- max(1, abs(mean(apparent_bootstrap)))
    spread <- diff(range(apparent_bootstrap))
    if (!is.finite(spread) ||
        spread <= degeneracy_tolerance * scale) {
      return(data.frame(
        metric = metric,
        estimate = estimate,
        lower = NA_real_,
        upper = NA_real_,
        level = level,
        supported = FALSE,
        reason = paste0(
          "degenerate_apparent_bootstrap_distribution;",
          "use_two_stage_bootstrap"
        ),
        method = "Noma_location_shifted_percentile",
        quantile_type = as.integer(quantile_type),
        stringsAsFactors = FALSE
      ))
    }
    apparent_interval <- v2_iv_percentile_interval(
      apparent_bootstrap,
      level = level,
      quantile_type = quantile_type
    )
    shifted <- apparent_interval - bias
    data.frame(
      metric = metric,
      estimate = estimate,
      lower = shifted[[1L]],
      upper = shifted[[2L]],
      level = level,
      supported = TRUE,
      reason = "",
      method = "Noma_location_shifted_percentile",
      quantile_type = as.integer(quantile_type),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, output)
}

v2_harrell_internal_validation <- function(
    data,
    outcome,
    fit_pipeline,
    predict_pipeline,
    score_pipeline = v2_iv_default_score,
    metrics = v2_iv_default_metrics,
    repetitions = 1000L,
    seed = 2026071601L,
    minimum_success_fraction = 0.95,
    pipeline_id = "fixed_model_development_pipeline",
    ci_level = 0.95,
    quantile_type = 7L) {
  v2_iv_validate_data(data, outcome)
  v2_iv_validate_callbacks(
    fit_pipeline, predict_pipeline, score_pipeline
  )
  if (!is.character(metrics) || !length(metrics) || anyNA(metrics) ||
      any(!nzchar(metrics)) || anyDuplicated(metrics)) {
    stop("metrics must be unique nonempty names.")
  }
  if (!is.character(pipeline_id) || length(pipeline_id) != 1L ||
      is.na(pipeline_id) || !nzchar(pipeline_id)) {
    stop("pipeline_id must be one nonempty string.")
  }
  v2_iv_assert_scalar_integer(repetitions, "repetitions", 20L)
  v2_iv_assert_scalar_integer(seed, "seed", 1L)
  v2_iv_assert_fraction(
    minimum_success_fraction,
    "minimum_success_fraction"
  )

  original_model <- fit_pipeline(data)
  apparent_pair <- v2_iv_score_model(
    original_model,
    data,
    data,
    outcome,
    predict_pipeline,
    score_pipeline,
    metrics
  )
  apparent <- apparent_pair$train

  set.seed(as.integer(seed))
  indices <- lapply(
    seq_len(as.integer(repetitions)),
    function(unused) sample.int(nrow(data), nrow(data), replace = TRUE)
  )
  replicate_rows <- vector("list", as.integer(repetitions))
  audit_rows <- vector("list", as.integer(repetitions))

  for (replicate_id in seq_len(as.integer(repetitions))) {
    sampled <- data[indices[[replicate_id]], , drop = FALSE]
    rownames(sampled) <- NULL
    result <- tryCatch({
      v2_assert_binary_outcome(sampled[[outcome]])
      model <- fit_pipeline(sampled)
      score <- v2_iv_score_model(
        model,
        sampled,
        data,
        outcome,
        predict_pipeline,
        score_pipeline,
        metrics
      )
      data.frame(
        replicate = replicate_id,
        metric = metrics,
        train_estimate = as.numeric(score$train[metrics]),
        test_estimate = as.numeric(score$test[metrics]),
        optimism = as.numeric(score$train[metrics] - score$test[metrics]),
        stringsAsFactors = FALSE
      )
    }, error = function(e) e)

    if (inherits(result, "error")) {
      audit_rows[[replicate_id]] <- data.frame(
        replicate = replicate_id,
        success = FALSE,
        reason = conditionMessage(result),
        stringsAsFactors = FALSE
      )
      replicate_rows[[replicate_id]] <- NULL
    } else {
      audit_rows[[replicate_id]] <- data.frame(
        replicate = replicate_id,
        success = TRUE,
        reason = "",
        stringsAsFactors = FALSE
      )
      replicate_rows[[replicate_id]] <- result
    }
  }

  audit <- do.call(rbind, audit_rows)
  successful <- sum(audit$success)
  success_fraction <- successful / as.integer(repetitions)
  reportable <- success_fraction >= minimum_success_fraction
  replicates <- do.call(rbind, replicate_rows)
  if (is.null(replicates)) {
    replicates <- data.frame(
      replicate = integer(),
      metric = character(),
      train_estimate = numeric(),
      test_estimate = numeric(),
      optimism = numeric(),
      stringsAsFactors = FALSE
    )
  }

  mean_optimism <- setNames(rep(NA_real_, length(metrics)), metrics)
  corrected <- setNames(rep(NA_real_, length(metrics)), metrics)
  if (successful > 0L) {
    split_optimism <- split(replicates$optimism, replicates$metric)
    mean_optimism[names(split_optimism)] <-
      vapply(split_optimism, mean, numeric(1L))
    corrected <- apparent - mean_optimism
  }

  output <- structure(
    list(
      method = "Harrell_bootstrap_optimism_correction",
      pipeline_id = pipeline_id,
      metrics = metrics,
      repetitions_requested = as.integer(repetitions),
      successful_replicates = successful,
      failed_replicates = as.integer(repetitions) - successful,
      success_fraction = success_fraction,
      minimum_success_fraction = minimum_success_fraction,
      reportable = reportable,
      apparent = apparent,
      mean_optimism = mean_optimism,
      corrected = corrected,
      replicates = replicates,
      audit = audit,
      failure_summary = v2_iv_failure_summary(audit),
      reference = "Noma_et_al_Stat_Med_2021_doi_10.1002_sim.9148",
      seed = as.integer(seed)
    ),
    class = "ards_v2_harrell_validation"
  )
  output$location_shifted_ci <- v2_location_shifted_ci(
    output,
    level = ci_level,
    quantile_type = quantile_type
  )
  output
}

v2_two_stage_internal_validation <- function(
    data,
    outcome,
    fit_pipeline,
    predict_pipeline,
    score_pipeline = v2_iv_default_score,
    metrics = v2_iv_default_metrics,
    outer_repetitions = 1000L,
    inner_repetitions = 1000L,
    seed = 2026071603L,
    minimum_inner_success_fraction = 0.95,
    minimum_outer_success_fraction = 0.95,
    pipeline_id = "fixed_model_development_pipeline",
    level = 0.95,
    quantile_type = 7L,
    point_validation = NULL) {
  v2_iv_validate_data(data, outcome)
  v2_iv_validate_callbacks(
    fit_pipeline, predict_pipeline, score_pipeline
  )
  v2_iv_assert_scalar_integer(
    outer_repetitions, "outer_repetitions", 20L
  )
  v2_iv_assert_scalar_integer(
    inner_repetitions, "inner_repetitions", 20L
  )
  v2_iv_assert_scalar_integer(seed, "seed", 1L)
  v2_iv_assert_fraction(
    minimum_inner_success_fraction,
    "minimum_inner_success_fraction"
  )
  v2_iv_assert_fraction(
    minimum_outer_success_fraction,
    "minimum_outer_success_fraction"
  )

  if (is.null(point_validation)) {
    point_validation <- v2_harrell_internal_validation(
      data = data,
      outcome = outcome,
      fit_pipeline = fit_pipeline,
      predict_pipeline = predict_pipeline,
      score_pipeline = score_pipeline,
      metrics = metrics,
      repetitions = inner_repetitions,
      seed = as.integer(seed),
      minimum_success_fraction = minimum_inner_success_fraction,
      pipeline_id = pipeline_id,
      ci_level = level,
      quantile_type = quantile_type
    )
  }
  if (!inherits(point_validation, "ards_v2_harrell_validation") ||
      !identical(point_validation$metrics, metrics) ||
      !identical(point_validation$pipeline_id, pipeline_id)) {
    stop(
      "point_validation must come from the same metrics and pipeline_id."
    )
  }
  if (!isTRUE(point_validation$reportable)) {
    stop(
      "point_validation is non-reportable; inspect and resolve its failure ",
      "audit before requesting a two-stage interval."
    )
  }

  set.seed(as.integer(seed))
  seed_pool <- sample.int(
    .Machine$integer.max,
    size = 2L * as.integer(outer_repetitions),
    replace = FALSE
  )
  outer_seed <- seed_pool[seq_len(as.integer(outer_repetitions))]
  inner_seed <- seed_pool[
    as.integer(outer_repetitions) +
      seq_len(as.integer(outer_repetitions))
  ]

  outer_rows <- vector("list", as.integer(outer_repetitions))
  outer_audit <- vector("list", as.integer(outer_repetitions))
  inner_failure_rows <- vector("list", as.integer(outer_repetitions))

  for (outer_id in seq_len(as.integer(outer_repetitions))) {
    set.seed(outer_seed[[outer_id]])
    outer_index <- sample.int(nrow(data), nrow(data), replace = TRUE)
    outer_data <- data[outer_index, , drop = FALSE]
    rownames(outer_data) <- NULL

    result <- tryCatch({
      v2_assert_binary_outcome(outer_data[[outcome]])
      inner <- v2_harrell_internal_validation(
        data = outer_data,
        outcome = outcome,
        fit_pipeline = fit_pipeline,
        predict_pipeline = predict_pipeline,
        score_pipeline = score_pipeline,
        metrics = metrics,
        repetitions = inner_repetitions,
        seed = inner_seed[[outer_id]],
        minimum_success_fraction = minimum_inner_success_fraction,
        pipeline_id = pipeline_id,
        ci_level = level,
        quantile_type = quantile_type
      )
      list(
        inner = inner,
        success = isTRUE(inner$reportable),
        reason = if (isTRUE(inner$reportable)) "" else paste0(
          "inner_bootstrap_success_below_minimum:",
          format(inner$success_fraction, digits = 5)
        )
      )
    }, error = function(e) e)

    if (inherits(result, "error")) {
      outer_audit[[outer_id]] <- data.frame(
        outer_replicate = outer_id,
        success = FALSE,
        reason = conditionMessage(result),
        inner_success_fraction = NA_real_,
        inner_failed_replicates = NA_integer_,
        stringsAsFactors = FALSE
      )
      outer_rows[[outer_id]] <- NULL
      inner_failure_rows[[outer_id]] <- NULL
    } else {
      inner <- result$inner
      inner_failed <- inner$audit[!inner$audit$success, , drop = FALSE]
      if (nrow(inner_failed)) {
        inner_failed$outer_replicate <- outer_id
        inner_failure_rows[[outer_id]] <- inner_failed[
          ,
          c("outer_replicate", "replicate", "reason"),
          drop = FALSE
        ]
        names(inner_failure_rows[[outer_id]])[2L] <- "inner_replicate"
      } else {
        inner_failure_rows[[outer_id]] <- NULL
      }
      outer_audit[[outer_id]] <- data.frame(
        outer_replicate = outer_id,
        success = result$success,
        reason = result$reason,
        inner_success_fraction = inner$success_fraction,
        inner_failed_replicates = inner$failed_replicates,
        stringsAsFactors = FALSE
      )
      outer_rows[[outer_id]] <- if (result$success) {
        data.frame(
          outer_replicate = outer_id,
          metric = metrics,
          corrected_estimate = as.numeric(inner$corrected[metrics]),
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }
  }

  audit <- do.call(rbind, outer_audit)
  successful <- sum(audit$success)
  success_fraction <- successful / as.integer(outer_repetitions)
  reportable <- success_fraction >= minimum_outer_success_fraction
  estimates <- do.call(rbind, outer_rows)
  if (is.null(estimates)) {
    estimates <- data.frame(
      outer_replicate = integer(),
      metric = character(),
      corrected_estimate = numeric(),
      stringsAsFactors = FALSE
    )
  }
  inner_failures <- do.call(rbind, inner_failure_rows)
  if (is.null(inner_failures)) {
    inner_failures <- data.frame(
      outer_replicate = integer(),
      inner_replicate = integer(),
      reason = character(),
      stringsAsFactors = FALSE
    )
  }

  ci <- lapply(metrics, function(metric) {
    values <- estimates$corrected_estimate[estimates$metric == metric]
    if (!reportable || length(values) < 20L ||
        anyNA(values) || any(!is.finite(values))) {
      return(data.frame(
        metric = metric,
        estimate = unname(point_validation$corrected[[metric]]),
        lower = NA_real_,
        upper = NA_real_,
        level = level,
        supported = FALSE,
        reason = "outer_bootstrap_success_below_prespecified_minimum",
        method = "Noma_two_stage_percentile",
        quantile_type = as.integer(quantile_type),
        stringsAsFactors = FALSE
      ))
    }
    interval <- v2_iv_percentile_interval(
      values,
      level = level,
      quantile_type = quantile_type
    )
    data.frame(
      metric = metric,
      estimate = unname(point_validation$corrected[[metric]]),
      lower = interval[[1L]],
      upper = interval[[2L]],
      level = level,
      supported = TRUE,
      reason = "",
      method = "Noma_two_stage_percentile",
      quantile_type = as.integer(quantile_type),
      stringsAsFactors = FALSE
    )
  })

  structure(
    list(
      method = "Noma_two_stage_bootstrap",
      pipeline_id = pipeline_id,
      metrics = metrics,
      point_validation = point_validation,
      outer_repetitions_requested = as.integer(outer_repetitions),
      inner_repetitions_requested = as.integer(inner_repetitions),
      successful_outer_replicates = successful,
      failed_outer_replicates =
        as.integer(outer_repetitions) - successful,
      outer_success_fraction = success_fraction,
      minimum_outer_success_fraction =
        minimum_outer_success_fraction,
      minimum_inner_success_fraction =
        minimum_inner_success_fraction,
      reportable = reportable,
      confidence_interval = do.call(rbind, ci),
      outer_estimates = estimates,
      outer_audit = audit,
      outer_failure_summary = v2_iv_failure_summary(data.frame(
        replicate = audit$outer_replicate,
        success = audit$success,
        reason = audit$reason,
        stringsAsFactors = FALSE
      )),
      inner_failures = inner_failures,
      reference = "Noma_et_al_Stat_Med_2021_doi_10.1002_sim.9148",
      seed = as.integer(seed)
    ),
    class = "ards_v2_two_stage_validation"
  )
}

v2_iv_assert_reportable <- function(validation) {
  if (inherits(validation, "ards_v2_harrell_validation")) {
    if (!isTRUE(validation$reportable)) {
      stop(
        "Harrell internal validation is non-reportable: ",
        validation$successful_replicates, "/",
        validation$repetitions_requested,
        " successful replicates (",
        format(validation$success_fraction, digits = 5),
        ")."
      )
    }
    return(invisible(TRUE))
  }
  if (inherits(validation, "ards_v2_two_stage_validation")) {
    if (!isTRUE(validation$reportable)) {
      stop(
        "Two-stage internal validation is non-reportable: ",
        validation$successful_outer_replicates, "/",
        validation$outer_repetitions_requested,
        " successful outer replicates (",
        format(validation$outer_success_fraction, digits = 5),
        ")."
      )
    }
    return(invisible(TRUE))
  }
  stop("Unknown internal-validation object.")
}
