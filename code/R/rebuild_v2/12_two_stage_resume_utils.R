# ARDS mechanical-power rebuild v2: resumable two-stage validation utilities
#
# These helpers reproduce the outer/inner seed schedule and replicate-level
# algorithm in v2_two_stage_internal_validation(), but publish one atomic
# checkpoint per outer replicate.  A resumed run therefore does not repeat
# completed outer replicates and does not depend on the ambient RNG state.

V2_TS_RESUME_SCHEMA_VERSION <- "ards_v2_two_stage_resume_1.0.0"

v2_ts_require_dependencies <- function() {
  required <- c(
    "v2_assert_binary_outcome",
    "v2_harrell_internal_validation",
    "v2_iv_assert_scalar_integer",
    "v2_iv_assert_fraction",
    "v2_iv_percentile_interval",
    "v2_iv_failure_summary"
  )
  missing <- required[!vapply(
    required, exists, logical(1L), mode = "function", inherits = TRUE
  )]
  if (length(missing)) {
    stop(
      "Source 01_analysis_utils.R and 03_internal_validation_utils.R ",
      "before 12_two_stage_resume_utils.R. Missing: ",
      paste(missing, collapse = ", ")
    )
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for resumable contract hashing.")
  }
  invisible(TRUE)
}

v2_ts_assert_nonempty_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one nonempty string.")
  }
  invisible(TRUE)
}

v2_ts_atomic_save_rds <- function(object, path) {
  v2_ts_assert_nonempty_string(path, "path")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(object, temporary, version = 3L, compress = "xz")
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish checkpoint: ", path)
  }
  invisible(path)
}

v2_ts_contract_hash <- function(contract) {
  v2_ts_require_dependencies()
  if (!is.list(contract) || is.null(names(contract)) ||
      anyDuplicated(names(contract))) {
    stop("contract must be a uniquely named list.")
  }
  digest::digest(contract, algo = "sha256", serialize = TRUE)
}

v2_ts_make_contract <- function(
    model_id,
    model_index,
    data_n,
    events,
    outcome,
    metrics,
    pipeline_id,
    outer_repetitions,
    inner_repetitions,
    seed,
    minimum_inner_success_fraction,
    minimum_outer_success_fraction,
    level = 0.95,
    quantile_type = 7L,
    source_hashes = character()) {
  v2_ts_require_dependencies()
  v2_ts_assert_nonempty_string(model_id, "model_id")
  v2_ts_assert_nonempty_string(outcome, "outcome")
  v2_ts_assert_nonempty_string(pipeline_id, "pipeline_id")
  v2_iv_assert_scalar_integer(model_index, "model_index", 1L)
  v2_iv_assert_scalar_integer(data_n, "data_n", 2L)
  if (!is.numeric(events) || length(events) != 1L || is.na(events) ||
      !is.finite(events) || events != as.integer(events) ||
      events < 1L || events >= data_n) {
    stop("events must be one integer in [1, data_n - 1].")
  }
  if (!is.character(metrics) || !length(metrics) || anyNA(metrics) ||
      any(!nzchar(metrics)) || anyDuplicated(metrics)) {
    stop("metrics must be unique nonempty names.")
  }
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
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be in (0, 1).")
  }
  v2_iv_assert_scalar_integer(quantile_type, "quantile_type", 1L)
  if (quantile_type > 9L) {
    stop("quantile_type must be between 1 and 9.")
  }
  if (!is.character(source_hashes) ||
      (length(source_hashes) &&
       (is.null(names(source_hashes)) ||
        anyNA(names(source_hashes)) ||
        any(!nzchar(names(source_hashes))) ||
        anyDuplicated(names(source_hashes)) ||
        any(!grepl("^[0-9a-f]{64}$", source_hashes))))) {
    stop("source_hashes must be a uniquely named SHA256 vector.")
  }
  source_hashes <- source_hashes[order(names(source_hashes))]
  list(
    schema_version = V2_TS_RESUME_SCHEMA_VERSION,
    model_id = model_id,
    model_index = as.integer(model_index),
    data_n = as.integer(data_n),
    events = as.integer(events),
    outcome = outcome,
    metrics = metrics,
    pipeline_id = pipeline_id,
    outer_repetitions = as.integer(outer_repetitions),
    inner_repetitions = as.integer(inner_repetitions),
    seed = as.integer(seed),
    minimum_inner_success_fraction =
      as.numeric(minimum_inner_success_fraction),
    minimum_outer_success_fraction =
      as.numeric(minimum_outer_success_fraction),
    level = as.numeric(level),
    quantile_type = as.integer(quantile_type),
    source_hashes = source_hashes
  )
}

v2_ts_seed_schedule <- function(seed, outer_repetitions) {
  v2_ts_require_dependencies()
  v2_iv_assert_scalar_integer(seed, "seed", 1L)
  v2_iv_assert_scalar_integer(
    outer_repetitions, "outer_repetitions", 20L
  )
  set.seed(as.integer(seed))
  seed_pool <- sample.int(
    .Machine$integer.max,
    size = 2L * as.integer(outer_repetitions),
    replace = FALSE
  )
  data.frame(
    outer_replicate = seq_len(as.integer(outer_repetitions)),
    outer_seed = seed_pool[seq_len(as.integer(outer_repetitions))],
    inner_seed = seed_pool[
      as.integer(outer_repetitions) +
        seq_len(as.integer(outer_repetitions))
    ],
    stringsAsFactors = FALSE
  )
}

v2_ts_checkpoint_path <- function(
    checkpoint_dir,
    outer_replicate,
    outer_repetitions) {
  v2_ts_assert_nonempty_string(checkpoint_dir, "checkpoint_dir")
  v2_iv_assert_scalar_integer(
    outer_replicate, "outer_replicate", 1L
  )
  v2_iv_assert_scalar_integer(
    outer_repetitions, "outer_repetitions", 20L
  )
  if (outer_replicate > outer_repetitions) {
    stop("outer_replicate exceeds outer_repetitions.")
  }
  width <- max(4L, nchar(as.character(outer_repetitions)))
  file.path(
    checkpoint_dir,
    sprintf(
      paste0("outer_%0", width, "d.rds"),
      as.integer(outer_replicate)
    )
  )
}

v2_ts_run_outer_replicate <- function(
    data,
    outcome,
    fit_pipeline,
    predict_pipeline,
    score_pipeline,
    metrics,
    inner_repetitions,
    outer_replicate,
    outer_seed,
    inner_seed,
    minimum_inner_success_fraction,
    pipeline_id,
    level,
    quantile_type,
    contract_hash) {
  v2_ts_require_dependencies()
  v2_ts_assert_nonempty_string(contract_hash, "contract_hash")
  v2_iv_assert_scalar_integer(
    outer_replicate, "outer_replicate", 1L
  )
  v2_iv_assert_scalar_integer(outer_seed, "outer_seed", 1L)
  v2_iv_assert_scalar_integer(inner_seed, "inner_seed", 1L)

  set.seed(as.integer(outer_seed))
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
      seed = as.integer(inner_seed),
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
    audit <- data.frame(
      outer_replicate = as.integer(outer_replicate),
      success = FALSE,
      reason = conditionMessage(result),
      inner_success_fraction = NA_real_,
      inner_failed_replicates = NA_integer_,
      stringsAsFactors = FALSE
    )
    estimates <- data.frame(
      outer_replicate = integer(),
      metric = character(),
      corrected_estimate = numeric(),
      stringsAsFactors = FALSE
    )
    inner_failures <- data.frame(
      outer_replicate = integer(),
      inner_replicate = integer(),
      reason = character(),
      stringsAsFactors = FALSE
    )
  } else {
    inner <- result$inner
    inner_failed <- inner$audit[!inner$audit$success, , drop = FALSE]
    if (nrow(inner_failed)) {
      inner_failed$outer_replicate <- as.integer(outer_replicate)
      inner_failures <- inner_failed[
        ,
        c("outer_replicate", "replicate", "reason"),
        drop = FALSE
      ]
      names(inner_failures)[2L] <- "inner_replicate"
    } else {
      inner_failures <- data.frame(
        outer_replicate = integer(),
        inner_replicate = integer(),
        reason = character(),
        stringsAsFactors = FALSE
      )
    }
    audit <- data.frame(
      outer_replicate = as.integer(outer_replicate),
      success = result$success,
      reason = result$reason,
      inner_success_fraction = inner$success_fraction,
      inner_failed_replicates = inner$failed_replicates,
      stringsAsFactors = FALSE
    )
    estimates <- if (result$success) {
      data.frame(
        outer_replicate = as.integer(outer_replicate),
        metric = metrics,
        corrected_estimate = as.numeric(inner$corrected[metrics]),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        outer_replicate = integer(),
        metric = character(),
        corrected_estimate = numeric(),
        stringsAsFactors = FALSE
      )
    }
  }

  structure(
    list(
      schema_version = V2_TS_RESUME_SCHEMA_VERSION,
      contract_hash = contract_hash,
      outer_replicate = as.integer(outer_replicate),
      outer_seed = as.integer(outer_seed),
      inner_seed = as.integer(inner_seed),
      audit = audit,
      estimates = estimates,
      inner_failures = inner_failures
    ),
    class = "ards_v2_two_stage_checkpoint"
  )
}

v2_ts_validate_checkpoint <- function(
    checkpoint,
    contract_hash,
    schedule_row,
    metrics) {
  if (!inherits(checkpoint, "ards_v2_two_stage_checkpoint") ||
      !is.list(checkpoint)) {
    stop("Malformed two-stage checkpoint object.")
  }
  required <- c(
    "schema_version", "contract_hash", "outer_replicate",
    "outer_seed", "inner_seed", "audit", "estimates", "inner_failures"
  )
  if (!all(required %in% names(checkpoint))) {
    stop("Two-stage checkpoint lacks required fields.")
  }
  if (!identical(
    checkpoint$schema_version, V2_TS_RESUME_SCHEMA_VERSION
  ) || !identical(checkpoint$contract_hash, contract_hash)) {
    stop("Two-stage checkpoint contract mismatch.")
  }
  expected_id <- as.integer(schedule_row$outer_replicate[[1L]])
  expected_outer_seed <- as.integer(schedule_row$outer_seed[[1L]])
  expected_inner_seed <- as.integer(schedule_row$inner_seed[[1L]])
  if (!identical(checkpoint$outer_replicate, expected_id) ||
      !identical(checkpoint$outer_seed, expected_outer_seed) ||
      !identical(checkpoint$inner_seed, expected_inner_seed)) {
    stop("Two-stage checkpoint seed schedule mismatch.")
  }
  audit <- checkpoint$audit
  if (!is.data.frame(audit) || nrow(audit) != 1L ||
      !all(c(
        "outer_replicate", "success", "reason",
        "inner_success_fraction", "inner_failed_replicates"
      ) %in% names(audit)) ||
      audit$outer_replicate[[1L]] != expected_id ||
      is.na(audit$success[[1L]])) {
    stop("Malformed two-stage checkpoint audit.")
  }
  estimates <- checkpoint$estimates
  if (!is.data.frame(estimates) ||
      !all(c(
        "outer_replicate", "metric", "corrected_estimate"
      ) %in% names(estimates))) {
    stop("Malformed two-stage checkpoint estimates.")
  }
  if (isTRUE(audit$success[[1L]])) {
    if (nrow(estimates) != length(metrics) ||
        !identical(as.character(estimates$metric), metrics) ||
        any(estimates$outer_replicate != expected_id) ||
        anyNA(estimates$corrected_estimate) ||
        any(!is.finite(estimates$corrected_estimate))) {
      stop("Successful checkpoint has invalid corrected estimates.")
    }
  } else if (nrow(estimates) != 0L) {
    stop("Failed checkpoint must not contain corrected estimates.")
  }
  inner_failures <- checkpoint$inner_failures
  if (!is.data.frame(inner_failures) ||
      !all(c(
        "outer_replicate", "inner_replicate", "reason"
      ) %in% names(inner_failures)) ||
      (nrow(inner_failures) &&
       any(inner_failures$outer_replicate != expected_id))) {
    stop("Malformed two-stage checkpoint inner-failure audit.")
  }
  invisible(TRUE)
}

v2_ts_resume <- function(
    data,
    outcome,
    fit_pipeline,
    predict_pipeline,
    score_pipeline = v2_iv_default_score,
    contract,
    checkpoint_dir,
    max_new_replicates = Inf,
    progress_every = 1L) {
  v2_ts_require_dependencies()
  contract_hash <- v2_ts_contract_hash(contract)
  if (!is.numeric(max_new_replicates) || length(max_new_replicates) != 1L ||
      is.na(max_new_replicates) || max_new_replicates <= 0 ||
      (!is.infinite(max_new_replicates) &&
       (max_new_replicates != as.integer(max_new_replicates)))) {
    stop("max_new_replicates must be a positive integer or Inf.")
  }
  v2_iv_assert_scalar_integer(progress_every, "progress_every", 1L)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  schedule <- v2_ts_seed_schedule(
    contract$seed, contract$outer_repetitions
  )
  completed_before <- 0L
  new_replicates <- 0L

  for (outer_id in seq_len(contract$outer_repetitions)) {
    path <- v2_ts_checkpoint_path(
      checkpoint_dir, outer_id, contract$outer_repetitions
    )
    schedule_row <- schedule[
      schedule$outer_replicate == outer_id,
      ,
      drop = FALSE
    ]
    if (file.exists(path)) {
      checkpoint <- readRDS(path)
      v2_ts_validate_checkpoint(
        checkpoint, contract_hash, schedule_row, contract$metrics
      )
      completed_before <- completed_before + 1L
      next
    }
    if (new_replicates >= max_new_replicates) next
    checkpoint <- v2_ts_run_outer_replicate(
      data = data,
      outcome = outcome,
      fit_pipeline = fit_pipeline,
      predict_pipeline = predict_pipeline,
      score_pipeline = score_pipeline,
      metrics = contract$metrics,
      inner_repetitions = contract$inner_repetitions,
      outer_replicate = outer_id,
      outer_seed = schedule_row$outer_seed[[1L]],
      inner_seed = schedule_row$inner_seed[[1L]],
      minimum_inner_success_fraction =
        contract$minimum_inner_success_fraction,
      pipeline_id = contract$pipeline_id,
      level = contract$level,
      quantile_type = contract$quantile_type,
      contract_hash = contract_hash
    )
    v2_ts_validate_checkpoint(
      checkpoint, contract_hash, schedule_row, contract$metrics
    )
    v2_ts_atomic_save_rds(checkpoint, path)
    new_replicates <- new_replicates + 1L
    if (new_replicates %% progress_every == 0L) {
      message(
        "Two-stage checkpoint ", outer_id, "/",
        contract$outer_repetitions, " published for ",
        contract$model_id, "."
      )
    }
  }

  checkpoint_paths <- vapply(
    seq_len(contract$outer_repetitions),
    function(outer_id) {
      v2_ts_checkpoint_path(
        checkpoint_dir, outer_id, contract$outer_repetitions
      )
    },
    character(1L)
  )
  completed_after <- sum(file.exists(checkpoint_paths))
  list(
    contract_hash = contract_hash,
    completed_before = completed_before,
    new_replicates = new_replicates,
    completed_after = completed_after,
    pending_after = contract$outer_repetitions - completed_after,
    complete = completed_after == contract$outer_repetitions,
    checkpoint_paths = checkpoint_paths
  )
}

v2_ts_collect_validation <- function(
    contract,
    checkpoint_dir,
    point_validation) {
  v2_ts_require_dependencies()
  if (!inherits(point_validation, "ards_v2_harrell_validation") ||
      !identical(point_validation$metrics, contract$metrics) ||
      !identical(point_validation$pipeline_id, contract$pipeline_id)) {
    stop(
      "point_validation must come from the same metrics and pipeline_id."
    )
  }
  if (!isTRUE(point_validation$reportable)) {
    stop("point_validation is non-reportable.")
  }
  contract_hash <- v2_ts_contract_hash(contract)
  schedule <- v2_ts_seed_schedule(
    contract$seed, contract$outer_repetitions
  )
  checkpoints <- vector("list", contract$outer_repetitions)
  for (outer_id in seq_len(contract$outer_repetitions)) {
    path <- v2_ts_checkpoint_path(
      checkpoint_dir, outer_id, contract$outer_repetitions
    )
    if (!file.exists(path)) {
      stop("Missing outer checkpoint: ", outer_id)
    }
    checkpoint <- readRDS(path)
    v2_ts_validate_checkpoint(
      checkpoint,
      contract_hash,
      schedule[schedule$outer_replicate == outer_id, , drop = FALSE],
      contract$metrics
    )
    checkpoints[[outer_id]] <- checkpoint
  }

  audit <- do.call(rbind, lapply(checkpoints, `[[`, "audit"))
  rownames(audit) <- NULL
  successful <- sum(audit$success)
  success_fraction <- successful / contract$outer_repetitions
  reportable <- success_fraction >=
    contract$minimum_outer_success_fraction

  estimate_rows <- lapply(checkpoints, `[[`, "estimates")
  estimate_rows <- estimate_rows[vapply(
    estimate_rows, nrow, integer(1L)
  ) > 0L]
  estimates <- if (length(estimate_rows)) {
    do.call(rbind, estimate_rows)
  } else {
    data.frame(
      outer_replicate = integer(),
      metric = character(),
      corrected_estimate = numeric(),
      stringsAsFactors = FALSE
    )
  }
  rownames(estimates) <- NULL

  failure_rows <- lapply(checkpoints, `[[`, "inner_failures")
  failure_rows <- failure_rows[vapply(
    failure_rows, nrow, integer(1L)
  ) > 0L]
  inner_failures <- if (length(failure_rows)) {
    do.call(rbind, failure_rows)
  } else {
    data.frame(
      outer_replicate = integer(),
      inner_replicate = integer(),
      reason = character(),
      stringsAsFactors = FALSE
    )
  }
  rownames(inner_failures) <- NULL

  ci <- lapply(contract$metrics, function(metric) {
    values <- estimates$corrected_estimate[estimates$metric == metric]
    if (!reportable || length(values) < 20L ||
        anyNA(values) || any(!is.finite(values))) {
      return(data.frame(
        metric = metric,
        estimate = unname(point_validation$corrected[[metric]]),
        lower = NA_real_,
        upper = NA_real_,
        level = contract$level,
        supported = FALSE,
        reason = "outer_bootstrap_success_below_prespecified_minimum",
        method = "Noma_two_stage_percentile",
        quantile_type = contract$quantile_type,
        stringsAsFactors = FALSE
      ))
    }
    interval <- v2_iv_percentile_interval(
      values,
      level = contract$level,
      quantile_type = contract$quantile_type
    )
    data.frame(
      metric = metric,
      estimate = unname(point_validation$corrected[[metric]]),
      lower = interval[[1L]],
      upper = interval[[2L]],
      level = contract$level,
      supported = TRUE,
      reason = "",
      method = "Noma_two_stage_percentile",
      quantile_type = contract$quantile_type,
      stringsAsFactors = FALSE
    )
  })

  structure(
    list(
      method = "Noma_two_stage_bootstrap",
      pipeline_id = contract$pipeline_id,
      metrics = contract$metrics,
      point_validation = point_validation,
      outer_repetitions_requested = contract$outer_repetitions,
      inner_repetitions_requested = contract$inner_repetitions,
      successful_outer_replicates = successful,
      failed_outer_replicates =
        contract$outer_repetitions - successful,
      outer_success_fraction = success_fraction,
      minimum_outer_success_fraction =
        contract$minimum_outer_success_fraction,
      minimum_inner_success_fraction =
        contract$minimum_inner_success_fraction,
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
      seed = contract$seed,
      resumable_schema_version = V2_TS_RESUME_SCHEMA_VERSION,
      resumable_contract = contract,
      resumable_contract_hash = contract_hash
    ),
    class = "ards_v2_two_stage_validation"
  )
}

v2_ts_checkpoint_manifest <- function(contract, checkpoint_dir, hash_file) {
  if (!is.function(hash_file)) stop("hash_file must be a function.")
  paths <- vapply(
    seq_len(contract$outer_repetitions),
    function(outer_id) {
      v2_ts_checkpoint_path(
        checkpoint_dir, outer_id, contract$outer_repetitions
      )
    },
    character(1L)
  )
  if (any(!file.exists(paths))) {
    stop("Cannot manifest incomplete checkpoint set.")
  }
  data.frame(
    model_id = contract$model_id,
    outer_replicate = seq_len(contract$outer_repetitions),
    path = paths,
    sha256 = vapply(paths, hash_file, character(1L)),
    stringsAsFactors = FALSE
  )
}
