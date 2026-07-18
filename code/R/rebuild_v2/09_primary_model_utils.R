# ARDS mechanical-power rebuild v2: primary model-frame and analysis helpers
#
# This module contains no top-level data access. Predictor-frame construction
# and outcome joining are deliberately separate so the MIMIC transformation
# bundle can be frozen without opening an outcome artifact.

v2_pm_model_columns <- c(
  "age", "sex_female", "pf_ratio", "map", "vasopressor",
  "platelet", "creatinine", "smp", "four_dprr",
  "driving_pressure", "rr", "static_power", "dynamic_power",
  "resistive_power"
)

v2_pm_baseline_columns <- c(
  "age", "sex_female", "pf_ratio", "map", "vasopressor",
  "platelet", "creatinine"
)

v2_pm_representation_columns <- c(
  "smp", "four_dprr", "driving_pressure", "rr",
  "static_power", "dynamic_power", "resistive_power"
)

v2_pm_forbidden_predictor_pattern <- paste(
  c(
    "mort", "death", "dead", "expire", "discharge.*status",
    "hospital.*expire", "outcome", "surviv", "post.*landmark.*death"
  ),
  collapse = "|"
)

v2_pm_require_columns <- function(x, columns, label = "data") {
  if (!is.data.frame(x)) stop(label, " must be a data frame.")
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(label, " lacks required column(s): ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

v2_pm_sha256_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !file.exists(path)) {
    stop("Cannot hash missing file: ", path)
  }
  output <- system2(
    "shasum",
    c("-a", "256", shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA256 failed for ", path, ": ", paste(output, collapse = " "))
  }
  hash <- strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
  if (!grepl("^[0-9a-f]{64}$", hash)) {
    stop("Invalid SHA256 output for ", path)
  }
  hash
}

v2_pm_atomic_save_rds <- function(object, path) {
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
    stop("Could not atomically publish RDS: ", path)
  }
  v2_pm_sha256_file(path)
}

v2_pm_atomic_write_csv <- function(x, path) {
  if (!is.data.frame(x)) stop("CSV output must be a data frame.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(x, temporary, row.names = FALSE, na = "")
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish CSV: ", path)
  }
  invisible(path)
}

v2_pm_predictor_leakage_audit <- function(x, label) {
  if (!is.data.frame(x)) stop(label, " must be a data frame.")
  flagged <- names(x)[grepl(
    v2_pm_forbidden_predictor_pattern,
    names(x),
    ignore.case = TRUE,
    perl = TRUE
  )]
  data.frame(
    source = label,
    column = if (length(flagged)) flagged else "",
    flagged = length(flagged) > 0L,
    pass = length(flagged) == 0L,
    stringsAsFactors = FALSE
  )
}

v2_pm_assert_outcome_free <- function(x, label) {
  audit <- v2_pm_predictor_leakage_audit(x, label)
  if (any(audit$flagged)) {
    stop(
      "Outcome-like predictor field found in ", label, ": ",
      paste(audit$column[audit$flagged], collapse = ", ")
    )
  }
  invisible(audit)
}

v2_pm_key <- function(x, columns, label) {
  v2_pm_require_columns(x, columns, label)
  values <- lapply(x[columns], function(value) {
    out <- as.character(value)
    if (anyNA(out) || any(!nzchar(out))) {
      stop(label, " contains missing or empty join keys.")
    }
    out
  })
  key <- do.call(paste, c(values, sep = "\r"))
  if (anyDuplicated(key)) {
    stop(label, " contains duplicate composite join keys.")
  }
  key
}

v2_pm_exact_left_join <- function(
    left,
    right,
    keys,
    right_columns,
    left_label = "left data",
    right_label = "right data",
    require_all_right_used = FALSE) {
  left_key <- v2_pm_key(left, keys, left_label)
  right_key <- v2_pm_key(right, keys, right_label)
  v2_pm_require_columns(right, right_columns, right_label)
  overlap <- intersect(right_columns, names(left))
  if (length(overlap)) {
    stop(
      "Exact join would overwrite left column(s): ",
      paste(overlap, collapse = ", ")
    )
  }
  position <- match(left_key, right_key)
  if (anyNA(position)) {
    stop(
      right_label, " lacks ", sum(is.na(position)),
      " required key(s) from ", left_label, "."
    )
  }
  if (require_all_right_used &&
      !setequal(left_key, right_key)) {
    stop(left_label, " and ", right_label, " do not have identical key sets.")
  }
  out <- as.data.frame(left, stringsAsFactors = FALSE)
  for (column in right_columns) {
    out[[column]] <- right[[column]][position]
  }
  if (!identical(v2_pm_key(out, keys, "joined data"), left_key)) {
    stop("Exact join changed patient row order.")
  }
  out
}

v2_pm_resolve_alias <- function(
    x, aliases, canonical_name, label, required = TRUE) {
  present <- intersect(aliases, names(x))
  if (!length(present)) {
    if (required) {
      stop(
        label, " lacks a controlled alias for ", canonical_name,
        ": ", paste(aliases, collapse = ", ")
      )
    }
    return(NULL)
  }
  # Alias order is a locked source priority. A canonical harmonized column
  # therefore takes precedence over a retained raw field such as sex/gender.
  present[[1L]]
}

v2_pm_numeric <- function(x, label) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    stripped <- trimws(x)
    valid <- !is.na(stripped) & grepl(
      "^[+-]?((([0-9]+)(\\.[0-9]*)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)?$",
      stripped
    )
    out <- rep(NA_real_, length(stripped))
    out[valid] <- suppressWarnings(as.numeric(stripped[valid]))
  } else {
    out <- suppressWarnings(as.numeric(x))
  }
  out[!is.finite(out)] <- NA_real_
  if (length(out) != length(x)) stop("Numeric conversion failed for ", label)
  out
}

v2_pm_sex_female <- function(x, label) {
  if (is.numeric(x) || is.integer(x) || is.logical(x)) {
    out <- as.numeric(x)
    out[!out %in% c(0, 1)] <- NA_real_
    return(out)
  }
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA_real_, length(z))
  out[z %in% c("f", "female", "woman")] <- 1
  out[z %in% c("m", "male", "man")] <- 0
  if (all(is.na(out))) stop("Sex coding is unrecognized in ", label)
  out
}

v2_pm_binary <- function(x, label) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    z <- tolower(trimws(x))
    out <- rep(NA_real_, length(z))
    out[z %in% c("1", "true", "yes", "y", "present")] <- 1
    out[z %in% c("0", "false", "no", "n", "absent")] <- 0
  } else {
    out <- suppressWarnings(as.numeric(x))
    out[!out %in% c(0, 1)] <- NA_real_
  }
  if (length(out) != length(x)) stop("Binary conversion failed for ", label)
  out
}

v2_pm_time_numeric <- function(x, label) {
  if (inherits(x, "POSIXt")) {
    out <- as.numeric(x)
  } else if (is.numeric(x) || is.integer(x)) {
    out <- as.numeric(x)
  } else {
    z <- trimws(as.character(x))
    parsed <- as.POSIXct(
      z,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
    out <- as.numeric(parsed)
  }
  if (anyNA(out) || any(!is.finite(out))) {
    stop("Incomplete or invalid time field: ", label)
  }
  out
}

v2_pm_core_aliases <- list(
  age = c("age"),
  sex_female = c("sex_female", "sex", "gender"),
  pf_ratio = c("pf_ratio", "index_pf"),
  map = c("map", "map_value", "map_harmonized"),
  vasopressor = c(
    "vasopressor", "vasopressor_any", "vasopressor_present"
  ),
  platelet = c("platelet", "platelets", "platelet_value"),
  creatinine = c("creatinine", "creatinine_value")
)

v2_pm_tuple_aliases <- list(
  smp = c("smp"),
  four_dprr = c("four_dprr"),
  driving_pressure = c("driving_pressure", "delta_p"),
  rr = c("rr", "rr_value"),
  static_power = c("static_power"),
  dynamic_power = c("dynamic_power"),
  resistive_power = c("resistive_power")
)

v2_pm_build_predictor_frame <- function(
    tuple_source,
    core_source,
    database = c("MIMIC-IV", "eICU-CRD")) {
  database <- match.arg(database)
  v2_pm_assert_outcome_free(tuple_source, paste(database, "tuple source"))
  v2_pm_assert_outcome_free(core_source, paste(database, "no-GCS core source"))

  contract <- if (database == "MIMIC-IV") {
    list(
      keys = c("subject_id", "hadm_id", "stay_id"),
      analysis_id = "stay_id",
      hospital = NULL,
      index = "index_time",
      landmark = "landmark_time",
      tuple_time = "ventilator_tuple_available_time",
      window_start = "covariate_window_start",
      window_end = "covariate_window_end"
    )
  } else {
    list(
      keys = c("patientunitstayid"),
      analysis_id = "patientunitstayid",
      hospital = "hospitalid",
      index = "index_time",
      landmark = "landmark_time",
      tuple_time = "ventilator_tuple_available_time",
      window_start = "covariate_window_start",
      window_end = "covariate_window_end"
    )
  }
  timing <- c(
    contract$index, contract$landmark, contract$tuple_time,
    contract$window_start, contract$window_end
  )
  v2_pm_require_columns(
    tuple_source,
    unique(c(contract$keys, contract$analysis_id, contract$hospital, timing)),
    paste(database, "tuple source")
  )
  v2_pm_require_columns(
    core_source,
    contract$keys,
    paste(database, "no-GCS core source")
  )

  core_mapping <- setNames(vapply(
    names(v2_pm_core_aliases),
    function(canonical) {
      v2_pm_resolve_alias(
        core_source,
        v2_pm_core_aliases[[canonical]],
        canonical,
        paste(database, "no-GCS core source")
      )
    },
    character(1L)
  ), names(v2_pm_core_aliases))
  tuple_mapping <- setNames(vapply(
    names(v2_pm_tuple_aliases),
    function(canonical) {
      v2_pm_resolve_alias(
        tuple_source,
        v2_pm_tuple_aliases[[canonical]],
        canonical,
        paste(database, "tuple source")
      )
    },
    character(1L)
  ), names(v2_pm_tuple_aliases))

  core_extract <- as.data.frame(core_source[contract$keys])
  for (canonical in names(core_mapping)) {
    source_column <- core_mapping[[canonical]]
    core_extract[[paste0(".core_", canonical)]] <-
      core_source[[source_column]]
  }
  joined <- v2_pm_exact_left_join(
    as.data.frame(tuple_source, stringsAsFactors = FALSE),
    core_extract,
    keys = contract$keys,
    right_columns = paste0(".core_", names(core_mapping)),
    left_label = paste(database, "tuple source"),
    right_label = paste(database, "no-GCS core source"),
    require_all_right_used = FALSE
  )

  analysis_id <- as.character(joined[[contract$analysis_id]])
  if (anyNA(analysis_id) || any(!nzchar(analysis_id)) ||
      anyDuplicated(analysis_id)) {
    stop(database, " analysis IDs must be unique and complete.")
  }
  hospital_id <- if (is.null(contract$hospital)) {
    rep("MIMIC_IV_SINGLE_CENTER", nrow(joined))
  } else {
    as.character(joined[[contract$hospital]])
  }
  if (anyNA(hospital_id) || any(!nzchar(hospital_id))) {
    stop(database, " hospital IDs are incomplete.")
  }

  frame <- data.frame(
    database = database,
    analysis_id = analysis_id,
    hospital_id = hospital_id,
    index_time_value =
      v2_pm_time_numeric(joined[[contract$index]], "index time"),
    landmark_time_value =
      v2_pm_time_numeric(joined[[contract$landmark]], "landmark time"),
    tuple_available_time_value =
      v2_pm_time_numeric(joined[[contract$tuple_time]], "tuple time"),
    covariate_window_start_value =
      v2_pm_time_numeric(joined[[contract$window_start]], "window start"),
    covariate_window_end_value =
      v2_pm_time_numeric(joined[[contract$window_end]], "window end"),
    age = v2_pm_numeric(joined$.core_age, "age"),
    sex_female = v2_pm_sex_female(joined$.core_sex_female, "sex"),
    pf_ratio = v2_pm_numeric(joined$.core_pf_ratio, "P/F ratio"),
    map = v2_pm_numeric(joined$.core_map, "MAP"),
    vasopressor =
      v2_pm_binary(joined$.core_vasopressor, "vasopressor"),
    platelet = v2_pm_numeric(joined$.core_platelet, "platelet"),
    creatinine = v2_pm_numeric(joined$.core_creatinine, "creatinine"),
    stringsAsFactors = FALSE
  )
  for (canonical in names(tuple_mapping)) {
    frame[[canonical]] <- v2_pm_numeric(
      joined[[tuple_mapping[[canonical]]]],
      canonical
    )
  }
  frame$core_complete <- stats::complete.cases(frame[v2_pm_model_columns])
  attr(frame, "source_contract") <- list(
    database = database,
    keys = contract$keys,
    core_mapping = core_mapping,
    tuple_mapping = tuple_mapping,
    tuple_n = nrow(tuple_source),
    core_n = nrow(core_source)
  )
  frame
}

v2_pm_range_rules <- data.frame(
  variable = v2_pm_model_columns,
  lower = c(
    18, 0, 0, 1, 0, 0, 0.1, 0, 5, 0, 5, 0, 0, 0
  ),
  upper = c(
    120, 1, 300, 250, 1, 9999, 28.28, 100, 220, 40, 60,
    100, 100, 100
  ),
  lower_inclusive = c(
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE
  ),
  upper_inclusive = rep(TRUE, length(v2_pm_model_columns)),
  stringsAsFactors = FALSE
)

v2_pm_range_qc <- function(frame, database) {
  v2_pm_require_columns(frame, v2_pm_model_columns, "predictor frame")
  rows <- lapply(seq_len(nrow(v2_pm_range_rules)), function(i) {
    rule <- v2_pm_range_rules[i, ]
    value <- frame[[rule$variable]]
    observed <- value[!is.na(value)]
    lower_ok <- if (rule$lower_inclusive) {
      observed >= rule$lower
    } else {
      observed > rule$lower
    }
    upper_ok <- if (rule$upper_inclusive) {
      observed <= rule$upper
    } else {
      observed < rule$upper
    }
    valid <- is.finite(observed) & lower_ok & upper_ok
    quantiles <- if (length(observed)) {
      as.numeric(stats::quantile(
        observed,
        probs = c(0, 0.05, 0.5, 0.95, 1),
        names = FALSE,
        type = 2L
      ))
    } else {
      rep(NA_real_, 5L)
    }
    data.frame(
      database = database,
      variable = rule$variable,
      total_n = length(value),
      available_n = length(observed),
      missing_n = sum(is.na(value)),
      invalid_n = sum(!valid),
      minimum = quantiles[[1L]],
      p05 = quantiles[[2L]],
      median = quantiles[[3L]],
      p95 = quantiles[[4L]],
      maximum = quantiles[[5L]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_pm_validate_predictor_frame <- function(
    frame,
    database,
    require_complete = FALSE,
    identity_tolerance = 1e-8) {
  required <- c(
    "database", "analysis_id", "hospital_id",
    "index_time_value", "landmark_time_value",
    "tuple_available_time_value", "covariate_window_start_value",
    "covariate_window_end_value", v2_pm_model_columns, "core_complete"
  )
  v2_pm_require_columns(frame, required, paste(database, "predictor frame"))
  if (nrow(frame) < 2L || anyDuplicated(frame$analysis_id) ||
      anyNA(frame$analysis_id) || any(!nzchar(frame$analysis_id))) {
    stop(database, " predictor frame has invalid analysis IDs.")
  }
  if (!all(frame$database == database) ||
      anyNA(frame$hospital_id) || any(!nzchar(frame$hospital_id))) {
    stop(database, " database/hospital labels are invalid.")
  }
  timing <- c(
    "index_time_value", "landmark_time_value",
    "tuple_available_time_value", "covariate_window_start_value",
    "covariate_window_end_value"
  )
  if (any(!vapply(frame[timing], is.numeric, logical(1L))) ||
      anyNA(frame[timing]) ||
      any(!vapply(frame[timing], function(x) all(is.finite(x)), logical(1L)))) {
    stop(database, " timing fields are not complete finite numerics.")
  }
  if (any(frame$landmark_time_value < frame$index_time_value) ||
      any(frame$tuple_available_time_value < frame$index_time_value) ||
      any(frame$tuple_available_time_value > frame$landmark_time_value) ||
      any(frame$covariate_window_start_value > frame$index_time_value) ||
      any(abs(
        frame$covariate_window_end_value - frame$landmark_time_value
      ) > identity_tolerance)) {
    stop(database, " predictor/tuple window timing invariant failed.")
  }
  expected_complete <- stats::complete.cases(frame[v2_pm_model_columns])
  if (!identical(as.logical(frame$core_complete), expected_complete)) {
    stop(database, " core_complete indicator is inconsistent.")
  }
  if (require_complete && !all(expected_complete)) {
    stop(database, " common-set frame contains incomplete predictors.")
  }
  range_qc <- v2_pm_range_qc(frame, database)
  complete_rows <- expected_complete
  if (any(range_qc$invalid_n > 0L)) {
    stop(
      database, " predictor range failure: ",
      paste(
        range_qc$variable[range_qc$invalid_n > 0L],
        collapse = ", "
      )
    )
  }
  if (any(complete_rows)) {
    identity_error <- frame$smp[complete_rows] - (
      frame$static_power[complete_rows] +
        frame$dynamic_power[complete_rows] +
        frame$resistive_power[complete_rows]
    )
    if (any(!is.finite(identity_error)) ||
        max(abs(identity_error)) > identity_tolerance) {
      stop(database, " surrogate algebraic identity failed.")
    }
    expected_four <- 4 * frame$driving_pressure[complete_rows] +
      frame$rr[complete_rows]
    if (max(abs(
      frame$four_dprr[complete_rows] - expected_four
    )) > identity_tolerance) {
      stop(database, " 4DPRR identity failed.")
    }
  }
  list(
    range_qc = range_qc,
    timing_qc = data.frame(
      database = database,
      n = nrow(frame),
      complete_n = sum(expected_complete),
      tuple_before_index_n = sum(
        frame$tuple_available_time_value < frame$index_time_value
      ),
      tuple_after_landmark_n = sum(
        frame$tuple_available_time_value > frame$landmark_time_value
      ),
      covariate_end_not_landmark_n = sum(abs(
        frame$covariate_window_end_value - frame$landmark_time_value
      ) > identity_tolerance),
      maximum_energy_identity_error = if (any(complete_rows)) {
        max(abs(identity_error))
      } else {
        NA_real_
      },
      maximum_4dprr_identity_error = if (any(complete_rows)) {
        max(abs(
          frame$four_dprr[complete_rows] - expected_four
        ))
      } else {
        NA_real_
      },
      pass = TRUE,
      stringsAsFactors = FALSE
    )
  )
}

v2_pm_complete_common_set <- function(frame, database) {
  v2_pm_validate_predictor_frame(frame, database, require_complete = FALSE)
  out <- frame[frame$core_complete, , drop = FALSE]
  rownames(out) <- NULL
  if (nrow(out) < 2L) stop(database, " complete common set is empty.")
  out$core_complete <- NULL
  v2_pm_validate_predictor_frame(
    transform(out, core_complete = TRUE),
    database,
    require_complete = TRUE
  )
  attr(out, "common_set_definition") <- paste(
    "Complete common set across the no-GCS baseline core and every locked",
    "ventilator representation before outcome access."
  )
  out
}

v2_pm_join_outcome <- function(
    predictor_frame,
    outcome_source,
    database,
    outcome_column =
      "in_hospital_mortality_after_6h_landmark") {
  v2_pm_validate_predictor_frame(
    transform(predictor_frame, core_complete = TRUE),
    database,
    require_complete = TRUE
  )
  if (!is.data.frame(outcome_source)) {
    stop(database, " outcome source must be a data frame.")
  }
  allowed <- if (database == "MIMIC-IV") {
    c("subject_id", "hadm_id", "stay_id", outcome_column)
  } else {
    c("patientunitstayid", "hospitalid", outcome_column)
  }
  unexpected <- setdiff(names(outcome_source), allowed)
  if (length(unexpected)) {
    stop(
      database, " outcome source contains unexpected field(s): ",
      paste(unexpected, collapse = ", ")
    )
  }
  id_column <- if (database == "MIMIC-IV") {
    "stay_id"
  } else {
    "patientunitstayid"
  }
  v2_pm_require_columns(
    outcome_source,
    c(id_column, outcome_column),
    paste(database, "outcome source")
  )
  outcome_id <- as.character(outcome_source[[id_column]])
  if (anyNA(outcome_id) || any(!nzchar(outcome_id)) ||
      anyDuplicated(outcome_id)) {
    stop(database, " outcome source IDs are invalid.")
  }
  position <- match(predictor_frame$analysis_id, outcome_id)
  if (anyNA(position)) {
    stop(database, " outcome source lacks common-set patients.")
  }
  outcome <- outcome_source[[outcome_column]][position]
  if ((!is.numeric(outcome) && !is.integer(outcome) &&
       !is.logical(outcome)) ||
      anyNA(outcome) ||
      !all(as.integer(outcome) %in% c(0L, 1L)) ||
      length(unique(as.integer(outcome))) != 2L) {
    stop(database, " outcome must be complete binary 0/1 with both classes.")
  }
  if (database == "eICU-CRD" &&
      "hospitalid" %in% names(outcome_source)) {
    outcome_hospital <- as.character(outcome_source$hospitalid[position])
    if (!identical(outcome_hospital, predictor_frame$hospital_id)) {
      stop("eICU hospital IDs disagree between predictors and outcomes.")
    }
  }
  out <- as.data.frame(predictor_frame, stringsAsFactors = FALSE)
  out$outcome <- as.integer(outcome)
  if (!identical(out$analysis_id, predictor_frame$analysis_id)) {
    stop("Outcome join changed common-set patient order.")
  }
  attr(out, "outcome_join") <- list(
    database = database,
    source_outcome_column = outcome_column,
    outcome_source_n = nrow(outcome_source),
    joined_n = nrow(out),
    events = sum(out$outcome),
    non_events = sum(out$outcome == 0L),
    exact_id_match = TRUE,
    row_order_preserved = TRUE
  )
  out
}

v2_pm_fit_models <- function(analysis_frame, transform_bundle) {
  v2_pm_require_columns(
    analysis_frame,
    c("analysis_id", "outcome", v2_pm_model_columns),
    "MIMIC analysis frame"
  )
  if (!exists("v2_build_design", mode = "function", inherits = TRUE) ||
      !exists("v2_fit_logistic", mode = "function", inherits = TRUE)) {
    stop("Source 01_analysis_utils.R before fitting primary models.")
  }
  fits <- setNames(lapply(
    v2_model_specification()$model_id,
    function(model_id) {
      design <- v2_build_design(
        analysis_frame, model_id, transform_bundle
      )
      v2_fit_logistic(
        design,
        analysis_frame$outcome,
        model_id,
        analysis_frame$analysis_id
      )
    }
  ), v2_model_specification()$model_id)
  fits
}

v2_pm_internal_fit_factory <- function(model_id) {
  valid <- v2_model_specification()$model_id
  if (!model_id %in% valid) stop("Unknown internal-validation model: ", model_id)
  force(model_id)
  function(data) {
    v2_pm_require_columns(
      data,
      c("analysis_id", "outcome", v2_pm_model_columns),
      paste(model_id, "bootstrap training data")
    )
    # This derivation is intentionally inside the callback. Every bootstrap
    # training sample gets its own knots; its paired test predictions use only
    # that replicate-specific bundle.
    replicate_bundle <- v2_derive_transform_bundle(data)
    design <- v2_build_design(data, model_id, replicate_bundle)
    fit <- v2_fit_logistic(
      design,
      data$outcome,
      model_id,
      paste0("resample_row_", seq_len(nrow(data)))
    )
    structure(
      list(
        model_id = model_id,
        fit = fit,
        transform_bundle = replicate_bundle,
        transformation_derivation_row_ids =
          as.character(data$analysis_id),
        transformation_rederived_in_training_sample = TRUE
      ),
      class = "ards_v2_pm_internal_model"
    )
  }
}

v2_pm_internal_predict_factory <- function(model_id) {
  valid <- v2_model_specification()$model_id
  if (!model_id %in% valid) stop("Unknown internal-validation model: ", model_id)
  force(model_id)
  function(model, data) {
    if (!inherits(model, "ards_v2_pm_internal_model") ||
        !identical(model$model_id, model_id) ||
        !isTRUE(model$transformation_rederived_in_training_sample)) {
      stop("Internal-validation object violates the refit contract.")
    }
    design <- v2_build_design(
      data, model_id, model$transform_bundle
    )
    stats::predict(model$fit, design, type = "response")
  }
}

v2_pm_internal_refit_contract_audit <- function(
    analysis_frame, model_id = "M_MP") {
  if (nrow(analysis_frame) < 20L) {
    stop("At least 20 rows are required for the refit-contract audit.")
  }
  full_bundle <- v2_derive_transform_bundle(analysis_frame)
  training <- analysis_frame[-seq_len(max(1L, floor(nrow(analysis_frame) / 5))), ,
                             drop = FALSE]
  model <- v2_pm_internal_fit_factory(model_id)(training)
  same_row_ids <- identical(
    model$transformation_derivation_row_ids,
    as.character(training$analysis_id)
  )
  not_full_sample <- !identical(
    model$transformation_derivation_row_ids,
    as.character(analysis_frame$analysis_id)
  )
  finite_prediction <- all(is.finite(
    v2_pm_internal_predict_factory(model_id)(model, analysis_frame)
  ))
  full_knots <- c(
    unlist(full_bundle$baseline_three_knots, use.names = TRUE),
    unlist(full_bundle$nonlinear_four_knots, use.names = TRUE)
  )
  replicate_knots <- c(
    unlist(
      model$transform_bundle$baseline_three_knots,
      use.names = TRUE
    ),
    unlist(
      model$transform_bundle$nonlinear_four_knots,
      use.names = TRUE
    )
  )
  parameter_difference_detected <- !isTRUE(all.equal(
    as.numeric(full_knots),
    as.numeric(replicate_knots),
    tolerance = 0
  ))
  data.frame(
    model_id = model_id,
    transformation_derivation_inside_fit_callback = TRUE,
    callback_derivation_rows_equal_training_rows = same_row_ids,
    callback_derivation_rows_not_full_sample = not_full_sample,
    replicate_bundle_differs_from_full_bundle =
      parameter_difference_detected,
    full_sample_prediction_with_replicate_bundle_finite =
      finite_prediction,
    pass = same_row_ids && not_full_sample &&
      parameter_difference_detected && finite_prediction,
    stringsAsFactors = FALSE
  )
}

v2_pm_predict_models <- function(
    fits,
    predictor_frame,
    transform_bundle) {
  model_ids <- v2_model_specification()$model_id
  if (!is.list(fits) || !identical(names(fits), model_ids)) {
    stop("Primary fit list does not match the locked model order.")
  }
  predictions <- setNames(lapply(model_ids, function(model_id) {
    design <- v2_build_design(
      predictor_frame, model_id, transform_bundle
    )
    stats::predict(fits[[model_id]], design, type = "response")
  }), model_ids)
  matrix <- do.call(cbind, predictions)
  colnames(matrix) <- model_ids
  if (nrow(matrix) != nrow(predictor_frame) ||
      anyNA(matrix) || any(!is.finite(matrix)) ||
      any(matrix < 0 | matrix > 1)) {
    stop("Frozen prediction matrix is invalid.")
  }
  matrix
}

v2_pm_coefficient_table <- function(
    fits,
    analysis_frame,
    transform_bundle,
    level = 0.95) {
  if (!is.numeric(level) || length(level) != 1L ||
      is.na(level) || level <= 0 || level >= 1) {
    stop("level must be in (0, 1).")
  }
  critical <- stats::qnorm(1 - (1 - level) / 2)
  rows <- lapply(names(fits), function(model_id) {
    fit <- fits[[model_id]]
    design <- v2_build_design(
      analysis_frame, model_id, transform_bundle
    )
    x <- cbind(`(Intercept)` = 1, design)
    probability <- stats::plogis(as.numeric(
      x %*% fit$coefficients
    ))
    information <- crossprod(
      x,
      x * (probability * (1 - probability))
    )
    covariance <- tryCatch(
      solve(information),
      error = function(e) e
    )
    if (inherits(covariance, "error") ||
        anyNA(covariance) || any(!is.finite(covariance))) {
      stop("Could not invert model information matrix: ", model_id)
    }
    standard_error <- sqrt(diag(covariance))
    estimate <- unname(fit$coefficients)
    z <- estimate / standard_error
    data.frame(
      model_id = model_id,
      term = names(fit$coefficients),
      estimate = estimate,
      standard_error = standard_error,
      lower = estimate - critical * standard_error,
      upper = estimate + critical * standard_error,
      z_value = z,
      p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
      confidence_level = level,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_pm_fit_summary <- function(fits) {
  rows <- lapply(fits, function(fit) {
    data.frame(
      model_id = fit$model_id,
      n = fit$n,
      events = fit$events,
      parameters = length(fit$coefficients),
      rank = fit$rank,
      log_likelihood = fit$log_likelihood,
      converged = fit$converged,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v2_pm_likelihood_ratio_tests <- function(fits) {
  rbind(
    v2_constraint_lrt(fits$M_4DPRR, fits$M_DPRR),
    v2_constraint_lrt(fits$M_MP, fits$M_ENERGY)
  )
}

v2_pm_collinearity_audits <- function(frame) {
  pressure_rate <- v2_increment_collinearity_audit(
    frame,
    c("driving_pressure", "rr"),
    audit_id = "M_DPRR_increment"
  )
  algebraic <- v2_increment_collinearity_audit(
    frame,
    c("static_power", "dynamic_power", "resistive_power"),
    audit_id = "M_ENERGY_increment"
  )
  list(
    summary = rbind(pressure_rate$summary, algebraic$summary),
    pressure_rate_correlation = pressure_rate$correlation,
    pressure_rate_vif = pressure_rate$vif,
    algebraic_correlation = algebraic$correlation,
    algebraic_vif = algebraic$vif
  )
}

v2_pm_predictions_long <- function(
    analysis_frame, predictions, database) {
  if (!is.matrix(predictions) ||
      nrow(predictions) != nrow(analysis_frame) ||
      is.null(colnames(predictions))) {
    stop("Prediction matrix and analysis frame do not align.")
  }
  rows <- lapply(seq_len(ncol(predictions)), function(j) {
    data.frame(
      database = database,
      analysis_id = analysis_frame$analysis_id,
      hospital_id = analysis_frame$hospital_id,
      outcome = analysis_frame$outcome,
      model_id = colnames(predictions)[j],
      frozen_probability = predictions[, j],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  expected <- rep(analysis_frame$analysis_id, times = ncol(predictions))
  if (!identical(out$analysis_id, expected)) {
    stop("Long prediction export changed patient/model ordering.")
  }
  out
}
