# ARDS mechanical-power rebuild v2: complete-GCS sensitivity helpers
#
# These functions are outcome blind. They implement the source-specific GCS
# definitions locked in decision V2-D021 and a complete-GCS model design whose
# transformation parameters are derived in MIMIC-IV and applied unchanged to
# eICU-CRD.

v2_cg_strict_numeric <- function(x) {
  z <- trimws(as.character(x))
  valid <- !is.na(z) & nzchar(z) & grepl(
    "^[+-]?((([0-9]+)(\\.[0-9]*)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)?$",
    z
  )
  out <- rep(NA_real_, length(z))
  out[valid] <- suppressWarnings(as.numeric(z[valid]))
  out[!is.finite(out)] <- NA_real_
  out
}

v2_cg_parse_mimic_time <- function(x, label) {
  z <- trimws(as.character(x))
  parsed <- as.POSIXct(
    z, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )
  out <- as.numeric(parsed)
  if (anyNA(out) || any(!is.finite(out))) {
    stop("Unparseable MIMIC time in ", label, ".")
  }
  out
}

v2_cg_prefix_score <- function(x) {
  match <- regexec(
    "^([1-6])(?:\\.0)?(?:[[:space:]]|$)", x, perl = TRUE
  )
  pieces <- regmatches(x, match)
  vapply(
    pieces,
    function(value) {
      if (length(value) >= 2L) as.numeric(value[[2L]]) else NA_real_
    },
    numeric(1L)
  )
}

v2_cg_mimic_category_score <- function(component, text) {
  data.table::fcase(
    component == "gcs_eye" & grepl("spont", text), 4,
    component == "gcs_eye" & grepl("to speech|speech", text), 3,
    component == "gcs_eye" & grepl("to pain|pain", text), 2,
    component == "gcs_eye" & grepl("no response|none", text), 1,
    component == "gcs_verbal" & grepl("orient", text), 5,
    component == "gcs_verbal" & grepl("confus", text), 4,
    component == "gcs_verbal" & grepl("inappropriate", text), 3,
    component == "gcs_verbal" & grepl("incomprehensible", text), 2,
    component == "gcs_verbal" & grepl("no response|none", text), 1,
    component == "gcs_motor" & grepl("obeys", text), 6,
    component == "gcs_motor" & grepl("localiz", text), 5,
    component == "gcs_motor" & grepl("withdraw|flex-withdraw", text), 4,
    component == "gcs_motor" & grepl("abnormal flex|flexion", text), 3,
    component == "gcs_motor" & grepl("abnormal extens|extension", text), 2,
    component == "gcs_motor" & grepl("no response|none", text), 1,
    default = NA_real_
  )
}

v2_cg_parse_mimic_components <- function(raw) {
  required <- c(
    "stay_id", "charttime", "storetime", "itemid", "value", "valuenum"
  )
  v2_pm_require_columns(raw, required, "MIMIC complete-GCS candidate cache")
  x <- data.table::as.data.table(raw)
  x[, itemid_numeric := v2_cg_strict_numeric(itemid)]
  if (anyNA(x$itemid_numeric) ||
      any(!x$itemid_numeric %in% c(220739, 223900, 223901))) {
    stop("Unexpected item entered the MIMIC complete-GCS cache.")
  }
  x[, component := data.table::fcase(
    itemid_numeric == 220739, "gcs_eye",
    itemid_numeric == 223900, "gcs_verbal",
    itemid_numeric == 223901, "gcs_motor",
    default = NA_character_
  )]
  x[, component_upper := data.table::fcase(
    component == "gcs_eye", 4,
    component == "gcs_verbal", 5,
    component == "gcs_motor", 6,
    default = NA_real_
  )]
  x[, measurement_time := v2_cg_parse_mimic_time(
    charttime, "charttime"
  )]
  store_text <- trimws(as.character(x$storetime))
  x[, storetime_missing := is.na(store_text) | !nzchar(store_text)]
  x[, available_time := NA_real_]
  if (any(!x$storetime_missing)) {
    x[storetime_missing == FALSE, available_time :=
      v2_cg_parse_mimic_time(storetime, "nonmissing storetime")]
  }
  x[, value_numeric := v2_cg_strict_numeric(valuenum)]
  x[, text_norm := tolower(trimws(as.character(value)))]
  x[is.na(text_norm), text_norm := ""]
  x[, unscorable_airway_text := grepl(
    "ett|et[/ -]?trach|trache|intubat|unable to score", text_norm
  )]
  x[, text_prefix_score := v2_cg_prefix_score(text_norm)]
  x[, text_category_score :=
    v2_cg_mimic_category_score(component, text_norm)]
  x[, text_internal_conflict :=
    !is.na(text_prefix_score) & !is.na(text_category_score) &
      text_prefix_score != text_category_score]
  x[, text_score := data.table::fcase(
    text_internal_conflict, NA_real_,
    !is.na(text_prefix_score), text_prefix_score,
    default = text_category_score
  )]
  x[, valuenum_valid :=
    !is.na(value_numeric) &
      abs(value_numeric - round(value_numeric)) < 1e-10 &
      value_numeric >= 1 & value_numeric <= component_upper]
  x[, text_score_valid :=
    !is.na(text_score) &
      abs(text_score - round(text_score)) < 1e-10 &
      text_score >= 1 & text_score <= component_upper]
  x[, value_text_conflict :=
    valuenum_valid & text_score_valid & value_numeric != text_score]
  x[, component_value := data.table::fcase(
    unscorable_airway_text |
      text_internal_conflict |
      value_text_conflict,
    NA_real_,
    valuenum_valid, value_numeric,
    text_score_valid, text_score,
    default = NA_real_
  )]
  x[, component_valid := !is.na(component_value)]
  x[]
}

v2_cg_mimic_bounds <- function(target) {
  required <- c(
    "stay_id", "intime", "index_time", "landmark_time",
    "covariate_window_start", "covariate_window_end"
  )
  v2_pm_require_columns(target, required, "MIMIC complete-GCS target")
  if (anyDuplicated(target$stay_id)) {
    stop("MIMIC complete-GCS target contains duplicate stays.")
  }
  out <- data.table::data.table(
    stay_id = as.character(target$stay_id),
    intime_value = v2_pm_time_numeric(target$intime, "MIMIC ICU intime"),
    index_time_value =
      v2_pm_time_numeric(target$index_time, "MIMIC index time"),
    landmark_time_value =
      v2_pm_time_numeric(target$landmark_time, "MIMIC landmark time"),
    window_start_value = v2_pm_time_numeric(
      target$covariate_window_start, "MIMIC GCS window start"
    ),
    window_end_value = v2_pm_time_numeric(
      target$covariate_window_end, "MIMIC GCS window end"
    )
  )
  expected_start <- pmax(
    out$intime_value, out$index_time_value - 24 * 60 * 60
  )
  if (any(abs(out$window_start_value - expected_start) > 1e-8) ||
      any(abs(
        out$window_end_value - out$landmark_time_value
      ) > 1e-8) ||
      any(abs(
        out$landmark_time_value - out$index_time_value - 6 * 60 * 60
      ) > 1e-8)) {
    stop("MIMIC complete-GCS fixed-6h window contract failed.")
  }
  out
}

v2_cg_derive_mimic <- function(raw, target) {
  parsed <- v2_cg_parse_mimic_components(raw)
  bounds <- v2_cg_mimic_bounds(target)
  parsed[, stay_id := as.character(stay_id)]
  if (any(!parsed$stay_id %in% bounds$stay_id)) {
    stop("MIMIC complete-GCS cache contains an off-target stay.")
  }
  z <- merge(parsed, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  if (nrow(z) != nrow(parsed)) {
    stop("MIMIC complete-GCS candidates did not map one-to-one to targets.")
  }
  z[, measurement_in_window :=
    measurement_time >= window_start_value &
      measurement_time <= window_end_value]
  z[, available_by_landmark :=
    !storetime_missing & !is.na(available_time) &
      available_time <= window_end_value]
  eligible <- z[
    measurement_in_window &
      available_by_landmark &
      component_valid
  ]
  reduced <- eligible[, {
    values <- sort(unique(component_value))
    list(
      value_conflict = length(values) > 1L,
      component_value =
        if (length(values) == 1L) values[[1L]] else NA_real_,
      component_available_time =
        if (length(values) == 1L) max(available_time) else NA_real_,
      duplicate_rows = .N
    )
  }, by = .(
    stay_id,
    measurement_time,
    component
  )]
  conflict_groups <- sum(reduced$value_conflict)
  reduced <- reduced[
    value_conflict == FALSE & !is.na(component_value)
  ]
  wide <- data.table::dcast(
    reduced,
    stay_id + measurement_time ~ component,
    value.var = c("component_value", "component_available_time")
  )
  needed <- c(
    paste0(
      "component_value_",
      c("gcs_eye", "gcs_verbal", "gcs_motor")
    ),
    paste0(
      "component_available_time_",
      c("gcs_eye", "gcs_verbal", "gcs_motor")
    )
  )
  for (column in needed) {
    if (!column %in% names(wide)) wide[, (column) := NA_real_]
  }
  complete <- wide[
    !is.na(component_value_gcs_eye) &
      !is.na(component_value_gcs_verbal) &
      !is.na(component_value_gcs_motor)
  ]
  complete[, gcs := component_value_gcs_eye +
    component_value_gcs_verbal +
    component_value_gcs_motor]
  complete[, gcs_available_time_value := pmax(
    component_available_time_gcs_eye,
    component_available_time_gcs_verbal,
    component_available_time_gcs_motor
  )]
  complete[, gcs_measurement_time_value := measurement_time]
  complete[, gcs_source :=
    "same_charttime_eye_verbal_motor_strict_reconstruction"]
  data.table::setorder(
    complete,
    stay_id,
    gcs,
    gcs_measurement_time_value,
    gcs_available_time_value
  )
  selected <- complete[, .SD[1L], by = stay_id]
  selected <- selected[, .(
    stay_id,
    gcs,
    gcs_measurement_time_value,
    gcs_available_time_value,
    gcs_eye = component_value_gcs_eye,
    gcs_verbal = component_value_gcs_verbal,
    gcs_motor = component_value_gcs_motor,
    gcs_source
  )]
  if (nrow(selected) &&
      (any(selected$gcs < 3 | selected$gcs > 15) ||
       any(selected$gcs_available_time_value >
             bounds$window_end_value[
               match(selected$stay_id, bounds$stay_id)
             ]))) {
    stop("Selected MIMIC GCS range/timing validation failed.")
  }
  list(
    selected = selected,
    parsed = parsed,
    timing_qc = data.frame(
      database = "MIMIC-IV",
      target_n = nrow(bounds),
      raw_candidate_rows = nrow(parsed),
      target_candidate_stays = data.table::uniqueN(parsed$stay_id),
      measurement_in_window_rows = sum(z$measurement_in_window),
      available_by_landmark_rows = sum(
        z$measurement_in_window & z$available_by_landmark
      ),
      valid_component_rows = nrow(eligible),
      storetime_missing_rows = sum(z$storetime_missing),
      airway_unscorable_rows = sum(
        z$measurement_in_window &
          z$available_by_landmark &
          z$unscorable_airway_text
      ),
      text_internal_conflict_rows = sum(
        z$measurement_in_window &
          z$available_by_landmark &
          z$text_internal_conflict
      ),
      value_text_conflict_rows = sum(
        z$measurement_in_window &
          z$available_by_landmark &
          z$value_text_conflict
      ),
      duplicate_component_time_conflict_groups = conflict_groups,
      complete_same_time_candidates = nrow(complete),
      selected_patients = nrow(selected),
      selected_explicit_total = 0L,
      selected_reconstructed = nrow(selected),
      stringsAsFactors = FALSE
    )
  )
}

v2_cg_eicu_mapping <- function(label, name) {
  data.table::fcase(
    label == "Glasgow coma score" & name == "GCS Total",
    "gcs_total",
    label == "Score (Glasgow Coma Scale)" & name == "Value",
    "gcs_total",
    label == "Glasgow coma score" & name == "Eyes",
    "gcs_eye",
    label == "Glasgow coma score" & name == "Verbal",
    "gcs_verbal",
    label == "Glasgow coma score" & name == "Motor",
    "gcs_motor",
    default = NA_character_
  )
}

v2_cg_eicu_bounds <- function(target) {
  required <- c(
    "patientunitstayid", "index_time", "landmark_time",
    "covariate_window_start", "covariate_window_end"
  )
  v2_pm_require_columns(target, required, "eICU complete-GCS target")
  if (anyDuplicated(target$patientunitstayid)) {
    stop("eICU complete-GCS target contains duplicate stays.")
  }
  out <- data.table::data.table(
    patientunitstayid = as.character(target$patientunitstayid),
    index_time_value =
      v2_pm_time_numeric(target$index_time, "eICU index time"),
    landmark_time_value =
      v2_pm_time_numeric(target$landmark_time, "eICU landmark time"),
    window_start_value = v2_pm_time_numeric(
      target$covariate_window_start, "eICU GCS window start"
    ),
    window_end_value = v2_pm_time_numeric(
      target$covariate_window_end, "eICU GCS window end"
    )
  )
  expected_start <- pmax(0, out$index_time_value - 24 * 60)
  if (any(abs(out$window_start_value - expected_start) > 1e-8) ||
      any(abs(
        out$window_end_value - out$landmark_time_value
      ) > 1e-8) ||
      any(abs(
        out$landmark_time_value - out$index_time_value - 6 * 60
      ) > 1e-8)) {
    stop("eICU complete-GCS fixed-6h window contract failed.")
  }
  out
}

v2_cg_derive_eicu <- function(raw, target) {
  required <- c(
    "patientunitstayid", "nursingchartoffset",
    "nursingchartentryoffset", "nursingchartcelltypevallabel",
    "nursingchartcelltypevalname", "nursingchartvalue"
  )
  v2_pm_require_columns(raw, required, "eICU complete-GCS candidate cache")
  x <- data.table::as.data.table(raw)
  x[, patientunitstayid := as.character(patientunitstayid)]
  x[, mapping := v2_cg_eicu_mapping(
    as.character(nursingchartcelltypevallabel),
    as.character(nursingchartcelltypevalname)
  )]
  if (anyNA(x$mapping)) {
    stop("Unexpected label/name pair entered the eICU complete-GCS cache.")
  }
  x[, measurement_time := v2_cg_strict_numeric(nursingchartoffset)]
  if (anyNA(x$measurement_time)) {
    stop("eICU GCS candidate has invalid measurement offset.")
  }
  x[, entry_time := v2_cg_strict_numeric(nursingchartentryoffset)]
  x[, available_time := data.table::fifelse(
    is.na(entry_time),
    measurement_time,
    pmax(measurement_time, entry_time)
  )]
  x[, value_numeric := v2_cg_strict_numeric(nursingchartvalue)]
  bounds <- v2_cg_eicu_bounds(target)
  if (any(!x$patientunitstayid %in% bounds$patientunitstayid)) {
    stop("eICU complete-GCS cache contains an off-target stay.")
  }
  z <- merge(
    x, bounds, by = "patientunitstayid", all = FALSE, sort = FALSE
  )
  if (nrow(z) != nrow(x)) {
    stop("eICU complete-GCS candidates did not map one-to-one to targets.")
  }
  z[, measurement_in_window :=
    measurement_time >= window_start_value &
      measurement_time <= window_end_value]
  z[, available_by_landmark :=
    !is.na(available_time) & available_time <= window_end_value]
  z <- z[measurement_in_window & available_by_landmark]

  explicit <- z[mapping == "gcs_total"]
  explicit[, valid_value :=
    !is.na(value_numeric) &
      abs(value_numeric - round(value_numeric)) < 1e-10 &
      value_numeric >= 3 & value_numeric <= 15]
  explicit_valid <- explicit[valid_value == TRUE]
  explicit_reduced <- explicit_valid[, {
    values <- sort(unique(value_numeric))
    list(
      value_conflict = length(values) > 1L,
      gcs = if (length(values) == 1L) values[[1L]] else NA_real_,
      gcs_available_time_value =
        if (length(values) == 1L) min(available_time) else NA_real_,
      source_label = paste(
        sort(unique(nursingchartcelltypevallabel)), collapse = ";"
      )
    )
  }, by = .(
    patientunitstayid,
    gcs_measurement_time_value = measurement_time
  )]
  explicit_conflicts <- sum(explicit_reduced$value_conflict)
  explicit_reduced <- explicit_reduced[
    value_conflict == FALSE & !is.na(gcs)
  ]
  explicit_reduced[, `:=`(
    gcs_source = paste0("explicit_total:", source_label),
    source_priority = 1L
  )]

  ranges <- data.table::data.table(
    mapping = c("gcs_eye", "gcs_verbal", "gcs_motor"),
    upper = c(4, 5, 6)
  )
  components <- ranges[z, on = "mapping", nomatch = 0L]
  components[, valid_value :=
    !is.na(value_numeric) &
      abs(value_numeric - round(value_numeric)) < 1e-10 &
      value_numeric >= 1 & value_numeric <= upper]
  component_valid <- components[valid_value == TRUE]
  component_reduced <- component_valid[, {
    values <- sort(unique(value_numeric))
    list(
      value_conflict = length(values) > 1L,
      component_value =
        if (length(values) == 1L) values[[1L]] else NA_real_,
      component_available_time =
        if (length(values) == 1L) min(available_time) else NA_real_
    )
  }, by = .(
    patientunitstayid,
    gcs_measurement_time_value = measurement_time,
    mapping
  )]
  component_conflicts <- sum(component_reduced$value_conflict)
  component_reduced <- component_reduced[
    value_conflict == FALSE & !is.na(component_value)
  ]
  wide <- data.table::dcast(
    component_reduced,
    patientunitstayid + gcs_measurement_time_value ~ mapping,
    value.var = c("component_value", "component_available_time")
  )
  needed <- c(
    paste0(
      "component_value_",
      c("gcs_eye", "gcs_verbal", "gcs_motor")
    ),
    paste0(
      "component_available_time_",
      c("gcs_eye", "gcs_verbal", "gcs_motor")
    )
  )
  for (column in needed) {
    if (!column %in% names(wide)) wide[, (column) := NA_real_]
  }
  reconstructed <- wide[
    !is.na(component_value_gcs_eye) &
      !is.na(component_value_gcs_verbal) &
      !is.na(component_value_gcs_motor)
  ]
  reconstructed[, gcs := component_value_gcs_eye +
    component_value_gcs_verbal +
    component_value_gcs_motor]
  reconstructed[, gcs_available_time_value := pmax(
    component_available_time_gcs_eye,
    component_available_time_gcs_verbal,
    component_available_time_gcs_motor
  )]
  reconstructed[, `:=`(
    gcs_source = "same_time_eye_verbal_motor_reconstruction",
    source_priority = 2L
  )]

  candidates <- data.table::rbindlist(
    list(
      explicit_reduced[, .(
        patientunitstayid,
        gcs_measurement_time_value,
        gcs_available_time_value,
        gcs,
        gcs_source,
        source_priority,
        gcs_eye = NA_real_,
        gcs_verbal = NA_real_,
        gcs_motor = NA_real_
      )],
      reconstructed[, .(
        patientunitstayid,
        gcs_measurement_time_value,
        gcs_available_time_value,
        gcs,
        gcs_source,
        source_priority,
        gcs_eye = component_value_gcs_eye,
        gcs_verbal = component_value_gcs_verbal,
        gcs_motor = component_value_gcs_motor
      )]
    ),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setorder(
    candidates,
    patientunitstayid,
    source_priority,
    gcs,
    gcs_measurement_time_value,
    gcs_available_time_value
  )
  selected <- candidates[, .SD[1L], by = patientunitstayid]
  selected[, source_priority := NULL]
  if (nrow(selected) &&
      (any(selected$gcs < 3 | selected$gcs > 15) ||
       any(selected$gcs_available_time_value >
             bounds$window_end_value[
               match(selected$patientunitstayid, bounds$patientunitstayid)
             ]))) {
    stop("Selected eICU GCS range/timing validation failed.")
  }
  list(
    selected = selected,
    parsed = x,
    timing_qc = data.frame(
      database = "eICU-CRD",
      target_n = nrow(bounds),
      raw_candidate_rows = nrow(x),
      target_candidate_stays =
        data.table::uniqueN(x$patientunitstayid),
      measurement_in_window_rows = sum(
        x$measurement_time >=
          bounds$window_start_value[
            match(x$patientunitstayid, bounds$patientunitstayid)
          ] &
          x$measurement_time <=
            bounds$window_end_value[
              match(x$patientunitstayid, bounds$patientunitstayid)
            ]
      ),
      available_by_landmark_rows = nrow(z),
      valid_component_rows = nrow(component_valid),
      storetime_missing_rows = sum(is.na(x$entry_time)),
      airway_unscorable_rows = 0L,
      text_internal_conflict_rows = 0L,
      value_text_conflict_rows = 0L,
      duplicate_component_time_conflict_groups =
        component_conflicts + explicit_conflicts,
      complete_same_time_candidates =
        nrow(reconstructed) + nrow(explicit_reduced),
      selected_patients = nrow(selected),
      selected_explicit_total =
        sum(grepl("^explicit_total:", selected$gcs_source)),
      selected_reconstructed = sum(
        selected$gcs_source ==
          "same_time_eye_verbal_motor_reconstruction"
      ),
      stringsAsFactors = FALSE
    )
  )
}

v2_cg_join_complete_frame <- function(
    joined,
    selected,
    database = c("MIMIC-IV", "eICU-CRD")) {
  database <- match.arg(database)
  id <- if (database == "MIMIC-IV") "stay_id" else "patientunitstayid"
  selected <- as.data.frame(selected, stringsAsFactors = FALSE)
  v2_pm_require_columns(
    selected,
    c(
      id, "gcs", "gcs_measurement_time_value",
      "gcs_available_time_value", "gcs_source"
    ),
    paste(database, "selected GCS")
  )
  selected_id <- as.character(selected[[id]])
  if (anyNA(selected_id) || any(!nzchar(selected_id)) ||
      anyDuplicated(selected_id)) {
    stop(database, " selected GCS IDs are invalid.")
  }
  position <- match(joined$analysis_id, selected_id)
  augmented <- as.data.frame(joined, stringsAsFactors = FALSE)
  for (column in c(
    "gcs", "gcs_measurement_time_value", "gcs_available_time_value",
    "gcs_eye", "gcs_verbal", "gcs_motor", "gcs_source"
  )) {
    if (!column %in% names(selected)) selected[[column]] <- NA
    augmented[[column]] <- selected[[column]][position]
  }
  retained <- as.logical(augmented$core_complete) & !is.na(augmented$gcs)
  out <- augmented[retained, , drop = FALSE]
  rownames(out) <- NULL
  if (nrow(out) < 2L || any(out$gcs < 3 | out$gcs > 15) ||
      any(out$gcs_measurement_time_value <
            out$covariate_window_start_value) ||
      any(out$gcs_measurement_time_value >
            out$covariate_window_end_value) ||
      any(out$gcs_available_time_value >
            out$covariate_window_end_value)) {
    stop(database, " complete-GCS frame timing/range validation failed.")
  }
  out$core_complete <- TRUE
  v2_pm_validate_predictor_frame(out, database, require_complete = TRUE)
  attr(out, "complete_gcs_selection") <- list(
    all_tuple_n = nrow(joined),
    selected_gcs_n = nrow(selected),
    no_gcs_core_complete_n = sum(joined$core_complete),
    complete_gcs_common_n = nrow(out),
    source_harmonization =
      "recorded source-specific GCS total; measurement not identical"
  )
  out
}

v2_cg_baseline_continuous_variables <- c(
  "age", "pf_ratio", "gcs", "map", "platelet", "creatinine"
)

v2_cg_derive_transform_bundle <- function(frame) {
  required <- c(
    v2_cg_baseline_continuous_variables,
    "smp", "four_dprr", "driving_pressure", "rr"
  )
  v2_require_columns(frame, required, "complete-GCS MIMIC frame")
  if (any(!vapply(frame[required], is.numeric, logical(1L))) ||
      any(!stats::complete.cases(frame[required]))) {
    stop("Complete finite numeric values are required for GCS transforms.")
  }
  list(
    baseline_three_knots = setNames(lapply(
      v2_cg_baseline_continuous_variables,
      function(variable) {
        v2_quantile_knots(
          frame[[variable]], c(0.10, 0.50, 0.90),
          variable, type = 2L
        )
      }
    ), v2_cg_baseline_continuous_variables),
    nonlinear_four_knots = list(
      smp = v2_quantile_knots(
        frame$smp, c(0.05, 0.35, 0.65, 0.95), "smp", type = 2L
      ),
      four_dprr = v2_quantile_knots(
        frame$four_dprr, c(0.05, 0.35, 0.65, 0.95),
        "four_dprr", type = 2L
      ),
      driving_pressure = v2_quantile_knots(
        frame$driving_pressure, c(0.05, 0.35, 0.65, 0.95),
        "driving_pressure", type = 2L
      ),
      rr = v2_quantile_knots(
        frame$rr, c(0.05, 0.35, 0.65, 0.95), "rr", type = 2L
      )
    ),
    quantile_type = 2L,
    derivation_database = "MIMIC-IV",
    complete_gcs_core = TRUE
  )
}

v2_cg_build_baseline_design <- function(frame, bundle) {
  required <- c(
    "age", "sex_female", "pf_ratio", "gcs", "map", "vasopressor",
    "platelet", "creatinine"
  )
  v2_require_columns(frame, required, "complete-GCS model frame")
  if (!all(v2_cg_baseline_continuous_variables %in%
           names(bundle$baseline_three_knots))) {
    stop("Malformed complete-GCS transformation bundle.")
  }
  design <- cbind(
    v2_natural_spline_basis(
      frame$age, bundle$baseline_three_knots$age, "age"
    ),
    v2_named_column(frame$sex_female, "sex_female"),
    v2_natural_spline_basis(
      frame$pf_ratio, bundle$baseline_three_knots$pf_ratio, "pf_ratio"
    ),
    v2_natural_spline_basis(
      frame$gcs, bundle$baseline_three_knots$gcs, "gcs"
    ),
    v2_natural_spline_basis(
      frame$map, bundle$baseline_three_knots$map, "map"
    ),
    v2_named_column(frame$vasopressor, "vasopressor"),
    v2_natural_spline_basis(
      frame$platelet, bundle$baseline_three_knots$platelet, "platelet"
    ),
    v2_natural_spline_basis(
      frame$creatinine, bundle$baseline_three_knots$creatinine, "creatinine"
    )
  )
  storage.mode(design) <- "double"
  if (anyNA(design) || any(!is.finite(design)) ||
      is.null(colnames(design)) || anyDuplicated(colnames(design))) {
    stop("Invalid complete-GCS baseline design.")
  }
  design
}

v2_cg_build_design <- function(frame, model_id, bundle) {
  if (!model_id %in% v2_model_specification()$model_id) {
    stop("Unknown complete-GCS model ID: ", model_id)
  }
  baseline <- v2_cg_build_baseline_design(frame, bundle)
  added <- switch(
    model_id,
    M0 = NULL,
    M_MP = v2_named_column(frame$smp, "smp"),
    M_4DPRR = v2_named_column(frame$four_dprr, "four_dprr"),
    M_DPRR = cbind(
      v2_named_column(frame$driving_pressure, "driving_pressure"),
      v2_named_column(frame$rr, "rr")
    ),
    M_ENERGY = cbind(
      v2_named_column(frame$static_power, "static_power"),
      v2_named_column(frame$dynamic_power, "dynamic_power"),
      v2_named_column(frame$resistive_power, "resistive_power")
    )
  )
  design <- if (is.null(added)) baseline else cbind(baseline, added)
  if (anyNA(design) || any(!is.finite(design)) ||
      is.null(colnames(design)) || anyDuplicated(colnames(design))) {
    stop("Invalid complete-GCS design for ", model_id, ".")
  }
  design
}
