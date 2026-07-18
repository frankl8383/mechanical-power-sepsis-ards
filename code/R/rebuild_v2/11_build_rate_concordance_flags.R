#!/usr/bin/env Rscript

# rebuild_v2 Phase 2C: outcome-blind primary-tuple source and rate quality.
#
# This script does not reselect a ventilator tuple.  It annotates each already
# selected fixed-landmark primary tuple with:
#   1. preferred observed/exhaled VT plus total measured RR;
#   2. availability of set and total RR close to the same plateau anchor; and
#   3. concordance of those paired rates within the locked absolute margin.
#
# No mortality or discharge artifact is opened.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/11_build_rate_concordance_flags.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required.")
}
sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

pair_window_minutes <- 15
maximum_rate_difference <- as.numeric(
  LOCKED_V2$measurement_quality$
    rate_concordance_absolute_difference_per_min
)
if (!identical(pair_window_minutes, 15) ||
    !identical(maximum_rate_difference, 2)) {
  stop("Rate-quality lock differs from the post-review SAP.")
}

private_out <- file.path(PRIVATE_ROOT, "construct_quality")
aggregate_out <- file.path(AGGREGATE_ROOT, "construct_quality")
qc_out <- file.path(QC_ROOT, "construct_quality")
for (directory in c(private_out, aggregate_out, qc_out)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}
completion_gate <- file.path(
  qc_out, "primary_tuple_rate_quality_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

input_paths <- list(
  mimic_tuple_target = file.path(
    PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds"
  ),
  eicu_tuple_target = file.path(
    PRIVATE_ROOT, "eicu", "eicu_no_gcs_core_targets_v2.rds"
  ),
  mimic_primary_exposure = file.path(
    PRIVATE_ROOT, "mimic", "mimic_paired_exposure_primary_60min_v2.rds"
  ),
  eicu_primary_exposure = file.path(
    PRIVATE_ROOT, "eicu", "eicu_paired_exposure_primary_60min_v2.rds"
  ),
  mimic_numeric_cache = file.path(
    PRIVATE_ROOT, "mimic", "cache_v2",
    "selected_paired_exposure_chartevents_v2.rds"
  ),
  eicu_rate_cache = file.path(
    PRIVATE_ROOT, "eicu", "cache_v2", "construct_quality",
    "eicu_set_total_rr_candidates_v2.csv.gz"
  ),
  eicu_rate_filter_gate = file.path(
    qc_out, "eicu_rate_filter_complete_v2.csv"
  ),
  eicu_rate_filter_manifest = file.path(
    qc_out, "eicu_rate_filter_manifest_v2.csv"
  ),
  eicu_rate_filter_helper = file.path(
    script_dir, "11a_filter_eicu_rate_inputs_v2.py"
  ),
  eicu_rate_target_ids = file.path(
    PRIVATE_ROOT, "eicu", "cache_v2", "construct_quality",
    "tuple_target_ids_v2.txt"
  ),
  fixed_landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  mimic_exposure_gate = file.path(
    QC_ROOT, "mimic_exposure",
    "phase2_mimic_exposure_complete_v2.csv"
  ),
  eicu_exposure_gate = file.path(
    QC_ROOT, "eicu_exposure",
    "phase2_eicu_exposure_complete_v2.csv"
  )
)
missing_input <- names(input_paths)[!file.exists(unlist(input_paths))]
if (length(missing_input)) {
  stop("Missing input(s): ", paste(missing_input, collapse = ", "))
}

forbidden_pattern <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
assert_outcome_blind <- function(x, label) {
  bad <- names(x)[grepl(forbidden_pattern, names(x), ignore.case = TRUE)]
  if (length(bad)) {
    stop(label, " contains outcome-like field(s): ", paste(bad, collapse = ", "))
  }
  invisible(TRUE)
}

read_field_gate <- function(path) {
  gate <- fread(path, showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field)) {
    stop("Malformed field/value gate: ", path)
  }
  setNames(as.character(gate$value), gate$field)
}
landmark_gate <- read_field_gate(input_paths$fixed_landmark_gate)
if (!identical(
      landmark_gate[["locked_config_version"]], LOCKED_V2$version
    ) ||
    !identical(landmark_gate[["all_energy_identities_pass"]], "TRUE") ||
    !identical(
      landmark_gate[["mimic_target_sha256"]],
      sha256_file(input_paths$mimic_tuple_target)
    ) ||
    !identical(
      landmark_gate[["eicu_target_sha256"]],
      sha256_file(input_paths$eicu_tuple_target)
    )) {
  stop("Fixed-landmark gate does not match the tuple targets.")
}
for (database in c("mimic", "eicu")) {
  gate <- read_field_gate(input_paths[[paste0(database, "_exposure_gate")]])
  if (!identical(gate[["locked_config_version"]], LOCKED_V2$version) ||
      !identical(gate[["all_invariants_pass"]], "TRUE") ||
      !identical(gate[["outcome_leakage_guard_pass"]], "TRUE")) {
    stop(database, " exposure gate is not a locked PASS.")
  }
  expected <- sha256_file(
    input_paths[[paste0(database, "_primary_exposure")]]
  )
  if (!identical(gate[["primary_60min_rds_sha256"]], expected)) {
    stop(database, " primary exposure hash does not match its gate.")
  }
}
eicu_filter_gate <- fread(
  input_paths$eicu_rate_filter_gate, showProgress = FALSE
)
if (nrow(eicu_filter_gate) != 1L ||
    eicu_filter_gate$status[[1L]] != "PASS" ||
    eicu_filter_gate$reached_eof[[1L]] != TRUE ||
    eicu_filter_gate$helper_sha256[[1L]] !=
      sha256_file(input_paths$eicu_rate_filter_helper) ||
    eicu_filter_gate$manifest_sha256[[1L]] !=
      sha256_file(input_paths$eicu_rate_filter_manifest) ||
    eicu_filter_gate$output_sha256[[1L]] !=
      sha256_file(input_paths$eicu_rate_cache)) {
  stop("eICU rate filter gate is not a complete PASS.")
}
eicu_filter_manifest <- fread(
  input_paths$eicu_rate_filter_manifest, showProgress = FALSE
)
if (nrow(eicu_filter_manifest) != 2L ||
    any(eicu_filter_manifest$status != "PASS") ||
    any(eicu_filter_manifest$reached_eof != TRUE) ||
    uniqueN(eicu_filter_manifest$source_path) != 1L ||
    uniqueN(eicu_filter_manifest$target_id_path) != 1L ||
    uniqueN(eicu_filter_manifest$target_id_sha256) != 1L ||
    uniqueN(eicu_filter_manifest$output_sha256) != 1L ||
    !setequal(
      eicu_filter_manifest$label, c("Vent Rate", "Total RR")
    ) ||
    !identical(
      normalizePath(
        eicu_filter_manifest$target_id_path[[1L]], mustWork = TRUE
      ),
      normalizePath(input_paths$eicu_rate_target_ids, mustWork = TRUE)
    ) ||
    eicu_filter_manifest$target_id_sha256[[1L]] !=
      sha256_file(input_paths$eicu_rate_target_ids) ||
    eicu_filter_manifest$output_sha256[[1L]] !=
      sha256_file(input_paths$eicu_rate_cache)) {
  stop("eICU rate-filter manifest provenance is inconsistent.")
}

collapse_candidates <- function(
    candidates,
    id,
    source,
    measurement_time,
    available_time,
    value,
    warning = NULL) {
  required <- c(id, source, measurement_time, available_time, value)
  missing <- setdiff(required, names(candidates))
  if (length(missing)) {
    stop("Rate candidate table lacks: ", paste(missing, collapse = ", "))
  }
  x <- copy(candidates)
  x[, rate_value_internal := suppressWarnings(as.numeric(get(value)))]
  x[, measurement_internal := get(measurement_time)]
  x[, available_internal := get(available_time)]
  x <- x[
    !is.na(rate_value_internal) & is.finite(rate_value_internal) &
      rate_value_internal >= 5 & rate_value_internal <= 60 &
      !is.na(measurement_internal) & !is.na(available_internal)
  ]
  if (!nrow(x)) {
    return(data.table())
  }
  warning_field <- if (!is.null(warning) && warning %in% names(x)) {
    warning
  } else {
    NULL
  }
  out <- x[, .(
    rate_value = median(rate_value_internal),
    available_time = max(available_internal),
    duplicate_rows = .N,
    duplicate_value_conflict = uniqueN(rate_value_internal) > 1L,
    warning_any = if (!is.null(warning_field)) {
      any(as.integer(get(warning_field)) == 1L, na.rm = TRUE)
    } else {
      FALSE
    }
  ), by = c(id, source, measurement_time)]
  setnames(out, measurement_time, "measurement_time")
  out
}

select_nearest_rate <- function(
    tuple,
    candidates,
    id,
    anchor = "anchor_time",
    index = "index_time",
    landmark = "landmark_time",
    source_value,
    prefix,
    time_scale_minutes = 1) {
  if (!nrow(candidates)) {
    out <- tuple[, c(id), with = FALSE]
    for (name in c(
      "value", "time", "available_time", "anchor_gap_minutes",
      "duplicate_rows", "duplicate_value_conflict", "warning_any"
    )) {
      out[, (paste0(prefix, "_", name)) := NA]
    }
    return(out)
  }
  source_candidates <- candidates[get("rate_source") == source_value]
  joined <- merge(
    source_candidates,
    tuple[, c(id, anchor, index, landmark), with = FALSE],
    by = id, all = FALSE, sort = FALSE
  )
  if (inherits(joined$measurement_time, "POSIXt")) {
    joined[, signed_anchor_gap_minutes := as.numeric(
      difftime(measurement_time, get(anchor), units = "mins")
    )]
  } else {
    joined[, signed_anchor_gap_minutes :=
      as.numeric(measurement_time - get(anchor)) * time_scale_minutes]
  }
  joined[, absolute_anchor_gap_minutes := abs(signed_anchor_gap_minutes)]
  joined <- joined[
    absolute_anchor_gap_minutes <= pair_window_minutes &
      measurement_time >= get(index) &
      available_time <= get(landmark)
  ]
  if (nrow(joined)) {
    joined[, future_tie := signed_anchor_gap_minutes > 0]
    setorderv(
      joined,
      c(
        id, "absolute_anchor_gap_minutes", "future_tie",
        "measurement_time", "available_time"
      ),
      c(1, 1, 1, 1, 1)
    )
    selected <- joined[, .SD[1L], by = id]
    selected <- selected[, .(
      source_id = get(id),
      value = rate_value,
      time = measurement_time,
      available_time,
      anchor_gap_minutes = signed_anchor_gap_minutes,
      duplicate_rows,
      duplicate_value_conflict,
      warning_any
    )]
    setnames(selected, "source_id", id)
  } else {
    selected <- tuple[0, c(id), with = FALSE]
  }
  out <- merge(
    tuple[, c(id), with = FALSE],
    selected, by = id, all.x = TRUE, sort = FALSE
  )
  setnames(
    out,
    setdiff(names(out), id),
    paste0(prefix, "_", setdiff(names(out), id))
  )
  out
}

prepare_tuple <- function(database) {
  mimic <- identical(database, "MIMIC-IV")
  target <- as.data.table(readRDS(
    input_paths[[if (mimic) "mimic_tuple_target" else "eicu_tuple_target"]]
  ))
  exposure <- as.data.table(readRDS(
    input_paths[[
      if (mimic) "mimic_primary_exposure" else "eicu_primary_exposure"
    ]]
  ))
  assert_outcome_blind(target, paste(database, "tuple target"))
  assert_outcome_blind(exposure, paste(database, "primary exposure"))
  id <- if (mimic) "stay_id" else "patientunitstayid"
  if (anyDuplicated(target[[id]]) || anyNA(target[[id]]) ||
      anyDuplicated(exposure[[id]]) || anyNA(exposure[[id]])) {
    stop(database, " tuple/exposure IDs must be complete and unique.")
  }
  exposure <- exposure[get(id) %in% target[[id]]]
  if (nrow(exposure) != nrow(target) ||
      !setequal(exposure[[id]], target[[id]]) ||
      any(exposure$tuple_observed != TRUE)) {
    stop(database, " fixed-landmark tuple target does not match exposure rows.")
  }
  setorderv(exposure, id)
  setorderv(target, id)
  numeric_identity <- c(
    pplat = "pplat",
    ppeak_value = "ppeak_value",
    peep_value = "peep_value",
    vt_value = "vt_value",
    rr_value = "rr_value",
    smp = "smp"
  )
  for (name in names(numeric_identity)) {
    error <- abs(
      as.numeric(exposure[[name]]) -
        as.numeric(target[[numeric_identity[[name]]]])
    )
    if (anyNA(error) || max(error) > 1e-10) {
      stop(database, " primary tuple changed for field ", name, ".")
    }
  }
  if (mimic) {
    target_available <- as.POSIXct(
      target$ventilator_tuple_available_time, tz = "UTC"
    )
    target_index <- as.POSIXct(target$index_time, tz = "UTC")
    target_landmark <- as.POSIXct(target$landmark_time, tz = "UTC")
  } else {
    target_available <- as.numeric(
      target$ventilator_tuple_available_time
    )
    target_index <- as.numeric(target$index_time)
    target_landmark <- as.numeric(target$landmark_time)
  }
  if (any(exposure$prediction_time != target_available) ||
      any(exposure$anchor_time < target_index) ||
      any(exposure$anchor_time > exposure$prediction_time) ||
      any(exposure$prediction_time > target_landmark)) {
    stop(database, " tuple timing contract is inconsistent.")
  }
  if (mimic) {
    exposure[, landmark_time := as.POSIXct(
      target$landmark_time, tz = "UTC"
    )]
  } else {
    exposure[, landmark_time := as.numeric(target$landmark_time)]
  }
  exposure
}

build_mimic <- function() {
  tuple <- prepare_tuple("MIMIC-IV")
  cache <- as.data.table(readRDS(input_paths$mimic_numeric_cache))
  assert_outcome_blind(cache, "MIMIC numeric ventilator cache")
  cache <- cache[
    stay_id %in% tuple$stay_id &
      itemid %in% c(224688L, 224690L)
  ]
  cache[, `:=`(
    rate_source = fifelse(
      itemid == 224688L, "set_rate", "total_rate"
    ),
    available_time_internal = charttime
  )]
  cache[
    !is.na(storetime),
    available_time_internal := pmax(charttime, storetime)
  ]
  collapsed <- collapse_candidates(
    cache,
    id = "stay_id",
    source = "rate_source",
    measurement_time = "charttime",
    available_time = "available_time_internal",
    value = "valuenum",
    warning = "warning"
  )
  set_rate <- select_nearest_rate(
    tuple, collapsed, id = "stay_id",
    source_value = "set_rate", prefix = "set_rr"
  )
  total_rate <- select_nearest_rate(
    tuple, collapsed, id = "stay_id",
    source_value = "total_rate", prefix = "total_rr"
  )
  out <- merge(tuple, set_rate, by = "stay_id", all = FALSE, sort = FALSE)
  out <- merge(out, total_rate, by = "stay_id", all = FALSE, sort = FALSE)
  out[, preferred_source_primary_tuple :=
    vt_source == "Tidal Volume (observed)" &
      rr_source == "Respiratory Rate (Total)"]
  out[, paired_set_total_available :=
    !is.na(set_rr_value) & !is.na(total_rr_value)]
  out[, set_total_time_gap_minutes := as.numeric(
    difftime(set_rr_time, total_rr_time, units = "mins")
  )]
  out[, set_total_absolute_time_gap_minutes :=
    abs(set_total_time_gap_minutes)]
  out[, set_total_difference_per_min := total_rr_value - set_rr_value]
  out[, set_total_absolute_difference_per_min :=
    abs(set_total_difference_per_min)]
  out[, rate_pair_within_15_minutes :=
    paired_set_total_available &
      set_total_absolute_time_gap_minutes <= pair_window_minutes]
  out[, rate_concordant :=
    rate_pair_within_15_minutes &
      set_total_absolute_difference_per_min <= maximum_rate_difference]
  out[, rate_concordant_preferred_source :=
    preferred_source_primary_tuple & rate_concordant]
  out[, selected_total_rr_reproduced :=
    fifelse(
      rr_source == "Respiratory Rate (Total)" &
        !is.na(total_rr_value),
      abs(rr_value - total_rr_value) <= 1e-10,
      NA
    )]
  out
}

build_eicu <- function() {
  tuple <- prepare_tuple("eICU-CRD")
  cache <- fread(
    cmd = sprintf(
      "gzip -cd %s", shQuote(input_paths$eicu_rate_cache)
    ),
    showProgress = FALSE
  )
  assert_outcome_blind(cache, "eICU rate cache")
  cache[, `:=`(
    rate_source = fifelse(
      respchartvaluelabel == "Vent Rate", "set_rate", "total_rate"
    ),
    available_time_internal = fifelse(
      is.na(respchartentryoffset),
      as.numeric(respchartoffset),
      pmax(
        as.numeric(respchartoffset),
        as.numeric(respchartentryoffset)
      )
    )
  )]
  collapsed <- collapse_candidates(
    cache,
    id = "patientunitstayid",
    source = "rate_source",
    measurement_time = "respchartoffset",
    available_time = "available_time_internal",
    value = "respchartvalue"
  )
  set_rate <- select_nearest_rate(
    tuple, collapsed, id = "patientunitstayid",
    source_value = "set_rate", prefix = "set_rr"
  )
  total_rate <- select_nearest_rate(
    tuple, collapsed, id = "patientunitstayid",
    source_value = "total_rate", prefix = "total_rr"
  )
  out <- merge(
    tuple, set_rate, by = "patientunitstayid",
    all = FALSE, sort = FALSE
  )
  out <- merge(
    out, total_rate, by = "patientunitstayid",
    all = FALSE, sort = FALSE
  )
  out[, preferred_source_primary_tuple :=
    vt_source %chin% c(
      "Exhaled TV (machine)", "Exhaled TV (patient)"
    ) & rr_source == "Total RR"]
  out[, paired_set_total_available :=
    !is.na(set_rr_value) & !is.na(total_rr_value)]
  out[, set_total_time_gap_minutes := set_rr_time - total_rr_time]
  out[, set_total_absolute_time_gap_minutes :=
    abs(set_total_time_gap_minutes)]
  out[, set_total_difference_per_min := total_rr_value - set_rr_value]
  out[, set_total_absolute_difference_per_min :=
    abs(set_total_difference_per_min)]
  out[, rate_pair_within_15_minutes :=
    paired_set_total_available &
      set_total_absolute_time_gap_minutes <= pair_window_minutes]
  out[, rate_concordant :=
    rate_pair_within_15_minutes &
      set_total_absolute_difference_per_min <= maximum_rate_difference]
  out[, rate_concordant_preferred_source :=
    preferred_source_primary_tuple & rate_concordant]
  out[, selected_total_rr_reproduced :=
    fifelse(
      rr_source == "Total RR" & !is.na(total_rr_value),
      abs(rr_value - total_rr_value) <= 1e-10,
      NA
    )]
  out
}

mimic <- build_mimic()
eicu <- build_eicu()
assert_outcome_blind(mimic, "MIMIC construct-quality flags")
assert_outcome_blind(eicu, "eICU construct-quality flags")

invariant_rows <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    check = c(
      "unique_ids", "preferred_flag_complete",
      "pair_flag_complete", "concordance_flag_complete",
      "selected_total_rr_reproduced",
      "selected_rates_not_before_index",
      "anchor_prediction_landmark_order"
    ),
    pass = c(
      !anyDuplicated(mimic$stay_id),
      !anyNA(mimic$preferred_source_primary_tuple),
      !anyNA(mimic$paired_set_total_available),
      !anyNA(mimic$rate_concordant),
      all(mimic$selected_total_rr_reproduced %in% c(TRUE, NA)),
      all(is.na(mimic$set_rr_time) |
            mimic$set_rr_time >= mimic$index_time) &&
        all(is.na(mimic$total_rr_time) |
              mimic$total_rr_time >= mimic$index_time),
      all(mimic$anchor_time >= mimic$index_time) &&
        all(mimic$anchor_time <= mimic$prediction_time) &&
        all(mimic$prediction_time <= mimic$landmark_time)
    )
  ),
  data.table(
    database = "eICU-CRD",
    check = c(
      "unique_ids", "preferred_flag_complete",
      "pair_flag_complete", "concordance_flag_complete",
      "selected_total_rr_reproduced",
      "selected_rates_not_before_index",
      "anchor_prediction_landmark_order"
    ),
    pass = c(
      !anyDuplicated(eicu$patientunitstayid),
      !anyNA(eicu$preferred_source_primary_tuple),
      !anyNA(eicu$paired_set_total_available),
      !anyNA(eicu$rate_concordant),
      all(eicu$selected_total_rr_reproduced %in% c(TRUE, NA)),
      all(is.na(eicu$set_rr_time) |
            eicu$set_rr_time >= eicu$index_time) &&
        all(is.na(eicu$total_rr_time) |
              eicu$total_rr_time >= eicu$index_time),
      all(eicu$anchor_time >= eicu$index_time) &&
        all(eicu$anchor_time <= eicu$prediction_time) &&
        all(eicu$prediction_time <= eicu$landmark_time)
    )
  )
))
if (any(invariant_rows$pass != TRUE)) {
  stop(
    "Construct-quality invariant failure: ",
    paste(
      invariant_rows[pass != TRUE, paste(database, check, sep = "/")],
      collapse = ", "
    )
  )
}

keep_columns <- function(x, id, database) {
  x[, .(
    database = database,
    analysis_id = as.character(get(id)),
    anchor_time,
    landmark_time,
    primary_rr_value = rr_value,
    primary_rr_source = rr_source,
    primary_vt_source = vt_source,
    preferred_source_primary_tuple,
    set_rr_value,
    set_rr_time,
    set_rr_available_time,
    set_rr_anchor_gap_minutes,
    set_rr_duplicate_rows,
    set_rr_duplicate_value_conflict,
    set_rr_warning_any,
    total_rr_value,
    total_rr_time,
    total_rr_available_time,
    total_rr_anchor_gap_minutes,
    total_rr_duplicate_rows,
    total_rr_duplicate_value_conflict,
    total_rr_warning_any,
    paired_set_total_available,
    set_total_time_gap_minutes,
    set_total_absolute_time_gap_minutes,
    set_total_difference_per_min,
    set_total_absolute_difference_per_min,
    rate_pair_within_15_minutes,
    rate_concordant,
    rate_concordant_preferred_source,
    selected_total_rr_reproduced
  )]
}
mimic_flags <- keep_columns(mimic, "stay_id", "MIMIC-IV")
eicu_flags <- keep_columns(eicu, "patientunitstayid", "eICU-CRD")

metadata <- list(
  version = "primary_tuple_rate_concordance_v2",
  locked_config_version = LOCKED_V2$version,
  outcome_blind = TRUE,
  tuple_reselection = FALSE,
  anchor_window_minutes = pair_window_minutes,
  maximum_set_total_pair_gap_minutes = pair_window_minutes,
  maximum_absolute_rate_difference_per_min = maximum_rate_difference,
  interpretation = paste(
    "Rate concordance supports a closer match to the simplified formula's",
    "measurement assumptions but does not establish passive ventilation."
  ),
  input_hashes = lapply(input_paths, sha256_file)
)
attr(mimic_flags, "rebuild_metadata") <- c(
  metadata, list(database = "MIMIC-IV")
)
attr(eicu_flags, "rebuild_metadata") <- c(
  metadata, list(database = "eICU-CRD")
)

mimic_output <- file.path(
  private_out, "mimic_primary_tuple_rate_quality_flags_v2.rds"
)
eicu_output <- file.path(
  private_out, "eicu_primary_tuple_rate_quality_flags_v2.rds"
)
atomic_save_rds <- function(object, path) {
  temporary <- paste0(path, ".tmp")
  unlink(temporary, force = TRUE)
  saveRDS(object, temporary, compress = "xz")
  if (!file.rename(temporary, path)) {
    unlink(temporary, force = TRUE)
    stop("Atomic RDS rename failed: ", path)
  }
  invisible(path)
}
atomic_write_csv <- function(object, path) {
  temporary <- paste0(path, ".tmp")
  unlink(temporary, force = TRUE)
  fwrite(object, temporary)
  if (!file.rename(temporary, path)) {
    unlink(temporary, force = TRUE)
    stop("Atomic CSV rename failed: ", path)
  }
  invisible(path)
}
atomic_save_rds(mimic_flags, mimic_output)
atomic_save_rds(eicu_flags, eicu_output)

summary_table <- rbindlist(lapply(
  list(`MIMIC-IV` = mimic_flags, `eICU-CRD` = eicu_flags),
  function(x) {
    data.table(
      tuple_n = nrow(x),
      preferred_source_n = sum(x$preferred_source_primary_tuple),
      preferred_source_percent =
        100 * mean(x$preferred_source_primary_tuple),
      paired_set_total_n = sum(x$paired_set_total_available),
      paired_set_total_percent =
        100 * mean(x$paired_set_total_available),
      pair_within_15_min_n = sum(x$rate_pair_within_15_minutes),
      pair_within_15_min_percent =
        100 * mean(x$rate_pair_within_15_minutes),
      rate_concordant_n = sum(x$rate_concordant),
      rate_concordant_percent = 100 * mean(x$rate_concordant),
      rate_concordant_preferred_n =
        sum(x$rate_concordant_preferred_source),
      rate_concordant_preferred_percent =
        100 * mean(x$rate_concordant_preferred_source)
    )
  }
), idcol = "database")
atomic_write_csv(
  summary_table,
  file.path(aggregate_out, "primary_tuple_rate_quality_summary_v2.csv")
)

distribution_table <- rbindlist(lapply(
  list(`MIMIC-IV` = mimic_flags, `eICU-CRD` = eicu_flags),
  function(x) {
    rbindlist(lapply(
      c(
        "set_rr_anchor_gap_minutes",
        "total_rr_anchor_gap_minutes",
        "set_total_absolute_time_gap_minutes",
        "set_total_absolute_difference_per_min"
      ),
      function(variable) {
        value <- x[[variable]]
        value <- value[is.finite(value)]
        quantile_values <- if (length(value)) {
          as.numeric(quantile(
            value,
            probs = c(0, .05, .25, .5, .75, .95, 1),
            names = FALSE,
            type = 2
          ))
        } else {
          rep(NA_real_, 7L)
        }
        data.table(
          variable,
          n = length(value),
          minimum = quantile_values[1L],
          p05 = quantile_values[2L],
          p25 = quantile_values[3L],
          median = quantile_values[4L],
          p75 = quantile_values[5L],
          p95 = quantile_values[6L],
          maximum = quantile_values[7L]
        )
      }
    ))
  }
), idcol = "database")
atomic_write_csv(
  distribution_table,
  file.path(
    aggregate_out, "primary_tuple_rate_quality_distributions_v2.csv"
  )
)

atomic_write_csv(
  invariant_rows,
  file.path(qc_out, "primary_tuple_rate_quality_invariants_v2.csv")
)
input_manifest <- data.table(
  input_name = names(input_paths),
  path = normalizePath(unlist(input_paths), mustWork = TRUE),
  sha256 = vapply(input_paths, sha256_file, character(1L)),
  outcome_artifact = FALSE
)
atomic_write_csv(
  input_manifest,
  file.path(qc_out, "primary_tuple_rate_quality_input_manifest_v2.csv")
)

completion <- data.table(
  field = c(
    "status", "locked_config_version", "script_sha256",
    "mimic_flags_sha256", "eicu_flags_sha256",
    "outcome_artifacts_opened", "tuple_reselection",
    "pair_window_minutes", "maximum_rate_difference_per_min",
    "all_invariants_pass", "completed_at"
  ),
  value = c(
    "PASS", LOCKED_V2$version, sha256_file(script_path),
    sha256_file(mimic_output), sha256_file(eicu_output),
    "FALSE", "FALSE", pair_window_minutes, maximum_rate_difference,
    "TRUE", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
)
atomic_write_csv(
  completion,
  completion_gate
)

message("REBUILD_V2_PRIMARY_TUPLE_RATE_QUALITY_PASS")
print(summary_table)
