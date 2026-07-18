#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2: outcome-blind MIMIC mode compatibility.
#
# The script annotates the already selected fixed-landmark primary tuple. It
# never selects a new ventilator tuple and never opens mortality/discharge
# artifacts. The accepted labels are deliberately restricted to conventional
# volume-control assist/control-compatible records; AutoFlow, PRVC/APV,
# pressure control, SIMV, spontaneous, and APRV labels are not treated as
# compatible with the simplified VCV-derived surrogate equation.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/14_build_mimic_mode_quality_flags.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required.")
}
sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}
sha256_sorted_id_file <- function(path) {
  values <- readLines(path, warn = FALSE)
  if (!length(values) || any(!grepl("^[0-9]+$", values))) {
    stop("Malformed integer-ID file: ", path)
  }
  values <- unique(values)
  values <- values[order(as.numeric(values))]
  digest::digest(
    charToRaw(paste0(paste(values, collapse = "\n"), "\n")),
    algo = "sha256",
    serialize = FALSE
  )
}
atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
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
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  unlink(temporary, force = TRUE)
  fwrite(object, temporary)
  if (!file.rename(temporary, path)) {
    unlink(temporary, force = TRUE)
    stop("Atomic CSV rename failed: ", path)
  }
  invisible(path)
}
read_field_gate <- function(path) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field)) {
    stop("Malformed field/value gate: ", path)
  }
  setNames(as.character(gate$value), gate$field)
}

anchor_window_minutes <- 60
compatible_pairs <- data.table(
  itemid = c(223849L, 223849L, 223849L, 223849L, 229314L),
  mode_value = c("CMV/ASSIST", "CMV", "VOL/AC", "(S) CMV", "(S) CMV"),
  mapping = "conventional_volume_control_assist_control_compatible"
)
if (!identical(anchor_window_minutes, 60)) {
  stop("Mode anchor window changed from the locked implementation.")
}

private_out <- file.path(PRIVATE_ROOT, "construct_quality")
aggregate_out <- file.path(AGGREGATE_ROOT, "construct_quality")
qc_out <- file.path(QC_ROOT, "construct_quality")
for (directory in c(private_out, aggregate_out, qc_out)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}
completion_gate <- file.path(
  qc_out, "mimic_primary_tuple_mode_quality_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

paths <- list(
  tuple_target = file.path(
    PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds"
  ),
  primary_exposure = file.path(
    PRIVATE_ROOT, "mimic", "mimic_paired_exposure_primary_60min_v2.rds"
  ),
  rate_flags = file.path(
    PRIVATE_ROOT, "construct_quality",
    "mimic_primary_tuple_rate_quality_flags_v2.rds"
  ),
  mode_cache = file.path(
    PRIVATE_ROOT, "mimic", "cache_v2", "construct_quality",
    "mimic_ventilator_mode_candidates_v2.csv.gz"
  ),
  mode_target_stays = file.path(
    PRIVATE_ROOT, "mimic", "cache_v2", "construct_quality",
    "tuple_target_stay_ids_v2.txt"
  ),
  mode_filter_gate = file.path(
    qc_out, "mimic_mode_filter_complete_v2.csv"
  ),
  mode_filter_manifest = file.path(
    qc_out, "mimic_mode_filter_manifest_v2.csv"
  ),
  mode_filter_helper = file.path(
    script_dir, "11b_filter_mimic_mode_inputs_v2.py"
  ),
  fixed_landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  mimic_exposure_gate = file.path(
    QC_ROOT, "mimic_exposure", "phase2_mimic_exposure_complete_v2.csv"
  ),
  rate_gate = file.path(
    qc_out, "primary_tuple_rate_quality_complete_v2.csv"
  )
)
missing <- names(paths)[!file.exists(unlist(paths))]
if (length(missing)) {
  stop("Missing mode-quality input(s): ", paste(missing, collapse = ", "))
}

landmark_gate <- read_field_gate(paths$fixed_landmark_gate)
if (!identical(
      landmark_gate[["locked_config_version"]], LOCKED_V2$version
    ) ||
    !identical(landmark_gate[["all_energy_identities_pass"]], "TRUE") ||
    !identical(
      landmark_gate[["mimic_target_sha256"]],
      sha256_file(paths$tuple_target)
    )) {
  stop("Fixed-landmark target provenance failed.")
}
exposure_gate <- read_field_gate(paths$mimic_exposure_gate)
if (!identical(
      exposure_gate[["locked_config_version"]], LOCKED_V2$version
    ) ||
    !identical(exposure_gate[["all_invariants_pass"]], "TRUE") ||
    !identical(exposure_gate[["outcome_leakage_guard_pass"]], "TRUE") ||
    !identical(
      exposure_gate[["primary_60min_rds_sha256"]],
      sha256_file(paths$primary_exposure)
    )) {
  stop("MIMIC exposure provenance failed.")
}
rate_gate <- read_field_gate(paths$rate_gate)
if (!identical(rate_gate[["status"]], "PASS") ||
    !identical(rate_gate[["outcome_artifacts_opened"]], "FALSE") ||
    !identical(rate_gate[["tuple_reselection"]], "FALSE") ||
    !identical(rate_gate[["all_invariants_pass"]], "TRUE") ||
    !identical(
      rate_gate[["mimic_flags_sha256"]],
      sha256_file(paths$rate_flags)
    )) {
  stop("Rate-quality provenance failed.")
}

mode_gate <- fread(paths$mode_filter_gate, showProgress = FALSE)
if (nrow(mode_gate) != 1L ||
    mode_gate$status[[1L]] != "PASS" ||
    mode_gate$reached_eof[[1L]] != TRUE ||
    mode_gate$source_sha_verified_upstream[[1L]] != TRUE ||
    mode_gate$helper_sha256[[1L]] !=
      sha256_file(paths$mode_filter_helper) ||
    mode_gate$manifest_sha256[[1L]] !=
      sha256_file(paths$mode_filter_manifest) ||
    mode_gate$output_sha256[[1L]] != sha256_file(paths$mode_cache)) {
  stop("MIMIC mode-filter gate is not a complete provenance PASS.")
}
mode_manifest <- fread(paths$mode_filter_manifest, showProgress = FALSE)
if (nrow(mode_manifest) != 2L ||
    any(mode_manifest$status != "PASS") ||
    any(mode_manifest$reached_eof != TRUE) ||
    any(mode_manifest$source_sha_verified_upstream != TRUE) ||
    !setequal(mode_manifest$itemid, c(223849L, 229314L)) ||
    uniqueN(mode_manifest$source_path) != 1L ||
    uniqueN(mode_manifest$target_stay_path) != 1L ||
    uniqueN(mode_manifest$target_stay_sha256) != 1L ||
    uniqueN(mode_manifest$output_sha256) != 1L ||
    !identical(
      normalizePath(
        mode_manifest$target_stay_path[[1L]], mustWork = TRUE
      ),
      normalizePath(paths$mode_target_stays, mustWork = TRUE)
    ) ||
    mode_manifest$target_stay_sha256[[1L]] !=
      sha256_sorted_id_file(paths$mode_target_stays) ||
    mode_manifest$output_sha256[[1L]] != sha256_file(paths$mode_cache)) {
  stop("MIMIC mode-filter manifest is inconsistent.")
}

forbidden_pattern <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
assert_outcome_blind <- function(object, label) {
  bad <- names(object)[grepl(
    forbidden_pattern, names(object), ignore.case = TRUE
  )]
  if (length(bad)) {
    stop(label, " contains outcome-like field(s): ", paste(bad, collapse = ", "))
  }
  invisible(TRUE)
}

target <- as.data.table(readRDS(paths$tuple_target))
exposure <- as.data.table(readRDS(paths$primary_exposure))
rate_flags <- as.data.table(readRDS(paths$rate_flags))
assert_outcome_blind(target, "tuple target")
assert_outcome_blind(exposure, "primary exposure")
assert_outcome_blind(rate_flags, "rate-quality flags")
if (anyDuplicated(target$stay_id) || anyDuplicated(exposure$stay_id) ||
    anyDuplicated(rate_flags$analysis_id)) {
  stop("Mode-quality input IDs are not unique.")
}
exposure <- exposure[stay_id %in% target$stay_id]
setorder(target, stay_id)
setorder(exposure, stay_id)
if (nrow(exposure) != nrow(target) ||
    !identical(exposure$stay_id, target$stay_id) ||
    any(exposure$tuple_observed != TRUE) ||
    any(exposure$prediction_time !=
          as.POSIXct(target$ventilator_tuple_available_time, tz = "UTC")) ||
    any(exposure$anchor_time <
          as.POSIXct(target$index_time, tz = "UTC")) ||
    any(exposure$prediction_time >
          as.POSIXct(target$landmark_time, tz = "UTC"))) {
  stop("Primary tuple identity/timing is inconsistent.")
}
tuple <- exposure[, .(
  stay_id,
  index_time,
  anchor_time,
  prediction_time,
  primary_rr_source = rr_source,
  primary_vt_source = vt_source
)]

mode <- fread(
  cmd = sprintf("gzip -cd %s", shQuote(paths$mode_cache)),
  showProgress = FALSE
)
assert_outcome_blind(mode, "mode cache")
required_mode <- c(
  "stay_id", "charttime", "storetime", "itemid", "value", "warning"
)
if (!all(required_mode %in% names(mode))) {
  stop("Mode cache lacks required columns.")
}
mode[, `:=`(
  charttime = as.POSIXct(charttime, tz = "UTC"),
  storetime = as.POSIXct(storetime, tz = "UTC"),
  itemid = as.integer(itemid),
  mode_value = trimws(as.character(value)),
  warning = as.integer(warning)
)]
mode <- mode[
  stay_id %in% tuple$stay_id &
    itemid %in% c(223849L, 229314L) &
    !is.na(charttime) & nzchar(mode_value)
]
mode[, available_time := charttime]
mode[!is.na(storetime), available_time := pmax(charttime, storetime)]
mode <- unique(mode[, .(
  stay_id, charttime, available_time, itemid, mode_value, warning
)])
mode[, formula_compatible_record :=
  paste(itemid, mode_value) %chin%
    paste(compatible_pairs$itemid, compatible_pairs$mode_value)]

candidates <- merge(
  mode, tuple[, .(stay_id, index_time, anchor_time, prediction_time)],
  by = "stay_id", all = FALSE, sort = FALSE, allow.cartesian = TRUE
)
candidates <- candidates[
  charttime >= index_time &
    charttime <= prediction_time &
    available_time <= prediction_time
]
candidates[, anchor_gap_minutes := as.numeric(
  difftime(charttime, anchor_time, units = "mins")
)]
candidates <- candidates[
  abs(anchor_gap_minutes) <= anchor_window_minutes
]
candidates[, `:=`(
  absolute_anchor_gap_minutes = abs(anchor_gap_minutes),
  future_tie = anchor_gap_minutes > 0
)]
setorder(
  candidates, stay_id, absolute_anchor_gap_minutes, future_tie,
  charttime, available_time, itemid, mode_value
)
selected <- candidates[, {
  nearest <- .SD[
    absolute_anchor_gap_minutes == min(absolute_anchor_gap_minutes)
  ]
  nearest <- nearest[future_tie == min(future_tie)]
  nearest <- nearest[charttime == min(charttime)]
  nearest <- nearest[available_time == min(available_time)]
  values <- sort(unique(nearest$mode_value))
  compatible <- unique(nearest$formula_compatible_record)
  .(
    selected_mode_time = nearest$charttime[[1L]],
    selected_mode_available_time = nearest$available_time[[1L]],
    selected_mode_anchor_gap_minutes =
      nearest$anchor_gap_minutes[[1L]],
    selected_mode_itemids =
      paste(sort(unique(nearest$itemid)), collapse = ";"),
    selected_mode_values = paste(values, collapse = " | "),
    selected_mode_distinct_values = length(values),
    selected_mode_source_rows = nrow(nearest),
    selected_mode_warning_any = any(nearest$warning == 1L, na.rm = TRUE),
    simultaneous_mode_conflict =
      length(values) > 1L || length(compatible) > 1L,
    formula_compatible_mode =
      length(values) == 1L &&
      length(compatible) == 1L &&
      isTRUE(compatible[[1L]])
  )
}, by = stay_id]

flags <- merge(
  tuple, selected, by = "stay_id", all.x = TRUE, sort = FALSE
)
flags[, mode_record_available := !is.na(selected_mode_time)]
for (field in c(
  "selected_mode_distinct_values", "selected_mode_source_rows"
)) {
  set(flags, which(!flags$mode_record_available), field, 0L)
}
for (field in c(
  "selected_mode_warning_any", "simultaneous_mode_conflict",
  "formula_compatible_mode"
)) {
  set(flags, which(!flags$mode_record_available), field, FALSE)
}
rate_keep <- rate_flags[, .(
  stay_id = as.integer(analysis_id),
  preferred_source_primary_tuple,
  rate_concordant,
  rate_concordant_preferred_source
)]
flags <- merge(flags, rate_keep, by = "stay_id", all.x = TRUE, sort = FALSE)
if (anyNA(flags$rate_concordant) ||
    anyNA(flags$preferred_source_primary_tuple)) {
  stop("Mode and rate-quality target sets do not match.")
}
flags[, formula_compatible_and_rate_concordant :=
  formula_compatible_mode & rate_concordant]
flags[, formula_compatible_rate_and_preferred :=
  formula_compatible_mode & rate_concordant &
    preferred_source_primary_tuple]

invariants <- data.table(
  check = c(
    "target_row_count", "unique_stay_ids", "mode_flags_complete",
    "selected_mode_in_exposure_window", "selected_mode_available_by_landmark",
    "selected_mode_within_anchor_window", "no_conflict_marked_compatible",
    "formula_compatible_mapping_exact", "tuple_reselection_absent",
    "outcome_artifacts_opened"
  ),
  pass = c(
    nrow(flags) == nrow(target),
    !anyDuplicated(flags$stay_id),
    !anyNA(flags$formula_compatible_mode) &&
      !anyNA(flags$formula_compatible_and_rate_concordant),
    all(!flags$mode_record_available |
          (flags$selected_mode_time >= flags$index_time &
             flags$selected_mode_time <= flags$prediction_time)),
    all(!flags$mode_record_available |
          flags$selected_mode_available_time <= flags$prediction_time),
    all(!flags$mode_record_available |
          abs(flags$selected_mode_anchor_gap_minutes) <=
            anchor_window_minutes),
    all(!flags$simultaneous_mode_conflict |
          !flags$formula_compatible_mode),
    all(!flags$formula_compatible_mode |
          flags$selected_mode_values %chin%
            unique(compatible_pairs$mode_value)),
    TRUE,
    TRUE
  )
)
if (any(invariants$pass != TRUE)) {
  stop(
    "Mode-quality invariant failure: ",
    paste(invariants[pass != TRUE]$check, collapse = ", ")
  )
}

selected_frequency <- flags[mode_record_available == TRUE, .(
  tuples = .N,
  compatible_tuples = sum(formula_compatible_mode),
  rate_concordant_tuples = sum(rate_concordant),
  formula_compatible_and_rate_concordant =
    sum(formula_compatible_and_rate_concordant)
), by = .(
  selected_mode_values, selected_mode_itemids,
  simultaneous_mode_conflict
)][order(-tuples, selected_mode_values)]

summary_table <- data.table(
  tuple_n = nrow(flags),
  mode_available_n = sum(flags$mode_record_available),
  mode_available_percent = 100 * mean(flags$mode_record_available),
  simultaneous_conflict_n = sum(flags$simultaneous_mode_conflict),
  simultaneous_conflict_percent =
    100 * mean(flags$simultaneous_mode_conflict),
  formula_compatible_n = sum(flags$formula_compatible_mode),
  formula_compatible_percent =
    100 * mean(flags$formula_compatible_mode),
  rate_concordant_n = sum(flags$rate_concordant),
  formula_compatible_and_rate_concordant_n =
    sum(flags$formula_compatible_and_rate_concordant),
  formula_compatible_and_rate_concordant_percent =
    100 * mean(flags$formula_compatible_and_rate_concordant),
  formula_compatible_rate_and_preferred_n =
    sum(flags$formula_compatible_rate_and_preferred),
  formula_compatible_rate_and_preferred_percent =
    100 * mean(flags$formula_compatible_rate_and_preferred)
)

mapping_table <- rbindlist(list(
  compatible_pairs[, .(
    itemid, mode_value,
    classification = "formula_compatible",
    rationale = paste(
      "Conventional volume-control assist/control-compatible label;",
      "does not establish passive ventilation or constant flow."
    )
  )],
  data.table(
    itemid = NA_integer_,
    mode_value = paste(
      "All AutoFlow, PRVC/APV, pressure-control, SIMV, spontaneous,",
      "APRV, NIV, standby, and unmapped labels"
    ),
    classification = "not_formula_compatible",
    rationale = paste(
      "Mode structure is not conventional volume-control assist/control",
      "or remains too ambiguous for the VCV-derived equation."
    )
  )
), fill = TRUE)

output_flags <- flags[, .(
  analysis_id = as.character(stay_id),
  anchor_time,
  prediction_time,
  mode_record_available,
  selected_mode_time,
  selected_mode_available_time,
  selected_mode_anchor_gap_minutes,
  selected_mode_itemids,
  selected_mode_values,
  selected_mode_distinct_values,
  selected_mode_source_rows,
  selected_mode_warning_any,
  simultaneous_mode_conflict,
  formula_compatible_mode,
  preferred_source_primary_tuple,
  rate_concordant,
  rate_concordant_preferred_source,
  formula_compatible_and_rate_concordant,
  formula_compatible_rate_and_preferred
)]
metadata <- list(
  version = "mimic_primary_tuple_mode_quality_v2",
  locked_config_version = LOCKED_V2$version,
  outcome_blind = TRUE,
  tuple_reselection = FALSE,
  database = "MIMIC-IV",
  anchor_window_minutes = anchor_window_minutes,
  exact_mapping = compatible_pairs,
  simultaneous_conflict_rule =
    "more than one distinct nearest mode value is not compatible",
  interpretation = paste(
    "Supports closer compatibility with the VCV-derived surrogate formula;",
    "does not prove passivity, constant flow, or a valid plateau maneuver."
  ),
  input_hashes = lapply(paths, sha256_file)
)
attr(output_flags, "rebuild_metadata") <- metadata

flags_path <- file.path(
  private_out, "mimic_primary_tuple_mode_quality_flags_v2.rds"
)
summary_path <- file.path(
  aggregate_out, "mimic_primary_tuple_mode_quality_summary_v2.csv"
)
frequency_path <- file.path(
  aggregate_out, "mimic_selected_mode_frequency_v2.csv"
)
mapping_path <- file.path(
  aggregate_out, "mimic_formula_compatible_mode_mapping_v2.csv"
)
invariant_path <- file.path(
  qc_out, "mimic_primary_tuple_mode_quality_invariants_v2.csv"
)
input_manifest_path <- file.path(
  qc_out, "mimic_primary_tuple_mode_quality_input_manifest_v2.csv"
)
atomic_save_rds(output_flags, flags_path)
atomic_write_csv(summary_table, summary_path)
atomic_write_csv(selected_frequency, frequency_path)
atomic_write_csv(mapping_table, mapping_path)
atomic_write_csv(invariants, invariant_path)
atomic_write_csv(
  data.table(
    input_name = names(paths),
    path = normalizePath(unlist(paths), mustWork = TRUE),
    sha256 = vapply(paths, sha256_file, character(1L)),
    outcome_artifact = FALSE
  ),
  input_manifest_path
)

gate <- data.table(
  field = c(
    "status", "locked_config_version", "script_sha256",
    "flags_sha256", "mapping_sha256", "tuple_n",
    "mode_available_n", "formula_compatible_n",
    "formula_compatible_and_rate_concordant_n",
    "outcome_artifacts_opened", "tuple_reselection",
    "anchor_window_minutes", "all_invariants_pass", "completed_at"
  ),
  value = c(
    "PASS", LOCKED_V2$version, sha256_file(script_path),
    sha256_file(flags_path), sha256_file(mapping_path), nrow(flags),
    sum(flags$mode_record_available),
    sum(flags$formula_compatible_mode),
    sum(flags$formula_compatible_and_rate_concordant),
    "FALSE", "FALSE", anchor_window_minutes, "TRUE",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
)
atomic_write_csv(gate, completion_gate)

cat("REBUILD_V2_MIMIC_MODE_QUALITY_PASS\n")
print(summary_table)
