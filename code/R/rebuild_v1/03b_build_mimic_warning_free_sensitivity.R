#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v1: MIMIC selected-tuple warning audit
#
# The MIMIC-IV `warning` field records a provider-documented warning; it is not
# the legacy `error` field and is not itself proof that a value is invalid.
# Primary tuples have already passed locked physiological ranges and ordering.
# This outcome-blind script annotates the selected tuple and creates a strict
# sensitivity restriction that retains only selected tuples with no warning on
# any of the five components. It never reselects a later tuple, so prediction
# time and the already built time-aligned severity window remain unchanged.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v1/03b_build_mimic_warning_free_sensitivity.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required for warning-sensitivity provenance.")
}

phase2_script <- file.path(dirname(script_path), "03_build_mimic_paired_exposure.R")
phase2_gate_path <- file.path(
  QC_ROOT, "mimic_exposure", "phase2_mimic_exposure_complete_v1.csv"
)
primary_path <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_paired_exposure_primary_60min_v1.rds"
)
cache_path <- file.path(
  PRIVATE_ROOT, "mimic", "cache_v1",
  "selected_paired_exposure_chartevents_v1.rds"
)
annotated_path <- file.path(
  PRIVATE_ROOT, "mimic", "mimic_primary_selected_tuple_warning_flags_v1.rds"
)
warning_free_path <- file.path(
  PRIVATE_ROOT, "mimic",
  "mimic_paired_exposure_sensitivity_warning_free_selected_v1.rds"
)
qc_out <- file.path(QC_ROOT, "mimic_warning_sensitivity")
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(
  qc_out, "phase2c_mimic_warning_sensitivity_complete_v1.csv"
)
completion_gate_tmp <- paste0(completion_gate, ".tmp")
unlink(c(completion_gate, completion_gate_tmp), force = TRUE)

required <- c(phase2_script, phase2_gate_path, primary_path, cache_path)
if (any(!file.exists(required))) {
  stop("Missing required input(s): ", paste(required[!file.exists(required)], collapse = ", "))
}

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")
read_gate_map <- function(path) {
  x <- fread(path, showProgress = FALSE)
  if (!identical(names(x), c("field", "value")) || anyDuplicated(x$field)) {
    stop("Malformed field/value completion gate: ", path)
  }
  setNames(x$value, x$field)
}
require_gate_value <- function(gate, field, expected = NULL) {
  value <- unname(gate[field])
  if (length(value) != 1L || is.na(value)) stop("Gate missing field: ", field)
  if (!is.null(expected) && !identical(value, as.character(expected))) {
    stop("Gate mismatch for ", field, ": ", value, " != ", expected)
  }
  value
}

gate <- read_gate_map(phase2_gate_path)
require_gate_value(gate, "locked_config_version", LOCKED$version)
require_gate_value(gate, "all_invariants_pass", "TRUE")
require_gate_value(gate, "outcome_leakage_guard_pass", "TRUE")
require_gate_value(gate, "all_required_qc_present", "TRUE")
require_gate_value(gate, "script_sha256", sha256_file(phase2_script))
require_gate_value(
  gate, "primary_60min_rds_sha256", sha256_file(primary_path)
)

primary <- as.data.table(readRDS(primary_path))
if (nrow(primary) != as.integer(require_gate_value(gate, "strict_cohort_n")) ||
    sum(primary$tuple_observed, na.rm = TRUE) !=
      as.integer(require_gate_value(gate, "primary_60min_n"))) {
  stop("Primary artifact row counts disagree with its completion gate.")
}

forbidden_pattern <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
if (any(grepl(forbidden_pattern, names(primary), ignore.case = TRUE))) {
  stop("Outcome-like field entered the warning audit.")
}

raw <- as.data.table(readRDS(cache_path))
required_raw <- c("stay_id", "charttime", "itemid", "valuenum", "warning")
if (length(setdiff(required_raw, names(raw)))) {
  stop("Exposure cache is missing required warning-audit columns.")
}
cache_ids <- attr(raw, "target_stay_ids")
if (is.null(cache_ids) || !identical(
  sort(as.integer(cache_ids)), sort(as.integer(primary$stay_id))
)) {
  stop("Exposure cache target IDs do not match the strict cohort.")
}

warning_lookup <- raw[, .(
  source_rows = .N,
  warning_rows = sum(warning == 1L, na.rm = TRUE),
  warning_missing_rows = sum(is.na(warning)),
  warning_any = any(warning == 1L, na.rm = TRUE)
), by = .(stay_id, itemid, measurement_time = charttime)]

selected <- primary[tuple_observed == TRUE]
selected[, audit_row_id := .I]

component_spec <- list(
  pplat = c("anchor_time", "pplat_itemid", "pplat"),
  ppeak = c("ppeak_time", "ppeak_itemid", "ppeak_value"),
  peep = c("peep_time", "peep_itemid", "peep_value"),
  vt = c("vt_time", "vt_itemid", "vt_value"),
  rr = c("rr_time", "rr_itemid", "rr_value")
)

component_flags <- selected[, .(audit_row_id, stay_id)]
component_value_qc <- list()
for (component_name in names(component_spec)) {
  spec <- component_spec[[component_name]]
  keys <- selected[, .(
    audit_row_id,
    stay_id,
    measurement_time = get(spec[[1L]]),
    itemid = as.integer(get(spec[[2L]])),
    selected_value = as.numeric(get(spec[[3L]]))
  )]
  joined <- merge(
    keys, warning_lookup,
    by = c("stay_id", "itemid", "measurement_time"),
    all.x = TRUE, sort = FALSE
  )
  setorder(joined, audit_row_id)
  if (nrow(joined) != nrow(selected) || anyNA(joined$source_rows)) {
    stop("Selected ", component_name, " rows did not map uniquely to raw warning records.")
  }
  component_flags <- merge(
    component_flags,
    joined[, .(
      audit_row_id,
      flag = warning_any,
      source_rows,
      warning_rows,
      warning_missing_rows
    )],
    by = "audit_row_id", all.x = TRUE, sort = FALSE
  )
  setnames(
    component_flags,
    c("flag", "source_rows", "warning_rows", "warning_missing_rows"),
    paste0(
      component_name,
      c("_warning", "_source_rows", "_warning_rows", "_warning_missing_rows")
    )
  )
  component_value_qc[[component_name]] <- joined[, .(
    selected_tuples = .N,
    selected_warning_tuples = sum(warning_any),
    selected_value_min = min(selected_value),
    selected_value_median = median(selected_value),
    selected_value_max = max(selected_value)
  ), by = itemid]
  component_value_qc[[component_name]][, component := component_name]
  setcolorder(component_value_qc[[component_name]], c("component", "itemid"))
}

setorder(component_flags, audit_row_id)
selected <- merge(
  selected, component_flags[, -"stay_id"],
  by = "audit_row_id", all.x = TRUE, sort = FALSE
)
setorder(selected, audit_row_id)
warning_cols <- paste0(names(component_spec), "_warning")
selected[, selected_tuple_any_warning := rowSums(.SD) > 0L, .SDcols = warning_cols]
selected[, selected_tuple_warning_count := rowSums(.SD), .SDcols = warning_cols]
selected[, audit_row_id := NULL]

annotated <- merge(
  primary,
  selected[, c(
    "stay_id", warning_cols, "selected_tuple_any_warning",
    "selected_tuple_warning_count"
  ), with = FALSE],
  by = "stay_id", all.x = TRUE, sort = FALSE
)
if (nrow(annotated) != nrow(primary) || anyDuplicated(annotated$stay_id)) {
  stop("Annotated primary artifact row invariant failed.")
}
warning_free <- annotated[
  tuple_observed == TRUE & selected_tuple_any_warning == FALSE
]
if (!nrow(warning_free) || any(warning_free$selected_tuple_any_warning)) {
  stop("Warning-free sensitivity construction failed.")
}

common_metadata <- list(
  version = "mimic_selected_tuple_warning_audit_v1",
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  locked_config_version = LOCKED$version,
  script = script_path,
  source_phase2_gate_sha256 = sha256_file(phase2_gate_path),
  outcome_blind = TRUE,
  warning_semantics = paste(
    "MIMIC-IV warning indicates a provider-documented warning; primary values",
    "remain governed by physiological ranges and pressure ordering"
  ),
  sensitivity_rule = paste(
    "restrict the already selected primary tuple to zero warnings; do not",
    "reselect a later tuple or alter prediction time"
  )
)
attr(annotated, "rebuild_metadata") <- c(
  common_metadata, list(artifact = basename(annotated_path))
)
attr(warning_free, "rebuild_metadata") <- c(
  common_metadata, list(artifact = basename(warning_free_path))
)
saveRDS(annotated, annotated_path, compress = "xz")
saveRDS(warning_free, warning_free_path, compress = "xz")

component_summary <- data.table(
  component = names(component_spec),
  selected_tuple_n = nrow(selected),
  warning_n = vapply(
    warning_cols, function(x) sum(selected[[x]]), integer(1L)
  )
)
component_summary[, warning_proportion := warning_n / selected_tuple_n]
fwrite(component_summary, file.path(qc_out, "selected_component_warning_QC.csv"))
fwrite(
  rbindlist(component_value_qc),
  file.path(qc_out, "selected_warning_by_component_item_QC.csv")
)
fwrite(
  data.table(
    metric = c(
      "strict_cohort_n", "primary_selected_tuple_n",
      "selected_tuple_any_warning_n", "warning_free_selected_tuple_n"
    ),
    value = c(
      nrow(primary), nrow(selected), sum(selected$selected_tuple_any_warning),
      nrow(warning_free)
    )
  ),
  file.path(qc_out, "warning_sensitivity_funnel.csv")
)

leakage_guard <- data.table(
  check = c(
    "annotated_artifact_has_no_outcome_like_columns",
    "warning_free_artifact_has_no_outcome_like_columns",
    "warning_free_is_subset_of_primary_selected",
    "prediction_time_unchanged"
  ),
  pass = c(
    !any(grepl(forbidden_pattern, names(annotated), ignore.case = TRUE)),
    !any(grepl(forbidden_pattern, names(warning_free), ignore.case = TRUE)),
    all(warning_free$stay_id %in% selected$stay_id),
    all(warning_free$prediction_time ==
          primary[warning_free, on = "stay_id", x.prediction_time])
  )
)
if (any(!leakage_guard$pass)) stop("Warning-sensitivity final guard failed.")
fwrite(leakage_guard, file.path(qc_out, "warning_sensitivity_guard.csv"))

summary_path <- file.path(qc_out, "mimic_warning_sensitivity_QC.md")
writeLines(c(
  "# MIMIC selected-tuple warning sensitivity QC",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("- Primary selected tuples: ", nrow(selected)),
  paste0("- Any provider-documented component warning: ",
         sum(selected$selected_tuple_any_warning)),
  paste0("- Warning-free selected-tuple restriction: ", nrow(warning_free)),
  "- Primary analysis retains range-valid warned values because warning is not the error field.",
  "- Sensitivity analysis restricts the already selected tuple and never changes prediction time.",
  "- No mortality, discharge status, effect, or performance field was read.",
  "",
  "BUILD_COMPLETE"
), summary_path, useBytes = TRUE)

required_qc <- file.path(qc_out, c(
  "selected_component_warning_QC.csv",
  "selected_warning_by_component_item_QC.csv",
  "warning_sensitivity_funnel.csv", "warning_sensitivity_guard.csv",
  "mimic_warning_sensitivity_QC.md"
))
if (!all(file.exists(c(annotated_path, warning_free_path, required_qc)))) {
  stop("Warning-sensitivity output set is incomplete.")
}

completion <- data.table(
  status = "PASS",
  config_version = LOCKED$version,
  completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  script_sha256 = sha256_file(script_path),
  phase2_gate_sha256 = sha256_file(phase2_gate_path),
  source_cache_sha256 = sha256_file(cache_path),
  annotated_rds_sha256 = sha256_file(annotated_path),
  warning_free_rds_sha256 = sha256_file(warning_free_path),
  primary_selected_tuple_n = nrow(selected),
  selected_tuple_any_warning_n = sum(selected$selected_tuple_any_warning),
  warning_free_selected_tuple_n = nrow(warning_free)
)
fwrite(completion, completion_gate_tmp)
if (!file.rename(completion_gate_tmp, completion_gate)) {
  stop("Could not atomically publish warning-sensitivity gate.")
}

message("MIMIC selected-tuple warning sensitivity complete (outcome-blind).")
message("  warned selected tuples: ", sum(selected$selected_tuple_any_warning))
message("  warning-free restriction: ", nrow(warning_free))
message("  BUILD_COMPLETE | script SHA256 ", sha256_file(script_path))
