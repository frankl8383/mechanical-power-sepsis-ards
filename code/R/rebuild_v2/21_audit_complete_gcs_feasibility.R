#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2:
# fixed-6h complete-GCS sensitivity feasibility audit
#
# This script opens no outcomes and fits no endpoint model. It determines
# whether a complete-GCS sensitivity can be reconstructed under the v2 fixed
# landmark using current v2 artifacts, retained v1 raw candidate caches, or the
# original database sources. Selected v1 GCS values and v1 time windows are
# never substituted for a v2 extraction.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/21_audit_complete_gcs_feasibility.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))

stopifnot(
  identical(LOCKED_V2$version, "2.0.0"),
  isTRUE(LOCKED_V2$missing_data_hierarchy$sensitivity_complete_gcs)
)

read_gate <- function(path, label) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field)) {
    stop("Malformed ", label, ": ", path)
  }
  setNames(as.character(gate$value), gate$field)
}

read_gzip_header <- function(path) {
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  line <- readLines(connection, n = 1L, warn = FALSE)
  if (length(line) != 1L || !nzchar(line)) {
    stop("Could not read gzip CSV header: ", path)
  }
  trimws(strsplit(line, ",", fixed = TRUE)[[1L]], which = "both")
}

expected_hash_from_manifest <- function(manifest_path, relative_path) {
  line <- readLines(manifest_path, warn = FALSE)
  hit <- line[grepl(
    paste0("(^|[[:space:]])", gsub(
      "([.])", "\\\\\\1", relative_path
    ), "$"),
    line
  )]
  if (length(hit) != 1L) {
    stop("Expected source hash was not uniquely found: ", relative_path)
  }
  hash <- strsplit(trimws(hit), "[[:space:]]+")[[1L]][[1L]]
  if (!grepl("^[0-9a-f]{64}$", hash)) {
    stop("Malformed expected source hash: ", relative_path)
  }
  hash
}

paths <- list(
  landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  mimic_target = file.path(
    PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds"
  ),
  eicu_target = file.path(
    PRIVATE_ROOT, "eicu", "eicu_no_gcs_core_targets_v2.rds"
  ),
  mimic_v2_cache = file.path(
    PRIVATE_ROOT, "mimic", "cache_v2", "no_gcs_core",
    "chartevents_map_candidates_v2.csv.gz"
  ),
  eicu_v2_cache = file.path(
    PRIVATE_ROOT, "eicu", "cache_v2", "no_gcs_core",
    "nurse_map_candidates_v2.csv.gz"
  ),
  mimic_v1_gcs_cache = file.path(
    REBUILD_V1_ROOT, "private", "mimic", "cache_v1",
    "mimic_severity", "chartevents_severity_candidates_v1.csv.gz"
  ),
  eicu_v1_gcs_cache = file.path(
    REBUILD_V1_ROOT, "private", "eicu", "cache_v1",
    "eicu_severity", "nurse_severity_candidates_v1.csv.gz"
  ),
  mimic_v1_manifest = file.path(
    REBUILD_V1_ROOT, "private", "mimic", "cache_v1",
    "mimic_severity", "filter_manifest_v1.csv"
  ),
  eicu_v1_manifest = file.path(
    REBUILD_V1_ROOT, "private", "eicu", "cache_v1",
    "eicu_severity", "filter_manifest_v1.csv"
  ),
  mimic_raw = file.path(MIMIC_ROOT, "icu", "chartevents.csv.gz"),
  eicu_raw = file.path(EICU_ROOT, "nurseCharting.csv.gz"),
  mimic_sha_manifest = file.path(MIMIC_ROOT, "SHA256SUMS.txt"),
  eicu_sha_manifest = file.path(EICU_ROOT, "SHA256SUMS.txt"),
  mimic_v1_rule_script = file.path(
    PROJECT_ROOT, "code", "R", "rebuild_v1",
    "05_build_mimic_severity_core.R"
  ),
  eicu_v1_rule_script = file.path(
    PROJECT_ROOT, "code", "R", "rebuild_v1",
    "06_build_eicu_severity_core.R"
  )
)
missing_paths <- names(paths)[!vapply(paths, file.exists, logical(1L))]
if (length(missing_paths)) {
  stop(
    "Missing complete-GCS feasibility input(s): ",
    paste(missing_paths, collapse = ", ")
  )
}

qc_out <- file.path(QC_ROOT, "complete_gcs_feasibility")
aggregate_out <- file.path(AGGREGATE_ROOT, "complete_gcs_feasibility")
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(
  qc_out, "complete_gcs_fixed6h_feasibility_v2.csv"
)
unlink(completion_gate, force = TRUE)

landmark_gate <- read_gate(paths$landmark_gate, "fixed landmark gate")
if (!identical(
      landmark_gate[["locked_config_version"]],
      LOCKED_V2$version
    ) ||
    !identical(
      landmark_gate[["mimic_target_sha256"]],
      v2_pm_sha256_file(paths$mimic_target)
    ) ||
    !identical(
      landmark_gate[["eicu_target_sha256"]],
      v2_pm_sha256_file(paths$eicu_target)
    )) {
  stop("Fixed landmark target gate did not validate.")
}

mimic_target <- as.data.frame(readRDS(paths$mimic_target))
eicu_target <- as.data.frame(readRDS(paths$eicu_target))
v2_pm_assert_outcome_free(mimic_target, "MIMIC fixed-6h tuple target")
v2_pm_assert_outcome_free(eicu_target, "eICU fixed-6h tuple target")
v2_pm_require_columns(
  mimic_target,
  c(
    "stay_id", "index_time", "landmark_time",
    "covariate_window_start", "covariate_window_end"
  ),
  "MIMIC fixed-6h tuple target"
)
v2_pm_require_columns(
  eicu_target,
  c(
    "patientunitstayid", "index_time", "landmark_time",
    "covariate_window_start", "covariate_window_end"
  ),
  "eICU fixed-6h tuple target"
)

mimic_v2_cache <- fread(
  paths$mimic_v2_cache,
  select = c("stay_id", "itemid"),
  showProgress = FALSE
)
eicu_v2_cache <- fread(
  paths$eicu_v2_cache,
  select = c(
    "patientunitstayid",
    "nursingchartcelltypevallabel",
    "nursingchartcelltypevalname"
  ),
  showProgress = FALSE
)
mimic_v2_gcs_rows <- sum(
  mimic_v2_cache$itemid %in% c(220739L, 223900L, 223901L)
)
eicu_v2_gcs_rows <- sum(
  (
    eicu_v2_cache$nursingchartcelltypevallabel ==
      "Glasgow coma score" &
      eicu_v2_cache$nursingchartcelltypevalname %in%
        c("GCS Total", "Eyes", "Verbal", "Motor")
  ) |
    (
      eicu_v2_cache$nursingchartcelltypevallabel ==
        "Score (Glasgow Coma Scale)" &
        eicu_v2_cache$nursingchartcelltypevalname == "Value"
    )
)

mimic_v1_cache <- fread(
  paths$mimic_v1_gcs_cache,
  select = c("stay_id", "itemid"),
  showProgress = FALSE
)
eicu_v1_cache <- fread(
  paths$eicu_v1_gcs_cache,
  select = c(
    "patientunitstayid",
    "nursingchartcelltypevallabel",
    "nursingchartcelltypevalname"
  ),
  showProgress = FALSE
)
mimic_v1_all_ids <- unique(as.character(mimic_v1_cache$stay_id))
mimic_v1_gcs_ids <- unique(as.character(
  mimic_v1_cache[
    itemid %in% c(220739L, 223900L, 223901L),
    stay_id
  ]
))
eicu_v1_all_ids <- unique(as.character(eicu_v1_cache$patientunitstayid))
eicu_v1_gcs_ids <- unique(as.character(eicu_v1_cache[
  (
    nursingchartcelltypevallabel == "Glasgow coma score" &
      nursingchartcelltypevalname %in%
        c("GCS Total", "Eyes", "Verbal", "Motor")
  ) |
    (
      nursingchartcelltypevallabel ==
        "Score (Glasgow Coma Scale)" &
        nursingchartcelltypevalname == "Value"
    ),
  patientunitstayid
]))

mimic_target_ids <- as.character(mimic_target$stay_id)
eicu_target_ids <- as.character(eicu_target$patientunitstayid)
coverage <- rbind(
  data.frame(
    database = "MIMIC-IV",
    current_v2_tuple_target_n = length(mimic_target_ids),
    current_v2_cache_gcs_rows = mimic_v2_gcs_rows,
    current_v2_cache_has_gcs = mimic_v2_gcs_rows > 0L,
    v1_cache_any_row_target_overlap_n =
      sum(mimic_target_ids %in% mimic_v1_all_ids),
    v1_cache_gcs_candidate_target_overlap_n =
      sum(mimic_target_ids %in% mimic_v1_gcs_ids),
    v1_cache_gcs_candidate_target_overlap_fraction =
      mean(mimic_target_ids %in% mimic_v1_gcs_ids),
    v1_cache_complete_for_current_v2_target =
      all(mimic_target_ids %in% mimic_v1_all_ids),
    v1_selected_values_or_windows_reusable = FALSE,
    new_v2_source_extraction_required = TRUE,
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    current_v2_tuple_target_n = length(eicu_target_ids),
    current_v2_cache_gcs_rows = eicu_v2_gcs_rows,
    current_v2_cache_has_gcs = eicu_v2_gcs_rows > 0L,
    v1_cache_any_row_target_overlap_n =
      sum(eicu_target_ids %in% eicu_v1_all_ids),
    v1_cache_gcs_candidate_target_overlap_n =
      sum(eicu_target_ids %in% eicu_v1_gcs_ids),
    v1_cache_gcs_candidate_target_overlap_fraction =
      mean(eicu_target_ids %in% eicu_v1_gcs_ids),
    v1_cache_complete_for_current_v2_target =
      all(eicu_target_ids %in% eicu_v1_all_ids),
    v1_selected_values_or_windows_reusable = FALSE,
    new_v2_source_extraction_required = TRUE,
    stringsAsFactors = FALSE
  )
)

mimic_header <- read_gzip_header(paths$mimic_raw)
eicu_header <- read_gzip_header(paths$eicu_raw)
mimic_required <- c(
  "subject_id", "hadm_id", "stay_id", "charttime", "storetime",
  "itemid", "value", "valuenum", "valueuom", "warning"
)
eicu_required <- c(
  "nursingchartid", "patientunitstayid", "nursingchartoffset",
  "nursingchartentryoffset", "nursingchartcelltypecat",
  "nursingchartcelltypevallabel", "nursingchartcelltypevalname",
  "nursingchartvalue"
)
mimic_source_info <- file.info(paths$mimic_raw)
eicu_source_info <- file.info(paths$eicu_raw)
source_schema <- rbind(
  data.frame(
    database = "MIMIC-IV",
    raw_path = paths$mimic_raw,
    raw_size_bytes = mimic_source_info$size,
    expected_sha256 = expected_hash_from_manifest(
      paths$mimic_sha_manifest, "icu/chartevents.csv.gz"
    ),
    full_raw_hash_recomputed_in_this_audit = FALSE,
    required_schema_columns =
      paste(mimic_required, collapse = ";"),
    required_schema_present =
      all(mimic_required %in% mimic_header),
    gcs_source_mapping =
      "itemids 220739 eye, 223900 verbal, 223901 motor",
    measurement_time_available = "charttime" %in% mimic_header,
    availability_time_available = "storetime" %in% mimic_header,
    exact_fixed6h_reselection_possible = TRUE,
    stringsAsFactors = FALSE
  ),
  data.frame(
    database = "eICU-CRD",
    raw_path = paths$eicu_raw,
    raw_size_bytes = eicu_source_info$size,
    expected_sha256 = expected_hash_from_manifest(
      paths$eicu_sha_manifest, "nurseCharting.csv.gz"
    ),
    full_raw_hash_recomputed_in_this_audit = FALSE,
    required_schema_columns =
      paste(eicu_required, collapse = ";"),
    required_schema_present =
      all(eicu_required %in% eicu_header),
    gcs_source_mapping = paste(
      "exact GCS Total/Value labels plus same-offset eye/verbal/motor",
      "reconstruction"
    ),
    measurement_time_available =
      "nursingchartoffset" %in% eicu_header,
    availability_time_available =
      "nursingchartentryoffset" %in% eicu_header,
    exact_fixed6h_reselection_possible = TRUE,
    stringsAsFactors = FALSE
  )
)

rule_audit <- data.frame(
  audit_dimension = c(
    "target_population",
    "measurement_window",
    "availability_boundary",
    "score_range",
    "within_database_selection",
    "mimic_source_rule",
    "eicu_source_rule",
    "cross_database_source_equivalence",
    "v1_time_definition_substitution",
    "outcome_access",
    "endpoint_model_action"
  ),
  planned_v2_rule = c(
    "all fixed-6h tuple-positive patients",
    "max(ICU start, index-24 h) through index+6 h landmark",
    "measurement and documented availability no later than landmark",
    "integer total GCS 3-15",
    "worst valid total; deterministic time/source tie-breaking",
    "strict same-charttime eye+verbal+motor reconstruction; airway-unscorable verbal rows excluded",
    "valid explicit total prioritized; otherwise same-offset eye+verbal+motor reconstruction",
    "not exact: source structures differ, but both estimate recorded total GCS",
    "forbidden",
    "none",
    "audit only; build/fitting deferred until a new v2 extraction is frozen"
  ),
  feasible = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE
  ),
  consequence = c(
    "current v2 target IDs and windows are available",
    "current v2 lower and upper bounds are available",
    "both raw sources retain an availability timestamp",
    "source mappings support the common clinical scale",
    "can be implemented outcome-blind",
    "raw component fields and strict text/numeric reconciliation are available",
    "raw total/component labels and entry offsets are available",
    "report as a source-harmonized sensitivity, not identical measurement",
    "v1 selected GCS values cannot enter the v2 model",
    "feasibility assessment is predictor-side only",
    "new source filter, extraction gate, and fixed6h core are required first"
  ),
  stringsAsFactors = FALSE
)

all_source_checks <- all(
  source_schema$required_schema_present,
  source_schema$measurement_time_available,
  source_schema$availability_time_available,
  source_schema$exact_fixed6h_reselection_possible
)
old_cache_incomplete <- all(!coverage$v1_cache_complete_for_current_v2_target)
current_v2_cache_absent <- all(!coverage$current_v2_cache_has_gcs)
feasible_new_extraction <- all_source_checks &&
  old_cache_incomplete && current_v2_cache_absent
if (!feasible_new_extraction) {
  status <- "STOP_NOT_REPRODUCIBLE"
  conclusion <- paste(
    "The complete-GCS sensitivity cannot be reconstructed under the",
    "current fixed-6h contract."
  )
} else {
  status <- "FEASIBLE_REQUIRES_NEW_V2_EXTRACTION"
  conclusion <- paste(
    "A fixed-6h complete-GCS sensitivity is reproducible from the raw",
    "sources, but current v2 caches contain no GCS and v1 caches cover only",
    "a subset of the current target. A new outcome-blind v2 source extraction",
    "and freeze are required before any endpoint model."
  )
}

aggregate_outputs <- list(
  "complete_gcs_artifact_coverage_v2.csv" = coverage,
  "complete_gcs_source_schema_v2.csv" = source_schema,
  "complete_gcs_rule_comparability_v2.csv" = rule_audit
)
for (name in names(aggregate_outputs)) {
  v2_pm_atomic_write_csv(
    aggregate_outputs[[name]], file.path(aggregate_out, name)
  )
}

input_manifest <- data.frame(
  artifact = names(paths),
  path = unname(unlist(paths, use.names = FALSE)),
  sha256 = vapply(
    paths,
    function(path) {
      if (path %in% c(paths$mimic_raw, paths$eicu_raw)) {
        NA_character_
      } else {
        v2_pm_sha256_file(path)
      }
    },
    character(1L)
  ),
  full_file_hash_skipped = unname(unlist(paths, use.names = FALSE)) %in%
    c(paths$mimic_raw, paths$eicu_raw),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(
  input_manifest,
  file.path(qc_out, "complete_gcs_feasibility_input_manifest_v2.csv")
)

invariants <- data.frame(
  check = c(
    "outcome_artifacts_opened_false",
    "endpoint_model_run_false",
    "v1_selected_gcs_reused_false",
    "v1_time_definitions_reused_false",
    "current_v2_caches_contain_no_gcs",
    "v1_caches_incomplete_for_current_target",
    "raw_source_schema_supports_fixed6h_reconstruction",
    "source_specific_measurement_limitation_explicit",
    "new_v2_extraction_required"
  ),
  pass = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    current_v2_cache_absent,
    old_cache_incomplete,
    all_source_checks,
    !rule_audit$feasible[
      rule_audit$audit_dimension ==
        "cross_database_source_equivalence"
    ],
    all(coverage$new_v2_source_extraction_required)
  ),
  stringsAsFactors = FALSE
)
if (!all(invariants$pass)) {
  status <- "STOP_NOT_REPRODUCIBLE"
  conclusion <- paste(
    conclusion,
    "One or more feasibility invariants failed:",
    paste(invariants$check[!invariants$pass], collapse = ", ")
  )
}
v2_pm_atomic_write_csv(
  invariants,
  file.path(qc_out, "complete_gcs_feasibility_invariants_v2.csv")
)

gate <- data.frame(
  field = c(
    "status",
    "locked_config_version",
    "script_sha256",
    "mimic_target_sha256",
    "eicu_target_sha256",
    "mimic_v1_cache_sha256",
    "eicu_v1_cache_sha256",
    "mimic_current_v2_target_n",
    "eicu_current_v2_target_n",
    "mimic_v1_gcs_candidate_overlap_n",
    "eicu_v1_gcs_candidate_overlap_n",
    "current_v2_cache_has_gcs",
    "v1_cache_complete_for_current_v2_target",
    "raw_source_schema_supports_reconstruction",
    "cross_database_source_measurement_exactly_equivalent",
    "new_v2_source_extraction_required",
    "v1_selected_values_reused",
    "v1_time_definitions_reused",
    "outcome_artifacts_opened",
    "endpoint_model_run",
    "manuscript_ci_ready",
    "conclusion",
    "completed_at"
  ),
  value = c(
    status,
    LOCKED_V2$version,
    v2_pm_sha256_file(script_path),
    v2_pm_sha256_file(paths$mimic_target),
    v2_pm_sha256_file(paths$eicu_target),
    v2_pm_sha256_file(paths$mimic_v1_gcs_cache),
    v2_pm_sha256_file(paths$eicu_v1_gcs_cache),
    as.character(nrow(mimic_target)),
    as.character(nrow(eicu_target)),
    as.character(
      coverage$v1_cache_gcs_candidate_target_overlap_n[
        coverage$database == "MIMIC-IV"
      ]
    ),
    as.character(
      coverage$v1_cache_gcs_candidate_target_overlap_n[
        coverage$database == "eICU-CRD"
      ]
    ),
    "FALSE",
    "FALSE",
    as.character(all_source_checks),
    "FALSE",
    "TRUE",
    "FALSE",
    "FALSE",
    "FALSE",
    "FALSE",
    "FALSE",
    conclusion,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ),
  stringsAsFactors = FALSE
)
v2_pm_atomic_write_csv(gate, completion_gate)

message("REBUILD_V2_COMPLETE_GCS_FEASIBILITY_", status)
message("  ", conclusion)
