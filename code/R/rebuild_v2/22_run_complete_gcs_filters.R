#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2:
# prepare full fixed-6h tuple target keys and run fresh outcome-blind GCS scans.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/22_run_complete_gcs_filters.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "09_primary_model_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
database_arg <- sub(
  "^--database=", "", trailing[grepl("^--database=", trailing)]
)
database_arg <- if (length(database_arg)) database_arg[[1L]] else "both"
if (!database_arg %in% c("mimic", "eicu", "both")) {
  stop("--database must be mimic, eicu, or both.")
}

helper_path <- file.path(
  script_dir, "22a_filter_complete_gcs_inputs_v2.py"
)
decision_log_path <- file.path(
  PROJECT_ROOT, "docs", "rebuild_v2", "analysis_decision_log_v2.md"
)
feasibility_gate_path <- file.path(
  QC_ROOT, "complete_gcs_feasibility",
  "complete_gcs_fixed6h_feasibility_v2.csv"
)
landmark_gate_path <- file.path(
  QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
)
required <- c(
  helper_path, decision_log_path,
  feasibility_gate_path, landmark_gate_path
)
if (any(!file.exists(required))) {
  stop(
    "Missing complete-GCS filter prerequisite(s): ",
    paste(required[!file.exists(required)], collapse = ", ")
  )
}

read_field_gate <- function(path, label) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field)) {
    stop("Malformed ", label, ": ", path)
  }
  setNames(as.character(gate$value), gate$field)
}

atomic_fwrite <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  fwrite(x, temporary, ...)
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish: ", path)
  }
  invisible(path)
}

decision_text <- readLines(decision_log_path, warn = FALSE)
if (sum(grepl(
  "^\\| V2-D021 \\|", decision_text, perl = TRUE
)) != 1L ||
    !any(grepl(
      "LOCKED BEFORE COMPLETE-GCS SOURCE EXTRACTION AND OUTCOME FIT",
      decision_text,
      fixed = TRUE
    ))) {
  stop("Complete-GCS preread decision V2-D021 is not locked.")
}

feasibility_gate <- read_field_gate(
  feasibility_gate_path, "complete-GCS feasibility gate"
)
if (!identical(
  feasibility_gate[["status"]],
  "FEASIBLE_REQUIRES_NEW_V2_EXTRACTION"
) ||
    !identical(
      feasibility_gate[["new_v2_source_extraction_required"]],
      "TRUE"
    ) ||
    !identical(feasibility_gate[["outcome_artifacts_opened"]], "FALSE") ||
    !identical(feasibility_gate[["endpoint_model_run"]], "FALSE")) {
  stop("Complete-GCS feasibility gate does not authorize a new extraction.")
}

landmark_gate <- read_field_gate(
  landmark_gate_path, "fixed-landmark gate"
)
if (!identical(
  landmark_gate[["locked_config_version"]],
  LOCKED_V2$version
)) {
  stop("Fixed-landmark gate version mismatch.")
}

run_one <- function(database) {
  contract <- if (database == "mimic") {
    list(
      target_path = file.path(
        PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds"
      ),
      target_hash_field = "mimic_target_sha256",
      target_n = 10468L,
      raw_root = MIMIC_ROOT,
      expected_scanned_rows = 432997491,
      source_hash =
        "fd0387653084e5b142756b98b74fdddc2e5e7eb0f496aa8bf5af3d4176e71098",
      cache_dir = file.path(
        PRIVATE_ROOT, "mimic", "cache_v2", "complete_gcs"
      ),
      key_name = "mimic_complete_gcs_target_keys_v2.csv",
      output_name = "mimic_gcs_candidates_v2.csv.gz"
    )
  } else {
    list(
      target_path = file.path(
        PRIVATE_ROOT, "eicu", "eicu_no_gcs_core_targets_v2.rds"
      ),
      target_hash_field = "eicu_target_sha256",
      target_n = 1459L,
      raw_root = EICU_ROOT,
      expected_scanned_rows = 151604232,
      source_hash =
        "d10444dc6b530dfd198b42f2841de7b76045570fbf10b08e991c133006661c2c",
      cache_dir = file.path(
        PRIVATE_ROOT, "eicu", "cache_v2", "complete_gcs"
      ),
      key_name = "eicu_complete_gcs_target_ids_v2.txt",
      output_name = "eicu_gcs_candidates_v2.csv.gz"
    )
  }
  if (!file.exists(contract$target_path)) {
    stop("Missing ", database, " fixed-6h tuple target.")
  }
  if (!identical(
    landmark_gate[[contract$target_hash_field]],
    v2_pm_sha256_file(contract$target_path)
  )) {
    stop(database, " target hash does not match the fixed-landmark gate.")
  }
  target <- as.data.frame(readRDS(contract$target_path))
  v2_pm_assert_outcome_free(
    target, paste(database, " fixed-6h complete-GCS filter target")
  )
  if (nrow(target) != contract$target_n) {
    stop(database, " fixed-6h tuple target size changed.")
  }
  dir.create(contract$cache_dir, recursive = TRUE, showWarnings = FALSE)
  key_path <- file.path(contract$cache_dir, contract$key_name)
  if (database == "mimic") {
    v2_pm_require_columns(
      target, c("subject_id", "hadm_id", "stay_id"), "MIMIC tuple target"
    )
    if (anyDuplicated(target$stay_id)) stop("Duplicate MIMIC target stay.")
    atomic_fwrite(
      as.data.table(target)[, .(subject_id, hadm_id, stay_id)],
      key_path
    )
  } else {
    v2_pm_require_columns(
      target, "patientunitstayid", "eICU tuple target"
    )
    if (anyDuplicated(target$patientunitstayid)) {
      stop("Duplicate eICU target stay.")
    }
    atomic_fwrite(
      data.table(patientunitstayid = target$patientunitstayid),
      key_path,
      col.names = FALSE
    )
  }

  command_args <- c(
    shQuote(helper_path),
    "--database", database,
    "--keys", shQuote(key_path),
    "--raw-root", shQuote(contract$raw_root),
    "--cache-dir", shQuote(contract$cache_dir)
  )
  output <- system2(
    "python3", command_args, stdout = TRUE, stderr = TRUE
  )
  writeLines(
    c(
      paste("command:", "python3", paste(command_args, collapse = " ")),
      paste("completed_at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
      output
    ),
    file.path(contract$cache_dir, "filter_invocation_log_v2.txt"),
    useBytes = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      database, " complete-GCS raw filter failed:\n",
      paste(output, collapse = "\n")
    )
  }

  manifest_path <- file.path(
    contract$cache_dir, "complete_gcs_filter_manifest_v2.csv"
  )
  raw_gate_path <- file.path(
    contract$cache_dir, "complete_gcs_filter_complete_v2.csv"
  )
  candidate_path <- file.path(
    contract$cache_dir, contract$output_name
  )
  if (any(!file.exists(c(
    manifest_path, raw_gate_path, candidate_path
  )))) {
    stop(database, " filter did not publish every required artifact.")
  }
  manifest <- fread(
    manifest_path, colClasses = "character", showProgress = FALSE
  )
  raw_gate <- fread(
    raw_gate_path, colClasses = "character", showProgress = FALSE
  )
  if (nrow(manifest) != 1L || nrow(raw_gate) != 1L ||
      !identical(manifest$status[[1L]], "PASS") ||
      !identical(raw_gate$status[[1L]], "PASS") ||
      !identical(manifest$reached_eof[[1L]], "TRUE") ||
      !identical(raw_gate$reached_eof[[1L]], "TRUE") ||
      as.integer(manifest$target_count[[1L]]) != contract$target_n ||
      as.numeric(manifest$scanned_rows[[1L]]) !=
        contract$expected_scanned_rows ||
      !identical(
        manifest$raw_sha256[[1L]], contract$source_hash
      ) ||
      !identical(
        manifest$official_sha256_match[[1L]], "TRUE"
      ) ||
      !identical(
        manifest$output_sha256[[1L]],
        v2_pm_sha256_file(candidate_path)
      ) ||
      !identical(
        raw_gate$helper_sha256[[1L]],
        v2_pm_sha256_file(helper_path)
      ) ||
      !identical(raw_gate$outcome_artifacts_opened[[1L]], "FALSE")) {
    stop(database, " complete-GCS filter gate/manifest validation failed.")
  }

  qc_dir <- file.path(
    QC_ROOT, "complete_gcs", "raw_filters", database
  )
  dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
  invocation_path <- file.path(
    contract$cache_dir, "filter_invocation_log_v2.txt"
  )
  gate <- data.frame(
    field = c(
      "status",
      "database",
      "locked_config_version",
      "script_sha256",
      "helper_sha256",
      "decision_log_sha256",
      "decision_id",
      "target_sha256",
      "target_n",
      "key_file_sha256",
      "raw_filter_gate_sha256",
      "raw_filter_manifest_sha256",
      "raw_source_sha256",
      "raw_source_official_hash_match",
      "scanned_rows",
      "kept_rows",
      "reached_eof",
      "candidate_output_sha256",
      "outcome_artifacts_opened",
      "completed_at"
    ),
    value = c(
      "PASS",
      database,
      LOCKED_V2$version,
      v2_pm_sha256_file(script_path),
      v2_pm_sha256_file(helper_path),
      v2_pm_sha256_file(decision_log_path),
      "V2-D021",
      v2_pm_sha256_file(contract$target_path),
      as.character(contract$target_n),
      v2_pm_sha256_file(key_path),
      v2_pm_sha256_file(raw_gate_path),
      v2_pm_sha256_file(manifest_path),
      contract$source_hash,
      "TRUE",
      manifest$scanned_rows[[1L]],
      manifest$kept_rows[[1L]],
      "TRUE",
      v2_pm_sha256_file(candidate_path),
      "FALSE",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ),
    stringsAsFactors = FALSE
  )
  gate_path <- file.path(
    qc_dir, paste0(database, "_complete_gcs_raw_filter_complete_v2.csv")
  )
  v2_pm_atomic_write_csv(gate, gate_path)
  message(
    toupper(database), " COMPLETE_GCS_RAW_FILTER_PASS scanned=",
    manifest$scanned_rows[[1L]], " kept=", manifest$kept_rows[[1L]]
  )
  invisible(list(
    target_path = contract$target_path,
    key_path = key_path,
    candidate_path = candidate_path,
    manifest_path = manifest_path,
    raw_gate_path = raw_gate_path,
    invocation_path = invocation_path,
    gate_path = gate_path
  ))
}

if (database_arg %in% c("mimic", "both")) run_one("mimic")
if (database_arg %in% c("eicu", "both")) run_one("eicu")

message("REBUILD_V2_COMPLETE_GCS_RAW_FILTERS_FINISHED database=", database_arg)
