#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "00_preflight.R"
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
source(file.path(script_dir, "00_config.R"))

required <- data.table(
  database = c(
    rep("MIMIC-IV", 13),
    rep("eICU-CRD", 13),
    "legacy"
  ),
  relative_path = c(
    "hosp/patients.csv.gz",
    "hosp/admissions.csv.gz",
    "hosp/omr.csv.gz",
    "hosp/labevents.csv.gz",
    "hosp/d_labitems.csv.gz",
    "hosp/microbiologyevents.csv.gz",
    "hosp/prescriptions.csv.gz",
    "icu/icustays.csv.gz",
    "icu/chartevents.csv.gz",
    "icu/d_items.csv.gz",
    "icu/procedureevents.csv.gz",
    "icu/inputevents.csv.gz",
    "icu/outputevents.csv.gz",
    "patient.csv.gz",
    "respiratoryCharting.csv.gz",
    "respiratoryCare.csv.gz",
    "lab.csv.gz",
    "diagnosis.csv.gz",
    "admissionDx.csv.gz",
    "apacheApsVar.csv.gz",
    "apachePatientResult.csv.gz",
    "hospital.csv.gz",
    "nurseCharting.csv.gz",
    "infusionDrug.csv.gz",
    "medication.csv.gz",
    "intakeOutput.csv.gz",
    "final_manuscript_package/ards_mp_FINAL_submission.zip"
  )
)

required[, absolute_path := fifelse(
  database == "MIMIC-IV",
  file.path(MIMIC_ROOT, relative_path),
  fifelse(
    database == "eICU-CRD",
    file.path(EICU_ROOT, relative_path),
    file.path(PROJECT_ROOT, relative_path)
  )
)]

read_gzip_header <- function(path) {
  if (!file.exists(path) || !grepl("\\.gz$", path)) return(NA_character_)
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  out <- tryCatch(readLines(con, n = 1L, warn = FALSE), error = function(e) NA_character_)
  if (!length(out)) NA_character_ else out[[1]]
}

required[, exists := file.exists(absolute_path)]
required[, size_bytes := fifelse(exists, file.info(absolute_path)$size, NA_real_)]
required[, modified_time := as.character(
  fifelse(exists, as.character(file.info(absolute_path)$mtime), NA_character_)
)]
required[, readable_header := vapply(absolute_path, read_gzip_header, character(1))]
required[, readable := exists & (is.na(readable_header) | nzchar(readable_header))]

if (!all(required$exists)) {
  fwrite(required, file.path(QC_ROOT, "preflight_file_inventory.csv"))
  stop("Preflight failed: one or more required inputs are missing.")
}

fwrite(required, file.path(QC_ROOT, "preflight_file_inventory.csv"))

legacy_zip <- required[database == "legacy", absolute_path]
sha256 <- tryCatch({
  sha_line <- trimws(system2("shasum", c("-a", "256", shQuote(legacy_zip)), stdout = TRUE))
  strsplit(sha_line[[1]], "[[:space:]]+")[[1]][[1]]
}, error = function(e) NA_character_)

manifest <- data.table(
  field = c(
    "run_time",
    "config_version",
    "config_freeze_date",
    "project_root",
    "mimic_root",
    "eicu_root",
    "legacy_submission_sha256",
    "mimic_code_reference_commit"
  ),
  value = c(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    LOCKED$version,
    as.character(LOCKED$freeze_date),
    PROJECT_ROOT,
    MIMIC_ROOT,
    EICU_ROOT,
    sha256,
    "5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4"
  )
)
fwrite(manifest, file.path(QC_ROOT, "preflight_run_manifest.csv"))

summary_lines <- c(
  "# Rebuild v1 preflight QC",
  "",
  sprintf("Run: %s", manifest[field == "run_time", value]),
  sprintf("Locked configuration: %s (%s)", LOCKED$version, LOCKED$freeze_date),
  "",
  sprintf("- Required inputs present: %d/%d", sum(required$exists), nrow(required)),
  sprintf("- Gzip inputs with readable headers: %d/%d", sum(required[grepl("\\.gz$", absolute_path), readable]), sum(grepl("\\.gz$", required$absolute_path))),
  "- Legacy submission archive retained without modification.",
  "- This preflight checks file presence and readable gzip headers; full table-level QC is performed by database-specific scripts.",
  "",
  "## Important provenance",
  "",
  "The official MIT-LCP mimic-code repository was consulted at commit `5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4`. Any deliberate deviations in cohort logic must be recorded in the analysis decision log."
)
writeLines(summary_lines, file.path(QC_ROOT, "preflight_QC.md"), useBytes = TRUE)

cat(sprintf("Preflight PASS: %d required inputs present.\n", nrow(required)))
