#!/usr/bin/env Rscript

## Verify agreement among the rebuild configuration and Phase-0 governance
## documents, then write a checksum manifest. This script is outcome-agnostic.

suppressPackageStartupMessages(library(data.table))

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else
  "00_phase0_lock.R"
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
source(file.path(script_dir, "00_config.R"))

docs <- file.path(PROJECT_ROOT, "docs", "rebuild_v1", c(
  "SAP_v1_0.md",
  "terminology_ledger.md",
  "data_dictionary_v1.md",
  "analysis_decision_log.md"
))
config <- file.path(script_dir, "00_config.R")
files <- c(config, docs)

if (!all(file.exists(files))) {
  stop("Phase-0 lock failed: missing file(s): ",
       paste(files[!file.exists(files)], collapse = ", "))
}

read_all <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
doc_text <- lapply(docs, read_all)
names(doc_text) <- basename(docs)

checks <- data.table(
  check = c(
    "all_lock_files_present",
    "all_docs_declare_config_version",
    "sap_primary_infection_available_by_index",
    "sap_plus24_is_sensitivity_only",
    "sap_primary_exposure_window_0_to_6h",
    "sap_primary_tuple_window_60min",
    "sap_sensitivity_tuple_window_30min",
    "sap_primary_outcome_in_hospital",
    "terminology_prohibits_unqualified_Berlin_ARDS",
    "decision_log_records_v1_0_1_amendment"
  ),
  passed = c(
    all(file.exists(files)),
    all(vapply(doc_text, grepl, logical(1),
               pattern = paste0("1\\.0\\.1|v1\\.0\\.1"))),
    grepl("infection evidence available by index", doc_text[["SAP_v1_0.md"]],
          fixed = TRUE),
    grepl("index\\+24 hours.*sensitivity", doc_text[["SAP_v1_0.md"]],
          ignore.case = TRUE),
    grepl("index through index\\+6 hours", doc_text[["SAP_v1_0.md"]],
          ignore.case = TRUE),
    grepl("Primary pairing window:.*60 minutes", doc_text[["SAP_v1_0.md"]]),
    grepl("Sensitivity pairing window:.*30 minutes", doc_text[["SAP_v1_0.md"]]),
    grepl("In-hospital mortality after prediction time", doc_text[["SAP_v1_0.md"]],
          fixed = TRUE),
    grepl("Berlin-defined ARDS", doc_text[["terminology_ledger.md"]],
          fixed = TRUE),
    grepl("D029.*configuration v1\\.0\\.1", doc_text[["analysis_decision_log.md"]])
  )
)

sha256 <- function(path) {
  out <- system2("shasum", c("-a", "256", path), stdout = TRUE)
  strsplit(trimws(out[[1]]), "[[:space:]]+")[[1]][[1]]
}

manifest <- data.table(
  file = normalizePath(files),
  bytes = as.numeric(file.info(files)$size),
  sha256 = vapply(files, sha256, character(1)),
  config_version = LOCKED$version,
  lock_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)

fwrite(checks, file.path(QC_ROOT, "phase0_consistency_checks.csv"))
fwrite(manifest, file.path(QC_ROOT, "phase0_lock_manifest.csv"))

lines <- c(
  "# Phase-0 design-lock QC",
  "",
  sprintf("- Configuration: `%s`", LOCKED$version),
  sprintf("- Checks passed: %d/%d", sum(checks$passed), nrow(checks)),
  sprintf("- Lock files checksummed: %d", nrow(manifest)),
  "- This manifest covers design documents only; analysis-script checksums are frozen at the later outcome-unblinding checkpoint.",
  "",
  "The SAP is a retrospective amendment after legacy-result access. The checksum does not constitute prospective registration."
)
writeLines(lines, file.path(QC_ROOT, "phase0_lock_QC.md"), useBytes = TRUE)

if (!all(checks$passed)) {
  stop("Phase-0 lock failed; see phase0_consistency_checks.csv")
}
cat(sprintf("Phase-0 lock PASS: %d checks; %d files checksummed.\n",
            nrow(checks), nrow(manifest)))
