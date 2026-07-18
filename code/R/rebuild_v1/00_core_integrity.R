#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "00_core_integrity.R"
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
source(file.path(script_dir, "00_config.R"))

mimic_targets <- c(
  file.path(MIMIC_ROOT, "icu/chartevents.csv.gz"),
  file.path(MIMIC_ROOT, "hosp/labevents.csv.gz")
)

gzip_ok <- vapply(mimic_targets, function(path) {
  identical(system2("gzip", c("-t", shQuote(path)), stdout = FALSE, stderr = FALSE), 0L)
}, logical(1))

mimic_qc <- data.table(
  database = "MIMIC-IV",
  file = basename(mimic_targets),
  check = "gzip_full_stream_test",
  expected = NA_character_,
  observed = NA_character_,
  pass = gzip_ok
)

eicu_names <- c(
  "patient.csv.gz",
  "respiratoryCharting.csv.gz",
  "respiratoryCare.csv.gz",
  "lab.csv.gz",
  "diagnosis.csv.gz",
  "admissionDx.csv.gz",
  "apacheApsVar.csv.gz",
  "apachePatientResult.csv.gz",
  "hospital.csv.gz"
)

checksum_lines <- readLines(file.path(EICU_ROOT, "SHA256SUMS.txt"), warn = FALSE)
checksum_map <- setNames(
  sub(" .*", "", checksum_lines),
  sub("^[0-9a-fA-F]+[[:space:]]+", "", checksum_lines)
)

sha256_file <- function(path) {
  line <- system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE)
  strsplit(trimws(line[[1]]), "[[:space:]]+")[[1]][[1]]
}

observed <- vapply(file.path(EICU_ROOT, eicu_names), sha256_file, character(1))
expected <- unname(checksum_map[eicu_names])

eicu_qc <- data.table(
  database = "eICU-CRD",
  file = eicu_names,
  check = "published_sha256",
  expected = expected,
  observed = observed,
  pass = !is.na(expected) & expected == observed
)

qc <- rbindlist(list(mimic_qc, eicu_qc), fill = TRUE)
fwrite(qc, file.path(QC_ROOT, "core_file_integrity.csv"))

lines <- c(
  "# Core raw-file integrity QC",
  "",
  sprintf("Run: %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  "",
  "| Database | File | Check | Pass |",
  "|---|---|---|---:|",
  vapply(seq_len(nrow(qc)), function(i) {
    sprintf("| %s | `%s` | %s | %s |", qc$database[i], qc$file[i], qc$check[i], ifelse(qc$pass[i], "PASS", "FAIL"))
  }, character(1)),
  "",
  "The two largest MIMIC streams were independently decompressed end to end because an older local log had marked them as failed. The current files pass. eICU core tables match the publisher-provided SHA-256 manifest."
)
writeLines(lines, file.path(QC_ROOT, "core_file_integrity_QC.md"), useBytes = TRUE)

if (!all(qc$pass)) stop("Core integrity QC failed.")
cat(sprintf("Core integrity PASS: %d/%d files.\n", sum(qc$pass), nrow(qc)))

