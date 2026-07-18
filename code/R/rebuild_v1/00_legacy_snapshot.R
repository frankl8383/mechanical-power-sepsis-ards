#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "00_legacy_snapshot.R"
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
source(file.path(script_dir, "00_config.R"))

ck <- file.path(PROJECT_ROOT, "checkpoints")
cohort <- as.data.table(readRDS(file.path(ck, "cohort_ids.rds")))
mc <- as.data.table(readRDS(file.path(ck, "modeling_cohort.rds")))
em <- as.data.table(readRDS(file.path(ck, "eicu_analysis_master.rds")))
ec <- as.data.table(readRDS(file.path(ck, "eicu_center.rds")))
sm <- as.data.table(readRDS(file.path(ck, "sofa_mimic.rds")))
se <- as.data.table(readRDS(file.path(ck, "sofa_eicu.rds")))

metric <- function(database, domain, name, value, denominator = NA_real_, note = "") {
  data.table(
    database = database,
    domain = domain,
    metric = name,
    value = as.numeric(value),
    denominator = as.numeric(denominator),
    percent = ifelse(is.na(denominator) || denominator == 0, NA_real_, 100 * value / denominator),
    note = note
  )
}

rows <- list(
  metric("MIMIC-IV", "legacy cohort", "qualifying_stays", nrow(cohort)),
  metric("MIMIC-IV", "legacy cohort", "unique_patients", uniqueN(cohort$subject_id)),
  metric("MIMIC-IV", "legacy cohort", "patients_with_multiple_stays", cohort[, .N, by = subject_id][N > 1, .N], uniqueN(cohort$subject_id)),
  metric("MIMIC-IV", "legacy model", "complete_case_stays", nrow(mc)),
  metric("MIMIC-IV", "legacy model", "unique_patients", uniqueN(mc$subject_id)),
  metric("MIMIC-IV", "legacy phenotype", "pf_missing", sum(is.na(mc$pf_day1_min)), nrow(mc)),
  metric("MIMIC-IV", "legacy phenotype", "pf_above_300", sum(mc$pf_day1_min > 300, na.rm = TRUE), nrow(mc)),
  metric("MIMIC-IV", "legacy outcome", "in_hospital_deaths", sum(mc$died_hosp == 1, na.rm = TRUE), sum(!is.na(mc$died_hosp))),
  metric("MIMIC-IV", "legacy outcome", "28_day_deaths", sum(mc$died_28d == 1, na.rm = TRUE), sum(!is.na(mc$died_28d))),
  metric("eICU-CRD", "legacy cohort", "candidate_stays", nrow(em)),
  metric("eICU-CRD", "legacy model", "primary_complete_cases", sum(em$has_primary == 1, na.rm = TRUE), nrow(em)),
  metric("eICU-CRD", "legacy phenotype", "primary_pf_above_300", em[has_primary == 1 & pf_day1_min > 300, .N], em[has_primary == 1, .N]),
  metric("eICU-CRD", "legacy phenotype", "primary_peep_below_5", em[has_primary == 1 & peep_use < 5, .N], em[has_primary == 1, .N]),
  metric("eICU-CRD", "legacy exposure", "primary_ppeak_below_plateau", em[has_primary == 1 & ppeak < plat, .N], em[has_primary == 1, .N]),
  metric("eICU-CRD", "legacy outcome", "in_hospital_deaths", ec[died_hosp == 1, .N], nrow(ec)),
  metric("MIMIC-IV", "legacy severity", "bilirubin_missing", sum(is.na(sm$bili)), nrow(sm)),
  metric("MIMIC-IV", "legacy severity", "urine_missing", sum(is.na(sm$urine_24h)), nrow(sm)),
  metric("eICU-CRD", "legacy severity", "bilirubin_missing", sum(is.na(se$bilirubin)), nrow(se)),
  metric("eICU-CRD", "legacy severity", "urine_missing", sum(is.na(se$urine)), nrow(se))
)

out <- rbindlist(rows, fill = TRUE)
fwrite(out, file.path(QC_ROOT, "legacy_snapshot_metrics.csv"))

fmt <- function(x) ifelse(is.na(x), "NA", format(round(x, 1), nsmall = 1, trim = TRUE))
table_lines <- c(
  "| Database | Domain | Metric | n/value | Denominator | Percent |",
  "|---|---|---|---:|---:|---:|",
  vapply(seq_len(nrow(out)), function(i) {
    sprintf(
      "| %s | %s | `%s` | %s | %s | %s |",
      out$database[i], out$domain[i], out$metric[i],
      format(out$value[i], scientific = FALSE, trim = TRUE),
      ifelse(is.na(out$denominator[i]), "—", format(out$denominator[i], scientific = FALSE, trim = TRUE)),
      ifelse(is.na(out$percent[i]), "—", paste0(fmt(out$percent[i]), "%"))
    )
  }, character(1))
)

writeLines(c(
  "# Legacy-analysis snapshot before rebuild v1",
  "",
  "This file freezes aggregate facts from the legacy checkpoints before any rebuilt cohort result is examined. It is a reconciliation baseline, not a validated result table.",
  "",
  table_lines,
  "",
  "## Pre-specified reasons for rebuild",
  "",
  "- The legacy MIMIC analysis includes repeated stays from the same patient.",
  "- Legacy MIMIC/eICU phenotype rows include missing or >300 day-1 P/F values despite a claimed baseline P/F threshold.",
  "- Legacy eICU enrollment did not require time-paired PEEP >=5 and includes physiologically inconsistent component-median combinations.",
  "- Severity components have substantial missingness and require transparent harmonization.",
  "",
  "All rebuilt results will be compared with this snapshot without overwriting it."
), file.path(QC_ROOT, "legacy_snapshot_QC.md"), useBytes = TRUE)

cat("Legacy snapshot written.\n")
