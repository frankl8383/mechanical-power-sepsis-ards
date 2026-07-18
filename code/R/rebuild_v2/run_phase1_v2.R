#!/usr/bin/env Rscript

# Execute the rebuild_v2 cohort, paired-exposure, fixed-landmark, and
# outcome-blind no-GCS severity-core chain.
# Every component script publishes its own checksum-bearing completion gate.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/run_phase1_v2.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)

steps <- c(
  "01_build_mimic_index_cohort.R",
  "02_build_eicu_index_cohort.R",
  "03_build_mimic_paired_exposure.R",
  "04_build_eicu_paired_exposure.R",
  "05_build_fixed_landmark_flow.R",
  "06_build_no_gcs_severity_core.R"
)

for (step in steps) {
  path <- file.path(script_dir, step)
  if (!file.exists(path)) stop("Missing rebuild_v2 step: ", path)
  message("\n=== Running ", step, " ===")
  status <- system2("Rscript", path)
  if (!identical(status, 0L)) {
    stop("rebuild_v2 Phase-1 chain failed in ", step, " (status ", status, ")")
  }
}

message(
  "\nrebuild_v2 broad-cohort/tuple/fixed-landmark/no-GCS chain completed."
)
