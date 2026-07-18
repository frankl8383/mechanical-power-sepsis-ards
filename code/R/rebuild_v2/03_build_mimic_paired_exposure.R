#!/usr/bin/env Rscript

# rebuild_v2 Phase 1C: outcome-blind 0-6 h MIMIC paired ventilator tuples.
# The audited v1 implementation is SHA-pinned and replayed with v2 filenames,
# v2 roots, and the broad v2 cohort as input.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/03_build_mimic_paired_exposure.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))
source(file.path(dirname(script_path), "00_phase1_replay_utils.R"))

set.seed(LOCKED_V2$bootstrap$seed_mimic)
run_transformed_v1_exposure("mimic")
