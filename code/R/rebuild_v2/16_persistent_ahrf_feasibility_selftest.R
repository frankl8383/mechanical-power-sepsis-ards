#!/usr/bin/env Rscript

# Lightweight audit of the completed persistent-AHRF feasibility gate.
# This script reads only completed rebuild-v2 artifacts and does not replay raw
# events, join new outcomes, fit models, or run a bootstrap.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/16_persistent_ahrf_feasibility_selftest.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))
source(file.path(dirname(script_path), "00_phase1_replay_utils.R"))

qc_dir <- file.path(QC_ROOT, "persistent_ahrf")
aggregate_dir <- file.path(AGGREGATE_ROOT, "persistent_ahrf")
gate_path <- file.path(
  qc_dir, "persistent_ahrf_72h_feasibility_complete_v2.csv"
)
private_manifest_path <- file.path(
  qc_dir, "persistent_ahrf_72h_private_manifest_v2.csv"
)
outcome_blind_manifest_path <- file.path(
  qc_dir, "persistent_ahrf_outcome_blind_artifact_manifest_v2.csv"
)
outcome_blind_invariants_path <- file.path(
  qc_dir, "persistent_ahrf_outcome_blind_invariants_v2.csv"
)
landmark_invariants_path <- file.path(
  qc_dir, "persistent_ahrf_72h_landmark_invariants_v2.csv"
)
gate_summary_path <- file.path(
  aggregate_dir, "persistent_ahrf_72h_gate_summary_v2.csv"
)

required <- c(
  gate_path, private_manifest_path, outcome_blind_manifest_path,
  outcome_blind_invariants_path, landmark_invariants_path, gate_summary_path
)
if (any(!file.exists(required))) {
  stop(
    "Missing persistent-AHRF audit inputs: ",
    paste(required[!file.exists(required)], collapse = ", ")
  )
}

gate <- fread(gate_path)
gate_summary <- fread(gate_summary_path)
private_manifest <- fread(private_manifest_path)
outcome_blind_manifest <- fread(outcome_blind_manifest_path)
outcome_blind_invariants <- fread(outcome_blind_invariants_path)
landmark_invariants <- fread(landmark_invariants_path)

as_named <- function(x) setNames(as.character(x$value), x$field)
g <- as_named(gate)
gs <- as_named(gate_summary)

manifest_hash_pass <- all(vapply(
  seq_len(nrow(private_manifest)),
  function(i) {
    file.exists(private_manifest$path[[i]]) &&
      identical(
        sha256_file(private_manifest$path[[i]]),
        private_manifest$sha256[[i]]
      )
  },
  logical(1)
))
outcome_blind_hash_pass <- all(vapply(
  seq_len(nrow(outcome_blind_manifest)),
  function(i) {
    file.exists(outcome_blind_manifest$path[[i]]) &&
      identical(
        sha256_file(outcome_blind_manifest$path[[i]]),
        outcome_blind_manifest$sha256[[i]]
      )
  },
  logical(1)
))

mimic_frame <- file.path(
  PRIVATE_ROOT, "persistent_ahrf",
  "mimic_persistent_ahrf_72h_analysis_frame_v2.rds"
)
eicu_frame <- file.path(
  PRIVATE_ROOT, "persistent_ahrf",
  "eicu_persistent_ahrf_72h_analysis_frame_v2.rds"
)

checks <- data.table(
  check = c(
    "completion_gate_is_stop",
    "event_gate_failed",
    "hospital_gate_passed",
    "event_count_is_70",
    "hospital_count_is_26",
    "no_outcome_model_or_bootstrap_run",
    "outcome_blind_invariants_all_pass",
    "landmark_invariants_all_pass",
    "private_manifest_hashes_match",
    "outcome_blind_manifest_hashes_match",
    "outcome_blind_artifacts_precede_outcome_read",
    "stopped_analysis_frames_absent",
    "gate_and_summary_status_agree"
  ),
  pass = c(
    identical(g[["analysis_gate_status"]], "STOP"),
    identical(g[["eicu_event_gate_pass"]], "FALSE"),
    identical(g[["eicu_hospital_gate_pass"]], "TRUE"),
    identical(g[["eicu_post_landmark_death_n"]], "70"),
    identical(g[["eicu_contributing_hospital_n"]], "26"),
    identical(g[["outcome_model_or_bootstrap_run"]], "FALSE"),
    all(outcome_blind_invariants$pass),
    all(landmark_invariants$pass),
    manifest_hash_pass,
    outcome_blind_hash_pass,
    all(
      outcome_blind_manifest$outcome_artifact_read_before_publish == FALSE
    ),
    !file.exists(mimic_frame) && !file.exists(eicu_frame),
    identical(g[["analysis_gate_status"]], gs[["status"]])
  )
)

out_path <- file.path(
  qc_dir, "persistent_ahrf_72h_feasibility_selftest_v2.csv"
)
fwrite(checks, out_path)
if (!all(checks$pass)) {
  stop(
    "Persistent-AHRF self-test failure: ",
    paste(checks[pass == FALSE, check], collapse = ", ")
  )
}

message("Persistent-AHRF feasibility self-test PASS.")
