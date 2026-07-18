#!/usr/bin/env Rscript

# Outcome-free representation audit for the fixed-6-hour tuple cohorts.
# Produces only disclosure-safe aggregate distributions, correlations,
# collinearity diagnostics, and source hashes.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/02b_outcome_free_representation_audit.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))
source(file.path(dirname(script_path), "01_analysis_utils.R"))

sha256_file <- function(path) {
  output <- system2(
    "shasum",
    c("-a", "256", shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA256 failed for ", path, ": ", paste(output, collapse = " "))
  }
  hash <- strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
  if (!grepl("^[0-9a-f]{64}$", hash)) stop("Invalid SHA256: ", path)
  hash
}

input_paths <- c(
  "MIMIC-IV" = file.path(
    PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds"
  ),
  "eICU-CRD" = file.path(
    PRIVATE_ROOT, "eicu", "eicu_no_gcs_core_targets_v2.rds"
  )
)
if (any(!file.exists(input_paths))) {
  stop(
    "Missing representation input(s): ",
    paste(input_paths[!file.exists(input_paths)], collapse = ", ")
  )
}

forbidden_pattern <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv"),
  collapse = "|"
)
variables <- c(
  "smp", "four_dprr", "driving_pressure", "rr",
  "static_power", "dynamic_power", "resistive_power",
  "smp_per_compliance"
)
primary_variables <- setdiff(variables, "smp_per_compliance")
quantile_probabilities <- c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1)

distribution_rows <- list()
correlation_rows <- list()
collinearity_rows <- list()
vif_rows <- list()
identity_rows <- list()

for (database in names(input_paths)) {
  frame <- as.data.frame(readRDS(input_paths[[database]]))
  if (any(grepl(
    forbidden_pattern, names(frame), ignore.case = TRUE, perl = TRUE
  ))) {
    stop(database, " tuple target contains an outcome-like column.")
  }
  if (!"rr" %in% names(frame) && "rr_value" %in% names(frame)) {
    frame$rr <- frame$rr_value
  }
  v2_require_columns(
    frame,
    c(variables, "energy_identity_error", "energy_identity_pass"),
    paste(database, " representation frame")
  )
  if (!all(frame$energy_identity_pass)) {
    stop(database, " contains a failed algebraic identity.")
  }
  if (any(!stats::complete.cases(frame[primary_variables]))) {
    stop(database, " primary representation variables are incomplete.")
  }

  distribution_rows[[database]] <- rbindlist(lapply(
    variables,
    function(variable) {
      source_value <- frame[[variable]]
      value <- source_value[!is.na(source_value) & is.finite(source_value)]
      if (!length(value)) {
        stop(database, " has no finite values for ", variable)
      }
      quantiles <- as.numeric(stats::quantile(
        value,
        probs = quantile_probabilities,
        names = FALSE,
        type = 2L
      ))
      data.table(
        database = database,
        variable = variable,
        n = length(source_value),
        available_n = length(value),
        missing_n = length(source_value) - length(value),
        mean = mean(value),
        standard_deviation = stats::sd(value),
        minimum = quantiles[[1L]],
        p05 = quantiles[[2L]],
        p25 = quantiles[[3L]],
        median = quantiles[[4L]],
        p75 = quantiles[[5L]],
        p95 = quantiles[[6L]],
        maximum = quantiles[[7L]]
      )
    }
  ))

  correlation <- stats::cor(
    frame[variables],
    use = "pairwise.complete.obs"
  )
  correlation_rows[[database]] <- rbindlist(lapply(
    seq_len(nrow(correlation)),
    function(i) {
      data.table(
        database = database,
        variable_1 = rownames(correlation)[[i]],
        variable_2 = colnames(correlation),
        correlation = as.numeric(correlation[i, ])
      )
    }
  ))

  audits <- list(
    v2_increment_collinearity_audit(
      frame,
      c("driving_pressure", "rr"),
      audit_id = "M_DPRR_increment"
    ),
    v2_increment_collinearity_audit(
      frame,
      c("static_power", "dynamic_power", "resistive_power"),
      audit_id = "M_ENERGY_increment"
    )
  )
  collinearity_rows[[database]] <- rbindlist(lapply(
    audits,
    function(audit) {
      out <- as.data.table(audit$summary)
      out[, database := database]
      setcolorder(out, c("database", setdiff(names(out), "database")))
      out
    }
  ), use.names = TRUE)
  vif_rows[[database]] <- rbindlist(lapply(
    audits,
    function(audit) {
      out <- as.data.table(audit$vif)
      out[, audit_id := audit$summary$audit_id[[1L]]]
      out[, database := database]
      setcolorder(out, c("database", "audit_id", "term", "vif"))
      out
    }
  ))
  identity_rows[[database]] <- data.table(
    database = database,
    n = nrow(frame),
    maximum_absolute_identity_error =
      max(abs(frame$energy_identity_error)),
    all_identity_checks_pass = all(frame$energy_identity_pass),
    all_primary_representation_values_complete =
      all(stats::complete.cases(frame[primary_variables]))
  )
}

out_dir <- file.path(AGGREGATE_ROOT, "representation_audit")
qc_dir <- file.path(QC_ROOT, "representation_audit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)

fwrite(
  rbindlist(distribution_rows),
  file.path(out_dir, "fixed6h_representation_distributions_v2.csv")
)
fwrite(
  rbindlist(correlation_rows),
  file.path(out_dir, "fixed6h_representation_correlations_v2.csv")
)
fwrite(
  rbindlist(collinearity_rows),
  file.path(out_dir, "fixed6h_representation_collinearity_v2.csv")
)
fwrite(
  rbindlist(vif_rows),
  file.path(out_dir, "fixed6h_representation_vif_v2.csv")
)
fwrite(
  rbindlist(identity_rows),
  file.path(qc_dir, "fixed6h_representation_identity_gate_v2.csv")
)

manifest <- data.table(
  field = c(
    "locked_config_version",
    "mimic_input_sha256",
    "eicu_input_sha256",
    "script_sha256",
    "outcome_fields_read",
    "all_identity_checks_pass",
    "completed_at"
  ),
  value = c(
    LOCKED_V2$version,
    sha256_file(input_paths[["MIMIC-IV"]]),
    sha256_file(input_paths[["eICU-CRD"]]),
    sha256_file(script_path),
    "FALSE",
    as.character(all(rbindlist(identity_rows)$all_identity_checks_pass)),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
)
fwrite(
  manifest,
  file.path(qc_dir, "fixed6h_representation_audit_complete_v2.csv")
)

message("Outcome-free fixed-landmark representation audit complete.")
