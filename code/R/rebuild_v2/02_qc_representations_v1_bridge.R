#!/usr/bin/env Rscript

# Validate the rebuild-v2 ventilator representations on the already audited,
# outcome-free rebuild-v1 selected tuples. This is a structural bridge only:
# it does not define the rebuild-v2 target population and writes no row-level
# copy of the source data.

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/02_qc_representations_v1_bridge.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))

output_dir <- file.path(QC_ROOT, "representation_bridge")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

inputs <- c(
  MIMIC = file.path(
    REBUILD_V1_ROOT, "private", "mimic",
    "mimic_paired_exposure_with_severity_core_v1.rds"
  ),
  eICU = file.path(
    REBUILD_V1_ROOT, "private", "eicu",
    "eicu_paired_exposure_with_severity_core_v1.rds"
  )
)
if (any(!file.exists(inputs))) {
  stop("Missing rebuild-v1 bridge input(s): ",
       paste(inputs[!file.exists(inputs)], collapse = ", "))
}

summarize_one <- function(database, path) {
  x <- readRDS(path)
  required <- c(
    "pplat", "ppeak_value", "peep_value", "vt_value", "rr_value",
    "delta_p", "resistive_pressure", "smp"
  )
  v2_require_columns(x, required, paste(database, "bridge input"))
  derived <- v2_derive_ventilator_representations(
    x,
    plateau = "pplat",
    peak = "ppeak_value",
    peep = "peep_value",
    tidal_volume = "vt_value",
    respiratory_rate = "rr_value"
  )
  if (!all(derived$tuple_valid) || !all(derived$energy_identity_pass)) {
    stop(database, " contains an invalid previously selected tuple.")
  }
  differences <- c(
    delta_p = max(abs(derived$driving_pressure - x$delta_p), na.rm = TRUE),
    resistive_pressure = max(
      abs(derived$resistive_pressure - x$resistive_pressure), na.rm = TRUE
    ),
    smp = max(abs(derived$smp - x$smp), na.rm = TRUE)
  )
  if (any(!is.finite(differences)) || any(differences > 1e-10)) {
    stop(database, " bridge representation differs from rebuild-v1.")
  }

  identity <- data.frame(
    database = database,
    n_selected_tuples = nrow(derived),
    n_valid_v2 = sum(derived$tuple_valid),
    n_energy_identity_pass = sum(derived$energy_identity_pass),
    maximum_energy_identity_error =
      max(abs(derived$energy_identity_error), na.rm = TRUE),
    maximum_existing_delta_p_difference = differences[["delta_p"]],
    maximum_existing_resistive_pressure_difference =
      differences[["resistive_pressure"]],
    maximum_existing_smp_difference = differences[["smp"]],
    compliance_normalized_available =
      sum(is.finite(derived$smp_per_compliance)),
    stringsAsFactors = FALSE
  )

  variables <- c(
    "smp", "four_dprr", "driving_pressure", "rr_value",
    "static_power", "dynamic_power", "resistive_power",
    "smp_per_compliance"
  )
  distributions <- do.call(rbind, lapply(variables, function(variable) {
    values <- if (variable == "rr_value") {
      derived$rr_value
    } else {
      derived[[variable]]
    }
    values <- values[is.finite(values)]
    quantiles <- stats::quantile(
      values, c(0.05, 0.25, 0.50, 0.75, 0.95),
      names = FALSE, type = 2L
    )
    data.frame(
      database = database,
      variable = variable,
      n = length(values),
      mean = mean(values),
      sd = stats::sd(values),
      p05 = quantiles[1L],
      p25 = quantiles[2L],
      median = quantiles[3L],
      p75 = quantiles[4L],
      p95 = quantiles[5L],
      stringsAsFactors = FALSE
    )
  }))

  correlation_variables <- c(
    "smp", "four_dprr", "driving_pressure", "rr_value",
    "static_power", "dynamic_power", "resistive_power"
  )
  correlation_frame <- derived[correlation_variables]
  correlation_matrix <- stats::cor(
    correlation_frame, use = "pairwise.complete.obs", method = "spearman"
  )
  correlations <- as.data.frame(as.table(correlation_matrix))
  names(correlations) <- c("variable_1", "variable_2", "spearman_rho")
  correlations$database <- database
  correlations <- correlations[
    match(correlations$variable_1, correlation_variables) <
      match(correlations$variable_2, correlation_variables),
    c("database", "variable_1", "variable_2", "spearman_rho")
  ]

  list(
    identity = identity,
    distributions = distributions,
    correlations = correlations
  )
}

results <- Map(summarize_one, names(inputs), unname(inputs))
identity <- do.call(rbind, lapply(results, `[[`, "identity"))
distributions <- do.call(rbind, lapply(results, `[[`, "distributions"))
correlations <- do.call(rbind, lapply(results, `[[`, "correlations"))

write.csv(
  identity,
  file.path(output_dir, "representation_identity_qc_v2.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  distributions,
  file.path(output_dir, "representation_distribution_qc_v2.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  correlations,
  file.path(output_dir, "representation_correlation_qc_v2.csv"),
  row.names = FALSE, na = ""
)

completion <- data.frame(
  field = c(
    "status", "config_version", "source_role",
    "mimic_rows", "eicu_rows", "all_identity_checks_pass",
    "row_level_output_written"
  ),
  value = c(
    "PASS", LOCKED_V2$version,
    "structural bridge only; not the rebuild-v2 target cohort",
    identity$n_selected_tuples[identity$database == "MIMIC"],
    identity$n_selected_tuples[identity$database == "eICU"],
    "TRUE", "FALSE"
  ),
  stringsAsFactors = FALSE
)
write.csv(
  completion,
  file.path(output_dir, "representation_bridge_complete_v2.csv"),
  row.names = FALSE, na = ""
)

cat("REBUILD_V2_REPRESENTATION_BRIDGE_PASS\n")

