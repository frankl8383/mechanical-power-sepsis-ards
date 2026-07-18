#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2: exact primary-common-set descriptives.
#
# This reporting script joins the frozen primary analysis frames to their
# fixed-landmark tuple targets. It does not refit a model or select a cohort.

suppressPackageStartupMessages(library(data.table))
options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/12_build_primary_descriptives.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required.")
}
sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}
atomic_write_csv <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  unlink(temporary, force = TRUE)
  fwrite(object, temporary)
  if (!file.rename(temporary, path)) {
    unlink(temporary, force = TRUE)
    stop("Atomic CSV rename failed: ", path)
  }
  invisible(path)
}

input_paths <- list(
  primary_model_gate = file.path(
    QC_ROOT, "primary_models", "phase4_primary_models_complete_v2.csv"
  ),
  primary_freeze_gate = file.path(
    QC_ROOT, "primary_model_freeze",
    "phase3_primary_model_freeze_complete_v2.csv"
  ),
  fixed_landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  mimic_analysis = file.path(
    PRIVATE_ROOT, "primary_models", "mimic_primary_analysis_frame_v2.rds"
  ),
  eicu_analysis = file.path(
    PRIVATE_ROOT, "primary_models", "eicu_primary_analysis_frame_v2.rds"
  ),
  mimic_target = file.path(
    PRIVATE_ROOT, "mimic", "mimic_no_gcs_core_targets_v2.rds"
  ),
  eicu_target = file.path(
    PRIVATE_ROOT, "eicu", "eicu_no_gcs_core_targets_v2.rds"
  )
)
missing <- names(input_paths)[!file.exists(unlist(input_paths))]
if (length(missing)) {
  stop(
    "Primary descriptive input(s) unavailable: ",
    paste(missing, collapse = ", ")
  )
}

read_field_gate <- function(path) {
  gate <- fread(path, colClasses = "character", showProgress = FALSE)
  if (!identical(names(gate), c("field", "value")) ||
      anyDuplicated(gate$field)) {
    stop("Malformed field/value gate: ", path)
  }
  setNames(as.character(gate$value), gate$field)
}
model_gate <- read_field_gate(input_paths$primary_model_gate)
freeze_gate <- read_field_gate(input_paths$primary_freeze_gate)
landmark_gate <- read_field_gate(input_paths$fixed_landmark_gate)
if (!identical(model_gate[["status"]], "PASS") ||
    !identical(
      model_gate[["locked_config_version"]], LOCKED_V2$version
    ) ||
    !identical(freeze_gate[["status"]], "PASS") ||
    !identical(
      freeze_gate[["locked_config_version"]], LOCKED_V2$version
    ) ||
    !identical(
      landmark_gate[["locked_config_version"]], LOCKED_V2$version
    ) ||
    !identical(landmark_gate[["all_energy_identities_pass"]], "TRUE") ||
    !identical(
      landmark_gate[["mimic_target_sha256"]],
      sha256_file(input_paths$mimic_target)
    ) ||
    !identical(
      landmark_gate[["eicu_target_sha256"]],
      sha256_file(input_paths$eicu_target)
    )) {
  stop("An upstream primary-analysis gate is not a locked PASS.")
}

prepare_database <- function(database) {
  mimic <- identical(database, "MIMIC-IV")
  analysis <- as.data.table(readRDS(
    input_paths[[if (mimic) "mimic_analysis" else "eicu_analysis"]]
  ))
  target <- as.data.table(readRDS(
    input_paths[[if (mimic) "mimic_target" else "eicu_target"]]
  ))
  id <- if (mimic) "stay_id" else "patientunitstayid"
  target[, analysis_id := as.character(get(id))]
  target <- target[analysis_id %chin% analysis$analysis_id]
  if (anyDuplicated(analysis$analysis_id) ||
      anyDuplicated(target$analysis_id) ||
      nrow(target) != nrow(analysis) ||
      !setequal(target$analysis_id, analysis$analysis_id)) {
    stop(database, " analysis/target ID join is not exact.")
  }
  keep <- c(
    "analysis_id", "pplat", "ppeak_value", "peep_value", "vt_value",
    "compliance_L_per_cmH2O", "smp_per_compliance"
  )
  joined <- merge(
    analysis, target[, ..keep],
    by = "analysis_id", all = FALSE, sort = FALSE
  )
  if (nrow(joined) != nrow(analysis) ||
      anyNA(joined[, c(
        "pplat", "ppeak_value", "peep_value", "vt_value"
      )])) {
    stop(database, " exact descriptive join failed.")
  }
  # Compliance and compliance-normalized sMP are undefined when the recorded
  # plateau pressure does not exceed PEEP. Preserve those structural missing
  # values and report their denominators instead of silently imputing them.
  compliance_missing <- !is.finite(joined$compliance_L_per_cmH2O) |
    !is.finite(joined$smp_per_compliance)
  if (any(
      xor(
        is.finite(joined$compliance_L_per_cmH2O),
        is.finite(joined$smp_per_compliance)
      )
    )) {
    stop(database, " compliance-derived variables have discordant support.")
  }
  joined[, compliance_structural_missing := compliance_missing]
  joined[, compliance_mL_per_cmH2O := 1000 * compliance_L_per_cmH2O]
  joined[, tuple_from_index_minutes := if (mimic) {
    (tuple_available_time_value - index_time_value) / 60
  } else {
    tuple_available_time_value - index_time_value
  }]
  if (any(!is.finite(joined$tuple_from_index_minutes)) ||
      any(joined$tuple_from_index_minutes < 0) ||
      any(joined$tuple_from_index_minutes > 360)) {
    stop(database, " tuple timing is outside the fixed window.")
  }
  joined
}

mimic <- prepare_database("MIMIC-IV")
eicu <- prepare_database("eICU-CRD")

continuous_dictionary <- data.table(
  variable = c(
    "age", "pf_ratio", "map", "platelet", "creatinine",
    "pplat", "ppeak_value", "peep_value", "vt_value",
    "driving_pressure", "rr", "smp", "four_dprr",
    "static_power", "dynamic_power", "resistive_power",
    "compliance_mL_per_cmH2O", "smp_per_compliance",
    "tuple_from_index_minutes"
  ),
  characteristic = c(
    "Age", "PaO2/FiO2 ratio at index", "Minimum mean arterial pressure",
    "Minimum platelet count", "Maximum serum creatinine",
    "Plateau pressure", "Peak inspiratory pressure", "PEEP",
    "Tidal volume", "Driving pressure", "Respiratory rate",
    "Surrogate mechanical power", "4 x driving pressure + respiratory rate",
    "Static-elastic algebraic term", "Dynamic-elastic algebraic term",
    "Resistive algebraic term", "Respiratory-system compliance",
    "Compliance-normalized surrogate mechanical power",
    "Tuple availability after index"
  ),
  unit = c(
    "years", "mm Hg", "mm Hg", "x10^3/uL", "mg/dL",
    "cm H2O", "cm H2O", "cm H2O", "mL", "cm H2O",
    "breaths/min", "J/min", "unitless score", "J/min", "J/min",
    "J/min", "mL/cm H2O", "J*cm H2O/(min*L)", "minutes"
  )
)
categorical_dictionary <- data.table(
  variable = c("sex_female", "vasopressor", "outcome"),
  characteristic = c(
    "Female sex", "Any vasopressor exposure",
    "Post-landmark in-hospital death"
  )
)

continuous_summary <- rbindlist(
  lapply(
    list(`MIMIC-IV` = mimic, `eICU-CRD` = eicu),
    function(frame) {
      rbindlist(lapply(
        seq_len(nrow(continuous_dictionary)),
        function(i) {
          item <- continuous_dictionary[i]
          raw_value <- as.numeric(frame[[item$variable]])
          value <- raw_value[is.finite(raw_value)]
          if (!length(value)) {
            stop("No finite values available for ", item$variable, ".")
          }
          quantiles <- as.numeric(quantile(
            value, c(0, .25, .5, .75, 1),
            names = FALSE, type = 2
          ))
          data.table(
            variable = item$variable,
            characteristic = item$characteristic,
            unit = item$unit,
            n = length(value),
            missing = length(raw_value) - length(value),
            mean = mean(value),
            standard_deviation = sd(value),
            minimum = quantiles[1L],
            q1 = quantiles[2L],
            median = quantiles[3L],
            q3 = quantiles[4L],
            maximum = quantiles[5L]
          )
        }
      ))
    }
  ),
  idcol = "database"
)

categorical_summary <- rbindlist(
  lapply(
    list(`MIMIC-IV` = mimic, `eICU-CRD` = eicu),
    function(frame) {
      rbindlist(lapply(
        seq_len(nrow(categorical_dictionary)),
        function(i) {
          item <- categorical_dictionary[i]
          value <- as.integer(frame[[item$variable]])
          if (anyNA(value) || any(!value %in% c(0L, 1L))) {
            stop("Non-binary value in ", item$variable, ".")
          }
          data.table(
            variable = item$variable,
            characteristic = item$characteristic,
            denominator = length(value),
            count = sum(value),
            percent = 100 * mean(value)
          )
        }
      ))
    }
  ),
  idcol = "database"
)

format_number <- function(x, digits) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}
digits_for <- function(variable) {
  if (variable %in% c(
      "creatinine", "smp", "static_power", "dynamic_power",
      "resistive_power", "smp_per_compliance",
      "compliance_L_per_cmH2O"
    )) {
    return(1L)
  }
  0L
}
formatted_continuous <- copy(continuous_summary)
formatted_continuous[, display := {
  digits <- digits_for(variable)
  paste0(
    format_number(median, digits), " (",
    format_number(q1, digits), "-", format_number(q3, digits), ")"
  )
}, by = seq_len(nrow(formatted_continuous))]
formatted_categorical <- copy(categorical_summary)
formatted_categorical[, display := paste0(
  formatC(count, format = "d", big.mark = ","),
  " (", format_number(percent, 1L), ")"
)]
formatted_table <- rbindlist(list(
  formatted_continuous[, .(
    characteristic, unit, database, display,
    display_order = match(variable, continuous_dictionary$variable)
  )],
  formatted_categorical[, .(
    characteristic, unit = "No. (%)", database, display,
    display_order =
      nrow(continuous_dictionary) +
      match(variable, categorical_dictionary$variable)
  )]
))
formatted_table <- dcast(
  formatted_table,
  characteristic + unit + display_order ~ database,
  value.var = "display"
)
setorder(formatted_table, display_order)
formatted_table[, display_order := NULL]
setcolorder(
  formatted_table,
  c("characteristic", "unit", "MIMIC-IV", "eICU-CRD")
)

hospital_summary <- rbindlist(list(
  data.table(
    database = "MIMIC-IV",
    patients = nrow(mimic),
    deaths = sum(mimic$outcome),
    hospitals = uniqueN(mimic$hospital_id),
    median_patients_per_hospital = nrow(mimic),
    q1_patients_per_hospital = nrow(mimic),
    q3_patients_per_hospital = nrow(mimic),
    largest_hospital_n = nrow(mimic),
    largest_hospital_percent = 100
  ),
  {
    center_n <- eicu[, .N, by = hospital_id]$N
    data.table(
      database = "eICU-CRD",
      patients = nrow(eicu),
      deaths = sum(eicu$outcome),
      hospitals = uniqueN(eicu$hospital_id),
      median_patients_per_hospital =
        as.numeric(quantile(center_n, .5, type = 2)),
      q1_patients_per_hospital =
        as.numeric(quantile(center_n, .25, type = 2)),
      q3_patients_per_hospital =
        as.numeric(quantile(center_n, .75, type = 2)),
      largest_hospital_n = max(center_n),
      largest_hospital_percent = 100 * max(center_n) / nrow(eicu)
    )
  }
))

aggregate_out <- file.path(AGGREGATE_ROOT, "descriptives")
qc_out <- file.path(QC_ROOT, "descriptives")
dir.create(aggregate_out, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_out, recursive = TRUE, showWarnings = FALSE)
completion_gate <- file.path(
  qc_out, "primary_descriptives_complete_v2.csv"
)
unlink(completion_gate, force = TRUE)

output_paths <- list(
  continuous = file.path(
    aggregate_out, "primary_common_set_continuous_v2.csv"
  ),
  categorical = file.path(
    aggregate_out, "primary_common_set_categorical_v2.csv"
  ),
  formatted_table = file.path(
    aggregate_out, "Table1_primary_common_set_v2.csv"
  ),
  hospital = file.path(
    aggregate_out, "primary_common_set_hospital_support_v2.csv"
  )
)
atomic_write_csv(continuous_summary, output_paths$continuous)
atomic_write_csv(categorical_summary, output_paths$categorical)
atomic_write_csv(formatted_table, output_paths$formatted_table)
atomic_write_csv(hospital_summary, output_paths$hospital)

input_manifest <- data.table(
  input_name = names(input_paths),
  path = normalizePath(unlist(input_paths), mustWork = TRUE),
  sha256 = vapply(input_paths, sha256_file, character(1L))
)
output_manifest <- data.table(
  output_name = names(output_paths),
  path = normalizePath(unlist(output_paths), mustWork = TRUE),
  sha256 = vapply(output_paths, sha256_file, character(1L))
)
atomic_write_csv(
  input_manifest,
  file.path(qc_out, "primary_descriptives_input_manifest_v2.csv")
)
atomic_write_csv(
  output_manifest,
  file.path(qc_out, "primary_descriptives_output_manifest_v2.csv")
)

gate <- data.table(
  field = c(
    "status", "locked_config_version", "script_sha256",
    "mimic_n", "mimic_events", "eicu_n", "eicu_events",
    "eicu_hospitals", "exact_id_join_pass",
    "no_missing_primary_tuple_variables",
    "mimic_compliance_structural_missing_n",
    "eicu_compliance_structural_missing_n",
    "formatted_table_sha256", "completed_at"
  ),
  value = c(
    "PASS", LOCKED_V2$version, sha256_file(script_path),
    nrow(mimic), sum(mimic$outcome),
    nrow(eicu), sum(eicu$outcome), uniqueN(eicu$hospital_id),
    "TRUE", "TRUE",
    sum(mimic$compliance_structural_missing),
    sum(eicu$compliance_structural_missing),
    sha256_file(output_paths$formatted_table),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
)
atomic_write_csv(gate, completion_gate)

cat(
  "REBUILD_V2_PRIMARY_DESCRIPTIVES_PASS\n",
  "MIMIC-IV: n=", nrow(mimic),
  ", events=", sum(mimic$outcome), "\n",
  "eICU-CRD: n=", nrow(eicu),
  ", events=", sum(eicu$outcome),
  ", hospitals=", uniqueN(eicu$hospital_id), "\n",
  sep = ""
)
