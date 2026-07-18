#!/usr/bin/env Rscript

# rebuild_v2 Phase 2B: outcome-blind tuple and joint-inclusion weights.
#
# This script may run only after the full landmark-at-risk no-GCS core has
# passed its own completion gate. It never opens an outcome artifact.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/07_build_selection_weights.R",
    mustWork = TRUE
  )
}
script_dir <- dirname(script_path)
source(file.path(script_dir, "00_config.R"))
source(file.path(script_dir, "01_analysis_utils.R"))
source(file.path(script_dir, "08_selection_utils.R"))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required for selection-weight gates.")
}
sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

set.seed(LOCKED_V2$bootstrap$seed_sensitivity)

private_out <- file.path(PRIVATE_ROOT, "selection_weights")
aggregate_out <- file.path(AGGREGATE_ROOT, "selection_weights")
qc_out <- file.path(QC_ROOT, "selection_weights")
for (d in c(private_out, aggregate_out, qc_out)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
completion_path <- file.path(qc_out, "selection_weights_complete_v2.csv")
completion_tmp <- paste0(completion_path, ".tmp")
unlink(c(completion_path, completion_tmp), force = TRUE)

input_paths <- list(
  fixed_landmark_gate = file.path(
    QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
  ),
  no_gcs_gate = file.path(
    QC_ROOT, "no_gcs_core", "phase2b_no_gcs_core_complete_v2.csv"
  ),
  mimic_target = file.path(
    PRIVATE_ROOT, "mimic",
    "mimic_all_landmark_at_risk_selection_targets_v2.rds"
  ),
  eicu_target = file.path(
    PRIVATE_ROOT, "eicu",
    "eicu_all_landmark_at_risk_selection_targets_v2.rds"
  ),
  mimic_core = file.path(
    PRIVATE_ROOT, "mimic",
    "mimic_fixed6h_all_at_risk_no_gcs_core_v2.rds"
  ),
  eicu_core = file.path(
    PRIVATE_ROOT, "eicu",
    "eicu_fixed6h_all_at_risk_no_gcs_core_v2.rds"
  )
)
missing_inputs <- names(input_paths)[!file.exists(unlist(input_paths))]
if (length(missing_inputs)) {
  stop(
    "Selection-weight input is missing: ",
    paste(missing_inputs, collapse = ", ")
  )
}

forbidden_name <- paste(
  c("mort", "death", "dead", "expire", "discharge", "surviv", "outcome"),
  collapse = "|"
)
assert_outcome_blind_object <- function(x, label) {
  bad <- names(x)[grepl(forbidden_name, names(x), ignore.case = TRUE)]
  if (length(bad)) {
    stop(
      label, " contains prohibited outcome-like field(s): ",
      paste(bad, collapse = ", ")
    )
  }
  invisible(TRUE)
}

gate_contains_hash <- function(path, expected_hash, label) {
  gate <- fread(path, showProgress = FALSE)
  values <- as.character(unlist(gate, use.names = FALSE))
  if (!expected_hash %in% values) {
    stop(label, " does not certify expected input SHA256: ", expected_hash)
  }
  invisible(TRUE)
}

fixed_gate <- fread(input_paths$fixed_landmark_gate, showProgress = FALSE)
if (!identical(names(fixed_gate), c("field", "value")) ||
    anyDuplicated(fixed_gate$field)) {
  stop("Malformed fixed-landmark completion gate.")
}
fixed_values <- setNames(fixed_gate$value, fixed_gate$field)
if (!identical(
  fixed_values[["locked_config_version"]], LOCKED_V2$version
)) {
  stop("Fixed-landmark gate does not match LOCKED_V2.")
}
no_gcs_gate <- fread(input_paths$no_gcs_gate, showProgress = FALSE)
if (!identical(names(no_gcs_gate), c("field", "value")) ||
    anyDuplicated(no_gcs_gate$field)) {
  stop("Malformed no-GCS-core completion gate.")
}
no_gcs_values <- setNames(no_gcs_gate$value, no_gcs_gate$field)
if (!identical(no_gcs_values[["status"]], "PASS") ||
    !identical(
      no_gcs_values[["locked_config_version"]], LOCKED_V2$version
    )) {
  stop("No-GCS-core gate is not a PASS for LOCKED_V2.")
}
gate_contains_hash(
  input_paths$fixed_landmark_gate,
  sha256_file(input_paths$mimic_target),
  "Fixed-landmark gate"
)
gate_contains_hash(
  input_paths$fixed_landmark_gate,
  sha256_file(input_paths$eicu_target),
  "Fixed-landmark gate"
)
gate_contains_hash(
  input_paths$no_gcs_gate,
  sha256_file(input_paths$mimic_core),
  "No-GCS core gate"
)
gate_contains_hash(
  input_paths$no_gcs_gate,
  sha256_file(input_paths$eicu_core),
  "No-GCS core gate"
)

make_selection_frame <- function(database) {
  mimic <- identical(database, "MIMIC-IV")
  target_path <- if (mimic) input_paths$mimic_target else input_paths$eicu_target
  core_path <- if (mimic) input_paths$mimic_core else input_paths$eicu_core
  id <- if (mimic) "stay_id" else "patientunitstayid"
  target <- as.data.table(readRDS(target_path))
  core <- as.data.table(readRDS(core_path))
  assert_outcome_blind_object(target, paste(database, "landmark target"))
  assert_outcome_blind_object(core, paste(database, "no-GCS core"))
  v2_require_columns(target, id, paste(database, "landmark target"))
  v2_require_columns(
    core,
    c(
      id, "hospital_id", "age", "sex", "sex_female", "pf_ratio",
      "map", "vasopressor", "platelet", "creatinine",
      "map_missing", "platelet_missing", "creatinine_missing",
      "complete_no_gcs_core", "tuple_observed",
      "tuple_and_complete_no_gcs_core", "index_time", "landmark_time"
    ),
    paste(database, "no-GCS core")
  )
  if (anyDuplicated(target[[id]]) || anyDuplicated(core[[id]]) ||
      anyNA(target[[id]]) || anyNA(core[[id]])) {
    stop(database, " input IDs must be complete and unique.")
  }
  if (!setequal(target[[id]], core[[id]]) ||
      nrow(target) != nrow(core)) {
    stop(database, " no-GCS core does not match the all-at-risk target.")
  }
  timing_fields <- if (mimic) {
    c(id, "icu_intime", "index_time", "tuple_observed")
  } else {
    c(id, "icu_intime_offset", "index_time", "tuple_observed")
  }
  v2_require_columns(target, timing_fields, paste(database, "landmark target"))
  timing <- target[, ..timing_fields]
  setnames(
    timing,
    c("index_time", "tuple_observed"),
    c("target_index_time", "target_tuple_observed")
  )
  x <- merge(core, timing, by = id, all = FALSE, sort = FALSE)
  if (nrow(x) != nrow(core)) stop(database, " timing join lost rows.")
  if (!all(
    as.integer(x$tuple_observed) ==
      as.integer(x$target_tuple_observed)
  )) {
    stop(database, " tuple flag differs between target and no-GCS core.")
  }
  if (mimic) {
    index_time <- as.POSIXct(x$index_time, tz = "UTC")
    target_index_time <- as.POSIXct(x$target_index_time, tz = "UTC")
    if (anyNA(index_time) || anyNA(target_index_time) ||
        !all(index_time == target_index_time)) {
      stop("MIMIC index time differs between target and core.")
    }
    index_hours <- as.numeric(difftime(
      index_time, as.POSIXct(x$icu_intime, tz = "UTC"), units = "hours"
    ))
  } else {
    core_index_time <- suppressWarnings(as.numeric(x$index_time))
    target_index_time <- suppressWarnings(as.numeric(x$target_index_time))
    if (anyNA(core_index_time) || anyNA(target_index_time) ||
        !all(core_index_time == target_index_time)) {
      stop("eICU index offset differs between target and core.")
    }
    index_hours <- (
      as.numeric(x$index_time) - as.numeric(x$icu_intime_offset)
    ) / 60
  }
  if (anyNA(index_hours) || any(!is.finite(index_hours)) ||
      any(index_hours < -1e-8)) {
    stop(database, " has invalid ICU-to-index times.")
  }

  sex_normalized <- tolower(trimws(as.character(x$sex)))
  sex_known <- as.integer(
    sex_normalized %in% c("f", "female", "m", "male")
  )
  sex_female_derived <- as.integer(
    sex_normalized %in% c("f", "female")
  )
  sex_unknown <- 1L - sex_known
  source_sex_female <- suppressWarnings(as.numeric(x$sex_female))
  known_sex_rows <- sex_known == 1L
  if (anyNA(source_sex_female[known_sex_rows]) ||
      !all(
        source_sex_female[known_sex_rows] ==
          sex_female_derived[known_sex_rows]
      )) {
    stop(database, " sex_female differs from recognized source sex.")
  }

  numeric_fields <- c(
    "age", "pf_ratio", "index_peep", "map", "vasopressor",
    "platelet", "creatinine", "map_missing", "platelet_missing",
    "creatinine_missing", "complete_no_gcs_core", "tuple_observed",
    "tuple_and_complete_no_gcs_core"
  )
  if (!"index_peep" %in% names(x)) {
    # The canonical core may retain the target name used at Phase 1.
    if ("peep_near_value" %in% names(x)) {
      x[, index_peep := as.numeric(peep_near_value)]
    } else {
      target_peep <- target[, c(id, "index_peep"), with = FALSE]
      x <- merge(x, target_peep, by = id, all = FALSE, sort = FALSE)
    }
  }
  for (field in numeric_fields) {
    x[, (field) := suppressWarnings(as.numeric(get(field)))]
  }
  required_finite_selection_fields <- c(
    "age", "pf_ratio", "index_peep"
  )
  for (field in required_finite_selection_fields) {
    if (anyNA(x[[field]]) || any(!is.finite(x[[field]]))) {
      stop(database, " has incomplete required selection field: ", field)
    }
  }
  binary_fields <- c(
    "vasopressor", "map_missing", "platelet_missing",
    "creatinine_missing", "complete_no_gcs_core", "tuple_observed",
    "tuple_and_complete_no_gcs_core"
  )
  for (field in binary_fields) {
    if (anyNA(x[[field]]) || !all(x[[field]] %in% c(0, 1))) {
      stop(database, " has an invalid binary field: ", field)
    }
  }
  expected_missing <- list(
    map_missing = is.na(x$map) | !is.finite(x$map),
    platelet_missing = is.na(x$platelet) | !is.finite(x$platelet),
    creatinine_missing = is.na(x$creatinine) | !is.finite(x$creatinine)
  )
  for (field in names(expected_missing)) {
    if (!all(as.logical(x[[field]]) == expected_missing[[field]])) {
      stop(database, " has an inconsistent missingness flag: ", field)
    }
  }
  expected_complete <- as.integer(
    sex_known == 1L &
      !is.na(x$age) & is.finite(x$age) &
      !is.na(x$pf_ratio) & is.finite(x$pf_ratio) &
      !expected_missing$map_missing &
      !expected_missing$platelet_missing &
      !expected_missing$creatinine_missing &
      !is.na(x$vasopressor) & is.finite(x$vasopressor) &
      x$vasopressor %in% c(0, 1)
  )
  if (!all(x$complete_no_gcs_core == expected_complete)) {
    stop(database, " complete_no_gcs_core is inconsistent.")
  }
  expected_joint <- as.integer(
    x$tuple_observed == 1L & x$complete_no_gcs_core == 1L
  )
  if (!all(x$tuple_and_complete_no_gcs_core == expected_joint)) {
    stop(database, " joint inclusion flag is inconsistent.")
  }

  data.table(
    analysis_id = as.character(x[[id]]),
    source_id = x[[id]],
    hospital = as.character(x$hospital_id),
    age = x$age,
    sex_female = sex_female_derived,
    sex_unknown = sex_unknown,
    sex_known = sex_known,
    pf_ratio = x$pf_ratio,
    index_peep = x$index_peep,
    index_hours_from_icu = index_hours,
    map = x$map,
    platelet = x$platelet,
    creatinine = x$creatinine,
    vasopressor = x$vasopressor,
    map_missing = x$map_missing,
    platelet_missing = x$platelet_missing,
    creatinine_missing = x$creatinine_missing,
    complete_no_gcs_core = x$complete_no_gcs_core,
    tuple_included = x$tuple_observed,
    joint_included = x$tuple_and_complete_no_gcs_core
  )
}

fit_one_target <- function(
    frame,
    database,
    target_name,
    inclusion,
    model_role,
    covariate_specification,
    endpoint_weight_eligible) {
  if (!covariate_specification %in%
      c("full_core_median_indicator", "always_observed_only")) {
    stop("Unknown selection covariate specification.")
  }
  if (!is.logical(endpoint_weight_eligible) ||
      length(endpoint_weight_eligible) != 1L ||
      is.na(endpoint_weight_eligible)) {
    stop("endpoint_weight_eligible must be one nonmissing logical.")
  }
  support <- v2_selection_common_support(
    frame, inclusion = inclusion, cluster = "hospital"
  )
  supported <- frame[support$keep]
  if (!nrow(supported)) stop(database, " has no supported rows for ", target_name)
  full_specification <- identical(
    covariate_specification, "full_core_median_indicator"
  )
  bundle <- v2_selection_derive_bundle(
    supported,
    always_observed_continuous = c(
      "age", "pf_ratio", "index_peep", "index_hours_from_icu"
    ),
    possibly_missing_continuous = if (full_specification) {
      c("map", "platelet", "creatinine")
    } else {
      character()
    },
    binary_variables = if (full_specification) {
      c("sex_female", "sex_unknown", "vasopressor")
    } else {
      c("sex_female", "sex_unknown")
    },
    knot_probabilities = c(0.10, 0.50, 0.90),
    minimum_unique_for_spline = 5L
  )
  fit <- v2_fit_selection_weights(
    supported,
    inclusion = inclusion,
    row_id = "analysis_id",
    bundle = bundle,
    truncation_quantiles =
      LOCKED_V2$selection_sensitivity$truncation_quantiles,
    model_id = paste(
      tolower(gsub("[^A-Za-z0-9]+", "_", database)),
      target_name,
      model_role,
      sep = "__"
    )
  )
  cluster_counts <- as.data.table(support$cluster_counts)
  setnames(cluster_counts, "cluster", "hospital")
  cluster_counts[, `:=`(
    database = database,
    selection_target = target_name,
    model_role = model_role,
    covariate_specification = covariate_specification,
    endpoint_weight_eligible = endpoint_weight_eligible
  )]
  fit$included_weights <- as.data.table(fit$included_weights)
  fit$included_weights[
    supported[, .(analysis_id, source_id, hospital)],
    on = .(row_id = analysis_id),
    `:=`(
      source_id = i.source_id,
      hospital = i.hospital,
      model_role = model_role,
      covariate_specification = covariate_specification,
      permitted_for_outcome_weighting = endpoint_weight_eligible
    )
  ]
  fit$all_probabilities <- as.data.table(fit$all_probabilities)
  fit$all_probabilities[
    supported[, .(analysis_id, source_id, hospital)],
    on = .(row_id = analysis_id),
    `:=`(
      source_id = i.source_id,
      hospital = i.hospital,
      model_role = model_role,
      covariate_specification = covariate_specification,
      permitted_for_outcome_weighting = endpoint_weight_eligible
    )
  ]
  fit$database <- database
  fit$selection_target <- target_name
  fit$target_inclusion_field <- inclusion
  fit$model_role <- model_role
  fit$covariate_specification <- covariate_specification
  fit$endpoint_weight_eligible <- endpoint_weight_eligible
  fit$interpretation <- if (endpoint_weight_eligible) {
    paste(
      "Outcome-blind measured-selection IPW sensitivity under positivity",
      "within supported hospitals; not proof of bias elimination."
    )
  } else {
    paste(
      "Selection diagnostic and nonpositivity audit only;",
      "weights must not be joined to an outcome model."
    )
  }
  fit$support <- list(
    cluster_counts = cluster_counts,
    supported_hospitals = support$supported_clusters,
    unsupported_hospitals = support$unsupported_clusters,
    full_target_n = nrow(frame),
    supported_target_n = nrow(supported),
    full_included_n = sum(frame[[inclusion]]),
    supported_included_n = sum(supported[[inclusion]])
  )
  fit
}

mimic_frame <- make_selection_frame("MIMIC-IV")
eicu_frame <- make_selection_frame("eICU-CRD")

fits <- list(
  mimic_tuple_ipw = fit_one_target(
    mimic_frame, "MIMIC-IV",
    "valid_tuple_by_6h_landmark", "tuple_included",
    model_role = "tuple_measured_selection_ipw",
    covariate_specification = "full_core_median_indicator",
    endpoint_weight_eligible = TRUE
  ),
  mimic_joint_diagnostic = fit_one_target(
    mimic_frame, "MIMIC-IV",
    "valid_tuple_and_complete_no_gcs_core", "joint_included",
    model_role = "joint_full_selection_diagnostic",
    covariate_specification = "full_core_median_indicator",
    endpoint_weight_eligible = FALSE
  ),
  mimic_joint_ipw = fit_one_target(
    mimic_frame, "MIMIC-IV",
    "valid_tuple_and_complete_no_gcs_core", "joint_included",
    model_role = "joint_always_observed_ipw",
    covariate_specification = "always_observed_only",
    endpoint_weight_eligible = TRUE
  ),
  eicu_tuple_ipw = fit_one_target(
    eicu_frame, "eICU-CRD",
    "valid_tuple_by_6h_landmark", "tuple_included",
    model_role = "tuple_measured_selection_ipw",
    covariate_specification = "full_core_median_indicator",
    endpoint_weight_eligible = TRUE
  ),
  eicu_joint_diagnostic = fit_one_target(
    eicu_frame, "eICU-CRD",
    "valid_tuple_and_complete_no_gcs_core", "joint_included",
    model_role = "joint_full_selection_diagnostic",
    covariate_specification = "full_core_median_indicator",
    endpoint_weight_eligible = FALSE
  ),
  eicu_joint_ipw = fit_one_target(
    eicu_frame, "eICU-CRD",
    "valid_tuple_and_complete_no_gcs_core", "joint_included",
    model_role = "joint_always_observed_ipw",
    covariate_specification = "always_observed_only",
    endpoint_weight_eligible = TRUE
  )
)

make_private_object <- function(database, frame, selected_fits) {
  list(
    version = "selection_weights_v2",
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    locked_config_version = LOCKED_V2$version,
    database = database,
    outcome_blind = TRUE,
    interpretation = LOCKED_V2$selection_sensitivity$interpretation,
    input_hashes = lapply(input_paths, sha256_file),
    target_n = nrow(frame),
    selection_frame_columns = names(frame),
    models = selected_fits
  )
}
mimic_object <- make_private_object(
  "MIMIC-IV",
  mimic_frame,
  fits[c(
    "mimic_tuple_ipw",
    "mimic_joint_diagnostic",
    "mimic_joint_ipw"
  )]
)
eicu_object <- make_private_object(
  "eICU-CRD",
  eicu_frame,
  fits[c(
    "eicu_tuple_ipw",
    "eicu_joint_diagnostic",
    "eicu_joint_ipw"
  )]
)
mimic_output <- file.path(private_out, "mimic_selection_weights_v2.rds")
eicu_output <- file.path(private_out, "eicu_selection_weights_v2.rds")
saveRDS(mimic_object, mimic_output, compress = "xz")
saveRDS(eicu_object, eicu_output, compress = "xz")

model_summary <- rbindlist(lapply(fits, function(fit) {
  x <- as.data.table(fit$summary)
  weights <- fit$included_weights
  x[, `:=`(
    database = fit$database,
    selection_target = fit$selection_target,
    model_role = fit$model_role,
    covariate_specification = fit$covariate_specification,
    endpoint_weight_eligible = fit$endpoint_weight_eligible,
    full_landmark_target_n = fit$support$full_target_n,
    supported_target_n = fit$support$supported_target_n,
    supported_hospital_n = length(fit$support$supported_hospitals),
    unsupported_hospital_n = length(fit$support$unsupported_hospitals),
    unsupported_patient_n =
      fit$support$full_target_n - fit$support$supported_target_n,
    raw_weight_minimum = min(weights$stabilized_weight_raw),
    raw_weight_maximum = max(weights$stabilized_weight_raw),
    truncated_weight_minimum =
      min(weights$stabilized_weight_truncated),
    truncated_weight_maximum =
      max(weights$stabilized_weight_truncated)
  )]
  x
}), use.names = TRUE, fill = TRUE)
setcolorder(
  model_summary,
  c(
    "database", "selection_target", "model_role",
    "covariate_specification", "endpoint_weight_eligible", "model_id",
    setdiff(
      names(model_summary),
      c(
        "database", "selection_target", "model_role",
        "covariate_specification", "endpoint_weight_eligible", "model_id"
      )
    )
  )
)
fwrite(
  model_summary,
  file.path(aggregate_out, "selection_model_summary_v2.csv")
)
model_role_dictionary <- unique(model_summary[, .(
  selection_target,
  model_role,
  covariate_specification,
  endpoint_weight_eligible
)])
model_role_dictionary[, interpretation := fifelse(
  endpoint_weight_eligible,
  paste(
    "May be joined by exact ID to the corresponding endpoint-model common",
    "set only after the selection completion gate is frozen."
  ),
  paste(
    "Selection diagnostic/nonpositivity audit only; row weights are",
    "prohibited from endpoint modeling."
  )
)]
fwrite(
  model_role_dictionary,
  file.path(aggregate_out, "selection_model_role_dictionary_v2.csv")
)

support_summary <- rbindlist(lapply(fits, function(fit) {
  counts <- fit$support$cluster_counts
  data.table(
    database = fit$database,
    selection_target = fit$selection_target,
    model_role = fit$model_role,
    covariate_specification = fit$covariate_specification,
    endpoint_weight_eligible = fit$endpoint_weight_eligible,
    target_hospital_n = nrow(counts),
    supported_hospital_n = sum(counts$supported),
    unsupported_hospital_n = sum(!counts$supported),
    full_target_n = fit$support$full_target_n,
    supported_target_n = fit$support$supported_target_n,
    unsupported_target_n =
      fit$support$full_target_n - fit$support$supported_target_n,
    full_included_n = fit$support$full_included_n,
    supported_included_n = fit$support$supported_included_n,
    largest_unsupported_hospital_n = if (any(!counts$supported)) {
      max(counts[supported == FALSE, n])
    } else {
      0L
    }
  )
}))
fwrite(
  support_summary,
  file.path(aggregate_out, "selection_structural_support_summary_v2.csv")
)

selection_frames <- list(`MIMIC-IV` = mimic_frame, `eICU-CRD` = eicu_frame)
population_counts <- rbindlist(Map(
  function(database, frame) {
    data.table(
      database = database,
      population = c(
        "fixed_6h_landmark_at_risk",
        "valid_tuple",
        "complete_no_gcs_core",
        "valid_tuple_and_complete_no_gcs_core"
      ),
      n = c(
        nrow(frame),
        sum(frame$tuple_included),
        sum(frame$complete_no_gcs_core),
        sum(frame$joint_included)
      )
    )
  },
  names(selection_frames),
  selection_frames
))
fwrite(
  population_counts,
  file.path(aggregate_out, "selection_population_counts_v2.csv")
)

missingness <- rbindlist(Map(
  function(database, frame) {
    rbindlist(lapply(
      c("map", "platelet", "creatinine"),
      function(variable) {
        missing <- is.na(frame[[variable]]) | !is.finite(frame[[variable]])
        data.table(
          database = database,
          variable = variable,
          target_n = nrow(frame),
          missing_n = sum(missing),
          missing_percent = 100 * mean(missing),
          tuple_rate_if_observed = mean(frame$tuple_included[!missing]),
          tuple_rate_if_missing = if (any(missing)) {
            mean(frame$tuple_included[missing])
          } else {
            NA_real_
          },
          joint_rate_if_observed = mean(frame$joint_included[!missing]),
          joint_rate_if_missing = if (any(missing)) {
            mean(frame$joint_included[missing])
          } else {
            NA_real_
          }
        )
      }
    ))
  },
  names(selection_frames),
  selection_frames
))
fwrite(
  missingness,
  file.path(aggregate_out, "selection_core_missingness_v2.csv")
)

sex_recognition <- rbindlist(Map(
  function(database, frame) {
    data.table(
      database = database,
      fixed_6h_landmark_target_n = nrow(frame),
      recognized_sex_n = sum(frame$sex_known),
      unknown_sex_n = sum(frame$sex_unknown),
      unknown_sex_tuple_n = sum(
        frame$sex_unknown == 1L & frame$tuple_included == 1L
      ),
      unknown_sex_complete_no_gcs_core_n = sum(
        frame$sex_unknown == 1L &
          frame$complete_no_gcs_core == 1L
      ),
      unknown_sex_joint_included_n = sum(
        frame$sex_unknown == 1L & frame$joint_included == 1L
      )
    )
  },
  names(selection_frames),
  selection_frames
))
fwrite(
  sex_recognition,
  file.path(aggregate_out, "selection_sex_recognition_v2.csv")
)
if (any(sex_recognition$unknown_sex_complete_no_gcs_core_n > 0L) ||
    any(sex_recognition$unknown_sex_joint_included_n > 0L)) {
  stop(
    "Unknown sex was incorrectly retained in the complete no-GCS core."
  )
}

bind_fit_table <- function(element) {
  rbindlist(lapply(fits, function(fit) {
    x <- as.data.table(fit[[element]])
    x[, `:=`(
      database = fit$database,
      selection_target = fit$selection_target,
      model_role = fit$model_role,
      covariate_specification = fit$covariate_specification,
      endpoint_weight_eligible = fit$endpoint_weight_eligible
    )]
    setcolorder(
      x,
      c(
        "database", "selection_target", "model_role",
        "covariate_specification", "endpoint_weight_eligible",
        setdiff(
          names(x),
          c(
            "database", "selection_target", "model_role",
            "covariate_specification", "endpoint_weight_eligible"
          )
        )
      )
    )
    x
  }), use.names = TRUE, fill = TRUE)
}
aggregate_tables <- list(
  selection_probability_distribution_v2.csv =
    bind_fit_table("probability_distribution"),
  selection_weight_distribution_v2.csv =
    bind_fit_table("weight_distribution"),
  selection_covariate_balance_v2.csv =
    bind_fit_table("balance"),
  selection_transformation_audit_v2.csv =
    rbindlist(lapply(fits, function(fit) {
      x <- as.data.table(fit$bundle$transformation_audit)
      x[, `:=`(
        database = fit$database,
        selection_target = fit$selection_target,
        model_role = fit$model_role,
        covariate_specification = fit$covariate_specification,
        endpoint_weight_eligible = fit$endpoint_weight_eligible
      )]
      x
    }), use.names = TRUE, fill = TRUE),
  selection_design_estimability_audit_v2.csv =
    bind_fit_table("design_audit"),
  selection_model_coefficients_v2.csv =
    rbindlist(lapply(fits, function(fit) {
      data.table(
        database = fit$database,
        selection_target = fit$selection_target,
        model_role = fit$model_role,
        covariate_specification = fit$covariate_specification,
        endpoint_weight_eligible = fit$endpoint_weight_eligible,
        design_term = names(fit$coefficients),
        coefficient = as.numeric(fit$coefficients)
      )
    }), use.names = TRUE, fill = TRUE)
)
for (name in names(aggregate_tables)) {
  fwrite(aggregate_tables[[name]], file.path(aggregate_out, name))
}
structural_nonreweightability <-
  aggregate_tables[["selection_covariate_balance_v2.csv"]][
  structurally_nonreweightable == TRUE
][
  ,
  interpretation := paste(
    "The variable varies in the supported target but is constant among",
    "included patients; no positive weighting of included patients can",
    "balance it. The corresponding model is a nonpositivity diagnostic,",
    "not evidence that the structural selection mechanism was corrected."
  )
]
fwrite(
  structural_nonreweightability,
  file.path(
    aggregate_out,
    "selection_structural_nonreweightability_v2.csv"
  )
)

# Detailed hospital IDs are private. The aggregate support summary above
# reports only hospital and patient counts.
private_support <- rbindlist(lapply(fits, function(fit) {
  fit$support$cluster_counts
}), use.names = TRUE, fill = TRUE)
saveRDS(
  private_support,
  file.path(private_out, "eicu_and_mimic_hospital_support_private_v2.rds"),
  compress = "xz"
)

aggregate_csv <- list.files(
  aggregate_out, pattern = "\\.csv$", full.names = TRUE
)
identifier_names <- c(
  "subject_id", "hadm_id", "stay_id", "patientunitstayid",
  "patienthealthsystemstayid", "person_key", "uniquepid",
  "hospital", "hospital_id", "source_id", "analysis_id", "row_id"
)
aggregate_leakage_guard <- rbindlist(lapply(aggregate_csv, function(path) {
  header <- names(fread(path, nrows = 0L, showProgress = FALSE))
  data.table(
    file = basename(path),
    identifier_column_present = any(header %in% identifier_names),
    outcome_token_present = any(grepl(
      forbidden_name, header, ignore.case = TRUE
    ))
  )
}))
fwrite(
  aggregate_leakage_guard,
  file.path(qc_out, "selection_aggregate_leakage_guard_v2.csv")
)
if (any(aggregate_leakage_guard$identifier_column_present) ||
    any(aggregate_leakage_guard$outcome_token_present)) {
  stop("Selection aggregate-output leakage guard failed.")
}

input_manifest <- data.table(
  input_name = names(input_paths),
  path = normalizePath(unlist(input_paths), mustWork = TRUE),
  sha256 = vapply(input_paths, sha256_file, character(1)),
  opened_before_weight_freeze = TRUE,
  contains_patient_rows = names(input_paths) %chin% c(
    "mimic_target", "eicu_target", "mimic_core", "eicu_core"
  ),
  outcome_artifact = FALSE
)
fwrite(
  input_manifest,
  file.path(qc_out, "selection_input_manifest_v2.csv")
)

output_manifest <- data.table(
  output_name = c("mimic_selection_weights", "eicu_selection_weights"),
  path = normalizePath(c(mimic_output, eicu_output), mustWork = TRUE),
  sha256 = c(sha256_file(mimic_output), sha256_file(eicu_output)),
  row_level_private = TRUE
)
fwrite(
  output_manifest,
  file.path(qc_out, "selection_private_output_manifest_v2.csv")
)

all_checks_pass <- all(
  model_summary$effective_sample_size_truncated > 0,
  model_summary$included_n > 0,
  model_summary$included_n < model_summary$target_n,
  model_summary$model_c_statistic >= 0.5,
  model_summary$model_c_statistic <= 1,
  model_summary$truncated_weight_minimum > 0,
  sum(model_summary$endpoint_weight_eligible) == 4L,
  sum(!model_summary$endpoint_weight_eligible) == 2L,
  model_summary$endpoint_weight_eligible[
    model_summary$model_role == "joint_full_selection_diagnostic"
  ] == FALSE,
  model_summary$covariate_specification[
    model_summary$model_role == "joint_always_observed_ipw"
  ] == "always_observed_only",
  is.finite(
    model_summary$maximum_absolute_smd_weighted_reweightable[
      model_summary$endpoint_weight_eligible
    ]
  ),
  !aggregate_leakage_guard$identifier_column_present,
  !aggregate_leakage_guard$outcome_token_present
)
if (!all_checks_pass) stop("Selection-weight completion checks failed.")

gate <- data.table(
  field = c(
    "status", "locked_config_version", "script_sha256",
    "config_sha256", "analysis_utils_sha256", "selection_utils_sha256",
    "fixed_landmark_gate_sha256", "no_gcs_core_gate_sha256",
    "mimic_target_sha256", "eicu_target_sha256",
    "mimic_core_sha256", "eicu_core_sha256",
    "mimic_selection_weights_sha256", "eicu_selection_weights_sha256",
    "outcome_artifacts_opened", "aggregate_leakage_guard_pass",
    "endpoint_weight_eligible_model_n",
    "diagnostic_only_model_n",
    "all_checks_pass", "completed_at"
  ),
  value = c(
    "PASS", LOCKED_V2$version, sha256_file(script_path),
    sha256_file(file.path(script_dir, "00_config.R")),
    sha256_file(file.path(script_dir, "01_analysis_utils.R")),
    sha256_file(file.path(script_dir, "08_selection_utils.R")),
    sha256_file(input_paths$fixed_landmark_gate),
    sha256_file(input_paths$no_gcs_gate),
    sha256_file(input_paths$mimic_target),
    sha256_file(input_paths$eicu_target),
    sha256_file(input_paths$mimic_core),
    sha256_file(input_paths$eicu_core),
    sha256_file(mimic_output), sha256_file(eicu_output),
    "FALSE", "TRUE",
    as.character(sum(model_summary$endpoint_weight_eligible)),
    as.character(sum(!model_summary$endpoint_weight_eligible)),
    as.character(all_checks_pass),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
)
fwrite(gate, completion_tmp)
if (!file.rename(completion_tmp, completion_path)) {
  stop("Could not atomically publish selection-weight completion gate.")
}

summary_lines <- c(
  "# rebuild_v2 outcome-blind selection-weight QC",
  "",
  paste0("- Completed: ", gate[field == "completed_at", value]),
  paste0("- MIMIC landmark target: ", nrow(mimic_frame)),
  paste0("- eICU landmark target: ", nrow(eicu_frame)),
  paste0(
    "- Targets: valid tuple; valid tuple plus complete no-GCS core."
  ),
  paste0(
    "- Tuple IPW uses the full outcome-blind median/indicator selection ",
    "model. Joint IPW uses only covariates observed in every target patient ",
    "(age, sex indicators, index P/F, index PEEP, and ICU-to-index time)."
  ),
  paste0(
    "- The full joint median/indicator model is retained only as a selection ",
    "diagnostic/nonpositivity audit and is prohibited from outcome weighting."
  ),
  paste0(
    "- eICU structural-zero hospitals are excluded separately for each ",
    "weighted estimand and retained in a private support audit."
  ),
  paste0(
    "- Stabilized weights use 1st/99th percentile truncation. ",
    "Raw/truncated distributions, ESS, inclusion AUC, and measured balance ",
    "are disclosure-safe aggregate outputs."
  ),
  paste0(
    "- Covariates that vary in the supported target but are constant among ",
    "included patients are explicitly marked structurally non-reweightable."
  ),
  "- No outcome artifact was opened before or during weight construction.",
  "- Interpretation: measured-selection sensitivity, not proof of bias removal."
)
writeLines(
  summary_lines,
  file.path(qc_out, "selection_weights_QC_v2.md"),
  useBytes = TRUE
)

message("Outcome-blind selection weights frozen.")
print(model_summary[, .(
  database, selection_target, model_role, endpoint_weight_eligible,
  supported_target_n, included_n,
  unsupported_hospital_n, unsupported_patient_n,
  effective_sample_size_truncated, model_c_statistic,
  truncated_weight_minimum, truncated_weight_maximum,
  maximum_absolute_smd_weighted_reweightable,
  nonreweightable_balance_variable_n
)])
