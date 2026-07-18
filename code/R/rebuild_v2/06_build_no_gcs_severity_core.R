#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild_v2: fixed-6 h, outcome-blind no-GCS core.
#
# Inputs are the all-landmark-at-risk selection targets written by
# 05_build_fixed_landmark_flow.R.  This script never reads the separate
# mortality artifacts.  It extracts only MAP, platelet, creatinine, and a
# six-drug vasoactive indicator through the fixed index+6 h landmark.

suppressPackageStartupMessages(library(data.table))

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/06_build_no_gcs_severity_core.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "00_config.R"))

trailing <- commandArgs(trailingOnly = TRUE)
database_arg <- sub("^--database=", "", trailing[grepl("^--database=", trailing)])
database_arg <- if (length(database_arg)) database_arg[[1L]] else "both"
if (!database_arg %chin% c("mimic", "eicu", "both")) {
  stop("--database must be mimic, eicu, or both.")
}

FORBIDDEN_PATTERN <- paste(
  c("mort", "death", "dead", "expire", "discharge", "outcome", "surviv", "status"),
  collapse = "|"
)
V1_SOURCE_SHA <- c(
  mimic_R = "a07c10a389c61706b7d5a16ecc4eacfe2413b9250fad059633f4f91cca3092ff",
  mimic_filter = "25c5463d8dea9f11c3347050559cefe6c26154d610f304d427a7180f5fcd8a30",
  eicu_R = "766629ce3d947d384905ef808efc03ac5e640e4054b7841c9a67d5d2507c490e",
  eicu_filter = "28e9f53a4e7f49a9b11aa775fd73a888198841e3f1ed776e1f6f45c9ae5ebd4a"
)

qc_root <- file.path(QC_ROOT, "no_gcs_core")
dir.create(qc_root, recursive = TRUE, showWarnings = FALSE)
helper_path <- file.path(dirname(script_path), "06a_filter_no_gcs_inputs_v2.py")
selftest_path <- file.path(dirname(script_path), "06_no_gcs_core_selftest.R")
fixed_gate_path <- file.path(
  QC_ROOT, "fixed_landmark", "fixed6h_landmark_complete_v2.csv"
)

sha256_file <- function(path) {
  z <- system2(
    "shasum", c("-a", "256", shQuote(path)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(z, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA256 failed for ", path, ": ", paste(z, collapse = " "))
  }
  hash <- strsplit(z[[1L]], "[[:space:]]+")[[1L]][1L]
  if (!grepl("^[0-9a-f]{64}$", hash)) stop("Invalid SHA256 for ", path)
  hash
}

read_gate_map <- function(path) {
  z <- fread(path, showProgress = FALSE)
  if (!identical(names(z), c("field", "value")) || anyDuplicated(z$field)) {
    stop("Malformed field/value gate: ", path)
  }
  setNames(as.character(z$value), z$field)
}

require_gate <- function(gate, field, expected = NULL) {
  value <- unname(gate[field])
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("Gate field missing: ", field)
  }
  if (!is.null(expected) && !identical(value, as.character(expected))) {
    stop("Gate mismatch for ", field, ": ", value, " != ", expected)
  }
  value
}

strict_numeric <- function(x) {
  z <- trimws(as.character(x))
  ok <- grepl(
    "^[+-]?((([0-9]+)(\\.[0-9]*)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)?$",
    z
  )
  out <- rep(NA_real_, length(z))
  out[ok] <- suppressWarnings(as.numeric(z[ok]))
  out[!is.finite(out)] <- NA_real_
  out
}

to_epoch <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(x))
  z <- as.POSIXct(as.character(x), tz = "UTC")
  as.numeric(z)
}

from_epoch <- function(x) {
  as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
}

q_safe <- function(x, probs = c(0, .05, .25, .5, .75, .95, 1)) {
  if (!length(x) || all(is.na(x))) return(rep(NA_real_, length(probs)))
  as.numeric(quantile(
    x, probs = probs, na.rm = TRUE, names = FALSE, type = 2
  ))
}

distribution_row <- function(database, variable, unit, x) {
  q <- q_safe(x)
  data.table(
    database, variable, unit,
    n = sum(!is.na(x)), missing_n = sum(is.na(x)),
    min = q[1L], q05 = q[2L], q25 = q[3L], median = q[4L],
    q75 = q[5L], q95 = q[6L], max = q[7L],
    mean = if (any(!is.na(x))) mean(x, na.rm = TRUE) else NA_real_,
    sd = if (sum(!is.na(x)) > 1L) sd(x, na.rm = TRUE) else NA_real_
  )
}

atomic_save_rds <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  unlink(tmp, force = TRUE)
  saveRDS(x, tmp, compress = "xz")
  if (!file.rename(tmp, path)) stop("Atomic RDS publish failed: ", path)
}

assert_no_leakage <- function(x, label) {
  hits <- names(x)[grepl(FORBIDDEN_PATTERN, names(x), ignore.case = TRUE)]
  if (length(hits)) {
    stop(label, " contains outcome/status-like fields: ", paste(hits, collapse = ", "))
  }
  invisible(TRUE)
}

assert_source_provenance <- function() {
  paths <- c(
    mimic_R = file.path(PROJECT_ROOT, "code/R/rebuild_v1/05_build_mimic_severity_core.R"),
    mimic_filter = file.path(PROJECT_ROOT, "code/R/rebuild_v1/05a_filter_mimic_severity_inputs.py"),
    eicu_R = file.path(PROJECT_ROOT, "code/R/rebuild_v1/06_build_eicu_severity_core.R"),
    eicu_filter = file.path(PROJECT_ROOT, "code/R/rebuild_v1/06a_filter_eicu_severity_inputs.py")
  )
  actual <- vapply(paths, sha256_file, character(1L))
  if (!identical(unname(actual), unname(V1_SOURCE_SHA[names(actual)]))) {
    stop("An audited rebuild_v1 mapping source changed.")
  }
  data.table(
    source = names(paths), path = unname(paths),
    expected_sha256 = unname(V1_SOURCE_SHA[names(paths)]),
    actual_sha256 = unname(actual), pass = actual == V1_SOURCE_SHA[names(actual)]
  )
}

read_cache <- function(path, source_name, required, manifest) {
  if (!file.exists(path)) stop("Missing filtered cache: ", path)
  z <- fread(
    cmd = sprintf("gzip -cd %s", shQuote(path)),
    showProgress = FALSE, fill = FALSE
  )
  missing <- setdiff(required, names(z))
  if (length(missing)) {
    stop(source_name, " cache missing: ", paste(missing, collapse = ", "))
  }
  expected <- manifest[["kept_rows"]][manifest[["source_name"]] == source_name]
  if (length(expected) != 1L || nrow(z) != expected) {
    stop(source_name, " cache row mismatch: ", nrow(z), " != ", expected)
  }
  z
}

run_filter <- function(database, key_path, raw_root, cache_dir, target_n) {
  manifest_path <- file.path(cache_dir, "filter_manifest_v2.csv")
  gate_path <- file.path(cache_dir, "no_gcs_input_cache_complete_v2.csv")
  command_args <- c(
    shQuote(helper_path),
    "--database", database,
    "--keys", shQuote(key_path),
    "--raw-root", shQuote(raw_root),
    "--cache-dir", shQuote(cache_dir)
  )
  output <- system2(
    "python3",
    command_args,
    stdout = TRUE, stderr = TRUE
  )
  writeLines(
    c(
      paste("command:", "python3", paste(command_args, collapse = " ")),
      paste("completed_at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
      output
    ),
    file.path(cache_dir, "filter_invocation_log_v2.txt"),
    useBytes = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(database, " raw filter failed:\n", paste(output, collapse = "\n"))
  }
  if (!file.exists(manifest_path) || !file.exists(gate_path)) {
    stop(database, " filter did not publish manifest/gate.")
  }
  manifest <- fread(manifest_path, showProgress = FALSE)
  gate <- fread(gate_path, showProgress = FALSE)
  expected_sources <- if (database == "mimic") 3L else 4L
  if (
    nrow(gate) != 1L || gate$status[[1L]] != "PASS" ||
    gate$target_count[[1L]] != target_n ||
    gate$source_count[[1L]] != expected_sources ||
    gate$helper_sha256[[1L]] != sha256_file(helper_path) ||
    nrow(manifest) != expected_sources ||
    any(manifest$status != "PASS") ||
    any(manifest$reached_eof != TRUE) ||
    any(manifest$target_count != target_n)
  ) {
    stop(database, " no-GCS cache gate/manifest failed.")
  }
  actual_output_sha <- vapply(manifest$output_path, sha256_file, character(1L))
  if (any(actual_output_sha != manifest$output_sha256)) {
    stop(database, " filtered cache SHA mismatch.")
  }
  list(
    manifest = manifest, gate = gate,
    manifest_path = manifest_path, gate_path = gate_path,
    log = output
  )
}

standardize_common <- function(x, database) {
  x[, `:=`(
    hospital_id = if (database == "mimic") {
      "MIMIC_BIDMC"
    } else {
      as.character(hospitalid)
    },
    sex_recognized = sex %chin% c("F", "Female", "M", "Male"),
    # Keep a numeric design-matrix column for the single eICU unknown-sex row;
    # sex_missing/sex_recognized, not this numeric coding alone, governs the
    # primary complete-core flag.
    sex_female = as.integer(sex %chin% c("F", "Female")),
    pf_ratio = as.numeric(index_pf),
    map = as.numeric(map_min),
    vasopressor = as.integer(vasopressor_any),
    platelet = as.numeric(platelet_min),
    creatinine = as.numeric(creatinine_max)
  )]
  x[, `:=`(
    age_missing = is.na(age),
    sex_missing = !sex_recognized,
    pf_ratio_missing = is.na(pf_ratio),
    map_missing = is.na(map),
    vasopressor_missing = is.na(vasopressor),
    platelet_missing = is.na(platelet),
    creatinine_missing = is.na(creatinine)
  )]
  x[, complete_no_gcs_core :=
    !age_missing & !sex_missing & !pf_ratio_missing &
      !map_missing & !vasopressor_missing &
      !platelet_missing & !creatinine_missing]
  x[, tuple_and_complete_no_gcs_core :=
    tuple_observed == TRUE & complete_no_gcs_core]
  x
}

write_database_qc <- function(
    database, core, timing, mapping, invariants, source_manifest,
    provenance, output_paths
) {
  db_qc <- file.path(qc_root, database)
  dir.create(db_qc, recursive = TRUE, showWarnings = FALSE)
  fwrite(timing, file.path(db_qc, "timing_availability_qc_v2.csv"))
  fwrite(mapping, file.path(db_qc, "mapping_unit_source_qc_v2.csv"))
  fwrite(invariants, file.path(db_qc, "invariant_tests_v2.csv"))
  fwrite(source_manifest, file.path(db_qc, "raw_cache_manifest_snapshot_v2.csv"))
  fwrite(provenance, file.path(db_qc, "v1_mapping_provenance_v2.csv"))
  missingness <- rbindlist(lapply(
    c(
      "age", "sex_female", "pf_ratio", "map", "vasopressor",
      "platelet", "creatinine"
    ),
    function(v) {
      missing_flag <- if (v == "sex_female") {
        core$sex_missing
      } else {
        is.na(core[[v]])
      }
      data.table(
        database, variable = v, denominator = nrow(core),
        observed_n = sum(!missing_flag),
        missing_n = sum(missing_flag),
        missing_fraction = mean(missing_flag)
      )
    }
  ))
  fwrite(missingness, file.path(db_qc, "no_gcs_core_missingness_v2.csv"))
  values <- rbindlist(list(
    distribution_row(database, "age", "years", core$age),
    distribution_row(database, "pf_ratio", "mmHg", core$pf_ratio),
    distribution_row(database, "map", "mmHg", core$map),
    distribution_row(database, "platelet", "10^3/uL", core$platelet),
    distribution_row(database, "creatinine", "mg/dL", core$creatinine),
    distribution_row(database, "vasopressor", "binary", core$vasopressor)
  ))
  fwrite(values, file.path(db_qc, "no_gcs_core_value_distribution_v2.csv"))
  flow <- data.table(
    database,
    step = c(
      "all_landmark_at_risk", "valid_tuple_by_landmark",
      "complete_no_gcs_core", "valid_tuple_and_complete_no_gcs_core"
    ),
    n = c(
      nrow(core), sum(core$tuple_observed),
      sum(core$complete_no_gcs_core),
      sum(core$tuple_and_complete_no_gcs_core)
    )
  )
  fwrite(flow, file.path(db_qc, "no_gcs_core_flow_v2.csv"))
  leakage <- data.table(
    database,
    artifact = names(output_paths),
    forbidden_pattern = FORBIDDEN_PATTERN,
    forbidden_field_n = vapply(output_paths, function(path) {
      x <- readRDS(path)
      sum(grepl(FORBIDDEN_PATTERN, names(x), ignore.case = TRUE))
    }, integer(1L)),
    pass = TRUE
  )
  leakage[, pass := forbidden_field_n == 0L]
  fwrite(leakage, file.path(db_qc, "outcome_leakage_guard_v2.csv"))
}

build_mimic <- function(provenance, fixed_gate) {
  database <- "mimic"
  private_dir <- file.path(PRIVATE_ROOT, database)
  cache_dir <- file.path(private_dir, "cache_v2", "no_gcs_core")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  target_path <- file.path(
    private_dir, "mimic_all_landmark_at_risk_selection_targets_v2.rds"
  )
  tuple_source_path <- file.path(
    private_dir, "mimic_no_gcs_core_targets_v2.rds"
  )
  required_inputs <- c(target_path, tuple_source_path, fixed_gate_path, helper_path)
  if (any(!file.exists(required_inputs))) {
    stop("Missing MIMIC input(s): ", paste(required_inputs[!file.exists(required_inputs)], collapse = ", "))
  }
  require_gate(fixed_gate, "locked_config_version", LOCKED_V2$version)
  require_gate(
    fixed_gate, "mimic_selection_target_sha256", sha256_file(target_path)
  )
  target <- as.data.table(readRDS(target_path))
  tuple_source <- as.data.table(readRDS(tuple_source_path))
  required <- c(
    "subject_id", "hadm_id", "stay_id", "age", "sex", "index_pf",
    "index_peep", "index_time", "landmark_time",
    "covariate_window_start", "covariate_window_end",
    "tuple_observed", "n_valid_tuples"
  )
  missing <- setdiff(required, names(target))
  if (length(missing)) stop("MIMIC target missing: ", paste(missing, collapse = ", "))
  assert_no_leakage(target, "MIMIC selection target")
  if (
    nrow(target) != 20388L || anyDuplicated(target$stay_id) ||
    any(to_epoch(target$covariate_window_start) >
          to_epoch(target$covariate_window_end)) ||
    any(abs(
      to_epoch(target$covariate_window_end) -
        to_epoch(target$landmark_time)
    ) > 1e-6)
  ) {
    stop("MIMIC target row/key/window invariant failed.")
  }

  key_path <- file.path(cache_dir, "target_keys_v2.csv")
  fwrite(target[, .(subject_id, hadm_id, stay_id)], key_path)
  filter <- run_filter(database, key_path, MIMIC_ROOT, cache_dir, nrow(target))
  manifest <- filter$manifest

  d_items <- fread(
    cmd = sprintf(
      "gzip -cd %s", shQuote(file.path(MIMIC_ROOT, "icu", "d_items.csv.gz"))
    ),
    select = c("itemid", "label", "linksto"), showProgress = FALSE
  )
  d_labs <- fread(
    cmd = sprintf(
      "gzip -cd %s", shQuote(file.path(MIMIC_ROOT, "hosp", "d_labitems.csv.gz"))
    ),
    select = c("itemid", "label", "fluid"), showProgress = FALSE
  )
  expected_items <- data.table(
    itemid = c(220052L, 220181L, 221906L, 221289L, 222315L, 221662L, 221653L, 221749L),
    expected_label = c(
      "Arterial Blood Pressure mean", "Non Invasive Blood Pressure mean",
      "Norepinephrine", "Epinephrine", "Vasopressin", "Dopamine",
      "Dobutamine", "Phenylephrine"
    ),
    expected_linksto = c(rep("chartevents", 2L), rep("inputevents", 6L))
  )
  item_meta <- merge(
    expected_items, d_items, by = "itemid", all.x = TRUE, sort = FALSE
  )
  item_meta[, pass := label == expected_label & linksto == expected_linksto]
  expected_labs <- data.table(
    itemid = c(51265L, 50912L),
    expected_label = c("Platelet Count", "Creatinine")
  )
  lab_meta <- merge(
    expected_labs, d_labs, by = "itemid", all.x = TRUE, sort = FALSE
  )
  lab_meta[, pass := label == expected_label & fluid == "Blood"]
  if (any(item_meta$pass != TRUE) || any(lab_meta$pass != TRUE)) {
    stop("MIMIC item metadata mismatch.")
  }

  bounds <- target[, .(
    stay_id,
    window_start = to_epoch(covariate_window_start),
    window_end = to_epoch(covariate_window_end)
  )]
  chart <- read_cache(
    file.path(cache_dir, "chartevents_map_candidates_v2.csv.gz"),
    "chartevents",
    c(
      "stay_id", "charttime", "storetime", "itemid",
      "valuenum", "valueuom", "warning"
    ),
    manifest
  )
  chart[, `:=`(
    measurement_time = to_epoch(charttime),
    store_time = to_epoch(storetime),
    available_time = pmax(to_epoch(charttime), to_epoch(storetime)),
    value_num = strict_numeric(valuenum),
    unit_normalized = tolower(trimws(as.character(valueuom))),
    map_source = fcase(
      itemid == 220052L, "map_invasive",
      itemid == 220181L, "map_noninvasive",
      default = NA_character_
    ),
    warning_num = suppressWarnings(as.integer(warning))
  )]
  if (anyNA(chart$measurement_time) || anyNA(chart$map_source)) {
    stop("MIMIC MAP cache contains invalid time/item.")
  }
  map_join <- merge(chart, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  map_join[, `:=`(
    measurement_in_window =
      measurement_time >= window_start & measurement_time <= window_end,
    available_by_end = !is.na(store_time) & available_time <= window_end,
    unit_valid = unit_normalized == "mmhg",
    value_valid = !is.na(value_num) & value_num >= 1 & value_num <= 250
  )]
  map_eligible <- map_join[
    measurement_in_window & available_by_end & unit_valid & value_valid
  ]
  map_reduced <- map_eligible[, .(
    map_value = median(value_num),
    map_available_time_num = max(available_time),
    map_warning_any = any(warning_num == 1L, na.rm = TRUE),
    duplicate_rows = .N
  ), by = .(
    stay_id, map_time_num = measurement_time, map_source, map_itemid = itemid
  )]
  map_reduced[, source_rank := fifelse(map_source == "map_invasive", 1L, 2L)]
  setorder(
    map_reduced, stay_id, map_value, source_rank,
    map_time_num, map_available_time_num
  )
  map_selected <- map_reduced[, .SD[1L], by = stay_id]
  setnames(map_selected, "map_value", "map_min")
  map_selected <- map_selected[, .(
    stay_id, map_min, map_time_num, map_available_time_num,
    map_source, map_itemid, map_warning_any
  )]

  lab_raw <- read_cache(
    file.path(cache_dir, "labevents_core_candidates_v2.csv.gz"),
    "labevents",
    c(
      "subject_id", "hadm_id", "charttime", "storetime",
      "itemid", "valuenum", "valueuom", "priority"
    ),
    manifest
  )
  lab <- merge(
    lab_raw, target[, .(subject_id, hadm_id, stay_id)],
    by = c("subject_id", "hadm_id"), all = FALSE, sort = FALSE
  )
  if (nrow(lab) != nrow(lab_raw)) stop("MIMIC lab-to-target join failed.")
  lab[, `:=`(
    measurement_time = to_epoch(charttime),
    store_time = to_epoch(storetime),
    available_time = pmax(to_epoch(charttime), to_epoch(storetime)),
    value_num = strict_numeric(valuenum),
    unit_normalized = tolower(trimws(as.character(valueuom))),
    lab_mapping = fcase(
      itemid == 51265L, "platelet",
      itemid == 50912L, "creatinine",
      default = NA_character_
    )
  )]
  lab[, unit_valid := fcase(
    lab_mapping == "platelet", unit_normalized == "k/ul",
    lab_mapping == "creatinine", unit_normalized == "mg/dl",
    default = FALSE
  )]
  lab[, value_valid := fcase(
    lab_mapping == "platelet",
    !is.na(value_num) & value_num > 0 & value_num <= 9999,
    lab_mapping == "creatinine",
    !is.na(value_num) & value_num >= 0.1 & value_num <= 28.28,
    default = FALSE
  )]
  lab_join <- merge(lab, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  lab_join[, `:=`(
    measurement_in_window =
      measurement_time >= window_start & measurement_time <= window_end,
    available_by_end = !is.na(store_time) & available_time <= window_end
  )]
  lab_eligible <- lab_join[
    measurement_in_window & available_by_end & unit_valid & value_valid
  ]
  lab_reduced <- lab_eligible[, .(
    lab_value = median(value_num),
    lab_available_time_num = max(available_time),
    duplicate_rows = .N,
    duplicate_value_conflict = uniqueN(value_num) > 1L
  ), by = .(
    stay_id, lab_time_num = measurement_time, lab_mapping, itemid
  )]
  platelet <- lab_reduced[lab_mapping == "platelet"]
  setorder(platelet, stay_id, lab_value, lab_time_num, lab_available_time_num)
  platelet <- platelet[, .SD[1L], by = stay_id]
  setnames(
    platelet,
    c("lab_value", "lab_time_num", "lab_available_time_num"),
    c("platelet_min", "platelet_time_num", "platelet_available_time_num")
  )
  platelet <- platelet[, .(
    stay_id, platelet_min, platelet_time_num, platelet_available_time_num,
    platelet_duplicate_value_conflict = duplicate_value_conflict
  )]
  creatinine <- lab_reduced[lab_mapping == "creatinine"]
  setorder(
    creatinine, stay_id, -lab_value, lab_time_num, lab_available_time_num
  )
  creatinine <- creatinine[, .SD[1L], by = stay_id]
  setnames(
    creatinine,
    c("lab_value", "lab_time_num", "lab_available_time_num"),
    c("creatinine_max", "creatinine_time_num", "creatinine_available_time_num")
  )
  creatinine <- creatinine[, .(
    stay_id, creatinine_max, creatinine_time_num,
    creatinine_available_time_num,
    creatinine_duplicate_value_conflict = duplicate_value_conflict
  )]

  input <- read_cache(
    file.path(cache_dir, "inputevents_pressor_candidates_v2.csv.gz"),
    "inputevents",
    c(
      "stay_id", "starttime", "endtime", "storetime", "itemid",
      "rate", "rateuom", "statusdescription"
    ),
    manifest
  )
  input[, `:=`(
    drug_class = fcase(
      itemid == 221906L, "norepinephrine",
      itemid == 221289L, "epinephrine",
      itemid == 222315L, "vasopressin",
      itemid == 221662L, "dopamine",
      itemid == 221653L, "dobutamine",
      itemid == 221749L, "phenylephrine",
      default = NA_character_
    ),
    start_time = to_epoch(starttime),
    end_time = to_epoch(endtime),
    store_time = to_epoch(storetime),
    rate_num = strict_numeric(rate),
    status_normalized = tolower(trimws(as.character(statusdescription)))
  )]
  if (anyNA(input$drug_class)) stop("Unmapped MIMIC pressor item.")
  input[, `:=`(
    interval_valid =
      !is.na(start_time) & !is.na(end_time) & end_time >= start_time,
    positive_rate = !is.na(rate_num) & rate_num > 0,
    noncancelled =
      !is.na(status_normalized) & nzchar(status_normalized) &
        !grepl("rewritten|cancel", status_normalized)
  )]
  pressor_join <- merge(input, bounds, by = "stay_id", all = FALSE, sort = FALSE)
  pressor_join[, `:=`(
    interval_overlaps_window =
      interval_valid & start_time <= window_end & end_time >= window_start,
    available_by_end = !is.na(store_time) & store_time <= window_end
  )]
  pressor_join[, active_row :=
    interval_overlaps_window & available_by_end &
      positive_rate & noncancelled]
  pressor_join[is.na(active_row), active_row := FALSE]
  pressor_active <- pressor_join[active_row == TRUE]
  pressor_by_stay <- pressor_active[, .(
    vasopressor_any = TRUE,
    vasoactive_active_row_n = .N,
    vasoactive_drugs = paste(sort(unique(drug_class)), collapse = ";"),
    vasopressor_first_time_num = min(pmax(start_time, window_start)),
    vasopressor_last_time_num = max(pmin(end_time, window_end)),
    vasopressor_available_time_num = max(store_time)
  ), by = stay_id]
  pressor <- merge(
    bounds[, .(stay_id)], pressor_by_stay,
    by = "stay_id", all.x = TRUE, sort = FALSE
  )
  pressor[is.na(vasopressor_any), vasopressor_any := FALSE]
  pressor[is.na(vasoactive_active_row_n), vasoactive_active_row_n := 0L]
  pressor[is.na(vasoactive_drugs), vasoactive_drugs := ""]

  core <- merge(
    target[, .(
      subject_id, hadm_id, stay_id, age, sex, index_pf, index_peep,
      index_time, landmark_time, covariate_window_start,
      covariate_window_end, tuple_observed, n_valid_tuples
    )],
    map_selected, by = "stay_id", all.x = TRUE, sort = FALSE
  )
  core <- merge(core, platelet, by = "stay_id", all.x = TRUE, sort = FALSE)
  core <- merge(core, creatinine, by = "stay_id", all.x = TRUE, sort = FALSE)
  core <- merge(core, pressor, by = "stay_id", all.x = TRUE, sort = FALSE)
  tuple_time <- tuple_source[, .(
    stay_id, tuple_time = ventilator_tuple_available_time
  )]
  core <- merge(core, tuple_time, by = "stay_id", all.x = TRUE, sort = FALSE)
  core <- standardize_common(core, database)
  core[, `:=`(
    map_time = from_epoch(map_time_num),
    map_available_time = from_epoch(map_available_time_num),
    platelet_time = from_epoch(platelet_time_num),
    platelet_available_time = from_epoch(platelet_available_time_num),
    creatinine_time = from_epoch(creatinine_time_num),
    creatinine_available_time = from_epoch(creatinine_available_time_num),
    vasopressor_first_time = from_epoch(vasopressor_first_time_num),
    vasopressor_last_time = from_epoch(vasopressor_last_time_num),
    vasopressor_available_time = from_epoch(vasopressor_available_time_num)
  )]
  core[, c(
    "map_time_num", "map_available_time_num",
    "platelet_time_num", "platelet_available_time_num",
    "creatinine_time_num", "creatinine_available_time_num",
    "vasopressor_first_time_num", "vasopressor_last_time_num",
    "vasopressor_available_time_num"
  ) := NULL]

  core_keep <- c(
    "subject_id", "hadm_id", "stay_id", "hospital_id",
    "age", "sex", "sex_recognized", "sex_female", "pf_ratio", "index_peep",
    "index_time", "landmark_time", "covariate_window_start",
    "covariate_window_end", "tuple_observed", "n_valid_tuples", "tuple_time",
    "map", "vasopressor", "platelet", "creatinine",
    "age_missing", "sex_missing", "pf_ratio_missing", "map_missing",
    "vasopressor_missing", "platelet_missing", "creatinine_missing",
    "complete_no_gcs_core", "tuple_and_complete_no_gcs_core",
    "map_time", "map_available_time", "map_source", "map_itemid",
    "map_warning_any",
    "platelet_time", "platelet_available_time",
    "platelet_duplicate_value_conflict",
    "creatinine_time", "creatinine_available_time",
    "creatinine_duplicate_value_conflict",
    "vasopressor_first_time", "vasopressor_last_time",
    "vasopressor_available_time", "vasoactive_active_row_n",
    "vasoactive_drugs"
  )
  core <- core[, ..core_keep]
  setorder(core, stay_id)
  assert_no_leakage(core, "MIMIC all-at-risk no-GCS core")

  tuple_core <- core[tuple_observed == TRUE]
  tuple_core <- tuple_core[
    match(tuple_source$stay_id, tuple_core$stay_id)
  ]
  if (
    nrow(tuple_core) != nrow(tuple_source) ||
    anyDuplicated(tuple_core$stay_id) ||
    !identical(tuple_core$stay_id, tuple_source$stay_id) ||
    anyNA(tuple_core$tuple_observed) ||
    any(tuple_core$tuple_observed != TRUE)
  ) {
    stop("MIMIC tuple-core join failed.")
  }
  assert_no_leakage(tuple_core, "MIMIC tuple no-GCS core")

  window_start_num <- to_epoch(core$covariate_window_start)
  window_end_num <- to_epoch(core$covariate_window_end)
  checks <- list(
    target_row_count = nrow(core) == nrow(target),
    unique_id = !anyDuplicated(core$stay_id),
    exact_id_set = setequal(core$stay_id, target$stay_id),
    tuple_count = sum(core$tuple_observed) == nrow(tuple_source),
    tuple_time_only_if_observed =
      all(is.na(core$tuple_time) == !core$tuple_observed),
    map_range = all(is.na(core$map) | core$map >= 1 & core$map <= 250),
    platelet_range =
      all(is.na(core$platelet) | core$platelet > 0 & core$platelet <= 9999),
    creatinine_range =
      all(is.na(core$creatinine) |
            core$creatinine >= 0.1 & core$creatinine <= 28.28),
    pressor_binary = all(core$vasopressor %in% c(0L, 1L)),
    map_measurement_in_window = all(
      is.na(core$map_time) |
        (to_epoch(core$map_time) >= window_start_num &
           to_epoch(core$map_time) <= window_end_num)
    ),
    map_available_by_landmark = all(
      is.na(core$map_available_time) |
        to_epoch(core$map_available_time) <= window_end_num
    ),
    platelet_measurement_in_window = all(
      is.na(core$platelet_time) |
        (to_epoch(core$platelet_time) >= window_start_num &
           to_epoch(core$platelet_time) <= window_end_num)
    ),
    platelet_available_by_landmark = all(
      is.na(core$platelet_available_time) |
        to_epoch(core$platelet_available_time) <= window_end_num
    ),
    creatinine_measurement_in_window = all(
      is.na(core$creatinine_time) |
        (to_epoch(core$creatinine_time) >= window_start_num &
           to_epoch(core$creatinine_time) <= window_end_num)
    ),
    creatinine_available_by_landmark = all(
      is.na(core$creatinine_available_time) |
        to_epoch(core$creatinine_available_time) <= window_end_num
    ),
    pressor_overlap_time_in_window = all(
      is.na(core$vasopressor_first_time) |
        (to_epoch(core$vasopressor_first_time) >= window_start_num &
           to_epoch(core$vasopressor_last_time) <= window_end_num)
    ),
    pressor_available_by_landmark = all(
      is.na(core$vasopressor_available_time) |
        to_epoch(core$vasopressor_available_time) <= window_end_num
    ),
    complete_flag_exact = all(
      core$complete_no_gcs_core ==
        (
          !core$age_missing & !core$sex_missing & !core$pf_ratio_missing &
            !core$map_missing & !core$vasopressor_missing &
            !core$platelet_missing & !core$creatinine_missing
        )
    ),
    no_outcome_like_fields =
      !any(grepl(FORBIDDEN_PATTERN, names(core), ignore.case = TRUE)),
    cache_all_sources_eof = all(manifest$reached_eof == TRUE),
    cache_all_official_sha_match =
      all(manifest$official_sha256_match == TRUE)
  )
  invariants <- data.table(
    database, check = names(checks),
    pass = unlist(checks, use.names = FALSE)
  )
  if (any(invariants$pass != TRUE)) {
    stop(
      "MIMIC no-GCS invariant failure: ",
      paste(invariants[pass != TRUE, check], collapse = ", ")
    )
  }

  timing <- rbindlist(list(
    data.table(
      database, component = "map",
      candidate_rows = nrow(map_join),
      measurement_in_window_rows = sum(map_join$measurement_in_window),
      available_by_landmark_rows =
        sum(map_join$measurement_in_window & map_join$available_by_end),
      unit_valid_rows = sum(
        map_join$measurement_in_window & map_join$available_by_end &
          map_join$unit_valid
      ),
      valid_rows = nrow(map_eligible),
      selected_patients = nrow(map_selected)
    ),
    rbindlist(lapply(c("platelet", "creatinine"), function(v) {
      z <- lab_join[lab_mapping == v]
      data.table(
        database, component = v,
        candidate_rows = nrow(z),
        measurement_in_window_rows = sum(z$measurement_in_window),
        available_by_landmark_rows =
          sum(z$measurement_in_window & z$available_by_end),
        unit_valid_rows = sum(
          z$measurement_in_window & z$available_by_end & z$unit_valid
        ),
        valid_rows = nrow(lab_eligible[lab_mapping == v]),
        selected_patients = if (v == "platelet") nrow(platelet) else nrow(creatinine)
      )
    }), fill = TRUE),
    data.table(
      database, component = "vasopressor",
      candidate_rows = nrow(pressor_join),
      measurement_in_window_rows = sum(pressor_join$interval_overlaps_window),
      available_by_landmark_rows = sum(
        pressor_join$interval_overlaps_window & pressor_join$available_by_end
      ),
      unit_valid_rows = NA_integer_,
      valid_rows = nrow(pressor_active),
      selected_patients = nrow(pressor_by_stay)
    )
  ), fill = TRUE)
  mapping <- rbindlist(list(
    chart[, .(
      database, source_table = "chartevents", variable = "map",
      raw_label = map_source[[1L]], raw_unit = as.character(valueuom[[1L]]),
      rows = .N, target_stays = uniqueN(stay_id),
      numeric_rows = sum(!is.na(value_num))
    ), by = .(itemid, map_source, valueuom)],
    lab[, .(
      database, source_table = "labevents", variable = lab_mapping[[1L]],
      raw_label = as.character(itemid[[1L]]),
      raw_unit = as.character(valueuom[[1L]]),
      rows = .N, target_stays = uniqueN(stay_id),
      numeric_rows = sum(!is.na(value_num))
    ), by = .(itemid, lab_mapping, valueuom)],
    input[, .(
      database, source_table = "inputevents", variable = "vasopressor",
      raw_label = drug_class[[1L]], raw_unit = as.character(rateuom[[1L]]),
      rows = .N, target_stays = uniqueN(stay_id),
      numeric_rows = sum(!is.na(rate_num))
    ), by = .(itemid, drug_class, rateuom)]
  ), fill = TRUE, use.names = TRUE)

  metadata <- list(
    version = "fixed6h_no_gcs_core_v2",
    database = "MIMIC-IV 3.1",
    outcome_blind = TRUE,
    freeze_date = as.character(LOCKED_V2$freeze_date),
    target_sha256 = sha256_file(target_path),
    script_sha256 = sha256_file(script_path),
    helper_sha256 = sha256_file(helper_path),
    window = "max(ICU intime,index-24h) through fixed index+6h landmark",
    availability = "chart/event time in window and storetime no later than landmark",
    units = c(
      map = "mmHg", vasopressor = "binary",
      platelet = "10^3/uL", creatinine = "mg/dL"
    ),
    outcome_artifact_read = FALSE
  )
  attr(core, "rebuild_metadata") <- metadata
  attr(tuple_core, "rebuild_metadata") <- c(
    metadata, list(role = "same-patient ventilator-model common-set source")
  )
  all_path <- file.path(
    private_dir, "mimic_fixed6h_all_at_risk_no_gcs_core_v2.rds"
  )
  tuple_path <- file.path(
    private_dir, "mimic_fixed6h_tuple_no_gcs_core_v2.rds"
  )
  atomic_save_rds(core, all_path)
  atomic_save_rds(tuple_core, tuple_path)
  output_paths <- c(all_at_risk = all_path, tuple = tuple_path)
  write_database_qc(
    database, core, timing, mapping, invariants, manifest,
    provenance, output_paths
  )
  db_gate <- data.table(
    field = c(
      "status", "locked_config_version", "all_invariants_pass",
      "outcome_leakage_guard_pass",
      "target_rows", "tuple_rows", "complete_no_gcs_rows",
      "tuple_and_complete_no_gcs_rows", "script_sha256", "helper_sha256",
      "selftest_sha256", "selftest_pass",
      "selection_target_sha256", "cache_gate_sha256",
      "all_at_risk_output_sha256", "tuple_output_sha256", "completed_at"
    ),
    value = c(
      "PASS", LOCKED_V2$version, "TRUE", "TRUE",
      nrow(core), nrow(tuple_core), sum(core$complete_no_gcs_core),
      sum(core$tuple_and_complete_no_gcs_core),
      sha256_file(script_path), sha256_file(helper_path),
      sha256_file(selftest_path), "TRUE",
      sha256_file(target_path), sha256_file(filter$gate_path),
      sha256_file(all_path), sha256_file(tuple_path),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    )
  )
  gate_path <- file.path(qc_root, database, "mimic_no_gcs_core_complete_v2.csv")
  fwrite(db_gate, gate_path)
  list(
    core = core, tuple = tuple_core,
    paths = output_paths, gate_path = gate_path, gate = db_gate
  )
}

build_eicu <- function(provenance, fixed_gate) {
  database <- "eicu"
  private_dir <- file.path(PRIVATE_ROOT, database)
  cache_dir <- file.path(private_dir, "cache_v2", "no_gcs_core")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  target_path <- file.path(
    private_dir, "eicu_all_landmark_at_risk_selection_targets_v2.rds"
  )
  tuple_source_path <- file.path(
    private_dir, "eicu_no_gcs_core_targets_v2.rds"
  )
  required_inputs <- c(target_path, tuple_source_path, fixed_gate_path, helper_path)
  if (any(!file.exists(required_inputs))) {
    stop("Missing eICU input(s): ", paste(required_inputs[!file.exists(required_inputs)], collapse = ", "))
  }
  require_gate(fixed_gate, "locked_config_version", LOCKED_V2$version)
  require_gate(
    fixed_gate, "eicu_selection_target_sha256", sha256_file(target_path)
  )
  target <- as.data.table(readRDS(target_path))
  tuple_source <- as.data.table(readRDS(tuple_source_path))
  required <- c(
    "patientunitstayid", "patienthealthsystemstayid", "person_key",
    "hospitalid", "age", "sex", "index_pf", "index_peep",
    "index_time", "landmark_time", "covariate_window_start",
    "covariate_window_end", "tuple_observed", "n_valid_tuples"
  )
  missing <- setdiff(required, names(target))
  if (length(missing)) stop("eICU target missing: ", paste(missing, collapse = ", "))
  assert_no_leakage(target, "eICU selection target")
  if (
    nrow(target) != 5509L || anyDuplicated(target$patientunitstayid) ||
    any(target$covariate_window_start > target$covariate_window_end) ||
    any(abs(target$covariate_window_end - target$landmark_time) > 1e-6)
  ) {
    stop("eICU target row/key/window invariant failed.")
  }
  key_path <- file.path(cache_dir, "target_ids_v2.txt")
  fwrite(target[, .(patientunitstayid)], key_path, col.names = FALSE)
  filter <- run_filter(database, key_path, EICU_ROOT, cache_dir, nrow(target))
  manifest <- filter$manifest
  bounds <- target[, .(
    patientunitstayid,
    window_start = as.numeric(covariate_window_start),
    window_end = as.numeric(covariate_window_end)
  )]

  nurse <- read_cache(
    file.path(cache_dir, "nurse_map_candidates_v2.csv.gz"),
    "nurseCharting",
    c(
      "patientunitstayid", "nursingchartoffset",
      "nursingchartentryoffset", "nursingchartcelltypevallabel",
      "nursingchartcelltypevalname", "nursingchartvalue"
    ),
    manifest
  )
  nurse[, `:=`(
    map_source = fcase(
      nursingchartcelltypevallabel == "Non-Invasive BP" &
        nursingchartcelltypevalname == "Non-Invasive BP Mean",
      "map_noninvasive",
      nursingchartcelltypevallabel == "Invasive BP" &
        nursingchartcelltypevalname == "Invasive BP Mean",
      "map_invasive",
      nursingchartcelltypevallabel == "MAP (mmHg)" &
        nursingchartcelltypevalname == "Value",
      "map_invasive",
      nursingchartcelltypevallabel == "Arterial Line MAP (mmHg)" &
        nursingchartcelltypevalname == "Value",
      "map_invasive",
      default = "candidate_not_used"
    ),
    value_num = strict_numeric(nursingchartvalue),
    measurement_time = suppressWarnings(as.numeric(nursingchartoffset)),
    entry_time = suppressWarnings(as.numeric(nursingchartentryoffset))
  )]
  nurse[, available_time := fifelse(
    is.na(entry_time), measurement_time, pmax(measurement_time, entry_time)
  )]
  map_join <- merge(
    nurse[map_source != "candidate_not_used"],
    bounds, by = "patientunitstayid", all = FALSE, sort = FALSE
  )
  map_join[, `:=`(
    measurement_in_window =
      measurement_time >= window_start & measurement_time <= window_end,
    available_by_end =
      !is.na(available_time) & available_time <= window_end,
    value_valid = !is.na(value_num) & value_num >= 1 & value_num <= 250
  )]
  map_eligible <- map_join[
    measurement_in_window & available_by_end & value_valid
  ]
  map_reduced <- map_eligible[, .(
    map_value = median(value_num),
    map_available_time = max(available_time),
    duplicate_rows = .N
  ), by = .(
    patientunitstayid, map_time = measurement_time, map_source,
    map_source_label = nursingchartcelltypevallabel,
    map_source_name = nursingchartcelltypevalname
  )]
  map_reduced[, source_rank := fifelse(map_source == "map_invasive", 1L, 2L)]
  setorder(
    map_reduced, patientunitstayid, map_value, source_rank,
    map_time, map_available_time
  )
  map_selected <- map_reduced[, .SD[1L], by = patientunitstayid]
  setnames(map_selected, "map_value", "map_min")
  map_selected[, c("source_rank", "duplicate_rows") := NULL]

  lab <- read_cache(
    file.path(cache_dir, "lab_core_candidates_v2.csv.gz"),
    "lab",
    c(
      "patientunitstayid", "labresultoffset", "labname", "labresult",
      "labmeasurenamesystem", "labmeasurenameinterface",
      "labresultrevisedoffset"
    ),
    manifest
  )
  lab[, `:=`(
    value_num = strict_numeric(labresult),
    measurement_time = suppressWarnings(as.numeric(labresultoffset)),
    revision_time = suppressWarnings(as.numeric(labresultrevisedoffset))
  )]
  lab[, available_time := fifelse(
    is.na(revision_time), measurement_time,
    pmax(measurement_time, revision_time)
  )]
  lab[, unit_valid := fcase(
    labname == "creatinine",
    tolower(trimws(labmeasurenamesystem)) == "mg/dl",
    labname == "platelets x 1000",
    tolower(trimws(labmeasurenamesystem)) == "k/mcl",
    default = FALSE
  )]
  lab[is.na(unit_valid), unit_valid := FALSE]
  lab[, value_valid := fcase(
    labname == "creatinine",
    !is.na(value_num) & value_num >= 0.1 & value_num <= 28.28,
    labname == "platelets x 1000",
    !is.na(value_num) & value_num > 0 & value_num <= 9999,
    default = FALSE
  )]
  lab_join <- merge(lab, bounds, by = "patientunitstayid", all = FALSE, sort = FALSE)
  lab_join[, `:=`(
    measurement_in_window =
      measurement_time >= window_start & measurement_time <= window_end,
    available_by_end =
      !is.na(available_time) & available_time <= window_end
  )]
  lab_eligible <- lab_join[
    measurement_in_window & available_by_end & unit_valid & value_valid
  ]
  latest <- lab_eligible[
    , .SD[available_time == max(available_time)],
    by = .(patientunitstayid, labname, measurement_time)
  ]
  reduced <- latest[, {
    values <- unique(value_num)
    list(
      value_conflict = length(values) > 1L,
      lab_value = if (length(values) == 1L) values[[1L]] else NA_real_,
      lab_available_time =
        if (length(values) == 1L) available_time[[1L]] else NA_real_
    )
  }, by = .(patientunitstayid, labname, lab_time = measurement_time)]
  reduced <- reduced[value_conflict == FALSE & !is.na(lab_value)]
  platelet <- reduced[labname == "platelets x 1000"]
  setorder(platelet, patientunitstayid, lab_value, lab_time, lab_available_time)
  platelet <- platelet[, .SD[1L], by = patientunitstayid]
  setnames(
    platelet,
    c("lab_value", "lab_time", "lab_available_time"),
    c("platelet_min", "platelet_time", "platelet_available_time")
  )
  platelet <- platelet[, .(
    patientunitstayid, platelet_min, platelet_time, platelet_available_time
  )]
  creatinine <- reduced[labname == "creatinine"]
  setorder(
    creatinine, patientunitstayid, -lab_value, lab_time, lab_available_time
  )
  creatinine <- creatinine[, .SD[1L], by = patientunitstayid]
  setnames(
    creatinine,
    c("lab_value", "lab_time", "lab_available_time"),
    c("creatinine_max", "creatinine_time", "creatinine_available_time")
  )
  creatinine <- creatinine[, .(
    patientunitstayid, creatinine_max, creatinine_time,
    creatinine_available_time
  )]

  infusion <- read_cache(
    file.path(cache_dir, "infusion_pressor_candidates_v2.csv.gz"),
    "infusionDrug",
    c(
      "patientunitstayid", "infusionoffset", "drugname",
      "drugrate", "infusionrate"
    ),
    manifest
  )
  infusion[, drugname_normalized := tolower(trimws(as.character(drugname)))]
  infusion[, drug_class := fcase(
    grepl(
      "norepinephrine|levophed|^nss with levo|^nss w/ levo/vaso",
      drugname_normalized
    ), "norepinephrine",
    grepl("epineph|adrenalin|^epi \\(", drugname_normalized), "epinephrine",
    grepl("vasopressin", drugname_normalized), "vasopressin",
    grepl("dopamine|inotropin", drugname_normalized), "dopamine",
    grepl("dobu", drugname_normalized), "dobutamine",
    grepl("phenylephrine|neo[- ]?synephrine|neosynsprine", drugname_normalized),
    "phenylephrine",
    default = NA_character_
  )]
  infusion[, `:=`(
    rate_num = strict_numeric(drugrate),
    measurement_time = suppressWarnings(as.numeric(infusionoffset))
  )]
  infusion[, rate_status := fcase(
    !is.na(rate_num) & rate_num > 0, "positive",
    !is.na(rate_num) & rate_num == 0, "zero",
    !is.na(rate_num) & rate_num < 0, "negative",
    default = "missing_or_nonnumeric"
  )]

  medication <- read_cache(
    file.path(cache_dir, "medication_pressor_candidates_v2.csv.gz"),
    "medication",
    c(
      "patientunitstayid", "drugorderoffset", "drugstartoffset",
      "drugivadmixture", "drugordercancelled", "drugname", "drughiclseqno",
      "dosage", "routeadmin", "prn", "drugstopoffset"
    ),
    manifest
  )
  hicl_map <- list(
    norepinephrine = c(37410L, 36346L, 2051L),
    epinephrine = c(37407L, 39089L, 36437L, 34361L, 2050L),
    dobutamine = c(8777L, 40L),
    dopamine = c(2060L, 2059L),
    vasopressin = c(38884L, 38883L, 2839L),
    phenylephrine = c(37028L, 35517L, 35587L, 2087L)
  )
  medication[, `:=`(
    hicl_num = suppressWarnings(as.integer(drughiclseqno)),
    drugname_normalized = tolower(trimws(as.character(drugname)))
  )]
  medication[, drug_class := fcase(
    hicl_num %in% hicl_map$norepinephrine, "norepinephrine",
    hicl_num %in% hicl_map$epinephrine, "epinephrine",
    hicl_num %in% hicl_map$dobutamine, "dobutamine",
    hicl_num %in% hicl_map$dopamine, "dopamine",
    hicl_num %in% hicl_map$vasopressin, "vasopressin",
    hicl_num %in% hicl_map$phenylephrine, "phenylephrine",
    is.na(hicl_num) & grepl("norepinephrine|levophed", drugname_normalized),
    "norepinephrine",
    is.na(hicl_num) & grepl("^epinephrine|adrenalin", drugname_normalized),
    "epinephrine",
    is.na(hicl_num) & grepl("vasopressin", drugname_normalized), "vasopressin",
    is.na(hicl_num) & grepl("dopamine|inotropin", drugname_normalized), "dopamine",
    is.na(hicl_num) & grepl("dobutamine|dobutrex", drugname_normalized),
    "dobutamine",
    is.na(hicl_num) &
      grepl("phenylephrine|neo[- ]?synephrine|neosynsprine", drugname_normalized),
    "phenylephrine",
    default = NA_character_
  )]
  medication[, `:=`(
    start_time_raw = suppressWarnings(as.numeric(drugstartoffset)),
    stop_time_raw = suppressWarnings(as.numeric(drugstopoffset)),
    order_time_raw = suppressWarnings(as.numeric(drugorderoffset))
  )]
  medication[, `:=`(
    start_time = fifelse(start_time_raw == 0, NA_real_, start_time_raw),
    stop_time = fifelse(stop_time_raw == 0, NA_real_, stop_time_raw),
    order_time = fifelse(order_time_raw == 0, NA_real_, order_time_raw)
  )]
  medication[, order_available_time := fcase(
    !is.na(order_time) & !is.na(start_time), pmax(order_time, start_time),
    is.na(order_time) & !is.na(start_time), start_time,
    default = NA_real_
  )]
  medication[, route_normalized := toupper(trimws(as.character(routeadmin)))]
  medication[, `:=`(
    parenteral_route =
      drugivadmixture == "Yes" |
        route_normalized %chin% c("IV", "IVPB", "CENTRAL IV", "ZPYXVEND") |
        grepl("^INTRAVEN", route_normalized),
    noncancelled = drugordercancelled == "No",
    nonprn = is.na(prn) | prn != "Yes",
    dosage_present = !is.na(dosage) & nzchar(trimws(as.character(dosage))),
    interval_valid =
      !is.na(start_time) & (is.na(stop_time) | stop_time >= start_time)
  )]
  for (v in c(
    "parenteral_route", "noncancelled", "nonprn",
    "dosage_present", "interval_valid"
  )) {
    set(medication, which(is.na(medication[[v]])), v, FALSE)
  }

  inf <- merge(
    infusion[!is.na(drug_class)], bounds,
    by = "patientunitstayid", all = FALSE, sort = FALSE
  )
  inf[, in_window :=
    measurement_time >= window_start & measurement_time <= window_end]
  inf_window <- inf[in_window == TRUE]
  inf_patient <- inf_window[, .(
    pressor_positive_infusion = any(rate_status == "positive"),
    pressor_infusion_missing_rate_documented =
      any(rate_status == "missing_or_nonnumeric"),
    pressor_infusion_zero_only =
      !any(rate_status == "positive") & any(rate_status == "zero"),
    pressor_classes_infusion = paste(
      sort(unique(drug_class[rate_status == "positive"])), collapse = ";"
    ),
    infusion_first_time = if (any(rate_status == "positive")) {
      min(measurement_time[rate_status == "positive"])
    } else {
      NA_real_
    },
    infusion_available_time = if (any(rate_status == "positive")) {
      max(measurement_time[rate_status == "positive"])
    } else {
      NA_real_
    }
  ), by = patientunitstayid]

  med <- merge(
    medication[!is.na(drug_class)], bounds,
    by = "patientunitstayid", all = FALSE, sort = FALSE
  )
  med[, `:=`(
    interval_overlaps_window =
      start_time <= window_end &
        (is.na(stop_time) | stop_time >= window_start),
    available_by_end =
      !is.na(order_available_time) & order_available_time <= window_end
  )]
  med[, active_order :=
    noncancelled & nonprn & dosage_present & parenteral_route &
      interval_valid & interval_overlaps_window & available_by_end]
  med[is.na(active_order), active_order := FALSE]
  med_patient <- med[, .(
    pressor_active_medication_order = any(active_order),
    pressor_classes_medication = paste(
      sort(unique(drug_class[active_order == TRUE])), collapse = ";"
    ),
    medication_first_time = if (any(active_order)) {
      min(pmax(start_time[active_order], window_start[active_order]))
    } else {
      NA_real_
    },
    medication_last_time = if (any(active_order)) {
      max(pmin(
        fifelse(is.na(stop_time[active_order]),
                window_end[active_order], stop_time[active_order]),
        window_end[active_order]
      ))
    } else {
      NA_real_
    },
    medication_available_time = if (any(active_order)) {
      max(order_available_time[active_order])
    } else {
      NA_real_
    }
  ), by = patientunitstayid]
  pressor <- merge(
    bounds[, .(patientunitstayid)], inf_patient,
    by = "patientunitstayid", all.x = TRUE, sort = FALSE
  )
  pressor <- merge(
    pressor, med_patient,
    by = "patientunitstayid", all.x = TRUE, sort = FALSE
  )
  logical_fields <- c(
    "pressor_positive_infusion", "pressor_infusion_missing_rate_documented",
    "pressor_infusion_zero_only", "pressor_active_medication_order"
  )
  for (v in logical_fields) set(pressor, which(is.na(pressor[[v]])), v, FALSE)
  for (v in c("pressor_classes_infusion", "pressor_classes_medication")) {
    set(pressor, which(is.na(pressor[[v]])), v, "")
  }
  pressor[, vasopressor_any :=
    pressor_positive_infusion | pressor_active_medication_order]
  pressor[, vasopressor_documented_sensitivity :=
    vasopressor_any | pressor_infusion_missing_rate_documented]
  pressor[, vasopressor_source := fcase(
    pressor_positive_infusion & pressor_active_medication_order, "both",
    pressor_positive_infusion, "positive_infusion",
    pressor_active_medication_order, "active_medication_order",
    pressor_infusion_missing_rate_documented, "missing_rate_infusion_only",
    pressor_infusion_zero_only, "zero_rate_infusion_only",
    default = "none"
  )]
  pressor[, vasopressor_first_time := pmin(
    infusion_first_time, medication_first_time, na.rm = TRUE
  )]
  pressor[
    !is.finite(vasopressor_first_time), vasopressor_first_time := NA_real_
  ]
  pressor[, vasopressor_last_time := pmax(
    infusion_available_time, medication_last_time, na.rm = TRUE
  )]
  pressor[
    !is.finite(vasopressor_last_time), vasopressor_last_time := NA_real_
  ]
  pressor[, vasopressor_available_time := pmax(
    infusion_available_time, medication_available_time, na.rm = TRUE
  )]
  pressor[
    !is.finite(vasopressor_available_time),
    vasopressor_available_time := NA_real_
  ]

  core <- merge(
    target[, .(
      patientunitstayid, patienthealthsystemstayid, person_key,
      hospitalid, age, sex, index_pf, index_peep,
      index_time, landmark_time, covariate_window_start,
      covariate_window_end, tuple_observed, n_valid_tuples
    )],
    map_selected, by = "patientunitstayid", all.x = TRUE, sort = FALSE
  )
  core <- merge(core, platelet, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  core <- merge(core, creatinine, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  core <- merge(core, pressor, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
  tuple_time <- tuple_source[, .(
    patientunitstayid, tuple_time = ventilator_tuple_available_time
  )]
  core <- merge(
    core, tuple_time, by = "patientunitstayid", all.x = TRUE, sort = FALSE
  )
  core <- standardize_common(core, database)
  core_keep <- c(
    "patientunitstayid", "patienthealthsystemstayid", "person_key",
    "hospitalid", "hospital_id", "age", "sex", "sex_recognized", "sex_female",
    "pf_ratio", "index_peep", "index_time", "landmark_time",
    "covariate_window_start", "covariate_window_end",
    "tuple_observed", "n_valid_tuples", "tuple_time",
    "map", "vasopressor", "platelet", "creatinine",
    "age_missing", "sex_missing", "pf_ratio_missing", "map_missing",
    "vasopressor_missing", "platelet_missing", "creatinine_missing",
    "complete_no_gcs_core", "tuple_and_complete_no_gcs_core",
    "map_time", "map_available_time", "map_source",
    "map_source_label", "map_source_name",
    "platelet_time", "platelet_available_time",
    "creatinine_time", "creatinine_available_time",
    "vasopressor_first_time", "vasopressor_last_time",
    "vasopressor_available_time", "vasopressor_source",
    "vasopressor_documented_sensitivity",
    "pressor_positive_infusion", "pressor_active_medication_order",
    "pressor_infusion_missing_rate_documented",
    "pressor_infusion_zero_only", "pressor_classes_infusion",
    "pressor_classes_medication"
  )
  core <- core[, ..core_keep]
  setorder(core, patientunitstayid)
  assert_no_leakage(core, "eICU all-at-risk no-GCS core")
  tuple_core <- core[tuple_observed == TRUE]
  tuple_core <- tuple_core[
    match(tuple_source$patientunitstayid, tuple_core$patientunitstayid)
  ]
  if (
    nrow(tuple_core) != nrow(tuple_source) ||
    anyDuplicated(tuple_core$patientunitstayid) ||
    !identical(
      tuple_core$patientunitstayid,
      tuple_source$patientunitstayid
    ) ||
    anyNA(tuple_core$tuple_observed) ||
    any(tuple_core$tuple_observed != TRUE)
  ) {
    stop("eICU tuple-core join failed.")
  }
  assert_no_leakage(tuple_core, "eICU tuple no-GCS core")

  window_start <- core$covariate_window_start
  window_end <- core$covariate_window_end
  checks <- list(
    target_row_count = nrow(core) == nrow(target),
    unique_id = !anyDuplicated(core$patientunitstayid),
    exact_id_set = setequal(core$patientunitstayid, target$patientunitstayid),
    tuple_count = sum(core$tuple_observed) == nrow(tuple_source),
    tuple_time_only_if_observed =
      all(is.na(core$tuple_time) == !core$tuple_observed),
    hospital_id_known = all(!is.na(core$hospital_id) & nzchar(core$hospital_id)),
    map_range = all(is.na(core$map) | core$map >= 1 & core$map <= 250),
    platelet_range =
      all(is.na(core$platelet) | core$platelet > 0 & core$platelet <= 9999),
    creatinine_range =
      all(is.na(core$creatinine) |
            core$creatinine >= 0.1 & core$creatinine <= 28.28),
    pressor_binary = all(core$vasopressor %in% c(0L, 1L)),
    map_measurement_in_window = all(
      is.na(core$map_time) |
        (core$map_time >= window_start & core$map_time <= window_end)
    ),
    map_available_by_landmark = all(
      is.na(core$map_available_time) |
        core$map_available_time <= window_end
    ),
    platelet_measurement_in_window = all(
      is.na(core$platelet_time) |
        (core$platelet_time >= window_start &
           core$platelet_time <= window_end)
    ),
    platelet_available_by_landmark = all(
      is.na(core$platelet_available_time) |
        core$platelet_available_time <= window_end
    ),
    creatinine_measurement_in_window = all(
      is.na(core$creatinine_time) |
        (core$creatinine_time >= window_start &
           core$creatinine_time <= window_end)
    ),
    creatinine_available_by_landmark = all(
      is.na(core$creatinine_available_time) |
        core$creatinine_available_time <= window_end
    ),
    pressor_time_in_window = all(
      is.na(core$vasopressor_first_time) |
        (core$vasopressor_first_time >= window_start &
           core$vasopressor_last_time <= window_end)
    ),
    pressor_available_by_landmark = all(
      is.na(core$vasopressor_available_time) |
        core$vasopressor_available_time <= window_end
    ),
    complete_flag_exact = all(
      core$complete_no_gcs_core ==
        (
          !core$age_missing & !core$sex_missing & !core$pf_ratio_missing &
            !core$map_missing & !core$vasopressor_missing &
            !core$platelet_missing & !core$creatinine_missing
        )
    ),
    no_outcome_like_fields =
      !any(grepl(FORBIDDEN_PATTERN, names(core), ignore.case = TRUE)),
    cache_all_sources_eof = all(manifest$reached_eof == TRUE)
  )
  invariants <- data.table(
    database, check = names(checks),
    pass = unlist(checks, use.names = FALSE)
  )
  if (any(invariants$pass != TRUE)) {
    stop(
      "eICU no-GCS invariant failure: ",
      paste(invariants[pass != TRUE, check], collapse = ", ")
    )
  }

  timing <- rbindlist(list(
    data.table(
      database, component = "map",
      candidate_rows = nrow(map_join),
      measurement_in_window_rows = sum(map_join$measurement_in_window),
      available_by_landmark_rows =
        sum(map_join$measurement_in_window & map_join$available_by_end),
      unit_valid_rows = NA_integer_,
      valid_rows = nrow(map_eligible),
      selected_patients = nrow(map_selected)
    ),
    rbindlist(lapply(c("platelets x 1000", "creatinine"), function(v) {
      z <- lab_join[labname == v]
      data.table(
        database,
        component = if (v == "platelets x 1000") "platelet" else "creatinine",
        candidate_rows = nrow(z),
        measurement_in_window_rows = sum(z$measurement_in_window),
        available_by_landmark_rows =
          sum(z$measurement_in_window & z$available_by_end),
        unit_valid_rows = sum(
          z$measurement_in_window & z$available_by_end & z$unit_valid
        ),
        valid_rows = nrow(lab_eligible[labname == v]),
        selected_patients =
          if (v == "platelets x 1000") nrow(platelet) else nrow(creatinine)
      )
    }), fill = TRUE),
    data.table(
      database, component = "vasopressor",
      candidate_rows = nrow(inf) + nrow(med),
      measurement_in_window_rows =
        nrow(inf_window) + sum(med$interval_overlaps_window),
      available_by_landmark_rows =
        nrow(inf_window) +
          sum(med$interval_overlaps_window & med$available_by_end),
      unit_valid_rows = NA_integer_,
      valid_rows =
        sum(inf_window$rate_status == "positive") + sum(med$active_order),
      selected_patients = sum(core$vasopressor == 1L)
    )
  ), fill = TRUE)
  mapping <- rbindlist(list(
    nurse[, .(
      database, source_table = "nurseCharting", variable = "map",
      raw_label = paste(
        nursingchartcelltypevallabel[[1L]],
        nursingchartcelltypevalname[[1L]], sep = " | "
      ),
      raw_unit = "mmHg (label-defined)", rows = .N,
      target_stays = uniqueN(patientunitstayid),
      numeric_rows = sum(!is.na(value_num))
    ), by = .(
      nursingchartcelltypevallabel, nursingchartcelltypevalname, map_source
    )],
    lab[, .(
      database, source_table = "lab",
      variable =
        if (labname[[1L]] == "platelets x 1000") "platelet" else "creatinine",
      raw_label = labname[[1L]],
      raw_unit = as.character(labmeasurenamesystem[[1L]]),
      rows = .N, target_stays = uniqueN(patientunitstayid),
      numeric_rows = sum(!is.na(value_num))
    ), by = .(labname, labmeasurenamesystem, labmeasurenameinterface)],
    infusion[, .(
      database, source_table = "infusionDrug", variable = "vasopressor",
      raw_label = as.character(drugname[[1L]]), raw_unit = "drugrate raw",
      rows = .N, target_stays = uniqueN(patientunitstayid),
      numeric_rows = sum(!is.na(rate_num))
    ), by = .(drug_class, drugname)],
    medication[, .(
      database, source_table = "medication", variable = "vasopressor",
      raw_label = as.character(drugname[[1L]]),
      raw_unit = as.character(routeadmin[[1L]]),
      rows = .N, target_stays = uniqueN(patientunitstayid),
      numeric_rows = sum(!is.na(hicl_num))
    ), by = .(drug_class, drugname, hicl_num, routeadmin)]
  ), fill = TRUE, use.names = TRUE)

  metadata <- list(
    version = "fixed6h_no_gcs_core_v2",
    database = "eICU-CRD 2.0",
    outcome_blind = TRUE,
    freeze_date = as.character(LOCKED_V2$freeze_date),
    target_sha256 = sha256_file(target_path),
    script_sha256 = sha256_file(script_path),
    helper_sha256 = sha256_file(helper_path),
    window = "max(ICU offset 0,index-1440 min) through fixed index+360 min landmark",
    availability = paste(
      "nurse entry offset and lab revision offset, when present,",
      "must be no later than landmark; order availability required for medication"
    ),
    units = c(
      map = "mmHg", vasopressor = "binary",
      platelet = "10^3/uL", creatinine = "mg/dL"
    ),
    mapping_provenance = paste(
      "MIT-LCP/eicu-code commit",
      "34cece8c70771a3fab48da84d4c47f0e133ca021"
    ),
    outcome_artifact_read = FALSE
  )
  attr(core, "rebuild_metadata") <- metadata
  attr(tuple_core, "rebuild_metadata") <- c(
    metadata, list(role = "same-patient external-validation common-set source")
  )
  all_path <- file.path(
    private_dir, "eicu_fixed6h_all_at_risk_no_gcs_core_v2.rds"
  )
  tuple_path <- file.path(
    private_dir, "eicu_fixed6h_tuple_no_gcs_core_v2.rds"
  )
  atomic_save_rds(core, all_path)
  atomic_save_rds(tuple_core, tuple_path)
  output_paths <- c(all_at_risk = all_path, tuple = tuple_path)
  write_database_qc(
    database, core, timing, mapping, invariants, manifest,
    provenance, output_paths
  )
  db_gate <- data.table(
    field = c(
      "status", "locked_config_version", "all_invariants_pass",
      "outcome_leakage_guard_pass",
      "target_rows", "tuple_rows", "complete_no_gcs_rows",
      "tuple_and_complete_no_gcs_rows", "script_sha256", "helper_sha256",
      "selftest_sha256", "selftest_pass",
      "selection_target_sha256", "cache_gate_sha256",
      "all_at_risk_output_sha256", "tuple_output_sha256", "completed_at"
    ),
    value = c(
      "PASS", LOCKED_V2$version, "TRUE", "TRUE",
      nrow(core), nrow(tuple_core), sum(core$complete_no_gcs_core),
      sum(core$tuple_and_complete_no_gcs_core),
      sha256_file(script_path), sha256_file(helper_path),
      sha256_file(selftest_path), "TRUE",
      sha256_file(target_path), sha256_file(filter$gate_path),
      sha256_file(all_path), sha256_file(tuple_path),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    )
  )
  gate_path <- file.path(qc_root, database, "eicu_no_gcs_core_complete_v2.csv")
  fwrite(db_gate, gate_path)
  list(
    core = core, tuple = tuple_core,
    paths = output_paths, gate_path = gate_path, gate = db_gate
  )
}

write_combined_gate <- function() {
  mimic_gate_path <- file.path(
    qc_root, "mimic", "mimic_no_gcs_core_complete_v2.csv"
  )
  eicu_gate_path <- file.path(
    qc_root, "eicu", "eicu_no_gcs_core_complete_v2.csv"
  )
  if (!file.exists(mimic_gate_path) || !file.exists(eicu_gate_path)) return(FALSE)
  mimic <- read_gate_map(mimic_gate_path)
  eicu <- read_gate_map(eicu_gate_path)
  require_gate(mimic, "status", "PASS")
  require_gate(eicu, "status", "PASS")
  require_gate(mimic, "all_invariants_pass", "TRUE")
  require_gate(eicu, "all_invariants_pass", "TRUE")
  require_gate(mimic, "selftest_pass", "TRUE")
  require_gate(eicu, "selftest_pass", "TRUE")
  require_gate(mimic, "outcome_leakage_guard_pass", "TRUE")
  require_gate(eicu, "outcome_leakage_guard_pass", "TRUE")
  combined <- data.table(
    field = c(
      "status", "locked_config_version", "all_invariants_pass",
      "outcome_leakage_guard_pass",
      "script_sha256", "helper_sha256", "selftest_sha256", "selftest_pass",
      "mimic_all_at_risk_rows", "mimic_tuple_rows",
      "mimic_complete_no_gcs_rows", "mimic_tuple_complete_no_gcs_rows",
      "mimic_all_at_risk_output_sha256", "mimic_tuple_output_sha256",
      "eicu_all_at_risk_rows", "eicu_tuple_rows",
      "eicu_complete_no_gcs_rows", "eicu_tuple_complete_no_gcs_rows",
      "eicu_all_at_risk_output_sha256", "eicu_tuple_output_sha256",
      "mimic_database_gate_sha256", "eicu_database_gate_sha256",
      "completed_at"
    ),
    value = c(
      "PASS", LOCKED_V2$version, "TRUE", "TRUE",
      sha256_file(script_path), sha256_file(helper_path),
      sha256_file(selftest_path), "TRUE",
      mimic[["target_rows"]], mimic[["tuple_rows"]],
      mimic[["complete_no_gcs_rows"]],
      mimic[["tuple_and_complete_no_gcs_rows"]],
      mimic[["all_at_risk_output_sha256"]], mimic[["tuple_output_sha256"]],
      eicu[["target_rows"]], eicu[["tuple_rows"]],
      eicu[["complete_no_gcs_rows"]],
      eicu[["tuple_and_complete_no_gcs_rows"]],
      eicu[["all_at_risk_output_sha256"]], eicu[["tuple_output_sha256"]],
      sha256_file(mimic_gate_path), sha256_file(eicu_gate_path),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    )
  )
  fwrite(
    combined,
    file.path(qc_root, "phase2b_no_gcs_core_complete_v2.csv")
  )
  TRUE
}

if (!file.exists(fixed_gate_path)) {
  stop("Fixed-landmark gate is missing: ", fixed_gate_path)
}
if (!file.exists(helper_path)) stop("No-GCS filter helper is missing.")
if (!file.exists(selftest_path)) stop("No-GCS selftest is missing.")
selftest_output <- system2(
  "Rscript", shQuote(selftest_path), stdout = TRUE, stderr = TRUE
)
selftest_status <- attr(selftest_output, "status")
if (
  (!is.null(selftest_status) && selftest_status != 0L) ||
  !"REBUILD_V2_NO_GCS_CORE_SYNTHETIC_PASS" %chin% selftest_output
) {
  stop("No-GCS synthetic selftest failed: ", paste(selftest_output, collapse = "\n"))
}
fixed_gate <- read_gate_map(fixed_gate_path)
provenance <- assert_source_provenance()

results <- list()
if (database_arg %chin% c("mimic", "both")) {
  results$mimic <- build_mimic(provenance, fixed_gate)
}
if (database_arg %chin% c("eicu", "both")) {
  results$eicu <- build_eicu(provenance, fixed_gate)
}
combined_written <- write_combined_gate()
message(
  "REBUILD_V2_NO_GCS_CORE_PASS database=", database_arg,
  " combined_gate=", combined_written
)
