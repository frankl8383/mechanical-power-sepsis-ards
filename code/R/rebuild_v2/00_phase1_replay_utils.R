#!/usr/bin/env Rscript

# Utilities for the post-review rebuild_v2 cohort/exposure pipeline.
#
# Design:
# - Reuse the already audited rebuild_v1 respiratory phenotype and ventilator
#   tuple code without copying thousands of lines into a divergent fork.
# - Pin every reused source file by SHA256.
# - Evaluate Phase-1 cohort code in an isolated environment and stop immediately
#   after the respiratory-eligible `stage5` object is created, before the MIMIC
#   antibiotic/culture infection block and before any formal v1 save/QC block.
# - Redirect every replay output to analysis_rebuild_v2 scratch/output roots.
# - Verify that no file under analysis_rebuild_v1 changed during replay.
#
# This file assumes code/R/rebuild_v2/00_config.R has already been sourced.

suppressPackageStartupMessages(library(data.table))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required for source pinning and completion gates.")
}

V1_SOURCE_SPEC <- list(
  mimic_phase1 = list(
    relative_path = "code/R/rebuild_v1/01_build_mimic_index_cohort.R",
    sha256 = "1a7c8d8b191c0284dbf3e004e3772789d03b2df441681d8c94dc315aa70de6ba"
  ),
  eicu_phase1 = list(
    relative_path = "code/R/rebuild_v1/02_build_eicu_index_cohort.R",
    sha256 = "7da33e3056157f2b564554c1f9074a40ba54861a9834379dcecf8635e0c2510c"
  ),
  mimic_exposure = list(
    relative_path = "code/R/rebuild_v1/03_build_mimic_paired_exposure.R",
    sha256 = "9b7b6acea6ca026eb1526a752fc7008e6b1012abdc9e07c38142a56c36e2ee2d"
  ),
  eicu_exposure = list(
    relative_path = "code/R/rebuild_v1/04_build_eicu_paired_exposure.R",
    sha256 = "a364d00ab715e99b4b40403a55651f1f2395cd1ba258e69fa000428d3762a8e0"
  )
)

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

assert_pinned_source <- function(spec_name) {
  spec <- V1_SOURCE_SPEC[[spec_name]]
  if (is.null(spec)) stop("Unknown pinned source: ", spec_name)
  path <- file.path(PROJECT_ROOT, spec$relative_path)
  if (!file.exists(path)) stop("Pinned v1 source is missing: ", path)
  observed <- sha256_file(path)
  if (!identical(observed, spec$sha256)) {
    stop(
      "Pinned v1 source changed; review before replaying. ",
      spec$relative_path, " expected ", spec$sha256, " observed ", observed
    )
  }
  normalizePath(path, mustWork = TRUE)
}

snapshot_v1_tree <- function() {
  files <- list.files(
    REBUILD_V1_ROOT, recursive = TRUE, full.names = TRUE,
    all.files = TRUE, include.dirs = FALSE
  )
  files <- files[file.exists(files)]
  info <- file.info(files)
  data.table(
    path = normalizePath(files, mustWork = TRUE),
    size = as.numeric(info$size),
    mtime = format(info$mtime, "%Y-%m-%d %H:%M:%OS6 %z")
  )[order(path)]
}

assert_v1_tree_unchanged <- function(before) {
  after <- snapshot_v1_tree()
  if (!identical(before, after)) {
    changed <- merge(
      before, after, by = "path", all = TRUE,
      suffixes = c("_before", "_after")
    )[
      is.na(size_before) | is.na(size_after) |
        size_before != size_after | mtime_before != mtime_after
    ]
    stop(
      "Read-only provenance guard failed: analysis_rebuild_v1 changed during replay. ",
      "Changed file count: ", nrow(changed)
    )
  }
  invisible(TRUE)
}

phase1_compatibility_lock <- function() {
  list(
    version = paste0(LOCKED_V2$version, "-phase1-compat"),
    minimum_age_years = LOCKED_V2$minimum_age_years,
    pf_threshold_mmHg = LOCKED_V2$pf_threshold_mmHg,
    minimum_index_peep_cmH2O = LOCKED_V2$minimum_index_peep_cmH2O,
    pao2_fio2_pair_window_minutes =
      LOCKED_V2$pao2_fio2_pair_window_minutes,
    pao2_peep_pair_window_minutes =
      LOCKED_V2$pao2_peep_pair_window_minutes,
    infection_window_hours_before_index =
      LOCKED_V2$infection_sensitivity$window_hours_before_index,
    infection_window_hours_after_index =
      LOCKED_V2$infection_sensitivity$window_hours_after_index,
    sensitivity_infection_window_hours_after_index = 24,
    primary_exposure_window_hours_after_index =
      LOCKED_V2$primary_exposure_window_hours,
    physiologic_ranges = LOCKED_V2$physiologic_ranges
  )
}

exposure_compatibility_lock <- function() {
  list(
    version = LOCKED_V2$version,
    primary_exposure_summary = LOCKED_V2$primary_tuple_rule,
    primary_exposure_window_hours_after_index =
      LOCKED_V2$primary_exposure_window_hours,
    primary_ventilator_tuple_pair_window_minutes =
      LOCKED_V2$tuple_pair_window_minutes,
    sensitivity_ventilator_tuple_pair_window_minutes =
      LOCKED_V2$tuple_pair_window_sensitivity_minutes,
    physiologic_ranges = LOCKED_V2$physiologic_ranges
  )
}

make_config_injector <- function(target_env, roots, locked_object) {
  values <- c(
    list(
      PROJECT_ROOT = PROJECT_ROOT,
      MIMIC_ROOT = MIMIC_ROOT,
      EICU_ROOT = EICU_ROOT,
      REBUILD_ROOT = roots$rebuild,
      PRIVATE_ROOT = roots$private,
      AGGREGATE_ROOT = roots$aggregate,
      QC_ROOT = roots$qc,
      LOCKED = locked_object
    )
  )
  force(target_env)
  force(values)
  function(file, local = FALSE, ...) {
    list2env(values, envir = target_env)
    invisible(NULL)
  }
}

seed_mimic_phase1_cache_links <- function(scratch_private) {
  source_dir <- file.path(
    REBUILD_V1_ROOT, "private", "mimic", "cache_v1"
  )
  target_dir <- file.path(scratch_private, "mimic", "cache_v1")
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  cache_names <- c(
    "selected_bg_labevents_v1.rds",
    "selected_index_chartevents_v1.rds"
  )
  source_files <- file.path(source_dir, cache_names)
  if (any(!file.exists(source_files))) {
    stop(
      "The audited v1 Phase-1 selection caches are required for safe replay: ",
      paste(source_files[!file.exists(source_files)], collapse = ", ")
    )
  }
  source_hash <- setNames(vapply(source_files, sha256_file, character(1)), cache_names)
  for (i in seq_along(source_files)) {
    target <- file.path(target_dir, cache_names[[i]])
    if (file.exists(target) || nzchar(Sys.readlink(target))) unlink(target)
    ok <- file.symlink(normalizePath(source_files[[i]]), target)
    if (!isTRUE(ok)) stop("Could not create read-only cache link: ", target)
  }
  list(source_files = source_files, source_hash = source_hash)
}

assert_cache_hashes_unchanged <- function(cache_guard) {
  after <- setNames(
    vapply(cache_guard$source_files, sha256_file, character(1)),
    names(cache_guard$source_hash)
  )
  if (!identical(cache_guard$source_hash, after)) {
    stop("A read-only v1 Phase-1 selection cache changed during replay.")
  }
  invisible(TRUE)
}

assignment_name <- function(expr) {
  if (!is.call(expr) || length(expr) < 3L) return(NA_character_)
  op <- as.character(expr[[1L]])
  if (!op %in% c("<-", "=")) return(NA_character_)
  lhs <- expr[[2L]]
  if (is.symbol(lhs)) as.character(lhs) else NA_character_
}

replay_v1_respiratory_stage5 <- function(database = c("mimic", "eicu")) {
  database <- match.arg(database)
  spec_name <- paste0(database, "_phase1")
  source_path <- assert_pinned_source(spec_name)
  v1_before <- snapshot_v1_tree()

  scratch_root <- file.path(
    PRIVATE_ROOT, "_phase1_replay_scratch", database
  )
  roots <- list(
    rebuild = scratch_root,
    private = file.path(scratch_root, "private"),
    aggregate = file.path(scratch_root, "aggregate"),
    qc = file.path(scratch_root, "qc")
  )
  for (d in roots) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  cache_guard <- NULL
  old_refresh <- Sys.getenv("MIMIC_REBUILD_REFRESH_CACHE", unset = NA_character_)
  if (database == "mimic") {
    Sys.setenv(MIMIC_REBUILD_REFRESH_CACHE = "0")
    cache_guard <- seed_mimic_phase1_cache_links(roots$private)
  }
  on.exit({
    if (is.na(old_refresh)) {
      Sys.unsetenv("MIMIC_REBUILD_REFRESH_CACHE")
    } else {
      Sys.setenv(MIMIC_REBUILD_REFRESH_CACHE = old_refresh)
    }
  }, add = TRUE)

  replay_env <- new.env(parent = globalenv())
  replay_env$source <- make_config_injector(
    replay_env, roots, phase1_compatibility_lock()
  )
  expressions <- parse(file = source_path, keep.source = FALSE)
  stop_expression <- NA_integer_
  for (i in seq_along(expressions)) {
    eval(expressions[[i]], envir = replay_env)
    if (identical(assignment_name(expressions[[i]]), "stage5")) {
      stop_expression <- i
      break
    }
  }
  if (!is.finite(stop_expression) ||
      !exists("stage5", envir = replay_env, inherits = FALSE)) {
    stop("Replay did not reach the respiratory-eligible stage5 boundary.")
  }
  stage5 <- get("stage5", envir = replay_env, inherits = FALSE)
  if (!is.data.table(stage5) || !nrow(stage5)) {
    stop("Respiratory-eligible stage5 is empty or malformed.")
  }

  stages <- lapply(0:5, function(k) {
    nm <- paste0("stage", k)
    if (!exists(nm, envir = replay_env, inherits = FALSE)) return(NULL)
    get(nm, envir = replay_env, inherits = FALSE)
  })
  names(stages) <- paste0("stage", 0:5)
  stage_counts <- rbindlist(lapply(names(stages), function(nm) {
    x <- stages[[nm]]
    if (is.null(x)) return(NULL)
    data.table(stage = nm, event_n = nrow(x))
  }))

  if (!is.null(cache_guard)) assert_cache_hashes_unchanged(cache_guard)
  assert_v1_tree_unchanged(v1_before)

  list(
    stage5 = copy(stage5),
    stage_counts = stage_counts,
    replay_manifest = data.table(
      database = database,
      source_path = source_path,
      source_sha256 = sha256_file(source_path),
      parsed_expression_n = length(expressions),
      stopped_after_expression = stop_expression,
      stop_assignment = "stage5",
      execution_boundary = if (database == "mimic") {
        paste(
          "Stopped immediately after respiratory eligibility/NIV exclusion;",
          "the antibiotic-culture infection block and all formal save/QC code",
          "were not executed."
        )
      } else {
        paste(
          "Stopped immediately after respiratory eligibility/NIV exclusion",
          "and before stage6 infection filtering and all formal save/QC code.",
          "eICU infection source mapping occurs upstream in the v1 script."
        )
      },
      v1_output_tree_unchanged = TRUE,
      completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    )
  )
}

run_transformed_v1_exposure <- function(database = c("mimic", "eicu")) {
  database <- match.arg(database)
  spec_name <- paste0(database, "_exposure")
  source_path <- assert_pinned_source(spec_name)
  source_text <- readLines(source_path, warn = FALSE)
  transformed <- gsub("_v1", "_v2", source_text, fixed = TRUE)
  replacement_n <- sum(source_text != transformed)

  v1_before <- snapshot_v1_tree()
  replay_env <- new.env(parent = globalenv())
  roots <- list(
    rebuild = REBUILD_ROOT,
    private = PRIVATE_ROOT,
    aggregate = AGGREGATE_ROOT,
    qc = QC_ROOT
  )
  replay_env$source <- make_config_injector(
    replay_env, roots, exposure_compatibility_lock()
  )

  expressions <- parse(text = transformed, keep.source = FALSE)
  for (expr in expressions) eval(expr, envir = replay_env)
  assert_v1_tree_unchanged(v1_before)

  manifest_dir <- file.path(QC_ROOT, paste0(database, "_exposure"))
  dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)
  replay_utils_path <- normalizePath(
    file.path(
      PROJECT_ROOT, "code", "R", "rebuild_v2",
      "00_phase1_replay_utils.R"
    ),
    mustWork = TRUE
  )
  wrapper_path <- if (exists(
    "script_path", envir = replay_env, inherits = FALSE
  )) {
    get("script_path", envir = replay_env, inherits = FALSE)
  } else {
    NA_character_
  }
  manifest <- data.table(
    database = database,
    upstream_source = source_path,
    upstream_sha256 = sha256_file(source_path),
    replay_utils_sha256 = sha256_file(replay_utils_path),
    wrapper_sha256 = if (!is.na(wrapper_path) && file.exists(wrapper_path)) {
      sha256_file(wrapper_path)
    } else {
      NA_character_
    },
    transformation = "literal filename/version suffix replacement: _v1 -> _v2",
    transformed_line_n = replacement_n,
    parsed_expression_n = length(expressions),
    config_injection = "LOCKED_V2 compatibility projection; all roots point to analysis_rebuild_v2",
    v1_output_tree_unchanged = TRUE,
    completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
  fwrite(
    manifest,
    file.path(manifest_dir, "controlled_v1_exposure_replay_manifest_v2.csv")
  )
  replay_gate <- data.table(
    field = c(
      "upstream_v1_source_sha256", "replay_utils_sha256",
      "wrapper_sha256", "v1_output_tree_unchanged", "completed_at"
    ),
    value = c(
      manifest$upstream_sha256,
      manifest$replay_utils_sha256,
      manifest$wrapper_sha256,
      as.character(manifest$v1_output_tree_unchanged),
      manifest$completed_at
    )
  )
  fwrite(
    replay_gate,
    file.path(
      manifest_dir, "controlled_v1_exposure_replay_gate_v2.csv"
    )
  )
  invisible(manifest)
}
