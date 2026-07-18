#!/usr/bin/env Rscript

# Synthetic-only tests. No project dataset or outcome artifact is opened.

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/08_selection_utils_selftest.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "01_analysis_utils.R"))
source(file.path(dirname(script_path), "08_selection_utils.R"))

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop("Synthetic self-test failed: ", label)
  invisible(TRUE)
}

set.seed(2026071608L)
n <- 5000L
synthetic <- data.frame(
  row_id = sprintf("selection_%05d", seq_len(n)),
  hospital = sprintf("hospital_%02d", sample(1:40, n, replace = TRUE)),
  age = runif(n, 18, 95),
  sex_female = rbinom(n, 1, 0.46),
  pf_ratio = runif(n, 45, 300),
  index_peep = sample(
    c(5, 8, 10, 12, 15),
    n,
    replace = TRUE,
    prob = c(0.62, 0.08, 0.20, 0.07, 0.03)
  ),
  index_hours_from_icu = runif(n, 0, 72),
  map = rnorm(n, 72, 14),
  platelet = exp(rnorm(n, log(190), 0.45)),
  creatinine = exp(rnorm(n, log(1.1), 0.65)),
  vasopressor = rbinom(n, 1, 0.31),
  sex_unknown = 0,
  constant_binary = 0
)
synthetic$map[sample.int(n, 450)] <- NA_real_
synthetic$platelet[sample.int(n, 600)] <- NA_real_
synthetic$creatinine[sample.int(n, 520)] <- NA_real_
linear_predictor <- with(
  synthetic,
  -0.5 + 0.015 * (age - 55) + 0.55 * sex_female +
    0.004 * (pf_ratio - 170) + 0.08 * (index_peep - 8) -
    0.25 * vasopressor + 0.55 * is.na(map) + 0.35 * is.na(platelet)
)
synthetic$included <- rbinom(n, 1, plogis(linear_predictor))
# Reproduce the real eICU edge case: an unknown-sex patient remains in the
# selection denominator but is deterministically absent from the complete
# primary common set. The inclusion model must remain auditable and finite.
unknown_row <- which(synthetic$hospital != "hospital_01")[[1L]]
synthetic$sex_female[unknown_row] <- 0
synthetic$sex_unknown[unknown_row] <- 1
synthetic$included[unknown_row] <- 0L
assert_true(
  length(unique(synthetic$included)) == 2L,
  "binary inclusion generated"
)

# Make one hospital structurally unsupported to test explicit positivity audit.
unsupported <- synthetic$hospital == "hospital_01"
synthetic$included[unsupported] <- 0L
support <- v2_selection_common_support(
  synthetic,
  inclusion = "included",
  cluster = "hospital"
)
assert_true(
  "hospital_01" %in% support$unsupported_clusters &&
    !any(support$keep[unsupported]),
  "zero-inclusion hospital excluded from weighted target"
)

analysis <- synthetic[support$keep, , drop = FALSE]
bundle <- v2_selection_derive_bundle(
  analysis,
  always_observed_continuous = c(
    "age", "pf_ratio", "index_peep", "index_hours_from_icu"
  ),
  possibly_missing_continuous = c("map", "platelet", "creatinine"),
  binary_variables = c(
    "sex_female", "sex_unknown", "vasopressor", "constant_binary"
  )
)
weights <- v2_fit_selection_weights(
  analysis,
  inclusion = "included",
  row_id = "row_id",
  bundle = bundle,
  truncation_quantiles = c(0.01, 0.99),
  model_id = "synthetic_joint_inclusion"
)

assert_true(
  bundle$transformations$index_peep$type == "robust_scaled_linear" &&
    bundle$transformation_audit$fallback_reason[
      bundle$transformation_audit$variable == "index_peep"
    ] == "non_unique_prespecified_quantile_knots",
  "outcome-blind linear fallback used for non-unique spline knots"
)
assert_true(
  nrow(weights$included_weights) == sum(analysis$included) &&
    all(weights$included_weights$stabilized_weight_truncated > 0) &&
    weights$summary$effective_sample_size_truncated > 0,
  "positive truncated weights and effective sample size"
)
assert_true(
  any(
    weights$design_audit$design_column == "sel_constant_binary" &
      !weights$design_audit$retained &
      weights$design_audit$reason == "constant_or_intercept_collinear"
  ),
  "constant prespecified term omitted by outcome-blind estimability gate"
)
assert_true(
  all(c(
    "map_missing", "platelet_missing", "creatinine_missing"
  ) %in% weights$balance$variable),
  "missingness indicators included in balance audit"
)
assert_true(
  weights$summary$maximum_absolute_smd_weighted <
    weights$summary$maximum_absolute_smd_unweighted,
  "weighting improves maximum measured imbalance"
)
assert_true(
  nrow(weights$probability_distribution) == 2L &&
    nrow(weights$weight_distribution) == 2L,
  "probability and weight distributions retained"
)
assert_true(
  is.finite(weights$coefficients[["sel_sex_unknown"]]) &&
    weights$all_probabilities$included[
      weights$all_probabilities$row_id ==
        synthetic$row_id[unknown_row]
    ] == 0L &&
    weights$all_probabilities$probability_was_clipped_low[
      weights$all_probabilities$row_id ==
        synthetic$row_id[unknown_row]
    ],
  "single unknown-sex structural exclusion remains finite and audited"
)
assert_true(
  weights$balance$structurally_nonreweightable[
    weights$balance$variable == "sex_unknown"
  ] &&
    weights$summary$nonreweightable_balance_variable_n >= 1L &&
    is.finite(
      weights$summary$maximum_absolute_smd_weighted_reweightable
    ),
  "structural nonpositivity is separated from reweightable balance"
)

cat("REBUILD_V2_SELECTION_UTILS_SYNTHETIC_PASS\n")
