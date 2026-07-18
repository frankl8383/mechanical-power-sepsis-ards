#!/usr/bin/env Rscript

# Synthetic-only tests. No project dataset or outcome artifact is opened.

options(warn = 2)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "code/R/rebuild_v2/08b_weighted_sensitivity_utils_selftest.R",
    mustWork = TRUE
  )
}
source(file.path(dirname(script_path), "01_analysis_utils.R"))
source(file.path(dirname(script_path), "08b_weighted_sensitivity_utils.R"))

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop("Synthetic self-test failed: ", label)
}

set.seed(2026071609L)
n <- 1500L
frame <- data.frame(
  analysis_id = sprintf("weighted_%05d", seq_len(n)),
  x1 = rnorm(n),
  x2 = rbinom(n, 1, 0.4)
)
frame$y <- rbinom(
  n, 1,
  plogis(-0.5 + 0.7 * frame$x1 - 0.35 * frame$x2)
)
weights <- exp(0.35 * frame$x1)
frozen <- data.frame(
  row_id = frame$analysis_id,
  stabilized_weight_truncated = weights,
  permitted_for_outcome_weighting = TRUE
)
attached <- v2_attach_frozen_selection_weights(
  frame,
  frozen,
  id_column = "analysis_id"
)
assert_true(
  identical(attached$selection_weight, weights),
  "frozen weights attached by exact ID"
)
diagnostic_only <- frozen
diagnostic_only$permitted_for_outcome_weighting <- FALSE
diagnostic_blocked <- inherits(try(
  v2_attach_frozen_selection_weights(
    frame,
    diagnostic_only,
    id_column = "analysis_id"
  ),
  silent = TRUE
), "try-error")
assert_true(
  diagnostic_blocked,
  "diagnostic-only selection weights cannot reach an outcome model"
)
design <- as.matrix(frame[c("x1", "x2")])
fit <- v2_fit_weighted_logistic(
  design,
  frame$y,
  attached$selection_weight,
  "synthetic_weighted",
  frame$analysis_id
)
prediction <- predict(fit, design)
performance <- v2_weighted_performance(frame$y, prediction, weights)

assert_true(fit$converged, "weighted fit converged")
assert_true(
  identical(fit$row_ids, frame$analysis_id),
  "row identity preserved"
)
assert_true(
  all(is.finite(prediction)) && all(prediction > 0 & prediction < 1),
  "finite probabilities"
)
assert_true(
  is.finite(performance[["brier"]]) &&
    performance[["brier"]] > 0 &&
    performance[["brier"]] < 1 &&
    is.finite(performance[["c_statistic"]]),
  "weighted performance finite"
)
assert_true(
  abs(mean(weights / mean(weights)) - 1) < 1e-12,
  "analysis weights use mean-one normalization"
)

tie_y <- c(1L, 0L, 1L, 0L, 1L, 0L)
tie_p <- c(0.2, 0.2, 0.7, 0.4, 0.7, 0.9)
tie_w <- c(1.5, 0.5, 2, 1, 0.75, 3)
brute_numerator <- sum(vapply(
  which(tie_y == 1L),
  function(i) {
    negative <- which(tie_y == 0L)
    tie_w[[i]] * sum(
      tie_w[negative] *
        ((tie_p[[i]] > tie_p[negative]) +
          0.5 * (tie_p[[i]] == tie_p[negative]))
    )
  },
  numeric(1L)
))
brute_auc <- brute_numerator /
  (sum(tie_w[tie_y == 1L]) * sum(tie_w[tie_y == 0L]))
assert_true(
  abs(v2_weighted_auc(tie_y, tie_p, tie_w) - brute_auc) < 1e-12,
  "fast weighted AUC preserves exact tie handling"
)

cat("REBUILD_V2_WEIGHTED_SENSITIVITY_UTILS_SYNTHETIC_PASS\n")
