#!/usr/bin/env Rscript
# =============================================================================
# 10_selection_bias.R
#
# Supplementary Analysis B — selection bias of the complete-case restriction
#   (a) Standardized mean differences (SMD): complete vs incomplete stays
#   (b) Inverse-probability-of-selection weighting (IPW) of the MP association
#   (c) Missing-not-at-random (MNAR) tipping-point on the imputed exposure
#
# Rationale
#   Plateau pressure (required for MP and dP) is charted less often in eICU-CRD.
#   The complete-case set may therefore be a non-random, higher-severity subgroup.
#   This script quantifies that selection and tests whether it could overturn the
#   severity-adjusted MP association.
#
# Inputs (derived cohort objects; not redistributed)
#   eicu_analysis_master.rds  (MP_baseline, dP_baseline, pf_day1_min, has_primary,
#                              has_mp, died_hosp, anchor_age, gender, ...)
#   sofa_eicu.rds             (sofa_nonresp, apache_iva, apache_pred_hosp)
#   eicu_mi_imputed.rds       (mice mids, m = 20, full eICU cohort with outcome)
#
# Outputs (results_aggregate/tables/)
#   completecase_vs_incomplete_baseline.csv   SMD of baseline covariates
#   ipw_sensitivity.csv                       MP OR, unweighted vs IPW-weighted
#   mnar_tipping.csv                          pooled MP OR across delta*SD shifts
#
# Usage
#   Rscript 10_selection_bias.R <DERIVED_DIR> <OUT_DIR>
# =============================================================================

suppressMessages({library(data.table); library(mice); library(pROC)})

args <- commandArgs(trailingOnly = TRUE)
DERIVED <- ifelse(length(args) >= 1, args[1], "derived")
OUT     <- ifelse(length(args) >= 2, args[2], "results_aggregate/tables")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

em <- readRDS(file.path(DERIVED, "eicu_analysis_master.rds")); setDT(em)
sofa_e <- readRDS(file.path(DERIVED, "sofa_eicu.rds")); setDT(sofa_e)
em <- merge(em, sofa_e[, .(stay_id, sofa_nonresp, apache_iva, apache_pred_hosp)],
            by = "stay_id", all.x = TRUE)
em[, complete := has_primary == TRUE]
em[, male := as.integer(gender == "M")]

## ---- (a) SMD: complete vs incomplete ---------------------------------------
smd <- function(x, g){
  m1 <- mean(x[g], na.rm=TRUE); m0 <- mean(x[!g], na.rm=TRUE)
  s1 <- var(x[g], na.rm=TRUE);  s0 <- var(x[!g], na.rm=TRUE)
  (m1 - m0) / sqrt((s1 + s0)/2)
}
vars <- c("anchor_age","sofa_nonresp","apache_iva","apache_pred_hosp",
          "pf_day1_min","died_hosp")
g <- em$complete
base_tab <- rbindlist(lapply(vars, function(v){
  x <- em[[v]]
  data.table(variable = v,
             complete_mean   = round(mean(x[g],  na.rm=TRUE), 3),
             incomplete_mean = round(mean(x[!g], na.rm=TRUE), 3),
             SMD = round(smd(x, g), 3))
}))
fwrite(base_tab, file.path(OUT, "completecase_vs_incomplete_baseline.csv"))
print(base_tab)

## ---- (b) IPW of the MP association -----------------------------------------
sel_vars <- c("anchor_age","male","sofa_nonresp","died_hosp")
ds <- em[complete.cases(em[, ..sel_vars])]
sel_mod <- glm(complete ~ anchor_age + male + sofa_nonresp + died_hosp,
               ds, family = binomial)
ds[, ps := predict(sel_mod, type = "response")]
ds[, sipw := ifelse(complete, mean(complete)/ps, 0)]   # stabilized IPW

dc <- merge(em[complete == TRUE,
               .(stay_id, MP_baseline, dP_baseline, pf_day1_min, anchor_age,
                 gender, sofa_nonresp, died_hosp)],
            ds[complete == TRUE, .(stay_id, sipw)], by = "stay_id")
dc <- dc[is.finite(MP_baseline) & is.finite(dP_baseline) & is.finite(pf_day1_min) &
         is.finite(sofa_nonresp) & is.finite(anchor_age) & !is.na(died_hosp)]
dc[, MP_z := scale(MP_baseline)[,1]]

f <- died_hosp ~ anchor_age + gender + pf_day1_min + sofa_nonresp + dP_baseline + MP_z
m_unw <- glm(f, dc, family = binomial)
m_ipw <- glm(f, dc, family = binomial, weights = sipw)
ipw_tab <- data.table(
  analysis = c("Complete-case (unweighted)","IPW-weighted (selection-corrected)"),
  N = nrow(dc), events = sum(dc$died_hosp),
  MP_OR_perSD = round(c(exp(coef(m_unw)["MP_z"]), exp(coef(m_ipw)["MP_z"])), 3),
  CI_low  = round(c(exp(confint.default(m_unw)["MP_z",1]), exp(confint.default(m_ipw)["MP_z",1])), 3),
  CI_high = round(c(exp(confint.default(m_unw)["MP_z",2]), exp(confint.default(m_ipw)["MP_z",2])), 3),
  p = signif(c(summary(m_unw)$coef["MP_z",4], summary(m_ipw)$coef["MP_z",4]), 3))
fwrite(ipw_tab, file.path(OUT, "ipw_sensitivity.csv"))
print(ipw_tab)

## ---- (c) MNAR tipping-point ------------------------------------------------
# Shift the imputed MP values by delta * SD(observed MP) before re-fitting, to ask
# how far a non-random departure would have to go to overturn the association.
mi <- readRDS(file.path(DERIVED, "eicu_mi_imputed.rds"))
sd_mp <- sd(em[has_mp == TRUE, MP_baseline], na.rm = TRUE)
cat("observed MP SD =", round(sd_mp, 2), "\n")

deltas <- c(-1.0, -0.5, -0.25, 0, 0.25, 0.5, 1.0)
res <- rbindlist(lapply(deltas, function(dl){
  ests <- c(); pvals <- c()
  for(i in seq_len(mi$m)){
    ci <- complete(mi, i); setDT(ci)
    imp_mask <- mi$where[, "MP_baseline"]
    ci[, MP_adj := MP_baseline + ifelse(imp_mask, dl * sd_mp, 0)]
    ci[, MP_z := scale(MP_adj)[,1]]
    m <- glm(died_hosp ~ anchor_age + male + pf_day1_min + sofa_nonresp + dP_baseline + MP_z,
             ci, family = binomial)
    ests  <- c(ests,  coef(m)["MP_z"])
    pvals <- c(pvals, summary(m)$coef["MP_z",4])
  }
  data.table(delta_SD = dl, MP_OR_perSD = round(exp(mean(ests)), 3),
             median_p = round(median(pvals), 4))
}))
fwrite(res, file.path(OUT, "mnar_tipping.csv"))
print(res)
cat("Analysis B complete.\n")
