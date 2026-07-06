#!/usr/bin/env Rscript
# =============================================================================
# 11_decision_curve_endpoint_bridge.R
#
# Supplementary Analysis C — clinical utility and endpoint concordance
#   (a) Decision-curve / net-benefit analysis of adding MP to a severity model
#   (b) Endpoint bridge: the same frozen model applied to 28-day and in-hospital
#       mortality within MIMIC-IV
#
# Rationale
#   (a) A significant odds ratio is not the same as clinical usefulness. The
#       decision curve reports the net benefit of the MP-augmented model across a
#       clinically relevant range of decision thresholds. We report this honestly:
#       MP is an incremental signal, not a strong stand-alone predictor, and its
#       net-benefit gain is small.
#   (b) Development used 28-day mortality; external validation used in-hospital
#       mortality (eICU-CRD lacks post-discharge follow-up). To show the two
#       endpoints are close, the frozen development model is applied to BOTH
#       endpoints within MIMIC-IV, where both are available.
#
# Frozen primary model (models_step5.rds), linear predictor L:
#   L = -3.349750 + 0.051562*MP + 0.034508*dP + 0.022360*age
#       - 0.328796*(sex==male) - 0.003069*(P/F)
#   p = 1 / (1 + exp(-L))
#
# Inputs (derived cohort objects; not redistributed)
#   modeling_cohort.rds        (MP_baseline, dP_baseline, pf_day1_min, anchor_age,
#                               gender, died_28d, subject_id, hadm_id, has_primary)
#   sofa_mimic.rds / sofa_eicu.rds  (sofa_nonresp)
#   eicu_analysis_master.rds   (external cohort, died_hosp)
#   MIMIC-IV 3.1 : hosp/admissions.csv.gz (hospital_expire_flag -> died_hosp)
#
# Outputs (results_aggregate/tables/)
#   decision_curve.csv         net benefit vs threshold, both cohorts
#   endpoint_bridge.csv        C-statistic and MP increment for both endpoints
#
# Usage
#   Rscript 11_decision_curve_endpoint_bridge.R <MIMIC_DIR> <DERIVED_DIR> <OUT_DIR>
# =============================================================================

suppressMessages({library(data.table); library(pROC)})

args <- commandArgs(trailingOnly = TRUE)
MIMIC   <- ifelse(length(args) >= 1, args[1], "/path/to/mimiciv/3.1")
DERIVED <- ifelse(length(args) >= 2, args[2], "derived")
OUT     <- ifelse(length(args) >= 3, args[3], "results_aggregate/tables")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

## ---- (a) Decision-curve / net-benefit --------------------------------------
# Net benefit of a model at threshold pt:  (TP/N) - (FP/N) * (pt/(1-pt))
net_benefit <- function(y, p, thresholds){
  N <- length(y)
  sapply(thresholds, function(pt){
    tp <- sum(p >= pt & y == 1); fp <- sum(p >= pt & y == 0)
    (tp/N) - (fp/N) * (pt/(1-pt))
  })
}
# "Treat-all" reference net benefit
nb_all <- function(y, thresholds){
  prev <- mean(y); prev - (1-prev) * (thresholds/(1-thresholds))
}

mc <- readRDS(file.path(DERIVED, "modeling_cohort.rds")); setDT(mc)
sofa_m <- readRDS(file.path(DERIVED, "sofa_mimic.rds")); setDT(sofa_m)
dm <- merge(mc[has_primary == TRUE], sofa_m[, .(stay_id, sofa_nonresp)], by = "stay_id")
dm <- dm[is.finite(MP_baseline) & is.finite(dP_baseline) & is.finite(pf_day1_min) &
         is.finite(sofa_nonresp) & is.finite(anchor_age) & !is.na(died_28d) &
         gender %in% c("M","F")]
dm[, MP_z := scale(MP_baseline)[,1]]

thr <- seq(0.05, 0.60, by = 0.01)
mb <- glm(died_28d ~ anchor_age + gender + pf_day1_min + sofa_nonresp + dP_baseline,
          dm, family = binomial)
mm <- update(mb, . ~ . + MP_z)
nb_base_m <- net_benefit(dm$died_28d, predict(mb, type="response"), thr)
nb_mp_m   <- net_benefit(dm$died_28d, predict(mm, type="response"), thr)
nb_all_m  <- nb_all(dm$died_28d, thr)

em <- readRDS(file.path(DERIVED, "eicu_analysis_master.rds")); setDT(em)
sofa_e <- readRDS(file.path(DERIVED, "sofa_eicu.rds")); setDT(sofa_e)
em[, complete := has_primary == TRUE]
de <- merge(em[complete == TRUE], sofa_e[, .(stay_id, sofa_nonresp)],
            by = "stay_id", suffixes = c("",".e"))
if(!"sofa_nonresp" %in% names(de) && "sofa_nonresp.e" %in% names(de))
  de[, sofa_nonresp := sofa_nonresp.e]
de <- de[is.finite(MP_baseline) & is.finite(dP_baseline) & is.finite(pf_day1_min) &
         is.finite(sofa_nonresp) & is.finite(anchor_age) & !is.na(died_hosp) &
         gender %in% c("M","F")]
de[, MP_z := scale(MP_baseline)[,1]]
eb <- glm(died_hosp ~ anchor_age + gender + pf_day1_min + sofa_nonresp + dP_baseline,
          de, family = binomial)
ee <- update(eb, . ~ . + MP_z)
nb_base_e <- net_benefit(de$died_hosp, predict(eb, type="response"), thr)
nb_mp_e   <- net_benefit(de$died_hosp, predict(ee, type="response"), thr)
nb_all_e  <- nb_all(de$died_hosp, thr)

dc <- data.table(threshold = thr,
  MIMIC_base = round(nb_base_m,5), MIMIC_MP = round(nb_mp_m,5), MIMIC_all = round(nb_all_m,5),
  eICU_base  = round(nb_base_e,5), eICU_MP  = round(nb_mp_e,5), eICU_all  = round(nb_all_e,5))
fwrite(dc, file.path(OUT, "decision_curve.csv"))
cat("Decision curve written. Median incremental NB (MIMIC, 15-35% range):",
    round(median((nb_mp_m - nb_base_m)[thr >= 0.15 & thr <= 0.35]), 5), "\n")

## ---- (b) Endpoint bridge ---------------------------------------------------
# died_hosp from admissions.hospital_expire_flag
adm <- fread(file.path(MIMIC, "hosp/admissions.csv.gz"),
             select = c("hadm_id","hospital_expire_flag"), showProgress = FALSE)
be <- merge(mc[has_primary == TRUE], adm, by = "hadm_id", all.x = TRUE)
be <- be[is.finite(MP_baseline) & is.finite(dP_baseline) & is.finite(pf_day1_min) &
         is.finite(anchor_age) & gender %in% c("M","F") &
         !is.na(died_28d) & !is.na(hospital_expire_flag)]
be[, died_hosp := as.integer(hospital_expire_flag)]

# Frozen linear predictor (identical coefficients for both endpoints)
be[, L := -3.349750 + 0.051562*MP_baseline + 0.034508*dP_baseline +
          0.022360*anchor_age - 0.328796*(gender=="M") - 0.003069*pf_day1_min]

roc28 <- roc(be$died_28d,  be$L, quiet = TRUE)
rocH  <- roc(be$died_hosp, be$L, quiet = TRUE)

# MP increment over base for each endpoint (per-SD)
be[, MP_z := scale(MP_baseline)[,1]]
b28 <- glm(died_28d  ~ anchor_age+gender+pf_day1_min+dP_baseline, be, family=binomial)
m28 <- update(b28, . ~ . + MP_z)
bH  <- glm(died_hosp ~ anchor_age+gender+pf_day1_min+dP_baseline, be, family=binomial)
mH  <- update(bH, . ~ . + MP_z)

bridge <- data.table(
  endpoint = c("28-day mortality","In-hospital mortality"),
  N = nrow(be),
  events = c(sum(be$died_28d), sum(be$died_hosp)),
  C_frozen_model = round(c(as.numeric(auc(roc28)), as.numeric(auc(rocH))), 4),
  C_low  = round(c(ci.auc(roc28)[1], ci.auc(rocH)[1]), 4),
  C_high = round(c(ci.auc(roc28)[3], ci.auc(rocH)[3]), 4),
  MP_incr_OR_perSD = round(c(exp(coef(m28)["MP_z"]), exp(coef(mH)["MP_z"])), 3),
  MP_incr_p = signif(c(summary(m28)$coef["MP_z",4], summary(mH)$coef["MP_z",4]), 3))
fwrite(bridge, file.path(OUT, "endpoint_bridge.csv"))
print(bridge)
cat("Analysis C complete.\n")
