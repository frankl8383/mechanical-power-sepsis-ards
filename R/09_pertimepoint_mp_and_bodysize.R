#!/usr/bin/env Rscript
# =============================================================================
# 09_pertimepoint_mp_and_bodysize.R
#
# Supplementary Analysis A — exposure-definition robustness
#   (a) Per-timepoint vs component-median mechanical power (MP) agreement
#   (b) Body-size (VT/PBW) normalization and sex-stratified MP association
#
# Purpose
#   The primary MP exposure is a component-median summary (median plateau, PEEP,
#   tidal volume and respiratory rate over day 1, combined once). This script
#   verifies that a genuinely per-timepoint MP — computed at every timestamp with
#   a recorded plateau pressure, then summarized per stay — agrees with the
#   component-median value, and that the severity-adjusted MP association is not
#   an artefact of body size.
#
# Locked MP equation (simplified VCV form; Chiumello 2020, PMID 32653011)
#   MP = 0.098 * RR * (VT/1000) * (Ppeak - 0.5 * dP),   dP = Pplat - PEEP
#   NOTE: the first pressure term is PEAK inspiratory pressure (Ppeak), NOT
#   plateau. Using plateau here mechanically deflates row-level MP.
#
# Inputs (not redistributed; obtain via PhysioNet under the applicable DUAs)
#   eICU-CRD 2.0 : respiratoryCharting.csv.gz
#   MIMIC-IV 3.1 : icu/chartevents.csv.gz (pre-filtered to ventilator itemids),
#                  icu/icustays.csv.gz
#   Derived cohort objects (this repo / analysis pipeline):
#     eicu_analysis_master.rds  (MP_baseline, dP_baseline, has_primary, died_hosp,
#                                tvpbw_baseline, anchor_age, gender, ...)
#     modeling_cohort.rds       (MP_baseline, tvpbw_baseline, died_28d, ...)
#     sofa_eicu.rds / sofa_mimic.rds (non-respiratory SOFA)
#
# Outputs (results_aggregate/tables/)
#   mp_aggregation_agreement.csv   Pearson/Spearman r, bias, Bland-Altman LoA
#   mp_rowlevel_increment.csv      nested-model increment using per-timepoint MP
#   mp_pbw_normalization.csv       MP OR with/without VT/PBW; male OR
#   mp_sex_stratified.csv          MP OR within sex strata
#
# Usage
#   Rscript 09_pertimepoint_mp_and_bodysize.R \
#           <EICU_DIR> <MIMIC_DIR> <DERIVED_DIR> <OUT_DIR>
# =============================================================================

suppressMessages({library(data.table); library(pROC)})

args <- commandArgs(trailingOnly = TRUE)
EICU    <- ifelse(length(args) >= 1, args[1], "/path/to/eicu-crd/2.0")
MIMIC   <- ifelse(length(args) >= 2, args[2], "/path/to/mimiciv/3.1")
DERIVED <- ifelse(length(args) >= 3, args[3], "derived")
OUT     <- ifelse(length(args) >= 4, args[4], "results_aggregate/tables")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

MP <- function(rr, tv, ppeak, dP) 0.098 * rr * (tv/1000) * (ppeak - 0.5 * dP)

## ---- (1) eICU per-timepoint MP ---------------------------------------------
em <- readRDS(file.path(DERIVED, "eicu_analysis_master.rds")); setDT(em)
target_ids <- em$stay_id

rc <- fread(file.path(EICU, "respiratoryCharting.csv.gz"),
            select = c("patientunitstayid","respchartoffset",
                       "respchartvaluelabel","respchartvalue"),
            showProgress = FALSE)
rc <- rc[patientunitstayid %in% target_ids]

labmap <- c("Plateau Pressure"="plat","PEEP"="peep","PEEP/CPAP"="peep",
            "Peak Insp. Pressure"="ppeak",
            "Tidal Volume Observed (VT)"="tv","Tidal Volume (set)"="tv_set",
            "Vent Rate"="rr_set","Total RR"="rr_tot","RR (patient)"="rr_pt")
rc <- rc[respchartvaluelabel %in% names(labmap)]
rc[, param := labmap[respchartvaluelabel]]
rc[, val := as.numeric(gsub("[^0-9.]","",respchartvalue))]
rc <- rc[is.finite(val) & val > 0]
rc24 <- rc[respchartoffset >= 0 & respchartoffset <= 1440]           # first 24 h

wide <- dcast(rc24, patientunitstayid + respchartoffset ~ param,
              value.var = "val", fun.aggregate = function(x) median(x, na.rm = TRUE))
wide[, rr_use := fifelse(is.finite(rr_tot), rr_tot,
                         fifelse(is.finite(rr_set), rr_set, rr_pt))]
wide[, tv_use := fifelse(is.finite(tv), tv, tv_set)]
setkey(wide, patientunitstayid, respchartoffset)

# Anchor each MP calculation on a timestamp with a measured plateau, then pull the
# nearest-in-time value of every other parameter.
anchor <- wide[is.finite(plat), .(patientunitstayid, respchartoffset, plat)]
setkey(anchor, patientunitstayid, respchartoffset)
get_near <- function(param){
  src <- wide[is.finite(get(param)), .(patientunitstayid, respchartoffset, v = get(param))]
  if(!nrow(src)) return(rep(NA_real_, nrow(anchor)))
  setkey(src, patientunitstayid, respchartoffset)
  src[anchor, on = .(patientunitstayid, respchartoffset), roll = "nearest"]$v
}
anchor[, ppeak := get_near("ppeak")]
anchor[, peep  := get_near("peep")]
anchor[, tv    := get_near("tv_use")]
anchor[, rr    := get_near("rr_use")]

a <- anchor[is.finite(plat) & is.finite(peep) & is.finite(ppeak) &
            is.finite(tv) & is.finite(rr) &
            tv >= 100 & tv <= 1500 & rr >= 5 & rr <= 60 &
            plat > peep & ppeak >= plat & ppeak <= 70]
a[, dP := plat - peep]
a[, MP_row := MP(rr, tv, ppeak, dP)]
a <- a[is.finite(MP_row) & MP_row > 0 & MP_row < 100]

rowmp <- a[, .(MP_rowlevel = median(MP_row), n_ts = .N), by = patientunitstayid]
setnames(rowmp, "patientunitstayid", "stay_id")
ce <- merge(rowmp, em[, .(stay_id, MP_component = MP_baseline, died_hosp, has_primary)],
            by = "stay_id")[is.finite(MP_component)]
ce[, diff := MP_rowlevel - MP_component]

## ---- (2) MIMIC per-timepoint MP --------------------------------------------
mc <- readRDS(file.path(DERIVED, "modeling_cohort.rds")); setDT(mc)
# chartevents pre-filtered to the eight ventilator itemids (see repo README);
# columns: stay_id, itemid, charttime, valuenum
ve <- fread(file.path(DERIVED, "mimic_vent_charts.csv.gz"), showProgress = FALSE)
icustays <- fread(file.path(MIMIC, "icu/icustays.csv.gz"),
                  select = c("stay_id","intime"), showProgress = FALSE)

imap <- c("224696"="plat","224695"="ppeak","220339"="peep","224700"="peep_tot",
          "224685"="tv","224684"="tv_set","220210"="rr","224690"="rr_tot")
ve <- ve[stay_id %in% mc$stay_id & itemid %in% as.integer(names(imap))]
ve[, param := imap[as.character(itemid)]]
ve[, valuenum := as.numeric(valuenum)]
ve <- ve[is.finite(valuenum) & valuenum > 0]
ve <- merge(ve, icustays, by = "stay_id")
ve[, charttime := as.POSIXct(charttime, tz = "UTC")]
ve[, intime    := as.POSIXct(intime,    tz = "UTC")]
ve[, rel_h := as.numeric(difftime(charttime, intime, units = "hours"))]
ve24 <- ve[rel_h >= 0 & rel_h <= 24]
ve24[, tstamp := as.numeric(charttime)]

wide_m <- dcast(ve24, stay_id + tstamp ~ param, value.var = "valuenum",
                fun.aggregate = function(x) median(x, na.rm = TRUE))
wide_m[, rr_use   := fifelse(is.finite(rr_tot), rr_tot, rr)]
wide_m[, tv_use   := fifelse(is.finite(tv), tv, tv_set)]
wide_m[, peep_use := fifelse(is.finite(peep), peep, peep_tot)]
setkey(wide_m, stay_id, tstamp)

anchor_m <- wide_m[is.finite(plat), .(stay_id, tstamp, plat)]
setkey(anchor_m, stay_id, tstamp)
get_near_m <- function(param){
  src <- wide_m[is.finite(get(param)), .(stay_id, tstamp, v = get(param))]
  if(!nrow(src)) return(rep(NA_real_, nrow(anchor_m)))
  setkey(src, stay_id, tstamp)
  j <- src[anchor_m, on = .(stay_id, tstamp), roll = "nearest"]
  d <- abs(j$tstamp - j$i.tstamp); v <- j$v; v[d > 3600] <- NA_real_   # <=1 h window
  v
}
anchor_m[, ppeak := get_near_m("ppeak")]
anchor_m[, peep  := get_near_m("peep_use")]
anchor_m[, tv    := get_near_m("tv_use")]
anchor_m[, rr    := get_near_m("rr_use")]

a_m <- anchor_m[is.finite(plat) & is.finite(peep) & is.finite(ppeak) &
                is.finite(tv) & is.finite(rr) &
                tv >= 100 & tv <= 1500 & rr >= 5 & rr <= 60 &
                plat > peep & ppeak >= plat & ppeak <= 70]
a_m[, MP_row := MP(rr, tv, ppeak, plat - peep)]
a_m <- a_m[is.finite(MP_row) & MP_row > 0 & MP_row < 100]
rowmp_m <- a_m[, .(MP_rowlevel = median(MP_row), n_ts = .N), by = stay_id]
cm <- merge(rowmp_m, mc[, .(stay_id, MP_component = MP_baseline, died_28d)],
            by = "stay_id")[is.finite(MP_component)]
cm[, diff := MP_rowlevel - MP_component]

## ---- (3) Agreement table (Bland-Altman) ------------------------------------
agree <- data.table(
  cohort = c("MIMIC (development)","eICU (external)"),
  N_stays = c(nrow(cm), nrow(ce)),
  Pearson_r  = round(c(cor(cm$MP_rowlevel, cm$MP_component),
                       cor(ce$MP_rowlevel, ce$MP_component)), 3),
  Spearman_r = round(c(cor(cm$MP_rowlevel, cm$MP_component, method="spearman"),
                       cor(ce$MP_rowlevel, ce$MP_component, method="spearman")), 3),
  Bias_row_minus_comp = round(c(mean(cm$diff), mean(ce$diff)), 2),
  LoA_lower = round(c(mean(cm$diff)-1.96*sd(cm$diff), mean(ce$diff)-1.96*sd(ce$diff)), 2),
  LoA_upper = round(c(mean(cm$diff)+1.96*sd(cm$diff), mean(ce$diff)+1.96*sd(ce$diff)), 2))
fwrite(agree, file.path(OUT, "mp_aggregation_agreement.csv"))
print(agree)

## ---- (4) Body-size (VT/PBW) normalization and sex stratification -----------
sofa_e <- readRDS(file.path(DERIVED, "sofa_eicu.rds"));  setDT(sofa_e)
sofa_m <- readRDS(file.path(DERIVED, "sofa_mimic.rds")); setDT(sofa_m)
sofa_col_m <- intersect(c("sofa_nonresp","sofa_nonresp_total"), names(sofa_m))[1]

dm <- mc[has_primary == TRUE & is.finite(MP_baseline) & is.finite(dP_baseline) &
         is.finite(pf_day1_min) & is.finite(anchor_age) & is.finite(tvpbw_baseline) &
         gender %in% c("M","F") & !is.na(died_28d)]
dm <- merge(dm, sofa_m[, .(stay_id, sofa_nonresp = get(sofa_col_m))], by = "stay_id")[is.finite(sofa_nonresp)]
dm[, MP_z := scale(MP_baseline)[,1]]; dm[, tvpbw_z := scale(tvpbw_baseline)[,1]]

de <- merge(em[has_primary == TRUE & is.finite(MP_baseline) & is.finite(dP_baseline) &
                is.finite(pf_day1_min) & is.finite(anchor_age) & is.finite(tvpbw_baseline) &
                gender %in% c("M","F") & !is.na(died_hosp)],
            sofa_e[, .(stay_id, sofa_nonresp)], by = "stay_id")[is.finite(sofa_nonresp)]
de[, MP_z := scale(MP_baseline)[,1]]; de[, tvpbw_z := scale(tvpbw_baseline)[,1]]

m1 <- glm(died_28d ~ anchor_age+gender+pf_day1_min+sofa_nonresp+dP_baseline+MP_z,          dm, family=binomial)
m2 <- glm(died_28d ~ anchor_age+gender+pf_day1_min+sofa_nonresp+dP_baseline+MP_z+tvpbw_z,  dm, family=binomial)
e1 <- glm(died_hosp ~ anchor_age+gender+pf_day1_min+sofa_nonresp+dP_baseline+MP_z,         de, family=binomial)
e2 <- glm(died_hosp ~ anchor_age+gender+pf_day1_min+sofa_nonresp+dP_baseline+MP_z+tvpbw_z, de, family=binomial)
pbw_tab <- data.table(
  cohort = c("MIMIC (28-day)","MIMIC (28-day)","eICU (in-hosp)","eICU (in-hosp)"),
  model  = c("+MP (no body-size)","+MP +VT/PBW","+MP (no body-size)","+MP +VT/PBW"),
  N = c(nrow(dm),nrow(dm),nrow(de),nrow(de)),
  MP_OR_perSD = round(c(exp(coef(m1)["MP_z"]),exp(coef(m2)["MP_z"]),
                        exp(coef(e1)["MP_z"]),exp(coef(e2)["MP_z"])),3),
  male_OR = round(c(exp(coef(m1)["genderM"]),exp(coef(m2)["genderM"]),
                    exp(coef(e1)["genderM"]),exp(coef(e2)["genderM"])),3),
  VTPBW_OR = round(c(NA,exp(coef(m2)["tvpbw_z"]),NA,exp(coef(e2)["tvpbw_z"])),3))
fwrite(pbw_tab, file.path(OUT, "mp_pbw_normalization.csv"))

# Sex-stratified MP association (severity-adjusted), per cohort and sex
sex_rows <- list()
for(sx in c("M","F")){
  ms <- glm(died_28d ~ anchor_age+pf_day1_min+sofa_nonresp+dP_baseline+MP_z, dm[gender==sx], family=binomial)
  es <- glm(died_hosp ~ anchor_age+pf_day1_min+sofa_nonresp+dP_baseline+MP_z, de[gender==sx], family=binomial)
  sex_rows[[length(sex_rows)+1]] <- data.table(cohort="MIMIC (28-day)", sex=sx, MP_OR_perSD=round(exp(coef(ms)["MP_z"]),3))
  sex_rows[[length(sex_rows)+1]] <- data.table(cohort="eICU (in-hosp)", sex=sx, MP_OR_perSD=round(exp(coef(es)["MP_z"]),3))
}
fwrite(rbindlist(sex_rows), file.path(OUT, "mp_sex_stratified.csv"))

## ---- (5) Incremental value using per-timepoint MP --------------------------
# Base = age+sex+P/F+SOFA+dP; add row-level MP, then component-median MP.
inc_rows <- list()
for(nm in c("eICU","MIMIC")){
  if(nm=="eICU"){
    d <- merge(ce, em[, .(stay_id, dP_baseline, pf_day1_min, anchor_age, gender, died_hosp)], by="stay_id")
    d <- merge(d, sofa_e[, .(stay_id, sofa_nonresp)], by="stay_id")
    y <- "died_hosp"
  } else {
    d <- merge(cm, mc[, .(stay_id, dP_baseline, pf_day1_min, anchor_age, gender, died_28d)], by="stay_id")
    d <- merge(d, sofa_m[, .(stay_id, sofa_nonresp=get(sofa_col_m))], by="stay_id")
    y <- "died_28d"
  }
  d <- d[is.finite(dP_baseline)&is.finite(pf_day1_min)&is.finite(sofa_nonresp)&
         is.finite(anchor_age)&gender%in%c("M","F")&!is.na(get(y))]
  d[, rowz := scale(MP_rowlevel)[,1]]; d[, compz := scale(MP_component)[,1]]
  f0 <- as.formula(paste(y,"~ anchor_age+gender+pf_day1_min+sofa_nonresp+dP_baseline"))
  b <- glm(f0, d, family=binomial)
  r <- update(b, .~.+rowz); cc <- update(b, .~.+compz)
  cb <- as.numeric(auc(d[[y]], predict(b,type="response"), quiet=TRUE))
  cr <- as.numeric(auc(d[[y]], predict(r,type="response"), quiet=TRUE))
  cco<- as.numeric(auc(d[[y]], predict(cc,type="response"), quiet=TRUE))
  inc_rows[[length(inc_rows)+1]] <- data.table(cohort=nm, model="Base (age+sex+P/F+SOFA+dP)", N=nrow(d), events=sum(d[[y]]), C=round(cb,4), MP_OR_perSD=NA, p=NA)
  inc_rows[[length(inc_rows)+1]] <- data.table(cohort=nm, model="+ per-timepoint MP", N=nrow(d), events=sum(d[[y]]), C=round(cr,4), MP_OR_perSD=round(exp(coef(r)["rowz"]),3), p=signif(summary(r)$coef["rowz",4],3))
  inc_rows[[length(inc_rows)+1]] <- data.table(cohort=nm, model="+ component-median MP", N=nrow(d), events=sum(d[[y]]), C=round(cco,4), MP_OR_perSD=round(exp(coef(cc)["compz"]),3), p=signif(summary(cc)$coef["compz",4],3))
}
fwrite(rbindlist(inc_rows), file.path(OUT, "mp_rowlevel_increment.csv"))
cat("Analysis A complete.\n")
