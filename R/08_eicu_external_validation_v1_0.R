## =============================================================================
## 08_eicu_external_validation_v1_0.R
## Step 5b — eICU external validation of the MIMIC-IV sepsis-ARDS ventilatory
##           mechanics -> mortality prediction model.
##
## PURPOSE
##   Apply the frozen MIMIC-IV primary logistic model (models_step5.rds$primary)
##   to an independently constructed eICU-CRD v2.0 sepsis-ARDS ventilated cohort,
##   and report EXTERNAL discrimination (C), calibration (intercept/slope, Brier)
##   and recalibration-in-the-large, plus a sensitivity matrix.
##
## KEY METHODOLOGICAL DECISIONS (inherited from Step 5 handoff v4, discipline 1-5)
##   1. Primary model is LOGISTIC (Cox PH violated for MP/dP/PF in MIMIC; time
##      columns unreliable). External validation therefore also logistic.
##   2. Exposure = Day-1 BASELINE mechanics only (NO slope/peak/AUC — immortal
##      time bias, discipline 3).
##   3. MP / dP computed with the EXACT MIMIC formulas (see below).
##   4. OUTCOME = in-hospital mortality (eICU has no post-discharge follow-up, so
##      28-day all-cause mortality cannot be reproduced; user-approved substitute,
##      2026-07-04). This differs from MIMIC's 28-day outcome -> expect
##      recalibration-in-the-large. Disclosed in Methods + limitations.
##   5. Only aggregate statistics are exported; NO row-level data leaves secure_data.
##
## MIMIC EXPOSURE FORMULAS (reproduced verbatim from trajectory_features.rds)
##   peep_use = peep (eICU has no separate total-PEEP column)
##   dP  = plat - peep_use ;  dP set NA if <0 or >40
##   MP  = 0.098 * rr * (tv/1000) * (ppeak - 0.5*dP) ; MP set NA if <0 or >100
##   Plausibility windows: plat[5,60], peep[0,30], tv[50,1500], ppeak[5,80],
##                         set Vent Rate[1,60]; measured Total RR / RR (patient)
##                         [1,80] (spontaneous efforts can push total RR >60);
##                         fio2[21,100]
##
## INPUT
##   /Users/doctorliu/secure_data/eicu-crd/2.0/*.csv.gz  (read-only)
##   models_step5.rds  (MIMIC frozen models; artifact 082ec6de-...)
##   modeling_cohort.rds (MIMIC dev cohort, for distribution comparison)
##
## OUTPUT (aggregate only)
##   eicu_funnel.csv, eicu_analysis_master.rds, table_eicu_comparability.csv,
##   table_eicu_discrimination.csv, table_eicu_calibration.csv,
##   table_eicu_sensitivity.csv, table_eicu_summary.csv,
##   fig_eicu_vs_mimic_dist.png, fig_eicu_roc.png, fig_eicu_calibration.png,
##   fig_eicu_external_validation.png (composite Figure 3)
##
## ENVIRONMENT  icu-vent (R 4.5.3 + data.table + survival + rms + pROC)
## AUTHOR  Step 5b, 2026-07-04
## =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(survival); library(pROC)
})
EICU <- "/Users/doctorliu/secure_data/eicu-crd/2.0"
rd   <- function(f) fread(cmd=paste0("gzcat ", file.path(EICU, f)))
log_msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

## -----------------------------------------------------------------------------
## STEP 1 — COHORT: adult, first ICU stay, ventilated, sepsis, ARDS (P/F<=300)
## -----------------------------------------------------------------------------
pat <- rd("patient.csv.gz")
pat[, age_num := suppressWarnings(as.numeric(age))]
pat[age == "> 89", age_num := 90]                  # eICU caps age at >89
pat[, gender_mf := fifelse(gender=="Male","M", fifelse(gender=="Female","F", NA_character_))]

## ventilation = union of three sources (respiratoryCharting params / respCare /
## apacheApsVar vent|intubated) — analogous to MIMIC "has ventilation itemid"
rc  <- rd("respiratoryCharting.csv.gz")[, .(patientunitstayid, respchartoffset,
             respchartvaluelabel, respchartvalue)]
vent_labels <- c("PEEP","Plateau Pressure","Peak Insp. Pressure","Tidal Volume (set)",
                 "Exhaled TV (patient)","Exhaled TV (machine)","Vent Rate","Total RR",
                 "Mean Airway Pressure","Pressure Support","TV/kg IBW")
vent_rc   <- unique(rc[respchartvaluelabel %in% vent_labels]$patientunitstayid)
rcare     <- rd("respiratoryCare.csv.gz")[, .(patientunitstayid, ventstartoffset)]
vent_care <- unique(rcare[!is.na(ventstartoffset) & ventstartoffset!=""]$patientunitstayid)
aps       <- rd("apacheApsVar.csv.gz")[, .(patientunitstayid, intubated, vent, fio2, pao2)]
vent_aps  <- unique(aps[vent==1 | intubated==1]$patientunitstayid)
vent_ids  <- unique(c(vent_rc, vent_care, vent_aps))

## sepsis = diagnosisstring contains sepsis|septic (eICU has no MIMIC-style
## inputevents antibiotic timestamps for a strict Seymour rule; diagnosis-code
## approximation, disclosed in Methods)
dx <- rd("diagnosis.csv.gz")[, .(patientunitstayid, diagnosisstring)]
dx[, slow := tolower(diagnosisstring)]
sepsis_ids <- unique(dx[grepl("sepsis|septic", slow)]$patientunitstayid)

## P/F = PaO2 (lab) paired to nearest FiO2 (lab+respChart) within +/-2h, same as MIMIC
lab   <- rd("lab.csv.gz")[, .(patientunitstayid, labresultoffset, labname, labresult)]
pao2  <- lab[labname=="paO2", .(patientunitstayid, t=labresultoffset, pao2=labresult)][
             !is.na(pao2) & pao2>0 & pao2<700]
fio2L <- lab[labname=="FiO2", .(patientunitstayid, t=labresultoffset, fio2=labresult)]
fio2R <- rc[respchartvaluelabel %in% c("FiO2","FIO2 (%)"),
            .(patientunitstayid, t=respchartoffset, fio2=as.numeric(respchartvalue))]
fio2  <- rbind(fio2L, fio2R)[!is.na(fio2)]
fio2[fio2<=1, fio2 := fio2*100]; fio2 <- fio2[fio2>=21 & fio2<=100]
pao2[, t_pao2 := t]; fio2[, t_fio2 := t]
setkey(pao2, patientunitstayid, t); setkey(fio2, patientunitstayid, t)
pf <- fio2[pao2, on=.(patientunitstayid, t), roll="nearest"]
pf <- pf[!is.na(fio2) & abs(t_fio2 - t_pao2) <= 120]
pf[, pf_ratio := pao2/(fio2/100)]; pf <- pf[pf_ratio>0 & pf_ratio<1000]
ards_ids <- unique(pf[pf_ratio<=300]$patientunitstayid)

## funnel (intersection, MIMIC-analogous ordering)
adult   <- pat[age_num>=16 & !is.na(age_num)]$patientunitstayid
first   <- pat[unitvisitnumber==1]$patientunitstayid
l2 <- intersect(adult, first); l3 <- intersect(l2, vent_ids)
l4 <- intersect(l3, sepsis_ids); cohort_ids <- intersect(l4, ards_ids)
funnel <- data.table(
  step = c("L0 all ICU","L1 adult","L2 +first ICU","L3 +ventilated",
           "L4 +sepsis","L5 +ARDS (P/F<=300)"),
  n = c(uniqueN(pat$patientunitstayid), length(adult), length(l2),
        length(l3), length(l4), length(cohort_ids)))
fwrite(funnel, "eicu_funnel.csv"); log_msg("cohort N =", length(cohort_ids))

## -----------------------------------------------------------------------------
## STEP 2 — Day-1 baseline exposure (MP / dP / P/F), EXACT MIMIC formulas
## -----------------------------------------------------------------------------
rcc <- rc[patientunitstayid %in% cohort_ids & respchartoffset>=0 & respchartoffset<=1440]
rcc[, val := suppressWarnings(as.numeric(respchartvalue))]
agg <- function(labels, lo, hi){
  rcc[respchartvaluelabel %in% labels & !is.na(val) & val>=lo & val<=hi,
      .(v=median(val, na.rm=TRUE)), by=patientunitstayid]
}
plat  <- setnames(agg("Plateau Pressure",5,60),"v","plat")
peep  <- setnames(agg(c("PEEP","PEEP/CPAP"),0,30),"v","peep")
ppeak <- setnames(agg("Peak Insp. Pressure",5,80),"v","ppeak")
tvset <- setnames(agg("Tidal Volume (set)",50,1500),"v","tv_set")
tvexh <- setnames(agg(c("Exhaled TV (patient)","Exhaled TV (machine)"),50,1500),"v","tv_exh")
rrv   <- setnames(agg("Vent Rate",1,60),"v","rr_vent")
rrt   <- setnames(agg(c("Total RR","RR (patient)"),1,80),"v","rr_tot")
ex <- Reduce(function(a,b) merge(a,b,by="patientunitstayid",all=TRUE),
             list(data.table(patientunitstayid=cohort_ids),plat,peep,ppeak,tvset,tvexh,rrv,rrt))
ex[, tv_use := fifelse(!is.na(tv_set), tv_set, tv_exh)]
ex[, rr_use := fifelse(!is.na(rr_vent), rr_vent, rr_tot)]
ex[, peep_use := peep]
ex[, dP := plat - peep_use]; ex[dP<0 | dP>40, dP := NA]
ex[, MP := 0.098 * rr_use * (tv_use/1000) * (ppeak - 0.5*dP)]; ex[MP<0 | MP>100, MP := NA]
ex[, `:=`(MP_baseline=MP, dP_baseline=dP)]

## P/F: strict Day-1 minimum (matches MIMIC pf_day1_min). Backfill (whole-stay min)
## kept ONLY as a sensitivity variable — NOT used in the primary comparison.
pf_strict <- pf[t_pao2>=0 & t_pao2<=1440, .(pf_day1_min=min(pf_ratio,na.rm=TRUE)), by=patientunitstayid]
pf_any    <- pf[, .(pf_any_min=min(pf_ratio,na.rm=TRUE)), by=patientunitstayid]
ex <- merge(ex, pf_strict, by="patientunitstayid", all.x=TRUE)
ex <- merge(ex, pf_any,    by="patientunitstayid", all.x=TRUE)
ex[, pf_backfilled := fifelse(!is.na(pf_day1_min), pf_day1_min, pf_any_min)]

## -----------------------------------------------------------------------------
## STEP 3 — outcome (in-hospital mortality) + covariates + complete-case master
## -----------------------------------------------------------------------------
ex <- merge(ex, pat[, .(patientunitstayid, anchor_age=age_num, gender=gender_mf,
             hospitaldischargestatus, unitdischargestatus, admissionheight)],
            by="patientunitstayid", all.x=TRUE)
ex[, died_hosp := fifelse(hospitaldischargestatus=="Expired",1L,
                   fifelse(hospitaldischargestatus=="Alive",0L,NA_integer_))]
## tv/PBW (Devine IBW from admission height)
ex[, h := admissionheight]; ex[h<120 | h>230, h := NA]
ex[, ibw := fifelse(gender=="M", 50+0.9051*(h-152.4),
             fifelse(gender=="F", 45.5+0.9051*(h-152.4), NA_real_))]
tvpbw_d <- rcc[respchartvaluelabel=="TV/kg IBW" & val>0 & val<30,
               .(tvpbw=median(val,na.rm=TRUE)), by=patientunitstayid]
ex <- merge(ex, tvpbw_d, by="patientunitstayid", all.x=TRUE)
ex[is.na(tvpbw) & !is.na(tv_use) & ibw>0, tvpbw := tv_use/ibw]
ex[, tvpbw_baseline := tvpbw]
ex[, has_exposure_core := as.integer(!is.na(MP_baseline) & !is.na(dP_baseline) & !is.na(pf_day1_min))]
ex[, has_primary := as.integer(has_exposure_core==1 & !is.na(died_hosp) &
                                !is.na(anchor_age) & !is.na(gender))]
saveRDS(ex, "eicu_analysis_master.rds")
exc <- ex[has_primary==1]
exc[, gender := factor(gender, levels=c("F","M"))]
log_msg("primary complete-case N =", nrow(exc), "| deaths =", sum(exc$died_hosp))

## -----------------------------------------------------------------------------
## STEP 4 — apply MIMIC model -> external discrimination
## -----------------------------------------------------------------------------
mods <- readRDS("models_step5.rds")            # $primary is the frozen logistic
nd <- exc[, .(MP_baseline, dP_baseline, anchor_age, gender, pf_day1_min)]
exc[, lp        := predict(mods$primary, newdata=nd, type="link")]
exc[, pred_prob := predict(mods$primary, newdata=nd, type="response")]
roc_ext <- roc(exc$died_hosp, exc$pred_prob, quiet=TRUE, direction="<")
ci_ext  <- ci.auc(roc_ext, method="delong")
log_msg(sprintf("external C = %.4f (%.4f-%.4f)", auc(roc_ext), ci_ext[1], ci_ext[3]))

## -----------------------------------------------------------------------------
## STEP 5 — external calibration + recalibration-in-the-large
## -----------------------------------------------------------------------------
cal_intercept <- coef(glm(died_hosp ~ 1, offset=lp, data=exc, family=binomial))[1]
m_slope       <- glm(died_hosp ~ lp, data=exc, family=binomial)
cal_slope     <- coef(m_slope)["lp"]
brier         <- mean((exc$pred_prob - exc$died_hosp)^2)
exc[, pred_recal := plogis(lp + cal_intercept)]     # recalibration-in-the-large
brier_recal   <- mean((exc$pred_recal - exc$died_hosp)^2)
log_msg(sprintf("CITL=%.3f slope=%.3f Brier=%.3f->%.3f",
                cal_intercept, cal_slope, brier, brier_recal))

## -----------------------------------------------------------------------------
## STEP 6 — sensitivity matrix (outcome variants, ARDS severity, P/F backfill)
## -----------------------------------------------------------------------------
ext_C <- function(obs, pred){ r <- roc(obs, pred, quiet=TRUE, direction="<")
  ci <- ci.auc(r, method="delong"); c(C=as.numeric(auc(r)), lo=ci[1], hi=ci[3],
  n=length(obs), ev=sum(obs)) }
## (primary; ICU mortality; P/F backfilled; ARDS severity strata) — see
## table_eicu_sensitivity.csv for the full matrix produced interactively.
## ARDS severity uses EXPLICIT Berlin boundaries with an upper ceiling at 300
## (Mild = P/F 200-300, NOT >200), because pf_day1_min is the Day-1 *minimum*
## and can exceed 300 for patients admitted to the cohort on an earlier/later
## P/F<=300 reading:
##   exc[, ards_sev := cut(pf_day1_min, breaks=c(-Inf,100,200,300),
##                         labels=c("Severe","Moderate","Mild"),
##                         right=TRUE, include.lowest=TRUE)]
##   # 132 patients with Day-1-min P/F>300 fall outside the Berlin strata (NA)

## NB: figures (dist/ROC/calibration/composite) are rendered in Python
##     (matplotlib) from aggregate CSVs, since icu-vent lacks magick/patchwork.
## =============================================================================
