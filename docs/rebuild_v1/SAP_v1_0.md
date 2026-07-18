# ARDS mechanical-power rebuild: Statistical Analysis Plan v1.0

**Version:** 1.0.1  
**Freeze date:** 2026-07-15 (Asia/Shanghai)  
**Status:** retrospective analysis-plan amendment before cohort reconstruction  
**Machine-readable lock:** code/R/rebuild_v1/00_config.R, LOCKED version 1.0.1  
**Companion records:** terminology_ledger.md, data_dictionary_v1.md, analysis_decision_log.md

## 1. Provenance, scope, and governance

This SAP was written after the investigators had seen results from the legacy analysis and after methodological weaknesses in the legacy cohort were identified. It is therefore **not** a prospective registration. It is a retrospective amendment intended to freeze the reconstruction and validation strategy before any association, effect estimate, model performance, calibration, subgroup, or outcome-stratified result from the rebuilt cohorts is inspected.

Legacy results may inform why a design defect must be repaired, but they may not be used to choose among otherwise reasonable definitions in the rebuilt analysis. No primary hypothesis, endpoint, model form, cut point, covariate, missing-data rule, or reporting tier will be changed after rebuilt outcome results are viewed in order to obtain a more favorable result.

The governance order is:

1. The machine-readable values in code/R/rebuild_v1/00_config.R govern parameters represented there.
2. This SAP governs scientific definitions not represented in the configuration file.
3. data_dictionary_v1.md governs source-to-variable mappings.
4. Any discrepancy, correction, or unavoidable deviation requires a dated entry in analysis_decision_log.md and a version increment before the affected outcome analysis is run.
5. If a definition remains marked 待核验, only metadata, schema, label-frequency, missingness, timing, and physiologic QC may be used to resolve it. Mortality, effect estimates, and model performance must remain hidden. If outcome-unblinded resolution is unavoidable, the analysis is exploratory.

For this governance boundary, header-only schema inspection, file existence/size/mtime, gzip stream validation, and an opaque whole-file checksum are outcome-blind reproducibility checks. Formal rebuilt-outcome unblinding begins with reading any row-level outcome value or deriving any new event count, outcome-stratified table, association, coefficient, discrimination, or calibration result. D031 remains the disclosed exception for one stale aggregate eICU death count; D059 records the metadata/integrity distinction.

The legacy files remain read-only provenance. Rebuild scripts and outputs must use separate versioned directories.

## 2. Study objectives and estimands

### 2.1 Primary objective

To evaluate, in mechanically ventilated adults with suspected infection and an oxygenation-defined acute hypoxemia phenotype, whether early surrogate mechanical power (sMP) provides prognostic information for in-hospital mortality beyond a harmonized pre-prediction severity model, and whether a locked sMP model developed in MIMIC-IV transports to eICU-CRD.

### 2.2 Primary estimands

1. **Conditional association:** the adjusted odds ratio for in-hospital mortality per 5 J/min higher early sMP in the primary complete-tuple population.
2. **Incremental prediction:** the difference in locked external performance between S3 (severity plus sMP) and S2 (severity plus driving pressure plus respiratory rate).
3. **Transport performance:** discrimination, calibration, and overall performance of each MIMIC-IV model applied to eICU-CRD without coefficient, transformation, knot, standardization, or threshold re-estimation.

These are prognostic, not causal, estimands. The coefficient for sMP is not the effect of an intervention that lowers sMP.

### 2.3 Prespecified hypotheses

- H1: higher early sMP is associated with higher in-hospital mortality after adjustment for measured pre-prediction severity.
- H2: S3 may summarize ventilatory burden as well as or better than S2, but superiority is not assumed.
- H3: any apparent development-set increment must be judged by locked external calibration and overall performance; a development-only improvement is considered non-transport.

The primary interpretation is comparative and may be negative. Statistical significance of the sMP coefficient is not sufficient evidence of incremental predictive value.

## 3. Data sources and roles

- **MIMIC-IV v3.1:** development cohort, model fitting, internal bootstrap validation, transformation and knot definition, and MIMIC-specific secondary outcomes.
- **eICU-CRD v2.0:** one-shot external validation cohort for the common in-hospital mortality endpoint and locked common predictors.

MIMIC-IV and eICU-CRD use different source-specific infection ascertainment. This difference is reported as a transportability limitation. If pre-outcome phenotype QC shows that the two operational definitions do not support a clinically coherent common target population, the claim is downgraded from external validation to cross-database replication.

## 4. Target population, eligibility, and index time

### 4.1 Target population

Adults receiving invasive mechanical ventilation in an ICU who have suspected infection and an acute oxygenation impairment defined by a paired P/F ratio of 300 mmHg or lower with PEEP of at least 5 cmH2O.

Because neither database supplies harmonized imaging adjudication, the cohort is called an **oxygenation-defined acute hypoxemia phenotype**. It is not called imaging-adjudicated Berlin ARDS or globally adjudicated ARDS.

### 4.2 Inclusion criteria

All criteria must be satisfied for the same qualifying index event:

1. Age at ICU admission at least 18 years.
2. A valid in-ICU arterial PaO2 measurement.
3. A valid FiO2 paired to PaO2 within ±120 minutes.
4. P/F = PaO2 / FiO2 fraction at or below 300 mmHg.
5. A valid PEEP measurement of at least 5 cmH2O paired to PaO2 within ±120 minutes.
6. Explicit invasive-airway or invasive-ventilation evidence active at, or in the locked source-specific vicinity of, that event.
7. No conflicting proximal non-invasive-ventilation evidence under the source-specific rule.
8. Suspected infection evidence available by index, from 48 hours before index through index itself.
9. A first valid complete ventilator tuple within 0–6 hours after index for the primary exposure analysis.

Criteria 1–8 define the strict source cohort. Criterion 9 defines the primary complete-tuple analysis population. The difference between these populations is audited as measurement selection.

### 4.3 Exclusions

- Age below 18 years.
- No valid time-aligned P/F and PEEP event meeting the thresholds.
- No explicit invasive ventilation evidence or proximal NIV conflict.
- No infection evidence in the locked window.
- Physiologically invalid or internally inconsistent values under Section 6.
- No complete ventilator tuple in the 0–6-hour exposure window for exposure models.

There is no exclusion based on ICU length of stay of at least 24 hours. There is no requirement that index occur during calendar ICU day 1. “Early” refers to the index-relative exposure window, not necessarily the first 24 hours after ICU admission.

### 4.4 Index time

For each ICU stay, index is the timestamp of the **earliest** PaO2 event satisfying all time-aligned P/F, PEEP, invasive ventilation, NIV exclusion, and infection criteria.

For multiple qualifying stays:

- MIMIC-IV: select the chronologically earliest qualifying ICU stay per subject using ICU intime, with stable identifier tie-breaks.
- eICU-CRD: select the earliest qualifying event per stay, then use hospitaldischargeyear, patienthealthsystemstayid, unitvisitnumber, and patientunitstayid as a deterministic ordering proxy within uniquepid because a fully comparable calendar admission date is unavailable.

The eICU ordering limitation must be reported. An all-qualifying-stays sensitivity analysis is specified in Section 12.

### 4.5 Source-specific infection rule

**MIMIC-IV primary rule:** a Seymour-style suspected-infection pair consisting of a systemic antibacterial prescription and a microbiology culture, with culture-to-antibiotic lag no more than 72 hours or antibiotic-to-culture lag no more than 24 hours. The antibacterial term and route exclusions reproduce the official MIT-LCP MIMIC-IV `antibiotic.sql` concept at the pinned commit; prescription `starttime` is the antibiotic timestamp. All microbiology specimen types eligible in the official concept are retained; culture positivity is descriptive and is not required. The pair time is the earlier of the two qualifying timestamps, it must fall from index−48 hours through index, and **both elements of the pair must be available no later than index**. For cultures with only `chartdate`, midnight is used solely for official date-window matching, while evidence availability is conservatively assigned to 23:59:59 on that date; an exact-`charttime`-only cohort is a prespecified source-precision sensitivity.

**eICU-CRD primary rule:** time-stamped `diagnosis.diagnosisstring` at `diagnosisoffset` and admission diagnoses in `admissionDx` at `admitdxenteredoffset` are screened using the locked infection term list in data_dictionary_v1.md. A qualifying diagnosis timestamp must be from index−48 hours through index. `patient.apacheadmissiondx` is not assigned a synthetic offset of 0 and does not establish primary eligibility because its actual availability time is not encoded in that field. This is the outcome-blinded U005 amendment recorded in D050.

The eICU definition is diagnosis-based and is not described as equivalent to the MIMIC Seymour rule.

**Prespecified retrospective phenotype sensitivity:** allow infection evidence through index+24 hours. This sensitivity deliberately uses post-index phenotype information, is labeled as such, and cannot replace the primary prediction cohort.

### 4.6 Source-specific invasive ventilation rule

**MIMIC-IV:** active procedureevents item 225792 is the preferred invasive-ventilation evidence. If it is absent, the most recent official MIT-LCP ventilation classification within 14 hours may confirm invasive ventilation through an endotracheal-tube device or a mode in the pinned official invasive-mode list. A tracheostomy device alone does not establish active positive-pressure ventilation. Procedure item 225794 is exclusionary when its interval overlaps index±120 minutes. Bipap/CPAP-mask or DuoPaP/NIV/NIV-ST chart evidence within ±120 minutes is independently exclusionary even if an invasive marker is charted at the same timestamp. PEEP alone is never sufficient evidence of invasive ventilation.

**eICU-CRD:** invasive airway states Oral ETT, Nasal ETT, Tracheostomy, Double-Lumen Tube, or Cricothyrotomy are accepted from respiratoryCare. A prior invasive airway state within 720 minutes is accepted; future confirmation within 120 minutes is accepted only if no “No Artificial Airway” state occurs within the previous 120 minutes. RespiratoryCharting ETT evidence follows the same timing rule. A narrowly mapped explicit invasive mode within ±120 minutes is also accepted: O2 Device equal to Ventilator or ETT; Mechanical Ventilator Mode equal to AC/CMV, SIMV, PCV w/assist, SIMV+, or APRV; or Ventilator Support Mode equal to CMV, SIMV, APV, or Pressure control. Proximal NIV evidence within ±120 minutes excludes the event. In `Non-invasive Ventilation Mode`, only S/T, CPAP, and AVAPS identify positive-pressure NIV; Nasal cannula, HFNC, Venturi, and Non-rebreather mask are oxygen-delivery interfaces and do not. NIV-prefixed settings/measurements, Bipap Delivery Mode, and O2 Device equal to Bi-PAP/CPAP remain NIV evidence. Generic RT-on markers, nonspecific support fields, CPAP/pressure-support modes, tracheostomy mask, T-piece, and APACHE intubated/vent flags are descriptive QC only and cannot establish primary eligibility by themselves.

## 5. Time origin, exposure window, and risk period

- **Index time:** first strict event defined in Section 4.4.
- **Primary exposure window:** index through index+6 hours.
- **Tuple anchor:** plateau-pressure timestamp.
- **Primary pairing window:** highest available locked source tier within ±60 minutes of the plateau anchor, then the nearest valid component within that tier.
- **Sensitivity pairing window:** the same source-tier rule within ±30 minutes.
- **Prediction time:** the timestamp of the final component required to make the selected complete tuple available. This is never earlier than the plateau anchor.
- **Outcome risk origin:** prediction time for the primary logistic endpoint.

The first physiologically valid complete tuple is selected. If multiple values share a timestamp and source tier, their median is used. Component pairing first selects the highest available locked source tier because observed/exhaled VT and total measured RR are not construct-equivalent to set values. Within that tier, the nearest value is selected by smallest absolute time gap; an earlier value wins an exact prior/future tie. Component source and signed time gap are retained. This outcome-blind ordering clarification is D052 and partially supersedes the unqualified wording in D010.

Patients who die or are discharged before a complete tuple becomes available cannot enter the complete-tuple analysis and are represented in the selection audit. No value charted after prediction time may enter a predictor or severity variable.

### 5.1 Twenty-four-hour landmark sensitivity

For the prespecified landmark sensitivity, sMP is the patient-level median of all valid tuples from index through index+24 hours. The landmark is index+24 hours. Only patients alive, still under observable follow-up, and with a defined risk state at the landmark are included; events before the landmark are not counted as post-landmark outcomes. This is not pooled with the primary analysis.

## 6. Exposure construction and preprocessing

### 6.1 Complete tuple

A complete tuple contains plateau pressure, peak inspiratory pressure, PEEP, tidal volume, and total respiratory rate.

Primary source hierarchy:

- PEEP: set PEEP, then total PEEP as flagged fallback.
- Tidal volume: observed/exhaled VT, then set VT as flagged fallback.
- Respiratory rate: total measured RR, then general measured RR, then set ventilator rate as flagged fallback.
- Plateau and peak pressures: explicitly labeled charted values only.

Source tier precedes temporal proximity, but only among candidates already inside the locked pairing window. Temporal proximity, prior/future tie direction, and deterministic timestamp/item ordering are applied within the highest available tier. The ±30-minute and strict preferred-source analyses assess dependence on this choice.

eICU “RR (patient)” is a spontaneous-rate component and is not accepted as total RR by itself. Every fallback is retained in a source flag and is tested in a source-restricted sensitivity analysis.

### 6.2 Unit normalization

- FiO2 values in 0.21–1.00 are multiplied by 100; values already in 21–100 remain percentages.
- PaO2 is mmHg.
- Pressures are cmH2O.
- VT is mL; a source explicitly stored in liters is multiplied by 1,000.
- RR is breaths/min.
- Height in inches is multiplied by 2.54.

Ambiguous units are set missing, not guessed.

### 6.3 Locked physiologic validity rules

Bounds are inclusive unless an ordering rule requires a strict inequality:

| Variable | Valid range |
|---|---:|
| PaO2 | 20–700 mmHg |
| FiO2 | 21–100% |
| PEEP | 5–30 cmH2O |
| Plateau pressure | 5–60 cmH2O |
| Peak pressure | 5–80 cmH2O |
| Tidal volume | 100–1,500 mL |
| Respiratory rate | 5–60/min |
| Driving pressure | 0–40 cmH2O |
| sMP | 0–100 J/min |

Required ordering is Ppeak ≥ Pplat ≥ PEEP. Values violating the range or ordering are excluded from tuple formation and counted by reason. Boundary values are retained but separately tabulated.

### 6.4 Derived variables

- Driving pressure: ΔP = Pplat − PEEP.
- Resistive pressure: Ppeak − Pplat.
- Surrogate mechanical power:

  sMP = 0.098 × RR × (VT_mL / 1,000) × [Ppeak − 0.5 × (Pplat − PEEP)].

The primary scale is absolute sMP in J/min. The primary association unit is 5 J/min. A second scale uses one MIMIC development-cohort SD; the MIMIC mean and SD are frozen and applied unchanged in eICU.

### 6.5 Predicted body weight and normalization

Height must be 120–230 cm. In MIMIC-IV, primary height uses a valid chartevents measurement documented and available by the relevant endpoint, preferring itemid 226730 cm over 226707 inches. If absent, the most recent valid OMR `Height (Inches)` is used only when its date is at least one calendar day and no more than 1,826 days before index; multiple valid values on one date are summarized by their median. Date-only OMR height is conservatively available at 23:59:59 on its chart date, and same-day/future values are prohibited. Chartevents-only and OMR≤366-day fallback definitions are named sensitivities. eICU uses valid documented admission height in cm. For other or missing values, PBW is missing.

- Male PBW = 50 + 0.91 × (height_cm − 152.4).
- Female PBW = 45.5 + 0.91 × (height_cm − 152.4).

VT/PBW and sMP/PBW are key secondary measures. Records with unknown or non-binary source sex cannot receive formula-based PBW unless an externally justified formula is added before outcome unblinding; they remain in absolute-sMP analyses. Source coverage, chartevents–OMR agreement, and component-complete counts under the 1,826-day, 366-day, and chartevents-only definitions are reported without outcomes.

### 6.6 Continuous-variable forms

The primary sMP association is linear per 5 J/min. The following reporting units are prespecified: age per 10 years, P/F per 50 mmHg, ΔP per 5 cmH2O, RR per 5/min, VT/PBW per 1 mL/kg, PEEP per 5 cmH2O, and resistive pressure per 5 cmH2O.

A secondary nonlinearity analysis uses a restricted cubic spline for sMP with four knots at the 5th, 35th, 65th, and 95th percentiles of the MIMIC development distribution. Numeric knots are frozen before eICU outcomes are evaluated. No form is selected using significance, AIC, visual appearance, or eICU performance.

Continuous variables in the harmonized severity core use prespecified three-knot restricted cubic splines at the 10th, 50th, and 90th MIMIC development percentiles. The same numeric knots are used externally. This transformation is outcome-agnostic and is repeated inside each internal bootstrap resample.

## 7. Outcomes

### 7.1 Primary outcome

**In-hospital mortality after prediction time.**

- MIMIC-IV: admissions.hospital_expire_flag.
- eICU-CRD: patient.hospitaldischargestatus equal to “Expired”.

The endpoint is binary. Patients discharged alive are non-events. Records with unknown discharge status are excluded and counted.

### 7.2 Secondary outcomes

1. **MIMIC 28-day mortality:** death date on or before calendar date prediction_time + 28 days using patients.dod. Death after 28 days or no recorded death is coded non-event, with the database-specific death-date limitation reported.
2. **ICU mortality:** death before ICU discharge, using the source-specific ICU discharge status/location fields.
3. **Twenty-four-hour landmark in-hospital mortality:** in the landmark population defined in Section 5.1.

MIMIC 28-day mortality is not applied to eICU in a claim of formal external validation. Different outcomes are reported as secondary replication only.

## 8. Harmonized severity and native benchmarks

### 8.1 Primary harmonized severity core

The same pre-prediction raw severity block is used in both databases:

- worst total GCS;
- minimum mean arterial pressure;
- any vasopressor exposure;
- minimum platelet count;
- maximum serum creatinine.

The measurement window is index−24 hours through prediction time. If index occurs less than 24 hours after ICU admission, the window begins at ICU admission. Measurements after prediction time are prohibited. GCS is reconstructed from eye, verbal, and motor components where possible; an intubation-related verbal limitation is not silently scored as normal. MAP prefers invasive measurement over non-invasive measurement at an exact tie. Vasopressor exposure is binary because cross-database dose equivalence is not reliable.

Age, sex, and index P/F are included separately in S0. Bilirubin and urine output are excluded from the primary core because known cross-database missingness and collection differences could make “harmonized severity” depend on unequal information. They remain in the non-respiratory SOFA benchmark.

This block is called a harmonized severity core, not SOFA.

### 8.2 Harmonized non-respiratory SOFA benchmark

A secondary benchmark reconstructs coagulation, hepatic, cardiovascular, renal, and neurologic SOFA domains with the same thresholds and index−24-hour-to-prediction window. Domain missingness is reported; missing is never automatically scored zero. Because vasopressor dose capture and urine output differ across databases, both complete-domain and creatinine/binary-pressor simplified versions are shown and explicitly labeled as modified benchmarks.

### 8.3 Database-native benchmarks

- MIMIC-IV: the verified, outcome-free official OASIS predictor-side implementation is retained as a separate contextual native first-day benchmark with versioned code provenance. Source-faithful OASIS assigns zero to an unavailable component through the official `COALESCE(component_score,0)` rule; this native exception never applies to HSC or harmonized/modified SOFA. Component missingness and an all-10-components-observed sensitivity are mandatory. Because its window may extend to ICU admission +24 hours, OASIS cannot replace the prediction-time HSC or enter the index-time S0 model. Modeled use is restricted to primary tuples whose hypoxemia index occurs at or after ICU admission +24 hours. The unchanged published probability is `logit(p) = -6.1746 + 0.1275*OASIS`, copied from the pinned official `oasis.sql`.
- eICU-CRD: APACHE IVa score and predicted hospital mortality from `apachePatientResult`. Because APACHE IVa is also a first-day construct, outcome modeling requires index offset at least 1,440 minutes and a valid native risk; earlier-index records contribute only outcome-blind feasibility counts.

For probabilities, values are clipped only for the logit calculation at 1e−6 and 1−1e−6; the unclipped distribution is reported. Native predictions are reported first without updating, then after intercept-only recalibration, then after intercept-and-slope updating. A local model adding linear sMP/5 to the recalibrated native logit is reported separately. Adding sMP to OASIS or APACHE is local model extension, not external validation of the native score or of the MIMIC model. Native-benchmark uncertainty uses the same 1,000 MIMIC patient and 2,000 eICU hospital-cluster bootstrap rules as the core analysis.

## 9. Prespecified models

All core models are logistic regression models for in-hospital mortality.

| Model | Predictors | Role |
|---|---|---|
| S0 | harmonized severity core + age + sex + index P/F | baseline severity model |
| S1 | S0 + ΔP | added driving pressure |
| S2 | S0 + ΔP + RR | simple Costa-type ventilatory model |
| S3 | S0 + sMP | composite sMP model |
| S4 | S0 + ΔP + RR + VT/PBW + PEEP + resistive pressure | full component model |
| S5 | S4 + sMP | descriptive mathematical stress test only |
| S2M | S2 + sMP | prespecified nested secondary extension |
| R2 | age + sex + index P/F + MAP + vasopressor + platelet + creatinine + ΔP + RR | reduced-core no-GCS sensitivity |
| R3 | age + sex + index P/F + MAP + vasopressor + platelet + creatinine + sMP | reduced-core no-GCS sensitivity |

The single primary model comparison is non-nested S2 versus S3. S2M resolves the plan’s separately requested nested “S2 plus sMP” comparison and is not counted as a replacement for S0–S5. S3 versus S4 and S2 versus S2M are key secondary comparisons.

R2 versus R3 is a prespecified sensitivity to the strict GCS-reconstruction measurement bottleneck. It is fit on one identical reduced-core complete set in each database, uses the same MIMIC-frozen transformations for all retained predictors, and may improve precision but has greater residual neurologic confounding. It cannot replace the primary S2-versus-S3 result.

Because sMP is a deterministic function of S4 components, the S5 coefficient has no independent biological interpretation. If S5 is unstable or non-estimable from collinearity, non-estimability is the result; no outcome-driven penalized rescue is used.

No data-driven variable selection, stepwise procedure, cut-point search, or eICU-based model revision is permitted.

## 10. Analysis populations and missing data

### 10.1 Populations

1. **Strict source cohort:** satisfies age, index, ventilation, oxygenation, PEEP, and infection rules.
2. **Primary tuple cohort:** strict source cohort with a valid first complete tuple in 0–6 hours.
3. **Primary comparison set:** tuple cohort with complete S0, S2, and S3 predictors and known primary outcome. S2 and S3 are compared only on this identical set.
4. **Component comparison set:** records with complete S0–S5 predictors, including valid height/PBW. S3 and S4 are compared only on this identical set.
5. **Reduced-core comparison set:** records with complete R2 and R3 predictors and known primary outcome. R2 and R3 are compared only on this identical set.
6. **All-stays sensitivity population:** all qualifying stays with patient clustering.

Flow counts, events, and missingness are given for every population in both databases.

### 10.2 Exposure missingness

Pplat or complete-tuple absence is not imputed in the primary analysis. This is a measurement/selection process defining the observed-tuple population. The strict source cohort is used to compare tuple-observed and tuple-unobserved patients using pre-prediction variables. Wording is “selection-weighted sensitivity” or “under the specified observation model,” never “selection bias corrected.”

### 10.3 Covariate missingness

Primary model comparisons use complete cases on a common model-comparison set. Missing is not coded as normal and is not replaced by a missing indicator in the core analysis.

Multiple imputation is a sensitivity analysis:

- 50 imputations, 20 iterations;
- database-specific association-focused imputation with fixed seeds 20260717 (MIMIC) and 20260718 (eICU);
- predictive mean matching with five donors for the only incomplete model covariates: GCS, MAP, platelet, and creatinine;
- the complete observed ventilator tuple, age, sex, P/F, vasopressor, delta pressure, RR, and sMP enter the predictor matrix but are not imputed; the primary outcome is included as a predictor and is never imputed;
- the D054 MIMIC-frozen transformations are applied after imputation; S2, S3, and S2M coefficients and linear sMP-per-5-J/min contrasts are pooled with Rubin's rules;
- imputation diagnostics, Monte Carlo error, convergence traces, and observed/imputed distributions are reported;
- fraction of missing information and relative efficiency are reported;
- no MI discrimination, calibration, overall-performance, or locked external-validation estimate is reported, because the eICU outcome is used in its database-specific association imputation.

An MNAR pattern-mixture sensitivity shifts only imputed GCS, MAP, platelet, and creatinine values by 0.5 and 1.0 frozen MIMIC SD in the clinically worse direction, bounded to the source-valid ranges. The corresponding frozen SDs are 4.6877682109, 14.5493740348, 108.8391842917, and 1.5262113315. P/F is not shifted because it is modeled separately from the HSC and is complete. The adverse-binary odds shift is not applicable in the rebuilt frames because vasopressor status is complete. These scenarios never impute or alter the observed exposure tuple.

### 10.4 Observation weighting

A prespecified selection model estimates the probability of complete-tuple observation separately in each database using only variables known by index and never the outcome. The fixed linear, no-interaction terms are age/10 years, female sex, index P/F/50 mmHg, index PEEP/5 cmH2O, index FiO2/10 percentage points, index time/24 hours, GCS, MAP/10 mmHg, platelets/100 K/µL, creatinine, and any vasopressor. Database-specific median imputation plus a missingness indicator is used only inside the observation model. Stabilized weights use the database-specific marginal tuple-observation probability as numerator, are defined only for observed tuples, and are truncated at their observed-record 1st and 99th percentiles.

Covariate balance, probability calibration, full weight distribution, and effective sample size are reported. If the effective sample size is below 50% of the unweighted complete-tuple sample, the 99th percentile exceeds 10, or any untruncated weight exceeds 20, the result is labeled positivity-sensitive and cannot support a correction claim. In eICU, hospitals with no observed tuple are a structural support failure that patient-level weighting cannot repair. Therefore the full-target analysis is automatically labeled structurally positivity-sensitive, and a separately named sensitivity refits the identical observation model after restricting the target to hospitals with at least one observed tuple. That restriction changes the target population, retains every observed tuple, and cannot replace the full-cohort external analysis.

## 11. Development, internal validation, and external validation

### 11.1 Development

Models are estimated in MIMIC-IV. All coefficients, intercepts, factor coding, transformations, spline knots, standardization constants, imputation rules, and prediction equations are frozen before eICU outcomes or performance are evaluated.

### 11.2 Internal validation

MIMIC-IV uses 1,000 bootstrap resamples at the patient level. The entire modeling pipeline, including transformation constants and any imputation, is repeated inside each resample. Apparent, optimism, and optimism-corrected performance are reported. A random train/test split is not used.

### 11.3 External validation

The locked models are applied once to eICU-CRD. No coefficient or form is chosen using eICU outcome. Ninety-five percent confidence intervals and between-model differences use 2,000 hospital-cluster bootstrap resamples with a fixed recorded random seed.

Original locked-model performance is always reported before:

1. intercept-only recalibration; and
2. intercept-and-slope updating.

Updated results are labeled model updating and cannot replace original validation results.

### 11.4 Performance metrics

Core metrics, in reporting order:

1. Brier score, scaled Brier score relative to the prevalence-only model, and log loss;
2. calibration-in-the-large, calibration slope, and a flexible calibration curve;
3. C statistic;
4. between-model differences with paired bootstrap confidence intervals;
5. development optimism-corrected performance.

Likelihood-ratio tests are used only for genuinely nested models, including S2 versus S2M. They are not used for S2 versus S3. Continuous NRI and IDI are not core analyses. Decision-curve analysis is not planned for the core paper because no explicit intervention and threshold context is locked; any later DCA is exploratory or journal-requested and must report its decision premise.

## 12. Clustering, repeat stays, and center heterogeneity

- Primary analysis uses one qualifying stay per patient.
- MIMIC internal uncertainty uses patient-level resampling.
- eICU validation uncertainty uses hospital-level resampling.
- The all-stays sensitivity uses patient-cluster robust standard errors or GEE; eICU hospital dependence is additionally handled by hospital-cluster resampling.
- Center heterogeneity in eICU is assessed on the primary common set with the frozen S3 fixed-effect design and a hospital random-intercept model, followed by a correlated random sMP/5 slope. The binomial model uses `glmer`, `bobyqa`, `nAGQ=1`, and `maxfun=200000`; the slope distribution is reported only with clean optimizer convergence, finite estimates, and a non-singular fit at tolerance 1e−4.
- If the random-slope model is unstable, the random-intercept result is retained and a prespecified two-stage fallback is attempted. Hospitals require at least 30 complete records, five events, and five non-events. Within each eligible hospital, mortality is regressed on the locked MIMIC S0 linear predictor and sMP/5. Conventional non-estimable or separated fits are excluded without penalized rescue. At least five eligible hospitals are required for REML pooling with Hartung-Knapp inference.
- Hospital-level tuple support is reported before any outcome heterogeneity analysis. Hospitals with zero observed tuple do not enter a complete-tuple outcome model and are not treated as if patient-level weighting restored their support.
- Center reporting includes the distribution of complete cases and events, tau-squared, I-squared, and a prediction interval where estimable.
- Leave-one-hospital-out influence removes one hospital at a time from fixed original S2/S3 predictions, without refitting or recalibration, and summarizes changes in raw external metrics and paired S3-minus-S2 differences. It assesses influence, not transportability.

Center identifiers and center-specific estimates remain in private artifacts. Public files contain only aggregate eligibility/support counts and pooled heterogeneity/influence summaries. Center-specific estimates are suppressed or pooled when disclosure or numerical-stability thresholds are not met.

## 13. Analysis hierarchy and multiplicity

### 13.1 Primary

- Primary outcome: in-hospital mortality.
- Primary exposure: first valid absolute sMP tuple in index+0–6 hours.
- Primary effect scale: per 5 J/min.
- Primary model comparison: S2 versus S3 in the common primary comparison set.
- Primary external evidence: original locked eICU performance and calibration.

### 13.2 Key secondary

- S0, S1, S4, S5, and S2M model comparisons.
- MIMIC 28-day mortality and ICU mortality.
- sMP/PBW and VT/PBW.
- MIMIC-SD-scaled sMP.
- Nonlinear sMP model frozen from MIMIC.
- Native severity benchmarks and harmonized non-respiratory SOFA benchmark.
- Center heterogeneity.
- Twenty-four-hour landmark analysis.

### 13.3 Sensitivity

- ±30-minute tuple pairing.
- Primary-tuple preferred-source restriction versus fallback-inclusive primary tuple: retain only records whose already selected primary tuple uses preferred source tiers for every component; never reselect a different tuple or change prediction time/HSC. The separate reselected preferred-tuple feasibility artifact is not outcome-modeled without a prediction-time-specific HSC rebuild (D058).
- Complete-case versus prespecified MI, observation weighting, and MNAR scenarios.
- Primary HSC S2/S3 versus the locked reduced-core no-GCS R2/R3 sensitivity on its larger identical complete set.
- OASIS native scoring versus the all-10-components-observed OASIS sensitivity in a timing-compatible landmark population.
- First qualifying stay versus all qualifying stays.
- Alternative stricter P/F strata (≤200 and ≤100) as phenotype sensitivity, not new primary cohorts.
- Infection-source restriction and a sepsis/septic-coded eICU sensitivity.
- Retrospective infection-ascertainment sensitivity extending the upper window from index to index+24 hours.
- Invasive-ventilation evidence restrictions.

### 13.4 Exploratory or conditional

- Infection-by-sMP, severity-by-sMP, ventilator-mode, and other interactions.
- Time-varying sMP, change in sMP, or cumulative burden.
- Coded-ARDS or other phenotype-enrichment analyses.
- Any journal-requested DCA, NRI, IDI, or subgroup.

Exploratory interaction families use Benjamini–Hochberg false-discovery-rate control and are interpreted from effect estimates and uncertainty, not a binary threshold alone. No exploratory branch can rescue an unsupported primary result.

## 14. Phase gates, pivot rules, and stopping rules

### 14.1 Phase 0: design freeze

Pass only when this SAP, the terminology ledger, data dictionary, decision log, and machine-readable configuration agree. All 待核验 source mappings required for cohort construction must be resolved using outcome-blinded QC.

### 14.2 Phase 1: reconstruction and QC

Pass only if every included record satisfies all index criteria at index; future whole-stay hypoxemia cannot determine eligibility; pairing gaps, invalid combinations, missingness, source fallbacks, and reconciliation with the legacy cohort are reported.

If the strict phenotype cannot be implemented coherently in both databases, stop the external-validation claim and use cross-database replication language.

### 14.3 Phase 2: core analysis

Proceed only after coefficients and preprocessing are frozen in MIMIC. eICU is a one-shot locked evaluation.

External sample-size interpretation:

- at least 200 events and 200 non-events: full calibration and performance claims allowed;
- 100–199 in either class: validation remains possible but calibration and heterogeneity claims are explicitly imprecise;
- fewer than 100 events or fewer than 100 non-events: downgrade to exploratory replication and do not claim a definitive external validation.

If fewer than 20 hospitals contribute to the complete comparison set, or most hospitals contribute fewer than five complete cases, center heterogeneity is descriptive only.

### 14.4 Result-contingent interpretation fixed in advance

- If S3 and S2/S4 have similar external performance, conclude that sMP is a compact summary without clear superiority over simpler ventilator variables.
- If S3 is stably better in calibration and overall external performance, incremental predictive value may be supported; a C-statistic difference alone is insufficient.
- If improvement appears only in MIMIC, conclude that the increment did not transport.
- If the strict definition attenuates or removes the legacy association, make design sensitivity a main result and do not restore the legacy loose cohort as primary.
- If only normalized sMP succeeds in one database, retain it as secondary.
- If IPW positivity fails, report instability and do not state that selection was corrected.
- If ventilator mode is unavailable in more than 30% of complete tuples, mode-restricted analyses remain exploratory and cannot define the main population.
- If S5 is non-estimable, report mathematical redundancy rather than searching for an alternative favorable parameterization.
- No empirically optimal sMP threshold, post hoc subgroup, or alternative endpoint may replace an unsupported primary result.

### 14.5 Deviations after outcome access

After rebuilt outcomes are opened, any newly necessary analysis is:

1. entered in the decision log with the exact information already seen;
2. versioned as a SAP amendment;
3. labeled exploratory unless it corrects a reproducible coding error that is independent of outcome;
4. reported alongside, not instead of, the frozen analysis.

## 15. Minimum reporting package

- Two-database flow diagram and index/exposure/prediction timeline.
- Source availability, timing-gap, and complete-tuple selection audit.
- Baseline table restricted to variables known by prediction time, with missing n and standardized differences.
- S0–S4 performance ladder with MIMIC optimism-corrected and original locked eICU results.
- Calibration plots before and after clearly separated model updating.
- sMP and component effect estimates on prespecified scales.
- Center distribution and heterogeneity.
- Missing-data, source, window, phenotype, repeated-stay, and landmark sensitivities.
- Exact code/configuration versions and all decision-log amendments.

The manuscript must follow the terminology ledger and distinguish association, incremental prediction, calibration, transport, replication, and model updating.
