# Data dictionary v1: ARDS mechanical-power rebuild

**Version:** 1.0.1  
**Freeze date:** 2026-07-15  
**Applies to:** MIMIC-IV v3.1 and eICU-CRD v2.0  
**Status vocabulary:** 已核验 = verified against local schema/metadata; 候选 = scientifically prespecified but implementation must pass outcome-blinded label/timing QC; 待核验 = must not be silently assumed.

This dictionary maps source data to the common analytic variables. A source mapping may be resolved only with schema, metadata, frequency, units, timing, missingness, and physiologic QC before rebuilt mortality/effect/performance results are viewed. Exact item IDs not verified locally are labeled 待核验.

The expanded versioned preflight manifest now requires all 27 inputs, including MIMIC-IV OMR/inputevents/outputevents and eICU nurseCharting/infusionDrug/medication/intakeOutput. U009–U010 were resolved by D030 before severity extraction; D053 adds OMR after an outcome-blind PBW feasibility audit. eICU HSC and age mappings U003/U007 were resolved by D038–D039; the six-drug vasoactive construct, medication zero-offset handling, CSV logical-record gate, and PBW derivation are frozen in D040–D043.

## 1. Common time variables and derived fields

| Analytic field | Definition | Window/rule | Unit/type |
|---|---|---|---|
| person_id | Source-specific stable patient identifier | MIMIC subject_id; eICU uniquepid with deterministic fallback only for QC | character/integer |
| stay_id | ICU stay identifier | MIMIC stay_id; eICU patientunitstayid | integer |
| hospital_id | Hospital cluster | MIMIC single source; eICU hospitalid | integer |
| index_time | Earliest PaO2 event satisfying every locked phenotype criterion | In ICU; not restricted to ICU day 1 | timestamp or minute offset |
| exposure_start | index_time | Fixed | timestamp/offset |
| exposure_end | index_time + 6 h | Fixed | timestamp/offset |
| tuple_anchor_time | Plateau-pressure timestamp | Within exposure window | timestamp/offset |
| prediction_time | Latest timestamp among fields needed for selected tuple and locked predictors | Must be no later than index+6 h | timestamp/offset |
| pf_ratio | PaO2 / FiO2 fraction | PaO2–FiO2 gap ≤120 min | mmHg |
| delta_p | Pplat − PEEP | Same valid tuple | cmH2O |
| resistive_pressure | Ppeak − Pplat | Same valid tuple | cmH2O |
| smp | 0.098 × RR × (VT_mL/1000) × [Ppeak − 0.5 × (Pplat−PEEP)] | Same valid tuple | J/min |
| pbw | Sex-specific ARDSNet formula | Valid height 120–230 cm | kg |
| vt_pbw | VT / PBW | Secondary/component model | mL/kg |
| smp_pbw | sMP / PBW | Secondary | J/min/kg |
| hospital_mortality | Death before hospital discharge | Primary common outcome | 0/1 |
| icu_mortality | Death before ICU discharge | Secondary | 0/1 |
| mortality_28d | Death date ≤ prediction date+28 | MIMIC only, secondary | 0/1 |

Complete-tuple pair rule (D052): choose the highest available locked source tier inside the pairing window, then the smallest absolute gap within that tier; a prior value wins an exact prior/future tie. Duplicate numeric observations at the same timestamp and source tier are reduced to the median. Source label, original value, normalized value, signed gap, and fallback status are retained. Index PaO2–FiO2/PEEP pairing follows its separate D036 rule.

Preferred-source outcome sensitivity (D058): restrict the primary selected tuple to records whose PEEP, VT, and RR (and any other tiered component) are all from their preferred tier. Do not reselect another tuple and do not alter prediction time or HSC. A separately generated preferred-only reselected tuple is exposure-QC only unless severity is rebuilt to its own prediction time.

## 2. Locked units, validity, and transformations

| Domain | Accepted normalized unit | Validity | Transformation/QC |
|---|---|---|---|
| PaO2 | mmHg | 20–700 | no unit conversion unless source explicitly records another unit |
| FiO2 | percent | 21–100 | 0.21–1.00 ×100; 21–100 unchanged; other values missing |
| PEEP | cmH2O | 5–30 | set preferred; total/CPAP-labeled fallback flagged |
| Pplat | cmH2O | 5–60 | explicit plateau label only |
| Ppeak | cmH2O | 5–80 | explicit peak label only |
| VT | mL | 100–1,500 | observed/exhaled preferred; set fallback flagged |
| RR | breaths/min | 5–60 | total preferred; measured then set fallback |
| ΔP | cmH2O | 0–40 | derived after pairing |
| sMP | J/min | 0–100 | derived after all component checks |
| Height | cm | 120–230 | inches ×2.54 |
| Ordering | — | Ppeak ≥ Pplat ≥ PEEP | violating tuple invalid |

## 3. MIMIC-IV mappings

### 3.1 Identifiers, times, demographics, and outcomes

| Analytic field | Table.field | itemid/label | Rule | Status |
|---|---|---|---|---|
| person_id | hosp/patients.subject_id | — | stable patient ID | 已核验 |
| hadm_id | hosp/admissions.hadm_id | — | hospital encounter | 已核验 |
| stay_id | icu/icustays.stay_id | — | ICU stay | 已核验 |
| ICU times | icu/icustays.intime/outtime | — | inclusion and temporal bounds | 已核验 |
| age | patients.anchor_age plus anchor_year method | — | official MIMIC age derivation at admission; age ≥18 | 候选 |
| sex | patients.gender | — | retain source coding; map M/F | 已核验 |
| height_cm primary | icu/chartevents.valuenum; fallback hosp/omr.result_value | 226730 Height (cm); 226707 Height (inch); exact OMR `Height (Inches)` | valid pre-endpoint chartevents cm preferred, then chartevents inches ×2.54; if absent, most recent valid OMR date index−1 through index−1,826 days, daily median; require 120–230 cm | 已核验; D046/D053 |
| height_cm sensitivities | same | same | chartevents-only and OMR≤366-day fallback; named fields, never replace primary after outcomes | locked; D053 |
| weight_kg | icu/chartevents.valuenum | 226512 Admission Weight (Kg) | descriptive only | 已核验 |
| hospital_mortality | hosp/admissions.hospital_expire_flag | — | primary outcome | 已核验 |
| mortality_28d | hosp/patients.dod | — | prediction calendar date +28 days | 已核验 |
| icu_mortality | admissions.deathtime and icustays.outtime | — | death at/before ICU outtime | 候选 |

### 3.2 Oxygenation and ventilation phenotype

| Analytic field | Table.field | itemid/label | Window/hierarchy | Status |
|---|---|---|---|---|
| PaO2 | hosp/labevents.valuenum | 50821 pO2, Blood Gas | in-ICU timestamp; 20–700 mmHg | 已核验 |
| FiO2 | hosp/labevents.valuenum; icu/chartevents.valuenum | 50816 FiO2 from the same blood-gas specimen; 223835 Inspired O2 Fraction | nearest to PaO2 within ±120 min; truly same-specimen 50816 is preferred at the PaO2 timestamp; no cross-source averaging | 已核验 |
| index PEEP | icu/chartevents.valuenum; hosp/labevents.valuenum | 220339 PEEP set; 224700 Total PEEP Level; 50819 PEEP blood-gas fallback | nearest within ±120 min; at an exact timestamp rank set PEEP, total PEEP, then lab fallback; ≥5; no cross-source averaging | 已核验 |
| invasive ventilation | icu/procedureevents.starttime/endtime | 225792 Invasive Ventilation | active at index preferred | 已核验 itemid; interval semantics 候选 |
| NIV exclusion | icu/procedureevents.starttime/endtime; icu/chartevents.value | 225794 Non-invasive Ventilation; 226732 Bipap/CPAP mask; 229314 DuoPaP/NIV/NIV-ST | procedure interval overlaps index±120 min or independent raw chart marker within ±120 min; simultaneous invasive marker does not erase NIV conflict | 已核验 |
| airway/device fallback | icu/chartevents.value | 226732 O2 Delivery Device(s) | Endotracheal tube may establish invasive status; tracheostomy tube/mask alone is not active ventilation | 已核验 |
| vent-mode fallback | icu/chartevents.value | 223849 Ventilator Mode; 229314 Ventilator Mode (Hamilton) | exact invasive-mode list reproduced from pinned official MIT-LCP ventilation concept; most recent status within 14 h | 已核验 |

### 3.3 Complete ventilator tuple

| Analytic field | Table.field | itemid/label | Hierarchy | Status |
|---|---|---|---|---|
| Pplat | icu/chartevents.valuenum | 224696 Plateau Pressure | anchor | 已核验 |
| Ppeak | icu/chartevents.valuenum | 224695 Peak Insp. Pressure | nearest ±60 min | 已核验 |
| PEEP | icu/chartevents.valuenum | 220339 PEEP set; 224700 Total PEEP Level | set then total | 已核验 |
| VT | icu/chartevents.valuenum | 224685 Tidal Volume (observed); 224684 Tidal Volume (set) | observed then set | 已核验 |
| RR | icu/chartevents.valuenum | 224690 Respiratory Rate (Total); 220210 Respiratory Rate; 224688 Respiratory Rate (Set) | total, measured, then set | 已核验 |

### 3.4 Suspected infection

| Analytic field | Table.field | Mapping | Rule | Status |
|---|---|---|---|---|
| culture_time | hosp/microbiologyevents.charttime/chartdate | all specimen types retained as in official concept | exact `charttime` preferred; `chartdate` supports official date matching but primary evidence availability is end-of-day | 已核验 |
| antibiotic_time | hosp/prescriptions.starttime | systemic antibacterial term and route list from pinned official `antibiotic.sql` | prescription start time; no inputevents administration substitution | 已核验 |
| suspected_infection_time | derived | paired culture + antibiotic within same hospital admission | culture first: antibiotic ≤72 h later; antibiotic first: culture ≤24 h later; pair anchor is earlier timestamp | 已核验 |
| infection_eligible_primary | derived | suspected_infection_time | pair anchor index−48 h through index, and both pair elements available by index | locked |
| infection_eligible_plus24 | derived | suspected_infection_time | index−48 h through index+24 h; retrospective phenotype sensitivity only | locked secondary |
| exact-culture-time sensitivity | derived | culture has nonmissing microbiologyevents.charttime | retain the primary −48/0 and evidence-availability rules while excluding date-only cultures | locked secondary |

The antibiotic/culture implementation was cross-checked against the pinned official MIMIC concepts and local schema without rebuilt MIMIC outcome access. D034–D035 resolve U001; the executable drug list, route exclusions, pair ordering, source precision, and conservative date-only availability rule remain the authoritative audit trail.

### 3.5 Harmonized severity core and benchmarks

| Analytic field | Table.field | itemid/label | Rule | Status |
|---|---|---|---|---|
| MAP | icu/chartevents.valuenum | 220052 arterial MAP; 220181 non-invasive MAP | valid 1–250 mmHg minimum in max(ICU intime,index−24 h) through endpoint; arterial wins exact minimum tie | 已核验; D044 |
| GCS eye | icu/chartevents | 220739 GCS Eye Opening | valid integer 1–4; same-charttime strict reconstruction | 已核验; D044 |
| GCS verbal | icu/chartevents | 223900 GCS Verbal Response | valid integer 1–5; intubation/ET-trach text and value conflicts remain unscorable | 已核验; D044 |
| GCS motor | icu/chartevents | 223901 GCS Motor Response | valid integer 1–6; same-charttime strict reconstruction | 已核验; D044 |
| norepinephrine | icu/inputevents | 221906 | qualifying positive-rate active interval | 已核验; D045 |
| epinephrine | icu/inputevents | 221289 | same | 已核验; D045 |
| vasopressin | icu/inputevents | 222315 | same | 已核验; D045 |
| dopamine | icu/inputevents | 221662 | same | 已核验; D045 |
| dobutamine | icu/inputevents | 221653 | same | 已核验; D045 |
| phenylephrine | icu/inputevents | 221749 | same | 已核验; D045 |
| platelet | hosp/labevents.valuenum | 51265 Platelet Count | valid K/uL >0–9999; minimum; charttime and storetime available by endpoint | 已核验; D044 |
| creatinine | hosp/labevents.valuenum | 50912 Creatinine | valid mg/dL 0.1–28.28; maximum; charttime and storetime available by endpoint | 已核验; D044 |
| bilirubin | hosp/labevents.valuenum | 50885 Bilirubin, Total | non-respiratory SOFA benchmark only | 已核验 |
| urine output | icu/outputevents.valuenum | 226559 Foley; 226627 OR Urine; 226631 PACU Urine; additional urine item set | benchmark only; irrigation handled explicitly | partial; complete item set 待核验 |
| OASIS | derived | official MIMIC predictor-side concept at commit `5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4` | separate outcome-free native first-day contextual benchmark; reproduce official missing-component score→0 rule, report component missingness and all-10-observed sensitivity; cannot replace prediction-time HSC or enter index-time S0 | 已核验; D048/D056 |

For complete-tuple prediction HSC, the MIMIC endpoint is tuple `prediction_time`; for the distinct observation-selection core, it is index. Every clinical measurement must be within max(ICU intime,index−24 h) through its endpoint and its nonmissing `storetime` must be no later than that endpoint. Chartevents height starts at ICU intime and cannot occur or become available after the endpoint. Historical OMR height is eligible only under D053, uses date-end availability, and must predate index by at least one day. The four source filters are valid only with both the extended D047/D053 cache PASS gate and the downstream Phase 2b PASS gate.

## 4. eICU-CRD mappings

### 4.1 Identifiers, times, demographics, and outcomes

| Analytic field | Table.field | Label/value | Rule | Status |
|---|---|---|---|---|
| person_id | patient.uniquepid | — | stable person key where present | 已核验 |
| health-system stay | patient.patienthealthsystemstayid | — | deterministic ordering support | 已核验 |
| stay_id | patient.patientunitstayid | — | ICU stay | 已核验 |
| hospital_id | patient.hospitalid | — | bootstrap cluster | 已核验 |
| age | patient.age | literal `> 89` | map to 90; retain `age_topcoded_gt89` and exclusion-sensitivity flag | 已核验; D039 |
| sex | patient.gender | source values | map Male/Female; other missing for PBW | 已核验 |
| height_cm / PBW | patient.admissionheight + patient.gender | documented cm | accept height 120–230 cm only; no unit guessing/conversion; ARDSNet sex-specific PBW | 已核验; D043 |
| weight_kg | patient.admissionweight | kg | descriptive | 已核验 |
| ICU end | patient.unitdischargeoffset | minutes from ICU admission | temporal bound | 已核验 |
| hospital_mortality | patient.hospitaldischargestatus | Expired | primary outcome | 已核验 |
| icu_mortality | patient.unitdischargestatus | Expired | secondary outcome | 已核验 |
| hospital metadata | hospital.region/teachingstatus/numbedscategory | — | descriptive/heterogeneity | 已核验 |

### 4.2 Oxygenation and ventilation phenotype

| Analytic field | Table.field | Label/value | Window/hierarchy | Status |
|---|---|---|---|---|
| PaO2 | lab.labresult | labname = paO2 | labresultoffset in ICU; 20–700 | 已核验 |
| FiO2 laboratory | lab.labresult | labname = FiO2 | nearest ±120 min | 已核验 |
| FiO2 respiratory | respiratoryCharting.respchartvalue | FiO2; FIO2 (%) | pooled with laboratory values, source retained | 已核验 |
| index PEEP | respiratoryCharting.respchartvalue | PEEP; PEEP/CPAP | nearest ±120 min; ≥5; PEEP/CPAP source flagged | 已核验 |
| invasive airway state | respiratoryCare.airwaytype | Oral ETT, Nasal ETT, Tracheostomy, Double-Lumen Tube, Cricothyrotomy | prior ≤720 min or non-contradicted future ≤120 min | 已核验 |
| no-airway contradiction | respiratoryCare.airwaytype | No Artificial Airway | prior ≤120 min blocks future confirmation | 已核验 |
| charted ETT fallback | respiratoryCharting | Endotracheal Tube Placement; O2 Device = ETT | same 720-min prior/120-min future logic | 已核验 |
| narrow explicit invasive mode | respiratoryCharting | O2 Device = Ventilator/ETT; Mechanical Ventilator Mode = AC/CMV, SIMV, PCV w/assist, SIMV+, APRV; Ventilator Support Mode = CMV, SIMV, APV, Pressure control | nearest ±120 min; allowed for inclusion only when no proximal NIV evidence | 已核验 outcome-independent mapping |
| NIV exclusion | respiratoryCharting | Non-invasive Ventilation Mode only when value = S/T, CPAP, or AVAPS; any NIV-prefixed setting/measurement field; any Bipap Delivery Mode interface; O2 Device = Bi-PAP/CPAP | nearest ±120 min; Nasal cannula, HFNC, Venturi, and Non-rebreather values in Non-invasive Ventilation Mode are ordinary oxygen interfaces and are not NIV evidence | 已核验 outcome-independent value audit |
| generic vent marker QC | respiratoryCharting | RT Vent On/Off and any ventilator mode/support value outside the narrow explicit list | descriptive only; cannot establish inclusion | 已核验 |
| APACHE vent QC | apacheApsVar.intubated/vent | 1 | descriptive only; not time-aligned inclusion | 已核验 |

### 4.3 Complete ventilator tuple

| Analytic field | Table.field | Locked label(s) | Hierarchy/status |
|---|---|---|---|
| Pplat | respiratoryCharting.respchartvalue | Plateau Pressure | anchor; label 已核验 |
| Ppeak | respiratoryCharting.respchartvalue | Peak Insp. Pressure | nearest ±60 min; label 已核验 |
| PEEP | respiratoryCharting.respchartvalue | PEEP; PEEP/CPAP | PEEP preferred; CPAP-labeled fallback flagged; labels 已核验 |
| VT | respiratoryCharting.respchartvalue | Tidal Volume Observed (VT); Exhaled TV (patient); Exhaled TV (machine); Tidal Volume (set) | observed/exhaled labels share preferred rank; set VT is flagged fallback; exact local frequencies retained | 已核验 |
| RR | respiratoryCharting.respchartvalue | Total RR; Vent Rate | total then ventilator rate fallback; labels 已核验 |
| spontaneous RR QC | respiratoryCharting.respchartvalue | RR (patient) | not a total-RR substitute by itself; QC only |

Label spelling, capitalization, numeric parsing, and duplicate handling must be checked before tuple extraction; no unlisted label may be added after outcome inspection.

For MIMIC-IV, `chartevents.warning=1` is retained when the measurement passes all locked physiological and pressure-order rules. D051 adds a warning-free restriction of the already selected tuple; it does not select a later tuple or alter prediction time.

### 4.4 Infection ascertainment

| Analytic field | Table.field | Rule | Status |
|---|---|---|---|
| timed diagnosis | diagnosis.diagnosisstring, diagnosisoffset | infection term match, negative phrase excluded | 已核验 |
| admission diagnosis | admissionDx.admitdxpath/admitdxname/admitdxtext, admitdxenteredoffset | infection term match; actual entry offset must be index−48 h through index | 已核验; primary per D050 |
| APACHE admission diagnosis | patient.apacheadmissiondx | never assigned synthetic offset 0; descriptive concordance only because availability time is absent | 已核验; non-primary per D050 |
| infection_eligible_primary | derived | qualifying timestamp index−48 h through index | locked |
| infection_eligible_plus24 | derived | qualifying timestamp index−48 h through index+24 h | locked retrospective sensitivity |
| sepsis-coded sensitivity | same fields | sepsis or septic term only | locked secondary |

Locked inclusive infection pattern:

sepsis, septic, pneumonia, infection, infectious, bacteremia, fungaemia, fungemia, viremia, meningitis, encephalitis, endocarditis, cholangitis, cholecystitis, pyelonephritis, urinary tract infection, peritonitis, abscess, cellulitis, or empyema.

Locked exclusions:

non-infectious, noninfectious, without infection, without evidence of infection, no infection, or no evidence of infection.

This is diagnosis-based ascertainment and is not a Seymour-equivalent label.

### 4.5 Harmonized severity core and native benchmarks

| Analytic field | Preferred source | Fallback/source notes | Status |
|---|---|---|---|
| MAP | nurseCharting | `Non-Invasive BP`/`Non-Invasive BP Mean`; `Invasive BP`/`Invasive BP Mean`; `MAP (mmHg)`/`Value`; `Arterial Line MAP (mmHg)`/`Value` | strict numeric 1–250 mmHg; minimum in window; invasive wins only an exact minimum tie; measurement and entry offsets must both be available by window end | 已核验; D038 |
| GCS | nurseCharting | explicit `Glasgow coma score`/`GCS Total` or `Score (Glasgow Coma Scale)`/`Value`; components `Glasgow coma score`/Eyes, Verbal, Motor | valid integer total 3–15; prefer any valid explicit total, otherwise same-time, unique, valid numeric components (1–4/1–5/1–6); intubation or other text is never recoded | 已核验; D038 |
| vasopressor/inotrope exposure | infusionDrug + medication | norepinephrine, epinephrine, vasopressin, dopamine, dobutamine, phenylephrine | binary primary = positive numeric infusion rate or qualifying active medication order; missing-rate infusion documentation and medication-zero-offset retention are separate sensitivities; exact name/HICL mappings and source overlap retained | 已核验; D040–D041 |
| platelet | lab | exact `labname = platelets x 1000`, system unit K/mcL | strict numeric >0–9999; minimum in window; use only the last revision available by window end | 已核验; D038 |
| creatinine | lab | exact `labname = creatinine`, system unit mg/dL | strict numeric 0.1–28.28; maximum in window; use only the last revision available by window end | 已核验; D038 |
| bilirubin | lab and/or apacheApsVar.bilirubin | SOFA benchmark only | exact primary label variants 待核验 |
| urine | intakeOutput and/or apacheApsVar.urine | SOFA benchmark only; source differences explicit | mapping 待核验 |
| APACHE IVa score | apachePatientResult.apachescore, apacheversion = IVa | negative sentinel becomes missing; native benchmark kept separate from HSC | 已核验; D038 |
| APACHE IVa hospital risk | apachePatientResult.predictedhospitalmortality, apacheversion = IVa | negative sentinel becomes missing; raw probability, clip only for logit | 已核验; D038 |

For the complete-tuple prediction HSC, every measurement must occur and be available from max(0,index−1,440 min) through tuple `prediction_time`. The separate strict-cohort selection core uses max(0,index−1,440 min) through index. `apacheApsVar` admission-day fields never back-fill either HSC because their exact pre-prediction availability is unknown. The APACHE IVa benchmark is a separate outcome-free artifact.

Primary medication-order offsets equal to zero are mapped to missing according to the official eICU `pivoted-med.sql` ETL warning. The raw-zero indicators and a zero-retained sensitivity are preserved. A qualifying medication order must be non-cancelled, non-PRN, dosage-present, parenteral, have a valid interval overlapping the HSC window, and be available by its end. The official phenylephrine HICL codes are 37028, 35517, 35587, and 2087; spelling variants remain auditable in the mapping-frequency output.

eICU PBW is `50 + 0.91 × (height_cm − 152.4)` for Male and `45.5 + 0.91 × (height_cm − 152.4)` for Female. VT/PBW and sMP/PBW remain secondary exposures and are missing when height or binary sex is invalid.

## 5. Model-ready encodings

| Variable | Encoding |
|---|---|
| age | continuous; eICU literal `> 89` = 90 with exclusion sensitivity; per 10 years for coefficient display; three-knot MIMIC spline in prediction models |
| sex | binary source category with explicit reference; unknown remains missing |
| P/F | continuous; per 50 mmHg display; three-knot MIMIC spline |
| GCS | lower is worse; continuous/spline after valid 3–15 reconstruction |
| MAP | mmHg; continuous/spline |
| vasopressor | any active qualifying drug before prediction, 0/1 |
| platelet | 10^9/L; continuous/spline after unit validation |
| creatinine | mg/dL; continuous/spline after unit validation |
| sMP | primary linear per 5 J/min; secondary four-knot spline |
| ΔP | linear per 5 cmH2O |
| RR | linear per 5/min |
| VT/PBW | linear per 1 mL/kg |
| PEEP | linear per 5 cmH2O |
| resistive pressure | linear per 5 cmH2O |

Numeric spline knots and MIMIC sMP mean/SD are generated from the MIMIC development data, frozen in a versioned parameter artifact before eICU outcome evaluation, and then copied unchanged to external validation.

The primary S0/S2/S3 models retain valid GCS. The D057 reduced-core sensitivity deliberately omits GCS but otherwise retains age, sex, P/F, MAP, vasopressor, platelet, and creatinine: R2 adds delta pressure and RR, whereas R3 adds sMP. R2 and R3 use one identical complete set and the same MIMIC-frozen transformations; they are not substitutes for the primary models.

The D055 tuple-observation model is separate from every outcome model. Its fixed raw index-known inputs are age, sex, P/F, index PEEP, index FiO2, index time, GCS, MAP, platelet, creatinine, and vasopressor. Model scales are age/10, P/F/50, PEEP/5, FiO2/10, index time/24, MAP/10, and platelets/100, with all other terms on their listed scales. It uses database-specific median imputation plus missingness indicators, a linear logit with no interactions, stabilized observed-tuple weights, and observed-record 1st/99th percentile truncation. eICU `hospitalid` is retained to identify hospitals with structural zero tuple support and the separately named supported-hospital target.

The D060 association-imputation frame is the primary tuple cohort with known in-hospital outcome. Its only imputed columns are `gcs`, `map`, `platelet`, and `creatinine`; all four use predictive mean matching with five donors. `age`, `sex_female`, `pf_ratio`, `vasopressor`, `delta_p`, `rr`, `smp`, `peep`, `resistive_pressure`, and the outcome are predictors with blank imputation methods. `peep` and `resistive_pressure` are complete tuple auxiliaries for imputation only and do not enter the S2/S3/S2M outcome designs; PBW-derived fields are not imputation auxiliaries because PBW is incomplete. The outcome and exposure tuple are never imputed. Frozen D054 transformations are applied after completion. MNAR delta scenarios alter imputed cells only: GCS, MAP, and platelet shift downward and creatinine upward by 0.5 or 1.0 frozen MIMIC SD, with bounds 3–15, 1–250 mmHg, >0–9999 K/µL, and 0.1–28.28 mg/dL.

The D061 eICU center frame is the primary common S3 set with `hospital_id` retained privately. The frozen MIMIC S0 linear predictor is the scalar adjustment used only in the two-stage fallback. Hospital identifiers and hospital-specific coefficients must not enter public CSV files. Leave-one-hospital-out rows remain private; public output contains only distribution/range summaries of the influence measures.

The D062 native-benchmark fields are:

| Variable | Encoding/use |
|---|---|
| OASIS native risk | `plogis(-6.1746 + 0.1275*oasis)` from the pinned official score formula; modeled only when MIMIC `index_time >= intime + 24 h` |
| OASIS all-components sensitivity | same timing-compatible population plus `component_available_n = 10` |
| APACHE IVa native risk | valid `apache_predicted_hospital_risk` in [0,1]; modeled only when eICU `index_time >= 1440` min |
| native logit | probability clipped to [1e−6, 1−1e−6] only for logit calculation |
| native+sMP extension | local logistic updating model containing native logit and linear `smp/5`; never called external validation |

## 6. Required QC outputs

Before outcome modeling, each database must produce:

- sequential flow counts and reasons for exclusion;
- pairing-gap distributions for PaO2–FiO2, PaO2–PEEP, and every tuple component;
- source/fallback frequencies;
- unit conversion and invalid-range counts;
- Ppeak/Pplat/PEEP ordering failures;
- complete-tuple count per patient and prediction-time distribution;
- severity-component availability before prediction;
- exact label-frequency tables for every 待核验 mapping;
- duplicate-key and repeat-stay audit;
- reconciliation against the legacy cohort without mortality stratification;
- a signed list of mappings promoted from 待核验 to 已核验 in the decision log.

Mortality, effect estimates, discrimination, calibration, and outcome-stratified summaries are prohibited during this mapping-QC step.
