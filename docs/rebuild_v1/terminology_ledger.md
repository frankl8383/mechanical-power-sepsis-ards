# Terminology ledger: ARDS mechanical-power rebuild v1

**Version:** 1.0.1  
**Freeze date:** 2026-07-15  
**Purpose:** keep the title, abstract, methods, results, tables, figures, supplement, cover letter, and response letters semantically consistent.

This ledger is mandatory for rebuild v1. It is designed to prevent a measurement definition from becoming a stronger disease, causal, or validation claim during writing.

## 1. Canonical terms

| Canonical term | Locked meaning | Appropriate use | Do not substitute |
|---|---|---|---|
| oxygenation-defined acute hypoxemia phenotype | Invasive ventilation plus paired P/F ≤300 and PEEP ≥5, with suspected infection; no harmonized imaging adjudication | Cohort, title, abstract, methods, limitations | Berlin ARDS, definite ARDS, adjudicated ARDS |
| ventilated adults with suspected infection and acute hypoxemia | Plain-language target-population description | Title, abstract, discussion | sepsis-associated ARDS patients without qualification |
| suspected infection | Source-specific infection evidence available by index in the locked index−48 h to index window | Methods and cohort description | confirmed infection, proven sepsis |
| retrospective infection-window sensitivity | Sensitivity allowing infection evidence through index+24 h | Phenotype sensitivity only | primary prediction cohort |
| source-specific infection ascertainment | MIMIC Seymour-style culture/antibiotic pairing versus eICU diagnosis-based evidence | Cross-database limitations | identical infection definition |
| index time | Earliest event satisfying all ventilation, P/F, PEEP, and infection rules | All temporal descriptions | day 1, baseline, ARDS onset unless those are literally true |
| exposure window | Index through index+6 hours for the primary analysis | Methods, figures | first ICU day |
| prediction time | Time when the selected complete tuple is fully available | Risk origin and predictor windows | index time when the two differ |
| complete ventilator tuple | Pplat, Ppeak, PEEP, VT, and total RR paired under the locked window | Exposure availability | routine ventilator data if Pplat is absent |
| surrogate mechanical power; sMP | Formula-based estimate from the complete tuple | Every exposure and result statement | exact mechanical power, measured mechanical power, energy delivered to lung tissue |
| absolute sMP | sMP in J/min | Primary exposure | normalized mechanical power |
| PBW-normalized sMP | sMP divided by predicted body weight | Key secondary analysis | primary sMP unless explicitly promoted by a future amendment |
| driving pressure; ΔP | Pplat − PEEP | Component model | transpulmonary driving pressure |
| resistive pressure | Ppeak − Pplat | Component model | airway resistance |
| primary outcome | In-hospital mortality after prediction time | Both databases | 28-day mortality |
| MIMIC 28-day mortality | Secondary database-specific outcome | MIMIC secondary results | externally validated outcome in eICU |
| development cohort | MIMIC-IV cohort used to fit and freeze the model | Modeling | training set if no random split is used |
| internal validation | Patient-level bootstrap assessment within MIMIC | Optimism correction | validation cohort |
| locked external validation | Unchanged MIMIC prediction equation applied to the same endpoint in eICU without outcome-informed revision | Only when all locked conditions are met | validation after eICU refitting or form selection |
| cross-database replication | Direction or association assessed in a second database when population, endpoint, predictor, or model is not identical enough for locked validation | Downgraded or secondary comparisons | external validation |
| transportability | Degree to which a locked prediction model retains performance in eICU | Performance plus calibration discussion | success based only on similar C statistics |
| original external performance | Performance before any eICU updating | Must be reported first | recalibrated performance |
| intercept-only recalibration | Updating only the model intercept in eICU | Model updating section | external validation result |
| intercept-and-slope updating | Updating intercept and global calibration slope | Model updating section | successful transport |
| conditional association | Association after adjustment for measured covariates | Coefficient interpretation | independent effect, causal effect |
| incremental predictive value | Better prespecified out-of-sample overall performance/calibration versus comparator | Only if externally supported | significant sMP coefficient |
| compact summary | sMP representation of multiple ventilator variables | Appropriate when S3 approximates S2/S4 | superior predictor |
| harmonized severity core | Common pre-prediction GCS, MAP, vasopressor, platelet, and creatinine block | S0 description | SOFA |
| native severity benchmark | Database-specific established score such as APACHE IVa or OASIS/SAPS II/APS III | Benchmark analysis | harmonized external model |
| timing-compatible native benchmark | Native first-day score evaluated only when its source window has closed before the hypoxemia index | Contextual OASIS/APACHE analysis | early-index native prediction |
| association-focused multiple imputation | Database-specific MI that includes outcome as a predictor and pools regression associations only | Missing-covariate sensitivity | MI external validation |
| local native-score extension | Outcome-fitted native-risk model with an added linear sMP term in the same database | Secondary severity-dependence analysis | validated OASIS/APACHE extension |
| measurement availability | Whether Pplat and the full tuple were observed | Flow and selection audit | random missingness |
| selection-weighted sensitivity | IPW analysis under a stated observation model | Sensitivity results | selection-corrected estimate |
| structural tuple-support failure | A hospital contributes strict-cohort patients but no observed complete tuple | eICU measurement-process limitation | patient-level weighting restored that hospital |
| supported-hospital sensitivity | Same observation model restricted to hospitals with at least one observed tuple; a different target population | Sensitivity results | correction of full-target positivity |
| reduced-core no-GCS sensitivity | R2/R3 comparison omitting GCS to test dependence on strict GCS availability | Missingness sensitivity with residual neurologic confounding | replacement primary model |
| primary-tuple preferred-source restriction | Subset whose already selected primary tuple uses preferred source tiers throughout; prediction time/HSC unchanged | Source-quality sensitivity | preferred tuple reselection |
| formal rebuilt-outcome unblinding | First row-level outcome-value read or newly derived event/outcome/model result after the authorization receipt | Governance provenance | header/checksum-only integrity check |
| common comparison set | Same patients used for a given model-performance contrast | Fair comparison | all available cases separately by model |
| center heterogeneity | Between-hospital variation in association or performance | eICU center analysis | transportability across new databases |
| leave-one-hospital-out influence | Change after omitting each eICU hospital | Influence analysis | internal–external validation |
| landmark analysis | Analysis beginning at a prespecified later risk time among those eligible then | 24-hour secondary analysis | primary analysis |

## 2. Claims permitted only under explicit evidence

| Proposed wording | Minimum evidence required | If requirement is unmet |
|---|---|---|
| “externally validated” | Same primary outcome, locked predictor mappings, model form, knots, scaling, imputation, coefficients, and intercept applied once in eICU | “evaluated in a second database” or “cross-database replication” |
| “transported well” | Acceptable original calibration, overall performance, and discrimination with uncertainty, not merely similar C statistics | State each performance dimension separately |
| “incremental predictive value” | Prespecified comparator, same analysis set, and stable external improvement in overall performance/calibration | “conditional association” or “no clear predictive increment” |
| “clinically useful” | Explicit decision, threshold range, intervention consequence, and meaningful net benefit | “prognostically informative” or omit |
| “robust” | Consistency across prespecified, scientifically relevant sensitivities with compatible uncertainty | Describe the exact sensitivities instead |
| “independent association” | Appropriate adjustment, temporal ordering, and acknowledgement of residual confounding | Prefer “conditional association” |
| “beyond severity” | Clearly named measured severity core and native-score benchmarks | “after adjustment for measured covariates” |
| “generalizable” | Multiple relevant settings and transparent population coverage | “observed in these two US critical-care databases” |
| “dose response” | Prespecified nonlinear analysis plus causal assumptions that are not available here | “graded association” |
| “optimal cutoff” | Independent prespecification and validation with decision context | No cutoff claim |
| “selection bias corrected” | Not permitted for the planned observational weighting | “selection-weighted under the specified observation model” |

## 3. Prohibited or misleading language

Use the replacement in the right column.

| Avoid | Reason | Required replacement |
|---|---|---|
| “ARDS cohort” without qualifier | Imaging criterion is unavailable | “oxygenation-defined acute hypoxemia cohort” |
| “Berlin-defined ARDS” | Bilateral opacities and edema origin are not adjudicated | State the measured oxygenation and PEEP criteria |
| “sepsis-associated ARDS” as a proven phenotype | Infection definitions differ and organ-dysfunction onset is not adjudicated identically | “suspected-infection-associated acute hypoxemia phenotype” |
| “day-1 MP” | Index may occur after ICU day 1 | “early index-relative sMP” |
| “mechanical power” alone at first mention | Formula is a surrogate | “surrogate mechanical power (sMP)” |
| “lung energy” or “energy delivered to lung tissue” | Airway-pressure surrogate is not transpulmonary energy | “ventilator-related mechanical load” |
| “not a proxy for severity” | Cannot be established by adjustment | Report the measured conditional association and increment |
| “independent predictor” | Blurs association and prediction | “sMP retained a conditional association” or “improved locked prediction” |
| “validated APACHE extension” | Adding sMP to APACHE in eICU is local fitting | “local APACHE model extension” |
| “successful external validation” after recalibration | Updating changes the model | Separate original validation from updating |
| “similar performance” without uncertainty | May conceal imprecision | Report difference and 95% CI for named metric |
| “no difference” from P>0.05 | Absence of evidence is not equivalence | Report estimate, CI, and prespecified equivalence interpretation if any |
| “MAR because groups were similar” | MAR is not diagnosed from observed balance | State the imputation assumption |
| “MNAR” as an observed fact | MNAR is an unverifiable mechanism | “MNAR sensitivity scenario” |
| “corrected for selection” | IPW depends on model and positivity assumptions | “selection-weighted sensitivity” |
| “internal–external validation” for leave-one-center-out influence | Withheld center is still from the same eICU source and workflow | “leave-one-hospital-out influence analysis” |
| “clinically meaningful” for a statistically significant coefficient | Clinical utility is not established | Give the absolute metric difference and uncertainty |
| “optimal 17/18 J/min threshold” | Data-driven threshold instability | Use continuous prespecified scales |
| “caused,” “effect,” “protective,” or “harmful” | Observational prognostic design | “associated with,” “higher/lower risk,” “prognostic” |

## 4. Result templates

### 4.1 Association

Preferred:

> Each 5 J/min higher early sMP was associated with an adjusted odds ratio of [estimate] for in-hospital mortality (95% CI [x–y]) in the prespecified complete-tuple population.

Avoid saying the same coefficient proves predictive improvement or a treatment effect.

### 4.2 Similar performance to simple components

> In locked external evaluation, S3 and S2 showed similar overall performance; the difference in [Brier/log loss/C statistic] was [estimate] (95% CI [x–y]). These findings support sMP as a compact summary of ventilatory load but not clear superiority over driving pressure and respiratory rate.

### 4.3 Development-only improvement

> The apparent increment in MIMIC-IV was not maintained when the locked model was applied to eICU-CRD.

### 4.4 Calibration mismatch

> Discrimination was broadly maintained, whereas absolute-risk calibration differed in eICU-CRD. Performance after recalibration is shown separately as model updating.

### 4.5 Null or attenuated strict-cohort result

> The association was attenuated under the time-aligned phenotype, indicating sensitivity to cohort and measurement definition.

### 4.6 Missingness and weighting

> Results were similar under the prespecified complete-case, imputation, and selection-weighted assumptions.

Use only if the estimates and uncertainty actually support the statement; otherwise state how they differed.

## 5. Naming in tables and figures

- Figure and table labels use “MIMIC-IV development” and “eICU-CRD locked external evaluation.”
- “Original,” “intercept recalibration,” and “intercept+slope updating” appear as separate rows or panels.
- P/F is defined once as PaO2/FiO2.
- All sMP axes include J/min; normalized axes include J/min/kg PBW.
- Model labels always carry the same definitions: S0, S1, S2, S3, S4, S5, and S2M.
- “Missing” is not merged with “normal” in severity-domain displays.
- Sample denominators accompany every model comparison.

## 6. Title and conclusion boundary

A defensible title may use:

> Early surrogate mechanical power in ventilated adults with suspected infection and acute hypoxemia: development and locked external evaluation across MIMIC-IV and eICU-CRD

The conclusion must answer three separate questions:

1. Was sMP conditionally associated with mortality?
2. Did it improve prediction versus simple ventilator variables?
3. Did the locked model retain discrimination, calibration, and overall performance externally?

These questions must not be collapsed into a single statement that sMP was “validated.”
