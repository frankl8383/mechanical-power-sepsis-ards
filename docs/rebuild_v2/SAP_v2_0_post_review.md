# ARDS mechanical-power rebuild v2: post-review statistical analysis plan

**Version:** 2.0.0  
**Freeze date:** 2026-07-16 (Asia/Shanghai)  
**Status:** hypothesis-driven amendment after presubmission methodological review  
**Machine-readable lock:** `code/R/rebuild_v2/00_config.R`  
**Protected provenance:** all `rebuild_v1` scripts and outputs remain unchanged

## 1. Governance and disclosure

This plan was written after the authors had seen the rebuild-v1 results and
received a structured presubmission methodological review. It is not a
prospective preregistration. The purpose of the freeze is to distinguish
scientifically motivated, review-driven analyses from later result-driven
exploration.

Information already known at freeze included the rebuild-v1
infection-supported cohort sizes, event counts, model-performance summaries,
missingness patterns, eICU center concentration, and methodological concerns
about construct validity, timing, selection, and between-center transport.
No rebuild-v2 broad-AHRF model result will be used to choose the target
population, predictor representation, functional form, missing-data hierarchy,
or reporting tier.

The governance order is:

1. `code/R/rebuild_v2/00_config.R`;
2. this SAP;
3. the rebuild-v1 data dictionary for unchanged source mappings;
4. the rebuild-v2 decision log for clarifications and deviations.

Any change after a rebuild-v2 outcome table has been inspected must retain the
frozen result, receive a new decision-log entry, and be labeled exploratory
unless it corrects a reproducible implementation error.

## 2. Scientific objective

The primary question is whether formula-based surrogate mechanical power (sMP)
contains prognostic information beyond simpler ventilatory representations when
all comparisons use the same patients, baseline information, prediction time,
and validation procedure.

The analysis specifically asks:

1. Does sMP outperform the one-degree-of-freedom index `4 × driving pressure +
   respiratory rate` (4DPRR)?
2. Does releasing the fixed 4:1 weighting of driving pressure and respiratory
   rate improve prediction?
3. Does releasing the equal prognostic weighting implicitly imposed on the
   static-elastic, dynamic-elastic, and resistive algebraic terms of the sMP
   equation improve prediction and external transport?

These are prognostic and information-compression questions. No coefficient is
interpreted as the causal effect of changing ventilator settings.

## 3. Data sources and roles

- **MIMIC-IV v3.1:** development, transformation and scale derivation, model
  fitting, and internal validation.
- **eICU-CRD v2.0:** unchanged-parameter external application and center-level
  robustness analyses.

The source mappings already audited in rebuild v1 will be reused unless the
broader target population exposes a documented source-coverage defect. Any
mapping change must be outcome-independent and logged before the affected model
is run.

## 4. Target population and index

### 4.1 Primary population

The primary population is adults with oxygenation-defined acute hypoxemic
respiratory failure during invasive ventilation, without requiring suspected
infection:

1. age at least 18 years;
2. valid arterial PaO2 in the ICU;
3. valid FiO2 paired within ±120 minutes;
4. PaO2/FiO2 no greater than 300 mmHg;
5. PEEP at least 5 cmH2O paired within ±120 minutes;
6. explicit active or proximal invasive-ventilation evidence under the locked
   source-specific rebuild-v1 rules;
7. no proximal explicit NIV conflict;
8. first qualifying event in a stay and first qualifying stay per patient.

This is not an imaging-adjudicated Berlin ARDS cohort. The main label is
“oxygenation-defined acute hypoxemic respiratory failure” (AHRF).

### 4.2 Index time

Index is the earliest PaO2 event satisfying the complete respiratory phenotype.
The source-specific deterministic first-stay ordering from rebuild v1 is
retained.

### 4.3 Infection-supported sensitivity

The locked rebuild-v1 suspected-infection requirement is applied only as a
clinical-context sensitivity. It does not define the primary external
validation population because its ascertainment is not harmonized across
databases.

## 5. Fixed prediction time and follow-up

### 5.1 Six-hour landmark

The prediction time is fixed at index plus 6 hours. Only patients alive and not
discharged from the hospital at this landmark can enter outcome models.
Predictors must be documented no later than the landmark.

The primary outcome is in-hospital mortality occurring after the landmark.
Discharge alive after the landmark is a non-event.

### 5.2 Required flow audit

Each database must separately report:

- respiratory-eligible patients;
- deaths before 6 hours;
- live discharges before 6 hours;
- patients still hospitalized at 6 hours;
- patients with no valid tuple in 0–6 hours;
- patients with a tuple but incomplete no-GCS core;
- patients in the no-GCS primary common set;
- patients in the complete-GCS sensitivity;
- event counts and hospital support at each modeled stage.

This landmark analysis estimates risk among patients who survive and remain
hospitalized through 6 hours. It is not described as prognosis from the initial
hypoxemia timestamp.

## 6. Ventilator exposure construction

### 6.1 Primary tuple

The primary exposure is the first physiologically valid, plateau-anchored
complete tuple recorded from index through index plus 6 hours. Pairing and
source hierarchy are unchanged from rebuild v1:

- explicit plateau and peak airway pressures;
- PEEP;
- observed/exhaled tidal volume preferred over set tidal volume;
- total measured respiratory rate preferred over general measured or set rate;
- highest available source tier before temporal proximity;
- ±60-minute primary component window;
- ±30-minute sensitivity window.

Required ordering is `Ppeak ≥ Pplat ≥ PEEP`. The locked physiologic bounds in
`00_config.R` apply.

### 6.2 Derived representations

Let tidal volume be in liters:

```text
driving pressure = Pplat − PEEP
resistive pressure = Ppeak − Pplat

sMP = 0.098 × RR × VT ×
      [Ppeak − 0.5 × (Pplat − PEEP)]

4DPRR = 4 × driving pressure + RR

static-elastic algebraic term  = 0.098 × RR × VT × PEEP
dynamic-elastic algebraic term = 0.098 × RR × VT × 0.5 × driving pressure
resistive algebraic term       = 0.098 × RR × VT × resistive pressure
```

The implementation must verify for every complete tuple that:

```text
sMP = static-elastic term + dynamic-elastic term + resistive term
```

within a numerical tolerance of `1e-10`. This identity is an exact algebraic
decomposition of the airway-pressure surrogate equation. It is not a direct
partition of transpulmonary energy, lung-tissue energy absorption, or
dissipated energy.

### 6.3 Construct boundary

The exposure is called “plateau-based airway-pressure surrogate mechanical
power” at first mention and “sMP” thereafter. The equation was derived for
specific controlled-volume conditions; database labels do not verify a
standardized inspiratory hold, constant inspiratory flow, complete absence of
spontaneous effort, or transpulmonary energy delivery. The three formula terms
are therefore called algebraic terms of the surrogate, not measured tissue
energy components.

## 7. Measurement-quality tiers

The primary EHR-computable analysis includes all valid locked-source tuples.

The following prespecified restrictions are sensitivities:

1. **Preferred-source restriction:** the selected primary tuple uses
   observed/exhaled VT and total measured RR, without reselecting a later tuple.
2. **Rate-concordant restriction:** without reselecting the primary tuple,
   independently select the nearest valid set and total RR within ±15 minutes
   of its plateau anchor, require both measurements to fall within the frozen
   index-to-landmark exposure window and to have been available by the fixed
   landmark, require the selected records to be no more than 15 minutes apart,
   and require an absolute set-total difference no greater than 2 breaths/min.
   Equal-distance ties prefer a preceding record, then earlier measurement and
   availability times.
3. **MIMIC formula-compatible restriction:** a pre-outcome mode mapping supports
   a volume-targeted assist/control-compatible mode and the tuple is
   rate-concordant.

The rate-concordant subset is not called passive ventilation. eICU mode fields
are not used to create a parallel restriction unless source coverage becomes
adequate under an outcome-blind audit.

## 8. Baseline severity and analysis samples

### 8.1 Primary no-GCS harmonized core

The primary baseline model contains:

- age;
- sex;
- index PaO2/FiO2;
- mean arterial pressure;
- any vasopressor exposure;
- platelet count;
- serum creatinine.

MAP, vasopressor, platelets, and creatinine use the locked rebuild-v1
source-specific definitions and are summarized from index minus 24 hours
through the 6-hour landmark. No post-landmark value may enter a predictor.

Continuous baseline variables use three-knot restricted cubic splines with
numeric knots derived in MIMIC at its 10th, 50th, and 90th percentiles and then
frozen.

### 8.2 Missing-data hierarchy

1. **Primary:** complete common set for the no-GCS core and complete ventilator
   tuple.
2. **Key sensitivity:** complete harmonized core including GCS.
3. **Key sensitivity:** all-tuple analysis using MIMIC-frozen medians and
   missingness indicators, applied unchanged in eICU.
4. **Association sensitivity only:** database-specific multiple imputation.

External outcomes may not be used to build an eICU-specific imputation model
that is then described as locked external prediction.

### 8.3 Measurement and complete-case selection sensitivity

Selection is audited among all broad-AHRF patients alive and still hospitalized
at the 6-hour landmark, including those without a valid ventilator tuple.
Outcome-free no-GCS core extraction is therefore performed for the full
landmark-at-risk set, not only for modeled patients.

Two inclusion processes are reported separately:

1. availability of a valid plateau-anchored tuple by the landmark; and
2. entry into the primary common set, defined jointly by a valid tuple and
   complete no-GCS core.

The valid-tuple inclusion model uses information available by the landmark:
age, sex, index PaO2/FiO2, index PEEP, time from ICU admission to index,
no-GCS core values when observed, and prespecified missingness indicators.
For joint entry into the complete common set, this full specification is
retained as a selection diagnostic. Missingness indicators that define
complete-case exclusion are structurally non-reweightable because every
included complete case has those indicators equal to zero. The actual
joint-inclusion IPW model therefore uses only covariates observed throughout
the landmark risk set: age, recognized female sex plus an unknown-sex
indicator, index PaO2/FiO2, index PEEP, and time from ICU admission to index.
The full diagnostic and always-observed weighting model are reported
separately; neither is described as repairing nonpositivity from missing
predictor values.

Continuous terms use frozen functional forms rather than outcome-selected
transformations. The outcome-blind estimability rule is to use the prespecified
three-knot restricted cubic spline only when the requested quantile knots are
unique and at least five unique values are available; otherwise the variable
is entered as a median-centered, IQR-standardized linear term. Every fallback
is recorded in the transformation audit. Design columns are then considered
in their prespecified order and retained only if they increase the rank of a
matrix that already contains an intercept. This outcome-blind gate removes
only constant columns and exact linear dependencies, such as an all-zero
missingness indicator; every omitted column and reason is reported.
Stabilized inverse-probability weights are truncated at the prespecified 1st
and 99th percentiles. Report the raw and truncated weight distribution,
effective sample size, inclusion-model discrimination, ordinary covariate
balance before and after weighting, and structurally non-reweightable fields.

In eICU, hospitals with no valid tuple have structural nonpositivity and cannot
be represented by weighting observed tuples. They are counted and described,
but the weighted estimand is explicitly restricted to hospitals with at least
one eligible tuple and adequate overlap. Weighted results are a sensitivity
under the measured inclusion model, not proof that selection bias has been
eliminated.

## 9. Model hierarchy

All primary comparisons use an identical patient set within each database.

| Model | Baseline plus | Incremental df |
|---|---|---:|
| M0 | no ventilator term | 0 |
| M-MP | sMP | 1 |
| M-4DPRR | 4DPRR | 1 |
| M-DPRR | driving pressure and RR | 2 |
| M-ENERGY | static-elastic, dynamic-elastic, and resistive algebraic terms | 3 |

The primary ventilator terms are linear. This preserves interpretable nested
constraints and avoids data-driven flexibility.

### 9.1 Constraint tests

The first 1-df likelihood-ratio test compares M-4DPRR with M-DPRR and tests the
4:1 coefficient constraint on driving pressure and RR.

The second 2-df likelihood-ratio test compares M-MP with M-ENERGY and tests
equal coefficients for the three algebraic terms of the surrogate equation.

The tests address prognostic weighting, not the causal toxicity of a component.

### 9.2 Nonlinearity fairness sensitivity

A single prespecified sensitivity gives sMP and 4DPRR identical four-knot spline
flexibility and gives driving pressure and RR symmetric spline flexibility.
Function form is not selected using P values, plots, AIC, or eICU performance.
The energy constraint test remains based on the linear primary models.

## 10. Development and internal validation

All transformations, knots, scales, coefficients, and missing-data parameters
are derived in MIMIC and frozen before rebuild-v2 eICU performance is inspected.

Internal validation uses 1,000 patient-level bootstrap samples and replays the
entire development pipeline in every resample. Point estimates are corrected
for optimism using Harrell's procedure. For Brier score, log loss, and
C-statistic, confidence intervals use the Noma location-shifted bootstrap: the
percentile interval of bootstrap-sample apparent performance is shifted by mean
optimism. Raw optimism quantiles are not labeled as sampling confidence
intervals.

For an unpenalized logistic model, the apparent calibration slope is
structurally 1 in each training resample, so the location-shifted distribution
is degenerate. Its optimism-corrected point estimate is therefore accompanied
by a Noma two-stage percentile interval using 1,000 outer and 200 inner
resamples. If fewer than 95% of either the one-stage replicates or the eligible
outer replicates succeed, the affected estimate is flagged non-reportable; an
invalid or zero-width location-shifted slope interval will not be substituted.

## 11. External application and performance

The original MIMIC intercept, coefficients, transformations, and scales are
applied to eICU without refitting.

The primary model comparison is paired external Brier-score difference.
Also report:

- log loss;
- C-statistic;
- calibration intercept;
- calibration slope;
- observed/expected event ratio;
- bootstrap confidence intervals for each measure and paired difference.

Calibration plots must show the original predictions, a bootstrap confidence
band, and the distribution/support of predicted risks. Intercept-only and
intercept-plus-slope updating are explicitly labeled model updating.

No finding is described as equivalence or non-inferiority without a
scientifically justified margin and adequate precision.

## 12. Hospital structure and transport robustness

The primary eICU estimate is patient-weighted. Uncertainty is estimated using
2,000 hospital-cluster bootstrap samples.

Prespecified center analyses are:

1. exclusion of the largest contributing hospital;
2. equal-center summaries among hospitals with at least 10 modeled patients;
3. leave-one-hospital-out influence analysis without model refitting;
4. hospital support counts and event support by analysis tier.

For equal-center Brier score and log loss, calculate the metric within each
eligible center and average center-specific metrics equally. For O:E, aggregate
the equally weighted center-specific observed and expected proportions.
C-statistic is not emphasized in small-center equal-weight analyses.

If results depend materially on one hospital or fewer than 10 hospitals support
the restricted analysis, the manuscript will use “independent database
replication” rather than “multicenter transportability.”

## 13. Limited secondary analyses

### 13.1 Compliance-normalized sMP

```text
compliance = VT(L) / driving pressure
compliance-normalized sMP = sMP / compliance
```

This measure is compared with absolute sMP on the same patients. It is a single
physiology-motivated normalization sensitivity; no collection of additional
normalizations will be screened.

### 13.2 Persistent-AHRF-enriched sensitivity

A 72-hour landmark sensitivity may be run only if eICU retains at least 100
post-landmark events and at least 10 contributing hospitals. It requires
persistent oxygenation-defined respiratory failure and excludes deaths or
discharges before 72 hours. It is labeled “persistent-AHRF-enriched,” never
“confirmed ARDS.”

If either stopping rule fails, the analysis is descriptive or omitted.

## 14. Analyses not to be added

The primary paper will not add NRI/IDI, data-driven thresholds, unanchored
decision-curve analysis, black-box machine learning, outcome-selected
subgroups, multiple competing mechanical-power formulas, trajectory clustering,
or several arbitrary exposure windows.

## 15. Reporting hierarchy

Primary manuscript results:

1. broad-AHRF 6-hour landmark cohort;
2. M-MP versus M-4DPRR;
3. M-MP versus M-DPRR;
4. M-MP versus M-ENERGY;
5. original external calibration and paired Brier differences;
6. largest-center and cluster-bootstrap robustness.

Key manuscript sensitivities:

1. infection-supported cohort;
2. complete-GCS core;
3. preferred-source and rate-concordant subsets;
4. MIMIC formula-compatible subset;
5. compliance-normalized sMP.

All other analyses belong in the Supplement. A negative result remains
reportable and will not trigger replacement of the primary representation.

## 16. Interpretation rules

Permitted conclusions concern association, predictive information,
information compression, calibration, and transport.

The manuscript must not claim:

- that lowering sMP improves outcome;
- that any algebraic term is a measured tissue-energy component or a causal
  injury mechanism;
- that database plateau pressure proves a valid inspiratory hold;
- that the cohort is imaging-adjudicated ARDS;
- that sMP is a treatment target or clinical decision threshold;
- that similar point estimates establish equivalence.
