# rebuild_v2 cohort, tuple, and fixed-landmark pipeline

This directory contains the post-review, hypothesis-driven rebuild for the
oxygenation-defined acute hypoxemic respiratory failure study. It does not
overwrite `rebuild_v1`.

## Implemented Phase-1 chain

1. `01_build_mimic_index_cohort.R`
   - Builds the broad MIMIC-IV respiratory-eligible cohort before infection
     restriction.
   - Selects the first eligible event per ICU stay and first eligible stay per
     subject.
   - Retains the audited `rebuild_v1` antibiotic/culture cohort only as a
     clinical-context sensitivity.
2. `02_build_eicu_index_cohort.R`
   - Builds the broad eICU-CRD respiratory-eligible cohort before
     diagnosis-based infection filtering.
   - Uses the existing deterministic repeat-encounter ordering.
   - Saves diagnosis-restricted infection as a sensitivity.
3. `03_build_mimic_paired_exposure.R`
   - Builds the first valid plateau-anchored ventilator tuple within index
     through index+6 hours using a ±60-minute component-pairing window.
4. `04_build_eicu_paired_exposure.R`
   - Applies the same tuple rules in eICU-CRD.
5. `05_build_fixed_landmark_flow.R`
   - Applies the fixed index+6-hour hospital-risk landmark.
   - Separately audits early death, early live discharge, unknown early
     follow-up, ICU exit, and absence of a valid tuple despite ICU observation.
   - Recomputes sMP, 4DPRR, and exact static/dynamic/resistive power components
     and requires the algebraic identity to hold.
   - Saves outcome-free targets for all landmark-at-risk patients and a separate
     tuple-positive target. Outcomes remain in separate private artifacts.
6. `02b_outcome_free_representation_audit.R`
   - Audits the fixed-landmark sMP, `4DPRR`, driving-pressure/rate, and exact
     algebraic-term distributions without opening an outcome artifact.
   - Reports correlations, VIFs, condition numbers, numerical identities, and
     source/script hashes as disclosure-safe aggregate outputs.
7. `06a_filter_no_gcs_inputs_v2.py` and `06_build_no_gcs_severity_core.R`
   - Filter only the prespecified MAP, platelet, creatinine, and vasopressor
     source rows for all patients at risk at the fixed landmark.
   - Build an outcome-free no-GCS baseline core for the full selection
     denominator and freeze it behind a SHA256 completion gate.
8. `07_build_selection_weights.R`
   - Freezes selection models before any outcome artifact is opened.
   - Fits a full median/indicator model for valid-tuple availability.
   - Retains the full joint tuple-plus-complete-core model only as a selection
     diagnostic because core-missingness indicators are structurally
     non-reweightable among complete cases.
   - Fits the outcome-eligible joint IPW model using only variables observed in
     every landmark target: age, female/unknown-sex indicators, index P/F,
     index PEEP, and ICU-to-index time.
   - Excludes eICU hospitals with zero corresponding included cases from each
     weighted target estimand, reports the excluded hospital/person counts,
     truncates stabilized weights at the 1st/99th percentiles, and reports AUC,
     ESS, weight extrema, measured balance, and nonpositivity.
9. `08b_weighted_sensitivity_utils.R`
   - Joins weights to outcome-model frames only by exact ID after the selection
     gate exists.
   - Refuses any table marked diagnostic-only, preventing the structurally
     non-reweightable full joint model from being used as outcome weights.
10. `12_secondary_sensitivity_utils.R` and
    `13_run_secondary_sensitivities.R`
    - Run real point estimates, without bootstrap confidence intervals, for the
      limited secondary analyses frozen in SAP v2.0.0.
    - Apply the MIMIC-derived spline knots unchanged to eICU for the nonlinear
      sMP, nonlinear 4DPRR, and symmetric nonlinear DP-plus-RR comparison.
    - Retain `M_ENERGY` as a linear anchor; no unplanned nine-degree-of-freedom
      nonlinear algebraic-energy model is added.
    - Compare absolute sMP with the single prespecified
      compliance-normalized sMP measure on the same positive-driving-pressure
      patients, using a MIMIC-derived IQR scale in eICU.
    - Restrict to rate-concordant plus preferred-source rows by joining the
      frozen outcome-blind flags to the already selected primary tuple. The
      tuple is never reselected.
    - Stop a harmonized infection-restricted external validation because MIMIC
      uses antibiotic/culture suspected infection whereas eICU uses diagnosis
      text. Only source-specific descriptive coverage is reported; no common
      infection definition is invented.
    - Permit endpoint weighting only from the joint always-observed IPW model
      whose row table explicitly carries
      `permitted_for_outcome_weighting=TRUE`.
11. `16_build_persistent_ahrf_feasibility.R`
    - Replays the pinned pre-threshold P/F event layer so normal P/F records
      remain in three prespecified 24-hour means.
    - Publishes and hashes the outcome-blind persistent-AHRF phenotype before
      reading hospital outcomes.
    - Applies a separate index+72-hour landmark and reuses, without reselection,
      the frozen index+6-hour tuple and complete no-GCS core.
    - Enforces the eICU gate of at least 100 post-landmark deaths and 10
      contributing hospitals. The completed gate stopped at 70 deaths across
      26 hospitals, so no outcome model or bootstrap was run.
    - `16_persistent_ahrf_feasibility_selftest.R` verifies the STOP branch,
      hashes, invariants, and absence of analysis-frame artifacts.

Run the complete chain from the project root with:

```bash
Rscript code/R/rebuild_v2/run_phase1_v2.R
Rscript code/R/rebuild_v2/02b_outcome_free_representation_audit.R
Rscript code/R/rebuild_v2/07_build_selection_weights.R
Rscript code/R/rebuild_v2/12_secondary_sensitivity_utils_selftest.R
Rscript code/R/rebuild_v2/13_run_secondary_sensitivities.R
Rscript code/R/rebuild_v2/16_build_persistent_ahrf_feasibility.R
Rscript code/R/rebuild_v2/16_persistent_ahrf_feasibility_selftest.R
```

## Controlled reuse of audited v1 logic

`00_phase1_replay_utils.R` provides two controlled replay mechanisms.

- Phase-1 phenotype scripts are SHA256-pinned and evaluated in an isolated
  environment. MIMIC execution stops immediately after respiratory `stage5`,
  before antibiotic/culture infection ascertainment and before all formal save
  or QC code. eICU execution stops after respiratory `stage5` and before
  `stage6` infection filtering and formal save/QC code; eICU infection-source
  mapping is located earlier in the upstream script.
- Paired-exposure scripts are SHA256-pinned and replayed with a literal
  `_v1` to `_v2` artifact suffix transformation and a compatibility projection
  of `LOCKED_V2`. All roots point to `analysis_rebuild_v2`.

Every replay snapshots the `analysis_rebuild_v1` file tree before and after
execution and fails if a file size or modification time changes. The MIMIC
Phase-1 replay also hashes the two source caches before and after use. Exposure
QC records the upstream source SHA, replay-utility SHA, and wrapper SHA because
the inherited exposure completion gate hashes the v2 wrapper.

Pinned upstream sources at this implementation:

| Source | SHA256 |
|---|---|
| `rebuild_v1/01_build_mimic_index_cohort.R` | `1a7c8d8b191c0284dbf3e004e3772789d03b2df441681d8c94dc315aa70de6ba` |
| `rebuild_v1/02_build_eicu_index_cohort.R` | `7da33e3056157f2b564554c1f9074a40ba54861a9834379dcecf8635e0c2510c` |
| `rebuild_v1/03_build_mimic_paired_exposure.R` | `9b7b6acea6ca026eb1526a752fc7008e6b1012abdc9e07c38142a56c36e2ee2d` |
| `rebuild_v1/04_build_eicu_paired_exposure.R` | `a364d00ab715e99b4b40403a55651f1f2395cd1ba258e69fa000428d3762a8e0` |

Any upstream source edit therefore requires explicit review and a pinned-hash
update rather than silent execution.

## Landmark and no-GCS target semantics

The primary risk time is fixed at index+6 hours. Patients who die or leave the
hospital alive on or before the landmark are excluded from the post-landmark
outcome analysis and counted explicitly in aggregate flow QC.

The no-GCS baseline extraction window is:

- MIMIC-IV: `max(ICU intime, index−24 hours)` through `index+6 hours`.
- eICU-CRD: `max(ICU offset 0, index−1440 minutes)` through
  `index+360 minutes`.

The all-landmark-at-risk selection targets are:

- `private/mimic/mimic_all_landmark_at_risk_selection_targets_v2.rds`
- `private/eicu/eicu_all_landmark_at_risk_selection_targets_v2.rds`

They include identifiers, hospital where applicable, age, sex, index P/F,
index PEEP, ICU/index/landmark timing, tuple availability, valid-tuple count,
and an outcome-free tuple nonmeasurement reason. They contain no death,
discharge-status, survival, or outcome field. These are the denominators for
the planned joint-inclusion model covering both valid-tuple availability and
complete no-GCS core availability.

The tuple-positive no-GCS extraction targets are:

- `private/mimic/mimic_no_gcs_core_targets_v2.rds`
- `private/eicu/eicu_no_gcs_core_targets_v2.rds`

Post-landmark outcomes are stored separately:

- `private/mimic/mimic_fixed6h_landmark_outcomes_v2.rds`
- `private/eicu/eicu_fixed6h_landmark_outcomes_v2.rds`

## Privacy boundary

All patient-, stay-, or hospital-identifiable derived objects remain under
`analysis_rebuild_v2/private/` and are ignored by version control. Only
disclosure-safe aggregate counts, invariants, manifests, and checksums are
written under `analysis_rebuild_v2/qc/` or `aggregate/`.

## Completed run on 2026-07-17 (Asia/Shanghai)

| Stage | MIMIC-IV | eICU-CRD |
|---|---:|---:|
| Broad index cohort | 20,765 | 5,624 |
| At risk with known outcome at 6 h | 20,388 | 5,509 |
| Valid tuple by 6 h | 10,468 | 1,459 |
| Post-landmark deaths among tuple patients | 2,662 | 399 |

The eICU broad cohort spans 83 hospitals. The fixed-landmark tuple cohort spans
36 hospitals; its largest hospital contributes 672/1,459 patients (46.1%).
This supports external validation across more centers than the prior
infection-restricted cohort but still requires center-robust analysis and
restrained transportability language.

## Outcome-blind selection-weight freeze completed on 2026-07-17

The completion gate is
`analysis_rebuild_v2/qc/selection_weights/selection_weights_complete_v2.csv`.
It records `outcome_artifacts_opened=FALSE`, four endpoint-eligible models,
two diagnostic-only models, private-output hashes, and a passing aggregate
leakage guard.

| Endpoint-eligible sensitivity | Supported target | Included | Supported hospitals | Excluded hospitals / patients | AUC | Truncated ESS | Raw weight range | 1%/99% range | Max reweightable absolute SMD |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| MIMIC valid tuple | 20,388 | 10,468 | 1 | 0 / 0 | 0.662 | 9,816.5 | 0.526–2.250 | 0.578–1.734 | 0.010 |
| MIMIC joint common set | 20,388 | 9,861 | 1 | 0 / 0 | 0.617 | 9,522.8 | 0.512–1.890 | 0.600–1.332 | 0.008 |
| eICU valid tuple | 3,748 | 1,459 | 36 | 47 / 1,761 | 0.613 | 1,365.9 | 0.574–2.383 | 0.648–2.020 | 0.016 |
| eICU joint common set | 3,679 | 1,211 | 34 | 49 / 1,830 | 0.609 | 1,143.6 | 0.504–1.610 | 0.579–1.556 | 0.009 |

The full joint median/indicator diagnostic identified structural
non-reweightability, as expected: all three core-missingness indicators in
MIMIC and MAP/platelet/creatinine missingness plus the single unknown-sex row
in eICU were constant among complete included cases. These diagnostic weights
are marked `permitted_for_outcome_weighting=FALSE` and are rejected by the
weighted-sensitivity join helper.

## Secondary/sensitivity point-estimate run completed on 2026-07-17

The completion gate is
`analysis_rebuild_v2/qc/secondary_sensitivities/phase5_secondary_sensitivities_complete_v2.csv`.
It records zero bootstrap replicates and
`manuscript_ci_ready=FALSE`; these outputs are real point estimates but are not
final confidence intervals.

| Analysis | MIMIC-IV | eICU-CRD | eICU hospitals |
|---|---:|---:|---:|
| Frozen-knot nonlinear fairness | 9,861 | 1,211 | 34 |
| Compliance-normalized sMP | 9,857 | 1,211 | 34 |
| Rate-concordant plus preferred source | 7,836 | 749 | 26 |
| Joint always-observed selection IPW | 9,861 | 1,211 | 34 |

Four MIMIC rows with zero driving pressure are excluded only from the
compliance-normalized analysis because compliance is undefined at zero driving
pressure. No eICU row is lost for this reason.

The infection audit confirms that the common timing window does not make the
two infection constructs equivalent. Consequently, no infection-restricted
endpoint model or harmonized external validation was run in this stage.

## Primary model and reporting chain

The model-development and unchanged external-evaluation stages are separated
from the outcome-blind frame freeze.

1. `09_primary_model_utils.R`
   - Defines the five locked logistic models, design-matrix construction,
     MIMIC-derived transformations, prediction helpers, algebraic identities,
     and guards against outcome fields entering predictor objects.
2. `10_freeze_primary_model_frames.R`
   - Joins the fixed-landmark ventilator representations to the no-GCS core
     without opening an outcome artifact.
   - Freezes the complete same-patient common set in each database and derives
     all spline knots, scales, and factor encodings in MIMIC-IV only.
3. `11_run_primary_models.R`
   - Opens outcomes only after the frame and transformation gates pass.
   - Fits `M0`, `M_MP`, `M_4DPRR`, `M_DPRR`, and `M_ENERGY` in MIMIC-IV.
   - Runs the two locked likelihood-ratio tests, 1,000-repetition patient
     bootstrap validation, unchanged eICU application, 2,000-repetition
     hospital-cluster bootstrap, flexible calibration, largest-center
     exclusion, equal-hospital weighting, and leave-one-hospital-out analyses.
4. `12_build_primary_descriptives.R`
   - Produces the disclosure-safe same-patient descriptive source for Table 1
     without refitting a model or redefining the cohort.
5. `12_run_two_stage_calibration_slope.R`
   - Runs one model-specific, exactly resumable two-stage bootstrap for
     internal calibration-slope uncertainty.
   - The locked specification is 1,000 outer repetitions and 200 inner
     repetitions for each of the five models. Atomic per-repetition
     checkpoints permit interruption and exact continuation.

The completed primary-model gate is:

```text
analysis_rebuild_v2/qc/primary_models/phase4_primary_models_complete_v2.csv
```

It records successful completion of all 1,000 ordinary internal-validation
repetitions and all 2,000 hospital-cluster external-validation repetitions.
The primary gate remains explicitly not final for the internal
calibration-slope interval until all five two-stage jobs satisfy their
prespecified completion threshold.

Run the locked primary stages from the project root with:

```bash
Rscript code/R/rebuild_v2/09_primary_model_utils_selftest.R
Rscript code/R/rebuild_v2/10_freeze_primary_model_frames.R
Rscript code/R/rebuild_v2/11_run_primary_models.R
Rscript code/R/rebuild_v2/12_build_primary_descriptives.R
Rscript code/R/rebuild_v2/12_two_stage_resume_utils_selftest.R
Rscript code/R/rebuild_v2/12_run_two_stage_calibration_slope.R --model-id M0
Rscript code/R/rebuild_v2/12_run_two_stage_calibration_slope.R --model-id M_MP
Rscript code/R/rebuild_v2/12_run_two_stage_calibration_slope.R --model-id M_4DPRR
Rscript code/R/rebuild_v2/12_run_two_stage_calibration_slope.R --model-id M_DPRR
Rscript code/R/rebuild_v2/12_run_two_stage_calibration_slope.R --model-id M_ENERGY
```

## Construct-validity and missing-predictor sensitivities

1. `14_build_mimic_mode_quality_flags.R`
   - Scans the MIMIC ventilation-mode source without outcomes and annotates
     only the already selected tuple.
   - Uses a narrow conventional volume-control assist/control-compatible
     mapping and does not infer passive ventilation.
2. `15_run_mimic_mode_compatibility_sensitivity.R`
   - Combines the frozen mode flag with rate concordance and reports
     same-patient apparent point estimates only.
   - It does not reselect tuples, bootstrap the small subset, or claim external
     transportability.
3. `18_missingness_sensitivity_utils.R`,
   `19_freeze_all_tuple_missingness_frames.R`, and
   `20_run_all_tuple_missingness_sensitivity.R`
   - Implement the prespecified all-tuple sensitivity with medians and
     missingness indicators derived exclusively in MIMIC-IV.
   - Apply the frozen imputation rule, transformations, and design columns
     unchanged to eICU-CRD.
   - Stop if eICU contains a novel missingness pattern that the MIMIC-derived
     rule cannot represent; external outcomes never inform imputation.
   - Report point estimates only and do not replace the complete-case primary
     analysis.

The completed mode-compatible gate is:

```text
analysis_rebuild_v2/qc/mode_compatibility_sensitivity/mimic_mode_compatibility_sensitivity_complete_v2.csv
```

The common-set mode-plus-rate restriction retained 446 MIMIC-IV patients and
103 deaths. It is a narrow construct check, not validation of the physiological
assumptions of the surrogate equation.

## Conditional persistent-AHRF analysis

`16_build_persistent_ahrf_feasibility.R` reconstructs an outcome-blind
72-hour persistent oxygenation phenotype and then imposes a separate
index-plus-72-hour hospital-risk landmark. The original 6-hour ventilator tuple
is not reselected. Outcome support is inspected only after the phenotype and
flow artifacts have been frozen and hashed.

The endpoint analysis is permitted only if the final eICU common set retains at
least 100 post-landmark deaths from at least 10 hospitals. If either condition
fails, the script publishes the flow, hospital support, and locked stop reason
without relaxing the threshold or fitting an outcome model. This sensitivity
is termed persistent AHRF and is never relabeled as imaging-adjudicated ARDS.
