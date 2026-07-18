# Complete-GCS sensitivity: frozen point-estimate interpretation

Status: **PASS under decision V2-D021**. This is a prespecified
complete-case sensitivity analysis with **zero bootstrap replicates**. All
results below are point estimates and are not manuscript-ready confidence
interval results.

## Locked analysis population and ascertainment

- The raw MIMIC-IV `chartevents` scan reached EOF after 432,997,491 rows and
  retained 1,454,928 target GCS candidate records. The raw eICU-CRD
  `nurseCharting` scan reached EOF after 151,604,232 rows and retained 587,158
  target candidate records. Both source hashes matched the official local
  manifests.
- MIMIC GCS was reconstructed only from strict eye, verbal, and motor
  components recorded at the exact same charttime. Airway-unscorable verbal
  records were excluded. eICU used a valid explicit total before considering
  same-offset reconstruction.
- A valid recorded GCS was selected for 3,564/10,468 MIMIC tuple-positive
  patients and 1,091/1,459 eICU tuple-positive patients. After requiring the
  already complete no-GCS core and every locked ventilator representation, the
  model sets contained 3,424 MIMIC patients (1,033 deaths) and 935 eICU
  patients (273 deaths) from 32 hospitals.
- The complete-GCS set retained 34.7% of the MIMIC primary common set and
  77.2% of the eICU primary common set. It is therefore a selected sensitivity
  population and cannot replace the primary no-GCS analysis.
- GCS ascertainment was not measurement-identical across databases. Median GCS
  was 14 in MIMIC and 8 in eICU. The frozen MIMIC type-2 GCS knots were 3, 14,
  and 15. This marked distributional difference is consistent with source,
  documentation, airway, sedation, and case-mix differences and must remain an
  explicit limitation.

## Five-model point estimates

| Database | Model | Brier score | C-statistic | Calibration slope |
|---|---|---:|---:|---:|
| MIMIC-IV apparent | GCS baseline | 0.17588 | 0.74431 | 1.000 |
| MIMIC-IV apparent | + sMP | 0.17297 | 0.75296 | 1.000 |
| MIMIC-IV apparent | + 4DPRR | 0.17349 | 0.75220 | 1.000 |
| MIMIC-IV apparent | + free DP and RR | 0.16793 | 0.76972 | 1.000 |
| MIMIC-IV apparent | + algebraic energy terms | 0.17264 | 0.75418 | 1.000 |
| eICU external | GCS baseline | 0.19042 | 0.68475 | 0.698 |
| eICU external | + sMP | 0.18945 | 0.68943 | 0.684 |
| eICU external | + 4DPRR | 0.18744 | 0.69702 | 0.705 |
| eICU external | + free DP and RR | 0.18668 | 0.70339 | 0.674 |
| eICU external | + algebraic energy terms | 0.18844 | 0.69215 | 0.688 |

Against the complete-GCS baseline in eICU, sMP changed Brier score by
-0.00097 and C-statistic by +0.00468. The corresponding point changes were
-0.00299 and +0.01227 for 4DPRR, and -0.00374 and +0.01864 for free DP and RR.

In the same eICU patients, compared directly with sMP:

- 4DPRR changed Brier score by -0.00202 and C-statistic by +0.00759.
- Free DP and RR changed Brier score by -0.00278 and C-statistic by +0.01395.
- The three free algebraic energy terms changed Brier score by -0.00101 and
  C-statistic by +0.00272.

## Scientific interpretation

The sensitivity does not suggest that omission of recorded neurologic
severity explains the main representation result. After adding GCS to the
baseline, sMP retained only a small external increment, whereas the simple
pressure-rate representations—particularly separately weighted driving
pressure and respiratory rate—had larger favorable point changes in both
overall accuracy and discrimination.

This pattern supports the manuscript's restrained mechanistic interpretation:
the formula-based sMP summary does not appear to contain uniquely transportable
prognostic information beyond its pressure-rate constituents. It does not
support a causal claim, a treatment threshold, or a formal claim that DP+RR is
statistically superior, because this sensitivity has no resampling uncertainty
and was conducted in a selected complete-case subgroup.

External calibration remained weak despite reasonable observed-to-expected
ratios: slopes ranged from 0.674 to 0.705. Adding GCS therefore did not repair
cross-database transportability. The large GCS distribution shift and
source-specific construction make it inappropriate to describe this as fully
harmonized neurologic adjustment.

## Recommended eventual reporting role

Use this as a compact Supplement sensitivity, not as a new primary analysis.
A defensible summary is:

> In a prespecified complete-case sensitivity that added recorded total GCS,
> sMP provided a small external increment over the severity baseline; within
> this selected subgroup, 4DPRR and separately weighted driving pressure and
> respiratory rate had larger favorable point changes. GCS ascertainment and
> distributions differed substantially between databases, and no confidence
> intervals were generated for this sensitivity.

Do not use “superior,” “equivalent,” “fully adjusted,” or “validated GCS
harmonization” for these results.

## Frozen evidence

- Predictor-freeze gate:
  `analysis_rebuild_v2/qc/complete_gcs_sensitivity/complete_gcs_predictor_freeze_complete_v2.csv`
- Endpoint gate:
  `analysis_rebuild_v2/qc/complete_gcs_sensitivity/complete_gcs_sensitivity_complete_v2.csv`
- Sample flow:
  `analysis_rebuild_v2/aggregate/complete_gcs_sensitivity/complete_gcs_endpoint_sample_qc_v2.csv`
- Performance:
  `analysis_rebuild_v2/aggregate/complete_gcs_sensitivity/complete_gcs_point_performance_v2.csv`
- Paired differences:
  `analysis_rebuild_v2/aggregate/complete_gcs_sensitivity/complete_gcs_paired_differences_v2.csv`
- Post-landmark-flow correction revalidation:
  `analysis_rebuild_v2/qc/complete_gcs_sensitivity/post_landmark_flow_rerun_revalidation_v2.csv`

After the fixed-landmark flow denominator was corrected, the MIMIC/eICU
target and outcome RDS hashes remained byte-identical to the scientific inputs
used here. The corrected flow counts therefore require no complete-GCS refit;
historical input manifests are retained unchanged as exact run provenance.
