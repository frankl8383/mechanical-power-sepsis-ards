# Fixed-6 h no-GCS severity core: data contract

## Role

These artifacts are outcome-blind predictor sources for the fixed
index-plus-6-hour landmark analysis. They are constructed for every person who
is at risk with known hospital follow-up at the landmark, before the separate
post-landmark mortality artifact is opened.

## Frozen paths

- MIMIC all-risk:
  `analysis_rebuild_v2/private/mimic/mimic_fixed6h_all_at_risk_no_gcs_core_v2.rds`
- MIMIC tuple subset:
  `analysis_rebuild_v2/private/mimic/mimic_fixed6h_tuple_no_gcs_core_v2.rds`
- eICU all-risk:
  `analysis_rebuild_v2/private/eicu/eicu_fixed6h_all_at_risk_no_gcs_core_v2.rds`
- eICU tuple subset:
  `analysis_rebuild_v2/private/eicu/eicu_fixed6h_tuple_no_gcs_core_v2.rds`
- Combined completion gate:
  `analysis_rebuild_v2/qc/no_gcs_core/phase2b_no_gcs_core_complete_v2.csv`

The tuple files are exact-ID/order subsets of the all-risk core. Ventilator
representations remain in the independent
`mimic_no_gcs_core_targets_v2.rds` and `eicu_no_gcs_core_targets_v2.rds`
objects and must be joined by the canonical stay ID under an exact-key gate.

## Canonical fields

Common modeling fields are `hospital_id`, `age`, `sex`, `sex_recognized`,
`sex_female`, `pf_ratio`, `index_peep`, `index_time`, `landmark_time`,
`tuple_time`, `map`, `vasopressor`, `platelet`, and `creatinine`.

- `map`: minimum valid MAP, mmHg.
- `vasopressor`: any active norepinephrine, epinephrine, vasopressin,
  dopamine, dobutamine, or phenylephrine record/order; binary.
- `platelet`: minimum platelet count, \(10^3/\mu L\).
- `creatinine`: maximum creatinine, mg/dL.
- `complete_no_gcs_core`: all seven baseline fields (age, recognized binary
  sex, index P/F ratio, MAP, vasopressor, platelet, creatinine) are known.
- `tuple_and_complete_no_gcs_core`: a valid ventilator tuple is observed by
  the landmark and the no-GCS core is complete.

An unknown sex remains in the all-risk denominator with `sex_female=0`, but
has `sex_recognized=FALSE`, `sex_missing=TRUE`, and cannot enter the complete
primary core.

## Timing and availability

The measurement window is
`max(ICU entry, index-24 h)` through the fixed index-plus-6-hour landmark.
MIMIC chart/laboratory records require both event time in this window and
`storetime` no later than the landmark. eICU nurse records use the later of
measurement and entry offsets; laboratory records use the later of result and
revision offsets. Medication orders must be available by the landmark and
overlap the window. Selected measurement and availability times are retained
for audit.

## Leakage and provenance gates

No all-risk or tuple core may contain a field matching
`mort|death|dead|expire|discharge|outcome|surviv|status`. Raw filters scan to
EOF, validate retained gzip CSVs with a strict parser, and record raw/cache
SHA256 hashes. Mapping logic is pinned to the audited rebuild_v1 MIMIC and
eICU scripts; rebuild_v1 files and outputs are never modified.
