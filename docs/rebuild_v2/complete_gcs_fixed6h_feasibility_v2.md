# Complete-GCS fixed-6-hour sensitivity: feasibility audit

## Conclusion

The sensitivity is reproducible from the raw MIMIC-IV and eICU-CRD sources,
but it cannot be run validly from the currently frozen v2 caches or by reusing
v1 selected GCS values. A new outcome-blind v2 GCS extraction and freeze are
required before any endpoint model.

The audit opened no outcome artifact and fitted no model.

## Why the old artifacts cannot be reused

The current v2 no-GCS caches contain no GCS rows. The retained v1 raw candidate
caches include appropriate measurement and availability timestamps, but cover
only part of the current fixed-6-hour tuple target:

| Database | Current v2 tuple target | Target IDs with v1 GCS candidates | Coverage |
|---|---:|---:|---:|
| MIMIC-IV | 10,468 | 6,919 | 66.1% |
| eICU-CRD | 1,459 | 697 | 47.8% |

Using that subset would recreate the v1 selection population and would not be
a complete-GCS sensitivity of the v2 cohort. V1 selected values and v1 window
definitions are therefore explicitly forbidden.

## Reproducible v2 extraction boundary

Both raw source files are present and retain the fields required to apply the
current v2 window:

- measurement window: max(ICU start, index − 24 hours) through the fixed
  index + 6-hour landmark;
- availability: the measurement must be documented no later than the
  landmark;
- score: integer total GCS from 3 to 15;
- selection: worst valid score within the window, with deterministic
  time/source tie-breaking.

For MIMIC-IV, the source supports strict same-charttime reconstruction from
eye, verbal, and motor components (itemids 220739, 223900, and 223901), with
airway-unscorable verbal text excluded. For eICU, valid explicit totals can be
prioritized, followed by same-offset eye/verbal/motor reconstruction.

This is a source-harmonized clinical construct, not an exactly identical
measurement process: eICU can contain an explicit total, whereas MIMIC uses
strict component reconstruction. That limitation must remain explicit if the
sensitivity is later run.

## Required next implementation

Before model fitting:

1. create v2 target-ID filters for the full 10,468 and 1,459 tuple-positive
   populations;
2. scan the original GCS source tables to EOF and freeze raw candidate caches
   with source hashes and filter manifests;
3. apply only the current v2 measurement and availability windows;
4. publish row-level GCS timing/source QC and a complete-GCS predictor freeze;
5. open outcomes only after the new freeze gate passes.

The machine-readable gate is
`analysis_rebuild_v2/qc/complete_gcs_feasibility/complete_gcs_fixed6h_feasibility_v2.csv`.
