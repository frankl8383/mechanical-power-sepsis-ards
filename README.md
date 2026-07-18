# Surrogate mechanical power in acute hypoxemic respiratory failure

Code and aggregate results for a fixed 6-hour landmark analysis using
MIMIC-IV 3.1 and eICU-CRD 2.0.

- `code/R/rebuild_v2`: analysis
- `results_aggregate`: disclosure-safe results
- `docs/rebuild_v2`: analysis plan and decision log

Set `ARDS_MP_PROJECT_ROOT`, `MIMIC_IV_DIR`, and `EICU_CRD_DIR`, then start with
`Rscript code/R/rebuild_v2/run_phase1_v2.R`. Source data are available to
credentialed users through [PhysioNet](https://physionet.org/) and are not
redistributed here.

MIT License.
