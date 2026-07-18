# Surrogate mechanical power and pressure-rate models in acute hypoxemic respiratory failure

This repository contains the analysis code and disclosure-safe aggregate
results for a fixed-landmark comparison of surrogate mechanical power (sMP)
with simpler pressure-rate representations in adults with
oxygenation-defined acute hypoxemic respiratory failure.

The primary analysis uses MIMIC-IV 3.1 for development and internal validation
and eICU-CRD 2.0 for unchanged external application. The prediction time is
fixed at 6 hours after the respiratory index. All five models use the same
patients, clinical baseline, outcome, and validation procedure.

## Scientific scope

The study compares:

- a clinical baseline model;
- the clinical baseline plus sMP;
- the clinical baseline plus `4 × driving pressure + respiratory rate`;
- the clinical baseline plus separately estimated driving pressure and
  respiratory rate;
- the clinical baseline plus the three exact algebraic terms of the sMP
  equation.

The exposure is an airway-pressure surrogate. The databases do not establish
passive ventilation, constant inspiratory flow, a valid inspiratory hold,
transpulmonary energy, or imaging-adjudicated ARDS. The analysis is prognostic
and does not estimate the causal effect of changing ventilator settings.

## Repository contents

- `code/R/rebuild_v2/`: current fixed-landmark cohort, modeling, validation,
  selection, construct-quality, and missing-data analyses.
- `code/R/rebuild_v1/`: audited source-mapping code reused by the current
  pipeline.
- `docs/rebuild_v2/`: statistical analysis plan and decision log for the
  current analysis.
- `docs/rebuild_v1/`: source-mapping provenance and earlier analysis
  decisions required by reused code.
- `results_aggregate/tables/`: machine-readable Supplementary Tables S1-S19.
- `results_aggregate/full_precision/`: full-precision model parameters and
  disclosure-safe performance summaries.
- `results_aggregate/SHA256_MANIFEST.csv`: checksums for the public aggregate
  files.

No patient-level records, protected hospital identifiers, credentialed source
files, or bootstrap-replicate records are included.

## Data access

MIMIC-IV and eICU-CRD are available to credentialed users through
[PhysioNet](https://physionet.org/) under their respective data-use
agreements. This repository does not redistribute either database.

Set the following environment variables before running the code:

```bash
export ARDS_MP_PROJECT_ROOT="/path/to/this/repository"
export MIMIC_IV_DIR="/path/to/mimiciv/3.1"
export EICU_CRD_DIR="/path/to/eicu-crd/2.0"
```

`ARDS_MP_PROJECT_ROOT` defaults to the current working directory. The two data
directories are required and have no embedded local default.

## Software

The completed analysis used R 4.5.1 with data.table 1.17.4, splines 4.5.1,
and survival 3.8.3. Some secondary analyses also require `digest`, `lme4`,
`metafor`, and `mice`. Python 3 is used only for streaming source filters.

## Execution

The current pipeline is documented in
[`code/R/rebuild_v2/README.md`](code/R/rebuild_v2/README.md). The principal
stages are:

```bash
Rscript code/R/rebuild_v2/run_phase1_v2.R
Rscript code/R/rebuild_v2/02b_outcome_free_representation_audit.R
Rscript code/R/rebuild_v2/07_build_selection_weights.R
Rscript code/R/rebuild_v2/10_freeze_primary_model_frames.R
Rscript code/R/rebuild_v2/11_run_primary_models.R
Rscript code/R/rebuild_v2/12_build_primary_descriptives.R
```

Internal calibration-slope uncertainty is generated separately for each model,
as described in the pipeline README. The analysis also contains prespecified
secondary and sensitivity stages; they should not be substituted for the
primary sequence.

The public release makes local filesystem configuration portable and removes
internal administrative records that are unrelated to reproducibility. It
does not alter the cohort definitions, formulas, model specifications, seeds,
or reported estimates used for the manuscript.

## Verify aggregate results

From the repository root:

```bash
python3 scripts/verify_aggregate_manifest.py
```

The verifier checks the byte size and SHA-256 hash of every file listed in
`results_aggregate/SHA256_MANIFEST.csv`.

## License

Code is released under the MIT License. MIMIC-IV and eICU-CRD remain subject to
their own access terms.
