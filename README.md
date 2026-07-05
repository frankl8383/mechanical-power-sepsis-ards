# Day-1 mechanical power in sepsis-associated ARDS — analysis code

Analysis code and **aggregate** results for the study:

> *Day-1 mechanical power as a severity-independent, transportable prognostic
> signal in sepsis-associated ARDS: development and external validation across
> two intensive care databases.*

## ⚠️ Data availability and PhysioNet Data Use Agreement

This repository contains **analysis code and aggregate outputs only**. It contains
**no row-level (individual patient) data**.

The two databases analysed —
[MIMIC-IV v3.1](https://physionet.org/content/mimiciv/3.1/) and the
[eICU Collaborative Research Database v2.0](https://physionet.org/content/eicu-crd/2.0/) —
are distributed by PhysioNet under a Data Use Agreement to **credentialed** users
who have completed the required human-subjects research (CITI) training. Under that
agreement, individual-level data **may not be redistributed**. To reproduce the
analysis you must obtain the databases directly from PhysioNet under your own
credentialed access.

## Repository structure

| Path | Contents |
|------|----------|
| `R/` | Analysis scripts (external validation, SOFA construction, incremental-value models). |
| `results_aggregate/` | Aggregate result objects and result tables (no patient-level rows). |
| `results_aggregate/tables/` | Machine-readable versions of the manuscript tables and cross-centre results. |
| `qc_reports/` | Quality-control reports for each analysis stage. |

## Software environment

- R 4.5.3
- data.table 1.17.8, survival 3.8.6, rms 8.1.1, pROC 1.19.0.1, mice 3.19.0

## Reproducing the analysis

1. Obtain MIMIC-IV v3.1 and eICU-CRD v2.0 from PhysioNet (credentialed access).
2. Point the data-path variables at the top of the scripts in `R/` to your local
   copies of the databases.
3. Run the scripts in order; aggregate outputs are written to a local results
   directory.

## Citation

If you use this code, please cite the manuscript (details to be added on
publication) and the two source databases (Johnson et al., MIMIC-IV; Pollard
et al., eICU-CRD).

## License

Code is released under the MIT License (see `LICENSE`).
