# Day-1 mechanical power in sepsis-associated ARDS — analysis code

Analysis code and **aggregate** results for the study:

> *Day-1 mechanical power as an incremental, transportable prognostic signal
> beyond illness severity in ventilated sepsis-associated ARDS: development and
> external validation across two intensive care databases.*

The study asks whether day-1 mechanical power carries prognostic information
**incremental to** established illness-severity scores (non-respiratory SOFA;
APACHE IVa), and whether that increment **transports** from a development
database (MIMIC-IV) to an independent multi-center database (eICU-CRD). The
cohort is an **oxygenation-defined, ventilated ARDS phenotype**, not
imaging-adjudicated Berlin ARDS.

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
| `R/` | Analysis scripts (external validation, SOFA construction, incremental-value models, and the supplementary robustness analyses below). |
| `results_aggregate/` | Aggregate result objects and result tables (no patient-level rows). |
| `results_aggregate/tables/` | Machine-readable versions of the manuscript tables, cross-center results, and the supplementary-analysis outputs. |
| `qc_reports/` | Quality-control reports for each analysis stage. |

### Analysis scripts (`R/`)

| Script | Analysis |
|--------|----------|
| `08_eicu_external_validation_v1_0.R` | External validation of the frozen model in eICU-CRD (discrimination, calibration, robustness). |
| `09_pertimepoint_mp_and_bodysize.R` | **Supplementary Analysis A** — per-timepoint vs component-median mechanical-power agreement (Bland-Altman); body-size (VT/PBW) normalization; sex-stratified MP association. |
| `10_selection_bias.R` | **Supplementary Analysis B** — complete-case selection: standardized mean differences, inverse-probability-of-selection weighting, and a missing-not-at-random (MNAR) tipping-point. |
| `11_decision_curve_endpoint_bridge.R` | **Supplementary Analysis C** — decision-curve / net-benefit analysis; endpoint bridge (frozen model applied to 28-day and in-hospital mortality within MIMIC-IV). |

The simplified volume-controlled mechanical-power equation used throughout is
`MP = 0.098 · RR · (VT/1000) · (Ppeak − 0.5·ΔP)`, with `ΔP = Pplat − PEEP`
(Chiumello et al., *Crit Care* 2020; PMID 32653011). The first pressure term is
**peak** inspiratory pressure, not plateau.

### Result tables (`results_aggregate/tables/`)

Primary/validation tables (`table1_baseline`, `table2_primary_model`,
`table3a_internal_robustness`, `table3b_external_validation`,
`table_sofa_incremental`, `table_external_increment`) and the multi-center
transportability tables (`center_strata_counts`, `region_transport`, `loro_cv`,
`size_teaching_transport`, `eicu_mi_sensitivity`, `eicu_citl_decomp`).

Supplementary-analysis outputs (this revision):
`mp_aggregation_agreement`, `mp_rowlevel_increment`, `mp_pbw_normalization`,
`mp_sex_stratified` (Analysis A); `completecase_vs_incomplete_baseline`,
`ipw_sensitivity`, `mnar_tipping` (Analysis B); `decision_curve`,
`endpoint_bridge` (Analysis C).

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
