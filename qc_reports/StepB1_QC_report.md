# Step B1 QC report — Severity-adjustment reanalysis and manuscript reframing

**Project:** MIMIC-IV sepsis-associated ARDS, ventilatory mechanics → mortality
**Environment:** conda `icu-vent` (R 4.5.3) + python | **Verdict: PASS**
**Scope:** Reviewer major issues B1 (severity adjustment), B4 (eICU selection
audit) and B5 (same-model outcome check), and the reframing of the manuscript
around the incremental-over-severity thesis.

## 1. Analyses performed

- **Day-1 SOFA built in both databases.** MIMIC-IV (N = 19,394) from raw
  labevents/chartevents/inputevents/outputevents; eICU-CRD (N = 7,959) from the
  APACHE physiology table + lab platelets. Non-respiratory SOFA (5 organ systems,
  excludes the PaO₂/FiO₂ term already in the model) was the pre-specified severity
  covariate; full SOFA retained for sensitivity.
- **Internal increment (MIMIC).** Nested logistic models; MP increment over
  base + SOFA + ΔP quantified by ΔC (DeLong), IDI, continuous NRI (bootstrap
  B = 1000) and likelihood-ratio test.
- **External transportability (eICU).** Same nested sequence; plus a benchmark of
  MP over the database-native APACHE IVa predicted mortality.
- **B4 selection audit.** Severity and mortality compared between stays with vs
  without recorded plateau pressure.
- **B5 same-model outcome check.** Frozen 28-day model applied to MIMIC
  in-hospital mortality.

## 2. Key results (verified against severity_results.rds)

| Quantity | Value |
|---|---|
| Internal nested C | 0.639 (base) → 0.707 (+SOFA) → 0.711 (+ΔP) → 0.721 (+MP) |
| MP OR per SD, before → after SOFA | 1.42 → 1.31 (95% CI 1.26–1.36) |
| MP increment over base+SOFA+ΔP | ΔC +0.010 (DeLong P < 0.001); IDI 0.010 (0.008–0.012); NRI 0.218; LR χ² = 193 |
| MP–SOFA correlation / max VIF | 0.21 / 1.22 |
| External MP OR over SOFA (eICU) | 1.20 (1.07–1.35); LR χ² = 10.3 |
| MP over APACHE IVa | OR 1.20 (P < 0.001); ΔC +0.004 (P = 0.20, NS) |
| B4: SOFA with vs without plateau | 5.7 vs 4.3 (APACHE 96 vs 80); mortality 33.9% vs 31.1% |
| B5: frozen model, 28-day vs in-hospital C | 0.665 vs 0.670 |

All 14 statistics above were programmatically confirmed present in the
manuscript and identical to the saved analysis object.

## 3. Numerical and consistency QC

- 14/14 key statistics cross-checked against severity_results.rds — all match.
- 15/15 document-consistency checks pass: citations 1–18 gap-free; cohort N's
  (23,807 / 19,394 / 1,837) consistent; Figures 4–5 and their legends present;
  limitations numbered First–Sixth with no duplicates.
- Three new references (Vincent 1996 SOFA, Zimmerman 2006 APACHE IV, Pencina 2008
  IDI/NRI) verified via PubMed esummary + efetch (PMID + DOI confirmed).

## 4. Integrity notes

- The simplified mechanical-power equation is attributed to its methodological
  source (Chiumello 2020); the manuscript states explicitly that equivalence to
  the geometric reference method was **not** independently verified in this
  cohort. No fabricated validation claim remains.
- The APACHE-IVa increment is reported honestly in both directions: MP is
  independently associated (significant OR and LR test) but its marginal ΔC over
  APACHE IVa is small and non-significant. The manuscript does not overstate.
- Measurement caveats of the severity adjustment (bilirubin ~50% available and
  scored 0 when missing; GCS depressed by sedation; eICU cardiovascular subscore
  from MAP alone) are disclosed in the Discussion.

## 5. Honest appraisal of the reframing

The reanalysis strengthened the paper's central claim from "MP is associated with
mortality" (established, single-cohort) to "MP carries prognostic information
incremental to organ-failure severity, reproducibly across two databases" — while
disclosing that the incremental discrimination is modest once a strong severity
score is in hand. This is a defensible, honestly-bounded contribution, and it
directly resolves reviewer issues B1, B4 and B5.
