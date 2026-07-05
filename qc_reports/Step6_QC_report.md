# Step 6 QC report — Manuscript assembly

**Project:** MIMIC-IV sepsis-associated ARDS, ventilatory mechanics → mortality
**Date:** 2026-07-04 | **Environment:** conda `icu-vent` (R 4.5.3) + python
**Scope:** Assembly of a complete, submission-ready English manuscript from
previously QC'd figures, tables and statistical results. **Verdict: PASS.**

## 1. Deliverables produced

| Component | File | Format |
|---|---|---|
| Figure 1 (STROBE) | fig1_strobe | PDF + PNG (300 dpi) |
| Figure 2 (primary results) | fig2_main | PDF + PNG (4524×3310 px, 300 dpi) |
| Figure 3 (external validation) | fig3_eicu | PDF + PNG (300 dpi) |
| Figure legends | figure_legends.md | Markdown |
| Tables 1–3 | tables_1-3.docx / .csv | Three-line docx + CSV |
| Table notes | table_notes.md | Markdown |
| Full manuscript | manuscript.md / manuscript.docx | Markdown + Word |
| Structured abstract + keywords | abstract.md | Markdown |
| References (14) | references.csv / references.bib | CSV + BibTeX |
| Literature search log | lit_search_log.md | Markdown |
| Cover letter | cover_letter.md | Markdown |
| Submission checklist (TRIPOD) | submission_checklist.md | Markdown |

## 2. Numerical cross-check (manuscript vs source artifacts)

All 25 key quantities in the manuscript were programmatically verified against
the source artifacts. All matched:

- Cohort: development N = 23,807 (deaths 4,894, 20.6%); complete-case modelling
  N = 19,394 (deaths 4,032, 20.8%).
- Primary model: MP OR 1.42 (1.37–1.47); ΔP OR 1.13; age OR 1.40; P/F OR 0.75.
- Discrimination: base C 0.639 → +MP 0.662 (ΔC 0.023); internal optimism-
  corrected C 0.664; calibration slope 0.997; Brier 0.154.
- External (eICU): N = 1,837 (610 in-hospital deaths, 33.2%); external C 0.656
  (0.630–0.683); CITL 0.539; slope 0.865; Brier 0.216 → 0.206 recalibrated.

The value "1,848" appears only in the Figure 3a legend, correctly labelled as the
exposure-complete distribution set (distinct from the N = 1,837 analysis set);
this is a deliberate dual-caliber distinction, not an inconsistency.

## 3. Citation integrity

- 14 references, every one located and verified via PubMed E-utilities
  (esearch → esummary → efetch); all PMIDs and DOIs confirmed against retrieved
  records. **No fabricated citations.**
- Three incorrect records were caught and rejected during verification (ARDSNet
  letters-to-editor vs the trial; MIMIC-IV author corrections vs the descriptor;
  TRIPOD companion vs the statement) — documented in lit_search_log.md.
- In-text citations numbered 1–14 in order of first appearance; all 14 map to a
  reference-list entry with no gaps.

## 4. Corrections applied during assembly

- S4 slope cohort restriction corrected from "≥ 3" to "≥ 2 observation days" to
  match the Step 5 QC source (Auditor finding), in **both** the Methods prose and
  the Table 3A footnote (the footnote is assembled from a separate table-notes
  source; an initial fix touched only Methods and was completed after a
  follow-up Auditor finding, so the two now agree).
- Reference issue-number float artifacts ("8.0" → "8") cleaned in the formatted
  reference list.

## 5. Honest-positioning audit

The manuscript consistently frames the finding as a moderate-discrimination
(C ≈ 0.66), transportable prognostic **signal** — not a high-precision
prediction tool and not causal. Abstract, Results and Discussion are mutually
consistent on this point. The six limitations (outcome-definition difference,
complete-case/plateau-pressure sparsity, retrospective confounding, diagnosis-
code sepsis approximation in eICU, US-only data, non-robust male-sex and
low-ΔP features) are disclosed.

## 6. Items left for author input

Author names/affiliations/ORCID, contributions, funding, COI disclosures,
PhysioNet credential references, and final data/code availability link — listed
in submission_checklist.md. These require author-level information and are not
inferable from the analysis.
