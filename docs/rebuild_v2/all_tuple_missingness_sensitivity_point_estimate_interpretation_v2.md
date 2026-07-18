# All-tuple missing-predictor sensitivity: point-estimate interpretation

## Analysis contract

The sensitivity retained all patients with a valid ventilator tuple by the
fixed 6-hour landmark: 10,468 MIMIC-IV patients (2,662 deaths) and 1,459
eICU-CRD patients (399 deaths across 36 hospitals).

Only MAP, platelet count, and creatinine were missing in the MIMIC all-tuple
baseline core. Their type-2 medians were 59 mm Hg, 159 ×10³/µL, and
1.1 mg/dL. These values and three missingness indicators were frozen from
MIMIC before outcome access. The same indicators were included in all five
models, and all transforms and coefficients were applied unchanged to eICU.
No eICU outcome contributed to imputation.

This recovered 607 MIMIC and 248 eICU patients excluded from the primary
complete-case common sets.

## External point estimates

| Model | Brier | C-statistic | Calibration intercept | Calibration slope | O:E |
|---|---:|---:|---:|---:|---:|
| M0 | 0.17957 | 0.7010 | -0.086 | 0.849 | 1.035 |
| M_MP | 0.17788 | 0.7071 | -0.042 | 0.850 | 1.068 |
| M_4DPRR | 0.17738 | 0.7097 | -0.160 | 0.819 | 0.996 |
| M_DPRR | 0.17768 | 0.7134 | -0.038 | 0.781 | 1.125 |
| M_ENERGY | 0.17797 | 0.7069 | -0.092 | 0.823 | 1.048 |

Against M_MP on the same eICU patients:

| Candidate | ΔBrier | ΔC-statistic |
|---|---:|---:|
| M_4DPRR | -0.00050 | +0.00257 |
| M_DPRR | -0.00020 | +0.00626 |
| M_ENERGY | +0.00008 | -0.00022 |

## Scientific reading

The missing-predictor strategy does not overturn the principal representation
comparison. The one-degree-of-freedom 4DPRR model has a small external Brier
advantage over sMP. Free driving-pressure and respiratory-rate weights improve
rank discrimination more, but this is accompanied by a lower calibration
slope and greater underprediction of total deaths. The free algebraic-term
model is effectively indistinguishable from sMP in external Brier and
C-statistic.

These results strengthen the selection/missingness argument because they use
every tuple-positive patient rather than only the no-GCS complete common set.
They do not establish superiority or equivalence: all estimates are
unbootstrapped point estimates, and `manuscript_ci_ready` remains false.

Absolute performance should not be numerically contrasted with the primary
complete-case analysis as though the samples were identical. The defensible
comparison is the direction and ranking of representations within each frozen
same-patient analysis.

