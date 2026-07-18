# Secondary-sensitivity point-estimate interpretation

**Run:** 2026-07-17  
**Inference status:** real point estimates; zero bootstrap replicates; no
manuscript-ready confidence intervals

## Nonlinear fairness

All models used the 9,861-patient MIMIC common set and were applied unchanged
to 1,211 eICU patients across 34 hospitals. The MIMIC-derived spline knots were
frozen before external application.

In eICU, nonlinear sMP had a Brier score of 0.18410 and C-statistic of 0.70615.
Nonlinear 4DPRR had a Brier score of 0.18195 and C-statistic of 0.71426,
corresponding to paired differences versus nonlinear sMP of −0.00215 and
\+0.00811, respectively. Symmetric nonlinear driving pressure plus respiratory
rate produced a smaller Brier improvement (−0.00025) but a larger
C-statistic difference (+0.00996). Its external calibration slope was lower
(0.741 versus 0.806 for nonlinear sMP) and its O:E was higher (1.166 versus
1.119).

This pattern does not support a simple claim that additional flexibility is
uniformly better. Releasing the compressed representation can improve ranking,
but the gain may not transport as improved overall accuracy or calibration.
The linear algebraic-energy model remains an anchor only and was not expanded
nonlinearly.

## Compliance-normalized sMP

Four MIMIC rows with zero driving pressure were excluded because compliance is
undefined; no eICU row was excluded. Absolute and compliance-normalized sMP
were compared on the resulting same-patient sets.

In eICU, compliance-normalized sMP had a Brier score of 0.18216 and
C-statistic of 0.71446, versus 0.18374 and 0.70804 for absolute sMP. Paired
differences were −0.00158 for Brier score and +0.00642 for C-statistic.
Calibration slope was 0.779 for the normalized measure versus 0.810 for
absolute sMP.

The direction is scientifically interesting but the magnitude is small and
the calibration slope does not improve. This remains a single prespecified
physiology-normalization sensitivity, not a replacement primary exposure.

## Rate-concordant plus preferred-source restriction

The restriction retained 7,836 MIMIC patients and 749 eICU patients across 26
hospitals. Every retained row used the already selected primary tuple,
observed/exhaled tidal volume, total measured respiratory rate, and a
concordant independently selected set-total rate pair. No tuple was reselected.

In eICU, 4DPRR improved Brier score by 0.00266 and C-statistic by 0.00895
relative to sMP. Free driving-pressure plus respiratory-rate weights improved
C-statistic by 0.01268 but had a slightly worse Brier score (+0.00055) and
substantially higher O:E (1.240 versus 1.115 for sMP). The linear
algebraic-energy representation differed little from sMP.

The high-quality restriction therefore preserves the main result: simpler
pressure-rate compression can transport at least as well as sMP in point
estimates, whereas additional free parameters chiefly improve ranking and can
worsen calibration.

## Measured-selection weighting

Only the frozen joint always-observed weights were accepted. The MIMIC and
eICU effective sample sizes were 9,522.8 and 1,143.6. Every row-level table was
explicitly marked `permitted_for_outcome_weighting=TRUE`; the diagnostic
median/missingness-indicator model was blocked.

In weighted eICU evaluation, 4DPRR versus sMP had paired differences of
−0.00100 for Brier score and +0.00326 for C-statistic. Free driving pressure
plus respiratory rate had a slightly worse Brier score (+0.00059) and higher
C-statistic (+0.00519). The algebraic-energy differences were very small
(−0.00025 Brier; +0.00148 C-statistic).

The weighted sensitivity does not materially reverse the unweighted
representation comparison. It addresses only measured selection within
supported hospitals and does not repair structural nonpositivity or missing
severity information.

## Infection construct stop

MIMIC antibiotic/culture suspected infection and eICU
diagnosis/admission-diagnosis-supported infection share an index-relative
window but are not the same clinical construct. A unified
infection-restricted external validation was therefore not run. Descriptive
same-index overlap was 6,554/9,861 in MIMIC and 611/1,211 in eICU; these counts
must not be presented as a harmonized validation cohort.

## Provisional scientific synthesis

Across the added sensitivities, the consistent signal is not that a more
complex representation clearly supersedes sMP. Rather, releasing nonlinear or
component constraints can improve discrimination, while gains in external
Brier score are small and calibration can deteriorate. Among the tested point
estimates, 4DPRR is the most consistently transportable simple comparator.
Compliance normalization is potentially informative but remains secondary.

These conclusions are provisional until the locked resampling uncertainty is
available. No point-estimate difference should be described as statistically
established, equivalent, or clinically important.
