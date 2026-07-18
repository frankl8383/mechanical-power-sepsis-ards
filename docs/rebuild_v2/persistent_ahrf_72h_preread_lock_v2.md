# Rebuild v2 preread lock: 72-hour persistent-AHRF-enriched sensitivity

**Locked:** 2026-07-17 00:59:43 +0800 (Asia/Shanghai)  
**Status:** outcome-blind operational lock, written before inspecting any
post-72-hour outcome count or fitting any model in this sensitivity

## Evidence and governance boundary

This sensitivity implements SAP section 13.2 and decision V2-D008. Its
phenotype is informed by Marshall et al. (2026), who evaluated persistence of
P/F <300 mm Hg with contemporaneous PEEP >=5 cm H2O across 72 hours and used
the mean P/F value within each 24-hour window. It is an enrichment rule, not a
diagnosis of ARDS.

The Marshall exception that included patients meeting 48-hour persistence who
died on day 3 is not used. The SAP requires a separate index+72-hour landmark
and exclusion of every death or discharge on or before that landmark.

## Locked physiologic definition

Time zero is the already frozen rebuild-v2 broad-AHRF index event. The
physiologic screen uses all valid PaO2/FiO2 records in the same index ICU stay,
including records with P/F >=300, paired to PEEP >=5 cm H2O by the already
locked symmetric +/-120-minute PEEP-pairing rule.

Three non-overlapping windows are evaluated relative to time zero:

1. 0 to <24 hours;
2. 24 to <48 hours;
3. 48 to <=72 hours.

Each window must contain at least one valid P/F-plus-PEEP record. A patient is
classified as having persistent oxygenation-defined respiratory failure only
when the arithmetic mean P/F ratio is <300 mm Hg in every window. Normal or
high P/F observations paired to PEEP >=5 remain in the window mean; the
algorithm must not retain only threshold-positive records.

No mortality, discharge status, future outcome, model coefficient, or
performance metric may influence this phenotype.

## Locked landmark and analysis population

The prediction landmark is index+72 hours. Patients who die, leave hospital
alive, or have unknown hospital follow-up on or before the landmark are not in
the landmark risk set.

The analysis, if permitted, retains the already frozen first valid ventilator
tuple available by index+6 hours and the already frozen complete no-GCS core
measured by index+6 hours. No tuple is reselected at 72 hours and no 72-hour
severity core is derived. The sensitivity therefore asks whether the primary
early ventilatory-representation comparison is similar among patients who
survive in hospital to 72 hours with persistent oxygenation-defined
respiratory failure. It does not estimate the prognostic value of a newly
measured 72-hour ventilator state.

Follow-up for the binary endpoint begins strictly after the 72-hour landmark
and ends at hospital discharge.

## Prespecified feasibility gate

The outcome analysis is permitted only if the final eICU analysis-ready set
(persistent physiology, at risk in hospital at 72 hours, valid frozen 6-hour
tuple, and complete frozen no-GCS core) contains:

- at least 100 post-landmark in-hospital deaths; and
- at least 10 contributing hospitals.

If either condition fails, the outcome model is stopped. No shorter
persistence duration, lower event threshold, relaxed core definition,
alternative tuple window, or outcome-selected phenotype is substituted.

## Required audit trail

The implementation must publish:

- source and input SHA-256 hashes;
- an outcome-blind physiology artifact before hospital outcomes are read;
- window-specific measurement coverage and mean-P/F summaries;
- pre-landmark death, live-discharge, and unknown-follow-up counts;
- tuple and complete-core feasibility counts;
- event and hospital support at the final tier;
- an explicit `PROCEED` or `STOP` gate.

The sensitivity must always be labeled
`persistent-AHRF-enriched sensitivity`, never `confirmed ARDS`.
