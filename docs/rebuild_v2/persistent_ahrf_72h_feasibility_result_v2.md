# Rebuild v2 result: conditional 72-hour persistent-AHRF sensitivity

## Decision

**STOP.** The prespecified eICU event gate failed. No outcome model, point
estimate, or bootstrap was run, and no threshold or phenotype rule was
relaxed.

The final eICU analysis-ready set contained 251 patients from 26 hospitals and
70 post-landmark in-hospital deaths. The hospital gate passed (26 >= 10), but
the event gate failed (70 < 100).

## Frozen definition and time ordering

The phenotype was locked before any 72-hour outcome count was inspected. Time
zero was the existing broad-AHRF index. All valid P/F records paired to
PEEP >=5 cm H2O were retained, including P/F values >=300. Persistence required
at least one record and mean P/F <300 in each of three windows: 0 to <24 hours,
24 to <48 hours, and 48 to <=72 hours.

The Marshall day-3-death exception was not used. A separate index+72-hour
landmark excluded deaths, live discharges, and unknown hospital follow-up on
or before the landmark. The planned analysis reused the frozen tuple and
complete no-GCS core available by index+6 hours; it did not reselect a
72-hour ventilator tuple.

This remains a `persistent-AHRF-enriched sensitivity`, not confirmed or
adjudicated ARDS.

## Feasibility flow

| Tier | MIMIC-IV | eICU-CRD |
|---|---:|---:|
| Broad index | 20,765 | 5,624 |
| All three windows observed | 5,234 | 1,459 |
| Persistent physiology | 3,399 | 1,050 |
| Persistent and at risk in hospital at 72 h | 3,274 | 1,018 |
| Plus frozen valid tuple by 6 h | 2,286 | 287 |
| Plus complete frozen no-GCS core | 2,137 | 251 |
| Post-72 h deaths in final set | 689 | 70 |

Among the eICU final set, 26 hospitals contributed patients. The largest
hospital contributed 72/251 (28.7%); 11 hospitals contributed at least five
patients and eight contributed at least ten.

## Interpretation

The persistent phenotype itself was reproducible in both databases. The
analysis became under-supported only after requiring the same early
plateau-based tuple and complete no-GCS core used by the primary comparison.
Running a five-model external comparison with 70 eICU deaths would violate the
prespecified stopping rule and risk an unstable subgroup result.

The MIMIC event count does not rescue the sensitivity because its purpose was
cross-database replication under a separate 72-hour landmark. The correct
action is omission of outcome estimates, not a MIMIC-only post hoc analysis or
a weaker eICU gate.

## Manuscript-ready wording

> A prespecified persistent-AHRF-enriched sensitivity required mean P/F <300
> mm Hg with PEEP >=5 cm H2O in each of three 24-hour windows and a separate
> 72-hour landmark. Although 26 eICU hospitals contributed to the final common
> set, only 70 post-landmark deaths remained, below the prespecified minimum of
> 100; therefore, no outcome model was fitted.

## Main audit files

- `analysis_rebuild_v2/qc/persistent_ahrf/persistent_ahrf_72h_feasibility_complete_v2.csv`
- `analysis_rebuild_v2/aggregate/persistent_ahrf/persistent_ahrf_72h_landmark_flow_v2.csv`
- `analysis_rebuild_v2/aggregate/persistent_ahrf/persistent_ahrf_eicu_hospital_support_v2.csv`
- `analysis_rebuild_v2/aggregate/persistent_ahrf/persistent_ahrf_72h_gate_summary_v2.csv`
- `docs/rebuild_v2/persistent_ahrf_72h_preread_lock_v2.md`
