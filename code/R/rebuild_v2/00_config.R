#!/usr/bin/env Rscript

# ARDS mechanical-power rebuild v2: post-review locked configuration
# Freeze date: 2026-07-16 (Asia/Shanghai)
#
# This configuration governs the hypothesis-driven analyses added after the
# strict JICM presubmission review. It is not a prospective preregistration.
# Existing rebuild_v1 code and outputs are read-only provenance.

options(stringsAsFactors = FALSE)

require_env_dir <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    stop(
      "Environment variable ", name,
      " must point to an accessible local data directory."
    )
  }
  normalizePath(value, mustWork = TRUE)
}

PROJECT_ROOT <- normalizePath(
  Sys.getenv(
    "ARDS_MP_PROJECT_ROOT",
    unset = getwd()
  ),
  mustWork = TRUE
)

MIMIC_ROOT <- require_env_dir("MIMIC_IV_DIR")

EICU_ROOT <- require_env_dir("EICU_CRD_DIR")

REBUILD_V1_ROOT <- file.path(PROJECT_ROOT, "analysis_rebuild_v1")
REBUILD_ROOT <- file.path(PROJECT_ROOT, "analysis_rebuild_v2")
PRIVATE_ROOT <- file.path(REBUILD_ROOT, "private")
AGGREGATE_ROOT <- file.path(REBUILD_ROOT, "aggregate")
QC_ROOT <- file.path(REBUILD_ROOT, "qc")
LOG_ROOT <- file.path(REBUILD_ROOT, "logs")

for (d in c(REBUILD_ROOT, PRIVATE_ROOT, AGGREGATE_ROOT, QC_ROOT, LOG_ROOT)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

LOCKED_V2 <- list(
  version = "2.0.0",
  freeze_date = as.Date("2026-07-16"),
  governance = "post-review hypothesis-driven amendment",
  target_population_label =
    "oxygenation-defined acute hypoxemic respiratory failure",
  minimum_age_years = 18,
  first_qualifying_stay_per_patient = TRUE,
  pf_threshold_mmHg = 300,
  minimum_index_peep_cmH2O = 5,
  pao2_fio2_pair_window_minutes = 120,
  pao2_peep_pair_window_minutes = 120,
  primary_exposure_window_hours = 6,
  landmark_hours = 6,
  tuple_pair_window_minutes = 60,
  tuple_pair_window_sensitivity_minutes = 30,
  primary_tuple_rule = "first_valid_complete_tuple",
  primary_outcome = "in_hospital_mortality_after_6h_landmark",
  primary_baseline_core = c(
    "age", "sex", "index_pf", "map", "vasopressor",
    "platelets", "creatinine"
  ),
  complete_gcs_core = c(
    "age", "sex", "index_pf", "gcs", "map", "vasopressor",
    "platelets", "creatinine"
  ),
  infection_sensitivity = list(
    enabled = TRUE,
    window_hours_before_index = 48,
    window_hours_after_index = 0,
    role = "clinical-context sensitivity only"
  ),
  formulae = list(
    driving_pressure = "Pplat - PEEP",
    resistive_pressure = "Ppeak - Pplat",
    smp = paste(
      "0.098 * RR * VT_L *",
      "(Ppeak - 0.5 * (Pplat - PEEP))"
    ),
    four_dprr = "4 * driving_pressure + RR",
    static_power = "0.098 * RR * VT_L * PEEP",
    dynamic_power = "0.098 * RR * VT_L * 0.5 * driving_pressure",
    resistive_power = "0.098 * RR * VT_L * resistive_pressure",
    compliance = "VT_L / driving_pressure",
    compliance_normalized_smp = "smp / compliance"
  ),
  construct_labels = list(
    exposure =
      "plateau-based airway-pressure surrogate mechanical power",
    decomposition =
      "exact algebraic decomposition of the surrogate equation",
    static_power = "static-elastic algebraic term",
    dynamic_power = "dynamic-elastic algebraic term",
    resistive_power = "resistive algebraic term",
    interpretation_boundary = paste(
      "Terms are formula components in J/min and are not direct measurements",
      "of transpulmonary energy, tissue energy absorption, or dissipated energy."
    )
  ),
  physiologic_ranges = list(
    pao2_mmHg = c(20, 700),
    fio2_percent = c(21, 100),
    peep_cmH2O = c(5, 30),
    plateau_cmH2O = c(5, 60),
    peak_cmH2O = c(5, 80),
    tidal_volume_mL = c(100, 1500),
    respiratory_rate_per_min = c(5, 60),
    driving_pressure_cmH2O = c(0, 40),
    surrogate_mp_J_per_min = c(0, 100)
  ),
  physiologic_ordering = "Ppeak >= Pplat >= PEEP",
  measurement_quality = list(
    rate_concordance_absolute_difference_per_min = 2,
    preferred_tidal_volume_source = "observed_or_exhaled",
    preferred_respiratory_rate_source = "total_measured",
    mimic_mode_restriction =
      "volume-targeted assist-control-compatible and rate-concordant",
    eicu_mode_restriction = "not estimable from available mode fields"
  ),
  model_ids = c("M0", "M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY"),
  model_increment_terms = list(
    M0 = character(),
    M_MP = "smp",
    M_4DPRR = "four_dprr",
    M_DPRR = c("driving_pressure", "RR"),
    M_ENERGY = c("static_power", "dynamic_power", "resistive_power")
  ),
  primary_increment_forms = "linear",
  baseline_continuous_form = list(
    type = "restricted_cubic_spline",
    knots = c(0.10, 0.50, 0.90),
    derivation_database = "MIMIC-IV"
  ),
  nonlinear_fairness_sensitivity = list(
    type = "restricted_cubic_spline",
    knots = c(0.05, 0.35, 0.65, 0.95),
    equal_flexibility_required = TRUE
  ),
  primary_external_comparison_metric = "paired_delta_brier",
  performance_metrics = c(
    "brier", "log_loss", "c_statistic", "calibration_intercept",
    "calibration_slope", "observed_expected_ratio"
  ),
  bootstrap = list(
    mimic_internal_replicates = 1000L,
    eicu_hospital_cluster_replicates = 2000L,
    minimum_success_fraction = 0.95,
    internal_ci =
      "Noma location-shifted for Brier/log loss/C; two-stage for slope",
    calibration_slope_outer_replicates = 1000L,
    calibration_slope_inner_replicates = 200L,
    seed_mimic = 2026071601L,
    seed_eicu = 2026071602L,
    seed_sensitivity = 2026071603L
  ),
  center_robustness = list(
    patient_weighted_primary = TRUE,
    exclude_largest_center = TRUE,
    equal_center_minimum_n = 10L,
    leave_one_hospital_out_role = "influence analysis",
    cluster_bootstrap_unit = "hospital"
  ),
  missing_data_hierarchy = list(
    primary = "no-GCS complete common set",
    sensitivity_complete_gcs = TRUE,
    sensitivity_frozen_median_indicator = TRUE,
    multiple_imputation_role = "association sensitivity only",
    external_outcome_in_imputation_forbidden = TRUE
  ),
  selection_sensitivity = list(
    targets = c(
      "valid_tuple_by_6h_landmark",
      "valid_tuple_and_complete_no_gcs_core"
    ),
    outcome_blind = TRUE,
    stabilized_weights = TRUE,
    truncation_quantiles = c(0.01, 0.99),
    report_effective_sample_size = TRUE,
    report_covariate_balance = TRUE,
    eicu_zero_tuple_hospitals =
      "reported separately and excluded from the weighted target estimand",
    interpretation =
      "sensitivity under the measured inclusion model, not bias elimination"
  ),
  persistence_sensitivity = list(
    landmark_hours = 72,
    minimum_eicu_events = 100L,
    minimum_eicu_hospitals = 10L,
    label = "persistent-AHRF-enriched sensitivity",
    never_label_as_confirmed_ards = TRUE
  ),
  prohibited_primary_claims = c(
    "causal effect of lowering sMP",
    "validated passive ventilation",
    "directly measured mechanical power",
    "directly measured tissue energy or dissipated energy",
    "imaging-adjudicated Berlin ARDS",
    "clinical treatment threshold",
    "equivalence or non-inferiority without a prespecified margin"
  ),
  prohibited_posthoc_analyses = c(
    "NRI", "IDI", "data-driven cutoff", "black-box machine learning",
    "unrestricted subgroup fishing", "outcome-selected functional form",
    "multiple competing normalization formulas"
  )
)

stopifnot(
  LOCKED_V2$landmark_hours == 6,
  LOCKED_V2$primary_exposure_window_hours == 6,
  LOCKED_V2$primary_outcome ==
    "in_hospital_mortality_after_6h_landmark",
  identical(
    LOCKED_V2$model_ids,
    c("M0", "M_MP", "M_4DPRR", "M_DPRR", "M_ENERGY")
  )
)
