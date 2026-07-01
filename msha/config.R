# =============================================================================
# msha/config.R — MSHA Mine-Safety-Specific Constants and Configuration
# =============================================================================
#
# Industry-specific settings for the MSHA mine-safety multiverse analysis.
# Shared functions are in common/data_prep.R.
#
# Structural template: nrc/config.R (the current canonical arm). MSHA mirrors
# NRC's facility(mine) x quarter cadence and log(exposure)-offset panel design.
# The one structural addition is a COMMODITY outer loop (coal vs. metal/
# non-metal), analogous to PHMSA's PIPELINE_TYPES: coal and MNM mines differ in
# operational regime, reporting, and linkage coverage, so they are fit as
# separate protocols and never pooled.
#
# Organisation random effect:  (1 | mine_id)   — a clean universal MSHA key
#   (no fuzzy name normalisation is needed, unlike NRC facility names).
# Universal exposure offset:   log(hours_worked)  — employee-hours worked in
#   the mine-quarter. Used for ALL panel-rate outcomes (self-reported and
#   regulatory) because every outcome is fundamentally worker-exposure scaled.
# =============================================================================


# =============================================================================
# COMMODITY OUTER LOOP
# =============================================================================
# Each commodity is a self-contained analysis (separate data prep, fits, and
# output files). "coal" and "mnm" (metal / non-metal) are the two MSHA
# commodity classes carried through events.parquet / ops_mine_quarter.parquet.
#
# Currently scoped to COAL only. Preliminary multiverse results showed the
# coal vs. mnm climate signal diverges sharply (e.g. injury-vs-accident-only
# flips sign), and mnm has weaker operator/exposure linkage coverage. mnm is
# deferred pending a dedicated heterogeneity deep-dive; until then we analyse
# coal (the cleaner, more homogeneous base) and move on. To re-enable mnm,
# restore: COMMODITIES <- c("coal", "mnm").
COMMODITIES <- c("coal", "mnm")


# =============================================================================
# WINDOW SPECIFICATIONS (identical structure to NRC / rail)
# =============================================================================

WINDOW_SPECS <- list(
  sma = tibble::tibble(window_size = c(5, 10, 20)),
  ewma = tibble::tribble(
    ~lag_days, ~halflife_days,
    360,  180,
    540,  270,
    720,  360
  )
)


# =============================================================================
# OPERATIONAL VARIABLES
# =============================================================================
# MSHA's operational exposure is employee-hours worked. The ops pipeline
# (ops_mine_quarter.parquet) already provides a between/within (Mundlak)
# decomposition of hours as `hours_between` / `hours_within`.
#
# Incident-level track (NO offset): both `hours_between` and `hours_within`
#   enter as covariates (mine scale + within-mine surge).
# Panel-rate track (offset = log(hours_worked)): only `hours_within` enters.
#   `hours_between` is the slow-moving mine mean of exposure and is collinear
#   with the log(hours) offset level — the NRC principle that drops
#   capacity_factor against the days_at_power offset. `hours_within`
#   (within-mine deviation) is operational volatility, NOT collinear, so it is
#   retained.
MSHA_OPS_VARS <- c("hours")   # base name; precomputed hours_between/_within in ops

# Rolling decomposition params (used only if a precomputed _between/_within is
# absent — the MSHA ops pipeline normally supplies them).
MSHA_OPS_ROLL_K   <- 4L
MSHA_OPS_LAG_K    <- 1L
MSHA_OPS_MIN_HIST <- 2L


# =============================================================================
# DETECTABILITY TIER MAPPING
# =============================================================================
# Maps DEGREE_INJURY_CD (events `degree_injury_cd`, codes "00"-"10") onto
# detectability tiers for the reporting-filter supplement
# (injury_detectability_supplemental_design.md §2). Single source of truth for
# the tier recode, used by BOTH the incident track (data_prep.R) and the panel
# track (panel_data_prep.R). Lives in config.R (not data_prep.R as the spec
# sketched) because the panel track does not source data_prep.R, and because the
# tiering is a config-level multiverse dimension.
#
#   T1 high      01 fatality, 02 permanent disability      ~100% capture
#   T2 lost-time 03 days-away, 04 days-away+restricted     moderate capture
#   T3 low       05 restricted-only, 06 no-restriction,    discretionary
#                10 all-other (incl. first aid)
#   T0           00 accident-only (no injury)              lowest-detectability anchor
#   EXCLUDE      07 illness, 08 natural causes,            different filter /
#                09 non-employee                            population mismatch
#
# 07/08/09 and any unmapped/unknown code -> NA: dropped from the acute-severity
# ladder. `t0_inclusion` and `detectability_tiering` are multiverse switches; the
# DEFAULT analysis is the 3-tier scheme with T0 excluded.

DETECTABILITY_TIER_MAP <- c(
  "00" = "T0",
  "01" = "T1", "02" = "T1",
  "03" = "T2", "04" = "T2",
  "05" = "T3", "06" = "T3", "10" = "T3"
  # 07, 08, 09, and any other code -> NA (excluded)
)

# 2-tier robustness collapse (Morantz 2013 traumatic-vs-minor): T1+T2 -> severe,
# T3 -> minor. T0 stays T0; excludes stay NA.
DETECTABILITY_TIER_MAP_2TIER <- c(
  "00" = "T0",
  "01" = "severe", "02" = "severe", "03" = "severe", "04" = "severe",
  "05" = "minor",  "06" = "minor",  "10" = "minor"
)

#' Assign detectability tier(s) from DEGREE_INJURY_CD.
#'
#' @param degree_cd Character vector of degree-injury codes ("00".."10").
#' @param scheme "three_tier" (default) or "two_tier".
#' @return Character vector of tier labels; NA for excluded/unknown codes.
msha_assign_tier <- function(degree_cd, scheme = c("three_tier", "two_tier")) {
  scheme <- match.arg(scheme)
  code <- trimws(as.character(degree_cd))
  map  <- if (scheme == "two_tier") DETECTABILITY_TIER_MAP_2TIER else DETECTABILITY_TIER_MAP
  unname(map[code])
}


# =============================================================================
# OUTCOME VARIABLES (incident-level / conditional-severity track)
# =============================================================================
# One row per reported accident/injury event. We model severity GIVEN that an
# event was reported (no exposure offset — that is the panel track's job).
#
#   fatality_binary : event was fatal (vs. non-fatal)         glmer binomial
#   injury_binary   : event involved an injury (accident_only==0 -> 1)
#                                                              glmer binomial
#   days_lost       : lost workdays severity                  glmmTMB hurdle
#                     (binary gate days_lost>0 + Gamma magnitude)

OUTCOME_VARS <- list(
  fatality_binary = list(
    var      = "fatality_binary",
    family   = "binomial",
    model_fn = "glmer",
    label    = "Fatal accident (binary)"
  ),
  injury_binary = list(
    var      = "injury_binary",
    family   = "binomial",
    model_fn = "glmer",
    label    = "Injury accident vs. accident-only (binary)"
  ),
  days_lost = list(
    var      = "days_lost",
    family   = "hurdle",
    model_fn = "glmmTMB",
    label    = "Lost workdays (hurdle)"
  )
)

get_outcome_config <- function(outcome) {
  if (!outcome %in% names(OUTCOME_VARS)) {
    stop(sprintf("Unknown outcome: '%s'. Must be one of: %s",
                 outcome, paste(names(OUTCOME_VARS), collapse = ", ")))
  }
  OUTCOME_VARS[[outcome]]
}


# =============================================================================
# MODEL FORMULA TEMPLATES (incident-level)
# =============================================================================

.temporal_controls <- "yearmonth_num_c + sin_month + cos_month"

# Incident-level track has NO offset, so both between & within hours enter.
.ops_covariates <- "hours_between + hours_within"

# Full formulas with climate variable
MODEL_FORMULAS <- list(
  days_lost = list(
    binary = paste0("days_lost_binary ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                    .ops_covariates, " + (1 | mine_id)"),
    gamma  = paste0("days_lost ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                    .ops_covariates, " + (1 | mine_id)")
  ),
  fatality_binary = paste0(
    "fatality_binary ~ CLIMATE_VAR + ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | mine_id)"
  ),
  injury_binary = paste0(
    "injury_binary ~ CLIMATE_VAR + ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | mine_id)"
  )
)

# No-climate formulas (for CV baselines)
MODEL_FORMULAS_NO_CLIMATE <- list(
  days_lost = list(
    binary = paste0("days_lost_binary ~ ", .temporal_controls, " + ",
                    .ops_covariates, " + (1 | mine_id)"),
    gamma  = paste0("days_lost ~ ", .temporal_controls, " + ",
                    .ops_covariates, " + (1 | mine_id)")
  ),
  fatality_binary = paste0(
    "fatality_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | mine_id)"
  ),
  injury_binary = paste0(
    "injury_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | mine_id)"
  )
)

# Intercept-only formulas (for CV baseline)
MODEL_FORMULAS_INTERCEPT <- list(
  days_lost = list(
    binary = "days_lost_binary ~ 1 + (1 | mine_id)",
    gamma  = "days_lost ~ 1 + (1 | mine_id)"
  ),
  fatality_binary = "fatality_binary ~ 1 + (1 | mine_id)",
  injury_binary   = "injury_binary ~ 1 + (1 | mine_id)"
)

# Seasonal + ops (no climate) baseline — same as NO_CLIMATE, kept as a separate
# object for CV code that references MODEL_FORMULAS_SEASONAL explicitly.
MODEL_FORMULAS_SEASONAL <- MODEL_FORMULAS_NO_CLIMATE

#' Replace CLIMATE_VAR placeholder with actual variable name
build_formula <- function(formula_template, climate_var) {
  if (is.list(formula_template)) {
    lapply(formula_template, function(f) gsub("CLIMATE_VAR", climate_var, f, fixed = TRUE))
  } else {
    gsub("CLIMATE_VAR", climate_var, formula_template, fixed = TRUE)
  }
}

rm(.temporal_controls, .ops_covariates)


# =============================================================================
# MINIMUM REPORTS THRESHOLD
# =============================================================================
# Mines with at least this many reported events (within the commodity) are
# retained. MSHA has many small mines with few reports; this keeps the
# windowed-climate and mine random effect estimable. Tunable.
MIN_REPORTS_DEFAULT <- 30L


# =============================================================================
# CV FORMULA BUILDERS (3-tier: intercept_only / seasonal_ops / full)
# =============================================================================
# Mirrors nrc/config.R. Used by 2_msha_cv.R.

.fml_pick <- function(template) {
  if (is.list(template) && !is.null(names(template))) {
    if ("binary" %in% names(template)) template$binary else template[[1]]
  } else {
    template
  }
}

build_outcome_formula <- function(outcome, climate_var) {
  template <- MODEL_FORMULAS[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS), collapse = ", ")))
  }
  as.formula(build_formula(.fml_pick(template), climate_var))
}

build_intercept_formula <- function(outcome) {
  template <- MODEL_FORMULAS_INTERCEPT[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS_INTERCEPT entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS_INTERCEPT), collapse = ", ")))
  }
  as.formula(.fml_pick(template))
}

build_seasonal_formula <- function(outcome) {
  template <- MODEL_FORMULAS_SEASONAL[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS_SEASONAL entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS_SEASONAL), collapse = ", ")))
  }
  as.formula(.fml_pick(template))
}

#' Build the complete set of baseline formulas for CV comparison
#' @return List of lists, each with $formula and $label
build_baseline_formulas <- function(outcome, climate_var) {
  list(
    list(formula = build_intercept_formula(outcome), label = "intercept_only"),
    list(formula = build_seasonal_formula(outcome),  label = "seasonal_ops"),
    list(formula = build_outcome_formula(outcome, climate_var), label = "full")
  )
}
