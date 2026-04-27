# =============================================================================
# aviation/config.R — Aviation-Specific Constants and Configuration
# =============================================================================
#
# Industry-specific settings for the aviation safety climate multiverse
# analysis. Shared functions (create_all_windows, etc.) are in common/data_prep.R.
#
# Unit of analysis: airport × month.
# Climate predictor: windowed ASRS safety climate scores from ATC reporters.
# Outcomes: NTSB accident occurrence and severity.
# =============================================================================


# =============================================================================
# WINDOW SPECIFICATIONS  (identical structure to NRC / rail)
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
# ATC REPORTER SCOPE
# =============================================================================
# These match values in the ASRS person_1function column.
# Used both here (to define multiverse levels) and in data_prep.R (to filter).

ATC_FUNCTIONS_LOCAL <- c(
  "Local",
  "Ground",
  "Flight Data / Clearance Delivery",
  "Handoff / Assist"
)

ATC_FUNCTIONS_TERMINAL <- c("Approach", "Departure")

ATC_FUNCTIONS_ENROUTE  <- c("Enroute")

# Named list maps scope label → function values included
ATC_SCOPE_MAP <- list(
  local          = ATC_FUNCTIONS_LOCAL,
  local_terminal = c(ATC_FUNCTIONS_LOCAL, ATC_FUNCTIONS_TERMINAL),
  all_atc        = c(ATC_FUNCTIONS_LOCAL, ATC_FUNCTIONS_TERMINAL, ATC_FUNCTIONS_ENROUTE)
)


# =============================================================================
# AVIATION-SPECIFIC MULTIVERSE DIMENSIONS
# =============================================================================
# These are crossed with WINDOW_SPECS in the multiverse grid.

# Airport proximity filter applied to NTSB accidents (nautical miles)
APT_DIST_CUTOFFS_NM <- c(5L, 15L)

# ATC reporter scope levels (keys into ATC_SCOPE_MAP)
ATC_SCOPE_LEVELS <- c("local", "local_terminal", "all_atc")

# Treatment of airport-months with zero ATC reports (unobserved climate)
MISSING_CLIMATE_LEVELS <- c("exclude", "impute_mean")


# =============================================================================
# OPERATIONAL VARIABLES
# =============================================================================

AV_OPS_VARS    <- c("departures")     # from BTS T100; seats/passengers available as extensions
AV_OPS_ROLL_K  <- 12L                 # 12-month rolling window
AV_OPS_LAG_K   <- 1L                  # 1-month lag
AV_OPS_MIN_HIST <- 6L                 # minimum 6 months before features are valid


# =============================================================================
# OUTCOME VARIABLES
# =============================================================================
# PRIMARY:
#   accident_binary:   Any NTSB accident in airport-month (glmer binomial)
# SECONDARY:
#   inj_serious_fatal: Count of serious + fatal injuries (glmmTMB hurdle)
#   sev_ord:           Severity ordinal — none/minor/serious/fatal (clmm)

OUTCOME_VARS <- list(
  accident_binary = list(
    var      = "accident_binary",
    family   = "binomial",
    model_fn = "glmer",
    label    = "Accident occurrence (binary)"
  ),
  inj_serious_fatal = list(
    var      = "inj_serious_fatal",
    family   = "hurdle",
    model_fn = "glmmTMB",
    label    = "Serious + fatal injuries (hurdle)"
  ),
  sev_ord = list(
    var      = "sev_ord_factor",
    family   = "cumulative",
    model_fn = "clmm",
    label    = "Accident severity (ordinal)"
  )
)

get_outcome_config <- function(outcome) {
  if (!outcome %in% names(OUTCOME_VARS)) {
    stop(sprintf("Unknown outcome: '%s'. Must be one of: %s",
                 outcome, paste(names(OUTCOME_VARS), collapse = ", ")))
  }
  OUTCOME_VARS[[outcome]]
}

get_outcome_var <- function(outcome) get_outcome_config(outcome)$var


# =============================================================================
# MODEL FORMULA TEMPLATES
# =============================================================================
# CLIMATE_VAR is replaced at runtime with the actual windowed variable name.
# Random effect grouping: airport_id (mirrors facility in NRC, org_id in rail).

.temporal_controls <- "yearmonth_num_c + sin_month + cos_month"

.ops_covariates <- "departures_between + departures_within"

# Full formulas with climate variable
MODEL_FORMULAS <- list(

  accident_binary = paste0(
    "accident_binary ~ CLIMATE_VAR + ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | airport_id)"
  ),

  inj_serious_fatal = list(
    cond = paste0("inj_serious_fatal ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | airport_id)"),
    zi   = paste0("~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | airport_id)")
  ),

  # clmm does support (1 | airport_id) but convergence is fragile at this
  # sample size; treat airport_id as a fixed stratum or drop RE for ordinal.
  sev_ord = paste0(
    "sev_ord_factor ~ CLIMATE_VAR + ", .temporal_controls, " + ", .ops_covariates
  )
)

# No-climate formulas (CV baseline)
MODEL_FORMULAS_NO_CLIMATE <- list(

  accident_binary = paste0(
    "accident_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | airport_id)"
  ),

  inj_serious_fatal = list(
    cond = paste0("inj_serious_fatal ~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | airport_id)"),
    zi   = paste0("~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | airport_id)")
  ),

  sev_ord = paste0(
    "sev_ord_factor ~ ", .temporal_controls, " + ", .ops_covariates
  )
)

# Intercept-only formulas (CV baseline)
MODEL_FORMULAS_INTERCEPT <- list(
  accident_binary   = "accident_binary ~ 1 + (1 | airport_id)",
  inj_serious_fatal = list(
    cond = "inj_serious_fatal ~ 1 + (1 | airport_id)",
    zi   = "~ 1 + (1 | airport_id)"
  )
)

# Seasonal + ops baseline (no climate) — alias used by CV code
MODEL_FORMULAS_SEASONAL <- MODEL_FORMULAS_NO_CLIMATE

rm(.temporal_controls, .ops_covariates)


# =============================================================================
# MINIMUM REPORTS THRESHOLD
# =============================================================================
# Lower than NRC (75) / rail defaults due to inherent ATC report sparsity.
# ~38 airports clear 30 reports; ~16 clear 50. Start at 30 and treat as a
# sensitivity check in the multiverse.

MIN_REPORTS_DEFAULT <- 30L


# =============================================================================
# FORMULA UTILITIES
# =============================================================================

#' Replace CLIMATE_VAR placeholder with actual variable name
build_formula <- function(formula_template, climate_var) {
  if (is.list(formula_template)) {
    lapply(formula_template, function(f) gsub("CLIMATE_VAR", climate_var, f, fixed = TRUE))
  } else {
    gsub("CLIMATE_VAR", climate_var, formula_template, fixed = TRUE)
  }
}


# =============================================================================
# CV FORMULA BUILDERS
# =============================================================================

#' Build the full model formula for an outcome, substituting climate variable.
#' For hurdle outcomes returns the conditional component (used for CV Brier score).
build_outcome_formula <- function(outcome, climate_var) {
  template <- MODEL_FORMULAS[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS), collapse = ", ")))
  }
  fml_str <- if (is.list(template)) template$cond %||% template[[1]] else template
  as.formula(build_formula(fml_str, climate_var))
}


#' Build intercept-only baseline formula for CV
build_intercept_formula <- function(outcome) {
  template <- MODEL_FORMULAS_INTERCEPT[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS_INTERCEPT entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS_INTERCEPT), collapse = ", ")))
  }
  fml_str <- if (is.list(template)) template$cond %||% template[[1]] else template
  as.formula(fml_str)
}


#' Build seasonal + ops baseline formula (no climate) for CV
build_seasonal_formula <- function(outcome) {
  template <- MODEL_FORMULAS_SEASONAL[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS_SEASONAL entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS_SEASONAL), collapse = ", ")))
  }
  fml_str <- if (is.list(template)) template$cond %||% template[[1]] else template
  as.formula(build_formula(fml_str, ""))   # no-climate: CLIMATE_VAR already absent
}


#' Build a binary (logistic) formula for CV from any outcome template.
#' For binary outcomes returns the template directly.
#' For hurdle/ordinal rewrites LHS as I(outcome > 0).
build_binary_formula <- function(outcome, climate_var = NULL) {
  outcome_var <- get_outcome_var(outcome)
  cfg         <- get_outcome_config(outcome)

  if (cfg$family == "binomial") {
    # Already binary — use full formula template directly
    f_str <- MODEL_FORMULAS[[outcome]]
  } else {
    # Hurdle or ordinal: use conditional component, rewrite LHS
    template <- MODEL_FORMULAS[[outcome]]
    f_str    <- if (is.list(template)) template$cond else template
    f_str    <- sub(
      paste0("^", outcome_var),
      paste0("I(", outcome_var, " > 0)"),
      f_str
    )
  }

  if (!is.null(climate_var)) {
    f_str <- gsub("CLIMATE_VAR", climate_var, f_str, fixed = TRUE)
  } else {
    f_str <- gsub("\\s*\\+\\s*CLIMATE_VAR", "", f_str)
    f_str <- gsub("CLIMATE_VAR\\s*\\+\\s*", "", f_str)
  }

  f_str
}


#' Build intercept-only binary formula for CV
build_intercept_only_formula <- function(outcome) {
  outcome_var <- get_outcome_var(outcome)
  cfg         <- get_outcome_config(outcome)
  lhs <- if (cfg$family == "binomial") outcome_var else paste0("I(", outcome_var, " > 0)")
  paste0(lhs, " ~ 1 + (1 | airport_id)")
}


#' Build seasonal-only binary formula for CV (time + harmonics, no ops, no climate)
build_seasonal_only_formula <- function(outcome) {
  outcome_var <- get_outcome_var(outcome)
  cfg         <- get_outcome_config(outcome)
  lhs <- if (cfg$family == "binomial") outcome_var else paste0("I(", outcome_var, " > 0)")
  paste0(lhs, " ~ yearmonth_num_c + sin_month + cos_month + (1 | airport_id)")
}


#' Build full CV baseline ladder: full → no-climate → seasonal-only → intercept-only
build_baseline_formulas <- function(outcome, climate_var) {
  list(
    list(formula = build_binary_formula(outcome, climate_var), label = climate_var),
    list(formula = build_binary_formula(outcome, NULL),        label = "no_climate"),
    list(formula = build_seasonal_only_formula(outcome),       label = "seasonal_only"),
    list(formula = build_intercept_only_formula(outcome),      label = "intercept_only")
  )
}
