# =============================================================================
# phmsa/config.R — PHMSA Pipeline-Specific Constants and Configuration
# =============================================================================
#
# Industry-specific settings for the PHMSA pipeline multiverse analysis.
# Three pipeline types (gas_distribution, gas_transmission, hazardous_liquid)
# are treated as separate analyses, analogous to NRC vs. rail.
#
# Shared functions are in common/data_prep.R.
# =============================================================================


# =============================================================================
# WINDOW SPECIFICATIONS (identical to NRC / rail / aviation)
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
# PIPELINE TYPES (outer analysis dimension)
# =============================================================================
# gravity_regulated has no annual report files and is excluded from ops coverage.

PIPELINE_TYPES <- c("gas_distribution", "gas_transmission", "hazardous_liquid")


# =============================================================================
# OPERATIONAL VARIABLES
# =============================================================================
# Annual data — rolling window measured in years, not months.

PHMSA_OPS_VARS    <- c("total_miles")
PHMSA_OPS_ROLL_K  <- 3L   # 3-year rolling window
PHMSA_OPS_LAG_K   <- 1L   # 1-year lag (use prior year's report)
PHMSA_OPS_MIN_HIST <- 2L  # minimum 2 years of history


# =============================================================================
# OUTCOME VARIABLES
# =============================================================================
# All outcomes vary across incidents (not all incidents cause injuries/deaths/costs).
#   injury_binary   : any injury (yes/no) — ~30% positive; binomial glmer
#   fatality_binary : any fatality (yes/no) — ~5% positive; binomial glmer
#   total_cost      : total estimated cost — right-skewed, many zeros; hurdle gamma

OUTCOME_VARS <- list(
  injury_binary = list(
    var      = "injury_binary",
    family   = "binomial",
    model_fn = "glmer",
    label    = "Any injury (binary)"
  ),
  fatality_binary = list(
    var      = "fatality_binary",
    family   = "binomial",
    model_fn = "glmer",
    label    = "Any fatality (binary)"
  ),
  total_cost = list(
    var      = "total_cost",
    family   = "gamma",
    model_fn = "glmmTMB",
    label    = "Total cost (Gamma, positive-only)"
  )
)

get_outcome_config <- function(outcome) {
  if (!outcome %in% names(OUTCOME_VARS)) {
    stop(sprintf("Unknown outcome: '%s'. Must be one of: %s",
                 outcome, paste(names(OUTCOME_VARS), collapse = ", ")))
  }
  OUTCOME_VARS[[outcome]]
}

get_outcome_var <- function(outcome) {
  get_outcome_config(outcome)$var
}


# =============================================================================
# MODEL FORMULA TEMPLATES
# =============================================================================
# CLIMATE_VAR is replaced at runtime with the actual windowed variable name.
# RE: (1 | operator_id) — operator random intercept.
# Temporal: yearmonth_num_c + sin_month + cos_month.
# Ops: total_miles_between + total_miles_within (Mundlak between/within).

.temporal_controls <- "yearmonth_num_c + sin_month + cos_month"

.ops_covariates <- "total_miles_between + total_miles_within"

MODEL_FORMULAS <- list(
  injury_binary = paste0(
    "injury_binary ~ CLIMATE_VAR + ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | operator_id)"
  ),
  fatality_binary = paste0(
    "fatality_binary ~ CLIMATE_VAR + ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | operator_id)"
  ),
  total_cost = list(
    cond = paste0("total_cost ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | operator_id)")
  )
)

MODEL_FORMULAS_NO_CLIMATE <- list(
  injury_binary = paste0(
    "injury_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | operator_id)"
  ),
  fatality_binary = paste0(
    "fatality_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | operator_id)"
  ),
  total_cost = list(
    cond = paste0("total_cost ~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | operator_id)")
  )
)

MODEL_FORMULAS_INTERCEPT <- list(
  injury_binary   = "injury_binary ~ 1 + (1 | operator_id)",
  fatality_binary = "fatality_binary ~ 1 + (1 | operator_id)",
  total_cost = list(
    cond = "total_cost ~ 1 + (1 | operator_id)"
  )
)

MODEL_FORMULAS_SEASONAL <- MODEL_FORMULAS_NO_CLIMATE

rm(.temporal_controls, .ops_covariates)


#' Replace CLIMATE_VAR placeholder with actual variable name
build_formula <- function(formula_template, climate_var) {
  if (is.list(formula_template)) {
    lapply(formula_template, function(f) gsub("CLIMATE_VAR", climate_var, f, fixed = TRUE))
  } else {
    gsub("CLIMATE_VAR", climate_var, formula_template, fixed = TRUE)
  }
}


# =============================================================================
# MINIMUM REPORTS THRESHOLD
# =============================================================================
# PHMSA datasets are smaller per pipeline type than NRC/rail.

MIN_REPORTS_DEFAULT <- 30L


# =============================================================================
# CV FORMULA BUILDERS
# =============================================================================

#' Build full model formula (climate included) for a given outcome
build_outcome_formula <- function(outcome, climate_var) {
  template <- MODEL_FORMULAS[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS), collapse = ", ")))
  }
  fml_str <- if (is.list(template)) template$cond else template
  as.formula(build_formula(fml_str, climate_var))
}


#' Build intercept-only baseline formula for CV
build_intercept_formula <- function(outcome) {
  template <- MODEL_FORMULAS_INTERCEPT[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS_INTERCEPT entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS_INTERCEPT), collapse = ", ")))
  }
  fml_str <- if (is.list(template)) template$cond else template
  as.formula(fml_str)
}


#' Build seasonal+ops baseline formula (no climate) for CV
build_seasonal_formula <- function(outcome) {
  template <- MODEL_FORMULAS_SEASONAL[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS_SEASONAL entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS_SEASONAL), collapse = ", ")))
  }
  fml_str <- if (is.list(template)) template$cond else template
  as.formula(fml_str)
}


#' Build binary formula for CV (for hurdle outcomes, wraps LHS as binary indicator)
build_binary_formula <- function(outcome, climate_var = NULL) {
  outcome_var <- get_outcome_var(outcome)
  template    <- MODEL_FORMULAS[[outcome]]
  f_str       <- if (is.list(template)) template$cond else template

  # Remap LHS to binary indicator for hurdle outcomes
  if (OUTCOME_VARS[[outcome]]$family %in% c("hurdle", "hurdle_gamma")) {
    f_str <- sub(paste0("^", outcome_var),
                 paste0("I(", outcome_var, " > 0)"), f_str)
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
  cfg         <- OUTCOME_VARS[[outcome]]
  lhs <- if (cfg$family %in% c("hurdle", "hurdle_gamma")) {
    paste0("I(", outcome_var, " > 0)")
  } else {
    outcome_var
  }
  paste0(lhs, " ~ 1 + (1 | operator_id)")
}


#' Build seasonal-only binary formula for CV
build_seasonal_only_formula <- function(outcome) {
  outcome_var <- get_outcome_var(outcome)
  cfg         <- OUTCOME_VARS[[outcome]]
  lhs <- if (cfg$family %in% c("hurdle", "hurdle_gamma")) {
    paste0("I(", outcome_var, " > 0)")
  } else {
    outcome_var
  }
  paste0(lhs, " ~ yearmonth_num_c + sin_month + cos_month + (1 | operator_id)")
}


#' Build full set of baseline formulas for CV comparison
build_baseline_formulas <- function(outcome, climate_var) {
  list(
    list(formula = build_binary_formula(outcome, climate_var),   label = climate_var),
    list(formula = build_binary_formula(outcome, NULL),          label = "no_climate"),
    list(formula = build_seasonal_only_formula(outcome),         label = "seasonal_only"),
    list(formula = build_intercept_only_formula(outcome),        label = "intercept_only")
  )
}
