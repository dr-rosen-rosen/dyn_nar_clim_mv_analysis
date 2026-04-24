# =============================================================================
# rail/config.R — Rail-Specific Constants and Configuration
# =============================================================================
#
# Industry-specific settings for the U.S. railroad multiverse analysis.
# Sourced by rail/data_prep.R, rail/fit_models.R, and the analysis scripts.
#
# Shared functions (create_all_windows, etc.) are in common/data_prep.R.
# =============================================================================


# =============================================================================
# WINDOW SPECIFICATIONS
# =============================================================================
# These define the temporal smoothing applied to climate composite scores.
# SMA: count-based (trailing k events)
# EWMA: calendar-time decay (lookback window × half-life)

WINDOW_SPECS <- list(
  sma = tibble::tibble(window_size = c(
    #3, 
    5, 10, 20)),
  ewma = tibble::tribble(
    ~lag_days, ~halflife_days,
    # 180,   60,
    # 180,   90,
    # 360,   90,
    360,  180,
    # 540,  180,
    540,  270,
    # 720,  180,
    720,  360
  )
)


# =============================================================================
# OPERATIONAL VARIABLES
# =============================================================================

OPS_VARS    <- c("train_miles", "passenger_miles", "staff_hours")
OPS_ROLL_K  <- 12L    # 12-month rolling window for between/within decomposition
OPS_LAG_K   <- 1L     # 1-month lag to ensure temporal ordering
OPS_MIN_HIST <- 6L    # Minimum 6 months of history before computing features


# =============================================================================
# OUTCOME VARIABLES
# =============================================================================

OUTCOME_VARS <- list(
  injuries = list(
    var    = "total_persons_injured",
    family = "hurdle",
    label  = "Total injuries (hurdle)"
  ),
  fatalities = list(
    var    = "total_persons_killed",
    family = "hurdle",
    label  = "Total fatalities (hurdle)"
  ),
  costs = list(
    var    = "total_damage_cost",
    family = "hurdle_gamma",
    label  = "Total damage cost (hurdle, Gamma)"
  )
)

#' Get outcome variable name from outcome key
get_outcome_var <- function(outcome) {
  if (!outcome %in% names(OUTCOME_VARS)) {
    stop(sprintf("Unknown outcome: '%s'. Must be one of: %s",
                 outcome, paste(names(OUTCOME_VARS), collapse = ", ")))
  }
  OUTCOME_VARS[[outcome]]$var
}


# =============================================================================
# MODEL FORMULA TEMPLATES
# =============================================================================
# CLIMATE_VAR is replaced at runtime with the actual windowed variable name.

# Temporal controls
.temporal_controls <- "yearmonth_num_c + sin_month + cos_month"

# Operational covariates (between/within decomposition)
.ops_covariates <- paste0(
  "train_miles_between + train_miles_within + ",
  "passenger_miles_between + passenger_miles_within + ",
  "staff_hours_between + staff_hours_within"
)

# Full formulas with climate variable
MODEL_FORMULAS <- list(
  injuries = list(
    cond = paste0("total_persons_injured ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)"),
    zi   = paste0("~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)")
  ),
  fatalities = list(
    cond = paste0("total_persons_killed ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)"),
    zi   = paste0("~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)")
  ),
  costs = list(
    cond = paste0("total_damage_cost ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)"),
    zi   = paste0("~ CLIMATE_VAR + ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)")
  )
)

# Baseline formulas (no climate variable — for CV comparison)
MODEL_FORMULAS_NO_CLIMATE <- list(
  injuries = list(
    cond = paste0("total_persons_injured ~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)"),
    zi   = paste0("~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)")
  ),
  fatalities = list(
    cond = paste0("total_persons_killed ~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)"),
    zi   = paste0("~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)")
  ),
  costs = list(
    cond = paste0("total_damage_cost ~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)"),
    zi   = paste0("~ ", .temporal_controls, " + ",
                  .ops_covariates, " + (1 | org_id)")
  )
)

# Intercept-only formulas (for CV baseline)
MODEL_FORMULAS_INTERCEPT <- list(
  injuries = list(
    cond = "total_persons_injured ~ 1 + (1 | org_id)",
    zi   = "~ 1 + (1 | org_id)"
  ),
  fatalities = list(
    cond = "total_persons_killed ~ 1 + (1 | org_id)",
    zi   = "~ 1 + (1 | org_id)"
  )
)

#' Replace CLIMATE_VAR placeholder with actual variable name
#' @param formula_template Formula string (or list of strings for hurdle)
#' @param climate_var Actual climate variable name
#' @return Formula string (or list) with substitution
build_formula <- function(formula_template, climate_var) {
  if (is.list(formula_template)) {
    lapply(formula_template, function(f) gsub("CLIMATE_VAR", climate_var, f, fixed = TRUE))
  } else {
    gsub("CLIMATE_VAR", climate_var, formula_template, fixed = TRUE)
  }
}

# Clean up private helpers
rm(.temporal_controls, .ops_covariates)


# =============================================================================
# CV FORMULA BUILDERS
# =============================================================================
# These build binary (logistic) formulas for cross-validation from the
# hurdle formula templates above. Used by 2_rail_cv.R.

#' Build a binary (logistic) formula from MODEL_FORMULAS template
#'
#' Takes the conditional formula string for a given outcome, replaces the
#' LHS with I(outcome > 0), and substitutes/removes the climate variable.
#'
#' @param outcome "injuries", "fatalities", or "costs"
#' @param climate_var Name of the climate variable (or NULL for no-climate model)
#' @return A formula string for the binary logistic GLMM
build_binary_formula <- function(outcome, climate_var = NULL) {

  outcome_var <- get_outcome_var(outcome)

  # Start from the conditional formula template
  f_str <- MODEL_FORMULAS[[outcome]]$cond

  # Replace LHS: count outcome -> binary indicator
  f_str <- sub(paste0("^", outcome_var), paste0("I(", outcome_var, " > 0)"), f_str)

  # Substitute or remove climate variable
  if (!is.null(climate_var)) {
    f_str <- gsub("CLIMATE_VAR", climate_var, f_str)
  } else {
    # Remove "CLIMATE_VAR + " or " + CLIMATE_VAR"
    f_str <- gsub("\\s*\\+\\s*CLIMATE_VAR", "", f_str)
    f_str <- gsub("CLIMATE_VAR\\s*\\+\\s*", "", f_str)
  }

  f_str
}


#' Build an intercept-only binary formula
#' @param outcome "injuries", "fatalities", or "costs"
#' @return Formula string: I(outcome > 0) ~ 1 + (1 | org_id)
build_intercept_only_formula <- function(outcome) {
  outcome_var <- get_outcome_var(outcome)
  paste0("I(", outcome_var, " > 0) ~ 1 + (1 | org_id)")
}


#' Build a seasonal-only binary formula (time trend + harmonics + ops + RE)
#' @param outcome "injuries", "fatalities", or "costs"
#' @return Formula string
build_seasonal_only_formula <- function(outcome) {
  outcome_var <- get_outcome_var(outcome)
  paste0("I(", outcome_var, " > 0) ~ yearmonth_num_c + sin_month + cos_month + (1 | org_id)")
}


#' Build all baseline comparison formulas for CV
#'
#' Returns the full ladder: full (with climate), no-climate, seasonal-only,
#' intercept-only.
#'
#' @param outcome "injuries", "fatalities", or "costs"
#' @param climate_var Climate variable name for the full model
#' @return List of lists, each with $formula (string) and $label (string)
build_baseline_formulas <- function(outcome, climate_var) {
  list(
    list(formula = build_binary_formula(outcome, climate_var),
         label   = climate_var),
    list(formula = build_binary_formula(outcome, climate_var = NULL),
         label   = "no_climate"),
    list(formula = build_seasonal_only_formula(outcome),
         label   = "seasonal_only"),
    list(formula = build_intercept_only_formula(outcome),
         label   = "intercept_only")
  )
}

get_outcome_config <- function(outcome) {
  if (!outcome %in% names(OUTCOME_VARS)) {
    stop(sprintf("Unknown outcome: '%s'", outcome))
  }
  OUTCOME_VARS[[outcome]]
}
