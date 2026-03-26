# common/formulas.R
# Formula builders for binary (logistic) models used in cross-validation
# Sourced by: 2_rail_cv_binary.R (and potentially 1_rail_multiverse_glmmtmb_w_ops.R)
#
# Requires: common/config.R to be sourced first (for MODEL_FORMULAS, OUTCOME_VARS)

# ==============================================================================
# BINARY FORMULA BUILDERS
# ==============================================================================

#' Build a binary (logistic) formula from MODEL_FORMULAS template
#'
#' Takes the conditional formula string for a given outcome (which targets the
#' count), replaces the LHS with I(outcome > 0), and substitutes/removes the
#' climate variable placeholder.
#'
#' @param outcome "injuries", "fatalities", or "costs"
#' @param climate_var Name of the climate variable (or NULL for no-climate model)
#' @return A formula string for the binary logistic GLMM
build_binary_formula <- function(outcome, climate_var = NULL) {
  
  outcome_var <- get_outcome_var(outcome)
  
  # Start from the conditional formula template
  f_str <- MODEL_FORMULAS[[outcome]]$conditional
  
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


#' Build a seasonal-only binary formula (time trend + harmonics + RE)
#' @param outcome "injuries", "fatalities", or "costs"
#' @return Formula string with yearmonth_num_c + sin/cos harmonics + org RE
build_seasonal_only_formula <- function(outcome) {
  outcome_var <- get_outcome_var(outcome)
  paste0("I(", outcome_var, " > 0) ~ yearmonth_num_c + sin_month + cos_month + (1 | org_id)")
}


#' Build all four baseline comparison formulas
#' 
#' Returns a named list of formula specifications for the baseline ladder:
#'   full, no_climate, seasonal_only, intercept_only
#' 
#' @param outcome "injuries", "fatalities", or "costs"
#' @param climate_var Name of the climate variable for the full model
#' @return Named list of lists, each with $formula (string) and $label (string)
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
