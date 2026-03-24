# common/formulas.R
# NRC Nuclear Multiverse — Formula Builders for CV Baselines
# Sourced by: 2_nrc_cv.R
#
# Requires: common/config.R sourced first

# ==============================================================================
# BINARY FORMULA BUILDERS (for CV — binary scram only)
# ==============================================================================

#' Build a binary formula from the template, substituting climate variable
#' @param climate_var Climate variable name (or NULL for no-climate model)
#' @return Formula object
build_binary_formula <- function(climate_var = NULL) {
  if (is.null(climate_var)) {
    as.formula(MODEL_FORMULAS_NO_CLIMATE$binary_scram)
  } else {
    as.formula(build_formula(MODEL_FORMULAS$binary_scram, climate_var))
  }
}

#' Build intercept-only baseline formula
#' @return Formula object
build_intercept_formula <- function() {
  as.formula(MODEL_FORMULAS_INTERCEPT$binary_scram)
}

#' Build seasonal/ops baseline formula (no climate)
#' @return Formula object
build_seasonal_formula <- function() {
  as.formula(MODEL_FORMULAS_SEASONAL$binary_scram)
}
