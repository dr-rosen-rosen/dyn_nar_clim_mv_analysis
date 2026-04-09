# common/formulas.R
# NRC Formula Builders — Generalized for Multiple Outcomes
#
# Provides formula construction for CV and multiverse analysis.
# Works with any outcome defined in MODEL_FORMULAS (from config.R).
#
# Requires: config.R to be sourced first (provides MODEL_FORMULAS,
#   MODEL_FORMULAS_NO_CLIMATE, MODEL_FORMULAS_INTERCEPT, MODEL_FORMULAS_SEASONAL)
#
# Mike Rose — Safety Climate Analysis


# ==============================================================================
# CORE HELPER
# ==============================================================================

#' Substitute CLIMATE_VAR placeholder in a formula template string
#' @param template Formula template string containing "CLIMATE_VAR"
#' @param climate_var Actual climate variable name
#' @return Formula string with substitution applied
build_formula <- function(template, climate_var) {
  gsub("CLIMATE_VAR", climate_var, template, fixed = TRUE)
}


# ==============================================================================
# OUTCOME-GENERIC FORMULA BUILDERS
# ==============================================================================

#' Build the full model formula for a given outcome, substituting climate variable
#'
#' For simple outcomes (binary, ordinal), returns a single formula.
#' For hurdle outcomes, returns the formula keyed by the outcome name in MODEL_FORMULAS.
#'
#' @param outcome Outcome name (e.g., "emerg_binary", "power_loss_pct", "ordinal_scram")
#' @param climate_var Climate variable name
#' @return Formula object
build_outcome_formula <- function(outcome, climate_var) {
  template <- MODEL_FORMULAS[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS), collapse = ", ")))
  }

  # Handle list-valued entries (hurdle models have $cond, $zi, $binary sub-entries)
  if (is.list(template) && !is.null(names(template))) {
    # For CV, we typically want the single/binary formula
    # Try 'binary' key first, then 'cond', then use the first entry
    if ("binary" %in% names(template)) {
      fml_str <- template$binary
    } else if (length(template) == 1) {
      fml_str <- template[[1]]
    } else {
      # Default to first entry
      fml_str <- template[[1]]
    }
  } else {
    fml_str <- template
  }

  as.formula(build_formula(fml_str, climate_var))
}


#' Build intercept-only baseline formula for a given outcome
#'
#' @param outcome Outcome name (default: "binary_scram" for backward compatibility)
#' @return Formula object
build_intercept_formula <- function(outcome = "binary_scram") {
  template <- MODEL_FORMULAS_INTERCEPT[[outcome]]

  if (is.null(template)) {
    # Try to find a matching entry — some configs use different keys
    available <- names(MODEL_FORMULAS_INTERCEPT)
    stop(sprintf("No MODEL_FORMULAS_INTERCEPT entry for outcome: '%s'. Available: %s",
                 outcome, paste(available, collapse = ", ")))
  }

  # Handle list-valued entries (hurdle)
  if (is.list(template) && !is.null(names(template))) {
    if ("binary" %in% names(template)) {
      fml_str <- template$binary
    } else {
      fml_str <- template[[1]]
    }
  } else {
    fml_str <- template
  }

  as.formula(fml_str)
}


#' Build seasonal/ops baseline formula (no climate) for a given outcome
#'
#' @param outcome Outcome name (default: "binary_scram" for backward compatibility)
#' @return Formula object
build_seasonal_formula <- function(outcome = "binary_scram") {
  template <- MODEL_FORMULAS_SEASONAL[[outcome]]

  if (is.null(template)) {
    available <- names(MODEL_FORMULAS_SEASONAL)
    stop(sprintf("No MODEL_FORMULAS_SEASONAL entry for outcome: '%s'. Available: %s",
                 outcome, paste(available, collapse = ", ")))
  }

  # Handle list-valued entries (hurdle)
  if (is.list(template) && !is.null(names(template))) {
    if ("binary" %in% names(template)) {
      fml_str <- template$binary
    } else {
      fml_str <- template[[1]]
    }
  } else {
    fml_str <- template
  }

  as.formula(fml_str)
}


# ==============================================================================
# BACKWARD-COMPATIBLE ALIASES (binary_scram specific)
# ==============================================================================

#' Build a binary scram formula (legacy — use build_outcome_formula instead)
#' @param climate_var Climate variable name (or NULL for no-climate model)
#' @return Formula object
build_binary_formula <- function(climate_var = NULL) {
  if (is.null(climate_var)) {
    as.formula(MODEL_FORMULAS_NO_CLIMATE$binary_scram)
  } else {
    build_outcome_formula("binary_scram", climate_var)
  }
}


# ==============================================================================
# BASELINE FORMULA SET BUILDER (for CV)
# ==============================================================================

#' Build the complete set of baseline formulas for CV comparison
#'
#' Returns intercept-only, seasonal+ops, and full (with climate) formulas
#' for a given outcome. Used by cv_with_baselines().
#'
#' @param outcome Outcome name
#' @param climate_var Climate variable name
#' @return List of lists, each with $formula (string or formula) and $label
build_baseline_formulas <- function(outcome, climate_var) {
  list(
    list(
      formula = build_intercept_formula(outcome),
      label   = "intercept_only"
    ),
    list(
      formula = build_seasonal_formula(outcome),
      label   = "seasonal_ops"
    ),
    list(
      formula = build_outcome_formula(outcome, climate_var),
      label   = "full"
    )
  )
}
