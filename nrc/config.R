# =============================================================================
# nrc/config.R — NRC Nuclear-Specific Constants and Configuration
# =============================================================================
#
# Industry-specific settings for the NRC nuclear multiverse analysis.
# Shared functions are in common/data_prep.R.
# =============================================================================


# =============================================================================
# WINDOW SPECIFICATIONS (shared with rail — identical structure)
# =============================================================================

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

NRC_OPS_VARS   <- c("capacity_factor", "power_std")
NRC_OPS_ROLL_K <- 4L    # 4-quarter rolling window
NRC_OPS_LAG_K  <- 1L    # 1-quarter lag
NRC_OPS_MIN_HIST <- 2L  # Minimum 2 quarters of history


# =============================================================================
# OUTCOME VARIABLES
# =============================================================================
# PRIMARY:
#   pct_power_loss:  Hurdle model (glmmTMB) — ~14% positive, ZI gate
#   emerg_binary:    Any emergency declaration vs. none (glmer binomial, ~5% rate)
# SECONDARY:
#   ordinal_scram:   Retained for completeness — weak signal
# DROPPED:
#   binary_scram:    Facility RE variance → 0 with ops covariates (singular)

OUTCOME_VARS <- list(
  pct_power_loss = list(
    var    = "pct_power_loss",
    family = "hurdle",
    model_fn = "glmmTMB",
    label  = "Pct power loss (hurdle)"
  ),
  emerg_binary = list(
    var    = "emerg_binary",
    family = "binomial",
    model_fn = "glmer",
    label  = "Emergency declaration (binary)"
  )#,
  # ordinal_scram = list(
  #   var    = "scram_ord_factor",
  #   family = "cumulative",
  #   model_fn = "clmm",
  #   label  = "Ordinal scram severity"
  # )
)

get_outcome_config <- function(outcome) {
  if (!outcome %in% names(OUTCOME_VARS)) {
    stop(sprintf("Unknown outcome: '%s'. Must be one of: %s",
                 outcome, paste(names(OUTCOME_VARS), collapse = ", ")))
  }
  OUTCOME_VARS[[outcome]]
}


# =============================================================================
# MODEL FORMULA TEMPLATES
# =============================================================================

# NRC uses year fixed effects + sin/cos seasonality (not linear yearmonth_num_c)
# to avoid complete separation in sparse years for ordinal models.
.temporal_controls <- "yearmonth_num_c + sin_month + cos_month"

.ops_covariates <- paste0(
  "capacity_factor_between + capacity_factor_within + ",
  "power_std_between + power_std_within"
)

# Full formulas with climate variable
MODEL_FORMULAS <- list(
  pct_power_loss = list(
    binary = paste0("power_loss_binary ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                    .ops_covariates, " + (1 | facility)"),
    gamma  = paste0("pct_power_loss ~ CLIMATE_VAR + ", .temporal_controls, " + ",
                    .ops_covariates, " + (1 | facility)")
  ),
  emerg_binary = paste0(
    "emerg_binary ~ CLIMATE_VAR + ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | facility)"
  ),
  ordinal_scram = paste0(
    "scram_ord_factor ~ CLIMATE_VAR + ", .temporal_controls, " + ",
    .ops_covariates
  )
)

# No-climate formulas (for CV baselines)
MODEL_FORMULAS_NO_CLIMATE <- list(
  pct_power_loss = list(
    binary = paste0("power_loss_binary ~ ", .temporal_controls, " + ",
                    .ops_covariates, " + (1 | facility)"),
    gamma  = paste0("pct_power_loss ~ ", .temporal_controls, " + ",
                    .ops_covariates, " + (1 | facility)")
  ),
  emerg_binary = paste0(
    "emerg_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | facility)"
  ),
  ordinal_scram = paste0(
    "scram_ord_factor ~ ", .temporal_controls, " + ",
    .ops_covariates
  )
)

# Intercept-only formulas (for CV baseline)
MODEL_FORMULAS_INTERCEPT <- list(
  pct_power_loss = list(
    binary = paste0("power_loss_binary ~ 1 + (1 | facility)"),
    gamma  = paste0("pct_power_loss ~ 1 + (1 | facility)")
  ),
  emerg_binary = "emerg_binary ~ 1 + (1 | facility)"
)

# Seasonal + ops formulas (no climate — for CV baseline)
# Identical to NO_CLIMATE but kept as a separate object for CV code that
# references MODEL_FORMULAS_SEASONAL explicitly.
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

MIN_REPORTS_DEFAULT <- 75L


# =============================================================================
# CV FORMULA BUILDERS
# =============================================================================
# Generalized for multiple NRC outcomes. Used by 2_nrc_cv.R.

#' Build the full model formula for a given outcome, substituting climate variable
#'
#' For simple outcomes (binary, ordinal), returns a single formula.
#' For hurdle outcomes, returns the binary gate formula for CV purposes.
#'
#' @param outcome Outcome name
#' @param climate_var Climate variable name
#' @return Formula object
build_outcome_formula <- function(outcome, climate_var) {
  template <- MODEL_FORMULAS[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS), collapse = ", ")))
  }

  # Handle list-valued entries (hurdle models)
  if (is.list(template) && !is.null(names(template))) {
    if ("binary" %in% names(template)) {
      fml_str <- template$binary
    } else if (length(template) == 1) {
      fml_str <- template[[1]]
    } else {
      fml_str <- template[[1]]
    }
  } else {
    fml_str <- template
  }

  as.formula(build_formula(fml_str, climate_var))
}


#' Build intercept-only baseline formula for CV
#' @param outcome Outcome name
#' @return Formula object
build_intercept_formula <- function(outcome) {
  template <- MODEL_FORMULAS_INTERCEPT[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS_INTERCEPT entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS_INTERCEPT), collapse = ", ")))
  }

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


#' Build seasonal+ops baseline formula (no climate) for CV
#' @param outcome Outcome name
#' @return Formula object
build_seasonal_formula <- function(outcome) {
  template <- MODEL_FORMULAS_SEASONAL[[outcome]]
  if (is.null(template)) {
    stop(sprintf("No MODEL_FORMULAS_SEASONAL entry for outcome: '%s'. Available: %s",
                 outcome, paste(names(MODEL_FORMULAS_SEASONAL), collapse = ", ")))
  }

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


#' Build a binary formula (legacy compatibility wrapper)
#' @param climate_var Climate variable name (or NULL for no-climate)
#' @return Formula object
build_binary_formula <- function(climate_var = NULL) {
  if (is.null(climate_var)) {
    as.formula(MODEL_FORMULAS_NO_CLIMATE$binary_scram)
  } else {
    build_outcome_formula("binary_scram", climate_var)
  }
}


#' Build the complete set of baseline formulas for CV comparison
#'
#' @param outcome Outcome name
#' @param climate_var Climate variable name
#' @return List of lists, each with $formula and $label
build_baseline_formulas <- function(outcome, climate_var) {
  list(
    list(formula = build_intercept_formula(outcome), label = "intercept_only"),
    list(formula = build_seasonal_formula(outcome),  label = "seasonal_ops"),
    list(formula = build_outcome_formula(outcome, climate_var), label = "full")
  )
}
