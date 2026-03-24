# common/config.R
# NRC Nuclear Multiverse Analysis — Constants and Configuration
# Sourced by: 1_nrc_multiverse.R, 2_nrc_cv.R, 3_nrc_plots.R
#
# Mike Rose — Safety Climate Analysis

# ==============================================================================
# WINDOW SPECIFICATIONS (same as rail — these are feature-pipeline settings)
# ==============================================================================

WINDOW_SPECS <- list(
  sma = tibble::tibble(window_size = c(3, 5, 10, 20)),
  ewma = tibble::tribble(
    ~lag_days, ~halflife_days,
    180,  60,
    180,  90,
    360,  90,
    360,  180,
    540,  180,
    540,  270,
    720,  180,
    720,  360,
    900,  270,
    900,  450,
    1080, 360,
    1080, 540
  )
)

# ==============================================================================
# OPERATIONAL VARIABLES — NRC quarterly covariates
# ==============================================================================

# These are the between/within decomposed covariates from nrc_operational_data.py
# Available after processing: action_matrix_col, findings_count, findings_nongreen_count
# Phase 2 (power status): capacity_factor, power_std, n_shutdowns
NRC_OPS_VARS <- c("action_matrix_col", "findings_count")

# Rolling window parameters for between/within decomposition
# (already computed in Python — these are for documentation/reference)
NRC_OPS_ROLL_K <- 4L    # 4-quarter rolling window
NRC_OPS_LAG_K  <- 1L    # 1-quarter lag
NRC_OPS_MIN_HIST <- 2L  # Minimum 2 quarters of history

# ==============================================================================
# OUTCOME VARIABLES
# ==============================================================================

# NRC outcomes and their model families
# binary_scram:    I(scram_ord > 0) ~ ...   binomial (glmer)
# ordinal_scram:   scram_ord ~ ...           cumulative link (clmm)
# emerg_class:     emerg_class_ord ~ ...     cumulative link (clmm)

OUTCOME_VARS <- list(
  binary_scram = list(
    var       = "scram_binary",       # 0/1: any scram vs. none
    family    = "binomial",
    model_fn  = "glmer",
    label     = "Binary scram (any)"
  ),
  ordinal_scram = list(
    var       = "scram_ord",          # 0/1/2: none/manual/auto
    family    = "cumulative",
    model_fn  = "clmm",
    label     = "Ordinal scram severity"
  ),
  emerg_class = list(
    var       = "emerg_class_ord",    # 0-4: none/UE/Alert/SAE/GE
    family    = "cumulative",
    model_fn  = "clmm",
    label     = "Emergency classification"
  )
)

#' Get outcome configuration
#' @param outcome One of names(OUTCOME_VARS)
#' @return List with var, family, model_fn, label
get_outcome_config <- function(outcome) {
  if (!outcome %in% names(OUTCOME_VARS)) {
    stop(sprintf("Unknown outcome: '%s'. Must be one of: %s",
                 outcome, paste(names(OUTCOME_VARS), collapse = ", ")))
  }
  OUTCOME_VARS[[outcome]]
}

# ==============================================================================
# MODEL FORMULA TEMPLATES
# ==============================================================================

# CLIMATE_VAR is replaced at runtime with the actual windowed variable name.
# Year fixed effects provide non-parametric trend control.
# Operational covariates: between (stable regulatory level) and within (recent change).
# facility is the random effect grouping variable.

# Base formula components
.temporal_controls <- "factor(year)"
.ops_covariates <- paste0(
  "action_matrix_col_between + action_matrix_col_within + ",
  "findings_count_between + findings_count_within"
)

MODEL_FORMULAS <- list(
  # Binary scram: glmer(binomial)
  binary_scram = paste0(
    "scram_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + ",
    "CLIMATE_VAR + (1 | facility)"
  ),

  # Ordinal scram: clmm — note: no explicit LHS for clmm, it's set separately
  # clmm formula: scram_ord_factor ~ fixed_effects + (1 | facility)
  ordinal_scram = paste0(
    "scram_ord_factor ~ ", .temporal_controls, " + ",
    .ops_covariates, " + ",
    "CLIMATE_VAR"
  ),

  # Emergency class: clmm
  emerg_class = paste0(
    "emerg_class_ord_factor ~ ", .temporal_controls, " + ",
    .ops_covariates, " + ",
    "CLIMATE_VAR"
  )
)

# Formula without climate variable (for baseline comparisons in CV)
MODEL_FORMULAS_NO_CLIMATE <- list(
  binary_scram = paste0(
    "scram_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | facility)"
  ),
  ordinal_scram = paste0(
    "scram_ord_factor ~ ", .temporal_controls, " + ",
    .ops_covariates
  ),
  emerg_class = paste0(
    "emerg_class_ord_factor ~ ", .temporal_controls, " + ",
    .ops_covariates
  )
)

# Intercept-only formula (for CV baseline)
MODEL_FORMULAS_INTERCEPT <- list(
  binary_scram = "scram_binary ~ 1 + (1 | facility)"
)

# Seasonal-only formula (temporal controls + ops, no climate — for CV)
MODEL_FORMULAS_SEASONAL <- list(
  binary_scram = paste0(
    "scram_binary ~ ", .temporal_controls, " + ",
    .ops_covariates, " + (1 | facility)"
  )
)

# ==============================================================================
# HELPER: Build formula with specific climate variable
# ==============================================================================

#' Replace CLIMATE_VAR placeholder with actual variable name
#' @param formula_template Formula string with CLIMATE_VAR placeholder
#' @param climate_var Actual climate variable name
#' @return Formula string with substitution
build_formula <- function(formula_template, climate_var) {
  gsub("CLIMATE_VAR", climate_var, formula_template, fixed = TRUE)
}

# ==============================================================================
# CLIMATE VARIABLE DETECTION
# ==============================================================================

#' Get all windowed climate variable names from a data frame
#' @param df Data frame with climate columns
#' @return Character vector of climate variable names
get_climate_vars <- function(df) {
  c(
    names(df) %>% stringr::str_subset("overall_final_score_sma_"),
    names(df) %>% stringr::str_subset("overall_final_score_ewmaLAG_")
  )
}
