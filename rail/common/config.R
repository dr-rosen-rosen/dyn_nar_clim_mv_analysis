# common/config.R
# Shared constants and configuration for rail safety multiverse analysis
# Sourced by: 1_rail_multiverse_glmmtmb_w_ops.R, 2_rail_cv_binary.R

# ==============================================================================
# WINDOW SPECIFICATIONS
# ==============================================================================

WINDOW_SPECS <- list(
  sma = tibble::tibble(window_size = c(3,10,20)),
  ewma = tibble::tribble(
    ~lag_days, ~halflife_days,
    # 180,  60,
    180,  90,
    # 360,  90,
    360,  180,
    # 540,  180,
    # 540,  270,
    # 720,  180,
    720,  360#,
    # 900,  270,
    # 900,  450,
    # 1080, 360,
    # 1080, 540
  )
)

# ==============================================================================
# OPERATIONAL VARIABLES CONFIGURATION
# ==============================================================================

OPS_VARS <- c("train_miles", "passenger_miles", "staff_hours")
OPS_ROLL_K <- 12L    # 12-month rolling window for between/within decomposition
OPS_LAG_K <- 1L      # 1-month lag to ensure temporal ordering
OPS_MIN_HIST <- 6L   # Minimum 6 months of history before computing features

# ==============================================================================
# MODEL FORMULA TEMPLATES
# ==============================================================================

# Conditional and zero-inflation formula templates for each outcome.
# CLIMATE_VAR is a placeholder replaced at runtime with the actual variable name.
MODEL_FORMULAS <- list(
  injuries = list(
    conditional = paste0(
      "total_persons_injured ~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    ),
    zero_inflation = paste0(
      "~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    )
  ),
  fatalities = list(
    conditional = paste0(
      "total_persons_killed ~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    ),
    zero_inflation = paste0(
      "~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    )
  ),
  costs = list(
    conditional = paste0(
      "total_damage_cost ~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    ),
    zero_inflation = paste0(
      "~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    )
  )
)

# ==============================================================================
# OUTCOME VARIABLE MAPPING
# ==============================================================================

OUTCOME_VARS <- c(
  injuries   = "total_persons_injured",
  fatalities = "total_persons_killed",
  costs      = "total_damage_cost"
)

#' Get outcome variable name from outcome label
#' @param outcome "injuries", "fatalities", or "costs"
#' @return Character string of the outcome column name
get_outcome_var <- function(outcome) {
  if (!outcome %in% names(OUTCOME_VARS)) {
    stop(sprintf("Unknown outcome: '%s'. Must be one of: %s",
                 outcome, paste(names(OUTCOME_VARS), collapse = ", ")))
  }
  OUTCOME_VARS[[outcome]]
}
