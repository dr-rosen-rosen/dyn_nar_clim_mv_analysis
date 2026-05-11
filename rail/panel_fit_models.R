# =============================================================================
# rail/panel_fit_models.R — Rail Panel-Rate Model Fitting
# =============================================================================
#
# Fits negative-binomial GLMM rate models on the org × month panel produced
# by prepare_rail_panel_data().
#
# Model form (template):
#   outcome ~ CLIMATE_VAR + temporal_controls + non_offset_ops_covariates
#             + offset(log(EXPOSURE)) + (1 | org_id)
#
# Three default outcomes:
#   rate_accidents : n_accidents,  offset = log(train_miles)
#   rate_injuries  : sum_injured,  offset = log(staff_hours)
#   rate_fatalities: sum_killed,   offset = log(train_miles)
#
# Climate variable name is one of the panel-windowed columns (e.g.
# "overall_final_score_ewma_540_270").
#
# Requires: rail/config.R sourced first.
# Dependencies: glmmTMB, broom.mixed, dplyr
# =============================================================================

suppressPackageStartupMessages({
  library(glmmTMB)
  library(broom.mixed)
  library(dplyr)
})


# =============================================================================
# PANEL OUTCOME REGISTRY
# =============================================================================

PANEL_OUTCOME_VARS <- list(
  rate_accidents = list(
    var      = "n_accidents",
    offset   = "train_miles",
    label    = "Accident rate per train-mile",
    family   = "nbinom2"
  ),
  rate_injuries = list(
    var      = "sum_injured",
    offset   = "staff_hours",
    label    = "Injury rate per staff-hour",
    family   = "nbinom2"
  ),
  rate_fatalities = list(
    var      = "sum_killed",
    offset   = "train_miles",
    label    = "Fatality rate per train-mile",
    family   = "nbinom2"
  )
)

#' Build the formula for a panel-rate model
#'
#' Uses temporal controls + the between/within decomposition of all OPS_VARS
#' EXCEPT the one chosen as the offset. The offset enters as
#' offset(log(<exposure>)).
#'
#' @param outcome Key in PANEL_OUTCOME_VARS.
#' @param climate_var Name of the climate variable column.
#' @param org_re Random effect spec (default "(1 | org_id)").
#' @return A formula object.
build_panel_formula <- function(outcome, climate_var, org_re = "(1 | org_id)") {
  cfg <- PANEL_OUTCOME_VARS[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)

  exposure <- cfg$offset
  non_offset_ops <- setdiff(OPS_VARS, exposure)
  bw_terms <- c(paste0(non_offset_ops, "_between"),
                paste0(non_offset_ops, "_within"))

  rhs_parts <- c(
    climate_var,
    "yearmonth_num_c", "sin_month", "cos_month",
    bw_terms,
    sprintf("offset(log(%s))", exposure),
    org_re
  )

  as.formula(paste(cfg$var, "~", paste(rhs_parts, collapse = " + ")))
}


# =============================================================================
# SINGLE PANEL FIT
# =============================================================================

#' Fit one panel-rate GLMM
#'
#' @param data Panel data frame from prepare_rail_panel_data().
#' @param climate_var Name of the climate variable column to use.
#' @param config_id Configuration ID for tracking.
#' @param outcome Key in PANEL_OUTCOME_VARS.
#' @param family glmmTMB family. Default: nbinom2.
#' @return Named list with model coefficients, fit stats, and convergence flag.
fit_single_rail_panel_model <- function(data, climate_var, config_id,
                                        outcome = "rate_accidents",
                                        family = NULL) {

  cfg <- PANEL_OUTCOME_VARS[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)
  if (is.null(family)) family <- glmmTMB::nbinom2()

  outcome_var <- cfg$var
  exposure <- cfg$offset
  non_offset_ops <- setdiff(OPS_VARS, exposure)
  bw_cols <- c(paste0(non_offset_ops, "_between"),
               paste0(non_offset_ops, "_within"))

  # Filter to complete cases for this fit
  required_cols <- c(climate_var, outcome_var, exposure,
                     "yearmonth_num_c", "sin_month", "cos_month",
                     "org_id", bw_cols)
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    return(list(
      config_id = config_id, climate_var = climate_var, outcome = outcome,
      status = "failed",
      error = sprintf("Missing columns: %s", paste(missing, collapse = ", ")),
      n_obs = NA_integer_
    ))
  }

  d <- data %>%
    filter(
      !is.na(.data[[climate_var]]),
      !is.na(.data[[outcome_var]]),
      !is.na(.data[[exposure]]),
      .data[[exposure]] > 0,
      !is.na(yearmonth_num_c), !is.na(sin_month), !is.na(cos_month),
      !is.na(org_id),
      if_all(all_of(bw_cols), ~ !is.na(.x))
    )

  if (nrow(d) < 100) {
    return(list(
      config_id = config_id, climate_var = climate_var, outcome = outcome,
      status = "failed",
      error = sprintf("Insufficient complete cases: %d", nrow(d)),
      n_obs = nrow(d)
    ))
  }

  if (sum(d[[outcome_var]] > 0) < 10) {
    return(list(
      config_id = config_id, climate_var = climate_var, outcome = outcome,
      status = "failed",
      error = sprintf("Too few non-zero outcome periods: %d", sum(d[[outcome_var]] > 0)),
      n_obs = nrow(d)
    ))
  }

  fml <- build_panel_formula(outcome, climate_var)
  message("Panel formula: ", deparse1(fml))

  res <- tryCatch({
    fit <- suppressWarnings(glmmTMB::glmmTMB(fml, data = d, family = family))
    fe <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
    re <- broom.mixed::tidy(fit, effects = "ran_pars")

    safe <- function(term, col) {
      row <- fe %>% filter(term == !!term)
      if (nrow(row) > 0) row[[col]][1] else NA_real_
    }

    # Family label + Tweedie power (NA for non-Tweedie); kept uniform across
    # tracks so the panel report appendix can show one schema.
    family_label <- if (inherits(family, "family")) family$family else as.character(family)
    tweedie_p <- if (!is.null(family_label) && family_label == "tweedie") {
      psi <- tryCatch(fit$fit$parfull[["thetaf"]], error = function(e) NA_real_)
      if (is.na(psi)) NA_real_ else 1 + plogis(psi)
    } else NA_real_

    out <- list(
      config_id    = config_id,
      climate_var  = climate_var,
      outcome      = outcome,
      offset_var   = exposure,
      family       = family_label %||% "nbinom2",
      tweedie_power = tweedie_p,
      status       = if (isTRUE(fit$sdr$pdHess)) "success" else "convergence_warning",

      climate_estimate = safe(climate_var, "estimate"),
      climate_se       = safe(climate_var, "std.error"),
      climate_pval     = safe(climate_var, "p.value"),
      climate_ci_low   = safe(climate_var, "conf.low"),
      climate_ci_high  = safe(climate_var, "conf.high"),

      AIC       = AIC(fit),
      BIC       = BIC(fit),
      logLik    = as.numeric(logLik(fit)),
      n_obs     = nrow(d),
      n_orgs    = n_distinct(d$org_id),
      n_periods = n_distinct(d$yearmonth),
      n_zero_outcome = sum(d[[outcome_var]] == 0),
      pct_zero_outcome = 100 * mean(d[[outcome_var]] == 0),
      total_outcome    = sum(d[[outcome_var]]),
      total_exposure   = sum(d[[exposure]]),
      mean_rate_per_unit = sum(d[[outcome_var]]) / sum(d[[exposure]]),
      random_intercept_sd = {
        rrow <- re %>% filter(term == "sd__(Intercept)")
        if (nrow(rrow) > 0) rrow$estimate[1] else NA_real_
      }
    )

    # Capture the non-offset ops effects too, for comparison with event-level.
    for (v in bw_cols) {
      out[[paste0(v, "_estimate")]] <- safe(v, "estimate")
      out[[paste0(v, "_se")]]       <- safe(v, "std.error")
      out[[paste0(v, "_pval")]]     <- safe(v, "p.value")
    }

    out
  }, error = function(e) {
    list(
      config_id = config_id, climate_var = climate_var, outcome = outcome,
      status = "failed",
      error = as.character(e$message),
      n_obs = nrow(d)
    )
  })

  res
}
