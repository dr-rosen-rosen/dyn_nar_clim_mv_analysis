# =============================================================================
# nrc/panel_fit_models.R — NRC Panel-Rate Model Fitting
# =============================================================================
#
# Fits negative-binomial GLMM rate models on the facility × quarter panel
# produced by prepare_nrc_panel_data().
#
# Model form (template):
#   outcome ~ CLIMATE_VAR + temporal_controls + power_std_between/within
#             + offset(log(days_at_power_total)) + (1 | facility_site)
#
# Default outcomes:
#   rate_lers   : n_lers,   offset = log(days_at_power_total)
#   rate_emerg  : n_emerg,  offset = log(days_at_power_total)
#
# capacity_factor is intentionally NOT in the RHS — it equals
# days_at_power / total_days, so including it alongside log(days_at_power)
# would be near-collinear. power_std (between/within) IS retained, since
# it is operational volatility, not exposure.
#
# Requires: nrc/config.R sourced first.
# Dependencies: glmmTMB, broom.mixed, dplyr
# =============================================================================

suppressPackageStartupMessages({
  library(glmmTMB)
  library(broom.mixed)
  library(dplyr)
})


PANEL_OUTCOME_VARS_NRC <- list(
  rate_lers = list(
    var      = "n_lers",
    offset   = "days_at_power_total",
    label    = "LER rate per reactor-day at power",
    family   = "nbinom2"
  ),
  rate_emerg = list(
    var      = "n_emerg",
    offset   = "days_at_power_total",
    label    = "Emergency-declaration rate per reactor-day at power",
    family   = "nbinom2"
  ),
  rate_scrams = list(
    var      = "n_scrams",
    offset   = "days_at_power_total",
    label    = "Scram rate per reactor-day at power",
    family   = "nbinom2"
  ),
  rate_pct_power_loss = list(
    var      = "sum_pct_power_loss",
    offset   = "days_at_power_total",
    label    = "Total %-power-loss per reactor-day at power",
    family   = "tweedie"
  )
)


#' Resolve a glmmTMB family object from a string family name.
#'
#' @param family_str One of "nbinom2", "tweedie", "poisson".
#' @return A glmmTMB family object.
.resolve_panel_family <- function(family_str) {
  switch(family_str,
    nbinom2 = glmmTMB::nbinom2(),
    nbinom1 = glmmTMB::nbinom1(),
    poisson = stats::poisson(link = "log"),
    tweedie = glmmTMB::tweedie(link = "log"),
    stop(sprintf("Unknown panel family: '%s'", family_str))
  )
}


#' Extract Tweedie variance power from a fitted glmmTMB model, or NA.
.extract_tweedie_power <- function(fit) {
  tryCatch({
    par <- fit$fit$parfull
    psi <- par[names(par) == "psi"]
    if (length(psi) == 0) return(NA_real_)
    # glmmTMB parameterization: p = 1 + plogis(psi), p in (1,2)
    1 + plogis(unname(psi[1]))
  }, error = function(e) NA_real_)
}


#' Build the formula for an NRC panel-rate model.
#'
#' Includes power_std between/within (operational volatility), temporal
#' controls, and a log(exposure) offset. Random intercept on facility_site.
#'
#' @param outcome Key in PANEL_OUTCOME_VARS_NRC.
#' @param climate_var Name of the climate variable column.
#' @param re_term Random effect spec (default "(1 | facility_site)").
#' @return A formula object.
build_nrc_panel_formula <- function(outcome, climate_var,
                                    re_term = "(1 | facility_site)") {
  cfg <- PANEL_OUTCOME_VARS_NRC[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)

  bw_terms <- c("power_std_mean_between", "power_std_mean_within")

  rhs_parts <- c(
    climate_var,
    "yearmonth_num_c", "sin_month", "cos_month",
    bw_terms,
    sprintf("offset(log(%s))", cfg$offset),
    re_term
  )

  as.formula(paste(cfg$var, "~", paste(rhs_parts, collapse = " + ")))
}


#' Fit one NRC panel-rate GLMM.
#'
#' @param data Panel data frame from prepare_nrc_panel_data().
#' @param climate_var Name of the climate variable column to use.
#' @param config_id Configuration ID for tracking.
#' @param outcome Key in PANEL_OUTCOME_VARS_NRC.
#' @param family glmmTMB family. Default: nbinom2.
#' @return Named list with model coefficients, fit stats, and convergence flag.
fit_single_nrc_panel_model <- function(data, climate_var, config_id,
                                       outcome = "rate_lers",
                                       family = NULL) {

  cfg <- PANEL_OUTCOME_VARS_NRC[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)
  if (is.null(family)) family <- .resolve_panel_family(cfg$family %||% "nbinom2")

  outcome_var <- cfg$var
  exposure    <- cfg$offset
  bw_cols     <- c("power_std_mean_between", "power_std_mean_within")

  required_cols <- c(climate_var, outcome_var, exposure,
                     "yearmonth_num_c", "sin_month", "cos_month",
                     "facility_site", bw_cols)
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
      !is.na(facility_site),
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

  fml <- build_nrc_panel_formula(outcome, climate_var)
  message("Panel formula: ", deparse1(fml))

  res <- tryCatch({
    fit <- suppressWarnings(glmmTMB::glmmTMB(fml, data = d, family = family))
    fe <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
    re <- broom.mixed::tidy(fit, effects = "ran_pars")

    safe <- function(term, col) {
      row <- fe %>% filter(term == !!term)
      if (nrow(row) > 0) row[[col]][1] else NA_real_
    }

    out <- list(
      config_id    = config_id,
      climate_var  = climate_var,
      outcome      = outcome,
      offset_var   = exposure,
      family       = cfg$family %||% "nbinom2",
      tweedie_power = if (identical(cfg$family, "tweedie")) .extract_tweedie_power(fit) else NA_real_,
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
      n_orgs    = n_distinct(d$facility_site),
      n_periods = n_distinct(d$quarter_start),
      n_zero_outcome   = sum(d[[outcome_var]] == 0),
      pct_zero_outcome = 100 * mean(d[[outcome_var]] == 0),
      total_outcome    = sum(d[[outcome_var]]),
      total_exposure   = sum(d[[exposure]]),
      mean_rate_per_unit = sum(d[[outcome_var]]) / sum(d[[exposure]]),
      random_intercept_sd = {
        rrow <- re %>% filter(term == "sd__(Intercept)")
        if (nrow(rrow) > 0) rrow$estimate[1] else NA_real_
      }
    )

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
