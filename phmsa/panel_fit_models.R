# =============================================================================
# phmsa/panel_fit_models.R — PHMSA Panel-Rate Model Fitting
# =============================================================================
#
# Fits NB / Tweedie GLMM rate models on the operator × year panel produced by
# prepare_phmsa_panel_data(). One pipeline_type at a time.
#
# Outcomes:
#   rate_incidents      n_incidents      ~ NB,      offset = log(total_miles)
#   rate_injuries       sum_injuries     ~ NB,      offset = log(total_miles)
#   rate_fatalities     sum_fatalities   ~ NB,      offset = log(total_miles)
#   rate_release_volume sum_release_bbls ~ Tweedie, offset = log(total_miles)
#
# Requires phmsa/config.R sourced first (for OPS_VARS-equivalents in the panel
# already named total_miles_between/total_miles_within).
# =============================================================================

suppressPackageStartupMessages({
  library(glmmTMB)
  library(broom.mixed)
  library(dplyr)
})


PANEL_OUTCOME_VARS_PHMSA <- list(
  rate_incidents = list(
    var      = "n_incidents",
    offset   = "total_miles",
    label    = "Incident rate per pipeline-mile (operator-year)",
    family   = "nbinom2"
  ),
  rate_injuries = list(
    var      = "sum_injuries",
    offset   = "total_miles",
    label    = "Injuries per pipeline-mile (operator-year)",
    family   = "nbinom2"
  ),
  rate_fatalities = list(
    var      = "sum_fatalities",
    offset   = "total_miles",
    label    = "Fatalities per pipeline-mile (operator-year)",
    family   = "nbinom2"
  ),
  rate_release_volume = list(
    var      = "sum_release_volume",
    offset   = "total_miles",
    label    = "Release volume per pipeline-mile (gas: MCF; HL: bbls)",
    family   = "tweedie"
  )
)


.resolve_panel_family <- function(family_str) {
  switch(family_str,
    nbinom2 = glmmTMB::nbinom2(),
    nbinom1 = glmmTMB::nbinom1(),
    poisson = stats::poisson(link = "log"),
    tweedie = glmmTMB::tweedie(link = "log"),
    stop(sprintf("Unknown panel family: '%s'", family_str))
  )
}


.extract_tweedie_power <- function(fit) {
  tryCatch({
    par <- fit$fit$parfull
    psi <- par[names(par) == "psi"]
    if (length(psi) == 0) return(NA_real_)
    1 + plogis(unname(psi[1]))
  }, error = function(e) NA_real_)
}


#' Build the formula for a PHMSA panel-rate model.
build_phmsa_panel_formula <- function(outcome, climate_var,
                                       re_term = "(1 | operator_id)") {
  cfg <- PANEL_OUTCOME_VARS_PHMSA[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)

  bw_terms <- c("total_miles_between", "total_miles_within")

  rhs_parts <- c(
    climate_var,
    "yearmonth_num_c",                # year-on-year linear trend (annual data)
    bw_terms,
    sprintf("offset(log(%s))", cfg$offset),
    re_term
  )

  as.formula(paste(cfg$var, "~", paste(rhs_parts, collapse = " + ")))
}


#' Fit one PHMSA panel-rate GLMM on an operator-year panel.
#'
#' @param data Operator-year panel from prepare_phmsa_panel_data().
#' @param climate_var Climate column to use.
#' @param config_id Config ID for tracking.
#' @param outcome Key in PANEL_OUTCOME_VARS_PHMSA.
#' @param family glmmTMB family override; default resolved from outcome cfg.
#' @return Named list of coefficients, fit stats, family info.
fit_single_phmsa_panel_model <- function(data, climate_var, config_id,
                                          outcome = "rate_incidents",
                                          family = NULL) {

  cfg <- PANEL_OUTCOME_VARS_PHMSA[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)
  if (is.null(family)) family <- .resolve_panel_family(cfg$family %||% "nbinom2")

  outcome_var <- cfg$var
  exposure    <- cfg$offset
  bw_cols     <- c("total_miles_between", "total_miles_within")

  required_cols <- c(climate_var, outcome_var, exposure,
                     "yearmonth_num_c", "operator_id", bw_cols)
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
      !is.na(yearmonth_num_c),
      !is.na(operator_id),
      if_all(all_of(bw_cols), ~ !is.na(.x))
    )

  if (nrow(d) < 50) {
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

  fml <- build_phmsa_panel_formula(outcome, climate_var)
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
      n_orgs    = n_distinct(d$operator_id),
      n_periods = n_distinct(d$year_start),
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
