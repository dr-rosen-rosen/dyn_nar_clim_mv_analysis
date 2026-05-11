# =============================================================================
# aviation/panel_fit_models.R — Aviation Panel-Rate Model Fitting
# =============================================================================
#
# Fits NB GLMM rate models on the airport × month panel produced by
# prepare_aviation_panel_data(). Mirrors rail/NRC/PHMSA fit_models.
#
# Outcomes (all NB; offset = log(departures)):
#   rate_accidents          n_accidents       per departure
#   rate_inj_serious_fatal  sum_serious_fatal per departure
#   rate_fatalities         sum_fatalities    per departure
#
# Mundlak ops covariates (between/within): seats, passengers. NOT departures —
# that's the offset; including its decomposition would be near-collinear.
#
# Requires aviation/config.R sourced first.
# =============================================================================

suppressPackageStartupMessages({
  library(glmmTMB)
  library(broom.mixed)
  library(dplyr)
})


PANEL_OUTCOME_VARS_AVIATION <- list(
  # --- AIDS-derived event-count outcomes ---
  # AIDS gives a much broader event base than NTSB; rate_aids_all is the
  # primary count signal, with the incident-only variant isolating the
  # operational-event subset (which a priori should be most climate-driven).
  rate_aids_all = list(
    var      = "n_aids_all",
    offset   = "departures",
    label    = "AIDS event rate per departure (airport-month, all event types)",
    family   = "nbinom2"
  ),
  rate_aids_incidents_only = list(
    var      = "n_aids_incidents",
    offset   = "departures",
    label    = "AIDS incident rate per departure (incident-only, no accidents)",
    family   = "nbinom2"
  ),

  # --- NTSB-derived severity outcomes ---
  # Kept on NTSB because investigator-confirmed casualty counts are the
  # canonical post-event source (per design decision: AIDS for breadth,
  # NTSB for casualties).
  rate_accidents = list(
    var      = "n_accidents",
    offset   = "departures",
    label    = "NTSB accident rate per departure (airport-month)",
    family   = "nbinom2"
  ),
  rate_inj_serious_fatal = list(
    var      = "sum_serious_fatal",
    offset   = "departures",
    label    = "Serious + fatal injuries per departure (NTSB)",
    family   = "nbinom2"
  ),
  rate_fatalities = list(
    var      = "sum_fatalities",
    offset   = "departures",
    label    = "Fatalities per departure (NTSB)",
    family   = "nbinom2"
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


#' Build the formula for an aviation panel-rate model.
build_aviation_panel_formula <- function(outcome, climate_var,
                                          re_term = "(1 | airport_id)") {
  cfg <- PANEL_OUTCOME_VARS_AVIATION[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)

  bw_terms <- c("seats_between", "seats_within",
                "passengers_between", "passengers_within")

  rhs_parts <- c(
    climate_var,
    "yearmonth_num_c", "sin_month", "cos_month",
    bw_terms,
    sprintf("offset(log(%s))", cfg$offset),
    re_term
  )

  as.formula(paste(cfg$var, "~", paste(rhs_parts, collapse = " + ")))
}


#' Fit one aviation panel-rate GLMM on an airport-month panel.
fit_single_aviation_panel_model <- function(data, climate_var, config_id,
                                             outcome = "rate_accidents",
                                             family = NULL) {

  cfg <- PANEL_OUTCOME_VARS_AVIATION[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)
  if (is.null(family)) family <- .resolve_panel_family(cfg$family %||% "nbinom2")

  outcome_var <- cfg$var
  exposure    <- cfg$offset
  bw_cols     <- c("seats_between", "seats_within",
                   "passengers_between", "passengers_within")

  required_cols <- c(climate_var, outcome_var, exposure,
                     "yearmonth_num_c", "sin_month", "cos_month",
                     "airport_id", bw_cols)
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
      !is.na(airport_id),
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

  fml <- build_aviation_panel_formula(outcome, climate_var)
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
      n_orgs    = n_distinct(d$airport_id),
      n_periods = n_distinct(d$yearmonth),
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
