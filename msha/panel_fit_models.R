# =============================================================================
# msha/panel_fit_models.R — MSHA Panel-Rate Model Fitting
# =============================================================================
#
# Fits negative-binomial / Tweedie GLMM rate models on the mine × quarter panel
# produced by prepare_msha_panel_data().
#
# Model form (template):
#   outcome ~ CLIMATE_VAR + yearmonth_num_c + sin_month + cos_month
#             + hours_within + offset(log(hours_worked)) + (1 | mine_id)
#
# Self-reported outcomes:
#   rate_accidents : n_accidents,   nbinom2
#   rate_injuries  : n_injuries,    nbinom2
#   rate_fatal     : n_fatal,       nbinom2
#   rate_days_lost : sum_days_lost, tweedie
# Externally-assessed (regulatory) outcomes:
#   rate_violations : violation_count, nbinom2
#   rate_ss         : ss_count,        nbinom2
#
# log(hours_worked) is the universal exposure offset. hours_within
# (within-mine volatility) is the only operational covariate — hours_between is
# collinear with the offset level and is excluded (mirrors NRC dropping
# capacity_factor against the days_at_power offset).
#
# Requires: msha/config.R sourced first.
# Dependencies: glmmTMB, broom.mixed, dplyr
# =============================================================================

suppressPackageStartupMessages({
  library(glmmTMB)
  library(broom.mixed)
  library(dplyr)
})


PANEL_OUTCOME_VARS_MSHA <- list(
  rate_accidents = list(
    var = "n_accidents", offset = "hours_worked", family = "nbinom2",
    label = "Reported-accident rate per employee-hour"
  ),
  rate_injuries = list(
    var = "n_injuries", offset = "hours_worked", family = "nbinom2",
    label = "Injury rate per employee-hour"
  ),
  rate_fatal = list(
    var = "n_fatal", offset = "hours_worked", family = "nbinom2",
    label = "Fatal-accident rate per employee-hour"
  ),
  rate_days_lost = list(
    var = "sum_days_lost", offset = "hours_worked", family = "tweedie",
    label = "Lost-workday rate per employee-hour"
  ),
  # ---- Detectability tiers (severity-graded reporting filter; see
  #      injury_detectability_supplemental_design.md §3.1). One rate model per
  #      tier, identical RHS, so the climate coefficients are comparable. The
  #      estimand is the ordered vector (beta_T1, beta_T2, beta_T3): the
  #      hypothesis is monotone-increasing with beta_T1 <= 0 < beta_T3.
  #      T1 is sparse (~1% of coal events) and unstable as a count -> recast as
  #      a binary `any_severe` outcome is a planned multiverse level (§6.1).
  #      rate_t0 (accident-only anchor) is registered but off by default
  #      (t0_inclusion multiverse switch), like rate_violations/rate_ss. ----
  rate_t1 = list(
    var = "n_t1", offset = "hours_worked", family = "nbinom2",
    label = "Detectability T1 (fatal/permanent-disability) rate per employee-hour"
  ),
  rate_t2 = list(
    var = "n_t2", offset = "hours_worked", family = "nbinom2",
    label = "Detectability T2 (lost-time) rate per employee-hour"
  ),
  rate_t3 = list(
    var = "n_t3", offset = "hours_worked", family = "nbinom2",
    label = "Detectability T3 (minor/discretionary) rate per employee-hour"
  ),
  rate_t0 = list(
    var = "n_t0", offset = "hours_worked", family = "nbinom2",
    label = "Detectability T0 (accident-only / no-injury) rate per employee-hour"
  ),
  # ---- Externally-assessed (regulatory) outcomes ----
  rate_violations = list(
    var = "violation_count", offset = "hours_worked", family = "nbinom2",
    label = "MSHA citation/violation rate per employee-hour"
  ),
  rate_ss = list(
    var = "ss_count", offset = "hours_worked", family = "nbinom2",
    label = "Significant-and-substantial violation rate per employee-hour"
  )
)


.resolve_panel_family <- function(family_str) {
  switch(family_str,
    nbinom2  = glmmTMB::nbinom2(),
    nbinom1  = glmmTMB::nbinom1(),
    poisson  = stats::poisson(link = "log"),
    tweedie  = glmmTMB::tweedie(link = "log"),
    binomial = stats::binomial(link = "logit"),
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


#' Build the formula for an MSHA panel-rate model.
#'
#' @param outcome Key in PANEL_OUTCOME_VARS_MSHA.
#' @param climate_var Name of the climate variable column.
#' @param re_term Random effect spec (default "(1 | mine_id)").
#' @return A formula object.
build_msha_panel_formula <- function(outcome, climate_var,
                                     re_term = "(1 | mine_id)") {
  cfg <- PANEL_OUTCOME_VARS_MSHA[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)

  rhs_parts <- c(
    climate_var,
    "yearmonth_num_c", "sin_month", "cos_month",
    "hours_within"
  )
  if (!is.null(cfg$offset)) {
    rhs_parts <- c(rhs_parts, sprintf("offset(log(%s))", cfg$offset))
  }
  rhs_parts <- c(rhs_parts, re_term)

  as.formula(paste(cfg$var, "~", paste(rhs_parts, collapse = " + ")))
}


#' Fit one MSHA panel-rate GLMM.
#'
#' @param data Panel data frame from prepare_msha_panel_data().
#' @param climate_var Name of the climate variable column to use.
#' @param config_id Configuration ID for tracking.
#' @param outcome Key in PANEL_OUTCOME_VARS_MSHA.
#' @param family glmmTMB family. Default resolved from the outcome spec.
#' @return Named list with model coefficients, fit stats, and convergence flag.
fit_single_msha_panel_model <- function(data, climate_var, config_id,
                                        outcome = "rate_accidents",
                                        family = NULL) {

  cfg <- PANEL_OUTCOME_VARS_MSHA[[outcome]]
  if (is.null(cfg)) stop("Unknown panel outcome: ", outcome)
  if (is.null(family)) family <- .resolve_panel_family(cfg$family %||% "nbinom2")

  outcome_var <- cfg$var
  exposure    <- cfg$offset
  has_offset  <- !is.null(exposure)
  bw_cols     <- c("hours_within")
  is_count    <- (cfg$family %||% "nbinom2") %in% c("nbinom2", "nbinom1", "poisson")

  required_cols <- c(climate_var, outcome_var,
                     if (has_offset) exposure else character(0),
                     "yearmonth_num_c", "sin_month", "cos_month",
                     "mine_id", bw_cols)
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
      !is.na(yearmonth_num_c), !is.na(sin_month), !is.na(cos_month),
      !is.na(mine_id),
      if_all(all_of(bw_cols), ~ !is.na(.x))
    )
  if (has_offset) {
    d <- d %>% filter(!is.na(.data[[exposure]]), .data[[exposure]] > 0)
  }
  # Count families require integer responses.
  if (is_count) d[[outcome_var]] <- as.integer(round(d[[outcome_var]]))

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

  fml <- build_msha_panel_formula(outcome, climate_var)
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
      offset_var   = if (has_offset) exposure else NA_character_,
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
      n_orgs    = n_distinct(d$mine_id),
      n_periods = n_distinct(d$quarter_start),
      n_zero_outcome   = sum(d[[outcome_var]] == 0),
      pct_zero_outcome = 100 * mean(d[[outcome_var]] == 0),
      total_outcome    = sum(d[[outcome_var]]),
      total_exposure     = if (has_offset) sum(d[[exposure]])     else NA_real_,
      mean_rate_per_unit = if (has_offset)
        sum(d[[outcome_var]]) / sum(d[[exposure]]) else NA_real_,
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
