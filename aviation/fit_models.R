# =============================================================================
# aviation/fit_models.R — Aviation Model Fitting Functions
# =============================================================================
#
# Provides:
#   fit_av_accident_binary()  : Accident occurrence via glmer (binomial)
#   fit_av_inj_hurdle()       : Serious+fatal injury count via glmmTMB hurdle
#   fit_av_sev_ordinal()      : Accident severity via clmm (ordinal)
#   fit_av_model()            : Dispatch function — routes by outcome
#
# Requires: aviation/config.R sourced first.
# Dependencies: lme4, glmmTMB, ordinal, broom.mixed, dplyr
# =============================================================================

suppressPackageStartupMessages({
  library(lme4)
  library(glmmTMB)
  library(ordinal)
  library(broom.mixed)
  library(dplyr)
})


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

.av_ops_cols <- function() {
  c(paste0(AV_OPS_VARS, "_between"), paste0(AV_OPS_VARS, "_within"))
}

.av_complete_cases <- function(data, climate_var, outcome_var, extra_cols = character(0)) {
  required <- unique(c(climate_var, outcome_var, "airport_id",
                       "yearmonth_num_c", .av_ops_cols(), extra_cols))
  existing <- intersect(required, names(data))
  data %>% filter(if_all(all_of(existing), ~ !is.na(.)))
}

.extract_ops_rows <- function(tidy_fit) {
  tidy_fit %>% filter(grepl("_between$|_within$", term))
}

.append_ops_results <- function(result, ops_rows) {
  for (i in seq_len(nrow(ops_rows))) {
    result[[paste0(ops_rows$term[i], "_estimate")]] <- ops_rows$estimate[i]
    result[[paste0(ops_rows$term[i], "_p")]]        <- ops_rows$p.value[i]
  }
  result
}

.append_extra_results <- function(result, tidy_fit, extra_terms) {
  for (et in extra_terms) {
    row <- tidy_fit %>% filter(term == et)
    if (nrow(row) > 0) {
      result[[paste0(et, "_estimate")]] <- row$estimate[1]
      result[[paste0(et, "_se")]]       <- row$std.error[1]
      result[[paste0(et, "_p")]]        <- row$p.value[1]
    }
  }
  result
}


# =============================================================================
# 1. BINARY ACCIDENT OCCURRENCE (glmer, binomial)
# =============================================================================

#' Fit binary accident occurrence model (glmer)
#'
#' @param data        Prepared airport × month panel
#' @param climate_var Windowed climate variable name
#' @param config_id   Config ID string
#' @param extra_terms Additional fixed-effect terms (temporal / EWS features)
#' @return Tibble row with model results
fit_av_accident_binary <- function(data, climate_var, config_id,
                                    extra_terms = character(0)) {

  outcome_var <- "accident_binary"
  fml_str     <- build_formula(MODEL_FORMULAS[[outcome_var]], climate_var)

  if (length(extra_terms) > 0) {
    fml_str <- sub("\\+ \\(1 \\| airport_id\\)",
                   paste0("+ ", paste(extra_terms, collapse = " + "),
                          " + (1 | airport_id)"),
                   fml_str)
  }

  model_data <- .av_complete_cases(data, climate_var, outcome_var)

  n_airports <- n_distinct(model_data$airport_id)
  n_events   <- sum(model_data[[outcome_var]])

  if (nrow(model_data) < 50 || n_airports < 3) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome_var, status = "insufficient_data",
                  n_obs = nrow(model_data), n_airports = n_airports))
  }
  if (n_events < 5 || n_events >= nrow(model_data) - 5) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome_var, status = "insufficient_variation",
                  n_obs = nrow(model_data), n_airports = n_airports,
                  n_events = n_events))
  }

  warn_msgs <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      glmer(as.formula(fml_str), data = model_data, family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  # Retry with nloptwrap on convergence issues
  if (is.null(fit) || length(warn_msgs) > 0) {
    warn_retry <- character(0)
    fit_retry <- tryCatch(
      withCallingHandlers(
        glmer(as.formula(fml_str), data = model_data, family = binomial,
              control = glmerControl(optimizer = "nloptwrap",
                                     optCtrl = list(maxeval = 2e5))),
        warning = function(w) {
          warn_retry <<- c(warn_retry, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
    if (!is.null(fit_retry) &&
        (is.null(fit) || is.null(fit_retry@optinfo$conv$lme4$messages))) {
      fit <- fit_retry
      warn_msgs <- warn_retry
    }
  }

  if (is.null(fit)) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome_var, status = "failed",
                  error = paste(warn_msgs, collapse = "; "),
                  n_obs = nrow(model_data), n_airports = n_airports))
  }

  tryCatch({
    tidy_fit   <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
    clim_row   <- tidy_fit %>% filter(term == climate_var)
    converged  <- is.null(fit@optinfo$conv$lme4$messages)
    is_sing    <- isSingular(fit)

    result <- tibble(
      config_id        = config_id,
      climate_var      = climate_var,
      outcome          = outcome_var,
      status           = case_when(
        converged & !is_sing ~ "success",
        converged & is_sing  ~ "singular_fit",
        TRUE                 ~ "convergence_warning"),
      warnings         = paste(warn_msgs, collapse = "; "),
      n_obs            = nrow(model_data),
      n_airports       = n_airports,
      n_events         = n_events,
      event_rate       = mean(model_data[[outcome_var]]),
      aic              = AIC(fit),
      bic              = BIC(fit),
      is_singular      = is_sing,
      climate_estimate = clim_row$estimate[1],
      climate_se       = clim_row$std.error[1],
      climate_z        = clim_row$statistic[1],
      climate_p        = clim_row$p.value[1],
      climate_ci_low   = clim_row$conf.low[1],
      climate_ci_high  = clim_row$conf.high[1]
    )
    result <- .append_ops_results(result, .extract_ops_rows(tidy_fit))
    result <- .append_extra_results(result, tidy_fit, extra_terms)
    result

  }, error = function(e) {
    tibble(config_id = config_id, climate_var = climate_var,
           outcome = outcome_var, status = "extraction_failed",
           error = as.character(e$message), n_obs = nrow(model_data))
  })
}


# =============================================================================
# 2. SERIOUS + FATAL INJURY COUNT (glmmTMB hurdle)
# =============================================================================

#' Fit hurdle model for serious + fatal injury count (glmmTMB)
#'
#' Two-part model:
#'   ZI gate:   P(any serious/fatal injury | accident)
#'   Cond:      E(count | count > 0) via truncated negative binomial
#'
#' @param data        Prepared airport × month panel
#' @param climate_var Windowed climate variable name
#' @param config_id   Config ID string
#' @param extra_terms Additional fixed-effect terms
#' @return Tibble row with cond and zi estimates
fit_av_inj_hurdle <- function(data, climate_var, config_id,
                               extra_terms = character(0)) {

  outcome_var <- "inj_serious_fatal"
  fmls        <- build_formula(MODEL_FORMULAS[[outcome_var]], climate_var)

  if (length(extra_terms) > 0) {
    extra_str <- paste(extra_terms, collapse = " + ")
    fmls$cond <- sub("\\+ \\(1 \\| airport_id\\)",
                     paste0("+ ", extra_str, " + (1 | airport_id)"), fmls$cond)
    fmls$zi   <- sub("\\+ \\(1 \\| airport_id\\)",
                     paste0("+ ", extra_str, " + (1 | airport_id)"), fmls$zi)
  }

  model_data <- .av_complete_cases(data, climate_var, outcome_var)
  n_pos      <- sum(model_data[[outcome_var]] > 0, na.rm = TRUE)

  if (nrow(model_data) < 50 || n_distinct(model_data$airport_id) < 3) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome_var, status = "insufficient_data",
                  n_obs = nrow(model_data)))
  }
  if (n_pos < 10) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome_var, status = "insufficient_positives",
                  n_obs = nrow(model_data), n_positive = n_pos))
  }

  fit <- tryCatch(
    suppressWarnings(
      glmmTMB(
        as.formula(fmls$cond),
        ziformula = as.formula(fmls$zi),
        data   = model_data,
        family = truncated_nbinom2
      )
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome_var, status = "failed",
                  n_obs = nrow(model_data), n_positive = n_pos))
  }

  tryCatch({
    tidy_cond <- broom.mixed::tidy(fit, effects = "fixed",
                                   component = "cond", conf.int = TRUE)
    tidy_zi   <- broom.mixed::tidy(fit, effects = "fixed",
                                   component = "zi",   conf.int = TRUE)

    safe_val <- function(df, term_name, col) {
      row <- df %>% filter(term == term_name)
      if (nrow(row) > 0) row[[col]][1] else NA_real_
    }

    conv <- isTRUE(fit$fit$convergence == 0)

    result <- tibble(
      config_id             = config_id,
      climate_var           = climate_var,
      outcome               = outcome_var,
      status                = if (conv) "success" else "convergence_warning",
      n_obs                 = nrow(model_data),
      n_airports            = n_distinct(model_data$airport_id),
      n_positive            = n_pos,
      aic                   = AIC(fit),
      climate_estimate_cond = safe_val(tidy_cond, climate_var, "estimate"),
      climate_se_cond       = safe_val(tidy_cond, climate_var, "std.error"),
      climate_p_cond        = safe_val(tidy_cond, climate_var, "p.value"),
      climate_ci_low_cond   = safe_val(tidy_cond, climate_var, "conf.low"),
      climate_ci_high_cond  = safe_val(tidy_cond, climate_var, "conf.high"),
      climate_estimate_zi   = safe_val(tidy_zi,   climate_var, "estimate"),
      climate_se_zi         = safe_val(tidy_zi,   climate_var, "std.error"),
      climate_p_zi          = safe_val(tidy_zi,   climate_var, "p.value"),
      climate_ci_low_zi     = safe_val(tidy_zi,   climate_var, "conf.low"),
      climate_ci_high_zi    = safe_val(tidy_zi,   climate_var, "conf.high")
    )

    for (v in .av_ops_cols()) {
      result[[paste0(v, "_estimate_cond")]] <- safe_val(tidy_cond, v, "estimate")
      result[[paste0(v, "_p_cond")]]        <- safe_val(tidy_cond, v, "p.value")
    }

    result

  }, error = function(e) {
    tibble(config_id = config_id, climate_var = climate_var,
           outcome = outcome_var, status = "extraction_failed",
           error = as.character(e$message), n_obs = nrow(model_data))
  })
}


# =============================================================================
# 3. ACCIDENT SEVERITY ORDINAL (clmm)
# =============================================================================

#' Fit ordinal severity model (clmm)
#'
#' Outcome: sev_ord_factor (0 = none, 1 = minor, 2 = serious, 3 = fatal).
#' RE (1 | airport_id) included via clmm.
#'
#' @param data        Prepared airport × month panel
#' @param climate_var Windowed climate variable name
#' @param config_id   Config ID string
#' @param extra_terms Additional fixed-effect terms
#' @return Tibble row with model results
fit_av_sev_ordinal <- function(data, climate_var, config_id,
                                extra_terms = character(0)) {

  outcome_var <- "sev_ord_factor"
  fml_str     <- build_formula(MODEL_FORMULAS$sev_ord, climate_var)

  if (length(extra_terms) > 0) {
    fml_str <- paste0(fml_str, " + ", paste(extra_terms, collapse = " + "))
  }

  # clmm RE appended separately
  clmm_fml <- as.formula(paste0(fml_str, " + (1 | airport_id)"))

  model_data <- .av_complete_cases(data, climate_var, outcome_var)
  n_levels   <- n_distinct(model_data[[outcome_var]])
  n_airports <- n_distinct(model_data$airport_id)

  if (nrow(model_data) < 50 || n_airports < 3 || n_levels < 2) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = "sev_ord", status = "insufficient_data",
                  n_obs = nrow(model_data), n_airports = n_airports,
                  n_levels = n_levels))
  }

  tryCatch({
    fit      <- ordinal::clmm(clmm_fml, data = model_data)
    coef_tbl <- as.data.frame(summary(fit)$coefficients)
    coef_tbl$term <- rownames(coef_tbl)
    clim_row <- coef_tbl %>% filter(term == climate_var)

    result <- tibble(
      config_id        = config_id,
      climate_var      = climate_var,
      outcome          = "sev_ord",
      status           = "success",
      n_obs            = nrow(model_data),
      n_airports       = n_airports,
      n_levels         = n_levels,
      aic              = AIC(fit),
      loglik           = as.numeric(logLik(fit)),
      climate_estimate = clim_row$Estimate[1],
      climate_se       = clim_row$`Std. Error`[1],
      climate_z        = clim_row$`z value`[1],
      climate_p        = clim_row$`Pr(>|z|)`[1]
    )

    for (v in .av_ops_cols()) {
      row <- coef_tbl %>% filter(term == v)
      if (nrow(row) == 1) {
        result[[paste0(v, "_estimate")]] <- row$Estimate[1]
        result[[paste0(v, "_p")]]        <- row$`Pr(>|z|)`[1]
      }
    }

    result

  }, error = function(e) {
    tibble(config_id = config_id, climate_var = climate_var,
           outcome = "sev_ord", status = "failed",
           error = as.character(e$message), n_obs = nrow(model_data))
  })
}


# =============================================================================
# 4. DISPATCH
# =============================================================================

#' Fit a single aviation model, routing by outcome
#'
#' @param df          Prepared panel
#' @param climate_var Windowed climate variable name
#' @param config_id   Config ID
#' @param outcome     One of: "accident_binary", "inj_serious_fatal", "sev_ord"
#' @param extra_terms Additional fixed-effect terms
#' @param protocol    Unused; kept for protocol interface compatibility
#' @return Tibble row with model results
fit_av_model <- function(df, climate_var, config_id, outcome,
                          extra_terms = character(0), protocol = NULL) {
  result <- switch(outcome,
    accident_binary   = fit_av_accident_binary(df, climate_var, config_id, extra_terms),
    inj_serious_fatal = fit_av_inj_hurdle(df, climate_var, config_id, extra_terms),
    sev_ord           = fit_av_sev_ordinal(df, climate_var, config_id, extra_terms),
    stop(sprintf("Unknown aviation outcome: '%s'", outcome))
  )
  as_tibble(result)
}
