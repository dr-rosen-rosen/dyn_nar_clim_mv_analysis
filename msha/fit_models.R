# =============================================================================
# msha/fit_models.R — MSHA Model Fitting Functions (incident-level track)
# =============================================================================
#
#   fit_binary_fatality()  : Fatal accident via glmer (binomial)
#   fit_binary_injury()    : Injury vs. accident-only via glmer (binomial)
#   fit_days_lost_hurdle() : Lost workdays via glmmTMB hurdle (binomial + Gamma)
#   .fit_binary_glmer()    : shared binary engine (bobyqa -> nloptwrap retry)
#
# Mirrors nrc/fit_models.R, with org random effect (1 | mine_id) and the
# hours_between / hours_within operational covariates. No exposure offset —
# these are conditional-severity models given a reported event.
#
# Requires: msha/config.R sourced first.
# Dependencies: lme4, glmmTMB, broom.mixed, dplyr
# =============================================================================

suppressPackageStartupMessages({
  library(lme4)
  library(glmmTMB)
  library(broom.mixed)
  library(dplyr)
})


.MSHA_OPS_COLS <- c("hours_between", "hours_within")


# ==============================================================================
# SHARED BINARY ENGINE (glmer)
# ==============================================================================

#' Internal: fit a binary glmer model (shared engine).
#'
#' @param data Prepared data frame.
#' @param fml_str Formula string (climate_var already substituted).
#' @param outcome_var Column name of the binary 0/1 outcome.
#' @param climate_var Climate variable name (for coefficient extraction).
#' @param config_id Configuration ID.
#' @param outcome_label Label for the results tibble.
#' @param extra_terms Additional fixed-effect terms.
#' @return Tibble row.
.fit_binary_glmer <- function(data, fml_str, outcome_var, climate_var,
                              config_id, outcome_label,
                              extra_terms = character(0)) {

  required_cols <- c(climate_var, outcome_var, "mine_id", "yearmonth_num_c",
                     .MSHA_OPS_COLS)
  existing_cols <- intersect(required_cols, names(data))

  model_data <- data %>% filter(if_all(all_of(existing_cols), ~ !is.na(.)))

  if (nrow(model_data) < 50 || n_distinct(model_data$mine_id) < 3) {
    return(tibble(
      config_id = config_id, climate_var = climate_var, outcome = outcome_label,
      status = "insufficient_data",
      n_obs = nrow(model_data), n_mines = n_distinct(model_data$mine_id)
    ))
  }

  n_events <- sum(model_data[[outcome_var]])
  if (n_events < 5 || n_events >= nrow(model_data) - 5) {
    return(tibble(
      config_id = config_id, climate_var = climate_var, outcome = outcome_label,
      status = "insufficient_variation",
      n_obs = nrow(model_data), n_mines = n_distinct(model_data$mine_id),
      n_events = n_events, event_rate = mean(model_data[[outcome_var]])
    ))
  }

  warn_messages <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      glmer(as.formula(fml_str), data = model_data, family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))),
      warning = function(w) {
        warn_messages <<- c(warn_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  if (is.null(fit) || length(warn_messages) > 0) {
    warn_retry <- character(0)
    fit_retry <- tryCatch(
      withCallingHandlers(
        glmer(as.formula(fml_str), data = model_data, family = binomial,
              control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxeval = 2e5))),
        warning = function(w) {
          warn_retry <<- c(warn_retry, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
    if (!is.null(fit_retry)) {
      if (is.null(fit) || is.null(fit_retry@optinfo$conv$lme4$messages)) {
        fit <- fit_retry
        warn_messages <- warn_retry
      }
    }
  }

  if (is.null(fit)) {
    return(tibble(
      config_id = config_id, climate_var = climate_var, outcome = outcome_label,
      status = "failed", error = paste(warn_messages, collapse = "; "),
      n_obs = nrow(model_data), n_mines = n_distinct(model_data$mine_id)
    ))
  }

  tryCatch({
    tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
    climate_row <- tidy_fit %>% filter(term == climate_var)
    ops_rows <- tidy_fit %>% filter(grepl("_between$|_within$", term))

    converged   <- is.null(fit@optinfo$conv$lme4$messages)
    is_singular <- isSingular(fit)

    result <- tibble(
      config_id = config_id, climate_var = climate_var, outcome = outcome_label,
      status = case_when(
        converged & !is_singular ~ "success",
        converged & is_singular  ~ "singular_fit",
        TRUE                     ~ "convergence_warning"
      ),
      warnings = paste(warn_messages, collapse = "; "),
      n_obs = nrow(model_data), n_mines = n_distinct(model_data$mine_id),
      n_events = n_events, event_rate = mean(model_data[[outcome_var]]),
      aic = AIC(fit), bic = BIC(fit), is_singular = is_singular,
      climate_estimate = climate_row$estimate[1],
      climate_se       = climate_row$std.error[1],
      climate_z        = climate_row$statistic[1],
      climate_p        = climate_row$p.value[1],
      climate_ci_low   = climate_row$conf.low[1],
      climate_ci_high  = climate_row$conf.high[1]
    )

    for (i in seq_len(nrow(ops_rows))) {
      result[[paste0(ops_rows$term[i], "_estimate")]] <- ops_rows$estimate[i]
      result[[paste0(ops_rows$term[i], "_p")]]        <- ops_rows$p.value[i]
    }

    if (length(extra_terms) > 0) {
      for (et in extra_terms) {
        et_row <- tidy_fit %>% filter(term == et)
        if (nrow(et_row) > 0) {
          result[[paste0(et, "_estimate")]] <- et_row$estimate[1]
          result[[paste0(et, "_se")]]       <- et_row$std.error[1]
          result[[paste0(et, "_p")]]        <- et_row$p.value[1]
        }
      }
    }
    result
  }, error = function(e) {
    tibble(
      config_id = config_id, climate_var = climate_var, outcome = outcome_label,
      status = "extraction_failed", error = as.character(e$message),
      n_obs = nrow(model_data), n_mines = n_distinct(model_data$mine_id)
    )
  })
}


# ==============================================================================
# BINARY OUTCOME WRAPPERS
# ==============================================================================

#' Fit binary model for fatal accident (glmer).
fit_binary_fatality <- function(data, climate_var, config_id, extra_terms = character(0)) {
  fml_str <- build_formula(MODEL_FORMULAS$fatality_binary, climate_var)
  if (length(extra_terms) > 0) {
    fml_str <- sub("\\+ \\(1 \\| mine_id\\)",
                   paste0("+ ", paste(extra_terms, collapse = " + "), " + (1 | mine_id)"),
                   fml_str)
  }
  .fit_binary_glmer(data, fml_str, "fatality_binary", climate_var, config_id,
                    outcome_label = "fatality_binary", extra_terms = extra_terms)
}

#' Fit binary model for injury vs. accident-only (glmer).
fit_binary_injury <- function(data, climate_var, config_id, extra_terms = character(0)) {
  fml_str <- build_formula(MODEL_FORMULAS$injury_binary, climate_var)
  if (length(extra_terms) > 0) {
    fml_str <- sub("\\+ \\(1 \\| mine_id\\)",
                   paste0("+ ", paste(extra_terms, collapse = " + "), " + (1 | mine_id)"),
                   fml_str)
  }
  .fit_binary_glmer(data, fml_str, "injury_binary", climate_var, config_id,
                    outcome_label = "injury_binary", extra_terms = extra_terms)
}


# ==============================================================================
# LOST WORKDAYS HURDLE (glmmTMB)
# ==============================================================================

#' Fit a hurdle model for lost workdays.
#'
#'   Binary:  P(days_lost > 0)               — does the injury cause lost time?
#'   Gamma:   E(days_lost | days_lost > 0)    — how many days, given lost time?
#'
#' @return Tibble row with both zi (binary gate) and cond (Gamma) estimates.
fit_days_lost_hurdle <- function(data, climate_var, config_id,
                                 extra_terms = character(0)) {

  fmls <- build_formula(MODEL_FORMULAS[["days_lost"]], climate_var)

  complete_data <- data %>%
    filter(
      !is.na(days_lost), !is.na(.data[[climate_var]]),
      !is.na(hours_between), !is.na(hours_within),
      !is.na(yearmonth_num_c)
    )

  n_obs <- nrow(complete_data)
  n_pos <- sum(complete_data$days_lost > 0)

  if (n_pos < 50) {
    return(tibble(
      config_id = config_id, climate_var = climate_var, outcome = "days_lost",
      status = "failed",
      error = sprintf("Insufficient positive lost-time: %d", n_pos), n_obs = n_obs
    ))
  }

  positive_data <- complete_data %>% filter(days_lost > 0)

  binary_fml_str <- fmls$binary
  gamma_fml_str  <- fmls$gamma
  if (length(extra_terms) > 0) {
    extra_str <- paste(extra_terms, collapse = " + ")
    binary_fml_str <- sub("\\+ \\(1 \\| mine_id\\)",
                          paste0("+ ", extra_str, " + (1 | mine_id)"), binary_fml_str)
    gamma_fml_str  <- sub("\\+ \\(1 \\| mine_id\\)",
                          paste0("+ ", extra_str, " + (1 | mine_id)"), gamma_fml_str)
  }

  fit_binary <- tryCatch(suppressWarnings(
    glmmTMB(as.formula(binary_fml_str), data = complete_data,
            family = binomial(link = "logit"))), error = function(e) NULL)
  fit_gamma <- tryCatch(suppressWarnings(
    glmmTMB(as.formula(gamma_fml_str), data = positive_data,
            family = Gamma(link = "log"))), error = function(e) NULL)

  if (is.null(fit_binary) || is.null(fit_gamma)) {
    return(tibble(
      config_id = config_id, climate_var = climate_var, outcome = "days_lost",
      status = "failed", error = "Model fitting failed", n_obs = n_obs
    ))
  }

  fixed_binary <- broom.mixed::tidy(fit_binary, effects = "fixed", conf.int = TRUE)
  fixed_gamma  <- broom.mixed::tidy(fit_gamma,  effects = "fixed", conf.int = TRUE)

  safe_extract <- function(d, term_name, col) {
    row <- d %>% filter(term == term_name)
    if (nrow(row) > 0) row[[col]][1] else NA_real_
  }

  conv_binary <- is.null(fit_binary$fit$convergence) || fit_binary$fit$convergence == 0
  conv_gamma  <- is.null(fit_gamma$fit$convergence)  || fit_gamma$fit$convergence == 0
  status <- if (conv_binary && conv_gamma) "success" else "convergence_warning"

  ops_results <- list()
  for (v in .MSHA_OPS_COLS) {
    ops_results[[paste0(v, "_estimate")]] <- safe_extract(fixed_binary, v, "estimate")
    ops_results[[paste0(v, "_p")]]        <- safe_extract(fixed_binary, v, "p.value")
  }

  extra_results <- list()
  if (length(extra_terms) > 0) {
    for (et in extra_terms) {
      extra_results[[paste0(et, "_estimate_zi")]]   <- safe_extract(fixed_binary, et, "estimate")
      extra_results[[paste0(et, "_pval_zi")]]       <- safe_extract(fixed_binary, et, "p.value")
      extra_results[[paste0(et, "_estimate_cond")]] <- safe_extract(fixed_gamma, et, "estimate")
      extra_results[[paste0(et, "_pval_cond")]]     <- safe_extract(fixed_gamma, et, "p.value")
    }
  }

  tibble(
    config_id = config_id, climate_var = climate_var, outcome = "days_lost",
    status = status, n_obs = n_obs, n_positive = n_pos,
    climate_estimate_zi   = safe_extract(fixed_binary, climate_var, "estimate"),
    climate_se_zi         = safe_extract(fixed_binary, climate_var, "std.error"),
    climate_pval_zi       = safe_extract(fixed_binary, climate_var, "p.value"),
    climate_ci_low_zi     = safe_extract(fixed_binary, climate_var, "conf.low"),
    climate_ci_high_zi    = safe_extract(fixed_binary, climate_var, "conf.high"),
    climate_estimate_cond = safe_extract(fixed_gamma, climate_var, "estimate"),
    climate_se_cond       = safe_extract(fixed_gamma, climate_var, "std.error"),
    climate_pval_cond     = safe_extract(fixed_gamma, climate_var, "p.value"),
    climate_ci_low_cond   = safe_extract(fixed_gamma, climate_var, "conf.low"),
    climate_ci_high_cond  = safe_extract(fixed_gamma, climate_var, "conf.high"),
    !!!ops_results,
    !!!extra_results,
    aic_binary = AIC(fit_binary),
    aic_gamma  = AIC(fit_gamma)
  )
}
