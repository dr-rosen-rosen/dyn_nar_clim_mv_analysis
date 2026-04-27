# =============================================================================
# phmsa/fit_models.R — PHMSA Model Fitting Functions
# =============================================================================
#
# Provides:
#   fit_phmsa_binary()    : Binary injury/fatality via glmer (binomial)
#   fit_phmsa_cost()      : Total cost via glmmTMB (hurdle gamma)
#   fit_phmsa_model()     : Dispatch function — routes by outcome
#
# Requires: phmsa/config.R sourced first.
# Dependencies: lme4, glmmTMB, broom.mixed, dplyr
# =============================================================================

suppressPackageStartupMessages({
  library(lme4)
  library(glmmTMB)
  library(broom.mixed)
  library(dplyr)
})


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

.phmsa_ops_cols <- function() {
  c(paste0(PHMSA_OPS_VARS, "_between"), paste0(PHMSA_OPS_VARS, "_within"))
}

.phmsa_complete_cases <- function(data, climate_var, outcome_var,
                                   extra_cols = character(0)) {
  required <- unique(c(climate_var, outcome_var, "operator_id",
                       "yearmonth_num_c", .phmsa_ops_cols(), extra_cols))
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
# 1. BINARY OUTCOME (glmer, binomial)
# =============================================================================

#' Fit binary injury or fatality model (glmer)
#'
#' @param data        Prepared incident-level panel
#' @param climate_var Windowed climate variable name
#' @param config_id   Config ID string
#' @param outcome     "injury_binary" or "fatality_binary"
#' @param extra_terms Additional fixed-effect terms (temporal / EWS features)
#' @return Tibble row with model results
fit_phmsa_binary <- function(data, climate_var, config_id, outcome,
                              extra_terms = character(0)) {

  outcome_var <- get_outcome_var(outcome)
  fml_str     <- build_formula(MODEL_FORMULAS[[outcome]], climate_var)

  if (length(extra_terms) > 0) {
    fml_str <- sub("\\+ \\(1 \\| operator_id\\)",
                   paste0("+ ", paste(extra_terms, collapse = " + "),
                          " + (1 | operator_id)"),
                   fml_str)
  }

  model_data   <- .phmsa_complete_cases(data, climate_var, outcome_var)
  n_operators  <- n_distinct(model_data$operator_id)
  n_events     <- sum(model_data[[outcome_var]], na.rm = TRUE)

  if (nrow(model_data) < 30 || n_operators < 3) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome, status = "insufficient_data",
                  n_obs = nrow(model_data), n_operators = n_operators))
  }
  if (n_events < 5 || n_events >= nrow(model_data) - 5) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome, status = "insufficient_variation",
                  n_obs = nrow(model_data), n_operators = n_operators,
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
      fit       <- fit_retry
      warn_msgs <- warn_retry
    }
  }

  if (is.null(fit)) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome, status = "failed",
                  error = paste(warn_msgs, collapse = "; "),
                  n_obs = nrow(model_data), n_operators = n_operators))
  }

  tryCatch({
    tidy_fit  <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
    clim_row  <- tidy_fit %>% filter(term == climate_var)
    converged <- is.null(fit@optinfo$conv$lme4$messages)
    is_sing   <- isSingular(fit)

    result <- tibble(
      config_id        = config_id,
      climate_var      = climate_var,
      outcome          = outcome,
      status           = case_when(
        converged & !is_sing ~ "success",
        converged & is_sing  ~ "singular_fit",
        TRUE                 ~ "convergence_warning"),
      warnings         = paste(warn_msgs, collapse = "; "),
      n_obs            = nrow(model_data),
      n_operators      = n_operators,
      n_events         = n_events,
      event_rate       = mean(model_data[[outcome_var]], na.rm = TRUE),
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
           outcome = outcome, status = "extraction_failed",
           error = as.character(e$message), n_obs = nrow(model_data))
  })
}


# =============================================================================
# 2. TOTAL COST — GAMMA GLMM (glmmTMB, positive-only)
# =============================================================================

#' Fit Gamma GLMM for total incident cost (glmmTMB)
#'
#' total_cost is >99% positive across all pipeline types. A zero-inflated
#' model fails when the filtered dataset contains even a handful of zeros
#' (the ZI component cannot estimate RE variance from near-zero positive
#' counts). Instead: drop the tiny zero-cost tail, fit pure Gamma(log).
#'
#' @param data        Prepared incident-level panel
#' @param climate_var Windowed climate variable name
#' @param config_id   Config ID string
#' @param extra_terms Additional fixed-effect terms
#' @return Tibble row with model results
fit_phmsa_cost <- function(data, climate_var, config_id,
                            extra_terms = character(0)) {

  outcome_var <- "total_cost"
  fml_str     <- build_formula(MODEL_FORMULAS[[outcome_var]]$cond, climate_var)

  if (length(extra_terms) > 0) {
    fml_str <- sub("\\+ \\(1 \\| operator_id\\)",
                   paste0("+ ", paste(extra_terms, collapse = " + "),
                          " + (1 | operator_id)"),
                   fml_str)
  }

  # Gamma requires strictly positive response — drop zero-cost rows (~0.7%)
  model_data <- .phmsa_complete_cases(data, climate_var, outcome_var) %>%
    filter(total_cost > 0)

  n_operators <- n_distinct(model_data$operator_id)

  if (nrow(model_data) < 30 || n_operators < 3) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome_var, status = "insufficient_data",
                  n_obs = nrow(model_data), n_operators = n_operators))
  }

  warn_msgs <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      glmmTMB(
        as.formula(fml_str),
        data   = model_data,
        family = Gamma(link = "log")
      ),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      warn_msgs <<- c(warn_msgs, conditionMessage(e))
      NULL
    }
  )

  if (is.null(fit)) {
    return(tibble(config_id = config_id, climate_var = climate_var,
                  outcome = outcome_var, status = "failed",
                  error = paste(warn_msgs, collapse = "; "),
                  n_obs = nrow(model_data), n_operators = n_operators))
  }

  tryCatch({
    tidy_fit <- broom.mixed::tidy(fit, effects = "fixed",
                                  component = "cond", conf.int = TRUE)

    safe_val <- function(df, term_name, col) {
      row <- df %>% filter(term == term_name)
      if (nrow(row) > 0) row[[col]][1] else NA_real_
    }

    conv <- isTRUE(fit$fit$convergence == 0)

    result <- tibble(
      config_id        = config_id,
      climate_var      = climate_var,
      outcome          = outcome_var,
      status           = if (conv) "success" else "convergence_warning",
      warnings         = paste(warn_msgs, collapse = "; "),
      n_obs            = nrow(model_data),
      n_operators      = n_operators,
      aic              = AIC(fit),
      climate_estimate = safe_val(tidy_fit, climate_var, "estimate"),
      climate_se       = safe_val(tidy_fit, climate_var, "std.error"),
      climate_z        = safe_val(tidy_fit, climate_var, "statistic"),
      climate_p        = safe_val(tidy_fit, climate_var, "p.value"),
      climate_ci_low   = safe_val(tidy_fit, climate_var, "conf.low"),
      climate_ci_high  = safe_val(tidy_fit, climate_var, "conf.high")
    )

    for (v in .phmsa_ops_cols()) {
      result[[paste0(v, "_estimate")]] <- safe_val(tidy_fit, v, "estimate")
      result[[paste0(v, "_p")]]        <- safe_val(tidy_fit, v, "p.value")
    }

    result <- .append_extra_results(result, tidy_fit, extra_terms)
    result

  }, error = function(e) {
    tibble(config_id = config_id, climate_var = climate_var,
           outcome = outcome_var, status = "extraction_failed",
           error = as.character(e$message), n_obs = nrow(model_data))
  })
}


# =============================================================================
# 3. DISPATCH
# =============================================================================

#' Fit a single PHMSA model, routing by outcome
#'
#' @param df          Prepared panel
#' @param climate_var Windowed climate variable name
#' @param config_id   Config ID
#' @param outcome     One of: "injury_binary", "fatality_binary", "total_cost"
#' @param extra_terms Additional fixed-effect terms
#' @param protocol    Unused; kept for protocol interface compatibility
#' @return Tibble row with model results
fit_phmsa_model <- function(df, climate_var, config_id, outcome,
                             extra_terms = character(0), protocol = NULL) {
  result <- switch(outcome,
    injury_binary   = fit_phmsa_binary(df, climate_var, config_id, "injury_binary",   extra_terms),
    fatality_binary = fit_phmsa_binary(df, climate_var, config_id, "fatality_binary", extra_terms),
    total_cost      = fit_phmsa_cost(df, climate_var, config_id, extra_terms),
    stop(sprintf("Unknown PHMSA outcome: '%s'", outcome))
  )
  as_tibble(result)
}
