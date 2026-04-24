# =============================================================================
# rail/fit_models.R — Rail Model Fitting Functions
# =============================================================================
#
# Extracted from 1_rail_multiverse.R. Contains:
#   - fit_single_rail_model()  : Fit one glmmTMB hurdle model for a given
#                                climate var × outcome
#
# Handles three outcome types:
#   - injuries/fatalities: truncated_poisson with zero-inflation
#   - costs: two-part hurdle (binomial gate + Gamma conditional)
#
# Requires: rail/config.R sourced first (provides OPS_VARS, OUTCOME_VARS,
#           MODEL_FORMULAS, get_outcome_var, build_formula)
# Dependencies: glmmTMB, broom.mixed, dplyr, purrr
# =============================================================================


#' Fit a single glmmTMB hurdle model
#'
#' @param data Data frame with all necessary variables (from prepare_config_data)
#' @param climate_var Name of the windowed climate variable to use
#' @param config_id Configuration ID for tracking
#' @param outcome Outcome key ("injuries", "fatalities", or "costs")
#' @param extra_terms Character vector of additional fixed-effect terms to add
#'   to the formula (for future layer integration). Currently unused for paper 1.
#' @return Named list with model results (climate estimates, ops estimates,
#'   fit statistics, convergence status)
fit_single_rail_model <- function(data, climate_var, config_id, outcome = "injuries",
                                   extra_terms = character(0)) {

  outcome_var <- get_outcome_var(outcome)

  # Operational variable names
  ops_between_vars <- paste0(OPS_VARS, "_between")
  ops_within_vars <- paste0(OPS_VARS, "_within")
  all_ops_vars <- c(ops_between_vars, ops_within_vars)

  # Filter to complete cases (including operational variables)
  complete_data <- tryCatch({
    data %>%
      filter(
        !is.na(.data[[climate_var]]),
        !is.na(.data[[outcome_var]]),
        !is.na(yearmonth_num_c),
        !is.na(sin_month),
        !is.na(cos_month),
        !is.na(org_id),
        if_all(all_of(all_ops_vars), ~ !is.na(.x))
      )
  }, error = function(e) {
    return(list(
      config_id = config_id,
      climate_var = climate_var,
      outcome = outcome,
      status = "failed",
      error = paste("Filter error:", as.character(e$message)),
      n_obs = NA_integer_
    ))
  })

  # Check if error was returned from filter
  if (is.list(complete_data) && !is.data.frame(complete_data)) {
    return(complete_data)
  }

  result <- tryCatch({

    # Check for sufficient data
    if (nrow(complete_data) < 100) {
      return(list(
        config_id = config_id,
        climate_var = climate_var,
        outcome = outcome,
        status = "failed",
        error = sprintf("Insufficient data: %d complete cases", nrow(complete_data)),
        n_obs = nrow(complete_data)
      ))
    }

    # Check for variation in climate
    clim_sd <- tryCatch({
      sd(complete_data[[climate_var]], na.rm = TRUE)
    }, error = function(e) {
      return(list(
        config_id = config_id,
        climate_var = climate_var,
        outcome = outcome,
        status = "failed",
        error = paste("SD calculation error:", as.character(e$message)),
        n_obs = nrow(complete_data)
      ))
    })

    if (is.list(clim_sd) && !is.numeric(clim_sd)) {
      return(clim_sd)
    }

    if (clim_sd < 0.001) {
      return(list(
        config_id = config_id,
        climate_var = climate_var,
        outcome = outcome,
        status = "failed",
        error = "No variation in climate variable",
        n_obs = nrow(complete_data)
      ))
    }

    # Check for variation in outcome
    n_zeros <- sum(complete_data[[outcome_var]] == 0)
    n_nonzeros <- sum(complete_data[[outcome_var]] > 0)

    if (n_nonzeros < 10) {
      return(list(
        config_id = config_id,
        climate_var = climate_var,
        outcome = outcome,
        status = "failed",
        error = sprintf("Insufficient non-zero outcomes: %d", n_nonzeros),
        n_obs = nrow(complete_data)
      ))
    }

    # Build formulas
    formulas <- MODEL_FORMULAS[[outcome]]
    cond_formula_str <- gsub("CLIMATE_VAR", climate_var, formulas$cond)
    zi_formula_str <- gsub("CLIMATE_VAR", climate_var, formulas$zi)

    # Append extra terms if provided (for future layer integration)
    if (length(extra_terms) > 0) {
      extra_str <- paste(extra_terms, collapse = " + ")
      # Insert before the random effect
      cond_formula_str <- sub("\\+ \\(1 \\| org_id\\)",
                              paste0("+ ", extra_str, " + (1 | org_id)"),
                              cond_formula_str)
      zi_formula_str <- sub("\\+ \\(1 \\| org_id\\)",
                            paste0("+ ", extra_str, " + (1 | org_id)"),
                            zi_formula_str)
    }

    # Helper function to safely extract coefficient info
    safe_extract <- function(df, term_name, col) {
      row <- df %>% filter(term == term_name)
      if (nrow(row) > 0) row[[col]][1] else NA_real_
    }

    # In fit_single_rail_model, temporarily add before the fit:
    message("Formula: ", cond_formula_str)
    
    # Fit hurdle model based on outcome
    if (outcome == "costs") {
      # For costs: two-part hurdle (binomial gate + Gamma conditional)
      positive_data <- complete_data %>% filter(.data[[outcome_var]] > 0)

      if (nrow(positive_data) < 50) {
        return(list(
          config_id = config_id,
          climate_var = climate_var,
          outcome = outcome,
          status = "failed",
          error = sprintf("Insufficient positive cost values: %d", nrow(positive_data)),
          n_obs = nrow(complete_data)
        ))
      }

      # Fit binary model for whether damage occurs
      binary_formula <- gsub(outcome_var,
                            sprintf("I(%s > 0)", outcome_var),
                            cond_formula_str)

      fit_binary <- suppressWarnings({
        glmmTMB(
          as.formula(binary_formula),
          data = complete_data,
          family = binomial(link = "logit")
        )
      })

      # Fit gamma model for damage amount (conditional on damage > 0)
      fit_gamma <- suppressWarnings({
        glmmTMB(
          as.formula(cond_formula_str),
          data = positive_data,
          family = Gamma(link = "log")
        )
      })

      fit <- list(
        binary = fit_binary,
        gamma = fit_gamma,
        type = "hurdle_gamma"
      )

    } else {
      # For counts: truncated poisson with zero-inflation
      fit <- suppressWarnings({
        glmmTMB(
          as.formula(cond_formula_str),
          ziformula = as.formula(zi_formula_str),
          data = complete_data,
          family = truncated_poisson(link = "log")
        )
      })
    }

    # Extract fixed effects based on model type
    if (outcome == "costs" && is.list(fit) && !is.null(fit$type)) {
      # Hurdle gamma model: extract from both parts
      fixed_eff_binary <- tidy(fit$binary, effects = "fixed", conf.int = TRUE)
      fixed_eff_gamma <- tidy(fit$gamma, effects = "fixed", conf.int = TRUE)

      climate_row_zi <- fixed_eff_binary %>% filter(term == climate_var)
      climate_row_cond <- fixed_eff_gamma %>% filter(term == climate_var)

      ops_effects_cond <- map_dfr(c(ops_between_vars, ops_within_vars), function(v) {
        tibble(
          term = v,
          estimate = safe_extract(fixed_eff_gamma, v, "estimate"),
          std.error = safe_extract(fixed_eff_gamma, v, "std.error"),
          p.value = safe_extract(fixed_eff_gamma, v, "p.value"),
          conf.low = safe_extract(fixed_eff_gamma, v, "conf.low"),
          conf.high = safe_extract(fixed_eff_gamma, v, "conf.high")
        )
      })

      ops_effects_zi <- map_dfr(c(ops_between_vars, ops_within_vars), function(v) {
        tibble(
          term = v,
          estimate = safe_extract(fixed_eff_binary, v, "estimate"),
          std.error = safe_extract(fixed_eff_binary, v, "std.error"),
          p.value = safe_extract(fixed_eff_binary, v, "p.value"),
          conf.low = safe_extract(fixed_eff_binary, v, "conf.low"),
          conf.high = safe_extract(fixed_eff_binary, v, "conf.high")
        )
      })

      random_eff_binary <- tidy(fit$binary, effects = "ran_pars")
      random_eff_gamma <- tidy(fit$gamma, effects = "ran_pars")

      re_intercept_zi <- random_eff_binary %>% filter(term == "sd__(Intercept)")
      re_intercept_cond <- random_eff_gamma %>% filter(term == "sd__(Intercept)")

      fit_stats <- tibble(
        AIC = AIC(fit$binary) + AIC(fit$gamma),
        BIC = BIC(fit$binary) + BIC(fit$gamma),
        logLik = as.numeric(logLik(fit$binary) + logLik(fit$gamma)),
        n_obs = nrow(complete_data),
        n_orgs = n_distinct(complete_data$org_id),
        n_zeros = n_zeros,
        n_nonzeros = n_nonzeros,
        pct_zeros = 100 * n_zeros / nrow(complete_data)
      )

      converged <- fit$binary$sdr$pdHess && fit$gamma$sdr$pdHess

      # Alias for consistent extra_terms extraction below
      fixed_eff_cond <- fixed_eff_gamma
      fixed_eff_zi <- fixed_eff_binary

    } else {
      # Count models: extract from single hurdle model
      fixed_eff_cond <- tidy(fit, effects = "fixed", component = "cond", conf.int = TRUE)
      fixed_eff_zi <- tidy(fit, effects = "fixed", component = "zi", conf.int = TRUE)

      climate_row_cond <- fixed_eff_cond %>% filter(term == climate_var)
      climate_row_zi <- fixed_eff_zi %>% filter(term == climate_var)

      ops_effects_cond <- map_dfr(c(ops_between_vars, ops_within_vars), function(v) {
        tibble(
          term = v,
          estimate = safe_extract(fixed_eff_cond, v, "estimate"),
          std.error = safe_extract(fixed_eff_cond, v, "std.error"),
          p.value = safe_extract(fixed_eff_cond, v, "p.value"),
          conf.low = safe_extract(fixed_eff_cond, v, "conf.low"),
          conf.high = safe_extract(fixed_eff_cond, v, "conf.high")
        )
      })

      ops_effects_zi <- map_dfr(c(ops_between_vars, ops_within_vars), function(v) {
        tibble(
          term = v,
          estimate = safe_extract(fixed_eff_zi, v, "estimate"),
          std.error = safe_extract(fixed_eff_zi, v, "std.error"),
          p.value = safe_extract(fixed_eff_zi, v, "p.value"),
          conf.low = safe_extract(fixed_eff_zi, v, "conf.low"),
          conf.high = safe_extract(fixed_eff_zi, v, "conf.high")
        )
      })

      random_eff <- tidy(fit, effects = "ran_pars")
      re_intercept_cond <- random_eff %>%
        filter(term == "sd__(Intercept)", component == "cond")
      re_intercept_zi <- random_eff %>%
        filter(term == "sd__(Intercept)", component == "zi")

      fit_stats <- tibble(
        AIC = AIC(fit),
        BIC = BIC(fit),
        logLik = as.numeric(logLik(fit)),
        n_obs = nrow(complete_data),
        n_orgs = n_distinct(complete_data$org_id),
        n_zeros = n_zeros,
        n_nonzeros = n_nonzeros,
        pct_zeros = 100 * n_zeros / nrow(complete_data)
      )

      converged <- fit$sdr$pdHess
    }

    # Build result list
    result_list <- list(
      config_id = config_id,
      climate_var = climate_var,
      outcome = outcome,
      status = if (converged) "success" else "convergence_warning",

      climate_estimate_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$estimate[1] else NA_real_,
      climate_se_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$std.error[1] else NA_real_,
      climate_pval_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$p.value[1] else NA_real_,
      climate_conf_low_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$conf.low[1] else NA_real_,
      climate_conf_high_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$conf.high[1] else NA_real_,

      climate_estimate_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$estimate[1] else NA_real_,
      climate_se_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$std.error[1] else NA_real_,
      climate_pval_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$p.value[1] else NA_real_,
      climate_conf_low_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$conf.low[1] else NA_real_,
      climate_conf_high_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$conf.high[1] else NA_real_,

      random_intercept_sd_cond = if (nrow(re_intercept_cond) > 0) re_intercept_cond$estimate[1] else NA_real_,
      random_intercept_sd_zi = if (nrow(re_intercept_zi) > 0) re_intercept_zi$estimate[1] else NA_real_,

      AIC = fit_stats$AIC,
      BIC = fit_stats$BIC,
      logLik = fit_stats$logLik,
      n_obs = fit_stats$n_obs,
      n_orgs = fit_stats$n_orgs,
      n_zeros = fit_stats$n_zeros,
      n_nonzeros = fit_stats$n_nonzeros,
      pct_zeros = fit_stats$pct_zeros
    )

    # Add operational effects dynamically
    for (i in 1:nrow(ops_effects_cond)) {
      v <- ops_effects_cond$term[i]
      result_list[[paste0(v, "_estimate_cond")]] <- ops_effects_cond$estimate[i]
      result_list[[paste0(v, "_se_cond")]] <- ops_effects_cond$std.error[i]
      result_list[[paste0(v, "_pval_cond")]] <- ops_effects_cond$p.value[i]
      result_list[[paste0(v, "_conf_low_cond")]] <- ops_effects_cond$conf.low[i]
      result_list[[paste0(v, "_conf_high_cond")]] <- ops_effects_cond$conf.high[i]
    }

    for (i in 1:nrow(ops_effects_zi)) {
      v <- ops_effects_zi$term[i]
      result_list[[paste0(v, "_estimate_zi")]] <- ops_effects_zi$estimate[i]
      result_list[[paste0(v, "_se_zi")]] <- ops_effects_zi$std.error[i]
      result_list[[paste0(v, "_pval_zi")]] <- ops_effects_zi$p.value[i]
      result_list[[paste0(v, "_conf_low_zi")]] <- ops_effects_zi$conf.low[i]
      result_list[[paste0(v, "_conf_high_zi")]] <- ops_effects_zi$conf.high[i]
    }

    # Add extra terms effects (temporal, EWS features) dynamically
    if (length(extra_terms) > 0) {
      for (et in extra_terms) {
        # Conditional component
        et_cond <- safe_extract(fixed_eff_cond, et, "estimate")
        if (!is.null(et_cond)) {
          result_list[[paste0(et, "_estimate_cond")]] <- safe_extract(fixed_eff_cond, et, "estimate")
          result_list[[paste0(et, "_se_cond")]] <- safe_extract(fixed_eff_cond, et, "std.error")
          result_list[[paste0(et, "_pval_cond")]] <- safe_extract(fixed_eff_cond, et, "p.value")
          result_list[[paste0(et, "_conf_low_cond")]] <- safe_extract(fixed_eff_cond, et, "conf.low")
          result_list[[paste0(et, "_conf_high_cond")]] <- safe_extract(fixed_eff_cond, et, "conf.high")
        }
        # ZI component
        et_zi <- safe_extract(fixed_eff_zi, et, "estimate")
        if (!is.null(et_zi)) {
          result_list[[paste0(et, "_estimate_zi")]] <- safe_extract(fixed_eff_zi, et, "estimate")
          result_list[[paste0(et, "_se_zi")]] <- safe_extract(fixed_eff_zi, et, "std.error")
          result_list[[paste0(et, "_pval_zi")]] <- safe_extract(fixed_eff_zi, et, "p.value")
          result_list[[paste0(et, "_conf_low_zi")]] <- safe_extract(fixed_eff_zi, et, "conf.low")
          result_list[[paste0(et, "_conf_high_zi")]] <- safe_extract(fixed_eff_zi, et, "conf.high")
        }
      }
    }

    result_list

  }, error = function(e) {
    list(
      config_id = config_id,
      climate_var = climate_var,
      outcome = outcome,
      status = "failed",
      error = as.character(e$message),
      n_obs = if (exists("complete_data")) nrow(complete_data) else NA_integer_
    )
  })

  return(result)
}
