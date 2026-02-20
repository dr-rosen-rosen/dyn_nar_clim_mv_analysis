# 1_rail_multiverse_glmmtmb_w_ops.R
# Rail Multiverse Analysis - Production Script (glmmTMB)
# Outcomes: injuries (hurdle), fatalities (hurdle), costs (hurdle with gamma)
# With operational covariates: train_miles, passenger_miles, staff_hours
# Mike Rose - Safety Climate Analysis
#
# This script sources shared code from common/ and provides:
#   - fit_single_rail_model()      : Fit one hurdle model for a given climate var
#   - analyze_single_config_rail() : Run all window analyses for one config
#   - run_rail_multiverse()        : Parallel batch runner across all configs

library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(arrow)
library(furrr)
library(glue)
library(here)
library(slider)

# Source shared modules
# Adjust path as needed depending on working directory
common_dir <- file.path(here::here(), "common")
if (!dir.exists(common_dir)) {
  # Fallback: try relative to script location
  common_dir <- "common"
}
source(file.path(common_dir, "config.R"))
source(file.path(common_dir, "data_prep.R"))
# formulas.R not strictly needed for the multiverse script but available if needed
# source(file.path(common_dir, "formulas.R"))


# ==============================================================================
# MODEL FITTING FUNCTION
# ==============================================================================

#' Fit a single glmmTMB hurdle model
#' @param data Data frame with all necessary variables
#' @param climate_var Name of the climate variable to use
#' @param config_id Configuration ID for tracking
#' @param outcome Outcome variable ("injuries", "fatalities", or "costs")
fit_single_rail_model <- function(data, climate_var, config_id, outcome = "injuries") {
  
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
    cond_formula_str <- gsub("CLIMATE_VAR", climate_var, formulas$conditional)
    zi_formula_str <- gsub("CLIMATE_VAR", climate_var, formulas$zero_inflation)
    
    # Helper function to safely extract coefficient info
    safe_extract <- function(df, term_name, col) {
      row <- df %>% filter(term == term_name)
      if (nrow(row) > 0) row[[col]][1] else NA_real_
    }
    
    # Fit hurdle model based on outcome
    if (outcome == "costs") {
      # For costs: Filter to positive values for Gamma model
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
      # For counts: Use truncated poisson with zero-inflation
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
      
      # Operational effects
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
      
      # Random effects
      random_eff_binary <- tidy(fit$binary, effects = "ran_pars")
      random_eff_gamma <- tidy(fit$gamma, effects = "ran_pars")
      
      re_intercept_zi <- random_eff_binary %>% filter(term == "sd__(Intercept)")
      re_intercept_cond <- random_eff_gamma %>% filter(term == "sd__(Intercept)")
      
      # Fit statistics
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
      
      # Climate effects - Conditional
      climate_estimate_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$estimate[1] else NA_real_,
      climate_se_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$std.error[1] else NA_real_,
      climate_pval_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$p.value[1] else NA_real_,
      climate_conf_low_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$conf.low[1] else NA_real_,
      climate_conf_high_cond = if (nrow(climate_row_cond) > 0) climate_row_cond$conf.high[1] else NA_real_,
      
      # Climate effects - ZI
      climate_estimate_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$estimate[1] else NA_real_,
      climate_se_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$std.error[1] else NA_real_,
      climate_pval_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$p.value[1] else NA_real_,
      climate_conf_low_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$conf.low[1] else NA_real_,
      climate_conf_high_zi = if (nrow(climate_row_zi) > 0) climate_row_zi$conf.high[1] else NA_real_,
      
      # Random effects
      random_intercept_sd_cond = if (nrow(re_intercept_cond) > 0) re_intercept_cond$estimate[1] else NA_real_,
      random_intercept_sd_zi = if (nrow(re_intercept_zi) > 0) re_intercept_zi$estimate[1] else NA_real_,
      
      # Fit statistics
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


# ==============================================================================
# MAIN ANALYSIS FUNCTION FOR SINGLE CONFIGURATION
# ==============================================================================

#' Run all window analyses for a single configuration
#' 
#' Uses prepare_config_data() from common/data_prep.R for data preparation,
#' then fits hurdle models for each windowed climate variable.
#' 
#' @param parquet_path Path to parquet file
#' @param config_id Configuration ID
#' @param rail_raw Raw rail data for joining (event-level, irregular time series)
#' @param ops_features Pre-computed operational features (monthly, or NULL)
#' @param outcome Outcome to model ("injuries", "fatalities", or "costs")
#' @param min_reports Minimum reports per org
#' @param min_n Minimum observations for window calculation
analyze_single_config_rail <- function(parquet_path, config_id, rail_raw,
                                       ops_features = NULL,
                                       outcome = "injuries",
                                       min_reports = 50, min_n = 1) {
  
  # Prepare data using shared function
  df <- prepare_config_data(parquet_path, config_id, rail_raw,
                            ops_features = ops_features,
                            min_reports = min_reports, min_n = min_n)
  
  # Get all climate variable names
  climate_vars <- get_climate_vars(df)
  
  # Fit models for each window
  results_list <- map(climate_vars, function(cvar) {
    fit_single_rail_model(df, cvar, config_id, outcome)
  })
  
  return(results_list)
}


# ==============================================================================
# BATCH PROCESSING FUNCTION
# ==============================================================================

#' Run multiverse analysis across all configurations
#' @param cfg_dir Directory containing configuration folders
#' @param rail_raw Raw rail data for joining (event-level data only)
#' @param ops_path Path to operational data file (CSV or Parquet with monthly data)
#' @param outcome Outcome to analyze ("injuries", "fatalities", or "costs")
#' @param config_ids Optional: specific config IDs to run
#' @param n_cores Number of cores for parallel processing
#' @param save_models Whether to save full model objects
#' @param output_dir Directory for saving results
run_rail_multiverse <- function(cfg_dir, 
                                rail_raw,
                                ops_path = NULL,
                                outcome = "injuries",
                                config_ids = NULL,
                                n_cores = 16,
                                save_models = TRUE,
                                output_dir = "results") {
  
  cat("========================================\n")
  cat(sprintf("RAIL MULTIVERSE ANALYSIS (glmmTMB) - %s\n", toupper(outcome)))
  cat("========================================\n\n")
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  if (save_models) {
    dir.create(file.path(output_dir, "model_objects"), showWarnings = FALSE)
  }
  
  # Load and prepare operational features (once, before parallel processing)
  ops_features <- NULL
  if (!is.null(ops_path) && file.exists(ops_path)) {
    cat("Loading and preparing operational data...\n")
    ops_features <- load_and_prepare_ops_features(
      ops_path,
      ops_vars = OPS_VARS
    )
    cat(sprintf("Operational features ready: %d org-months\n\n", nrow(ops_features)))
  } else {
    cat("No operational data provided. Models will be fit without operational covariates.\n")
    cat("To include operational data, provide ops_path argument.\n\n")
  }
  
  # Get all configuration IDs
  if (is.null(config_ids)) {
    cfg_path <- file.path(cfg_dir, "_cfg")
    if (!dir.exists(cfg_path)) {
      stop(sprintf("Configuration directory not found: %s", cfg_path))
    }
    config_ids <- list.dirs(cfg_path, full.names = FALSE, recursive = FALSE)
    config_ids <- config_ids[config_ids != "" & !startsWith(config_ids, ".")]
  }
  
  # Reduce rail_raw to only necessary columns (event-level data)
  outcome_var <- get_outcome_var(outcome)
  
  rail_raw_minimal <- rail_raw %>%
    select(eid, org_id, event_date, all_of(outcome_var)) %>%
    distinct()
  
  cat(sprintf("Reduced rail_raw from %.1f MB to %.1f MB\n",
              object.size(rail_raw) / 1e6,
              object.size(rail_raw_minimal) / 1e6))
  
  n_configs <- length(config_ids)
  n_windows <- nrow(WINDOW_SPECS$sma) + nrow(WINDOW_SPECS$ewma)
  total_models <- n_configs * n_windows
  
  cat(sprintf("Configurations to analyze: %d\n", n_configs))
  cat(sprintf("Windows per configuration: %d\n", n_windows))
  cat(sprintf("Total models to fit: %d\n", total_models))
  cat(sprintf("Using %d cores\n\n", n_cores))
  
  # Setup parallel processing
  plan(multisession, workers = n_cores)
  
  # Process all configurations
  start_time <- Sys.time()
  cat("Processing configurations...\n")
  progressr::with_progress({
    p <- progressr::progressor(steps = n_configs)
    all_results <- future_map(config_ids, function(cfg_id) {
      
      parquet_path <- file.path(cfg_dir, "_cfg", cfg_id, "results.parquet")
      
      if (!file.exists(parquet_path)) {
        p()
        return(list(list(
          config_id = cfg_id,
          climate_var = NA_character_,
          outcome = outcome,
          status = "failed",
          error = "Parquet file not found",
          n_obs = NA_integer_
        )))
      }
      
      cfg_results <- tryCatch({
        analyze_single_config_rail(parquet_path, cfg_id, rail_raw_minimal, 
                                   ops_features = ops_features, outcome = outcome)
      }, error = function(e) {
        list(list(
          config_id = cfg_id,
          climate_var = NA_character_,
          outcome = outcome,
          status = "failed",
          error = as.character(e$message),
          n_obs = NA_integer_
        ))
      })
      p()
      return(cfg_results)
      
    }, .options = furrr_options(
      seed = TRUE,
      packages = c("tidyverse", "glmmTMB", "broom.mixed", "arrow", "glue", "slider"),
      globals = list(
        cfg_dir = cfg_dir,
        rail_raw_minimal = rail_raw_minimal,
        ops_features = ops_features,
        outcome = outcome,
        WINDOW_SPECS = WINDOW_SPECS,
        MODEL_FORMULAS = MODEL_FORMULAS,
        OPS_VARS = OPS_VARS,
        OPS_LAG_K = OPS_LAG_K,
        OPS_ROLL_K = OPS_ROLL_K,
        OPS_MIN_HIST = OPS_MIN_HIST,
        OUTCOME_VARS = OUTCOME_VARS,
        get_outcome_var = get_outcome_var,
        analyze_single_config_rail = analyze_single_config_rail,
        prepare_config_data = prepare_config_data,
        get_climate_vars = get_climate_vars,
        create_all_windows = create_all_windows,
        ewma_time_decay_irregular_lag = ewma_time_decay_irregular_lag,
        fit_single_rail_model = fit_single_rail_model
      )
    ))
  })
  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "mins")
  
  cat(sprintf("\n\nCompleted in %.1f minutes\n\n", elapsed))
  
  # Flatten results
  all_results_flat <- flatten(all_results)
  
  # Separate successful and failed
  successful <- keep(all_results_flat, ~.x$status %in% c("success", "convergence_warning"))
  failed <- keep(all_results_flat, ~.x$status == "failed")
  
  cat(sprintf("Results Summary:\n"))
  cat(sprintf("  Successful: %d (%.1f%%)\n", 
              length(successful), 
              100 * length(successful) / length(all_results_flat)))
  cat(sprintf("  Failed: %d (%.1f%%)\n\n",
              length(failed),
              100 * length(failed) / length(all_results_flat)))
  
  # Save failed results
  if (length(failed) > 0) {
    failed_table <- map_dfr(failed, function(r) {
      tibble(
        config_id = r$config_id %||% NA_character_,
        climate_var = r$climate_var %||% NA_character_,
        outcome = r$outcome %||% outcome,
        status = r$status %||% "failed",
        error = r$error %||% "Unknown error",
        n_obs = r$n_obs %||% NA_integer_
      )
    })
    failed_path <- file.path(output_dir, sprintf("rail_%s_failed_models.csv", outcome))
    write_csv(failed_table, failed_path)
    cat(sprintf("Failed models log saved to: %s\n", failed_path))
    
    cat("\nSample of failure reasons:\n")
    error_summary <- failed_table %>%
      count(error, sort = TRUE) %>%
      head(10)
    print(error_summary)
  }
  
  # Early return if no successes
  if (length(successful) == 0) {
    cat("\nWARNING: No successful models! Check failed_models.csv for errors.\n")
    plan(sequential)
    return(list(
      results_table = tibble(),
      n_successful = 0,
      n_failed = length(failed),
      elapsed_minutes = as.numeric(elapsed)
    ))
  }
  
  # Extract results table
  results_table <- map_dfr(successful, function(r) {
    base_result <- tibble(
      config_id = r$config_id,
      climate_var = r$climate_var,
      outcome = r$outcome,
      status = r$status,
      
      climate_estimate_cond = r$climate_estimate_cond,
      climate_se_cond = r$climate_se_cond,
      climate_pval_cond = r$climate_pval_cond,
      climate_conf_low_cond = r$climate_conf_low_cond,
      climate_conf_high_cond = r$climate_conf_high_cond,
      
      climate_estimate_zi = r$climate_estimate_zi,
      climate_se_zi = r$climate_se_zi,
      climate_pval_zi = r$climate_pval_zi,
      climate_conf_low_zi = r$climate_conf_low_zi,
      climate_conf_high_zi = r$climate_conf_high_zi,
      
      random_intercept_sd_cond = r$random_intercept_sd_cond,
      random_intercept_sd_zi = r$random_intercept_sd_zi,
      
      AIC = r$AIC,
      BIC = r$BIC,
      logLik = r$logLik,
      n_obs = r$n_obs,
      n_orgs = r$n_orgs,
      n_zeros = r$n_zeros,
      n_nonzeros = r$n_nonzeros,
      pct_zeros = r$pct_zeros
    )
    
    # Add operational effects dynamically
    ops_between_vars <- paste0(OPS_VARS, "_between")
    ops_within_vars <- paste0(OPS_VARS, "_within")
    
    for (v in c(ops_between_vars, ops_within_vars)) {
      base_result[[paste0(v, "_estimate_cond")]] <- r[[paste0(v, "_estimate_cond")]] %||% NA_real_
      base_result[[paste0(v, "_se_cond")]] <- r[[paste0(v, "_se_cond")]] %||% NA_real_
      base_result[[paste0(v, "_pval_cond")]] <- r[[paste0(v, "_pval_cond")]] %||% NA_real_
      base_result[[paste0(v, "_conf_low_cond")]] <- r[[paste0(v, "_conf_low_cond")]] %||% NA_real_
      base_result[[paste0(v, "_conf_high_cond")]] <- r[[paste0(v, "_conf_high_cond")]] %||% NA_real_
      base_result[[paste0(v, "_estimate_zi")]] <- r[[paste0(v, "_estimate_zi")]] %||% NA_real_
      base_result[[paste0(v, "_se_zi")]] <- r[[paste0(v, "_se_zi")]] %||% NA_real_
      base_result[[paste0(v, "_pval_zi")]] <- r[[paste0(v, "_pval_zi")]] %||% NA_real_
      base_result[[paste0(v, "_conf_low_zi")]] <- r[[paste0(v, "_conf_low_zi")]] %||% NA_real_
      base_result[[paste0(v, "_conf_high_zi")]] <- r[[paste0(v, "_conf_high_zi")]] %||% NA_real_
    }
    
    base_result
  })
  
  # Add window metadata
  results_table <- results_table %>%
    mutate(
      window_type = case_when(
        str_detect(.data$climate_var, "_sma_") ~ "sma",
        str_detect(.data$climate_var, "_ewmaLAG_") ~ "ewma",
        TRUE ~ "unknown"
      ),
      window_size = case_when(
        .data$window_type == "sma" ~ as.numeric(str_extract(.data$climate_var, "(?<=_sma_)\\d+")),
        TRUE ~ NA_real_
      ),
      lag_days = case_when(
        .data$window_type == "ewma" ~ as.numeric(str_extract(.data$climate_var, "(?<=_ewmaLAG_)\\d+")),
        TRUE ~ NA_real_
      ),
      halflife_days = case_when(
        .data$window_type == "ewma" ~ as.numeric(str_extract(.data$climate_var, "(?<=_hl)\\d+")),
        TRUE ~ NA_real_
      )
    )
  
  # Save results table
  results_path <- file.path(output_dir, sprintf("rail_%s_multiverse_results.parquet", outcome))
  arrow::write_parquet(results_table, results_path)
  cat(sprintf("Results table saved to: %s\n", results_path))
  
  # Save model objects if requested
  if (save_models) {
    models_path <- file.path(output_dir, "model_objects", 
                             sprintf("rail_%s_models.rds", outcome))
    saveRDS(successful, models_path)
    cat(sprintf("Model objects saved to: %s\n", models_path))
  }
  
  # Clean up
  plan(sequential)
  
  return(list(
    results_table = results_table,
    n_successful = length(successful),
    n_failed = length(failed),
    elapsed_minutes = as.numeric(elapsed)
  ))
}
