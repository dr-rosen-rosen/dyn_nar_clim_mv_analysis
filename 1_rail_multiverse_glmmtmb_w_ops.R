# Rail Multiverse Analysis - Production Script (glmmTMB)
# Outcomes: injuries (hurdle), fatalities (hurdle), costs (hurdle with gamma)
# With operational covariates: train_miles, passenger_miles, staff_hours
# Mike Rose - Safety Climate Analysis

library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(arrow)
library(furrr)
library(glue)
library(here)
library(slider)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Window specifications
WINDOW_SPECS <- list(
  sma = tibble(window_size = c(3, 5, 10, 20)),
  ewma = tribble(
    ~lag_days, ~halflife_days,
    180,  60,
    180,  90,
    360,  90,
    360,  180,
    540,  180,
    540,  270,
    720,  180,
    720,  360,
    900,  270,
    900,  450,
    1080, 360,
    1080, 540
  )
)

# Operational variables configuration
OPS_VARS <- c("train_miles", "passenger_miles", "staff_hours")
OPS_ROLL_K <- 12L    # 12-month rolling window for between/within decomposition
OPS_LAG_K <- 1L      # 1-month lag to ensure temporal ordering
OPS_MIN_HIST <- 6L   # Minimum 6 months of history before computing features

# Expected format for operational data file:
# - org_id: Organization identifier (must match rail_raw)
# - yearmonth: Date in monthly format (e.g., "2020-01-01" for Jan 2020)
# - train_miles: Monthly train miles
# - passenger_miles: Monthly passenger miles  
# - staff_hours: Monthly staff hours

# Model specifications for each outcome
# Now includes operational covariates (between and within for each)
MODEL_FORMULAS <- list(
  injuries = list(
    conditional = paste0(
      "total_persons_injured ~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    ),
    zero_inflation = paste0(
      "~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    )
  ),
  fatalities = list(
    conditional = paste0(
      "total_persons_killed ~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    ),
    zero_inflation = paste0(
      "~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    )
  ),
  costs = list(
    conditional = paste0(
      "total_damage_cost ~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    ),
    zero_inflation = paste0(
      "~ yearmonth_num_c + sin_month + cos_month + ",
      "train_miles_between + train_miles_within + ",
      "passenger_miles_between + passenger_miles_within + ",
      "staff_hours_between + staff_hours_within + ",
      "CLIMATE_VAR + (1 | org_id)"
    )
  )
)

# ==============================================================================
# OPERATIONAL DATA PROCESSING
# ==============================================================================

#' Create operational features with rolling window between/within decomposition
#' 
#' This function operates on MONTHLY operational data (not event-level data).
#' For each operational variable, creates:
#' - {var}_between: Rolling mean of lagged values (stable operational scale)
#' - {var}_within: Deviation from rolling mean (temporal fluctuations)
#' 
#' @param ops_df Data frame with monthly operational data (one row per org-month)
#' @param date_var Name of yearmonth variable (should be Date class, first of month)
#' @param org_var Name of organization ID variable
#' @param vars Vector of operational variable names
#' @param lag_k Lag in months (default 1)
#' @param roll_k Rolling window size in months (default 12)
#' @param min_hist Minimum history required before computing features (default 6)
#' @param log1p_transform Whether to apply log1p transformation (default TRUE)
#' @param standardize Whether to standardize final features (default TRUE)
#' @return Data frame with org_id, yearmonth, and operational features ready for joining
make_ops_features_rolling <- function(ops_df,
                                      date_var = "yearmonth",
                                      org_var  = "org_id",
                                      vars     = OPS_VARS,
                                      lag_k    = OPS_LAG_K,
                                      roll_k   = OPS_ROLL_K,
                                      min_hist = OPS_MIN_HIST,
                                      log1p_transform = TRUE,
                                      standardize = TRUE) {
  
  # Check required columns exist
  required_cols <- c(date_var, org_var, vars)
  missing_cols <- setdiff(required_cols, names(ops_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns in operational data: %s",
                 paste(missing_cols, collapse = ", ")))
  }
  
  # Ensure yearmonth is Date class
  ops_df <- ops_df %>%
    mutate(!!date_var := as.Date(.data[[date_var]]))
  
  ops_df <- ops_df %>%
    arrange(.data[[org_var]], .data[[date_var]]) %>%
    group_by(.data[[org_var]])
  
  # Step 1: Log transform if requested
  for (v in vars) {
    col_name <- paste0(v, "_log")
    if (log1p_transform) {
      ops_df <- ops_df %>% mutate(!!col_name := log1p(.data[[v]]))
    } else {
      ops_df <- ops_df %>% mutate(!!col_name := .data[[v]])
    }
  }
  
  # Step 2: Create lagged values
  for (v in vars) {
    log_col <- paste0(v, "_log")
    lag_col <- paste0(v, "_lag", lag_k)
    ops_df <- ops_df %>% mutate(!!lag_col := dplyr::lag(.data[[log_col]], lag_k))
  }
  
  # Step 3: Compute rolling mean of lagged values (between component)
  for (v in vars) {
    lag_col <- paste0(v, "_lag", lag_k)
    between_col <- paste0(v, "_between_raw")
    ops_df <- ops_df %>%
      mutate(
        !!between_col := slider::slide_dbl(
          .data[[lag_col]],
          mean,
          .before = roll_k - 1,
          .complete = FALSE,
          na.rm = TRUE
        )
      )
  }
  
  # Step 4: Compute within component (lagged value minus rolling mean)
  for (v in vars) {
    lag_col <- paste0(v, "_lag", lag_k)
    between_col <- paste0(v, "_between_raw")
    within_col <- paste0(v, "_within_raw")
    ops_df <- ops_df %>%
      mutate(!!within_col := .data[[lag_col]] - .data[[between_col]])
  }
  
  # Step 5: Count history and enforce minimum
  first_lag_col <- paste0(vars[1], "_lag", lag_k)
  ops_df <- ops_df %>%
    mutate(ops_hist_n = cumsum(!is.na(.data[[first_lag_col]])))
  
  # Set features to NA if insufficient history
  for (v in vars) {
    between_raw <- paste0(v, "_between_raw")
    within_raw <- paste0(v, "_within_raw")
    ops_df <- ops_df %>%
      mutate(
        !!between_raw := ifelse(ops_hist_n >= min_hist, .data[[between_raw]], NA_real_),
        !!within_raw := ifelse(ops_hist_n >= min_hist, .data[[within_raw]], NA_real_)
      )
  }
  
  ops_df <- ops_df %>% ungroup()
  
  # Step 6: Standardize if requested (across all data)
  if (standardize) {
    for (v in vars) {
      between_raw <- paste0(v, "_between_raw")
      within_raw <- paste0(v, "_within_raw")
      between_final <- paste0(v, "_between")
      within_final <- paste0(v, "_within")
      
      # Standardize between
      between_mean <- mean(ops_df[[between_raw]], na.rm = TRUE)
      between_sd <- sd(ops_df[[between_raw]], na.rm = TRUE)
      if (is.na(between_sd) || between_sd < 1e-10) between_sd <- 1
      ops_df[[between_final]] <- (ops_df[[between_raw]] - between_mean) / between_sd
      
      # Standardize within
      within_mean <- mean(ops_df[[within_raw]], na.rm = TRUE)
      within_sd <- sd(ops_df[[within_raw]], na.rm = TRUE)
      if (is.na(within_sd) || within_sd < 1e-10) within_sd <- 1
      ops_df[[within_final]] <- (ops_df[[within_raw]] - within_mean) / within_sd
    }
  } else {
    # Just rename raw to final
    for (v in vars) {
      ops_df[[paste0(v, "_between")]] <- ops_df[[paste0(v, "_between_raw")]]
      ops_df[[paste0(v, "_within")]] <- ops_df[[paste0(v, "_within_raw")]]
    }
  }
  
  # Select only the columns needed for joining
  output_cols <- c(org_var, date_var, 
                   paste0(vars, "_between"),
                   paste0(vars, "_within"))
  
  ops_df <- ops_df %>% select(all_of(output_cols))
  
  return(ops_df)
}


#' Load and prepare operational features from file
#' 
#' @param ops_path Path to operational data file (CSV or Parquet)
#' @param ... Additional arguments passed to make_ops_features_rolling
#' @return Data frame with operational features ready for joining
load_and_prepare_ops_features <- function(ops_path, 
                                          ops_vars = c("train_miles", "passenger_miles", "staff_hours"),
                                          sentinel_values = c(-11111, -1111, -111, 99999),
                                          ...) {
  
  if (is.null(ops_path) || !file.exists(ops_path)) {
    message("No operational data file provided or file not found. Proceeding without operational covariates.")
    return(NULL)
  }
  
  # Load based on file extension
  ext <- tools::file_ext(ops_path)
  if (ext == "parquet") {
    ops_raw <- arrow::read_parquet(ops_path)
  } else if (ext %in% c("csv", "CSV")) {
    ops_raw <- readr::read_csv(ops_path, show_col_types = FALSE)
  } else {
    stop(sprintf("Unsupported file format: %s. Use .csv or .parquet", ext))
  }
  # After loading ops_raw, ensure yearmonth is proper Date:
  ops_raw <- ops_raw %>%
    mutate(yearmonth = as.Date(yearmonth))
  
  cat(sprintf("Loaded operational data: %d rows, %d organizations\n",
              nrow(ops_raw), n_distinct(ops_raw$org_id)))
  
  # --- 1. Replace sentinel values with NA ---
  # Common FRA placeholders: -11111, -1111, 99999, etc.
  ops_vars_present <- intersect(ops_vars, names(ops_raw))
  
  if (length(ops_vars_present) > 0) {
    n_sentinels <- ops_raw %>%
      summarise(across(all_of(ops_vars_present), 
                       ~sum(.x %in% sentinel_values, na.rm = TRUE))) %>%
      rowSums()
    
    if (n_sentinels > 0) {
      cat(sprintf("Replacing %d sentinel values (%s) with NA\n", 
                  n_sentinels, paste(sentinel_values, collapse = ", ")))
    }
    
    ops_raw <- ops_raw %>%
      mutate(across(all_of(ops_vars_present), 
                    ~if_else(.x %in% sentinel_values, NA_real_, as.numeric(.x))))
    
    # --- 2. Replace negative values with NA (shouldn't have negative miles/hours) ---
    n_negative <- ops_raw %>%
      summarise(across(all_of(ops_vars_present), 
                       ~sum(.x < 0, na.rm = TRUE))) %>%
      rowSums()
    
    if (n_negative > 0) {
      cat(sprintf("Replacing %d negative values with NA\n", n_negative))
    }
    
    ops_raw <- ops_raw %>%
      mutate(across(all_of(ops_vars_present), 
                    ~if_else(.x < 0, NA_real_, .x)))
  }
  
  # --- 3. Deduplicate: keep distinct org_id + yearmonth + ops vars ---
  # First check for duplicates
  key_cols <- c("org_id", "yearmonth", ops_vars_present)
  key_cols <- intersect(key_cols, names(ops_raw))
  
  n_before <- nrow(ops_raw)
  
  # Option A: Take distinct rows on key columns
  ops_raw <- ops_raw %>%
    distinct(across(all_of(key_cols)), .keep_all = TRUE)
  
  n_after_distinct <- nrow(ops_raw)
  
  if (n_before != n_after_distinct) {
    cat(sprintf("Removed %d exact duplicate rows\n", n_before - n_after_distinct))
  }
  
  # Option B: If still duplicates on (org_id, yearmonth), aggregate
  n_dupes <- ops_raw %>%
    group_by(org_id, yearmonth) %>%
    filter(n() > 1) %>%
    nrow()
  
  if (n_dupes > 0) {
    cat(sprintf("Aggregating %d remaining duplicate org-months (summing ops vars)\n", n_dupes))
    
    ops_raw <- ops_raw %>%
      group_by(org_id, yearmonth) %>%
      summarise(
        across(all_of(ops_vars_present), ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }
  
  # Final duplicate check
  final_dupes <- ops_raw %>%
    group_by(org_id, yearmonth) %>%
    filter(n() > 1) %>%
    nrow()
  
  if (final_dupes > 0) {
    warning(sprintf("Still have %d duplicate org-months after cleaning!", final_dupes))
  }
  
  cat(sprintf("Clean operational data: %d rows, %d organizations\n",
              nrow(ops_raw), n_distinct(ops_raw$org_id)))
  
  # --- 4. Create features ---
  ops_features <- make_ops_features_rolling(ops_raw, ...)
  
  cat(sprintf("Created operational features: %d org-months with complete data\n",
              sum(complete.cases(ops_features))))
  
  return(ops_features)
}

# Helper to align column names and date format from raw FRA operational data download
format_ops_data <- function(ops_path, overwrite = FALSE) {
  ops_df <- read.csv(ops_path) %>%
    rename(
      org_id = RAILROAD,
      train_miles = TOTMI, # there are other breakdowns for miles, this is total
      passenger_miles = PASSMI,
      staff_hours = EMPHRS) %>%
    mutate(
      IYR_full = if_else(between(IYR,0,25),2000+IYR,1900+IYR),
      yearmonth = paste0(IYR_full,"-",IMO,"-01")
    )
  if (overwrite) {
    write_csv(ops_df,ops_path)
  }
  return(ops_df)
}

# ==============================================================================
# WINDOW CREATION FUNCTIONS
# ==============================================================================

ewma_time_decay_irregular_lag <- function(x, dates, window_days, half_life_days, min_n = 1) {
  n <- length(x)
  result <- rep(NA_real_, n)
  lambda <- log(2) / half_life_days
  
  for (i in 2:n) {
    lookback_start <- dates[i] - window_days
    prior_idx <- which(dates[1:(i-1)] >= lookback_start)
    
    if (length(prior_idx) >= min_n) {
      prior_dates <- dates[prior_idx]
      prior_vals <- x[prior_idx]
      
      valid <- !is.na(prior_vals)
      if (sum(valid) >= min_n) {
        prior_dates <- prior_dates[valid]
        prior_vals <- prior_vals[valid]
        
        time_diffs <- as.numeric(dates[i] - prior_dates)
        weights <- exp(-lambda * time_diffs)
        
        result[i] <- sum(prior_vals * weights) / sum(weights)
      }
    }
  }
  return(result)
}

create_all_windows <- function(df, climate_var_base = "overall_final_score", min_n = 1) {
  
  # Ensure temporal variables exist
  df <- df %>%
    mutate(
      year = lubridate::year(event_date),
      month = lubridate::month(event_date),
      yearmonth_num = (year - min(year)) * 12 + month,
      yearmonth_num_c = as.vector(scale(yearmonth_num, center = TRUE, scale = TRUE)),
      sin_month = sin(2 * pi * month / 12),
      cos_month = cos(2 * pi * month / 12)
    ) %>%
    arrange(org_id, event_date) %>%
    group_by(org_id)
  
  # Add SMA windows
  for (win in WINDOW_SPECS$sma$window_size) {
    var_name <- glue("{climate_var_base}_sma_{win}")
    
    df <- df %>%
      mutate(
        !!var_name := slider::slide_dbl(
          lag(.data[[climate_var_base]], 1),
          ~ mean(.x, na.rm = TRUE),
          .before = win - 1,
          .complete = TRUE
        )
      )
  }
  
  df <- df %>% ungroup()
  
  # Add EWMA windows
  for (i in 1:nrow(WINDOW_SPECS$ewma)) {
    lag_d <- WINDOW_SPECS$ewma$lag_days[i]
    hl_d <- WINDOW_SPECS$ewma$halflife_days[i]
    var_name <- glue("{climate_var_base}_ewmaLAG_{lag_d}d_hl{hl_d}d")
    
    df <- df %>%
      arrange(org_id, event_date) %>%
      group_by(org_id) %>%
      mutate(
        !!var_name := ewma_time_decay_irregular_lag(
          .data[[climate_var_base]], event_date, lag_d, hl_d, min_n
        )
      ) %>%
      ungroup()
  }
  
  return(df)
}

# ==============================================================================
# MODEL FITTING FUNCTION
# ==============================================================================

#' Fit a single glmmTMB hurdle model
#' @param data Data frame with all necessary variables
#' @param climate_var Name of the climate variable to use
#' @param config_id Configuration ID for tracking
#' @param outcome Outcome variable ("injuries", "fatalities", or "costs")
fit_single_rail_model <- function(data, climate_var, config_id, outcome = "injuries") {
  
  # Get outcome variable name
  outcome_var <- case_when(
    outcome == "injuries" ~ "total_persons_injured",
    outcome == "fatalities" ~ "total_persons_killed",
    outcome == "costs" ~ "total_damage_cost"
  )
  
  # Operational variable names
  ops_between_vars <- paste0(OPS_VARS, "_between")
  ops_within_vars <- paste0(OPS_VARS, "_within")
  all_ops_vars <- c(ops_between_vars, ops_within_vars)
  
  # Add this BEFORE the filter to debug
  cat("=== DIAGNOSTIC ===\n")
  cat("OPS_VARS:", paste(OPS_VARS, collapse = ", "), "\n")
  cat("all_ops_vars:", paste(all_ops_vars, collapse = ", "), "\n")
  cat("\nColumns in data containing 'train':\n")
  print(grep("train", names(data), value = TRUE))
  cat("\nColumns in data containing 'between' or 'within':\n")
  print(grep("between|within", names(data), value = TRUE))
  cat("\nMissing ops vars:\n")
  print(setdiff(all_ops_vars, names(data)))
  cat("=================\n")

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
      # filter(
      #   !is.na(.data[[climate_var]]),
      #   !is.na(.data[[outcome_var]]),
      #   !is.na(yearmonth_num_c),
      #   !is.na(sin_month),
      #   !is.na(cos_month),
      #   !is.na(org_id),
      #   # Operational variables - check all exist and are non-NA
      #   across(all_of(all_ops_vars), ~ !is.na(.x))
      # )
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
    
    # Helper function to safely extract coefficient info
    safe_extract <- function(df, term_name, col) {
      row <- df %>% filter(term == term_name)
      if (nrow(row) > 0) row[[col]][1] else NA_real_
    }
    
    # Extract fixed effects based on model type
    if (outcome == "costs" && is.list(fit) && !is.null(fit$type)) {
      # Hurdle gamma model: extract from both parts
      
      # Binary part (whether damage occurs)
      fixed_eff_binary <- tidy(fit$binary, effects = "fixed", conf.int = TRUE)
      # Gamma part (damage amount given damage)
      fixed_eff_gamma <- tidy(fit$gamma, effects = "fixed", conf.int = TRUE)
      
      # Climate effects
      climate_row_zi <- fixed_eff_binary %>% filter(term == climate_var)
      climate_row_cond <- fixed_eff_gamma %>% filter(term == climate_var)
      
      # Operational effects - Conditional (Gamma)
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
      
      # Operational effects - ZI (Binary)
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
      
      # Operational effects - Conditional
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
      
      # Operational effects - ZI
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
      
      # Extract random effects
      random_eff <- tidy(fit, effects = "ran_pars")
      re_intercept_cond <- random_eff %>% 
        filter(term == "sd__(Intercept)", component == "cond")
      re_intercept_zi <- random_eff %>% 
        filter(term == "sd__(Intercept)", component == "zi")
      
      # Model fit statistics
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
      n_obs = if(exists("complete_data")) nrow(complete_data) else NA_integer_
    )
  })
  
  return(result)
}

# ==============================================================================
# MAIN ANALYSIS FUNCTION FOR SINGLE CONFIGURATION
# ==============================================================================

#' Run all window analyses for a single configuration
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
  
  # Load and prepare data
 df <- arrow::read_parquet(parquet_path) %>%
    select(
      ends_with('_id'),
      starts_with('overall_'),
      ends_with('_domain_score')
    ) %>%
    rename(eid = report_id) %>%
    left_join(rail_raw, by = 'eid') %>%
    filter(!is.na(event_date)) %>%
    group_by(org_id) %>%
    filter(n() > min_reports) %>%
    arrange(event_date) %>%
    mutate(t = row_number()) %>%
    ungroup()
  
  df$config_id <- config_id
  df$source_file <- normalizePath(parquet_path)
  
  # Create all windowed climate variables
  df <- create_all_windows(df, climate_var_base = "overall_final_score", min_n = min_n)
  
  # Create yearmonth for joining with operational data
  df <- df %>%
    mutate(yearmonth = as.Date(lubridate::floor_date(event_date, "month")))
  
  # Join operational features if provided
  if (!is.null(ops_features)) {
    df <- df %>%
      left_join(ops_features, by = c("org_id", "yearmonth"))
    
    # Report join success
    ops_cols <- paste0(OPS_VARS[1], "_between")
    n_with_ops <- sum(!is.na(df[[ops_cols]]))
    cat(sprintf("  Config %s: %d/%d events matched to operational data (%.1f%%)\n",
                config_id, n_with_ops, nrow(df), 100*n_with_ops/nrow(df)))
  } else {
    # Create placeholder columns with NA if no operational data
    for (v in OPS_VARS) {
      df[[paste0(v, "_between")]] <- NA_real_
      df[[paste0(v, "_within")]] <- NA_real_
    }
  }
  
  
  # Get all climate variable names
  climate_vars <- c(
    names(df) %>% str_subset("overall_final_score_sma_"),
    names(df) %>% str_subset("overall_final_score_ewmaLAG_")
  )
  
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
      date_var = "yearmonth",
      org_var = "org_id",
      vars = OPS_VARS,
      lag_k = OPS_LAG_K,
      roll_k = OPS_ROLL_K,
      min_hist = OPS_MIN_HIST,
      log1p_transform = TRUE,
      standardize = TRUE
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
  outcome_var <- case_when(
    outcome == "injuries" ~ "total_persons_injured",
    outcome == "fatalities" ~ "total_persons_killed",
    outcome == "costs" ~ "total_damage_cost"
  )
  
  rail_raw_minimal <- rail_raw %>%
    select(eid, org_id, event_date, all_of(outcome_var)) %>%
    distinct()
  
  cat(sprintf("Reduced rail_raw from %.1f MB to %.1f MB\n",
              object.size(rail_raw)/1e6,
              object.size(rail_raw_minimal)/1e6))
  
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
        analyze_single_config_rail = analyze_single_config_rail,
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
              100*length(successful)/length(all_results_flat)))
  cat(sprintf("  Failed: %d (%.1f%%)\n\n",
              length(failed),
              100*length(failed)/length(all_results_flat)))
  
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
  
  # Extract results table - now with operational effects
  results_table <- map_dfr(successful, function(r) {
    # Base tibble with core results
    base_result <- tibble(
      config_id = r$config_id,
      climate_var = r$climate_var,
      outcome = r$outcome,
      status = r$status,
      
      # Climate effects - Conditional
      climate_estimate_cond = r$climate_estimate_cond,
      climate_se_cond = r$climate_se_cond,
      climate_pval_cond = r$climate_pval_cond,
      climate_conf_low_cond = r$climate_conf_low_cond,
      climate_conf_high_cond = r$climate_conf_high_cond,
      
      # Climate effects - ZI
      climate_estimate_zi = r$climate_estimate_zi,
      climate_se_zi = r$climate_se_zi,
      climate_pval_zi = r$climate_pval_zi,
      climate_conf_low_zi = r$climate_conf_low_zi,
      climate_conf_high_zi = r$climate_conf_high_zi,
      
      # Random effects
      random_intercept_sd_cond = r$random_intercept_sd_cond,
      random_intercept_sd_zi = r$random_intercept_sd_zi,
      
      # Fit stats
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
      # Conditional
      base_result[[paste0(v, "_estimate_cond")]] <- r[[paste0(v, "_estimate_cond")]] %||% NA_real_
      base_result[[paste0(v, "_se_cond")]] <- r[[paste0(v, "_se_cond")]] %||% NA_real_
      base_result[[paste0(v, "_pval_cond")]] <- r[[paste0(v, "_pval_cond")]] %||% NA_real_
      base_result[[paste0(v, "_conf_low_cond")]] <- r[[paste0(v, "_conf_low_cond")]] %||% NA_real_
      base_result[[paste0(v, "_conf_high_cond")]] <- r[[paste0(v, "_conf_high_cond")]] %||% NA_real_
      # ZI
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
