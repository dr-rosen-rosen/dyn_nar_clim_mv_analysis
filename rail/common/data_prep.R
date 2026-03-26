# common/data_prep.R
# Shared data preparation functions for rail safety multiverse analysis
# Sourced by: 1_rail_multiverse_glmmtmb_w_ops.R, 2_rail_cv_binary.R
#
# Dependencies: tidyverse, arrow, slider, lubridate, glue
# Also requires: common/config.R to be sourced first

# ==============================================================================
# OPERATIONAL DATA PROCESSING
# ==============================================================================

#' Create operational features with rolling window between/within decomposition
#' 
#' Operates on MONTHLY operational data (not event-level data).
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
#' @param ops_vars Vector of operational variable names to process
#' @param sentinel_values Values to treat as missing in raw data
#' @param ... Additional arguments passed to make_ops_features_rolling
#' @return Data frame with operational features ready for joining
load_and_prepare_ops_features <- function(ops_path, 
                                          ops_vars = OPS_VARS,
                                          sentinel_values = c(-11111, -1111, -111, 99999),
                                          ...) {
  
  if (is.null(ops_path) || !file.exists(ops_path)) {
    message("No operational data file provided or file not found. ",
            "Proceeding without operational covariates.")
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
  
  # Ensure yearmonth is proper Date
  ops_raw <- ops_raw %>%
    mutate(yearmonth = as.Date(yearmonth))
  
  cat(sprintf("Loaded operational data: %d rows, %d organizations\n",
              nrow(ops_raw), n_distinct(ops_raw$org_id)))
  
  # --- 1. Replace sentinel values with NA ---
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
    
    # --- 2. Replace negative values with NA ---
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
  
  # --- 3. Deduplicate ---
  key_cols <- c("org_id", "yearmonth", ops_vars_present)
  key_cols <- intersect(key_cols, names(ops_raw))
  
  n_before <- nrow(ops_raw)
  ops_raw <- ops_raw %>%
    distinct(across(all_of(key_cols)), .keep_all = TRUE)
  n_after_distinct <- nrow(ops_raw)
  
  if (n_before != n_after_distinct) {
    cat(sprintf("Removed %d exact duplicate rows\n", n_before - n_after_distinct))
  }
  
  # If still duplicates on (org_id, yearmonth), aggregate
  n_dupes <- ops_raw %>%
    group_by(org_id, yearmonth) %>%
    filter(n() > 1) %>%
    nrow()
  
  if (n_dupes > 0) {
    cat(sprintf("Aggregating %d remaining duplicate org-months (summing ops vars)\n",
                n_dupes))
    
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


#' Helper to align column names from raw FRA operational data download
#' @param ops_path Path to raw FRA CSV
#' @param overwrite Whether to overwrite the file with cleaned version
#' @return Data frame with standardized column names
format_ops_data <- function(ops_path, overwrite = FALSE) {
  ops_df <- read.csv(ops_path) %>%
    rename(
      org_id = RAILROAD,
      train_miles = TOTMI,
      passenger_miles = PASSMI,
      staff_hours = EMPHRS
    ) %>%
    mutate(
      IYR_full = if_else(between(IYR, 0, 25), 2000 + IYR, 1900 + IYR),
      yearmonth = paste0(IYR_full, "-", IMO, "-01")
    )
  if (overwrite) {
    readr::write_csv(ops_df, ops_path)
  }
  return(ops_df)
}


# ==============================================================================
# WINDOW CREATION FUNCTIONS
# ==============================================================================

#' EWMA with time-decay for irregularly spaced events
#' @param x Numeric vector of values
#' @param dates Date vector (same length as x)
#' @param window_days Lookback window in days
#' @param half_life_days Half-life for exponential decay
#' @param min_n Minimum number of prior observations required
#' @return Numeric vector of EWMA values
ewma_time_decay_irregular_lag <- function(x, dates, window_days, half_life_days,
                                          min_n = 1) {
  n <- length(x)
  result <- rep(NA_real_, n)
  
  # Guard: need at least 2 observations
  if (n <= 1) return(result)
  
  # Ensure dates are Date class (not POSIXct) for clean arithmetic
  dates <- as.Date(dates)
  
  lambda <- log(2) / half_life_days
  
  for (i in 2:n) {
    lookback_start <- dates[i] - as.integer(window_days)
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


#' Create all windowed climate variables (SMA and EWMA) for a data frame
#' 
#' Also creates temporal variables: year, month, yearmonth_num, yearmonth_num_c,
#' sin_month, cos_month.
#' 
#' @param df Data frame with org_id, event_date, and the climate base variable
#' @param climate_var_base Name of the base climate variable (default "overall_final_score")
#' @param min_n Minimum observations for window calculation
#' @return Data frame with all windowed climate variables added
create_all_windows <- function(df, climate_var_base = "overall_final_score",
                               min_n = 1) {
  
  # Ensure event_date is Date class (not POSIXct)
  df <- df %>%
    mutate(event_date = as.Date(event_date)) %>%
    filter(!is.na(event_date))
  
  if (nrow(df) == 0) {
    warning("No valid event_date rows in data")
    return(df)
  }
  
  # Compute temporal variables (min year is global, across all orgs)
  min_year <- min(lubridate::year(df$event_date))
  
  df <- df %>%
    mutate(
      year = lubridate::year(event_date),
      month = lubridate::month(event_date),
      yearmonth_num = (year - min_year) * 12 + month,
      yearmonth_num_c = as.vector(scale(yearmonth_num, center = TRUE, scale = TRUE)),
      sin_month = sin(2 * pi * month / 12),
      cos_month = cos(2 * pi * month / 12)
    ) %>%
    arrange(org_id, event_date) %>%
    group_by(org_id)
  
  # Add SMA windows
  for (win in WINDOW_SPECS$sma$window_size) {
    var_name <- glue::glue("{climate_var_base}_sma_{win}")
    
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
    var_name <- glue::glue("{climate_var_base}_ewmaLAG_{lag_d}d_hl{hl_d}d")
    
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
# UNIFIED DATA PREPARATION
# ==============================================================================

#' Prepare data for a single configuration
#' 
#' Loads the config parquet, joins to rail_raw and ops_features, creates all
#' windowed climate variables. Returns a ready-to-model data frame with all
#' climate variable columns.
#' 
#' Used by both the multiverse analysis (script 1) and cross-validation (script 2).
#' 
#' @param parquet_path Path to the configuration's parquet file
#' @param config_id Configuration ID string
#' @param rail_raw Raw rail data (event-level) with eid, org_id, event_date, outcome
#' @param ops_features Pre-computed operational features (monthly), or NULL
#' @param min_reports Minimum reports per org to include
#' @param min_n Minimum observations for window calculation
#' @return Data frame ready for model fitting, with all climate vars and covariates
prepare_config_data <- function(parquet_path, config_id, rail_raw,
                                ops_features = NULL,
                                min_reports = 50, min_n = 1) {
  
  if (!file.exists(parquet_path)) {
    stop(sprintf("Parquet file not found: %s", parquet_path))
  }
  
  # Load and join
  df <- arrow::read_parquet(parquet_path) %>%
    select(
      ends_with('_id'),
      starts_with('overall_'),
      ends_with('_domain_score')
    ) %>%
    rename(eid = report_id) %>%
    left_join(rail_raw, by = 'eid') %>%
    mutate(event_date = as.Date(event_date)) %>%
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
    ops_col_check <- paste0(OPS_VARS[1], "_between")
    n_with_ops <- sum(!is.na(df[[ops_col_check]]))
    cat(sprintf("  Config %s: %d/%d events matched to operational data (%.1f%%)\n",
                config_id, n_with_ops, nrow(df), 100 * n_with_ops / nrow(df)))
  } else {
    # Create placeholder columns with NA if no operational data
    for (v in OPS_VARS) {
      df[[paste0(v, "_between")]] <- NA_real_
      df[[paste0(v, "_within")]] <- NA_real_
    }
  }
  
  return(df)
}


#' Get all climate variable names from a prepared data frame
#' @param df Prepared data frame (output of prepare_config_data)
#' @return Character vector of climate variable column names
get_climate_vars <- function(df) {
  c(
    names(df) %>% stringr::str_subset("overall_final_score_sma_"),
    names(df) %>% stringr::str_subset("overall_final_score_ewmaLAG_")
  )
}
