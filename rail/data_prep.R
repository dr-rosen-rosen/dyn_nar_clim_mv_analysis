# =============================================================================
# rail/data_prep.R — Rail-Specific Data Preparation
# =============================================================================
#
# Industry-specific data preparation: loading config parquets, joining to
# event data and operational features, outcome encoding.
#
# Windowing (create_all_windows, ewma_time_decay_irregular_lag) and climate
# variable detection (get_climate_vars) are in common/data_prep.R.
#
# Requires: rail/config.R and common/data_prep.R sourced first.
# Dependencies: dplyr, arrow, glue, slider
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(glue)
})


# =============================================================================
# OPERATIONAL DATA PROCESSING
# =============================================================================

#' Create operational features with rolling between/within decomposition
#'
#' Operates on MONTHLY operational data (one row per org-month).
#' For each operational variable, creates:
#'   {var}_between: Rolling mean of lagged values (stable org-level scale)
#'   {var}_within:  Deviation from rolling mean (temporal fluctuations)
#'
#' The between/within decomposition is a simplified version of the Mundlak
#' approach — it separates the stable facility-level effect from time-varying
#' deviations, allowing the model to estimate both.
#'
#' @param ops_df Data frame with monthly operational data
#' @param date_var Name of yearmonth variable (Date class, first of month)
#' @param org_var Name of organization ID variable
#' @param vars Vector of operational variable names (from OPS_VARS)
#' @param lag_k Lag in months (default: OPS_LAG_K)
#' @param roll_k Rolling window size in months (default: OPS_ROLL_K)
#' @param min_hist Minimum history required (default: OPS_MIN_HIST)
#' @param log1p_transform Apply log(1+x) before decomposition (default: TRUE)
#' @param standardize Standardize final features to mean=0, sd=1 (default: TRUE)
#' @return Data frame with org_id, yearmonth, and {var}_between/{var}_within columns
make_ops_features_rolling <- function(ops_df,
                                       date_var = "yearmonth",
                                       org_var  = "org_id",
                                       vars     = OPS_VARS,
                                       lag_k    = OPS_LAG_K,
                                       roll_k   = OPS_ROLL_K,
                                       min_hist = OPS_MIN_HIST,
                                       log1p_transform = TRUE,
                                       standardize = TRUE) {

  # Check required columns
  required_cols <- c(date_var, org_var, vars)
  missing_cols <- setdiff(required_cols, names(ops_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in operational data: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  # Ensure yearmonth is Date
  ops_df[[date_var]] <- as.Date(ops_df[[date_var]])

  df <- ops_df %>%
    arrange(.data[[org_var]], .data[[date_var]]) %>%
    group_by(.data[[org_var]])

  # Step 1: Log transform
  for (v in vars) {
    log_col <- paste0(v, "_log")
    if (log1p_transform) {
      df <- df %>% mutate(!!log_col := log1p(.data[[v]]))
    } else {
      df <- df %>% mutate(!!log_col := .data[[v]])
    }
  }

  # Step 2: Lag
  for (v in vars) {
    log_col <- paste0(v, "_log")
    lag_col <- paste0(v, "_lag", lag_k)
    df <- df %>% mutate(!!lag_col := dplyr::lag(.data[[log_col]], lag_k))
  }

  # Step 3: Rolling mean of lagged values (between component)
  for (v in vars) {
    lag_col <- paste0(v, "_lag", lag_k)
    between_raw <- paste0(v, "_between_raw")
    df <- df %>%
      mutate(
        !!between_raw := slider::slide_dbl(
          .data[[lag_col]], mean, na.rm = TRUE,
          .before = roll_k - 1L, .complete = FALSE
        )
      )
  }

  # Step 4: Within = lagged value - rolling mean
  for (v in vars) {
    lag_col <- paste0(v, "_lag", lag_k)
    between_raw <- paste0(v, "_between_raw")
    within_raw <- paste0(v, "_within_raw")
    df <- df %>%
      mutate(!!within_raw := .data[[lag_col]] - .data[[between_raw]])
  }

  # Step 5: Mark rows with insufficient history
  df <- df %>%
    mutate(ops_hist_n = row_number()) %>%
    ungroup()

  # Step 6: Standardize across all orgs
  for (v in vars) {
    between_raw <- paste0(v, "_between_raw")
    within_raw  <- paste0(v, "_within_raw")
    between_col <- paste0(v, "_between")
    within_col  <- paste0(v, "_within")

    if (standardize) {
      df <- df %>%
        mutate(
          !!between_col := as.vector(scale(.data[[between_raw]])),
          !!within_col  := as.vector(scale(.data[[within_raw]]))
        )
    } else {
      df[[between_col]] <- df[[between_raw]]
      df[[within_col]]  <- df[[within_raw]]
    }

    # NA out rows with insufficient history
    df <- df %>%
      mutate(
        !!between_col := ifelse(ops_hist_n <= min_hist, NA_real_, .data[[between_col]]),
        !!within_col  := ifelse(ops_hist_n <= min_hist, NA_real_, .data[[within_col]])
      )
  }

  # Clean intermediate columns
  intermediate <- c(
    paste0(vars, "_log"),
    paste0(vars, "_lag", lag_k),
    paste0(vars, "_between_raw"),
    paste0(vars, "_within_raw"),
    "ops_hist_n"
  )
  df <- df %>% select(-any_of(intermediate))

  return(df)
}


# =============================================================================
# UNIFIED DATA PREPARATION
# =============================================================================

#' Prepare data for a single rail configuration
#'
#' Loads the climate config parquet, joins to event data and operational
#' features, then creates windowed climate variables via common/data_prep.R.
#'
#' @param parquet_path Path to the config's climate scores parquet
#' @param config_id Configuration ID string
#' @param events_df Event-level data (rail_raw) with eid, org_id, event_date, outcomes
#' @param ops_features Pre-computed operational features from make_ops_features_rolling()
#'   (monthly data with org_id, yearmonth, {var}_between, {var}_within), or NULL
#' @param min_reports Minimum events per org to include
#' @param min_n Minimum events for window calculation
#' @return Data frame ready for model fitting
prepare_config_data <- function(parquet_path, config_id, events_df,
                                 ops_features = NULL,
                                 min_reports = 50, min_n = 1) {

  if (!file.exists(parquet_path)) {
    stop(sprintf("Parquet file not found: %s", parquet_path))
  }

  # Load climate scores
  df <- arrow::read_parquet(parquet_path) %>%
    select(
      ends_with('_id'),
      starts_with('overall_'),
      ends_with('_domain_score')
    ) %>%
    rename(eid = report_id) %>%
    left_join(events_df, by = 'eid') %>%
    filter(!is.na(event_date)) %>%
    group_by(org_id) %>%
    filter(n() > min_reports) %>%
    arrange(event_date) %>%
    mutate(t = row_number()) %>%
    ungroup()

  df$config_id <- config_id

  # Create windowed climate variables (from common/data_prep.R)
  df <- create_all_windows(df, climate_var_base = "overall_final_score",
                           org_var = "org_id", min_n = min_n)

  # Create yearmonth for ops join
  df <- df %>%
    mutate(yearmonth = as.Date(lubridate::floor_date(event_date, "month")))

  # Join operational features
  if (!is.null(ops_features)) {
    df <- df %>%
      left_join(ops_features, by = c("org_id", "yearmonth"))

    ops_col_check <- paste0(OPS_VARS[1], "_between")
    n_with_ops <- sum(!is.na(df[[ops_col_check]]))
    message(sprintf("  Config %s: %d/%d events matched to ops (%.1f%%)",
                    config_id, n_with_ops, nrow(df), 100 * n_with_ops / nrow(df)))
  } else {
    # Placeholder NAs
    for (v in OPS_VARS) {
      df[[paste0(v, "_between")]] <- NA_real_
      df[[paste0(v, "_within")]]  <- NA_real_
    }
  }

  return(df)
}
