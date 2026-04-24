# =============================================================================
# common/data_prep.R — Shared Data Preparation Functions
# =============================================================================
#
# Functions that are IDENTICAL across industries. Industry-specific data prep
# (joins, outcome encoding, ops decomposition) lives in {industry}/data_prep.R.
#
# This file provides:
#   - ewma_time_decay_irregular_lag()  : EWMA with calendar-time decay
#   - create_all_windows()             : SMA + EWMA windowed climate variables
#   - get_climate_vars()               : Detect windowed climate variable names
#   - restandardize_in_fold()          : Re-standardize ops within a CV fold
#
# Requires: WINDOW_SPECS to be defined (from {industry}/config.R) before calling
#           create_all_windows(). All other functions are self-contained.
#
# Dependencies: dplyr, slider, lubridate, glue, stringr
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(slider)
  library(lubridate)
  library(glue)
  library(stringr)
})


# =============================================================================
# EWMA WITH TIME-DECAY FOR IRREGULAR EVENT SERIES
# =============================================================================

#' EWMA with time-decay for irregularly spaced events
#'
#' Computes a weighted moving average where weights decay exponentially
#' based on calendar time gaps between events. Only looks backward from
#' each event (strictly lagged — event i uses only events 1..i-1).
#'
#' This function is used by both rail and NRC pipelines identically.
#'
#' @param x Numeric vector of values (e.g., climate scores)
#' @param dates Date vector of event dates (same length as x, must be sorted
#'   within each org group — caller is responsible for sorting)
#' @param window_days Lookback window in calendar days
#' @param half_life_days Half-life for exponential decay in days
#' @param min_n Minimum number of prior events required for a valid estimate
#' @return Numeric vector of EWMA values (NA where insufficient history)
ewma_time_decay_irregular_lag <- function(x, dates, window_days, half_life_days, min_n = 1) {
  n <- length(x)
  result <- rep(NA_real_, n)
  lambda <- log(2) / half_life_days

  for (i in 2:n) {
    lookback_start <- dates[i] - window_days
    prior_idx <- which(dates[1:(i - 1)] >= lookback_start)

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


# =============================================================================
# WINDOWED CLIMATE VARIABLE CREATION
# =============================================================================

#' Create all windowed climate variables (SMA + EWMA)
#'
#' Adds one column per window specification to the data frame. Also creates
#' temporal control variables (year, yearmonth_num_c, sin/cos month).
#'
#' Both SMA and EWMA use strict lagging — each event's windowed score
#' is computed only from PRIOR events (no data leakage).
#'
#' SMA: count-based trailing average of the previous k events' scores.
#' EWMA: calendar-time exponential decay over a lookback window.
#'
#' @param df Data frame with org grouping var, event_date, and climate base var
#' @param climate_var_base Name of the base climate variable
#'   (default: "overall_final_score")
#' @param org_var Name of the organization grouping variable
#'   (e.g., "org_id" for rail, "facility" for NRC)
#' @param window_specs A list with $sma (tibble with window_size column) and
#'   $ewma (tibble with lag_days, halflife_days columns). If NULL, uses the
#'   global WINDOW_SPECS.
#' @param min_n Minimum prior observations for a valid window estimate
#' @return Data frame with windowed climate columns and temporal controls added
create_all_windows <- function(df,
                                climate_var_base = "overall_final_score",
                                org_var = "org_id",
                                window_specs = NULL,
                                min_n = 1) {

  # Use global WINDOW_SPECS if not provided
  if (is.null(window_specs)) {
    if (!exists("WINDOW_SPECS")) {
      stop("WINDOW_SPECS not found. Source your industry config.R first, ",
           "or pass window_specs explicitly.")
    }
    window_specs <- WINDOW_SPECS
  }

  # Create temporal control variables
  df <- df %>%
    mutate(
      year = lubridate::year(event_date),
      month = lubridate::month(event_date),
      yearmonth_num = (year - min(year)) * 12 + month,
      yearmonth_num_c = as.vector(scale(yearmonth_num, center = TRUE, scale = TRUE)),
      sin_month = sin(2 * pi * month / 12),
      cos_month = cos(2 * pi * month / 12)
    ) %>%
    arrange(.data[[org_var]], event_date) %>%
    group_by(.data[[org_var]])

  # Add SMA windows
  for (win in window_specs$sma$window_size) {
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
  for (i in 1:nrow(window_specs$ewma)) {
    lag_d <- window_specs$ewma$lag_days[i]
    hl_d  <- window_specs$ewma$halflife_days[i]
    var_name <- glue("{climate_var_base}_ewmaLAG_{lag_d}d_hl{hl_d}d")

    df <- df %>%
      arrange(.data[[org_var]], event_date) %>%
      group_by(.data[[org_var]]) %>%
      mutate(
        !!var_name := ewma_time_decay_irregular_lag(
          .data[[climate_var_base]], event_date, lag_d, hl_d, min_n
        )
      ) %>%
      ungroup()
  }

  return(df)
}


# =============================================================================
# CLIMATE VARIABLE DETECTION
# =============================================================================

#' Detect windowed climate variable names in a data frame
#'
#' Matches columns following the naming conventions produced by
#' create_all_windows(): {base}_sma_{k} and {base}_ewmaLAG_{d}d_hl{d}d.
#'
#' @param df Data frame with climate columns
#' @param base_var Base climate variable name prefix
#'   (default: "overall_final_score")
#' @return Character vector of windowed climate variable names
get_climate_vars <- function(df, base_var = "overall_final_score") {
  pattern <- paste0("^", base_var, "_(sma_|ewmaLAG_)")
  grep(pattern, names(df), value = TRUE)
}


# =============================================================================
# CV FOLD HELPERS
# =============================================================================

#' Re-standardize operational covariates within a CV fold
#'
#' When running cross-validation, operational covariates should be standardized
#' using only the training fold's statistics (not the full dataset). This
#' function re-standardizes specified columns in both train and test sets.
#'
#' @param train Training fold data frame
#' @param test Test fold data frame
#' @param vars Character vector of column names to re-standardize
#' @return List with $train and $test data frames
restandardize_in_fold <- function(train, test, vars) {
  for (v in vars) {
    if (!v %in% names(train)) next
    mu <- mean(train[[v]], na.rm = TRUE)
    s  <- sd(train[[v]], na.rm = TRUE)
    if (is.na(s) || s == 0) s <- 1  # avoid division by zero
    train[[v]] <- (train[[v]] - mu) / s
    test[[v]]  <- (test[[v]] - mu) / s
  }
  list(train = train, test = test)
}


# =============================================================================
# WINDOW SPEC PARSING (for postprocessing / reporting)
# =============================================================================

#' Parse window type and parameters from climate variable names
#'
#' Extracts structured metadata from the naming conventions used by
#' create_all_windows(). Useful for spec curve faceting and concordance analysis.
#'
#' @param climate_vars Character vector of climate variable names
#' @return Tibble with climate_var, window_type, window_size, lag_days, halflife_days
parse_window_spec <- function(climate_vars) {
  tibble(climate_var = unique(climate_vars)) %>%
    mutate(
      window_type = case_when(
        str_detect(climate_var, "_sma_")     ~ "sma",
        str_detect(climate_var, "_ewmaLAG_") ~ "ewma",
        TRUE                                 ~ "unknown"
      ),
      window_size = case_when(
        window_type == "sma" ~ str_extract(climate_var, "sma_(\\d+)", group = 1),
        TRUE                 ~ NA_character_
      ),
      lag_days = case_when(
        window_type == "ewma" ~ str_extract(climate_var, "ewmaLAG_(\\d+)d", group = 1),
        TRUE                  ~ NA_character_
      ),
      halflife_days = case_when(
        window_type == "ewma" ~ str_extract(climate_var, "hl(\\d+)d", group = 1),
        TRUE                  ~ NA_character_
      )
    )
}
