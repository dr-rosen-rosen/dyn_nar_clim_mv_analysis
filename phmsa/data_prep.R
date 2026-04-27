# =============================================================================
# phmsa/data_prep.R — PHMSA Pipeline-Specific Data Preparation
# =============================================================================
#
# Provides:
#   encode_phmsa_outcomes()     : Compute binary injury/fatality flags
#   join_phmsa_ops()            : Join annual operator mileage by prior year
#   prepare_phmsa_config_data() : Unified prep for one config parquet
#
# All three pipeline types use the same functions; the outer loop in
# 0_main_phmsa.R filters to a single pipeline_type before calling prep.
#
# Requires: phmsa/config.R and common/data_prep.R sourced first.
# Dependencies: dplyr, arrow, lubridate, slider
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
  library(stringr)
})


# =============================================================================
# OUTCOME ENCODING
# =============================================================================

#' Encode PHMSA outcome variables from raw fields
#'
#' @param df Event-level data with total_injuries, total_fatalities, total_cost
#' @return df with injury_binary and fatality_binary columns added
encode_phmsa_outcomes <- function(df) {

  if ("total_injuries" %in% names(df) && !"injury_binary" %in% names(df)) {
    df <- df %>%
      mutate(injury_binary = as.integer(!is.na(total_injuries) & total_injuries > 0))
  }

  if ("total_fatalities" %in% names(df) && !"fatality_binary" %in% names(df)) {
    df <- df %>%
      mutate(fatality_binary = as.integer(!is.na(total_fatalities) & total_fatalities > 0))
  }

  if ("total_cost" %in% names(df)) {
    df <- df %>%
      mutate(total_cost = pmax(as.numeric(total_cost), 0, na.rm = FALSE))
  }

  df
}


# =============================================================================
# OPERATIONAL DATA JOIN
# =============================================================================

#' Join PHMSA annual operator mileage to event-level data
#'
#' Uses a 1-year lag: events in year Y are matched to the annual report
#' filed for year Y-1. Only the between/within features (already computed
#' in operator_annual_ops.parquet) are joined; total_miles is also included
#' for reference.
#'
#' @param df Event-level data with operator_id, event_date, pipeline_type
#' @param ops_features operator_annual_ops.parquet as a data frame
#' @return df with total_miles, total_miles_between, total_miles_within added
join_phmsa_ops <- function(df, ops_features) {

  if (is.null(ops_features) || nrow(ops_features) == 0) {
    message("  No operational data provided — creating NA placeholders")
    df$total_miles         <- NA_real_
    df$total_miles_between <- NA_real_
    df$total_miles_within  <- NA_real_
    return(df)
  }

  df <- df %>%
    mutate(
      .op_id    = as.integer(operator_id),
      .join_yr  = as.integer(lubridate::year(event_date)) - PHMSA_OPS_LAG_K
    )

  ops_join <- ops_features %>%
    select(OPERATOR_ID, REPORT_YEAR, pipeline_type,
           total_miles, total_miles_between, total_miles_within)

  df <- df %>%
    left_join(
      ops_join,
      by = c(".op_id" = "OPERATOR_ID",
             ".join_yr" = "REPORT_YEAR",
             "pipeline_type")
    ) %>%
    select(-.op_id, -.join_yr)

  n_matched <- sum(!is.na(df$total_miles))
  message(sprintf("  Ops join: %d/%d events matched (%.1f%%)",
                  n_matched, nrow(df), 100 * n_matched / nrow(df)))

  df
}


# =============================================================================
# UNIFIED DATA PREPARATION
# =============================================================================

#' Prepare data for a single PHMSA configuration
#'
#' Loads config parquet (climate scores), joins to event metadata filtered
#' to one pipeline type, creates windowed climate variables, joins annual
#' ops features, and encodes outcomes.
#'
#' @param parquet_path    Path to the config's climate scores parquet
#' @param config_id       Configuration ID string
#' @param events_df       PHMSA events filtered to one pipeline_type
#'                        (columns: eid, operator_id, event_date, pipeline_type,
#'                         total_injuries, total_fatalities, total_cost, ...)
#' @param ops_features    operator_annual_ops.parquet data frame (or NULL)
#' @param min_reports     Minimum events per operator to include
#' @param min_n           Minimum events for window calculation
#' @return Data frame ready for modeling, or NULL if insufficient data
prepare_phmsa_config_data <- function(parquet_path, config_id, events_df,
                                       ops_features = NULL,
                                       min_reports = MIN_REPORTS_DEFAULT,
                                       min_n = 1) {

  # Load climate scores
  df <- arrow::read_parquet(parquet_path) %>%
    select(
      ends_with("_id"),
      starts_with("overall_"),
      ends_with("_domain_score")
    )

  # Harmonize ID column
  if ("report_id" %in% names(df) && !"eid" %in% names(df)) {
    df <- df %>% rename(eid = report_id)
  }

  # Join to event metadata
  df <- df %>%
    left_join(events_df, by = "eid") %>%
    filter(!is.na(event_date), !is.na(operator_id))

  if (nrow(df) == 0) {
    warning(sprintf("Config %s: No rows after event join", config_id))
    return(NULL)
  }

  # Filter to operators with enough events
  df <- df %>%
    group_by(operator_id) %>%
    filter(n() >= min_reports) %>%
    ungroup()

  if (nrow(df) == 0) {
    warning(sprintf("Config %s: No data after filtering (min_reports=%d)",
                    config_id, min_reports))
    return(NULL)
  }

  df <- df %>%
    arrange(operator_id, event_date) %>%
    group_by(operator_id) %>%
    mutate(t = row_number()) %>%
    ungroup()

  df$config_id <- config_id

  # Add temporal controls
  df <- df %>%
    mutate(
      yearmonth_num   = as.integer(format(event_date, "%Y")) * 12L +
                        as.integer(format(event_date, "%m")),
      yearmonth_num_c = as.numeric(scale(yearmonth_num)),
      month_num       = as.integer(format(event_date, "%m")),
      sin_month       = sin(2 * pi * month_num / 12),
      cos_month       = cos(2 * pi * month_num / 12)
    )

  # Create windowed climate variables (from common/data_prep.R)
  df <- create_all_windows(df, climate_var_base = "overall_final_score",
                           org_var = "operator_id", min_n = min_n)

  # Join annual ops features
  df <- join_phmsa_ops(df, ops_features)

  # Encode outcomes
  df <- encode_phmsa_outcomes(df)

  df
}
