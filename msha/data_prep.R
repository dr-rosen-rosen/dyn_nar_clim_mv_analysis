# =============================================================================
# msha/data_prep.R — MSHA-Specific Data Preparation (incident-level track)
# =============================================================================
#
# Industry-specific: loading config parquets, joining to MSHA event data and
# quarterly operational features (employee-hours), outcome encoding (fatality,
# injury, lost workdays).
#
# Windowing and climate-var detection are in common/data_prep.R.
#
# Requires: msha/config.R and common/data_prep.R sourced first.
# Dependencies: dplyr, arrow, glue, slider, lubridate, stringr
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(glue)
  library(lubridate)
  library(stringr)
})


# =============================================================================
# OPERATIONAL DATA JOIN
# =============================================================================

#' Join MSHA operational covariates to event-level data.
#'
#' Matches each event to the PRIOR quarter's mine ops (employee-hours,
#' between/within decomposition) to avoid simultaneity with the outcome. The
#' ops pipeline (ops_mine_quarter.parquet) already supplies `hours_between` and
#' `hours_within`; if absent they are computed here via a rolling lag.
#'
#' @param df Event-level data with 'mine_id' and 'event_date' columns.
#' @param ops_features Mine-quarter ops (mine_id, cal_yr, cal_qtr, hours_worked,
#'   log_hours, hours_between, hours_within). NULL to skip (NA placeholders).
#' @return df with hours_between / hours_within (and log_hours) added.
join_msha_ops <- function(df, ops_features) {

  if (is.null(ops_features) || nrow(ops_features) == 0) {
    message("  No operational data provided — creating NA placeholders")
    df$hours_between <- NA_real_
    df$hours_within  <- NA_real_
    df$log_hours     <- NA_real_
    return(df)
  }

  ops <- ops_features %>%
    mutate(mine_id = as.character(mine_id))

  # Compute between/within on the fly only if the pipeline did not supply them.
  if (!all(c("hours_between", "hours_within") %in% names(ops))) {
    message("  Computing between/within decomposition for hours_worked...")
    ops <- ops %>%
      arrange(mine_id, cal_yr, cal_qtr) %>%
      group_by(mine_id) %>%
      mutate(
        .roll = slider::slide_dbl(hours_worked, mean, na.rm = TRUE,
                                  .before = MSHA_OPS_ROLL_K - 1L, .complete = TRUE),
        hours_between = dplyr::lag(.roll, MSHA_OPS_LAG_K),
        hours_within  = hours_worked - hours_between
      ) %>%
      select(-.roll) %>%
      ungroup()
  }

  if (!"log_hours" %in% names(ops)) {
    ops <- ops %>% mutate(log_hours = ifelse(hours_worked > 0, log(hours_worked), NA_real_))
  }

  # Prior-quarter key for each event.
  df <- df %>%
    mutate(
      .ev_q  = lubridate::quarter(event_date),
      .ev_y  = lubridate::year(event_date),
      .pri_q = ifelse(.ev_q == 1L, 4L, .ev_q - 1L),
      .pri_y = ifelse(.ev_q == 1L, .ev_y - 1L, .ev_y),
      mine_id = as.character(mine_id)
    )

  ops_join <- ops %>%
    select(mine_id, cal_yr, cal_qtr, log_hours, hours_between, hours_within) %>%
    rename(.pri_y = cal_yr, .pri_q = cal_qtr)

  df <- df %>% left_join(ops_join, by = c("mine_id", ".pri_y", ".pri_q"))

  n_matched <- sum(!is.na(df$hours_between))
  message(sprintf("  Ops join: %d/%d events matched (%.1f%%)",
                  n_matched, nrow(df), 100 * n_matched / nrow(df)))

  df %>% select(-starts_with("."))
}


# =============================================================================
# OUTCOME ENCODING
# =============================================================================

#' Encode MSHA incident-level outcome variables from raw event fields.
#'
#'   fatality_binary  : 1 if the event was fatal (fatality > 0)
#'   injury_binary    : 1 if the event involved an injury (accident_only == 0);
#'                      accident_only == 1 flags a property-damage / no-injury
#'                      accident report.
#'   days_lost        : lost workdays (>= 0; negatives floored to 0)
#'   days_lost_binary : hurdle gate, 1 if days_lost > 0
#'
#' @param df Event-level data with raw outcome columns.
#' @return df with encoded outcome columns added.
encode_msha_outcomes <- function(df) {

  if ("fatality" %in% names(df)) {
    df <- df %>% mutate(
      fatality_num    = suppressWarnings(as.numeric(fatality)),
      fatality_binary = as.integer(!is.na(fatality_num) & fatality_num > 0)
    )
  }

  if ("accident_only" %in% names(df)) {
    df <- df %>% mutate(
      injury_binary = as.integer(!is.na(accident_only) & accident_only == 0L)
    )
  }

  if ("days_lost" %in% names(df)) {
    df <- df %>% mutate(
      days_lost = suppressWarnings(as.numeric(days_lost)),
      days_lost = ifelse(!is.na(days_lost) & days_lost < 0, 0, days_lost),
      days_lost_binary = as.integer(!is.na(days_lost) & days_lost > 0)
    )
  }

  df
}


# =============================================================================
# UNIFIED DATA PREPARATION
# =============================================================================

#' Prepare data for a single MSHA configuration (one commodity).
#'
#' Loads config parquet (per-event climate scores), joins to event metadata and
#' prior-quarter operational features, creates windowed climate variables, and
#' encodes outcomes.
#'
#' @param parquet_path Path to the config's results.parquet (climate scores).
#' @param config_id Configuration ID string.
#' @param events_df Event-level data (already filtered to ONE commodity) with
#'   eid, mine_id, event_date, fatality, accident_only, days_lost, etc.
#' @param ops_features Mine-quarter ops (already filtered to the commodity, or
#'   superset keyed by mine_id) or NULL.
#' @param min_reports Minimum events per mine to include.
#' @param min_n Minimum events for window calculation.
#' @return Data frame ready for modeling, or NULL if empty after filtering.
prepare_msha_config_data <- function(parquet_path, config_id, events_df,
                                     ops_features = NULL,
                                     min_reports = MIN_REPORTS_DEFAULT,
                                     min_n = 1) {

  df <- arrow::read_parquet(parquet_path) %>%
    select(
      ends_with('_id'),
      starts_with('overall_'),
      ends_with('_domain_score')
    )

  # Harmonise ID column (config uses report_id; events use eid).
  if ("report_id" %in% names(df) && !"eid" %in% names(df)) {
    df <- df %>% rename(eid = report_id)
  }
  df <- df %>% mutate(eid = as.character(eid))

  df <- df %>%
    inner_join(events_df %>% mutate(eid = as.character(eid)), by = "eid") %>%
    filter(!is.na(event_date))

  # Keep mines with sufficient events.
  df <- df %>%
    group_by(mine_id) %>%
    filter(n() >= min_reports) %>%
    ungroup()

  if (nrow(df) == 0) {
    warning(sprintf("Config %s: No data after filtering (min_reports=%d)",
                    config_id, min_reports))
    return(NULL)
  }

  df <- df %>%
    arrange(mine_id, event_date) %>%
    group_by(mine_id) %>%
    mutate(t = row_number()) %>%
    ungroup()

  df$config_id <- config_id

  # Windowed climate variables + temporal controls (common/data_prep.R).
  df <- create_all_windows(df, climate_var_base = "overall_final_score",
                           org_var = "mine_id", min_n = min_n)

  df <- join_msha_ops(df, ops_features)
  df <- encode_msha_outcomes(df)

  df
}
