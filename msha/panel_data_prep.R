# =============================================================================
# msha/panel_data_prep.R — MSHA Panel-Rate Data Preparation
# =============================================================================
#
# Builds a mine × quarter panel for rate-based modeling, alongside (not
# replacing) the event-level MSHA pipeline. Structural twin of
# nrc/panel_data_prep.R, adapted for a clean mine_id key (no name
# normalisation) and a log(employee-hours) exposure offset.
#
# Outputs one row per (mine_id, quarter_start) with:
#   - Self-reported outcome counts over events in the quarter:
#       n_accidents   = number of reported events
#       n_injuries    = number of injury events (accident_only == 0)
#       n_fatal       = number of fatal events  (fatality > 0)
#       sum_days_lost = total lost workdays
#   - Regulatory outcome counts from the violations / inspections covariates
#     pipeline (zero-filled against the grid):
#       violation_count, ss_count
#   - Operational exposure:  hours_worked (offset = log(hours_worked)),
#     log_hours, and hours_within (within-mine volatility covariate).
#     hours_between is intentionally NOT used as a panel covariate — it is the
#     slow-moving mine mean and collinear with the log(hours) offset level.
#   - Climate score per window (overall_final_score_sma_*, *_ewmaLAG_*) at
#     QUARTER-START using strictly prior events with a 365-day recency cap.
#   - Temporal controls (yearmonth_num_c, sin_month, cos_month).
#
# Requires:
#   msha/config.R            (WINDOW_SPECS, MIN_REPORTS_DEFAULT, MSHA_OPS_VARS)
#   common/panel_data_prep.R (make_panel_grid, build_climate_panel)
# Dependencies: dplyr, arrow, lubridate
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
})


# =============================================================================
# DETECTABILITY TIER MAP (self-sufficient fallback)
# =============================================================================
# The canonical definitions live in msha/config.R (single source of truth). But
# some callers (e.g. the manuscript script 5_manuscript_plots.R) source this
# file WITHOUT msha/config.R to avoid clobbering shared globals. Define the tier
# map here too, guarded, so aggregate_msha_outcomes() always has it.
if (!exists("msha_assign_tier")) {
  DETECTABILITY_TIER_MAP <- c(
    "00" = "T0",
    "01" = "T1", "02" = "T1",
    "03" = "T2", "04" = "T2",
    "05" = "T3", "06" = "T3", "10" = "T3"
  )
  msha_assign_tier <- function(degree_cd, scheme = "three_tier") {
    code <- trimws(as.character(degree_cd))
    unname(DETECTABILITY_TIER_MAP[code])
  }
}


# =============================================================================
# HELPERS
# =============================================================================

#' Derive a quarter-start Date from cal_yr / cal_qtr integers.
.msha_quarter_start <- function(cal_yr, cal_qtr) {
  lubridate::make_date(cal_yr, (cal_qtr - 1L) * 3L + 1L, 1L)
}


# =============================================================================
# OUTCOME AGGREGATION (events -> mine, quarter)
# =============================================================================

#' Aggregate event-level MSHA outcomes to mine × quarter.
#'
#' @param events_df Frame with mine_id, event_date, fatality, accident_only,
#'   days_lost (and eid). Should already be filtered to one commodity.
#' @return Tibble: mine_id, quarter_start, n_accidents, n_injuries, n_fatal,
#'   sum_days_lost.
aggregate_msha_outcomes <- function(events_df) {
  # Defensive: tier counts collapse to 0 if the severity code is absent.
  if (!"degree_injury_cd" %in% names(events_df)) {
    events_df$degree_injury_cd <- NA_character_
  }
  events_df %>%
    filter(!is.na(event_date)) %>%
    mutate(
      mine_id        = as.character(mine_id),
      quarter_start  = lubridate::floor_date(as.Date(event_date), unit = "quarter"),
      .fatal         = as.integer(suppressWarnings(as.numeric(fatality)) > 0),
      .injury        = as.integer(!is.na(accident_only) & accident_only == 0L),
      .days_lost     = pmax(0, suppressWarnings(as.numeric(days_lost)), na.rm = TRUE),
      .tier          = msha_assign_tier(degree_injury_cd)
    ) %>%
    group_by(mine_id, quarter_start) %>%
    summarise(
      n_accidents   = n(),
      n_injuries    = sum(.injury, na.rm = TRUE),
      n_fatal       = sum(.fatal,  na.rm = TRUE),
      sum_days_lost = sum(.days_lost, na.rm = TRUE),
      # Detectability tiers (acute-severity ladder; 07/08/09/unknown -> NA, so
      # n_t0+n_t1+n_t2+n_t3 <= n_accidents). See config.R DETECTABILITY_TIER_MAP.
      n_t0          = sum(.tier == "T0", na.rm = TRUE),
      n_t1          = sum(.tier == "T1", na.rm = TRUE),
      n_t2          = sum(.tier == "T2", na.rm = TRUE),
      n_t3          = sum(.tier == "T3", na.rm = TRUE),
      .groups = "drop"
    )
}


# =============================================================================
# OPS AGGREGATION (mine, quarter exposure)
# =============================================================================

#' Prepare mine-quarter operational exposure for the panel.
#'
#' The MSHA ops pipeline is already at mine × quarter cadence and supplies
#' hours_worked, log_hours, hours_between, hours_within. We only attach a
#' quarter_start Date and (if absent) compute log_hours / within.
#'
#' @param ops_raw ops_mine_quarter frame (filtered to the commodity, or a
#'   superset keyed by mine_id).
#' @return Tibble: mine_id, quarter_start, hours_worked, log_hours, hours_within.
prepare_msha_ops <- function(ops_raw) {
  out <- ops_raw %>%
    mutate(
      mine_id       = as.character(mine_id),
      quarter_start = .msha_quarter_start(cal_yr, cal_qtr)
    ) %>%
    filter(!is.na(mine_id), !is.na(quarter_start))

  if (!"log_hours" %in% names(out)) {
    out <- out %>% mutate(log_hours = ifelse(hours_worked > 0, log(hours_worked), NA_real_))
  }
  if (!"hours_within" %in% names(out)) {
    out <- out %>%
      arrange(mine_id, quarter_start) %>%
      group_by(mine_id) %>%
      mutate(
        .roll = slider::slide_dbl(hours_worked, mean, na.rm = TRUE,
                                  .before = MSHA_OPS_ROLL_K - 1L, .complete = TRUE),
        .between = dplyr::lag(.roll, MSHA_OPS_LAG_K),
        hours_within = hours_worked - .between
      ) %>%
      select(-.roll, -.between) %>%
      ungroup()
  }

  out %>%
    group_by(mine_id, quarter_start) %>%
    summarise(
      hours_worked = sum(hours_worked, na.rm = TRUE),
      log_hours    = ifelse(sum(hours_worked, na.rm = TRUE) > 0,
                            log(sum(hours_worked, na.rm = TRUE)), NA_real_),
      hours_within = mean(hours_within, na.rm = TRUE),
      .groups = "drop"
    )
}


# =============================================================================
# UNIFIED PANEL PREPARATION
# =============================================================================

#' Prepare an MSHA mine-quarter panel for one climate config (one commodity).
#'
#' Pipeline:
#'   1. Load config parquet (per-event climate) and inner-join to events.
#'   2. Filter to mines with >= min_reports events.
#'   3. Prepare mine-quarter ops exposure (hours).
#'   4. Build a regular mine × quarter grid bounded by ops coverage.
#'   5. Compute panel climate scores at quarter-start (strict lag + recency cap).
#'   6. Aggregate self-reported event outcomes (zero where no events).
#'   7. Join regulatory outcomes (violations / SS) zero-filled.
#'   8. Join ops exposure.
#'   9. Add temporal controls.
#'
#' @param parquet_path Path to a config's results.parquet (per-event climate).
#' @param config_id Configuration ID string.
#' @param events_df Event-level frame (one commodity) with eid, mine_id,
#'   event_date, fatality, accident_only, days_lost.
#' @param ops_raw Mine-quarter ops frame (mine_id, cal_yr, cal_qtr,
#'   hours_worked, log_hours, hours_within).
#' @param covariates_df Optional. Mine-quarter regulatory covariates
#'   (mine_id, cal_yr, cal_qtr, violation_count, ss_count). Zero-filled.
#' @param min_reports Minimum events per mine to retain.
#' @param max_holdover_days Climate-score recency cap (default 365).
#' @param climate_base Per-event climate score column.
#' @param window_specs Optional override of WINDOW_SPECS.
#' @return Tibble ready for panel-rate modeling, or NULL if empty.
prepare_msha_panel_data <- function(parquet_path, config_id, events_df,
                                    ops_raw,
                                    covariates_df = NULL,
                                    min_reports = MIN_REPORTS_DEFAULT,
                                    max_holdover_days = 365L,
                                    climate_base = "overall_final_score",
                                    window_specs = NULL) {

  if (!file.exists(parquet_path)) stop(sprintf("Parquet file not found: %s", parquet_path))
  if (is.null(window_specs)) {
    if (!exists("WINDOW_SPECS")) stop("WINDOW_SPECS not found; source msha/config.R first.")
    window_specs <- WINDOW_SPECS
  }

  # --- 1. Per-event climate scores joined to event metadata ---
  climate_events <- arrow::read_parquet(parquet_path) %>%
    select(report_id, all_of(climate_base)) %>%
    rename(eid = report_id) %>%
    mutate(eid = as.character(eid)) %>%
    inner_join(
      events_df %>%
        mutate(eid = as.character(eid), mine_id = as.character(mine_id)) %>%
        select(eid, mine_id, event_date, fatality, accident_only, days_lost,
               any_of("degree_injury_cd")),
      by = "eid"
    ) %>%
    filter(!is.na(event_date))

  # --- 2. Filter to mines with sufficient events ---
  mine_counts <- climate_events %>% count(mine_id, name = "n_events")
  keep_mines <- mine_counts %>% filter(n_events >= min_reports) %>% pull(mine_id)
  climate_events <- climate_events %>% filter(mine_id %in% keep_mines)

  if (nrow(climate_events) == 0) {
    warning(sprintf("Config %s: no events remain after min_reports=%d filter",
                    config_id, min_reports))
    return(NULL)
  }

  # --- 3. Ops exposure (mine-quarter) ---
  ops_mine <- prepare_msha_ops(ops_raw) %>% filter(mine_id %in% keep_mines)
  if (nrow(ops_mine) == 0) {
    stop(sprintf("Config %s: no ops rows survived mine filtering.", config_id))
  }
  ops_min <- min(ops_mine$quarter_start, na.rm = TRUE)
  ops_max <- max(ops_mine$quarter_start, na.rm = TRUE)

  # --- 4. Panel grid (one row per mine × quarter) ---
  grid <- make_panel_grid(climate_events,
                          org_var = "mine_id",
                          date_var = "event_date",
                          period = "quarter",
                          min_date = ops_min,
                          max_date = ops_max) %>%
    rename(quarter_start = period_start) %>%
    select(mine_id, quarter_start)

  # --- 5. Climate scores at quarter-start with recency cap ---
  climate_panel <- build_climate_panel(
    events_df = climate_events,
    grid = grid %>% rename(period_start = quarter_start) %>% mutate(period_end = NA),
    org_var = "mine_id",
    date_var = "event_date",
    climate_var = climate_base,
    window_specs = window_specs,
    max_holdover_days = max_holdover_days
  ) %>%
    rename(quarter_start = period_start) %>%
    select(-period_end)

  panel <- grid %>% left_join(climate_panel, by = c("mine_id", "quarter_start"))

  # --- 6. Self-reported outcomes (zero where no events) ---
  outcomes <- aggregate_msha_outcomes(climate_events)
  panel <- panel %>%
    left_join(outcomes, by = c("mine_id", "quarter_start")) %>%
    mutate(
      n_accidents   = coalesce(n_accidents, 0L),
      n_injuries    = coalesce(n_injuries, 0L),
      n_fatal       = coalesce(n_fatal, 0L),
      sum_days_lost = coalesce(sum_days_lost, 0),
      n_t0          = coalesce(n_t0, 0L),
      n_t1          = coalesce(n_t1, 0L),
      n_t2          = coalesce(n_t2, 0L),
      n_t3          = coalesce(n_t3, 0L)
    )

  # --- 7. Regulatory outcomes (violations / SS), zero-filled ---
  if (!is.null(covariates_df)) {
    cov_keep <- covariates_df %>%
      mutate(
        mine_id       = as.character(mine_id),
        quarter_start = .msha_quarter_start(cal_yr, cal_qtr)
      ) %>%
      filter(mine_id %in% keep_mines) %>%
      group_by(mine_id, quarter_start) %>%
      summarise(
        violation_count = sum(violation_count, na.rm = TRUE),
        ss_count        = sum(ss_count,        na.rm = TRUE),
        .groups = "drop"
      )

    panel <- panel %>%
      left_join(cov_keep, by = c("mine_id", "quarter_start")) %>%
      mutate(
        violation_count = coalesce(violation_count, 0),
        ss_count        = coalesce(ss_count, 0)
      )
  }

  # --- 8. Operational exposure ---
  panel <- panel %>% left_join(ops_mine, by = c("mine_id", "quarter_start"))

  # --- 9. Temporal controls ---
  panel <- panel %>%
    arrange(mine_id, quarter_start) %>%
    mutate(
      year      = lubridate::year(quarter_start),
      month_num = lubridate::month(quarter_start),
      yearmonth_num = year + (month_num - 1) / 12
    ) %>%
    mutate(
      yearmonth_num_c = yearmonth_num - mean(yearmonth_num, na.rm = TRUE),
      sin_month = sin(2 * pi * month_num / 12),
      cos_month = cos(2 * pi * month_num / 12)
    ) %>%
    select(-yearmonth_num)

  panel$config_id <- config_id

  message(sprintf(
    "  Panel %s: %d mine-quarters across %d mines (%s to %s); %d quarters with >=1 accident (%.1f%%)",
    config_id, nrow(panel), n_distinct(panel$mine_id),
    min(panel$quarter_start), max(panel$quarter_start),
    sum(panel$n_accidents > 0), 100 * mean(panel$n_accidents > 0)
  ))
  if ("violation_count" %in% names(panel)) {
    message(sprintf(
      "    violations: %d non-zero (%.1f%%); ss: %d non-zero (%.1f%%)",
      sum(panel$violation_count > 0), 100 * mean(panel$violation_count > 0),
      sum(panel$ss_count > 0), 100 * mean(panel$ss_count > 0)
    ))
  }
  # QC: detectability-tier event totals + excluded (07/08/09/unknown) volume.
  if ("degree_injury_cd" %in% names(climate_events)) {
    .n_excl <- sum(is.na(msha_assign_tier(climate_events$degree_injury_cd)))
    message(sprintf(
      "    tiers (mine-quarter sums): T1=%d T2=%d T3=%d T0=%d; excluded events (07/08/09/unknown)=%d (%.1f%%)",
      sum(panel$n_t1), sum(panel$n_t2), sum(panel$n_t3), sum(panel$n_t0),
      .n_excl, 100 * .n_excl / nrow(climate_events)
    ))
  }

  panel
}
