# =============================================================================
# rail/panel_data_prep.R — Rail Panel-Rate Data Preparation
# =============================================================================
#
# Builds an org × month panel for rate-based modeling, alongside (not
# replacing) the existing event-level pipeline.
#
# Outputs one row per (org_id, yearmonth) with:
#   - Outcome counts/sums (n_accidents, sum_injured, sum_killed, sum_damage)
#   - Operational exposure (train_miles, passenger_miles, staff_hours) — RAW
#     monthly values, intended for use as offsets.
#   - Operational covariates (between/within decomposition) for any ops vars
#     not used as the offset. Reused from the existing event-level pipeline.
#   - Climate score per window (overall_final_score_sma_*, *_ewma_*_*)
#     evaluated AT MONTH-START using strictly prior events with a 365-day
#     recency cap.
#   - Temporal controls (yearmonth_num_c, sin_month, cos_month).
#
# Requires:
#   rail/config.R           (OPS_VARS, WINDOW_SPECS)
#   rail/data_prep.R        (make_ops_features_rolling)
#   common/panel_data_prep.R (make_panel_grid, build_climate_panel)
#
# Dependencies: dplyr, arrow, lubridate
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
})


# =============================================================================
# OUTCOME AGGREGATION
# =============================================================================

#' Aggregate event-level outcomes to org × month
#'
#' @param events_df Data frame with org_id, event_date, total_persons_injured,
#'   total_persons_killed, total_damage_cost.
#' @return Tibble: org_id, yearmonth (Date, first of month),
#'   n_accidents, sum_injured, sum_killed, sum_damage.
aggregate_rail_outcomes <- function(events_df) {
  events_df %>%
    filter(!is.na(event_date)) %>%
    mutate(yearmonth = floor_date(as.Date(event_date), unit = "month")) %>%
    group_by(org_id, yearmonth) %>%
    summarise(
      n_accidents = n(),
      sum_injured = sum(coalesce(total_persons_injured, 0L), na.rm = TRUE),
      sum_killed  = sum(coalesce(total_persons_killed, 0L),  na.rm = TRUE),
      sum_damage  = sum(coalesce(total_damage_cost, 0),      na.rm = TRUE),
      .groups = "drop"
    )
}


# =============================================================================
# UNIFIED PANEL PREPARATION
# =============================================================================

#' Prepare a rail panel-rate dataset for one climate config
#'
#' Pipeline:
#'   1. Load config parquet (per-event climate scores) and join to events.
#'   2. Filter to orgs with >= min_reports events (matches event-level filter).
#'   3. Build a regular org × month grid bounded by ops-data coverage.
#'   4. Compute panel climate scores (one per window) at each month-start
#'      using strictly-lagged events + 365-day recency cap.
#'   5. Aggregate event outcomes to org × month (zero where no events).
#'   6. Join raw ops (for offsets) and ops between/within features.
#'   7. Add temporal controls.
#'   8. Filter rows where the chosen offset variable is missing/zero.
#'
#' @param parquet_path Path to a config's results.parquet (per-event climate).
#' @param config_id Configuration ID string.
#' @param events_df Event-level frame with org_id, eid, event_date,
#'   total_persons_killed, total_persons_injured, total_damage_cost.
#' @param ops_raw Raw monthly ops data: org_id, yearmonth, train_miles,
#'   passenger_miles, staff_hours (and any other columns).
#' @param ops_features Optional pre-computed between/within ops features
#'   (from make_ops_features_rolling). If NULL, computed here.
#' @param min_reports Minimum events per org to retain (default 50, matches
#'   event-level pipeline default).
#' @param max_holdover_days Climate-score recency cap (default 365).
#' @param climate_base Per-event climate score column (default
#'   "overall_final_score").
#' @param window_specs Optional override of WINDOW_SPECS.
#' @return Tibble ready for panel-rate modeling.
prepare_rail_panel_data <- function(parquet_path, config_id, events_df,
                                    ops_raw, ops_features = NULL,
                                    min_reports = 50L,
                                    max_holdover_days = 365L,
                                    climate_base = "overall_final_score",
                                    window_specs = NULL) {

  if (!file.exists(parquet_path)) {
    stop(sprintf("Parquet file not found: %s", parquet_path))
  }
  if (is.null(window_specs)) {
    if (!exists("WINDOW_SPECS")) stop("WINDOW_SPECS not found; source rail/config.R first.")
    window_specs <- WINDOW_SPECS
  }

  # --- 1. Per-event climate scores joined to event metadata ---
  climate_events <- arrow::read_parquet(parquet_path) %>%
    select(report_id, all_of(climate_base)) %>%
    rename(eid = report_id) %>%
    inner_join(events_df %>% select(eid, org_id, event_date,
                                    total_persons_killed,
                                    total_persons_injured,
                                    total_damage_cost),
               by = "eid") %>%
    filter(!is.na(event_date))

  # Filter to orgs with sufficient events (matches event-level convention).
  org_counts <- climate_events %>% count(org_id, name = "n_events")
  keep_orgs <- org_counts %>% filter(n_events >= min_reports) %>% pull(org_id)
  climate_events <- climate_events %>% filter(org_id %in% keep_orgs)

  if (nrow(climate_events) == 0) {
    warning(sprintf("Config %s: no events remain after min_reports=%d filter",
                    config_id, min_reports))
    return(NULL)
  }

  # --- 2. Restrict panel time bounds to ops-data coverage ---
  ops_raw <- ops_raw %>% mutate(yearmonth = as.Date(yearmonth))
  ops_min <- min(ops_raw$yearmonth, na.rm = TRUE)
  ops_max <- max(ops_raw$yearmonth, na.rm = TRUE)

  # --- 3. Build panel grid (one row per org × month) ---
  grid <- make_panel_grid(climate_events,
                          org_var = "org_id",
                          date_var = "event_date",
                          period = "month",
                          min_date = ops_min,
                          max_date = ops_max) %>%
    rename(yearmonth = period_start) %>%
    select(org_id, yearmonth)

  # --- 4. Climate scores at month-start with recency cap ---
  climate_panel <- build_climate_panel(
    events_df = climate_events,
    grid = grid %>% rename(period_start = yearmonth) %>% mutate(period_end = NA),
    org_var = "org_id",
    date_var = "event_date",
    climate_var = climate_base,
    window_specs = window_specs,
    max_holdover_days = max_holdover_days
  ) %>%
    rename(yearmonth = period_start) %>%
    select(-period_end)

  panel <- grid %>% left_join(climate_panel, by = c("org_id", "yearmonth"))

  # --- 5. Outcome aggregation (zero where no events) ---
  outcomes <- aggregate_rail_outcomes(climate_events)
  panel <- panel %>%
    left_join(outcomes, by = c("org_id", "yearmonth")) %>%
    mutate(
      n_accidents = coalesce(n_accidents, 0L),
      sum_injured = coalesce(sum_injured, 0),
      sum_killed  = coalesce(sum_killed,  0),
      sum_damage  = coalesce(sum_damage,  0)
    )

  # --- 6. Operational exposure (raw, for offsets) + between/within features ---
  ops_keep <- ops_raw %>%
    select(org_id, yearmonth, all_of(OPS_VARS)) %>%
    distinct(org_id, yearmonth, .keep_all = TRUE) %>%
    mutate(across(all_of(OPS_VARS), ~ ifelse(.x < 0, NA_real_, .x)))

  panel <- panel %>% left_join(ops_keep, by = c("org_id", "yearmonth"))

  if (is.null(ops_features)) {
    ops_features <- make_ops_features_rolling(ops_raw) %>%
      distinct(org_id, yearmonth, .keep_all = TRUE)
  }
  bw_cols <- c(paste0(OPS_VARS, "_between"), paste0(OPS_VARS, "_within"))
  ops_features <- ops_features %>%
    select(org_id, yearmonth, any_of(bw_cols))

  panel <- panel %>% left_join(ops_features, by = c("org_id", "yearmonth"))

  # --- 7. Temporal controls ---
  panel <- panel %>%
    arrange(org_id, yearmonth) %>%
    mutate(
      year = lubridate::year(yearmonth),
      month_num = lubridate::month(yearmonth),
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
    "  Panel %s: %d org-months across %d orgs (%s to %s); %d months with >=1 event (%.1f%%)",
    config_id, nrow(panel), n_distinct(panel$org_id),
    min(panel$yearmonth), max(panel$yearmonth),
    sum(panel$n_accidents > 0),
    100 * mean(panel$n_accidents > 0)
  ))

  panel
}
