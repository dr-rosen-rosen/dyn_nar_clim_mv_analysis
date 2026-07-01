# =============================================================================
# nrc/panel_data_prep.R — NRC Panel-Rate Data Preparation
# =============================================================================
#
# Builds a facility × quarter panel for rate-based modeling, alongside (not
# replacing) the existing event-level NRC pipeline.
#
# Outputs one row per (facility, quarter_start) with:
#   - Outcome counts (n_lers, n_emerg, sum_pct_power_loss) over events that
#     occurred AT POWER (initial_pwr > 0) within the quarter.
#   - Operational exposure aggregated across all units at the facility:
#       days_at_power_total  = sum of days_at_power across units
#       days_at_full_total   = sum of days_at_full across units
#       n_units              = number of units reporting that quarter
#     These are intended as offsets. days_at_power_total scales naturally with
#     reactor count, capturing "how many reactor-days of operation that
#     facility had this quarter".
#   - Operational covariates (between/within) for power_std (operational
#     volatility, NOT collinear with the offset). capacity_factor is dropped
#     because it is mathematically redundant with days_at_power / total_days.
#   - Climate score per window (overall_final_score_sma_*, *_ewmaLAG_*_*)
#     evaluated AT QUARTER-START using strictly prior events with a 365-day
#     recency cap.
#   - Temporal controls (yearmonth_num_c, sin_month, cos_month) where
#     yearmonth = quarter_start.
#
# Requires:
#   nrc/config.R            (NRC_OPS_VARS, WINDOW_SPECS, MIN_REPORTS_DEFAULT)
#   common/panel_data_prep.R (make_panel_grid, build_climate_panel)
#
# Dependencies: dplyr, arrow, lubridate
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
  library(stringr)
})


# =============================================================================
# FACILITY NAME NORMALIZATION
# =============================================================================

#' Normalize a facility-or-facility_unit string to a comparable site key.
#'
#' Lowercases, trims, and strips trailing unit indicators in any of the forms
#' commonly seen in the NRC data:
#'   "Beaver Valley"        -> "beaver valley"
#'   "Beaver Valley 1"      -> "beaver valley"
#'   "beaver valley-1"      -> "beaver valley"
#'   "Beaver Valley Unit 2" -> "beaver valley"
#'
#' @param x Character vector.
#' @return Lowercase site key.
normalize_nrc_facility <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("[[:space:]\\-]*(unit[[:space:]]*)?\\d+[[:space:]]*$", "", x)
  trimws(x)
}


# =============================================================================
# OUTCOME AGGREGATION
# =============================================================================

#' Aggregate event-level NRC outcomes to facility × quarter
#'
#' Filters to AT-POWER events (initial_pwr > 0) before aggregating, matching
#' the days_at_power offset.
#'
#' @param events_df Frame with: facility (raw), event_date, initial_pwr,
#'   current_pwr, emerg_class. Should already have eid.
#' @return Tibble: facility_site, quarter_start, n_lers, n_emerg, n_scrams,
#'   sum_pct_power_loss, mean_pct_power_loss.
aggregate_nrc_outcomes <- function(events_df) {

  ev <- events_df %>%
    filter(!is.na(event_date)) %>%
    mutate(
      initial_pwr_num = suppressWarnings(as.numeric(initial_pwr)),
      current_pwr_num = suppressWarnings(as.numeric(current_pwr)),
      .ec_raw = tolower(trimws(as.character(emerg_class))),
      emerg_binary = as.integer(.ec_raw %in% c(
        "ale", "alert",
        "unu", "unusual event", "ue",
        "sae", "site area emergency",
        "ge", "general emergency"
      )),
      # Scram binary (any scram: A, A/R, M, M/R). Mirrors the corrected
      # parser in nrc/data_prep.R::encode_nrc_outcomes() — /R variants are
      # scrams with reactor trip, not "no scram."
      .scram_raw = tolower(trimws(as.character(scram_code))),
      scram_binary = as.integer(.scram_raw %in% c(
        "a", "a/r", "auto", "automatic", "a/m",
        "m", "m/r", "manual"
      )),
      pct_power_loss = pmax(0, initial_pwr_num - current_pwr_num),
      facility_site = normalize_nrc_facility(facility),
      quarter_start = lubridate::floor_date(as.Date(event_date), unit = "quarter")
    ) %>%
    filter(!is.na(initial_pwr_num), initial_pwr_num > 0)  # at-power only

  ev %>%
    group_by(facility_site, quarter_start) %>%
    summarise(
      n_lers              = n(),
      n_emerg             = sum(emerg_binary, na.rm = TRUE),
      n_scrams            = sum(scram_binary, na.rm = TRUE),
      sum_pct_power_loss  = sum(pct_power_loss, na.rm = TRUE),
      mean_pct_power_loss = mean(pct_power_loss, na.rm = TRUE),
      .groups = "drop"
    )
}


# =============================================================================
# OPS AGGREGATION (unit -> facility, quarter)
# =============================================================================

#' Aggregate unit-level quarterly ops to facility × quarter.
#'
#' @param ops_raw Frame with facility_unit, quarter_start, days_at_power,
#'   days_at_full, capacity_factor, power_std (and possibly more).
#' @return Tibble: facility_site, quarter_start, days_at_power_total,
#'   days_at_full_total, n_units, capacity_factor_mean, power_std_mean.
aggregate_nrc_ops <- function(ops_raw) {
  ops_raw %>%
    mutate(
      facility_site = normalize_nrc_facility(facility_unit),
      quarter_start = as.Date(quarter_start)
    ) %>%
    filter(!is.na(facility_site), !is.na(quarter_start)) %>%
    group_by(facility_site, quarter_start) %>%
    summarise(
      days_at_power_total  = sum(days_at_power, na.rm = TRUE),
      days_at_full_total   = sum(days_at_full,  na.rm = TRUE),
      capacity_factor_mean = mean(capacity_factor, na.rm = TRUE),
      power_std_mean       = mean(power_std,       na.rm = TRUE),
      n_units              = n(),
      .groups = "drop"
    )
}


#' Compute between/within decomposition for facility-level ops covariates.
#'
#' Uses a rolling mean (NRC_OPS_ROLL_K quarters) lagged by NRC_OPS_LAG_K to
#' build the "between" component (slow-moving facility average), with
#' "within" = current quarter minus that.
#'
#' @param ops_facility Output of aggregate_nrc_ops().
#' @param ops_vars Character vector of base var names to decompose. Default:
#'   "power_std_mean". (capacity_factor_mean is intentionally excluded — it
#'   is collinear with the days_at_power offset.)
#' @return ops_facility with `<v>_between` and `<v>_within` columns added.
add_facility_ops_features <- function(ops_facility,
                                       ops_vars = "power_std_mean") {
  if (!requireNamespace("slider", quietly = TRUE)) {
    stop("slider package required for rolling between/within decomposition")
  }

  out <- ops_facility %>% arrange(facility_site, quarter_start)

  for (v in ops_vars) {
    if (!v %in% names(out)) next
    roll_col    <- paste0(v, "_roll_mean")
    between_col <- paste0(v, "_between")
    within_col  <- paste0(v, "_within")
    out <- out %>%
      group_by(facility_site) %>%
      mutate(
        !!roll_col := slider::slide_dbl(
          .data[[v]], mean, na.rm = TRUE,
          .before = NRC_OPS_ROLL_K - 1L, .complete = TRUE
        ),
        !!between_col := dplyr::lag(.data[[roll_col]], NRC_OPS_LAG_K),
        !!within_col  := .data[[v]] - .data[[between_col]]
      ) %>%
      select(-!!roll_col) %>%
      ungroup()
  }
  out
}


# =============================================================================
# UNIFIED PANEL PREPARATION
# =============================================================================

#' Prepare an NRC facility-quarter panel for one climate config
#'
#' Pipeline:
#'   1. Load config parquet (per-event climate scores) and join to events.
#'   2. Normalize facility name to a site key (strips unit suffixes).
#'   3. Filter to facilities with >= min_reports at-power events.
#'   4. Aggregate ops (unit -> facility-quarter) and decompose power_std.
#'   5. Build a regular facility × quarter grid bounded by ops coverage.
#'   6. Compute panel climate scores (one per window) at each quarter-start
#'      using strictly-lagged events + 365-day recency cap.
#'   7. Aggregate at-power event outcomes (zero where no events).
#'   8. Join ops exposure + between/within covariates.
#'   9. Add temporal controls.
#'
#' @param parquet_path Path to a config's results.parquet (per-event climate).
#' @param config_id Configuration ID string.
#' @param events_df Event-level frame with eid, facility, event_date,
#'   initial_pwr, current_pwr, emerg_class.
#' @param ops_raw Quarterly unit-level ops frame: facility_unit, quarter_start,
#'   days_at_power, days_at_full, capacity_factor, power_std.
#' @param findings_df Optional. Quarterly NRC inspection findings:
#'   facility_unit, quarter_start, findings_count, findings_green,
#'   findings_white, findings_yellow, findings_red, findings_nongreen_count.
#'   Loaded from processed/nrc/findings_quarterly.parquet. When provided, the
#'   panel gains findings_count, findings_nongreen, and per-color count
#'   columns (zero-filled against the panel grid).
#' @param action_matrix_df Optional. Quarterly NRC Reactor Oversight Process
#'   action matrix column assignments: facility_unit, quarter_start,
#'   action_matrix_col (1..5; higher = worse). Loaded from
#'   processed/nrc/action_matrix_long.parquet. When provided, the panel gains
#'   action_matrix_col_raw and above_col1 (binary: 1 if column >= 2).
#'   NA where no assessment was recorded that quarter.
#' @param min_reports Minimum at-power events per facility to retain.
#' @param max_holdover_days Climate-score recency cap (default 365).
#' @param climate_base Per-event climate score column.
#' @param window_specs Optional override of WINDOW_SPECS.
#' @return Tibble ready for panel-rate modeling.
prepare_nrc_panel_data <- function(parquet_path, config_id, events_df,
                                    ops_raw,
                                    findings_df      = NULL,
                                    action_matrix_df = NULL,
                                    min_reports = MIN_REPORTS_DEFAULT,
                                    max_holdover_days = 365L,
                                    climate_base = "overall_final_score",
                                    window_specs = NULL) {

  if (!file.exists(parquet_path)) {
    stop(sprintf("Parquet file not found: %s", parquet_path))
  }
  if (is.null(window_specs)) {
    if (!exists("WINDOW_SPECS")) stop("WINDOW_SPECS not found; source nrc/config.R first.")
    window_specs <- WINDOW_SPECS
  }

  # --- 1. Per-event climate scores joined to event metadata ---
  climate_events <- arrow::read_parquet(parquet_path) %>%
    select(report_id, all_of(climate_base)) %>%
    rename(eid = report_id) %>%
    inner_join(events_df %>% select(eid, facility, event_date,
                                    initial_pwr, current_pwr, emerg_class,
                                    scram_code),
               by = "eid") %>%
    filter(!is.na(event_date)) %>%
    mutate(
      facility_site = normalize_nrc_facility(facility),
      initial_pwr_num = suppressWarnings(as.numeric(initial_pwr))
    )

  # At-power filter for the events that drive the panel rate.
  at_power_events <- climate_events %>%
    filter(!is.na(initial_pwr_num), initial_pwr_num > 0)

  # Filter to facilities with sufficient at-power events.
  fac_counts <- at_power_events %>% count(facility_site, name = "n_events")
  keep_facs <- fac_counts %>% filter(n_events >= min_reports) %>% pull(facility_site)
  at_power_events <- at_power_events %>% filter(facility_site %in% keep_facs)

  if (nrow(at_power_events) == 0) {
    warning(sprintf("Config %s: no events remain after min_reports=%d filter",
                    config_id, min_reports))
    return(NULL)
  }

  # --- 2. Aggregate ops to facility-quarter and decompose ---
  ops_facility <- aggregate_nrc_ops(ops_raw) %>%
    filter(facility_site %in% keep_facs) %>%
    add_facility_ops_features(ops_vars = "power_std_mean")

  if (nrow(ops_facility) == 0) {
    stop(sprintf("Config %s: no ops rows survived facility filtering. Check name normalization.",
                 config_id))
  }

  ops_min <- min(ops_facility$quarter_start, na.rm = TRUE)
  ops_max <- max(ops_facility$quarter_start, na.rm = TRUE)

  # --- 3. Build panel grid (one row per facility × quarter) ---
  # Pass at_power_events here so make_panel_grid can per-facility bound by
  # event activity; the global ops min/max keeps us inside ops coverage.
  grid <- make_panel_grid(at_power_events,
                          org_var = "facility_site",
                          date_var = "event_date",
                          period = "quarter",
                          min_date = ops_min,
                          max_date = ops_max) %>%
    rename(quarter_start = period_start) %>%
    select(facility_site, quarter_start)

  # --- 4. Climate scores at quarter-start with recency cap ---
  # Climate windowing uses ALL events from kept facilities (not just at-power),
  # because narrative climate is about discourse, not operational state. The
  # at-power filter only governs which events count toward outcomes/exposure.
  climate_events_kept <- climate_events %>% filter(facility_site %in% keep_facs)

  climate_panel <- build_climate_panel(
    events_df = climate_events_kept,
    grid = grid %>% rename(period_start = quarter_start) %>% mutate(period_end = NA),
    org_var = "facility_site",
    date_var = "event_date",
    climate_var = climate_base,
    window_specs = window_specs,
    max_holdover_days = max_holdover_days
  ) %>%
    rename(quarter_start = period_start) %>%
    select(-period_end)

  panel <- grid %>% left_join(climate_panel, by = c("facility_site", "quarter_start"))

  # --- 5. Outcome aggregation (zero where no events) ---
  outcomes <- aggregate_nrc_outcomes(climate_events_kept)
  panel <- panel %>%
    left_join(outcomes, by = c("facility_site", "quarter_start")) %>%
    mutate(
      n_lers              = coalesce(n_lers, 0L),
      n_emerg             = coalesce(n_emerg, 0L),
      n_scrams            = coalesce(n_scrams, 0L),
      sum_pct_power_loss  = coalesce(sum_pct_power_loss, 0),
      mean_pct_power_loss = coalesce(mean_pct_power_loss, 0)
    )

  # --- 5b. External NRC assessment outcomes (findings + action matrix) ---
  # These come from separate quarterly files (not events.parquet) and serve as
  # externally-judged "performance" outcomes complementary to the self-reported
  # LER-derived measures. Joined on facility_site × quarter_start and
  # zero-filled (findings) / dropped-where-missing (action matrix) against the
  # panel grid.
  #
  # Derived columns:
  #   findings_count        — count of NRC inspection findings, all colors
  #   findings_nongreen     — count of safety-significant (white+yellow+red)
  #                            findings; green findings are predominantly
  #                            procedural and dominate raw counts ~95%
  #   above_col1            — 1 if action_matrix_col >= 2 in the quarter (i.e.
  #                            facility is above NRC baseline regulatory
  #                            response), else 0; NA if no assessment that
  #                            quarter
  #   action_matrix_col_raw — original 1..5 column (NA elsewhere) for sensitivity
  if (!is.null(findings_df)) {
    f_keep <- findings_df %>%
      mutate(
        facility_site = normalize_nrc_facility(facility_unit),
        quarter_start = as.Date(quarter_start)
      ) %>%
      filter(facility_site %in% keep_facs) %>%
      group_by(facility_site, quarter_start) %>%
      summarise(
        findings_count    = sum(findings_count,    na.rm = TRUE),
        findings_green    = sum(findings_green,    na.rm = TRUE),
        findings_white    = sum(findings_white,    na.rm = TRUE),
        findings_yellow   = sum(findings_yellow,   na.rm = TRUE),
        findings_red      = sum(findings_red,      na.rm = TRUE),
        findings_nongreen = sum(findings_nongreen_count, na.rm = TRUE),
        .groups = "drop"
      )

    panel <- panel %>%
      left_join(f_keep, by = c("facility_site", "quarter_start")) %>%
      mutate(
        findings_count    = coalesce(findings_count,    0L),
        findings_green    = coalesce(findings_green,    0L),
        findings_white    = coalesce(findings_white,    0L),
        findings_yellow   = coalesce(findings_yellow,   0L),
        findings_red      = coalesce(findings_red,      0L),
        findings_nongreen = coalesce(findings_nongreen, 0L)
      )
  }

  if (!is.null(action_matrix_df)) {
    am_keep <- action_matrix_df %>%
      mutate(
        facility_site = normalize_nrc_facility(facility_unit),
        quarter_start = as.Date(quarter_start)
      ) %>%
      filter(facility_site %in% keep_facs,
              action_matrix_col %in% 1:5) %>%
      # Multiple units at a facility can have different column assignments —
      # take the WORST (highest column number) to reflect the site-level
      # regulatory response posture.
      group_by(facility_site, quarter_start) %>%
      summarise(action_matrix_col_raw = max(action_matrix_col, na.rm = TRUE),
                 .groups = "drop") %>%
      mutate(above_col1 = as.integer(action_matrix_col_raw >= 2))

    panel <- panel %>%
      left_join(am_keep, by = c("facility_site", "quarter_start"))
    # Note: above_col1 and action_matrix_col_raw are NA where no assessment
    # was recorded for that facility-quarter. Downstream filtering drops
    # those rows in the binomial fit (cannot zero-fill — absence of
    # assessment is structurally different from "assessed at column 1").
  }

  # --- 6. Operational exposure + between/within ---
  panel <- panel %>%
    left_join(ops_facility, by = c("facility_site", "quarter_start"))

  # --- 7. Temporal controls ---
  panel <- panel %>%
    arrange(facility_site, quarter_start) %>%
    mutate(
      year = lubridate::year(quarter_start),
      month_num = lubridate::month(quarter_start),
      yearmonth_num = year + (month_num - 1) / 12
    ) %>%
    mutate(
      yearmonth_num_c = yearmonth_num - mean(yearmonth_num, na.rm = TRUE),
      sin_month = sin(2 * pi * month_num / 12),
      cos_month = cos(2 * pi * month_num / 12)
    ) %>%
    select(-yearmonth_num)

  # Standard alias so downstream code can use `facility` like the event-level pipeline.
  panel$facility <- panel$facility_site
  panel$config_id <- config_id

  message(sprintf(
    "  Panel %s: %d facility-quarters across %d facilities (%s to %s); %d quarters with >=1 LER (%.1f%%)",
    config_id, nrow(panel), n_distinct(panel$facility_site),
    min(panel$quarter_start), max(panel$quarter_start),
    sum(panel$n_lers > 0),
    100 * mean(panel$n_lers > 0)
  ))
  if ("findings_count" %in% names(panel)) {
    message(sprintf(
      "    findings_count: %d non-zero (%.1f%%); findings_nongreen: %d non-zero (%.1f%%)",
      sum(panel$findings_count > 0),
      100 * mean(panel$findings_count > 0),
      sum(panel$findings_nongreen > 0),
      100 * mean(panel$findings_nongreen > 0)
    ))
  }
  if ("above_col1" %in% names(panel)) {
    n_assessed <- sum(!is.na(panel$above_col1))
    message(sprintf(
      "    action_matrix: %d quarters assessed (%.1f%%); above_col1: %d (%.1f%% of assessed)",
      n_assessed, 100 * n_assessed / nrow(panel),
      sum(panel$above_col1, na.rm = TRUE),
      100 * mean(panel$above_col1, na.rm = TRUE)
    ))
  }

  panel
}
