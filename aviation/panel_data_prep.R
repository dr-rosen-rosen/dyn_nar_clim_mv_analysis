# =============================================================================
# aviation/panel_data_prep.R — Aviation Panel-Rate Data Preparation
# =============================================================================
#
# Builds an airport × month panel for one PRIMARY outer-spec combination
# (atc_scope, apt_dist_nm, missing_climate). For each (airport, month) cell,
# aggregates NTSB accidents and severity to counts/sums and joins prior-month
# `departures` (BTS T100) as the rate denominator.
#
# Outcomes (per panel row):
#   n_accidents        : count of NTSB accidents at this airport in this month
#   sum_serious_fatal  : sum of serious + fatal injuries
#   sum_fatalities     : sum of fatal injuries
#
# Exposure: departures (BTS T100, monthly per airport).
# Mundlak between/within for ops controls: seats, passengers (NOT departures —
# that's the offset, including its decomposition would be near-collinear).
#
# Climate: per-report ASRS scores filtered to the configured atc_scope, then
# aggregated to airport-month with strict-lag windowing via build_climate_panel.
#
# Outer-spec defaults (PRIMARY):
#   atc_scope       = "local_terminal"
#   apt_dist_nm     = 5
#   missing_climate = "exclude"
#
# Requires aviation/config.R sourced first (for WINDOW_SPECS, ATC_SCOPE_MAP,
# MIN_REPORTS_DEFAULT, AV_OPS_VARS) and common/panel_data_prep.R.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(lubridate)
  library(stringr)
  library(readxl)
})


# =============================================================================
# OUTCOME AGGREGATION  (NTSB accidents → airport-month)
# =============================================================================

#' Load NTSB accident data and aggregate to airport-month.
#'
#' Mirrors load_ntsb_panel() in aviation/data_prep.R but emits panel-track
#' outcome columns (n_accidents, sum_serious_fatal, sum_fatalities) instead of
#' the event-level binary/hurdle/ordinal triple.
#'
#' @param ntsb_post_path Path to events.xlsx (post-2008)
#' @param ntsb_pre_path  Path to events_pre2008.xlsx
#' @param apt_dist_nm    Maximum distance from nearest airport in nautical miles
#' @return Tibble: airport_id, yearmonth (Date), n_accidents, sum_serious_fatal,
#'   sum_fatalities. One row per airport-month with at least one accident.
load_ntsb_panel_rate <- function(ntsb_post_path, ntsb_pre_path, apt_dist_nm = 5L) {

  message(sprintf("  Loading NTSB data (apt_dist <= %d NM)...", apt_dist_nm))

  shared_cols <- c(
    "ev_type", "ev_date", "ev_year", "ev_month",
    "ev_nr_apt_id", "apt_dist",
    "ev_highest_injury",
    "inj_tot_f", "inj_tot_s", "inj_tot_m", "inj_tot_t"
  )

  read_ntsb <- function(path) {
    readxl::read_excel(path) %>%
      select(any_of(shared_cols)) %>%
      mutate(across(everything(), as.character))
  }

  ntsb <- bind_rows(read_ntsb(ntsb_post_path), read_ntsb(ntsb_pre_path)) %>%
    mutate(
      ev_date  = as.Date(ev_date),
      ev_year  = as.integer(ev_year),
      ev_month = as.integer(ev_month),
      apt_dist = as.numeric(apt_dist),
      inj_tot_f = suppressWarnings(as.integer(inj_tot_f)),
      inj_tot_s = suppressWarnings(as.integer(inj_tot_s)),
      inj_tot_m = suppressWarnings(as.integer(inj_tot_m))
    )

  # Filter to accidents near airports
  acc <- ntsb %>%
    filter(
      ev_type == "ACC",
      !is.na(ev_nr_apt_id),
      is.na(apt_dist) | apt_dist <= apt_dist_nm
    ) %>%
    mutate(
      airport_id = str_to_upper(str_trim(ev_nr_apt_id)),
      yearmonth  = as.Date(paste0(ev_year, "-",
                                  str_pad(ev_month, 2, pad = "0"), "-01"))
    ) %>%
    filter(!is.na(yearmonth))

  acc %>%
    group_by(airport_id, yearmonth) %>%
    summarise(
      n_accidents       = n(),
      sum_serious_fatal = sum(coalesce(inj_tot_f, 0L) + coalesce(inj_tot_s, 0L),
                               na.rm = TRUE),
      sum_fatalities    = sum(coalesce(inj_tot_f, 0L), na.rm = TRUE),
      .groups = "drop"
    )
}


# =============================================================================
# OPERATOR-MONTH PANEL CONSTRUCTION
# =============================================================================

#' Prepare an airport × month panel for one climate config.
#'
#' Steps:
#'   1. Read climate scores from parquet, harmonize report_id → acn
#'   2. Join ASRS metadata, filter to chosen atc_scope, drop airports below
#'      min_reports threshold
#'   3. Build airport × month grid spanning the joint date range
#'   4. Project windowed climate forward (strict-lag) via build_climate_panel
#'   5. Join NTSB accident counts (zero-fill missing airport-months)
#'   6. Join `departures` exposure + ops between/within from BTS T100
#'   7. Apply missing_climate handling
#'
#' @param parquet_path Per-event climate scores parquet for this config.
#' @param config_id Config identifier.
#' @param asrs_meta_df Output of load_asrs_meta() — loaded once per run.
#' @param ntsb_panel_df Output of load_ntsb_panel_rate() — already filtered to
#'   the chosen apt_dist_nm.
#' @param ops_features BTS T100 airport-month parquet rows.
#' @param atc_scope One of names(ATC_SCOPE_MAP). Default "local_terminal".
#' @param missing_climate "exclude" or "impute_mean". Default "exclude".
#' @param min_reports Minimum ATC reports per airport. Default MIN_REPORTS_DEFAULT.
#' @param max_holdover_days Climate recency cap. Default 365.
#' @param climate_base Per-event climate score column.
#' @param window_specs Optional override of WINDOW_SPECS.
#' @return Airport-month panel ready for modeling, or NULL if empty.
prepare_aviation_panel_data <- function(parquet_path, config_id,
                                         asrs_meta_df,
                                         ntsb_panel_df,
                                         ops_features,
                                         aids_panel_df = NULL,
                                         atc_scope = "local_terminal",
                                         missing_climate = "exclude",
                                         min_reports = MIN_REPORTS_DEFAULT,
                                         max_holdover_days = 365L,
                                         climate_base = "overall_final_score",
                                         window_specs = NULL) {

  if (!file.exists(parquet_path)) {
    stop(sprintf("Parquet file not found: %s", parquet_path))
  }
  if (is.null(window_specs)) {
    if (!exists("WINDOW_SPECS")) stop("WINDOW_SPECS not found; source aviation/config.R first.")
    window_specs <- WINDOW_SPECS
  }

  atc_functions <- ATC_SCOPE_MAP[[atc_scope]]
  if (is.null(atc_functions)) {
    stop(sprintf("Unknown atc_scope '%s'. Must be one of: %s",
                 atc_scope, paste(names(ATC_SCOPE_MAP), collapse = ", ")))
  }

  # --- 1. Climate scores joined to ASRS metadata ---
  # The Python feature extractor outputs `report_id` = eid (UUID5 derived from
  # ACN). asrs_meta_df is expected to come from load_asrs_meta_panel() which
  # reads events.parquet and carries the same `eid`. Joining on eid avoids the
  # ACN ↔ UUID translation problem.
  scores <- arrow::read_parquet(parquet_path) %>%
    select(ends_with("_id"), all_of(climate_base))

  if ("report_id" %in% names(scores) && !"eid" %in% names(scores)) {
    scores <- scores %>% rename(eid = report_id)
  }
  scores <- scores %>% mutate(eid = as.character(eid))

  meta_filtered <- asrs_meta_df %>%
    mutate(eid = as.character(eid)) %>%
    filter(
      case_when(
        atc_scope == "local"          ~ atc_local,
        atc_scope == "local_terminal" ~ atc_local | atc_terminal,
        atc_scope == "all_atc"        ~ atc_any,
        TRUE                          ~ FALSE
      )
    )

  climate_events <- scores %>%
    inner_join(meta_filtered, by = "eid") %>%
    filter(!is.na(event_date), !is.na(airport_id))

  if (nrow(climate_events) == 0) {
    warning(sprintf("Config %s: no reports after ATC scope filter (%s)",
                    config_id, atc_scope))
    return(NULL)
  }

  # Filter airports with enough ATC reports
  apt_counts <- climate_events %>% count(airport_id, name = "n_reports")
  keep_apts <- apt_counts %>% filter(n_reports >= min_reports) %>% pull(airport_id)
  climate_events <- climate_events %>% filter(airport_id %in% keep_apts)

  if (nrow(climate_events) == 0) {
    warning(sprintf("Config %s: no airports >= %d reports", config_id, min_reports))
    return(NULL)
  }

  # --- 2. Restrict ops_features to kept airports + harmonize ---
  ops_keep <- ops_features %>%
    mutate(
      airport_id = str_to_upper(str_trim(airport_id)),
      yearmonth  = as.Date(yearmonth)
    ) %>%
    filter(airport_id %in% keep_apts)

  if (nrow(ops_keep) == 0) {
    stop(sprintf("Config %s: no ops rows for kept airports (atc_scope=%s)",
                 config_id, atc_scope))
  }

  ops_min_date <- min(ops_keep$yearmonth, na.rm = TRUE)
  ops_max_date <- max(ops_keep$yearmonth, na.rm = TRUE)

  # --- 3. Build airport × month grid, bounded by ops coverage ---
  grid <- make_panel_grid(climate_events,
                          org_var  = "airport_id",
                          date_var = "event_date",
                          period   = "month",
                          min_date = ops_min_date,
                          max_date = ops_max_date) %>%
    rename(yearmonth = period_start)

  # --- 4. Climate scores at month-start with windowing ---
  climate_panel <- build_climate_panel(
    events_df       = climate_events,
    grid            = grid %>% rename(period_start = yearmonth) %>%
                       mutate(period_end = NA),
    org_var         = "airport_id",
    date_var        = "event_date",
    climate_var     = climate_base,
    window_specs    = window_specs,
    max_holdover_days = max_holdover_days
  ) %>%
    rename(yearmonth = period_start) %>%
    select(-period_end)

  panel <- grid %>%
    select(airport_id, yearmonth) %>%
    left_join(climate_panel, by = c("airport_id", "yearmonth"))

  # --- 5. NTSB outcome aggregation (zero where no accidents) ---
  ntsb_keep <- ntsb_panel_df %>%
    mutate(
      airport_id = str_to_upper(str_trim(airport_id)),
      yearmonth  = as.Date(yearmonth)
    ) %>%
    filter(airport_id %in% keep_apts)

  panel <- panel %>%
    left_join(ntsb_keep, by = c("airport_id", "yearmonth")) %>%
    mutate(
      n_accidents       = coalesce(n_accidents,       0L),
      sum_serious_fatal = coalesce(sum_serious_fatal, 0L),
      sum_fatalities    = coalesce(sum_fatalities,    0L)
    )

  # --- 5b. AIDS outcome aggregation (zero-fill where no AIDS events) ---
  # AIDS provides the broad event-count signal (n_aids_all, n_aids_incidents,
  # n_aids_accidents). NTSB stays canonical for casualty counts (sum_*) per
  # the design decision documented in aviation/panel_fit_models.R.
  if (!is.null(aids_panel_df) && nrow(aids_panel_df) > 0) {
    aids_keep <- aids_panel_df %>%
      mutate(
        airport_id = str_to_upper(str_trim(airport_id)),
        yearmonth  = as.Date(yearmonth)
      ) %>%
      filter(airport_id %in% keep_apts)

    panel <- panel %>%
      left_join(aids_keep, by = c("airport_id", "yearmonth")) %>%
      mutate(
        n_aids_all       = coalesce(n_aids_all,       0L),
        n_aids_accidents = coalesce(n_aids_accidents, 0L),
        n_aids_incidents = coalesce(n_aids_incidents, 0L)
      )
  } else {
    panel <- panel %>%
      mutate(
        n_aids_all       = NA_integer_,
        n_aids_accidents = NA_integer_,
        n_aids_incidents = NA_integer_
      )
  }

  # --- 6. Exposure + between/within ---
  panel <- panel %>%
    left_join(
      ops_keep %>% select(airport_id, yearmonth,
                          departures,
                          seats_between, seats_within,
                          passengers_between, passengers_within),
      by = c("airport_id", "yearmonth")
    ) %>%
    filter(!is.na(departures), departures > 0)

  if (nrow(panel) == 0) {
    warning(sprintf("Config %s: panel empty after departures filter", config_id))
    return(NULL)
  }

  # --- 7. Missing-climate handling ---
  climate_vars_present <- grep(paste0("^", climate_base, "_(sma_|ewmaLAG_)"),
                                names(panel), value = TRUE)
  if (length(climate_vars_present) == 0) {
    warning(sprintf("Config %s: no windowed climate columns produced", config_id))
    return(NULL)
  }

  if (missing_climate == "exclude") {
    # Drop airport-months with NA on the FIRST climate var (others share NA pattern)
    n_before <- nrow(panel)
    panel <- panel %>% filter(!is.na(.data[[climate_vars_present[1]]]))
    message(sprintf("  Excluded %d airport-months with no prior ATC climate",
                    n_before - nrow(panel)))
  } else if (missing_climate == "impute_mean") {
    for (cv in climate_vars_present) {
      panel <- panel %>%
        group_by(airport_id) %>%
        mutate(!!cv := if_else(is.na(.data[[cv]]),
                               mean(.data[[cv]], na.rm = TRUE),
                               .data[[cv]])) %>%
        ungroup()
    }
  }

  if (nrow(panel) == 0) {
    warning(sprintf("Config %s: panel empty after missing_climate handling", config_id))
    return(NULL)
  }

  # --- 8. Temporal controls ---
  panel <- panel %>%
    arrange(airport_id, yearmonth) %>%
    mutate(
      year      = lubridate::year(yearmonth),
      month_num = lubridate::month(yearmonth),
      yearmonth_num = year + (month_num - 1) / 12,
      yearmonth_num_c = as.numeric(scale(yearmonth_num)),
      sin_month = sin(2 * pi * month_num / 12),
      cos_month = cos(2 * pi * month_num / 12),
      config_id = config_id,
      atc_scope = atc_scope,
      missing_climate = missing_climate
    ) %>%
    relocate(airport_id, yearmonth, atc_scope, missing_climate, config_id)

  panel
}
