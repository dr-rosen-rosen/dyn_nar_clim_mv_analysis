# =============================================================================
# phmsa/panel_data_prep.R — PHMSA Panel-Rate Data Preparation
# =============================================================================
#
# Builds an operator-year panel for one pipeline_type at a time. For each
# (operator, year) cell, aggregates incidents and severity to counts/sums and
# joins prior-year `total_miles` as the rate denominator.
#
# Outcomes (per panel row):
#   n_incidents          : count of events
#   sum_injuries         : sum of total_injuries (workers + GP + ER + contractors)
#   sum_fatalities       : sum of total_fatalities
#   sum_release_volume   : sum of natural-unit release volume (Tweedie target)
#                          gas_*: GAS_COST_IN_MCF (thousand cubic feet)
#                          hazardous_liquid: UNINTENTIONAL_RELEASE_BBLS (barrels)
#                          Units differ by pipeline_type; never compared.
#
# Exposure: total_miles (prior-year report; PARTDTOTALMILES underneath).
#
# Climate: per-event scores aggregated to operator-year via the windowing
#   helpers in common/panel_data_prep.R. Strict-lag enforced — for each panel
#   row (operator, year_start), only events strictly before year_start
#   contribute, with a 365-day max holdover (reusable from the rail/NRC tracks).
#
# Requires phmsa/config.R sourced first (for WINDOW_SPECS, PIPELINE_TYPES,
# MIN_REPORTS_DEFAULT) and common/panel_data_prep.R (for make_panel_grid /
# build_climate_panel).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(lubridate)
  library(stringr)
})


# =============================================================================
# OUTCOME AGGREGATION  (events → operator-year)
# =============================================================================

#' Aggregate event severity to operator-year counts/sums.
#'
#' @param events_df Tibble of events filtered to one pipeline_type.
#' @param release_col Name of the column to sum as natural-unit release volume.
#'   Resolves automatically by pipeline_type if NULL: gas_* → GAS_COST_IN_MCF,
#'   hazardous_liquid → UNINTENTIONAL_RELEASE_BBLS.
#' @return Tibble: operator_id, year_start (Date), n_incidents, sum_injuries,
#'   sum_fatalities, sum_release_volume.
aggregate_phmsa_outcomes <- function(events_df, release_col = NULL) {

  # Resolve release_col by pipeline_type if not supplied.
  if (is.null(release_col)) {
    pt <- unique(events_df$pipeline_type)
    if (length(pt) != 1) {
      stop("aggregate_phmsa_outcomes: events_df must contain exactly one ",
           "pipeline_type, or release_col must be supplied. Found: ",
           paste(pt, collapse = ", "))
    }
    release_col <- if (pt %in% c("gas_distribution", "gas_transmission")) {
      "GAS_COST_IN_MCF"
    } else if (pt == "hazardous_liquid") {
      "UNINTENTIONAL_RELEASE_BBLS"
    } else {
      NA_character_
    }
  }

  # Tolerate missing release column (not all PHMSA pipeline_types have one)
  if (!is.na(release_col) && release_col %in% names(events_df)) {
    events_df$.rel_vol <- suppressWarnings(as.numeric(events_df[[release_col]]))
  } else {
    events_df$.rel_vol <- NA_real_
  }

  events_df %>%
    mutate(
      year_start = floor_date(as.Date(event_date), unit = "year"),
      .ti = suppressWarnings(as.numeric(total_injuries)),
      .tf = suppressWarnings(as.numeric(total_fatalities))
    ) %>%
    filter(!is.na(year_start), !is.na(operator_id)) %>%
    group_by(operator_id, year_start) %>%
    summarise(
      n_incidents        = n(),
      sum_injuries       = sum(.ti, na.rm = TRUE),
      sum_fatalities     = sum(.tf, na.rm = TRUE),
      sum_release_volume = sum(.rel_vol, na.rm = TRUE),
      .groups = "drop"
    )
}


# =============================================================================
# OPERATOR-YEAR PANEL CONSTRUCTION
# =============================================================================

#' Prepare an operator-year panel for one pipeline_type.
#'
#' @param parquet_path Per-event climate scores parquet for this config.
#' @param config_id Config identifier.
#' @param events_df Events filtered to one pipeline_type. Must have:
#'   eid, operator_id, event_date, total_injuries, total_fatalities,
#'   UNINTENTIONAL_RELEASE_BBLS (or lowercase variant), pipeline_type.
#' @param ops_features operator_annual_ops.parquet rows for THIS pipeline_type.
#' @param pipeline_type Which pipeline_type we're building for (used to filter
#'   ops to matching rows; events_df should already be filtered).
#' @param min_reports Minimum incidents per operator across the panel window
#'   to retain. Mirrors the rail/NRC threshold.
#' @param max_holdover_days Climate recency cap.
#' @param climate_base Per-event climate score column.
#' @param window_specs Optional override of WINDOW_SPECS.
#' @return Operator-year panel tibble ready for modeling, or NULL if empty.
prepare_phmsa_panel_data <- function(parquet_path, config_id, events_df,
                                      ops_features,
                                      pipeline_type,
                                      min_reports = MIN_REPORTS_DEFAULT,
                                      max_holdover_days = 365L,
                                      climate_base = "overall_final_score",
                                      window_specs = NULL) {

  if (!file.exists(parquet_path)) {
    stop(sprintf("Parquet file not found: %s", parquet_path))
  }
  if (is.null(window_specs)) {
    if (!exists("WINDOW_SPECS")) stop("WINDOW_SPECS not found; source phmsa/config.R first.")
    window_specs <- WINDOW_SPECS
  }

  # --- 1. Climate scores joined to event metadata ---
  climate_events <- arrow::read_parquet(parquet_path) %>%
    select(report_id, all_of(climate_base)) %>%
    rename(eid = report_id) %>%
    inner_join(events_df, by = "eid") %>%
    filter(!is.na(event_date), !is.na(operator_id))

  if (nrow(climate_events) == 0) {
    warning(sprintf("Config %s: no events after climate join", config_id))
    return(NULL)
  }

  # Filter to operators with sufficient incidents (panel-level threshold)
  op_counts <- climate_events %>% count(operator_id, name = "n_events")
  keep_ops <- op_counts %>% filter(n_events >= min_reports) %>% pull(operator_id)
  climate_events <- climate_events %>% filter(operator_id %in% keep_ops)

  if (nrow(climate_events) == 0) {
    warning(sprintf("Config %s: no operators above min_reports=%d for pipeline_type %s",
                    config_id, min_reports, pipeline_type))
    return(NULL)
  }

  # --- 2. Filter ops to the same pipeline_type and keep_ops ---
  ops_keep <- ops_features %>%
    filter(pipeline_type == !!pipeline_type, OPERATOR_ID %in% keep_ops) %>%
    rename(operator_id = OPERATOR_ID)

  if (nrow(ops_keep) == 0) {
    stop(sprintf("Config %s: no ops rows for pipeline_type %s after operator filter",
                 config_id, pipeline_type))
  }

  ops_min_year <- min(ops_keep$REPORT_YEAR, na.rm = TRUE)
  ops_max_year <- max(ops_keep$REPORT_YEAR, na.rm = TRUE)
  ops_min_date <- as.Date(sprintf("%d-01-01", ops_min_year + 1L))   # +1: prior-year join
  ops_max_date <- as.Date(sprintf("%d-12-31", ops_max_year + 1L))

  # --- 3. Build operator-year grid ---
  grid <- make_panel_grid(climate_events,
                          org_var  = "operator_id",
                          date_var = "event_date",
                          period   = "year",
                          min_date = ops_min_date,
                          max_date = ops_max_date) %>%
    rename(year_start = period_start)

  # --- 4. Climate scores at year-start with windowing ---
  climate_panel <- build_climate_panel(
    events_df       = climate_events,
    grid            = grid %>% rename(period_start = year_start) %>%
                       mutate(period_end = NA),
    org_var         = "operator_id",
    date_var        = "event_date",
    climate_var     = climate_base,
    window_specs    = window_specs,
    max_holdover_days = max_holdover_days
  ) %>%
    rename(year_start = period_start) %>%
    select(-period_end)

  panel <- grid %>%
    select(operator_id, year_start) %>%
    left_join(climate_panel, by = c("operator_id", "year_start"))

  # --- 5. Outcomes (zero where no events) ---
  outcomes <- aggregate_phmsa_outcomes(climate_events)
  panel <- panel %>%
    left_join(outcomes, by = c("operator_id", "year_start")) %>%
    mutate(
      n_incidents        = coalesce(n_incidents, 0L),
      sum_injuries       = coalesce(sum_injuries, 0),
      sum_fatalities     = coalesce(sum_fatalities, 0),
      sum_release_volume = coalesce(sum_release_volume, 0)
    )

  # --- 6. Exposure + between/within (prior-year join) ---
  # ops REPORT_YEAR maps to year_start of (REPORT_YEAR + 1) — prior-year report.
  ops_for_join <- ops_keep %>%
    mutate(year_start = as.Date(sprintf("%d-01-01", REPORT_YEAR + 1L))) %>%
    select(operator_id, year_start, total_miles,
           total_miles_between, total_miles_within)

  panel <- panel %>%
    left_join(ops_for_join, by = c("operator_id", "year_start")) %>%
    filter(!is.na(total_miles), total_miles > 0)

  if (nrow(panel) == 0) {
    warning(sprintf("Config %s pipeline_type %s: panel empty after ops join + total_miles>0",
                    config_id, pipeline_type))
    return(NULL)
  }

  # --- 7. Temporal controls ---
  panel <- panel %>%
    arrange(operator_id, year_start) %>%
    mutate(
      year          = year(year_start),
      yearmonth_num = year,
      yearmonth_num_c = as.numeric(scale(year))
    ) %>%
    mutate(config_id = config_id, pipeline_type = pipeline_type) %>%
    relocate(operator_id, year_start, pipeline_type, config_id)

  panel
}
