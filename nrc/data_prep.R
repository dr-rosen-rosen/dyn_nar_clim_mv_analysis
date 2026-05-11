# =============================================================================
# nrc/data_prep.R — NRC-Specific Data Preparation
# =============================================================================
#
# Industry-specific: loading config parquets, joining to NRC event data and
# quarterly operational features, outcome encoding (scram, emergency class,
# power loss).
#
# Windowing and climate var detection are in common/data_prep.R.
#
# Requires: nrc/config.R and common/data_prep.R sourced first.
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

#' Join NRC operational covariates to event-level data
#'
#' Matches events to the PRIOR quarter's operational data. Computes
#' between/within decomposition if not already present. Aggregates from
#' unit-level to site-level (NRC ops data is per unit; events are per site).
#'
#' @param df Event-level data with 'facility' and 'event_date' columns
#' @param ops_features Quarterly operational features (or NULL to skip)
#' @return df with operational covariate columns added
join_nrc_ops <- function(df, ops_features) {

  if (is.null(ops_features) || nrow(ops_features) == 0) {
    message("  No operational data provided — creating NA placeholders")
    for (v in NRC_OPS_VARS) {
      df[[paste0(v, "_between")]] <- NA_real_
      df[[paste0(v, "_within")]]  <- NA_real_
    }
    return(df)
  }

  ops <- ops_features %>%
    mutate(quarter_start = as.Date(quarter_start))

  # Check if between/within already computed
  has_bw <- any(grepl("_between$", names(ops)))

  if (!has_bw) {
    message("  Computing between/within decomposition for NRC ops variables...")
    available_vars <- intersect(NRC_OPS_VARS, names(ops))

    for (v in available_vars) {
      ops <- ops %>%
        arrange(facility_unit, quarter_start) %>%
        group_by(facility_unit) %>%
        mutate(
          !!paste0(v, "_roll_mean") := slider::slide_dbl(
            .data[[v]], mean, na.rm = TRUE,
            .before = NRC_OPS_ROLL_K - 1L, .complete = TRUE
          ),
          !!paste0(v, "_between") := dplyr::lag(
            .data[[paste0(v, "_roll_mean")]], NRC_OPS_LAG_K
          ),
          !!paste0(v, "_within") := .data[[v]] - .data[[paste0(v, "_between")]]
        ) %>%
        select(-!!paste0(v, "_roll_mean")) %>%
        ungroup()
    }
  }

  # Compute prior quarter start for each event
  df <- df %>%
    mutate(
      .event_quarter = lubridate::quarter(event_date),
      .event_year    = lubridate::year(event_date),
      .prior_quarter = ifelse(.event_quarter == 1L, 4L, .event_quarter - 1L),
      .prior_year    = ifelse(.event_quarter == 1L, .event_year - 1L, .event_year),
      .prior_quarter_start = as.Date(paste0(
        .prior_year, "-",
        c("01", "04", "07", "10")[.prior_quarter], "-01"
      )),
      .facility_site = tolower(trimws(facility))
    )

  # Identify ops columns to aggregate
  between_cols <- paste0(NRC_OPS_VARS, "_between")
  within_cols  <- paste0(NRC_OPS_VARS, "_within")
  numeric_ops_cols <- intersect(c(between_cols, within_cols, NRC_OPS_VARS), names(ops))

  # Normalize ops facility names and strip unit numbers for site-level matching
  ops <- ops %>%
    mutate(.facility_site = tolower(trimws(
      sub("\\s*(unit\\s*)?\\d+\\s*$", "", facility_unit, ignore.case = TRUE)
    )))

  # Aggregate to site × quarter level (mean across units at same site)
  ops_site <- ops %>%
    group_by(.facility_site, quarter_start) %>%
    summarise(
      across(any_of(numeric_ops_cols), ~ mean(.x, na.rm = TRUE)),
      .n_units = n(),
      .groups = "drop"
    ) %>%
    rename(.prior_quarter_start = quarter_start)

  # Join
  df <- df %>%
    left_join(ops_site, by = c(".facility_site", ".prior_quarter_start"))

  # Report join rate
  ops_check_col <- intersect(between_cols, names(df))[1]
  if (!is.na(ops_check_col) && ops_check_col %in% names(df)) {
    n_matched <- sum(!is.na(df[[ops_check_col]]))
    message(sprintf("  Ops join: %d/%d events matched (%.1f%%)",
                    n_matched, nrow(df), 100 * n_matched / nrow(df)))
  }

  # Clean temp columns
  df <- df %>% select(-starts_with("."))

  return(df)
}


# =============================================================================
# OUTCOME ENCODING
# =============================================================================

#' Encode NRC outcome variables from raw event fields
#'
#' Creates scram (binary + ordinal), emergency (binary), and power loss
#' outcomes from the raw scram_code, emerg_class, and power loss fields.
#'
#' @param df Event-level data with raw outcome columns
#' @return df with encoded outcome columns added
encode_nrc_outcomes <- function(df) {

  # --- Scram severity ---
  if ("scram_code" %in% names(df)) {
    df <- df %>%
      mutate(
        .scram_raw = tolower(trimws(as.character(scram_code))),
        scram_ord = case_when(
          # Automatic scram (A) and automatic scram with reactor trip (A/R)
          # are both severity-2 ("automatic"). The /R suffix only flags that
          # the reactor tripped sub-critical; it doesn't increase severity.
          .scram_raw %in% c("a", "a/r", "auto", "automatic", "a/m") ~ 2L,
          # Manual scram (M) and manual with reactor trip (M/R) are both
          # severity-1 ("manual").
          .scram_raw %in% c("m", "m/r", "manual")                   ~ 1L,
          TRUE                                                       ~ 0L
        ),
        scram_binary = as.integer(scram_ord > 0),
        scram_ord_factor = factor(scram_ord, ordered = TRUE)
      ) %>%
      select(-.scram_raw)
  } else if ("scram_ord" %in% names(df)) {
    df <- df %>%
      mutate(
        scram_binary = as.integer(scram_ord > 0),
        scram_ord_factor = factor(scram_ord, ordered = TRUE)
      )
  }

  # --- Emergency classification (binary: any emergency vs. none) ---
  if ("emerg_class" %in% names(df) && !"emerg_binary" %in% names(df)) {
    df <- df %>%
      mutate(
        .ec_raw = tolower(trimws(as.character(emerg_class))),
        emerg_binary = case_when(
          .ec_raw %in% c("ale", "alert")                                    ~ 1L,
          .ec_raw %in% c("unu", "unusual event", "ue")                      ~ 1L,
          .ec_raw %in% c("sae", "site area emergency")                      ~ 1L,
          .ec_raw %in% c("ge", "general emergency")                         ~ 1L,
          .ec_raw %in% c("", "na", "n/a", "none", "not applicable")         ~ 0L,
          is.na(.ec_raw)                                                     ~ 0L,
          TRUE                                                               ~ 0L
        )
      ) %>%
      select(-.ec_raw)
  }

  # --- Power loss percentage ---
  if (all(c("initial_pwr", "current_pwr") %in% names(df))) {
    df <- df %>%
      mutate(
        initial_pwr = as.numeric(initial_pwr),
        current_pwr = as.numeric(current_pwr),
        pct_power_loss = initial_pwr - current_pwr,
        # Floor negative values to 0 (data quality: current > initial shouldn't count as loss)
        pct_power_loss = ifelse(!is.na(pct_power_loss) & pct_power_loss < 0,
                                0, pct_power_loss),
        # Binary gate for hurdle model
        power_loss_binary = as.integer(!is.na(pct_power_loss) & pct_power_loss > 0)
      )
  } else if ("pct_power_loss" %in% names(df)) {
    # Already computed upstream
    df <- df %>%
      mutate(
        pct_power_loss = as.numeric(pct_power_loss),
        pct_power_loss = ifelse(!is.na(pct_power_loss) & pct_power_loss < 0,
                                0, pct_power_loss),
        power_loss_binary = as.integer(!is.na(pct_power_loss) & pct_power_loss > 0)
      )
  }

  return(df)
}


# =============================================================================
# UNIFIED DATA PREPARATION
# =============================================================================

#' Prepare data for a single NRC configuration
#'
#' Loads config parquet (climate scores), joins to event metadata and
#' operational features, creates windowed climate variables, encodes outcomes.
#'
#' @param parquet_path Path to the config's climate scores parquet
#' @param config_id Configuration ID string
#' @param events_df Event-level data (nrc_events) with eid, facility, event_date, etc.
#' @param ops_features Quarterly operational features (or NULL)
#' @param min_reports Minimum events per facility to include
#' @param min_n Minimum events for window calculation
#' @return Data frame ready for modeling
prepare_nrc_config_data <- function(parquet_path, config_id, events_df,
                                     ops_features = NULL,
                                     min_reports = MIN_REPORTS_DEFAULT,
                                     min_n = 1) {

  # Load climate scores
  df <- arrow::read_parquet(parquet_path) %>%
    select(
      ends_with('_id'),
      starts_with('overall_'),
      ends_with('_domain_score')
    )

  # Harmonize ID column
  if ("report_id" %in% names(df) && !"eid" %in% names(df)) {
    df <- df %>% rename(eid = report_id)
  }

  # Join to event metadata
  df <- df %>%
    left_join(events_df, by = "eid") %>%
    filter(!is.na(event_date))

  # Filter to facilities with enough events
  df <- df %>%
    group_by(facility) %>%
    filter(n() >= min_reports) %>%
    ungroup()

  if (nrow(df) == 0) {
    warning(sprintf("Config %s: No data after filtering (min_reports=%d)",
                    config_id, min_reports))
    return(NULL)
  }

  # Sort and add sequence number
  df <- df %>%
    arrange(facility, event_date) %>%
    group_by(facility) %>%
    mutate(t = row_number()) %>%
    ungroup()

  df$config_id <- config_id

  # Create windowed climate variables (from common/data_prep.R)
  df <- create_all_windows(df, climate_var_base = "overall_final_score",
                           org_var = "facility", min_n = min_n)

  # Join operational features
  df <- join_nrc_ops(df, ops_features)

  # Encode outcomes
  df <- encode_nrc_outcomes(df)

  return(df)
}
