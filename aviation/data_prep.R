# =============================================================================
# aviation/data_prep.R — Aviation-Specific Data Preparation
# =============================================================================
#
# Provides functions for loading and aligning the three aviation data sources:
#   ASRS  — event-level ATC reports with climate scores (predictor)
#   NTSB  — airport × month accident outcomes
#   BTS   — airport × month operational covariates (T100)
#
# Structural difference from NRC / rail: climate scores come from ASRS (reports)
# and outcomes come from NTSB (accidents) — different sources joined by
# airport_id × yearmonth. The extra step is project_climate_to_panel(), which
# maps report-level windowed climate scores onto the outcome panel grid.
#
# Call order in main script:
#   1. asrs_meta   <- load_asrs_meta(ASRS_CSV_PATH)
#   2. ntsb_panel  <- load_ntsb_panel(NTSB_POST_PATH, NTSB_PRE_PATH, apt_dist_nm)
#   3. ops_features <- read_parquet(OPS_PATH)
#   4. per config: prepare_av_config_data(parquet_path, ..., asrs_meta, ntsb_panel, ops_features)
#
# Requires: aviation/config.R and common/data_prep.R sourced first.
# Dependencies: dplyr, arrow, readxl, readr, lubridate, stringr, slider
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(readxl)
  library(readr)
  library(lubridate)
  library(stringr)
  library(slider)
})


# =============================================================================
# SECTION 1 — ASRS METADATA LOADING
# =============================================================================

#' Load ASRS metadata needed for the aviation analysis
#'
#' Reads only the columns required for airport identification, date parsing,
#' and ATC reporter classification. This is called once in the main script
#' before the config loop.
#'
#' @param asrs_csv_path Path to the ASRS CSV (asrs_dict_df.csv)
#' @return Tibble with one row per report: acn, airport_id, event_date,
#'   atc_local, atc_terminal, atc_enroute, atc_any
load_asrs_meta <- function(asrs_csv_path) {

  message("  Loading ASRS metadata...")

  raw <- read_csv(
    asrs_csv_path,
    col_select     = c(acn, time_date, place_locale_reference, person_1function),
    col_types      = cols(.default = "c"),
    show_col_types = FALSE
  )

  # Parse airport code: take the first token before any ";" separator
  # place_locale_reference can be "CVG", "SEA; BFI", etc.
  raw <- raw %>%
    mutate(
      airport_id = str_to_upper(str_trim(
        str_split_fixed(place_locale_reference, ";", 2)[, 1]
      )),
      airport_id = na_if(airport_id, ""),
      airport_id = na_if(airport_id, "NA")
    )

  # Parse time_date (YYYYMM integer-as-character) to first-of-month Date
  raw <- raw %>%
    mutate(
      .td  = str_pad(time_date, 6, side = "left", pad = "0"),
      .yr  = as.integer(str_sub(.td, 1, 4)),
      .mo  = as.integer(str_sub(.td, 5, 6)),
      event_date = if_else(
        !is.na(.yr) & !is.na(.mo) & .yr >= 1988 & .yr <= 2030 & .mo >= 1 & .mo <= 12,
        as.Date(paste0(.yr, "-", str_pad(.mo, 2, pad = "0"), "-01")),
        as.Date(NA)
      )
    ) %>%
    select(-.td, -.yr, -.mo)

  # ATC reporter flags — match on individual tokens within semicolon-delimited string
  is_any_of <- function(col, targets) {
    map_lgl(str_split(col, ";\\s*"), ~ any(str_trim(.x) %in% targets))
  }

  raw <- raw %>%
    mutate(
      atc_local    = is_any_of(person_1function, ATC_FUNCTIONS_LOCAL),
      atc_terminal = is_any_of(person_1function, ATC_FUNCTIONS_TERMINAL),
      atc_enroute  = is_any_of(person_1function, ATC_FUNCTIONS_ENROUTE),
      atc_any      = atc_local | atc_terminal | atc_enroute
    ) %>%
    select(acn, airport_id, event_date, atc_local, atc_terminal, atc_enroute, atc_any) %>%
    filter(!is.na(event_date), !is.na(airport_id))

  message(sprintf("  ASRS metadata: %s reports | %d airports | ATC: %s (%.1f%%)",
    format(nrow(raw), big.mark = ","),
    n_distinct(raw$airport_id),
    format(sum(raw$atc_any), big.mark = ","),
    100 * mean(raw$atc_any)
  ))

  raw
}


# =============================================================================
# SECTION 2 — NTSB OUTCOME LOADING AND ENCODING
# =============================================================================

#' Encode NTSB outcome variables from raw accident fields
#'
#' @param df NTSB events data frame with raw injury and severity columns
#' @return df with accident_binary, inj_serious_fatal, sev_ord, sev_ord_factor added
encode_av_outcomes <- function(df) {

  df %>%
    mutate(
      inj_tot_f = as.integer(inj_tot_f),
      inj_tot_s = as.integer(inj_tot_s),
      inj_tot_m = as.integer(inj_tot_m),

      # Combined serious + fatal injury count (primary severity outcome)
      inj_serious_fatal = coalesce(inj_tot_f, 0L) + coalesce(inj_tot_s, 0L),

      # Ordinal severity: 0 = none, 1 = minor, 2 = serious, 3 = fatal
      sev_ord = case_when(
        !is.na(inj_tot_f) & inj_tot_f > 0  ~ 3L,
        !is.na(inj_tot_s) & inj_tot_s > 0  ~ 2L,
        !is.na(inj_tot_m) & inj_tot_m > 0  ~ 1L,
        TRUE                                ~ 0L
      ),
      sev_ord_factor = factor(sev_ord, levels = 0:3, ordered = TRUE)
    )
}


#' Load and aggregate NTSB accident data to airport × month panel
#'
#' Combines pre- and post-2008 NTSB files, filters to accidents near airports,
#' encodes outcomes, and collapses to one row per airport × month.
#'
#' @param ntsb_post_path Path to events.xlsx (post-2008)
#' @param ntsb_pre_path  Path to events_pre2008.xlsx
#' @param apt_dist_nm    Maximum distance from nearest airport in nautical miles
#' @return Tibble with one row per airport × month that had >= 1 accident,
#'   plus aggregated accident_binary, inj_serious_fatal, sev_ord columns
load_ntsb_panel <- function(ntsb_post_path, ntsb_pre_path, apt_dist_nm = 5L) {

  message(sprintf("  Loading NTSB data (apt_dist <= %d NM)...", apt_dist_nm))

  shared_cols <- c(
    "ev_type", "ev_date", "ev_year", "ev_month",
    "ev_nr_apt_id", "apt_dist",
    "ev_highest_injury",
    "inj_tot_f", "inj_tot_s", "inj_tot_m", "inj_tot_t"
  )

  read_ntsb <- function(path) {
    read_excel(path) %>%
      select(any_of(shared_cols)) %>%
      mutate(across(everything(), as.character))
  }

  ntsb <- bind_rows(read_ntsb(ntsb_post_path), read_ntsb(ntsb_pre_path)) %>%
    mutate(
      ev_date  = as.Date(ev_date),
      ev_year  = as.integer(ev_year),
      ev_month = as.integer(ev_month),
      apt_dist = as.numeric(apt_dist)
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
    filter(!is.na(yearmonth)) %>%
    encode_av_outcomes()

  # Aggregate to airport × month
  panel <- acc %>%
    group_by(airport_id, yearmonth) %>%
    summarise(
      n_accidents       = n(),
      accident_binary   = 1L,
      inj_serious_fatal = sum(inj_serious_fatal, na.rm = TRUE),
      n_fatal           = sum(coalesce(inj_tot_f %>% as.integer(), 0L), na.rm = TRUE),
      max_sev_ord       = max(sev_ord, na.rm = TRUE),
      .groups           = "drop"
    ) %>%
    mutate(
      sev_ord        = max_sev_ord,
      sev_ord_factor = factor(sev_ord, levels = 0:3, ordered = TRUE)
    ) %>%
    select(-max_sev_ord)

  message(sprintf("  NTSB panel: %s accident-months | %d airports | date range: %s – %s",
    format(nrow(panel), big.mark = ","),
    n_distinct(panel$airport_id),
    min(panel$yearmonth), max(panel$yearmonth)
  ))

  panel
}


# =============================================================================
# SECTION 3 — OPERATIONAL DATA JOIN
# =============================================================================

#' Join BTS T100 operational features to airport × month panel
#'
#' @param panel_df Panel with airport_id and yearmonth columns
#' @param ops_features Output of 1_get_bts_t100.R (airport_month_ops.parquet)
#' @return panel_df with departures_between and departures_within columns added
join_av_ops <- function(panel_df, ops_features) {

  if (is.null(ops_features) || nrow(ops_features) == 0) {
    message("  No ops data — inserting NA placeholders")
    for (v in paste0(AV_OPS_VARS, c("_between", "_within"))) {
      panel_df[[v]] <- NA_real_
    }
    return(panel_df)
  }

  ops <- ops_features %>%
    mutate(
      airport_id = str_to_upper(str_trim(airport_id)),
      yearmonth  = as.Date(yearmonth)
    ) %>%
    select(airport_id, yearmonth,
           matches("_between$|_within$"))

  df <- panel_df %>%
    left_join(ops, by = c("airport_id", "yearmonth"))

  # Report join rate for primary ops var
  check_col <- paste0(AV_OPS_VARS[1], "_between")
  if (check_col %in% names(df)) {
    n_matched <- sum(!is.na(df[[check_col]]))
    message(sprintf("  Ops join: %d / %d rows matched (%.1f%%)",
        n_matched, nrow(df), 100 * n_matched / nrow(df)))
  }

  df
}


# =============================================================================
# SECTION 4 — CLIMATE PROJECTION: REPORTS → AIRPORT × MONTH PANEL
# =============================================================================

#' Project report-level windowed climate onto the airport × month outcome grid
#'
#' For each airport × outcome_month cell, finds the most recent ATC report
#' submitted BEFORE that month and carries its windowed climate value forward.
#' This enforces strict temporal ordering: outcome month M uses only climate
#' information from reports dated before the first day of M.
#'
#' @param report_df  Report-level data with airport_id, event_date, climate cols
#' @param panel_df   Airport × month outcome panel (airport_id, yearmonth)
#' @param climate_vars Character vector of windowed climate column names
#' @return panel_df with climate columns joined
project_climate_to_panel <- function(report_df, panel_df, climate_vars) {

  report_slim <- report_df %>%
    select(airport_id, report_date = event_date, all_of(climate_vars)) %>%
    filter(!is.na(report_date))

  # Inequality join: panel yearmonth > report_date (strict: report before month)
  # Then keep only the most recent report per airport × month.
  joined <- panel_df %>%
    left_join(
      report_slim,
      by      = join_by(airport_id, yearmonth > report_date),
      relationship = "many-to-many"
    ) %>%
    group_by(airport_id, yearmonth) %>%
    slice_max(report_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-report_date)

  n_with_climate <- sum(!is.na(joined[[climate_vars[1]]]))
  message(sprintf("  Climate projection: %d / %d panel rows have prior ATC climate (%.1f%%)",
      n_with_climate, nrow(joined), 100 * n_with_climate / nrow(joined)))

  joined
}


# =============================================================================
# SECTION 5 — UNIFIED DATA PREPARATION
# =============================================================================

#' Prepare analysis-ready panel for one aviation config
#'
#' Loads climate scores from a config parquet, joins to pre-loaded ASRS metadata,
#' filters to the requested ATC scope and airport set, computes windowed climate
#' variables, projects onto the NTSB accident panel, joins BTS ops, and adds
#' temporal controls.
#'
#' @param parquet_path   Path to config's climate score parquet
#' @param config_id      Config ID string (stored in output)
#' @param asrs_meta_df   Output of load_asrs_meta() — loaded once per run
#' @param ntsb_panel_df  Output of load_ntsb_panel() — one row per accident-month
#' @param ops_features   BTS T100 ops parquet loaded into memory
#' @param atc_scope      ATC tier: "local", "local_terminal", or "all_atc"
#' @param missing_climate How to handle airport-months with no prior ATC reports:
#'                        "exclude" (drop rows) or "impute_mean" (airport mean)
#' @param min_reports    Minimum ATC reports per airport to include that airport
#' @param min_n          Minimum prior events for a valid climate window
#' @return Analysis-ready panel (one row per airport × outcome_month) or NULL
prepare_av_config_data <- function(parquet_path,
                                    config_id,
                                    asrs_meta_df,
                                    ntsb_panel_df,
                                    ops_features   = NULL,
                                    atc_scope      = "local_terminal",
                                    missing_climate = "exclude",
                                    min_reports    = MIN_REPORTS_DEFAULT,
                                    min_n          = 1L) {

  message(sprintf("\n  Config: %s | scope: %s | missing: %s",
      config_id, atc_scope, missing_climate))

  # ------------------------------------------------------------------
  # 1. Load climate scores and harmonise report ID
  # ------------------------------------------------------------------
  scores <- read_parquet(parquet_path) %>%
    select(
      ends_with("_id"),
      starts_with("overall_"),
      ends_with("_domain_score")
    )

  if ("report_id" %in% names(scores) && !"acn" %in% names(scores)) {
    scores <- scores %>% rename(acn = report_id)
  }
  scores <- scores %>% mutate(acn = as.character(acn))

  # ------------------------------------------------------------------
  # 2. Join to ASRS metadata → airport, date, ATC flags
  # ------------------------------------------------------------------
  atc_functions <- ATC_SCOPE_MAP[[atc_scope]]
  if (is.null(atc_functions)) {
    stop(sprintf("Unknown atc_scope '%s'. Must be one of: %s",
                 atc_scope, paste(names(ATC_SCOPE_MAP), collapse = ", ")))
  }

  meta_filtered <- asrs_meta_df %>%
    mutate(acn = as.character(acn)) %>%
    filter(
      case_when(
        atc_scope == "local"          ~ atc_local,
        atc_scope == "local_terminal" ~ atc_local | atc_terminal,
        atc_scope == "all_atc"        ~ atc_any,
        TRUE                          ~ FALSE
      )
    )

  df <- scores %>%
    inner_join(meta_filtered, by = "acn") %>%
    filter(!is.na(event_date), !is.na(airport_id))

  if (nrow(df) == 0) {
    warning(sprintf("Config %s: no reports after ATC scope filter", config_id))
    return(NULL)
  }

  # ------------------------------------------------------------------
  # 3. Filter airports with enough ATC reports
  # ------------------------------------------------------------------
  df <- df %>%
    group_by(airport_id) %>%
    filter(n() >= min_reports) %>%
    ungroup()

  if (nrow(df) == 0) {
    warning(sprintf("Config %s: no airports with >= %d reports", config_id, min_reports))
    return(NULL)
  }

  message(sprintf("  After filters: %s reports | %d airports",
      format(nrow(df), big.mark = ","), n_distinct(df$airport_id)))

  # ------------------------------------------------------------------
  # 4. Sort and sequence within airport; create windowed climate vars
  # ------------------------------------------------------------------
  df <- df %>%
    arrange(airport_id, event_date) %>%
    group_by(airport_id) %>%
    mutate(t = row_number()) %>%
    ungroup()

  df$config_id <- config_id

  # create_all_windows() uses event_date and WINDOW_SPECS (from config.R)
  df <- create_all_windows(
    df,
    climate_var_base = "overall_final_score",
    org_var          = "airport_id",
    min_n            = min_n
  )

  climate_vars <- get_climate_vars(df, base_var = "overall_final_score")

  if (length(climate_vars) == 0) {
    warning(sprintf("Config %s: no windowed climate vars created", config_id))
    return(NULL)
  }

  # ------------------------------------------------------------------
  # 5. Build the airport × month outcome panel
  #    Use NTSB panel airports intersected with airports in the ASRS data
  # ------------------------------------------------------------------
  asrs_airports <- unique(df$airport_id)
  ntsb_airports <- unique(ntsb_panel_df$airport_id)
  common_airports <- intersect(asrs_airports, ntsb_airports)

  if (length(common_airports) == 0) {
    warning(sprintf("Config %s: no airport overlap between ASRS and NTSB", config_id))
    return(NULL)
  }

  # Full panel: every airport × every month in the analysis window
  date_range <- range(c(df$event_date, ntsb_panel_df$yearmonth), na.rm = TRUE)
  all_months <- seq.Date(
    floor_date(date_range[1], "month"),
    floor_date(date_range[2], "month"),
    by = "month"
  )

  full_panel <- expand_grid(
    airport_id = common_airports,
    yearmonth  = all_months
  ) %>%
    # Join NTSB outcomes (only accident-months have rows; others get 0 / NA)
    left_join(
      ntsb_panel_df %>% filter(airport_id %in% common_airports),
      by = c("airport_id", "yearmonth")
    ) %>%
    mutate(
      accident_binary   = coalesce(accident_binary,   0L),
      inj_serious_fatal = coalesce(inj_serious_fatal, 0L),
      sev_ord           = coalesce(sev_ord,           0L),
      sev_ord_factor    = factor(sev_ord, levels = 0:3, ordered = TRUE)
    )

  # ------------------------------------------------------------------
  # 6. Project windowed climate onto the panel
  # ------------------------------------------------------------------
  full_panel <- project_climate_to_panel(df, full_panel, climate_vars)

  # ------------------------------------------------------------------
  # 7. Handle missing climate (airport-months with no prior ATC reports)
  # ------------------------------------------------------------------
  n_missing <- sum(is.na(full_panel[[climate_vars[1]]]))

  if (missing_climate == "exclude") {
    full_panel <- full_panel %>%
      filter(!is.na(.data[[climate_vars[1]]]))
    message(sprintf("  Excluded %d airport-months with no prior ATC climate",
        n_missing))

  } else if (missing_climate == "impute_mean") {
    for (cv in climate_vars) {
      full_panel <- full_panel %>%
        group_by(airport_id) %>%
        mutate(!!cv := if_else(is.na(.data[[cv]]),
                               mean(.data[[cv]], na.rm = TRUE),
                               .data[[cv]])) %>%
        ungroup()
    }
    message(sprintf("  Imputed %d airport-months with airport-mean climate", n_missing))
  }

  if (nrow(full_panel) == 0) {
    warning(sprintf("Config %s: empty panel after missing climate treatment", config_id))
    return(NULL)
  }

  # ------------------------------------------------------------------
  # 8. Join operational covariates
  # ------------------------------------------------------------------
  full_panel <- join_av_ops(full_panel, ops_features)

  # ------------------------------------------------------------------
  # 9. Add temporal controls on the panel (not carried from report level)
  # ------------------------------------------------------------------
  full_panel <- full_panel %>%
    mutate(
      year            = year(yearmonth),
      month           = month(yearmonth),
      yearmonth_num   = (year - min(year)) * 12 + month,
      yearmonth_num_c = as.vector(scale(yearmonth_num)),
      sin_month       = sin(2 * pi * month / 12),
      cos_month       = cos(2 * pi * month / 12)
    )

  full_panel$config_id <- config_id

  message(sprintf("  Final panel: %s rows | %d airports | accident rate: %.2f%%",
      format(nrow(full_panel), big.mark = ","),
      n_distinct(full_panel$airport_id),
      100 * mean(full_panel$accident_binary)))

  full_panel
}
