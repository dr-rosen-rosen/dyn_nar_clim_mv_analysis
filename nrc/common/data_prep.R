# common/data_prep.R
# NRC Nuclear Multiverse — Shared Data Preparation Functions
# Sourced by: 1_nrc_multiverse.R, 2_nrc_cv.R
#
# Requires: common/config.R sourced first
# Dependencies: tidyverse, arrow, slider, lubridate, glue

# ==============================================================================
# WINDOW CREATION FUNCTIONS (same core logic as rail)
# ==============================================================================

#' EWMA with time-decay for irregularly spaced events
#'
#' Computes a weighted moving average where weights decay exponentially
#' based on calendar time gaps between events. Only looks backward from
#' each event (strictly lagged).
#'
#' @param x Numeric vector of values (e.g., climate scores)
#' @param dates Date vector of event dates (same length as x)
#' @param window_days Lookback window in days
#' @param half_life_days Half-life for exponential decay
#' @param min_n Minimum prior events required for a valid estimate
#' @return Numeric vector of EWMA values (NA where insufficient history)
ewma_time_decay_irregular_lag <- function(x, dates, window_days, half_life_days, min_n = 1) {
  n <- length(x)
  result <- rep(NA_real_, n)
  lambda <- log(2) / half_life_days

  for (i in 2:n) {
    lookback_start <- dates[i] - window_days
    prior_idx <- which(dates[1:(i-1)] >= lookback_start)

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


#' Create all windowed climate variables (SMA + EWMA)
#'
#' Adds one column per window specification to the data frame.
#' Also creates temporal control variables (year, yearmonth_num_c, sin/cos month).
#'
#' @param df Data frame with facility (org grouping), event_date, and the climate base variable
#' @param climate_var_base Name of the base climate variable (default "overall_final_score")
#' @param org_var Name of the facility/organization grouping variable (default "facility")
#' @param min_n Minimum observations for window calculation
#' @return Data frame with all windowed climate variables added
create_all_windows <- function(df, climate_var_base = "overall_final_score",
                               org_var = "facility", min_n = 1) {

  # Ensure temporal variables exist
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
  for (win in WINDOW_SPECS$sma$window_size) {
    var_name <- glue::glue("{climate_var_base}_sma_{win}")

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
  for (i in 1:nrow(WINDOW_SPECS$ewma)) {
    lag_d <- WINDOW_SPECS$ewma$lag_days[i]
    hl_d  <- WINDOW_SPECS$ewma$halflife_days[i]
    var_name <- glue::glue("{climate_var_base}_ewmaLAG_{lag_d}d_hl{hl_d}d")

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


# ==============================================================================
# OPERATIONAL DATA JOINING
# ==============================================================================

#' Join NRC operational covariates to event-level data
#'
#' Joins pre-computed quarterly operational features (action matrix, findings,
#' power status) to events based on facility name and the quarter PRIOR to the event.
#'
#' Handles two cases:
#'   1. ops_features already has _between/_within columns (from Python pipeline)
#'   2. ops_features has raw values only — computes between/within here
#'
#' @param df Event-level data with 'facility' and 'event_date'
#' @param ops_features Quarterly operational features
#' @return df with operational covariate columns added
join_nrc_ops <- function(df, ops_features) {

  if (is.null(ops_features) || nrow(ops_features) == 0) {
    message("No operational features provided — creating NA placeholders")
    for (v in NRC_OPS_VARS) {
      df[[paste0(v, "_between")]] <- NA_real_
      df[[paste0(v, "_within")]]  <- NA_real_
    }
    return(df)
  }

  ops <- ops_features

  # Ensure quarter_start is Date (not datetime with tz offset)
  if ("quarter_start" %in% names(ops)) {
    ops$quarter_start <- as.Date(ops$quarter_start)
  }

  # Check if between/within already computed
  has_decomposed <- any(grepl("_between$", names(ops)))

  if (!has_decomposed) {
    # Compute between/within decomposition on available NRC_OPS_VARS
    available_vars <- intersect(NRC_OPS_VARS, names(ops))
    if (length(available_vars) > 0) {
      message(sprintf("  Computing between/within decomposition for: %s",
                      paste(available_vars, collapse = ", ")))

      ops <- ops %>%
        arrange(facility_unit, quarter_start) %>%
        group_by(facility_unit)

      for (v in available_vars) {
        lag_col     <- paste0(".", v, "_lag")
        between_col <- paste0(v, "_between")
        within_col  <- paste0(v, "_within")

        ops <- ops %>%
          mutate(
            !!lag_col := lag(.data[[v]], NRC_OPS_LAG_K),
            !!between_col := slider::slide_dbl(
              .data[[lag_col]],
              ~ mean(.x, na.rm = TRUE),
              .before = NRC_OPS_ROLL_K - 1,
              .complete = FALSE
            ),
            !!within_col := .data[[lag_col]] - .data[[between_col]]
          )
      }

      ops <- ops %>% ungroup()

      # Standardize
      for (v in available_vars) {
        for (suffix in c("_between", "_within")) {
          col <- paste0(v, suffix)
          vals <- ops[[col]]
          mu <- mean(vals, na.rm = TRUE)
          sd_val <- sd(vals, na.rm = TRUE)
          if (is.na(sd_val) || sd_val < 1e-10) sd_val <- 1
          ops[[col]] <- (vals - mu) / sd_val
        }
      }

      # Drop intermediate columns
      ops <- ops %>% select(-starts_with("."))
    }
  }

  # Compute prior quarter start for each event
  df <- df %>%
    mutate(
      .event_year    = lubridate::year(event_date),
      .event_quarter = lubridate::quarter(event_date),
      .prior_quarter = ifelse(.event_quarter == 1L, 4L, .event_quarter - 1L),
      .prior_year    = ifelse(.event_quarter == 1L, .event_year - 1L, .event_year),
      .prior_quarter_start = as.Date(paste0(
        .prior_year, "-",
        c("01","04","07","10")[.prior_quarter], "-01"
      ))
    )

  # Normalize facility names for matching
  df <- df %>%
    mutate(.facility_lower = tolower(trimws(facility)))

  ops_join <- ops %>%
    mutate(.facility_lower = tolower(trimws(facility_unit)))

  # Select columns to join: facility key, quarter key, and all _between/_within cols
  between_within_cols <- names(ops_join)[grepl("_between$|_within$", names(ops_join))]

  # Also include raw ops vars that might be useful (action_matrix_col, findings_count, etc.)
  raw_ops_cols <- intersect(NRC_OPS_VARS, names(ops_join))

  join_cols <- unique(c(".facility_lower", "quarter_start", between_within_cols, raw_ops_cols))
  join_cols <- intersect(join_cols, names(ops_join))

  ops_join <- ops_join %>%
    select(all_of(join_cols)) %>%
    rename(.prior_quarter_start = quarter_start)

  # Deduplicate ops (in case of multiple rows per facility-quarter)
  ops_join <- ops_join %>% distinct(.facility_lower, .prior_quarter_start, .keep_all = TRUE)

  # Join
  df <- df %>%
    left_join(ops_join, by = c(".facility_lower", ".prior_quarter_start"))

  # Report join rate
  ops_check_col <- if (length(between_within_cols) > 0) between_within_cols[1] else raw_ops_cols[1]
  if (!is.null(ops_check_col) && ops_check_col %in% names(df)) {
    n_matched <- sum(!is.na(df[[ops_check_col]]))
    message(sprintf("  Ops join: %d/%d events matched (%.1f%%)",
                    n_matched, nrow(df), 100 * n_matched / nrow(df)))
  }

  # Clean temp columns
  df <- df %>% select(-starts_with("."))

  # Ensure all expected between/within columns exist (NA if not)
  for (v in NRC_OPS_VARS) {
    if (!paste0(v, "_between") %in% names(df)) df[[paste0(v, "_between")]] <- NA_real_
    if (!paste0(v, "_within") %in% names(df))  df[[paste0(v, "_within")]]  <- NA_real_
  }

  return(df)
}


# ==============================================================================
# UNIFIED DATA PREPARATION
# ==============================================================================

#' Prepare data for a single NRC configuration
#'
#' Loads the config parquet (climate scores), joins to event metadata,
#' joins operational covariates, creates windowed climate variables,
#' and prepares outcome variables.
#'
#' @param parquet_path Path to the config's climate scores parquet
#' @param config_id Configuration identifier
#' @param nrc_events Event-level data frame with event_num, facility, event_date,
#'   scram_code, emerg_class, etc. (raw from nrc_data_pipeline)
#' @param ops_features Quarterly operational features (or NULL)
#' @param min_reports Minimum events per facility to include
#' @param min_n Minimum observations for window calculation
#' @return Data frame ready for modeling, with all climate + ops + outcome columns
prepare_nrc_config_data <- function(parquet_path, config_id, nrc_events,
                                    ops_features = NULL,
                                    min_reports = 20, min_n = 1) {

  # Load climate scores for this configuration
  df <- arrow::read_parquet(parquet_path) %>%
    select(
      ends_with('_id'),
      starts_with('overall_'),
      ends_with('_domain_score')
    )

  # Harmonize ID column: config uses report_id, events use event_num
  if ("report_id" %in% names(df)) {
    df <- df %>% mutate(report_id = as.character(report_id))
  }

  # Harmonize events ID column
  events <- nrc_events
  if ("event_num" %in% names(events) && !"report_id" %in% names(events)) {
    events <- events %>% mutate(report_id = as.character(event_num))
  }

  # Join config scores to event metadata
  df <- df %>%
    left_join(events, by = "report_id") %>%
    filter(!is.na(event_date))

  # Filter to facilities with enough events
  df <- df %>%
    group_by(facility) %>%
    filter(n() >= min_reports) %>%
    ungroup()

  if (nrow(df) == 0) {
    warning(sprintf("Config %s: No data after filtering (min_reports=%d)", config_id, min_reports))
    return(NULL)
  }

  # Sort and add event sequence number
  df <- df %>%
    arrange(facility, event_date) %>%
    group_by(facility) %>%
    mutate(t = row_number()) %>%
    ungroup()

  df$config_id <- config_id

  # Create windowed climate variables
  df <- create_all_windows(df, climate_var_base = "overall_final_score",
                           org_var = "facility", min_n = min_n)

  # Join operational features
  df <- join_nrc_ops(df, ops_features)

  # Prepare outcome variables from raw fields
  # --- Scram severity ---
  if ("scram_code" %in% names(df)) {
    # Encode: N/A/empty -> 0 (no scram), M -> 1 (manual), A -> 2 (automatic)
    df <- df %>%
      mutate(
        .scram_raw = tolower(trimws(as.character(scram_code))),
        scram_ord = case_when(
          .scram_raw %in% c("a", "auto", "automatic", "a/m") ~ 2L,
          .scram_raw %in% c("m", "manual")                   ~ 1L,
          TRUE                                                ~ 0L
        ),
        scram_binary = as.integer(scram_ord > 0),
        scram_ord_factor = factor(scram_ord, ordered = TRUE)
      ) %>%
      select(-.scram_raw)
  } else if ("scram_ord" %in% names(df)) {
    # Already encoded (from Python pipeline)
    df <- df %>%
      mutate(
        scram_binary = as.integer(scram_ord > 0),
        scram_ord_factor = factor(scram_ord, ordered = TRUE)
      )
  }

  # --- Emergency classification ---
  if ("emerg_class" %in% names(df) && !"emerg_class_ord" %in% names(df)) {
    df <- df %>%
      mutate(
        .ec_raw = tolower(trimws(as.character(emerg_class))),
        emerg_class_ord = case_when(
          .ec_raw %in% c("ge", "general emergency")      ~ 4L,
          .ec_raw %in% c("sae", "site area emergency")   ~ 3L,
          .ec_raw == "alert"                              ~ 2L,
          .ec_raw %in% c("ue", "nue", "unusual event")   ~ 1L,
          TRUE                                            ~ 0L
        ),
        emerg_class_ord_factor = factor(emerg_class_ord, ordered = TRUE)
      ) %>%
      select(-.ec_raw)
  } else if ("emerg_class_ord" %in% names(df)) {
    df <- df %>%
      mutate(emerg_class_ord_factor = factor(emerg_class_ord, ordered = TRUE))
  }

  # Ensure year is a factor for fixed effects
  df <- df %>% mutate(year = factor(lubridate::year(event_date)))

  message(sprintf("  Config %s: %d events, %d facilities, %d climate vars",
                  config_id, nrow(df), n_distinct(df$facility),
                  length(get_climate_vars(df))))

  return(df)
}
