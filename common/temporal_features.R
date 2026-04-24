# =============================================================================
# common/temporal_features.R — R-Native Temporal Feature Computation
# =============================================================================
#
# Ports the core temporal features (burstiness, memory coefficient, IED
# statistics) from Python temporal_features.py to R, with two windowing modes:
#
#   MATCHED:  Temporal features computed over the same window as each climate
#             variable (SMA count-based, EWMA time-based). Ensures the temporal
#             construct aligns with the climate construct's lookback horizon.
#
#   FIXED:    Temporal features computed at canonical window sizes independent
#             of the climate windows. Crossed with climate windows in the
#             multiverse, giving (climate_window × temporal_window) configs.
#
# Both modes produce per-event features anchored to the trailing edge (same
# event alignment as the climate SMA/EWMA windows).
#
# Features computed per window:
#   tf_burstiness     : B = (CV - 1) / (CV + 1) [Goh & Barabási 2008]
#                       or finite-size corrected A_n [Kim & Jo 2016]
#   tf_burstiness_fs  : Finite-size corrected (Kim & Jo periodic) — always computed
#   tf_memory         : Lag-1 Pearson correlation of consecutive IEDs
#   tf_mean_ied       : Mean inter-event duration (in days)
#   tf_sd_ied         : SD of inter-event durations
#   tf_cv_ied         : Coefficient of variation of IEDs
#   tf_n_intervals    : Number of intervals in the window (diagnostic)
#
# References:
#   Goh & Barabási (2008) EPL 81, 48002
#   Kim & Jo (2016) Phys Rev E 94, 032311
#
# Dependencies: dplyr, slider, glue
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
})


# =============================================================================
# 1. CORE COMPUTATION FUNCTIONS
# =============================================================================

#' Compute burstiness from inter-event durations
#'
#' @param ieds Numeric vector of inter-event durations (any unit, must be consistent)
#' @param method "goh" for original B = (CV-1)/(CV+1),
#'   "kimjo_periodic" for finite-size corrected A_n (periodic boundary),
#'   "kimjo_open" for finite-size corrected A_n (open boundary)
#' @param min_intervals Minimum number of intervals for a valid estimate
#' @return Named list with b (burstiness), cv, mean_dt, sd_dt, n
tf_burstiness <- function(ieds, method = "goh", min_intervals = 3L) {
  ieds <- ieds[is.finite(ieds)]
  n <- length(ieds)

  na_result <- list(b = NA_real_, cv = NA_real_, mean_dt = NA_real_,
                    sd_dt = NA_real_, n = n)

  if (n < min_intervals) return(na_result)

  mean_dt <- mean(ieds)
  sd_dt <- sd(ieds)  # R's sd uses ddof=1; consistent with sample estimate

  if (is.na(mean_dt) || mean_dt <= 0) return(na_result)

  cv <- sd_dt / mean_dt

  b <- switch(method,
    "goh" = {
      (cv - 1) / (cv + 1)
    },
    "kimjo_periodic" = {
      if (n < 2) {
        NA_real_
      } else {
        num <- sqrt(n + 1) * cv - sqrt(n - 1)
        den <- (sqrt(n + 1) - 2) * cv + sqrt(n - 1)
        if (abs(den) < .Machine$double.eps) NA_real_ else num / den
      }
    },
    "kimjo_open" = {
      if (n < 1) {
        NA_real_
      } else {
        num <- sqrt(n + 2) * cv - sqrt(n)
        den <- (sqrt(n + 2) - 2) * cv + sqrt(n)
        if (abs(den) < .Machine$double.eps) NA_real_ else num / den
      }
    },
    # default
    NA_real_
  )

  list(b = b, cv = cv, mean_dt = mean_dt, sd_dt = sd_dt, n = n)
}


#' Compute memory coefficient (lag-k correlation of consecutive IEDs)
#'
#' @param ieds Numeric vector of inter-event durations
#' @param lag Lag for the correlation (default 1)
#' @return Scalar memory coefficient (Pearson r), or NA
tf_memory_coefficient <- function(ieds, lag = 1L) {
  ieds <- ieds[is.finite(ieds)]
  if (length(ieds) < lag + 2) return(NA_real_)

  x1 <- ieds[1:(length(ieds) - lag)]
  x2 <- ieds[(lag + 1):length(ieds)]

  if (sd(x1) == 0 || sd(x2) == 0) return(0.0)

  cor(x1, x2)
}


#' Compute all temporal features for a vector of inter-event durations
#'
#' @param ieds Numeric vector of IEDs (in days)
#' @param burstiness_method Method for burstiness ("goh", "kimjo_periodic", "kimjo_open")
#' @param min_intervals Minimum intervals for valid estimates
#' @return Named list with all tf_* features
compute_temporal_indicators <- function(ieds, burstiness_method = "goh",
                                         min_intervals = 3L) {
  ieds <- ieds[is.finite(ieds)]
  n <- length(ieds)

  if (n < min_intervals) {
    return(list(
      tf_burstiness = NA_real_,
      tf_burstiness_fs = NA_real_,
      tf_memory = NA_real_,
      tf_mean_ied = NA_real_,
      tf_sd_ied = NA_real_,
      tf_cv_ied = NA_real_,
      tf_n_intervals = as.integer(n)
    ))
  }

  # Burstiness (user-selected method)
  burst <- tf_burstiness(ieds, method = burstiness_method, min_intervals = min_intervals)

  # Always compute finite-size corrected version too
  burst_fs <- tf_burstiness(ieds, method = "kimjo_periodic", min_intervals = min_intervals)

  # Memory coefficient
  mem <- tf_memory_coefficient(ieds, lag = 1L)

  list(
    tf_burstiness = burst$b,
    tf_burstiness_fs = burst_fs$b,
    tf_memory = mem,
    tf_mean_ied = burst$mean_dt,
    tf_sd_ied = burst$sd_dt,
    tf_cv_ied = burst$cv,
    tf_n_intervals = as.integer(n)
  )
}


# =============================================================================
# 2. COUNT-BASED ROLLING TEMPORAL FEATURES (matches SMA climate windows)
# =============================================================================

#' Compute rolling temporal features for one organization (count-based)
#'
#' For each event, computes temporal features over the trailing k events
#' (strictly lagged — event i uses events i-k through i-1). This matches
#' the SMA climate window convention.
#'
#' @param dates Sorted Date vector for one org
#' @param eids Character vector of event IDs (same order as dates)
#' @param window_size Number of events in the trailing window (matches SMA k)
#' @param burstiness_method Method for burstiness computation
#' @param min_intervals Minimum intervals for valid window
#' @param unit Time unit for IEDs ("d" = days, "h" = hours, etc.)
#' @return Tibble with eid + tf_* columns (one row per event with valid window)
rolling_temporal_count <- function(dates, eids, window_size,
                                    burstiness_method = "goh",
                                    min_intervals = 3L,
                                    unit = "d") {

  n <- length(dates)
  if (n < window_size + 1) return(tibble())

  unit_divisor <- switch(unit,
    "d" = 86400, "h" = 3600, "m" = 60, "s" = 1,
    86400  # default days
  )

  results <- vector("list", n)

  for (i in (window_size + 1):n) {
    # Trailing window: events (i - window_size) through (i - 1)
    # This is strictly lagged — does NOT include event i itself
    win_start <- i - window_size
    win_end <- i - 1

    win_dates <- dates[win_start:win_end]
    # IEDs: differences between consecutive events in the window
    diffs_sec <- as.numeric(difftime(win_dates[-1], win_dates[-length(win_dates)],
                                      units = "secs"))
    ieds <- diffs_sec / unit_divisor

    inds <- compute_temporal_indicators(ieds, burstiness_method, min_intervals)
    inds$eid <- eids[i]  # anchor to current event (trailing edge)
    results[[i]] <- inds
  }

  bind_rows(compact(results))
}


# =============================================================================
# 3. TIME-BASED ROLLING TEMPORAL FEATURES (matches EWMA climate windows)
# =============================================================================

#' Compute rolling temporal features for one organization (time-based)
#'
#' For each event, computes temporal features over all prior events within
#' a calendar-time lookback window. This matches the EWMA climate window
#' convention (strictly lagged, calendar-time based).
#'
#' @param dates Sorted Date vector for one org
#' @param eids Character vector of event IDs
#' @param lookback_days Number of calendar days to look back (matches EWMA lag_days)
#' @param burstiness_method Method for burstiness computation
#' @param min_intervals Minimum intervals for valid window
#' @param unit Time unit for IEDs
#' @return Tibble with eid + tf_* columns
rolling_temporal_time <- function(dates, eids, lookback_days,
                                   burstiness_method = "goh",
                                   min_intervals = 3L,
                                   unit = "d") {

  n <- length(dates)
  if (n < 3) return(tibble())

  unit_divisor <- switch(unit,
    "d" = 86400, "h" = 3600, "m" = 60, "s" = 1,
    86400
  )

  results <- vector("list", n)

  for (i in 2:n) {
    # All prior events within the lookback window
    lookback_start <- dates[i] - lookback_days
    prior_mask <- dates[1:(i - 1)] >= lookback_start
    prior_idx <- which(prior_mask)

    if (length(prior_idx) < min_intervals + 1) next  # need min_intervals+1 events for min_intervals IEDs

    win_dates <- dates[prior_idx]
    diffs_sec <- as.numeric(difftime(win_dates[-1], win_dates[-length(win_dates)],
                                      units = "secs"))
    ieds <- diffs_sec / unit_divisor

    inds <- compute_temporal_indicators(ieds, burstiness_method, min_intervals)
    inds$eid <- eids[i]
    results[[i]] <- inds
  }

  bind_rows(compact(results))
}


# =============================================================================
# 4. DATASET-LEVEL WRAPPERS
# =============================================================================

#' Compute temporal features for all organizations in a dataset (count-based)
#'
#' @param df Data frame with eid, event_date, and org grouping variable
#' @param org_var Name of the org grouping column
#' @param window_size Number of trailing events
#' @param burstiness_method Burstiness method
#' @param min_intervals Minimum intervals per window
#' @param min_events_per_org Minimum total events to attempt computation
#' @param unit Time unit for IEDs
#' @return Tibble with eid + tf_* columns for all orgs
compute_temporal_count_dataset <- function(df, org_var = "org_id",
                                            window_size = 10L,
                                            burstiness_method = "goh",
                                            min_intervals = 3L,
                                            min_events_per_org = NULL,
                                            unit = "d") {
  if (is.null(min_events_per_org)) min_events_per_org <- window_size + 2L

  df %>%
    arrange(.data[[org_var]], event_date) %>%
    group_by(.data[[org_var]]) %>%
    filter(n() >= min_events_per_org) %>%
    group_modify(~ rolling_temporal_count(
      dates = .x$event_date,
      eids = .x$eid,
      window_size = window_size,
      burstiness_method = burstiness_method,
      min_intervals = min_intervals,
      unit = unit
    )) %>%
    ungroup()
}


#' Compute temporal features for all organizations in a dataset (time-based)
#'
#' @param df Data frame with eid, event_date, and org grouping variable
#' @param org_var Name of the org grouping column
#' @param lookback_days Calendar days to look back
#' @param burstiness_method Burstiness method
#' @param min_intervals Minimum intervals per window
#' @param min_events_per_org Minimum total events per org
#' @param unit Time unit for IEDs
#' @return Tibble with eid + tf_* columns for all orgs
compute_temporal_time_dataset <- function(df, org_var = "org_id",
                                           lookback_days = 360L,
                                           burstiness_method = "goh",
                                           min_intervals = 3L,
                                           min_events_per_org = 10L,
                                           unit = "d") {
  df %>%
    arrange(.data[[org_var]], event_date) %>%
    group_by(.data[[org_var]]) %>%
    filter(n() >= min_events_per_org) %>%
    group_modify(~ rolling_temporal_time(
      dates = .x$event_date,
      eids = .x$eid,
      lookback_days = lookback_days,
      burstiness_method = burstiness_method,
      min_intervals = min_intervals,
      unit = unit
    )) %>%
    ungroup()
}


# =============================================================================
# 5. MATCHED WINDOW HELPER
# =============================================================================

#' Compute temporal features matched to a specific climate variable's window
#'
#' Parses the climate variable name to determine window type and parameters,
#' then computes temporal features over the corresponding lookback.
#'
#'   SMA-k      → count-based trailing k events
#'   EWMA-lag_d → time-based trailing lag_days calendar days
#'
#' @param df Prepared data frame (with eid, event_date, org_var)
#' @param climate_var Name of the climate variable to match
#' @param org_var Organization grouping variable
#' @param burstiness_method Burstiness computation method
#' @param min_intervals Minimum intervals for valid estimate
#' @return Tibble with eid + tf_* columns
compute_temporal_matched <- function(df, climate_var, org_var = "org_id",
                                      burstiness_method = "goh",
                                      min_intervals = 3L) {

  # Parse the climate variable name to determine window type
  if (grepl("_sma_(\\d+)$", climate_var)) {
    # Count-based: extract window size
    k <- as.integer(sub(".*_sma_(\\d+)$", "\\1", climate_var))
    compute_temporal_count_dataset(
      df, org_var = org_var, window_size = k,
      burstiness_method = burstiness_method,
      min_intervals = min_intervals
    )
  } else if (grepl("_ewmaLAG_(\\d+)d", climate_var)) {
    # Time-based: extract lookback days
    lag_d <- as.integer(sub(".*_ewmaLAG_(\\d+)d.*", "\\1", climate_var))
    compute_temporal_time_dataset(
      df, org_var = org_var, lookback_days = lag_d,
      burstiness_method = burstiness_method,
      min_intervals = min_intervals
    )
  } else {
    warning(glue("Cannot parse window type from climate var '{climate_var}'. ",
                 "Using default count-based window of 10."))
    compute_temporal_count_dataset(
      df, org_var = org_var, window_size = 10L,
      burstiness_method = burstiness_method,
      min_intervals = min_intervals
    )
  }
}
