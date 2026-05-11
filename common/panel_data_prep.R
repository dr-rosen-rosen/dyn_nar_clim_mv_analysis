# =============================================================================
# common/panel_data_prep.R — Shared Panel-Rate Data Preparation
# =============================================================================
#
# Companion to common/data_prep.R. Provides the building blocks for panel-rate
# (org × period) modeling alongside the existing event-level pipeline.
#
# Key idea: the existing windowed climate score is computed AT EVENTS using
# strictly-lagged SMA/EWMA. For panel modeling we need the same logic but
# evaluated AT PERIOD-START GRID DATES, so each panel period gets one climate
# value built from events strictly before that period.
#
# Empty periods are handled by carry-over with a recency cap: if the most
# recent prior event is older than `max_holdover_days`, the climate score is
# dropped (NA) — we don't claim signal from stale narratives.
#
# Functions:
#   make_panel_grid()           : monthly/quarterly grid per org, bounded by
#                                  ops-data coverage
#   ewma_at_grid()              : EWMA with calendar-time decay, evaluated at
#                                  arbitrary grid dates
#   sma_at_grid()               : count-based SMA over last k events strictly
#                                  before each grid date
#   apply_holdover_cap()        : NA out grid dates whose most recent prior
#                                  event is too old
#   build_climate_panel()       : convenience wrapper — given events with
#                                  scores + a grid + a window spec, returns
#                                  one climate value per (org, period)
#
# Dependencies: dplyr, lubridate, tibble
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
})


# =============================================================================
# PANEL GRID CONSTRUCTION
# =============================================================================

#' Build a regular panel grid for each org
#'
#' One row per (org, period). The grid spans from `min_date` to `max_date`
#' (defaults: per-org first/last event date) at monthly or quarterly cadence.
#'
#' Each period is anchored on its FIRST DAY (period_start). For modeling,
#' the climate predictor for period t is evaluated using events strictly
#' before period_start[t] (i.e. events occurring up through period t-1).
#' Outcomes are aggregated over events occurring within period t.
#'
#' @param events_df Data frame with columns matching `org_var` and `date_var`.
#' @param org_var Column name of org/facility ID.
#' @param date_var Column name of event date (Date class).
#' @param period One of "month", "quarter", or "year".
#' @param min_date Optional global floor (Date). Defaults to per-org min.
#' @param max_date Optional global ceiling (Date). Defaults to per-org max.
#' @return Tibble with columns: <org_var>, period_start (Date), period_end (Date).
make_panel_grid <- function(events_df,
                            org_var = "org_id",
                            date_var = "event_date",
                            period = c("month", "quarter", "year"),
                            min_date = NULL,
                            max_date = NULL) {
  period <- match.arg(period)
  unit <- period

  if (!org_var %in% names(events_df)) stop("org_var not in events_df")
  if (!date_var %in% names(events_df)) stop("date_var not in events_df")

  ranges <- events_df %>%
    filter(!is.na(.data[[date_var]])) %>%
    group_by(.data[[org_var]]) %>%
    summarise(.org_min = min(.data[[date_var]]),
              .org_max = max(.data[[date_var]]),
              .groups = "drop")

  if (!is.null(min_date)) ranges$.org_min <- pmax(ranges$.org_min, as.Date(min_date))
  if (!is.null(max_date)) ranges$.org_max <- pmin(ranges$.org_max, as.Date(max_date))

  ranges <- ranges %>% filter(.org_min <= .org_max)

  grid <- ranges %>%
    rowwise() %>%
    mutate(period_start = list(seq(
      from = floor_date(.org_min, unit = unit),
      to   = floor_date(.org_max, unit = unit),
      by   = unit
    ))) %>%
    ungroup() %>%
    tidyr::unnest(period_start) %>%
    mutate(period_end = ceiling_date(period_start, unit = unit) - days(1)) %>%
    select(all_of(org_var), period_start, period_end)

  grid
}


# =============================================================================
# EWMA AT GRID DATES (calendar-time decay)
# =============================================================================

#' EWMA with time-decay, evaluated at arbitrary grid dates
#'
#' Mirrors ewma_time_decay_irregular_lag() from common/data_prep.R, but the
#' EVALUATION POINTS are arbitrary grid dates rather than the event dates
#' themselves. Strict lagging is preserved: at each grid_date, only events
#' with date < grid_date contribute to the weighted mean.
#'
#' @param event_dates Date vector of event timestamps (sorted ascending).
#' @param event_values Numeric vector of event scores (same length).
#' @param grid_dates Date vector of evaluation points (any order).
#' @param window_days Lookback window (days). Events older than this are
#'   excluded from the window.
#' @param half_life_days Exponential decay half-life (days).
#' @param min_n Minimum prior events within the lookback window for a valid
#'   estimate. Default 1.
#' @return Numeric vector of EWMA values, same length as grid_dates.
#'   NA where insufficient prior history.
ewma_at_grid <- function(event_dates, event_values, grid_dates,
                         window_days, half_life_days, min_n = 1L) {
  stopifnot(length(event_dates) == length(event_values))

  out <- rep(NA_real_, length(grid_dates))
  if (length(event_dates) == 0L) return(out)

  # Sort events by date
  ord <- order(event_dates)
  ed  <- event_dates[ord]
  ev  <- event_values[ord]

  lambda <- log(2) / half_life_days

  for (i in seq_along(grid_dates)) {
    g <- grid_dates[i]
    # Strictly prior events within lookback
    in_win <- which(ed < g & ed >= (g - window_days))
    if (length(in_win) < min_n) next

    vals  <- ev[in_win]
    dates <- ed[in_win]
    valid <- !is.na(vals)
    if (sum(valid) < min_n) next
    vals  <- vals[valid]
    dates <- dates[valid]

    time_diffs <- as.numeric(g - dates)  # days, > 0
    w <- exp(-lambda * time_diffs)
    out[i] <- sum(vals * w) / sum(w)
  }
  out
}


# =============================================================================
# SMA AT GRID DATES (count-based)
# =============================================================================

#' Count-based SMA over last k events strictly before each grid date
#'
#' Mirrors the count-based SMA used by create_all_windows(). At each grid
#' date, takes the k most-recent events with date < grid_date and returns
#' their unweighted mean. NA if fewer than `min_n` qualifying events exist.
#'
#' @param event_dates Date vector of event timestamps.
#' @param event_values Numeric vector of event scores.
#' @param grid_dates Date vector of evaluation points.
#' @param k Window size (number of trailing events).
#' @param min_n Minimum events required (default = k).
#' @return Numeric vector, same length as grid_dates.
sma_at_grid <- function(event_dates, event_values, grid_dates,
                        k, min_n = k) {
  stopifnot(length(event_dates) == length(event_values))

  out <- rep(NA_real_, length(grid_dates))
  if (length(event_dates) == 0L) return(out)

  ord <- order(event_dates)
  ed  <- event_dates[ord]
  ev  <- event_values[ord]

  for (i in seq_along(grid_dates)) {
    g <- grid_dates[i]
    prior <- which(ed < g)
    if (length(prior) < min_n) next

    take <- tail(prior, k)
    vals <- ev[take]
    valid <- !is.na(vals)
    if (sum(valid) < min_n) next
    out[i] <- mean(vals[valid])
  }
  out
}


# =============================================================================
# HOLDOVER CAP (recency floor)
# =============================================================================

#' NA-out climate values whose most recent prior event is too stale
#'
#' Even with carry-over via EWMA decay, we may not want a climate value at
#' grid date g if the latest event before g is more than `max_holdover_days`
#' old. This protects against extrapolating signal across long quiet stretches.
#'
#' @param climate_values Numeric vector to be capped.
#' @param event_dates Date vector of all events for the org.
#' @param grid_dates Date vector matching `climate_values`.
#' @param max_holdover_days Cap in days (default 365).
#' @return climate_values with NAs inserted where the most recent prior event
#'   is older than the cap (or no prior event exists).
apply_holdover_cap <- function(climate_values, event_dates, grid_dates,
                               max_holdover_days = 365L) {
  if (length(event_dates) == 0L) return(rep(NA_real_, length(climate_values)))

  ed <- sort(event_dates)
  for (i in seq_along(grid_dates)) {
    if (is.na(climate_values[i])) next
    g <- grid_dates[i]
    prior <- ed[ed < g]
    if (length(prior) == 0L) {
      climate_values[i] <- NA_real_
    } else {
      most_recent <- max(prior)
      if (as.numeric(g - most_recent) > max_holdover_days) {
        climate_values[i] <- NA_real_
      }
    }
  }
  climate_values
}


# =============================================================================
# WRAPPER: BUILD CLIMATE PANEL
# =============================================================================

#' Compute panel climate variables (one row per org × period) from events
#'
#' Given an events frame with per-event climate scores and a panel grid,
#' produce a tibble of climate values per (org, period_start) for each
#' window in `window_specs`. Applies the recency cap at the end.
#'
#' Window names mirror the event-level convention used by create_all_windows()
#' in common/data_prep.R, so the same column names work in both pipelines:
#'   <base>_sma_<k>                       e.g. overall_final_score_sma_10
#'   <base>_ewmaLAG_<lag>d_hl<hl>d        e.g. overall_final_score_ewmaLAG_540d_hl270d
#'
#' @param events_df Data frame with org_var, date_var, and climate_var columns.
#'   One row per event. May include events with NA climate_var (skipped).
#' @param grid Panel grid from make_panel_grid() (rows = org × period_start).
#' @param org_var Column name of org/facility ID. Must exist in both inputs.
#' @param date_var Column name of event date.
#' @param climate_var Column name of the per-event climate score.
#' @param window_specs A list with $sma (tibble: window_size) and
#'   $ewma (tibble: lag_days, halflife_days). Same structure as the
#'   global WINDOW_SPECS used in event-level prep.
#' @param max_holdover_days Recency cap (days). Default 365.
#' @return Tibble: org_var, period_start, period_end, plus one column per window.
build_climate_panel <- function(events_df, grid,
                                org_var = "org_id",
                                date_var = "event_date",
                                climate_var = "overall_final_score",
                                window_specs,
                                max_holdover_days = 365L) {

  required <- c(org_var, date_var, climate_var)
  missing <- setdiff(required, names(events_df))
  if (length(missing) > 0) stop(sprintf("Missing in events_df: %s",
                                        paste(missing, collapse = ", ")))

  # Drop events with NA dates or NA climate (they can't contribute weight).
  ev <- events_df %>%
    select(all_of(c(org_var, date_var, climate_var))) %>%
    rename(.org = !!org_var, .date = !!date_var, .val = !!climate_var) %>%
    filter(!is.na(.date), !is.na(.val)) %>%
    mutate(.date = as.Date(.date))

  g <- grid %>% rename(.org = !!org_var) %>% mutate(period_start = as.Date(period_start))

  # Pre-split events by org for speed.
  ev_split <- split(ev, ev$.org)

  # Build column names from window_specs.
  base <- climate_var
  sma_cols  <- if (!is.null(window_specs$sma)) {
    paste0(base, "_sma_", window_specs$sma$window_size)
  } else character(0)
  ewma_cols <- if (!is.null(window_specs$ewma)) {
    paste0(base, "_ewmaLAG_",
           window_specs$ewma$lag_days, "d_hl",
           window_specs$ewma$halflife_days, "d")
  } else character(0)

  # Compute per-org, per-period climate values.
  result_list <- vector("list", length(unique(g$.org)))
  orgs <- unique(g$.org)

  for (j in seq_along(orgs)) {
    o <- orgs[j]
    g_o <- g %>% filter(.org == o) %>% arrange(period_start)
    ev_o <- ev_split[[as.character(o)]]
    if (is.null(ev_o)) ev_o <- ev[0, ]

    out <- g_o
    grid_dates <- g_o$period_start

    # SMA windows
    if (nrow(window_specs$sma %||% data.frame()) > 0) {
      for (r in seq_len(nrow(window_specs$sma))) {
        k <- window_specs$sma$window_size[r]
        col <- paste0(base, "_sma_", k)
        vals <- sma_at_grid(ev_o$.date, ev_o$.val, grid_dates, k = k, min_n = 1L)
        vals <- apply_holdover_cap(vals, ev_o$.date, grid_dates, max_holdover_days)
        out[[col]] <- vals
      }
    }

    # EWMA windows
    if (nrow(window_specs$ewma %||% data.frame()) > 0) {
      for (r in seq_len(nrow(window_specs$ewma))) {
        wd <- window_specs$ewma$lag_days[r]
        hl <- window_specs$ewma$halflife_days[r]
        col <- paste0(base, "_ewmaLAG_", wd, "d_hl", hl, "d")
        vals <- ewma_at_grid(ev_o$.date, ev_o$.val, grid_dates,
                             window_days = wd, half_life_days = hl, min_n = 1L)
        vals <- apply_holdover_cap(vals, ev_o$.date, grid_dates, max_holdover_days)
        out[[col]] <- vals
      }
    }

    result_list[[j]] <- out
  }

  out_df <- bind_rows(result_list) %>% rename(!!org_var := .org)
  out_df
}

# Tiny null-coalesce helper used above.
`%||%` <- function(a, b) if (is.null(a)) b else a
