# =============================================================================
# layer_ews.R — Early Warning Signals Feature Layer Definition
# =============================================================================
#
# Computes EWS (Critical Slowing Down) indicators on climate composite scores.
# This is a config-DEPENDENT layer: EWS features must be recomputed for each
# climate configuration because they operate on the composite scores.
#
# Config grid: detrend_method × window_size combinations.
# Each config produces per-event ews_* columns via rolling window computation.
#
# Column prefix: ews_
# Config ID column: ews_config_id
#
# Core indicators computed per window:
#   ews_variance, ews_std, ews_cv, ews_skew, ews_kurtosis,
#   ews_acf1, ews_ar1, ews_return_rate, ews_spectral_ratio
#
# Dependencies: dplyr, purrr, slider, glue
#              + common/feature_layers.R (define_feature_layer)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(slider)
  library(glue)
})


# =============================================================================
# EWS COMPUTATION FUNCTIONS (from ews_and_temporal_integration.R)
# =============================================================================

#' Detrend a numeric vector
#' @param x numeric vector
#' @param method one of "none", "linear", "ma" (moving average)
#' @param ma_window window size for moving average (odd integer)
#' @return residuals after detrending
ews_detrend_series <- function(x, method = "linear", ma_window = NULL) {
  n <- length(x)
  if (n < 3) return(x)

  switch(method,
    "none" = x,
    "linear" = {
      t_idx <- seq_len(n)
      fit <- lm(x ~ t_idx)
      residuals(fit)
    },
    "ma" = {
      if (is.null(ma_window)) ma_window <- max(3, (n %/% 5) %/% 2 * 2 + 1)
      trend <- slider::slide_dbl(x, mean, .before = ma_window %/% 2,
                                  .after = ma_window %/% 2, .complete = FALSE)
      x - trend
    },
    x  # default
  )
}


#' Compute AR(1) coefficient via OLS
ews_ar1_coef <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  y <- x[-1]
  x_lag <- x[-length(x)]
  if (sd(x_lag) == 0) return(NA_real_)
  unname(coef(lm(y ~ x_lag - 1)))
}


#' Compute ACF at lag 1
ews_acf1 <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  a <- acf(x, lag.max = 1, plot = FALSE, na.action = na.pass)
  unname(a$acf[2])
}


#' Compute spectral ratio (low/high power)
ews_spectral_ratio <- function(x, low_frac = 0.1, high_frac = 0.1) {
  x <- x[is.finite(x)]
  if (length(x) < 8) return(NA_real_)
  x <- x - mean(x)
  p <- Mod(fft(x))^2
  n <- length(p) %/% 2
  if (n < 4) return(NA_real_)
  p <- p[2:(n + 1)]
  k_low <- max(1, round(low_frac * n))
  k_high <- max(1, round(high_frac * n))
  low <- sum(p[seq_len(k_low)])
  high <- sum(p[(n - k_high + 1):n])
  if (high <= 0) return(NA_real_)
  low / high
}


#' Compute spectral slope (log-log regression of power vs. frequency)
#'
#' OLS slope of log10(power) vs. log10(frequency) over the positive spectrum.
#' Steeper negative slopes indicate spectral reddening (CSD signature).
#'
#' @param x numeric vector (detrended values within window)
#' @return Scalar spectral slope, or NA
ews_spectral_slope <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 8) return(NA_real_)
  x <- x - mean(x)

  # FFT periodogram
  n_fft <- 2^ceiling(log2(length(x)))  # next power of 2
  X <- fft(x, n_fft)
  P <- (Mod(X)^2)[2:(n_fft %/% 2 + 1)]  # positive freqs, drop DC
  f <- seq_len(length(P)) / n_fft         # normalized frequencies

  # Filter to valid (positive) values for log transform
  mask <- P > 0 & f > 0
  if (sum(mask) < 4) return(NA_real_)

  log_f <- log10(f[mask])
  log_P <- log10(P[mask])

  # OLS regression
  fit <- lm(log_P ~ log_f)
  unname(coef(fit)[2])  # slope
}


#' Compute DFA scaling exponent (Detrended Fluctuation Analysis)
#'
#' Estimates the scaling exponent alpha from the log-log relationship between
#' fluctuation F(s) and scale s. alpha > 0.5 indicates long-range correlations
#' (persistence); alpha = 0.5 is white noise; alpha < 0.5 is anti-persistent.
#'
#' Port of the Python dfa_alpha() from ews.py. Uses linear (order=1) detrending
#' within segments by default.
#'
#' @param x numeric vector (raw or detrended values within window)
#' @param order Polynomial order for local detrending (1 = linear)
#' @param min_scale Minimum segment size
#' @param max_scale Maximum segment size (NULL = auto, n/4 capped at 200)
#' @param num_scales Number of logarithmically spaced scales
#' @return Scalar DFA alpha, or NA if insufficient data
ews_dfa_alpha <- function(x, order = 1L, min_scale = 4L,
                           max_scale = NULL, num_scales = 12L) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 16) return(NA_real_)

  # Profile: cumulative sum of mean-centered series
  x <- x - mean(x)
  Y <- cumsum(x)

  if (is.null(max_scale)) {
    max_scale <- max(min(n %/% 4, 200), min_scale + 1L)
  }

  # Logarithmically spaced scales
  scales <- unique(floor(10^seq(log10(min_scale), log10(max_scale),
                                 length.out = num_scales)))
  scales <- scales[scales >= 2]

  F_vals <- numeric(0)
  s_vals <- numeric(0)

  for (s in scales) {
    s <- as.integer(s)
    nseg <- n %/% s
    if (nseg < 2) next

    rms_segs <- numeric(nseg)
    for (k in 0:(nseg - 1)) {
      seg <- Y[(k * s + 1):(k * s + s)]
      t_idx <- seq_len(s)

      # Local detrending (linear by default)
      if (order == 1) {
        A <- cbind(t_idx, rep(1, s))
      } else {
        A <- outer(t_idx, 0:order, "^")
      }
      beta <- qr.solve(A, seg)
      trend <- A %*% beta
      rms_segs[k + 1] <- sqrt(mean((seg - trend)^2))
    }

    F_vals <- c(F_vals, sqrt(mean(rms_segs^2)))
    s_vals <- c(s_vals, s)
  }

  if (length(F_vals) < 3) return(NA_real_)

  # Log-log regression
  mask <- s_vals > 0 & F_vals > 0
  if (sum(mask) < 3) return(NA_real_)

  log_s <- log10(s_vals[mask])
  log_F <- log10(F_vals[mask])

  fit <- lm(log_F ~ log_s)
  unname(coef(fit)[2])  # alpha = slope
}


#' Compute all EWS indicators for a single window
#' @param x numeric vector (values within window)
#' @param detrend_method detrending method
#' @return named list of indicators
ews_compute_indicators <- function(x, detrend_method = "linear") {
  res <- ews_detrend_series(x, method = detrend_method)
  res_clean <- res[is.finite(res)]
  n <- length(res_clean)

  na_result <- list(
    ews_variance = NA_real_, ews_std = NA_real_, ews_cv = NA_real_,
    ews_skew = NA_real_, ews_kurtosis = NA_real_,
    ews_acf1 = NA_real_, ews_ar1 = NA_real_, ews_return_rate = NA_real_,
    ews_spectral_ratio = NA_real_, ews_spectral_slope = NA_real_,
    ews_dfa_alpha = NA_real_
  )

  if (n < 3) return(na_result)

  mu <- mean(res_clean)
  v <- var(res_clean)
  s <- sd(res_clean)
  cv_val <- if (abs(mu) > 0 & is.finite(s)) s / abs(mu) else NA_real_

  m3 <- mean((res_clean - mu)^3)
  m4 <- mean((res_clean - mu)^4)
  skew_val <- if (s > 0) m3 / s^3 else NA_real_
  kurt_val <- if (s > 0) m4 / s^4 - 3 else NA_real_

  phi <- ews_ar1_coef(res_clean)

  list(
    ews_variance = v,
    ews_std = s,
    ews_cv = cv_val,
    ews_skew = skew_val,
    ews_kurtosis = kurt_val,
    ews_acf1 = ews_acf1(res_clean),
    ews_ar1 = phi,
    ews_return_rate = if (is.finite(phi)) 1 - phi else NA_real_,
    ews_spectral_ratio = ews_spectral_ratio(res_clean),
    ews_spectral_slope = ews_spectral_slope(res_clean),
    ews_dfa_alpha = ews_dfa_alpha(res_clean)
  )
}


#' Compute rolling EWS for a sorted event-level dataframe (one org)
#' @param df tibble with eid, event_date, and value_col, sorted by event_date
#' @param value_col name of the column to compute EWS on
#' @param window_size number of events per window
#' @param step step between windows (default 1 for per-event)
#' @param detrend_method detrending approach
#' @return tibble with eid (trailing edge) and ews_* columns
ews_rolling <- function(df, value_col, window_size, step = 1L,
                        detrend_method = "linear") {
  n <- nrow(df)
  if (n < window_size) return(tibble())

  vals <- df[[value_col]]
  eids <- df[["eid"]]

  starts <- seq(1L, n - window_size + 1L, by = step)

  results <- map_dfr(starts, function(s) {
    e <- s + window_size - 1L
    window_vals <- vals[s:e]
    inds <- ews_compute_indicators(window_vals, detrend_method = detrend_method)
    inds$eid <- eids[e]  # trailing edge anchor
    as_tibble(inds)
  })

  results
}


#' Compute EWS for all organizations in a dataset
#' @param df tibble with eid, event_date, org_id/facility, and composite score columns
#' @param value_col column name for the composite score to analyze
#' @param org_var name of the org grouping variable
#' @param window_size events per window
#' @param step step size
#' @param detrend_method detrending method
#' @param min_events_per_org minimum events to attempt EWS
#' @return tibble with eid and ews_* columns for all orgs
ews_compute_for_dataset <- function(df, value_col = "sc_composite",
                                     org_var = "org_id",
                                     window_size = 50L, step = 1L,
                                     detrend_method = "linear",
                                     min_events_per_org = NULL) {
  if (is.null(min_events_per_org)) min_events_per_org <- window_size + 5L

  df %>%
    arrange(.data[[org_var]], event_date) %>%
    group_by(.data[[org_var]]) %>%
    filter(n() >= min_events_per_org) %>%
    group_modify(~ ews_rolling(
      .x, value_col = value_col,
      window_size = window_size, step = step,
      detrend_method = detrend_method
    )) %>%
    ungroup()
}


# =============================================================================
# LAYER DEFINITION
# =============================================================================

#' Create the EWS feature layer
#'
#' @param detrend_methods Character vector of detrending methods to include
#'   in the config grid (default: c("linear", "none"))
#' @param window_sizes Integer vector of window sizes (event counts) to include
#'   in the config grid (default: c(20, 50, 100))
#' @param composite_col Name of the composite score column in the prepared data.
#'   This is the column that EWS indicators will be computed on.
#' @param org_var Name of the organization grouping variable (default: "org_id")
#' @param formula_vars Character vector of EWS indicator names to include as
#'   fixed effects in the model formula. Defaults to the CSD core set.
#' @return A feature_layer object
create_ews_layer <- function(
    detrend_methods = c("linear", "none"),
    window_sizes = c(20L, 50L, 100L),
    composite_col = "overall_final_score",
    org_var = "org_id",
    formula_vars = c("ews_variance", "ews_ar1", "ews_return_rate")
) {

  # Build config grid
  ews_grid <- expand.grid(
    ews_detrend = detrend_methods,
    ews_window_size = as.integer(window_sizes),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(ews_config_id = glue("ews_{ews_detrend}_w{ews_window_size}"))

  define_feature_layer(
    name = "ews",
    prefix = "ews_",
    depends_on_climate = TRUE,
    description = glue(
      "Early Warning Signals (CSD indicators) computed on climate composites. ",
      "{nrow(ews_grid)} configs: {paste(detrend_methods, collapse='/')} detrend × ",
      "w={paste(window_sizes, collapse='/')}."
    ),

    config_grid = function() {
      ews_grid
    },

    compute = function(df, layer_config, ...) {
      detrend <- layer_config[["ews_detrend"]]
      win_size <- as.integer(layer_config[["ews_window_size"]])

      # Determine which composite column to use
      # Prefer a windowed climate var if one is being tested, otherwise use base
      value_col <- composite_col

      # The caller may pass a specific climate_var via ...
      dots <- list(...)
      if (!is.null(dots$climate_var) && dots$climate_var %in% names(df)) {
        value_col <- dots$climate_var
      }

      if (!value_col %in% names(df)) {
        warning(glue("EWS composite column '{value_col}' not in data"))
        return(tibble(eid = df$eid[0]))
      }

      ews_compute_for_dataset(
        df,
        value_col = value_col,
        org_var = org_var,
        window_size = win_size,
        detrend_method = detrend
      )
    },

    formula_terms = function(layer_config) {
      formula_vars
    }
  )
}
