# =============================================================================
# common/layer_temporal.R — Temporal Feature Layer (Revised)
# =============================================================================
#
# Three windowing strategies:
#
#   MATCHED:  Temporal features computed in R, matched to each climate variable's
#             window type and size. SMA-10 climate → trailing 10-event temporal
#             window. EWMA-360d climate → trailing 360-day temporal window.
#             This mode is climate-config-DEPENDENT (recomputes per climate var).
#
#   FIXED:    Temporal features computed in R at canonical window sizes,
#             independent of climate windows. Produces one set of features per
#             fixed window config. This mode is climate-config-INDEPENDENT.
#
#   LEGACY:   Loads precomputed temporal features from Python parquets (the
#             original approach). Config-INDEPENDENT. Retained for backward
#             compatibility and for cases where the Python pipeline produced
#             features not yet ported to R.
#
# The layer's depends_on_climate flag is set based on the mode:
#   - matched: TRUE (must recompute per climate var)
#   - fixed:   FALSE (precomputed once)
#   - legacy:  FALSE (precomputed in Python)
#
# Column prefix: tf_
# Config ID column: temporal_config_id
#
# Dependencies: common/temporal_features.R, common/feature_layers.R
#               dplyr, purrr, glue, arrow (for legacy mode)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(glue)
})


# =============================================================================
# MATCHED MODE
# =============================================================================

#' Create temporal layer in MATCHED mode
#'
#' Temporal features are computed dynamically, matched to each climate
#' variable's window. The config grid has one entry per burstiness method —
#' the window size/type is derived from the climate variable being tested.
#'
#' Because features depend on which climate variable is active, this layer
#' sets depends_on_climate = TRUE. The runner passes climate_var via ... in
#' the compute() call.
#'
#' @param burstiness_methods Character vector of methods to test in multiverse
#' @param org_var Organization grouping variable
#' @param min_intervals Minimum intervals for valid temporal estimate
#' @param formula_vars Which tf_* columns to include as model terms
#' @return A feature_layer object
create_temporal_layer_matched <- function(
    burstiness_methods = c("goh", "kimjo_periodic"),
    org_var = "org_id",
    min_intervals = 3L,
    formula_vars = c("tf_burstiness", "tf_memory")
) {

  grid <- tibble(
    tf_burstiness_method = burstiness_methods,
    tf_window_mode = "matched",
    temporal_config_id = paste0("tf_matched_", burstiness_methods)
  )

  # Cache: keyed by climate_var + burstiness_method to avoid recomputing
  # the same (org × window) combination when it appears with different
  # EWS configs

  cache <- new.env(parent = emptyenv())

  define_feature_layer(
    name = "temporal",
    prefix = "tf_",
    depends_on_climate = TRUE,

    description = glue(
      "Temporal features (matched windows). ",
      "{nrow(grid)} configs: {paste(burstiness_methods, collapse='/')} methods."
    ),

    config_grid = function() {
      grid
    },

    compute = function(df, layer_config, ...) {
      burst_method <- layer_config[["tf_burstiness_method"]]

      # The runner passes climate_var via ...
      dots <- list(...)
      climate_var <- dots$climate_var

      if (is.null(climate_var)) {
        warning("Matched temporal layer requires climate_var in ... — returning empty")
        return(tibble(eid = character(0)))
      }

      # Check cache
      cache_key <- paste(climate_var, burst_method, sep = "||")
      if (!is.null(cache[[cache_key]])) {
        return(cache[[cache_key]])
      }

      result <- match.fun("compute_temporal_matched")(
        df, climate_var = climate_var, org_var = org_var,
        burstiness_method = burst_method, min_intervals = min_intervals
      )

      cache[[cache_key]] <- result
      result
    },

    formula_terms = function(layer_config) {
      formula_vars
    }
  )
}


# =============================================================================
# FIXED MODE
# =============================================================================

#' Create temporal layer in FIXED mode
#'
#' Temporal features are computed at specified window sizes, independent
#' of climate windows. The config grid crosses window sizes × burstiness methods.
#'
#' For count-based windows, features are computed per trailing k events.
#' For time-based windows, features are computed per trailing d calendar days.
#'
#' @param count_windows Integer vector of count-based window sizes to test
#' @param time_windows Integer vector of time-based lookback days to test (NULL to skip)
#' @param burstiness_methods Character vector of methods to test
#' @param org_var Organization grouping variable
#' @param min_intervals Minimum intervals for valid temporal estimate
#' @param formula_vars Which tf_* columns to include as model terms
#' @return A feature_layer object
create_temporal_layer_fixed <- function(
    count_windows = c(10L, 20L, 50L),
    time_windows = NULL,
    burstiness_methods = c("goh"),
    org_var = "org_id",
    min_intervals = 3L,
    formula_vars = c("tf_burstiness", "tf_memory")
) {

  # Build grid: count-based windows
  grid_parts <- list()

  if (length(count_windows) > 0) {
    grid_count <- expand.grid(
      tf_window_type = "count",
      tf_window_param = as.integer(count_windows),
      tf_burstiness_method = burstiness_methods,
      stringsAsFactors = FALSE
    ) %>%
      as_tibble() %>%
      mutate(
        tf_window_mode = "fixed",
        temporal_config_id = paste0("tf_fixed_count", tf_window_param, "_", tf_burstiness_method)
      )
    grid_parts <- c(grid_parts, list(grid_count))
  }

  # Time-based windows
  if (!is.null(time_windows) && length(time_windows) > 0) {
    grid_time <- expand.grid(
      tf_window_type = "time",
      tf_window_param = as.integer(time_windows),
      tf_burstiness_method = burstiness_methods,
      stringsAsFactors = FALSE
    ) %>%
      as_tibble() %>%
      mutate(
        tf_window_mode = "fixed",
        temporal_config_id = paste0("tf_fixed_count", tf_window_param, "_", tf_burstiness_method)
      )
    grid_parts <- c(grid_parts, list(grid_time))
  }

  grid <- bind_rows(grid_parts)

  cache <- new.env(parent = emptyenv())

  define_feature_layer(
    name = "temporal",
    prefix = "tf_",
    depends_on_climate = FALSE,

    description = glue(
      "Temporal features (fixed windows). ",
      "{nrow(grid)} configs."
    ),

    config_grid = function() {
      grid
    },

    compute = function(df, layer_config, ...) {
      win_type <- layer_config[["tf_window_type"]]
      win_param <- as.integer(layer_config[["tf_window_param"]])
      burst_method <- layer_config[["tf_burstiness_method"]]
      cfg_id <- layer_config[["temporal_config_id"]]

      if (!is.null(cache[[cfg_id]])) {
        return(cache[[cfg_id]])
      }

      result <- if (win_type == "count") {
        match.fun("compute_temporal_count_dataset")(
          df, org_var = org_var, window_size = win_param,
          burstiness_method = burst_method, min_intervals = min_intervals
        )
      } else {
        compute_temporal_time_dataset(
          df, org_var = org_var, lookback_days = win_param,
          burstiness_method = burst_method, min_intervals = min_intervals
        )
      }

      cache[[cfg_id]] <- result
      result
    },

    formula_terms = function(layer_config) {
      formula_vars
    }
  )
}


# =============================================================================
# LEGACY MODE (Python parquets)
# =============================================================================

#' Create temporal layer in LEGACY mode
#'
#' Loads precomputed temporal features from Python parquets. Config grid
#' is determined by scanning the temporal output directory.
#'
#' @param temporal_dir Directory containing temporal_{config_id}.parquet files
#' @param formula_vars Which columns to include as model terms
#' @return A feature_layer object
create_temporal_layer_legacy <- function(
    temporal_dir = NULL,
    formula_vars = c("tf_burstiness", "tf_memory")
) {

  # Pre-scan available configs
  if (!is.null(temporal_dir) && dir.exists(temporal_dir)) {
    files <- list.files(temporal_dir, pattern = "^temporal_t_.*\\.parquet$",
                        full.names = TRUE)
    available_configs <- tibble(
      temporal_config_id = tools::file_path_sans_ext(basename(files)) %>%
        sub("^temporal_", "", .),
      path = files
    ) %>%
      mutate(tf_window_mode = "legacy")
  } else {
    available_configs <- tibble(
      temporal_config_id = character(0),
      path = character(0),
      tf_window_mode = character(0)
    )
  }

  cache <- new.env(parent = emptyenv())

  define_feature_layer(
    name = "temporal",
    prefix = "tf_",
    depends_on_climate = FALSE,

    description = glue(
      "Temporal features (legacy Python parquets). ",
      "{nrow(available_configs)} configs available."
    ),

    config_grid = function() {
      if (nrow(available_configs) == 0) {
        return(tibble(temporal_config_id = "none", tf_window_mode = "legacy"))
      }
      available_configs %>% select(temporal_config_id, tf_window_mode)
    },

    compute = function(df, layer_config, ...) {
      cfg_id <- layer_config[["temporal_config_id"]]

      if (cfg_id == "none") return(tibble(eid = character(0)))
      if (!is.null(cache[[cfg_id]])) return(cache[[cfg_id]])

      path_row <- available_configs %>% filter(temporal_config_id == cfg_id)
      if (nrow(path_row) == 0) {
        warning(glue("Temporal config '{cfg_id}' not found"))
        return(tibble(eid = character(0)))
      }

      result <- arrow::read_parquet(path_row$path[1]) %>% as_tibble()

      if (!"eid" %in% names(result)) {
        if ("report_id" %in% names(result)) result <- result %>% rename(eid = report_id)
        else if ("event_id" %in% names(result)) result <- result %>% rename(eid = event_id)
      }

      cache[[cfg_id]] <- result
      result
    },

    formula_terms = function(layer_config) {
      cfg_id <- layer_config[["temporal_config_id"]]
      if (cfg_id == "none") return(character(0))
      formula_vars
    }
  )
}


# =============================================================================
# CONVENIENCE: COMBINED LAYER (matched + fixed)
# =============================================================================

#' Create a temporal layer that includes both matched and fixed configs
#'
#' This is the recommended approach for the multiverse: it lets the data
#' decide whether matched or fixed windows perform better. The config grid
#' is the union of matched and fixed configs.
#'
#' Note: because matched mode is climate-dependent, the combined layer
#' must be marked depends_on_climate = TRUE. Fixed configs will still
#' be cached efficiently (they're recomputed on first encounter per org_var
#' partition, then served from cache for all subsequent climate vars).
#'
#' @param burstiness_methods Methods for both matched and fixed
#' @param fixed_count_windows Count-based window sizes for fixed mode
#' @param fixed_time_windows Time-based lookback days for fixed mode (NULL to skip)
#' @param org_var Organization grouping variable
#' @param min_intervals Minimum intervals for valid estimate
#' @param formula_vars Model formula terms
#' @return A feature_layer object
create_temporal_layer <- function(
    burstiness_methods = c("goh", "kimjo_periodic"),
    fixed_count_windows = c(10L, 20L, 50L),
    fixed_time_windows = NULL,
    org_var = "org_id",
    min_intervals = 3L,
    formula_vars = c("tf_burstiness", "tf_memory")
) {

  # Build combined grid
  grid_parts <- list()

  # Matched configs (one per burstiness method)
  grid_matched <- tibble(
    tf_window_mode = "matched",
    tf_window_type = NA_character_,
    tf_window_param = NA_integer_,
    tf_burstiness_method = burstiness_methods,
    temporal_config_id = paste0("tf_matched_", burstiness_methods)
  )
  grid_parts <- c(grid_parts, list(grid_matched))

  # Fixed count configs
  if (length(fixed_count_windows) > 0) {
    grid_fc <- expand.grid(
      tf_window_type = "count",
      tf_window_param = as.integer(fixed_count_windows),
      tf_burstiness_method = burstiness_methods,
      stringsAsFactors = FALSE
    ) %>%
      as_tibble() %>%
      mutate(
        tf_window_mode = "fixed",
        temporal_config_id = paste0("tf_fixed_count", tf_window_param, "_", tf_burstiness_method)
      )
    grid_parts <- c(grid_parts, list(grid_fc))
  }

  # Fixed time configs
  if (!is.null(fixed_time_windows) && length(fixed_time_windows) > 0) {
    grid_ft <- expand.grid(
      tf_window_type = "time",
      tf_window_param = as.integer(fixed_time_windows),
      tf_burstiness_method = burstiness_methods,
      stringsAsFactors = FALSE
    ) %>%
      as_tibble() %>%
      mutate(
        tf_window_mode = "fixed",
        temporal_config_id = paste0("tf_fixed_count", tf_window_param, "_", tf_burstiness_method)
      )
    grid_parts <- c(grid_parts, list(grid_ft))
  }

  grid <- bind_rows(grid_parts) %>%
    select(temporal_config_id, tf_window_mode, tf_window_type,
           tf_window_param, tf_burstiness_method)

  cache <- new.env(parent = emptyenv())

  define_feature_layer(
    name = "temporal",
    prefix = "tf_",
    depends_on_climate = TRUE,  # matched mode requires it

    description = glue(
      "Temporal features (matched + fixed windows). ",
      "{sum(grid$tf_window_mode == 'matched')} matched + ",
      "{sum(grid$tf_window_mode == 'fixed')} fixed = ",
      "{nrow(grid)} total configs."
    ),

    config_grid = function() {
      grid
    },

    compute = function(df, layer_config, ...) {
      mode <- layer_config[["tf_window_mode"]]
      burst_method <- layer_config[["tf_burstiness_method"]]
      cfg_id <- layer_config[["temporal_config_id"]]
      dots <- list(...)

      if (mode == "matched") {
        climate_var <- dots$climate_var
        if (is.null(climate_var)) {
          warning("Matched temporal layer requires climate_var — returning empty")
          return(tibble(eid = character(0)))
        }

        cache_key <- paste(climate_var, burst_method, sep = "||")
        if (!is.null(cache[[cache_key]])) return(cache[[cache_key]])

        result <- match.fun("compute_temporal_matched")(
          df, climate_var = climate_var, org_var = org_var,
          burstiness_method = burst_method, min_intervals = min_intervals
        )

        cache[[cache_key]] <- result
        return(result)

      } else {
        # Fixed mode
        if (!is.null(cache[[cfg_id]])) return(cache[[cfg_id]])

        win_type <- layer_config[["tf_window_type"]]
        win_param <- as.integer(layer_config[["tf_window_param"]])

        result <- if (win_type == "count") {
          match.fun("compute_temporal_count_dataset")(
            df, org_var = org_var, window_size = win_param,
            burstiness_method = burst_method, min_intervals = min_intervals
          )
        } else {
          compute_temporal_time_dataset(
            df, org_var = org_var, lookback_days = win_param,
            burstiness_method = burst_method, min_intervals = min_intervals
          )
        }

        cache[[cfg_id]] <- result
        return(result)
      }
    },

    formula_terms = function(layer_config) {
      formula_vars
    }
  )
}
