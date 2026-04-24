# =============================================================================
# validation_protocol.R — Declarative Analysis Configuration
# =============================================================================
#
# Separates the "what to test" (protocol) from the "how to test it" (pipeline).
# Each industry analysis is defined by a protocol that specifies:
#   - Which feature layers to test
#   - What model hierarchy to fit (incremental layer comparisons)
#   - CV strategy and credibility gates
#   - Industry-specific data prep and model fitting functions
#
# The multiverse and CV runners execute protocols without knowing domain specifics.
#
# This extends the existing industry_config() pattern from generate_mv_report.R
# to include feature layers and model hierarchy.
#
# Dependencies: common/feature_layers.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
})


#' Define a complete validation protocol for an industry analysis
#'
#' @param industry Short identifier (e.g., "rail", "nrc", "healthcare")
#' @param label Human-readable name for reports (e.g., "U.S. Railroad")
#' @param layer_registry A feature_layer_registry from create_layer_registry()
#' @param model_hierarchy A model_hierarchy from define_model_hierarchy()
#' @param outcomes Character vector of outcome names to analyze
#' @param outcome_configs Named list of outcome specifications (var, family, model_fn, etc.)
#' @param ops_vars Character vector of operational covariate names
#' @param org_var Name of the organization grouping variable (e.g., "org_id", "facility")
#' @param cv List of CV parameters:
#'   - strategies: character vector (e.g., c("group_kfold", "timeseries"))
#'   - require_both: logical
#'   - baseline_strategy: character
#'   - K: integer (folds for group_kfold)
#'   - n_splits: integer (splits for timeseries)
#'   - test_duration_months: integer
#'   - gap_months: integer
#'   - cv_outcomes: character vector of outcomes to cross-validate (may be subset)
#' @param credibility_gate List of credibility filter settings:
#'   - metric: character (e.g., "delta_brier")
#'   - threshold: numeric
#'   - filter_level: character (e.g., "config_window")
#' @param data_prep_fn Function(parquet_path, config_id, events_df, ops_features, ...) -> df
#'   Industry-specific data preparation function.
#' @param fit_fn Function(df, formula, outcome_config, ...) -> model result tibble
#'   Industry-specific model fitting function, or named list of functions
#'   keyed by outcome name.
#' @param window_specs WINDOW_SPECS list (sma + ewma tibbles)
#' @param min_reports Minimum events per org to include
#' @param exclude_outcomes Character vector of outcomes to exclude from reports
#' @param config_dir_structure Character: how configs are organized on disk.
#'   "subdir" (default) = cfg_dir/_cfg/{id}/results.parquet
#'   "flat" = cfg_dir/{id}.parquet
#' @return A validation_protocol object
define_validation_protocol <- function(
    industry,
    label,
    layer_registry,
    model_hierarchy,
    outcomes,
    outcome_configs,
    ops_vars,
    org_var = "org_id",
    source_files = character(0),
    cv = list(
      strategies = c("group_kfold", "timeseries"),
      require_both = TRUE,
      baseline_strategy = "best_non_climate",
      K = 5L,
      n_splits = 5L,
      test_duration_months = 24L,
      gap_months = 6L,
      cv_outcomes = NULL  # NULL = same as outcomes
    ),
    credibility_gate = list(
      metric = "delta_brier",
      threshold = 0,
      filter_level = "config_window"
    ),
    data_prep_fn,
    fit_fn,
    window_specs,
    min_reports = 20L,
    exclude_outcomes = character(0),
    config_dir_structure = "subdir"
) {

  # Validate
  stopifnot(
    is.character(industry), length(industry) == 1,
    is.character(label), length(label) == 1,
    inherits(layer_registry, "feature_layer_registry"),
    inherits(model_hierarchy, "model_hierarchy"),
    is.character(outcomes), length(outcomes) >= 1,
    is.list(outcome_configs),
    is.character(ops_vars),
    is.function(data_prep_fn)
  )

  # Default cv_outcomes to all outcomes
  if (is.null(cv$cv_outcomes)) cv$cv_outcomes <- outcomes

  structure(
    list(
      industry = industry,
      label = label,
      layer_registry = layer_registry,
      model_hierarchy = model_hierarchy,
      outcomes = outcomes,
      outcome_configs = outcome_configs,
      ops_vars = ops_vars,
      org_var = org_var,
      cv = cv,
      credibility_gate = credibility_gate,
      data_prep_fn = data_prep_fn,
      fit_fn = fit_fn,
      window_specs = window_specs,
      min_reports = min_reports,
      exclude_outcomes = exclude_outcomes,
      config_dir_structure = config_dir_structure,
      source_files = source_files
    ),
    class = "validation_protocol"
  )
}


print.validation_protocol <- function(x, ...) {
  cat(glue("Validation Protocol: {x$label} ({x$industry})"), "\n")
  cat(glue("  Outcomes: {paste(x$outcomes, collapse=', ')}"), "\n")
  cat(glue("  Ops vars: {paste(x$ops_vars, collapse=', ')}"), "\n")
  cat(glue("  Org var:  {x$org_var}"), "\n")
  cat("\n")
  print(x$layer_registry)
  cat("\n")
  print(x$model_hierarchy)
  cat("\n")
  cat(glue("  CV strategies: {paste(x$cv$strategies, collapse=', ')}"), "\n")
  cat(glue("  CV outcomes:   {paste(x$cv$cv_outcomes, collapse=', ')}"), "\n")
  cat(glue("  Credibility:   {x$credibility_gate$metric} < {x$credibility_gate$threshold} ",
           "({x$credibility_gate$filter_level})"), "\n")
  invisible(x)
}


# =============================================================================
# CONVENIENCE FUNCTIONS FOR COMMON PROTOCOLS
# =============================================================================

#' Discover config IDs from a config directory
#'
#' Supports both "subdir" (cfg_dir/_cfg/{id}/results.parquet) and
#' "flat" (cfg_dir/{id}.parquet) layouts.
#'
#' @param cfg_dir Base config directory
#' @param structure "subdir" or "flat"
#' @return Character vector of config IDs
discover_config_ids <- function(cfg_dir, structure = "subdir") {
  if (structure == "subdir") {
    cfg_path <- file.path(cfg_dir, "_cfg")
    if (!dir.exists(cfg_path)) {
      stop(glue("Config directory not found: {cfg_path}"))
    }
    ids <- list.dirs(cfg_path, full.names = FALSE, recursive = FALSE)
    ids <- ids[ids != "" & !startsWith(ids, ".")]
  } else {
    files <- list.files(cfg_dir, pattern = "\\.parquet$", full.names = FALSE)
    ids <- tools::file_path_sans_ext(files)
  }

  if (length(ids) == 0) {
    stop(glue("No configs found in {cfg_dir} (structure={structure})"))
  }

  ids
}


#' Get the parquet path for a given config ID
#'
#' @param cfg_dir Base config directory
#' @param config_id Config ID string
#' @param structure "subdir" or "flat"
#' @return File path
get_config_parquet_path <- function(cfg_dir, config_id, structure = "subdir") {
  if (structure == "subdir") {
    file.path(cfg_dir, "_cfg", config_id, "results.parquet")
  } else {
    file.path(cfg_dir, paste0(config_id, ".parquet"))
  }
}


# =============================================================================
# PROTOCOL TEMPLATES FOR EXISTING INDUSTRIES
# =============================================================================

#' Create the rail validation protocol
#'
#' Template that sets up the standard rail multiverse analysis.
#' The caller provides data paths and optionally overrides defaults.
#'
#' @param temporal_dir Path to temporal features directory (NULL to skip)
#' @param ews_detrend_methods EWS detrend methods for config grid
#' @param ews_window_sizes EWS window sizes for config grid
#' @param window_specs Override default WINDOW_SPECS
#' @return A validation_protocol object (caller must still set data_prep_fn and fit_fn)
rail_protocol_template <- function(
    temporal_dir = NULL,
    ews_detrend_methods = c("linear", "none"),
    ews_window_sizes = c(20L, 50L, 100L),
    window_specs = NULL
) {

  # Build layer registry
  temporal_layer <- create_temporal_layer(org_var = "facility")
  ews_layer <- create_ews_layer(
    detrend_methods = ews_detrend_methods,
    window_sizes = ews_window_sizes,
    composite_col = "overall_final_score",
    org_var = "org_id"
  )

  registry <- create_layer_registry(
    temporal = temporal_layer,
    ews = ews_layer
  )

  # Model hierarchy: climate paper + future temporal/EWS papers
  hierarchy <- define_model_hierarchy(
    M0 = "baseline",
    M1 = c("baseline", "climate"),
    M2 = c("baseline", "climate", "temporal"),
    M3 = c("baseline", "climate", "ews"),
    M4 = c("baseline", "climate", "temporal", "ews")
  )

  list(
    layer_registry = registry,
    model_hierarchy = hierarchy,
    outcomes = c("injuries", "fatalities", "costs"),
    ops_vars = c("train_miles", "passenger_miles", "staff_hours"),
    org_var = "org_id",
    cv_outcomes = c("injuries", "fatalities"),
    exclude_outcomes = c("costs"),
    window_specs = window_specs
  )
}


#' Create the NRC validation protocol template
#'
#' @param temporal_dir Path to temporal features directory (NULL to skip)
#' @param ews_detrend_methods EWS detrend methods for config grid
#' @param ews_window_sizes EWS window sizes for config grid
#' @param window_specs Override default WINDOW_SPECS
#' @return List of protocol components (caller wraps with define_validation_protocol)
nrc_protocol_template <- function(
    temporal_dir = NULL,
    ews_detrend_methods = c("linear", "none"),
    ews_window_sizes = c(20L, 50L, 100L),
    window_specs = NULL
) {

  temporal_layer <- create_temporal_layer(org_var = "facility")
  ews_layer <- create_ews_layer(
    detrend_methods = ews_detrend_methods,
    window_sizes = ews_window_sizes,
    composite_col = "overall_final_score",
    org_var = "facility"
  )

  registry <- create_layer_registry(
    temporal = temporal_layer,
    ews = ews_layer
  )

  hierarchy <- define_model_hierarchy(
    M0 = "baseline",
    M1 = c("baseline", "climate"),
    M2 = c("baseline", "climate", "temporal"),
    M3 = c("baseline", "climate", "ews"),
    M4 = c("baseline", "climate", "temporal", "ews")
  )

  list(
    layer_registry = registry,
    model_hierarchy = hierarchy,
    outcomes = c("pct_power_loss", "emerg_binary", "ordinal_scram"),
    ops_vars = c("capacity_factor", "power_std"),
    org_var = "facility",
    cv_outcomes = c("emerg_binary"),
    exclude_outcomes = character(0),
    window_specs = window_specs
  )
}
