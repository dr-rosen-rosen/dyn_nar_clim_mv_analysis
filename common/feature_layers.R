# =============================================================================
# feature_layers.R — Feature Layer Registry
# =============================================================================
#
# Core abstraction for integrating multiple feature families (climate, temporal,
# EWS, lexical, etc.) into the multiverse and cross-validation pipelines.
#
# Each feature layer is a list with a standard interface:
#   - name:               Human-readable identifier (e.g., "ews")
#   - prefix:             Column prefix for namespace separation (e.g., "ews_")
#   - depends_on_climate: If TRUE, must recompute per climate config
#   - config_grid:        Function() -> tibble of layer-specific configs
#   - compute:            Function(df, layer_config, ...) -> df with eid + prefixed cols
#   - formula_terms:      Function(layer_config) -> character vector of terms to add to model
#
# Architecture:
#   - Config-INDEPENDENT layers (temporal, lexical): precomputed in Python,
#     loaded once, filtered by layer config_id. Their config grid enumerates
#     the available precomputed files.
#   - Config-DEPENDENT layers (EWS): computed in R on climate composites.
#     Must be recomputed for each climate config. Their config grid enumerates
#     parameter combinations (detrend method × window size, etc.).
#
# The multiverse/CV runners call:
#   1. load_independent_layers() — once per run
#   2. compute_dependent_layers() — once per climate config
#   3. join_layer_features() — once per (climate_config × layer_config) combo
#   4. get_layer_formula_terms() — at model-build time
#
# Dependencies: dplyr, tidyr, purrr, glue, arrow, slider
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(glue)
})


# =============================================================================
# 1. LAYER DEFINITION HELPERS
# =============================================================================

#' Create a validated feature layer specification
#'
#' @param name Human-readable layer name (e.g., "temporal", "ews")
#' @param prefix Column prefix used by this layer (e.g., "tf_", "ews_")
#' @param depends_on_climate Logical: does this layer require climate composite scores?
#' @param config_grid Function() -> tibble with one row per config, must include
#'   a column named {prefix}config_id (e.g., "tf_config_id", "ews_config_id")
#' @param compute Function(df, layer_config, ...) -> tibble with eid + prefixed cols.
#'   For independent layers: df is only used for eid filtering.
#'   For dependent layers: df must include the climate composite scores.
#' @param formula_terms Function(layer_config) -> character vector of variable names
#'   to add as fixed effects in the model formula. These should match columns
#'   produced by compute(). Return character(0) if the layer provides no direct
#'   model terms (e.g., if it only provides features used by downstream layers).
#' @param description Optional human-readable description
#' @return A validated feature_layer list
define_feature_layer <- function(name,
                                  prefix,
                                  depends_on_climate,
                                  config_grid,
                                  compute,
                                  formula_terms = function(layer_config) character(0),
                                  description = "") {
  stopifnot(
    is.character(name), length(name) == 1,
    is.character(prefix), length(prefix) == 1,
    is.logical(depends_on_climate), length(depends_on_climate) == 1,
    is.function(config_grid),
    is.function(compute),
    is.function(formula_terms)
  )

  # Validate config_grid output
  test_grid <- config_grid()
  config_id_col <- paste0(name, "_config_id")
  if (!config_id_col %in% names(test_grid)) {
    stop(glue(
      "config_grid() for layer '{name}' must produce a column named '{config_id_col}'. ",
      "Got columns: {paste(names(test_grid), collapse=', ')}"
    ))
  }

  structure(
    list(
      name = name,
      prefix = prefix,
      depends_on_climate = depends_on_climate,
      config_grid = config_grid,
      compute = compute,
      formula_terms = formula_terms,
      description = description
    ),
    class = "feature_layer"
  )
}

#' Print method for feature layers
print.feature_layer <- function(x, ...) {
  grid <- x$config_grid()
  cat(glue("Feature Layer: {x$name}"), "\n")
  cat(glue("  Prefix: {x$prefix}"), "\n")
  cat(glue("  Climate-dependent: {x$depends_on_climate}"), "\n")
  cat(glue("  Configurations: {nrow(grid)}"), "\n")
  if (nchar(x$description) > 0) cat(glue("  Description: {x$description}"), "\n")
  invisible(x)
}


# =============================================================================
# 2. LAYER REGISTRY
# =============================================================================

#' Create a feature layer registry
#'
#' The registry manages the set of active feature layers for a given analysis.
#' It handles config grid expansion, layer ordering (independent before dependent),
#' and provides lookup methods.
#'
#' @param ... Named feature_layer objects to register
#' @return A feature_layer_registry object
create_layer_registry <- function(...) {
  layers <- list(...)

  # Validate all are feature_layer objects
  for (nm in names(layers)) {
    if (!inherits(layers[[nm]], "feature_layer")) {
      stop(glue("'{nm}' is not a feature_layer object"))
    }
  }

  # Use layer name as list name if not provided
  if (is.null(names(layers)) || any(names(layers) == "")) {
    names(layers) <- map_chr(layers, ~ .x$name)
  }

  structure(
    list(
      layers = layers,
      # Separate into independent and dependent for ordering
      independent = keep(layers, ~ !.x$depends_on_climate),
      dependent = keep(layers, ~ .x$depends_on_climate)
    ),
    class = "feature_layer_registry"
  )
}


#' Print method for layer registry
print.feature_layer_registry <- function(x, ...) {
  cat("Feature Layer Registry\n")
  cat(glue("  Independent layers: {length(x$independent)} ({paste(names(x$independent), collapse=', ')})"), "\n")
  cat(glue("  Dependent layers:   {length(x$dependent)} ({paste(names(x$dependent), collapse=', ')})"), "\n")

  # Total config combinations
  grids <- map(x$layers, ~ nrow(.x$config_grid()))
  total <- prod(unlist(grids))
  cat(glue("  Total layer-config combinations: {total}"), "\n")
  for (nm in names(grids)) {
    cat(glue("    {nm}: {grids[[nm]]} configs"), "\n")
  }
  invisible(x)
}


#' Get the full crossed configuration grid for all layers
#'
#' Returns a tibble where each row is a unique combination of configs
#' across all registered layers. Each layer contributes a {name}_config_id column.
#'
#' @param registry A feature_layer_registry object
#' @return tibble with one row per config combination
get_crossed_config_grid <- function(registry) {
  grids <- map(registry$layers, ~ .x$config_grid())

  if (length(grids) == 0) return(tibble())
  if (length(grids) == 1) return(grids[[1]])

  # Cross all grids
  result <- grids[[1]]
  for (i in 2:length(grids)) {
    result <- tidyr::crossing(result, grids[[i]])
  }
  result
}


# =============================================================================
# 3. LAYER COMPUTATION AND JOINING
# =============================================================================

#' Load / precompute all config-independent layers
#'
#' For independent layers (temporal, lexical), this loads the precomputed
#' features once. The result is a named list of layer_name -> list of
#' (config_id -> tibble with eid + features).
#'
#' @param registry A feature_layer_registry object
#' @param df The base event-level data frame (used for eid filtering)
#' @param ... Additional arguments passed to each layer's compute()
#' @return Named list: layer_name -> list(config_id -> tibble)
load_independent_layers <- function(registry, df, ...) {
  results <- list()

  for (nm in names(registry$independent)) {
    layer <- registry$independent[[nm]]
    grid <- layer$config_grid()
    config_id_col <- paste0(nm, "_config_id")

    layer_data <- list()
    for (i in seq_len(nrow(grid))) {
      cfg_row <- grid[i, , drop = FALSE]
      cfg_id <- cfg_row[[config_id_col]]

      computed <- tryCatch(
        layer$compute(df, cfg_row, ...),
        error = function(e) {
          warning(glue("Failed to compute layer '{nm}' config '{cfg_id}': {conditionMessage(e)}"))
          tibble()
        }
      )

      layer_data[[cfg_id]] <- computed
    }

    results[[nm]] <- layer_data
    message(glue("  Loaded {nm}: {length(layer_data)} configs, ",
                 "{sum(map_int(layer_data, nrow))} total feature rows"))
  }

  results
}


#' Compute config-dependent layer features for a single climate config
#'
#' For dependent layers (EWS), this computes features on the climate composite
#' scores from the current config. Returns a named list of
#' layer_name -> list(config_id -> tibble with eid + features).
#'
#' @param registry A feature_layer_registry object
#' @param df Prepared data frame with climate composite scores
#' @param ... Additional arguments passed to each layer's compute()
#' @return Named list: layer_name -> list(config_id -> tibble)
compute_dependent_layers <- function(registry, df, ...) {
  results <- list()

  for (nm in names(registry$dependent)) {
    layer <- registry$dependent[[nm]]
    grid <- layer$config_grid()
    config_id_col <- paste0(nm, "_config_id")

    layer_data <- list()
    for (i in seq_len(nrow(grid))) {
      cfg_row <- grid[i, , drop = FALSE]
      cfg_id <- cfg_row[[config_id_col]]

      computed <- tryCatch(
        layer$compute(df, cfg_row, ...),
        error = function(e) {
          warning(glue("Failed to compute layer '{nm}' config '{cfg_id}': {conditionMessage(e)}"))
          tibble()
        }
      )

      layer_data[[cfg_id]] <- computed
    }

    results[[nm]] <- layer_data
  }

  results
}


#' Join feature layer columns onto the base data frame
#'
#' Given precomputed layer data (from load_independent_layers / compute_dependent_layers)
#' and a specific config combination, joins the appropriate feature columns onto df.
#'
#' @param df Base data frame (must have an 'eid' column)
#' @param layer_data Named list of layer_name -> list(config_id -> tibble)
#' @param config_row A single-row tibble from get_crossed_config_grid() specifying
#'   which config to use for each layer (via {name}_config_id columns)
#' @param registry The feature_layer_registry (used for prefix lookup)
#' @return df with feature columns joined
join_layer_features <- function(df, layer_data, config_row, registry) {
  for (nm in names(layer_data)) {
    config_id_col <- paste0(nm, "_config_id")
    if (!config_id_col %in% names(config_row)) next

    cfg_id <- config_row[[config_id_col]]
    if (is.null(layer_data[[nm]][[cfg_id]])) {
      warning(glue("No data for layer '{nm}' config '{cfg_id}'"))
      next
    }

    features <- layer_data[[nm]][[cfg_id]]
    if (nrow(features) == 0) next

    prefix <- registry$layers[[nm]]$prefix
    # Select eid + prefixed columns only
    feature_cols <- c("eid", grep(paste0("^", prefix), names(features), value = TRUE))
    features_to_join <- features %>% select(any_of(feature_cols))

    if (ncol(features_to_join) <= 1) next  # only eid, no features

    # Deduplicate on eid (take first occurrence if multiple per eid)
    features_to_join <- features_to_join %>%
      distinct(eid, .keep_all = TRUE)

    df <- df %>%
      left_join(features_to_join, by = "eid")
  }

  df
}


#' Get formula terms contributed by all active layers for a given config
#'
#' @param registry A feature_layer_registry object
#' @param config_row A single-row tibble from get_crossed_config_grid()
#' @return Character vector of variable names to add to the model formula
get_layer_formula_terms <- function(registry, config_row) {
  terms <- character(0)

  for (nm in names(registry$layers)) {
    layer <- registry$layers[[nm]]
    config_id_col <- paste0(nm, "_config_id")

    if (config_id_col %in% names(config_row)) {
      cfg_row <- config_row  # pass full row; layer can extract what it needs
      layer_terms <- tryCatch(
        layer$formula_terms(cfg_row),
        error = function(e) {
          warning(glue("formula_terms() failed for layer '{nm}': {conditionMessage(e)}"))
          character(0)
        }
      )
      terms <- c(terms, layer_terms)
    }
  }

  terms
}


# =============================================================================
# 4. MODEL HIERARCHY SUPPORT
# =============================================================================

#' Define an incremental model comparison hierarchy
#'
#' Specifies the sequence of models to fit for incremental layer comparison.
#' Each level adds one or more layers on top of the previous.
#'
#' @param ... Named character vectors. Each name is the model label (e.g., "M0", "M1").
#'   Each value is a character vector of layer names included at that level.
#'   Use "baseline" for the intercept/ops/temporal-controls-only model.
#' @return A model_hierarchy object
#' @examples
#' h <- define_model_hierarchy(
#'   M0 = "baseline",
#'   M1 = c("baseline", "climate"),
#'   M2 = c("baseline", "climate", "temporal"),
#'   M3 = c("baseline", "climate", "ews")
#' )
define_model_hierarchy <- function(...) {
  levels <- list(...)

  if (is.null(names(levels)) || any(names(levels) == "")) {
    stop("All model hierarchy levels must be named (e.g., M0 = 'baseline')")
  }

  structure(
    list(
      levels = levels,
      names = names(levels)
    ),
    class = "model_hierarchy"
  )
}

print.model_hierarchy <- function(x, ...) {
  cat("Model Comparison Hierarchy\n")
  for (nm in x$names) {
    layers <- x$levels[[nm]]
    cat(glue("  {nm}: {paste(layers, collapse=' + ')}"), "\n")
  }
  invisible(x)
}


#' Get the formula terms for a specific hierarchy level
#'
#' @param hierarchy A model_hierarchy object
#' @param level Name of the level (e.g., "M1")
#' @param registry A feature_layer_registry
#' @param config_row Config row for extracting layer-specific terms
#' @param climate_var The climate variable name (for the "climate" pseudo-layer)
#' @return Character vector of additional terms to add to the base formula
get_hierarchy_terms <- function(hierarchy, level, registry, config_row,
                                 climate_var = NULL) {
  if (!level %in% hierarchy$names) {
    stop(glue("Unknown hierarchy level: '{level}'. Available: {paste(hierarchy$names, collapse=', ')}"))
  }

  layer_names <- hierarchy$levels[[level]]
  terms <- character(0)

  for (ln in layer_names) {
    if (ln == "baseline") {
      # No additional terms — baseline is ops + temporal controls
      next
    } else if (ln == "climate") {
      # Climate variable as a single term
      if (!is.null(climate_var)) {
        terms <- c(terms, climate_var)
      }
    } else if (ln %in% names(registry$layers)) {
      # Get terms from the registered layer
      layer_terms <- registry$layers[[ln]]$formula_terms(config_row)
      terms <- c(terms, layer_terms)
    } else {
      warning(glue("Unknown layer in hierarchy: '{ln}'"))
    }
  }

  terms
}


# =============================================================================
# 5. CONFIG MANIFEST SUPPORT
# =============================================================================

#' Write a configuration manifest alongside a parquet output
#'
#' Records the full provenance of a feature extraction run: model versions,
#' pipeline parameters, software environment, and timestamps.
#'
#' @param manifest_data Named list of key-value pairs to record
#' @param output_path Path where the manifest JSON will be written
#'   (typically alongside the parquet: "config_dir/config_manifest.json")
write_config_manifest <- function(manifest_data, output_path) {
  # Add standard metadata
  manifest_data$manifest_version <- "1.0"
  manifest_data$created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  manifest_data$r_version <- paste0(R.version$major, ".", R.version$minor)

  # Get package versions for key dependencies
  pkgs <- c("glmmTMB", "lme4", "ordinal", "arrow", "dplyr", "slider")
  manifest_data$r_packages <- map_chr(pkgs, function(p) {
    tryCatch(as.character(packageVersion(p)), error = function(e) "not installed")
  }) %>% set_names(pkgs)

  jsonlite::write_json(manifest_data, output_path, pretty = TRUE, auto_unbox = TRUE)
  message(glue("  Manifest written: {output_path}"))
}


#' Read a configuration manifest
#'
#' @param manifest_path Path to the manifest JSON
#' @return Named list
read_config_manifest <- function(manifest_path) {
  if (!file.exists(manifest_path)) {
    warning(glue("Manifest not found: {manifest_path}"))
    return(list())
  }
  jsonlite::read_json(manifest_path, simplifyVector = TRUE)
}
