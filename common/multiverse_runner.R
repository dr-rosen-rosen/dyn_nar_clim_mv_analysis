# =============================================================================
# common/multiverse_runner.R — Layer-Aware Multiverse Runner
# =============================================================================
#
# Fits ONE model per (climate_config × climate_var × layer_config × outcome).
#
# The model includes: climate variable + layer features (temporal, EWS) +
# operational covariates + temporal controls + random effects.
#
# There is NO model hierarchy in the multiverse. The multiverse asks:
# "How does the climate effect vary across pipeline configurations?"
# Nested model comparisons (climate vs. no-climate, climate+temporal vs.
# climate-only) belong in cross-validation, not here.
#
# When no layers are active (paper 1), this reduces to the original pattern:
# one model per (config × climate_var × outcome).
#
# When layers are active (paper 2), each layer config combination adds
# temporal/EWS features as additional covariates to the same model.
#
# Dependencies:
#   common/feature_layers.R, common/validation_protocol.R
#   dplyr, purrr, furrr, arrow, glue
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(furrr)
  library(arrow)
  library(glue)
})


# =============================================================================
# 1. SINGLE CLIMATE CONFIG ANALYSIS
# =============================================================================

#' Analyze a single climate configuration with all feature layers
#'
#' For one climate config:
#'   - Prepares data via protocol$data_prep_fn
#'   - For each climate_var: computes dependent layers (EWS, matched temporal)
#'   - For each (climate_var × layer_config × outcome): fits ONE model
#'
#' @param protocol A validation_protocol object
#' @param cfg_dir Base config directory
#' @param config_id Climate config ID string
#' @param events_df Event-level data frame
#' @param ops_features Operational features (or NULL)
#' @param independent_layer_data Preloaded independent layer features
#' @return List of result tibbles (one per climate_var × layer_config × outcome)
analyze_single_config <- function(protocol,
                                   cfg_dir,
                                   config_id,
                                   events_df,
                                   ops_features = NULL,
                                   independent_layer_data = list()) {

  registry <- protocol$layer_registry

  # --- Step 1: Prepare data ---
  parquet_path <- get_config_parquet_path(
    cfg_dir, config_id, protocol$config_dir_structure
  )

  prepared <- tryCatch(
    protocol$data_prep_fn(
      parquet_path = parquet_path,
      config_id = config_id,
      events_df = events_df,
      ops_features = ops_features,
      min_reports = protocol$min_reports
    ),
    error = function(e) {
      warning(glue("Data prep failed for config {config_id}: {conditionMessage(e)}"))
      return(NULL)
    }
  )

  if (is.null(prepared) || nrow(prepared) == 0) {
    return(list(tibble(
      config_id = config_id, status = "failed",
      error = "Data prep returned NULL or empty"
    )))
  }

  # --- Step 2: Get climate variables and layer config grid ---
  climate_vars <- get_climate_vars(prepared)

  layer_grid <- get_crossed_config_grid(registry)
  has_layers <- nrow(layer_grid) > 0 && !(".dummy" %in% names(layer_grid))

  if (!has_layers) {
    layer_grid <- tibble(.no_layers = TRUE)
  }

  # --- Step 3: Iterate over climate_vars × layer_configs × outcomes ---
  results <- list()
  result_idx <- 0L

  for (cv in climate_vars) {

    # Compute dependent layers for this climate_var (if any)
    dependent_layer_data <- if (length(registry$dependent) > 0) {
      compute_dependent_layers_for_var(registry, prepared, climate_var = cv)
    } else {
      list()
    }

    all_layer_data <- c(independent_layer_data, dependent_layer_data)

    for (li in seq_len(nrow(layer_grid))) {
      layer_config_row <- layer_grid[li, , drop = FALSE]

      # Get extra terms from layers (temporal + EWS feature names)
      extra_terms <- if (has_layers) {
        get_layer_formula_terms(registry, layer_config_row)
      } else {
        character(0)
      }

      # Join layer features onto prepared data
      df_with_layers <- if (has_layers) {
        join_layer_features(prepared, all_layer_data, layer_config_row, registry)
      } else {
        prepared
      }

      # Filter extra_terms to only those present and non-NA in the data
      if (length(extra_terms) > 0) {
        extra_terms <- extra_terms[
          extra_terms %in% names(df_with_layers) &
            sapply(extra_terms, function(v) sum(!is.na(df_with_layers[[v]])) > 10)
        ]
      }
      for (outcome in protocol$outcomes) {
        result_idx <- result_idx + 1L

        result <- tryCatch({
          # Fit ONE model: climate + layers + ops + controls
          fit_result <- if (is.list(protocol$fit_fn) && outcome %in% names(protocol$fit_fn)) {
            protocol$fit_fn[[outcome]](
              df = df_with_layers, climate_var = cv, config_id = config_id,
              outcome = outcome, extra_terms = extra_terms, protocol = protocol
            )
          } else if (is.function(protocol$fit_fn)) {
            protocol$fit_fn(
              df = df_with_layers, climate_var = cv, config_id = config_id,
              outcome = outcome, extra_terms = extra_terms, protocol = protocol
            )
          } else {
            stop("protocol$fit_fn must be a function or named list of functions")
          }

          # Annotate with metadata
          fit_result <- fit_result %>%
            mutate(
              config_id = config_id,
              climate_var = cv,
              outcome = outcome,
              .before = 1
            )

          # Add layer config IDs
          if (has_layers) {
            for (nm in names(registry$layers)) {
              col <- paste0(nm, "_config_id")
              if (col %in% names(layer_config_row)) {
                fit_result[[col]] <- layer_config_row[[col]]
              }
            }
          }

          fit_result

        }, error = function(e) {
          err_row <- tibble(
            config_id = config_id,
            climate_var = cv,
            outcome = outcome,
            status = "failed",
            error = conditionMessage(e)
          )

          if (has_layers) {
            for (nm in names(registry$layers)) {
              col <- paste0(nm, "_config_id")
              if (col %in% names(layer_config_row)) {
                err_row[[col]] <- layer_config_row[[col]]
              }
            }
          }

          err_row
        })

        results[[result_idx]] <- result
      }
    }
  }

  results
}


# =============================================================================
# 2. DEPENDENT LAYER COMPUTATION
# =============================================================================

#' Compute dependent layers for a specific climate variable
#'
#' Passes climate_var to each dependent layer's compute() function.
#' Required for matched temporal windows.
#'
#' @param registry A feature_layer_registry
#' @param df Prepared data frame
#' @param climate_var The climate variable being tested
#' @return Named list: layer_name -> list(config_id -> tibble)
compute_dependent_layers_for_var <- function(registry, df, climate_var) {
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
        layer$compute(df, cfg_row, climate_var = climate_var),
        error = function(e) {
          warning(glue("Failed to compute layer '{nm}' config '{cfg_id}' ",
                       "for climate_var '{climate_var}': {conditionMessage(e)}"))
          tibble()
        }
      )

      layer_data[[cfg_id]] <- computed
    }

    results[[nm]] <- layer_data
  }

  results
}


# =============================================================================
# 3. PARALLEL MULTIVERSE RUNNER
# =============================================================================

#' Run the full multiverse analysis
#'
#' Discovers climate configs, loads independent layers, then sweeps in parallel.
#' Fits one model per (config × climate_var × layer_config × outcome).
#'
#' @param protocol A validation_protocol object
#' @param cfg_dir Base config directory
#' @param events_df Event-level data frame
#' @param ops_features Operational features (or NULL)
#' @param config_ids Config IDs to analyze (NULL = discover all)
#' @param n_workers Number of parallel workers
#' @param output_path Path for output parquet (NULL = don't save)
#' @param checkpoint_dir Checkpoint directory (NULL = no checkpoints)
#' @return Tibble of all results
run_multiverse <- function(protocol,
                            cfg_dir,
                            events_df,
                            ops_features = NULL,
                            config_ids = NULL,
                            n_workers = 4L,
                            output_path = NULL,
                            checkpoint_dir = NULL) {

  # --- Discover configs ---
  if (is.null(config_ids)) {
    config_ids <- discover_config_ids(cfg_dir, protocol$config_dir_structure)
  }

  layer_grid <- get_crossed_config_grid(protocol$layer_registry)
  n_layer_combos <- max(1, nrow(layer_grid))

  message(glue("\n{'=' |> rep(60) |> paste(collapse='')}\n",
               "Multiverse Analysis: {protocol$label}\n",
               "{'=' |> rep(60) |> paste(collapse='')}\n",
               "Climate configs: {length(config_ids)}\n",
               "Outcomes: {paste(protocol$outcomes, collapse=', ')}\n",
               "Layer config combinations: {n_layer_combos}\n",
               "Estimated models per config: ~{12 * n_layer_combos * length(protocol$outcomes)}"))

  # --- Load independent layers ---
  message("\nLoading config-independent feature layers...")
  independent_data <- load_independent_layers(protocol$layer_registry, events_df)

  # --- Setup checkpoints ---
  if (!is.null(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    existing <- list.files(checkpoint_dir, pattern = "\\.parquet$")
    existing_ids <- tools::file_path_sans_ext(existing)
    remaining_ids <- setdiff(config_ids, existing_ids)
    if (length(existing_ids) > 0) {
      message(glue("  Found {length(existing_ids)} existing checkpoints, ",
                   "{length(remaining_ids)} remaining"))
    }
    config_ids <- remaining_ids
  }

  if (length(config_ids) == 0) {
    message("All configs already checkpointed. Loading results...")
    return(load_checkpointed_results(checkpoint_dir))
  }

  # --- Run in parallel ---
  plan(multisession, workers = n_workers)
  on.exit(plan(sequential), add = TRUE)

  start_time <- Sys.time()

  all_results <- future_map(config_ids, function(cid) {

    # Re-source in worker to make all functions available
    source("common/data_prep.R", local = FALSE)
    source("common/temporal_features.R", local = FALSE)
    source("common/feature_layers.R", local = FALSE)
    source("common/layer_temporal.R", local = FALSE)
    source("common/layer_ews.R", local = FALSE)

    # Re-source industry-specific files
    for (f in protocol$source_files) {
      source(f, local = FALSE)
    }

    cfg_results <- tryCatch(
      analyze_single_config(
        protocol = protocol,
        cfg_dir = cfg_dir,
        config_id = cid,
        events_df = events_df,
        ops_features = ops_features,
        independent_layer_data = independent_data
      ),
      error = function(e) {
        list(tibble(config_id = cid, status = "failed",
                    error = conditionMessage(e)))
      }
    )

    result_df <- bind_rows(cfg_results)

    if (!is.null(checkpoint_dir)) {
      cp_path <- file.path(checkpoint_dir, glue("{cid}.parquet"))
      tryCatch(
        arrow::write_parquet(result_df, cp_path),
        error = function(e) warning(glue("Checkpoint failed for {cid}"))
      )
    }

    result_df
  },
  .options = furrr_options(
    seed = TRUE,
    packages = c("dplyr", "purrr", "tidyr", "glue", "arrow", "slider",
                 "glmmTMB", "lme4", "ordinal", "broom.mixed"),
    globals = c(
      as.list(globalenv()),
      list(
        protocol = protocol,
        cfg_dir = cfg_dir,
        events_df = events_df,
        ops_features = ops_features,
        independent_data = independent_data
      )
    )
  ),
  .progress = TRUE)

  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "mins")

  combined <- bind_rows(all_results)

  message(glue("\nCompleted in {round(as.numeric(elapsed), 1)} minutes"))
  message(glue("Total result rows: {nrow(combined)}"))
  message(glue("Successful: {sum(combined$status == 'success', na.rm=TRUE)} | ",
               "Failed: {sum(combined$status == 'failed', na.rm=TRUE)}"))

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(combined, output_path)
    message(glue("Results saved: {output_path}"))
  }

  combined
}


# =============================================================================
# 4. CHECKPOINT HELPERS
# =============================================================================

#' Load and combine all checkpointed results
load_checkpointed_results <- function(checkpoint_dir) {
  files <- list.files(checkpoint_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) {
    warning(glue("No checkpoint files found in {checkpoint_dir}"))
    return(tibble())
  }
  message(glue("Loading {length(files)} checkpoint files..."))
  map_dfr(files, ~ arrow::read_parquet(.x) %>% as_tibble())
}
