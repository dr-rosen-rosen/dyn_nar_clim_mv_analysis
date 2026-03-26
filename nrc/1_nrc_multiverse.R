# 1_nrc_multiverse.R
# NRC Nuclear Multiverse Analysis — Traditional
# Outcomes: binary scram (glmer), ordinal scram (clmm), emergency class (clmm)
# With operational covariates: action matrix, findings count
# Mike Rose — Safety Climate Analysis
#
# Sources common/ for config, data prep, window creation.
# Parallel execution across configurations via furrr.

library(tidyverse)
library(lme4)        # glmer for binary
library(ordinal)     # clmm for ordinal
library(broom.mixed) # tidy extraction
library(arrow)
library(furrr)
library(glue)
library(here)
library(slider)

# Source shared modules
common_dir <- file.path(here::here(), "common")
if (!dir.exists(common_dir)) common_dir <- "common"
source(file.path(common_dir, "config.R"))
source(file.path(common_dir, "data_prep.R"))


# ==============================================================================
# MODEL FITTING: BINARY SCRAM (glmer)
# ==============================================================================

#' Fit a single binary scram model (glmer, binomial)
#'
#' @param data Prepared data frame
#' @param climate_var Name of the windowed climate variable
#' @param config_id Configuration ID
#' @return Tibble row with model results
fit_binary_scram <- function(data, climate_var, config_id) {

  outcome_cfg <- get_outcome_config("binary_scram")
  fml_str <- build_formula(MODEL_FORMULAS$binary_scram, climate_var)

  # Filter to complete cases for this climate var + ops
  ops_cols <- c(paste0(NRC_OPS_VARS, "_between"), paste0(NRC_OPS_VARS, "_within"))
  required_cols <- c(climate_var, "scram_binary", "facility", "year", ops_cols)
  existing_cols <- intersect(required_cols, names(data))

  model_data <- data %>%
    filter(if_all(all_of(existing_cols), ~ !is.na(.)))

  if (nrow(model_data) < 50 || n_distinct(model_data$facility) < 3) {
    return(tibble(
      config_id   = config_id,
      climate_var = climate_var,
      outcome     = "binary_scram",
      status      = "insufficient_data",
      n_obs       = nrow(model_data),
      n_facilities = n_distinct(model_data$facility)
    ))
  }

  # Drop year levels that would cause separation:
  # - fewer than 10 total events in that year, OR
  # - zero scram events (complete separation), OR
  # - all scram events (complete separation)
  model_data <- model_data %>%
    group_by(year) %>%
    filter(
      n() >= 10,
      sum(scram_binary) > 0,
      sum(scram_binary) < n()
    ) %>%
    ungroup() %>%
    mutate(year = droplevels(year))

  if (nrow(model_data) < 50 || n_distinct(model_data$year) < 2) {
    return(tibble(
      config_id   = config_id,
      climate_var = climate_var,
      outcome     = "binary_scram",
      status      = "insufficient_data_after_year_filter",
      n_obs       = nrow(model_data),
      n_facilities = n_distinct(model_data$facility)
    ))
  }

  # Capture warnings alongside the fit
  warn_messages <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      glmer(
        as.formula(fml_str),
        data = model_data,
        family = binomial,
        control = glmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 2e5)
        )
      ),
      warning = function(w) {
        warn_messages <<- c(warn_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  # If bobyqa failed or had convergence issues, retry with nloptwrap
  if (is.null(fit) || (!is.null(fit@optinfo$conv$lme4$messages) && length(warn_messages) > 0)) {
    warn_messages_retry <- character(0)
    fit_retry <- tryCatch(
      withCallingHandlers(
        glmer(
          as.formula(fml_str),
          data = model_data,
          family = binomial,
          control = glmerControl(
            optimizer = "nloptwrap",
            optCtrl = list(maxeval = 2e5)
          )
        ),
        warning = function(w) {
          warn_messages_retry <<- c(warn_messages_retry, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )

    # Use retry if it converged better
    if (!is.null(fit_retry)) {
      retry_converged <- is.null(fit_retry@optinfo$conv$lme4$messages)
      orig_converged  <- !is.null(fit) && is.null(fit@optinfo$conv$lme4$messages)
      if (retry_converged || is.null(fit)) {
        fit <- fit_retry
        warn_messages <- warn_messages_retry
      }
    }
  }

  if (is.null(fit)) {
    return(tibble(
      config_id    = config_id,
      climate_var  = climate_var,
      outcome      = "binary_scram",
      status       = "failed",
      error        = paste(warn_messages, collapse = "; "),
      n_obs        = nrow(model_data),
      n_facilities = n_distinct(model_data$facility)
    ))
  }

  tryCatch({
    # Extract climate effect
    tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
    climate_row <- tidy_fit %>% filter(term == climate_var)

    # Extract ops effects
    ops_rows <- tidy_fit %>%
      filter(grepl("_between$|_within$", term))

    # Model diagnostics
    aic_val   <- AIC(fit)
    bic_val   <- BIC(fit)
    converged <- is.null(fit@optinfo$conv$lme4$messages)

    # Singular fit check
    is_singular <- isSingular(fit)

    result <- tibble(
      config_id         = config_id,
      climate_var       = climate_var,
      outcome           = "binary_scram",
      status            = case_when(
        converged & !is_singular ~ "success",
        converged & is_singular  ~ "singular_fit",
        TRUE                     ~ "convergence_warning"
      ),
      warnings          = paste(warn_messages, collapse = "; "),
      n_obs             = nrow(model_data),
      n_facilities      = n_distinct(model_data$facility),
      n_events          = sum(model_data$scram_binary),
      event_rate        = mean(model_data$scram_binary),
      n_year_levels     = n_distinct(model_data$year),
      aic               = aic_val,
      bic               = bic_val,
      is_singular       = is_singular,
      climate_estimate  = climate_row$estimate[1],
      climate_se        = climate_row$std.error[1],
      climate_z         = climate_row$statistic[1],
      climate_p         = climate_row$p.value[1],
      climate_ci_low    = climate_row$conf.low[1],
      climate_ci_high   = climate_row$conf.high[1]
    )

    # Add ops effects as wide columns
    for (i in seq_len(nrow(ops_rows))) {
      term_name <- ops_rows$term[i]
      result[[paste0(term_name, "_estimate")]] <- ops_rows$estimate[i]
      result[[paste0(term_name, "_p")]]        <- ops_rows$p.value[i]
    }

    return(result)

  }, error = function(e) {
    tibble(
      config_id   = config_id,
      climate_var = climate_var,
      outcome     = "binary_scram",
      status      = "extraction_failed",
      error       = as.character(e$message),
      warnings    = paste(warn_messages, collapse = "; "),
      n_obs       = nrow(model_data),
      n_facilities = n_distinct(model_data$facility)
    )
  })
}


# ==============================================================================
# MODEL FITTING: ORDINAL (clmm)
# ==============================================================================

#' Fit a single ordinal model (clmm) for scram severity or emergency class
#'
#' @param data Prepared data frame
#' @param climate_var Name of the windowed climate variable
#' @param config_id Configuration ID
#' @param outcome "ordinal_scram" or "emerg_class"
#' @return Tibble row with model results
fit_ordinal_model <- function(data, climate_var, config_id, outcome = "ordinal_scram") {

  outcome_cfg <- get_outcome_config(outcome)
  outcome_var <- outcome_cfg$var  # e.g., "scram_ord" or "emerg_class_ord"
  factor_var  <- paste0(outcome_var, "_factor")
  fml_str     <- build_formula(MODEL_FORMULAS[[outcome]], climate_var)

  # Complete cases
  ops_cols <- c(paste0(NRC_OPS_VARS, "_between"), paste0(NRC_OPS_VARS, "_within"))
  required_cols <- c(climate_var, factor_var, "facility", "year", ops_cols)
  existing_cols <- intersect(required_cols, names(data))

  model_data <- data %>%
    filter(if_all(all_of(existing_cols), ~ !is.na(.)))

  # For ordinal models, need at least 2 levels present
  n_levels <- n_distinct(model_data[[factor_var]])
  if (nrow(model_data) < 50 || n_distinct(model_data$facility) < 3 || n_levels < 2) {
    return(tibble(
      config_id   = config_id,
      climate_var = climate_var,
      outcome     = outcome,
      status      = "insufficient_data",
      n_obs       = nrow(model_data),
      n_facilities = n_distinct(model_data$facility),
      n_levels    = n_levels
    ))
  }

  # Drop sparse year levels
  model_data <- model_data %>%
    group_by(year) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    mutate(year = droplevels(year))

  # clmm formula: response ~ fixed + (1 | group)
  # clmm requires the random effect in the formula differently
  clmm_fml <- as.formula(paste0(fml_str, " + (1 | facility)"))

  tryCatch({
    fit <- ordinal::clmm(
      clmm_fml,
      data = model_data
    )

    # Extract coefficients
    coef_tbl <- summary(fit)$coefficients
    coef_df  <- as.data.frame(coef_tbl)
    coef_df$term <- rownames(coef_df)

    # Find climate effect
    climate_row <- coef_df %>% filter(term == climate_var)

    # Threshold coefficients
    thresh_df <- coef_df %>% filter(!term %in% c(climate_var, rownames(coef_tbl)[grepl("factor|_between|_within", rownames(coef_tbl))]))

    # AIC
    aic_val <- AIC(fit)

    result <- tibble(
      config_id         = config_id,
      climate_var       = climate_var,
      outcome           = outcome,
      status            = "success",
      n_obs             = nrow(model_data),
      n_facilities      = n_distinct(model_data$facility),
      n_levels          = n_levels,
      aic               = aic_val,
      loglik            = as.numeric(logLik(fit)),
      climate_estimate  = climate_row$Estimate[1],
      climate_se        = climate_row$`Std. Error`[1],
      climate_z         = climate_row$`z value`[1],
      climate_p         = climate_row$`Pr(>|z|)`[1]
    )

    # Add ops effects
    for (v in NRC_OPS_VARS) {
      for (component in c("_between", "_within")) {
        term_name <- paste0(v, component)
        ops_row <- coef_df %>% filter(term == term_name)
        if (nrow(ops_row) == 1) {
          result[[paste0(term_name, "_estimate")]] <- ops_row$Estimate[1]
          result[[paste0(term_name, "_p")]]        <- ops_row$`Pr(>|z|)`[1]
        }
      }
    }

    return(result)

  }, error = function(e) {
    tibble(
      config_id   = config_id,
      climate_var = climate_var,
      outcome     = outcome,
      status      = "failed",
      error       = as.character(e$message),
      n_obs       = nrow(model_data),
      n_facilities = n_distinct(model_data$facility)
    )
  })
}


# ==============================================================================
# ANALYZE SINGLE CONFIGURATION (all outcomes × all windows)
# ==============================================================================

#' Run all models for a single configuration
#'
#' @param parquet_path Path to the config's climate scores parquet
#' @param config_id Configuration ID
#' @param nrc_events Event metadata data frame
#' @param ops_features Quarterly operational features (or NULL)
#' @param outcomes Character vector of outcomes to fit
#' @param min_reports Minimum events per facility
#' @return List of tibble rows (one per model)
analyze_single_config_nrc <- function(parquet_path, config_id, nrc_events,
                                      ops_features = NULL,
                                      outcomes = c("ordinal_scram", "emerg_class"),
                                      min_reports = 20) {

  # Prepare data
  df <- prepare_nrc_config_data(parquet_path, config_id, nrc_events,
                                 ops_features, min_reports = min_reports)

  if (is.null(df) || nrow(df) == 0) {
    return(list(tibble(
      config_id = config_id,
      climate_var = NA_character_,
      outcome = "all",
      status = "no_data_after_prep",
      n_obs = 0L
    )))
  }

  # Get all climate variables
  climate_vars <- get_climate_vars(df)

  if (length(climate_vars) == 0) {
    return(list(tibble(
      config_id = config_id,
      climate_var = NA_character_,
      outcome = "all",
      status = "no_climate_vars",
      n_obs = nrow(df)
    )))
  }

  # Fit all outcome × climate_var combinations
  results_list <- list()

  for (cvar in climate_vars) {
    if ("binary_scram" %in% outcomes) {
      results_list <- c(results_list, list(
        fit_binary_scram(df, cvar, config_id)
      ))
    }
    if ("ordinal_scram" %in% outcomes) {
      results_list <- c(results_list, list(
        fit_ordinal_model(df, cvar, config_id, "ordinal_scram")
      ))
    }
    if ("emerg_class" %in% outcomes) {
      results_list <- c(results_list, list(
        fit_ordinal_model(df, cvar, config_id, "emerg_class")
      ))
    }
  }

  return(results_list)
}


# ==============================================================================
# PARALLEL MULTIVERSE RUNNER
# ==============================================================================

#' Run NRC multiverse analysis across all configurations
#'
#' @param cfg_dir Directory containing config parquet files
#' @param nrc_events Event metadata data frame
#' @param ops_features Quarterly operational features (or NULL)
#' @param outcomes Outcomes to model
#' @param n_workers Number of parallel workers
#' @param output_path Path to save results parquet
#' @param min_reports Minimum events per facility
#' @return Data frame of all results
run_nrc_multiverse <- function(cfg_dir,
                                nrc_events,
                                ops_features = NULL,
                                config_ids = NULL,
                                outcomes = c("ordinal_scram", "emerg_class"),
                                n_workers = 4L,
                                output_path = "results/nrc_multiverse_results.parquet",
                                min_reports = 20) {

  # Discover configurations — same structure as rail: cfg_dir/_cfg/{id}/results.parquet
  if (is.null(config_ids)) {
    cfg_path <- file.path(cfg_dir, "_cfg")
    if (!dir.exists(cfg_path)) {
      stop(sprintf("Configuration directory not found: %s", cfg_path))
    }
    config_ids <- list.dirs(cfg_path, full.names = FALSE, recursive = FALSE)
    config_ids <- config_ids[config_ids != "" & !startsWith(config_ids, ".")]
  }

  n_configs <- length(config_ids)
  n_windows <- nrow(WINDOW_SPECS$sma) + nrow(WINDOW_SPECS$ewma)

  cat(sprintf("NRC Multiverse: %d configurations × %d outcomes × %d window specs\n",
              n_configs, length(outcomes), n_windows))
  cat(sprintf("Total models: ~%d\n", n_configs * length(outcomes) * n_windows))

  # Set up parallel plan
  plan(multisession, workers = n_workers)
  on.exit(plan(sequential), add = TRUE)

  start_time <- Sys.time()

  # Run in parallel across configurations
  all_results <- future_map(config_ids, function(cfg_id) {

    parquet_path <- file.path(cfg_dir, "_cfg", cfg_id, "results.parquet")

    if (!file.exists(parquet_path)) {
      return(list(tibble(
        config_id = cfg_id,
        climate_var = NA_character_,
        outcome = "all",
        status = "failed",
        error = "Parquet file not found"
      )))
    }

    cfg_results <- tryCatch({
      analyze_single_config_nrc(
        parquet_path = parquet_path,
        config_id    = cfg_id,
        nrc_events   = nrc_events,
        ops_features = ops_features,
        outcomes     = outcomes,
        min_reports  = min_reports
      )
    }, error = function(e) {
      list(tibble(
        config_id = cfg_id,
        climate_var = NA_character_,
        outcome = "all",
        status = "failed",
        error = as.character(e$message)
      ))
    })

    return(cfg_results)

  }, .options = furrr_options(
    seed = TRUE,
    packages = c("tidyverse", "lme4", "ordinal", "broom.mixed",
                 "arrow", "glue", "slider"),
    globals = list(
      cfg_dir = cfg_dir,
      nrc_events = nrc_events,
      ops_features = ops_features,
      outcomes = outcomes,
      min_reports = min_reports,
      WINDOW_SPECS = WINDOW_SPECS,
      MODEL_FORMULAS = MODEL_FORMULAS,
      NRC_OPS_VARS = NRC_OPS_VARS,
      NRC_OPS_LAG_K = NRC_OPS_LAG_K,
      NRC_OPS_ROLL_K = NRC_OPS_ROLL_K,
      NRC_OPS_MIN_HIST = NRC_OPS_MIN_HIST,
      OUTCOME_VARS = OUTCOME_VARS,
      analyze_single_config_nrc = analyze_single_config_nrc,
      prepare_nrc_config_data = prepare_nrc_config_data,
      create_all_windows = create_all_windows,
      ewma_time_decay_irregular_lag = ewma_time_decay_irregular_lag,
      join_nrc_ops = join_nrc_ops,
      fit_binary_scram = fit_binary_scram,
      fit_ordinal_model = fit_ordinal_model,
      get_climate_vars = get_climate_vars,
      get_outcome_config = get_outcome_config,
      build_formula = build_formula
    )
  ), .progress = TRUE)

  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "mins")

  # Flatten and combine
  results_df <- all_results %>%
    purrr::flatten() %>%
    bind_rows()

  cat(sprintf("\nMultiverse complete: %.1f minutes\n", as.numeric(elapsed)))
  cat(sprintf("Total results: %d rows\n", nrow(results_df)))

  # Summary by outcome × status
  summary_tbl <- results_df %>%
    count(outcome, status) %>%
    arrange(outcome, desc(n))
  print(summary_tbl)

  # Save
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(results_df, output_path)
  cat(sprintf("Results saved to: %s\n", output_path))

  return(results_df)
}


# ==============================================================================
# MAIN: Example usage
# ==============================================================================

# Uncomment and adjust paths to run:
#
# # Load event data (from nrc_data_pipeline.py output)
# nrc_events <- arrow::read_parquet("data/processed/nrc/nrc_events.parquet") %>%
#   mutate(event_date = as.Date(event_date))
#
# # Load operational features (from nrc_operational_data.py output)
# ops_features <- arrow::read_parquet("data/processed/nrc/ops_quarterly_features.parquet")
#
# # Run multiverse
# results <- run_nrc_multiverse(
#   cfg_dir      = "data/processed/nrc/configs",
#   nrc_events   = nrc_events,
#   ops_features = ops_features,
#   outcomes     = c("binary_scram", "ordinal_scram", "emerg_class"),
#   n_workers    = 8L,
#   output_path  = "results/nrc/nrc_multiverse_results.parquet"
# )
