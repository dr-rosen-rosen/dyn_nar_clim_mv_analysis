# 2_nrc_cv.R
# NRC Nuclear Cross-Validation Analysis — Binary Scram Only
# Strategies: Group K-Fold (hold out facilities) + Time-Series Expanding Window
# Baselines: intercept-only, seasonal+ops-only, full with climate
# Mike Rose — Safety Climate Analysis
#
# Runs CV for all configurations × all window specs.
# Produces Brier score, AUC, log-loss for each specification.

library(tidyverse)
library(lme4)
library(pROC)
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
source(file.path(common_dir, "formulas.R"))


# ==============================================================================
# CV METRICS
# ==============================================================================

#' Compute binary classification metrics
#'
#' @param y_true Binary outcome vector (0/1)
#' @param p_pred Predicted probability vector
#' @return Named list: brier_score, log_loss, auc_roc, pred_event_rate, obs_event_rate
compute_binary_metrics <- function(y_true, p_pred) {

  # Clamp predictions to avoid log(0)
  p_pred <- pmax(pmin(p_pred, 1 - 1e-15), 1e-15)
  n <- length(y_true)

  brier <- mean((y_true - p_pred)^2)
  logloss <- -mean(y_true * log(p_pred) + (1 - y_true) * log(1 - p_pred))

  auc_val <- tryCatch({
    as.numeric(pROC::auc(pROC::roc(y_true, p_pred, quiet = TRUE)))
  }, error = function(e) NA_real_)

  list(
    brier_score     = brier,
    log_loss        = logloss,
    auc_roc         = auc_val,
    pred_event_rate = mean(p_pred),
    obs_event_rate  = mean(y_true),
    n_events        = sum(y_true),
    n_zeros         = sum(y_true == 0)
  )
}


# ==============================================================================
# CV ENGINE: GROUP K-FOLD (Strategy A)
# ==============================================================================

#' Group K-Fold CV — hold out entire facilities
#'
#' @param data Prepared data frame
#' @param formula_obj Formula object for glmer
#' @param model_label Label for this model variant
#' @param K Number of folds
#' @return List with fold_results and summary
cv_group_kfold <- function(data, formula_obj, model_label = "full",
                            K = 5) {

  facilities <- unique(data$facility)
  n_fac <- length(facilities)

  if (n_fac < K) {
    K <- max(2, n_fac)
  }

  # Random assignment of facilities to folds
  set.seed(42)
  fold_assignment <- tibble(
    facility = facilities,
    fold     = sample(rep(1:K, length.out = n_fac))
  )

  data <- data %>% left_join(fold_assignment, by = "facility")

  fold_results <- vector("list", K)

  for (k in 1:K) {
    train_data <- data %>% filter(fold != k)
    test_data  <- data %>% filter(fold == k)

    if (nrow(test_data) == 0 || sum(test_data$scram_binary) == 0) {
      fold_results[[k]] <- tibble(fold = k, status = "empty_test")
      next
    }

    # Fit on training data
    fit <- tryCatch({
      glmer(formula_obj, data = train_data, family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
    }, error = function(e) NULL)

    if (is.null(fit)) {
      fold_results[[k]] <- tibble(fold = k, status = "fit_failed")
      next
    }

    converged <- is.null(fit@optinfo$conv$lme4$messages)

    # Predict on test data — new facilities, so use re.form = NA (population level)
    p_event <- tryCatch({
      predict(fit, newdata = test_data, type = "response", re.form = NA)
    }, error = function(e) NULL)

    if (is.null(p_event)) {
      fold_results[[k]] <- tibble(fold = k, status = "predict_failed")
      next
    }

    metrics <- compute_binary_metrics(test_data$scram_binary, p_event)

    fold_results[[k]] <- tibble(
      fold            = k,
      status          = ifelse(converged, "success", "convergence_warning"),
      n_train         = nrow(train_data),
      n_test          = nrow(test_data),
      n_train_fac     = n_distinct(train_data$facility),
      n_test_fac      = n_distinct(test_data$facility),
      brier_score     = metrics$brier_score,
      log_loss        = metrics$log_loss,
      auc_roc         = metrics$auc_roc,
      pred_event_rate = metrics$pred_event_rate,
      obs_event_rate  = metrics$obs_event_rate
    )
  }

  fold_table <- bind_rows(fold_results)
  successful <- fold_table %>% filter(status %in% c("success", "convergence_warning"))

  if (nrow(successful) == 0) {
    return(list(fold_results = fold_table, summary = NULL))
  }

  summary_row <- successful %>%
    summarise(
      brier_mean  = mean(brier_score, na.rm = TRUE),
      brier_sd    = sd(brier_score, na.rm = TRUE),
      auc_mean    = mean(auc_roc, na.rm = TRUE),
      logloss_mean = mean(log_loss, na.rm = TRUE),
      n_folds     = n(),
      .groups = "drop"
    ) %>%
    mutate(model_label = model_label, cv_strategy = "group_kfold")

  list(fold_results = fold_table, summary = summary_row)
}


# ==============================================================================
# CV ENGINE: TIME-SERIES EXPANDING WINDOW (Strategy B)
# ==============================================================================

#' Time-Series CV — expanding training window, predict next period
#'
#' Facilities appear in both train and test. Uses conditional predictions
#' (facility random effects) since facilities are known.
#'
#' @param data Prepared data frame with year column
#' @param formula_obj Formula object for glmer
#' @param model_label Label for this model variant
#' @param min_train_years Minimum years of training data before first test
#' @return List with fold_results and summary
cv_timeseries <- function(data, formula_obj, model_label = "full",
                           min_train_years = 5) {

  years_available <- sort(unique(as.integer(as.character(data$year))))

  if (length(years_available) <= min_train_years) {
    return(list(fold_results = tibble(status = "insufficient_years"), summary = NULL))
  }

  test_years <- years_available[(min_train_years + 1):length(years_available)]
  fold_results <- vector("list", length(test_years))

  for (i in seq_along(test_years)) {
    test_yr <- test_years[i]
    train_yrs <- years_available[years_available < test_yr]

    train_data <- data %>% filter(as.integer(as.character(year)) %in% train_yrs)
    test_data  <- data %>% filter(as.integer(as.character(year)) == test_yr)

    if (nrow(test_data) == 0 || sum(test_data$scram_binary) == 0) {
      fold_results[[i]] <- tibble(fold = i, test_year = test_yr, status = "empty_test")
      next
    }

    fit <- tryCatch({
      glmer(formula_obj, data = train_data, family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
    }, error = function(e) NULL)

    if (is.null(fit)) {
      fold_results[[i]] <- tibble(fold = i, test_year = test_yr, status = "fit_failed")
      next
    }

    converged <- is.null(fit@optinfo$conv$lme4$messages)

    # Conditional predictions (facilities are known)
    p_event <- tryCatch({
      predict(fit, newdata = test_data, type = "response",
              allow.new.levels = TRUE)
    }, error = function(e) NULL)

    if (is.null(p_event)) {
      fold_results[[i]] <- tibble(fold = i, test_year = test_yr, status = "predict_failed")
      next
    }

    metrics <- compute_binary_metrics(test_data$scram_binary, p_event)

    fold_results[[i]] <- tibble(
      fold            = i,
      test_year       = test_yr,
      status          = ifelse(converged, "success", "convergence_warning"),
      n_train         = nrow(train_data),
      n_test          = nrow(test_data),
      brier_score     = metrics$brier_score,
      log_loss        = metrics$log_loss,
      auc_roc         = metrics$auc_roc,
      pred_event_rate = metrics$pred_event_rate,
      obs_event_rate  = metrics$obs_event_rate
    )
  }

  fold_table <- bind_rows(fold_results)
  successful <- fold_table %>% filter(status %in% c("success", "convergence_warning"))

  if (nrow(successful) == 0) {
    return(list(fold_results = fold_table, summary = NULL))
  }

  summary_row <- successful %>%
    summarise(
      brier_mean   = mean(brier_score, na.rm = TRUE),
      brier_sd     = sd(brier_score, na.rm = TRUE),
      auc_mean     = mean(auc_roc, na.rm = TRUE),
      logloss_mean = mean(log_loss, na.rm = TRUE),
      n_folds      = n(),
      .groups = "drop"
    ) %>%
    mutate(model_label = model_label, cv_strategy = "timeseries")

  list(fold_results = fold_table, summary = summary_row)
}


# ==============================================================================
# CV WITH BASELINES: Run all model variants for a single climate var
# ==============================================================================

#' Run CV for full model + baselines for one climate variable
#'
#' Models compared:
#'   1. intercept_only: scram ~ 1 + (1|facility)
#'   2. seasonal_ops:   scram ~ year + ops + (1|facility)
#'   3. full:           scram ~ year + ops + climate + (1|facility)
#'
#' @param data Prepared data frame
#' @param climate_var Climate variable name
#' @param K Folds for group k-fold
#' @param min_train_years Min years for time-series CV
#' @return Tibble with one row per model × strategy
cv_with_baselines <- function(data, climate_var, K = 5, min_train_years = 5) {

  fml_full      <- build_binary_formula(climate_var)
  fml_seasonal  <- build_seasonal_formula()
  fml_intercept <- build_intercept_formula()

  models <- list(
    list(fml = fml_intercept, label = "intercept_only"),
    list(fml = fml_seasonal,  label = "seasonal_ops"),
    list(fml = fml_full,      label = "full")
  )

  all_summaries <- list()

  for (m in models) {
    # Strategy A: Group K-Fold
    res_gkf <- tryCatch({
      cv_group_kfold(data, m$fml, model_label = m$label, K = K)
    }, error = function(e) list(summary = NULL))

    if (!is.null(res_gkf$summary)) {
      all_summaries <- c(all_summaries, list(res_gkf$summary))
    }

    # Strategy B: Time-Series
    res_ts <- tryCatch({
      cv_timeseries(data, m$fml, model_label = m$label,
                     min_train_years = min_train_years)
    }, error = function(e) list(summary = NULL))

    if (!is.null(res_ts$summary)) {
      all_summaries <- c(all_summaries, list(res_ts$summary))
    }
  }

  if (length(all_summaries) == 0) return(NULL)

  bind_rows(all_summaries) %>%
    mutate(climate_var = climate_var)
}


# ==============================================================================
# ANALYZE SINGLE CONFIGURATION (all climate vars × CV)
# ==============================================================================

#' Run CV for a single NRC configuration
#'
#' @param parquet_path Path to config parquet
#' @param config_id Configuration ID
#' @param nrc_events Event metadata
#' @param ops_features Operational features
#' @param K Folds for group k-fold
#' @param min_train_years Min years for time-series CV
#' @param min_reports Min events per facility
#' @return Tibble of CV results for this config
cv_single_config_nrc <- function(parquet_path, config_id, nrc_events,
                                  ops_features = NULL,
                                  K = 5, min_train_years = 5,
                                  min_reports = 20) {

  df <- prepare_nrc_config_data(parquet_path, config_id, nrc_events,
                                 ops_features, min_reports = min_reports)

  if (is.null(df) || nrow(df) == 0) {
    return(tibble(config_id = config_id, status = "no_data"))
  }

  climate_vars <- get_climate_vars(df)
  if (length(climate_vars) == 0) {
    return(tibble(config_id = config_id, status = "no_climate_vars"))
  }

  # Run CV for each climate variable
  cv_results <- map(climate_vars, function(cvar) {
    res <- tryCatch({
      cv_with_baselines(df, cvar, K = K, min_train_years = min_train_years)
    }, error = function(e) {
      tibble(climate_var = cvar, model_label = "error",
             cv_strategy = NA, brier_mean = NA, error = as.character(e$message))
    })

    if (!is.null(res)) {
      res$config_id <- config_id
    }
    return(res)
  })

  bind_rows(cv_results)
}


# ==============================================================================
# PARALLEL CV RUNNER WITH CHECKPOINTING
# ==============================================================================

#' Run NRC CV across all configurations
#'
#' @param cfg_dir Directory with config parquet files
#' @param nrc_events Event metadata
#' @param ops_features Operational features
#' @param n_workers Parallel workers
#' @param K Group k-fold K
#' @param min_train_years Time-series min training years
#' @param output_path Output parquet path
#' @param checkpoint_dir Directory for per-config checkpoints
#' @param min_reports Min events per facility
#' @return Data frame of all CV results
run_nrc_cv <- function(cfg_dir, nrc_events, ops_features = NULL,
                        config_ids = NULL,
                        n_workers = 4L, K = 5, min_train_years = 5,
                        output_path = "results/nrc/nrc_cv_results.parquet",
                        checkpoint_dir = "results/nrc/cv_checkpoints",
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

  # Checkpointing — skip already-completed configs
  dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
  completed <- tools::file_path_sans_ext(
    list.files(checkpoint_dir, pattern = "\\.parquet$")
  )
  todo_ids <- config_ids[!config_ids %in% completed]

  cat(sprintf("NRC CV: %d configs total, %d already done, %d remaining\n",
              length(config_ids), length(completed), length(todo_ids)))

  if (length(todo_ids) == 0) {
    cat("All configs complete — loading from checkpoints\n")
    return(.load_cv_checkpoints(checkpoint_dir))
  }

  # Set up parallel plan
  plan(multisession, workers = n_workers)
  on.exit(plan(sequential), add = TRUE)

  start_time <- Sys.time()

  # Process remaining configs
  future_walk(todo_ids, function(cfg_id) {

    parquet_path <- file.path(cfg_dir, "_cfg", cfg_id, "results.parquet")
    cp_path <- file.path(checkpoint_dir, paste0(cfg_id, ".parquet"))

    if (!file.exists(parquet_path)) {
      result <- tibble(config_id = cfg_id, status = "parquet_not_found")
      arrow::write_parquet(result, cp_path)
      return(invisible(NULL))
    }

    result <- tryCatch({
      cv_single_config_nrc(
        parquet_path = parquet_path,
        config_id    = cfg_id,
        nrc_events   = nrc_events,
        ops_features = ops_features,
        K = K,
        min_train_years = min_train_years,
        min_reports = min_reports
      )
    }, error = function(e) {
      tibble(config_id = cfg_id, status = "failed", error = as.character(e$message))
    })

    # Save checkpoint
    arrow::write_parquet(result, cp_path)

  }, .options = furrr_options(
    seed = TRUE,
    packages = c("tidyverse", "lme4", "pROC", "arrow", "glue", "slider"),
    globals = list(
      cfg_dir = cfg_dir,
      nrc_events = nrc_events,
      ops_features = ops_features,
      K = K,
      min_train_years = min_train_years,
      min_reports = min_reports,
      WINDOW_SPECS = WINDOW_SPECS,
      MODEL_FORMULAS = MODEL_FORMULAS,
      MODEL_FORMULAS_NO_CLIMATE = MODEL_FORMULAS_NO_CLIMATE,
      MODEL_FORMULAS_INTERCEPT = MODEL_FORMULAS_INTERCEPT,
      MODEL_FORMULAS_SEASONAL = MODEL_FORMULAS_SEASONAL,
      NRC_OPS_VARS = NRC_OPS_VARS,
      NRC_OPS_LAG_K = NRC_OPS_LAG_K,
      NRC_OPS_ROLL_K = NRC_OPS_ROLL_K,
      NRC_OPS_MIN_HIST = NRC_OPS_MIN_HIST,
      OUTCOME_VARS = OUTCOME_VARS,
      cv_single_config_nrc = cv_single_config_nrc,
      prepare_nrc_config_data = prepare_nrc_config_data,
      create_all_windows = create_all_windows,
      ewma_time_decay_irregular_lag = ewma_time_decay_irregular_lag,
      join_nrc_ops = join_nrc_ops,
      get_climate_vars = get_climate_vars,
      get_outcome_config = get_outcome_config,
      build_formula = build_formula,
      build_binary_formula = build_binary_formula,
      build_intercept_formula = build_intercept_formula,
      build_seasonal_formula = build_seasonal_formula,
      cv_with_baselines = cv_with_baselines,
      cv_group_kfold = cv_group_kfold,
      cv_timeseries = cv_timeseries,
      compute_binary_metrics = compute_binary_metrics
    )
  ), .progress = TRUE)

  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  cat(sprintf("CV complete: %.1f minutes\n", as.numeric(elapsed)))

  # Combine all checkpoints
  all_results <- .load_cv_checkpoints(checkpoint_dir)

  # Save final combined output
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(all_results, output_path)
  cat(sprintf("Results saved to: %s (%d rows)\n", output_path, nrow(all_results)))

  return(all_results)
}


#' Load and combine all CV checkpoint files
.load_cv_checkpoints <- function(checkpoint_dir) {
  cp_files <- list.files(checkpoint_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(cp_files) == 0) return(tibble())
  purrr::map_dfr(cp_files, arrow::read_parquet)
}


# ==============================================================================
# MAIN: Example usage
# ==============================================================================

# Uncomment and adjust paths to run:
#
# nrc_events <- arrow::read_parquet("data/processed/nrc/nrc_events.parquet") %>%
#   mutate(event_date = as.Date(event_date))
#
# ops_features <- arrow::read_parquet("data/processed/nrc/ops_quarterly_features.parquet")
#
# cv_results <- run_nrc_cv(
#   cfg_dir      = "data/processed/nrc/configs",
#   nrc_events   = nrc_events,
#   ops_features = ops_features,
#   n_workers    = 8L,
#   K            = 5,
#   output_path  = "results/nrc/nrc_cv_results.parquet"
# )
