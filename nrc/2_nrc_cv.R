# 2_nrc_cv.R
# NRC Nuclear Cross-Validation Analysis
# Outcomes: power_loss_pct (binary gate), emerg_binary, (optionally binary_scram)
# Strategies: Group K-Fold (hold out facilities) + Time-Series Expanding Window
# Baselines: intercept-only, seasonal+ops-only, full with climate
# Mike Rose — Safety Climate Analysis
#
# Updated: March 2026
# Changes:
#   - Generalized from binary_scram-only to multiple outcomes
#   - power_loss_pct CV uses binary gate (did power loss occur?) with Brier/AUC
#   - Optional Gamma conditional CV (RMSE on positive cases)
#   - emerg_binary CV uses same binary framework
#   - Outcome-specific formula building via build_outcome_formula()
#   - glmmTMB support for hurdle binary gate alongside lme4::glmer

library(tidyverse)
library(lme4)
library(glmmTMB)
library(pROC)
library(broom.mixed)
library(arrow)
library(furrr)
library(glue)
library(here)
library(slider)

# Source shared modules
common_dir <- file.path(here::here(), "nrc/common")
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


#' Compute metrics for Gamma conditional model (positive cases only)
#'
#' @param y_true Observed positive values
#' @param y_pred Predicted positive values
#' @return Named list: rmse, mae, mape, n_obs
compute_gamma_metrics <- function(y_true, y_pred) {
  residuals <- y_true - y_pred
  list(
    rmse  = sqrt(mean(residuals^2)),
    mae   = mean(abs(residuals)),
    mape  = mean(abs(residuals / pmax(y_true, 0.01))) * 100,
    n_obs = length(y_true)
  )
}


# ==============================================================================
# OUTCOME-SPECIFIC FORMULA HELPERS FOR CV
# ==============================================================================

#' Build CV formulas for a given outcome
#'
#' Returns a list of formula objects for the three model variants:
#'   intercept_only, seasonal_ops, full (with climate)
#'
#' For hurdle outcomes (power_loss_pct), returns the BINARY GATE formula
#' (i.e., power_loss_binary ~ ...) since CV uses Brier/AUC on the binary part.
#'
#' @param outcome Outcome name (e.g., "power_loss_pct", "emerg_binary")
#' @param climate_var Climate variable name
#' @return List of lists: each with $fml (formula) and $label (string)
build_cv_formulas <- function(outcome, climate_var) {

  # Determine the outcome variable and formula source
  outcome_config <- get_outcome_config(outcome)

  if (outcome_config$family == "hurdle") {
    # Hurdle outcome: CV on the binary gate
    # Extract the binary formula from the list templates
    fml_full_template <- MODEL_FORMULAS[[outcome]]
    fml_seasonal_template <- MODEL_FORMULAS_SEASONAL[[outcome]]
    fml_intercept_template <- MODEL_FORMULAS_INTERCEPT[[outcome]]

    # Get the binary part — could be keyed as 'binary' or 'cond'/'zi'
    # For CV, we need the formula that predicts the binary gate
    # The binary gate formula uses power_loss_binary as the LHS
    get_binary_part <- function(template) {
      if ("binary" %in% names(template)) {
        return(template$binary)
      } else {
        # If using cond/zi convention, build a binary formula from the cond part
        # by replacing the outcome var with the binary version
        cond_str <- template$cond
        # Replace "power_loss_pct ~" with "power_loss_binary ~"
        # or "pct_power_loss ~" with "power_loss_binary ~"
        cond_str <- sub("^\\S+\\s*~", "power_loss_binary ~", cond_str)
        return(cond_str)
      }
    }

    fml_full_str <- get_binary_part(fml_full_template)
    fml_full_str <- gsub("CLIMATE_VAR", climate_var, fml_full_str, fixed = TRUE)
    fml_seasonal_str <- get_binary_part(fml_seasonal_template)
    fml_intercept_str <- get_binary_part(fml_intercept_template)

    # Also replace outcome var in intercept/seasonal if needed
    fml_intercept_str <- sub("^\\S+\\s*~", "power_loss_binary ~", fml_intercept_str)
    fml_seasonal_str <- sub("^\\S+\\s*~", "power_loss_binary ~", fml_seasonal_str)

    list(
      list(fml = as.formula(fml_intercept_str), label = "intercept_only"),
      list(fml = as.formula(fml_seasonal_str),  label = "seasonal_ops"),
      list(fml = as.formula(fml_full_str),      label = "full")
    )

  } else {
    # Binary or ordinal — straightforward single-formula
    fml_full      <- build_outcome_formula(outcome, climate_var)
    fml_seasonal  <- build_seasonal_formula(outcome)
    fml_intercept <- build_intercept_formula(outcome)

    list(
      list(fml = fml_intercept, label = "intercept_only"),
      list(fml = fml_seasonal,  label = "seasonal_ops"),
      list(fml = fml_full,      label = "full")
    )
  }
}


#' Get the outcome variable name to use for CV predictions
#'
#' For hurdle outcomes, returns the binary gate variable.
#' For binary outcomes, returns the outcome variable directly.
#'
#' @param outcome Outcome name
#' @return Character: column name in the data to evaluate predictions against
get_cv_outcome_var <- function(outcome) {
  outcome_config <- get_outcome_config(outcome)

  if (outcome_config$family == "hurdle") {
    # CV evaluates the binary gate
    "power_loss_binary"
  } else if (outcome_config$family == "binomial") {
    outcome_config$var
  } else {
    stop(sprintf("CV not supported for family: %s (outcome: %s)",
                 outcome_config$family, outcome))
  }
}


#' Get the model fitting function for CV
#'
#' @param outcome Outcome name
#' @return Character: "glmer" or "glmmTMB"
get_cv_model_fn <- function(outcome) {
  outcome_config <- get_outcome_config(outcome)

  if (outcome_config$family == "hurdle") {
    # Binary gate of hurdle — use glmmTMB for consistency with multiverse
    "glmmTMB"
  } else if (outcome_config$model_fn == "glmer") {
    "glmer"
  } else {
    # Default to glmer for binary
    "glmer"
  }
}


# ==============================================================================
# CV ENGINE: GROUP K-FOLD (Strategy A)
# ==============================================================================

#' Group K-Fold CV — hold out entire facilities
#'
#' Generalized to support any binary outcome via outcome_var and model_fn.
#'
#' @param data Prepared data frame
#' @param formula_obj Formula object
#' @param model_label Label for this model variant
#' @param K Number of folds
#' @param outcome_var Column name of the binary outcome (0/1)
#' @param model_fn "glmer" or "glmmTMB"
#' @return List with fold_results and summary
cv_group_kfold <- function(data, formula_obj, model_label = "full",
                           K = 5, outcome_var = "scram_binary",
                           model_fn = "glmer") {

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

    if (nrow(test_data) == 0 || sum(test_data[[outcome_var]]) == 0) {
      fold_results[[k]] <- tibble(fold = k, status = "empty_test")
      next
    }

    # Fit model on training data
    fit <- tryCatch({
      if (model_fn == "glmmTMB") {
        glmmTMB(formula_obj, data = train_data, family = binomial(link = "logit"))
      } else {
        glmer(formula_obj, data = train_data, family = binomial,
              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
      }
    }, error = function(e) NULL)

    if (is.null(fit)) {
      fold_results[[k]] <- tibble(fold = k, status = "fit_failed")
      next
    }

    # Check convergence
    converged <- if (model_fn == "glmmTMB") {
      is.null(fit$fit$convergence) || fit$fit$convergence == 0
    } else {
      is.null(fit@optinfo$conv$lme4$messages)
    }

    # Predict on test data — new facilities, so use population-level (re.form = NA)
    p_event <- tryCatch({
      if (model_fn == "glmmTMB") {
        predict(fit, newdata = test_data, type = "response", re.form = NA)
      } else {
        predict(fit, newdata = test_data, type = "response", re.form = NA)
      }
    }, error = function(e) NULL)

    if (is.null(p_event)) {
      fold_results[[k]] <- tibble(fold = k, status = "predict_failed")
      next
    }

    metrics <- compute_binary_metrics(test_data[[outcome_var]], p_event)

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
      brier_score_mean = mean(brier_score, na.rm = TRUE),
      brier_score_sd   = sd(brier_score, na.rm = TRUE),
      auc_roc_mean     = mean(auc_roc, na.rm = TRUE),
      log_loss_mean    = mean(log_loss, na.rm = TRUE),
      n_folds          = n(),
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
#' @param data Prepared data frame with event_date or year column
#' @param formula_obj Formula object
#' @param model_label Label for this model variant
#' @param min_train_years Minimum years of training data before first test
#' @param outcome_var Column name of the binary outcome (0/1)
#' @param model_fn "glmer" or "glmmTMB"
#' @return List with fold_results and summary
cv_timeseries <- function(data, formula_obj, model_label = "full",
                          min_train_years = 5, outcome_var = "scram_binary",
                          model_fn = "glmer") {

  # Derive year if not present
  if (!"year" %in% names(data) && "event_date" %in% names(data)) {
    data <- data %>% mutate(year = factor(lubridate::year(event_date)))
  }

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

    if (nrow(test_data) == 0 || sum(test_data[[outcome_var]]) == 0) {
      fold_results[[i]] <- tibble(fold = i, test_year = test_yr, status = "empty_test")
      next
    }

    # Fit on training data
    fit <- tryCatch({
      if (model_fn == "glmmTMB") {
        glmmTMB(formula_obj, data = train_data, family = binomial(link = "logit"))
      } else {
        glmer(formula_obj, data = train_data, family = binomial,
              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
      }
    }, error = function(e) NULL)

    if (is.null(fit)) {
      fold_results[[i]] <- tibble(fold = i, test_year = test_yr, status = "fit_failed")
      next
    }

    # Check convergence
    converged <- if (model_fn == "glmmTMB") {
      is.null(fit$fit$convergence) || fit$fit$convergence == 0
    } else {
      is.null(fit@optinfo$conv$lme4$messages)
    }

    # Conditional predictions (facilities are known in time-series CV)
    p_event <- tryCatch({
      if (model_fn == "glmmTMB") {
        predict(fit, newdata = test_data, type = "response", allow.new.levels = TRUE)
      } else {
        predict(fit, newdata = test_data, type = "response", allow.new.levels = TRUE)
      }
    }, error = function(e) NULL)

    if (is.null(p_event)) {
      fold_results[[i]] <- tibble(fold = i, test_year = test_yr, status = "predict_failed")
      next
    }

    metrics <- compute_binary_metrics(test_data[[outcome_var]], p_event)

    fold_results[[i]] <- tibble(
      fold            = i,
      test_year       = test_yr,
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
      brier_score_mean = mean(brier_score, na.rm = TRUE),
      brier_score_sd   = sd(brier_score, na.rm = TRUE),
      auc_roc_mean     = mean(auc_roc, na.rm = TRUE),
      log_loss_mean    = mean(log_loss, na.rm = TRUE),
      n_folds          = n(),
      .groups = "drop"
    ) %>%
    mutate(model_label = model_label, cv_strategy = "timeseries")

  list(fold_results = fold_table, summary = summary_row)
}


# ==============================================================================
# CV WITH BASELINES: Run all model variants for a single climate var + outcome
# ==============================================================================

#' Run CV for full model + baselines for one climate variable and outcome
#'
#' Models compared:
#'   1. intercept_only: outcome ~ 1 + (1|facility)
#'   2. seasonal_ops:   outcome ~ temporal + ops + (1|facility)
#'   3. full:           outcome ~ temporal + ops + climate + (1|facility)
#'
#' For hurdle outcomes, CV evaluates the binary gate (did event occur?)
#' using Brier, AUC, log-loss — same metrics as binary outcomes.
#'
#' @param data Prepared data frame
#' @param climate_var Climate variable name
#' @param outcome Outcome name (e.g., "power_loss_pct", "emerg_binary")
#' @param K Folds for group k-fold
#' @param min_train_years Min years for time-series CV
#' @return Tibble with one row per model × strategy
cv_with_baselines <- function(data, climate_var, outcome = "emerg_binary",
                              K = 5, min_train_years = 5) {

  # Get outcome-specific settings
  outcome_var <- get_cv_outcome_var(outcome)
  model_fn    <- get_cv_model_fn(outcome)

  # Build the three formula variants
  models <- build_cv_formulas(outcome, climate_var)

  # Filter to complete cases for this outcome
  data <- data %>%
    filter(
      !is.na(.data[[outcome_var]]),
      !is.na(.data[[climate_var]]),
      !is.na(yearmonth_num_c)
    )

  # Also require ops covariates
  ops_cols <- grep("_(between|within)$", names(data), value = TRUE)
  if (length(ops_cols) > 0) {
    for (col in ops_cols) {
      data <- data %>% filter(!is.na(.data[[col]]))
    }
  }

  if (nrow(data) < 100) {
    return(tibble(
      climate_var = climate_var, outcome = outcome,
      model_label = "insufficient_data", cv_strategy = NA,
      brier_mean = NA, n_obs = nrow(data)
    ))
  }

  all_summaries <- list()

  for (m in models) {
    # Strategy A: Group K-Fold
    res_gkf <- tryCatch({
      cv_group_kfold(data, m$fml, model_label = m$label, K = K,
                     outcome_var = outcome_var, model_fn = model_fn)
    }, error = function(e) list(summary = NULL))

    if (!is.null(res_gkf$summary)) {
      all_summaries <- c(all_summaries, list(res_gkf$summary))
    }

    # Strategy B: Time-Series
    res_ts <- tryCatch({
      cv_timeseries(data, m$fml, model_label = m$label,
                    min_train_years = min_train_years,
                    outcome_var = outcome_var, model_fn = model_fn)
    }, error = function(e) list(summary = NULL))

    if (!is.null(res_ts$summary)) {
      all_summaries <- c(all_summaries, list(res_ts$summary))
    }
  }

  if (length(all_summaries) == 0) return(NULL)

  bind_rows(all_summaries) %>%
    mutate(climate_var = climate_var, climate_var_tested = climate_var, outcome = outcome)
}


# ==============================================================================
# ANALYZE SINGLE CONFIGURATION (all outcomes × climate vars × CV)
# ==============================================================================

#' Run CV for a single NRC configuration across all outcomes
#'
#' @param parquet_path Path to config parquet
#' @param config_id Configuration ID
#' @param nrc_events Event metadata
#' @param ops_features Operational features
#' @param outcomes Character vector of outcomes to CV
#' @param K Folds for group k-fold
#' @param min_train_years Min years for time-series CV
#' @param min_reports Min events per facility
#' @return Tibble of CV results for this config
cv_single_config_nrc <- function(parquet_path, config_id, nrc_events,
                                 ops_features = NULL,
                                 outcomes = c("power_loss_pct", "emerg_binary"),
                                 K = 5, min_train_years = 5,
                                 min_reports = 75) {

  df <- prepare_nrc_config_data(parquet_path, config_id, nrc_events,
                                ops_features, min_reports = min_reports)

  if (is.null(df) || nrow(df) == 0) {
    return(tibble(config_id = config_id, status = "no_data"))
  }

  climate_vars <- get_climate_vars(df)
  if (length(climate_vars) == 0) {
    return(tibble(config_id = config_id, status = "no_climate_vars"))
  }

  # Run CV for each outcome × climate variable
  cv_results <- list()

  for (outcome in outcomes) {
    # Check if this outcome's family supports binary CV
    outcome_family <- tryCatch(
      get_outcome_config(outcome)$family,
      error = function(e) "unknown"
    )
    if (!outcome_family %in% c("binomial", "hurdle")) {
      warning(sprintf("  Config %s: skipping '%s' — CV not supported for family '%s'",
                      config_id, outcome, outcome_family))
      next
    }

    for (cvar in climate_vars) {
      res <- tryCatch({
        cv_with_baselines(df, cvar, outcome = outcome,
                          K = K, min_train_years = min_train_years)
      }, error = function(e) {
        tibble(climate_var = cvar, outcome = outcome, model_label = "error",
               cv_strategy = NA, brier_mean = NA, error = as.character(e$message))
      })

      if (!is.null(res)) {
        res$config_id <- config_id
        cv_results <- c(cv_results, list(res))
      }
    }
  }

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
#' @param config_ids Config IDs to run (NULL = all)
#' @param outcomes Character vector of outcomes to CV
#' @param n_workers Parallel workers (also accepts n_cores for rail consistency)
#' @param K Group k-fold K
#' @param min_train_years Time-series min training years (NRC uses year-based
#'   expanding window; rail-style n_splits/test_duration_months/gap_months are
#'   not applicable because NRC events are sparse and year-level granularity
#'   is more appropriate)
#' @param output_path Output parquet path
#' @param checkpoint_dir Directory for per-config checkpoints
#' @param min_reports Min events per facility
#' @param seed Random seed for fold assignment
#' @param n_cores Alias for n_workers (for consistency with rail CV interface)
#' @param ... Additional arguments (absorbed silently for cross-industry compatibility,
#'   e.g. n_splits, test_duration_months, gap_months from rail CV calls)
#' @return Data frame of all CV results
run_nrc_cv <- function(cfg_dir, nrc_events, ops_features = NULL,
                       config_ids = NULL,
                       outcomes = c("power_loss_pct", "emerg_binary", "ordinal_scram"),
                       n_workers = NULL, K = 5, min_train_years = 5,
                       output_path = "results/nrc/nrc_cv_results.parquet",
                       checkpoint_dir = "results/nrc/cv_checkpoints",
                       min_reports = 75,
                       seed = 42,
                       n_cores = NULL,
                       ...) {

  # Handle n_cores / n_workers aliasing
  if (!is.null(n_cores) && is.null(n_workers)) {
    n_workers <- n_cores
  }
  if (is.null(n_workers)) n_workers <- 4L

  # Warn about unused rail-specific params if passed
  dots <- list(...)
  rail_params <- intersect(names(dots), c("n_splits", "test_duration_months", "gap_months"))
  if (length(rail_params) > 0) {
    message(sprintf("Note: %s ignored for NRC CV (uses year-based expanding window via min_train_years=%d instead)",
                    paste(rail_params, collapse = ", "), min_train_years))
  }

  # Discover configurations
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
  cat(sprintf("  Outcomes: %s\n", paste(outcomes, collapse = ", ")))

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
        outcomes     = outcomes,
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
    packages = c("tidyverse", "lme4", "glmmTMB", "pROC", "broom.mixed",
                 "arrow", "glue", "slider"),
    globals = {
      # Build globals list dynamically — only include objects that exist
      # Use globalenv() because parent.frame() inside furrr_options resolves
      # to the wrong scope and misses sourced functions
      env <- globalenv()

      candidate_globals <- list(
        # Data
        cfg_dir = cfg_dir,
        nrc_events = nrc_events,
        ops_features = ops_features,
        outcomes = outcomes,
        K = K,
        min_train_years = min_train_years,
        min_reports = min_reports
      )

      # Config constants — include if they exist in the global environment
      config_vars <- c("WINDOW_SPECS", "MODEL_FORMULAS", "MODEL_FORMULAS_NO_CLIMATE",
                       "MODEL_FORMULAS_INTERCEPT", "MODEL_FORMULAS_SEASONAL",
                       "NRC_OPS_VARS", "NRC_OPS_LAG_K", "NRC_OPS_ROLL_K",
                       "NRC_OPS_MIN_HIST", "OUTCOME_VARS", "MIN_REPORTS_DEFAULT")
      for (v in config_vars) {
        if (exists(v, envir = env)) {
          candidate_globals[[v]] <- get(v, envir = env)
        }
      }

      # Functions — include if they exist in the global environment
      fn_names <- c("cv_single_config_nrc", "prepare_nrc_config_data",
                    "create_all_windows", "encode_nrc_outcomes", "join_nrc_ops",
                    "get_climate_vars", "get_outcome_config", "get_cv_outcome_var",
                    "get_cv_model_fn", "build_formula", "build_outcome_formula",
                    "build_intercept_formula", "build_seasonal_formula",
                    "build_cv_formulas", "cv_with_baselines", "cv_group_kfold",
                    "cv_timeseries", "compute_binary_metrics", "compute_gamma_metrics",
                    "ewma_time_decay_irregular_lag", "make_ops_features_rolling")
      for (fn in fn_names) {
        if (exists(fn, envir = env)) {
          candidate_globals[[fn]] <- get(fn, envir = env)
        }
      }

      # Report what was found vs missing for debugging
      found_fns <- fn_names[sapply(fn_names, function(fn) exists(fn, envir = env))]
      missing_fns <- setdiff(fn_names, found_fns)
      if (length(missing_fns) > 0) {
        warning(sprintf("NRC CV globals: %d functions found, %d missing: %s",
                        length(found_fns), length(missing_fns),
                        paste(missing_fns, collapse = ", ")))
      }

      candidate_globals
    }
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
#   outcomes     = c("power_loss_pct", "emerg_binary"),
#   n_workers    = 8L,
#   K            = 5,
#   output_path  = "results/nrc/nrc_cv_results.parquet"
# )
