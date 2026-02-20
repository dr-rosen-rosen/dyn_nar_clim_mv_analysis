# 2_rail_cv_binary.R
# Cross-Validation for Stage 1 (Binary Occurrence) Models
# Implements Group K-Fold CV (Strategy A) and Time-Series CV (Strategy B)
# for the logistic GLMM: I(outcome > 0) ~ covariates + (1 | org_id)
#
# See cross_validation_metrics.md for full design rationale.
#
# Mike Rose - Safety Climate Analysis

library(tidyverse)
library(glmmTMB)
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
# METRIC FUNCTIONS
# ==============================================================================

#' Compute Stage 1 (binary) metrics on test-fold data
#'
#' @param y_binary Integer vector (0/1). Observed binary outcome.
#' @param p_event Numeric vector. Predicted P(outcome > 0).
#' @return Named list: brier_score (decision metric), plus diagnostics.
compute_stage1_metrics <- function(y_binary, p_event) {
  
  eps <- 1e-15
  p_event <- pmin(pmax(p_event, eps), 1 - eps)
  n <- length(y_binary)
  
  # Brier Score: decision metric
  brier <- mean((p_event - y_binary)^2)
  
  # Log-loss: diagnostic
  logloss <- -mean(y_binary * log(p_event) + (1 - y_binary) * log(1 - p_event))
  
  # AUC-ROC: diagnostic (requires both classes present)
  auc_roc <- tryCatch({
    roc_obj <- pROC::roc(y_binary, p_event, quiet = TRUE)
    as.numeric(pROC::auc(roc_obj))
  }, error = function(e) NA_real_)
  
  list(
    brier_score     = brier,
    log_loss        = logloss,
    auc_roc         = auc_roc,
    pred_event_rate = mean(p_event),
    obs_event_rate  = mean(y_binary),
    n_obs           = n,
    n_events        = sum(y_binary),
    n_zeros         = sum(y_binary == 0)
  )
}


# ==============================================================================
# RE-STANDARDIZATION HELPER
# ==============================================================================

#' Re-standardize operational features using training-set parameters
#'
#' Prevents information leakage: means/SDs computed on training data only,
#' then applied to both train and test.
#'
#' @param train_data Training data frame (modified in place)
#' @param test_data Test data frame (modified in place)
#' @param vars Character vector of variable names to re-standardize
#' @return List with $train and $test data frames
restandardize_ops <- function(train_data, test_data, vars = NULL) {
  if (is.null(vars)) {
    vars <- c(paste0(OPS_VARS, "_between"), paste0(OPS_VARS, "_within"))
  }
  
  for (v in vars) {
    if (v %in% names(train_data)) {
      train_mean <- mean(train_data[[v]], na.rm = TRUE)
      train_sd   <- sd(train_data[[v]], na.rm = TRUE)
      if (is.na(train_sd) || train_sd < 1e-10) train_sd <- 1
      train_data[[v]] <- (train_data[[v]] - train_mean) / train_sd
      test_data[[v]]  <- (test_data[[v]]  - train_mean) / train_sd
    }
  }
  
  list(train = train_data, test = test_data)
}


#' Check convergence of a glmmTMB fit
#' @param fit A glmmTMB model object
#' @return Logical: TRUE if converged (positive-definite Hessian)
check_convergence <- function(fit) {
  tryCatch({
    fit$sdr$pdHess
  }, error = function(e) FALSE)
}


# ==============================================================================
# CROSS-VALIDATION ENGINE: GROUP K-FOLD (STRATEGY A)
# ==============================================================================

#' Perform Group K-Fold CV for a single binary model specification
#'
#' Holds out entire organizations. Predictions use population-average
#' (re.form = NA) since held-out orgs have no estimated random effects.
#'
#' @param data Prepared data frame (complete cases with all covariates)
#' @param outcome "injuries", "fatalities", or "costs"
#' @param formula_str Formula string for the binary GLMM
#' @param model_label Label for this specification
#' @param K Number of folds (default 5)
#' @param seed Random seed for fold assignment
#' @return List with fold_results tibble and summary row
cv_binary_model <- function(data, outcome, formula_str, model_label = "full",
                            K = 5, seed = 42) {
  
  set.seed(seed)
  outcome_var <- get_outcome_var(outcome)
  
  # Assign folds at the organization level
  orgs <- unique(data$org_id)
  n_orgs <- length(orgs)
  
  if (n_orgs < K) {
    warning(sprintf("Only %d orgs available; reducing K from %d to %d",
                    n_orgs, K, n_orgs))
    K <- n_orgs
  }
  
  org_folds <- tibble(
    org_id = orgs,
    fold = sample(rep(1:K, length.out = n_orgs))
  )
  
  data <- data %>% left_join(org_folds, by = "org_id")
  
  # CV loop
  fold_results <- vector("list", K)
  
  for (k in 1:K) {
    train_data <- data %>% filter(fold != k)
    test_data  <- data %>% filter(fold == k)
    
    # Re-standardize ops features using training data
    restd <- restandardize_ops(train_data, test_data)
    train_data <- restd$train
    test_data  <- restd$test
    
    # Fit
    fit <- tryCatch({
      glmmTMB(as.formula(formula_str),
              data = train_data,
              family = binomial(link = "logit"))
    }, error = function(e) {
      warning(sprintf("  Group K-Fold %d/%d fit failed: %s", k, K, e$message))
      NULL
    })
    
    if (is.null(fit)) {
      fold_results[[k]] <- tibble(fold = k, status = "fit_failed")
      next
    }
    
    # Check convergence
    converged <- check_convergence(fit)
    
    # Predict: population-average (re.form = NA) for held-out orgs
    p_event <- tryCatch({
      as.numeric(predict(fit, newdata = test_data, type = "response",
                         re.form = NA, allow.new.levels = TRUE))
    }, error = function(e) {
      warning(sprintf("  Group K-Fold %d/%d predict failed: %s", k, K, e$message))
      NULL
    })
    
    if (is.null(p_event)) {
      fold_results[[k]] <- tibble(fold = k, status = "predict_failed")
      next
    }
    
    y_binary <- as.integer(test_data[[outcome_var]] > 0)
    s1 <- compute_stage1_metrics(y_binary, p_event)
    
    fold_results[[k]] <- tibble(
      fold             = k,
      status           = if (converged) "success" else "convergence_warning",
      converged        = converged,
      n_train_obs      = nrow(train_data),
      n_test_obs       = nrow(test_data),
      n_train_orgs     = n_distinct(train_data$org_id),
      n_test_orgs      = n_distinct(test_data$org_id),
      brier_score      = s1$brier_score,
      log_loss         = s1$log_loss,
      auc_roc          = s1$auc_roc,
      pred_event_rate  = s1$pred_event_rate,
      obs_event_rate   = s1$obs_event_rate,
      n_events         = s1$n_events,
      n_zeros          = s1$n_zeros
    )
  }
  
  # Aggregate
  fold_table <- bind_rows(fold_results)
  successful <- fold_table %>% filter(status %in% c("success", "convergence_warning"))
  
  if (nrow(successful) == 0) {
    warning("All folds failed!")
    return(list(fold_results = fold_table, summary = NULL))
  }
  
  summary_row <- successful %>%
    summarise(
      brier_score_mean    = mean(brier_score, na.rm = TRUE),
      brier_score_sd      = sd(brier_score, na.rm = TRUE),
      auc_roc_mean        = mean(auc_roc, na.rm = TRUE),
      log_loss_mean       = mean(log_loss, na.rm = TRUE),
      pred_event_rate_mean = mean(pred_event_rate, na.rm = TRUE),
      obs_event_rate_mean  = mean(obs_event_rate, na.rm = TRUE),
      n_successful_folds  = n(),
      n_converged         = sum(converged, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      model_label = model_label,
      outcome     = outcome,
      cv_strategy = "group_kfold",
      K           = K,
      n_orgs      = n_orgs
    )
  
  list(fold_results = fold_table, summary = summary_row)
}


# ==============================================================================
# CROSS-VALIDATION ENGINE: TIME-SERIES (STRATEGY B)
# ==============================================================================

#' Perform Time-Series CV (expanding window) for a single binary model
#'
#' Organizations appear in both train and test, so predictions use
#' conditional (org-specific) random effects.
#'
#' @param data Prepared data frame
#' @param outcome "injuries", "fatalities", or "costs"
#' @param formula_str Formula string for the binary GLMM
#' @param model_label Label for this specification
#' @param n_splits Number of temporal splits (default 4)
#' @param test_duration_months Duration of each test window (default 6)
#' @param gap_months Gap between train and test periods (default 0)
#' @return List with split_results tibble and summary row
cv_binary_timeseries <- function(data, outcome, formula_str,
                                 model_label = "full",
                                 n_splits = 4, test_duration_months = 6,
                                 gap_months = 0) {
  
  outcome_var <- get_outcome_var(outcome)
  
  # Define temporal cutpoints
  date_range <- range(data$event_date, na.rm = TRUE)
  max_date <- date_range[2]
  min_date <- date_range[1]
  
  cutpoints <- vector("list", n_splits)
  for (s in 1:n_splits) {
    test_end   <- max_date - months((s - 1) * test_duration_months)
    test_start <- test_end - months(test_duration_months)
    train_end  <- test_start - months(gap_months)
    
    cutpoints[[s]] <- list(
      split      = s,
      train_end  = train_end,
      test_start = test_start,
      test_end   = test_end
    )
  }
  
  # Filter out splits with insufficient training data
  cutpoints <- keep(cutpoints, ~ .x$train_end > min_date + months(12))
  
  if (length(cutpoints) == 0) {
    warning("No valid temporal splits. Data range too short.")
    return(list(split_results = tibble(), summary = NULL))
  }
  
  # CV loop
  split_results <- vector("list", length(cutpoints))
  
  for (i in seq_along(cutpoints)) {
    cp <- cutpoints[[i]]
    
    train_data <- data %>% filter(event_date <= cp$train_end)
    test_data  <- data %>% filter(event_date >= cp$test_start,
                                   event_date <= cp$test_end)
    
    if (nrow(train_data) < 100 || nrow(test_data) < 20) {
      split_results[[i]] <- tibble(split = cp$split, status = "insufficient_data")
      next
    }
    
    # Re-standardize ops features
    restd <- restandardize_ops(train_data, test_data)
    train_data <- restd$train
    test_data  <- restd$test
    
    # Re-center yearmonth_num using training data
    ym_mean <- mean(train_data$yearmonth_num, na.rm = TRUE)
    ym_sd   <- sd(train_data$yearmonth_num, na.rm = TRUE)
    if (is.na(ym_sd) || ym_sd < 1e-10) ym_sd <- 1
    train_data$yearmonth_num_c <- (train_data$yearmonth_num - ym_mean) / ym_sd
    test_data$yearmonth_num_c  <- (test_data$yearmonth_num  - ym_mean) / ym_sd
    
    # Fit
    fit <- tryCatch({
      glmmTMB(as.formula(formula_str),
              data = train_data,
              family = binomial(link = "logit"))
    }, error = function(e) {
      warning(sprintf("  TS split %d fit failed: %s", cp$split, e$message))
      NULL
    })
    
    if (is.null(fit)) {
      split_results[[i]] <- tibble(split = cp$split, status = "fit_failed")
      next
    }
    
    converged <- check_convergence(fit)
    
    # Predict: conditional (org-specific RE), orgs are in training data
    p_event <- tryCatch({
      as.numeric(predict(fit, newdata = test_data, type = "response",
                         allow.new.levels = TRUE))
    }, error = function(e) {
      warning(sprintf("  TS split %d predict failed: %s", cp$split, e$message))
      NULL
    })
    
    if (is.null(p_event)) {
      split_results[[i]] <- tibble(split = cp$split, status = "predict_failed")
      next
    }
    
    y_binary <- as.integer(test_data[[outcome_var]] > 0)
    s1 <- compute_stage1_metrics(y_binary, p_event)
    
    split_results[[i]] <- tibble(
      split            = cp$split,
      status           = if (converged) "success" else "convergence_warning",
      converged        = converged,
      train_end        = cp$train_end,
      test_start       = cp$test_start,
      test_end         = cp$test_end,
      n_train_obs      = nrow(train_data),
      n_test_obs       = nrow(test_data),
      n_train_orgs     = n_distinct(train_data$org_id),
      n_test_orgs      = n_distinct(test_data$org_id),
      # Effective sample size: distinct org-months in test
      n_test_org_months = n_distinct(test_data$org_id, test_data$yearmonth),
      brier_score      = s1$brier_score,
      log_loss         = s1$log_loss,
      auc_roc          = s1$auc_roc,
      pred_event_rate  = s1$pred_event_rate,
      obs_event_rate   = s1$obs_event_rate,
      n_events         = s1$n_events,
      n_zeros          = s1$n_zeros
    )
  }
  
  # Aggregate
  split_table <- bind_rows(split_results)
  successful <- split_table %>% filter(status %in% c("success", "convergence_warning"))
  
  if (nrow(successful) == 0) {
    warning("All temporal splits failed!")
    return(list(split_results = split_table, summary = NULL))
  }
  
  summary_row <- successful %>%
    summarise(
      brier_score_mean    = mean(brier_score, na.rm = TRUE),
      brier_score_sd      = sd(brier_score, na.rm = TRUE),
      auc_roc_mean        = mean(auc_roc, na.rm = TRUE),
      log_loss_mean       = mean(log_loss, na.rm = TRUE),
      pred_event_rate_mean = mean(pred_event_rate, na.rm = TRUE),
      obs_event_rate_mean  = mean(obs_event_rate, na.rm = TRUE),
      n_successful_splits = n(),
      n_converged         = sum(converged, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      model_label          = model_label,
      outcome              = outcome,
      cv_strategy          = "timeseries",
      n_splits             = length(cutpoints),
      test_duration_months = test_duration_months,
      gap_months           = gap_months
    )
  
  list(split_results = split_table, summary = summary_row)
}


# ==============================================================================
# RUNNING CV WITH BASELINES (BOTH STRATEGIES)
# ==============================================================================

#' Run CV for a climate variable plus baseline models under one strategy
#'
#' Fits all four models (full, no-climate, seasonal-only, intercept-only)
#' using the same fold/split assignments.
#'
#' @param data Prepared data frame with the climate variable column
#' @param climate_var Name of the climate variable to evaluate
#' @param outcome "injuries", "fatalities", or "costs"
#' @param strategy "group_kfold" or "timeseries"
#' @param K Number of folds (Strategy A only, default 5)
#' @param n_splits Number of splits (Strategy B only, default 4)
#' @param test_duration_months Test window size (Strategy B only, default 6)
#' @param gap_months Gap between train/test (Strategy B only, default 0)
#' @param seed Random seed
#' @return Tibble with one summary row per model, or NULL on failure
cv_with_baselines <- function(data, climate_var, outcome = "injuries",
                              strategy = "group_kfold",
                              K = 5, n_splits = 4, test_duration_months = 6,
                              gap_months = 0, seed = 42) {
  
  outcome_var <- get_outcome_var(outcome)
  ops_vars <- c(paste0(OPS_VARS, "_between"), paste0(OPS_VARS, "_within"))
  
  # Filter to complete cases (for the full model — includes climate var)
  complete_data <- data %>%
    filter(
      !is.na(.data[[climate_var]]),
      !is.na(.data[[outcome_var]]),
      !is.na(yearmonth_num_c),
      if_all(all_of(ops_vars), ~ !is.na(.x))
    )
  
  if (nrow(complete_data) < 100) {
    warning(sprintf("Insufficient complete cases (%d) for CV with %s",
                    nrow(complete_data), climate_var))
    return(NULL)
  }
  
  # Build all four formulas
  specs <- build_baseline_formulas(outcome, climate_var)
  
  # Dispatch to appropriate CV function
  cv_fn <- if (strategy == "group_kfold") {
    function(d, f_str, label) {
      cv_binary_model(d, outcome, f_str, model_label = label, K = K, seed = seed)
    }
  } else {
    function(d, f_str, label) {
      cv_binary_timeseries(d, outcome, f_str, model_label = label,
                           n_splits = n_splits,
                           test_duration_months = test_duration_months,
                           gap_months = gap_months)
    }
  }
  
  results <- map_dfr(specs, function(sp) {
    res <- tryCatch(
      cv_fn(complete_data, sp$formula, sp$label),
      error = function(e) {
        warning(sprintf("    CV failed for %s: %s", sp$label, e$message))
        list(summary = NULL)
      }
    )
    res$summary
  })
  
  if (nrow(results) == 0) return(NULL)
  
  # Add climate_var identifier so we can trace which climate var the baselines belong to
  results$climate_var_tested <- climate_var
  
  results
}


# ==============================================================================
# CV FOR A SINGLE CONFIGURATION (ALL CLIMATE VARS + BASELINES + BOTH STRATEGIES)
# ==============================================================================

#' Run full CV analysis for one configuration
#'
#' @param prepared_data Data frame from prepare_config_data() with all climate vars
#' @param config_id Configuration ID
#' @param outcome Outcome to evaluate
#' @param strategies Character vector: which strategies to run
#' @param K, n_splits, test_duration_months, gap_months CV parameters
#' @param seed Random seed
#' @return Tibble with CV summary rows for all climate vars × baselines × strategies
cv_single_config <- function(prepared_data, config_id, outcome = "injuries",
                             strategies = c("group_kfold", "timeseries"),
                             K = 5, n_splits = 4, test_duration_months = 6,
                             gap_months = 0, seed = 42) {
  
  climate_vars <- get_climate_vars(prepared_data)
  
  if (length(climate_vars) == 0) {
    warning(sprintf("No climate variables found for config %s", config_id))
    return(NULL)
  }
  
  all_results <- vector("list", length(climate_vars) * length(strategies))
  idx <- 0
  
  for (cvar in climate_vars) {
    for (strat in strategies) {
      idx <- idx + 1
      cat(sprintf("  Config %s | %s | %s\n", config_id, cvar, strat))
      
      res <- tryCatch(
        cv_with_baselines(prepared_data, cvar, outcome = outcome,
                          strategy = strat, K = K, n_splits = n_splits,
                          test_duration_months = test_duration_months,
                          gap_months = gap_months, seed = seed),
        error = function(e) {
          warning(sprintf("    Failed: %s", e$message))
          NULL
        }
      )
      
      if (!is.null(res)) {
        res$config_id <- config_id
        all_results[[idx]] <- res
      }
    }
  }
  
  bind_rows(all_results)
}


# ==============================================================================
# BATCH RUNNER WITH CHECKPOINTING
# ==============================================================================

#' Run CV across all configurations with checkpoint/resume
#'
#' @param cfg_dir Directory containing configuration folders
#' @param rail_raw Raw rail data (event-level)
#' @param ops_path Path to operational data file
#' @param outcome Outcome to evaluate
#' @param config_ids Optional: specific config IDs (default: all)
#' @param strategies CV strategies to run
#' @param n_cores Number of parallel workers
#' @param output_dir Directory for results and checkpoints
#' @param K, n_splits, test_duration_months, gap_months CV parameters
#' @param seed Random seed
#' @return Combined tibble of all CV results
run_cv_all_configs <- function(cfg_dir,
                               rail_raw,
                               ops_path = NULL,
                               outcome = "injuries",
                               config_ids = NULL,
                               strategies = c("group_kfold", "timeseries"),
                               n_cores = 16,
                               output_dir = "results/cv",
                               K = 5, n_splits = 4,
                               test_duration_months = 6,
                               gap_months = 0,
                               seed = 42) {
  
  cat("========================================\n")
  cat(sprintf("CROSS-VALIDATION: Stage 1 Binary - %s\n", toupper(outcome)))
  cat("========================================\n\n")
  
  # Create directories
  checkpoint_dir <- file.path(output_dir, "checkpoints")
  dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Load operational features
  ops_features <- NULL
  if (!is.null(ops_path) && file.exists(ops_path)) {
    cat("Loading operational data...\n")
    ops_features <- load_and_prepare_ops_features(ops_path, ops_vars = OPS_VARS)
    cat(sprintf("Operational features ready: %d org-months\n\n", nrow(ops_features)))
  } else {
    cat("No operational data. Proceeding without ops covariates.\n\n")
  }
  
  # Get configuration IDs
  if (is.null(config_ids)) {
    cfg_path <- file.path(cfg_dir, "_cfg")
    if (!dir.exists(cfg_path)) {
      stop(sprintf("Configuration directory not found: %s", cfg_path))
    }
    config_ids <- list.dirs(cfg_path, full.names = FALSE, recursive = FALSE)
    config_ids <- config_ids[config_ids != "" & !startsWith(config_ids, ".")]
  }
  
  # Reduce rail_raw
  outcome_var <- get_outcome_var(outcome)
  rail_raw_minimal <- rail_raw %>%
    select(eid, org_id, event_date, all_of(outcome_var)) %>%
    distinct()
  
  # Check for existing checkpoints (resume support)
  completed_ids <- list.files(checkpoint_dir, pattern = "\\.parquet$") %>%
    str_remove("\\.parquet$")
  remaining_ids <- setdiff(config_ids, completed_ids)
  
  cat(sprintf("Total configurations: %d\n", length(config_ids)))
  cat(sprintf("Already completed: %d\n", length(completed_ids)))
  cat(sprintf("Remaining: %d\n", length(remaining_ids)))
  cat(sprintf("Strategies: %s\n", paste(strategies, collapse = ", ")))
  
  n_windows <- nrow(WINDOW_SPECS$sma) + nrow(WINDOW_SPECS$ewma)
  n_models_per_config <- n_windows * 4 * length(strategies)  # 4 baselines per climate var
  cat(sprintf("~%d model fits per configuration\n", n_models_per_config))
  cat(sprintf("~%d total model fits remaining\n\n", n_models_per_config * length(remaining_ids)))
  
  if (length(remaining_ids) == 0) {
    cat("All configurations already completed. Loading checkpoints...\n")
  } else {
    cat(sprintf("Using %d cores\n", n_cores))
    cat("Processing...\n\n")
    
    # Parallel processing over configs
    plan(multisession, workers = n_cores)
    start_time <- Sys.time()
    
    progressr::with_progress({
      p <- progressr::progressor(steps = length(remaining_ids))
      
      future_walk(remaining_ids, function(cfg_id) {
        
        checkpoint_path <- file.path(checkpoint_dir, paste0(cfg_id, ".parquet"))
        parquet_path <- file.path(cfg_dir, "_cfg", cfg_id, "results.parquet")
        
        if (!file.exists(parquet_path)) {
          p()
          return(invisible(NULL))
        }
        
        cfg_cv <- tryCatch({
          # Prepare data (shared function)
          prepared <- prepare_config_data(parquet_path, cfg_id, rail_raw_minimal,
                                          ops_features = ops_features)
          
          # Run CV for all climate vars × baselines × strategies
          cv_single_config(prepared, cfg_id, outcome = outcome,
                           strategies = strategies, K = K,
                           n_splits = n_splits,
                           test_duration_months = test_duration_months,
                           gap_months = gap_months, seed = seed)
        }, error = function(e) {
          warning(sprintf("Config %s failed entirely: %s", cfg_id, e$message))
          tibble(
            config_id = cfg_id,
            status    = "config_failed",
            error     = as.character(e$message)
          )
        })
        
        # Save checkpoint
        if (!is.null(cfg_cv) && nrow(cfg_cv) > 0) {
          arrow::write_parquet(cfg_cv, checkpoint_path)
        }
        
        p()
        invisible(NULL)
        
      }, .options = furrr_options(
        seed = TRUE,
        packages = c("tidyverse", "glmmTMB", "pROC", "arrow", "glue", "slider"),
        globals = list(
          cfg_dir = cfg_dir,
          rail_raw_minimal = rail_raw_minimal,
          ops_features = ops_features,
          outcome = outcome,
          strategies = strategies,
          K = K,
          n_splits = n_splits,
          test_duration_months = test_duration_months,
          gap_months = gap_months,
          seed = seed,
          checkpoint_dir = checkpoint_dir,
          # Shared config
          WINDOW_SPECS = WINDOW_SPECS,
          MODEL_FORMULAS = MODEL_FORMULAS,
          OPS_VARS = OPS_VARS,
          OPS_LAG_K = OPS_LAG_K,
          OPS_ROLL_K = OPS_ROLL_K,
          OPS_MIN_HIST = OPS_MIN_HIST,
          OUTCOME_VARS = OUTCOME_VARS,
          # Functions
          get_outcome_var = get_outcome_var,
          prepare_config_data = prepare_config_data,
          get_climate_vars = get_climate_vars,
          create_all_windows = create_all_windows,
          ewma_time_decay_irregular_lag = ewma_time_decay_irregular_lag,
          make_ops_features_rolling = make_ops_features_rolling,
          build_binary_formula = build_binary_formula,
          build_intercept_only_formula = build_intercept_only_formula,
          build_seasonal_only_formula = build_seasonal_only_formula,
          build_baseline_formulas = build_baseline_formulas,
          compute_stage1_metrics = compute_stage1_metrics,
          restandardize_ops = restandardize_ops,
          check_convergence = check_convergence,
          cv_binary_model = cv_binary_model,
          cv_binary_timeseries = cv_binary_timeseries,
          cv_with_baselines = cv_with_baselines,
          cv_single_config = cv_single_config
        )
      ))
    })
    
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    cat(sprintf("\n\nCV completed in %.1f minutes\n\n", elapsed))
    
    plan(sequential)
  }
  
  # Combine all checkpoints
  cat("Combining checkpoint results...\n")
  checkpoint_files <- list.files(checkpoint_dir, pattern = "\\.parquet$",
                                 full.names = TRUE)
  
  if (length(checkpoint_files) == 0) {
    warning("No checkpoint files found!")
    return(tibble())
  }
  
  cv_results <- map_dfr(checkpoint_files, arrow::read_parquet)
  
  # Add window metadata (same logic as multiverse script)
  if ("climate_var_tested" %in% names(cv_results)) {
    cv_results <- cv_results %>%
      mutate(
        window_type = case_when(
          str_detect(climate_var_tested, "_sma_") ~ "sma",
          str_detect(climate_var_tested, "_ewmaLAG_") ~ "ewma",
          TRUE ~ "unknown"
        ),
        window_size = case_when(
          window_type == "sma" ~ as.numeric(str_extract(climate_var_tested, "(?<=_sma_)\\d+")),
          TRUE ~ NA_real_
        ),
        lag_days = case_when(
          window_type == "ewma" ~ as.numeric(str_extract(climate_var_tested, "(?<=_ewmaLAG_)\\d+")),
          TRUE ~ NA_real_
        ),
        halflife_days = case_when(
          window_type == "ewma" ~ as.numeric(str_extract(climate_var_tested, "(?<=_hl)\\d+")),
          TRUE ~ NA_real_
        )
      )
  }
  
  # Compute delta-Brier for each specification
  if (nrow(cv_results) > 0 && "model_label" %in% names(cv_results)) {
    # For each (config_id, climate_var_tested, cv_strategy),
    # compute full_brier - no_climate_brier
    delta_brier <- cv_results %>%
      filter(model_label %in% c(climate_var_tested, "no_climate")) %>%
      select(config_id, climate_var_tested, cv_strategy, model_label, brier_score_mean) %>%
      pivot_wider(
        names_from = model_label,
        values_from = brier_score_mean,
        names_prefix = "brier_"
      )
    
    # The full model column name varies by climate_var, so handle dynamically
    # Actually, model_label for the full model IS the climate_var name
    # This needs special handling since column names vary
    # Instead, compute per-row in a simpler way:
    full_models <- cv_results %>%
      filter(model_label == climate_var_tested) %>%
      select(config_id, climate_var_tested, cv_strategy,
             brier_full = brier_score_mean)
    
    no_climate_models <- cv_results %>%
      filter(model_label == "no_climate") %>%
      select(config_id, climate_var_tested, cv_strategy,
             brier_no_climate = brier_score_mean)
    
    delta_brier <- full_models %>%
      inner_join(no_climate_models,
                 by = c("config_id", "climate_var_tested", "cv_strategy")) %>%
      mutate(delta_brier = brier_full - brier_no_climate)
    
    # Negative delta = climate helps; positive = climate hurts
    cv_results <- cv_results %>%
      left_join(
        delta_brier %>% select(config_id, climate_var_tested, cv_strategy, delta_brier),
        by = c("config_id", "climate_var_tested", "cv_strategy")
      )
  }
  
  # Save combined results
  results_path <- file.path(output_dir,
                            sprintf("rail_%s_cv_results.parquet", outcome))
  arrow::write_parquet(cv_results, results_path)
  cat(sprintf("Combined CV results saved to: %s\n", results_path))
  
  # Also save as CSV for easy inspection
  csv_path <- file.path(output_dir,
                        sprintf("rail_%s_cv_results.csv", outcome))
  write_csv(cv_results, csv_path)
  cat(sprintf("CSV copy saved to: %s\n", csv_path))
  
  # Print summary
  cat("\n--- CV Summary ---\n")
  if ("delta_brier" %in% names(cv_results)) {
    delta_summary <- cv_results %>%
      filter(model_label == climate_var_tested) %>%
      group_by(cv_strategy) %>%
      summarise(
        n_specs          = n(),
        n_climate_helps  = sum(delta_brier < 0, na.rm = TRUE),
        n_climate_hurts  = sum(delta_brier > 0, na.rm = TRUE),
        mean_delta_brier = mean(delta_brier, na.rm = TRUE),
        median_delta_brier = median(delta_brier, na.rm = TRUE),
        .groups = "drop"
      )
    print(delta_summary)
  }
  
  return(cv_results)
}
