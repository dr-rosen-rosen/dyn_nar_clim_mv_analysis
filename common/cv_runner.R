# =============================================================================
# common/cv_runner.R — Layer-Aware Cross-Validation Runner
# =============================================================================
#
# Shared CV infrastructure for binary (Stage 1) model evaluation.
# Supports Group K-Fold and Time-Series expanding window strategies.
#
# The CV compares nested model tiers:
#   - intercept_only:  outcome ~ 1 + (1 | org)
#   - seasonal_ops:    outcome ~ temporal_controls + ops + (1 | org)
#   - climate:         outcome ~ climate_var + temporal_controls + ops + (1 | org)
#   - climate_temporal: climate + tf_burstiness + tf_memory + ...  (paper 2)
#   - climate_ews:     climate + ews_variance + ews_ar1 + ...      (paper 2)
#   - climate_all:     climate + temporal + ews                     (paper 2)
#
# Each tier uses the same fold/split assignments so delta-Brier is a fair
# within-fold comparison.
#
# Industry-specific details (which model fitting function, org variable name,
# outcome variable mapping) come from the protocol.
#
# Dependencies: dplyr, purrr, furrr, arrow, glue, pROC
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(furrr)
  library(arrow)
  library(glue)
})


# =============================================================================
# 1. METRIC COMPUTATION
# =============================================================================

#' Compute binary classification metrics
#'
#' @param y_true Binary outcome vector (0/1)
#' @param p_pred Predicted probability vector
#' @return Named list: brier_score, log_loss, auc_roc, event rates, counts
compute_binary_cv_metrics <- function(y_true, p_pred) {
  eps <- 1e-15
  p_pred <- pmin(pmax(p_pred, eps), 1 - eps)
  n <- length(y_true)

  brier <- mean((p_pred - y_true)^2)
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
    n_obs           = n,
    n_events        = sum(y_true),
    n_zeros         = sum(y_true == 0)
  )
}


#' Re-standardize variables using training-set parameters
#'
#' @param train Training data frame
#' @param test Test data frame
#' @param vars Character vector of variable names to re-standardize
#' @return List with $train and $test
restandardize_cv <- function(train, test, vars) {
  for (v in vars) {
    if (!v %in% names(train)) next
    mu <- mean(train[[v]], na.rm = TRUE)
    s  <- sd(train[[v]], na.rm = TRUE)
    if (is.na(s) || s < 1e-10) s <- 1
    train[[v]] <- (train[[v]] - mu) / s
    test[[v]]  <- (test[[v]] - mu) / s
  }
  list(train = train, test = test)
}


# =============================================================================
# 2. CV ENGINES
# =============================================================================

#' Group K-Fold CV engine
#'
#' Holds out entire organizations. Predictions use population-average
#' (re.form = NA) since held-out orgs have no estimated random effects.
#'
#' @param data Prepared data frame (complete cases)
#' @param formula_str Formula string for the binary model
#' @param model_label Label for this model tier (e.g., "climate", "climate_temporal")
#' @param K Number of folds
#' @param outcome_var Column name of binary outcome (0/1)
#' @param org_var Organization grouping variable
#' @param ops_vars Character vector of ops variable names to re-standardize
#' @param fit_fn_name "glmmTMB" or "glmer" — which fitting function to use
#' @param seed Random seed for fold assignment
#' @return List with $fold_results tibble and $summary row
cv_group_kfold <- function(data, formula_str, model_label = "full",
                            K = 5, outcome_var = "outcome_binary",
                            org_var = "org_id", ops_vars = character(0),
                            fit_fn_name = "glmmTMB", seed = 42) {

  set.seed(seed)
  orgs <- unique(data[[org_var]])
  n_orgs <- length(orgs)

  if (n_orgs < K) K <- max(2, n_orgs)

  org_folds <- tibble(
    !!org_var := orgs,
    fold = sample(rep(1:K, length.out = n_orgs))
  )

  data <- data %>% left_join(org_folds, by = org_var)

  fold_results <- vector("list", K)

  for (k in 1:K) {
    train_data <- data %>% filter(fold != k)
    test_data  <- data %>% filter(fold == k)

    if (nrow(test_data) < 5 || sum(test_data[[outcome_var]]) == 0) {
      fold_results[[k]] <- tibble(fold = k, status = "empty_test")
      next
    }

    # Re-standardize ops using training data
    if (length(ops_vars) > 0) {
      restd <- restandardize_cv(train_data, test_data, ops_vars)
      train_data <- restd$train
      test_data  <- restd$test
    }

    # Fit
    fit <- tryCatch({
      if (fit_fn_name == "glmmTMB") {
        glmmTMB::glmmTMB(as.formula(formula_str), data = train_data,
                         family = binomial(link = "logit"))
      } else {
        lme4::glmer(as.formula(formula_str), data = train_data,
                    family = binomial,
                    control = lme4::glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 2e5)))
      }
    }, error = function(e) NULL)

    if (is.null(fit)) {
      fold_results[[k]] <- tibble(fold = k, status = "fit_failed")
      next
    }

    # Convergence check
    converged <- tryCatch({
      if (fit_fn_name == "glmmTMB") {
        is.null(fit$fit$convergence) || fit$fit$convergence == 0
      } else {
        is.null(fit@optinfo$conv$lme4$messages)
      }
    }, error = function(e) FALSE)

    # Predict — population-average for held-out orgs
    p_event <- tryCatch({
      predict(fit, newdata = test_data, type = "response",
              re.form = NA, allow.new.levels = TRUE)
    }, error = function(e) NULL)

    if (is.null(p_event)) {
      fold_results[[k]] <- tibble(fold = k, status = "predict_failed")
      next
    }

    metrics <- compute_binary_cv_metrics(test_data[[outcome_var]], p_event)

    fold_results[[k]] <- tibble(
      fold            = k,
      status          = ifelse(converged, "success", "convergence_warning"),
      converged       = converged,
      n_train         = nrow(train_data),
      n_test          = nrow(test_data),
      brier_score     = metrics$brier_score,
      log_loss        = metrics$log_loss,
      auc_roc         = metrics$auc_roc,
      pred_event_rate = metrics$pred_event_rate,
      obs_event_rate  = metrics$obs_event_rate,
      n_events        = metrics$n_events
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
      n_converged      = sum(converged, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(model_label = model_label, cv_strategy = "group_kfold")

  list(fold_results = fold_table, summary = summary_row)
}


#' Time-Series expanding window CV engine
#'
#' Organizations appear in both train and test. Predictions use conditional
#' (org-specific) random effects since orgs are known.
#'
#' @param data Prepared data frame
#' @param formula_str Formula string
#' @param model_label Label for this model tier
#' @param n_splits Number of temporal splits
#' @param test_duration_months Duration of each test window in months
#' @param gap_months Gap between train end and test start
#' @param outcome_var Binary outcome column name
#' @param org_var Organization grouping variable
#' @param ops_vars Ops variables to re-standardize
#' @param fit_fn_name "glmmTMB" or "glmer"
#' @return List with $split_results and $summary
cv_timeseries <- function(data, formula_str, model_label = "full",
                           n_splits = 4, test_duration_months = 6,
                           gap_months = 0,
                           outcome_var = "outcome_binary",
                           org_var = "org_id", ops_vars = character(0),
                           fit_fn_name = "glmmTMB") {

  date_range <- range(data$event_date, na.rm = TRUE)
  max_date <- date_range[2]
  min_date <- date_range[1]

  # Build temporal cutpoints
  cutpoints <- vector("list", n_splits)
  for (s in 1:n_splits) {
    test_end   <- max_date - months((s - 1) * test_duration_months)
    test_start <- test_end - months(test_duration_months)
    train_end  <- test_start - months(gap_months)

    cutpoints[[s]] <- list(
      split = s, train_end = train_end,
      test_start = test_start, test_end = test_end
    )
  }

  # Filter out splits with insufficient training data
  cutpoints <- keep(cutpoints, ~ .x$train_end > min_date + months(12))

  if (length(cutpoints) == 0) {
    return(list(split_results = tibble(), summary = NULL))
  }

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

    # Re-standardize
    if (length(ops_vars) > 0) {
      restd <- restandardize_cv(train_data, test_data, ops_vars)
      train_data <- restd$train
      test_data  <- restd$test
    }

    # Re-center yearmonth_num using training data
    ym_mean <- mean(train_data$yearmonth_num, na.rm = TRUE)
    ym_sd   <- sd(train_data$yearmonth_num, na.rm = TRUE)
    if (is.na(ym_sd) || ym_sd < 1e-10) ym_sd <- 1
    train_data$yearmonth_num_c <- (train_data$yearmonth_num - ym_mean) / ym_sd
    test_data$yearmonth_num_c  <- (test_data$yearmonth_num  - ym_mean) / ym_sd

    # Fit
    fit <- tryCatch({
      if (fit_fn_name == "glmmTMB") {
        glmmTMB::glmmTMB(as.formula(formula_str), data = train_data,
                         family = binomial(link = "logit"))
      } else {
        lme4::glmer(as.formula(formula_str), data = train_data,
                    family = binomial,
                    control = lme4::glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 2e5)))
      }
    }, error = function(e) NULL)

    if (is.null(fit)) {
      split_results[[i]] <- tibble(split = cp$split, status = "fit_failed")
      next
    }

    converged <- tryCatch({
      if (fit_fn_name == "glmmTMB") {
        is.null(fit$fit$convergence) || fit$fit$convergence == 0
      } else {
        is.null(fit@optinfo$conv$lme4$messages)
      }
    }, error = function(e) FALSE)

    # Predict — conditional (org-specific RE), orgs are in training data
    p_event <- tryCatch({
      predict(fit, newdata = test_data, type = "response",
              allow.new.levels = TRUE)
    }, error = function(e) NULL)

    if (is.null(p_event)) {
      split_results[[i]] <- tibble(split = cp$split, status = "predict_failed")
      next
    }

    metrics <- compute_binary_cv_metrics(test_data[[outcome_var]], p_event)

    split_results[[i]] <- tibble(
      split           = cp$split,
      status          = ifelse(converged, "success", "convergence_warning"),
      converged       = converged,
      train_end       = cp$train_end,
      test_start      = cp$test_start,
      test_end        = cp$test_end,
      n_train         = nrow(train_data),
      n_test          = nrow(test_data),
      brier_score     = metrics$brier_score,
      log_loss        = metrics$log_loss,
      auc_roc         = metrics$auc_roc,
      pred_event_rate = metrics$pred_event_rate,
      obs_event_rate  = metrics$obs_event_rate,
      n_events        = metrics$n_events
    )
  }

  split_table <- bind_rows(split_results)
  successful <- split_table %>% filter(status %in% c("success", "convergence_warning"))

  if (nrow(successful) == 0) {
    return(list(split_results = split_table, summary = NULL))
  }

  summary_row <- successful %>%
    summarise(
      brier_score_mean = mean(brier_score, na.rm = TRUE),
      brier_score_sd   = sd(brier_score, na.rm = TRUE),
      auc_roc_mean     = mean(auc_roc, na.rm = TRUE),
      log_loss_mean    = mean(log_loss, na.rm = TRUE),
      n_splits         = n(),
      n_converged      = sum(converged, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(model_label = model_label, cv_strategy = "timeseries")

  list(split_results = split_table, summary = summary_row)
}


# =============================================================================
# 3. LAYER-AWARE MODEL TIER BUILDER
# =============================================================================

#' Build the set of nested model formulas for CV comparison
#'
#' Produces a list of (formula_string, label) pairs representing the
#' comparison ladder. For paper 1, this is:
#'   intercept_only, seasonal_ops, climate
#'
#' For paper 2, it extends to:
#'   intercept_only, seasonal_ops, climate, climate_temporal,
#'   climate_ews, climate_all
#'
#' @param base_formula_str The full climate model formula string (with
#'   climate_var already substituted)
#' @param no_climate_formula_str Formula without climate variable
#' @param intercept_formula_str Intercept-only formula
#' @param climate_var The climate variable name (used as label for full model)
#' @param extra_terms Character vector of layer terms to add (from layer registry).
#'   If empty, only the standard 3-tier ladder is produced.
#' @param layer_term_groups Named list of term groups for incremental comparison.
#'   e.g., list(temporal = c("tf_burstiness", "tf_memory"),
#'              ews = c("ews_variance", "ews_ar1", "ews_return_rate"))
#'   If NULL but extra_terms is non-empty, all extra_terms are added as one tier.
#' @param org_var Organization random effect variable (for formula insertion)
#' @return List of lists, each with $formula (string) and $label (string)
build_cv_model_tiers <- function(base_formula_str,
                                  no_climate_formula_str,
                                  intercept_formula_str,
                                  climate_var,
                                  extra_terms = character(0),
                                  layer_term_groups = NULL,
                                  org_var = "org_id") {

  tiers <- list(
    list(formula = intercept_formula_str, label = "intercept_only"),
    list(formula = no_climate_formula_str, label = "no_climate"),
    list(formula = base_formula_str, label = climate_var)
  )

  # If no layer terms, return the standard 3-tier ladder
  if (length(extra_terms) == 0) {
    return(tiers)
  }

  re_pattern <- paste0("\\+ \\(1 \\| ", org_var, "\\)")

  # If layer_term_groups provided, add each group as a separate tier
  if (!is.null(layer_term_groups) && length(layer_term_groups) > 0) {
    cumulative_terms <- character(0)

    for (group_name in names(layer_term_groups)) {
      group_terms <- layer_term_groups[[group_name]]
      cumulative_terms <- c(cumulative_terms, group_terms)
      extra_str <- paste(cumulative_terms, collapse = " + ")

      tier_formula <- sub(re_pattern,
                          paste0("+ ", extra_str, " + (1 | ", org_var, ")"),
                          base_formula_str)

      tiers <- c(tiers, list(list(
        formula = tier_formula,
        label = paste0("climate_", group_name)
      )))
    }

    # Add the "all layers" tier if there are multiple groups
    if (length(layer_term_groups) > 1) {
      all_str <- paste(extra_terms, collapse = " + ")
      all_formula <- sub(re_pattern,
                         paste0("+ ", all_str, " + (1 | ", org_var, ")"),
                         base_formula_str)
      tiers <- c(tiers, list(list(
        formula = all_formula,
        label = "climate_all_layers"
      )))
    }
  } else {
    # Single tier with all extra terms
    extra_str <- paste(extra_terms, collapse = " + ")
    tier_formula <- sub(re_pattern,
                        paste0("+ ", extra_str, " + (1 | ", org_var, ")"),
                        base_formula_str)
    tiers <- c(tiers, list(list(formula = tier_formula, label = "climate_layers")))
  }

  tiers
}


# =============================================================================
# 4. CV FOR ONE SPEC (climate_var × layer_config)
# =============================================================================

#' Run CV for one climate variable with all model tiers and both strategies
#'
#' @param data Prepared data frame (already filtered to complete cases)
#' @param climate_var Climate variable name
#' @param outcome Outcome name
#' @param outcome_var Binary outcome column name
#' @param tiers List from build_cv_model_tiers()
#' @param strategies Character vector of CV strategies
#' @param org_var Organization grouping variable
#' @param ops_vars Ops vars to re-standardize
#' @param fit_fn_name "glmmTMB" or "glmer"
#' @param K Folds for group_kfold
#' @param n_splits Splits for timeseries
#' @param test_duration_months Test window for timeseries
#' @param gap_months Gap for timeseries
#' @param seed Random seed
#' @return Tibble with one row per (tier × strategy)
cv_single_spec <- function(data, climate_var, outcome, outcome_var,
                            tiers,
                            strategies = c("group_kfold", "timeseries"),
                            org_var = "org_id",
                            ops_vars = character(0),
                            fit_fn_name = "glmmTMB",
                            K = 5, n_splits = 4,
                            test_duration_months = 6, gap_months = 0,
                            seed = 42) {

  all_summaries <- list()

  for (tier in tiers) {
    for (strat in strategies) {
      res <- tryCatch({
        if (strat == "group_kfold") {
          cv_group_kfold(data, tier$formula, model_label = tier$label,
                         K = K, outcome_var = outcome_var, org_var = org_var,
                         ops_vars = ops_vars, fit_fn_name = fit_fn_name,
                         seed = seed)
        } else {
          cv_timeseries(data, tier$formula, model_label = tier$label,
                        n_splits = n_splits,
                        test_duration_months = test_duration_months,
                        gap_months = gap_months,
                        outcome_var = outcome_var, org_var = org_var,
                        ops_vars = ops_vars, fit_fn_name = fit_fn_name)
        }
      }, error = function(e) {
        list(summary = NULL)
      })

      if (!is.null(res$summary)) {
        all_summaries <- c(all_summaries, list(res$summary))
      }
    }
  }

  if (length(all_summaries) == 0) return(NULL)

  bind_rows(all_summaries) %>%
    mutate(
      climate_var_tested = climate_var,
      outcome = outcome
    )
}


# =============================================================================
# 5. CV FOR A SINGLE CONFIGURATION
# =============================================================================

#' Run CV for one climate config across all climate_vars × outcomes
#'
#' @param protocol Validation protocol
#' @param cfg_dir Config directory
#' @param config_id Climate config ID
#' @param events_df Event-level data
#' @param ops_features Operational features
#' @param layer_data Precomputed layer data (from load_independent_layers)
#' @return Tibble of CV results for this config
cv_single_config <- function(protocol, cfg_dir, config_id, events_df,
                              ops_features = NULL, layer_data = list()) {

  registry <- protocol$layer_registry
  config_id <- as.character(config_id)
  # Prepare data
  parquet_path <- get_config_parquet_path(cfg_dir, config_id,
                                          protocol$config_dir_structure)

  prepared <- tryCatch(
    protocol$data_prep_fn(
      parquet_path = parquet_path,
      config_id = config_id,
      events_df = events_df,
      ops_features = ops_features,
      min_reports = protocol$min_reports
    ),
    error = function(e) NULL
  )

  if (is.null(prepared) || nrow(prepared) == 0) {
    return(tibble(config_id = as.character(config_id), status = "no_data"))
  }

  climate_vars <- get_climate_vars(prepared)
  if (length(climate_vars) == 0) {
    return(tibble(config_id = as.character(config_id), status = "no_climate_vars"))
  }

  # Determine CV outcomes (may be subset of MV outcomes)
  cv_outcomes <- protocol$cv$cv_outcomes %||% protocol$outcomes

  # Determine org var and ops vars for re-standardization
  org_var <- protocol$org_var
  ops_vars <- c(paste0(protocol$ops_vars, "_between"),
                paste0(protocol$ops_vars, "_within"))

  # Layer config grid
  layer_grid <- get_crossed_config_grid(registry)
  has_layers <- nrow(layer_grid) > 0 && !(".dummy" %in% names(layer_grid))

  if (!has_layers) {
    layer_grid <- tibble(.no_layers = TRUE)
  }

  cv_results <- list()

  for (cv in climate_vars) {

    # Compute dependent layers for this climate_var
    dep_data <- if (length(registry$dependent) > 0) {
      compute_dependent_layers_for_var(registry, prepared, climate_var = cv)
    } else {
      list()
    }

    all_layer_data <- c(layer_data, dep_data)

    for (li in seq_len(nrow(layer_grid))) {
      layer_config_row <- layer_grid[li, , drop = FALSE]

      # Get layer terms and join features
      extra_terms <- if (has_layers) {
        get_layer_formula_terms(registry, layer_config_row)
      } else {
        character(0)
      }

      df_with_layers <- if (has_layers) {
        join_layer_features(prepared, all_layer_data, layer_config_row, registry)
      } else {
        prepared
      }

      # Build layer term groups for incremental CV tiers
      layer_term_groups <- if (has_layers && length(extra_terms) > 0) {
        groups <- list()
        for (nm in names(registry$layers)) {
          layer_terms <- registry$layers[[nm]]$formula_terms(layer_config_row)
          if (length(layer_terms) > 0) {
            groups[[nm]] <- layer_terms
          }
        }
        groups
      } else {
        NULL
      }

      for (outcome in cv_outcomes) {

        # Get outcome-specific settings
        outcome_config <- tryCatch(get_outcome_config(outcome), error = function(e) NULL)
        if (is.null(outcome_config)) next

        # Determine binary outcome var for CV
        # Hurdle outcomes use binary gate; binary outcomes use the var directly
        if (outcome_config$family == "hurdle") {
          outcome_var <- "power_loss_binary"
          fit_fn_name <- "glmmTMB"
        } else if (outcome_config$family == "binomial") {
          outcome_var <- outcome_config$var
          fit_fn_name <- if (!is.null(outcome_config$model_fn) &&
                             outcome_config$model_fn == "glmer") "glmer" else "glmmTMB"
        } else {
          next
        }
        
        # For count outcomes (rail hurdle), create binary column if needed
        if (!outcome_var %in% names(df_with_layers) && outcome_config$family == "hurdle") {
          # Rail pattern: outcome var is a count, need binary conversion
          count_var <- outcome_config$var
          outcome_var <- paste0(count_var, "_binary")
          df_with_layers[[outcome_var]] <- as.integer(df_with_layers[[count_var]] > 0)
        }

        # Build the base formulas using config's formula builders
        # These should exist from sourcing industry config.R
        base_formula <- tryCatch({
          build_baseline_formulas(outcome, cv)
        }, error = function(e) NULL)

        if (is.null(base_formula) || length(base_formula) == 0) next

        # Extract the formula strings from build_baseline_formulas output
        # Rail returns: list(list(formula=str, label=str), ...)
        # NRC returns: list(list(fml=formula_obj, label=str), ...)
        # Normalize to strings
        get_fml_str <- function(spec) {
          f <- spec$formula %||% spec$fml
          if (inherits(f, "formula")) deparse(f, width.cutoff = 500) else as.character(f)
        }

        # Find the specific tiers from baseline formulas
        intercept_str <- NULL
        no_climate_str <- NULL
        climate_str <- NULL

        for (spec in base_formula) {
          fml_s <- get_fml_str(spec)
          if (grepl("intercept", spec$label, ignore.case = TRUE)) {
            intercept_str <- fml_s
          } else if (grepl("no_climate|seasonal", spec$label, ignore.case = TRUE)) {
            no_climate_str <- fml_s
          } else {
            climate_str <- fml_s  # the full model with climate
          }
        }

        if (is.null(climate_str)) next

        # Build layer-aware tiers
        tiers <- build_cv_model_tiers(
          base_formula_str = climate_str,
          no_climate_formula_str = no_climate_str %||% intercept_str,
          intercept_formula_str = intercept_str %||% paste0(outcome_var, " ~ 1 + (1 | ", org_var, ")"),
          climate_var = cv,
          extra_terms = extra_terms,
          layer_term_groups = layer_term_groups,
          org_var = org_var
        )

        # Filter data to complete cases
        all_model_vars <- c(cv, outcome_var, "yearmonth_num_c", ops_vars, extra_terms)
        existing_vars <- intersect(all_model_vars, names(df_with_layers))

        complete_data <- df_with_layers %>%
          filter(if_all(all_of(existing_vars), ~ !is.na(.)))

        if (nrow(complete_data) < 100) next

        # Run CV
        res <- tryCatch({
          cv_single_spec(
            data = complete_data,
            climate_var = cv,
            outcome = outcome,
            outcome_var = outcome_var,
            tiers = tiers,
            strategies = protocol$cv$strategies,
            org_var = org_var,
            ops_vars = ops_vars,
            fit_fn_name = fit_fn_name,
            K = protocol$cv$K %||% 5L,
            n_splits = protocol$cv$n_splits %||% 4L,
            test_duration_months = protocol$cv$test_duration_months %||% 6L,
            gap_months = protocol$cv$gap_months %||% 0L,
            seed = 42
          )
        }, error = function(e) NULL)

        if (!is.null(res)) {
          res$config_id <- config_id

          # Add layer config IDs
          if (has_layers) {
            for (nm in names(registry$layers)) {
              col <- paste0(nm, "_config_id")
              if (col %in% names(layer_config_row)) {
                res[[col]] <- layer_config_row[[col]]
              }
            }
          }

          cv_results <- c(cv_results, list(res))
        }
      }
    }
  }

  if (length(cv_results) == 0) {
    return(tibble(config_id = config_id, status = "no_cv_results"))
  }

  bind_rows(cv_results)
}


# =============================================================================
# 6. PARALLEL CV RUNNER
# =============================================================================

#' Run CV across all configurations
#'
#' @param protocol Validation protocol
#' @param cfg_dir Config directory
#' @param events_df Event-level data
#' @param ops_features Operational features
#' @param config_ids Config IDs (NULL = discover all)
#' @param n_workers Parallel workers
#' @param output_path Output parquet path
#' @param checkpoint_dir Checkpoint directory
#' @return Tibble of all CV results
run_cv <- function(protocol,
                    cfg_dir,
                    events_df,
                    ops_features = NULL,
                    config_ids = NULL,
                    n_workers = 4L,
                    output_path = NULL,
                    checkpoint_dir = NULL) {

  # Discover configs
  if (is.null(config_ids)) {
    config_ids <- discover_config_ids(cfg_dir, protocol$config_dir_structure)
  }

  message(glue("\n{'=' |> rep(60) |> paste(collapse='')}\n",
               "Cross-Validation: {protocol$label}\n",
               "{'=' |> rep(60) |> paste(collapse='')}\n",
               "Climate configs: {length(config_ids)}\n",
               "CV outcomes: {paste(protocol$cv$cv_outcomes %||% protocol$outcomes, collapse=', ')}\n",
               "Strategies: {paste(protocol$cv$strategies, collapse=', ')}"))

  # Load independent layers
  message("\nLoading config-independent feature layers...")
  independent_data <- load_independent_layers(protocol$layer_registry, events_df)

  # Checkpointing
  if (!is.null(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    existing <- tools::file_path_sans_ext(
      list.files(checkpoint_dir, pattern = "\\.parquet$")
    )
    remaining <- setdiff(config_ids, existing)
    if (length(existing) > 0) {
      message(glue("  Found {length(existing)} checkpoints, {length(remaining)} remaining"))
    }
    config_ids <- remaining
  }

  if (length(config_ids) == 0) {
    message("All configs checkpointed. Loading results...")
    return(load_checkpointed_results(checkpoint_dir))
  }

  # Parallel run
  plan(multisession, workers = n_workers)
  on.exit(plan(sequential), add = TRUE)

  start_time <- Sys.time()

  future_walk(config_ids, function(cid) {

    # Re-source in worker
    source("common/data_prep.R", local = FALSE)
    source("common/temporal_features.R", local = FALSE)
    source("common/feature_layers.R", local = FALSE)
    source("common/layer_temporal.R", local = FALSE)
    source("common/layer_ews.R", local = FALSE)
    source("common/cv_runner.R", local = FALSE)

    for (f in protocol$source_files) {
      source(f, local = FALSE)
    }

    result <- tryCatch(
      cv_single_config(
        protocol = protocol,
        cfg_dir = cfg_dir,
        config_id = cid,
        events_df = events_df,
        ops_features = ops_features,
        layer_data = independent_data
      ),
      error = function(e) {
        tibble(config_id = as.character(cid), status = "failed", error = conditionMessage(e))
      }
    )

    # Save checkpoint
    if (!is.null(checkpoint_dir) && nrow(result) > 0) {
      cp_path <- file.path(checkpoint_dir, paste0(cid, ".parquet"))
      tryCatch(
        arrow::write_parquet(result, cp_path),
        error = function(e) warning(glue("Checkpoint failed for {cid}"))
      )
    }

  }, .options = furrr_options(
    seed = TRUE,
    packages = c("dplyr", "purrr", "tidyr", "glue", "arrow", "slider",
                 "glmmTMB", "lme4", "pROC", "broom.mixed"),
    globals = c(
      as.list(globalenv()),
      list(
        protocol = protocol,
        cfg_dir = cfg_dir,
        events_df = events_df,
        ops_features = ops_features,
        independent_data = independent_data,
        checkpoint_dir = checkpoint_dir
      )
    )
  ), .progress = TRUE)

  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  message(glue("CV complete: {round(as.numeric(elapsed), 1)} minutes"))

  # Load all results
  if (!is.null(checkpoint_dir)) {
    all_results <- load_checkpointed_results(checkpoint_dir)
  } else {
    all_results <- tibble()
  }

  # Compute delta-Brier
  if (nrow(all_results) > 0 && "model_label" %in% names(all_results) &&
      "climate_var_tested" %in% names(all_results)) {

    # Delta vs. no-climate baseline
    climate_models <- all_results %>%
      filter(model_label == climate_var_tested) %>%
      select(config_id, climate_var_tested, cv_strategy, outcome,
             any_of(c("temporal_config_id", "ews_config_id")),
             brier_climate = brier_score_mean)

    baseline_models <- all_results %>%
      filter(model_label == "no_climate") %>%
      select(config_id, climate_var_tested, cv_strategy, outcome,
             any_of(c("temporal_config_id", "ews_config_id")),
             brier_baseline = brier_score_mean)

    join_cols <- intersect(names(climate_models), names(baseline_models))
    join_cols <- setdiff(join_cols, c("brier_climate", "brier_baseline"))

    delta <- climate_models %>%
      inner_join(baseline_models, by = join_cols) %>%
      mutate(delta_brier = brier_climate - brier_baseline)

    all_results <- all_results %>%
      left_join(
        delta %>% select(all_of(join_cols), delta_brier),
        by = join_cols
      )
  }

  # Save
  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(all_results, output_path)
    message(glue("Results saved: {output_path} ({nrow(all_results)} rows)"))
  }

  all_results
}
