# =============================================================================
# common/panel_cv_runner.R — Panel-Rate Cross-Validation
# =============================================================================
#
# Cross-validation for panel-rate GLMMs (NB and Tweedie). Parallel to
# common/cv_runner.R, but the held-out scoring is a proper scoring rule for
# count / continuous-positive outcomes:
#
#   PRIMARY: held-out log-likelihood per observation (higher = better)
#   SECONDARY: deviance (Poisson/NB) or Tweedie deviance (lower = better)
#   TERTIARY: pearson correlation of predicted vs observed rate
#
# Two strategies, mirroring the event-level CV:
#   group_kfold  — hold out facilities (population-average prediction)
#   timeseries   — hold out tail periods (conditional prediction)
#
# Three model tiers per outcome (delta-loglik climate vs no-climate is the
# headline metric):
#   intercept_only : <outcome> ~ offset(log(exposure)) + (1 | org)
#   seasonal_ops   : intercept + temporal_controls + ops_bw
#   climate        : seasonal_ops + climate_var
#
# Dependencies: dplyr, purrr, furrr, glmmTMB, tweedie (for log-density),
#               lubridate
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(furrr)
  library(glmmTMB)
  library(lubridate)
  library(glue)
  library(tibble)
})


# =============================================================================
# 1. SCORING — held-out log-likelihood per family
# =============================================================================

#' Held-out log-likelihood per observation, dispatched by family.
#'
#' Uses the fitted model's family to compute the predictive density for each
#' held-out row at its predicted mean. The "size" / power parameters needed
#' for NB and Tweedie densities come from the fit object.
#'
#' Returns a list with mean log-lik per obs, total log-lik, and components:
#'   loglik_mean, loglik_total, deviance_mean, n_obs, family
score_panel_holdout <- function(fit, newdata, outcome_var) {
  fam <- family(fit)$family
  mu <- predict(fit, newdata = newdata, type = "response",
                re.form = NA, allow.new.levels = TRUE)
  y <- newdata[[outcome_var]]
  ok <- !is.na(mu) & !is.na(y) & is.finite(mu) & mu > 0

  if (!any(ok)) {
    return(list(loglik_mean = NA_real_, loglik_total = NA_real_,
                deviance_mean = NA_real_, n_obs = 0L, family = fam))
  }

  mu <- mu[ok]; y <- y[ok]
  ll <- switch(fam,
    poisson = dpois(y, lambda = mu, log = TRUE),
    nbinom2 = {
      sz <- glmmTMB::sigma(fit)
      dnbinom(y, mu = mu, size = sz, log = TRUE)
    },
    nbinom1 = {
      # nbinom1: var = mu * (1 + phi). Convert to size = mu/phi.
      phi <- glmmTMB::sigma(fit)
      sz <- mu / phi
      dnbinom(y, mu = mu, size = sz, log = TRUE)
    },
    tweedie = {
      if (!requireNamespace("tweedie", quietly = TRUE)) {
        stop("Install package 'tweedie' for Tweedie CV scoring.")
      }
      psi <- fit$fit$parfull[names(fit$fit$parfull) == "psi"]
      p_var <- if (length(psi) > 0) 1 + plogis(unname(psi[1])) else 1.5
      phi <- glmmTMB::sigma(fit)
      tweedie::dtweedie(y, mu = mu, phi = phi, power = p_var) %>%
        pmax(.Machine$double.xmin) %>% log()
    },
    {
      warning(glue("score_panel_holdout: unsupported family '{fam}', ",
                   "falling back to Gaussian density."))
      sd_resid <- sd(residuals(fit), na.rm = TRUE)
      dnorm(y, mean = mu, sd = max(sd_resid, 1e-6), log = TRUE)
    }
  )

  # Deviance (poisson/NB analytic; Tweedie via -2 * loglik vs saturated approx).
  dev_mean <- switch(fam,
    poisson = mean(2 * (y * log(pmax(y, 1) / mu) - (y - mu))),
    nbinom2 = NA_real_,
    nbinom1 = NA_real_,
    tweedie = NA_real_,
    NA_real_
  )

  list(
    loglik_mean   = mean(ll),
    loglik_total  = sum(ll),
    deviance_mean = dev_mean,
    n_obs         = length(y),
    family        = fam,
    obs_pred_cor  = suppressWarnings(cor(y, mu))
  )
}


# =============================================================================
# 2. FORMULA BUILDERS PER TIER
# =============================================================================

#' Build the three CV formulas (intercept_only, seasonal_ops, climate) for a
#' panel-rate model.
#'
#' @param outcome_var Name of count/continuous outcome.
#' @param exposure Name of exposure column (used as offset).
#' @param climate_var Name of climate column.
#' @param bw_terms Character vector of between/within ops covariates.
#' @param org_var Random-intercept grouping variable.
#' @return Named list of formula strings.
build_panel_cv_formulas <- function(outcome_var, exposure, climate_var,
                                    bw_terms, org_var,
                                    data_names = NULL) {
  off <- sprintf("offset(log(%s))", exposure)
  re  <- sprintf("(1 | %s)", org_var)

  # Optional temporal/seasonal terms — included only if the panel actually has
  # them. Annual-cadence panels (PHMSA) drop sin_month/cos_month.
  candidate_seasonal <- c("yearmonth_num_c", "sin_month", "cos_month")
  seasonal_terms <- if (is.null(data_names)) {
    candidate_seasonal
  } else {
    intersect(candidate_seasonal, data_names)
  }

  ops_chunk <- paste(bw_terms, collapse = " + ")
  rhs_seasonal_ops <- if (length(seasonal_terms) > 0) {
    paste(c(seasonal_terms, ops_chunk), collapse = " + ")
  } else {
    ops_chunk
  }

  intercept <- paste(outcome_var, "~", off, "+", re)
  seasonal  <- paste(outcome_var, "~", rhs_seasonal_ops, "+", off, "+", re)
  climate   <- paste(outcome_var, "~", climate_var, "+",
                     rhs_seasonal_ops, "+", off, "+", re)
  list(intercept_only = intercept, seasonal_ops = seasonal, climate = climate)
}


# =============================================================================
# 3. CV ENGINES
# =============================================================================

#' Group K-Fold for panel-rate models. Holds out facilities/orgs.
#'
#' @param data Panel data (complete cases for the chosen formula).
#' @param formula_str Model formula string.
#' @param outcome_var Outcome column.
#' @param exposure Exposure column (used to require >0 in test).
#' @param org_var Grouping variable for fold assignment.
#' @param family glmmTMB family.
#' @param model_label Tier label (e.g. "climate", "seasonal_ops").
#' @param K Number of folds.
#' @param seed RNG seed.
#' @return List with $fold_results tibble and $summary tibble.
panel_cv_group_kfold <- function(data, formula_str, outcome_var, exposure,
                                  org_var, family,
                                  model_label = "climate",
                                  K = 5L, seed = 42L,
                                  min_test_rows = 10L) {
  set.seed(seed)
  orgs <- unique(data[[org_var]])
  if (length(orgs) < K) K <- max(2L, length(orgs))

  fold_map <- tibble(.org = orgs, fold = sample(rep(1:K, length.out = length(orgs))))
  data$.org <- data[[org_var]]
  data <- data %>% left_join(fold_map, by = ".org")

  rows <- vector("list", K)
  for (k in seq_len(K)) {
    train <- data %>% filter(fold != k)
    test  <- data %>% filter(fold == k, .data[[exposure]] > 0,
                             !is.na(.data[[outcome_var]]))
    if (nrow(test) < min_test_rows || sum(test[[outcome_var]] > 0) == 0) {
      rows[[k]] <- tibble(fold = k, status = "empty_test",
                          model_label = model_label)
      next
    }
    fit <- tryCatch(
      glmmTMB::glmmTMB(as.formula(formula_str), data = train, family = family),
      error = function(e) NULL
    )
    if (is.null(fit) || !isTRUE(fit$sdr$pdHess)) {
      rows[[k]] <- tibble(fold = k, status = "fit_failed",
                          model_label = model_label,
                          n_train = nrow(train), n_test = nrow(test))
      next
    }
    sc <- tryCatch(score_panel_holdout(fit, test, outcome_var),
                   error = function(e) NULL)
    if (is.null(sc)) {
      rows[[k]] <- tibble(fold = k, status = "score_failed",
                          model_label = model_label)
      next
    }
    rows[[k]] <- tibble(
      fold        = k,
      status      = "success",
      model_label = model_label,
      n_train     = nrow(train),
      n_test      = nrow(test),
      family      = sc$family,
      loglik_mean   = sc$loglik_mean,
      loglik_total  = sc$loglik_total,
      deviance_mean = sc$deviance_mean,
      obs_pred_cor  = sc$obs_pred_cor
    )
  }
  fold_table <- bind_rows(rows)
  ok <- fold_table %>% filter(status == "success")
  summary_row <- if (nrow(ok) > 0) {
    ok %>% summarise(
      cv_strategy   = "group_kfold",
      model_label   = first(model_label),
      n_folds       = n(),
      loglik_mean   = mean(loglik_mean,   na.rm = TRUE),
      loglik_sd     = sd(loglik_mean,     na.rm = TRUE),
      deviance_mean = mean(deviance_mean, na.rm = TRUE),
      obs_pred_cor  = mean(obs_pred_cor,  na.rm = TRUE),
      .groups = "drop"
    )
  } else NULL
  list(fold_results = fold_table, summary = summary_row)
}


#' Time-series CV for panel-rate models. Holds out tail periods.
#'
#' @param data Panel with a `period_date` column (Date). Set up by caller —
#'   typically yearmonth (rail) or quarter_start (NRC).
#' @param formula_str Model formula string.
#' @param outcome_var Outcome column.
#' @param exposure Exposure column.
#' @param family glmmTMB family.
#' @param model_label Tier label.
#' @param n_splits Number of expanding-window splits.
#' @param test_duration_months Duration of each test window.
#' @param gap_months Gap between train end and test start.
#' @return Same shape as panel_cv_group_kfold().
panel_cv_timeseries <- function(data, formula_str, outcome_var, exposure,
                                 family,
                                 model_label = "climate",
                                 n_splits = 5L,
                                 test_duration_months = 24L,
                                 gap_months = 6L,
                                 min_train_rows = 200L,
                                 min_test_rows = 20L) {
  if (!"period_date" %in% names(data)) {
    stop("panel_cv_timeseries requires a `period_date` column on data")
  }
  date_range <- range(data$period_date, na.rm = TRUE)
  max_date <- date_range[2]; min_date <- date_range[1]

  cutpoints <- vector("list", n_splits)
  for (s in seq_len(n_splits)) {
    test_end   <- max_date - months((s - 1) * test_duration_months)
    test_start <- test_end - months(test_duration_months)
    train_end  <- test_start - months(gap_months)
    cutpoints[[s]] <- list(split = s, train_end = train_end,
                           test_start = test_start, test_end = test_end)
  }
  cutpoints <- keep(cutpoints, ~ .x$train_end > min_date + months(12))
  if (length(cutpoints) == 0) {
    return(list(split_results = tibble(), summary = NULL))
  }

  rows <- vector("list", length(cutpoints))
  for (i in seq_along(cutpoints)) {
    cp <- cutpoints[[i]]
    train <- data %>% filter(period_date <= cp$train_end)
    test  <- data %>% filter(period_date >= cp$test_start,
                             period_date <= cp$test_end,
                             .data[[exposure]] > 0,
                             !is.na(.data[[outcome_var]]))
    if (nrow(train) < min_train_rows || nrow(test) < min_test_rows ||
        sum(test[[outcome_var]] > 0) == 0) {
      rows[[i]] <- tibble(split = cp$split, status = "insufficient_data",
                          model_label = model_label)
      next
    }
    fit <- tryCatch(
      glmmTMB::glmmTMB(as.formula(formula_str), data = train, family = family),
      error = function(e) NULL
    )
    if (is.null(fit) || !isTRUE(fit$sdr$pdHess)) {
      rows[[i]] <- tibble(split = cp$split, status = "fit_failed",
                          model_label = model_label,
                          n_train = nrow(train), n_test = nrow(test))
      next
    }
    # Conditional prediction: orgs are in training data, so use re.form=NULL
    # via an override of score_panel_holdout's predict (default re.form=NA).
    mu <- tryCatch(
      predict(fit, newdata = test, type = "response", allow.new.levels = TRUE),
      error = function(e) NULL
    )
    if (is.null(mu)) {
      rows[[i]] <- tibble(split = cp$split, status = "predict_failed",
                          model_label = model_label)
      next
    }
    # Score using the same density logic but with our own mu.
    fam <- family(fit)$family
    y <- test[[outcome_var]]
    ok <- !is.na(mu) & !is.na(y) & is.finite(mu) & mu > 0
    if (!any(ok)) {
      rows[[i]] <- tibble(split = cp$split, status = "score_failed",
                          model_label = model_label)
      next
    }
    mu_ok <- mu[ok]; y_ok <- y[ok]
    ll <- switch(fam,
      poisson = dpois(y_ok, lambda = mu_ok, log = TRUE),
      nbinom2 = {
        sz <- glmmTMB::sigma(fit)
        dnbinom(y_ok, mu = mu_ok, size = sz, log = TRUE)
      },
      nbinom1 = {
        phi <- glmmTMB::sigma(fit); sz <- mu_ok / phi
        dnbinom(y_ok, mu = mu_ok, size = sz, log = TRUE)
      },
      tweedie = {
        psi <- fit$fit$parfull[names(fit$fit$parfull) == "psi"]
        p_var <- if (length(psi) > 0) 1 + plogis(unname(psi[1])) else 1.5
        phi <- glmmTMB::sigma(fit)
        log(pmax(tweedie::dtweedie(y_ok, mu = mu_ok, phi = phi, power = p_var),
                 .Machine$double.xmin))
      },
      {
        sd_r <- sd(residuals(fit), na.rm = TRUE)
        dnorm(y_ok, mean = mu_ok, sd = max(sd_r, 1e-6), log = TRUE)
      }
    )
    rows[[i]] <- tibble(
      split        = cp$split,
      status       = "success",
      model_label  = model_label,
      train_end    = as.Date(cp$train_end),
      test_start   = as.Date(cp$test_start),
      test_end     = as.Date(cp$test_end),
      n_train      = nrow(train),
      n_test       = sum(ok),
      family       = fam,
      loglik_mean  = mean(ll),
      loglik_total = sum(ll),
      obs_pred_cor = suppressWarnings(cor(y_ok, mu_ok))
    )
  }
  split_table <- bind_rows(rows)
  ok <- split_table %>% filter(status == "success")
  summary_row <- if (nrow(ok) > 0) {
    ok %>% summarise(
      cv_strategy  = "timeseries",
      model_label  = first(model_label),
      n_splits     = n(),
      loglik_mean  = mean(loglik_mean, na.rm = TRUE),
      loglik_sd    = sd(loglik_mean,   na.rm = TRUE),
      obs_pred_cor = mean(obs_pred_cor, na.rm = TRUE),
      .groups = "drop"
    )
  } else NULL
  list(split_results = split_table, summary = summary_row)
}


# =============================================================================
# 4. ORCHESTRATION — three tiers per outcome × strategy
# =============================================================================

#' Run all CV tiers (intercept, seasonal_ops, climate) for one outcome.
#'
#' @param panel Panel data frame (single config, all climate windows present).
#' @param outcome_var Outcome column.
#' @param exposure Exposure column.
#' @param climate_var Climate column.
#' @param bw_terms Between/within covariate names.
#' @param org_var Org grouping var.
#' @param family glmmTMB family.
#' @param config_id For tagging.
#' @param strategies Character vector subset of c("group_kfold", "timeseries").
#' @param period_date_var Name of the period date column (for timeseries).
#' @param ... Passed through to the engines (K, n_splits, etc).
#' @return Tibble of per-tier × per-strategy summary rows; one extra row per
#'   strategy with delta-loglik (climate − seasonal_ops).
run_panel_cv_for_outcome <- function(panel, outcome_var, exposure,
                                      climate_var, bw_terms, org_var, family,
                                      config_id,
                                      strategies = c("group_kfold", "timeseries"),
                                      period_date_var = NULL,
                                      K = 5L, n_splits = 5L,
                                      test_duration_months = 24L,
                                      gap_months = 6L,
                                      min_train_rows = 200L,
                                      min_test_rows_kfold = 10L,
                                      min_test_rows_timeseries = 20L,
                                      seed = 42L) {

  fmls <- build_panel_cv_formulas(outcome_var, exposure, climate_var,
                                  bw_terms, org_var,
                                  data_names = names(panel))

  # Filter to complete cases for the climate formula (most permissive: needs
  # everything). All tiers use the same data slice for fair comparison.
  required <- c(outcome_var, exposure, climate_var,
                "yearmonth_num_c", "sin_month", "cos_month",
                bw_terms, org_var)
  d <- panel %>% filter(if_all(all_of(intersect(required, names(panel))),
                                ~ !is.na(.x)),
                         .data[[exposure]] > 0)

  # Provide a `period_date` alias for timeseries CV.
  if (!is.null(period_date_var) && period_date_var %in% names(d)) {
    d$period_date <- d[[period_date_var]]
  }

  out <- list()

  for (strat in strategies) {
    tier_rows <- list()

    for (tier in c("intercept_only", "seasonal_ops", "climate")) {
      eng <- if (strat == "group_kfold") {
        panel_cv_group_kfold(
          d, fmls[[tier]], outcome_var, exposure, org_var, family,
          model_label = tier, K = K, seed = seed,
          min_test_rows = min_test_rows_kfold
        )
      } else {
        panel_cv_timeseries(
          d, fmls[[tier]], outcome_var, exposure, family,
          model_label = tier,
          n_splits = n_splits,
          test_duration_months = test_duration_months,
          gap_months = gap_months,
          min_train_rows = min_train_rows,
          min_test_rows = min_test_rows_timeseries
        )
      }
      if (!is.null(eng$summary)) {
        tier_rows[[tier]] <- eng$summary %>%
          mutate(config_id = config_id,
                 outcome = outcome_var,
                 climate_var = climate_var)
      }
    }

    tiers <- bind_rows(tier_rows)

    # Delta log-likelihood: climate − seasonal_ops (positive = climate helps).
    if (all(c("seasonal_ops", "climate") %in% tiers$model_label)) {
      ll_climate <- tiers %>% filter(model_label == "climate")  %>% pull(loglik_mean)
      ll_seasonal <- tiers %>% filter(model_label == "seasonal_ops") %>% pull(loglik_mean)
      delta <- tibble(
        cv_strategy = strat,
        model_label = "delta_climate_vs_seasonal",
        loglik_mean = ll_climate - ll_seasonal,
        config_id   = config_id,
        outcome     = outcome_var,
        climate_var = climate_var
      )
      tiers <- bind_rows(tiers, delta)
    }
    out[[strat]] <- tiers
  }

  bind_rows(out)
}


# =============================================================================
# 5. PARALLEL DRIVER
# =============================================================================

#' Run panel CV across (config × climate_var × outcome) in parallel.
#'
#' @param cfg_dir Climate config root (subdir layout).
#' @param panel_data_fn Function(parquet_path, config_id) -> panel tibble.
#' @param outcome_specs List of outcome specs. Each entry: list(
#'   outcome_var, exposure, family, bw_terms, org_var, period_date_var).
#' @param config_ids NULL = discover.
#' @param strategies See run_panel_cv_for_outcome().
#' @param climate_var_base Used to discover windowed columns.
#' @param K, n_splits, test_duration_months, gap_months CV parameters.
#' @param n_workers Parallel workers.
#' @param output_path Optional parquet output.
#' @param worker_source_files Files to source in workers.
run_panel_cv <- function(cfg_dir,
                          panel_data_fn,
                          outcome_specs,
                          config_ids = NULL,
                          strategies = c("group_kfold", "timeseries"),
                          climate_var_base = "overall_final_score",
                          K = 5L, n_splits = 5L,
                          test_duration_months = 24L, gap_months = 6L,
                          min_train_rows = 200L,
                          min_test_rows_kfold = 10L,
                          min_test_rows_timeseries = 20L,
                          n_workers = 4L,
                          output_path = NULL,
                          worker_source_files = character(0),
                          worker_globals = list(),
                          seed = 42L) {

  if (is.null(config_ids)) {
    cfg_path <- file.path(cfg_dir, "_cfg")
    ids <- list.dirs(cfg_path, full.names = FALSE, recursive = FALSE)
    config_ids <- ids[ids != "" & !startsWith(ids, ".")]
  }

  message(glue("\nPanel CV: {length(config_ids)} configs × ",
               "{length(outcome_specs)} outcomes × ",
               "{length(strategies)} strategies"))

  plan(multisession, workers = n_workers)
  on.exit(plan(sequential), add = TRUE)

  results <- future_map(config_ids, function(cid) {
    for (f in worker_source_files) source(f, local = FALSE)

    parquet_path <- file.path(cfg_dir, "_cfg", cid, "results.parquet")
    panel <- tryCatch(
      panel_data_fn(parquet_path = parquet_path, config_id = cid),
      error = function(e) NULL
    )
    if (is.null(panel)) {
      return(tibble(config_id = cid, status = "panel_prep_failed"))
    }

    pattern <- paste0("^", climate_var_base, "_(sma_|ewmaLAG_)")
    climate_vars <- grep(pattern, names(panel), value = TRUE)

    rows <- list()
    for (cv in climate_vars) {
      for (spec_name in names(outcome_specs)) {
        spec <- outcome_specs[[spec_name]]
        fam <- switch(spec$family,
                      nbinom2 = glmmTMB::nbinom2(),
                      tweedie = glmmTMB::tweedie(link = "log"),
                      poisson = stats::poisson(link = "log"))
        r <- tryCatch(
          run_panel_cv_for_outcome(
            panel = panel,
            outcome_var  = spec$outcome_var,
            exposure     = spec$exposure,
            climate_var  = cv,
            bw_terms     = spec$bw_terms,
            org_var      = spec$org_var,
            family       = fam,
            config_id    = cid,
            strategies   = strategies,
            period_date_var = spec$period_date_var,
            K = K, n_splits = n_splits,
            test_duration_months = test_duration_months,
            gap_months = gap_months,
            min_train_rows = min_train_rows,
            min_test_rows_kfold = min_test_rows_kfold,
            min_test_rows_timeseries = min_test_rows_timeseries,
            seed = seed
          ) %>% mutate(panel_outcome = spec_name),
          error = function(e) {
            tibble(config_id = cid, climate_var = cv,
                   panel_outcome = spec_name,
                   status = "cv_failed", error = conditionMessage(e))
          }
        )
        rows[[length(rows) + 1L]] <- r
      }
    }
    bind_rows(rows)
  },
  .options = furrr_options(
    seed = TRUE,
    packages = c("dplyr", "purrr", "tidyr", "tibble", "glue", "arrow",
                 "lubridate", "stringr", "slider",
                 "glmmTMB", "broom.mixed", "tweedie"),
    globals = c(as.list(globalenv()), worker_globals)
  ),
  .progress = TRUE)

  combined <- bind_rows(results)

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(combined, output_path)
    message(glue("Saved: {output_path}"))
  }

  combined
}
