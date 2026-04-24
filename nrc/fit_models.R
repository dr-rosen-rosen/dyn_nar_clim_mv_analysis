# =============================================================================
# nrc/fit_models.R — NRC Model Fitting Functions
# =============================================================================
#
# Extracted from 1_nrc_multiverse.R. Contains:
#   - fit_binary_scram()        : Binary scram via glmer (binomial)
#   - fit_ordinal_model()       : Ordinal scram/emergency via clmm
#   - fit_power_loss_hurdle()   : Power loss % via glmmTMB hurdle (binomial + Gamma)
#   - fit_binary_power_loss()   : Wrapper: binary power loss via fit_binary_scram
#   - fit_binary_emerg()        : Wrapper: binary emergency via fit_binary_scram
#
# Requires: nrc/config.R sourced first (provides NRC_OPS_VARS, OUTCOME_VARS,
#           MODEL_FORMULAS, get_outcome_config, build_formula)
# Dependencies: lme4, ordinal, glmmTMB, broom.mixed, dplyr, purrr
# =============================================================================


# ==============================================================================
# BINARY SCRAM (glmer)
# ==============================================================================

#' Fit a single binary model (glmer, binomial)
#'
#' Used directly for binary scram, and as the base for fit_binary_power_loss()
#' and fit_binary_emerg() wrappers.
#'
#' Includes dual-optimizer retry (bobyqa → nloptwrap) for convergence robustness.
#'
#' @param data Prepared data frame
#' @param climate_var Name of the windowed climate variable
#' @param config_id Configuration ID
#' @param extra_terms Character vector of additional fixed-effect terms
#' @return Tibble row with model results
fit_binary_scram <- function(data, climate_var, config_id, extra_terms = character(0)) {

  outcome_cfg <- get_outcome_config("binary_scram")
  fml_str <- build_formula(MODEL_FORMULAS$binary_scram, climate_var)

  # Append extra terms if provided
  if (length(extra_terms) > 0) {
    extra_str <- paste(extra_terms, collapse = " + ")
    fml_str <- sub("\\+ \\(1 \\| facility\\)",
                   paste0("+ ", extra_str, " + (1 | facility)"),
                   fml_str)
  }

  # Filter to complete cases
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

  # Drop year levels that would cause separation
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

  # Retry with nloptwrap if bobyqa had issues
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
    tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
    climate_row <- tidy_fit %>% filter(term == climate_var)
    ops_rows <- tidy_fit %>% filter(grepl("_between$|_within$", term))

    aic_val   <- AIC(fit)
    bic_val   <- BIC(fit)
    converged <- is.null(fit@optinfo$conv$lme4$messages)
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
# ORDINAL (clmm)
# ==============================================================================

#' Fit a single ordinal model (clmm) for scram severity or emergency class
#'
#' @param data Prepared data frame
#' @param climate_var Name of the windowed climate variable
#' @param config_id Configuration ID
#' @param outcome "ordinal_scram" or "emerg_class"
#' @param extra_terms Character vector of additional fixed-effect terms
#' @return Tibble row with model results
fit_ordinal_model <- function(data, climate_var, config_id, outcome = "ordinal_scram",
                               extra_terms = character(0)) {

  outcome_cfg <- get_outcome_config(outcome)
  outcome_var <- outcome_cfg$var
  factor_var  <- paste0(outcome_var, "_factor")
  fml_str     <- build_formula(MODEL_FORMULAS[[outcome]], climate_var)

  # Append extra terms
  if (length(extra_terms) > 0) {
    extra_str <- paste(extra_terms, collapse = " + ")
    fml_str <- paste0(fml_str, " + ", extra_str)
  }

  # Complete cases
  ops_cols <- c(paste0(NRC_OPS_VARS, "_between"), paste0(NRC_OPS_VARS, "_within"))
  required_cols <- c(climate_var, factor_var, "facility", "year", ops_cols)
  existing_cols <- intersect(required_cols, names(data))

  model_data <- data %>%
    filter(if_all(all_of(existing_cols), ~ !is.na(.)))

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

  clmm_fml <- as.formula(paste0(fml_str, " + (1 | facility)"))

  tryCatch({
    fit <- ordinal::clmm(clmm_fml, data = model_data)

    coef_tbl <- summary(fit)$coefficients
    coef_df  <- as.data.frame(coef_tbl)
    coef_df$term <- rownames(coef_df)

    climate_row <- coef_df %>% filter(term == climate_var)

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

    # Extra terms (temporal, EWS features)
    if (length(extra_terms) > 0) {
      for (et in extra_terms) {
        et_row <- coef_df %>% filter(term == et)
        if (nrow(et_row) == 1) {
          result[[paste0(et, "_estimate")]] <- et_row$Estimate[1]
          result[[paste0(et, "_se")]]       <- et_row$`Std. Error`[1]
          result[[paste0(et, "_p")]]        <- et_row$`Pr(>|z|)`[1]
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
# POWER LOSS HURDLE (glmmTMB)
# ==============================================================================

#' Fit a hurdle model for power loss percentage
#'
#' Two-part model:
#'   Binary:  P(power_loss > 0) — does any power loss occur?
#'   Gamma:   E(power_loss | power_loss > 0) — how severe?
#'
#' @param data Prepared data frame
#' @param climate_var Name of the windowed climate variable
#' @param config_id Configuration ID
#' @param extra_terms Character vector of additional fixed-effect terms
#' @return Tibble row with model results (both cond and zi estimates)
fit_power_loss_hurdle <- function(data, climate_var, config_id,
                                   extra_terms = character(0)) {

  fmls <- build_formula(MODEL_FORMULAS[["pct_power_loss"]], climate_var)

  complete_data <- data %>%
    filter(
      !is.na(pct_power_loss),
      !is.na(.data[[climate_var]]),
      !is.na(capacity_factor_between),
      !is.na(yearmonth_num_c)
    )

  n_obs <- nrow(complete_data)
  n_pos <- sum(complete_data$pct_power_loss > 0)

  if (n_pos < 50) {
    return(tibble(
      config_id = config_id, climate_var = climate_var,
      outcome = "pct_power_loss", status = "failed",
      error = sprintf("Insufficient positive power loss: %d", n_pos),
      n_obs = n_obs
    ))
  }

  positive_data <- complete_data %>% filter(pct_power_loss > 0)

  # Build formula strings, appending extra terms if provided
  binary_fml_str <- fmls$binary
  gamma_fml_str  <- fmls$gamma

  if (length(extra_terms) > 0) {
    extra_str <- paste(extra_terms, collapse = " + ")
    binary_fml_str <- sub("\\+ \\(1 \\| facility\\)",
                          paste0("+ ", extra_str, " + (1 | facility)"),
                          binary_fml_str)
    gamma_fml_str <- sub("\\+ \\(1 \\| facility\\)",
                         paste0("+ ", extra_str, " + (1 | facility)"),
                         gamma_fml_str)
  }

  fit_binary <- tryCatch(
    suppressWarnings({
      glmmTMB(
        as.formula(binary_fml_str),
        data = complete_data,
        family = binomial(link = "logit")
      )
    }),
    error = function(e) NULL
  )

  fit_gamma <- tryCatch(
    suppressWarnings({
      glmmTMB(
        as.formula(gamma_fml_str),
        data = positive_data,
        family = Gamma(link = "log")
      )
    }),
    error = function(e) NULL
  )

  if (is.null(fit_binary) || is.null(fit_gamma)) {
    return(tibble(
      config_id = config_id, climate_var = climate_var,
      outcome = "pct_power_loss", status = "failed",
      error = "Model fitting failed",
      n_obs = n_obs
    ))
  }

  fixed_binary <- broom.mixed::tidy(fit_binary, effects = "fixed", conf.int = TRUE)
  fixed_gamma  <- broom.mixed::tidy(fit_gamma,  effects = "fixed", conf.int = TRUE)

  safe_extract <- function(df, term_name, col) {
    row <- df %>% filter(term == term_name)
    if (nrow(row) > 0) row[[col]][1] else NA_real_
  }

  conv_binary <- is.null(fit_binary$fit$convergence) || fit_binary$fit$convergence == 0
  conv_gamma  <- is.null(fit_gamma$fit$convergence)  || fit_gamma$fit$convergence == 0
  status <- if (conv_binary && conv_gamma) "success" else "convergence_warning"

  ops_between_vars <- paste0(NRC_OPS_VARS, "_between")
  ops_within_vars  <- paste0(NRC_OPS_VARS, "_within")
  all_ops_vars     <- c(ops_between_vars, ops_within_vars)

  ops_results <- list()
  for (v in all_ops_vars) {
    ops_results[[paste0(v, "_estimate")]] <- safe_extract(fixed_binary, v, "estimate")
    ops_results[[paste0(v, "_p")]]        <- safe_extract(fixed_binary, v, "p.value")
  }

  # Extra terms (temporal, EWS features)
  extra_results <- list()
  if (length(extra_terms) > 0) {
    for (et in extra_terms) {
      # Binary/ZI component
      extra_results[[paste0(et, "_estimate_zi")]] <- safe_extract(fixed_binary, et, "estimate")
      extra_results[[paste0(et, "_se_zi")]]       <- safe_extract(fixed_binary, et, "std.error")
      extra_results[[paste0(et, "_pval_zi")]]     <- safe_extract(fixed_binary, et, "p.value")
      # Gamma/Cond component
      extra_results[[paste0(et, "_estimate_cond")]] <- safe_extract(fixed_gamma, et, "estimate")
      extra_results[[paste0(et, "_se_cond")]]       <- safe_extract(fixed_gamma, et, "std.error")
      extra_results[[paste0(et, "_pval_cond")]]     <- safe_extract(fixed_gamma, et, "p.value")
    }
  }

  tibble(
    config_id           = config_id,
    climate_var         = climate_var,
    outcome             = "pct_power_loss",
    status              = status,
    n_obs               = n_obs,
    n_positive          = n_pos,
    climate_estimate_zi   = safe_extract(fixed_binary, climate_var, "estimate"),
    climate_se_zi         = safe_extract(fixed_binary, climate_var, "std.error"),
    climate_pval_zi       = safe_extract(fixed_binary, climate_var, "p.value"),
    climate_ci_low_zi     = safe_extract(fixed_binary, climate_var, "conf.low"),
    climate_ci_high_zi    = safe_extract(fixed_binary, climate_var, "conf.high"),
    climate_estimate_cond = safe_extract(fixed_gamma, climate_var, "estimate"),
    climate_se_cond       = safe_extract(fixed_gamma, climate_var, "std.error"),
    climate_pval_cond     = safe_extract(fixed_gamma, climate_var, "p.value"),
    climate_ci_low_cond   = safe_extract(fixed_gamma, climate_var, "conf.low"),
    climate_ci_high_cond  = safe_extract(fixed_gamma, climate_var, "conf.high"),
    !!!ops_results,
    !!!extra_results,
    aic_binary = AIC(fit_binary),
    aic_gamma  = AIC(fit_gamma)
  )
}


# ==============================================================================
# STANDALONE BINARY FITTING FUNCTIONS
# ==============================================================================
# These are self-contained — they don't route through fit_binary_scram,
# which avoids the dependency on 'binary_scram' in OUTCOME_VARS.

#' Fit binary model for power loss occurrence (glmer)
#'
#' @param data Prepared data frame with power_loss_binary column
#' @param climate_var Climate variable name
#' @param config_id Configuration ID
#' @param extra_terms Additional fixed-effect terms
#' @return Tibble row with model results
fit_binary_power_loss <- function(data, climate_var, config_id, extra_terms = character(0)) {

  outcome_var <- "power_loss_binary"
  fml_str <- build_formula(MODEL_FORMULAS$emerg_binary, climate_var)
  # Replace LHS with power_loss_binary
  fml_str <- sub("^emerg_binary", outcome_var, fml_str)

  if (length(extra_terms) > 0) {
    extra_str <- paste(extra_terms, collapse = " + ")
    fml_str <- sub("\\+ \\(1 \\| facility\\)",
                   paste0("+ ", extra_str, " + (1 | facility)"),
                   fml_str)
  }

  .fit_binary_glmer(data, fml_str, outcome_var, climate_var, config_id,
                    outcome_label = "power_loss_binary",
                    extra_terms = extra_terms)
}


#' Fit binary model for emergency declaration (glmer)
#'
#' @param data Prepared data frame with emerg_binary column
#' @param climate_var Climate variable name
#' @param config_id Configuration ID
#' @param extra_terms Additional fixed-effect terms
#' @return Tibble row with model results
fit_binary_emerg <- function(data, climate_var, config_id, extra_terms = character(0)) {

  outcome_var <- "emerg_binary"
  fml_str <- build_formula(MODEL_FORMULAS$emerg_binary, climate_var)

  if (length(extra_terms) > 0) {
    extra_str <- paste(extra_terms, collapse = " + ")
    fml_str <- sub("\\+ \\(1 \\| facility\\)",
                   paste0("+ ", extra_str, " + (1 | facility)"),
                   fml_str)
  }

  .fit_binary_glmer(data, fml_str, outcome_var, climate_var, config_id,
                    outcome_label = "emerg_binary",
                    extra_terms = extra_terms)
}


#' Internal: fit a binary glmer model (shared engine for emerg and power_loss)
#'
#' @param data Prepared data frame
#' @param fml_str Formula string (already built with climate_var substituted)
#' @param outcome_var Column name of the binary outcome (0/1)
#' @param climate_var Climate variable name (for coefficient extraction)
#' @param config_id Configuration ID
#' @param outcome_label Label for the results tibble
#' @return Tibble row
.fit_binary_glmer <- function(data, fml_str, outcome_var, climate_var,
                               config_id, outcome_label,
                               extra_terms = character(0)) {

  ops_cols <- c(paste0(NRC_OPS_VARS, "_between"), paste0(NRC_OPS_VARS, "_within"))
  required_cols <- c(climate_var, outcome_var, "facility", "yearmonth_num_c", ops_cols)
  existing_cols <- intersect(required_cols, names(data))

  model_data <- data %>%
    filter(if_all(all_of(existing_cols), ~ !is.na(.)))

  if (nrow(model_data) < 50 || n_distinct(model_data$facility) < 3) {
    return(tibble(
      config_id    = config_id,
      climate_var  = climate_var,
      outcome      = outcome_label,
      status       = "insufficient_data",
      n_obs        = nrow(model_data),
      n_facilities = n_distinct(model_data$facility)
    ))
  }

  # Check for variation in outcome
  n_events <- sum(model_data[[outcome_var]])
  if (n_events < 5 || n_events >= nrow(model_data) - 5) {
    return(tibble(
      config_id    = config_id,
      climate_var  = climate_var,
      outcome      = outcome_label,
      status       = "insufficient_variation",
      n_obs        = nrow(model_data),
      n_facilities = n_distinct(model_data$facility),
      n_events     = n_events,
      event_rate   = mean(model_data[[outcome_var]])
    ))
  }

  warn_messages <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      glmer(
        as.formula(fml_str),
        data = model_data,
        family = binomial,
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
      ),
      warning = function(w) {
        warn_messages <<- c(warn_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  # Retry with nloptwrap if needed
  if (is.null(fit) || length(warn_messages) > 0) {
    warn_retry <- character(0)
    fit_retry <- tryCatch(
      withCallingHandlers(
        glmer(
          as.formula(fml_str),
          data = model_data,
          family = binomial,
          control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxeval = 2e5))
        ),
        warning = function(w) {
          warn_retry <<- c(warn_retry, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
    if (!is.null(fit_retry)) {
      if (is.null(fit) || is.null(fit_retry@optinfo$conv$lme4$messages)) {
        fit <- fit_retry
        warn_messages <- warn_retry
      }
    }
  }

  if (is.null(fit)) {
    return(tibble(
      config_id    = config_id,
      climate_var  = climate_var,
      outcome      = outcome_label,
      status       = "failed",
      error        = paste(warn_messages, collapse = "; "),
      n_obs        = nrow(model_data),
      n_facilities = n_distinct(model_data$facility)
    ))
  }

  tryCatch({
    tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
    climate_row <- tidy_fit %>% filter(term == climate_var)
    ops_rows <- tidy_fit %>% filter(grepl("_between$|_within$", term))

    converged <- is.null(fit@optinfo$conv$lme4$messages)
    is_singular <- isSingular(fit)

    result <- tibble(
      config_id        = config_id,
      climate_var      = climate_var,
      outcome          = outcome_label,
      status           = case_when(
        converged & !is_singular ~ "success",
        converged & is_singular  ~ "singular_fit",
        TRUE                     ~ "convergence_warning"
      ),
      warnings         = paste(warn_messages, collapse = "; "),
      n_obs            = nrow(model_data),
      n_facilities     = n_distinct(model_data$facility),
      n_events         = n_events,
      event_rate       = mean(model_data[[outcome_var]]),
      aic              = AIC(fit),
      bic              = BIC(fit),
      is_singular      = is_singular,
      climate_estimate = climate_row$estimate[1],
      climate_se       = climate_row$std.error[1],
      climate_z        = climate_row$statistic[1],
      climate_p        = climate_row$p.value[1],
      climate_ci_low   = climate_row$conf.low[1],
      climate_ci_high  = climate_row$conf.high[1]
    )

    for (i in seq_len(nrow(ops_rows))) {
      result[[paste0(ops_rows$term[i], "_estimate")]] <- ops_rows$estimate[i]
      result[[paste0(ops_rows$term[i], "_p")]]        <- ops_rows$p.value[i]
    }

    # Extra terms (temporal, EWS features)
    if (length(extra_terms) > 0) {
      for (et in extra_terms) {
        et_row <- tidy_fit %>% filter(term == et)
        if (nrow(et_row) > 0) {
          result[[paste0(et, "_estimate")]] <- et_row$estimate[1]
          result[[paste0(et, "_se")]]       <- et_row$std.error[1]
          result[[paste0(et, "_p")]]        <- et_row$p.value[1]
          if ("conf.low" %in% names(et_row)) {
            result[[paste0(et, "_ci_low")]]  <- et_row$conf.low[1]
            result[[paste0(et, "_ci_high")]] <- et_row$conf.high[1]
          }
        }
      }
    }

    return(result)

  }, error = function(e) {
    tibble(
      config_id    = config_id,
      climate_var  = climate_var,
      outcome      = outcome_label,
      status       = "extraction_failed",
      error        = as.character(e$message),
      n_obs        = nrow(model_data),
      n_facilities = n_distinct(model_data$facility)
    )
  })
}
