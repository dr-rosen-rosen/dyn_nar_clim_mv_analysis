# Railway Multiverse Analysis - Post-processing Functions
# 1. Link results to configuration metadata
# 2. Generate specification curve plots (separate for conditional and hurdle components)
# 3. Handle multiple outcomes (injuries, fatalities, costs)
# 4. Summarize operational covariate effects

library(tidyverse)
library(arrow)
library(ggplot2)
library(patchwork)

# ==============================================================================
# CONFIGURATION - Operational Variables
# ==============================================================================

OPS_VARS <- c("train_miles", "passenger_miles", "staff_hours")

# ==============================================================================
# CONFIGURATION LINKING
# ==============================================================================

#' Link multiverse results to configuration registry
#' @param results_table Results table from multiverse analysis
#' @param config_registry_path Path to config_registry.csv
#' @param select_config_vars Which config variables to include (NULL = all)
link_results_to_config_railway <- function(results_table,
                                            config_registry_path,
                                            select_config_vars = NULL) {
  
  # Read config registry
  config_data <- read_csv(config_registry_path, show_col_types = FALSE)
  
  # Default: select the key configuration variables
  if (is.null(select_config_vars)) {
    select_config_vars <- c(
      "config_id",
      "embedding_model",
      "sent_method",
      "seg__min_length",
      "th__k",
      "th__lambda_",
      "th__method",
      "th__q",
      "comp__apply_over",
      "comp__method",
      "comp__power",
      "comp__sentiment_weight",
      "comp__similarity_weight",
      "comp__temperature"
    )
  }
  
  # Select only needed config columns
  config_subset <- config_data %>%
    select(any_of(select_config_vars))
  
  # Convert config_id to character for joining
  config_subset <- config_subset %>%
    mutate(config_id = as.character(config_id))
  
  results_table <- results_table %>%
    mutate(config_id = as.character(config_id))
  
  # Join
  results_with_config <- results_table %>%
    left_join(config_subset, by = "config_id")
  
  # Report
  n_matched <- sum(!is.na(results_with_config$embedding_model))
  n_total <- nrow(results_with_config)
  cat(sprintf("Linked %d/%d results to config data (%.1f%%)\n",
              n_matched, n_total, 100*n_matched/n_total))
  
  return(results_with_config)
}

# ==============================================================================
# SPECIFICATION CURVE PLOTTING (RAILWAY-SPECIFIC)
# ==============================================================================

#' Create specification curve for one outcome and model component
#' @param results_with_config Results table with config metadata joined
#' @param outcome_filter Which outcome to plot ("injuries", "fatalities", or "costs")
#' @param component Which component to plot ("cond" or "zi")
#' @param sort_by How to sort specifications ("estimate", "AIC", "pval")
plot_specification_curve_railway <- function(results_with_config,
                                              outcome_filter = NULL,
                                              component = "cond",
                                              sort_by = "estimate") {
  
  # Filter to outcome if specified
  if (!is.null(outcome_filter)) {
    plot_data <- results_with_config %>%
      filter(outcome == outcome_filter)
  } else {
    plot_data <- results_with_config
  }
  
  # Select appropriate variables based on component
  if (component == "cond") {
    estimate_var <- "climate_estimate_cond"
    se_var <- "climate_se_cond"
    pval_var <- "climate_pval_cond"
    title_suffix <- "(Conditional Model - Magnitude Given Non-Zero)"
  } else {
    estimate_var <- "climate_estimate_zi"
    se_var <- "climate_se_zi"
    pval_var <- "climate_pval_zi"
    title_suffix <- "(Zero-Inflation/Hurdle Model - Probability of Zero)"
  }
  
  # Remove NAs
  plot_data <- plot_data %>%
    filter(!is.na(.data[[estimate_var]]))
  
  # Sort specifications
  if (sort_by == "estimate") {
    plot_data <- plot_data %>%
      arrange(.data[[estimate_var]])
  } else if (sort_by == "AIC") {
    plot_data <- plot_data %>%
      arrange(AIC)
  } else if (sort_by == "pval") {
    plot_data <- plot_data %>%
      arrange(.data[[pval_var]])
  }
  
  # Add rank and significance
  plot_data <- plot_data %>%
    mutate(
      rank = row_number(),
      significant = .data[[pval_var]] < 0.05,
      ci_lower = .data[[estimate_var]] - 1.96 * .data[[se_var]],
      ci_upper = .data[[estimate_var]] + 1.96 * .data[[se_var]]
    )
  
  # Create plot
  outcome_label <- if (!is.null(outcome_filter)) {
    paste0(tools::toTitleCase(outcome_filter), " - ")
  } else {
    ""
  }
  
  p <- ggplot(plot_data, aes(x = rank, y = .data[[estimate_var]])) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.2, fill = "gray50") +
    geom_point(aes(color = significant), size = 0.8, alpha = 0.7) +
    scale_color_manual(
      values = c("TRUE" = "#2c7bb6", "FALSE" = "gray70"),
      labels = c("ns", "p < 0.05")
    ) +
    labs(
      title = paste0(outcome_label, "Climate Effect ", title_suffix),
      x = "Specification (sorted by estimate)",
      y = "Climate Effect Estimate (95% CI)",
      color = "Significance"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  return(list(
    curve = p,
    data = plot_data
  ))
}

#' Create specification curves for all outcomes and both components
#' @param results_with_config Results with config metadata
plot_all_specification_curves_railway <- function(results_with_config) {
  
  outcomes <- unique(results_with_config$outcome)
  
  plots <- list()
  
  for (outcome in outcomes) {
    # Conditional model
    cond_result <- plot_specification_curve_railway(
      results_with_config,
      outcome_filter = outcome,
      component = "cond"
    )
    plots[[paste0(outcome, "_cond")]] <- cond_result$curve
    
    # Check if hurdle model (has zi estimates)
    has_zi <- results_with_config %>%
      filter(outcome == outcome) %>%
      summarize(has_zi = any(!is.na(climate_estimate_zi))) %>%
      pull(has_zi)
    
    if (has_zi) {
      zi_result <- plot_specification_curve_railway(
        results_with_config,
        outcome_filter = outcome,
        component = "zi"
      )
      plots[[paste0(outcome, "_zi")]] <- zi_result$curve
    }
  }
  
  return(plots)
}

#' Create specification curve with decision panels (railway version)
#' @param results_with_config Results table with config metadata
#' @param outcome_filter Which outcome to plot
#' @param component Which component ("cond" or "zi")
#' @param decision_vars Vector of configuration variables to show in panels
plot_specification_curve_with_panels_railway <- function(
    results_with_config,
    outcome_filter,
    component = "cond",
    decision_vars = c(
      "window_type",
      "embedding_model",
      "sent_method",
      "th__method",
      "comp__method"
    )) {
  
  # Create main curve
  curve_result <- plot_specification_curve_railway(
    results_with_config,
    outcome_filter = outcome_filter,
    component = component
  )
  plot_data <- curve_result$data
  
  # Create decision panels
  decision_panels <- map(decision_vars, function(var) {
    
    # Get unique values
    unique_vals <- plot_data %>%
      pull({{ var }}) %>%
      unique() %>%
      na.omit()
    
    # Create categorical version
    plot_data_panel <- plot_data %>%
      mutate(
        decision_value = factor(.data[[var]], levels = unique_vals)
      )
    
    # Plot
    ggplot(plot_data_panel, aes(x = rank, y = decision_value)) +
      geom_point(size = 0.5, alpha = 0.6) +
      labs(x = NULL, y = var) +
      theme_minimal(base_size = 9) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 8, face = "bold")
      )
  })
  
  # Combine using patchwork
  combined_plot <- curve_result$curve /
    wrap_plots(decision_panels, ncol = 1) +
    plot_layout(heights = c(3, rep(1, length(decision_vars))))
  
  return(combined_plot)
}

# ==============================================================================
# OUTCOME COMPARISON PLOTS
# ==============================================================================

#' Compare effects across outcomes
#' @param results_with_config Results with config metadata
plot_outcome_comparison <- function(results_with_config) {
  
  # Conditional effects by outcome
  p1 <- ggplot(results_with_config,
               aes(x = outcome, y = climate_estimate_cond, 
                   fill = outcome)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_violin(alpha = 0.6) +
    geom_boxplot(width = 0.2, outlier.size = 0.5) +
    labs(
      title = "Climate Effect on Count/Severity (Conditional Model)",
      x = "Outcome Type",
      y = "Climate Effect Estimate"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Zero-inflation effects by outcome (for hurdle models)
  zi_data <- results_with_config %>%
    filter(!is.na(climate_estimate_zi))
  
  if (nrow(zi_data) > 0) {
    p2 <- ggplot(zi_data,
                 aes(x = outcome, y = climate_estimate_zi,
                     fill = outcome)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_violin(alpha = 0.6) +
      geom_boxplot(width = 0.2, outlier.size = 0.5) +
      labs(
        title = "Climate Effect on Zero Probability (Hurdle Model)",
        x = "Outcome Type",
        y = "Climate Effect Estimate"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  } else {
    p2 <- NULL
  }
  
  # Significance rates
  sig_summary <- results_with_config %>%
    group_by(outcome) %>%
    summarize(
      pct_cond_sig = 100 * mean(climate_pval_cond < 0.05, na.rm = TRUE),
      pct_zi_sig = 100 * mean(climate_pval_zi < 0.05, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = starts_with("pct_"),
      names_to = "component",
      values_to = "pct_significant"
    ) %>%
    mutate(
      component = recode(component,
                         "pct_cond_sig" = "Conditional",
                         "pct_zi_sig" = "Zero-Inflation")
    ) %>%
    filter(!is.na(pct_significant))
  
  p3 <- ggplot(sig_summary,
               aes(x = outcome, y = pct_significant, fill = component)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = sprintf("%.1f%%", pct_significant)),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3) +
    labs(
      title = "Percentage of Significant Effects",
      x = "Outcome Type",
      y = "% with p < 0.05",
      fill = "Component"
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(list(
    conditional = p1,
    zero_inflation = p2,
    significance = p3
  ))
}

# ==============================================================================
# WINDOW TYPE COMPARISON (RAILWAY-SPECIFIC)
# ==============================================================================

#' Compare effects across window types for each outcome
#' @param results_with_config Results with config metadata
#' @param outcome_filter Optional outcome filter
plot_window_comparison_railway <- function(results_with_config,
                                            outcome_filter = NULL) {
  
  if (!is.null(outcome_filter)) {
    plot_data <- results_with_config %>%
      filter(outcome == outcome_filter)
  } else {
    plot_data <- results_with_config
  }
  
  # Summary by window and outcome
  window_summary <- plot_data %>%
    group_by(outcome, window_type, climate_var) %>%
    summarize(
      mean_cond_estimate = mean(climate_estimate_cond, na.rm = TRUE),
      se_cond_estimate = sd(climate_estimate_cond, na.rm = TRUE) / sqrt(n()),
      pct_cond_sig = 100 * mean(climate_pval_cond < 0.05, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Plot: Effect by window
  p1 <- ggplot(window_summary,
               aes(x = reorder(climate_var, mean_cond_estimate),
                   y = mean_cond_estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_pointrange(
      aes(ymin = mean_cond_estimate - 1.96*se_cond_estimate,
          ymax = mean_cond_estimate + 1.96*se_cond_estimate,
          color = window_type),
      size = 0.3
    ) +
    geom_text(aes(label = sprintf("%.0f%%", pct_cond_sig), y = Inf),
              vjust = 1.5, size = 2, color = "gray30") +
    facet_wrap(~outcome, scales = "free") +
    labs(
      title = "Mean Conditional Effect by Window (% significant shown)",
      x = "Window",
      y = "Mean Climate Effect (95% CI)",
      color = "Window Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "top"
    )
  
  return(list(
    window_effects = p1,
    summary = window_summary
  ))
}

# ==============================================================================
# CONFIGURATION IMPORTANCE ANALYSIS
# ==============================================================================

#' Calculate variance explained by each configuration choice (railway)
#' @param results_with_config Results with config metadata
#' @param outcome_filter Optional outcome to analyze
#' @param component Which component to analyze ("cond" or "zi")
#' @param config_vars Configuration variables to test
analyze_config_importance_railway <- function(
    results_with_config,
    outcome_filter = NULL,
    component = "cond",
    config_vars = c(
      "window_type",
      "embedding_model",
      "sent_method",
      "th__method",
      "comp__method"
    )) {
  
  # Filter to outcome if specified
  if (!is.null(outcome_filter)) {
    analysis_data <- results_with_config %>%
      filter(outcome == outcome_filter)
  } else {
    analysis_data <- results_with_config
  }
  
  # Select estimate variable
  if (component == "cond") {
    estimate_var <- "climate_estimate_cond"
  } else {
    estimate_var <- "climate_estimate_zi"
  }
  
  # Remove NAs
  analysis_data <- analysis_data %>%
    filter(!is.na(.data[[estimate_var]]))
  
  # Fit models to decompose variance
  variance_results <- map_dfr(config_vars, function(var) {
    
    # Skip if variable has only one level
    n_levels <- n_distinct(analysis_data[[var]], na.rm = TRUE)
    if (n_levels <= 1) {
      return(tibble(
        variable = var,
        r_squared = NA_real_,
        f_statistic = NA_real_,
        p_value = NA_real_,
        n_levels = n_levels
      ))
    }
    
    # Fit ANOVA
    formula_str <- paste(estimate_var, "~", var)
    fit <- lm(as.formula(formula_str), data = analysis_data)
    
    # Extract R²
    r2 <- summary(fit)$r.squared
    
    # F-test
    aov_result <- anova(fit)
    f_stat <- aov_result$`F value`[1]
    p_val <- aov_result$`Pr(>F)`[1]
    
    tibble(
      variable = var,
      r_squared = r2,
      f_statistic = f_stat,
      p_value = p_val,
      n_levels = n_levels
    )
  })
  
  variance_results %>%
    arrange(desc(r_squared)) %>%
    mutate(
      pct_variance = 100 * r_squared,
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
}

# ==============================================================================
# SUMMARY STATISTICS BY CONFIGURATION CHOICE
# ==============================================================================

#' Summarize effect estimates by configuration choices (railway)
#' @param results_with_config Results with config metadata
#' @param group_vars Variables to group by
#' @param outcome_filter Optional outcome filter
summarize_by_config_choice_railway <- function(results_with_config,
                                                group_vars,
                                                outcome_filter = NULL) {
  
  if (!is.null(outcome_filter)) {
    results_with_config <- results_with_config %>%
      filter(outcome == outcome_filter)
  }
  
  results_with_config %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      n = n(),
      mean_cond_estimate = mean(climate_estimate_cond, na.rm = TRUE),
      sd_cond_estimate = sd(climate_estimate_cond, na.rm = TRUE),
      pct_cond_significant = 100 * mean(climate_pval_cond < 0.05, na.rm = TRUE),
      mean_zi_estimate = mean(climate_estimate_zi, na.rm = TRUE),
      pct_zi_significant = 100 * mean(climate_pval_zi < 0.05, na.rm = TRUE),
      mean_AIC = mean(AIC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_cond_significant))
}

# ==============================================================================
# OPERATIONAL COVARIATE ANALYSIS
# ==============================================================================

#' Summarize operational covariate effects across multiverse
#' @param results_with_config Results with config metadata
#' @param outcome_filter Optional outcome filter
#' @param component Which component ("cond" or "zi")
summarize_operational_effects <- function(results_with_config,
                                          outcome_filter = NULL,
                                          component = "cond") {
  
  if (!is.null(outcome_filter)) {
    results_with_config <- results_with_config %>%
      filter(outcome == outcome_filter)
  }
  
  # Build variable names for operational effects
  ops_between_vars <- paste0(OPS_VARS, "_between")
  ops_within_vars <- paste0(OPS_VARS, "_within")
  all_ops_vars <- c(ops_between_vars, ops_within_vars)
  
  # Summarize each operational variable
  ops_summary <- map_dfr(all_ops_vars, function(v) {
    estimate_col <- paste0(v, "_estimate_", component)
    pval_col <- paste0(v, "_pval_", component)
    
    # Check if columns exist
    if (!estimate_col %in% names(results_with_config)) {
      return(tibble(
        variable = v,
        component = component,
        n_models = NA_integer_,
        mean_estimate = NA_real_,
        sd_estimate = NA_real_,
        median_estimate = NA_real_,
        pct_significant = NA_real_,
        pct_positive = NA_real_,
        pct_negative = NA_real_
      ))
    }
    
    tibble(
      variable = v,
      component = component,
      n_models = sum(!is.na(results_with_config[[estimate_col]])),
      mean_estimate = mean(results_with_config[[estimate_col]], na.rm = TRUE),
      sd_estimate = sd(results_with_config[[estimate_col]], na.rm = TRUE),
      median_estimate = median(results_with_config[[estimate_col]], na.rm = TRUE),
      pct_significant = 100 * mean(results_with_config[[pval_col]] < 0.05, na.rm = TRUE),
      pct_positive = 100 * mean(results_with_config[[estimate_col]] > 0, na.rm = TRUE),
      pct_negative = 100 * mean(results_with_config[[estimate_col]] < 0, na.rm = TRUE)
    )
  })
  
  # Add variable type (between vs within)
  ops_summary <- ops_summary %>%
    mutate(
      var_type = case_when(
        str_detect(variable, "_between$") ~ "between",
        str_detect(variable, "_within$") ~ "within",
        TRUE ~ "unknown"
      ),
      base_var = str_remove(variable, "_(between|within)$")
    ) %>%
    arrange(base_var, var_type)
  
  return(ops_summary)
}

#' Plot operational covariate effects
#' @param results_with_config Results with config metadata
#' @param outcome_filter Optional outcome filter
#' @param component Which component ("cond" or "zi")
plot_operational_effects <- function(results_with_config,
                                     outcome_filter = NULL,
                                     component = "cond") {
  
  if (!is.null(outcome_filter)) {
    plot_data <- results_with_config %>%
      filter(outcome == outcome_filter)
  } else {
    plot_data <- results_with_config
  }
  
  # Build variable names
  ops_between_vars <- paste0(OPS_VARS, "_between")
  ops_within_vars <- paste0(OPS_VARS, "_within")
  
  # Reshape data for plotting
  ops_long <- map_dfr(c(ops_between_vars, ops_within_vars), function(v) {
    estimate_col <- paste0(v, "_estimate_", component)
    pval_col <- paste0(v, "_pval_", component)
    
    if (!estimate_col %in% names(plot_data)) return(NULL)
    
    plot_data %>%
      select(config_id, climate_var, all_of(c(estimate_col, pval_col))) %>%
      mutate(
        variable = v,
        estimate = .data[[estimate_col]],
        pval = .data[[pval_col]],
        significant = pval < 0.05
      ) %>%
      select(config_id, climate_var, variable, estimate, pval, significant)
  })
  
  if (nrow(ops_long) == 0) {
    message("No operational effect data available for plotting.")
    return(NULL)
  }
  
  # Add variable type
  ops_long <- ops_long %>%
    mutate(
      var_type = case_when(
        str_detect(variable, "_between$") ~ "Between-org\n(stable scale)",
        str_detect(variable, "_within$") ~ "Within-org\n(fluctuations)",
        TRUE ~ "unknown"
      ),
      base_var = str_remove(variable, "_(between|within)$") %>%
        str_replace_all("_", " ") %>%
        tools::toTitleCase()
    )
  
  # Create violin plot
  p <- ggplot(ops_long, aes(x = base_var, y = estimate, fill = var_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_violin(alpha = 0.6, position = position_dodge(width = 0.8)) +
    geom_boxplot(width = 0.15, position = position_dodge(width = 0.8),
                 outlier.size = 0.5, alpha = 0.8) +
    labs(
      title = sprintf("Operational Covariate Effects (%s model)",
                      ifelse(component == "cond", "Conditional", "Zero-Inflation")),
      subtitle = if (!is.null(outcome_filter)) paste("Outcome:", outcome_filter) else "All outcomes",
      x = "Operational Variable",
      y = "Effect Estimate",
      fill = "Component"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 15, hjust = 1)
    ) +
    scale_fill_brewer(palette = "Set2")
  
  return(p)
}

#' Compare climate vs operational effects
#' @param results_with_config Results with config metadata
#' @param outcome_filter Optional outcome filter
#' @param component Which component ("cond" or "zi")
compare_climate_vs_operational <- function(results_with_config,
                                           outcome_filter = NULL,
                                           component = "cond") {
  
  if (!is.null(outcome_filter)) {
    data <- results_with_config %>%
      filter(outcome == outcome_filter)
  } else {
    data <- results_with_config
  }
  
  # Climate effect
  climate_col <- paste0("climate_estimate_", component)
  climate_pval_col <- paste0("climate_pval_", component)
  
  climate_summary <- tibble(
    variable = "Climate Score",
    var_type = "primary",
    mean_estimate = mean(data[[climate_col]], na.rm = TRUE),
    sd_estimate = sd(data[[climate_col]], na.rm = TRUE),
    pct_significant = 100 * mean(data[[climate_pval_col]] < 0.05, na.rm = TRUE)
  )
  
  # Operational effects
  ops_summary <- summarize_operational_effects(data, outcome_filter = NULL, component = component) %>%
    select(variable, var_type, mean_estimate, sd_estimate, pct_significant)
  
  # Combine
  combined <- bind_rows(climate_summary, ops_summary) %>%
    mutate(
      abs_mean = abs(mean_estimate),
      variable = factor(variable, levels = variable[order(abs_mean, decreasing = TRUE)])
    )
  
  # Create comparison plot
  p <- ggplot(combined, aes(x = variable, y = mean_estimate, fill = var_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_col(alpha = 0.8) +
    geom_errorbar(
      aes(ymin = mean_estimate - sd_estimate, ymax = mean_estimate + sd_estimate),
      width = 0.3, alpha = 0.7
    ) +
    geom_text(aes(label = sprintf("%.0f%%", pct_significant), 
                  y = ifelse(mean_estimate >= 0, mean_estimate + sd_estimate + 0.02,
                            mean_estimate - sd_estimate - 0.02)),
              size = 3, vjust = ifelse(combined$mean_estimate >= 0, -0.5, 1.5)) +
    labs(
      title = sprintf("Effect Size Comparison (%s model)", 
                      ifelse(component == "cond", "Conditional", "Zero-Inflation")),
      subtitle = "Mean estimate ± SD across multiverse; % significant shown",
      x = NULL,
      y = "Mean Effect Estimate",
      fill = "Variable Type"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    ) +
    scale_fill_manual(
      values = c("primary" = "#e41a1c", "between" = "#377eb8", "within" = "#4daf4a"),
      labels = c("primary" = "Climate", "between" = "Between-org", "within" = "Within-org")
    )
  
  return(list(
    plot = p,
    summary = combined
  ))
}

# ==============================================================================
# ROBUSTNESS CHECK: CLIMATE EFFECT WITH/WITHOUT OPERATIONAL CONTROLS
# ==============================================================================

#' Summarize how climate effects vary by operational covariate availability
#' This helps assess robustness of climate findings to operational controls
#' @param results_with_config Results with config metadata
#' @param outcome_filter Optional outcome filter
summarize_climate_robustness <- function(results_with_config,
                                         outcome_filter = NULL) {
  
  if (!is.null(outcome_filter)) {
    data <- results_with_config %>%
      filter(outcome == outcome_filter)
  } else {
    data <- results_with_config
  }
  
  # Check if operational effects are present
  ops_col_check <- paste0(OPS_VARS[1], "_between_estimate_cond")
  has_ops <- ops_col_check %in% names(data) && any(!is.na(data[[ops_col_check]]))
  
  if (!has_ops) {
    message("No operational covariate data found. Cannot assess robustness.")
    return(NULL)
  }
  
  # Summary of climate effects
  summary_df <- data %>%
    summarize(
      n_models = n(),
      
      # Conditional model
      climate_mean_cond = mean(climate_estimate_cond, na.rm = TRUE),
      climate_sd_cond = sd(climate_estimate_cond, na.rm = TRUE),
      climate_median_cond = median(climate_estimate_cond, na.rm = TRUE),
      pct_sig_cond = 100 * mean(climate_pval_cond < 0.05, na.rm = TRUE),
      pct_neg_sig_cond = 100 * mean(climate_pval_cond < 0.05 & climate_estimate_cond < 0, na.rm = TRUE),
      
      # ZI model
      climate_mean_zi = mean(climate_estimate_zi, na.rm = TRUE),
      climate_sd_zi = sd(climate_estimate_zi, na.rm = TRUE),
      climate_median_zi = median(climate_estimate_zi, na.rm = TRUE),
      pct_sig_zi = 100 * mean(climate_pval_zi < 0.05, na.rm = TRUE),
      pct_neg_sig_zi = 100 * mean(climate_pval_zi < 0.05 & climate_estimate_zi < 0, na.rm = TRUE)
    )
  
  return(summary_df)
}

# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================

# # After running multiverse
# results_table <- arrow::read_parquet("results/railway_multiverse/railway_multiverse_results.parquet")
# 
# # Link to config data
# results_full <- link_results_to_config_railway(
#   results_table,
#   config_registry_path = "config_registry.csv"
# )
# 
# # Create specification curves for all outcomes
# all_curves <- plot_all_specification_curves_railway(results_full)
# all_curves$injuries_cond
# all_curves$injuries_zi
# 
# # Create full curve with panels for injuries (conditional model)
# injuries_curve <- plot_specification_curve_with_panels_railway(
#   results_full,
#   outcome_filter = "injuries",
#   component = "cond"
# )
# ggsave("injuries_spec_curve.png", injuries_curve, width = 12, height = 10, dpi = 300)
# 
# # Compare outcomes
# outcome_plots <- plot_outcome_comparison(results_full)
# outcome_plots$conditional
# outcome_plots$significance
# 
# # Compare windows for each outcome
# injuries_windows <- plot_window_comparison_railway(results_full, "injuries")
# injuries_windows$window_effects
# 
# # Analyze configuration importance for injuries
# importance_mort <- analyze_config_importance_railway(
#   results_full,
#   outcome_filter = "injuries",
#   component = "cond"
# )
# print(importance_mort)
# 
# # Summarize by key choices
# by_embedding <- summarize_by_config_choice_railway(
#   results_full,
#   group_vars = c("embedding_model", "window_type"),
#   outcome_filter = "injuries"
# )
# print(by_embedding)
#
# # ============================================================================
# # NEW: Operational covariate analysis
# # ============================================================================
#
# # Summarize operational effects
# ops_summary <- summarize_operational_effects(
#   results_full,
#   outcome_filter = "injuries",
#   component = "cond"
# )
# print(ops_summary)
#
# # Plot operational effects
# ops_plot <- plot_operational_effects(
#   results_full,
#   outcome_filter = "injuries",
#   component = "cond"
# )
# ggsave("injuries_operational_effects.png", ops_plot, width = 10, height = 6, dpi = 300)
#
# # Compare climate vs operational effect sizes
# comparison <- compare_climate_vs_operational(
#   results_full,
#   outcome_filter = "injuries",
#   component = "cond"
# )
# comparison$plot
# print(comparison$summary)
#
# # Check robustness of climate findings
# robustness <- summarize_climate_robustness(results_full, outcome_filter = "injuries")
# print(robustness)
