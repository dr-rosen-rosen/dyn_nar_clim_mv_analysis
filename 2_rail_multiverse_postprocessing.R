# Railway Multiverse Analysis - Post-processing Functions
# 1. Link results to configuration metadata
# 2. Generate specification curve plots (separate for conditional and hurdle components)
# 3. Handle multiple outcomes (injuries, injuries, damages)

library(tidyverse)
library(arrow)
library(ggplot2)
library(patchwork)

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
#' @param component Which component ("conditional" or "zi")
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
      pct_zi_sig = 100 * mean(climate_zi_pval < 0.05, na.rm = TRUE),
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
#' @param component Which component to analyze ("conditional" or "zi")
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
  if (component == "conditional") {
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
      pct_zi_significant = 100 * mean(climate_zi_pval < 0.05, na.rm = TRUE),
      mean_AIC = mean(AIC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_cond_significant))
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
