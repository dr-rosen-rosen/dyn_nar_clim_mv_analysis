# 3_rail_cv_plots.R
# Visualization for cross-validation results
# Generates:
#   1. Specification curve plot (Brier scores across all specs)
#   2. Delta-Brier plot (full model - no_climate model)
#   3. Delta-Brier distribution (histogram + density)
#   4. Summary table
#
# Reads CV results parquet from 2_rail_cv_binary.R output.
# Mike Rose - Safety Climate Analysis

library(tidyverse)
library(arrow)
library(patchwork)
library(here)

# Source config for window metadata constants
common_dir <- file.path(here::here(), "common")
if (!dir.exists(common_dir)) common_dir <- "common"
source(file.path(common_dir, "config.R"))


# ==============================================================================
# DATA LOADING
# ==============================================================================

#' Load CV results from parquet
#' @param cv_results_path Path to the CV results parquet file
#' @return Tibble of CV results
load_cv_results <- function(cv_results_path) {
  if (!file.exists(cv_results_path)) {
    stop(sprintf("CV results file not found: %s", cv_results_path))
  }
  arrow::read_parquet(cv_results_path)
}


# ==============================================================================
# SPECIFICATION CURVE: BRIER SCORES
# ==============================================================================

#' Create specification curve plot of Brier scores
#'
#' Top panel: Brier score (mean +/- SD) for each specification, ordered.
#' Bottom panels: Binary indicators showing which methodological choices
#' (window type, window size, config) are active for each specification.
#'
#' @param cv_results Tibble of CV results (from run_cv_all_configs)
#' @param strategy Filter to one CV strategy: "group_kfold" or "timeseries"
#' @param show_baselines If TRUE, overlay no_climate baseline as reference line
#' @param max_specs Maximum number of specifications to show (default: all)
#' @return A patchwork plot object
plot_spec_curve_brier <- function(cv_results,
                                  strategy = "timeseries",
                                  show_baselines = TRUE,
                                  max_specs = NULL) {

  # Filter to the chosen strategy and full-model specifications only
  full_specs <- cv_results %>%
    filter(
      cv_strategy == strategy,
      model_label == climate_var_tested
    ) %>%
    filter(!is.na(brier_score_mean)) %>%
    arrange(brier_score_mean) %>%
    mutate(spec_rank = row_number())

  if (nrow(full_specs) == 0) {
    warning("No full-model specifications found for strategy: ", strategy)
    return(NULL)
  }

  if (!is.null(max_specs) && nrow(full_specs) > max_specs) {
    full_specs <- full_specs %>% head(max_specs)
  }

  n_specs <- nrow(full_specs)

  # Get no-climate baseline for reference
  baseline_brier <- cv_results %>%
    filter(cv_strategy == strategy, model_label == "no_climate") %>%
    pull(brier_score_mean) %>%
    median(na.rm = TRUE)

  intercept_brier <- cv_results %>%
    filter(cv_strategy == strategy, model_label == "intercept_only") %>%
    pull(brier_score_mean) %>%
    median(na.rm = TRUE)

  # --- Top panel: Brier scores ---
  p_brier <- ggplot(full_specs, aes(x = spec_rank, y = brier_score_mean)) +
    geom_point(size = 0.8, alpha = 0.7, color = "#2166AC") +
    geom_errorbar(
      aes(ymin = brier_score_mean - brier_score_sd,
          ymax = brier_score_mean + brier_score_sd),
      width = 0, alpha = 0.15, color = "#2166AC"
    )

  if (show_baselines && !is.na(baseline_brier)) {
    p_brier <- p_brier +
      geom_hline(yintercept = baseline_brier, linetype = "dashed",
                 color = "#B2182B", linewidth = 0.5) +
      annotate("text", x = n_specs * 0.02, y = baseline_brier,
               label = "No-climate baseline", hjust = 0, vjust = -0.5,
               color = "#B2182B", size = 3)
  }

  if (show_baselines && !is.na(intercept_brier)) {
    p_brier <- p_brier +
      geom_hline(yintercept = intercept_brier, linetype = "dotted",
                 color = "#999999", linewidth = 0.4) +
      annotate("text", x = n_specs * 0.02, y = intercept_brier,
               label = "Intercept-only", hjust = 0, vjust = -0.5,
               color = "#999999", size = 2.5)
  }

  strategy_label <- if (strategy == "timeseries") {
    "Time-Series CV (leading indicator test)"
  } else {
    "Group K-Fold CV (cross-organization generalization)"
  }

  p_brier <- p_brier +
    labs(
      title = sprintf("Specification Curve: Brier Score [%s]", strategy_label),
      subtitle = sprintf("%d specifications ordered by Brier score (lower = better)",
                         n_specs),
      y = "Brier Score (mean +/- SD across folds)",
      x = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40")
    )

  # --- Bottom panels: choice indicators ---

  choice_data <- full_specs %>%
    select(spec_rank, window_type, window_size, lag_days, halflife_days, config_id)

  # Window type
  p_window_type <- ggplot(choice_data, aes(x = spec_rank, y = "Window Type",
                                            fill = window_type)) +
    geom_tile(height = 0.8) +
    scale_fill_manual(values = c("sma" = "#4393C3", "ewma" = "#D6604D",
                                 "unknown" = "#CCCCCC"),
                      name = NULL) +
    labs(y = NULL, x = NULL) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.size = unit(0.4, "cm")
    )

  # Window param (SMA size or EWMA halflife)
  choice_data <- choice_data %>%
    mutate(window_param = coalesce(window_size, halflife_days))

  p_window_param <- ggplot(choice_data, aes(x = spec_rank, y = "Window Param",
                                             fill = window_param)) +
    geom_tile(height = 0.8) +
    scale_fill_viridis_c(name = "Size/\nHalflife", option = "C", na.value = "grey80") +
    labs(y = NULL, x = NULL) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.size = unit(0.4, "cm")
    )

  # Config ID indicator
  n_configs <- n_distinct(choice_data$config_id)
  choice_data <- choice_data %>%
    mutate(config_id = as.factor(config_id))

  if (n_configs <= 20) {
    p_config <- ggplot(choice_data, aes(x = spec_rank, y = "Config",
                                         fill = config_id)) +
      geom_tile(height = 0.8) +
      scale_fill_discrete(name = "Config") +
      labs(y = NULL, x = "Specification (ranked by Brier score)") +
      theme_minimal(base_size = 9) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 6)
      )
  } else {
    choice_data <- choice_data %>%
      mutate(config_num = as.numeric(as.factor(config_id)))

    p_config <- ggplot(choice_data, aes(x = spec_rank, y = "Config",
                                         fill = config_num)) +
      geom_tile(height = 0.8) +
      scale_fill_viridis_c(name = "Config", option = "A") +
      labs(y = NULL, x = "Specification (ranked by Brier score)") +
      theme_minimal(base_size = 9) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.key.size = unit(0.4, "cm")
      )
  }

  # Combine with patchwork
  combined <- p_brier / p_window_type / p_window_param / p_config +
    plot_layout(heights = c(4, 1, 1, 1))

  combined
}


# ==============================================================================
# DELTA-BRIER PLOT
# ==============================================================================

#' Create delta-Brier plot: does climate improve prediction?
#'
#' Each point is one specification. delta_brier = brier_full - brier_no_climate.
#' Negative values mean climate helps; positive means climate hurts (overfitting).
#'
#' @param cv_results Tibble of CV results
#' @param strategy "group_kfold" or "timeseries"
#' @param color_by What to color points by: "window_type", "config_id", or NULL
#' @return A ggplot object
plot_delta_brier <- function(cv_results,
                             strategy = "timeseries",
                             color_by = "window_type") {

  plot_data <- cv_results %>%
    filter(
      cv_strategy == strategy,
      model_label == climate_var_tested,
      !is.na(delta_brier)
    ) %>%
    arrange(delta_brier) %>%
    mutate(spec_rank = row_number())

  if (nrow(plot_data) == 0) {
    warning("No delta-Brier data available for strategy: ", strategy)
    return(NULL)
  }

  n_specs <- nrow(plot_data)
  n_helps <- sum(plot_data$delta_brier < 0, na.rm = TRUE)
  n_hurts <- sum(plot_data$delta_brier > 0, na.rm = TRUE)
  pct_helps <- 100 * n_helps / n_specs

  strategy_label <- if (strategy == "timeseries") {
    "Time-Series CV"
  } else {
    "Group K-Fold CV"
  }

  p <- ggplot(plot_data, aes(x = spec_rank, y = delta_brier))

  if (!is.null(color_by) && color_by %in% names(plot_data)) {
    p <- p + geom_point(aes(color = .data[[color_by]]), size = 1.2, alpha = 0.7)

    if (color_by == "window_type") {
      p <- p + scale_color_manual(
        values = c("sma" = "#4393C3", "ewma" = "#D6604D", "unknown" = "#999999"),
        name = "Window"
      )
    }
  } else {
    p <- p + geom_point(size = 1.2, alpha = 0.7, color = "#2166AC")
  }

  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
    annotate("text", x = n_specs * 0.98, y = min(plot_data$delta_brier) * 0.3,
             label = sprintf("Climate helps: %d/%d (%.0f%%)", n_helps, n_specs, pct_helps),
             hjust = 1, size = 3.5, color = "#2166AC") +
    annotate("text", x = n_specs * 0.98, y = max(plot_data$delta_brier) * 0.3,
             label = sprintf("Climate hurts: %d/%d (%.0f%%)", n_hurts, n_specs, 100 - pct_helps),
             hjust = 1, size = 3.5, color = "#B2182B") +
    labs(
      title = sprintf("Delta-Brier: Climate Predictive Value [%s]", strategy_label),
      subtitle = "Brier(full) - Brier(no-climate). Negative = climate improves prediction.",
      x = "Specification (ranked by delta-Brier)",
      y = "Delta Brier Score"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      panel.grid.minor = element_blank()
    )

  p
}


#' Create a delta-Brier distribution plot (histogram + density)
#'
#' @param cv_results Tibble of CV results
#' @param strategy "group_kfold" or "timeseries"
#' @return A ggplot object
plot_delta_brier_distribution <- function(cv_results,
                                          strategy = "timeseries") {

  plot_data <- cv_results %>%
    filter(
      cv_strategy == strategy,
      model_label == climate_var_tested,
      !is.na(delta_brier)
    )

  if (nrow(plot_data) == 0) return(NULL)

  median_delta <- median(plot_data$delta_brier, na.rm = TRUE)
  mean_delta <- mean(plot_data$delta_brier, na.rm = TRUE)

  strategy_label <- if (strategy == "timeseries") "Time-Series CV" else "Group K-Fold CV"

  ggplot(plot_data, aes(x = delta_brier)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 40, fill = "#4393C3", alpha = 0.5, color = "white") +
    geom_density(linewidth = 0.8, color = "#2166AC") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = median_delta, linetype = "solid",
               color = "#B2182B", linewidth = 0.5) +
    annotate("text", x = median_delta, y = Inf,
             label = sprintf("Median: %.4f", median_delta),
             hjust = -0.1, vjust = 1.5, color = "#B2182B", size = 3.5) +
    labs(
      title = sprintf("Distribution of Delta Brier [%s]", strategy_label),
      subtitle = "Across all multiverse specifications",
      x = "Delta Brier (full - no-climate)",
      y = "Density"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40")
    )
}


# ==============================================================================
# SUMMARY TABLE
# ==============================================================================

#' Create a summary table comparing model types across strategies
#'
#' @param cv_results Tibble of CV results
#' @return Tibble summarizing Brier scores by model_label and cv_strategy
summarize_cv_results <- function(cv_results) {

  summary_tbl <- cv_results %>%
    mutate(
      model_type = case_when(
        model_label == "no_climate"     ~ "No Climate",
        model_label == "seasonal_only"  ~ "Seasonal Only",
        model_label == "intercept_only" ~ "Intercept Only",
        model_label == climate_var_tested ~ "Full (with climate)",
        TRUE ~ model_label
      )
    ) %>%
    group_by(cv_strategy, model_type) %>%
    summarise(
      n_specs      = n(),
      brier_mean   = mean(brier_score_mean, na.rm = TRUE),
      brier_sd     = sd(brier_score_mean, na.rm = TRUE),
      brier_median = median(brier_score_mean, na.rm = TRUE),
      brier_q25    = quantile(brier_score_mean, 0.25, na.rm = TRUE),
      brier_q75    = quantile(brier_score_mean, 0.75, na.rm = TRUE),
      auc_mean     = mean(auc_roc_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(cv_strategy, brier_mean)

  summary_tbl
}


# ==============================================================================
# MAIN: GENERATE ALL PLOTS
# ==============================================================================

#' Generate and save all CV visualization outputs
#'
#' @param cv_results_path Path to CV results parquet
#' @param output_dir Directory to save plots
#' @param outcome Outcome label (for filenames)
#' @param width Plot width in inches
#' @param height_spec Height for specification curve plots
#' @param height_delta Height for delta-Brier plots
generate_cv_plots <- function(cv_results_path,
                              output_dir = "results/cv/plots",
                              outcome = "injuries",
                              width = 14,
                              height_spec = 10,
                              height_delta = 6) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  cat("Loading CV results...\n")
  cv_results <- load_cv_results(cv_results_path)
  cat(sprintf("  %d rows loaded\n", nrow(cv_results)))

  strategies <- unique(cv_results$cv_strategy)
  cat(sprintf("  Strategies found: %s\n\n", paste(strategies, collapse = ", ")))

  for (strat in strategies) {

    strat_short <- if (strat == "timeseries") "ts" else "gkf"

    # 1. Specification curve
    cat(sprintf("Generating spec curve [%s]...\n", strat))
    p_spec <- tryCatch(
      plot_spec_curve_brier(cv_results, strategy = strat),
      error = function(e) { warning(e$message); NULL }
    )
    if (!is.null(p_spec)) {
      spec_path <- file.path(output_dir,
                             sprintf("%s_spec_curve_%s.pdf", outcome, strat_short))
      ggsave(spec_path, p_spec, width = width, height = height_spec)
      cat(sprintf("  Saved: %s\n", spec_path))

      # Also PNG for quick viewing
      spec_png <- sub("\\.pdf$", ".png", spec_path)
      ggsave(spec_png, p_spec, width = width, height = height_spec, dpi = 150)
    }

    # 2. Delta-Brier ranked plot
    cat(sprintf("Generating delta-Brier plot [%s]...\n", strat))
    p_delta <- tryCatch(
      plot_delta_brier(cv_results, strategy = strat),
      error = function(e) { warning(e$message); NULL }
    )
    if (!is.null(p_delta)) {
      delta_path <- file.path(output_dir,
                              sprintf("%s_delta_brier_%s.pdf", outcome, strat_short))
      ggsave(delta_path, p_delta, width = width, height = height_delta)
      cat(sprintf("  Saved: %s\n", delta_path))

      delta_png <- sub("\\.pdf$", ".png", delta_path)
      ggsave(delta_png, p_delta, width = width, height = height_delta, dpi = 150)
    }

    # 3. Delta-Brier distribution
    cat(sprintf("Generating delta-Brier distribution [%s]...\n", strat))
    p_dist <- tryCatch(
      plot_delta_brier_distribution(cv_results, strategy = strat),
      error = function(e) { warning(e$message); NULL }
    )
    if (!is.null(p_dist)) {
      dist_path <- file.path(output_dir,
                             sprintf("%s_delta_brier_dist_%s.pdf", outcome, strat_short))
      ggsave(dist_path, p_dist, width = 8, height = 5)
      cat(sprintf("  Saved: %s\n", dist_path))

      dist_png <- sub("\\.pdf$", ".png", dist_path)
      ggsave(dist_png, p_dist, width = 8, height = 5, dpi = 150)
    }
  }

  # 4. Summary table
  cat("\nGenerating summary table...\n")
  summary_tbl <- summarize_cv_results(cv_results)
  summary_path <- file.path(output_dir, sprintf("%s_cv_summary.csv", outcome))
  write_csv(summary_tbl, summary_path)
  cat(sprintf("  Saved: %s\n", summary_path))

  cat("\n--- Summary ---\n")
  print(summary_tbl, n = 20)

  cat("\nAll plots generated.\n")
  invisible(list(summary = summary_tbl))
}


# ==============================================================================
# USAGE EXAMPLE
# ==============================================================================

# generate_cv_plots(
#   cv_results_path = "results/cv/rail_injuries_cv_results.parquet",
#   output_dir = "results/cv/plots",
#   outcome = "injuries"
# )
