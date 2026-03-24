# 3_nrc_plots.R
# NRC Nuclear Multiverse — Visualization and Specification Curves
# Generates:
#   1. Multiverse specification curve (climate effects across all specs)
#   2. CV specification curve (Brier scores)
#   3. Delta-Brier plot (full - no_climate)
#   4. Summary tables
#
# Mike Rose — Safety Climate Analysis

library(tidyverse)
library(arrow)
library(patchwork)
library(here)

# Source config for metadata
common_dir <- file.path(here::here(), "common")
if (!dir.exists(common_dir)) common_dir <- "common"
source(file.path(common_dir, "config.R"))


# ==============================================================================
# HELPERS: Parse specification metadata from climate variable names
# ==============================================================================

#' Extract window type and parameters from climate variable name
#' @param climate_var Character vector of variable names
#' @return Tibble with window_type, window_param columns
parse_window_spec <- function(climate_var) {
  tibble(climate_var = climate_var) %>%
    mutate(
      window_type = case_when(
        str_detect(climate_var, "_sma_")     ~ "SMA",
        str_detect(climate_var, "_ewmaLAG_") ~ "EWMA",
        TRUE                                 ~ "unknown"
      ),
      window_param = case_when(
        window_type == "SMA" ~ str_extract(climate_var, "sma_(\\d+)", group = 1),
        window_type == "EWMA" ~ str_extract(climate_var, "ewmaLAG_(.+)$", group = 1),
        TRUE ~ NA_character_
      )
    )
}


# ==============================================================================
# 1. MULTIVERSE SPECIFICATION CURVE — Climate Effects
# ==============================================================================

#' Specification curve for multiverse results (traditional)
#'
#' Top panel: Climate effect estimate (point + CI) ordered by magnitude
#' Bottom panels: Binary indicators for methodological choices
#'
#' @param results Multiverse results tibble (from 1_nrc_multiverse.R)
#' @param outcome Which outcome to plot
#' @param title Plot title
#' @return patchwork plot object
plot_multiverse_spec_curve <- function(results, outcome = "binary_scram",
                                       title = NULL) {

  df <- results %>%
    filter(outcome == !!outcome, status == "success",
           !is.na(climate_estimate)) %>%
    left_join(parse_window_spec(.$climate_var), by = "climate_var") %>%
    arrange(climate_estimate) %>%
    mutate(spec_rank = row_number())

  if (nrow(df) == 0) {
    message("No successful results for outcome: ", outcome)
    return(NULL)
  }

  if (is.null(title)) {
    title <- paste0("Specification curve: ", get_outcome_config(outcome)$label)
  }

  # Top panel: effect estimates
  p_top <- ggplot(df, aes(x = spec_rank, y = climate_estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = climate_p < 0.05), size = 0.8, alpha = 0.7) +
    geom_errorbar(aes(ymin = climate_ci_low, ymax = climate_ci_high,
                      color = climate_p < 0.05),
                  width = 0, linewidth = 0.3, alpha = 0.4) +
    scale_color_manual(values = c("TRUE" = "#1D9E75", "FALSE" = "#888780"),
                       labels = c("TRUE" = "p < .05", "FALSE" = "p >= .05"),
                       name = NULL) +
    labs(y = "Climate effect estimate", title = title) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          legend.position = "top", panel.grid.major.x = element_blank())

  # Bottom panel: specification indicators
  spec_long <- df %>%
    select(spec_rank, config_id, window_type, window_param) %>%
    pivot_longer(cols = c(config_id, window_type, window_param),
                 names_to = "dimension", values_to = "value")

  p_bottom <- ggplot(spec_long, aes(x = spec_rank, y = value)) +
    geom_point(size = 0.3, alpha = 0.5) +
    facet_wrap(~dimension, ncol = 1, scales = "free_y", strip.position = "left") +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_blank(), axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text.y.left = element_text(angle = 0, hjust = 1))

  p_top / p_bottom + plot_layout(heights = c(3, 2))
}


# ==============================================================================
# 2. CV SPECIFICATION CURVE — Brier Scores
# ==============================================================================

#' Specification curve of Brier scores from CV
#'
#' @param cv_results CV results tibble (from 2_nrc_cv.R)
#' @param cv_strategy "group_kfold" or "timeseries"
#' @param title Plot title
#' @return ggplot object
plot_cv_spec_curve <- function(cv_results, cv_strategy = "group_kfold",
                                title = NULL) {

  df <- cv_results %>%
    filter(cv_strategy == !!cv_strategy, !is.na(brier_mean)) %>%
    left_join(parse_window_spec(.$climate_var), by = "climate_var")

  if (nrow(df) == 0) {
    message("No CV results for strategy: ", cv_strategy)
    return(NULL)
  }

  # Separate by model type for overlay
  df_full <- df %>% filter(model_label == "full") %>%
    arrange(brier_mean) %>% mutate(spec_rank = row_number())

  df_baselines <- df %>% filter(model_label != "full") %>%
    group_by(model_label) %>%
    summarise(brier_mean = mean(brier_mean, na.rm = TRUE),
              brier_sd = mean(brier_sd, na.rm = TRUE),
              .groups = "drop")

  if (is.null(title)) {
    strategy_label <- ifelse(cv_strategy == "group_kfold",
                              "Group K-Fold", "Time-Series")
    title <- paste0("CV Brier scores: ", strategy_label)
  }

  p <- ggplot(df_full, aes(x = spec_rank, y = brier_mean)) +
    geom_point(aes(color = window_type), size = 0.8, alpha = 0.7) +
    geom_errorbar(aes(ymin = brier_mean - brier_sd, ymax = brier_mean + brier_sd),
                  width = 0, linewidth = 0.2, alpha = 0.3) +
    scale_color_manual(values = c("SMA" = "#378ADD", "EWMA" = "#D85A30"),
                       name = "Window type") +
    labs(x = "Specification (ordered)", y = "Brier score (mean +/- SD)",
         title = title) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")

  # Add baseline reference lines
  for (i in seq_len(nrow(df_baselines))) {
    p <- p + geom_hline(
      yintercept = df_baselines$brier_mean[i],
      linetype = "dashed", color = "gray50", linewidth = 0.5
    ) +
    annotate("text", x = max(df_full$spec_rank) * 0.95,
             y = df_baselines$brier_mean[i],
             label = df_baselines$model_label[i],
             hjust = 1, vjust = -0.5, size = 3, color = "gray40")
  }

  return(p)
}


# ==============================================================================
# 3. DELTA-BRIER PLOT
# ==============================================================================

#' Delta-Brier: how much does climate improve over no-climate baseline?
#'
#' @param cv_results CV results tibble
#' @param cv_strategy CV strategy to plot
#' @param title Plot title
#' @return ggplot object
plot_delta_brier <- function(cv_results, cv_strategy = "group_kfold",
                              title = NULL) {

  df <- cv_results %>%
    filter(cv_strategy == !!cv_strategy, !is.na(brier_mean)) %>%
    select(config_id, climate_var, model_label, brier_mean)

  # Pivot to get full - seasonal_ops per specification
  df_wide <- df %>%
    filter(model_label %in% c("full", "seasonal_ops")) %>%
    pivot_wider(names_from = model_label, values_from = brier_mean,
                id_cols = c(config_id, climate_var))

  if (!"full" %in% names(df_wide) || !"seasonal_ops" %in% names(df_wide)) {
    message("Cannot compute delta-Brier: missing model labels")
    return(NULL)
  }

  df_wide <- df_wide %>%
    mutate(delta_brier = full - seasonal_ops) %>%
    filter(!is.na(delta_brier)) %>%
    left_join(parse_window_spec(.$climate_var), by = "climate_var") %>%
    arrange(delta_brier) %>%
    mutate(spec_rank = row_number())

  if (is.null(title)) {
    title <- "Delta-Brier: full model - seasonal+ops baseline"
  }

  n_better <- sum(df_wide$delta_brier < 0)
  n_worse  <- sum(df_wide$delta_brier >= 0)

  ggplot(df_wide, aes(x = spec_rank, y = delta_brier)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_col(aes(fill = delta_brier < 0), width = 0.8, alpha = 0.7) +
    scale_fill_manual(values = c("TRUE" = "#1D9E75", "FALSE" = "#E24B4A"),
                      labels = c("TRUE" = "Climate helps", "FALSE" = "Climate hurts"),
                      name = NULL) +
    labs(x = "Specification (ordered)", y = "Delta Brier (negative = better)",
         title = title,
         subtitle = sprintf("%d/%d specifications improved by climate",
                            n_better, n_better + n_worse)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
}


# ==============================================================================
# 4. SUMMARY TABLES
# ==============================================================================

#' Summary table of multiverse results by outcome
#' @param results Multiverse results tibble
#' @return Summary tibble
summarize_multiverse <- function(results) {
  results %>%
    filter(status == "success", !is.na(climate_estimate)) %>%
    group_by(outcome) %>%
    summarise(
      n_specs          = n(),
      estimate_mean    = mean(climate_estimate, na.rm = TRUE),
      estimate_median  = median(climate_estimate, na.rm = TRUE),
      estimate_sd      = sd(climate_estimate, na.rm = TRUE),
      pct_significant  = mean(climate_p < 0.05, na.rm = TRUE) * 100,
      pct_positive     = mean(climate_estimate > 0, na.rm = TRUE) * 100,
      median_aic       = median(aic, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Summary table of CV results
#' @param cv_results CV results tibble
#' @return Summary tibble
summarize_cv <- function(cv_results) {
  cv_results %>%
    filter(!is.na(brier_mean)) %>%
    group_by(cv_strategy, model_label) %>%
    summarise(
      n_specs      = n(),
      brier_mean   = mean(brier_mean, na.rm = TRUE),
      brier_sd     = sd(brier_mean, na.rm = TRUE),
      auc_mean     = mean(auc_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(cv_strategy, brier_mean)
}


# ==============================================================================
# MAIN: Generate all plots
# ==============================================================================

#' Generate and save all NRC multiverse visualizations
#'
#' @param mv_results_path Path to multiverse results parquet
#' @param cv_results_path Path to CV results parquet (or NULL)
#' @param output_dir Directory to save plots
generate_nrc_plots <- function(mv_results_path,
                                cv_results_path = NULL,
                                output_dir = "results/nrc/plots") {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Load multiverse results
  mv <- arrow::read_parquet(mv_results_path)
  cat(sprintf("Loaded multiverse: %d rows\n", nrow(mv)))

  # Summary table
  mv_summary <- summarize_multiverse(mv)
  print(mv_summary)
  write_csv(mv_summary, file.path(output_dir, "multiverse_summary.csv"))

  # Specification curves by outcome
  for (outcome in unique(mv$outcome)) {
    p <- plot_multiverse_spec_curve(mv, outcome)
    if (!is.null(p)) {
      ggsave(file.path(output_dir, paste0("spec_curve_", outcome, ".pdf")),
             p, width = 10, height = 7)
      ggsave(file.path(output_dir, paste0("spec_curve_", outcome, ".png")),
             p, width = 10, height = 7, dpi = 300)
    }
  }

  # CV plots (if available)
  if (!is.null(cv_results_path) && file.exists(cv_results_path)) {
    cv <- arrow::read_parquet(cv_results_path)
    cat(sprintf("Loaded CV: %d rows\n", nrow(cv)))

    cv_summary <- summarize_cv(cv)
    print(cv_summary)
    write_csv(cv_summary, file.path(output_dir, "cv_summary.csv"))

    for (strategy in c("group_kfold", "timeseries")) {
      # Brier spec curve
      p_brier <- plot_cv_spec_curve(cv, strategy)
      if (!is.null(p_brier)) {
        ggsave(file.path(output_dir, paste0("cv_brier_", strategy, ".pdf")),
               p_brier, width = 10, height = 6)
      }

      # Delta-Brier
      p_delta <- plot_delta_brier(cv, strategy)
      if (!is.null(p_delta)) {
        ggsave(file.path(output_dir, paste0("cv_delta_brier_", strategy, ".pdf")),
               p_delta, width = 10, height = 6)
      }
    }
  }

  cat(sprintf("Plots saved to: %s\n", output_dir))
}

# Uncomment to run:
# generate_nrc_plots(
#   mv_results_path = "results/nrc/nrc_multiverse_results.parquet",
#   cv_results_path = "results/nrc/nrc_cv_results.parquet",
#   output_dir      = "results/nrc/plots"
# )
