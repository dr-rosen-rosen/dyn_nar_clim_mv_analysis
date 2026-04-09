# postprocessing.R
# Unified Multiverse Postprocessing — Works for Both Rail and NRC
#
# Functions:
#   - link_results_to_config()        : Join results to config registry
#   - plot_spec_curve()               : Basic specification curve
#   - plot_spec_curve_with_panels()   : Spec curve + decision indicator panels
#   - plot_outcome_comparison()       : Violin/boxplot across outcomes
#   - plot_window_comparison()        : Effects by window type
#   - analyze_config_importance()     : ANOVA variance decomposition
#   - summarize_by_config_choice()    : Tabular summary by config choice
#   - summarize_operational_effects() : Ops covariate summaries
#   - plot_operational_effects()      : Ops covariate violin plots
#   - compare_climate_vs_operational(): Climate vs ops effect comparison
#   - plot_cv_spec_curve()            : CV Brier score spec curve
#   - plot_delta_brier()              : CV delta-Brier plot
#
# Handles both rail (cond/zi hurdle columns) and NRC (single estimate columns)
# via the `estimate_col` / `se_col` / `pval_col` pattern.
#
# Mike Rose — Safety Climate Analysis

library(tidyverse)
library(arrow)
library(patchwork)


# ==============================================================================
# PLOT STYLING DEFAULTS
# ==============================================================================
# Adjust PLOT_BASE_SIZE to scale all text in figures.
# 11 = good for PDF reports; 14-16 = better for slides/presentations.
# Can be overridden before sourcing: e.g., PLOT_BASE_SIZE <- 16
if (!exists("PLOT_BASE_SIZE")) PLOT_BASE_SIZE <- 11

# Derived sizes (scale proportionally)
PLOT_TITLE_SIZE    <- PLOT_BASE_SIZE + 2
PLOT_SUBTITLE_SIZE <- PLOT_BASE_SIZE - 1
PLOT_PANEL_SIZE    <- PLOT_BASE_SIZE - 2
PLOT_LEGEND_SIZE   <- PLOT_BASE_SIZE - 3
PLOT_LABEL_SIZE    <- (PLOT_BASE_SIZE - 5) * 0.352778  # geom_text size (mm -> pt approx)


# ==============================================================================
# HELPERS
# ==============================================================================

#' Detect column naming convention (rail hurdle vs NRC ordinal)
#' @param df Results data frame
#' @return List with estimate_col, se_col, pval_col
detect_estimate_cols <- function(df, component = NULL) {
  if (!is.null(component) && paste0("climate_estimate_", component) %in% names(df)) {
    # Rail hurdle: climate_estimate_cond / climate_estimate_zi
    list(
      estimate = paste0("climate_estimate_", component),
      se       = paste0("climate_se_", component),
      pval     = paste0("climate_pval_", component),
      style    = "rail"
    )
  } else if ("climate_estimate" %in% names(df)) {
    # NRC ordinal: climate_estimate / climate_se / climate_p
    list(
      estimate = "climate_estimate",
      se       = "climate_se",
      pval     = "climate_p",
      style    = "nrc"
    )
  } else {
    stop("Cannot detect estimate columns. Expected 'climate_estimate' or 'climate_estimate_cond'.")
  }
}

#' Parse window type and parameters from climate variable name
#' @param climate_vars Character vector of unique climate variable names
#' @return Tibble with climate_var, window_type, window_size, lag_days, halflife_days
parse_window_spec <- function(climate_vars) {
  tibble(climate_var = unique(climate_vars)) %>%
    mutate(
      window_type = case_when(
        str_detect(climate_var, "_sma_")     ~ "sma",
        str_detect(climate_var, "_ewmaLAG_") ~ "ewma",
        TRUE                                 ~ "unknown"
      ),
      # SMA window size: sma_3, sma_5, sma_10, sma_20
      window_size = case_when(
        window_type == "sma" ~ as.numeric(str_extract(climate_var, "sma_(\\d+)", group = 1)),
        TRUE                 ~ NA_real_
      ),
      # EWMA lag days: ewmaLAG_360d_hl180d -> 360
      lag_days = case_when(
        window_type == "ewma" ~ as.numeric(str_extract(climate_var, "ewmaLAG_(\\d+)d", group = 1)),
        TRUE                  ~ NA_real_
      ),
      # EWMA halflife days: ewmaLAG_360d_hl180d -> 180
      halflife_days = case_when(
        window_type == "ewma" ~ as.numeric(str_extract(climate_var, "hl(\\d+)d", group = 1)),
        TRUE                  ~ NA_real_
      )
    )
}


# ==============================================================================
# CONFIGURATION LINKING
# ==============================================================================

#' Link multiverse results to configuration registry
#' @param results Results table from multiverse analysis
#' @param config_registry_path Path to config_registry.csv
#' @param select_config_vars Config variables to include (NULL = sensible defaults)
#' @return Results with config metadata joined
link_results_to_config <- function(results, config_registry_path,
                                    select_config_vars = NULL) {

  config_data <- read_csv(config_registry_path, show_col_types = FALSE)

  if (is.null(select_config_vars)) {
    select_config_vars <- c(
      "config_id", "embedding_model", "sent_method",
      "seg__min_length", "th__method", "th__k", "th__q",
      "comp__method", "comp__apply_over",
      "comp__power", "comp__sentiment_weight",
      "comp__similarity_weight", "comp__temperature"
    )
  }

  config_subset <- config_data %>%
    select(any_of(select_config_vars)) %>%
    mutate(config_id = as.character(config_id))

  results <- results %>%
    mutate(config_id = as.character(config_id))

  results_linked <- results %>%
    left_join(config_subset, by = "config_id")

  # Add window metadata
  if ("climate_var" %in% names(results_linked)) {
    window_info <- parse_window_spec(unique(results_linked$climate_var))
    # Drop any pre-existing window columns to avoid .x/.y suffixes
    window_cols <- setdiff(names(window_info), "climate_var")
    existing_overlap <- intersect(window_cols, names(results_linked))
    if (length(existing_overlap) > 0) {
      results_linked <- results_linked %>% select(-all_of(existing_overlap))
    }
    results_linked <- results_linked %>%
      left_join(window_info, by = "climate_var")
  }

  n_matched <- sum(!is.na(results_linked$embedding_model))
  cat(sprintf("Linked %d/%d results to config data (%.1f%%)\n",
              n_matched, nrow(results_linked), 100 * n_matched / nrow(results_linked)))

  return(results_linked)
}


# ==============================================================================
# SPECIFICATION CURVE — Basic
# ==============================================================================

#' Create specification curve plot
#'
#' @param df Results with config metadata (from link_results_to_config)
#' @param outcome_filter Outcome to plot (NULL = all)
#' @param component "cond" or "zi" for rail; NULL for NRC
#' @param sort_by How to sort: "estimate", "AIC", "pval"
#' @return List with $curve (ggplot) and $data (sorted tibble)
plot_spec_curve <- function(df, outcome_filter = NULL, component = NULL,
                             sort_by = "estimate") {

  if (!is.null(outcome_filter)) {
    df <- df %>% filter(outcome == outcome_filter)
  }

  cols <- detect_estimate_cols(df, component)

  plot_data <- df %>%
    filter(status == "success", !is.na(.data[[cols$estimate]])) %>%
    mutate(
      .estimate = .data[[cols$estimate]],
      .se       = .data[[cols$se]],
      .pval     = .data[[cols$pval]],
      .sig      = .pval < 0.05,
      .ci_low   = .estimate - 1.96 * .se,
      .ci_high  = .estimate + 1.96 * .se
    )

  if (sort_by == "estimate") {
    plot_data <- plot_data %>% arrange(.estimate)
  } else if (sort_by == "AIC" && "aic" %in% names(plot_data)) {
    plot_data <- plot_data %>% arrange(aic)
  } else if (sort_by == "pval") {
    plot_data <- plot_data %>% arrange(.pval)
  }

  plot_data <- plot_data %>% mutate(rank = row_number())

  # Build title
  outcome_label <- if (!is.null(outcome_filter)) paste0(tools::toTitleCase(outcome_filter), " - ") else ""
  component_label <- if (!is.null(component)) {
    ifelse(component == "cond", "(Conditional)", "(Zero-Inflation)")
  } else ""

  p <- ggplot(plot_data, aes(x = rank, y = .estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
    geom_ribbon(aes(ymin = .ci_low, ymax = .ci_high), alpha = 0.2, fill = "gray50") +
    geom_point(aes(color = .sig), size = 0.8, alpha = 0.7) +
    scale_color_manual(
      values = c("TRUE" = "#2c7bb6", "FALSE" = "gray70"),
      labels = c("TRUE" = "p < 0.05", "FALSE" = "ns"),
      name = "Significance"
    ) +
    labs(
      title = paste0(outcome_label, "Climate Effect Specification Curve ", component_label),
      x = "Specification (sorted by estimate)",
      y = "Climate Effect Estimate (95% CI)"
    ) +
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    theme(panel.grid.minor = element_blank(), legend.position = "top")

  return(list(curve = p, data = plot_data))
}


# ==============================================================================
# SPECIFICATION CURVE — With Decision Panels
# ==============================================================================

#' Specification curve with colored decision tile panels below
#'
#' Matches the publication-style layout: main effect curve on top,
#' colored tile strips below showing which analytical choice is active
#' for each specification. Each strip has its own legend to the right.
#'
#' Default decision_vars includes window parameters parsed from the climate
#' variable name (window_type, window_size, lag_days) plus config registry
#' variables (embedding_model, sent_method, comp__method).
#'
#' @param df Results with config metadata (from link_results_to_config)
#' @param outcome_filter Outcome to plot
#' @param component "cond"/"zi" for rail, NULL for NRC
#' @param decision_vars Config variables to show in panels.
#'   Variables "window_type", "window_size", "lag_days", "halflife_days"
#'   are auto-generated from parse_window_spec if not already in df.
#' @param sort_by How to sort: "estimate", "AIC", "pval"
#' @return Combined patchwork plot
plot_spec_curve_with_panels <- function(df, outcome_filter = NULL,
                                         component = NULL,
                                         decision_vars = NULL,
                                         sort_by = "estimate") {

  # Default decision vars — auto-detect what's available
  if (is.null(decision_vars)) {
    # Always include window params
    decision_vars <- c("window_type", "window_size", "lag_days")
    # Add config vars that exist
    config_candidates <- c("embedding_model", "sent_method", "comp__method",
                           "th__method")
    decision_vars <- c(decision_vars, intersect(config_candidates, names(df)))
  }

  # Get the main curve data (sorted, ranked)
  curve_result <- plot_spec_curve(df, outcome_filter, component, sort_by)
  plot_data <- curve_result$data

  if (nrow(plot_data) == 0) return(NULL)

  # Ensure window spec columns are present
  if (!"window_size" %in% names(plot_data) && "climate_var" %in% names(plot_data)) {
    window_info <- parse_window_spec(unique(plot_data$climate_var))
    # Drop any overlapping columns before joining
    existing_window_cols <- intersect(names(window_info), names(plot_data))
    existing_window_cols <- setdiff(existing_window_cols, "climate_var")
    if (length(existing_window_cols) > 0) {
      plot_data <- plot_data %>% select(-all_of(existing_window_cols))
    }
    plot_data <- plot_data %>%
      left_join(window_info, by = "climate_var")
  }

  # Only use decision vars that actually exist in the data
  decision_vars <- intersect(decision_vars, names(plot_data))

  if (length(decision_vars) == 0) {
    message("No decision variables available for panels")
    return(curve_result$curve)
  }

  # Remove the x-axis from the main curve (shared with panels below)
  p_top <- curve_result$curve +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())

  # Create colored tile strip for each decision variable
  panel_plots <- map(decision_vars, function(var) {

    # Clean up label for y-axis
    var_label <- var %>%
      str_replace_all("__", ": ") %>%
      str_replace_all("_", " ") %>%
      tools::toTitleCase()

    panel_data <- plot_data %>%
      mutate(.value = as.character(.data[[var]])) %>%
      filter(!is.na(.value)) %>%
      mutate(.value = {
        nums <- suppressWarnings(as.numeric(.value))
        if (all(!is.na(nums))) {
          factor(.value, levels = as.character(sort(unique(nums))))
        } else {
          factor(.value, levels = sort(unique(.value)))
        }
      })
    
    ggplot(panel_data, aes(x = rank, y = 1, fill = .value)) +
      geom_tile(width = 1, height = 1) +
      scale_y_continuous(breaks = NULL) +
      labs(x = NULL, y = var_label, fill = NULL) +
      guides(fill = guide_legend(
        ncol = 1,
        keywidth = unit(0.4, "cm"),
        keyheight = unit(0.4, "cm")
      )) +
      theme_minimal(base_size = PLOT_PANEL_SIZE) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = PLOT_PANEL_SIZE, face = "bold", angle = 0,
                                    vjust = 0.5, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.justification = "left",
        legend.box.just = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = PLOT_LEGEND_SIZE),
        legend.key.size = unit(PLOT_BASE_SIZE * 0.032, "cm"),
        plot.margin = margin(0, 0, 0, 0)
      )
  })

  # Add x-axis label to the last panel only
  n_panels <- length(panel_plots)
  panel_plots[[n_panels]] <- panel_plots[[n_panels]] +
    labs(x = "Specification (sorted by estimate)") +
    theme(axis.title.x = element_text(size = PLOT_BASE_SIZE))

  # Combine: main curve on top (tall), strips below (thin)
  heights <- c(4, rep(0.6, n_panels))

  combined <- p_top
  for (pp in panel_plots) {
    combined <- combined / pp
  }
  combined <- combined + plot_layout(heights = heights)

  return(combined)
}


# ==============================================================================
# OUTCOME COMPARISON
# ==============================================================================

#' Compare climate effects across outcomes
#' @param df Results with config metadata
#' @param component "cond"/"zi" for rail, NULL for NRC
#' @return List of ggplot objects
plot_outcome_comparison <- function(df, component = NULL) {

  cols <- detect_estimate_cols(df, component)

  plot_data <- df %>%
    filter(status == "success", !is.na(.data[[cols$estimate]]))

  # Violin + boxplot
  p_effects <- ggplot(plot_data, aes(x = outcome, y = .data[[cols$estimate]], fill = outcome)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_violin(alpha = 0.6) +
    geom_boxplot(width = 0.2, outlier.size = 0.5) +
    labs(title = "Climate Effect Distribution by Outcome",
         x = "Outcome", y = "Climate Effect Estimate") +
    theme_minimal() +
    theme(legend.position = "none")

  # Significance rates
  sig_summary <- plot_data %>%
    group_by(outcome) %>%
    summarize(
      pct_sig = 100 * mean(.data[[cols$pval]] < 0.05, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  p_sig <- ggplot(sig_summary, aes(x = outcome, y = pct_sig, fill = outcome)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", pct_sig, n)),
              vjust = -0.3, size = PLOT_LABEL_SIZE + 0.5) +
    labs(title = "% Significant Effects by Outcome",
         x = "Outcome", y = "% with p < 0.05") +
    theme_minimal() +
    theme(legend.position = "none")

  return(list(effects = p_effects, significance = p_sig))
}


# ==============================================================================
# WINDOW COMPARISON
# ==============================================================================

#' Compare effects across window types
#' @param df Results with config metadata
#' @param outcome_filter Optional outcome filter
#' @param component "cond"/"zi" for rail, NULL for NRC
#' @return List with $plot and $summary
plot_window_comparison <- function(df, outcome_filter = NULL, component = NULL) {

  if (!is.null(outcome_filter)) df <- df %>% filter(outcome == outcome_filter)

  cols <- detect_estimate_cols(df, component)

  window_summary <- df %>%
    filter(status == "success") %>%
    group_by(outcome, window_type, climate_var) %>%
    summarize(
      mean_estimate = mean(.data[[cols$estimate]], na.rm = TRUE),
      se_estimate   = sd(.data[[cols$estimate]], na.rm = TRUE) / sqrt(n()),
      pct_sig       = 100 * mean(.data[[cols$pval]] < 0.05, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  p <- ggplot(window_summary,
              aes(x = reorder(climate_var, mean_estimate), y = mean_estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_pointrange(
      aes(ymin = mean_estimate - 1.96 * se_estimate,
          ymax = mean_estimate + 1.96 * se_estimate,
          color = window_type),
      size = 0.3
    ) +
    geom_text(aes(label = sprintf("%.0f%%", pct_sig), y = Inf),
              vjust = 1.5, size = 2, color = "gray30") +
    facet_wrap(~outcome, scales = "free") +
    labs(
      title = "Mean Climate Effect by Window (% significant shown)",
      x = "Window", y = "Mean Climate Effect (95% CI)",
      color = "Window Type"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          legend.position = "top")

  return(list(plot = p, summary = window_summary))
}


# ==============================================================================
# CONFIGURATION IMPORTANCE (ANOVA decomposition)
# ==============================================================================

#' Variance explained by each configuration choice
#' @param df Results with config metadata
#' @param outcome_filter Optional outcome filter
#' @param component "cond"/"zi" for rail, NULL for NRC
#' @param config_vars Config variables to test
#' @return Tibble with R², F-stat, p-value per variable
analyze_config_importance <- function(df, outcome_filter = NULL,
                                      component = NULL,
                                      config_vars = c(
                                        "window_type", "embedding_model",
                                        "sent_method", "comp__method"
                                      )) {

  if (!is.null(outcome_filter)) df <- df %>% filter(outcome == outcome_filter)

  cols <- detect_estimate_cols(df, component)
  estimate_var <- cols$estimate

  analysis_data <- df %>% filter(!is.na(.data[[estimate_var]]))

  # Only test vars that exist and have > 1 level
  config_vars <- intersect(config_vars, names(analysis_data))

  variance_results <- map_dfr(config_vars, function(var) {
    n_levels <- n_distinct(analysis_data[[var]], na.rm = TRUE)
    if (n_levels <= 1) {
      return(tibble(variable = var, r_squared = NA_real_,
                    f_statistic = NA_real_, p_value = NA_real_, n_levels = n_levels))
    }

    fit <- lm(as.formula(paste(estimate_var, "~", var)), data = analysis_data)
    r2 <- summary(fit)$r.squared
    aov_result <- anova(fit)

    tibble(
      variable    = var,
      r_squared   = r2,
      f_statistic = aov_result$`F value`[1],
      p_value     = aov_result$`Pr(>F)`[1],
      n_levels    = n_levels
    )
  })

  variance_results %>%
    arrange(desc(r_squared)) %>%
    mutate(
      pct_variance = 100 * r_squared,
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE            ~ ""
      )
    )
}


# ==============================================================================
# SUMMARY BY CONFIG CHOICE
# ==============================================================================

#' Tabular summary of effects by configuration choices
#' @param df Results with config metadata
#' @param group_vars Variables to group by
#' @param outcome_filter Optional outcome filter
#' @param component "cond"/"zi" for rail, NULL for NRC
summarize_by_config_choice <- function(df, group_vars, outcome_filter = NULL,
                                        component = NULL) {

  if (!is.null(outcome_filter)) df <- df %>% filter(outcome == outcome_filter)

  cols <- detect_estimate_cols(df, component)

  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      n = n(),
      mean_estimate    = mean(.data[[cols$estimate]], na.rm = TRUE),
      sd_estimate      = sd(.data[[cols$estimate]], na.rm = TRUE),
      pct_significant  = 100 * mean(.data[[cols$pval]] < 0.05, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_significant))
}


# ==============================================================================
# OPERATIONAL COVARIATE ANALYSIS
# ==============================================================================

#' Summarize operational covariate effects across multiverse
#' @param df Results with config metadata
#' @param ops_vars Vector of base operational variable names
#' @param outcome_filter Optional outcome filter
#' @param component "cond"/"zi" for rail, NULL for NRC
summarize_operational_effects <- function(df, ops_vars = NULL,
                                          outcome_filter = NULL,
                                          component = NULL) {

  if (!is.null(outcome_filter)) df <- df %>% filter(outcome == outcome_filter)

  # Auto-detect ops vars from column names
  if (is.null(ops_vars)) {
    est_cols <- names(df)[grepl("_between_estimate|_within_estimate", names(df))]
    if (length(est_cols) == 0) {
      # NRC style: action_matrix_col_between_estimate
      est_cols <- names(df)[grepl("_between_estimate$|_within_estimate$", names(df))]
    }
    ops_vars_full <- str_remove(est_cols, "_estimate.*$")
  } else {
    ops_vars_full <- c(paste0(ops_vars, "_between"), paste0(ops_vars, "_within"))
  }

  map_dfr(ops_vars_full, function(v) {
    # Try multiple naming patterns
    est_col <- NULL
    p_col   <- NULL

    for (suffix in c("_estimate", paste0("_estimate_", component))) {
      candidate <- paste0(v, suffix)
      if (candidate %in% names(df)) { est_col <- candidate; break }
    }
    for (suffix in c("_p", paste0("_pval_", component), paste0("_p_", component))) {
      candidate <- paste0(v, suffix)
      if (candidate %in% names(df)) { p_col <- candidate; break }
    }

    if (is.null(est_col)) return(NULL)

    tibble(
      variable       = v,
      var_type       = ifelse(str_detect(v, "_between$"), "between", "within"),
      base_var       = str_remove(v, "_(between|within)$"),
      n_models       = sum(!is.na(df[[est_col]])),
      mean_estimate  = mean(df[[est_col]], na.rm = TRUE),
      sd_estimate    = sd(df[[est_col]], na.rm = TRUE),
      median_estimate = median(df[[est_col]], na.rm = TRUE),
      pct_significant = if (!is.null(p_col)) 100 * mean(df[[p_col]] < 0.05, na.rm = TRUE) else NA_real_,
      pct_positive   = 100 * mean(df[[est_col]] > 0, na.rm = TRUE),
      pct_negative   = 100 * mean(df[[est_col]] < 0, na.rm = TRUE)
    )
  })
}


#' Plot operational covariate effects
#' @param df Results with config metadata
#' @param ops_vars Base operational variable names
#' @param outcome_filter Optional outcome filter
#' @param component "cond"/"zi" for rail, NULL for NRC
plot_operational_effects <- function(df, ops_vars = NULL,
                                     outcome_filter = NULL,
                                     component = NULL) {

  if (!is.null(outcome_filter)) df <- df %>% filter(outcome == outcome_filter)

  # Auto-detect ops estimate columns
  est_pattern <- if (!is.null(component)) {
    paste0("_estimate_", component, "$")
  } else {
    "_estimate$"
  }

  ops_est_cols <- names(df)[grepl("(between|within)", names(df)) & grepl(est_pattern, names(df))]

  if (length(ops_est_cols) == 0) {
    message("No operational effect columns found for plotting.")
    return(NULL)
  }

  # Reshape to long
  ops_long <- map_dfr(ops_est_cols, function(col) {
    var_name <- str_remove(col, est_pattern)
    df %>%
      transmute(
        config_id, climate_var,
        variable = var_name,
        estimate = .data[[col]]
      )
  }) %>%
    filter(!is.na(estimate)) %>%
    mutate(
      var_type = ifelse(str_detect(variable, "_between"), "Between-org", "Within-org"),
      base_var = str_remove(variable, "_(between|within)") %>%
        str_replace_all("_", " ") %>%
        tools::toTitleCase()
    )

  if (nrow(ops_long) == 0) return(NULL)

  ggplot(ops_long, aes(x = base_var, y = estimate, fill = var_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_violin(alpha = 0.6, position = position_dodge(width = 0.8)) +
    geom_boxplot(width = 0.15, position = position_dodge(width = 0.8),
                 outlier.size = 0.5, alpha = 0.8) +
    labs(
      title = "Operational Covariate Effects Across Multiverse",
      subtitle = if (!is.null(outcome_filter)) paste("Outcome:", outcome_filter) else "All outcomes",
      x = "Operational Variable", y = "Effect Estimate", fill = "Component"
    ) +
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    theme(legend.position = "top", axis.text.x = element_text(angle = 15, hjust = 1)) +
    scale_fill_brewer(palette = "Set2")
}


#' Compare climate vs operational effect sizes
#' @param df Results with config metadata
#' @param ops_vars Base operational variable names
#' @param outcome_filter Optional outcome filter
#' @param component "cond"/"zi" for rail, NULL for NRC
compare_climate_vs_operational <- function(df, ops_vars = NULL,
                                            outcome_filter = NULL,
                                            component = NULL) {

  if (!is.null(outcome_filter)) df <- df %>% filter(outcome == outcome_filter)

  cols <- detect_estimate_cols(df, component)

  climate_summary <- tibble(
    variable       = "Climate Score",
    var_type       = "primary",
    mean_estimate  = mean(df[[cols$estimate]], na.rm = TRUE),
    sd_estimate    = sd(df[[cols$estimate]], na.rm = TRUE),
    pct_significant = 100 * mean(df[[cols$pval]] < 0.05, na.rm = TRUE)
  )

  ops_summary <- summarize_operational_effects(df, ops_vars, component = component) %>%
    select(variable, var_type, mean_estimate, sd_estimate, pct_significant)

  combined <- bind_rows(climate_summary, ops_summary) %>%
    mutate(
      abs_mean = abs(mean_estimate),
      variable = factor(variable, levels = unique(variable[order(abs_mean)]))
    )

  # Compute offset for text labels based on data range
  range_max <- max(abs(combined$mean_estimate) + combined$sd_estimate, na.rm = TRUE)
  text_offset <- range_max * 0.05

  p <- ggplot(combined, aes(x = variable, y = mean_estimate, fill = var_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_col(alpha = 0.8) +
    geom_errorbar(
      aes(ymin = mean_estimate - sd_estimate, ymax = mean_estimate + sd_estimate),
      width = 0.3, alpha = 0.7
    ) +
    geom_text(aes(label = sprintf("%.0f%%", pct_significant),
                  y = ifelse(mean_estimate >= 0,
                             mean_estimate + sd_estimate + text_offset,
                             mean_estimate - sd_estimate - text_offset)),
              size = PLOT_LABEL_SIZE, hjust = ifelse(combined$mean_estimate >= 0, -0.1, 1.1),
              color = "gray30") +
    coord_flip() +
    labs(
      title = "Effect Size Comparison: Climate vs Operational Covariates",
      subtitle = "Mean estimate +/- SD across multiverse; % significant shown",
      x = NULL, y = "Mean Effect Estimate", fill = "Variable Type"
    ) +
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    theme(legend.position = "top", panel.grid.major.y = element_blank()) +
    scale_fill_manual(
      values = c("primary" = "#e41a1c", "between" = "#377eb8", "within" = "#4daf4a"),
      labels = c("primary" = "Climate", "between" = "Between-org", "within" = "Within-org")
    )

  return(list(plot = p, summary = combined))
}


# ==============================================================================
# CV SPECIFICATION CURVES
# ==============================================================================

#' Normalize CV result column names across rail and NRC conventions
#'
#' Rail uses: brier_score_mean, brier_score_sd, climate_var_tested
#' NRC uses:  brier_score_mean (after fix), climate_var + climate_var_tested
#' Legacy:    brier_mean, brier_sd (pre-harmonization NRC)
#'
#' This function ensures a consistent set of columns for downstream plotting.
#' @param cv_results CV results tibble
#' @return CV results with normalized column names
normalize_cv_columns <- function(cv_results) {
  # Brier: accept brier_mean or brier_score_mean -> standardize to brier_mean
  if ("brier_score_mean" %in% names(cv_results) && !"brier_mean" %in% names(cv_results)) {
    cv_results <- cv_results %>% rename(brier_mean = brier_score_mean)
  }
  if ("brier_score_sd" %in% names(cv_results) && !"brier_sd" %in% names(cv_results)) {
    cv_results <- cv_results %>% rename(brier_sd = brier_score_sd)
  }

  # Climate var: ensure climate_var exists (rail uses climate_var_tested)
  if (!"climate_var" %in% names(cv_results) && "climate_var_tested" %in% names(cv_results)) {
    cv_results <- cv_results %>% rename(climate_var = climate_var_tested)
  }

  # Model label: rail uses climate_var name as "full" label; NRC uses "full"
  # Normalize: if model_label matches climate_var, remap to "full"
  if ("climate_var" %in% names(cv_results) && "model_label" %in% names(cv_results)) {
    cv_results <- cv_results %>%
      mutate(model_label = ifelse(model_label == climate_var, "full", model_label))
  }

  cv_results
}

#' Specification curve of CV Brier scores
#' @param cv_results CV results tibble
#' @param cv_strategy "group_kfold" or "timeseries"
#' @return ggplot object
plot_cv_spec_curve <- function(cv_results, cv_strategy = "group_kfold") {

  cv_results <- normalize_cv_columns(cv_results)

  df <- cv_results %>%
    filter(cv_strategy == !!cv_strategy, !is.na(brier_mean))

  if (nrow(df) == 0) {
    message("No CV results for strategy: ", cv_strategy)
    return(NULL)
  }

  # Add window info
  if ("climate_var" %in% names(df)) {
    window_info <- parse_window_spec(unique(df$climate_var))
    df <- df %>% left_join(window_info, by = "climate_var")
  }

  df_full <- df %>%
    filter(model_label == "full") %>%
    arrange(brier_mean) %>%
    mutate(spec_rank = row_number())

  df_baselines <- df %>%
    filter(model_label != "full") %>%
    group_by(model_label) %>%
    summarise(brier_mean = mean(brier_mean, na.rm = TRUE), .groups = "drop")

  strategy_label <- ifelse(cv_strategy == "group_kfold", "Group K-Fold", "Time-Series")

  p <- ggplot(df_full, aes(x = spec_rank, y = brier_mean)) +
    geom_point(aes(color = if ("window_type" %in% names(df_full)) window_type else NULL),
               size = 0.8, alpha = 0.7) +
    labs(x = "Specification (ordered)", y = "Brier score (mean)",
         title = paste0("CV Brier Scores: ", strategy_label)) +
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    theme(legend.position = "top")

  for (i in seq_len(nrow(df_baselines))) {
    p <- p +
      geom_hline(yintercept = df_baselines$brier_mean[i],
                 linetype = "dashed", color = "gray50") +
      annotate("text", x = max(df_full$spec_rank) * 0.95,
               y = df_baselines$brier_mean[i],
               label = df_baselines$model_label[i],
               hjust = 1, vjust = -0.5, size = PLOT_LABEL_SIZE + 0.5, color = "gray40")
  }

  return(p)
}


#' Specification curve of CV Brier scores with decision indicator panels
#'
#' Same layout as plot_spec_curve_with_panels but for CV Brier scores:
#' main Brier curve on top with baseline reference lines, tile strip panels below.
#'
#' @param cv_results CV results tibble (should include config metadata columns)
#' @param cv_strategy "group_kfold" or "timeseries"
#' @param decision_vars Config variables to show in panels (NULL = auto-detect)
#' @param config_registry_path Optional: path to config_registry.csv to link metadata.
#'   If NULL, assumes config metadata is already joined to cv_results.
#' @return Combined patchwork plot, or NULL
plot_cv_spec_curve_with_panels <- function(cv_results, cv_strategy = "group_kfold",
                                            decision_vars = NULL,
                                            config_registry_path = NULL) {

  cv_results <- normalize_cv_columns(cv_results)

  # Link to config registry if path provided and config vars not yet present
  if (!is.null(config_registry_path) && !"embedding_model" %in% names(cv_results)) {
    cv_results <- link_results_to_config(cv_results, config_registry_path)
  }

  df <- cv_results %>%
    filter(cv_strategy == !!cv_strategy, !is.na(brier_mean), model_label == "full")

  if (nrow(df) == 0) {
    message("No CV full-model results for strategy: ", cv_strategy)
    return(NULL)
  }

  # Ensure window spec columns
  if (!"window_size" %in% names(df) && "climate_var" %in% names(df)) {
    window_info <- parse_window_spec(unique(df$climate_var))
    existing <- intersect(setdiff(names(window_info), "climate_var"), names(df))
    if (length(existing) > 0) df <- df %>% select(-all_of(existing))
    df <- df %>% left_join(window_info, by = "climate_var")
  }

  # Default decision vars
  if (is.null(decision_vars)) {
    decision_vars <- c("window_type", "window_size", "lag_days")
    config_candidates <- c("embedding_model", "sent_method", "comp__method", "th__method")
    decision_vars <- c(decision_vars, intersect(config_candidates, names(df)))
  }
  decision_vars <- intersect(decision_vars, names(df))

  if (length(decision_vars) == 0) {
    message("No decision variables available for CV panels — falling back to plain curve")
    return(plot_cv_spec_curve(cv_results, cv_strategy))
  }

  # Sort and rank
  plot_data <- df %>%
    arrange(brier_mean) %>%
    mutate(rank = row_number())

  # Baselines
  df_baselines <- cv_results %>%
    normalize_cv_columns() %>%
    filter(cv_strategy == !!cv_strategy, !is.na(brier_mean), model_label != "full") %>%
    group_by(model_label) %>%
    summarise(brier_mean = mean(brier_mean, na.rm = TRUE), .groups = "drop")

  strategy_label <- ifelse(cv_strategy == "group_kfold", "Group K-Fold", "Time-Series")

  # Build the top panel (Brier curve)
  p_top <- ggplot(plot_data, aes(x = rank, y = brier_mean)) +
    geom_point(size = 0.8, alpha = 0.7, color = "#2c7bb6") +
    labs(y = "Brier score (mean)",
         title = paste0("CV Brier Scores: ", strategy_label)) +
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  # Add baseline reference lines
  for (i in seq_len(nrow(df_baselines))) {
    p_top <- p_top +
      geom_hline(yintercept = df_baselines$brier_mean[i],
                 linetype = "dashed", color = "gray50") +
      annotate("text", x = max(plot_data$rank) * 0.95,
               y = df_baselines$brier_mean[i],
               label = df_baselines$model_label[i],
               hjust = 1, vjust = -0.5, size = PLOT_LABEL_SIZE + 0.5, color = "gray40")
  }

  # Build decision indicator panels (same pattern as MV version)
  panel_plots <- map(decision_vars, function(var) {
    var_label <- var %>%
      str_replace_all("__", ": ") %>%
      str_replace_all("_", " ") %>%
      tools::toTitleCase()

    panel_data <- plot_data %>%
      mutate(.value = as.character(.data[[var]])) %>%
      filter(!is.na(.value)) %>%
      mutate(.value = {
        nums <- suppressWarnings(as.numeric(.value))
        if (all(!is.na(nums))) {
          factor(.value, levels = as.character(sort(unique(nums))))
        } else {
          factor(.value, levels = sort(unique(.value)))
        }
      })

    ggplot(panel_data, aes(x = rank, y = 1, fill = .value)) +
      geom_tile(width = 1, height = 1) +
      scale_y_continuous(breaks = NULL) +
      labs(x = NULL, y = var_label, fill = NULL) +
      guides(fill = guide_legend(
        ncol = 1,
        keywidth = unit(0.4, "cm"),
        keyheight = unit(0.4, "cm")
      )) +
      theme_minimal(base_size = PLOT_PANEL_SIZE) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = PLOT_PANEL_SIZE, face = "bold", angle = 0,
                                    vjust = 0.5, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.justification = "left",
        legend.box.just = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = PLOT_LEGEND_SIZE),
        legend.key.size = unit(PLOT_BASE_SIZE * 0.032, "cm"),
        plot.margin = margin(0, 0, 0, 0)
      )
  })

  # Add x-axis to last panel
  n_panels <- length(panel_plots)
  panel_plots[[n_panels]] <- panel_plots[[n_panels]] +
    labs(x = "Specification (sorted by Brier score)") +
    theme(axis.title.x = element_text(size = PLOT_BASE_SIZE))

  # Combine
  heights <- c(4, rep(0.6, n_panels))
  combined <- p_top
  for (pp in panel_plots) {
    combined <- combined / pp
  }
  combined <- combined + plot_layout(heights = heights)

  return(combined)
}
#' @param cv_results CV results tibble
#' @param cv_strategy "group_kfold" or "timeseries"
#' @return ggplot object
plot_delta_brier <- function(cv_results, cv_strategy = "group_kfold") {

  cv_results <- normalize_cv_columns(cv_results)

  df <- cv_results %>%
    filter(cv_strategy == !!cv_strategy, !is.na(brier_mean)) %>%
    filter(model_label %in% c("full", "seasonal_ops")) %>%
    select(config_id, climate_var, model_label, brier_mean) %>%
    pivot_wider(names_from = model_label, values_from = brier_mean,
                id_cols = c(config_id, climate_var)) %>%
    filter(!is.na(full), !is.na(seasonal_ops)) %>%
    mutate(delta_brier = full - seasonal_ops) %>%
    arrange(delta_brier) %>%
    mutate(spec_rank = row_number())

  if (nrow(df) == 0) {
    message("Cannot compute delta-Brier")
    return(NULL)
  }

  n_better <- sum(df$delta_brier < 0)
  n_total  <- nrow(df)

  ggplot(df, aes(x = spec_rank, y = delta_brier)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_col(aes(fill = delta_brier < 0), width = 0.8, alpha = 0.7) +
    scale_fill_manual(
      values = c("TRUE" = "#1D9E75", "FALSE" = "#E24B4A"),
      labels = c("TRUE" = "Climate helps", "FALSE" = "Climate hurts"),
      name = NULL
    ) +
    labs(x = "Specification (ordered)", y = "Delta Brier (negative = better)",
         title = "Delta-Brier: Full Model - Seasonal+Ops Baseline",
         subtitle = sprintf("%d/%d specifications improved by climate", n_better, n_total)) +
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    theme(legend.position = "top")
}


# ==============================================================================
# ROBUSTNESS SUMMARY
# ==============================================================================

#' Summarize climate effect robustness across multiverse
#' @param df Results with config metadata
#' @param outcome_filter Optional outcome filter
#' @param component "cond"/"zi" for rail, NULL for NRC
summarize_climate_robustness <- function(df, outcome_filter = NULL,
                                          component = NULL) {

  if (!is.null(outcome_filter)) df <- df %>% filter(outcome == outcome_filter)

  cols <- detect_estimate_cols(df, component)

  df %>%
    filter(status == "success") %>%
    summarize(
      n_models        = n(),
      mean_estimate   = mean(.data[[cols$estimate]], na.rm = TRUE),
      sd_estimate     = sd(.data[[cols$estimate]], na.rm = TRUE),
      median_estimate = median(.data[[cols$estimate]], na.rm = TRUE),
      pct_significant = 100 * mean(.data[[cols$pval]] < 0.05, na.rm = TRUE),
      pct_negative    = 100 * mean(.data[[cols$estimate]] < 0, na.rm = TRUE),
      pct_neg_sig     = 100 * mean(.data[[cols$pval]] < 0.05 &
                                     .data[[cols$estimate]] < 0, na.rm = TRUE)
    )
}

# ==============================================================================
# CROSS-INDUSTRY SUMMARY FUNCTIONS
# ==============================================================================

#' Build cross-industry robustness summary table
#'
#' Loads per-industry robustness CSVs from the manifest directory and
#' combines them into a single publication-ready table.
#'
#' @param figure_dir Top-level figure output directory (contains industry subdirs)
#' @param industry_labels Named vector: c(rail = "Rail (FRA)", nrc = "Nuclear (NRC)")
#' @return Tibble with industry, outcome, component, and robustness metrics
build_cross_industry_robustness <- function(figure_dir, industry_labels = NULL) {

  industry_dirs <- list.dirs(figure_dir, full.names = TRUE, recursive = FALSE)
  industry_keys <- basename(industry_dirs)
  # Exclude internal directories (prefixed with _)
  industry_keys <- industry_keys[!grepl("^_", industry_keys)]

  all_robustness <- map_dfr(industry_keys, function(ind) {
    rob_files <- list.files(file.path(figure_dir, ind),
                            pattern = "_robustness\\.csv$",
                            full.names = TRUE)
    if (length(rob_files) == 0) return(NULL)

    map_dfr(rob_files, function(f) {
      rob <- read_csv(f, show_col_types = FALSE)
      # Parse outcome and component from filename
      fname <- tools::file_path_sans_ext(basename(f))
      fname <- sub("_robustness$", "", fname)

      # Split: outcome_component or just outcome
      if (grepl("_(cond|zi)$", fname)) {
        component <- sub(".*_(cond|zi)$", "\\1", fname)
        outcome <- sub("_(cond|zi)$", "", fname)
      } else {
        component <- "single"
        outcome <- fname
      }

      rob %>%
        mutate(
          industry = ind,
          industry_label = if (!is.null(industry_labels)) industry_labels[ind] %||% ind else ind,
          outcome = outcome,
          component = component
        )
    })
  })

  if (nrow(all_robustness) == 0) return(NULL)

  # Clean up for presentation
  all_robustness %>%
    mutate(
      outcome_label = outcome %>%
        str_replace_all("_", " ") %>%
        tools::toTitleCase(),
      component_label = case_when(
        component == "cond"   ~ "Conditional",
        component == "zi"     ~ "Zero-Inflation",
        component == "single" ~ "--",
        TRUE                  ~ component
      )
    ) %>%
    select(industry_label, outcome_label, component_label,
           n_models, mean_estimate, sd_estimate, median_estimate,
           pct_significant, pct_negative, pct_neg_sig) %>%
    arrange(industry_label, outcome_label, desc(component_label))
}


#' Build cross-industry config importance comparison table
#'
#' Loads per-industry config importance CSVs and combines them.
#'
#' @param figure_dir Top-level figure output directory
#' @param industry_labels Named vector of labels
#' @return Tibble with industry, outcome, component, variable, pct_variance, etc.
build_cross_industry_importance <- function(figure_dir, industry_labels = NULL) {

  industry_dirs <- list.dirs(figure_dir, full.names = TRUE, recursive = FALSE)
  industry_keys <- basename(industry_dirs)
  # Exclude internal directories (prefixed with _)
  industry_keys <- industry_keys[!grepl("^_", industry_keys)]

  all_importance <- map_dfr(industry_keys, function(ind) {
    imp_files <- list.files(file.path(figure_dir, ind),
                            pattern = "_config_importance\\.csv$",
                            full.names = TRUE)
    if (length(imp_files) == 0) return(NULL)

    map_dfr(imp_files, function(f) {
      imp <- read_csv(f, show_col_types = FALSE)
      fname <- tools::file_path_sans_ext(basename(f))
      fname <- sub("_config_importance$", "", fname)

      if (grepl("_(cond|zi)$", fname)) {
        component <- sub(".*_(cond|zi)$", "\\1", fname)
        outcome <- sub("_(cond|zi)$", "", fname)
      } else {
        component <- "single"
        outcome <- fname
      }

      imp %>%
        mutate(
          industry = ind,
          industry_label = if (!is.null(industry_labels)) industry_labels[ind] %||% ind else ind,
          outcome = outcome,
          component = component
        )
    })
  })

  if (nrow(all_importance) == 0) return(NULL)

  all_importance %>%
    mutate(
      outcome_label = outcome %>%
        str_replace_all("_", " ") %>%
        tools::toTitleCase(),
      component_label = case_when(
        component == "cond"   ~ "Cond",
        component == "zi"     ~ "ZI",
        component == "single" ~ "--",
        TRUE                  ~ component
      ),
      var_label = variable %>%
        str_replace_all("__", ": ") %>%
        str_replace_all("_", " ") %>%
        tools::toTitleCase()
    )
}


#' Plot cross-industry config importance comparison
#'
#' Two-panel layout (one per industry). Within each panel, horizontal grouped
#' bars show % variance explained by each analytical choice, colored by
#' outcome x component. This reveals whether the same choices dominate across
#' outcomes within and between industries.
#'
#' @param importance_data Output of build_cross_industry_importance()
#' @param top_n Show top N variables overall (default 8)
#' @param exclude_outcomes Character vector of outcomes to exclude (optional)
#' @return patchwork plot
plot_cross_industry_importance <- function(importance_data, top_n = 8,
                                           exclude_outcomes = NULL) {

  if (is.null(importance_data) || nrow(importance_data) == 0) return(NULL)

  # Apply outcome exclusions if specified
  if (!is.null(exclude_outcomes)) {
    importance_data <- importance_data %>%
      filter(!outcome %in% exclude_outcomes)
  }

  # Create a combined outcome-component label for bar color
  plot_data <- importance_data %>%
    filter(!is.na(pct_variance)) %>%
    mutate(
      outcome_comp = ifelse(component_label == "--",
                            outcome_label,
                            paste0(outcome_label, " (", component_label, ")")),
      outcome_comp = factor(outcome_comp)
    )

  # Identify the top_n most important variables across all data
  top_var_names <- plot_data %>%
    group_by(var_label) %>%
    summarise(max_var = max(pct_variance, na.rm = TRUE), .groups = "drop") %>%
    slice_max(max_var, n = top_n, with_ties = FALSE) %>%
    pull(var_label)

  plot_data <- plot_data %>%
    filter(var_label %in% top_var_names) %>%
    mutate(var_label = factor(var_label, levels = rev(top_var_names)))

  # Build one panel per industry using patchwork
  industries <- unique(plot_data$industry_label)

  panels <- map(industries, function(ind) {
    ind_data <- plot_data %>% filter(industry_label == ind)

    # Count outcome_comp levels for this industry
    n_outcomes <- n_distinct(ind_data$outcome_comp)

    ggplot(ind_data, aes(x = var_label, y = pct_variance, fill = outcome_comp)) +
      geom_col(position = position_dodge(width = 0.8), alpha = 0.85, width = 0.75) +
      geom_text(aes(label = ifelse(pct_variance >= 1,
                                    paste0(sprintf("%.0f", pct_variance), "%"),
                                    "")),
                position = position_dodge(width = 0.8),
                hjust = -0.1, size = PLOT_LABEL_SIZE, color = "gray30") +
      coord_flip() +
      scale_fill_brewer(palette = "Set2", name = "Outcome") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(
        title = ind,
        x = NULL, y = "% Variance Explained"
      ) +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = PLOT_TITLE_SIZE)
      ) +
      guides(fill = guide_legend(ncol = min(n_outcomes, 3)))
  })

  # Stack panels vertically with shared title
  combined <- wrap_plots(panels, ncol = 1) +
    plot_annotation(
      title = "Configuration Importance Across Industries",
      subtitle = "% variance in climate effect explained by each analytical choice (univariate R-squared)",
      theme = theme(
        plot.title = element_text(size = PLOT_TITLE_SIZE + 2, face = "bold"),
        plot.subtitle = element_text(size = PLOT_SUBTITLE_SIZE, color = "gray40")
      )
    )

  return(combined)
}


#' Plot cross-industry climate vs operational effects comparison
#'
#' Faceted by industry. Each panel shows the climate effect alongside
#' operational covariate effects for that domain.
#'
#' @param industries Named list of industry configs (from generate_mv_report)
#' @param industry_labels Named vector of labels
#' @param credible_configs Named list of character vectors of credible config IDs
#'   per industry. If provided, results are filtered to only these configs.
#'   Names should match industry keys. NULL = no filtering (default).
#' @param credible_pairs Named list of tibbles with (config_id, climate_var_tested)
#'   per industry. If provided, takes precedence over credible_configs for
#'   pair-level filtering. NULL = use credible_configs or no filtering.
#' @return ggplot (faceted by industry)
plot_cross_industry_climate_vs_ops <- function(industries, industry_labels = NULL,
                                                credible_configs = NULL,
                                                credible_pairs = NULL) {

  all_summaries <- map_dfr(names(industries), function(ind_key) {
    ind <- industries[[ind_key]]
    label <- if (!is.null(industry_labels)) industry_labels[ind_key] %||% ind_key else ind_key

    mv_results <- tryCatch(
      arrow::read_parquet(ind$mv_results),
      error = function(e) return(NULL)
    )
    if (is.null(mv_results)) return(NULL)

    # Apply CV filtering if provided for this industry
    # Pair-level takes precedence over config-level
    if (!is.null(credible_pairs) && ind_key %in% names(credible_pairs)) {
      pairs <- credible_pairs[[ind_key]]
      mv_results <- mv_results %>%
        inner_join(pairs, by = c("config_id" = "config_id",
                                  "climate_var" = "climate_var_tested"))
    } else if (!is.null(credible_configs) && ind_key %in% names(credible_configs)) {
      mv_results <- mv_results %>%
        filter(config_id %in% credible_configs[[ind_key]])
    }

    results_full <- link_results_to_config(mv_results, ind$config_registry)
    outcomes <- unique(results_full$outcome[results_full$status == "success"])

    # Apply per-industry outcome exclusions
    if (!is.null(ind$exclude_outcomes)) {
      outcomes <- setdiff(outcomes, ind$exclude_outcomes)
    }

    map_dfr(outcomes, function(out) {
      outcome_data <- results_full %>% filter(outcome == out)

      # Detect components — check non-NA counts within THIS outcome's rows only
      has_cond <- "climate_estimate_cond" %in% names(outcome_data) &&
                  sum(!is.na(outcome_data$climate_estimate_cond)) > 0
      has_zi   <- "climate_estimate_zi" %in% names(outcome_data) &&
                  sum(!is.na(outcome_data$climate_estimate_zi)) > 0
      has_base <- "climate_estimate" %in% names(outcome_data) &&
                  sum(!is.na(outcome_data$climate_estimate)) > 0

      # Build list of components to iterate
      # Use "none" as sentinel for single-estimate models (component=NULL in function calls)
      components <- list()
      if (has_zi)   components[["zi"]]     <- "zi"
      if (has_cond) components[["cond"]]   <- "cond"
      if (!has_cond && !has_zi && has_base) components[["single"]] <- "none"

      if (length(components) == 0) return(NULL)

      map_dfr(names(components), function(comp_name) {
        comp <- if (components[[comp_name]] == "none") NULL else components[[comp_name]]
        comp_label <- dplyr::case_when(
          comp_name == "cond"   ~ "Conditional",
          comp_name == "zi"     ~ "Zero-Inflation",
          comp_name == "single" ~ "",
          TRUE                  ~ comp_name
        )

        tryCatch({
          result <- compare_climate_vs_operational(
            results_full,
            ops_vars = ind$ops_vars,
            outcome_filter = out,
            component = comp
          )
          if (is.null(result$summary) || nrow(result$summary) == 0) {
            message(sprintf("    [SKIP] %s / %s (%s): empty summary", ind_key, out, comp_name))
            return(NULL)
          }
          result$summary %>%
            mutate(
              industry = ind_key,
              industry_label = label,
              outcome = out,
              outcome_label = if (comp_label == "") {
                out %>% str_replace_all("_", " ") %>% tools::toTitleCase()
              } else {
                paste0(out %>% str_replace_all("_", " ") %>% tools::toTitleCase(),
                       " (", comp_label, ")")
              },
              component = comp_name
            )
        }, error = function(e) {
          message(sprintf("    [ERR] %s / %s (%s): %s", ind_key, out, comp_name, conditionMessage(e)))
          NULL
        })
      })
    })
  })

  if (is.null(all_summaries) || nrow(all_summaries) == 0) return(NULL)

  # Create facet label
  plot_data <- all_summaries %>%
    mutate(
      facet_label = paste0(industry_label, " - ", outcome_label),
      var_clean = variable %>%
        str_replace_all("_", " ") %>%
        tools::toTitleCase()
    )

  ggplot(plot_data, aes(x = reorder(var_clean, abs_mean),
                         y = mean_estimate, fill = var_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_col(alpha = 0.8) +
    geom_errorbar(
      aes(ymin = mean_estimate - sd_estimate, ymax = mean_estimate + sd_estimate),
      width = 0.3, alpha = 0.7
    ) +
    geom_text(aes(label = sprintf("%.0f%%", pct_significant),
                  y = ifelse(mean_estimate >= 0,
                             mean_estimate + sd_estimate + max(abs(plot_data$mean_estimate)) * 0.05,
                             mean_estimate - sd_estimate - max(abs(plot_data$mean_estimate)) * 0.05)),
              size = PLOT_LABEL_SIZE - 0.3, color = "gray30") +
    coord_flip() +
    facet_wrap(~ facet_label, scales = "free", ncol = 2) +
    scale_fill_manual(
      values = c("primary" = "#e41a1c", "between" = "#377eb8", "within" = "#4daf4a"),
      labels = c("primary" = "Climate", "between" = "Between-org", "within" = "Within-org"),
      name = "Variable Type"
    ) +
    labs(
      title = "Climate vs. Operational Effects Across Industries",
      subtitle = "Mean estimate +/- SD across multiverse; % significant shown",
      x = NULL, y = "Mean Effect Estimate"
    ) +
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    theme(
      legend.position = "top",
      strip.text = element_text(size = PLOT_PANEL_SIZE, face = "bold"),
      axis.text.y = element_text(size = 7)
    )
}
