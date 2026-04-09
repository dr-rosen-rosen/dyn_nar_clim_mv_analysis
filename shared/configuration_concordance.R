#!/usr/bin/env Rscript
# ==============================================================================
# CONFIGURATION CONCORDANCE ANALYSIS
# ==============================================================================
# Identifies defensible configuration patterns across analyses and industries.
#
# Four layers:
#   1. Concordance heatmaps (visual/qualitative)
#   2. Kendall's W concordance testing (formal)
#   3. Mixed-effects dominance analysis (quantitative)
#   4. Qualitative Comparative Analysis (configurational — requires QCA package)
#
# Layers 1-3 decompose performance by individual facets.
# Layer 4 identifies *combinations* of facet levels that are jointly
# sufficient for good performance (equifinality / conjunctural causation).
#
# Designed to work with multiverse output from both traditional MV and CV
# analyses across multiple industry datasets.
#
# Usage:
#   source("configuration_concordance.R")
#   
#   # Load results from multiple industry analyses
#   combined <- load_and_combine_results(
#     list(
#       rail    = "results/mv/rail_multiverse_results.parquet",
#       nuclear = "results/mv/nrc_multiverse_results.parquet"
#       # healthcare = "results/mv/healthcare_multiverse_results.parquet"
#     ),
#     config_registry_paths = list(
#       rail    = "data/rail/_cfg/config_registry.csv",
#       nuclear = "data/nrc/_cfg/config_registry.csv"
#     )
#   )
#   
#   # Run full concordance analysis
#   concordance <- analyze_configuration_concordance(combined)
#   
#   # Explore results
#   print(concordance$kendall_w)
#   concordance$plots$facet_heatmap
#   concordance$dominance_models
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(patchwork)
})

# ==============================================================================
# DATA LOADING AND HARMONIZATION
# ==============================================================================

#' Load and combine multiverse results across industries
#'
#' @param result_paths Named list of file paths to multiverse result files
#'   (parquet or csv). Names become the industry labels.
#' @param config_registry_paths Named list of config registry CSV paths,
#'   matching names in result_paths. If NULL, assumes config columns are
#'   already in the results.
#' @param performance_cols Character vector of performance metric column names
#'   to extract. Searches for common patterns if NULL.
#' @param outcome_col Column name identifying the outcome variable within
#'   each industry's results (e.g., "outcome").
#' @return A tibble with columns: industry, outcome, config_id, performance
#'   metrics, and all configuration facets.
load_and_combine_results <- function(result_paths,
                                     config_registry_paths = NULL,
                                     performance_cols = NULL,
                                     outcome_col = "outcome") {

  combined <- map2_dfr(names(result_paths), result_paths, function(industry, path) {
    # Read results
    if (grepl("\\.parquet$", path)) {
      res <- read_parquet(path)
    } else {
      res <- read_csv(path, show_col_types = FALSE)
    }

    # Join config registry if provided
    if (!is.null(config_registry_paths) && industry %in% names(config_registry_paths)) {
      reg_path <- config_registry_paths[[industry]]
      if (file.exists(reg_path)) {
        registry <- read_csv(reg_path, show_col_types = FALSE)
        # Join on config_id
        res <- left_join(res, registry, by = "config_id", suffix = c("", ".reg"))
      }
    }

    res %>% mutate(industry = industry)
  })

  # Auto-detect performance columns if not specified
  if (is.null(performance_cols)) {
    performance_cols <- detect_performance_cols(combined)
    message("Auto-detected performance columns: ", paste(performance_cols, collapse = ", "))
  }

  # Standardize configuration facet names
  combined <- standardize_config_facets(combined)

  attr(combined, "performance_cols") <- performance_cols
  attr(combined, "outcome_col") <- outcome_col
  return(combined)
}


#' Detect performance metric columns from common naming patterns
detect_performance_cols <- function(df) {
  patterns <- c(
    "^climate_estimate",    # MV effect sizes
    "^climate_pval",        # MV p-values
    "^AIC$", "^BIC$",      # Information criteria
    "^brier_",             # CV Brier scores
    "^auc_",               # CV AUC
    "^rmse_",              # CV RMSE
    "^mae_",               # CV MAE
    "^log_lik"             # Log likelihood
  )
  cols <- names(df)
  detected <- unique(unlist(lapply(patterns, function(p) grep(p, cols, value = TRUE))))
  # Fall back to anything with "estimate", "score", "metric" if nothing found

  if (length(detected) == 0) {
    detected <- grep("estimate|score|metric|brier|auc|rmse", cols, value = TRUE, ignore.case = TRUE)
  }
  return(detected)
}


#' Standardize configuration facet column names across industries
#' Maps common variations to canonical names.
standardize_config_facets <- function(df) {
  # Canonical facet names and their common aliases
  facet_map <- list(
    embedding_model = c("embedding_model", "embed_model", "embedding"),
    sentiment_model = c("sent_method", "sentiment_model", "sent_model", "sentiment_method"),
    window_type     = c("window_type", "win_type"),
    window_size     = c("window_size", "win_size", "window_length"),
    composite_method = c("comp__method", "composite_method", "comp_method"),
    composite_power  = c("comp__power", "composite_power", "comp_power"),
    composite_temp   = c("comp__temperature", "composite_temp", "comp_temp"),
    threshold_k      = c("th__k", "threshold_k", "topk"),
    halflife         = c("halflife_days", "halflife", "hl_days")
  )

  cols <- names(df)
  for (canonical in names(facet_map)) {
    aliases <- facet_map[[canonical]]
    found <- intersect(aliases, cols)
    if (length(found) > 0 && found[1] != canonical) {
      # Rename the first match to canonical
      df <- rename(df, !!canonical := !!sym(found[1]))
    }
  }
  return(df)
}


# ==============================================================================
# CV-BASED PRE-FILTERING AND MV+CV INTEGRATION
# ==============================================================================
#
# The multiverse (MV) analysis tells you the direction and magnitude of
# climate effects across specifications. Cross-validation (CV) tells you
# which specifications actually predict out-of-sample. Combining them:
#   1. Use CV to identify the "credible" specification set
#   2. Run MV spec curves / concordance on the credible set only
#
# This section provides tools for:
#   - Computing delta-Brier against flexible baselines
#   - Pre-filtering specifications that beat their baseline
#   - Joining MV and CV results by config_id
# ==============================================================================

#' Compute delta-Brier against flexible baselines
#'
#' For each config × climate_var × cv_strategy, computes the improvement
#' of the full (climate-inclusive) model over a baseline.
#'
#' @param cv_results CV results tibble with columns: config_id,
#'   model_label, brier_score_mean (or brier_mean), cv_strategy,
#'   and climate_var_tested (or climate_var).
#' @param baseline_strategy How to select the baseline:
#'   "best_non_climate" — compares against whichever non-climate model
#'     (intercept_only, seasonal_only/seasonal_ops, no_climate) has the
#'     lowest Brier score for that config. Best for NRC where ops baselines
#'     can be worse than intercepts.
#'   "no_climate" — compares against the no_climate model (seasonal+ops
#'     without climate). Standard for rail.
#'   "intercept_only" — compares against intercept-only. Most conservative
#'     and cross-industry-comparable.
#'   "all" — computes delta against all baselines; returns multiple columns.
#' @return Tibble with config_id, climate_var, cv_strategy, delta_brier,
#'   baseline_used, brier_full, brier_baseline.
compute_delta_brier <- function(cv_results,
                                baseline_strategy = c("best_non_climate",
                                                      "no_climate",
                                                      "intercept_only",
                                                      "all")) {

  baseline_strategy <- match.arg(baseline_strategy)

  # Harmonize column names across rail/NRC conventions
  cv <- cv_results
  if ("brier_mean" %in% names(cv) && !"brier_score_mean" %in% names(cv)) {
    cv <- rename(cv, brier_score_mean = brier_mean)
  }
  if ("climate_var" %in% names(cv) && !"climate_var_tested" %in% names(cv)) {
    cv <- rename(cv, climate_var_tested = climate_var)
  }

  # Identify full model rows: model_label matches climate_var_tested, or == "full"
  cv <- cv %>%
    mutate(is_full_model = (model_label == climate_var_tested) |
             (model_label == "full"))

  # Extract full model Brier scores
  full_models <- cv %>%
    filter(is_full_model, !is.na(brier_score_mean)) %>%
    select(config_id, climate_var_tested, cv_strategy,
           brier_full = brier_score_mean)

  # Extract baseline Brier scores
  baseline_labels <- c("intercept_only", "no_climate", "seasonal_only",
                       "seasonal_ops", "seasonal")
  baselines <- cv %>%
    filter(model_label %in% baseline_labels, !is.na(brier_score_mean)) %>%
    select(config_id, climate_var_tested, cv_strategy,
           model_label, brier_baseline = brier_score_mean)

  if (baseline_strategy == "all") {
    # Return delta against every available baseline
    delta <- full_models %>%
      inner_join(baselines,
                 by = c("config_id", "climate_var_tested", "cv_strategy")) %>%
      mutate(
        delta_brier = brier_full - brier_baseline,
        climate_helps = delta_brier < 0,
        baseline_used = model_label
      )
    return(delta)
  }

  if (baseline_strategy == "best_non_climate") {
    # For each config, pick the baseline with the lowest Brier
    best_baselines <- baselines %>%
      group_by(config_id, climate_var_tested, cv_strategy) %>%
      slice_min(brier_baseline, n = 1, with_ties = FALSE) %>%
      ungroup()
  } else if (baseline_strategy == "no_climate") {
    best_baselines <- baselines %>%
      filter(model_label %in% c("no_climate", "seasonal_ops"))
  } else if (baseline_strategy == "intercept_only") {
    best_baselines <- baselines %>%
      filter(model_label == "intercept_only")
  }

  delta <- full_models %>%
    inner_join(best_baselines,
               by = c("config_id", "climate_var_tested", "cv_strategy")) %>%
    mutate(
      delta_brier = brier_full - brier_baseline,
      climate_helps = delta_brier < 0,
      baseline_used = model_label
    )

  return(delta)
}


#' Pre-filter specifications to those where climate adds predictive value
#'
#' Returns config_ids that beat their baseline in CV, optionally requiring
#' this across multiple CV strategies.
#'
#' @param cv_results CV results tibble
#' @param baseline_strategy Passed to compute_delta_brier()
#' @param require_both_strategies If TRUE, a config must beat the baseline
#'   in *both* timeseries and group_kfold CV. More conservative.
#' @param max_delta Maximum delta-Brier to retain (configs where climate
#'   hurts less than this are still included). Default 0 = strict improvement.
#' @return List with:
#'   $credible_configs — character vector of config_ids
#'   $delta_summary — tibble with delta-Brier per config
#'   $filter_stats — summary statistics about what was kept/dropped
filter_credible_specs <- function(cv_results,
                                  baseline_strategy = "best_non_climate",
                                  require_both_strategies = FALSE,
                                  max_delta = 0,
                                  filter_level = c("config_window", "config")) {

  filter_level <- match.arg(filter_level)
  delta <- compute_delta_brier(cv_results, baseline_strategy)

  if (require_both_strategies) {
    strategies <- unique(delta$cv_strategy)
    if (length(strategies) < 2) {
      message("Only one CV strategy found; cannot require both.")
      require_both_strategies <- FALSE
    }
  }

  # Identify credible specs at the (config_id, climate_var) pair level
  if (require_both_strategies) {
    credible_pairs <- delta %>%
      group_by(config_id, climate_var_tested) %>%
      summarize(
        n_strategies = n_distinct(cv_strategy),
        n_pass = sum(delta_brier <= max_delta),
        mean_delta = mean(delta_brier, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n_pass == n_strategies)
  } else {
    # At least one strategy shows improvement for this config × window
    credible_pairs <- delta %>%
      group_by(config_id, climate_var_tested) %>%
      summarize(
        n_pass = sum(delta_brier <= max_delta),
        mean_delta = mean(delta_brier, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n_pass > 0)
  }

  # Extract credible IDs at both levels
  credible_config_ids <- unique(credible_pairs$config_id)
  credible_config_window_pairs <- credible_pairs %>%
    select(config_id, climate_var_tested)

  # Stats depend on filter_level
  if (filter_level == "config_window") {
    # Total = unique (config_id, climate_var) pairs
    all_pairs <- delta %>%
      distinct(config_id, climate_var_tested)
    n_total <- nrow(all_pairs)
    n_kept  <- nrow(credible_config_window_pairs)
  } else {
    # Total = unique config_ids
    n_total <- length(unique(delta$config_id))
    n_kept  <- length(credible_config_ids)
  }

  filter_stats <- list(
    n_total = n_total,
    n_credible = n_kept,
    n_dropped = n_total - n_kept,
    pct_retained = round(100 * n_kept / max(n_total, 1), 1),
    baseline_strategy = baseline_strategy,
    require_both = require_both_strategies,
    max_delta = max_delta,
    filter_level = filter_level,
    # Also report the other level for reference
    n_configs_credible = length(credible_config_ids),
    n_config_window_pairs_credible = nrow(credible_config_window_pairs)
  )

  level_label <- ifelse(filter_level == "config_window",
                        "config x window pairs", "configs")
  message(sprintf("CV pre-filter: %d/%d %s retained (%.1f%%) using '%s' baseline%s",
                  n_kept, n_total, level_label, filter_stats$pct_retained,
                  baseline_strategy,
                  if (require_both_strategies) " (both strategies required)" else ""))
  if (filter_level == "config_window") {
    message(sprintf("  (%d unique config_ids have at least one credible window)",
                    length(credible_config_ids)))
  }

  # Summarize delta per config × window pair
  delta_summary <- delta %>%
    group_by(config_id, climate_var_tested) %>%
    summarize(
      mean_delta_brier = mean(delta_brier, na.rm = TRUE),
      min_delta_brier = min(delta_brier, na.rm = TRUE),
      max_delta_brier = max(delta_brier, na.rm = TRUE),
      n_strategies_pass = sum(delta_brier <= max_delta),
      baseline_used = paste(unique(baseline_used), collapse = "/"),
      .groups = "drop"
    ) %>%
    mutate(
      credible_pair = paste(config_id, climate_var_tested) %in%
        paste(credible_config_window_pairs$config_id,
              credible_config_window_pairs$climate_var_tested),
      credible_config = config_id %in% credible_config_ids
    ) %>%
    arrange(mean_delta_brier)

  return(list(
    credible_configs = credible_config_ids,
    credible_pairs = credible_config_window_pairs,
    delta_summary = delta_summary,
    filter_stats = filter_stats,
    filter_level = filter_level,
    delta_raw = delta
  ))
}


#' Join MV and CV results by config_id for integrated analysis
#'
#' Creates a single tibble where each row has both the MV effect estimates
#' and the CV predictive performance, enabling analyses like "do specs
#' with the largest climate effects also predict best?"
#'
#' @param mv_results MV results tibble (one row per config × outcome)
#' @param cv_results CV results tibble
#' @param baseline_strategy For delta-Brier computation
#' @param cv_strategy_filter Which CV strategy to use for the join
#'   (e.g., "group_kfold"). If NULL, keeps all strategies (wide format).
#' @return Joined tibble with both MV estimates and CV metrics
join_mv_cv <- function(mv_results,
                       cv_results,
                       baseline_strategy = "best_non_climate",
                       cv_strategy_filter = NULL) {

  # Compute delta-Brier
  delta <- compute_delta_brier(cv_results, baseline_strategy)

  # Filter to requested CV strategy
  if (!is.null(cv_strategy_filter)) {
    delta <- delta %>% filter(cv_strategy == cv_strategy_filter)
  }

  # Summarize CV metrics per config
  cv_summary <- delta %>%
    group_by(config_id) %>%
    summarize(
      brier_full = mean(brier_full, na.rm = TRUE),
      brier_baseline = mean(brier_baseline, na.rm = TRUE),
      delta_brier = mean(delta_brier, na.rm = TRUE),
      climate_helps = all(climate_helps),
      baseline_used = paste(unique(baseline_used), collapse = "/"),
      .groups = "drop"
    )

  # Join onto MV results
  # MV results may have climate_var_tested or climate_var as the variable name
  joined <- mv_results %>%
    left_join(cv_summary, by = "config_id")

  n_matched <- sum(!is.na(joined$delta_brier))
  n_total <- nrow(joined)
  message(sprintf("MV+CV join: %d/%d MV rows matched to CV results", n_matched, n_total))

  return(joined)
}


#' Plot MV effect size vs CV delta-Brier (the "credibility scatterplot")
#'
#' Specifications that have both large effects AND good predictive
#' performance (negative delta-Brier) are in the lower-right quadrant —
#' these are your most defensible configurations.
#'
#' @param joined Output from join_mv_cv()
#' @param estimate_col MV estimate column name
#' @param color_by Optional facet to color points by
#' @return ggplot
plot_mv_vs_cv <- function(joined,
                          estimate_col = "climate_estimate_cond",
                          color_by = NULL) {

  plot_data <- joined %>%
    filter(!is.na(.data[[estimate_col]]), !is.na(delta_brier))

  if (nrow(plot_data) == 0) {
    message("No overlapping data for MV vs CV plot.")
    return(NULL)
  }

  p <- ggplot(plot_data, aes(x = .data[[estimate_col]], y = delta_brier))

  if (!is.null(color_by) && color_by %in% names(plot_data)) {
    p <- p + geom_point(aes(color = .data[[color_by]]), alpha = 0.6, size = 2) +
      scale_color_brewer(palette = "Set2",
                         name = str_to_title(gsub("_", " ", color_by)))
  } else {
    p <- p + geom_point(alpha = 0.5, size = 2, color = "#2166ac")
  }

  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    annotate("text", x = Inf, y = -Inf, label = "Large effect +\ngood prediction",
             hjust = 1.1, vjust = -0.5, size = 3, color = "#1a9641",
             fontface = "italic") +
    annotate("text", x = -Inf, y = Inf, label = "No effect +\npoor prediction",
             hjust = -0.1, vjust = 1.5, size = 3, color = "#d7191c",
             fontface = "italic") +
    labs(
      title = "Multiverse Effect Size vs. Cross-Validation Performance",
      subtitle = "Credible specifications: significant effect AND predictive improvement",
      x = paste("Climate effect (", estimate_col, ")"),
      y = "Delta Brier (negative = climate helps)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.subtitle = element_text(size = 9, color = "gray40")
    )

  return(p)
}


#' Full integrated workflow: filter by CV, then run concordance on credible set
#'
#' @param mv_results MV results (one row per config × outcome × component)
#' @param cv_results CV results
#' @param metric MV metric for concordance analysis
#' @param baseline_strategy CV baseline strategy
#' @param cv_strategy_filter CV strategy for filtering
#' @param require_both_strategies Require improvement in both CV strategies?
#' @param ... Additional arguments passed to analyze_configuration_concordance()
#' @return List with filtering results, concordance results, and integration plots
integrated_mv_cv_analysis <- function(mv_results,
                                      cv_results,
                                      metric = "climate_estimate_cond",
                                      baseline_strategy = "best_non_climate",
                                      cv_strategy_filter = "group_kfold",
                                      require_both_strategies = FALSE,
                                      ...) {

  results <- list()

  # Step 1: Pre-filter by CV
  message("\n========================================")
  message("STEP 1: CV Pre-Filtering")
  message("========================================")
  results$cv_filter <- filter_credible_specs(
    cv_results,
    baseline_strategy = baseline_strategy,
    require_both_strategies = require_both_strategies
  )

  # Step 2: Filter MV results to credible specs
  credible_ids <- results$cv_filter$credible_configs
  mv_filtered <- mv_results %>%
    filter(config_id %in% credible_ids)

  message(sprintf("\nMV results: %d rows total → %d after CV filter",
                  nrow(mv_results), nrow(mv_filtered)))

  if (nrow(mv_filtered) < 10) {
    warning("Very few specs survived filtering. Consider relaxing the baseline strategy.")
  }

  # Step 3: Join MV + CV for scatterplot
  message("\n========================================")
  message("STEP 2: MV + CV Integration")
  message("========================================")
  results$joined <- join_mv_cv(
    mv_results, cv_results,
    baseline_strategy = baseline_strategy,
    cv_strategy_filter = cv_strategy_filter
  )

  # Credibility scatterplot (all specs, with filtering annotation)
  results$plots$mv_vs_cv <- plot_mv_vs_cv(
    results$joined, estimate_col = metric
  )

  # Step 4: Run concordance on filtered set
  message("\n========================================")
  message("STEP 3: Concordance Analysis (CV-filtered)")
  message("========================================")

  # Need to set up combined format for concordance
  mv_for_concordance <- mv_filtered
  attr(mv_for_concordance, "performance_cols") <- attr(mv_results, "performance_cols")
  attr(mv_for_concordance, "outcome_col") <- attr(mv_results, "outcome_col")

  results$concordance <- analyze_configuration_concordance(
    mv_for_concordance,
    metric = metric,
    ...
  )

  # Step 5: Compare filtered vs unfiltered concordance
  message("\n========================================")
  message("STEP 4: Filter Impact Assessment")
  message("========================================")
  results$concordance_unfiltered <- compute_all_concordances(
    mv_results, metric,
    higher_is_better = list(...)$higher_is_better %||% FALSE
  )

  message("\nKendall's W comparison (filtered vs. unfiltered):")
  comparison <- results$concordance$kendall_w %>%
    select(facet, W_filtered = W) %>%
    left_join(
      results$concordance_unfiltered %>% select(facet, W_unfiltered = W),
      by = "facet"
    ) %>%
    mutate(delta_W = W_filtered - W_unfiltered)
  print(comparison)

  results$filter_impact <- comparison

  return(results)
}


# ==============================================================================
# LAYER 1: CONCORDANCE HEATMAPS
# ==============================================================================

#' Create rank percentile matrix across industry-outcome contexts
#'
#' @param combined Combined results from load_and_combine_results()
#' @param metric Performance metric to rank by (column name)
#' @param higher_is_better If TRUE, higher values = better rank. Set FALSE
#'   for metrics like Brier score, AIC, RMSE.
#' @param facet_level If NULL, ranks full configurations. If a facet name
#'   (e.g., "sentiment_model"), computes median rank percentile per level
#'   of that facet, marginalizing over other facets.
#' @return A tibble suitable for heatmap plotting.
compute_rank_matrix <- function(combined,
                                metric,
                                higher_is_better = FALSE,
                                facet_level = NULL) {

  outcome_col <- attr(combined, "outcome_col") %||% "outcome"

  # Create context labels
  ranked <- combined %>%
    filter(!is.na(.data[[metric]])) %>%
    mutate(context = paste(industry, .data[[outcome_col]], sep = " | "))

  if (is.null(facet_level)) {
    # Full configuration ranking
    ranked <- ranked %>%
      group_by(context) %>%
      mutate(
        rank = if (higher_is_better) rank(-.data[[metric]], ties.method = "average")
               else rank(.data[[metric]], ties.method = "average"),
        rank_pct = rank / n()
      ) %>%
      ungroup()
  } else {
    # Facet-level: median rank percentile per level, marginalizing over others
    ranked <- ranked %>%
      group_by(context) %>%
      mutate(
        rank = if (higher_is_better) rank(-.data[[metric]], ties.method = "average")
               else rank(.data[[metric]], ties.method = "average"),
        rank_pct = rank / n()
      ) %>%
      ungroup() %>%
      group_by(context, .data[[facet_level]]) %>%
      summarize(
        median_rank_pct = median(rank_pct, na.rm = TRUE),
        mean_rank_pct   = mean(rank_pct, na.rm = TRUE),
        q25_rank_pct    = quantile(rank_pct, 0.25, na.rm = TRUE),
        q75_rank_pct    = quantile(rank_pct, 0.75, na.rm = TRUE),
        n_configs       = n(),
        .groups = "drop"
      )
  }

  return(ranked)
}


#' Plot facet-level concordance heatmap
#'
#' Shows whether a given facet level (e.g., "VADER") consistently ranks
#' well across all industry-outcome contexts.
#'
#' @param combined Combined results
#' @param metric Performance metric column
#' @param facets Character vector of facet names to plot (one heatmap each)
#' @param higher_is_better Metric direction
#' @return A patchwork of heatmaps
plot_facet_concordance_heatmaps <- function(combined,
                                            metric,
                                            facets = NULL,
                                            higher_is_better = FALSE) {

  if (is.null(facets)) {
    facets <- detect_config_facets(combined)
    message("Auto-detected facets: ", paste(facets, collapse = ", "))
  }

  # Filter to facets that actually vary
  facets <- facets[sapply(facets, function(f) {
    f %in% names(combined) && length(unique(na.omit(combined[[f]]))) > 1
  })]

  plots <- map(facets, function(facet) {
    rank_data <- compute_rank_matrix(combined, metric,
                                     higher_is_better = higher_is_better,
                                     facet_level = facet)

    # Shorten labels for display
    rank_data <- rank_data %>%
      mutate(level_label = abbreviate_level(.data[[facet]]))

    ggplot(rank_data, aes(x = context, y = level_label, fill = median_rank_pct)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.0f", median_rank_pct * 100)),
                size = 3, color = "black") +
      scale_fill_gradient2(
        low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
        midpoint = 0.5,
        limits = c(0, 1),
        name = "Median\nRank %ile",
        labels = scales::percent
      ) +
      labs(
        title = gsub("_", " ", facet) %>% str_to_title(),
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 12),
        panel.grid = element_blank(),
        legend.position = if (facet == last(facets)) "right" else "none"
      )
  })

  # Combine with patchwork
  combined_plot <- wrap_plots(plots, ncol = 1) +
    plot_annotation(
      title = paste("Configuration Concordance:", metric),
      subtitle = "Lower rank percentile (blue) = better performance. Consistent color across columns = defensible choice.",
      theme = theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40")
      )
    )

  return(combined_plot)
}


#' Helper: detect which columns are configuration facets
detect_config_facets <- function(df) {
  canonical <- c("embedding_model", "sentiment_model", "window_type",
                 "window_size", "composite_method", "composite_power",
                 "composite_temp", "threshold_k", "halflife")
  intersect(canonical, names(df))
}


#' Helper: shorten level labels for display
abbreviate_level <- function(x) {
  x <- as.character(x)
  x <- gsub("all-MiniLM-L6-v2", "MiniLM", x)
  x <- gsub("mxbai-embed-large-v1", "mxbai-large", x)
  x <- gsub("weighted_average", "wt_avg", x)
  x <- gsub("attention_weight", "attn", x)
  x <- gsub("power_attention", "pwr_attn", x)
  x <- gsub("cardiffnlp.*roberta", "cardiffnlp", x)
  x <- gsub("siebert.*", "siebert", x)
  return(x)
}


# ==============================================================================
# LAYER 2: KENDALL'S W CONCORDANCE
# ==============================================================================

#' Compute Kendall's W (coefficient of concordance) for configurations
#'
#' Treats each industry-outcome context as a "rater" that ranks configurations.
#' W near 1 = contexts agree on the ranking. W near 0 = no agreement.
#'
#' @param combined Combined results
#' @param metric Performance metric column
#' @param higher_is_better Metric direction
#' @param facet_level If NULL, tests concordance of full config rankings.
#'   If a facet name, tests concordance of that facet's level rankings.
#' @param n_perm Number of permutations for p-value (0 = asymptotic only)
#' @return List with W, chi-squared, df, p-value, and per-context rankings
compute_kendall_w <- function(combined,
                              metric,
                              higher_is_better = FALSE,
                              facet_level = NULL,
                              n_perm = 999) {

  outcome_col <- attr(combined, "outcome_col") %||% "outcome"

  if (!is.null(facet_level)) {
    # Facet-level: rank levels within each context by median performance
    rank_data <- combined %>%
      filter(!is.na(.data[[metric]])) %>%
      mutate(context = paste(industry, .data[[outcome_col]], sep = " | ")) %>%
      group_by(context, .data[[facet_level]]) %>%
      summarize(median_metric = median(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
      group_by(context) %>%
      mutate(
        rank = if (higher_is_better) rank(-median_metric, ties.method = "average")
               else rank(median_metric, ties.method = "average")
      ) %>%
      ungroup()

    # Pivot to matrix: rows = levels, cols = contexts
    rank_matrix <- rank_data %>%
      select(context, .data[[facet_level]], rank) %>%
      pivot_wider(names_from = context, values_from = rank) %>%
      column_to_rownames(facet_level)

  } else {
    # Full config: rank config_ids within each context
    rank_data <- combined %>%
      filter(!is.na(.data[[metric]])) %>%
      mutate(context = paste(industry, .data[[outcome_col]], sep = " | ")) %>%
      group_by(context) %>%
      mutate(
        rank = if (higher_is_better) rank(-.data[[metric]], ties.method = "average")
               else rank(.data[[metric]], ties.method = "average")
      ) %>%
      ungroup()

    # Only keep configs present in ALL contexts
    n_contexts <- length(unique(rank_data$context))
    common_configs <- rank_data %>%
      count(config_id) %>%
      filter(n == n_contexts) %>%
      pull(config_id)

    if (length(common_configs) < 3) {
      warning("Fewer than 3 configurations present in all contexts. ",
              "Consider using facet_level analysis instead.")
      return(list(W = NA, chi_sq = NA, df = NA, p_value = NA,
                  note = "Insufficient common configurations across contexts"))
    }

    rank_data <- rank_data %>% filter(config_id %in% common_configs)

    # Re-rank within filtered set
    rank_data <- rank_data %>%
      group_by(context) %>%
      mutate(rank = rank(rank, ties.method = "average")) %>%
      ungroup()

    rank_matrix <- rank_data %>%
      select(context, config_id, rank) %>%
      pivot_wider(names_from = context, values_from = rank) %>%
      column_to_rownames("config_id")
  }

  # Compute W
  R <- as.matrix(rank_matrix)
  if (any(is.na(R))) {
    # Drop rows/cols with NAs
    complete <- complete.cases(R)
    R <- R[complete, , drop = FALSE]
  }

  k <- ncol(R)  # number of raters (contexts)
  n <- nrow(R)  # number of objects (configs or levels)

  if (n < 2 || k < 2) {
    return(list(W = NA, chi_sq = NA, df = NA, p_value = NA,
                note = "Need at least 2 objects and 2 contexts"))
  }

  # Column sums of ranks
  R_bar <- colMeans(R)
  S <- sum((colSums(R) - k * (n + 1) / 2)^2)
  # Actually Kendall's W uses row sums deviation
  Rj <- rowSums(R)
  R_mean <- mean(Rj)
  S <- sum((Rj - R_mean)^2)
  W <- 12 * S / (k^2 * (n^3 - n))

  # Chi-squared approximation
  chi_sq <- k * (n - 1) * W
  df <- n - 1
  p_asymptotic <- pchisq(chi_sq, df, lower.tail = FALSE)

  # Permutation test
  p_perm <- NA
  if (n_perm > 0) {
    W_perm <- replicate(n_perm, {
      R_shuffled <- apply(R, 2, sample)
      Rj_s <- rowSums(R_shuffled)
      S_s <- sum((Rj_s - mean(Rj_s))^2)
      12 * S_s / (k^2 * (n^3 - n))
    })
    p_perm <- (sum(W_perm >= W) + 1) / (n_perm + 1)
  }

  return(list(
    W = W,
    chi_sq = chi_sq,
    df = df,
    p_asymptotic = p_asymptotic,
    p_permutation = p_perm,
    n_objects = n,
    n_contexts = k,
    rank_matrix = rank_matrix,
    facet = facet_level %||% "full_config"
  ))
}


#' Run concordance analysis for all facets
#'
#' @param combined Combined results
#' @param metric Performance metric column
#' @param higher_is_better Metric direction
#' @return A tibble summarizing W per facet, plus a row for full_config
compute_all_concordances <- function(combined,
                                     metric,
                                     higher_is_better = FALSE) {

  facets <- detect_config_facets(combined)
  results <- list()

  # Full-config concordance
  full <- compute_kendall_w(combined, metric, higher_is_better, facet_level = NULL)
  results[["full_config"]] <- tibble(
    facet = "full_config",
    W = full$W,
    chi_sq = full$chi_sq,
    p_asymptotic = full$p_asymptotic,
    p_permutation = full$p_permutation,
    n_objects = full$n_objects,
    n_contexts = full$n_contexts
  )

  # Per-facet concordance
  for (f in facets) {
    if (!(f %in% names(combined))) next
    n_levels <- length(unique(na.omit(combined[[f]])))
    if (n_levels < 2) next

    w_result <- compute_kendall_w(combined, metric, higher_is_better, facet_level = f)
    results[[f]] <- tibble(
      facet = f,
      W = w_result$W,
      chi_sq = w_result$chi_sq,
      p_asymptotic = w_result$p_asymptotic,
      p_permutation = w_result$p_permutation,
      n_objects = w_result$n_objects,
      n_contexts = w_result$n_contexts
    )
  }

  bind_rows(results) %>%
    arrange(desc(W))
}


# ==============================================================================
# LAYER 3: MIXED-EFFECTS DOMINANCE ANALYSIS
# ==============================================================================

#' Fit mixed-effects models for each configuration facet
#'
#' For each facet, fits: metric ~ facet_level + (1 | context)
#' where context = industry × outcome. This tests whether a facet level
#' has a consistent advantage after accounting for baseline differences
#' across industry-outcome pairs.
#'
#' @param combined Combined results
#' @param metric Performance metric column
#' @param facets Which facets to model (auto-detected if NULL)
#' @return List of model summaries per facet
fit_dominance_models <- function(combined,
                                 metric,
                                 facets = NULL) {

  # Check for lme4

  if (!requireNamespace("lme4", quietly = TRUE)) {
    warning("lme4 not installed. Install with: install.packages('lme4')")
    return(NULL)
  }

  outcome_col <- attr(combined, "outcome_col") %||% "outcome"

  if (is.null(facets)) {
    facets <- detect_config_facets(combined)
  }

  model_data <- combined %>%
    filter(!is.na(.data[[metric]])) %>%
    mutate(context = paste(industry, .data[[outcome_col]], sep = " | "))

  results <- list()

  for (f in facets) {
    if (!(f %in% names(model_data))) next
    n_levels <- length(unique(na.omit(model_data[[f]])))
    if (n_levels < 2) next

    # Ensure factor
    model_data[[f]] <- as.factor(model_data[[f]])

    # Fit model
    formula_str <- paste0("`", metric, "` ~ `", f, "` + (1 | context)")
    tryCatch({
      mod <- lme4::lmer(as.formula(formula_str), data = model_data,
                        REML = FALSE,
                        control = lme4::lmerControl(optimizer = "bobyqa"))

      # Extract fixed effects
      fe <- broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE)
      # Extract random effects variance
      re_var <- lme4::VarCorr(mod)
      context_var <- as.numeric(re_var$context)
      residual_var <- attr(re_var, "sc")^2
      icc <- context_var / (context_var + residual_var)

      results[[f]] <- list(
        facet = f,
        fixed_effects = fe,
        context_variance = context_var,
        residual_variance = residual_var,
        icc = icc,
        model = mod,
        interpretation = interpret_dominance(fe, f, icc)
      )
    }, error = function(e) {
      message("Model failed for facet '", f, "': ", e$message)
      results[[f]] <<- list(facet = f, error = e$message)
    })
  }

  return(results)
}


#' Generate interpretive text for dominance model results
interpret_dominance <- function(fixed_effects, facet_name, icc) {
  # Reference level is the intercept
  ref_level <- fixed_effects$term[1]
  contrasts <- fixed_effects %>% filter(term != "(Intercept)")

  if (nrow(contrasts) == 0) return("No contrasts available.")

  # Check if any contrasts are significant
  sig <- contrasts %>% filter(p.value < 0.05)
  nonsig <- contrasts %>% filter(p.value >= 0.05)

  lines <- c()

  if (icc > 0.5) {
    lines <- c(lines, sprintf(
      "Context (industry × outcome) explains %.0f%% of variance in %s performance (ICC = %.2f), indicating that performance differences between contexts dominate over facet effects.",
      icc * 100, facet_name, icc))
  } else if (icc > 0.1) {
    lines <- c(lines, sprintf(
      "Context explains a moderate %.0f%% of variance (ICC = %.2f).",
      icc * 100, icc))
  } else {
    lines <- c(lines, sprintf(
      "Context explains little variance (ICC = %.2f), suggesting %s effects are relatively stable across industries and outcomes.",
      icc, facet_name))
  }

  if (nrow(sig) > 0) {
    for (i in seq_len(nrow(sig))) {
      direction <- if (sig$estimate[i] > 0) "worse" else "better"
      # Flip if the metric is "higher = better"
      lines <- c(lines, sprintf(
        "  %s differs from reference by %.4f (p = %.3f)",
        sig$term[i], sig$estimate[i], sig$p.value[i]))
    }
  }

  if (nrow(nonsig) > 0) {
    lines <- c(lines, sprintf(
      "  %d level(s) show no significant difference from reference.",
      nrow(nonsig)))
  }

  paste(lines, collapse = "\n")
}


# ==============================================================================
# LAYER 2.5: FACET INTERACTION PROFILING
# ==============================================================================

#' Test whether facet effects are additive or interactive
#'
#' Compares models with vs. without interaction terms for pairs of facets.
#' If interactions are strong, no single facet can be recommended independently.
#'
#' @param combined Combined results
#' @param metric Performance metric column
#' @param facet_pairs List of length-2 character vectors, or NULL to test
#'   all pairs of detected facets.
#' @return Tibble with interaction test results per pair
test_facet_interactions <- function(combined,
                                    metric,
                                    facet_pairs = NULL) {

  if (!requireNamespace("lme4", quietly = TRUE)) {
    warning("lme4 not installed.")
    return(NULL)
  }

  outcome_col <- attr(combined, "outcome_col") %||% "outcome"
  facets <- detect_config_facets(combined)

  if (is.null(facet_pairs)) {
    facet_pairs <- combn(facets, 2, simplify = FALSE)
  }

  model_data <- combined %>%
    filter(!is.na(.data[[metric]])) %>%
    mutate(context = paste(industry, .data[[outcome_col]], sep = " | "))

  results <- map_dfr(facet_pairs, function(pair) {
    f1 <- pair[1]; f2 <- pair[2]
    if (!(f1 %in% names(model_data)) || !(f2 %in% names(model_data))) {
      return(tibble(facet1 = f1, facet2 = f2, note = "missing column"))
    }
    # Need sufficient data for interaction
    n_combos <- model_data %>%
      count(.data[[f1]], .data[[f2]]) %>%
      nrow()
    if (n_combos < 4) {
      return(tibble(facet1 = f1, facet2 = f2, note = "too few combinations"))
    }

    model_data[[f1]] <- as.factor(model_data[[f1]])
    model_data[[f2]] <- as.factor(model_data[[f2]])

    tryCatch({
      form_add <- as.formula(paste0(
        "`", metric, "` ~ `", f1, "` + `", f2, "` + (1|context)"))
      form_int <- as.formula(paste0(
        "`", metric, "` ~ `", f1, "` * `", f2, "` + (1|context)"))

      mod_add <- lme4::lmer(form_add, data = model_data, REML = FALSE,
                            control = lme4::lmerControl(optimizer = "bobyqa"))
      mod_int <- lme4::lmer(form_int, data = model_data, REML = FALSE,
                            control = lme4::lmerControl(optimizer = "bobyqa"))

      lr_test <- anova(mod_add, mod_int)

      tibble(
        facet1 = f1,
        facet2 = f2,
        aic_additive = AIC(mod_add),
        aic_interaction = AIC(mod_int),
        delta_aic = AIC(mod_add) - AIC(mod_int),
        lr_chisq = lr_test$Chisq[2],
        lr_df = lr_test$Df[2],
        lr_pvalue = lr_test$`Pr(>Chisq)`[2],
        interaction_important = lr_test$`Pr(>Chisq)`[2] < 0.05
      )
    }, error = function(e) {
      tibble(facet1 = f1, facet2 = f2, note = e$message)
    })
  })

  return(results)
}


# ==============================================================================
# LAYER 4: QUALITATIVE COMPARATIVE ANALYSIS (QCA)
# ==============================================================================
#
# QCA adds a fundamentally different logic to the analysis. Layers 1-3 ask
# "which individual facets matter?" — a correlational/net-effects question.
# QCA asks "which *combinations* of choices are sufficient for good outcomes?"
# — a set-theoretic/configurational question.
#
# Key QCA concepts for this application:
#   - Equifinality: Multiple distinct configuration paths may each be
#     sufficient for good performance (e.g., VADER+MiniLM+EWMA *or*
#     siebert+mxbai+SMA could both work well).
#   - Conjunctural causation: A facet level might only matter in
#     combination with specific other choices (e.g., VADER is only good
#     when paired with weighted-average compositing).
#   - Asymmetric causation: Conditions for good performance may differ
#     from conditions for poor performance.
#
# We implement both within-context QCA (one truth table per industry-outcome)
# and a pooled/cross-context approach that identifies configurations
# consistently sufficient across contexts.
# ==============================================================================

#' Prepare QCA data from multiverse results
#'
#' Calibrates performance into fuzzy-set membership and encodes
#' configuration facets as conditions.
#'
#' @param combined Combined results from load_and_combine_results()
#' @param metric Performance metric column name
#' @param higher_is_better Metric direction
#' @param outcome_threshold Quantile threshold for "good performance"
#'   outcome set membership. Default 0.75 = top quartile.
#' @param calibration Method for outcome calibration:
#'   "crisp" = binary above/below threshold,
#'   "fuzzy" = continuous calibration using anchor points.
#' @param facets Which facets to include as conditions (auto-detect if NULL)
#' @return List with calibrated data, condition names, and metadata
prepare_qca_data <- function(combined,
                             metric,
                             higher_is_better = FALSE,
                             outcome_threshold = 0.75,
                             calibration = c("crisp", "fuzzy"),
                             facets = NULL) {

  calibration <- match.arg(calibration)
  outcome_col <- attr(combined, "outcome_col") %||% "outcome"

  if (is.null(facets)) {
    facets <- detect_config_facets(combined)
  }
  # Keep only varying facets that exist
  facets <- facets[sapply(facets, function(f) {
    f %in% names(combined) && length(unique(na.omit(combined[[f]]))) > 1
  })]

  if (length(facets) == 0) {
    warning("No varying facets found for QCA.")
    return(NULL)
  }
  if (length(facets) > 7) {
    message("Note: ", length(facets), " conditions is high for QCA. ",
            "Consider selecting the most theoretically important 5-6.")
  }

  qca_data <- combined %>%
    filter(!is.na(.data[[metric]])) %>%
    mutate(context = paste(industry, .data[[outcome_col]], sep = " | "))

  # --- Calibrate outcome ---
  if (calibration == "crisp") {
    # Within each context, is this config in the top quartile?
    qca_data <- qca_data %>%
      group_by(context) %>%
      mutate(
        outcome_rank_pct = if (higher_is_better) {
          rank(.data[[metric]]) / n()
        } else {
          rank(-.data[[metric]]) / n()
        },
        OUTCOME = as.integer(outcome_rank_pct >= outcome_threshold)
      ) %>%
      ungroup()
  } else {
    # Fuzzy calibration: use within-context percentiles as fuzzy membership
    # Anchors: fully out at 10th pctile, crossover at 50th, fully in at 90th
    qca_data <- qca_data %>%
      group_by(context) %>%
      mutate(
        p10 = quantile(.data[[metric]], 0.10, na.rm = TRUE),
        p50 = quantile(.data[[metric]], 0.50, na.rm = TRUE),
        p90 = quantile(.data[[metric]], 0.90, na.rm = TRUE)
      ) %>%
      ungroup()

    # Direct calibration (logistic scaling between anchors)
    if (higher_is_better) {
      qca_data <- qca_data %>%
        mutate(OUTCOME = calibrate_fuzzy(.data[[metric]], p90, p50, p10))
    } else {
      # Lower is better: flip anchors
      qca_data <- qca_data %>%
        mutate(OUTCOME = calibrate_fuzzy(.data[[metric]], p10, p50, p90))
    }
    qca_data <- qca_data %>% select(-p10, -p50, -p90)
  }

  # --- Encode conditions ---
  # For QCA, multi-level facets need to be dummy-coded or treated as
  # multi-value conditions. We use dummy coding for crisp-set QCA
  # (each level becomes a binary condition).
  condition_cols <- c()

  for (f in facets) {
    levels_f <- sort(unique(na.omit(qca_data[[f]])))

    if (length(levels_f) == 2) {
      # Binary: single condition (1 = second level)
      cond_name <- toupper(gsub("[^[:alnum:]]", "", f))
      qca_data[[cond_name]] <- as.integer(qca_data[[f]] == levels_f[2])
      condition_cols <- c(condition_cols, cond_name)

    } else if (is.numeric(qca_data[[f]])) {
      # Continuous: calibrate as fuzzy set
      cond_name <- toupper(gsub("[^[:alnum:]]", "", f))
      vals <- qca_data[[f]]
      qca_data[[cond_name]] <- (vals - min(vals, na.rm = TRUE)) /
        (max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE))
      condition_cols <- c(condition_cols, cond_name)

    } else {
      # Multi-level categorical: dummy code (drop first as reference)
      for (lev in levels_f[-1]) {
        cond_name <- paste0(
          toupper(gsub("[^[:alnum:]]", "", f)), "_",
          toupper(gsub("[^[:alnum:]]", "", abbreviate_level(lev)))
        )
        qca_data[[cond_name]] <- as.integer(qca_data[[f]] == lev)
        condition_cols <- c(condition_cols, cond_name)
      }
    }
  }

  return(list(
    data = qca_data,
    conditions = condition_cols,
    outcome = "OUTCOME",
    facets = facets,
    calibration = calibration,
    threshold = outcome_threshold,
    n_contexts = length(unique(qca_data$context)),
    contexts = unique(qca_data$context)
  ))
}


#' Simple fuzzy calibration using logistic function
#' @param x Values to calibrate
#' @param fully_in Anchor for full membership (0.95)
#' @param crossover Anchor for crossover point (0.5)
#' @param fully_out Anchor for full non-membership (0.05)
#' @return Fuzzy membership scores in [0.001, 0.999]
calibrate_fuzzy <- function(x, fully_in, crossover, fully_out) {
  # QCA package has calibrate(), but we provide a standalone version
  # Uses logistic curve through three anchors
  # Simplified: linear interpolation between anchors, then logistic squash
  x <- as.numeric(x)
  fi <- as.numeric(fully_in)
  co <- as.numeric(crossover)
  fo <- as.numeric(fully_out)

  # Scale to [-6, 6] (logistic maps this to ~[0.0025, 0.9975])
  scaled <- case_when(
    x >= fi ~ 6,
    x <= fo ~ -6,
    x >= co ~ 6 * (x - co) / (fi - co),
    TRUE     ~ -6 * (co - x) / (co - fo)
  )

  membership <- 1 / (1 + exp(-scaled))
  # Clamp to avoid exact 0 or 1 (QCA requires strict fuzzy)
  pmin(pmax(membership, 0.001), 0.999)
}


#' Run QCA analysis within each context
#'
#' Performs truth table analysis and logical minimization per
#' industry-outcome context.
#'
#' @param qca_prep Output from prepare_qca_data()
#' @param incl_cut Inclusion/consistency threshold for truth table rows
#'   (typically 0.80 for crisp, 0.75-0.85 for fuzzy)
#' @param n_cut Minimum number of cases per truth table row
#' @param sol_type Solution type: "C" (complex/conservative), "P" (parsimonious),
#'   "E" (enhanced/intermediate with easy counterfactuals)
#' @return List with per-context solutions and a cross-context summary
run_qca_per_context <- function(qca_prep,
                                incl_cut = 0.80,
                                n_cut = 1,
                                sol_type = "C") {

  if (!requireNamespace("QCA", quietly = TRUE)) {
    warning("QCA package not installed. Install with: install.packages('QCA')")
    return(NULL)
  }

  conditions <- qca_prep$conditions
  outcome <- qca_prep$outcome

  context_results <- list()

  for (ctx in qca_prep$contexts) {
    ctx_data <- qca_prep$data %>%
      filter(context == ctx) %>%
      select(all_of(c(conditions, outcome))) %>%
      as.data.frame()

    if (nrow(ctx_data) < 5) {
      context_results[[ctx]] <- list(context = ctx, note = "Too few cases")
      next
    }

    tryCatch({
      # Truth table
      tt <- QCA::truthTable(
        ctx_data,
        outcome = outcome,
        conditions = conditions,
        incl.cut = incl_cut,
        n.cut = n_cut,
        show.cases = TRUE,
        complete = TRUE,
        sort.by = c("incl", "n")
      )

      # Minimization
      sol <- QCA::minimize(
        tt,
        details = TRUE,
        show.cases = TRUE
      )

      # Extract solution paths
      paths <- extract_solution_paths(sol, conditions)

      context_results[[ctx]] <- list(
        context = ctx,
        truth_table = tt,
        solution = sol,
        paths = paths,
        n_cases = nrow(ctx_data),
        n_paths = length(paths),
        overall_coverage = if (!is.null(sol$IC$sol.cov)) sol$IC$sol.cov[1] else NA,
        overall_consistency = if (!is.null(sol$IC$sol.incl)) sol$IC$sol.incl[1] else NA
      )
    }, error = function(e) {
      context_results[[ctx]] <<- list(context = ctx, error = e$message)
    })
  }

  # Cross-context comparison: which paths appear in multiple contexts?
  cross_ctx <- compare_paths_across_contexts(context_results)

  return(list(
    per_context = context_results,
    cross_context = cross_ctx,
    parameters = list(incl_cut = incl_cut, n_cut = n_cut, sol_type = sol_type)
  ))
}


#' Run pooled QCA across all contexts
#'
#' Treats all contexts together. A configuration that is sufficient for
#' good performance in the pooled analysis is one that works broadly,
#' not just in one industry-outcome pair.
#'
#' @param qca_prep Output from prepare_qca_data()
#' @param incl_cut Consistency threshold
#' @param n_cut Minimum cases per row
#' @return QCA solution for pooled data
run_qca_pooled <- function(qca_prep,
                           incl_cut = 0.80,
                           n_cut = 2) {

  if (!requireNamespace("QCA", quietly = TRUE)) {
    warning("QCA package not installed.")
    return(NULL)
  }

  conditions <- qca_prep$conditions
  outcome <- qca_prep$outcome

  pooled_data <- qca_prep$data %>%
    select(all_of(c(conditions, outcome))) %>%
    as.data.frame()

  tryCatch({
    tt <- QCA::truthTable(
      pooled_data,
      outcome = outcome,
      conditions = conditions,
      incl.cut = incl_cut,
      n.cut = n_cut,
      show.cases = TRUE,
      complete = TRUE,
      sort.by = c("incl", "n")
    )

    sol <- QCA::minimize(
      tt,
      details = TRUE,
      show.cases = TRUE
    )

    paths <- extract_solution_paths(sol, conditions)

    return(list(
      truth_table = tt,
      solution = sol,
      paths = paths,
      n_cases = nrow(pooled_data),
      n_paths = length(paths),
      overall_coverage = if (!is.null(sol$IC$sol.cov)) sol$IC$sol.cov[1] else NA,
      overall_consistency = if (!is.null(sol$IC$sol.incl)) sol$IC$sol.incl[1] else NA
    ))
  }, error = function(e) {
    warning("Pooled QCA failed: ", e$message)
    return(list(error = e$message))
  })
}


#' Extract readable solution paths from QCA minimize output
extract_solution_paths <- function(sol, conditions) {
  if (is.null(sol) || is.null(sol$essential)) return(list())

  # The solution's essential primes or solution components
  paths <- list()

  # Try to extract from the solution object
  # QCA::minimize returns different structures depending on solution type
  tryCatch({
    if (!is.null(sol$solution)) {
      for (i in seq_along(sol$solution)) {
        sol_set <- sol$solution[[i]]
        for (j in seq_along(sol_set)) {
          path_name <- sol_set[j]
          paths[[path_name]] <- list(
            expression = path_name,
            index = paste(i, j, sep = "."),
            # Extract per-path coverage and consistency if available
            raw_coverage = tryCatch(sol$IC$individual[[i]]$covS[j], error = function(e) NA),
            unique_coverage = tryCatch(sol$IC$individual[[i]]$covU[j], error = function(e) NA),
            consistency = tryCatch(sol$IC$individual[[i]]$inclS[j], error = function(e) NA)
          )
        }
      }
    }
  }, error = function(e) {
    message("Could not extract solution paths: ", e$message)
  })

  return(paths)
}


#' Compare solution paths across contexts
#'
#' Identifies paths that appear in solutions for multiple industry-outcome
#' contexts — these are the most defensible configurations.
compare_paths_across_contexts <- function(context_results) {
  # Collect all paths with their contexts
  all_paths <- list()

  for (ctx_name in names(context_results)) {
    ctx <- context_results[[ctx_name]]
    if (is.null(ctx$paths) || length(ctx$paths) == 0) next

    for (path_name in names(ctx$paths)) {
      if (is.null(all_paths[[path_name]])) {
        all_paths[[path_name]] <- list(
          expression = path_name,
          contexts = c(),
          consistency_vals = c(),
          coverage_vals = c()
        )
      }
      all_paths[[path_name]]$contexts <- c(
        all_paths[[path_name]]$contexts, ctx_name)
      all_paths[[path_name]]$consistency_vals <- c(
        all_paths[[path_name]]$consistency_vals,
        ctx$paths[[path_name]]$consistency)
      all_paths[[path_name]]$coverage_vals <- c(
        all_paths[[path_name]]$coverage_vals,
        ctx$paths[[path_name]]$raw_coverage)
    }
  }

  # Rank by number of contexts in which each path appears
  n_total_contexts <- length(context_results)

  path_summary <- map_dfr(names(all_paths), function(p) {
    info <- all_paths[[p]]
    tibble(
      path = p,
      n_contexts = length(info$contexts),
      pct_contexts = length(info$contexts) / n_total_contexts,
      contexts = paste(info$contexts, collapse = "; "),
      mean_consistency = mean(info$consistency_vals, na.rm = TRUE),
      mean_coverage = mean(info$coverage_vals, na.rm = TRUE)
    )
  }) %>%
    arrange(desc(n_contexts), desc(mean_consistency))

  return(path_summary)
}


#' Visualize QCA results: path frequency across contexts
#'
#' @param qca_results Output from run_qca_per_context()
#' @param max_paths Maximum paths to display
#' @return ggplot
plot_qca_cross_context <- function(qca_results, max_paths = 15) {

  cross_ctx <- qca_results$cross_context
  if (is.null(cross_ctx) || nrow(cross_ctx) == 0) {
    message("No cross-context paths to plot.")
    return(NULL)
  }

  plot_data <- cross_ctx %>%
    head(max_paths) %>%
    mutate(
      path_short = str_trunc(path, 50),
      path_short = fct_reorder(path_short, n_contexts)
    )

  ggplot(plot_data, aes(x = n_contexts, y = path_short)) +
    geom_col(aes(fill = mean_consistency), width = 0.7) +
    geom_text(aes(label = sprintf("cov=%.2f", mean_coverage)),
              hjust = -0.1, size = 3) +
    scale_fill_gradient(low = "#fee0d2", high = "#de2d26",
                        name = "Mean\nConsistency",
                        limits = c(0.5, 1)) +
    scale_x_continuous(breaks = seq(1, max(plot_data$n_contexts)),
                       expand = expansion(mult = c(0, 0.3))) +
    labs(
      title = "QCA Solution Paths Across Contexts",
      subtitle = "Paths appearing in more contexts are more defensible configurations",
      x = "Number of contexts where path is sufficient",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 8, family = "mono"),
      plot.subtitle = element_text(size = 9, color = "gray40")
    )
}


#' Convenience wrapper: run full QCA analysis (both per-context and pooled)
#'
#' @param combined Combined results from load_and_combine_results()
#' @param metric Performance metric column
#' @param higher_is_better Metric direction
#' @param calibration "crisp" or "fuzzy"
#' @param outcome_threshold Quantile threshold for "good" outcome
#' @param incl_cut Consistency cutoff
#' @param facets Facets to use as conditions (NULL = auto-detect)
#' @return List with per-context results, pooled results, and plots
run_qca_analysis <- function(combined,
                             metric,
                             higher_is_better = FALSE,
                             calibration = "crisp",
                             outcome_threshold = 0.75,
                             incl_cut = 0.80,
                             facets = NULL) {

  message("\n=== Layer 4: Qualitative Comparative Analysis ===")

  # Prepare data
  qca_prep <- prepare_qca_data(
    combined, metric,
    higher_is_better = higher_is_better,
    outcome_threshold = outcome_threshold,
    calibration = calibration,
    facets = facets
  )

  if (is.null(qca_prep)) return(NULL)

  message("QCA conditions: ", paste(qca_prep$conditions, collapse = ", "))
  message("Calibration: ", calibration, " | Outcome threshold: ", outcome_threshold)
  message("Contexts: ", qca_prep$n_contexts)

  # Per-context analysis
  message("\n--- Per-context QCA ---")
  per_context <- run_qca_per_context(qca_prep, incl_cut = incl_cut)

  if (!is.null(per_context)) {
    for (ctx in names(per_context$per_context)) {
      r <- per_context$per_context[[ctx]]
      if (!is.null(r$n_paths)) {
        message(sprintf("  %s: %d paths (coverage=%.2f, consistency=%.2f)",
                        ctx, r$n_paths,
                        r$overall_coverage %||% NA,
                        r$overall_consistency %||% NA))
      } else if (!is.null(r$error)) {
        message(sprintf("  %s: FAILED — %s", ctx, r$error))
      }
    }

    if (!is.null(per_context$cross_context) && nrow(per_context$cross_context) > 0) {
      message("\nCross-context path summary (top 5):")
      print(per_context$cross_context %>% head(5) %>%
              select(path, n_contexts, pct_contexts, mean_consistency))
    }
  }

  # Pooled analysis
  message("\n--- Pooled QCA ---")
  pooled <- run_qca_pooled(qca_prep, incl_cut = incl_cut)

  if (!is.null(pooled) && is.null(pooled$error)) {
    message(sprintf("Pooled: %d paths (coverage=%.2f, consistency=%.2f)",
                    pooled$n_paths,
                    pooled$overall_coverage %||% NA,
                    pooled$overall_consistency %||% NA))
  }

  # Visualization
  plot_cross <- NULL
  if (!is.null(per_context)) {
    plot_cross <- plot_qca_cross_context(per_context)
  }

  return(list(
    preparation = qca_prep,
    per_context = per_context,
    pooled = pooled,
    plot_cross_context = plot_cross
  ))
}


# ==============================================================================
# INTEGRATED ANALYSIS WRAPPER
# ==============================================================================

#' Run full concordance analysis pipeline
#'
#' @param combined Combined results from load_and_combine_results()
#' @param metric Performance metric column name
#' @param higher_is_better Metric direction
#' @param run_dominance Whether to fit mixed-effects models (requires lme4)
#' @param run_interactions Whether to test facet interactions
#' @param run_qca Whether to run QCA analysis (requires QCA package)
#' @param qca_calibration "crisp" or "fuzzy" for QCA outcome calibration
#' @param qca_threshold Quantile threshold for QCA outcome set membership
#' @return Named list with all results and plots
analyze_configuration_concordance <- function(combined,
                                               metric,
                                               higher_is_better = FALSE,
                                               run_dominance = TRUE,
                                               run_interactions = TRUE,
                                               run_qca = TRUE,
                                               qca_calibration = "crisp",
                                               qca_threshold = 0.75) {

  results <- list()

  # --- Layer 1: Heatmaps ---
  message("\n=== Layer 1: Configuration Concordance Heatmaps ===")
  results$plots$facet_heatmap <- plot_facet_concordance_heatmaps(
    combined, metric, higher_is_better = higher_is_better
  )

  # --- Layer 2: Kendall's W ---
  message("\n=== Layer 2: Kendall's W Concordance ===")
  results$kendall_w <- compute_all_concordances(
    combined, metric, higher_is_better = higher_is_better
  )
  message("\nConcordance summary:")
  print(results$kendall_w %>% select(facet, W, p_permutation, n_objects, n_contexts))

  # --- Layer 2.5: Interaction profiling ---
  if (run_interactions) {
    message("\n=== Layer 2.5: Facet Interaction Tests ===")
    results$interactions <- test_facet_interactions(combined, metric)
    if (!is.null(results$interactions) && nrow(results$interactions) > 0) {
      sig_int <- results$interactions %>% filter(interaction_important == TRUE)
      if (nrow(sig_int) > 0) {
        message("Significant interactions found:")
        print(sig_int %>% select(facet1, facet2, delta_aic, lr_pvalue))
      } else {
        message("No significant facet interactions detected — additive effects are sufficient.")
      }
    }
  }

  # --- Layer 3: Mixed-effects dominance ---
  if (run_dominance) {
    message("\n=== Layer 3: Mixed-Effects Dominance Analysis ===")
    results$dominance_models <- fit_dominance_models(combined, metric)
    if (!is.null(results$dominance_models)) {
      for (f in names(results$dominance_models)) {
        mod <- results$dominance_models[[f]]
        if (!is.null(mod$interpretation)) {
          message("\n--- ", f, " ---")
          message(mod$interpretation)
        }
      }
    }
  }

  # --- Layer 4: QCA ---
  if (run_qca) {
    results$qca <- run_qca_analysis(
      combined, metric,
      higher_is_better = higher_is_better,
      calibration = qca_calibration,
      outcome_threshold = qca_threshold
    )
    if (!is.null(results$qca$plot_cross_context)) {
      results$plots$qca_cross_context <- results$qca$plot_cross_context
    }
  }

  # --- Summary report ---
  results$summary <- generate_defensibility_summary(results)

  return(results)
}


#' Generate plain-language summary of defensibility findings
generate_defensibility_summary <- function(results) {
  lines <- c("CONFIGURATION DEFENSIBILITY SUMMARY",
             "===================================", "")

  # Kendall W interpretation
  if (!is.null(results$kendall_w)) {
    w_data <- results$kendall_w
    high_w <- w_data %>% filter(W > 0.7, facet != "full_config")
    mod_w  <- w_data %>% filter(W > 0.3, W <= 0.7, facet != "full_config")
    low_w  <- w_data %>% filter(W <= 0.3, facet != "full_config")

    if (nrow(high_w) > 0) {
      lines <- c(lines, "STRONG CONCORDANCE (W > 0.7):",
                 paste(" ", high_w$facet, "(W =", round(high_w$W, 3), ")"),
                 "  -> These facets have a defensible 'best' level across contexts.", "")
    }
    if (nrow(mod_w) > 0) {
      lines <- c(lines, "MODERATE CONCORDANCE (0.3 < W <= 0.7):",
                 paste(" ", mod_w$facet, "(W =", round(mod_w$W, 3), ")"),
                 "  -> Some consistency, but context-dependent variation exists.", "")
    }
    if (nrow(low_w) > 0) {
      lines <- c(lines, "LOW CONCORDANCE (W <= 0.3):",
                 paste(" ", low_w$facet, "(W =", round(low_w$W, 3), ")"),
                 "  -> This choice is context-dependent; no single level dominates.", "")
    }
  }

  # Interaction summary
  if (!is.null(results$interactions)) {
    sig_int <- results$interactions %>%
      filter(!is.na(interaction_important)) %>%
      filter(interaction_important == TRUE)
    if (nrow(sig_int) > 0) {
      lines <- c(lines, "FACET INTERACTIONS:",
                 paste("  ", sig_int$facet1, "×", sig_int$facet2,
                       "(ΔAIC =", round(sig_int$delta_aic, 1), ")"),
                 "  -> These facets should be considered jointly, not independently.", "")
    } else {
      lines <- c(lines, "FACET INTERACTIONS:",
                 "  No significant interactions detected.",
                 "  -> Facet effects appear additive; independent recommendations are defensible.", "")
    }
  }

  # Dominance highlights
  if (!is.null(results$dominance_models)) {
    lines <- c(lines, "DOMINANCE ANALYSIS HIGHLIGHTS:")
    for (f in names(results$dominance_models)) {
      mod <- results$dominance_models[[f]]
      if (!is.null(mod$icc)) {
        lines <- c(lines, sprintf("  %s: ICC = %.2f", f, mod$icc))
      }
    }
    lines <- c(lines, "")
  }

  # QCA summary
  if (!is.null(results$qca)) {
    lines <- c(lines, "QCA CONFIGURATIONAL ANALYSIS:")

    # Pooled results
    pooled <- results$qca$pooled
    if (!is.null(pooled) && is.null(pooled$error) && !is.null(pooled$n_paths)) {
      lines <- c(lines, sprintf(
        "  Pooled analysis: %d sufficient paths (coverage=%.2f, consistency=%.2f)",
        pooled$n_paths,
        pooled$overall_coverage %||% NA,
        pooled$overall_consistency %||% NA))
    }

    # Cross-context paths
    cross_ctx <- results$qca$per_context$cross_context
    if (!is.null(cross_ctx) && nrow(cross_ctx) > 0) {
      n_ctx <- results$qca$preparation$n_contexts
      universal <- cross_ctx %>% filter(n_contexts == n_ctx)
      broad <- cross_ctx %>% filter(n_contexts >= n_ctx * 0.5, n_contexts < n_ctx)

      if (nrow(universal) > 0) {
        lines <- c(lines,
                   sprintf("  %d path(s) sufficient in ALL %d contexts (universal):",
                           nrow(universal), n_ctx),
                   paste("   ", universal$path))
      }
      if (nrow(broad) > 0) {
        lines <- c(lines,
                   sprintf("  %d path(s) sufficient in ≥50%% of contexts:",
                           nrow(broad)),
                   paste("   ", broad$path,
                         sprintf("(%d/%d contexts)", broad$n_contexts, n_ctx)))
      }
      if (nrow(universal) == 0 && nrow(broad) == 0) {
        lines <- c(lines,
                   "  No paths found that generalize across ≥50% of contexts.",
                   "  -> Configuration requirements are highly context-specific.")
      }
    }
    lines <- c(lines, "")

    # Equifinality assessment
    per_ctx <- results$qca$per_context$per_context
    n_paths_per_ctx <- sapply(per_ctx, function(r) r$n_paths %||% 0)
    if (any(n_paths_per_ctx > 1)) {
      lines <- c(lines, "EQUIFINALITY:",
                 sprintf("  %d of %d contexts have multiple sufficient paths,",
                         sum(n_paths_per_ctx > 1), length(n_paths_per_ctx)),
                 "  indicating equifinality — multiple distinct configurations",
                 "  can produce good performance.", "")
    }
  }

  report <- paste(lines, collapse = "\n")
  message("\n", report)
  return(report)
}


# ==============================================================================
# ADDITIONAL VISUALIZATION: SPECIFICATION PROFILE PLOT
# ==============================================================================

#' Profile plot showing how each facet level performs across contexts
#'
#' Similar to an interaction plot in ANOVA. Lines that are parallel
#' indicate consistent effects; crossing lines indicate context-dependence.
#'
#' @param combined Combined results
#' @param metric Performance metric column
#' @param facet Which configuration facet to profile
#' @param higher_is_better Metric direction
#' @return A ggplot
plot_facet_profile <- function(combined,
                               metric,
                               facet,
                               higher_is_better = FALSE) {

  outcome_col <- attr(combined, "outcome_col") %||% "outcome"

  profile_data <- combined %>%
    filter(!is.na(.data[[metric]])) %>%
    mutate(context = paste(industry, .data[[outcome_col]], sep = " | ")) %>%
    group_by(context, .data[[facet]]) %>%
    summarize(
      median_metric = median(.data[[metric]], na.rm = TRUE),
      q25 = quantile(.data[[metric]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[metric]], 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(level_label = abbreviate_level(.data[[facet]]))

  ggplot(profile_data, aes(x = context, y = median_metric,
                           color = level_label, group = level_label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.15, alpha = 0.5) +
    scale_color_brewer(palette = "Set2", name = str_to_title(gsub("_", " ", facet))) +
    labs(
      title = paste("Profile:", str_to_title(gsub("_", " ", facet))),
      subtitle = "Parallel lines = consistent effect; crossing = context-dependent",
      x = NULL,
      y = paste("Median", metric)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size = 9, color = "gray40")
    )
}


#' Plot profiles for all facets
plot_all_facet_profiles <- function(combined,
                                    metric,
                                    higher_is_better = FALSE) {

  facets <- detect_config_facets(combined)
  facets <- facets[sapply(facets, function(f) {
    f %in% names(combined) && length(unique(na.omit(combined[[f]]))) > 1
  })]

  plots <- map(facets, ~ plot_facet_profile(combined, metric, .x, higher_is_better))
  wrap_plots(plots, ncol = 2) +
    plot_annotation(
      title = paste("Facet Performance Profiles:", metric),
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
}
