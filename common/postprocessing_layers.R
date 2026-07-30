# =============================================================================
# common/postprocessing_layers.R — Layer-Aware Postprocessing Extensions
# =============================================================================
#
# Extensions to postprocessing.R and configuration_concordance.R for handling
# temporal and EWS feature layer configurations in spec curves, concordance
# analysis, and reporting.
#
# Source this AFTER postprocessing.R and configuration_concordance.R.
# It adds/wraps functions rather than replacing them.
#
# New capabilities:
#   - Parse temporal_config_id and ews_config_id into readable facets
#   - Augment results with layer facet columns for concordance/plotting
#   - Filter results by layer configuration
#   - Compare effect sizes across layer configurations
#   - Summarize layer impact on climate estimates
#
# Dependencies: dplyr, tidyr, stringr, ggplot2, glue
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(glue)
})


# =============================================================================
# 1. PARSE LAYER CONFIG IDS INTO FACETS
# =============================================================================

#' Parse temporal_config_id into readable components
#'
#' Extracts window mode, window type, window size, and burstiness method
#' from temporal config IDs like:
#'   "tf_matched_goh", "tf_fixed_count20_kimjo_periodic",
#'   "tf_fixed_time360d_goh"
#'
#' @param temporal_config_ids Character vector of temporal config IDs
#' @return Tibble with temporal_config_id + parsed columns
parse_temporal_config <- function(temporal_config_ids) {
  tibble(temporal_config_id = unique(temporal_config_ids)) %>%
    mutate(
      tf_window_mode = case_when(
        str_detect(temporal_config_id, "^tf_matched") ~ "matched",
        str_detect(temporal_config_id, "^tf_fixed")   ~ "fixed",
        str_detect(temporal_config_id, "^t_")         ~ "legacy",
        TRUE                                          ~ "unknown"
      ),
      tf_window_type = case_when(
        tf_window_mode == "matched"                              ~ "matched",
        str_detect(temporal_config_id, "count")                  ~ "count",
        str_detect(temporal_config_id, "time")                   ~ "time",
        TRUE                                                     ~ NA_character_
      ),
      tf_window_size = case_when(
        str_detect(temporal_config_id, "count(\\d+)")            ~
          str_extract(temporal_config_id, "count(\\d+)", group = 1),
        str_detect(temporal_config_id, "time(\\d+)")             ~
          str_extract(temporal_config_id, "time(\\d+)", group = 1),
        TRUE                                                     ~ NA_character_
      ),
      tf_burstiness_method = case_when(
        str_detect(temporal_config_id, "kimjo_periodic")         ~ "kimjo_periodic",
        str_detect(temporal_config_id, "kimjo_open")             ~ "kimjo_open",
        str_detect(temporal_config_id, "goh")                    ~ "goh",
        TRUE                                                     ~ NA_character_
      ),
      # Human-readable label for plots
      tf_label = case_when(
        tf_window_mode == "matched" ~ paste0("Matched (", tf_burstiness_method, ")"),
        tf_window_type == "count"   ~ paste0("Fixed-", tf_window_size, " (", tf_burstiness_method, ")"),
        tf_window_type == "time"    ~ paste0("Fixed-", tf_window_size, "d (", tf_burstiness_method, ")"),
        TRUE                        ~ temporal_config_id
      )
    )
}


#' Parse ews_config_id into readable components
#'
#' Extracts detrend method and window size from EWS config IDs like:
#'   "ews_linear_w50", "ews_none_w100"
#'
#' @param ews_config_ids Character vector of EWS config IDs
#' @return Tibble with ews_config_id + parsed columns
parse_ews_config <- function(ews_config_ids) {
  tibble(ews_config_id = unique(ews_config_ids)) %>%
    mutate(
      ews_detrend = str_extract(ews_config_id, "(?<=ews_)[^_]+"),
      ews_window_size = str_extract(ews_config_id, "(?<=_w)\\d+"),
      # Human-readable label
      ews_label = paste0(ews_detrend, " (w=", ews_window_size, ")")
    )
}


# =============================================================================
# 2. AUGMENT RESULTS WITH PARSED LAYER FACETS
# =============================================================================

#' Add parsed layer facet columns to MV or CV results
#'
#' Joins parsed temporal and EWS facets onto results for use in
#' spec curves, concordance, and reporting. Safe to call on results
#' that don't have layer columns (paper 1) — returns unchanged.
#'
#' @param results Tibble of MV or CV results
#' @return results with additional parsed facet columns
augment_layer_facets <- function(results) {

  if ("temporal_config_id" %in% names(results)) {
    tf_parsed <- parse_temporal_config(results$temporal_config_id)
    results <- results %>%
      left_join(tf_parsed, by = "temporal_config_id")
  }

  if ("ews_config_id" %in% names(results)) {
    ews_parsed <- parse_ews_config(results$ews_config_id)
    results <- results %>%
      left_join(ews_parsed, by = "ews_config_id")
  }

  results
}


#' Get all available facet columns from results
#'
#' Returns the standard pipeline facets (from config registry) plus
#' any layer facets present. Used by concordance and spec curve functions
#' to automatically include layer facets.
#'
#' @param results Results tibble (already linked to config registry)
#' @param include_layers Whether to include layer config facets
#' @return Character vector of facet column names
get_all_facets <- function(results, include_layers = TRUE) {

  # Standard pipeline facets (from config registry linking)
  standard_facets <- c("embedding_model", "sent_method", "comp__method",
                       "window_type", "window_size")
  standard_facets <- intersect(standard_facets, names(results))

  if (!include_layers) return(standard_facets)

  # Layer facets (from augment_layer_facets)
  layer_facets <- c(
    # Temporal parsed facets
    "tf_window_mode", "tf_window_type", "tf_window_size",
    "tf_burstiness_method",
    # EWS parsed facets
    "ews_detrend", "ews_window_size"
  )
  layer_facets <- intersect(layer_facets, names(results))

  c(standard_facets, layer_facets)
}


# =============================================================================
# 3. LAYER IMPACT ANALYSIS
# =============================================================================

#' Compare climate effect estimates across layer configurations
#'
#' For results that include temporal/EWS layers, shows how the climate
#' effect changes as different layer configurations are applied.
#' Useful for assessing whether layer features absorb climate signal.
#'
#' @param results Augmented results tibble
#' @param outcome_filter Outcome to analyze
#' @param component "cond", "zi", or NULL (for NRC ordinal/binary)
#' @param estimate_col Override for the estimate column name
#' @return List with $summary tibble and $plot ggplot object
compare_layer_impact <- function(results,
                                  outcome_filter = NULL,
                                  component = NULL,
                                  estimate_col = NULL) {

  # Auto-detect estimate column
  if (is.null(estimate_col)) {
    if (!is.null(component)) {
      estimate_col <- paste0("climate_estimate_", component)
    } else if ("climate_estimate" %in% names(results)) {
      estimate_col <- "climate_estimate"
    } else if ("climate_estimate_cond" %in% names(results)) {
      estimate_col <- "climate_estimate_cond"
    } else {
      stop("Cannot detect estimate column. Pass estimate_col explicitly.")
    }
  }

  if (!estimate_col %in% names(results)) {
    stop(glue("Column '{estimate_col}' not found in results"))
  }

  # Filter to outcome
  df <- results %>% filter(status %in% c("success", "convergence_warning", "singular_fit"))
  if (!is.null(outcome_filter)) {
    df <- df %>% filter(outcome == outcome_filter)
  }

  if (nrow(df) == 0) return(NULL)

  # Check what layer columns are available
  has_temporal <- "temporal_config_id" %in% names(df)
  has_ews <- "ews_config_id" %in% names(df)

  if (!has_temporal && !has_ews) {
    message("No layer config columns found — nothing to compare")
    return(NULL)
  }

  # Build grouping variable
  if (has_temporal && has_ews) {
    df <- df %>%
      mutate(layer_config = paste0(
        ifelse(!is.na(tf_label), tf_label, temporal_config_id), " | ",
        ifelse(!is.na(ews_label), ews_label, ews_config_id)
      ))
    group_var <- "layer_config"
  } else if (has_temporal) {
    df <- df %>%
      mutate(layer_config = ifelse(!is.na(tf_label), tf_label, temporal_config_id))
    group_var <- "layer_config"
  } else {
    df <- df %>%
      mutate(layer_config = ifelse(!is.na(ews_label), ews_label, ews_config_id))
    group_var <- "layer_config"
  }

  # Summarize climate effect by layer config
  summary_df <- df %>%
    group_by(layer_config) %>%
    summarise(
      n_specs = n(),
      mean_estimate = mean(.data[[estimate_col]], na.rm = TRUE),
      median_estimate = median(.data[[estimate_col]], na.rm = TRUE),
      sd_estimate = sd(.data[[estimate_col]], na.rm = TRUE),
      pct_significant = 100 * mean(
        .data[[sub("estimate", "pval", estimate_col)]] < 0.05, na.rm = TRUE
      ),
      pct_negative = 100 * mean(.data[[estimate_col]] < 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(median_estimate)

  # Plot: violin of estimates by layer config
  p <- ggplot(df, aes(x = reorder(layer_config, .data[[estimate_col]], FUN = median),
                       y = .data[[estimate_col]])) +
    geom_violin(fill = "grey80", alpha = 0.5) +
    geom_boxplot(width = 0.15, outlier.size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    coord_flip() +
    labs(
      title = glue("Climate Effect by Layer Configuration"),
      subtitle = if (!is.null(outcome_filter)) glue("Outcome: {outcome_filter}") else NULL,
      x = "Layer Configuration",
      y = glue("Climate Estimate ({estimate_col})")
    ) +
    theme_minimal(base_size = 11)

  list(summary = summary_df, plot = p)
}


#' Summarize layer configuration impact on model fit
#'
#' Compares AIC/BIC across layer configurations to assess whether
#' adding temporal/EWS features improves model fit on average.
#'
#' @param results Augmented results tibble
#' @param outcome_filter Outcome to analyze
#' @param fit_metric "aic" or "bic" (case-insensitive, matched flexibly)
#' @return Tibble with mean fit statistics by layer config
summarize_layer_fit <- function(results, outcome_filter = NULL,
                                 fit_metric = "aic") {

  df <- results %>% filter(status %in% c("success", "convergence_warning", "singular_fit"))
  if (!is.null(outcome_filter)) df <- df %>% filter(outcome == outcome_filter)

  # Find AIC/BIC column
  aic_col <- grep(paste0("^", fit_metric), names(df), value = TRUE, ignore.case = TRUE)
  if (length(aic_col) == 0) {
    # Try common variants
    aic_col <- grep("AIC|aic", names(df), value = TRUE)[1]
  }
  if (is.na(aic_col) || length(aic_col) == 0) {
    warning("No AIC/BIC column found")
    return(NULL)
  }
  aic_col <- aic_col[1]

  has_temporal <- "temporal_config_id" %in% names(df)
  has_ews <- "ews_config_id" %in% names(df)

  group_cols <- character(0)
  if (has_temporal) group_cols <- c(group_cols, "temporal_config_id")
  if (has_ews) group_cols <- c(group_cols, "ews_config_id")

  if (length(group_cols) == 0) return(NULL)

  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_specs = n(),
      mean_fit = mean(.data[[aic_col]], na.rm = TRUE),
      sd_fit = sd(.data[[aic_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_fit)
}


# =============================================================================
# 4. CV DELTA-BRIER BY LAYER TIER
# =============================================================================

#' Compute delta-Brier between adjacent model tiers
#'
#' For layered CV results, computes delta-Brier between each pair of
#' adjacent tiers: climate vs. no_climate, climate_temporal vs. climate,
#' climate_ews vs. climate, climate_all vs. climate.
#'
#' @param cv_results CV results tibble with model_label column
#' @param reference_label The baseline tier for each comparison
#'   (default: "no_climate" for climate delta, or the climate_var label)
#' @return Tibble with delta-Brier for each (config × climate_var × comparison)
compute_layered_delta_brier <- function(cv_results,
                                         reference_label = NULL) {

  if (!"model_label" %in% names(cv_results) ||
      !"brier_score_mean" %in% names(cv_results)) {
    warning("CV results missing model_label or brier_score_mean columns")
    return(tibble())
  }

  # Get unique model labels (tiers)
  labels <- unique(cv_results$model_label)

  # Define comparison pairs
  # Each comparison: list(target = "climate_temporal", baseline = climate_var_label)
  # Climate vs. no_climate is the standard paper 1 comparison
  # Climate+layer vs. climate is the paper 2 comparison

  # Group columns for joining
  join_cols <- intersect(
    c("config_id", "climate_var_tested", "cv_strategy", "outcome",
      "temporal_config_id", "ews_config_id"),
    names(cv_results)
  )

  # All pairwise comparisons against the tier below
  tier_order <- c("intercept_only", "no_climate")
  layer_tiers <- setdiff(labels, c("intercept_only", "no_climate"))

  # Sort layer tiers: climate first, then extensions
  climate_labels <- layer_tiers[!grepl("^climate_", layer_tiers)]
  extension_labels <- layer_tiers[grepl("^climate_", layer_tiers)]
  tier_order <- c(tier_order, climate_labels, sort(extension_labels))

  deltas <- list()

  # Compare each tier to its natural baseline
  for (tier in tier_order) {
    if (tier %in% c("intercept_only", "no_climate")) next

    # Determine baseline: climate tiers compare to no_climate,
    # layer tiers compare to the climate-only tier
    if (tier %in% climate_labels) {
      baseline <- "no_climate"
    } else {
      # climate_temporal, climate_ews, etc. compare to climate-only
      baseline <- climate_labels[1]  # first non-standard label = the climate var
    }

    if (!baseline %in% labels) next

    target_rows <- cv_results %>%
      filter(model_label == tier) %>%
      select(all_of(join_cols), brier_target = brier_score_mean)

    baseline_rows <- cv_results %>%
      filter(model_label == baseline) %>%
      select(all_of(join_cols), brier_baseline = brier_score_mean)

    delta <- target_rows %>%
      inner_join(baseline_rows, by = join_cols) %>%
      mutate(
        delta_brier = brier_target - brier_baseline,
        comparison = paste0(tier, " vs. ", baseline),
        target_tier = tier,
        baseline_tier = baseline
      )

    deltas <- c(deltas, list(delta))
  }

  bind_rows(deltas)
}


#' Plot delta-Brier for layered model comparisons
#'
#' @param delta_results Output from compute_layered_delta_brier()
#' @param facet_by Column to facet by (e.g., "comparison", "cv_strategy")
#' @return ggplot object
plot_layered_delta_brier <- function(delta_results, facet_by = "comparison") {

  if (nrow(delta_results) == 0) return(NULL)

  p <- ggplot(delta_results, aes(x = reorder(config_id, delta_brier),
                                  y = delta_brier)) +
    geom_point(size = 0.5, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Delta-Brier: Layer Comparisons",
      subtitle = "Negative = target model predicts better than baseline",
      x = "Specification (sorted)",
      y = "Delta Brier Score"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  if (facet_by %in% names(delta_results)) {
    p <- p + facet_wrap(as.formula(paste0("~ ", facet_by)), scales = "free_x")
  }

  p
}


# =============================================================================
# 5. REPORT INTEGRATION HELPERS
# =============================================================================

#' Check whether results include layer configurations
#'
#' @param results Tibble of MV or CV results
#' @return Named logical vector: temporal, ews
has_layer_results <- function(results) {
  c(
    temporal = "temporal_config_id" %in% names(results) &&
      any(!is.na(results$temporal_config_id)),
    ews = "ews_config_id" %in% names(results) &&
      any(!is.na(results$ews_config_id))
  )
}


#' Generate layer configuration summary for reports
#'
#' Produces a human-readable summary of which layer configurations were tested,
#' suitable for inclusion in Quarto reports.
#'
#' @param results Augmented results tibble
#' @return Character string with formatted summary
layer_config_summary <- function(results) {
  layers <- has_layer_results(results)

  lines <- character(0)

  if (layers["temporal"]) {
    tf_configs <- unique(results$temporal_config_id)
    n_matched <- sum(grepl("matched", tf_configs))
    n_fixed <- sum(grepl("fixed", tf_configs))
    lines <- c(lines, glue(
      "Temporal features: {length(tf_configs)} configurations ",
      "({n_matched} matched + {n_fixed} fixed)"
    ))

    if ("tf_burstiness_method" %in% names(results)) {
      methods <- unique(na.omit(results$tf_burstiness_method))
      lines <- c(lines, glue("  Burstiness methods: {paste(methods, collapse=', ')}"))
    }
  }

  if (layers["ews"]) {
    ews_configs <- unique(results$ews_config_id)
    lines <- c(lines, glue(
      "EWS features: {length(ews_configs)} configurations"
    ))

    if ("ews_detrend" %in% names(results)) {
      detrends <- unique(na.omit(results$ews_detrend))
      windows <- unique(na.omit(results$ews_window_size))
      lines <- c(lines, glue("  Detrend: {paste(detrends, collapse=', ')}"))
      lines <- c(lines, glue("  Windows: {paste(windows, collapse=', ')}"))
    }
  }

  if (length(lines) == 0) {
    return("No feature layers active (climate-only analysis)")
  }

  paste(lines, collapse = "\n")
}
