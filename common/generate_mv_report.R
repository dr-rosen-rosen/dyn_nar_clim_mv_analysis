# generate_mv_report.R
# Orchestration layer for multiverse/CV figure generation and Quarto report assembly
#
# Main entry points:
#   generate_mv_report()    — full pipeline: generate figures + render Quarto
#   generate_mv_figures()   — generate and save all figures to disk
#   generate_industry_figures() — figures for a single industry (called internally)
#
# Reads multiverse and CV results, calls postprocessing.R functions,
# saves figures with predictable naming, writes a manifest CSV,
# and optionally renders a parameterized Quarto document.
#
# Mike Rose — Dynamic Narrative Climate Toolkit
# ==============================================================================

library(tidyverse)
library(arrow)
library(patchwork)

# Source the shared postprocessing functions
# Adjust this path to match your project layout
# source("shared/postprocessing.R")


# ==============================================================================
# INDUSTRY CONFIGURATION SPEC
# ==============================================================================

#' Create an industry configuration
#'
#' @param mv_results Path to multiverse results parquet
#' @param config_registry Path to config_registry.csv
#' @param cv_results Path to CV results parquet (NULL if unavailable)
#' @param label Human-readable label (e.g., "Rail (FRA)")
#' @param decision_vars Character vector of config variables for spec curve panels.
#'   If NULL, uses postprocessing.R defaults (auto-detect from data).
#' @param ops_vars Base operational variable names for ops analysis (NULL = auto-detect)
#' @param cv_strategies Character vector of CV strategies to plot (default both)
#' @param exclude_outcomes Character vector of outcome names to exclude from the report.
#'   These outcomes will be skipped during figure generation even if present in the data.
#' @param cv_filter_baseline Baseline strategy for CV-based pre-filtering. One of:
#'   "best_non_climate" (recommended for NRC), "no_climate" (standard for rail),
#'   "intercept_only" (conservative). If NULL, no CV filtering is applied.
#'   Requires configuration_concordance.R to be sourced.
#' @param cv_filter_require_both If TRUE, a specification must beat the baseline
#'   in BOTH group k-fold and time-series CV to be retained. If FALSE (default),
#'   passing either strategy is sufficient. TRUE is more conservative and
#'   recommended for publication.
#' @param cv_filter_level Granularity of CV filtering:
#'   "config_window" (default) — retains only specific config x window combinations
#'   that demonstrate predictive value. Most precise.
#'   "config" — retains all windows for a config if ANY window shows predictive value.
#'   More permissive.
#' @param analysis_type "mv" (default), "temporal", or "ews" — controls which
#'   figure generators are dispatched. Currently only "mv" is implemented;
#'   "temporal" and "ews" are reserved for future expansion.
#' @return Named list (industry config spec)
industry_config <- function(mv_results,
                            config_registry,
                            cv_results = NULL,
                            label = NULL,
                            decision_vars = NULL,
                            ops_vars = NULL,
                            cv_strategies = c("group_kfold", "timeseries"),
                            exclude_outcomes = NULL,
                            cv_filter_baseline = NULL,
                            cv_filter_require_both = FALSE,
                            cv_filter_level = "config_window",
                            analysis_type = "mv") {
  list(
    mv_results              = mv_results,
    config_registry         = config_registry,
    cv_results              = cv_results,
    label                   = label,
    decision_vars           = decision_vars,
    ops_vars                = ops_vars,
    cv_strategies           = cv_strategies,
    exclude_outcomes        = exclude_outcomes,
    cv_filter_baseline      = cv_filter_baseline,
    cv_filter_require_both  = cv_filter_require_both,
    cv_filter_level         = cv_filter_level,
    analysis_type           = analysis_type
  )
}


# ==============================================================================
# FIGURE DIMENSION DEFAULTS
# ==============================================================================

#' Default figure dimensions by figure type
#' Returns list(width, height) in inches
figure_dims <- function(figure_type, n_panels = 6) {
  switch(figure_type,
    spec_curve        = list(width = 14, height = 6 + n_panels * 0.7),
    config_importance = list(width = 10, height = 6),
    climate_vs_ops    = list(width = 11, height = 7),
    cv_brier_spec     = list(width = 14, height = 6 + n_panels * 0.7),
    delta_brier       = list(width = 12, height = 6),
    robustness_table  = list(width = 10, height = 4),
    # fallback
    list(width = 11, height = 8)
  )
}


# ==============================================================================
# COMPONENT DETECTION PER OUTCOME
# ==============================================================================

#' Detect which model components are present for a given outcome
#' @param df Results data frame filtered to one outcome
#' @return Character vector: c("cond", "zi"), c("cond"), or NULL
detect_components <- function(df) {
  has_cond <- "climate_estimate_cond" %in% names(df)
  has_zi   <- "climate_estimate_zi" %in% names(df)
  has_base <- "climate_estimate" %in% names(df)

  if (has_cond && has_zi) {
    # Check that both actually have non-NA data for this outcome
    has_cond_data <- any(!is.na(df$climate_estimate_cond))
    has_zi_data   <- any(!is.na(df$climate_estimate_zi))
    components <- c()
    if (has_cond_data) components <- c(components, "cond")
    if (has_zi_data)   components <- c(components, "zi")
    if (length(components) == 0) return(NULL)
    return(components)
  } else if (has_cond && !has_zi) {
    if (any(!is.na(df$climate_estimate_cond))) return("cond")
    return(NULL)
  } else if (has_base) {
    if (any(!is.na(df$climate_estimate))) return(NULL)  # NULL = no component suffix
    return(NULL)
  } else {
    return(NULL)
  }
}

#' Wrapper: returns NULL for ordinal/binary, or c("cond","zi") etc for hurdle
#' The distinction matters: NULL means call functions with component=NULL,
#' while character vector means iterate over components.
detect_outcome_model_type <- function(df) {
  has_cond <- "climate_estimate_cond" %in% names(df)
  has_zi   <- "climate_estimate_zi" %in% names(df)
  has_base <- "climate_estimate" %in% names(df)

  if (has_cond || has_zi) {
    components <- c()
    if (has_cond && any(!is.na(df$climate_estimate_cond))) components <- c(components, "cond")
    if (has_zi && any(!is.na(df$climate_estimate_zi)))     components <- c(components, "zi")
    if (length(components) > 0) return(components)
  }

  if (has_base && any(!is.na(df$climate_estimate))) {
    return("none")  # signal: single-estimate model, pass component=NULL
  }

  return(NULL)  # no usable data
}


# ==============================================================================
# SINGLE-FIGURE SAVE HELPER
# ==============================================================================

#' Save a figure and return a manifest row
#'
#' @param plot ggplot or patchwork object
#' @param industry_dir Output directory for this industry
#' @param outcome Outcome name
#' @param component Component name or NULL
#' @param figure_type Figure type string
#' @param industry Industry key
#' @param dims List(width, height)
#' @param format "pdf", "png", or "both"
#' @return Tibble row for manifest
save_figure <- function(plot, industry_dir, outcome, component, figure_type,
                        industry, dims, format = "pdf") {

  if (is.null(plot)) return(NULL)

  # Build filename: outcome_component_figuretype.ext
  comp_part <- if (!is.null(component) && component != "none") paste0("_", component) else ""
  base_name <- paste0(outcome, comp_part, "_", figure_type)

  saved_paths <- c()

  if (format %in% c("pdf", "both")) {
    path <- file.path(industry_dir, paste0(base_name, ".pdf"))
    ggsave(path, plot, width = dims$width, height = dims$height, device = "pdf")
    saved_paths <- c(saved_paths, path)
  }

  if (format %in% c("png", "both")) {
    path <- file.path(industry_dir, paste0(base_name, ".png"))
    ggsave(path, plot, width = dims$width, height = dims$height, dpi = 300, device = "png")
    saved_paths <- c(saved_paths, path)
  }

  # Return manifest row (use PDF path if both)
  tibble(
    industry     = industry,
    outcome      = outcome,
    component    = component %||% "single",
    figure_type  = figure_type,
    filename     = paste0(base_name, ".", ifelse(format == "png", "png", "pdf")),
    path         = normalizePath(saved_paths[1], mustWork = FALSE),
    width        = dims$width,
    height       = dims$height
  )
}


# ==============================================================================
# CONFIG IMPORTANCE BAR CHART
# ==============================================================================

#' Create a bar chart from analyze_config_importance output
#' @param importance_df Output of analyze_config_importance()
#' @param outcome_label Label for title
#' @param component_label Label for title
#' @return ggplot object
plot_config_importance_bar <- function(importance_df, outcome_label = "",
                                       component_label = "",
                                       plot_subtitle = NULL) {
  if (is.null(importance_df) || nrow(importance_df) == 0) return(NULL)

  plot_data <- importance_df %>%
    filter(!is.na(pct_variance)) %>%
    mutate(
      var_label = variable %>%
        str_replace_all("__", ": ") %>%
        str_replace_all("_", " ") %>%
        tools::toTitleCase(),
      var_label = fct_reorder(var_label, pct_variance)
    )

  subtitle_text <- plot_subtitle %||%
    "% variance in climate effect explained by each analytical choice (univariate R\u00b2)"

  ggplot(plot_data, aes(x = var_label, y = pct_variance, fill = pct_variance)) +
    geom_col(alpha = 0.85, show.legend = FALSE) +
    geom_text(aes(label = paste0(sprintf("%.1f", pct_variance), "% ", significance)),
              hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_fill_gradient(low = "#a6bddb", high = "#2b8cbe") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste0("Configuration Importance: ", outcome_label, " ", component_label),
      subtitle = subtitle_text,
      x = NULL,
      y = "% Variance Explained"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      plot.subtitle = if (!is.null(plot_subtitle)) {
        element_text(color = "#2166ac", face = "italic")
      } else {
        element_text()
      }
    )
}


# ==============================================================================
# GENERATE FIGURES FOR ONE INDUSTRY
# ==============================================================================

#' Generate all multiverse + CV figures for a single industry
#'
#' @param industry_key Short key like "rail", "nrc"
#' @param ind_config Industry config list (from industry_config())
#' @param output_dir Top-level output directory
#' @param format "pdf", "png", or "both"
#' @param verbose Print progress messages
#' @return Tibble of manifest rows
generate_industry_figures <- function(industry_key, ind_config, output_dir,
                                      format = "pdf", verbose = TRUE) {

  industry_dir <- file.path(output_dir, industry_key)
  dir.create(industry_dir, recursive = TRUE, showWarnings = FALSE)

  # Remove stale figures and CSVs from previous runs before generating new ones.
  # This prevents accumulation of files with old outcome names, old filter settings,
  # or old component labels that would corrupt the manifest and cross-industry tables.
  stale <- list.files(industry_dir, pattern = "\\.(pdf|csv)$", full.names = TRUE)
  if (length(stale) > 0) {
    file.remove(stale)
    if (verbose) message(sprintf("  Cleaned %d stale file(s) from %s", length(stale), industry_dir))
  }

  label <- ind_config$label %||% toupper(industry_key)

  # --- Load and link MV results ---
  if (verbose) message(sprintf("\n=== %s: Loading multiverse results ===", label))

  mv_results <- read_parquet(ind_config$mv_results)
  results_full <- link_results_to_config(mv_results, ind_config$config_registry)

  # --- Discover outcomes ---
  outcomes <- unique(results_full$outcome[results_full$status == "success"])
  if (verbose) message(sprintf("  Outcomes found: %s", paste(outcomes, collapse = ", ")))

  # Apply outcome exclusions if specified
  if (!is.null(ind_config$exclude_outcomes)) {
    excluded <- intersect(outcomes, ind_config$exclude_outcomes)
    if (length(excluded) > 0 && verbose) {
      message(sprintf("  Excluding outcomes: %s", paste(excluded, collapse = ", ")))
    }
    outcomes <- setdiff(outcomes, ind_config$exclude_outcomes)
  }

  # --- Load CV results if available ---
  cv_results <- NULL
  if (!is.null(ind_config$cv_results) && file.exists(ind_config$cv_results)) {
    if (verbose) message("  Loading CV results...")
    cv_results <- read_parquet(ind_config$cv_results)
    cv_results <- normalize_cv_columns(cv_results)
  }

  # --- CV-based pre-filtering ---
  cv_filter_result <- NULL
  results_filtered <- NULL
  if (!is.null(ind_config$cv_filter_baseline) && !is.null(cv_results)) {
    if (!exists("filter_credible_specs", mode = "function")) {
      # Try to auto-source configuration_concordance.R from the common/ directory.
      concordance_path <- file.path(
        dirname(normalizePath("common/generate_mv_report.R", mustWork = FALSE)),
        "configuration_concordance.R"
      )
      if (file.exists(concordance_path)) {
        source(concordance_path, local = FALSE)
      } else {
        warning("cv_filter_baseline is set but filter_credible_specs() not found. ",
                "Source common/configuration_concordance.R first. Skipping CV filtering.")
      }
    }
    if (exists("filter_credible_specs", mode = "function")) {
      if (verbose) message(sprintf("  CV pre-filtering (baseline: %s, require both: %s, level: %s)...",
                                   ind_config$cv_filter_baseline,
                                   ind_config$cv_filter_require_both,
                                   ind_config$cv_filter_level %||% "config_window"))
      cv_filter_result <- filter_credible_specs(
        cv_results,
        baseline_strategy = ind_config$cv_filter_baseline,
        require_both_strategies = ind_config$cv_filter_require_both,
        filter_level = ind_config$cv_filter_level %||% "config_window"
      )
      credible_ids <- cv_filter_result$credible_configs

      # Apply filtering based on filter_level
      if ((ind_config$cv_filter_level %||% "config_window") == "config_window") {
        # Pair-level: only retain specific config × climate_var combinations.
        # Coerce config_id to character to match results_full (link_results_to_config
        # always produces character config_id; CV parquet may store it as integer).
        credible_pairs <- cv_filter_result$credible_pairs %>%
          mutate(config_id = as.character(config_id))
        results_filtered <- results_full %>%
          inner_join(credible_pairs,
                     by = c("config_id" = "config_id",
                            "climate_var" = "climate_var_tested"))
      } else {
        # Config-level: retain all windows for any config with at least one good window
        results_filtered <- results_full %>%
          filter(config_id %in% as.character(credible_ids))
      }

      if (verbose) {
        fs <- cv_filter_result$filter_stats
        message(sprintf("  CV filter: %d/%d %s retained (%.1f%%)",
                        fs$n_credible, fs$n_total, fs$filter_level, fs$pct_retained))
        if (fs$filter_level == "config_window") {
          message(sprintf("    (%d unique configs, %d config x window pairs)",
                          fs$n_configs_credible, fs$n_config_window_pairs_credible))
        }
        message(sprintf("  Filtered results: %d rows (from %d unfiltered)",
                        nrow(results_filtered), nrow(results_full)))
      }

      # Save filter stats as CSV
      filter_stats_df <- as_tibble(cv_filter_result$filter_stats)
      write_csv(filter_stats_df, file.path(industry_dir, "cv_filter_stats.csv"))

      # Save delta summary
      write_csv(cv_filter_result$delta_summary,
                file.path(industry_dir, "cv_delta_summary.csv"))

      # Save credible config IDs for cross-industry filtering
      write_csv(tibble(config_id = credible_ids),
                file.path(industry_dir, "cv_credible_configs.csv"))

      # Save credible pairs for pair-level cross-industry filtering
      write_csv(cv_filter_result$credible_pairs,
                file.path(industry_dir, "cv_credible_pairs.csv"))
    }
  }

  # --- Helper: generate all MV figures for a given result set ---
  generate_outcome_figures <- function(results_data, cv_data, outcome_list,
                                        suffix = "", suffix_label = "") {
    local_rows <- list()
    local_idx <- 0

    for (outcome in outcome_list) {
      if (verbose) message(sprintf("\n  --- %s: %s%s ---", label, outcome, suffix_label))

      outcome_data <- results_data %>% filter(outcome == !!outcome)
      model_type <- detect_outcome_model_type(outcome_data)

      if (is.null(model_type)) {
        if (verbose) message(sprintf("    Skipping %s: no usable estimate data", outcome))
        next
      }

      # Component ordering: zi before cond
      if (identical(model_type, "none")) {
        components_to_plot <- list(NULL)
        names(components_to_plot) <- "single"
      } else {
        ordered_types <- c()
        if ("zi" %in% model_type) ordered_types <- c(ordered_types, "zi")
        if ("cond" %in% model_type) ordered_types <- c(ordered_types, "cond")
        components_to_plot <- as.list(ordered_types)
        names(components_to_plot) <- ordered_types
      }

      # Banner added to plot subtitle when generating filtered figures
      filter_banner <- if (suffix_label != "") {
        "CV-Filtered \u2014 only specifications where climate improves out-of-sample prediction"
      } else {
        NULL
      }

      for (comp_name in names(components_to_plot)) {
        comp <- components_to_plot[[comp_name]]
        comp_label <- if (is.null(comp)) "" else ifelse(comp == "cond", "(Conditional)", "(Zero-Inflation)")
        comp_file  <- if (is.null(comp)) NULL else comp

        if (verbose) message(sprintf("    Component: %s", comp_name))

        # Spec curve with panels
        tryCatch({
          n_panels <- length(ind_config$decision_vars %||%
                              c("window_type", "embedding_model", "sent_method", "comp__method"))
          p_spec <- plot_spec_curve_with_panels(
            results_data, outcome_filter = outcome, component = comp,
            decision_vars = ind_config$decision_vars,
            plot_subtitle = filter_banner
          )
          dims <- figure_dims("spec_curve", n_panels)
          local_idx <- local_idx + 1
          local_rows[[local_idx]] <- save_figure(
            p_spec, industry_dir, paste0(outcome, suffix), comp_file, "spec_curve",
            industry_key, dims, format
          )
          if (verbose) message("      [OK] Spec curve")
        }, error = function(e) {
          if (verbose) message(sprintf("      [ERR] Spec curve: %s", conditionMessage(e)))
        })

        # Config importance
        tryCatch({
          importance_vars <- unique(c(
            ind_config$decision_vars %||% character(0),
            "window_type", "window_size", "lag_days", "halflife_days",
            "embedding_model", "sent_method", "comp__method"
          ))
          importance_df <- analyze_config_importance(
            results_data, outcome_filter = outcome, component = comp,
            config_vars = importance_vars
          )
          csv_name <- paste0(outcome, suffix,
                            if (!is.null(comp_file)) paste0("_", comp_file) else "",
                            "_config_importance.csv")
          write_csv(importance_df, file.path(industry_dir, csv_name))

          p_importance <- plot_config_importance_bar(
            importance_df,
            outcome_label = tools::toTitleCase(outcome),
            component_label = comp_label,
            plot_subtitle = filter_banner
          )
          dims <- figure_dims("config_importance")
          local_idx <- local_idx + 1
          local_rows[[local_idx]] <- save_figure(
            p_importance, industry_dir, paste0(outcome, suffix), comp_file, "config_importance",
            industry_key, dims, format
          )
          if (verbose) message("      [OK] Config importance")
        }, error = function(e) {
          if (verbose) message(sprintf("      [ERR] Config importance: %s", conditionMessage(e)))
        })

        # Climate vs ops
        tryCatch({
          result <- compare_climate_vs_operational(
            results_data, ops_vars = ind_config$ops_vars,
            outcome_filter = outcome, component = comp
          )
          if (!is.null(filter_banner) && !is.null(result$plot)) {
            result$plot <- result$plot + labs(subtitle = filter_banner) +
              theme(plot.subtitle = element_text(color = "#2166ac", face = "italic"))
          }
          dims <- figure_dims("climate_vs_ops")
          local_idx <- local_idx + 1
          local_rows[[local_idx]] <- save_figure(
            result$plot, industry_dir, paste0(outcome, suffix), comp_file, "climate_vs_ops",
            industry_key, dims, format
          )
          if (verbose) message("      [OK] Climate vs ops")
        }, error = function(e) {
          if (verbose) message(sprintf("      [ERR] Climate vs ops: %s", conditionMessage(e)))
        })

        # Robustness summary
        tryCatch({
          robustness <- summarize_climate_robustness(
            results_data, outcome_filter = outcome, component = comp
          )
          csv_name <- paste0(outcome, suffix,
                            if (!is.null(comp_file)) paste0("_", comp_file) else "",
                            "_robustness.csv")
          write_csv(robustness, file.path(industry_dir, csv_name))
          if (verbose) message("      [OK] Robustness summary")
        }, error = function(e) {
          if (verbose) message(sprintf("      [ERR] Robustness summary: %s", conditionMessage(e)))
        })
      }  # end component loop

      # CV figures (unfiltered pass only)
      if (suffix == "" && !is.null(cv_data)) {
        cv_for_outcome <- cv_data %>%
          filter(if ("outcome" %in% names(.)) outcome == !!outcome else TRUE)

        if (!"embedding_model" %in% names(cv_for_outcome)) {
          cv_for_outcome <- tryCatch(
            link_results_to_config(cv_for_outcome, ind_config$config_registry),
            error = function(e) cv_for_outcome
          )
        }

        if (nrow(cv_for_outcome) > 0) {
          for (cv_strat in ind_config$cv_strategies) {
            tryCatch({
              p_brier <- plot_cv_spec_curve_with_panels(
                cv_for_outcome, cv_strategy = cv_strat,
                decision_vars = ind_config$decision_vars
              )
              if (!is.null(p_brier)) {
                n_p <- length(ind_config$decision_vars %||%
                               c("window_type", "embedding_model", "sent_method", "comp__method"))
                dims <- figure_dims("cv_brier_spec", n_p)
                local_idx <- local_idx + 1
                local_rows[[local_idx]] <- save_figure(
                  p_brier, industry_dir, outcome, cv_strat, "cv_brier_spec",
                  industry_key, dims, format
                )
                if (verbose) message(sprintf("      [OK] CV Brier spec (%s)", cv_strat))
              }
            }, error = function(e) {
              if (verbose) message(sprintf("      [ERR] CV Brier spec (%s): %s", cv_strat, conditionMessage(e)))
            })

            tryCatch({
              p_delta <- plot_delta_brier(cv_for_outcome, cv_strategy = cv_strat)
              if (!is.null(p_delta)) {
                dims <- figure_dims("delta_brier")
                local_idx <- local_idx + 1
                local_rows[[local_idx]] <- save_figure(
                  p_delta, industry_dir, outcome, cv_strat, "delta_brier",
                  industry_key, dims, format
                )
                if (verbose) message(sprintf("      [OK] Delta-Brier (%s)", cv_strat))
              }
            }, error = function(e) {
              if (verbose) message(sprintf("      [ERR] Delta-Brier (%s): %s", cv_strat, conditionMessage(e)))
            })
          }
        }
      }
    }  # end outcome loop

    bind_rows(local_rows)
  }

  # --- Generate unfiltered figures ---
  if (verbose) message("\n  === Unfiltered figures ===")
  manifest_unfiltered <- generate_outcome_figures(results_full, cv_results, outcomes)

  # --- Generate CV-filtered figures (if filtering enabled) ---
  manifest_filtered <- NULL
  if (!is.null(results_filtered) && nrow(results_filtered) > 0) {
    if (verbose) message("\n  === CV-filtered figures ===")
    manifest_filtered <- generate_outcome_figures(
      results_filtered, NULL, outcomes,
      suffix = "_filtered", suffix_label = " [CV-filtered]"
    )
  }

  # Combine manifest
  manifest <- bind_rows(manifest_unfiltered, manifest_filtered)
  if (verbose) message(sprintf("\n  %s complete: %d figures generated", label, nrow(manifest)))
  return(manifest)
}


# ==============================================================================
# MAIN: GENERATE ALL FIGURES
# ==============================================================================

#' Generate all multiverse/CV figures across industries
#'
#' @param industries Named list of industry configs (from industry_config())
#' @param output_dir Top-level output directory for figures
#' @param format "pdf", "png", or "both"
#' @param verbose Print progress
#' @return Manifest tibble (also saved as CSV)
generate_mv_figures <- function(industries, output_dir = "report_figures",
                                 format = "pdf", verbose = TRUE) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  all_manifests <- list()

  for (ind_key in names(industries)) {
    manifest <- generate_industry_figures(
      industry_key = ind_key,
      ind_config   = industries[[ind_key]],
      output_dir   = output_dir,
      format       = format,
      verbose      = verbose
    )
    all_manifests[[ind_key]] <- manifest
  }

  full_manifest <- bind_rows(all_manifests)

  # --- Cross-industry summary figures (only if 2+ industries) ---
  if (length(industries) >= 2) {
    if (verbose) message("\n=== Cross-Industry Summary Figures ===")

    cross_dir <- file.path(output_dir, "_cross_industry")
    # Clean previous cross-industry outputs to avoid stale files
    if (dir.exists(cross_dir)) unlink(cross_dir, recursive = TRUE)
    dir.create(cross_dir, showWarnings = FALSE)

    industry_labels <- setNames(
      sapply(industries, function(x) x$label %||% ""),
      names(industries)
    )

    cross_rows <- list()
    cross_idx <- 0

    # Collect all excluded outcomes across industries
    all_excluded <- unique(unlist(lapply(industries, function(x) x$exclude_outcomes)))

    # 1. Cross-industry robustness table (saved as CSV for Quarto)
    tryCatch({
      robustness_table <- build_cross_industry_robustness(output_dir, industry_labels)
      if (!is.null(robustness_table)) {
        # Filter out excluded outcomes
        if (length(all_excluded) > 0) {
          robustness_table <- robustness_table %>%
            filter(!tolower(gsub(" ", "_", outcome_label)) %in% all_excluded)
        }
        write_csv(robustness_table, file.path(cross_dir, "cross_industry_robustness.csv"))
        if (verbose) message("  [OK] Cross-industry robustness table")
      }
    }, error = function(e) {
      if (verbose) message(sprintf("  [ERR] Robustness table: %s", conditionMessage(e)))
    })

    # 2. Cross-industry config importance comparison
    tryCatch({
      importance_data <- build_cross_industry_importance(output_dir, industry_labels)
      if (!is.null(importance_data)) {
        # Filter out excluded outcomes
        if (length(all_excluded) > 0) {
          importance_data <- importance_data %>%
            filter(!outcome %in% all_excluded)
        }
        # Save combined data for reference
        write_csv(importance_data, file.path(cross_dir, "cross_industry_importance.csv"))

        # Two-panel comparison plot (one per industry)
        p_imp <- plot_cross_industry_importance(importance_data)
        if (!is.null(p_imp)) {
          n_outcomes <- n_distinct(paste(importance_data$industry, importance_data$outcome, importance_data$component))
          dims <- list(width = 14, height = max(10, 5 + length(unique(importance_data$industry_label)) * 5))
          cross_idx <- cross_idx + 1
          cross_rows[[cross_idx]] <- save_figure(
            p_imp, cross_dir, "summary", NULL, "config_importance_comparison",
            "_cross_industry", dims, format
          )
          if (verbose) message("  [OK] Config importance comparison plot")
        }
      }
    }, error = function(e) {
      if (verbose) message(sprintf("  [ERR] Importance comparison: %s", conditionMessage(e)))
    })

    # 3. Cross-industry climate vs ops (faceted by industry) — UNFILTERED
    tryCatch({
      p_ops <- plot_cross_industry_climate_vs_ops(industries, industry_labels)
      if (!is.null(p_ops)) {
        n_panels <- length(industries) * 3
        dims <- list(width = 14, height = max(8, 3 + n_panels * 1.2))
        cross_idx <- cross_idx + 1
        cross_rows[[cross_idx]] <- save_figure(
          p_ops, cross_dir, "summary", NULL, "climate_vs_ops_comparison",
          "_cross_industry", dims, format
        )
        if (verbose) message("  [OK] Climate vs ops comparison plot (unfiltered)")
      }
    }, error = function(e) {
      if (verbose) message(sprintf("  [ERR] Ops comparison: %s", conditionMessage(e)))
    })

    # 4. Cross-industry climate vs ops — FILTERED (if any industry has filtering)
    # Load credible config IDs and pairs from per-industry files
    credible_by_industry <- list()
    credible_pairs_by_industry <- list()
    for (ind_key in names(industries)) {
      cred_path <- file.path(output_dir, ind_key, "cv_credible_configs.csv")
      pairs_path <- file.path(output_dir, ind_key, "cv_credible_pairs.csv")
      if (file.exists(cred_path)) {
        credible_by_industry[[ind_key]] <- as.character(
          read_csv(cred_path, show_col_types = FALSE)$config_id
        )
      }
      if (file.exists(pairs_path)) {
        credible_pairs_by_industry[[ind_key]] <- read_csv(pairs_path, show_col_types = FALSE) %>%
          mutate(config_id = as.character(config_id))
      }
    }

    if (length(credible_by_industry) > 0 || length(credible_pairs_by_industry) > 0) {
      tryCatch({
        p_ops_filt <- plot_cross_industry_climate_vs_ops(
          industries, industry_labels,
          credible_configs = credible_by_industry,
          credible_pairs = if (length(credible_pairs_by_industry) > 0) credible_pairs_by_industry else NULL
        )
        if (!is.null(p_ops_filt)) {
          n_panels <- length(industries) * 3
          dims <- list(width = 14, height = max(8, 3 + n_panels * 1.2))
          cross_idx <- cross_idx + 1
          cross_rows[[cross_idx]] <- save_figure(
            p_ops_filt, cross_dir, "summary", NULL, "climate_vs_ops_comparison_filtered",
            "_cross_industry", dims, format
          )
          if (verbose) message("  [OK] Climate vs ops comparison plot (CV-filtered)")
        }
      }, error = function(e) {
        if (verbose) message(sprintf("  [ERR] Filtered ops comparison: %s", conditionMessage(e)))
      })
    }

    # Add cross-industry rows to manifest
    cross_manifest <- bind_rows(cross_rows)
    full_manifest <- bind_rows(full_manifest, cross_manifest)
  }

  # Save manifest
  manifest_path <- file.path(output_dir, "_manifest.csv")
  write_csv(full_manifest, manifest_path)
  if (verbose) message(sprintf("\nManifest saved: %s (%d total figures)", manifest_path, nrow(full_manifest)))

  invisible(full_manifest)
}


# ==============================================================================
# MAIN: FULL REPORT PIPELINE
# ==============================================================================

#' Generate figures and render Quarto report
#'
#' @param industries Named list of industry configs
#' @param output_dir Figure output directory
#' @param qmd_template Path to the Quarto template (.qmd)
#' @param report_output Path for rendered report (default: output_dir/mv_report.pdf)
#' @param format Figure format: "pdf", "png", or "both"
#' @param regenerate_figures If FALSE, skip figure generation (use existing manifest)
#' @param render If FALSE, skip Quarto rendering (just generate figures)
#' @param verbose Print progress
#' @return Manifest tibble (invisibly)
generate_mv_report <- function(industries,
                                output_dir = "report_figures",
                                qmd_template = "mv_report_template.qmd",
                                report_output = NULL,
                                format = "pdf",
                                regenerate_figures = TRUE,
                                render = TRUE,
                                verbose = TRUE) {

  # Step 1: Generate figures (or load existing manifest)
  if (regenerate_figures) {
    manifest <- generate_mv_figures(industries, output_dir, format, verbose)
  } else {
    manifest_path <- file.path(output_dir, "_manifest.csv")
    if (!file.exists(manifest_path)) {
      stop("No manifest found at ", manifest_path, ". Run with regenerate_figures = TRUE first.")
    }
    manifest <- read_csv(manifest_path, show_col_types = FALSE)
    if (verbose) message(sprintf("Loaded existing manifest: %d figures", nrow(manifest)))
  }

  # Step 2: Render Quarto
  if (render) {
    if (!file.exists(qmd_template)) {
      stop("Quarto template not found: ", qmd_template)
    }

    if (is.null(report_output)) {
      report_output <- file.path(output_dir, "mv_report.pdf")
    }

    if (verbose) message(sprintf("\nRendering Quarto: %s -> %s", qmd_template, report_output))

    # Build industry labels for Quarto params
    industry_labels <- setNames(
      sapply(industries, function(x) x$label %||% ""),
      names(industries)
    )

    quarto::quarto_render(
      input = qmd_template,
      output_file = basename(report_output),
      execute_params = list(
        figure_dir = normalizePath(output_dir),
        manifest   = normalizePath(file.path(output_dir, "_manifest.csv")),
        industry_labels = paste(names(industry_labels), industry_labels, sep = "=", collapse = ";")
      )
    )

    # Quarto places the output next to the .qmd file, not in our output_dir.
    # Search for it and move it to the desired location.
    qmd_dir <- dirname(normalizePath(qmd_template))
    rendered_in_qmd_dir <- file.path(qmd_dir, basename(report_output))
    rendered_next_to_qmd <- sub("\\.qmd$", ".pdf", normalizePath(qmd_template))

    # Also check current working directory
    rendered_in_wd <- file.path(getwd(), basename(report_output))

    # Find the rendered PDF — check all possible locations
    candidate_paths <- unique(c(
      rendered_in_qmd_dir,
      rendered_next_to_qmd,
      rendered_in_wd,
      file.path(qmd_dir, "mv_report.pdf"),
      basename(report_output)
    ))

    found_path <- NULL
    for (cp in candidate_paths) {
      if (file.exists(cp) && normalizePath(cp) != normalizePath(report_output)) {
        found_path <- cp
        break
      }
    }

    # If the file already exists at the target, we're done
    report_output_full <- normalizePath(report_output, mustWork = FALSE)
    if (file.exists(report_output_full)) {
      if (verbose) message(sprintf("Report rendered: %s", report_output_full))
    } else if (!is.null(found_path)) {
      dir.create(dirname(report_output_full), showWarnings = FALSE, recursive = TRUE)
      file.copy(found_path, report_output_full, overwrite = TRUE)
      file.remove(found_path)
      if (verbose) message(sprintf("Report moved to: %s", report_output_full))
    } else {
      # Last resort: search recursively
      search_result <- list.files(path = c(qmd_dir, getwd(), output_dir),
                                  pattern = "mv_report\\.pdf$",
                                  full.names = TRUE, recursive = TRUE)
      if (length(search_result) > 0) {
        file.copy(search_result[1], report_output_full, overwrite = TRUE)
        if (verbose) message(sprintf("Report found at %s, moved to: %s",
                                     search_result[1], report_output_full))
      } else {
        warning("Quarto reported success but rendered PDF could not be located. ",
                "Check the directory containing your .qmd template: ", qmd_dir)
      }
    }
  }

  invisible(manifest)
}


# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================

# source("shared/postprocessing.R")
# source("generate_mv_report.R")
#
# industries <- list(
#   rail = industry_config(
#     mv_results      = "results/rail/rail_mv_results.parquet",
#     cv_results      = "results/rail/rail_cv_results.parquet",
#     config_registry = "results/rail/config_registry.csv",
#     label           = "Rail (FRA)",
#     decision_vars   = c("window_type", "embedding_model", "sent_method",
#                          "comp__method", "lag_days", "halflife_days"),
#     ops_vars        = c("train_miles", "passenger_miles", "staff_hours")
#   ),
#   nrc = industry_config(
#     mv_results      = "results/nrc/nrc_mv_results.parquet",
#     cv_results      = "results/nrc/nrc_cv_results.parquet",
#     config_registry = "results/nrc/config_registry.csv",
#     label           = "Nuclear (NRC)",
#     decision_vars   = c("window_type", "embedding_model", "sent_method",
#                          "comp__method"),
#     ops_vars        = c("capacity_factor", "power_std")
#   )
# )
#
# # Generate everything
# generate_mv_report(industries, output_dir = "report_figures/",
#                     qmd_template = "mv_report_template.qmd")
#
# # Just regenerate figures, don't re-render
# generate_mv_report(industries, render = FALSE)
#
# # Just re-render with existing figures
# generate_mv_report(industries, regenerate_figures = FALSE)
