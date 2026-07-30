# =============================================================================
# common/best_model_analysis.R — Champion-Model & Facet-Enrichment Analysis
# =============================================================================
#
# Complements the multiverse spec-curve view with a "where's the ceiling, and
# what gets you there" view. For each (industry × outcome × cv_strategy):
#
#   1. champions: identify the single best spec (highest delta-loglik vs
#      seasonal_ops baseline) AND the pool of "candidate champions" within
#      THRESHOLD_PCT of the best. The best alone is noisy; the candidate pool
#      is what feeds the enrichment statistics.
#
#   2. facet enrichment: for each analytical choice (embedding_model,
#      sent_method, comp__method, th__method, window_type, window_size,
#      lag_days, halflife_days), compare its level distribution among
#      candidate champions vs. the full multiverse. Reports lift
#      (log2(top_share / all_share)) and a hypergeometric p-value with BH-FDR
#      correction within each cell.
#
#   3. cross-industry roll-up: counts how often each facet level dominates
#      the champion pool across all (industry × outcome × cv_strategy) cells.
#
# Outputs are CSV tables, written per-industry and one cross-industry, that
# the Quarto report renders in a "Champion Models" appendix.
#
# Dependencies: dplyr, tidyr, arrow, stringr (loaded via tidyverse upstream).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(arrow)
})


# =============================================================================
# 1. CHAMPIONS — best spec + candidate-champion pool per cell
# =============================================================================

#' For each (outcome × cv_strategy), compute the best climate-vs-seasonal_ops
#' delta-loglik and the pool of "candidate champion" specs within
#' threshold_pct (absolute fractional gap from the best).
#'
#' @param cv_results CV results (already passed through normalize_cv_columns).
#'   Must include the precomputed model_label == "delta_climate_vs_seasonal"
#'   rows produced by run_panel_cv_for_outcome.
#' @param mv_results_linked MV results linked to config registry (output of
#'   link_results_to_config()). Used to enrich champions with config metadata.
#' @param threshold_pct Specs with delta within this fraction of |best_delta|
#'   are kept as "candidate champions". Default 0.10 = "within 10% of the
#'   best." When best_delta <= 0 (climate doesn't help), the candidate pool
#'   collapses to the single best spec.
#' @param min_pool_size Refuse to compute enrichment when the candidate pool
#'   has fewer than this many specs (statistics not meaningful).
#' @return List with $best, $candidates, $diagnostics.
compute_champions <- function(cv_results,
                              mv_results_linked,
                              threshold_pct = 0.10,
                              min_pool_size = 5L) {

  cv <- cv_results %>%
    filter(model_label == "delta_climate_vs_seasonal", !is.na(loglik_mean))

  if (nrow(cv) == 0) {
    return(list(best = tibble(), candidates = tibble(),
                diagnostics = tibble()))
  }

  # Standardize the metric column to .delta for downstream uniformity
  cv <- cv %>% mutate(.delta = loglik_mean)

  # Per-cell best delta and candidate pool
  per_cell <- cv %>%
    group_by(outcome, cv_strategy) %>%
    mutate(
      best_delta = max(.delta, na.rm = TRUE),
      gap        = best_delta - .delta,
      tol        = threshold_pct * abs(best_delta)
    ) %>%
    ungroup()

  best <- per_cell %>%
    group_by(outcome, cv_strategy) %>%
    slice_max(.delta, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(outcome, cv_strategy, best_config_id = config_id,
           best_climate_var = climate_var, best_delta_ll = .delta)

  candidates <- per_cell %>%
    filter(gap <= tol) %>%
    select(outcome, cv_strategy, config_id, climate_var,
           delta_ll = .delta, best_delta_ll = best_delta)

  # Enrich champions with config + window metadata (one row per candidate)
  if (nrow(mv_results_linked) > 0 && nrow(candidates) > 0) {
    keys <- c("config_id", "climate_var", "outcome")
    if (!all(keys %in% names(mv_results_linked))) {
      keys <- intersect(keys, names(mv_results_linked))
    }
    mv_meta <- mv_results_linked %>%
      mutate(config_id = as.character(config_id)) %>%
      select(any_of(c(keys, "embedding_model", "sent_method", "comp__method",
                      "th__method", "window_type", "window_size",
                      "lag_days", "halflife_days",
                      "climate_estimate", "climate_pval"))) %>%
      distinct()
    candidates <- candidates %>%
      mutate(config_id = as.character(config_id)) %>%
      left_join(mv_meta, by = keys)
    best <- best %>%
      mutate(config_id = as.character(best_config_id),
             climate_var = best_climate_var) %>%
      left_join(mv_meta, by = keys) %>%
      select(-config_id, -climate_var)
  }

  # Diagnostics: pool size per cell
  diagnostics <- candidates %>%
    group_by(outcome, cv_strategy) %>%
    summarise(
      n_candidates = n(),
      best_delta_ll = first(best_delta_ll),
      pool_meaningful = n() >= min_pool_size,
      .groups = "drop"
    )

  list(best = best, candidates = candidates, diagnostics = diagnostics)
}


# =============================================================================
# 2. FACET ENRICHMENT
# =============================================================================

#' For each (outcome × cv_strategy × facet × level), compute lift of the
#' level's representation in the candidate-champion pool vs the full
#' multiverse, plus a hypergeometric p-value, FDR-corrected within each
#' (outcome × cv_strategy) cell.
#'
#' @param champions Output of compute_champions(); requires $candidates.
#' @param mv_results_linked Full MV results with config metadata. Defines the
#'   "all-spec" reference distribution per (outcome).
#' @param facets Character vector of facet column names. Levels with all-NA
#'   are skipped.
#' @param min_level_count Drop levels with all_n < this in the full multiverse
#'   (too few specs to compare meaningfully).
#' @return Tibble of enrichment rows.
compute_facet_enrichment <- function(champions, mv_results_linked,
                                      facets = c("window_type", "window_size",
                                                 "lag_days", "halflife_days",
                                                 "embedding_model", "sent_method",
                                                 "comp__method", "th__method"),
                                      min_level_count = 3L) {

  cand <- champions$candidates
  if (is.null(cand) || nrow(cand) == 0) return(tibble())

  facets <- intersect(facets, names(mv_results_linked))
  if (length(facets) == 0) return(tibble())

  # Per (outcome, cv_strategy, facet, level): top vs all counts
  out_rows <- list()
  for (cv_strat in unique(cand$cv_strategy)) {
    for (outc in unique(cand$outcome)) {
      cand_cell <- cand %>% filter(cv_strategy == !!cv_strat, outcome == !!outc)
      if (nrow(cand_cell) == 0) next

      all_cell <- mv_results_linked %>%
        filter(outcome == !!outc) %>%
        # MV is per-(config, climate_var); CV cell pools across strategies, but
        # the multiverse spec set is the same. So all_cell is the universe.
        distinct(config_id, climate_var, .keep_all = TRUE)

      n_top <- nrow(cand_cell)
      n_all <- nrow(all_cell)
      if (n_all == 0 || n_top == 0) next

      cell_rows <- list()
      for (f in facets) {
        if (all(is.na(all_cell[[f]]))) next

        top_levels <- cand_cell[[f]] %>% as.character()
        all_levels <- all_cell[[f]] %>% as.character()

        levels_seen <- sort(unique(c(top_levels, all_levels)))
        levels_seen <- levels_seen[!is.na(levels_seen) & levels_seen != "NA"]

        for (lv in levels_seen) {
          top_n <- sum(top_levels == lv, na.rm = TRUE)
          all_n <- sum(all_levels == lv, na.rm = TRUE)
          if (all_n < min_level_count) next

          top_share <- top_n / n_top
          all_share <- all_n / n_all
          # log2 lift, smoothed to avoid -Inf when top_share == 0
          enrichment <- log2((top_share + 1e-6) / (all_share + 1e-6))

          # Hypergeometric: P(X >= top_n) drawing n_top from urn with all_n
          # successes out of n_all balls. phyper(q-1, m, n, k, lower.tail=FALSE)
          # gives upper-tail; mirror for the lower tail and take 2× min as a
          # two-sided p-value.
          p_upper <- phyper(top_n - 1L, all_n, n_all - all_n, n_top,
                             lower.tail = FALSE)
          p_lower <- phyper(top_n,     all_n, n_all - all_n, n_top,
                             lower.tail = TRUE)
          p_value <- min(1, 2 * min(p_upper, p_lower))

          cell_rows[[length(cell_rows) + 1L]] <- tibble(
            outcome     = outc,
            cv_strategy = cv_strat,
            facet       = f,
            level       = lv,
            top_n       = top_n,
            top_share   = top_share,
            all_n       = all_n,
            all_share   = all_share,
            enrichment  = enrichment,
            p_value     = p_value
          )
        }
      }

      if (length(cell_rows) > 0) {
        cell_df <- bind_rows(cell_rows)
        cell_df$q_value <- p.adjust(cell_df$p_value, method = "BH")
        out_rows[[length(out_rows) + 1L]] <- cell_df
      }
    }
  }

  bind_rows(out_rows)
}


# =============================================================================
# 3. CROSS-INDUSTRY CHAMPION ROLL-UP
# =============================================================================

#' Combine per-industry champions into a cross-industry roll-up that counts
#' how often each facet level dominates the candidate pool across all cells.
#'
#' @param industry_champions Named list of compute_champions() outputs, one
#'   per industry.
#' @param facets Facet columns to roll up.
#' @return Tibble with industry × outcome × cv_strategy × facet × dominant_level
#'   rows, AND a cross-industry summary frame counting how many cells each
#'   level dominates.
summarize_champions_cross_industry <- function(industry_champions,
                                                facets = c("window_type",
                                                           "window_size",
                                                           "lag_days",
                                                           "halflife_days",
                                                           "embedding_model",
                                                           "sent_method",
                                                           "comp__method",
                                                           "th__method")) {

  # 1) Per cell × facet: which level appears most often in the candidate pool?
  per_cell <- list()
  for (ind_key in names(industry_champions)) {
    cand <- industry_champions[[ind_key]]$candidates
    if (is.null(cand) || nrow(cand) == 0) next
    avail <- intersect(facets, names(cand))
    if (length(avail) == 0) next

    for (f in avail) {
      tbl <- cand %>%
        mutate(.lv = as.character(.data[[f]])) %>%
        filter(!is.na(.lv), .lv != "NA") %>%
        count(outcome, cv_strategy, .lv, name = "n") %>%
        group_by(outcome, cv_strategy) %>%
        slice_max(n, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(industry = ind_key, facet = f) %>%
        rename(dominant_level = .lv)
      per_cell[[length(per_cell) + 1L]] <- tbl
    }
  }
  per_cell_df <- bind_rows(per_cell)

  # 2) Cross-industry: for each facet × dominant_level, how many cells?
  if (nrow(per_cell_df) == 0) {
    return(list(per_cell = per_cell_df,
                cross_industry = tibble()))
  }

  total_cells_per_facet <- per_cell_df %>%
    group_by(facet) %>%
    summarise(total_cells = n(), .groups = "drop")

  cross_industry <- per_cell_df %>%
    count(facet, dominant_level, name = "cells_dominated") %>%
    left_join(total_cells_per_facet, by = "facet") %>%
    mutate(pct_dominated = round(100 * cells_dominated / total_cells, 1)) %>%
    arrange(facet, desc(cells_dominated))

  list(per_cell = per_cell_df, cross_industry = cross_industry)
}
