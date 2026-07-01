# =============================================================================
# common/sensitivity_analyses.R — Sensitivity analyses for reporting bias
# =============================================================================
#
# Implements org-level report-rate sensitivity for the reporting-bias
# discussion. For each (industry × outcome) cell, computes:
#
#   - org-level mean climate score (from the champion's climate_var)
#   - org-level report rate = sum(outcome_count) / sum(exposure)
#   - Spearman + Pearson correlation between mean_climate and report_rate
#
# Interpretation:
#   - Positive correlation → high-climate orgs file MORE per unit exposure.
#     Inconsistent with a pure "suppression at high-climate orgs" story.
#   - Negative correlation → high-climate orgs file FEWER per unit exposure.
#     Consistent with either causal climate effect OR suppression — does not
#     by itself distinguish them.
#   - Near zero → no evidence of an org-level reporting-bias gradient.
#
# Output is a tidy tibble (one row per industry × outcome × org) plus a
# correlation summary table (one row per industry × outcome). The driver
# script (6_sensitivity_plots.R) renders the scatter plots.
#
# Dependencies: dplyr, tidyr, broom (for cor.test → tidy)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})


#' Compute the org-level summary for one champion cell.
#'
#' @param panel Panel tibble from prepare_<industry>_panel_data() for the
#'   champion's config.
#' @param champ_row Single-row tibble from build_champion_manuscript_table()
#'   identifying the champion.
#' @param outcome_var Outcome column name (e.g., "n_accidents").
#' @param offset_var Exposure column name (e.g., "train_miles").
#' @param org_var Organization grouping column name.
#' @param min_panel_cells Minimum panel cells per org to include in the
#'   summary (drops thin orgs).
#' @return Tibble with columns: org, mean_climate, total_reports,
#'   total_exposure, report_rate, n_panel_cells, climate_var.
org_report_rate_summary <- function(panel, champ_row,
                                     outcome_var, offset_var, org_var,
                                     min_panel_cells = 6L) {

  climate_var <- champ_row$best_climate_var

  if (!climate_var %in% names(panel)) {
    stop(sprintf("Climate variable '%s' not in panel", climate_var))
  }
  for (col in c(outcome_var, offset_var, org_var)) {
    if (!col %in% names(panel)) stop(sprintf("Column '%s' not in panel", col))
  }

  # Filter to rows with valid climate (the panel keeps NA climate cells but
  # we exclude them from the org mean since they don't enter the model)
  d <- panel %>%
    filter(!is.na(.data[[climate_var]]),
           !is.na(.data[[outcome_var]]),
           !is.na(.data[[offset_var]]),
           .data[[offset_var]] > 0)

  summary_df <- d %>%
    group_by(.org = .data[[org_var]]) %>%
    summarise(
      mean_climate     = mean(.data[[climate_var]], na.rm = TRUE),
      total_reports    = sum(.data[[outcome_var]], na.rm = TRUE),
      total_exposure   = sum(.data[[offset_var]], na.rm = TRUE),
      n_panel_cells    = n(),
      .groups = "drop"
    ) %>%
    filter(n_panel_cells >= min_panel_cells,
           total_exposure > 0,
           is.finite(mean_climate)) %>%
    mutate(report_rate = total_reports / total_exposure,
           # Log-report-rate is often more linear vs. climate; add it for the
           # secondary correlation. Add a small epsilon (half the smallest
           # non-zero rate) to handle zero-count orgs.
           log_report_rate = ifelse(
             report_rate > 0,
             log(report_rate),
             log(0.5 * min(report_rate[report_rate > 0], na.rm = TRUE))
           ),
           climate_var = climate_var) %>%
    rename(!!org_var := .org)

  summary_df
}


#' Compute correlation summary for one org-level table.
#'
#' Uses both Spearman (rank-based, robust to log-scale outliers) and Pearson
#' (linear) and reports both. Bootstrap CI on Spearman ρ via 1000 resamples.
#'
#' @param org_summary Tibble from org_report_rate_summary().
#' @param n_boot Bootstrap resamples for the Spearman CI (default 1000).
#' @return One-row tibble with correlation point estimates, p-values, CIs.
report_rate_correlation <- function(org_summary, n_boot = 1000L) {
  if (nrow(org_summary) < 5) {
    return(tibble(
      n_orgs   = nrow(org_summary),
      spearman_rho = NA_real_, spearman_p = NA_real_,
      spearman_ci_low = NA_real_, spearman_ci_high = NA_real_,
      pearson_r = NA_real_,    pearson_p = NA_real_,
      log_pearson_r = NA_real_, log_pearson_p = NA_real_,
      note = "Too few orgs (< 5)"
    ))
  }

  sp <- suppressWarnings(
    cor.test(org_summary$mean_climate, org_summary$report_rate,
              method = "spearman", exact = FALSE)
  )
  pe <- suppressWarnings(
    cor.test(org_summary$mean_climate, org_summary$report_rate,
              method = "pearson")
  )
  pe_log <- suppressWarnings(
    cor.test(org_summary$mean_climate, org_summary$log_report_rate,
              method = "pearson")
  )

  # Bootstrap CI on Spearman ρ
  set.seed(42)
  ix <- replicate(n_boot,
                  sample.int(nrow(org_summary), replace = TRUE))
  rho_boot <- apply(ix, 2, function(i) {
    suppressWarnings(cor(org_summary$mean_climate[i],
                          org_summary$report_rate[i],
                          method = "spearman"))
  })
  rho_ci <- quantile(rho_boot, c(0.025, 0.975), na.rm = TRUE)

  tibble(
    n_orgs            = nrow(org_summary),
    spearman_rho      = unname(sp$estimate),
    spearman_p        = sp$p.value,
    spearman_ci_low   = unname(rho_ci[1]),
    spearman_ci_high  = unname(rho_ci[2]),
    pearson_r         = unname(pe$estimate),
    pearson_p         = pe$p.value,
    log_pearson_r     = unname(pe_log$estimate),
    log_pearson_p     = pe_log$p.value,
    note              = ""
  )
}


#' Combine the per-cell summaries into one tidy long table for downstream
#' plotting and reporting.
#'
#' @param org_results Named list keyed by "industry|outcome" with elements
#'   $org_summary (tibble) and $champ_row.
#' @return Tibble with industry, outcome, industry_label, outcome_label,
#'   org, mean_climate, report_rate, log_report_rate, total_reports,
#'   total_exposure, n_panel_cells, climate_var.
combine_org_summaries <- function(org_results) {
  purrr::map_dfr(names(org_results), function(k) {
    parts <- strsplit(k, "\\|")[[1]]
    s <- org_results[[k]]$org_summary
    s %>%
      mutate(industry       = parts[1],
              outcome        = parts[2],
              industry_label = org_results[[k]]$champ_row$industry_label,
              outcome_label  = org_results[[k]]$champ_row$outcome_label)
  })
}
