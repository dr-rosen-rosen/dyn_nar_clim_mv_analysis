############ Sensitivity: org-level report-rate vs. climate
# Tests whether high-climate orgs file proportionally more or fewer reports
# per unit exposure. Used to assess the selection-bias / reporting-culture
# alternative interpretations of the negative climate effects.
#
# Reuses the per-industry panel-data closures from 5_manuscript_plots.R.
# Reads champions from build_champion_manuscript_table() (same as Figure 1).
#
# Outputs:
#   report_figures_manuscript/sensitivity_report_rate_orgs.csv     (one row per industry × outcome × org)
#   report_figures_manuscript/sensitivity_report_rate_corr.csv     (one row per industry × outcome)
#   report_figures_manuscript/sensitivity_report_rate_panels.pdf   (scatter-plot grid)

PLOT_BASE_SIZE <- 12

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(patchwork)
  library(glmmTMB)
  library(broom.mixed)
  library(slider)
  library(lubridate)
  library(stringr)
  library(readxl)
  library(here)
})

source("common/postprocessing.R")
source("common/configuration_concordance.R")
source("common/manuscript_figures.R")
source("common/sensitivity_analyses.R")

# Per-industry modules
source("rail/config.R"); source("common/data_prep.R")
source("common/panel_data_prep.R")
source("rail/data_prep.R"); source("rail/panel_data_prep.R"); source("rail/panel_fit_models.R")
source("nrc/config.R");  source("nrc/panel_data_prep.R"); source("nrc/panel_fit_models.R")
source("aviation/config.R"); source("aviation/data_prep.R")
source("aviation/panel_data_prep.R"); source("aviation/panel_fit_models.R")
# MSHA (coal): panel-prep + fit modules only (not msha/config.R).
source("msha/panel_data_prep.R"); source("msha/panel_fit_models.R")


# =============================================================================
# 1. INDUSTRY CONFIGS (same as 5_manuscript_plots.R)
# =============================================================================

industries <- list(
  rail = list(
    label           = "Rail (FRA)",
    mv_results      = "results_new_new/rail/panel_mv_results.parquet",
    cv_results      = "results_new_new/rail/panel_cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026",
                                 "config_registry.csv"),
    cfg_dir         = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026"
  ),
  nrc = list(
    label           = "Nuclear (NRC)",
    mv_results      = "results_new_new/nrc/panel_mv_results.parquet",
    cv_results      = "results_new_new/nrc/panel_cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026",
                                 "config_registry.csv"),
    cfg_dir         = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026"
  ),
  aviation = list(
    label           = "Aviation (NTSB / AIDS / ASRS)",
    mv_results      = "results_new_new/aviation/panel_mv_results_quarterly.parquet",
    cv_results      = "results_new_new/aviation/panel_cv_results_quarterly.parquet",
    config_registry = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026/config_registry.csv",
    cfg_dir         = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026"
  ),
  msha = list(
    label           = "Mining (MSHA coal)",
    mv_results      = "results_new_new/msha/panel_mv_results_coal_detectability_full.parquet",
    cv_results      = "results_new_new/msha/panel_cv_results_coal_detectability_full.parquet",
    config_registry = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026/config_registry.csv",
    cfg_dir         = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026"
  )
)


# =============================================================================
# 2. CHAMPIONS (timeseries only)
# =============================================================================

champion_table <- build_champion_manuscript_table(
  industries     = industries,
  report_dir     = "report_figures_panel",
  cv_strategies  = c("timeseries")
) %>% filter(cv_strategy == "timeseries")

cat(sprintf("Loaded %d timeseries champions across %d industries\n",
            nrow(champion_table), n_distinct(champion_table$industry)))


# =============================================================================
# 3. PER-INDUSTRY DATA-LOADING (reuse closures)
# =============================================================================

# --- Rail ---
rail_events <- read_parquet("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet") %>%
  rename(total_persons_killed  = `Total Persons Killed`,
         total_persons_injured = `Total Persons Injured`,
         total_damage_cost     = `Total Damage Cost`) %>%
  mutate(total_damage_cost = as.numeric(gsub("[^0-9.]", "", total_damage_cost))) %>%
  select(org_id, eid, event_date,
         total_persons_killed, total_persons_injured, total_damage_cost) %>%
  mutate(event_date = as.Date(event_date))
rail_ops_raw <- readr::read_csv("data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv",
                                 show_col_types = FALSE) %>%
  mutate(across(all_of(OPS_VARS), ~ ifelse(.x < 0, NA_real_, .x))) %>%
  mutate(yearmonth = as.Date(yearmonth))
rail_ops_features <- make_ops_features_rolling(rail_ops_raw) %>%
  distinct(org_id, yearmonth, .keep_all = TRUE)
rail_panel_data_fn <- function(parquet_path, config_id) {
  prepare_rail_panel_data(
    parquet_path = parquet_path, config_id = config_id,
    events_df = rail_events, ops_raw = rail_ops_raw,
    ops_features = rail_ops_features,
    min_reports = 50L, max_holdover_days = 365L,
    climate_base = "overall_final_score"
  )
}

# --- NRC ---
nrc_events <- read_parquet("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet") %>%
  mutate(event_date = as.Date(event_date), eid = as.character(event_num))
nrc_ops_raw <- read_parquet("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet") %>%
  mutate(quarter_start = as.Date(quarter_start))
nrc_panel_data_fn <- function(parquet_path, config_id) {
  prepare_nrc_panel_data(
    parquet_path = parquet_path, config_id = config_id,
    events_df = nrc_events, ops_raw = nrc_ops_raw,
    min_reports = MIN_REPORTS_DEFAULT,
    max_holdover_days = 365L, climate_base = "overall_final_score"
  )
}

# --- Aviation ---
asrs_meta <- load_asrs_meta_panel("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet")
av_ops_features <- read_parquet("data/aviation/bts_t100/airport_month_ops.parquet")
ntsb_panel <- load_ntsb_panel_rate("data/aviation/ntsb_av_accident_data/events.xlsx",
                                    "data/aviation/ntsb_av_accident_data/events_pre2008.xlsx",
                                    apt_dist_nm = 5L, period = "quarter")
aids_panel <- load_aids_panel_rate("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/aids_events.parquet",
                                    apt_dist_nm = 5L, min_year = 1988L, period = "quarter")
aviation_panel_data_fn <- function(parquet_path, config_id) {
  prepare_aviation_panel_data(
    parquet_path = parquet_path, config_id = config_id,
    asrs_meta_df = asrs_meta, ntsb_panel_df = ntsb_panel,
    ops_features = av_ops_features, aids_panel_df = aids_panel,
    atc_scope = "local_terminal", missing_climate = "exclude",
    min_reports = MIN_REPORTS_DEFAULT, period = "quarter", max_holdover_days = 365L,
    climate_base = "overall_final_score"
  )
}

# --- MSHA (coal) ---
msha_events <- read_parquet("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/events.parquet") %>%
  mutate(mine_id = as.character(mine_id), eid = as.character(eid),
         event_date = as.Date(accident_date), commodity = tolower(trimws(commodity))) %>%
  filter(!is.na(mine_id), !is.na(event_date), commodity == "coal")
msha_ops <- read_parquet("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/ops_mine_quarter.parquet") %>%
  mutate(mine_id = as.character(mine_id), commodity = tolower(trimws(commodity))) %>%
  filter(commodity == "coal")
msha_cov <- read_parquet("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/covariates_mine_quarter.parquet") %>%
  mutate(mine_id = as.character(mine_id))
msha_panel_data_fn <- function(parquet_path, config_id) {
  prepare_msha_panel_data(
    parquet_path = parquet_path, config_id = config_id,
    events_df = msha_events, ops_raw = msha_ops, covariates_df = msha_cov,
    min_reports = 30L, max_holdover_days = 365L,
    climate_base = "overall_final_score"
  )
}


# =============================================================================
# 4. PER-CELL OUTCOME-COLUMN MAP
# =============================================================================

# (outcome_var, offset_var, org_var) per (industry × outcome) — same shape as
# the spec_map in 5_manuscript_plots.R.
spec_map <- list(
  rail = list(
    var = list(rate_accidents = "n_accidents", rate_injuries = "sum_injured",
               rate_fatalities = "sum_killed"),
    offset = list(rate_accidents = "train_miles", rate_injuries = "staff_hours",
                  rate_fatalities = "train_miles"),
    org = "org_id"
  ),
  nrc = list(
    var = list(rate_lers = "n_lers", rate_emerg = "n_emerg",
               rate_scrams = "n_scrams", rate_pct_power_loss = "sum_pct_power_loss"),
    offset = list(rate_lers = "days_at_power_total", rate_emerg = "days_at_power_total",
                  rate_scrams = "days_at_power_total",
                  rate_pct_power_loss = "days_at_power_total"),
    org = "facility_site"
  ),
  aviation = list(
    var = list(rate_aids_noharm = "n_aids_noharm", rate_aids_propdamage = "n_aids_propdamage",
               rate_ntsb_serious_fatal = "n_ntsb_serious_fatal"),
    offset = list(rate_aids_noharm = "departures", rate_aids_propdamage = "departures",
                  rate_ntsb_serious_fatal = "departures"),
    org = "airport_id"
  ),
  msha = list(
    var = list(rate_t0 = "n_t0", rate_t1 = "n_t1", rate_t2 = "n_t2", rate_t3 = "n_t3",
               rate_days_lost = "sum_days_lost"),
    offset = list(rate_t0 = "hours_worked", rate_t1 = "hours_worked", rate_t2 = "hours_worked",
                  rate_t3 = "hours_worked", rate_days_lost = "hours_worked"),
    org = "mine_id"
  )
)
panel_fns <- list(rail = rail_panel_data_fn,
                  nrc  = nrc_panel_data_fn,
                  aviation = aviation_panel_data_fn,
                  msha = msha_panel_data_fn)


# =============================================================================
# 5. PER-CELL REPORT-RATE SUMMARIES
# =============================================================================

cat("\n=== Computing org-level report-rate summaries ===\n")

org_results <- list()
for (i in seq_len(nrow(champion_table))) {
  row <- champion_table[i, ]
  ind <- row$industry
  outc <- row$outcome
  sm <- spec_map[[ind]]
  if (is.null(sm$var[[outc]])) {
    cat(sprintf("  [%d/%d] %s — %s : skipped (no spec_map entry)\n",
                i, nrow(champion_table), ind, outc))
    next
  }
  cat(sprintf("  [%d/%d] %s — %s ... ", i, nrow(champion_table), ind, outc))

  cid <- as.character(row$best_config_id)
  parquet_path <- file.path(industries[[ind]]$cfg_dir, "_cfg", cid, "results.parquet")
  panel <- tryCatch(
    panel_fns[[ind]](parquet_path = parquet_path, config_id = cid),
    error = function(e) { cat("PANEL ERR: ", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(panel)) next

  org_summary <- tryCatch(
    org_report_rate_summary(
      panel        = panel,
      champ_row    = row,
      outcome_var  = sm$var[[outc]],
      offset_var   = sm$offset[[outc]],
      org_var      = sm$org,
      min_panel_cells = 6L
    ),
    error = function(e) { cat("SUMMARY ERR: ", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(org_summary)) next

  org_results[[paste(ind, outc, sep = "|")]] <- list(
    org_summary = org_summary,
    champ_row   = row
  )
  cat(sprintf("ok (%d orgs)\n", nrow(org_summary)))
}


# =============================================================================
# 6. CORRELATION SUMMARY TABLE
# =============================================================================

cat("\n=== Computing correlations ===\n")

corr_summary <- purrr::map_dfr(names(org_results), function(k) {
  parts <- strsplit(k, "\\|")[[1]]
  r <- report_rate_correlation(org_results[[k]]$org_summary)
  r %>%
    mutate(industry = parts[1], outcome = parts[2],
           industry_label = org_results[[k]]$champ_row$industry_label,
           outcome_label  = org_results[[k]]$champ_row$outcome_label) %>%
    relocate(industry, industry_label, outcome, outcome_label)
})

dir.create("report_figures_manuscript", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(corr_summary, "report_figures_manuscript/sensitivity_report_rate_corr.csv")
print(corr_summary %>% select(industry_label, outcome_label, n_orgs,
                              spearman_rho, spearman_p,
                              spearman_ci_low, spearman_ci_high,
                              log_pearson_r, log_pearson_p))


# =============================================================================
# 7. LONG ORG-LEVEL TABLE + SCATTER GRID
# =============================================================================

orgs_long <- combine_org_summaries(org_results)
readr::write_csv(orgs_long,
                 "report_figures_manuscript/sensitivity_report_rate_orgs.csv")

cat(sprintf("\n=== Building scatter grid (%d orgs across %d cells) ===\n",
            nrow(orgs_long), length(org_results)))

# Annotate facets with the Spearman ρ + p-value
facet_labs <- corr_summary %>%
  mutate(.lab = sprintf("%s — %s\n%s = %.2f, p = %.2g (n = %d)",
                        industry_label, outcome_label,
                        "rho", spearman_rho, spearman_p, n_orgs)) %>%
  select(industry, outcome, .lab)
orgs_long <- orgs_long %>%
  left_join(facet_labs, by = c("industry", "outcome"))

p <- ggplot(orgs_long,
            aes(x = mean_climate, y = report_rate)) +
  geom_point(alpha = 0.55, size = 1.4, color = "#2c7bb6") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#d6604d", fill = "#d6604d", alpha = 0.15) +
  facet_wrap(~ .lab, scales = "free", ncol = 3) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Sensitivity: org-level mean climate vs. report rate per unit exposure",
    subtitle = paste0("If high-climate orgs file MORE per unit exposure (positive slope), the negative ",
                       "climate-outcome effect cannot be explained by suppression at high-climate orgs alone.\n",
                       "Each point is one organization; report rate = total events / total exposure."),
    x = "Mean climate score across panel (best-performing-model climate_var)",
    y = "Report rate (events per unit exposure)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.subtitle = element_text(color = "gray40"),
        panel.grid.minor = element_blank())

ggsave("report_figures_manuscript/sensitivity_report_rate_panels.pdf",
       p, width = 13, height = 9)

cat("\nSensitivity output:\n")
cat("  report_figures_manuscript/sensitivity_report_rate_orgs.csv\n")
cat("  report_figures_manuscript/sensitivity_report_rate_corr.csv\n")
cat("  report_figures_manuscript/sensitivity_report_rate_panels.pdf\n")


# =============================================================================
# 8. MIN-REPORTS CUTOFF SENSITIVITY
# =============================================================================
#
# The paper analyses use industry-specific minimum-event cutoffs (rail=50,
# nrc=75, aviation=30) to retain organizations with enough longitudinal
# coverage to support stable climate-window estimation. Reviewers may ask
# whether the headline cross-industry findings depend on those specific
# values. This section refits each main-text champion specification on
# panels constructed with an alternate cutoff and reports the resulting
# climate coefficient + CI alongside the paper's value.
#
# Tests (all main-text outcomes, paper cutoff vs an alternate that moves
# toward the modal 50-event value used in rail):
#   - Rail:     50 (paper) vs 75 (tightens; matches NRC's cutoff)
#   - NRC:      75 (paper) vs 50 (loosens; matches rail's cutoff)
#   - Aviation: 30 (paper) vs 50 (tightens; matches rail's cutoff)
#
# The 50-event value is the natural reference because it is rail's
# baseline; using it as the alternate gives a symmetric three-industry
# comparison.
#
# Outputs:
#   report_figures_manuscript/sensitivity_min_reports_table.csv
#   report_figures_manuscript/sensitivity_min_reports_forest.pdf

cat("\n=== Building min-reports cutoff sensitivity ===\n")

# Cells to test: paper's main-text outcomes for each industry, paired
# with an alternate cutoff. Each cell is fit twice — once at each cutoff —
# and the climate coefficient is compared.
sensitivity_cells <- tribble(
  ~industry,  ~outcome,             ~baseline_cutoff, ~alternate_cutoff,
  "rail",     "rate_accidents",                  50L,                75L,
  "rail",     "rate_injuries",                   50L,                75L,
  "rail",     "rate_fatalities",                 50L,                75L,
  "nrc",      "rate_lers",                       75L,                50L,
  "nrc",      "rate_emerg",                      75L,                50L,
  "nrc",      "rate_scrams",                     75L,                50L,
  "nrc",      "rate_pct_power_loss",             75L,                50L,
  "aviation", "rate_aids_all",                   30L,                50L,
  "aviation", "rate_accidents",                  30L,                50L,
  "msha",     "rate_accidents",                  30L,                50L,
  "msha",     "rate_injuries",                   30L,                50L,
  "msha",     "rate_days_lost",                  30L,                50L,
  "msha",     "rate_fatal",                      30L,                50L
)

# Industry-specific panel-rebuild closure with cutoff override.
# Reuses the data objects already loaded by the report-rate section above.
build_panel_at_cutoff <- function(industry, parquet_path, config_id, cutoff) {
  switch(industry,
    "rail" = prepare_rail_panel_data(
      parquet_path = parquet_path, config_id = config_id,
      events_df = rail_events, ops_raw = rail_ops_raw,
      ops_features = rail_ops_features,
      min_reports = cutoff,
      max_holdover_days = 365L, climate_base = "overall_final_score"
    ),
    "nrc" = prepare_nrc_panel_data(
      parquet_path = parquet_path, config_id = config_id,
      events_df = nrc_events, ops_raw = nrc_ops_raw,
      min_reports = cutoff,
      max_holdover_days = 365L, climate_base = "overall_final_score"
    ),
    "aviation" = prepare_aviation_panel_data(
      parquet_path = parquet_path, config_id = config_id,
      asrs_meta_df = asrs_meta, ntsb_panel_df = ntsb_panel,
      ops_features = av_ops_features, aids_panel_df = aids_panel,
      atc_scope = "local_terminal", missing_climate = "exclude",
      min_reports = cutoff,
      max_holdover_days = 365L, climate_base = "overall_final_score"
    ),
    "msha" = prepare_msha_panel_data(
      parquet_path = parquet_path, config_id = config_id,
      events_df = msha_events, ops_raw = msha_ops, covariates_df = msha_cov,
      min_reports = cutoff,
      max_holdover_days = 365L, climate_base = "overall_final_score"
    ),
    stop(sprintf("No build_panel_at_cutoff handler for industry %s", industry))
  )
}

# Industry-specific fit closure that builds the champion-formula and fits.
fit_champion_at_panel <- function(industry, outcome, panel, climate_var,
                                    config_id) {
  res <- switch(industry,
    "rail" = fit_single_rail_panel_model(
      data = panel, climate_var = climate_var,
      config_id = config_id, outcome = outcome
    ),
    "nrc" = fit_single_nrc_panel_model(
      data = panel, climate_var = climate_var,
      config_id = config_id, outcome = outcome
    ),
    "aviation" = fit_single_aviation_panel_model(
      data = panel, climate_var = climate_var,
      config_id = config_id, outcome = outcome
    ),
    "msha" = fit_single_msha_panel_model(
      data = panel, climate_var = climate_var,
      config_id = config_id, outcome = outcome
    ),
    stop(sprintf("No fit_champion_at_panel handler for industry %s", industry))
  )
  # Standardize climate coefficient to PER-SD (match the main-figure forest:
  # est_std = raw * in-panel SD of climate). p-value invariant.
  if (!is.null(res) && !is.null(res$climate_estimate) &&
      is.finite(res$climate_estimate) && climate_var %in% names(panel)) {
    sigma <- sd(panel[[climate_var]], na.rm = TRUE)
    if (is.finite(sigma) && sigma != 0) {
      for (k in c("climate_estimate","climate_se","climate_ci_low","climate_ci_high"))
        if (!is.null(res[[k]]) && is.finite(res[[k]])) res[[k]] <- res[[k]] * sigma
    }
  }
  res
}

# Loop over sensitivity cells × (baseline, alternate) cutoff.
sensitivity_rows <- list()
for (i in seq_len(nrow(sensitivity_cells))) {
  cell <- sensitivity_cells[i, ]

  champ <- champion_table %>%
    filter(industry == cell$industry, outcome == cell$outcome)
  if (nrow(champ) != 1) {
    cat(sprintf("  [%s | %s] no unique champion — skipping\n",
                cell$industry, cell$outcome))
    next
  }
  cid <- as.character(champ$best_config_id)
  cv  <- champ$best_climate_var
  parquet_path <- file.path(industries[[cell$industry]]$cfg_dir,
                             "_cfg", cid, "results.parquet")

  for (cutoff in c(cell$baseline_cutoff, cell$alternate_cutoff)) {
    label <- if (cutoff == cell$baseline_cutoff) "baseline" else "alternate"
    cat(sprintf("  [%s | %s] min_reports = %d (%s) ... ",
                cell$industry, cell$outcome, cutoff, label))

    panel <- tryCatch(
      build_panel_at_cutoff(cell$industry, parquet_path, cid, cutoff),
      error = function(e) { cat("PANEL ERR:", conditionMessage(e), "\n"); NULL }
    )
    if (is.null(panel)) next

    fit <- tryCatch(
      fit_champion_at_panel(cell$industry, cell$outcome, panel, cv, cid),
      error = function(e) { cat("FIT ERR:", conditionMessage(e), "\n"); NULL }
    )
    if (is.null(fit)) next

    sensitivity_rows[[length(sensitivity_rows) + 1L]] <- tibble(
      industry              = cell$industry,
      industry_label        = champ$industry_label,
      outcome               = cell$outcome,
      outcome_label         = champ$outcome_label,
      cutoff                = cutoff,
      cutoff_label          = label,
      best_config_id        = cid,
      best_climate_var      = cv,
      n_orgs                = fit$n_orgs %||% NA_integer_,
      n_obs                 = fit$n_obs  %||% NA_integer_,
      pct_zero_outcome      = fit$pct_zero_outcome %||% NA_real_,
      total_outcome         = fit$total_outcome    %||% NA_real_,
      climate_estimate      = fit$climate_estimate %||% NA_real_,
      climate_se            = fit$climate_se       %||% NA_real_,
      climate_pval          = fit$climate_pval     %||% NA_real_,
      climate_ci_low        = fit$climate_ci_low   %||% NA_real_,
      climate_ci_high       = fit$climate_ci_high  %||% NA_real_,
      status                = fit$status           %||% NA_character_
    )
    cat(sprintf("ok (n_orgs=%d, beta=%+.3f)\n",
                fit$n_orgs %||% NA, fit$climate_estimate %||% NA))
  }
}

sensitivity_table <- bind_rows(sensitivity_rows)

if (nrow(sensitivity_table) == 0) {
  cat("  No sensitivity rows produced — skipping output.\n")
} else {
  readr::write_csv(sensitivity_table,
                   "report_figures_manuscript/sensitivity_min_reports_table.csv")

  cat("\n=== Summary ===\n")
  print(sensitivity_table %>%
          arrange(industry, outcome, cutoff) %>%
          select(industry_label, outcome_label, cutoff, cutoff_label,
                  n_orgs, n_obs,
                  climate_estimate, climate_se, climate_pval))

  # Forest-style comparison plot. Each (industry × outcome) is a row with
  # two points: baseline (filled) and alternate (open). Tight visual cue
  # for whether the climate effect direction and magnitude survive the
  # cutoff change.
  d_plot <- sensitivity_table %>%
    mutate(.cell = sprintf("%s — %s",
                            .short_industry_label(industry_label),
                            outcome_label),
           .cell = forcats::fct_inorder(.cell),
           .label = sprintf("min = %d (%s)", cutoff, cutoff_label))

  # Reusable paired-forest builder for sensitivity comparisons. `group_col`
  # is the column distinguishing the two variants (e.g., cutoff_label or
  # variant). Produces a log-scale or rate-ratio plot; the two are saved as
  # separate files by the caller.
  build_sensitivity_forest <- function(d, group_col, scale,
                                        color_vals, color_labs,
                                        title, subtitle) {
    if (scale == "rate_ratio") {
      d <- d %>% mutate(.est = exp(climate_estimate),
                        .lo  = exp(climate_ci_low),
                        .hi  = exp(climate_ci_high))
      ref_line <- 1
      x_lab <- "Rate ratio per unit climate (95% CI)"
    } else {
      d <- d %>% mutate(.est = climate_estimate,
                        .lo  = climate_ci_low,
                        .hi  = climate_ci_high)
      ref_line <- 0
      x_lab <- "Climate coefficient (log rate-ratio per unit climate, 95% CI)"
    }
    p <- ggplot(d, aes(x = .est, y = .cell,
                        color = .data[[group_col]], shape = .data[[group_col]])) +
      geom_vline(xintercept = ref_line, linetype = "dashed", color = "gray60") +
      geom_pointrange(aes(xmin = .lo, xmax = .hi),
                      position = position_dodge(width = 0.5), size = 0.5) +
      scale_color_manual(values = color_vals, labels = color_labs, name = NULL) +
      scale_shape_manual(values = setNames(c(16, 1), names(color_vals)),
                          labels = color_labs, name = NULL) +
      labs(x = x_lab, y = NULL, title = title, subtitle = subtitle) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            plot.subtitle = element_text(color = "gray40"),
            legend.position = "bottom")
    if (scale == "rate_ratio") p <- p + scale_x_log10(labels = scales::label_number(drop0trailing = TRUE))
    p
  }

  cutoff_cols <- c(baseline = "#2c7bb6", alternate = "#d6604d")
  cutoff_labs <- c(baseline = "Paper cutoff", alternate = "Alternate cutoff")
  cutoff_sub <- paste0("NRC tested at min = 75 (paper) vs 50; aviation tested at min = 30 (paper) vs 50.\n",
                       "Bars are 95% CIs. Overlapping CIs indicate the climate signal is robust to the cutoff choice.")

  ggsave("report_figures_manuscript/sensitivity_min_reports_forest_log.pdf",
         build_sensitivity_forest(d_plot, "cutoff_label", "log",
           cutoff_cols, cutoff_labs,
           "Cutoff sensitivity: climate coefficient (log scale)", cutoff_sub),
         width = 9, height = 5)
  ggsave("report_figures_manuscript/sensitivity_min_reports_forest_rateratio.pdf",
         build_sensitivity_forest(d_plot, "cutoff_label", "rate_ratio",
           cutoff_cols, cutoff_labs,
           "Cutoff sensitivity: rate ratio per unit climate", cutoff_sub),
         width = 9, height = 5)

  cat("\nCutoff-sensitivity output:\n")
  cat("  report_figures_manuscript/sensitivity_min_reports_table.csv\n")
  cat("  report_figures_manuscript/sensitivity_min_reports_forest_{log,rateratio}.pdf\n")
}


# =============================================================================
# 9. ZERO-AS-MISSING SENSITIVITY
# =============================================================================
#
# The dynclim per-report composite climate score is exactly zero for a
# nontrivial fraction of reports per industry (rail ~54%, NRC ~27%, aviation
# ~27% under the champion specifications). Diagnostic inspection (see
# 8b_zero_score_diagnostic.R) reveals two distinct mechanisms:
#
#   - In rail and NRC: the sentiment model returns 0 on procedural /
#     engineering text. Semantic similarity is computed normally; sentiment
#     fails. The power_attention composite propagates this to a zero overall
#     score.
#   - In aviation: the semantic-similarity step rejects all segments above
#     the thresholding cutoff. Narrative content does not align with the
#     cross-industry safety-climate scale items.
#
# Crucially, build_climate_panel() in common/panel_data_prep.R filters only
# on is.na(.val) — NOT on zero values. Reports with an exact-zero composite
# are passed through to the SMA/EWMA windowing as literal zeros, which pulls
# windowed climate scores toward zero in proportion to each organization's
# share of zero-scored reports. Whether this is the right treatment is a
# substantive modeling choice: zeros could be treated as "neutral
# observations" (the current default) or as "no usable signal" (i.e., NA).
#
# This robustness check refits the champion model per main-text cell after
# replacing exact-zero report scores with NA before windowing, and compares
# the climate coefficient to the paper baseline.
#
# Outputs:
#   report_figures_manuscript/sensitivity_zero_as_na_table.csv
#   report_figures_manuscript/sensitivity_zero_as_na_forest.pdf

cat("\n=== Building zero-as-NA robustness check ===\n")

# Cells to test (same nine main-text outcomes as the cutoff sensitivity).
zero_na_cells <- tribble(
  ~industry,  ~outcome,             ~paper_cutoff,
  "rail",     "rate_accidents",                  50L,
  "rail",     "rate_injuries",                   50L,
  "rail",     "rate_fatalities",                 50L,
  "nrc",      "rate_lers",                       75L,
  "nrc",      "rate_emerg",                      75L,
  "nrc",      "rate_scrams",                     75L,
  "nrc",      "rate_pct_power_loss",             75L,
  "aviation", "rate_aids_all",                   30L,
  "aviation", "rate_accidents",                  30L,
  "msha",     "rate_accidents",                  30L,
  "msha",     "rate_injuries",                   30L,
  "msha",     "rate_days_lost",                  30L,
  "msha",     "rate_fatal",                      30L
)

# Helper: create a temporary modified parquet where overall_final_score = 0
# is rewritten as NA. The rest of the per-report scores remain. Returns the
# path to the new parquet so panel-prep can use it transparently.
write_zero_as_na_parquet <- function(cfg_dir, config_id, scratch_dir) {
  src <- file.path(cfg_dir, "_cfg", config_id, "results.parquet")
  if (!file.exists(src)) {
    stop(sprintf("Source parquet not found: %s", src))
  }
  dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(scratch_dir, sprintf("%s_zerona.parquet", config_id))
  d <- arrow::read_parquet(src)
  d$overall_final_score <- ifelse(d$overall_final_score == 0,
                                  NA_real_, d$overall_final_score)
  arrow::write_parquet(d, out)
  out
}

scratch_dir <- "report_figures_manuscript/_scratch_zero_as_na_parquets"

zero_na_rows <- list()
for (i in seq_len(nrow(zero_na_cells))) {
  cell <- zero_na_cells[i, ]

  champ <- champion_table %>%
    filter(industry == cell$industry, outcome == cell$outcome)
  if (nrow(champ) != 1) {
    cat(sprintf("  [%s | %s] no unique champion — skipping\n",
                cell$industry, cell$outcome))
    next
  }
  cid <- as.character(champ$best_config_id)
  cv  <- champ$best_climate_var
  cfg_dir <- industries[[cell$industry]]$cfg_dir

  # Original parquet path (zeros included as numerical zeros)
  paper_parquet <- file.path(cfg_dir, "_cfg", cid, "results.parquet")

  # Modified parquet (zeros rewritten as NA)
  na_parquet <- tryCatch(
    write_zero_as_na_parquet(cfg_dir, cid, scratch_dir),
    error = function(e) { cat("WRITE ERR:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(na_parquet)) next

  for (variant in c("baseline", "zero_as_na")) {
    cat(sprintf("  [%s | %s] %-12s ... ",
                cell$industry, cell$outcome, variant))

    pp <- if (variant == "baseline") paper_parquet else na_parquet

    panel <- tryCatch(
      build_panel_at_cutoff(cell$industry, pp, cid, cell$paper_cutoff),
      error = function(e) { cat("PANEL ERR:", conditionMessage(e), "\n"); NULL }
    )
    if (is.null(panel)) next

    fit <- tryCatch(
      fit_champion_at_panel(cell$industry, cell$outcome, panel, cv, cid),
      error = function(e) { cat("FIT ERR:", conditionMessage(e), "\n"); NULL }
    )
    if (is.null(fit)) next

    zero_na_rows[[length(zero_na_rows) + 1L]] <- tibble(
      industry              = cell$industry,
      industry_label        = champ$industry_label,
      outcome               = cell$outcome,
      outcome_label         = champ$outcome_label,
      variant               = variant,
      best_config_id        = cid,
      best_climate_var      = cv,
      n_orgs                = fit$n_orgs %||% NA_integer_,
      n_obs                 = fit$n_obs  %||% NA_integer_,
      pct_zero_outcome      = fit$pct_zero_outcome %||% NA_real_,
      total_outcome         = fit$total_outcome    %||% NA_real_,
      climate_estimate      = fit$climate_estimate %||% NA_real_,
      climate_se            = fit$climate_se       %||% NA_real_,
      climate_pval          = fit$climate_pval     %||% NA_real_,
      climate_ci_low        = fit$climate_ci_low   %||% NA_real_,
      climate_ci_high       = fit$climate_ci_high  %||% NA_real_,
      status                = fit$status           %||% NA_character_
    )
    cat(sprintf("ok (n_orgs=%d, beta=%+.3f)\n",
                fit$n_orgs %||% NA, fit$climate_estimate %||% NA))
  }
}

zero_na_table <- bind_rows(zero_na_rows)

if (nrow(zero_na_table) == 0) {
  cat("  No zero-as-NA rows produced — skipping output.\n")
} else {
  readr::write_csv(zero_na_table,
                   "report_figures_manuscript/sensitivity_zero_as_na_table.csv")

  cat("\n=== Summary: climate coefficient under baseline vs zero-as-NA ===\n")
  print(zero_na_table %>%
          arrange(industry, outcome, variant) %>%
          select(industry_label, outcome_label, variant,
                  n_orgs, n_obs,
                  climate_estimate, climate_se, climate_pval))

  # Forest-style comparison plot (paired baseline vs zero-as-NA).
  d_plot <- zero_na_table %>%
    mutate(.cell = sprintf("%s — %s",
                            .short_industry_label(industry_label),
                            outcome_label),
           .cell = forcats::fct_inorder(.cell))

  zna_cols <- c(baseline = "#2c7bb6", zero_as_na = "#d6604d")
  zna_labs <- c(baseline = "Paper (zeros included)", zero_as_na = "Zeros set to NA")
  zna_sub <- paste0("Paper baseline treats exact-zero report scores as numerical zeros in temporal windowing; ",
                    "alternative treats them as missing (NA), excluding them from SMA/EWMA aggregation.\n",
                    "Bars are 95% CIs.")

  ggsave("report_figures_manuscript/sensitivity_zero_as_na_forest_log.pdf",
         build_sensitivity_forest(d_plot, "variant", "log",
           zna_cols, zna_labs,
           "Zero-as-NA robustness: climate coefficient (log scale)", zna_sub),
         width = 9, height = 5)
  ggsave("report_figures_manuscript/sensitivity_zero_as_na_forest_rateratio.pdf",
         build_sensitivity_forest(d_plot, "variant", "rate_ratio",
           zna_cols, zna_labs,
           "Zero-as-NA robustness: rate ratio per unit climate", zna_sub),
         width = 9, height = 5)

  cat("\nZero-as-NA sensitivity output:\n")
  cat("  report_figures_manuscript/sensitivity_zero_as_na_table.csv\n")
  cat("  report_figures_manuscript/sensitivity_zero_as_na_forest_{log,rateratio}.pdf\n")
}

# Clean up scratch parquets after the run
if (dir.exists(scratch_dir)) {
  unlink(scratch_dir, recursive = TRUE)
}
