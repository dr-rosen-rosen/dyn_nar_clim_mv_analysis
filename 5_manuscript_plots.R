############ Manuscript figures (simplified, three-industry version)
# Drops PHMSA; leads with champion-spec results from time-series CV.
# Produces:
#   - report_figures_manuscript/figure1_forest.pdf
#   - report_figures_manuscript/figure2_partial_dependence.pdf
#   - report_figures_manuscript/champion_summary.csv
#   - report_figures_manuscript/champion_annotation.csv

PLOT_BASE_SIZE <- 12

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(patchwork)
  library(ggrepel)
  library(glmmTMB)
  library(broom.mixed)
  library(slider)
  library(lubridate)
  library(stringr)
  library(readxl)
  library(here)
})

# Main-text exclusion criterion is IDENTIFICATION STABILITY, not rarity per se.
# A rare outcome is excluded only when its multiverse shows quasi-separation /
# flat-likelihood pathology: a large share of specifications with |beta|>5 and a
# wildly dispersed coefficient band. The aviation cells below qualify (46-56% of
# specs have |beta|>5; beta spans roughly +/-20-40; median SE 5-9). MSHA coal
# fatalities, despite >98% zero cells, is well-identified (only ~5% of specs with
# |beta|>5, beta band entirely negative, SE ~1.7 — comparable to rail
# fatalities, which is a main-text cell) and is therefore retained in the main
# text as a harm-side outcome. See Methods.
# MAIN-TEXT OUTCOME SET = the detectability/consequence ladders (mining +
# aviation) plus the established rail/NRC cells. Everything else present in the
# result parquets (coarse mining cells, granular aviation tiers, monthly
# aviation) is an Extended-Data ROBUSTNESS ANALYSIS, not a main-text outcome.
#   Mining (detectability): t0 (no-injury) -> t1/t2/t3 -> days_lost
#   Aviation (quarterly):   noharm -> propdamage -> serious+fatal -> fatal
#   Rail:  accidents, injuries, fatalities
#   NRC:   lers, emerg, scrams, pct_power_loss   (external findings dropped)
MAIN_CELLS <- c(
  "rail|rate_accidents","rail|rate_injuries","rail|rate_fatalities",
  "nrc|rate_lers","nrc|rate_emerg","nrc|rate_scrams","nrc|rate_pct_power_loss",
  "aviation|rate_aids_noharm","aviation|rate_aids_propdamage",
  "aviation|rate_ntsb_serious_fatal",
  "msha|rate_t0","msha|rate_t1","msha|rate_t2","msha|rate_t3","msha|rate_days_lost"
)
# Aviation raw fatalities (rate_ntsb_fatal) dropped from main: subsumed by
# serious+fatal (same effect, worse-powered given sparsity). -> ED robustness.
RARE_EVENT_CELLS <- character(0)  # legacy hook; rare cells handled per-arm now
# EXCLUDE_FROM_MAIN is derived from the champion table (complement of MAIN_CELLS)
# just after it is built below.

source("common/postprocessing.R")
source("common/configuration_concordance.R")
source("common/manuscript_figures.R")

# Per-industry modules (panel data prep + fit_models)
source("rail/config.R");  source("common/data_prep.R")
source("common/panel_data_prep.R")
source("rail/data_prep.R"); source("rail/panel_data_prep.R"); source("rail/panel_fit_models.R")
source("nrc/config.R");  source("nrc/panel_data_prep.R"); source("nrc/panel_fit_models.R")
source("aviation/config.R"); source("aviation/data_prep.R")
source("aviation/panel_data_prep.R"); source("aviation/panel_fit_models.R")
# MSHA (coal): source ONLY the panel-prep + fit modules, NOT msha/config.R.
# msha/config.R would clobber the shared globals MODEL_FORMULAS /
# MIN_REPORTS_DEFAULT that the rail/nrc/aviation refit closures read at loop
# time. MSHA's fit path is self-contained (build_msha_panel_formula +
# PANEL_OUTCOME_VARS_MSHA), WINDOW_SPECS is identical across arms, and the
# MSHA panel closure passes min_reports = 30L explicitly below.
source("msha/panel_data_prep.R"); source("msha/panel_fit_models.R")


# =============================================================================
# 1. INDUSTRY CONFIGS (three industries; identical to 4_plots_panel.R minus PHMSA)
# =============================================================================

industries <- list(
  rail = list(
    label           = "Rail (FRA)",
    mv_results      = "results_new_new/rail/panel_mv_results.parquet",
    cv_results      = "results_new_new/rail/panel_cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026",
                                 "config_registry.csv"),
    cfg_dir         = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026",
    ops_vars        = c("train_miles", "passenger_miles", "staff_hours")
  ),
  nrc = list(
    label           = "Nuclear (NRC)",
    mv_results      = "results_new_new/nrc/panel_mv_results.parquet",
    cv_results      = "results_new_new/nrc/panel_cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026",
                                 "config_registry.csv"),
    cfg_dir         = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026",
    ops_vars        = c("power_std_mean")
  ),
  aviation = list(
    label           = "Aviation (NTSB / AIDS / ASRS)",
    mv_results      = "results_new_new/aviation/panel_mv_results_quarterly.parquet",
    cv_results      = "results_new_new/aviation/panel_cv_results_quarterly.parquet",
    config_registry = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026/config_registry.csv",
    cfg_dir         = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026",
    ops_vars        = c("seats", "passengers")
  ),
  msha = list(
    label           = "Mining (MSHA coal)",
    mv_results      = "results_new_new/msha/panel_mv_results_coal_detectability_full.parquet",
    cv_results      = "results_new_new/msha/panel_cv_results_coal_detectability_full.parquet",
    config_registry = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026/config_registry.csv",
    cfg_dir         = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026",
    ops_vars        = c("hours")
  )
)


# =============================================================================
# 2. CHAMPION SUMMARY TABLE
# =============================================================================

cat("\n=== Building champion summary ===\n")
champion_table <- build_champion_manuscript_table(
  industries     = industries,
  report_dir     = "report_figures_panel",
  cv_strategies  = c("timeseries", "group_kfold")
)

dir.create("report_figures_manuscript", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(champion_table,
                 "report_figures_manuscript/champion_summary.csv")

cat(sprintf("  Loaded %d champion rows across %d industries\n",
            nrow(champion_table), n_distinct(champion_table$industry)))

# Derive main-text exclusions = every (industry|outcome) cell present in the
# champion table that is NOT in MAIN_CELLS. Keeps main figures locked to the
# detectability/consequence ladders regardless of what extra outcomes the
# parquets carry.
.ct_oc <- if ("outcome" %in% names(champion_table)) champion_table$outcome else champion_table$panel_outcome
.all_cells <- unique(paste(champion_table$industry, .ct_oc, sep = "|"))
EXCLUDE_FROM_MAIN <- setdiff(.all_cells, MAIN_CELLS)
cat(sprintf("  Main cells: %d; excluded-from-main (robustness/ED): %d\n",
            length(intersect(.all_cells, MAIN_CELLS)), length(EXCLUDE_FROM_MAIN)))


# =============================================================================
# 3. FIGURE 1 — FOREST PLOT (timeseries primary)
# =============================================================================

cat("\n=== Building Figure 1: forest plot ===\n")

# MAIN figure: timeseries CV, well-powered cells only.
# Drops rare-event aviation cells AND aviation|rate_aids_incidents_only
# (redundant with rate_aids_all per team review).
forest_ts_main <- plot_champion_forest(champion_table,
                                        cv_strategy   = "timeseries",
                                        order_by      = "industry",
                                        exclude_cells = EXCLUDE_FROM_MAIN) +
  theme(plot.subtitle = element_text(color = "gray40"))

# APPENDIX variant: same plot WITH the rare-event cells, for transparency
forest_ts_full <- plot_champion_forest(champion_table,
                                        cv_strategy   = "timeseries",
                                        order_by      = "industry") +
  theme(plot.subtitle = element_text(color = "gray40")) +
  labs(subtitle = paste0("Including rare-event outcomes (aviation rate_fatalities, ",
                          "rate_inj_serious_fatal) — large CIs reflect <50 non-zero ",
                          "panel observations"))

# Companion tables — log-scale and rate-ratio versions written to SEPARATE
# files (per manuscript-team preference: do not mix scales in one file).
annotation_ts <- build_champion_annotation_table(
  champion_table, cv_strategy = "timeseries", scale = "log"
)
readr::write_csv(annotation_ts,
                 "report_figures_manuscript/bestmodel_annotation_timeseries_log.csv")
annotation_ts_rr <- build_champion_annotation_table(
  champion_table, cv_strategy = "timeseries", scale = "rate_ratio"
)
readr::write_csv(annotation_ts_rr,
                 "report_figures_manuscript/bestmodel_annotation_timeseries_rateratio.csv")

annotation_gk <- build_champion_annotation_table(
  champion_table, cv_strategy = "group_kfold", scale = "log"
)
readr::write_csv(annotation_gk,
                 "report_figures_manuscript/bestmodel_annotation_group_kfold_log.csv")
annotation_gk_rr <- build_champion_annotation_table(
  champion_table, cv_strategy = "group_kfold", scale = "rate_ratio"
)
readr::write_csv(annotation_gk_rr,
                 "report_figures_manuscript/bestmodel_annotation_group_kfold_rateratio.csv")

# NOTE: the main-text Figure 1A is written later (figure1a_forest_{log,
# rateratio}.pdf). Here we write only the APPENDIX forest variants, each in
# log and rate-ratio versions (separate files).

# Appendix: all outcomes incl. rare-event cells (log + rate ratio)
ggsave("report_figures_manuscript/appendix_forest_timeseries_all_outcomes_log.pdf",
       forest_ts_full, width = 9, height = 5.5)
forest_ts_full_rr <- plot_champion_forest(champion_table,
                                           cv_strategy   = "timeseries",
                                           order_by      = "industry",
                                           scale         = "rate_ratio") +
  theme(plot.subtitle = element_text(color = "gray40")) +
  labs(subtitle = paste0("Including rare-event outcomes (aviation rate_fatalities, ",
                          "rate_inj_serious_fatal) — wide CIs reflect <50 non-zero ",
                          "panel observations"))
ggsave("report_figures_manuscript/appendix_forest_timeseries_all_outcomes_rateratio.pdf",
       forest_ts_full_rr, width = 9, height = 5.5)

# Appendix: group-kfold CV (log + rate ratio)
forest_gk <- plot_champion_forest(champion_table, cv_strategy = "group_kfold",
                                   order_by = "industry", scale = "log")
ggsave("report_figures_manuscript/appendix_forest_group_kfold_log.pdf",
       forest_gk, width = 9, height = 5.5)
forest_gk_rr <- plot_champion_forest(champion_table, cv_strategy = "group_kfold",
                                      order_by = "industry", scale = "rate_ratio")
ggsave("report_figures_manuscript/appendix_forest_group_kfold_rateratio.pdf",
       forest_gk_rr, width = 9, height = 5.5)

cat("  Saved Figure 1 appendix forest variants (log + rate ratio)\n")


# =============================================================================
# 3b. PANEL DIAGNOSTIC FIGURE (zero-inflation × sample size × precision)
# =============================================================================
cat("\n=== Building panel diagnostic figure ===\n")

mv_paths <- setNames(
  sapply(industries, function(x) x$mv_results),
  names(industries)
)
diag_table <- build_panel_diagnostic_table(mv_paths)
readr::write_csv(diag_table,
                 "report_figures_manuscript/panel_diagnostic_table.csv")

diag_plot <- plot_panel_diagnostic(
  champion_table  = champion_table,
  diag_table      = diag_table,
  cv_strategy     = "timeseries",
  rare_cells      = RARE_EVENT_CELLS
)
ggsave("report_figures_manuscript/figure_diagnostic_panel.pdf",
       diag_plot, width = 12, height = 6)

cat("  Saved figure_diagnostic_panel.pdf + panel_diagnostic_table.csv\n")


# =============================================================================
# 4. CHAMPION REFIT + PARTIAL-DEPENDENCE DATA
# =============================================================================
#
# For each (industry × outcome) cell from the timeseries champions, re-fit
# the champion panel-rate model on the full panel data and compute the
# partial-dependence curve. This requires re-loading each industry's data
# sources (events / ops) and calling the same panel_data_fn closure the
# multiverse driver used.
#
# We treat NRC's rate_pct_power_loss as Tweedie; everything else NB.

cat("\n=== Loading per-industry data for champion refits ===\n")

# --- Rail ---
RAIL_EVENTS <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet"
RAIL_OPS    <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"
rail_events <- read_parquet(RAIL_EVENTS) %>%
  rename(total_persons_killed  = `Total Persons Killed`,
         total_persons_injured = `Total Persons Injured`,
         total_damage_cost     = `Total Damage Cost`) %>%
  mutate(total_damage_cost = as.numeric(gsub("[^0-9.]", "", total_damage_cost))) %>%
  select(org_id, eid, event_date,
         total_persons_killed, total_persons_injured, total_damage_cost) %>%
  mutate(event_date = as.Date(event_date))
rail_ops_raw <- readr::read_csv(RAIL_OPS, show_col_types = FALSE) %>%
  mutate(across(all_of(OPS_VARS), ~ ifelse(.x < 0, NA_real_, .x))) %>%
  mutate(yearmonth = as.Date(yearmonth))
rail_ops_features <- make_ops_features_rolling(rail_ops_raw) %>%
  distinct(org_id, yearmonth, .keep_all = TRUE)

rail_panel_data_fn <- function(parquet_path, config_id) {
  prepare_rail_panel_data(
    parquet_path  = parquet_path,
    config_id     = config_id,
    events_df     = rail_events,
    ops_raw       = rail_ops_raw,
    ops_features  = rail_ops_features,
    min_reports   = 50L,
    max_holdover_days = 365L,
    climate_base  = "overall_final_score"
  )
}

# --- NRC ---
NRC_EVENTS <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet"
NRC_OPS    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet"
nrc_events <- read_parquet(NRC_EVENTS) %>%
  mutate(event_date = as.Date(event_date), eid = as.character(event_num))
nrc_ops_raw <- read_parquet(NRC_OPS) %>%
  mutate(quarter_start = as.Date(quarter_start))

nrc_panel_data_fn <- function(parquet_path, config_id) {
  prepare_nrc_panel_data(
    parquet_path  = parquet_path,
    config_id     = config_id,
    events_df     = nrc_events,
    ops_raw       = nrc_ops_raw,
    min_reports   = MIN_REPORTS_DEFAULT,
    max_holdover_days = 365L,
    climate_base  = "overall_final_score"
  )
}

# --- Aviation ---
AV_ASRS  <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet"
AV_AIDS  <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/aids_events.parquet"
AV_NTSB_POST <- "data/aviation/ntsb_av_accident_data/events.xlsx"
AV_NTSB_PRE  <- "data/aviation/ntsb_av_accident_data/events_pre2008.xlsx"
AV_OPS   <- "data/aviation/bts_t100/airport_month_ops.parquet"
asrs_meta    <- load_asrs_meta_panel(AV_ASRS)
av_ops_features <- read_parquet(AV_OPS)
# Aviation primary grain is now airport-QUARTER (NRC-consistent; denser).
ntsb_panel   <- load_ntsb_panel_rate(AV_NTSB_POST, AV_NTSB_PRE, apt_dist_nm = 5L,
                                     period = "quarter")
aids_panel   <- load_aids_panel_rate(AV_AIDS, apt_dist_nm = 5L, min_year = 1988L,
                                     period = "quarter")

aviation_panel_data_fn <- function(parquet_path, config_id) {
  prepare_aviation_panel_data(
    parquet_path  = parquet_path,
    config_id     = config_id,
    asrs_meta_df  = asrs_meta,
    ntsb_panel_df = ntsb_panel,
    ops_features  = av_ops_features,
    aids_panel_df = aids_panel,
    atc_scope     = "local_terminal",
    missing_climate = "exclude",
    min_reports   = MIN_REPORTS_DEFAULT,
    period        = "quarter",
    max_holdover_days = 365L,
    climate_base  = "overall_final_score"
  )
}

# --- MSHA (coal) ---
MSHA_EVENTS <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/events.parquet"
MSHA_OPS    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/ops_mine_quarter.parquet"
MSHA_COV    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/covariates_mine_quarter.parquet"
msha_events <- read_parquet(MSHA_EVENTS) %>%
  mutate(mine_id    = as.character(mine_id),
         eid        = as.character(eid),
         event_date = as.Date(accident_date),
         commodity  = tolower(trimws(commodity))) %>%
  filter(!is.na(mine_id), !is.na(event_date), commodity == "coal")
msha_ops <- read_parquet(MSHA_OPS) %>%
  mutate(mine_id = as.character(mine_id),
         commodity = tolower(trimws(commodity))) %>%
  filter(commodity == "coal")
msha_cov <- read_parquet(MSHA_COV) %>%
  mutate(mine_id = as.character(mine_id))

msha_panel_data_fn <- function(parquet_path, config_id) {
  prepare_msha_panel_data(
    parquet_path  = parquet_path,
    config_id     = config_id,
    events_df     = msha_events,
    ops_raw       = msha_ops,
    covariates_df = msha_cov,
    min_reports   = 30L,             # MSHA threshold (passed explicitly; see source note)
    max_holdover_days = 365L,
    climate_base  = "overall_final_score"
  )
}


# =============================================================================
# 5. REFIT CHAMPIONS — TIMESERIES CV STRATEGY
# =============================================================================

cat("\n=== Refitting champion specs for partial-dependence ===\n")

# Outcome → fit_fn + offset / bw_terms / org_var / family per industry
spec_map <- list(
  rail = list(
    fit_fn  = function(panel, cv, cid, outcome)
                fit_single_rail_panel_model(panel, cv, cid, outcome = outcome),
    var     = list(rate_accidents = "n_accidents", rate_injuries = "sum_injured",
                   rate_fatalities = "sum_killed"),
    offset  = list(rate_accidents   = "train_miles",
                   rate_injuries    = "staff_hours",
                   rate_fatalities  = "train_miles"),
    bw      = list(rate_accidents   = c("passenger_miles_between","passenger_miles_within",
                                          "staff_hours_between","staff_hours_within"),
                   rate_injuries    = c("train_miles_between","train_miles_within",
                                          "passenger_miles_between","passenger_miles_within"),
                   rate_fatalities  = c("passenger_miles_between","passenger_miles_within",
                                          "staff_hours_between","staff_hours_within")),
    org     = "org_id",
    family  = list(rate_accidents = "nbinom2", rate_injuries = "nbinom2",
                   rate_fatalities = "nbinom2")
  ),
  nrc = list(
    fit_fn  = function(panel, cv, cid, outcome)
                fit_single_nrc_panel_model(panel, cv, cid, outcome = outcome),
    var     = list(rate_lers = "n_lers", rate_emerg = "n_emerg",
                   rate_scrams = "n_scrams",
                   rate_pct_power_loss = "sum_pct_power_loss"),
    offset  = list(rate_lers = "days_at_power_total", rate_emerg = "days_at_power_total",
                   rate_scrams = "days_at_power_total",
                   rate_pct_power_loss = "days_at_power_total"),
    bw      = list(rate_lers = c("power_std_mean_between","power_std_mean_within"),
                   rate_emerg = c("power_std_mean_between","power_std_mean_within"),
                   rate_scrams = c("power_std_mean_between","power_std_mean_within"),
                   rate_pct_power_loss = c("power_std_mean_between","power_std_mean_within")),
    org     = "facility_site",
    family  = list(rate_lers = "nbinom2", rate_emerg = "nbinom2",
                   rate_scrams = "nbinom2",
                   rate_pct_power_loss = "tweedie")
  ),
  aviation = list(
    fit_fn  = function(panel, cv, cid, outcome)
                fit_single_aviation_panel_model(panel, cv, cid, outcome = outcome),
    # MAIN: noharm/propdamage (AIDS damage axis) + serious_fatal/fatal (NTSB
    # casualty axis). Remaining cells refit for ED robustness.
    var     = list(rate_aids_noharm = "n_aids_noharm",
                   rate_aids_propdamage = "n_aids_propdamage",
                   rate_ntsb_serious_fatal = "n_ntsb_serious_fatal",
                   rate_ntsb_fatal = "n_ntsb_fatal",
                   rate_ntsb_serious = "n_ntsb_serious", rate_ntsb_minor = "n_ntsb_minor",
                   rate_aids_injury = "n_aids_injury", rate_aids_harm = "n_aids_harm",
                   rate_aids_all = "n_aids_all", rate_accidents = "n_accidents",
                   rate_inj_serious_fatal = "sum_serious_fatal",
                   rate_fatalities = "sum_fatalities"),
    offset  = setNames(rep(list("departures"), 12),
                       c("rate_aids_noharm","rate_aids_propdamage","rate_ntsb_serious_fatal",
                         "rate_ntsb_fatal","rate_ntsb_serious","rate_ntsb_minor",
                         "rate_aids_injury","rate_aids_harm","rate_aids_all","rate_accidents",
                         "rate_inj_serious_fatal","rate_fatalities")),
    bw      = setNames(rep(list(c("seats_between","seats_within",
                                  "passengers_between","passengers_within")), 12),
                       c("rate_aids_noharm","rate_aids_propdamage","rate_ntsb_serious_fatal",
                         "rate_ntsb_fatal","rate_ntsb_serious","rate_ntsb_minor",
                         "rate_aids_injury","rate_aids_harm","rate_aids_all","rate_accidents",
                         "rate_inj_serious_fatal","rate_fatalities")),
    org     = "airport_id",
    family  = setNames(rep(list("nbinom2"), 12),
                       c("rate_aids_noharm","rate_aids_propdamage","rate_ntsb_serious_fatal",
                         "rate_ntsb_fatal","rate_ntsb_serious","rate_ntsb_minor",
                         "rate_aids_injury","rate_aids_harm","rate_aids_all","rate_accidents",
                         "rate_inj_serious_fatal","rate_fatalities"))
  ),
  msha = list(
    fit_fn  = function(panel, cv, cid, outcome)
                fit_single_msha_panel_model(panel, cv, cid, outcome = outcome),
    var     = list(rate_t0 = "n_t0", rate_t1 = "n_t1", rate_t2 = "n_t2", rate_t3 = "n_t3",
                   rate_days_lost = "sum_days_lost",
                   rate_accidents = "n_accidents", rate_injuries = "n_injuries",
                   rate_fatal = "n_fatal"),
    offset  = list(rate_t0 = "hours_worked", rate_t1 = "hours_worked",
                   rate_t2 = "hours_worked", rate_t3 = "hours_worked",
                   rate_days_lost = "hours_worked",
                   rate_accidents = "hours_worked", rate_injuries = "hours_worked",
                   rate_fatal = "hours_worked"),
    bw      = list(rate_t0 = c("hours_within"), rate_t1 = c("hours_within"),
                   rate_t2 = c("hours_within"), rate_t3 = c("hours_within"),
                   rate_days_lost = c("hours_within"),
                   rate_accidents = c("hours_within"), rate_injuries = c("hours_within"),
                   rate_fatal = c("hours_within")),
    org     = "mine_id",
    family  = list(rate_t0 = "nbinom2", rate_t1 = "nbinom2", rate_t2 = "nbinom2",
                   rate_t3 = "nbinom2", rate_days_lost = "tweedie",
                   rate_accidents = "nbinom2", rate_injuries = "nbinom2",
                   rate_fatal = "nbinom2")
  )
)

panel_fns <- list(rail = rail_panel_data_fn,
                  nrc  = nrc_panel_data_fn,
                  aviation = aviation_panel_data_fn,
                  msha = msha_panel_data_fn)

ts_champs <- champion_table %>% filter(cv_strategy == "timeseries")

pd_results <- list()
for (i in seq_len(nrow(ts_champs))) {
  row  <- ts_champs[i, ]
  ind  <- row$industry
  outc <- row$outcome
  cat(sprintf("  [%d/%d] %s — %s ... ", i, nrow(ts_champs), ind, outc))
  sm <- spec_map[[ind]]
  if (is.null(sm$offset[[outc]])) {
    cat("(no spec_map entry; skipping)\n"); next
  }
  res <- tryCatch(
    refit_champion_and_predict(
      industry_key      = ind,
      outcome           = outc,
      cv_strategy       = "timeseries",
      champion_table    = champion_table,
      panel_data_fn     = panel_fns[[ind]],
      cfg_dir           = industries[[ind]]$cfg_dir,
      fit_fn            = sm$fit_fn,
      outcome_offset    = sm$offset[[outc]],
      outcome_bw_terms  = sm$bw[[outc]],
      outcome_org_var   = sm$org,
      outcome_var       = sm$var[[outc]],
      outcome_family    = sm$family[[outc]]
    ),
    error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(res)) {
    pd_results[[paste(ind, outc, sep = "|")]] <- res
    cat("ok\n")
  }
}


# =============================================================================
# 6. FIGURE 1 COMPOSITE (forest plot + standardized-coefficient grid)
# =============================================================================

cat("\n=== Building Figure 1A (forest) + Figure 1B (coefficient grid) ===\n")
coefs_long <- extract_champion_standardized_coefs(pd_results, champion_table)
# Also carry the rate-ratio scale (exp of the per-SD standardized coef + CI) so
# the data file matches Figure 1A's rate-ratio axis without manual exponentiation.
coefs_long <- coefs_long %>%
  mutate(rr = exp(estimate_std), rr_ci_low = exp(ci_low_std), rr_ci_high = exp(ci_high_std)) %>%
  relocate(rr, rr_ci_low, rr_ci_high, .after = ci_high_std)
readr::write_csv(coefs_long,
                 "report_figures_manuscript/figure1_coefs_standardized.csv")

# Figure 1A (MAIN TEXT) — PER-SD forest. Effects expressed per +1 SD of
# climate (the realistic, interpretable scale). Log + rate-ratio as
# separate files. Built from the standardized coefficients.
ggsave("report_figures_manuscript/figure1a_forest_persd_rateratio.pdf",
       plot_bestmodel_forest_per_sd(coefs_long, scale = "rate_ratio",
                                    exclude_cells = EXCLUDE_FROM_MAIN),
       width = 9, height = 5)
ggsave("report_figures_manuscript/figure1a_forest_persd_log.pdf",
       plot_bestmodel_forest_per_sd(coefs_long, scale = "log",
                                    exclude_cells = EXCLUDE_FROM_MAIN),
       width = 9, height = 5)

# EXTENDED DATA — per-unit forest (exp of the raw coefficient). Retained for
# completeness; NOT main text because a 1.0-unit change in the climate score
# is far outside its observed range, making per-unit rate ratios extreme and
# hard to interpret.
ggsave("report_figures_manuscript/extended_data_forest_perunit_log.pdf",
       forest_ts_main, width = 9, height = 5)
forest_ts_main_rr <- plot_champion_forest(champion_table,
                                           cv_strategy   = "timeseries",
                                           order_by      = "industry",
                                           exclude_cells = EXCLUDE_FROM_MAIN,
                                           scale         = "rate_ratio") +
  theme(plot.subtitle = element_text(color = "gray40"))
ggsave("report_figures_manuscript/extended_data_forest_perunit_rateratio.pdf",
       forest_ts_main_rr, width = 9, height = 5)

# Figure 1B — per-cell standardized-coefficient grid. 3-column layout for
# 10 main-text cells works out to ~4 rows depending on exclusions; allow the
# height to scale with the number of facets. Log + rate-ratio as separate files.
n_cells_b <- coefs_long %>%
  filter(!paste(industry, outcome, sep = "|") %in% EXCLUDE_FROM_MAIN) %>%
  distinct(industry, outcome) %>% nrow()
n_rows_b <- ceiling(n_cells_b / 3)
b_height <- max(6, 2.8 * n_rows_b + 1.5)

coef_grid_log <- plot_champion_coef_grid(coefs_long,
                                          exclude_cells = EXCLUDE_FROM_MAIN,
                                          ncol = 3L, scale = "log")
ggsave("report_figures_manuscript/figure1b_coef_grid_log.pdf",
       coef_grid_log, width = 11, height = b_height)

coef_grid_rr <- plot_champion_coef_grid(coefs_long,
                                         exclude_cells = EXCLUDE_FROM_MAIN,
                                         ncol = 3L, scale = "rate_ratio")
ggsave("report_figures_manuscript/figure1b_coef_grid_rateratio.pdf",
       coef_grid_rr, width = 11, height = b_height)

cat(sprintf("  Saved figure1a_forest_persd_{log,rateratio}.pdf (main), extended_data_forest_perunit_*.pdf, figure1b_coef_grid_{log,rateratio}.pdf (%d cells, %d rows)\n",
            n_cells_b, n_rows_b))


# =============================================================================
# 7. FIGURE 2 — PARTIAL-DEPENDENCE GRID
# =============================================================================

cat("\n=== Building Figure 2: partial-dependence grid ===\n")
if (length(pd_results) > 0) {
  pd_plot <- plot_champion_partial_dependence(
    pd_results,
    free_y        = TRUE,
    x_scale       = "percentile",
    y_scale       = "rate_ratio",
    exclude_cells = EXCLUDE_FROM_MAIN
  )
  ggsave("report_figures_manuscript/figure2_partial_dependence.pdf",
         pd_plot, width = 11, height = 8)

  # Appendix variant with ALL cells (incl. rare-event)
  pd_plot_full <- plot_champion_partial_dependence(
    pd_results,
    free_y        = TRUE,
    x_scale       = "percentile",
    y_scale       = "rate_ratio"
  )
  ggsave("report_figures_manuscript/appendix_partial_dependence_all.pdf",
         pd_plot_full, width = 11, height = 9)

  # Raw prediction tibbles for downstream use
  pd_long <- bind_rows(lapply(names(pd_results), function(k) {
    parts <- str_split(k, "\\|")[[1]]
    pd_results[[k]]$pd_data %>%
      mutate(industry = parts[1], outcome = parts[2])
  }))
  arrow::write_parquet(pd_long,
                       "report_figures_manuscript/figure2_pd_data.parquet")
  cat("  Saved figure2_partial_dependence.pdf (main + appendix)\n")
} else {
  cat("  No PD results produced — Figure 2 skipped\n")
}


# =============================================================================
# 8. CV SPEC CURVES ON dLL — per industry (time-series + organization-blocked)
# =============================================================================
#
# One faceted Extended-Data figure per industry x CV strategy, restricted to the
# 15 main-text outcomes (MAIN_CELLS). Each panel shows the full multiverse of
# held-out dLL sorted ascending, with five colored decision strips beneath;
# legends are collected at the bottom and panels are ordered by the
# detectability/severity ladder. dll_table_ts is reused by the facet-importance
# section below, so both CV tables are still built and cached here.

cat("\n=== Building CV spec curves on dLL (per industry) ===\n")

dll_table_ts <- build_dll_multiverse_table(industries, cv_strategy = "timeseries")
arrow::write_parquet(dll_table_ts,
                     "report_figures_manuscript/figure3_dll_multiverse_ts.parquet")
dll_table_gk <- build_dll_multiverse_table(industries, cv_strategy = "group_kfold")
arrow::write_parquet(dll_table_gk,
                     "report_figures_manuscript/figure3_dll_multiverse_gk.parquet")

# Per-industry spec-curve writer: filters a multiverse table to that industry's
# main-text outcomes, orders panels by the detectability/severity ladder, and
# saves one faceted figure. Shared by the CV (dLL) and traditional (beta) curves.
SPECCURVE_IND_LABEL <- c(rail = "Rail", nrc = "Nuclear",
                         aviation = "Aviation", msha = "Mining")
SPECCURVE_NCOL      <- c(rail = 3, nrc = 2, aviation = 3, msha = 3)

save_speccurve_by_industry <- function(tbl, plot_fn, tag, lab) {
  present <- tbl %>% mutate(.k = paste(industry, outcome, sep = "|")) %>%
    pull(.k) %>% unique()
  for (ind in names(SPECCURVE_IND_LABEL)) {
    cells <- MAIN_CELLS[MAIN_CELLS %in% present &
                          startsWith(MAIN_CELLS, paste0(ind, "|"))]
    if (!length(cells)) { cat(sprintf("  skip %s/%s (no cells)\n", tag, ind)); next }
    cells <- cells[order(outcome_rank(sub("^[^|]+\\|", "", cells)))]
    nc <- min(SPECCURVE_NCOL[[ind]], length(cells)); nr <- ceiling(length(cells) / nc)
    p <- plot_fn(tbl, cells = cells, ncol = nc) +
      patchwork::plot_annotation(title = sprintf("%s — %s",
                                                 SPECCURVE_IND_LABEL[[ind]], lab))
    out <- sprintf("report_figures_manuscript/ED_speccurve_%s_%s.pdf", tag, ind)
    ggsave(out, p, width = 5 * nc, height = max(5.5, 5.5 * nr + 1.5), limitsize = FALSE)
    cat(sprintf("  saved %s (%d cells)\n", basename(out), length(cells)))
  }
}

save_speccurve_by_industry(dll_table_ts, plot_cv_spec_curve_manuscript,
                           "cv_timeseries", "Time-series CV (held-out dLL)")
save_speccurve_by_industry(dll_table_gk, plot_cv_spec_curve_manuscript,
                           "cv_groupkfold", "Organization-blocked CV (held-out dLL)")
cat("  Saved 8 per-industry CV spec-curve images (4 time-series + 4 org-blocked)\n")


# =============================================================================
# 9. FIGURE 4 — ANOVA-ON-Δll FACET IMPORTANCE (timeseries only)
# =============================================================================
#
# For each cell, partition the variance of Δll across (window, embedding,
# sentiment, composite, thresholding) plus a residual. Stacked-bar layout
# makes it immediate which design choices move the validation gain.

cat("\n=== Building Figure 4: Δll facet importance ===\n")

importance_all <- compute_dll_facet_importance(dll_table_ts) %>%
  filter(!paste(industry, outcome, sep = "|") %in% RARE_EVENT_CELLS)
readr::write_csv(importance_all,
                 "report_figures_manuscript/figure4_facet_importance_ts.csv")

# MAIN figure: only the locked-in outcomes (drops aviation aids_incidents_only
# and the exploratory NRC findings / action-matrix outcomes — those appear in
# the appendix variant below).
importance_main <- importance_all %>%
  filter(!paste(industry, outcome, sep = "|") %in% EXCLUDE_FROM_MAIN)

fig4 <- plot_dll_facet_importance(importance_main, exclude_residual = FALSE)
ggsave("report_figures_manuscript/figure4_facet_importance.pdf",
       fig4, width = 10, height = 6)

# Appendix variants:
#   - All non-rare cells (includes aids_incidents_only + new NRC outcomes)
#   - Main outcomes, rescaled to explained-variance-only (drops residual)
fig4_appendix_all <- plot_dll_facet_importance(importance_all,
                                                  exclude_residual = FALSE)
ggsave("report_figures_manuscript/appendix_facet_importance_all_outcomes.pdf",
       fig4_appendix_all, width = 10, height = 7)

fig4_explained <- plot_dll_facet_importance(importance_main,
                                             exclude_residual = TRUE)
ggsave("report_figures_manuscript/appendix_facet_importance_explained_only.pdf",
       fig4_explained, width = 10, height = 6)

cat("  Saved figure4_facet_importance.pdf + appendix variant\n")


# =============================================================================
# 10. TRADITIONAL MULTIVERSE SPEC CURVES (climate coefficient) — per industry
# =============================================================================
#
# Companion to the dLL-based CV spec curves: plots the climate coefficient
# estimate (with 95% CI ribbon) across the 810 specifications per cell, with
# the same five facet strips beneath each curve, reading from the multiverse MV
# results (panel_mv_results.parquet). One faceted figure per industry, restricted
# to the 15 main-text outcomes via the shared save_speccurve_by_industry() above.

cat("\n=== Building traditional spec curves (climate coefficient, per industry) ===\n")

beta_table <- build_beta_multiverse_table(industries)
arrow::write_parquet(beta_table,
                     "report_figures_manuscript/appendix_beta_multiverse.parquet")

save_speccurve_by_industry(beta_table, plot_traditional_spec_curve_manuscript,
                           "traditional_mv", "Traditional multiverse (climate coefficient)")
cat("  Saved 4 per-industry traditional spec-curve images\n")


cat("\n=== Manuscript figures complete ===\n")
cat("  Outputs in: report_figures_manuscript/\n")
