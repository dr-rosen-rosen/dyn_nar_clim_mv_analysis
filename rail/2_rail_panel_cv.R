# =============================================================================
# rail/2_rail_panel_cv.R — Rail Panel-Rate Cross-Validation
# =============================================================================
#
# For every (config × climate_var × outcome), fits three nested tiers
# (intercept_only, seasonal_ops, climate) under group_kfold and timeseries
# CV strategies. Primary metric: held-out log-likelihood per observation.
# Secondary: pearson correlation predicted vs observed.
#
# Run: Rscript rail/2_rail_panel_cv.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(furrr)
  library(glmmTMB)
  library(slider)
  library(lubridate)
})

source("rail/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("rail/data_prep.R")
source("rail/panel_data_prep.R")
source("rail/panel_fit_models.R")
source("common/panel_cv_runner.R")


CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_2026-07-14"
EVENTS_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet"
OPS_PATH    <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"
OUTPUT_PATH <- "results_new_new/rail/panel_cv_results.parquet"
N_WORKERS   <- 20L


# --- Load data ---
rail_events <- arrow::read_parquet(EVENTS_PATH) %>%
  rename(total_persons_killed  = `Total Persons Killed`,
         total_persons_injured = `Total Persons Injured`,
         total_damage_cost     = `Total Damage Cost`) %>%
  mutate(total_damage_cost = as.numeric(gsub("[^0-9.]", "", total_damage_cost))) %>%
  select(org_id, eid, event_date,
         total_persons_killed, total_persons_injured, total_damage_cost) %>%
  mutate(event_date = as.Date(event_date))

ops_raw <- read_csv(OPS_PATH, show_col_types = FALSE) %>%
  mutate(across(all_of(OPS_VARS), ~ ifelse(.x < 0, NA_real_, .x))) %>%
  mutate(yearmonth = as.Date(yearmonth))

ops_features <- make_ops_features_rolling(ops_raw) %>%
  distinct(org_id, yearmonth, .keep_all = TRUE)


# --- Outcome specs (one per panel outcome) ---
panel_data_fn <- function(parquet_path, config_id) {
  prepare_rail_panel_data(parquet_path, config_id, rail_events,
                          ops_raw = ops_raw, ops_features = ops_features,
                          min_reports = 50L)
}

# bw_terms exclude the offset variable. Mirrors build_panel_formula().
.bw <- function(off) {
  rest <- setdiff(OPS_VARS, off)
  c(paste0(rest, "_between"), paste0(rest, "_within"))
}

outcome_specs <- list(
  rate_accidents = list(
    outcome_var = "n_accidents", exposure = "train_miles",
    family = "nbinom2", bw_terms = .bw("train_miles"),
    org_var = "org_id", period_date_var = "yearmonth"
  ),
  rate_injuries = list(
    outcome_var = "sum_injured", exposure = "staff_hours",
    family = "nbinom2", bw_terms = .bw("staff_hours"),
    org_var = "org_id", period_date_var = "yearmonth"
  ),
  rate_fatalities = list(
    outcome_var = "sum_killed", exposure = "train_miles",
    family = "nbinom2", bw_terms = .bw("train_miles"),
    org_var = "org_id", period_date_var = "yearmonth"
  )
)


# --- Run ---
results <- run_panel_cv(
  cfg_dir        = CFG_DIR,
  panel_data_fn  = panel_data_fn,
  outcome_specs  = outcome_specs,
  config_ids     = NULL,
  strategies     = c("group_kfold", "timeseries"),
  K = 5L, n_splits = 5L,
  test_duration_months = 24L, gap_months = 6L,
  n_workers = N_WORKERS,
  output_path = OUTPUT_PATH,
  worker_source_files = c(
    "rail/config.R",
    "common/data_prep.R",
    "common/panel_data_prep.R",
    "rail/data_prep.R",
    "rail/panel_data_prep.R",
    "rail/panel_fit_models.R",
    "common/panel_cv_runner.R"
  ),
  worker_globals = list(
    rail_events = rail_events,
    ops_raw     = ops_raw,
    ops_features = ops_features,
    panel_data_fn = panel_data_fn
  )
)

cat("\n=== Summary: delta-loglik (climate vs seasonal_ops) ===\n")
print(results %>%
        filter(model_label == "delta_climate_vs_seasonal") %>%
        group_by(panel_outcome, cv_strategy) %>%
        summarise(median_delta_ll = median(loglik_mean, na.rm = TRUE),
                  pct_positive = mean(loglik_mean > 0, na.rm = TRUE),
                  n = n(), .groups = "drop"))
