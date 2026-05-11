# =============================================================================
# rail/1_rail_panel_multiverse.R — Rail Panel-Rate Multiverse
# =============================================================================
#
# Sweeps every climate config and every windowed climate variable, fitting
# panel-rate models for accidents (n_accidents / train_miles) and injuries
# (sum_injured / staff_hours). Writes ONE row per (config × climate_var ×
# outcome) to OUTPUT_PATH.
#
# Run from project root: Rscript rail/1_rail_panel_multiverse.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(furrr)
  library(glmmTMB)
  library(broom.mixed)
  library(slider)
  library(lubridate)
})

# --- Source modules ---
source("rail/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("rail/data_prep.R")
source("rail/panel_data_prep.R")
source("rail/panel_fit_models.R")
source("common/panel_multiverse_runner.R")


# =============================================================================
# CONFIG
# =============================================================================

CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026"
EVENTS_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet"
OPS_PATH    <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"

OUTPUT_PATH    <- "results_new_new/rail/panel_mv_results.parquet"
CHECKPOINT_DIR <- "results_new_new/rail/panel_checkpoints"
N_WORKERS      <- 20L
PANEL_OUTCOMES <- c("rate_accidents", "rate_injuries", "rate_fatalities")


# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n=== Loading data ===\n")

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

cat(sprintf("  events: %d rows, %d orgs\n",
            nrow(rail_events), n_distinct(rail_events$org_id)))
cat(sprintf("  ops:    %d rows, %d orgs\n",
            nrow(ops_raw), n_distinct(ops_raw$org_id)))


# =============================================================================
# PROTOCOL: data_prep_fn + fit_fns
# =============================================================================

# Closure captures rail_events, ops_raw, ops_features.
panel_data_fn <- function(parquet_path, config_id) {
  prepare_rail_panel_data(
    parquet_path     = parquet_path,
    config_id        = config_id,
    events_df        = rail_events,
    ops_raw          = ops_raw,
    ops_features     = ops_features,
    min_reports      = 50L,
    max_holdover_days = 365L,
    climate_base     = "overall_final_score"
  )
}

fit_fns <- list(
  rate_accidents  = function(panel, cv, cid, outcome) {
    fit_single_rail_panel_model(panel, cv, cid, outcome = "rate_accidents")
  },
  rate_injuries   = function(panel, cv, cid, outcome) {
    fit_single_rail_panel_model(panel, cv, cid, outcome = "rate_injuries")
  },
  rate_fatalities = function(panel, cv, cid, outcome) {
    fit_single_rail_panel_model(panel, cv, cid, outcome = "rate_fatalities")
  }
)


# =============================================================================
# RUN
# =============================================================================

results <- run_panel_multiverse(
  cfg_dir       = CFG_DIR,
  panel_data_fn = panel_data_fn,
  fit_fns       = fit_fns,
  outcomes      = PANEL_OUTCOMES,
  config_ids    = NULL,            # discover all
  n_workers     = N_WORKERS,
  output_path   = OUTPUT_PATH,
  checkpoint_dir = CHECKPOINT_DIR,
  worker_source_files = c(
    "rail/config.R",
    "common/data_prep.R",
    "common/panel_data_prep.R",
    "rail/data_prep.R",
    "rail/panel_data_prep.R",
    "rail/panel_fit_models.R",
    "common/panel_multiverse_runner.R"
  ),
  worker_globals = list(
    rail_events  = rail_events,
    ops_raw      = ops_raw,
    ops_features = ops_features,
    panel_data_fn = panel_data_fn,
    fit_fns       = fit_fns
  )
)

cat("\nDone. Status table:\n")
print(table(results$status, useNA = "ifany"))
