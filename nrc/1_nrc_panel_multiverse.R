# =============================================================================
# nrc/1_nrc_panel_multiverse.R — NRC Panel-Rate Multiverse
# =============================================================================
#
# Sweeps every climate config × climate window × panel outcome.
#
# Self-reported outcomes (derived from events.parquet):
#   rate_lers           : n_lers ~ . + offset(log(days_at_power_total)), nbinom2
#   rate_emerg          : n_emerg ~ . + offset(log(days_at_power_total)), nbinom2
#   rate_scrams         : n_scrams ~ . + offset(...), nbinom2
#   rate_pct_power_loss : sum_pct_power_loss ~ . + offset(...), tweedie
#
# Externally-assessed outcomes (NRC inspection / regulatory oversight):
#   rate_findings_all      : findings_count ~ . + offset(...), nbinom2
#   rate_findings_nongreen : findings_nongreen ~ . + offset(...), nbinom2
#   prob_above_col1        : above_col1 ~ ., binomial (no offset)
#
# Run from project root: Rscript nrc/1_nrc_panel_multiverse.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(furrr)
  library(glmmTMB)
  library(broom.mixed)
  library(slider)
  library(lubridate)
  library(stringr)
})

source("nrc/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("nrc/panel_data_prep.R")
source("nrc/panel_fit_models.R")
source("common/panel_multiverse_runner.R")


# =============================================================================
# 
# =============================================================================

CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026"
EVENTS_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet"
OPS_PATH    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet"
FINDINGS_PATH      <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/findings_quarterly.parquet"
ACTION_MATRIX_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/action_matrix_long.parquet"

OUTPUT_PATH    <- "results_new_new/nrc/panel_mv_results.parquet"
CHECKPOINT_DIR <- "results_new_new/nrc/panel_checkpoints"
N_WORKERS      <- 20L
PANEL_OUTCOMES <- c("rate_lers", "rate_emerg", "rate_scrams", "rate_pct_power_loss",
                    "rate_findings_all", "rate_findings_nongreen",
                    "prob_above_col1")


# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n=== Loading data ===\n")

nrc_events <- arrow::read_parquet(EVENTS_PATH) %>%
  mutate(event_date = as.Date(event_date),
         eid = as.character(event_num))

ops_raw <- arrow::read_parquet(OPS_PATH) %>%
  mutate(quarter_start = as.Date(quarter_start))

findings_raw <- arrow::read_parquet(FINDINGS_PATH) %>%
  mutate(quarter_start = as.Date(quarter_start))

action_matrix_raw <- arrow::read_parquet(ACTION_MATRIX_PATH) %>%
  mutate(quarter_start = as.Date(quarter_start))

cat(sprintf("  events:        %d, facilities: %d\n",
            nrow(nrc_events), n_distinct(nrc_events$facility)))
cat(sprintf("  ops:           %d, facility_units: %d\n",
            nrow(ops_raw), n_distinct(ops_raw$facility_unit)))
cat(sprintf("  findings:      %d, facility_units: %d\n",
            nrow(findings_raw), n_distinct(findings_raw$facility_unit)))
cat(sprintf("  action matrix: %d, facility_units: %d\n",
            nrow(action_matrix_raw), n_distinct(action_matrix_raw$facility_unit)))


# =============================================================================
# PROTOCOL
# =============================================================================

panel_data_fn <- function(parquet_path, config_id) {
  prepare_nrc_panel_data(
    parquet_path     = parquet_path,
    config_id        = config_id,
    events_df        = nrc_events,
    ops_raw          = ops_raw,
    findings_df      = findings_raw,
    action_matrix_df = action_matrix_raw,
    min_reports      = MIN_REPORTS_DEFAULT,
    max_holdover_days = 365L,
    climate_base     = "overall_final_score"
  )
}

fit_fns <- list(
  rate_lers              = function(panel, cv, cid, outcome) {
    fit_single_nrc_panel_model(panel, cv, cid, outcome = "rate_lers")
  },
  rate_emerg             = function(panel, cv, cid, outcome) {
    fit_single_nrc_panel_model(panel, cv, cid, outcome = "rate_emerg")
  },
  rate_scrams            = function(panel, cv, cid, outcome) {
    fit_single_nrc_panel_model(panel, cv, cid, outcome = "rate_scrams")
  },
  rate_pct_power_loss    = function(panel, cv, cid, outcome) {
    fit_single_nrc_panel_model(panel, cv, cid, outcome = "rate_pct_power_loss")
  },
  rate_findings_all      = function(panel, cv, cid, outcome) {
    fit_single_nrc_panel_model(panel, cv, cid, outcome = "rate_findings_all")
  },
  rate_findings_nongreen = function(panel, cv, cid, outcome) {
    fit_single_nrc_panel_model(panel, cv, cid, outcome = "rate_findings_nongreen")
  },
  prob_above_col1        = function(panel, cv, cid, outcome) {
    fit_single_nrc_panel_model(panel, cv, cid, outcome = "prob_above_col1")
  }
)


# =============================================================================
# RUN
# =============================================================================

results <- run_panel_multiverse(
  cfg_dir        = CFG_DIR,
  panel_data_fn  = panel_data_fn,
  fit_fns        = fit_fns,
  outcomes       = PANEL_OUTCOMES,
  config_ids     = NULL,
  n_workers      = N_WORKERS,
  output_path    = OUTPUT_PATH,
  checkpoint_dir = CHECKPOINT_DIR,
  worker_source_files = c(
    "nrc/config.R",
    "common/data_prep.R",
    "common/panel_data_prep.R",
    "nrc/panel_data_prep.R",
    "nrc/panel_fit_models.R",
    "common/panel_multiverse_runner.R"
  ),
  worker_globals = list(
    nrc_events        = nrc_events,
    ops_raw           = ops_raw,
    findings_raw      = findings_raw,
    action_matrix_raw = action_matrix_raw,
    panel_data_fn     = panel_data_fn,
    fit_fns           = fit_fns
  )
)

cat("\nDone. Status table:\n")
print(table(results$status, useNA = "ifany"))
cat("\nBy outcome:\n")
print(results %>% count(outcome, status))
