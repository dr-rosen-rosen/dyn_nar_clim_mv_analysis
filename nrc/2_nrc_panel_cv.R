# =============================================================================
# nrc/2_nrc_panel_cv.R — NRC Panel-Rate Cross-Validation
# =============================================================================
#
# For every (config × climate_var × outcome), fits three nested tiers
# (intercept_only, seasonal_ops, climate) under group_kfold and timeseries
# CV. Primary metric: held-out log-likelihood per observation.
#
# Run: Rscript nrc/2_nrc_panel_cv.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(furrr)
  library(glmmTMB)
  library(slider)
  library(lubridate)
  library(stringr)
})

source("nrc/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("nrc/panel_data_prep.R")
source("nrc/panel_fit_models.R")
source("common/panel_cv_runner.R")


CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026"
EVENTS_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet"
OPS_PATH    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet"
FINDINGS_PATH      <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/findings_quarterly.parquet"
ACTION_MATRIX_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/action_matrix_long.parquet"
OUTPUT_PATH <- "results_new_new/nrc/panel_cv_results.parquet"
N_WORKERS   <- 20L


# --- Load data ---
nrc_events <- arrow::read_parquet(EVENTS_PATH) %>%
  mutate(event_date = as.Date(event_date),
         eid = as.character(event_num))
ops_raw <- arrow::read_parquet(OPS_PATH) %>%
  mutate(quarter_start = as.Date(quarter_start))
findings_raw <- arrow::read_parquet(FINDINGS_PATH) %>%
  mutate(quarter_start = as.Date(quarter_start))
action_matrix_raw <- arrow::read_parquet(ACTION_MATRIX_PATH) %>%
  mutate(quarter_start = as.Date(quarter_start))


# --- Panel prep closure ---
panel_data_fn <- function(parquet_path, config_id) {
  prepare_nrc_panel_data(parquet_path, config_id, nrc_events,
                          ops_raw          = ops_raw,
                          findings_df      = findings_raw,
                          action_matrix_df = action_matrix_raw,
                          min_reports      = MIN_REPORTS_DEFAULT)
}


# --- Outcome specs ---
.nrc_bw <- c("power_std_mean_between", "power_std_mean_within")

outcome_specs <- list(
  rate_lers = list(
    outcome_var = "n_lers", exposure = "days_at_power_total",
    family = "nbinom2", bw_terms = .nrc_bw,
    org_var = "facility_site", period_date_var = "quarter_start"
  ),
  rate_emerg = list(
    outcome_var = "n_emerg", exposure = "days_at_power_total",
    family = "nbinom2", bw_terms = .nrc_bw,
    org_var = "facility_site", period_date_var = "quarter_start"
  ),
  rate_scrams = list(
    outcome_var = "n_scrams", exposure = "days_at_power_total",
    family = "nbinom2", bw_terms = .nrc_bw,
    org_var = "facility_site", period_date_var = "quarter_start"
  ),
  rate_pct_power_loss = list(
    outcome_var = "sum_pct_power_loss", exposure = "days_at_power_total",
    family = "tweedie", bw_terms = .nrc_bw,
    org_var = "facility_site", period_date_var = "quarter_start"
  ),
  rate_findings_all = list(
    outcome_var = "findings_count", exposure = "days_at_power_total",
    family = "nbinom2", bw_terms = .nrc_bw,
    org_var = "facility_site", period_date_var = "quarter_start"
  ),
  rate_findings_nongreen = list(
    outcome_var = "findings_nongreen", exposure = "days_at_power_total",
    family = "nbinom2", bw_terms = .nrc_bw,
    org_var = "facility_site", period_date_var = "quarter_start"
  ),
  prob_above_col1 = list(
    outcome_var = "above_col1", exposure = NULL,   # binomial — no offset
    family = "binomial", bw_terms = .nrc_bw,
    org_var = "facility_site", period_date_var = "quarter_start"
  )
)


# --- Run ---
# NRC quarterly cadence: tighten timeseries window to 12 months (4 quarters).
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
    "nrc/config.R",
    "common/data_prep.R",
    "common/panel_data_prep.R",
    "nrc/panel_data_prep.R",
    "nrc/panel_fit_models.R",
    "common/panel_cv_runner.R"
  ),
  worker_globals = list(
    nrc_events        = nrc_events,
    ops_raw           = ops_raw,
    findings_raw      = findings_raw,
    action_matrix_raw = action_matrix_raw,
    panel_data_fn     = panel_data_fn
  )
)

cat("\n=== Summary: delta-loglik (climate vs seasonal_ops) ===\n")
print(results %>%
        filter(model_label == "delta_climate_vs_seasonal") %>%
        group_by(panel_outcome, cv_strategy) %>%
        summarise(median_delta_ll = median(loglik_mean, na.rm = TRUE),
                  pct_positive = mean(loglik_mean > 0, na.rm = TRUE),
                  n = n(), .groups = "drop"))
