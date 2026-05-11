# =============================================================================
# aviation/2_aviation_panel_cv.R — Aviation Panel-Rate Cross-Validation
# =============================================================================
#
# For every (config × climate_var × outcome), fits three nested tiers
# (intercept_only, seasonal_ops, climate) under group_kfold and timeseries
# CV strategies. Primary metric: held-out log-likelihood per observation.
#
# Same PRIMARY outer spec as the multiverse driver.
#
# Run: Rscript aviation/2_aviation_panel_cv.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(furrr)
  library(glmmTMB)
  library(slider)
  library(lubridate)
  library(stringr)
  library(readxl)
  library(here)
})

source("aviation/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("aviation/data_prep.R")
source("aviation/panel_data_prep.R")
source("aviation/panel_fit_models.R")
source("common/panel_cv_runner.R")


CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026"
ASRS_PARQUET_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet"
AIDS_PARQUET_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/aids_events.parquet"
NTSB_POST_PATH <- here("data/aviation/ntsb_av_accident_data/events.xlsx")
NTSB_PRE_PATH  <- here("data/aviation/ntsb_av_accident_data/events_pre2008.xlsx")
OPS_PATH       <- here("data/aviation/bts_t100/airport_month_ops.parquet")

OUTPUT_PATH <- "results_new_new/aviation/panel_cv_results.parquet"
N_WORKERS   <- 20L

# PRIMARY outer spec (must match the multiverse driver)
ATC_SCOPE       <- "local_terminal"
APT_DIST_NM     <- 5L
MISSING_CLIMATE <- "exclude"


# --- Load data ---
asrs_meta    <- load_asrs_meta_panel(ASRS_PARQUET_PATH)
ops_features <- read_parquet(OPS_PATH)
ntsb_panel   <- load_ntsb_panel_rate(NTSB_POST_PATH, NTSB_PRE_PATH,
                                      apt_dist_nm = APT_DIST_NM)
aids_panel   <- load_aids_panel_rate(AIDS_PARQUET_PATH,
                                      apt_dist_nm = APT_DIST_NM,
                                      min_year = 1988L)


# --- Panel prep closure ---
panel_data_fn <- function(parquet_path, config_id) {
  prepare_aviation_panel_data(
    parquet_path     = parquet_path,
    config_id        = config_id,
    asrs_meta_df     = asrs_meta,
    ntsb_panel_df    = ntsb_panel,
    ops_features     = ops_features,
    aids_panel_df    = aids_panel,
    atc_scope        = ATC_SCOPE,
    missing_climate  = MISSING_CLIMATE,
    min_reports      = MIN_REPORTS_DEFAULT
  )
}


# --- Outcome specs ---
.av_bw <- c("seats_between", "seats_within",
            "passengers_between", "passengers_within")

outcome_specs <- list(
  rate_aids_all = list(
    outcome_var = "n_aids_all", exposure = "departures",
    family = "nbinom2", bw_terms = .av_bw,
    org_var = "airport_id", period_date_var = "yearmonth"
  ),
  rate_aids_incidents_only = list(
    outcome_var = "n_aids_incidents", exposure = "departures",
    family = "nbinom2", bw_terms = .av_bw,
    org_var = "airport_id", period_date_var = "yearmonth"
  ),
  rate_accidents = list(
    outcome_var = "n_accidents", exposure = "departures",
    family = "nbinom2", bw_terms = .av_bw,
    org_var = "airport_id", period_date_var = "yearmonth"
  ),
  rate_inj_serious_fatal = list(
    outcome_var = "sum_serious_fatal", exposure = "departures",
    family = "nbinom2", bw_terms = .av_bw,
    org_var = "airport_id", period_date_var = "yearmonth"
  ),
  rate_fatalities = list(
    outcome_var = "sum_fatalities", exposure = "departures",
    family = "nbinom2", bw_terms = .av_bw,
    org_var = "airport_id", period_date_var = "yearmonth"
  )
)


# --- Run ---
# Monthly cadence; defaults (24mo test, 6mo gap, 200/20 row thresholds) match
# rail. Override here if aviation panel turns out smaller after climate-NA drop.
results <- run_panel_cv(
  cfg_dir        = CFG_DIR,
  panel_data_fn  = panel_data_fn,
  outcome_specs  = outcome_specs,
  config_ids     = NULL,#c("0","1","2"),  # smoke; NULL for full sweep
  strategies     = c("group_kfold", "timeseries"),
  K = 5L, n_splits = 5L,
  test_duration_months = 24L, gap_months = 6L,
  n_workers   = N_WORKERS,
  output_path = OUTPUT_PATH,
  worker_source_files = c(
    "aviation/config.R",
    "common/data_prep.R",
    "common/panel_data_prep.R",
    "aviation/data_prep.R",
    "aviation/panel_data_prep.R",
    "aviation/panel_fit_models.R",
    "common/panel_cv_runner.R"
  ),
  worker_globals = list(
    asrs_meta    = asrs_meta,
    ops_features = ops_features,
    ntsb_panel   = ntsb_panel,
    aids_panel   = aids_panel,
    panel_data_fn = panel_data_fn,
    ATC_SCOPE    = ATC_SCOPE,
    APT_DIST_NM  = APT_DIST_NM,
    MISSING_CLIMATE = MISSING_CLIMATE
  )
)

cat("\n=== Summary: delta-loglik (climate vs seasonal_ops) ===\n")
print(results %>%
        filter(model_label == "delta_climate_vs_seasonal") %>%
        group_by(panel_outcome, cv_strategy) %>%
        summarise(median_delta_ll = median(loglik_mean, na.rm = TRUE),
                  pct_positive = mean(loglik_mean > 0, na.rm = TRUE),
                  n = n(), .groups = "drop"))
