# =============================================================================
# phmsa/2_phmsa_panel_cv.R — PHMSA Panel-Rate Cross-Validation
# =============================================================================
#
# For each pipeline_type, fits three nested tiers (intercept_only,
# seasonal_ops, climate) under group_kfold and timeseries CV.
# Primary metric: held-out log-likelihood per observation.
#
# Run: Rscript phmsa/2_phmsa_panel_cv.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(furrr)
  library(glmmTMB)
  library(slider)
  library(lubridate)
  library(stringr)
  library(here)
})

source("phmsa/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("phmsa/panel_data_prep.R")
source("phmsa/panel_fit_models.R")
source("common/panel_cv_runner.R")


CFG_DIR      <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/phmsa_04-28-2026"
EVENTS_PATH  <- here("data/phmsa/events.parquet")
OPS_PATH     <- here("data/phmsa/operator_annual_ops.parquet")
OUTPUT_BASE  <- "results_new_new/phmsa"
N_WORKERS    <- 20L
CONFIG_IDS   <- NULL #c("0","1","2")   # smoke; NULL for full
MIN_REPORTS_DEFAULT <- 5L

# Match the multiverse driver's per-pipeline-type outcome filter
PANEL_OUTCOMES_BY_TYPE <- list(
  gas_distribution = c("rate_incidents", "rate_injuries", "rate_release_volume"),
  gas_transmission = c("rate_incidents", "rate_release_volume"),
  hazardous_liquid = c("rate_incidents", "rate_release_volume")
)


# --- Load data ---
phmsa_events_all <- read_parquet(EVENTS_PATH) %>%
  mutate(
    operator_id = as.integer(operator_raw_id),
    event_date  = as.Date(event_date)
  ) %>%
  filter(!is.na(operator_id), !is.na(event_date),
         pipeline_type %in% PIPELINE_TYPES) %>%
  select(eid, operator_id, event_date, pipeline_type,
         total_injuries, total_fatalities, total_cost,
         UNINTENTIONAL_RELEASE_BBLS, GAS_COST_IN_MCF)

ops_features <- read_parquet(OPS_PATH)


# Yearly cadence; CV windows in months (panel_cv_runner accepts months).
# Annual data ~9 yrs total. Per-year test windows + 0-month gap (year
# boundaries already provide a natural buffer). Min row thresholds tuned for
# annual cadence — defaults (200 train / 20 test) are calibrated for monthly.
TEST_DURATION_MONTHS <- 12L
GAP_MONTHS           <- 0L
MIN_TRAIN_ROWS       <- 80L
MIN_TEST_ROWS_KFOLD  <- 5L
MIN_TEST_ROWS_TS     <- 10L


# --- Per-pipeline-type loop ---

results_all <- list()

for (pt in PIPELINE_TYPES) {
  cat(sprintf("\n\n#################  %s  #################\n", pt))
  events_pt <- phmsa_events_all %>% filter(pipeline_type == !!pt)

  output_pq <- file.path(OUTPUT_BASE, pt, "panel_cv_results.parquet")
  dir.create(dirname(output_pq), recursive = TRUE, showWarnings = FALSE)

  panel_data_fn <- function(parquet_path, config_id) {
    prepare_phmsa_panel_data(
      parquet_path  = parquet_path,
      config_id     = config_id,
      events_df     = events_pt,
      ops_features  = ops_features,
      pipeline_type = pt,
      min_reports   = MIN_REPORTS_DEFAULT,
      max_holdover_days = 365L,
      climate_base  = "overall_final_score"
    )
  }

  outcome_specs_all <- list(
    rate_incidents = list(
      outcome_var = "n_incidents",        exposure = "total_miles",
      family = "nbinom2",
      bw_terms = c("total_miles_between", "total_miles_within"),
      org_var = "operator_id", period_date_var = "year_start"
    ),
    rate_injuries = list(
      outcome_var = "sum_injuries",       exposure = "total_miles",
      family = "nbinom2",
      bw_terms = c("total_miles_between", "total_miles_within"),
      org_var = "operator_id", period_date_var = "year_start"
    ),
    rate_fatalities = list(
      outcome_var = "sum_fatalities",     exposure = "total_miles",
      family = "nbinom2",
      bw_terms = c("total_miles_between", "total_miles_within"),
      org_var = "operator_id", period_date_var = "year_start"
    ),
    rate_release_volume = list(
      outcome_var = "sum_release_volume", exposure = "total_miles",
      family = "tweedie",
      bw_terms = c("total_miles_between", "total_miles_within"),
      org_var = "operator_id", period_date_var = "year_start"
    )
  )

  outcomes_pt <- PANEL_OUTCOMES_BY_TYPE[[pt]]
  if (is.null(outcomes_pt) || length(outcomes_pt) == 0) {
    cat(sprintf("  [SKIP] no outcomes configured for pipeline_type %s\n", pt))
    next
  }
  cat(sprintf("  outcomes: %s\n", paste(outcomes_pt, collapse = ", ")))
  outcome_specs <- outcome_specs_all[outcomes_pt]

  results_pt <- run_panel_cv(
    cfg_dir        = CFG_DIR,
    panel_data_fn  = panel_data_fn,
    outcome_specs  = outcome_specs,
    config_ids     = CONFIG_IDS,
    strategies     = c("group_kfold", "timeseries"),
    K = 5L, n_splits = 5L,
    test_duration_months = TEST_DURATION_MONTHS, gap_months = GAP_MONTHS,
    min_train_rows = MIN_TRAIN_ROWS,
    min_test_rows_kfold = MIN_TEST_ROWS_KFOLD,
    min_test_rows_timeseries = MIN_TEST_ROWS_TS,
    n_workers   = N_WORKERS,
    output_path = output_pq,
    worker_source_files = c(
      "phmsa/config.R",
      "common/data_prep.R",
      "common/panel_data_prep.R",
      "phmsa/panel_data_prep.R",
      "phmsa/panel_fit_models.R",
      "common/panel_cv_runner.R"
    ),
    worker_globals = list(
      events_pt = events_pt,
      ops_features = ops_features,
      pt = pt,
      panel_data_fn = panel_data_fn,
      MIN_REPORTS_DEFAULT = MIN_REPORTS_DEFAULT
    )
  )

  results_all[[pt]] <- results_pt %>% mutate(pipeline_type = pt)
}

cat("\n=== PHMSA Panel CV Complete ===\n")
combined <- bind_rows(results_all)
cat("\nDelta-loglik (climate vs seasonal_ops) by pipeline_type × outcome × strategy:\n")
print(combined %>%
        filter(model_label == "delta_climate_vs_seasonal") %>%
        group_by(pipeline_type, panel_outcome, cv_strategy) %>%
        summarise(median_delta_ll = median(loglik_mean, na.rm = TRUE),
                  pct_positive = mean(loglik_mean > 0, na.rm = TRUE),
                  n = n(), .groups = "drop"))
