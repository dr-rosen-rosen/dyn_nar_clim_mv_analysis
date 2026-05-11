# =============================================================================
# phmsa/1_phmsa_panel_multiverse.R — PHMSA Panel-Rate Multiverse
# =============================================================================
#
# Sweeps every climate config × climate window × panel outcome, separately for
# each pipeline_type (gas_distribution, gas_transmission, hazardous_liquid).
# Each pipeline_type produces its own output parquet.
#
# Outcomes (same model_fn list reused per type; the fitter early-returns
# "failed: too few non-zero" when an outcome is non-applicable, e.g.
# rate_release_volume on gas pipelines).
#
# Run from project root: Rscript phmsa/1_phmsa_panel_multiverse.R
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
  library(here)
})

source("phmsa/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("phmsa/panel_data_prep.R")
source("phmsa/panel_fit_models.R")
source("common/panel_multiverse_runner.R")


# =============================================================================
# CONFIG
# =============================================================================

CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/phmsa_04-28-2026"
EVENTS_PATH <- here("data/phmsa/events.parquet")
OPS_PATH    <- here("data/phmsa/operator_annual_ops.parquet")

OUTPUT_BASE     <- "results_new_new/phmsa"
N_WORKERS       <- 20L
# Per-pipeline-type outcome lists. Sparse outcomes (fatalities everywhere;
# injuries for non-distribution; release_volume for unrelated phases) are
# pre-filtered out so we don't burn 58% of compute on early-returns.
PANEL_OUTCOMES_BY_TYPE <- list(
  gas_distribution = c("rate_incidents", "rate_injuries", "rate_release_volume"),
  gas_transmission = c("rate_incidents", "rate_release_volume"),
  hazardous_liquid = c("rate_incidents", "rate_release_volume")
)
CONFIG_IDS      <- NULL #c("0","1","2")  # smoke; set to NULL for full sweep
MIN_REPORTS_DEFAULT <- 5L            # smaller than rail/nrc (annual cadence)


# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n=== Loading PHMSA data ===\n")

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

cat(sprintf("  events: %d  operators: %d\n",
            nrow(phmsa_events_all), n_distinct(phmsa_events_all$operator_id)))
print(count(phmsa_events_all, pipeline_type))


# =============================================================================
# PER-PIPELINE-TYPE SWEEP
# =============================================================================

results_all <- list()

for (pt in PIPELINE_TYPES) {
  cat(sprintf("\n\n#################  %s  #################\n", pt))

  events_pt <- phmsa_events_all %>% filter(pipeline_type == !!pt)

  out_dir   <- file.path(OUTPUT_BASE, pt)
  output_pq <- file.path(out_dir, "panel_mv_results.parquet")
  cp_dir    <- file.path(out_dir, "panel_checkpoints")
  dir.create(cp_dir, recursive = TRUE, showWarnings = FALSE)

  # Closure captures events_pt, ops_features, pipeline_type for workers
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

  outcomes_pt <- PANEL_OUTCOMES_BY_TYPE[[pt]]
  if (is.null(outcomes_pt) || length(outcomes_pt) == 0) {
    cat(sprintf("  [SKIP] no outcomes configured for pipeline_type %s\n", pt))
    next
  }
  cat(sprintf("  outcomes: %s\n", paste(outcomes_pt, collapse = ", ")))

  fit_fns <- list(
    rate_incidents      = function(panel, cv, cid, outcome)
      fit_single_phmsa_panel_model(panel, cv, cid, outcome = "rate_incidents"),
    rate_injuries       = function(panel, cv, cid, outcome)
      fit_single_phmsa_panel_model(panel, cv, cid, outcome = "rate_injuries"),
    rate_fatalities     = function(panel, cv, cid, outcome)
      fit_single_phmsa_panel_model(panel, cv, cid, outcome = "rate_fatalities"),
    rate_release_volume = function(panel, cv, cid, outcome)
      fit_single_phmsa_panel_model(panel, cv, cid, outcome = "rate_release_volume")
  )
  fit_fns <- fit_fns[outcomes_pt]

  results_pt <- run_panel_multiverse(
    cfg_dir        = CFG_DIR,
    panel_data_fn  = panel_data_fn,
    fit_fns        = fit_fns,
    outcomes       = outcomes_pt,
    config_ids     = CONFIG_IDS,
    n_workers      = N_WORKERS,
    output_path    = output_pq,
    checkpoint_dir = cp_dir,
    worker_source_files = c(
      "phmsa/config.R",
      "common/data_prep.R",
      "common/panel_data_prep.R",
      "phmsa/panel_data_prep.R",
      "phmsa/panel_fit_models.R",
      "common/panel_multiverse_runner.R"
    ),
    worker_globals = list(
      events_pt    = events_pt,
      ops_features = ops_features,
      pt           = pt,
      panel_data_fn = panel_data_fn,
      fit_fns      = fit_fns,
      MIN_REPORTS_DEFAULT = MIN_REPORTS_DEFAULT
    )
  )

  results_all[[pt]] <- results_pt %>% mutate(pipeline_type = pt)
}

cat("\n\n=== PHMSA Panel Multiverse Complete ===\n")
combined <- bind_rows(results_all)
cat(sprintf("  Total rows: %s\n", format(nrow(combined), big.mark = ",")))
cat("  Status by pipeline_type × outcome:\n")
print(count(combined, pipeline_type, outcome, status))
