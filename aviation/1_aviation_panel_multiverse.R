# =============================================================================
# aviation/1_aviation_panel_multiverse.R — Aviation Panel-Rate Multiverse
# =============================================================================
#
# Sweeps every climate config × climate window × panel outcome, on the PRIMARY
# outer spec (atc_scope=local_terminal × apt_dist_nm=5 × missing_climate=exclude).
# Other outer-spec combinations are kept as sensitivity-only — re-run by
# changing the constants below.
#
# Outcomes:
#   rate_accidents          n_accidents       / departures   nbinom2
#   rate_inj_serious_fatal  sum_serious_fatal / departures   nbinom2
#   rate_fatalities         sum_fatalities    / departures   nbinom2
#
# Run from project root: Rscript aviation/1_aviation_panel_multiverse.R
#
# NOTE: Update CFG_DIR to the aviation climate features dir once it's available.
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
  library(readxl)
  library(here)
})

source("aviation/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("aviation/data_prep.R")           # for load_asrs_meta()
source("aviation/panel_data_prep.R")
source("aviation/panel_fit_models.R")
source("common/panel_multiverse_runner.R")


# =============================================================================
# CONFIG
# =============================================================================

CFG_DIR        <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026"
ASRS_PARQUET_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet"
AIDS_PARQUET_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/aids_events.parquet"
NTSB_POST_PATH <- here("data/aviation/ntsb_av_accident_data/events.xlsx")
NTSB_PRE_PATH  <- here("data/aviation/ntsb_av_accident_data/events_pre2008.xlsx")
OPS_PATH       <- here("data/aviation/bts_t100/airport_month_ops.parquet")

OUTPUT_PATH    <- "results_new_new/aviation/panel_mv_results.parquet"
CHECKPOINT_DIR <- "results_new_new/aviation/panel_checkpoints"
N_WORKERS      <- 20L
# Two AIDS-derived event-count outcomes plus three NTSB severity outcomes.
PANEL_OUTCOMES <- c("rate_aids_all", "rate_aids_incidents_only",
                    "rate_accidents", "rate_inj_serious_fatal",
                    "rate_fatalities")

# PRIMARY outer spec (mirror in CV driver if changed)
ATC_SCOPE       <- "local_terminal"
APT_DIST_NM     <- 5L
MISSING_CLIMATE <- "exclude"


# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n=== Loading aviation data (PRIMARY outer spec) ===\n")
cat(sprintf("  atc_scope=%s  apt_dist_nm=%d  missing_climate=%s\n",
            ATC_SCOPE, APT_DIST_NM, MISSING_CLIMATE))

asrs_meta    <- load_asrs_meta_panel(ASRS_PARQUET_PATH)
ops_features <- read_parquet(OPS_PATH)
ntsb_panel   <- load_ntsb_panel_rate(NTSB_POST_PATH, NTSB_PRE_PATH,
                                      apt_dist_nm = APT_DIST_NM)
aids_panel   <- load_aids_panel_rate(AIDS_PARQUET_PATH,
                                      apt_dist_nm = APT_DIST_NM,
                                      min_year = 1988L)

cat(sprintf("  ASRS meta:  %d reports\n", nrow(asrs_meta)))
cat(sprintf("  BTS ops:    %d airport-months\n", nrow(ops_features)))
cat(sprintf("  NTSB panel: %d accident-months across %d airports\n",
            nrow(ntsb_panel), n_distinct(ntsb_panel$airport_id)))
cat(sprintf("  AIDS panel: %d airport-months across %d airports\n",
            nrow(aids_panel), n_distinct(aids_panel$airport_id)))


# =============================================================================
# PROTOCOL
# =============================================================================

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
    min_reports      = MIN_REPORTS_DEFAULT,
    max_holdover_days = 365L,
    climate_base     = "overall_final_score"
  )
}

fit_fns <- list(
  rate_aids_all              = function(panel, cv, cid, outcome) {
    fit_single_aviation_panel_model(panel, cv, cid, outcome = "rate_aids_all")
  },
  rate_aids_incidents_only   = function(panel, cv, cid, outcome) {
    fit_single_aviation_panel_model(panel, cv, cid, outcome = "rate_aids_incidents_only")
  },
  rate_accidents             = function(panel, cv, cid, outcome) {
    fit_single_aviation_panel_model(panel, cv, cid, outcome = "rate_accidents")
  },
  rate_inj_serious_fatal     = function(panel, cv, cid, outcome) {
    fit_single_aviation_panel_model(panel, cv, cid, outcome = "rate_inj_serious_fatal")
  },
  rate_fatalities            = function(panel, cv, cid, outcome) {
    fit_single_aviation_panel_model(panel, cv, cid, outcome = "rate_fatalities")
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
  config_ids     = NULL, #c("0","1","2"),  # smoke; set to NULL for full sweep
  n_workers      = N_WORKERS,
  output_path    = OUTPUT_PATH,
  checkpoint_dir = CHECKPOINT_DIR,
  worker_source_files = c(
    "aviation/config.R",
    "common/data_prep.R",
    "common/panel_data_prep.R",
    "aviation/data_prep.R",
    "aviation/panel_data_prep.R",
    "aviation/panel_fit_models.R",
    "common/panel_multiverse_runner.R"
  ),
  worker_globals = list(
    asrs_meta    = asrs_meta,
    ops_features = ops_features,
    ntsb_panel   = ntsb_panel,
    aids_panel   = aids_panel,
    panel_data_fn = panel_data_fn,
    fit_fns      = fit_fns,
    ATC_SCOPE    = ATC_SCOPE,
    APT_DIST_NM  = APT_DIST_NM,
    MISSING_CLIMATE = MISSING_CLIMATE
  )
)

cat("\nDone. Status table:\n")
print(table(results$status, useNA = "ifany"))
cat("\nBy outcome:\n")
print(results %>% count(outcome, status))
