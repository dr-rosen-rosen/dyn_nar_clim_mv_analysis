# =============================================================================
# aviation/phase1_panel_diagnostic.R — Aviation Panel-Rate Phase 1 Diagnostic
# =============================================================================
#
# Fits panel-rate models on a single climate config × one fixed climate window
# for the PRIMARY outer spec (atc_scope=local_terminal × apt_dist=5 × exclude).
# Confirms grid construction, ops join, and model convergence before launching
# the full multiverse / CV.
#
# Run from project root:
#   Rscript aviation/phase1_panel_diagnostic.R
#
# NOTE: requires CFG_DIR to be set to a real aviation climate-features dir.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
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


# ---- CONFIG ----
# Update CFG_DIR to the aviation climate features dir once it's available.
CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_04-29-2026"
CONFIG_ID   <- "0"
CLIMATE_VAR <- "overall_final_score_sma_5"

ASRS_PARQUET_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet"
AIDS_PARQUET_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/aids_events.parquet"
NTSB_POST_PATH <- here("data/aviation/ntsb_av_accident_data/events.xlsx")
NTSB_PRE_PATH  <- here("data/aviation/ntsb_av_accident_data/events_pre2008.xlsx")
OPS_PATH       <- here("data/aviation/bts_t100/airport_month_ops.parquet")

PARQUET_PATH <- file.path(CFG_DIR, "_cfg", CONFIG_ID, "results.parquet")

# Primary outer spec
ATC_SCOPE       <- "local_terminal"
APT_DIST_NM     <- 5L
MISSING_CLIMATE <- "exclude"


# ---- LOAD DATA ----
cat("\n=== Loading aviation data (PRIMARY outer spec) ===\n")
cat(sprintf("  atc_scope=%s, apt_dist_nm=%d, missing_climate=%s\n",
            ATC_SCOPE, APT_DIST_NM, MISSING_CLIMATE))

asrs_meta    <- load_asrs_meta_panel(ASRS_PARQUET_PATH)
ops_features <- read_parquet(OPS_PATH)
ntsb_panel   <- load_ntsb_panel_rate(NTSB_POST_PATH, NTSB_PRE_PATH,
                                      apt_dist_nm = APT_DIST_NM)
aids_panel   <- load_aids_panel_rate(AIDS_PARQUET_PATH,
                                      apt_dist_nm = APT_DIST_NM,
                                      min_year = 1988L)


# ---- PANEL CONSTRUCTION ----
cat(sprintf("\n=== Building panel for config %s ===\n", CONFIG_ID))

panel <- prepare_aviation_panel_data(
  parquet_path     = PARQUET_PATH,
  config_id        = CONFIG_ID,
  asrs_meta_df     = asrs_meta,
  ntsb_panel_df    = ntsb_panel,
  ops_features     = ops_features,
  aids_panel_df    = aids_panel,
  atc_scope        = ATC_SCOPE,
  missing_climate  = MISSING_CLIMATE,
  min_reports      = MIN_REPORTS_DEFAULT
)

if (is.null(panel) || nrow(panel) == 0) {
  stop("Phase 1: empty panel — check parquet path / outer spec inputs")
}

cat(sprintf("  Panel: %d rows  airports: %d  months: %d\n",
            nrow(panel), n_distinct(panel$airport_id), n_distinct(panel$yearmonth)))
cat(sprintf("  Climate %s NA-rate: %.1f%%\n", CLIMATE_VAR,
            100 * mean(is.na(panel[[CLIMATE_VAR]]))))
cat("  Outcome zero-inflation:\n")
print(panel |>
        summarise(across(c(n_aids_all, n_aids_incidents, n_accidents,
                            sum_serious_fatal, sum_fatalities),
                         list(mean = ~mean(.x, na.rm = TRUE),
                              pct_zero = ~100 * mean(.x == 0, na.rm = TRUE)))))


# ---- FIT EACH OUTCOME ----
for (outc in names(PANEL_OUTCOME_VARS_AVIATION)) {
  cat(sprintf("\n--- Fitting %s ---\n", outc))
  res <- fit_single_aviation_panel_model(panel, CLIMATE_VAR, CONFIG_ID, outcome = outc)
  if (!is.null(res$status) && res$status == "failed") {
    cat(sprintf("  [FAILED] %s\n", res$error %||% ""))
  } else {
    cat(sprintf("  status: %s | n_obs=%d | n_orgs=%d | climate_est=%.3f (p=%.3g) | RE_sd=%.3f\n",
                res$status, res$n_obs %||% NA, res$n_orgs %||% NA,
                res$climate_estimate %||% NA, res$climate_pval %||% NA,
                res$random_intercept_sd %||% NA))
  }
}

cat("\n=== Diagnostic complete ===\n")
