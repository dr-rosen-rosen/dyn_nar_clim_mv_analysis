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

CFG_DIR        <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_2026-07-14"
ASRS_PARQUET_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet"
AIDS_PARQUET_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/aids_events.parquet"
NTSB_POST_PATH <- here("data/aviation/ntsb_av_accident_data/events.xlsx")
NTSB_PRE_PATH  <- here("data/aviation/ntsb_av_accident_data/events_pre2008.xlsx")
OPS_PATH       <- here("data/aviation/bts_t100/airport_month_ops.parquet")

# Run isolation (defaults preserve original behavior). Override via env:
#   AV_MV_TAG=harm_ladder  -> writes panel_mv_results_harm_ladder.parquet
#   AV_MV_OUTCOMES=a,b,c    -> restrict the outcome set (comma-separated)
RUN_TAG  <- Sys.getenv("AV_MV_TAG", "")
.tag_sfx <- if (nzchar(RUN_TAG)) paste0("_", RUN_TAG) else ""
OUTPUT_PATH    <- sprintf("results_new_new/aviation/panel_mv_results%s.parquet", .tag_sfx)
CHECKPOINT_DIR <- sprintf("results_new_new/aviation/panel_checkpoints%s", .tag_sfx)
N_WORKERS      <- { e <- suppressWarnings(as.integer(Sys.getenv("AV_MV_WORKERS",""))); if (is.na(e)||e<1L) 20L else e }
# Consolidated two-axis consequence ladder:
#   DAMAGE axis (AIDS):   noharm -> propdamage  (substantial_damage, c98)
#   CASUALTY axis (NTSB): minor -> serious -> serious_fatal -> fatal (ev_highest_injury)
# Plus AIDS breadth (all/incidents/injury/harm), NTSB accidents, and the legacy
# sum-based casualty measures for continuity. See panel_fit_models.R.
PANEL_OUTCOMES <- c(# AIDS damage axis
                    "rate_aids_noharm", "rate_aids_propdamage",
                    # AIDS breadth / coarse casualty
                    "rate_aids_all", "rate_aids_incidents_only",
                    "rate_aids_zeroharm", "rate_aids_injury",
                    "rate_aids_fatal", "rate_aids_harm",
                    # NTSB casualty-severity axis (event counts)
                    "rate_ntsb_minor", "rate_ntsb_serious",
                    "rate_ntsb_fatal", "rate_ntsb_serious_fatal",
                    # NTSB accidents + legacy sum-based (continuity)
                    "rate_accidents", "rate_inj_serious_fatal",
                    "rate_fatalities")
.oc_override <- Sys.getenv("AV_MV_OUTCOMES", "")
if (nzchar(.oc_override)) PANEL_OUTCOMES <- trimws(strsplit(.oc_override, ",")[[1]])

# PRIMARY outer spec (mirror in CV driver if changed)
# Primary spec is local_terminal (tower+TRACON) / 5 NM. Override via env for
# robustness variants, e.g. AV_ATC_SCOPE=local (tower-only) or AV_APT_DIST_NM=15.
ATC_SCOPE       <- { e <- Sys.getenv("AV_ATC_SCOPE", "");   if (nzchar(e)) e else "local_terminal" }
APT_DIST_NM     <- { e <- suppressWarnings(as.integer(Sys.getenv("AV_APT_DIST_NM",""))); if (is.na(e)) 5L else e }
MISSING_CLIMATE <- "exclude"
# AV_PERIOD=quarter -> airport-QUARTER grain (NRC-style; denser, preserves
# dynamic signal). Default month (original behavior).
AV_PERIOD <- { e <- Sys.getenv("AV_PERIOD", ""); if (e %in% c("month","quarter")) e else "month" }


# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n=== Loading aviation data (PRIMARY outer spec) ===\n")
cat(sprintf("  atc_scope=%s  apt_dist_nm=%d  missing_climate=%s  period=%s\n",
            ATC_SCOPE, APT_DIST_NM, MISSING_CLIMATE, AV_PERIOD))

asrs_meta    <- load_asrs_meta_panel(ASRS_PARQUET_PATH)
ops_features <- read_parquet(OPS_PATH)
ntsb_panel   <- load_ntsb_panel_rate(NTSB_POST_PATH, NTSB_PRE_PATH,
                                      apt_dist_nm = APT_DIST_NM, period = AV_PERIOD)
aids_panel   <- load_aids_panel_rate(AIDS_PARQUET_PATH,
                                      apt_dist_nm = APT_DIST_NM,
                                      min_year = 1988L, period = AV_PERIOD)

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
    period           = AV_PERIOD,
    max_holdover_days = 365L,
    climate_base     = "overall_final_score"
  )
}

# Build fit_fns programmatically from PANEL_OUTCOMES so new/overridden outcomes
# (e.g. the harm-axis ladder) are always registered. Each closure captures its
# own outcome key via force().
fit_fns <- setNames(lapply(PANEL_OUTCOMES, function(oc) {
  force(oc)
  function(panel, cv, cid, outcome) {
    fit_single_aviation_panel_model(panel, cv, cid, outcome = oc)
  }
}), PANEL_OUTCOMES)


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
