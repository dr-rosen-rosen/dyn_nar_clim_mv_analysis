# =============================================================================
# msha/1_msha_panel_multiverse.R — MSHA Panel-Rate Multiverse
# =============================================================================
#
# For each commodity (coal, mnm), sweeps every climate config × climate window
# × panel-rate outcome on the mine × quarter panel.
#
# Self-reported outcomes (from events.parquet):
#   rate_accidents : n_accidents   ~ . + offset(log(hours_worked)), nbinom2
#   rate_injuries  : n_injuries    ~ . + offset(...),               nbinom2
#   rate_fatal     : n_fatal       ~ . + offset(...),               nbinom2
#   rate_days_lost : sum_days_lost ~ . + offset(...),               tweedie
#
# Externally-assessed (regulatory) outcomes (from covariates_mine_quarter):
#   rate_violations : violation_count ~ . + offset(...), nbinom2
#   rate_ss         : ss_count        ~ . + offset(...), nbinom2
#
# Run from project root: Rscript msha/1_msha_panel_multiverse.R
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

source("msha/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("msha/panel_data_prep.R")
source("msha/panel_fit_models.R")
source("common/panel_multiverse_runner.R")


# =============================================================================
# PATHS
# =============================================================================

CFG_DIR         <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026"
DATA_DIR        <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha"
EVENTS_PATH     <- file.path(DATA_DIR, "events.parquet")
OPS_PATH        <- file.path(DATA_DIR, "ops_mine_quarter.parquet")
COVARIATES_PATH <- file.path(DATA_DIR, "covariates_mine_quarter.parquet")

OUTPUT_BASE    <- "results_new_new/msha"
N_WORKERS      <- 20L
# Regulatory outcomes (rate_violations, rate_ss) dropped: they are
# externally-assessed inspector counts, not self-reported safety events, and
# behave like covariates rather than climate-responsive outcomes (climate
# consistently HURT their out-of-sample fit in panel CV). To re-enable, append
# "rate_violations", "rate_ss" — their specs remain registered downstream.
# Detectability tiers (rate_t1/t2/t3) decompose the contested "injuries" cell
# onto the DEGREE_INJURY severity ladder (supplemental design §3.1). rate_t0
# (accident-only anchor) is off by default; append it to test t0_inclusion.
PANEL_OUTCOMES <- c("rate_accidents", "rate_injuries", "rate_fatal",
                    "rate_days_lost",
                    "rate_t1", "rate_t2", "rate_t3")

# Run isolation (defaults preserve the original behavior). Override via env:
#   MSHA_MV_TAG=detectability  -> writes panel_mv_results_<cm>_detectability.parquet
#                                 (+ a matching checkpoint dir), leaving the locked
#                                 anchor-paper artifacts untouched.
#   MSHA_MV_COMMODITIES=coal   -> restrict the commodity loop (comma-separated).
RUN_TAG  <- Sys.getenv("MSHA_MV_TAG", "")
.tag_sfx <- if (nzchar(RUN_TAG)) paste0("_", RUN_TAG) else ""
RUN_COMMODITIES <- {
  e <- Sys.getenv("MSHA_MV_COMMODITIES", "")
  if (nzchar(e)) trimws(strsplit(e, ",")[[1]]) else COMMODITIES
}
# MSHA_MV_OUTCOMES=rate_t0  -> restrict the outcome set (comma-separated).
RUN_OUTCOMES <- {
  e <- Sys.getenv("MSHA_MV_OUTCOMES", "")
  if (nzchar(e)) trimws(strsplit(e, ",")[[1]]) else PANEL_OUTCOMES
}
# MSHA_SUBCANVASS=Metal  -> restrict to mines whose primary_canvass is in this
# list (comma-separated; from mines_master.parquet {Coal,Metal,Nonmetal,Stone,
# SandAndGravel}). Disaggregates the heterogeneous MNM commodity into its
# operationally-distinct sub-types. When set without an explicit MSHA_MV_TAG,
# the sub-canvass label becomes the output tag, so pooled-commodity artifacts
# are never overwritten. Typical Metal run:
#   MSHA_MV_COMMODITIES=mnm MSHA_SUBCANVASS=Metal Rscript msha/1_msha_panel_multiverse.R
SUBCANVASS <- {
  e <- Sys.getenv("MSHA_SUBCANVASS", "")
  if (nzchar(e)) trimws(strsplit(e, ",")[[1]]) else NULL
}
if (!is.null(SUBCANVASS) && !nzchar(RUN_TAG)) {
  RUN_TAG  <- tolower(paste(SUBCANVASS, collapse = "_"))
  .tag_sfx <- paste0("_", RUN_TAG)
}


# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n=== Loading MSHA panel data ===\n")

msha_events_all <- arrow::read_parquet(EVENTS_PATH) %>%
  mutate(mine_id = as.character(mine_id),
         eid = as.character(eid),
         event_date = as.Date(accident_date),
         commodity = tolower(trimws(commodity))) %>%
  filter(!is.na(mine_id), !is.na(event_date), commodity %in% COMMODITIES)

ops_all <- arrow::read_parquet(OPS_PATH) %>%
  mutate(mine_id = as.character(mine_id),
         commodity = tolower(trimws(commodity)))

covariates_all <- arrow::read_parquet(COVARIATES_PATH) %>%
  mutate(mine_id = as.character(mine_id))

# Optional sub-commodity (primary_canvass) restriction, applied up front so the
# commodity-filtered events_c/ops_c handed to workers are already scoped.
if (!is.null(SUBCANVASS)) {
  .canvass_mines <- arrow::read_parquet(file.path(DATA_DIR, "mines_master.parquet")) %>%
    mutate(mine_id = as.character(mine_id)) %>%
    filter(primary_canvass %in% SUBCANVASS) %>%
    pull(mine_id) %>% unique()
  .n0 <- nrow(msha_events_all)
  msha_events_all <- msha_events_all %>% filter(mine_id %in% .canvass_mines)
  ops_all         <- ops_all         %>% filter(mine_id %in% .canvass_mines)
  cat(sprintf("  [SUBCANVASS=%s -> tag '%s'] kept %s of %s events across %s mines\n",
              paste(SUBCANVASS, collapse = ","), RUN_TAG,
              format(nrow(msha_events_all), big.mark = ","),
              format(.n0, big.mark = ","),
              format(length(.canvass_mines), big.mark = ",")))
}

cat(sprintf("  events: %s; ops mine-quarters: %s; covariate mine-quarters: %s\n",
            format(nrow(msha_events_all), big.mark = ","),
            format(nrow(ops_all), big.mark = ","),
            format(nrow(covariates_all), big.mark = ",")))


# =============================================================================
# PER-COMMODITY LOOP
# =============================================================================

results_all <- list()

for (cm in RUN_COMMODITIES) {
  cat(sprintf("\n\n#################  MSHA %s — panel MV%s  #################\n",
              toupper(cm), if (nzchar(RUN_TAG)) paste0(" [", RUN_TAG, "]") else ""))

  events_c <- msha_events_all %>% filter(commodity == cm)
  ops_c    <- ops_all %>% filter(commodity == cm)
  cov_c    <- covariates_all   # keyed by mine_id; commodity filter via events

  output_pq      <- file.path(OUTPUT_BASE, glue::glue("panel_mv_results_{cm}{.tag_sfx}.parquet"))
  checkpoint_dir <- file.path(OUTPUT_BASE, glue::glue("panel_checkpoints_{cm}{.tag_sfx}"))
  dir.create(dirname(output_pq), recursive = TRUE, showWarnings = FALSE)

  panel_data_fn <- function(parquet_path, config_id) {
    prepare_msha_panel_data(
      parquet_path  = parquet_path,
      config_id     = config_id,
      events_df     = events_c,
      ops_raw       = ops_c,
      covariates_df = cov_c,
      min_reports   = MIN_REPORTS_DEFAULT,
      max_holdover_days = 365L,
      climate_base  = "overall_final_score"
    )
  }

  fit_fns <- setNames(lapply(RUN_OUTCOMES, function(oc) {
    force(oc)
    function(panel, cv, cid, outcome) {
      fit_single_msha_panel_model(panel, cv, cid, outcome = oc)
    }
  }), RUN_OUTCOMES)

  res_c <- run_panel_multiverse(
    cfg_dir        = CFG_DIR,
    panel_data_fn  = panel_data_fn,
    fit_fns        = fit_fns,
    outcomes       = RUN_OUTCOMES,
    config_ids     = NULL,
    n_workers      = N_WORKERS,
    output_path    = output_pq,
    checkpoint_dir = checkpoint_dir,
    worker_source_files = c(
      "msha/config.R",
      "common/data_prep.R",
      "common/panel_data_prep.R",
      "msha/panel_data_prep.R",
      "msha/panel_fit_models.R",
      "common/panel_multiverse_runner.R"
    ),
    worker_globals = list(
      events_c      = events_c,
      ops_c         = ops_c,
      cov_c         = cov_c,
      panel_data_fn = panel_data_fn,
      fit_fns       = fit_fns
    )
  )

  results_all[[cm]] <- res_c %>% mutate(commodity = cm)
}

cat("\n=== MSHA Panel Multiverse complete ===\n")
combined <- bind_rows(results_all)
cat("\nStatus by commodity × outcome:\n")
print(combined %>% count(commodity, outcome, status))
