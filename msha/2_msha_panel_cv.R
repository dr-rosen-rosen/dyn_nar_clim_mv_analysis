# =============================================================================
# msha/2_msha_panel_cv.R — MSHA Panel-Rate Cross-Validation
# =============================================================================
#
# For each commodity (coal, mnm) and panel outcome, fits three nested tiers
# (intercept_only, seasonal_ops, climate) under group_kfold and timeseries CV.
# Primary metric: held-out log-likelihood per observation.
#
# Run from project root: Rscript msha/2_msha_panel_cv.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(furrr)
  library(glmmTMB)
  library(slider)
  library(lubridate)
})

source("msha/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("msha/panel_data_prep.R")
source("msha/panel_fit_models.R")
source("common/panel_cv_runner.R")


# =============================================================================
# PATHS
# =============================================================================

CFG_DIR         <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026"
DATA_DIR        <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha"
EVENTS_PATH     <- file.path(DATA_DIR, "events.parquet")
OPS_PATH        <- file.path(DATA_DIR, "ops_mine_quarter.parquet")
COVARIATES_PATH <- file.path(DATA_DIR, "covariates_mine_quarter.parquet")

OUTPUT_BASE <- "results_new_new/msha"
N_WORKERS   <- 20L

# Mine-quarter cadence (same as NRC). ~quarterly panel.
# Regulatory outcomes (rate_violations, rate_ss) dropped: they are
# externally-assessed inspector counts, not self-reported safety events, and
# behave like covariates rather than climate-responsive outcomes (climate
# consistently HURT their out-of-sample fit in panel CV). To re-enable, append
# "rate_violations", "rate_ss" — their specs remain registered in
# outcome_specs_all below.
PANEL_OUTCOMES <- c("rate_accidents", "rate_injuries", "rate_fatal",
                    "rate_days_lost")

# --- Run isolation + scoping (defaults preserve original behavior) ---
# MSHA_MV_TAG=detectability -> writes panel_cv_results_<cm>_detectability.parquet
# MSHA_MV_COMMODITIES=coal  -> restrict commodity loop (comma-separated)
# MSHA_MV_OUTCOMES=rate_t0,rate_t1,rate_t2,rate_t3 -> restrict outcomes
# MSHA_CV_CONFIG_STRIDE=3   -> 1-in-N config subsample (fast read)
RUN_TAG  <- Sys.getenv("MSHA_MV_TAG", "")
.tag_sfx <- if (nzchar(RUN_TAG)) paste0("_", RUN_TAG) else ""
RUN_COMMODITIES <- {
  e <- Sys.getenv("MSHA_MV_COMMODITIES", "")
  if (nzchar(e)) trimws(strsplit(e, ",")[[1]]) else COMMODITIES
}
.outcome_override <- {
  e <- Sys.getenv("MSHA_MV_OUTCOMES", "")
  if (nzchar(e)) trimws(strsplit(e, ",")[[1]]) else NULL
}
if (!is.null(.outcome_override)) PANEL_OUTCOMES <- .outcome_override
CV_CONFIG_STRIDE <- {
  e <- suppressWarnings(as.integer(Sys.getenv("MSHA_CV_CONFIG_STRIDE", "")))
  if (is.na(e) || e < 1L) 1L else e
}
# MSHA_SUBCANVASS=Metal -> restrict to mines whose primary_canvass is in this
# list (comma-separated; from mines_master.parquet). Disaggregates MNM into its
# operationally-distinct sub-types. When set without an explicit MSHA_MV_TAG,
# the sub-canvass label becomes the output tag, so pooled artifacts are never
# overwritten. Typical Metal run:
#   MSHA_MV_COMMODITIES=mnm MSHA_SUBCANVASS=Metal Rscript msha/2_msha_panel_cv.R
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


# =============================================================================
# OUTCOME SPECS
# =============================================================================
.msha_bw <- c("hours_within")

.spec <- function(outcome_var, family) {
  list(outcome_var = outcome_var, exposure = "hours_worked",
       family = family, bw_terms = .msha_bw,
       org_var = "mine_id", period_date_var = "quarter_start")
}

outcome_specs_all <- list(
  rate_accidents  = .spec("n_accidents",     "nbinom2"),
  rate_injuries   = .spec("n_injuries",      "nbinom2"),
  rate_fatal      = .spec("n_fatal",         "nbinom2"),
  rate_days_lost  = .spec("sum_days_lost",   "tweedie"),
  # Detectability tiers (supplemental design §3.1; see 1_msha_panel_multiverse.R)
  rate_t0         = .spec("n_t0",            "nbinom2"),
  rate_t1         = .spec("n_t1",            "nbinom2"),
  rate_t2         = .spec("n_t2",            "nbinom2"),
  rate_t3         = .spec("n_t3",            "nbinom2"),
  rate_violations = .spec("violation_count", "nbinom2"),
  rate_ss         = .spec("ss_count",        "nbinom2")
)
outcome_specs <- outcome_specs_all[PANEL_OUTCOMES]


# =============================================================================
# PER-COMMODITY LOOP
# =============================================================================

results_all <- list()

# Config subsample for fast reads: take every CV_CONFIG_STRIDE-th config id.
.cv_config_ids <- NULL
if (CV_CONFIG_STRIDE > 1L) {
  .all_ids <- list.dirs(file.path(CFG_DIR, "_cfg"), full.names = FALSE, recursive = FALSE)
  .all_ids <- .all_ids[.all_ids != "" & !startsWith(.all_ids, ".")]
  .all_ids <- .all_ids[order(suppressWarnings(as.integer(.all_ids)))]
  .cv_config_ids <- .all_ids[seq(1L, length(.all_ids), by = CV_CONFIG_STRIDE)]
  cat(sprintf("Config subsample: %d of %d configs (stride %d)\n",
              length(.cv_config_ids), length(.all_ids), CV_CONFIG_STRIDE))
}

for (cm in RUN_COMMODITIES) {
  cat(sprintf("\n\n#################  MSHA %s — panel CV%s  #################\n",
              toupper(cm), if (nzchar(RUN_TAG)) paste0(" [", RUN_TAG, "]") else ""))

  events_c <- msha_events_all %>% filter(commodity == cm)
  ops_c    <- ops_all %>% filter(commodity == cm)
  cov_c    <- covariates_all

  output_pq <- file.path(OUTPUT_BASE, glue::glue("panel_cv_results_{cm}{.tag_sfx}.parquet"))
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

  res_c <- run_panel_cv(
    cfg_dir        = CFG_DIR,
    panel_data_fn  = panel_data_fn,
    outcome_specs  = outcome_specs,
    config_ids     = .cv_config_ids,
    strategies     = c("group_kfold", "timeseries"),
    K = 5L, n_splits = 5L,
    test_duration_months = 24L, gap_months = 6L,
    n_workers   = N_WORKERS,
    output_path = output_pq,
    worker_source_files = c(
      "msha/config.R",
      "common/data_prep.R",
      "common/panel_data_prep.R",
      "msha/panel_data_prep.R",
      "msha/panel_fit_models.R",
      "common/panel_cv_runner.R"
    ),
    worker_globals = list(
      events_c      = events_c,
      ops_c         = ops_c,
      cov_c         = cov_c,
      panel_data_fn = panel_data_fn
    )
  )

  results_all[[cm]] <- res_c %>% mutate(commodity = cm)
}

cat("\n=== MSHA Panel CV complete ===\n")
combined <- bind_rows(results_all)
cat("\nDelta-loglik (climate vs seasonal_ops) by commodity × outcome × strategy:\n")
print(combined %>%
        filter(model_label == "delta_climate_vs_seasonal") %>%
        group_by(commodity, panel_outcome, cv_strategy) %>%
        summarise(median_delta_ll = median(loglik_mean, na.rm = TRUE),
                  pct_positive = mean(loglik_mean > 0, na.rm = TRUE),
                  n = n(), .groups = "drop"))
