# =============================================================================
# msha/_metal_cv_batched.R — durable, resumable Metal (MNM) panel CV
# =============================================================================
# The stock 2_msha_panel_cv.R runs one future_map over ALL configs and writes
# only at the very end, so any interruption loses everything. This wrapper runs
# the SAME run_panel_cv in config CHUNKS, writing one parquet per chunk under a
# checkpoint dir. Completed chunks persist across kills; re-running skips chunks
# whose output already exists (resume). A final aggregate is rebuilt from all
# chunk files after every chunk, so a partial run still yields a usable parquet.
#
# Scope: MNM commodity, Metal sub-canvass, base outcomes only.
# Run (background-safe):  Rscript msha/_metal_cv_batched.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(furrr); library(glmmTMB)
  library(slider); library(lubridate); library(glue)
})

source("msha/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("msha/panel_data_prep.R")
source("msha/panel_fit_models.R")
source("common/panel_cv_runner.R")

CFG_DIR  <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026"
DATA_DIR <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha"
OUTPUT_BASE <- "results_new_new/msha"
N_WORKERS   <- 16L
CHUNK_SIZE  <- 3L    # small chunks -> checkpoint every ~2-3 min; kill-resilient
STRIDE      <- 3L    # 1-in-3 config subsample (45 of 135) — enough for the OOS
                     # go/no-go gate; rerun full (STRIDE=1) for paper champions.
SUBCANVASS  <- "Metal"
PANEL_OUTCOMES <- c("rate_accidents", "rate_injuries", "rate_fatal", "rate_days_lost")

ck_dir     <- file.path(OUTPUT_BASE, "cv_checkpoints_mnm_metal")
output_pq  <- file.path(OUTPUT_BASE, "panel_cv_results_mnm_metal.parquet")
dir.create(ck_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load + filter to MNM / Metal (mirrors 2_msha_panel_cv.R + SUBCANVASS) ---
canvass_mines <- read_parquet(file.path(DATA_DIR, "mines_master.parquet")) %>%
  mutate(mine_id = as.character(mine_id)) %>%
  filter(primary_canvass %in% SUBCANVASS) %>% pull(mine_id) %>% unique()

events_c <- read_parquet(file.path(DATA_DIR, "events.parquet")) %>%
  mutate(mine_id = as.character(mine_id), eid = as.character(eid),
         event_date = as.Date(accident_date), commodity = tolower(trimws(commodity))) %>%
  filter(!is.na(mine_id), !is.na(event_date), commodity == "mnm", mine_id %in% canvass_mines)
ops_c <- read_parquet(file.path(DATA_DIR, "ops_mine_quarter.parquet")) %>%
  mutate(mine_id = as.character(mine_id), commodity = tolower(trimws(commodity))) %>%
  filter(commodity == "mnm", mine_id %in% canvass_mines)
cov_c <- read_parquet(file.path(DATA_DIR, "covariates_mine_quarter.parquet")) %>%
  mutate(mine_id = as.character(mine_id))
cat(sprintf("Metal: %s events, %s ops mine-quarters, %s mines\n",
            format(nrow(events_c), big.mark=","), format(nrow(ops_c), big.mark=","),
            format(length(canvass_mines), big.mark=",")))

.msha_bw <- c("hours_within")
.spec <- function(v, fam) list(outcome_var=v, exposure="hours_worked", family=fam,
                               bw_terms=.msha_bw, org_var="mine_id", period_date_var="quarter_start")
outcome_specs <- list(
  rate_accidents = .spec("n_accidents","nbinom2"), rate_injuries = .spec("n_injuries","nbinom2"),
  rate_fatal = .spec("n_fatal","nbinom2"),         rate_days_lost = .spec("sum_days_lost","tweedie")
)[PANEL_OUTCOMES]

panel_data_fn <- function(parquet_path, config_id) {
  prepare_msha_panel_data(parquet_path=parquet_path, config_id=config_id,
    events_df=events_c, ops_raw=ops_c, covariates_df=cov_c,
    min_reports=MIN_REPORTS_DEFAULT, max_holdover_days=365L, climate_base="overall_final_score")
}

# --- Chunk the config ids; skip chunks already on disk (resume) ---
all_ids <- list.dirs(file.path(CFG_DIR, "_cfg"), full.names=FALSE, recursive=FALSE)
all_ids <- all_ids[all_ids != "" & !startsWith(all_ids, ".")]
all_ids <- all_ids[order(suppressWarnings(as.integer(all_ids)))]
if (STRIDE > 1L) all_ids <- all_ids[seq(1L, length(all_ids), by = STRIDE)]
chunks  <- split(all_ids, ceiling(seq_along(all_ids) / CHUNK_SIZE))
cat(sprintf("%d configs -> %d chunks of <=%d\n", length(all_ids), length(chunks), CHUNK_SIZE))

for (ci in seq_along(chunks)) {
  chunk_pq <- file.path(ck_dir, sprintf("chunk_%03d.parquet", ci))
  if (file.exists(chunk_pq)) { cat(sprintf("[chunk %d/%d] exists, skip\n", ci, length(chunks))); next }
  cat(sprintf("[chunk %d/%d] configs %s  %s\n", ci, length(chunks),
              paste(range(chunks[[ci]]), collapse="-"), format(Sys.time(), "%H:%M:%S")))
  res <- run_panel_cv(
    cfg_dir=CFG_DIR, panel_data_fn=panel_data_fn, outcome_specs=outcome_specs,
    config_ids=chunks[[ci]], strategies=c("group_kfold","timeseries"),
    K=5L, n_splits=5L, test_duration_months=24L, gap_months=6L,
    n_workers=N_WORKERS, output_path=NULL,
    worker_source_files=c("msha/config.R","common/data_prep.R","common/panel_data_prep.R",
      "msha/panel_data_prep.R","msha/panel_fit_models.R","common/panel_cv_runner.R"),
    worker_globals=list(events_c=events_c, ops_c=ops_c, cov_c=cov_c, panel_data_fn=panel_data_fn))
  write_parquet(res %>% mutate(commodity="mnm", subcanvass="Metal"), chunk_pq)
  # Rebuild aggregate after each chunk so partial runs are usable.
  agg <- map_dfr(list.files(ck_dir, "^chunk_.*parquet$", full.names=TRUE), read_parquet)
  write_parquet(agg, output_pq)
  cat(sprintf("  aggregate now %s rows across %d chunks\n",
              format(nrow(agg), big.mark=","), length(list.files(ck_dir, "chunk_"))))
}

cat("\n=== Metal CV complete ===\n")
final <- read_parquet(output_pq)
print(final %>% filter(model_label=="delta_climate_vs_seasonal") %>%
        group_by(panel_outcome, cv_strategy) %>%
        summarise(median_dll=median(loglik_mean, na.rm=TRUE),
                  pct_pos=mean(loglik_mean>0, na.rm=TRUE), n=n(), .groups="drop"))
