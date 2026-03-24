# ==============================================================================
# Main script for running MV, CV, and post-processing NRC event data
# ==============================================================================
library(tidyverse)

# ==============================================================================
# Prep data
# ==============================================================================

# # Load event data (from nrc_data_pipeline.py output)
cfg_dir <- "/Volumes/calculon/event_reporting/nrc_full_run_02-17-2026"
nrc_events <- arrow::read_parquet(
  here::here(cfg_dir,"events.parquet")
) %>%
  mutate(event_date = as.Date(event_date))
ops_dir <- "/Users/mrosen44/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/data/processed/nrc"
# # Load operational features (from nrc_operational_data.py output)

ops_features <- arrow::read_parquet(
  here::here(ops_dir,'power_status_quarterly.parquet/power_status_quarterly.parquet')
) |>
  full_join(
    arrow::read_parquet(
      here::here(ops_dir,'action_matrix_long.parquet')
    ), by = c('facility_unit','year_quarter', 'year','quarter','quarter_start')
  ) |>
  full_join(
    arrow::read_parquet(
      here::here(ops_dir,'findings_quarterly.parquet')
    ), by = c('facility_unit','year_quarter', 'year','quarter','quarter_start')
  )

skimr::skim(ops_features)

# ==============================================================================
# Multiverse analysis for NRC
# ==============================================================================
source("1_nrc_multiverse.R")
# # Run multiverse
tictoc::tic()
results <- run_nrc_multiverse(
  cfg_dir      = cfg_dir,#here::here(cfg_dir,"_cfg"),
  nrc_events   = nrc_events,
  ops_features = ops_features,
  outcomes     = c("binary_scram", "ordinal_scram", "emerg_class"),
  n_workers    = 8L,
  output_path  = "results/nrc/nrc_multiverse_results.parquet"
)
tictoc::toc()
t <- arrow::read_parquet("results/nrc/nrc_multiverse_results.parquet")

# ==============================================================================
# Multiverse CV analysis for NRC
# ==============================================================================
source("2_nrc_cv.R")
tictoc::tic()
cv_results <- run_nrc_cv(
  cfg_dir      = cfg_dir,
  nrc_events   = nrc_events,
  ops_features = ops_features,
  n_workers    = 8L,
  K            = 5,
  output_path  = "results/nrc/nrc_cv_results.parquet"
)
tictoc::toc()
# ==============================================================================
# Plots for NRC MV/CV analysis
# ==============================================================================
source("3_nrc_plots.R")
generate_nrc_plots(
  mv_results_path = "results/nrc/nrc_multiverse_results.parquet",
  # cv_results_path = "results/nrc/nrc_cv_results.parquet",
  output_dir      = "results/nrc/plots"
)
