# ==============================================================================
# Main script for running MV, CV, and post-processing NRC event data
# ==============================================================================
library(tidyverse)

# ==============================================================================
# Prep data
# ==============================================================================

# # Load event data (from nrc_data_pipeline.py output)
cfg_dir <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_full_run_03-25-2026"
events_dir <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc"
nrc_events <- arrow::read_parquet(
  here::here(events_dir,"events.parquet")
) %>%
  mutate(event_date = as.Date(event_date))
#ops_dir <- "/Users/mrosen44/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/data/processed/nrc"
ops_dir <- here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/","data/processed/nrc")
# # Load operational features (from nrc_operational_data.py output)
skimr::skim(nrc_events)
unique(nrc_events$facility)

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
# ============================================================================
source("nrc/1_nrc_multiverse.R")
output_dir <- "results/nrc/mv/nrc_multiverse_resultsTEST.parquet"
results <- run_nrc_multiverse(
  cfg_dir      = cfg_dir,
  nrc_events   = nrc_events,
  ops_features = ops_features,
  #config_ids = c(1,2,3), # ,2,3,4,5,6,7,8,57
  outcomes     = c("power_loss_pct", "ordinal_scram", "emerg_binary"),
  n_workers    = 22L,
  output_path  = glue::glue(output_dir)#,
  #min_reports = 100
)

# ==============================================================================
# Multiverse CV analysis for NRC
# ==============================================================================
source("nrc/2_nrc_cv.R")

cv_results <- run_nrc_cv(
  cfg_dir      = cfg_dir,
  nrc_events   = nrc_events,
  ops_features = ops_features,
  outcomes     = c("power_loss_pct", "emerg_binary","ordinal_scram"),
  n_cores = 22L,
  K = 5,
  n_splits = 4,
  test_duration_months = 6,
  gap_months = 0,
  seed = 42,
  output_path  = "results/nrc/cv/nrc_cv_results.parquet"
)



# tictoc::tic()
# cv_results <- run_nrc_cv(
#   cfg_dir      = cfg_dir,
#   nrc_events   = nrc_events,
#   ops_features = ops_features,
#   n_workers    = 22L,
#   K            = 5,
#   output_path  = "results/nrc/cv/nrc_cv_results.parquet"
# )
# 
# tictoc::toc()
# ==============================================================================
# Plots for NRC MV/CV analysis
# ==============================================================================


source("shared/postprocessing.R")

results <- arrow::read_parquet("results/nrc/mv/nrc_multiverse_results.parquet")
results_full <- link_results_to_config(results, here::here(cfg_dir,"config_registry.csv"))

# Spec curve with panels
plot_spec_curve_with_panels(results_full, outcome_filter = "power_loss_pct",
                            #component = 'zi',
                            component = 'cond',
                            decision_vars = c("window_type", "window_size",
                                              #"lag_days", 
                                              "halflife_days",
                                              "embedding_model", "sent_method",
                                              "comp__method"))

plot_spec_curve_with_panels(results_full, outcome_filter = "emerg_binary",
                            #component = 'zi',
                            #component = 'cond',
                            decision_vars = c("window_type", "window_size",
                                              #"lag_days", 
                                              "halflife_days",
                                              "embedding_model", "sent_method",
                                              "comp__method"))
plot_spec_curve_with_panels(results_full, outcome_filter = "ordinal_scram",
                            #component = 'zi',
                            #component = 'cond',
                            decision_vars = c("window_type", "window_size",
                                              #"lag_days", 
                                              "halflife_days",
                                              "embedding_model", "sent_method",
                                              "comp__method"))


# Config importance
analyze_config_importance(results_full, outcome_filter = "power_loss_pct", component = 'zi')

# Ops effects
compare_climate_vs_operational(results_full, outcome_filter = "power_loss_pct", component = 'zi')
compare_climate_vs_operational(results_full, outcome_filter = "ordinal_scram")
compare_climate_vs_operational(results_full, outcome_filter = "emerg_binary")

summarize_climate_robustness(df = results_full, component = 'zi')




# Check what's in ops_features
print(NRC_OPS_VARS)

# Check which are found
available_vars <- intersect(NRC_OPS_VARS, names(ops_features))
cat("Available vars:", paste(available_vars, collapse = ", "), "\n")

# Check if between/within already exist
has_decomposed <- any(grepl("_between$", names(ops_features)))
cat("Already decomposed:", has_decomposed, "\n")

# Check what between/within columns exist after prep
cfg_path <- file.path(cfg_dir, "_cfg", "0", "results.parquet")
df <- prepare_nrc_config_data(cfg_path, "0", nrc_events, ops_features)

bw_cols <- grep("_(between|within)$", names(df), value = TRUE)
cat("Between/within columns in prepped data:\n")
print(bw_cols)

# Check for NAs
for (col in bw_cols) {
  cat(sprintf("  %s: %d non-NA / %d total\n", col, sum(!is.na(df[[col]])), nrow(df)))
}


# Delete old NRC CV checkpoints so they get re-run
unlink("results/nrc/cv_checkpoints", recursive = TRUE)
