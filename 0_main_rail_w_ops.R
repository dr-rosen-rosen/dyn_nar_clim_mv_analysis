# ==============================================================================
# RUN the MV STUDY
# ==============================================================================

# Load MV scripts and settings
# the window parameters are set at the top of this file
source('rail_multiverse_glmmtmb_w_ops.R')

# Load rail data (event-level, irregular time series)
# rail_raw <- read_csv("data/rail/rail_raw.csv")
# print(colnames(rail_raw))
rail_raw <- arrow::read_parquet('/Volumes/calculon/event_reporting/rail_full_run_02-04-2026/events.parquet') |>
  rename('total_persons_killed' = 'Total Persons Killed',
         'total_persons_injured' = 'Total Persons Injured',
         'total_damage_cost' = 'Total Damage Cost') |>
  select(org_id,eid,event_date,total_persons_killed,total_persons_injured,total_damage_cost)

# ==============================================================================
# DATA REQUIREMENTS
# ==============================================================================
# 
# 1. rail_raw (event-level data, irregular time series):
#    - eid: Event ID (for joining with climate scores)
#    - org_id: Organization ID
#    - event_date: Date of the event (can be any day)
#    - total_persons_injured: Outcome variable for injuries
#    - total_persons_killed: Outcome variable for fatalities  
#    - total_damage_cost: Outcome variable for costs
#
# 2. ops_path (monthly operational data, regular time series):
#    Path to CSV or Parquet file containing:
#    - org_id: Organization ID (must match rail_raw)
#    - yearmonth: First day of month (e.g., "2020-01-01" for Jan 2020)
#    - train_miles: Monthly train miles
#    - passenger_miles: Monthly passenger miles
#    - staff_hours: Monthly staff hours
#
# The operational data is processed separately and joined to events by 
# org_id and yearmonth (floor of event_date to month).
# ==============================================================================


# Path to operational data file (set to NULL to run without operational covariates)
ops_path <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260129.csv"  
# ops_df <- format_ops_data(ops_path = ops_path, overwrite = TRUE)
ops_df <- read.csv(ops_path)
# cfg_dir <- "/Volumes/calculon/event_reporting/rail_11-16-2025"
cfg_dir <- "/Volumes/calculon/event_reporting/rail_full_run_02-04-2026"
output_dir <- "results/rail_mv_{outcome}_w_ops"

## #example configs and safety climate data

test <- arrow::read_parquet("/Volumes/calculon/event_reporting/rail_11-16-2025/_cfg/277/results.parquet")
test2 <- arrow::read_parquet("/Volumes/calculon/event_reporting/rail_full_run_02-04-2026/_cfg/494/results.parquet")
cfg_df <- read.csv(paste0(cfg_dir,"/config_registry.csv"))
example_safety_clim <- arrow::read_parquet(paste0(cfg_dir,"/_cfg/227/results.parquet"))

# # Run for injuries
outcome <- "injuries"
tictoc::tic()
results_injuries <- run_rail_multiverse(
  cfg_dir = cfg_dir,
  rail_raw = rail_raw,
  ops_path = ops_path, # NULL runs w/o operational data
  outcome = outcome,
  config_ids = NULL,
  # config_ids = c(0,1,2,3,4),
  # config_ids = sample(
  #   0:(length(list.dirs(path = paste0(cfg_dir,"/_cfg"), full.names = FALSE, recursive = FALSE))-1),
  #   10),# NULL to run all,
  n_cores = 8,
  save_models = FALSE,
  output_dir = glue::glue(output_dir)
)
tictoc::toc()
# 
# # Run for fatalities
outcome <- "fatalities"
tictoc::tic()
results_fatalities <- run_rail_multiverse(
  cfg_dir = cfg_dir,
  rail_raw = rail_raw,
  ops_path = ops_path,
  outcome = outcome,
  config_ids = NULL,
  # config_ids = sample(
  #   0:(length(list.dirs(path = paste0(cfg_dir,"/_cfg"), full.names = FALSE, recursive = FALSE))-1), 
  #   10),# NULL to run all,
  n_cores = 8,
  save_models = FALSE,
  output_dir = glue::glue(output_dir)
)
tictoc::toc()
# 
# # Run for costs
outcome <- "costs"
results_costs <- run_rail_multiverse(
  cfg_dir = cfg_dir,
  rail_raw = rail_raw,
  ops_path = ops_path,
  outcome = outcome,
  config_ids = NULL,
  # config_ids = sample(
  #   0:(length(list.dirs(path = paste0(cfg_dir,"/_cfg"), full.names = FALSE, recursive = FALSE))-1), 
  #   10),# NULL to run all,
  n_cores = 4,
  save_models = FALSE,
  output_dir = glue::glue(output_dir)
)

# ==============================================================================
# DO POST PROCESSSING ON RAIL MV RESULTS
# ==============================================================================

source("rail_multiverse_postprocessing_w_ops.R")

# ==============================================================================
# SCENARIO 1: ANALYZE A SINGLE OUTCOME
# ==============================================================================

# Load results for one outcome (e.g., injuries)
# outcome <- "fatalities"
# outcome <- "costs"
outcome <- "injuries"
mv_results_rail <- arrow::read_parquet(
  glue::glue(output_dir,'/rail_{outcome}_multiverse_results.parquet')
  )

# Link to config data
mv_results_rail <- link_results_to_config_railway(
  mv_results_rail,
  config_registry_path = here::here(cfg_dir,'config_registry.csv')
) |>
  mutate(
    th__method = if_else(comp__apply_over == 'all', 'none',th__method)
  )


# Create specification curve for conditional model
curve_cond <- plot_specification_curve_railway(
  mv_results_rail,
  component = "cond"
)
curve_cond$curve
# ggsave("injuries_cond_spec_curve.png", curve_cond$curve, width = 10, height = 6, dpi = 300)

# Create specification curve for zero-inflation model
curve_zi <- plot_specification_curve_railway(
  mv_results_rail,
  # outcome_filter = outcome_filter,
  component = "zi"
)
curve_zi$curve
ggsave("injuries_zi_spec_curve.png", curve_zi$curve, width = 10, height = 6, dpi = 300)

# Create full specification curve with decision panels
full_curve <- plot_specification_curve_with_panels_railway(
  mv_results_rail,
  outcome_filter = outcome,
  component = "cond"
)
ggsave("injuries_spec_curve_full.png", full_curve, width = 12, height = 10, dpi = 300)

# Compare windows

window_comparison <- plot_window_comparison_railway(mv_results_rail, outcome_filter = outcome)
window_comparison$window_effects

# Analyze which configuration choices matter most
importance <- analyze_config_importance_railway(
  mv_results_rail,
  outcome_filter = outcome,
  component = "cond"
)
print(importance)

# Summarize by specific configuration choices
by_embedding <- summarize_by_config_choice_railway(
  mv_results_rail,
  group_vars = c("embedding_model", "window_type","th__method"),
  outcome_filter = outcome
)
print(by_embedding)

# ==============================================================================
# SCENARIO 2: OPERATIONAL COVARIATE ANALYSIS
# ==============================================================================

# Summarize operational effects across multiverse
ops_summary_cond <- summarize_operational_effects(
  mv_results_rail,
  outcome_filter = outcome,
  component = "cond"
)
print(ops_summary_cond)

ops_summary_zi <- summarize_operational_effects(
  mv_results_rail,
  outcome_filter = outcome,
  component = "zi"
)
print(ops_summary_zi)

# Plot operational effects (conditional model)
ops_plot_cond <- plot_operational_effects(
  mv_results_rail,
  outcome_filter = outcome,
  component = "cond"
)
ops_plot_cond
ggsave(sprintf("%s_operational_effects_cond.png", outcome_filter), 
       ops_plot_cond, width = 10, height = 6, dpi = 300)

# Plot operational effects (zero-inflation model)
ops_plot_zi <- plot_operational_effects(
  mv_results_rail,
  outcome_filter = outcome,
  component = "zi"
)
ops_plot_zi
ggsave(sprintf("%s_operational_effects_zi.png", outcome_filter), 
       ops_plot_zi, width = 10, height = 6, dpi = 300)

# Compare climate vs operational effect sizes
comparison_cond <- compare_climate_vs_operational(
  mv_results_rail,
  outcome_filter = outcome_filter,
  component = "cond"
)
comparison_cond$plot
ggsave(sprintf("%s_climate_vs_ops_cond.png", outcome_filter), 
       comparison_cond$plot, width = 10, height = 6, dpi = 300)
print(comparison_cond$summary)

comparison_zi <- compare_climate_vs_operational(
  mv_results_rail,
  outcome_filter = outcome_filter,
  component = "zi"
)
comparison_zi$plot
ggsave(sprintf("%s_climate_vs_ops_zi.png", outcome_filter), 
       comparison_zi$plot, width = 10, height = 6, dpi = 300)

# Check robustness of climate findings with operational controls
robustness <- summarize_climate_robustness(mv_results_rail, outcome_filter = outcome_filter)
print(robustness)

# ==============================================================================
# SCENARIO 3: COMPARE MULTIPLE OUTCOMES
# ==============================================================================

# Load all three outcomes
results_injuries <- arrow::read_parquet(
  "results/rail_multiverse_injury_2/rail_injuries_multiverse_results.parquet"
)
results_fatalities <- arrow::read_parquet(
  "results/rail_multiverse_fatalities_2/rail_fatalities_multiverse_results.parquet"
)
results_costs <- arrow::read_parquet(
  "results/rail_multiverse_cost_2/rail_costs_multiverse_results.parquet"
)

# Combine all outcomes
results_combined <- bind_rows(
  results_injuries,
  results_fatalities,
  results_costs
)

# Link to config data
results_combined_full <- link_results_to_config_railway(
  results_combined,
  config_registry_path = "/Volumes/calculon/event_reporting/rail_11-16-2025/config_registry.csv"
) |>
  mutate(
    th__method = if_else(comp__apply_over == 'all', 'none', th__method)
  )

# Compare outcomes
outcome_plots <- plot_outcome_comparison(results_combined_full)
outcome_plots$conditional
outcome_plots$zero_inflation
outcome_plots$significance

# Compare operational effects across outcomes
for (out in c("injuries", "fatalities", "costs")) {
  cat(sprintf("\n=== Operational Effects: %s ===\n", out))
  ops_sum <- summarize_operational_effects(results_combined_full, outcome_filter = out, component = "cond")
  print(ops_sum)
}

# ==============================================================================
# SCENARIO 4: FOCUS ON SPECIFIC CONFIGURATIONS
# ==============================================================================

# Filter to specific configurations of interest
# For example, only models using BAAI embedding and distilbert sentiment
results_filtered <- results_combined_full %>%
  filter(
    embedding_model == "BAAI/bge-large-en-v1.5",
    sent_method == "distilbert"
  )

# Create curves for this subset
filtered_curves <- plot_all_specification_curves_railway(results_filtered)
filtered_curves$injuries_cond

# Compare operational effects in filtered subset
ops_filtered <- summarize_operational_effects(
  results_filtered,
  outcome_filter = "injuries",
  component = "cond"
)
print(ops_filtered)
