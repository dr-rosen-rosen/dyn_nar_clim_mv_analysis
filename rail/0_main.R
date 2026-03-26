source('rail/1_rail_multiverse.R')
source('rail/2_rail_cv.R')
source('shared/postprocessing.R')


ops_path <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"
cfg_dir <- "/Volumes/calculon/event_reporting/rail_02-18-2026"
# cfg_dir <- "/Volumes/calculon/event_reporting/rail_full_run_02-04-2026"


# ops_df <- format_ops_data(ops_path = ops_path, overwrite = TRUE)

rail_raw <- arrow::read_parquet(glue::glue('{cfg_dir}/events.parquet')) |>
  rename('total_persons_killed' = 'Total Persons Killed',
         'total_persons_injured' = 'Total Persons Injured',
         'total_damage_cost' = 'Total Damage Cost') |>
  select(org_id,eid,event_date,total_persons_killed,total_persons_injured,total_damage_cost)

############ Regular MV
outcome <- 'fatalities'
output_dir <- "results/rail/{outcome}/mv/"
run_rail_multiverse(cfg_dir = cfg_dir,
                    rail_raw = rail_raw,
                    ops_path = ops_path,
                    outcome = outcome,
                    config_ids = NULL,
                    n_cores = 20,
                    save_models = FALSE,
                    output_dir = glue::glue(output_dir))


############ CV models

run_cv_all_configs(
  cfg_dir = cfg_dir,
  rail_raw = rail_raw,
  ops_path = ops_path,  
  outcome = outcome,
  config_ids = NULL,
  # config_ids = c(1,2,3,4),
  n_cores = 20,
  output_dir = glue::glue(output_dir),
  K = 5, 
  n_splits = 4,
  test_duration_months = 6,
  gap_months = 0,
  seed = 42
)



############ Plots
results <- arrow::read_parquet("results/rail/{outcome}/mv/rail_multiverse_results.parquet")
results_full <- link_results_to_config(results, here::here(cfg_dir,"config_registry.csv"))
# Spec curve with panels
plot_spec_curve_with_panels(results_full, outcome_filter = "ordinal_scram",#"emerg_class",
                            decision_vars = c("window_type", "embedding_model", 
                                              "sent_method", "comp__method",
                                              "lag_days", "halflife_days"))

# Config importance
analyze_config_importance(results_full, outcome_filter = "ordinal_scram")
# Ops effects
compare_climate_vs_operational(results_full, outcome_filter = "ordinal_scram")
generate_cv_plots(
  cv_results_path = glue::glue("results/cv/{outcome}/rail_{outcome}_cv_results.parquet"),
  output_dir = "results/cv/plots",
  outcome = outcome
)
