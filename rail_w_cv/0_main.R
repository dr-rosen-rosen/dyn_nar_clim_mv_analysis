source('2_rail_cv_binary.R')
source('3_rail_cv_plots.R')

outcome <- 'fatalities'
ops_path <- "data/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"
cfg_dir <- "/Volumes/calculon/event_reporting/rail_02-18-2026"
# cfg_dir <- "/Volumes/calculon/event_reporting/rail_full_run_02-04-2026"
output_dir <- "results/cv/{outcome}"

rail_raw <- arrow::read_parquet(glue::glue('{cfg_dir}/events.parquet')) |>
  rename('total_persons_killed' = 'Total Persons Killed',
         'total_persons_injured' = 'Total Persons Injured',
         'total_damage_cost' = 'Total Damage Cost') |>
  select(org_id,eid,event_date,total_persons_killed,total_persons_injured,total_damage_cost)



run_cv_all_configs(
  cfg_dir = cfg_dir,
  rail_raw = rail_raw,
  ops_path = ops_path,  
  outcome = outcome,
  config_ids = c(1,2,3,4,5,100,101,102,103,104,105,277,278,279),
  # config_ids = c(277,278,279,280,281,282,283,284,285,286),
  n_cores = 8,
  output_dir = glue::glue(output_dir),
  K = 5, 
  n_splits = 4,
  test_duration_months = 6,
  gap_months = 0,
  seed = 42
)

generate_cv_plots(
  cv_results_path = glue::glue("results/cv/{outcome}/rail_{outcome}_cv_results.parquet"),
  output_dir = "results/cv/plots",
  outcome = outcome
)
