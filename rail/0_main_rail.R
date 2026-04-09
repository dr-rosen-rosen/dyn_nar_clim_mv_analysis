source('rail/1_rail_multiverse.R')
source('rail/2_rail_cv.R')
source('shared/postprocessing.R')


ops_path <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"
cfg_dir <- "/Volumes/calculon/event_reporting/rail_02-18-2026"
rail_raw <- arrow::read_parquet(glue::glue('{cfg_dir}/events.parquet')) |>
  rename('total_persons_killed' = 'Total Persons Killed',
         'total_persons_injured' = 'Total Persons Injured',
         'total_damage_cost' = 'Total Damage Cost') |>
  mutate(total_damage_cost = as.numeric(gsub("[^0-9.]", "", total_damage_cost))) |>
  select(org_id,eid,event_date,total_persons_killed,total_persons_injured,total_damage_cost)

unique(rail_raw$org_id)
skimr::skim(rail_raw)
############ Regular MV

run_rail_multiverse(
  cfg_dir = cfg_dir,
  rail_raw = rail_raw,
  ops_path = ops_path,
  output_dir = 'results/rail/',
  n_cores = 22L
)


############ CV models

run_cv_all_configs(
  cfg_dir = cfg_dir,
  rail_raw = rail_raw,
  ops_path = ops_path,
  output_dir = 'results/rail/',
  n_cores = 22L,
  K = 5,
  n_splits = 4,
  test_duration_months = 6,
  gap_months = 0,
  seed = 42
)


############ Plots
source("shared/postprocessing.R")
source("shared/generate_mv_report.R")

industries <- list(
  rail = industry_config(
    mv_results      = "results/rail/rail_multiverse_results.parquet",
    cv_results      = "results/rail/rail_cv_results.parquet",
    config_registry = here::here(cfg_dir,'config_registry.csv'),
    label           = "Rail (FRA)",
    decision_vars   = c("window_type", "embedding_model", "sent_method",
                         "comp__method", "lag_days", "halflife_days"),
    ops_vars        = c("train_miles", "passenger_miles", "staff_hours")
  )#,
  # nrc = industry_config(
  #   mv_results      = "results/nrc/nrc_mv_results.parquet",
  #   cv_results      = "results/nrc/nrc_cv_results.parquet",
  #   config_registry = "results/nrc/config_registry.csv",
  #   label           = "Nuclear (NRC)",
  #   decision_vars   = c("window_type", "embedding_model", "sent_method",
  #                        "comp__method"),
  #   ops_vars        = c("capacity_factor", "power_std")
  # )
)
#
# # Generate everything
generate_mv_report(industries, output_dir = "report_figures/",
                    qmd_template = "shared/mv_report_template.qmd")
#
# # Just regenerate figures, don't re-render
# generate_mv_report(industries, render = FALSE)
#
# # Just re-render with existing figures
# generate_mv_report(industries, regenerate_figures = FALSE)

