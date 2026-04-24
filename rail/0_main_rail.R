# =============================================================================
# CONFIGURATION
# =============================================================================

CFG_DIR    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026"             # Climate config parquets
OPS_PATH   <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"
EVENTS_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail"
OUTPUT_PATH <- "results_new_new/rail/mv_results.parquet"

TEMPORAL_DIR <- NULL #"temporal_output/rail"      # NULL to skip temporal features
N_WORKERS   <- 20L


# =============================================================================
# LOAD DATA
# =============================================================================
library(tidyverse)
source('rail/1_rail_multiverse.R')
rail_raw <- arrow::read_parquet(glue::glue('{EVENTS_PATH}/events.parquet')) |>
  rename('total_persons_killed' = 'Total Persons Killed',
         'total_persons_injured' = 'Total Persons Injured',
         'total_damage_cost' = 'Total Damage Cost') |>
  mutate(total_damage_cost = as.numeric(gsub("[^0-9.]", "", total_damage_cost))) |>
  select(org_id,eid,event_date,total_persons_killed,total_persons_injured,total_damage_cost)
# ops_raw  <- arrow::read_parquet(OPS_PATH)
ops_raw <- read_csv(OPS_PATH) %>%
  mutate(across(all_of(OPS_VARS), ~ ifelse(.x < 0, NA_real_, .x)))
# Compute operational features (between/within decomposition)
ops_features <- make_ops_features_rolling(ops_raw)  %>%
  distinct(org_id, yearmonth, .keep_all = TRUE)

class(rail_raw$org_id)
n_distinct(rail_raw$org_id)

# =============================================================================
# Define protocol
# =============================================================================
# This matches the current production approach. Add model_level for
# forward compatibility with the layer system.

# for (outcome in c("injuries", "fatalities")) {
#   config_ids <- discover_config_ids(CFG_DIR, "subdir")
#
#   plan(multisession, workers = N_WORKERS)
#   results <- future_map(config_ids, function(cid) {
#     parquet_path <- get_config_parquet_path(CFG_DIR, cid, "subdir")
#     df <- prepare_config_data(parquet_path, cid, rail_raw, ops_features)
#     climate_vars <- get_climate_vars(df)
#     map(climate_vars, ~ fit_single_rail_model(df, .x, cid, outcome))
#   }, ...)
#
#   combined <- bind_rows(results) %>% mutate(model_level = "M1")
# }


# =============================================================================
# OPTION B: LAYER-AWARE (recommended for new runs)
# =============================================================================

# Build feature layer registry
# tmpl <- rail_protocol_template(temporal_dir = TEMPORAL_DIR)

# Build layers with NRC-specific org_var
# temporal_layer <- create_temporal_layer(
#   burstiness_methods = c("goh", "kimjo_periodic"),
#   fixed_count_windows = c(10L, 20L, 50L),
#   org_var = "facility"
# )
# 
# ews_layer <- create_ews_layer(
#   detrend_methods = c("linear", "none"),
#   window_sizes = c(20L, 50L, 100L),
#   composite_col = "overall_final_score",
#   org_var = "facility"
# )

# temporal_layer <- create_temporal_layer(
#   burstiness_methods = c("goh", "kimjo_periodic"),
#   fixed_count_windows = c(20L),
#   org_var = "org_id"   # or "org_id" for rail
# )
# 
# ews_layer <- create_ews_layer(
#   detrend_methods = c("linear"),
#   window_sizes = c(50L),
#   composite_col = "overall_final_score",
#   org_var = "org_id",
#   formula_vars = c("ews_variance", "ews_ar1")
#   # formula_vars = c("ews_variance", "ews_ar1", "ews_skew", 
#   #                  "ews_dfa_alpha", "ews_spectral_slope")
# )
# registry <- create_layer_registry(
#   temporal = temporal_layer,
#   ews = ews_layer
# )


protocol <- define_validation_protocol(
  industry = "rail",
  label = "U.S. Railroad Safety",
  layer_registry = create_layer_registry(), # no layers, paper 1
  # layer_registry = registry, # layers defined above
  model_hierarchy = define_model_hierarchy(M1 = c("baseline", "climate")), # vestigial, can be dropped
  outcomes = c("injuries", "fatalities"),
  source_files = c("rail/config.R", "rail/data_prep.R", "rail/fit_models.R"),
  outcome_configs = OUTCOME_VARS,
  ops_vars = OPS_VARS,
  org_var = "org_id",
  data_prep_fn = function(parquet_path, config_id, events_df, ops_features, ...) {
    prepare_config_data(parquet_path, config_id, events_df,
                        ops_features = ops_features, ...)
  },
  fit_fn = function(df, climate_var, config_id, outcome, extra_terms = character(0),
                    protocol = NULL) {
    result <- fit_single_rail_model(df, climate_var, config_id, outcome, extra_terms = extra_terms)
    as_tibble(result)
  },
  window_specs = WINDOW_SPECS,
  exclude_outcomes = c("costs")
)
source('rail/1_rail_multiverse.R')
############ Regular MV

run_rail_multiverse(
  cfg_dir = cfg_dir,
  rail_raw = rail_raw,
  ops_path = ops_path,
  output_dir = 'results/rail/',
  n_cores = 22L
)
source("rail/1_rail_multiverse.R")
mv <- arrow::read_parquet("results_new_new/rail/mv_results.parquet")
view(mv)
table(mv$error
      )
names(mv)
skimr::skim(mv)
mv %>% filter(grepl("org_id", error)) %>% select(config_id, climate_var, outcome, temporal_config_id, ews_config_id) %>% head(10)
############ CV models

# run_cv_all_configs(
#   cfg_dir = cfg_dir,
#   rail_raw = rail_raw,
#   ops_path = ops_path,
#   output_dir = 'results/rail/',
#   n_cores = 22L,
#   K = 5,
#   n_splits = 4,
#   test_duration_months = 6,
#   gap_months = 0,
#   seed = 42
# )
source("rail/2_rail_cv.R")
cv <- arrow::read_parquet("results_new_new/rail/cv_results.parquet")
skimr::skim(cv)
view(cv)
############ Plots
source("common/postprocessing.R")
source("common/generate_mv_report.R")

industries <- list(
  rail = industry_config(
    mv_results      = "results_new/rail/mv_multiverse_results.parquet",
    cv_results      = "results_new/rail/cv_results.parquet",
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


# NEW APPROACH WITH LAYERS
# # Generate everything
generate_mv_report(industries, output_dir = "report_figures/",
                    qmd_template = "common/mv_report_template.qmd")
#
# # Just regenerate figures, don't re-render
# generate_mv_report(industries, render = FALSE)
#
# # Just re-render with existing figures
# generate_mv_report(industries, regenerate_figures = FALSE)


# Load and augment results
source("common/postprocessing.R")
source("common/configuration_concordance.R")
source("common/postprocessing_layers.R")   # new
mv <- arrow::read_parquet("results_new/rail/mv_results.parquet")
mv <- augment_layer_facets(mv)
mv <- link_results_to_config(mv, here::here(CFG_DIR,"config_registry.csv"))

# Standard spec curve (existing function, now with layer facets available)
all_facets <- get_all_facets(mv)
plot_spec_curve_with_panels(mv, outcome_filter = "fatalities",
                            decision_vars = all_facets,
                            component = 'zi')

# Layer impact: does adding temporal/EWS change the climate effect?
compare_layer_impact(mv, outcome_filter = "fatalities",component = 'zi')

# Concordance with layer facets included
analyze_configuration_concordance(mv, metric = "climate_estimate",
                                  facets = all_facets,
                                  comnponent = 'zi')

# CV layered delta-Brier
cv <- arrow::read_parquet("results_new/rail/cv_results.parquet")
deltas <- compute_layered_delta_brier(cv)
plot_layered_delta_brier(deltas, facet_by = "comparison")


