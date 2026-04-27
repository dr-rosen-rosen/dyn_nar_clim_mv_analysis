# ==============================================================================
# Main script for running MV, CV, and post-processing NRC event data
# ==============================================================================
library(tidyverse)
source('nrc/config.R')
# =============================================================================
# CONFIGURATION
# =============================================================================


CFG_DIR      <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026"
EVENTS_PATH  <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet"
OUTPUT_PATH     <- "results_new_new/nrc/mv_results.parquet"
OPS_PATH  <- here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/","data/processed/nrc")

TEMPORAL_DIR <- NULL #"temporal_output/nrc"  # NULL to skip
N_WORKERS    <- 20L


# =============================================================================
# LOAD DATA
# =============================================================================

nrc_events   <- arrow::read_parquet(EVENTS_PATH) %>%
  mutate(event_date = as.Date(event_date),
         eid = as.character(event_num))
ops_features <-arrow::read_parquet(
  here::here(OPS_PATH,'power_status_quarterly.parquet/power_status_quarterly.parquet')
) |>
  full_join(
    arrow::read_parquet(
      here::here(OPS_PATH,'action_matrix_long.parquet')
    ), by = c('facility_unit','year_quarter', 'year','quarter','quarter_start')
  ) |>
  full_join(
    arrow::read_parquet(
      here::here(OPS_PATH,'findings_quarterly.parquet')
    ), by = c('facility_unit','year_quarter', 'year','quarter','quarter_start')
  )


# =============================================================================
# RUN ANALYSIS
# =============================================================================

tmpl <- nrc_protocol_template(temporal_dir = TEMPORAL_DIR)

protocol <- define_validation_protocol(
  industry = "nrc",
  label = "U.S. Nuclear (NRC)",
  layer_registry = create_layer_registry(),
  model_hierarchy = define_model_hierarchy(M1 = c("baseline", "climate")),
  outcomes = c("pct_power_loss", "emerg_binary"),
  outcome_configs = OUTCOME_VARS,
  ops_vars = NRC_OPS_VARS,
  org_var = "facility",
  cv = list(
    strategies = c("group_kfold", "timeseries"),
    require_both = FALSE,
    baseline_strategy = "best_non_climate",
    K = 5L,
    n_splits = 5L,
    test_duration_months = 24L,
    gap_months = 6L,
    cv_outcomes = c("emerg_binary")   # only binary outcomes for CV
  ),
  data_prep_fn = local({
    fn <- prepare_nrc_config_data
    function(parquet_path, config_id, events_df, ops_features, ...) {
      fn(parquet_path, config_id, events_df,
         ops_features = ops_features, ...)
    }
  }),
  fit_fn = list(
    pct_power_loss = function(df, climate_var, config_id, outcome,
                              extra_terms = character(0), protocol = NULL) {
      fit_power_loss_hurdle(df, climate_var, config_id, extra_terms = extra_terms)
    },
    emerg_binary = function(df, climate_var, config_id, outcome,
                            extra_terms = character(0), protocol = NULL) {
      fit_binary_emerg(df, climate_var, config_id, extra_terms = extra_terms)
    },
    ordinal_scram = function(df, climate_var, config_id, outcome,
                             extra_terms = character(0), protocol = NULL) {
      fit_ordinal_model(df, climate_var, config_id, extra_terms = extra_terms)
    }
  ),
  window_specs = WINDOW_SPECS,
  min_reports = MIN_REPORTS_DEFAULT,
  source_files = c("nrc/config.R", "nrc/data_prep.R", "nrc/fit_models.R")
)
# ==============================================================================
# Multiverse analysis for NRC
# ============================================================================
source("nrc/1_nrc_multiverse.R")
t <- arrow::read_parquet(OUTPUT_PATH)
view(t)
table(t$status)


# ==============================================================================
# Multiverse CV analysis for NRC
# ==============================================================================
source("nrc/2_nrc_cv.R")
# cv_results <- run_nrc_cv(
#   cfg_dir      = cfg_dir,
#   nrc_events   = nrc_events,
#   ops_features = ops_features,
#   outcomes     = c("power_loss_pct", "emerg_binary","ordinal_scram"),
#   n_cores = 22L,
#   K = 5,
#   n_splits = 4,
#   test_duration_months = 6,
#   gap_months = 0,
#   seed = 42,
#   output_path  = "results/nrc/cv/nrc_cv_results.parquet"
# )



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



###################
### EWS and temporal layers
###################

# Load and augment results
mv <- arrow::read_parquet("results/nrc/mv_results.parquet")
mv <- augment_layer_facets(mv)
mv <- link_results_to_config(mv, "config_registry.csv")

# Standard spec curve (existing function, now with layer facets available)
all_facets <- get_all_facets(mv)
plot_spec_curve_with_panels(mv, outcome_filter = "emerg_binary",
                            decision_vars = all_facets)

# Layer impact: does adding temporal/EWS change the climate effect?
compare_layer_impact(mv, outcome_filter = "emerg_binary")

# Concordance with layer facets included
analyze_configuration_concordance(mv, metric = "climate_estimate",
                                  facets = all_facets)

# CV layered delta-Brier
cv <- arrow::read_parquet("results/nrc/cv_results.parquet")
deltas <- compute_layered_delta_brier(cv)
plot_layered_delta_brier(deltas, facet_by = "comparison")
