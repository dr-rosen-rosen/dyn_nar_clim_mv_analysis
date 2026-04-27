############ Plots
PLOT_BASE_SIZE <- 16 # 11 for reports

############ Plots
source("common/postprocessing.R")
source("common/configuration_concordance.R")
source("common/generate_mv_report.R")
source("common/postprocessing_layers.R")


industries <- list(
  rail = industry_config(
    mv_results      = "results_new_new/rail/mv_results.parquet",
    cv_results      = "results_new_new/rail/cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026",
                                 'config_registry.csv'),
    label           = "Rail (FRA)",
    decision_vars   = c("window_type", "window_size","lag_days", "embedding_model", "sent_method",
                        "comp__method"),
    ops_vars        = c("train_miles", "passenger_miles", "staff_hours"),
    exclude_outcomes = c("costs"),
    cv_filter_baseline = "best_non_climate", #"intercept_only", #"no_climate",#"best_non_climate",
    cv_filter_require_both = FALSE,  # pass either strategy (default)
    cv_filter_level = "config_window" # "config"
  ),
  nrc = industry_config(
    mv_results      = "results_new_new/nrc/mv_results.parquet",
    cv_results      = "results_new_new/nrc/cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026"
                                 ,'config_registry.csv'),
    label           = "Nuclear (NRC)",
    decision_vars   = c("window_type", "window_size","lag_days", "embedding_model", "sent_method",
                        "comp__method"),
    ops_vars        = c("capacity_factor", "power_std"#,
                        #"action_matrix_col", "findings_count"
                        ),
    exclude_outcomes = c("ordinal_scram"),
    cv_filter_baseline = "best_non_climate", #"intercept_only",#,no_climate",#"best_non_climate",
    cv_filter_require_both = FALSE,  # pass either strategy (default)
    cv_filter_level = "config_window" # "config"
  )
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
# source("common/postprocessing.R")
# source("common/configuration_concordance.R")
# source("common/postprocessing_layers.R")   # new
# mv <- arrow::read_parquet("results_new/rail/mv_results.parquet")
# mv <- augment_layer_facets(mv)
# mv <- link_results_to_config(mv, here::here(CFG_DIR,"config_registry.csv"))
# 
# # Standard spec curve (existing function, now with layer facets available)
# all_facets <- get_all_facets(mv)
# plot_spec_curve_with_panels(mv, outcome_filter = "fatalities",
#                             decision_vars = all_facets,
#                             component = 'zi')
# 
# # Layer impact: does adding temporal/EWS change the climate effect?
# compare_layer_impact(mv, outcome_filter = "fatalities",component = 'zi')
# 
# # Concordance with layer facets included
# analyze_configuration_concordance(mv, metric = "climate_estimate",
#                                   facets = all_facets,
#                                   comnponent = 'zi')
# 
# # CV layered delta-Brier
# cv <- arrow::read_parquet("results_new/rail/cv_results.parquet")
# deltas <- compute_layered_delta_brier(cv)
# plot_layered_delta_brier(deltas, facet_by = "comparison")




# # NCR Site level plots
# source("nrc/common/config.R")
# source("nrc/common/data_prep.R")
# source("nrc/facility_highlight_chart.R")
# 
# # --- Both FPL plants, auto colors (red + blue) ---
# p <- facility_highlight_chart(
#   cfg_dir       = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_full_run_03-25-2026",
#   config_id     = "72",
#   nrc_events    = nrc_events,
#   facility_name = c("st. lucie", "turkey point"),
#   annotation_dates = list(
#     "Staff fired for reporting safety violoation" = "2017-01-01"
#   ),
#   climate_var   = "overall_final_score_ewmaLAG_180d_hl90d",
#   save_path     = "fpl_facilities_highlight.pdf"
# )
# 
# # --- Custom colors ---
# p <- facility_highlight_chart(
#   ...,
#   facility_name = c("st. lucie" = "#CC0000", "turkey point" = "#0066CC")
# )
# 
# # --- Domain panel with both ---
# p <- facility_highlight_panel(
#   cfg_dir       = "path/to/config_dir",
#   config_id     = "0",
#   nrc_events    = nrc_events,
#   facility_name = c("st. lucie", "turkey point"),
#   climate_var   = "overall_final_score_sma_5",
#   save_path     = "fpl_domains_panel.pdf"
# )
# 
# # --- With annotation dates ---
# p <- facility_highlight_chart(
#   ...,
#   facility_name = c("st. lucie", "turkey point"),
#   annotation_dates = list(
#     "NRC Action" = "2010-03-15",
#     "FPL Leadership Change" = "2012-06-01"
#   )
# )



