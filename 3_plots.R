############ Plots
PLOT_BASE_SIZE <- 16 # 11 for reports
source("shared/postprocessing.R")
source("shared/generate_mv_report.R")
source("shared/configuration_concordance.R")
industries <- list(
  rail = industry_config(
    mv_results      = "results/rail/rail_multiverse_results.parquet",
    cv_results      = "results/rail/rail_cv_results.parquet",
    config_registry = here::here("/Volumes/calculon/event_reporting/rail_02-18-2026",'config_registry.csv'),
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
    mv_results      = "results/nrc/mv/nrc_multiverse_results.parquet",
    cv_results      = "results/nrc/cv/nrc_cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_full_run_03-25-2026",'config_registry.csv'),
    label           = "Nuclear (NRC)",
    decision_vars   = c("window_type", "window_size","lag_days", "embedding_model", "sent_method",
                        "comp__method"),
    ops_vars        = c("capacity_factor", "power_std",
                        "action_matrix_col", "findings_count"),
    exclude_outcomes = c("ordinal_scram"),
    cv_filter_baseline = "best_non_climate", #"intercept_only",#,no_climate",#"best_non_climate",
    cv_filter_require_both = FALSE,  # pass either strategy (default)
    cv_filter_level = "config_window" # "config"
  )
  )

#
# # Generate everything
generate_mv_report(industries, output_dir = "report_figures/",
                   qmd_template = "shared/mv_report_template.qmd",
                   regenerate_figures = TRUE)
#
# # Just regenerate figures, don't re-render
# generate_mv_report(industries, render = FALSE)
#
# # Just re-render with existing figures
# generate_mv_report(industries, regenerate_figures = FALSE)

# 1. Does the file exist?
file.exists("results/nrc/cv/nrc_cv_results.parquet")

# 2. If so, what's in it?
nrc_cv <- arrow::read_parquet("results/nrc/cv/nrc_cv_results.parquet")
nrow(nrc_cv)
names(nrc_cv)
view(nrc_cv)

# 3. What outcomes are in the CV results?
unique(nrc_cv$outcome)

# 4. What outcomes are in the MV results?
nrc_mv <- arrow::read_parquet("results/nrc/nrc_multiverse_results.parquet")
unique(nrc_mv$outcome)

# 5. Do they match?
intersect(unique(nrc_cv$outcome), unique(nrc_mv$outcome))

# 6. Check the industry config you passed — what cv_results path did you use?







source("nrc/common/config.R")
source("nrc/common/data_prep.R")
source("nrc/facility_highlight_chart.R")

# Single score run chart
p <- facility_highlight_chart(
  cfg_dir       = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_full_run_03-25-2026",
  config_id     = "2",
  nrc_events    = nrc_events,
  facility_name = "turkey point",#"saint lucie",          # fuzzy matched
  #climate_var   = "overall_final_score_ewmaLAG_360d_hl180d", 
  climate_var   = "overall_final_score_ewmaLAG_180d_hl90d", 
  #climate_var   = "overall_final_score_sma_20",
  save_path     = "st_lucie_highlight.pdf"
)

# With known-event annotation lines
p <- facility_highlight_chart(
  ...,
  annotation_dates = list(
    "NRC Action" = "2010-03-15",
    "Leadership Change" = "2012-06-01"
  )
)

# Multi-domain panel (LSC, SC, ST, CSP, SEH, SI, SR)
p <- facility_highlight_panel(
  cfg_dir       = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_full_run_03-25-2026",
  config_id     = "45",
  nrc_events    = nrc_events,
  facility_name = "saint lucie",
  #climate_var   = "overall_final_score_sma_20",  # determines window spec
  #climate_var   = "overall_final_score_sma_3",
  climate_var   = "overall_final_score_ewmaLAG_180d_h90d", 
  save_path     = "st_lucie_domains_panel.pdf"
)

# Explore what's available first
list_configs("path/to/config_dir")
list_climate_vars("path/to/config_dir", "0", nrc_events)









source("nrc/common/config.R")
source("nrc/common/data_prep.R")
source("nrc/facility_highlight_chart.R")

# --- Both FPL plants, auto colors (red + blue) ---
p <- facility_highlight_chart(
  cfg_dir       = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_full_run_03-25-2026",
  config_id     = "72",
  nrc_events    = nrc_events,
  facility_name = c("st. lucie", "turkey point"),
  annotation_dates = list(
    "Staff fired for reporting safety violoation" = "2017-01-01"
  ),
  climate_var   = "overall_final_score_ewmaLAG_180d_hl90d",
  save_path     = "fpl_facilities_highlight.pdf"
)

# --- Custom colors ---
p <- facility_highlight_chart(
  ...,
  facility_name = c("st. lucie" = "#CC0000", "turkey point" = "#0066CC")
)

# --- Domain panel with both ---
p <- facility_highlight_panel(
  cfg_dir       = "path/to/config_dir",
  config_id     = "0",
  nrc_events    = nrc_events,
  facility_name = c("st. lucie", "turkey point"),
  climate_var   = "overall_final_score_sma_5",
  save_path     = "fpl_domains_panel.pdf"
)

# --- With annotation dates ---
p <- facility_highlight_chart(
  ...,
  facility_name = c("st. lucie", "turkey point"),
  annotation_dates = list(
    "NRC Action" = "2010-03-15",
    "FPL Leadership Change" = "2012-06-01"
  )
)





source("shared/configuration_concordance.R")
source("shared/postprocessing.R")
source("shared/spec_curve_common.R")
# Step 1: Get credible configs from CV
cv_results <- arrow::read_parquet("/Users/michaelrosen/Documents/data_anlaysis/dyn_nar_clim_mv_analysis/results/nrc/cv/nrc_cv_results.parquet")
mv_results <- arrow::read_parquet("/Users/michaelrosen/Documents/data_anlaysis/dyn_nar_clim_mv_analysis/results/nrc/mv/nrc_multiverse_results.parquet")
credible <- filter_credible_specs(
  cv_results,
  baseline_strategy = "best_non_climate"
)

# Step 2: Filter MV results
mv_filtered <- mv_results %>%
  filter(config_id %in% credible$credible_configs)

# Step 3: Pass to existing plotting functions as usual
plot_spec_curve(
  data = mv_filtered |> filter(outcome == "power_loss_pct")

)
head(mv_filtered)
unique(mv_filtered$outcome)
