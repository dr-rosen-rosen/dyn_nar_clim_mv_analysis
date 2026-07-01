############ Panel-Rate Track Plots
# Parallel to 3_plots.R but operates on the panel-rate multiverse + CV outputs.
# Uses track = "panel" so postprocessing dispatches to:
#   - climate_pval (not climate_p)
#   - loglik_mean (not brier_mean), higher = better
#   - cv_filter_baseline ∈ {"seasonal_ops","best_non_climate","intercept_only"}

PLOT_BASE_SIZE <- 16 # 11 for reports

source("common/postprocessing.R")
source("common/configuration_concordance.R")
source("common/generate_mv_report.R")
source("common/postprocessing_layers.R")


industries <- list(
  rail = industry_config(
    mv_results      = "results_new_new/rail/panel_mv_results.parquet",
    cv_results      = "results_new_new/rail/panel_cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026",
                                 'config_registry.csv'),
    label           = "Rail (FRA)",
    decision_vars   = c("window_type", "window_size","lag_days", "embedding_model", "sent_method",
                        "comp__method"),
    ops_vars        = c("train_miles", "passenger_miles", "staff_hours"),
    exclude_outcomes = NULL,  # panel outcomes are rate_accidents/injuries/fatalities
    cv_filter_baseline = "seasonal_ops",  # strong baseline for panel track
    cv_filter_require_both = FALSE,
    cv_filter_level = "config_window",
    track = "panel"
  ),
  nrc = industry_config(
    mv_results      = "results_new_new/nrc/panel_mv_results.parquet",
    cv_results      = "results_new_new/nrc/panel_cv_results.parquet",
    config_registry = here::here("/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026",
                                 'config_registry.csv'),
    label           = "Nuclear (NRC)",
    decision_vars   = c("window_type", "window_size","lag_days", "embedding_model", "sent_method",
                        "comp__method"),
    ops_vars        = c("power_std_mean"),  # panel decomposes only power_std_mean (between/within)
    exclude_outcomes = NULL,  # panel outcomes are rate_lers/emerg/pct_power_loss
    cv_filter_baseline = "seasonal_ops",
    cv_filter_require_both = FALSE,
    cv_filter_level = "config_window",
    track = "panel"
  )
)

# --- Aviation (uncomment after aviation panel sweeps complete) ---
# Uses the PRIMARY outer spec (atc_scope=local_terminal × apt_dist_nm=5 × exclude).
# Other outer-spec combinations are sensitivity-only.
industries$aviation <- industry_config(
  mv_results      = "results_new_new/aviation/panel_mv_results_quarterly.parquet",
  cv_results      = "results_new_new/aviation/panel_cv_results_quarterly.parquet",
  config_registry = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026/config_registry.csv",
  label           = "Aviation (NTSB/ASRS)",
  decision_vars   = c("window_type","window_size","lag_days","embedding_model","sent_method","comp__method"),
  ops_vars        = c("seats", "passengers"),  # departures is the offset
  cv_filter_baseline = "seasonal_ops",
  cv_filter_require_both = FALSE,
  cv_filter_level = "config_window",
  track = "panel"
)


# --- MSHA (coal) ---
# Fourth industry arm. Coal only (mnm deferred to the PHMSA follow-up). Outer
# loop on commodity in the driver writes panel_{mv,cv}_results_coal.parquet.
# Offset is log(hours_worked); only hours_within enters (between is collinear
# with the offset level). Champions land in report_figures_panel/msha/.
industries$msha <- industry_config(
  mv_results      = "results_new_new/msha/panel_mv_results_coal_detectability_full.parquet",
  cv_results      = "results_new_new/msha/panel_cv_results_coal_detectability_full.parquet",
  config_registry = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026/config_registry.csv",
  label           = "Mining (MSHA coal)",
  decision_vars   = c("window_type","window_size","lag_days","embedding_model","sent_method","comp__method"),
  ops_vars        = c("hours"),  # hours_worked is the offset; hours_within decomposed
  cv_filter_baseline = "seasonal_ops",
  cv_filter_require_both = FALSE,
  cv_filter_level = "config_window",
  track = "panel"
)


# --- PHMSA (uncomment after panel sweeps complete) ---
# PHMSA is split by pipeline_type; each is one "industry" entry. Output dir
# subdirs match what phmsa/1_phmsa_panel_multiverse.R writes.
phmsa_industry <- function(pt, label) {
  industry_config(
    mv_results      = file.path("results_new_new/phmsa", pt, "panel_mv_results.parquet"),
    cv_results      = file.path("results_new_new/phmsa", pt, "panel_cv_results.parquet"),
    config_registry = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/phmsa_04-28-2026/config_registry.csv",
    label           = label,
    decision_vars   = c("window_type","window_size","lag_days","embedding_model","sent_method","comp__method"),
    ops_vars        = c("total_miles"),
    cv_filter_baseline = "seasonal_ops",
    cv_filter_require_both = FALSE,
    cv_filter_level = "config_window",
    track = "panel"
  )
}
industries$phmsa_gd <- phmsa_industry("gas_distribution",  "PHMSA Gas Distribution")
industries$phmsa_gt <- phmsa_industry("gas_transmission",  "PHMSA Gas Transmission")
industries$phmsa_hl <- phmsa_industry("hazardous_liquid",  "PHMSA Hazardous Liquid")


# Generate everything (figures + Quarto reports) into a separate dir.
# Two reports: full (in-depth) and condensed (executive summary). The
# condensed render skips figure regeneration since it reads the same
# artifacts as the full run.
generate_mv_report(industries,
                   output_dir    = "report_figures_panel/",
                   qmd_template  = "common/mv_report_template.qmd",
                   report_output = "report_figures_panel/mv_report_panel_full.pdf")

generate_mv_report(industries,
                   output_dir         = "report_figures_panel/",
                   qmd_template       = "common/mv_report_condensed_template.qmd",
                   report_output      = "report_figures_panel/mv_report_panel_executive.pdf",
                   regenerate_figures = FALSE)

# # Just regenerate figures, don't re-render
# generate_mv_report(industries, output_dir = "report_figures_panel/", render = FALSE)

# # Just re-render with existing figures
# generate_mv_report(industries, output_dir = "report_figures_panel/",
#                    qmd_template = "common/mv_report_template.qmd",
#                    report_output = "report_figures_panel/mv_report_panel_full.pdf",
#                    regenerate_figures = FALSE)
