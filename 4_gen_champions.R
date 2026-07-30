# Minimal champion generation for the 4 manuscript industries with the new
# outcome set (mining detectability + aviation quarterly). render=FALSE skips
# Quarto. Validates the foundation before the full manuscript regen.
source("common/postprocessing.R")
source("common/configuration_concordance.R")
source("common/best_model_analysis.R")
source("common/generate_mv_report.R")
source("common/postprocessing_layers.R")

CK <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints"
mk <- function(mv, cv, reg, label, ops) industry_config(
  mv_results = mv, cv_results = cv, config_registry = reg, label = label,
  decision_vars = c("window_type","window_size","lag_days","embedding_model","sent_method","comp__method"),
  ops_vars = ops, cv_filter_baseline = "seasonal_ops",
  cv_filter_require_both = FALSE, cv_filter_level = "config_window", track = "panel")

industries <- list(
  rail = mk("results_new_new/rail/panel_mv_results.parquet",
            "results_new_new/rail/panel_cv_results.parquet",
            file.path(CK,"rail_04-14-2026/config_registry.csv"), "Rail (FRA)",
            c("train_miles","passenger_miles","staff_hours")),
  nrc = mk("results_new_new/nrc/panel_mv_results.parquet",
           "results_new_new/nrc/panel_cv_results.parquet",
           file.path(CK,"nrc_04-14-2026/config_registry.csv"), "Nuclear (NRC)",
           c("power_std_mean")),
  aviation = mk("results_new_new/aviation/panel_mv_results_quarterly.parquet",
                "results_new_new/aviation/panel_cv_results_quarterly.parquet",
                file.path(CK,"asrs_05-01-2026/config_registry.csv"), "Aviation (NTSB/ASRS)",
                c("seats","passengers")),
  msha = mk("results_new_new/msha/panel_mv_results_coal_detectability_full.parquet",
            "results_new_new/msha/panel_cv_results_coal_detectability_full.parquet",
            file.path(CK,"msha_06-01-2026/config_registry.csv"), "Mining (MSHA coal)",
            c("hours"))
)

generate_mv_report(industries, output_dir = "report_figures_panel/", render = FALSE)
cat("\n=== champions generated ===\n")
for (ind in names(industries)) {
  f <- sprintf("report_figures_panel/%s/champions_best.csv", ind)
  if (file.exists(f)) {
    d <- readr::read_csv(f, show_col_types = FALSE)
    oc <- if ("outcome" %in% names(d)) d$outcome else d$panel_outcome
    cat(sprintf("  %-9s outcomes: %s\n", ind, paste(sort(unique(oc)), collapse=", ")))
  } else cat(sprintf("  %-9s NO champions_best.csv\n", ind))
}
