# =============================================================================
# phmsa/phase1_panel_diagnostic.R — PHMSA Panel-Rate Phase 1 Diagnostic
# =============================================================================
#
# For each pipeline_type, fits panel-rate models on a single climate config
# with one fixed climate window. Confirms the panel grid, ops join, and model
# convergence before launching the full multiverse / CV.
#
# Run from project root: Rscript phmsa/phase1_panel_diagnostic.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(glmmTMB)
  library(broom.mixed)
  library(slider)
  library(lubridate)
  library(stringr)
  library(here)
})

source("phmsa/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("phmsa/panel_data_prep.R")
source("phmsa/panel_fit_models.R")


# ---- CONFIG ----
CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/phmsa_04-24-2026"
CONFIG_ID   <- "0"
CLIMATE_VAR <- "overall_final_score_sma_5"
EVENTS_PATH <- here("data/phmsa/events.parquet")
OPS_PATH    <- here("data/phmsa/operator_annual_ops.parquet")

PARQUET_PATH <- file.path(CFG_DIR, "_cfg", CONFIG_ID, "results.parquet")


# ---- LOAD DATA ----
cat("\n=== Loading PHMSA data ===\n")

phmsa_events_all <- read_parquet(EVENTS_PATH) %>%
  mutate(
    operator_id = as.integer(operator_raw_id),
    event_date  = as.Date(event_date)
  ) %>%
  filter(!is.na(operator_id), !is.na(event_date),
         pipeline_type %in% PIPELINE_TYPES) %>%
  select(eid, operator_id, event_date, pipeline_type,
         total_injuries, total_fatalities, total_cost,
         UNINTENTIONAL_RELEASE_BBLS)

ops_features <- read_parquet(OPS_PATH)

cat(sprintf("  events: %d  operators: %d\n",
            nrow(phmsa_events_all), n_distinct(phmsa_events_all$operator_id)))
print(count(phmsa_events_all, pipeline_type))


# ---- DIAGNOSTIC LOOP ----

run_diagnostic_for_type <- function(pipeline_type) {
  cat(sprintf("\n\n#################  %s  #################\n", pipeline_type))

  events_one <- phmsa_events_all %>% filter(pipeline_type == !!pipeline_type)
  cat(sprintf("  Events for this type: %d  operators: %d\n",
              nrow(events_one), n_distinct(events_one$operator_id)))

  # Use a smaller min_reports for the diagnostic so all three pipelines have
  # something to fit. Production runs can re-tune.
  panel <- prepare_phmsa_panel_data(
    parquet_path  = PARQUET_PATH,
    config_id     = CONFIG_ID,
    events_df     = events_one,
    ops_features  = ops_features,
    pipeline_type = pipeline_type,
    min_reports   = 5L
  )

  if (is.null(panel) || nrow(panel) == 0) {
    cat("  [SKIP] empty panel\n")
    return(invisible(NULL))
  }

  cat(sprintf("  Panel: %d rows  operators: %d  years: %d\n",
              nrow(panel), n_distinct(panel$operator_id), n_distinct(panel$year_start)))
  cat(sprintf("  Climate %s NA-rate: %.1f%%\n", CLIMATE_VAR,
              100 * mean(is.na(panel[[CLIMATE_VAR]]))))
  cat("  Outcome zero-inflation:\n")
  print(panel %>%
          summarise(across(c(n_incidents, sum_injuries, sum_fatalities, sum_release_volume),
                           list(mean = ~mean(.x, na.rm = TRUE),
                                pct_zero = ~100 * mean(.x == 0, na.rm = TRUE)))))

  for (outc in names(PANEL_OUTCOME_VARS_PHMSA)) {
    cat(sprintf("\n  --- Fitting %s ---\n", outc))
    res <- fit_single_phmsa_panel_model(panel, CLIMATE_VAR, CONFIG_ID, outcome = outc)
    if (!is.null(res$status) && res$status == "failed") {
      cat(sprintf("    [FAILED] %s\n", res$error %||% ""))
    } else {
      cat(sprintf("    status: %s | n_obs=%d | n_orgs=%d | climate_est=%.3f (p=%.3g)\n",
                  res$status, res$n_obs %||% NA, res$n_orgs %||% NA,
                  res$climate_estimate %||% NA, res$climate_pval %||% NA))
      if (!is.null(res$family) && res$family == "tweedie") {
        cat(sprintf("    Tweedie p = %.3f\n", res$tweedie_power %||% NA))
      }
    }
  }
}

for (pt in PIPELINE_TYPES) {
  run_diagnostic_for_type(pt)
}

cat("\n=== Diagnostic complete ===\n")
