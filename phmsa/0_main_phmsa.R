# =============================================================================
# phmsa/0_main_phmsa.R — PHMSA Pipeline Safety Climate Analysis
# =============================================================================
#
# Orchestrates multiverse and CV analyses for three PHMSA pipeline types:
#   gas_distribution, gas_transmission, hazardous_liquid
#
# Each pipeline type is treated as a separate analysis with its own protocol,
# analogous to NRC vs. rail. An outer loop over PIPELINE_TYPES calls
# make_phmsa_protocol() for each type.
#
# Usage: source this file from the project root, then call
#   source("phmsa/1_phmsa_multiverse.R")   # multiverse
#   source("phmsa/2_phmsa_cv.R")            # cross-validation
# =============================================================================

library(tidyverse)
library(glmmTMB)
library(lme4)
library(broom.mixed)
library(arrow)
library(furrr)
library(glue)
library(slider)
library(lubridate)
library(here)

# --- Source shared modules ---
source("common/data_prep.R")
source("common/feature_layers.R")
source("common/layer_temporal.R")
source("common/layer_ews.R")
source("common/validation_protocol.R")
source("common/multiverse_runner.R")
source("common/cv_runner.R")

# --- Source PHMSA modules ---
source("phmsa/config.R")
source("phmsa/data_prep.R")
source("phmsa/fit_models.R")


# =============================================================================
# PATHS
# =============================================================================

CFG_DIR   <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/phmsa_04-24-2026"
EVENTS_PATH  <- here("data/phmsa/events.parquet")
OPS_PATH     <- here("data/phmsa/operator_annual_ops.parquet")
OUTPUT_DIR   <- here("results/phmsa")
CV_CHECKPOINT_DIR <- here("results/phmsa/cv_checkpoints")

N_WORKERS <- 20L

dir.create(OUTPUT_DIR,        showWarnings = FALSE, recursive = TRUE)
dir.create(CV_CHECKPOINT_DIR, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# LOAD SHARED DATA  (loaded once; split by pipeline_type per protocol)
# =============================================================================

cat("--- Loading shared data ---\n")

phmsa_events_all <- read_parquet(EVENTS_PATH) %>%
  mutate(
    operator_id = as.integer(operator_raw_id),
    event_date  = as.Date(event_date)
  ) %>%
  filter(!is.na(operator_id), !is.na(event_date),
         pipeline_type %in% PIPELINE_TYPES)

ops_features <- read_parquet(OPS_PATH)

cat(sprintf("  PHMSA events (3 pipeline types): %s\n",
    format(nrow(phmsa_events_all), big.mark = ",")))
cat(sprintf("  Events by pipeline type:\n"))
print(count(phmsa_events_all, pipeline_type))
cat(sprintf("  Ops features: %s operator-years\n",
    format(nrow(ops_features), big.mark = ",")))


# =============================================================================
# PROTOCOL FACTORY
# =============================================================================

#' Build a validation protocol for one PHMSA pipeline type
#'
#' @param pipeline_type One of PIPELINE_TYPES
#' @return A fully configured validation_protocol
make_phmsa_protocol <- function(pipeline_type) {

  events_type <- phmsa_events_all %>%
    filter(pipeline_type == !!pipeline_type)

  define_validation_protocol(
    industry        = "phmsa",
    label           = sprintf("PHMSA %s", gsub("_", " ", pipeline_type)),
    layer_registry  = create_layer_registry(),
    model_hierarchy = define_model_hierarchy(M1 = c("baseline", "climate")),
    outcomes        = c("injury_binary", "fatality_binary", "total_cost"),
    outcome_configs = OUTCOME_VARS,
    ops_vars        = PHMSA_OPS_VARS,
    org_var         = "operator_id",
    source_files    = c("phmsa/config.R", "phmsa/data_prep.R",
                        "phmsa/fit_models.R"),
    data_prep_fn    = local({
      .events <- events_type
      function(parquet_path, config_id, events_df, ops_features,
               min_reports = MIN_REPORTS_DEFAULT, ...) {
        prepare_phmsa_config_data(
          parquet_path = parquet_path,
          config_id    = config_id,
          events_df    = .events,     # pipeline-filtered events
          ops_features = ops_features,
          min_reports  = min_reports,
          ...
        )
      }
    }),
    fit_fn = function(df, climate_var, config_id, outcome,
                      extra_terms = character(0), protocol = NULL) {
      fit_phmsa_model(df, climate_var, config_id, outcome, extra_terms)
    },
    window_specs    = WINDOW_SPECS,
    min_reports     = MIN_REPORTS_DEFAULT,
    cv = list(
      strategies           = c("group_kfold", "timeseries"),
      require_both         = FALSE,
      baseline_strategy    = "best_non_climate",
      K                    = 5L,
      n_splits             = 5L,
      test_duration_months = 24L,
      gap_months           = 6L,
      cv_outcomes          = c("injury_binary", "fatality_binary")
    )
  )
}


# =============================================================================
# LABEL HELPERS
# =============================================================================

pipeline_label <- function(pipeline_type) {
  gsub("_", "-", pipeline_type)
}

mv_output_path <- function(pipeline_type) {
  file.path(OUTPUT_DIR, glue("mv_results_{pipeline_label(pipeline_type)}.parquet"))
}

cv_output_path <- function(pipeline_type) {
  file.path(OUTPUT_DIR, glue("cv_results_{pipeline_label(pipeline_type)}.parquet"))
}


# =============================================================================
# NOTE: The multiverse and CV loops are defined in
# phmsa/1_phmsa_multiverse.R and phmsa/2_phmsa_cv.R respectively.
# Source this file first, then source the script for the desired analysis.
# =============================================================================

cat("\n--- phmsa/0_main_phmsa.R loaded ---\n")
cat("  Source phmsa/1_phmsa_multiverse.R to run the multiverse.\n")
cat("  Source phmsa/2_phmsa_cv.R to run cross-validation.\n\n")


source("phmsa/1_phmsa_multiverse.R")

mv <- arrow::read_parquet("/Users/michaelrosen/Documents/data_anlaysis/dyn_nar_clim_mv_analysis/results/phmsa/mv_results_all_types.parquet")
source("phmsa/2_phmsa_cv.R")
cv <- arrow::read_parquet("/Users/michaelrosen/Documents/data_anlaysis/dyn_nar_clim_mv_analysis/results/phmsa/cv_results_all_types.parquet")
