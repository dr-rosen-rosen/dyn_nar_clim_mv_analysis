# =============================================================================
# msha/0_main_msha.R — MSHA Mine-Safety Climate Analysis
# =============================================================================
#
# Orchestrates multiverse and CV analyses for the two MSHA commodity classes:
#   coal, mnm (metal / non-metal)
#
# Each commodity is treated as a separate analysis with its own protocol,
# analogous to PHMSA's pipeline types and NRC's single-industry arm. An outer
# loop over COMMODITIES (in the 1_/2_ scripts) calls make_msha_protocol() for
# each class. coal and mnm are never pooled — they differ in operational
# regime, reporting, and operator/exposure linkage coverage.
#
# Usage: source this file from the project root, then call
#   source("msha/1_msha_multiverse.R")        # incident-level multiverse
#   source("msha/1_msha_panel_multiverse.R")  # panel-rate multiverse
#   source("msha/2_msha_cv.R")                # incident-level CV
#   source("msha/2_msha_panel_cv.R")          # panel-rate CV
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

# --- Shared modules ---
source("common/data_prep.R")
source("common/feature_layers.R")
source("common/layer_temporal.R")
source("common/layer_ews.R")
source("common/validation_protocol.R")
source("common/multiverse_runner.R")
source("common/cv_runner.R")

# --- MSHA modules ---
source("msha/config.R")
source("msha/data_prep.R")
source("msha/fit_models.R")


# =============================================================================
# PATHS
# =============================================================================

CFG_DIR    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026"
DATA_DIR   <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha"
EVENTS_PATH      <- file.path(DATA_DIR, "events.parquet")
OPS_PATH         <- file.path(DATA_DIR, "ops_mine_quarter.parquet")
COVARIATES_PATH  <- file.path(DATA_DIR, "covariates_mine_quarter.parquet")

OUTPUT_DIR        <- "results_new_new/msha"
CV_CHECKPOINT_DIR <- "results_new_new/msha/cv_checkpoints"

N_WORKERS <- 20L

dir.create(OUTPUT_DIR,        showWarnings = FALSE, recursive = TRUE)
dir.create(CV_CHECKPOINT_DIR, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# LOAD SHARED DATA  (loaded once; split by commodity per protocol)
# =============================================================================

cat("--- Loading shared MSHA data ---\n")

msha_events_all <- arrow::read_parquet(EVENTS_PATH) %>%
  mutate(
    mine_id    = as.character(mine_id),
    eid        = as.character(eid),
    event_date = as.Date(accident_date),
    commodity  = tolower(trimws(commodity))
  ) %>%
  filter(!is.na(mine_id), !is.na(event_date), commodity %in% COMMODITIES)

ops_all <- arrow::read_parquet(OPS_PATH) %>%
  mutate(mine_id = as.character(mine_id),
         commodity = tolower(trimws(commodity)))

covariates_all <- arrow::read_parquet(COVARIATES_PATH) %>%
  mutate(mine_id = as.character(mine_id))

cat(sprintf("  MSHA events (both commodities): %s\n",
            format(nrow(msha_events_all), big.mark = ",")))
print(count(msha_events_all, commodity))
cat(sprintf("  Ops mine-quarters: %s\n", format(nrow(ops_all), big.mark = ",")))
cat(sprintf("  Covariate mine-quarters: %s\n", format(nrow(covariates_all), big.mark = ",")))


# =============================================================================
# PROTOCOL FACTORY (incident-level track)
# =============================================================================

#' Build an incident-level validation protocol for one MSHA commodity.
#'
#' @param commodity One of COMMODITIES ("coal", "mnm").
#' @return A fully configured validation_protocol.
make_msha_protocol <- function(commodity) {

  events_c <- msha_events_all %>% filter(commodity == !!commodity)

  define_validation_protocol(
    industry        = "msha",
    label           = sprintf("MSHA %s", toupper(commodity)),
    layer_registry  = create_layer_registry(),
    model_hierarchy = define_model_hierarchy(M1 = c("baseline", "climate")),
    outcomes        = c("fatality_binary", "injury_binary", "days_lost"),
    outcome_configs = OUTCOME_VARS,
    ops_vars        = MSHA_OPS_VARS,
    org_var         = "mine_id",
    source_files    = c("msha/config.R", "msha/data_prep.R", "msha/fit_models.R"),
    data_prep_fn    = local({
      .events <- events_c
      function(parquet_path, config_id, events_df, ops_features,
               min_reports = MIN_REPORTS_DEFAULT, ...) {
        prepare_msha_config_data(
          parquet_path = parquet_path,
          config_id    = config_id,
          events_df    = .events,        # commodity-filtered events
          ops_features = ops_features,
          min_reports  = min_reports,
          ...
        )
      }
    }),
    fit_fn = list(
      fatality_binary = function(df, climate_var, config_id, outcome,
                                 extra_terms = character(0), protocol = NULL) {
        fit_binary_fatality(df, climate_var, config_id, extra_terms = extra_terms)
      },
      injury_binary = function(df, climate_var, config_id, outcome,
                               extra_terms = character(0), protocol = NULL) {
        fit_binary_injury(df, climate_var, config_id, extra_terms = extra_terms)
      },
      days_lost = function(df, climate_var, config_id, outcome,
                           extra_terms = character(0), protocol = NULL) {
        fit_days_lost_hurdle(df, climate_var, config_id, extra_terms = extra_terms)
      }
    ),
    window_specs = WINDOW_SPECS,
    min_reports  = MIN_REPORTS_DEFAULT,
    cv = list(
      strategies           = c("group_kfold", "timeseries"),
      require_both         = FALSE,
      baseline_strategy    = "best_non_climate",
      K                    = 5L,
      n_splits             = 5L,
      test_duration_months = 24L,
      gap_months           = 6L,
      cv_outcomes          = c("fatality_binary", "injury_binary")
    )
  )
}


# =============================================================================
# LABEL / PATH HELPERS
# =============================================================================

mv_output_path        <- function(commodity) file.path(OUTPUT_DIR, glue("mv_results_{commodity}.parquet"))
panel_mv_output_path  <- function(commodity) file.path(OUTPUT_DIR, glue("panel_mv_results_{commodity}.parquet"))
cv_output_path        <- function(commodity) file.path(OUTPUT_DIR, glue("cv_results_{commodity}.parquet"))
panel_cv_output_path  <- function(commodity) file.path(OUTPUT_DIR, glue("panel_cv_results_{commodity}.parquet"))


cat("\n--- msha/0_main_msha.R loaded ---\n")
cat("  source('msha/1_msha_multiverse.R')        # incident multiverse\n")
cat("  source('msha/1_msha_panel_multiverse.R')  # panel multiverse\n")
cat("  source('msha/2_msha_cv.R')                # incident CV\n")
cat("  source('msha/2_msha_panel_cv.R')          # panel CV\n\n")
