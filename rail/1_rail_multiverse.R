# =============================================================================
# rail/1_rail_multiverse.R — Rail Multiverse Analysis
# =============================================================================
#
# Thin wrapper that:
#   1. Sources shared + industry-specific modules
#   2. Loads data
#   3. Runs the multiverse analysis
#
# Source paths assume the project root as working directory.
# =============================================================================

library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(arrow)
library(furrr)
library(glue)
library(slider)

# --- Source shared modules ---
source("common/data_prep.R")                # create_all_windows, get_climate_vars, etc.
source("common/feature_layers.R")           # Layer registry (Phase 1)
source("common/layer_temporal.R")           # Temporal feature layer
source("common/layer_ews.R")               # EWS feature layer
source("common/validation_protocol.R")      # Protocol specification
source("common/multiverse_runner.R")        # Generic runner

# --- Source rail-specific modules ---
source("rail/config.R")                     # WINDOW_SPECS, OPS_VARS, OUTCOME_VARS, MODEL_FORMULAS
source("rail/data_prep.R")                  # prepare_config_data, make_ops_features_rolling
source("rail/fit_models.R")                 # fit_single_rail_model (existing, not recreated here)




# Paper 1: climate only (M0 baseline + M1 climate)
results <- run_multiverse(
  protocol = protocol,
  cfg_dir = CFG_DIR,
  events_df = rail_raw,
  ops_features = ops_features,
  n_workers = N_WORKERS,
  output_path = OUTPUT_PATH,
  config_ids = NULL #c(1) #c(1,2,3,4,5,6,7,8,9,10)
)


# t <- arrow::read_parquet(OUTPUT_PATH)
# table(t$error)

