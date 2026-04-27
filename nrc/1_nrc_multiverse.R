# =============================================================================
# nrc/1_nrc_multiverse.R — NRC Multiverse Analysis
# =============================================================================
#
# Thin wrapper: sources shared + NRC modules, loads data, runs analysis.
# Source paths assume the project root as working directory.
# =============================================================================

library(tidyverse)
library(glmmTMB)
library(lme4)
library(ordinal)
library(broom.mixed)
library(arrow)
library(furrr)
library(glue)
library(slider)

# --- Source shared modules ---
source("common/data_prep.R")
source("common/feature_layers.R")
source("common/layer_temporal.R")
source("common/layer_ews.R")
source("common/validation_protocol.R")
source("common/multiverse_runner.R")

# --- Source NRC-specific modules ---
source("nrc/config.R")
source("nrc/data_prep.R")
source("nrc/fit_models.R")           # fit_binary_scram, fit_ordinal_model, etc.



# Paper 1: climate only
results <- run_multiverse(
  protocol = protocol,
  cfg_dir = CFG_DIR,
  events_df = nrc_events,
  ops_features = ops_features,
  # model_levels = c("M0", "M1"),
  n_workers = N_WORKERS,
  output_path = OUTPUT_PATH,
  config_ids = NULL #c(1,2,3,4,5,6,7,8,9,10)
)



