# =============================================================================
# aviation/0_main_aviation.R — Aviation Safety Climate Analysis
# =============================================================================
#
# Orchestrates the full aviation multiverse and CV analyses.
#
# Aviation adds an outer spec grid over three dimensions not present in
# NRC / rail: ATC reporter scope, airport proximity cutoff, and missing-climate
# treatment. For each combination, a protocol closure is defined and the
# standard run_multiverse() / run_cv() machinery is called.
#
# Usage: source this file from the project root, then call
#   source("aviation/1_aviation_multiverse.R")   # multiverse
#   source("aviation/2_aviation_cv.R")            # cross-validation
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
library(here)

# --- Source shared modules ---
source("common/data_prep.R")
source("common/feature_layers.R")
source("common/layer_temporal.R")
source("common/layer_ews.R")
source("common/validation_protocol.R")
source("common/multiverse_runner.R")
source("common/cv_runner.R")

# --- Source aviation modules ---
source("aviation/config.R")
source("aviation/data_prep.R")
source("aviation/fit_models.R")


# =============================================================================
# PATHS
# =============================================================================

CFG_DIR        <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/aviation_PLACEHOLDER"
ASRS_CSV_PATH  <- here("data/aviation/asrs_dict_df.csv")
NTSB_POST_PATH <- here("data/aviation/ntsb_av_accident_data/events.xlsx")
NTSB_PRE_PATH  <- here("data/aviation/ntsb_av_accident_data/events_pre2008.xlsx")
OPS_PATH       <- here("data/aviation/bts_t100/airport_month_ops.parquet")
OUTPUT_DIR     <- here("results/aviation")
CV_CHECKPOINT_DIR <- here("results/aviation/cv_checkpoints")

N_WORKERS <- 20L

dir.create(OUTPUT_DIR,        showWarnings = FALSE, recursive = TRUE)
dir.create(CV_CHECKPOINT_DIR, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# LOAD SHARED DATA  (loaded once; reused across all spec combinations)
# =============================================================================

cat("--- Loading shared data ---\n")

asrs_meta    <- load_asrs_meta(ASRS_CSV_PATH)
ops_features <- read_parquet(OPS_PATH)

cat(sprintf("  ASRS meta: %s reports\n", format(nrow(asrs_meta), big.mark = ",")))
cat(sprintf("  T100 ops:  %s airport-months\n", format(nrow(ops_features), big.mark = ",")))


# =============================================================================
# AVIATION OUTER SPEC GRID
# =============================================================================
# These dimensions are specific to aviation and are crossed with the
# climate config multiverse (window type × window size × embedding × etc.).

av_spec_grid <- expand_grid(
  atc_scope       = ATC_SCOPE_LEVELS,         # "local", "local_terminal", "all_atc"
  apt_dist_nm     = APT_DIST_CUTOFFS_NM,      # 5L, 15L
  missing_climate = MISSING_CLIMATE_LEVELS    # "exclude", "impute_mean"
)

cat(sprintf("\n--- Aviation spec grid: %d combinations ---\n", nrow(av_spec_grid)))
print(av_spec_grid)


# =============================================================================
# PROTOCOL FACTORY
# =============================================================================
# Returns a fully configured validation_protocol for one row of av_spec_grid.
# The data_prep_fn closure captures the spec-specific NTSB panel so the
# standard run_multiverse() call signature (events_df, ops_features) is
# satisfied with asrs_meta passed as events_df.

make_av_protocol <- function(atc_scope, apt_dist_nm, missing_climate) {

  ntsb_panel <- load_ntsb_panel(NTSB_POST_PATH, NTSB_PRE_PATH, apt_dist_nm)

  define_validation_protocol(
    industry       = "aviation",
    label          = sprintf("Aviation (scope=%s, dist=%dNM, missing=%s)",
                             atc_scope, apt_dist_nm, missing_climate),
    layer_registry = create_layer_registry(),
    model_hierarchy = define_model_hierarchy(M1 = c("baseline", "climate")),
    outcomes       = c("accident_binary", "inj_serious_fatal"),
    outcome_configs = OUTCOME_VARS,
    ops_vars       = AV_OPS_VARS,
    org_var        = "airport_id",
    source_files   = c("aviation/config.R", "aviation/data_prep.R",
                       "aviation/fit_models.R"),
    data_prep_fn   = local({
      .ntsb    <- ntsb_panel
      .scope   <- atc_scope
      .missing <- missing_climate
      function(parquet_path, config_id, events_df, ops_features,
               min_reports = MIN_REPORTS_DEFAULT, ...) {
        prepare_av_config_data(
          parquet_path    = parquet_path,
          config_id       = config_id,
          asrs_meta_df    = events_df,   # events_df slot carries asrs_meta
          ntsb_panel_df   = .ntsb,
          ops_features    = ops_features,
          atc_scope       = .scope,
          missing_climate = .missing,
          min_reports     = min_reports,
          ...
        )
      }
    }),
    fit_fn         = function(df, climate_var, config_id, outcome,
                              extra_terms = character(0), protocol = NULL) {
      fit_av_model(df, climate_var, config_id, outcome, extra_terms)
    },
    window_specs   = WINDOW_SPECS,
    min_reports    = MIN_REPORTS_DEFAULT,
    cv = list(
      strategies           = c("group_kfold", "timeseries"),
      require_both         = FALSE,
      baseline_strategy    = "best_non_climate",
      K                    = 5L,
      n_splits             = 5L,
      test_duration_months = 24L,
      gap_months           = 6L,
      cv_outcomes          = c("accident_binary")  # binary only for CV Brier score
    )
  )
}


# =============================================================================
# SPEC LABEL HELPER
# =============================================================================

spec_label <- function(atc_scope, apt_dist_nm, missing_climate) {
  glue("scope-{atc_scope}_dist-{apt_dist_nm}NM_missing-{missing_climate}")
}

mv_output_path <- function(atc_scope, apt_dist_nm, missing_climate) {
  file.path(OUTPUT_DIR,
            glue("mv_results_{spec_label(atc_scope, apt_dist_nm, missing_climate)}.parquet"))
}

cv_output_path <- function(atc_scope, apt_dist_nm, missing_climate) {
  file.path(OUTPUT_DIR,
            glue("cv_results_{spec_label(atc_scope, apt_dist_nm, missing_climate)}.parquet"))
}


# =============================================================================
# NOTE: run_aviation_multiverse() and run_aviation_cv() are defined in
# 1_aviation_multiverse.R and 2_aviation_cv.R respectively.
# Source this file first, then source the script for the desired analysis.
# =============================================================================

cat("\n--- aviation/0_main_aviation.R loaded ---\n")
cat("  Source aviation/1_aviation_multiverse.R to run the multiverse.\n")
cat("  Source aviation/2_aviation_cv.R to run cross-validation.\n\n")
