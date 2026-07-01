# =============================================================================
# msha/2_msha_cv.R — MSHA Incident-Level Cross-Validation
# =============================================================================
#
# For each commodity (coal, mnm), runs the 3-tier nested CV
# (intercept_only / seasonal_ops / climate) for the binary incident outcomes
# under group_kfold and timeseries strategies. Primary metric: held-out
# log-likelihood / Brier per observation (handled by common/cv_runner.R).
#
# Source msha/0_main_msha.R FIRST (provides data, make_msha_protocol(),
# CFG_DIR, N_WORKERS, CV_CHECKPOINT_DIR, and path helpers).
# =============================================================================

source("common/data_prep.R")
source("common/feature_layers.R")
source("common/layer_temporal.R")
source("common/layer_ews.R")
source("common/validation_protocol.R")
source("common/cv_runner.R")
source("msha/config.R")
source("msha/data_prep.R")
source("msha/fit_models.R")


cv_results_all <- list()

for (cm in COMMODITIES) {
  cat(sprintf("\n\n#################  MSHA %s — incident CV  #################\n",
              toupper(cm)))

  protocol  <- make_msha_protocol(cm)
  events_c  <- msha_events_all %>% filter(commodity == cm)
  ops_c     <- ops_all %>% filter(commodity == cm)

  res_c <- run_cv(
    protocol       = protocol,
    cfg_dir        = CFG_DIR,
    events_df      = events_c,
    ops_features   = ops_c,
    n_workers      = N_WORKERS,
    output_path    = cv_output_path(cm),
    checkpoint_dir = file.path(CV_CHECKPOINT_DIR, cm),
    config_ids     = NULL
  )

  cv_results_all[[cm]] <- res_c %>% mutate(commodity = cm)
}

cat("\n=== MSHA Incident CV complete ===\n")
