source("common/data_prep.R")
source("common/temporal_features.R")
source("common/feature_layers.R")
source("common/layer_temporal.R")
source("common/layer_ews.R")
source("common/validation_protocol.R")
source("common/cv_runner.R")
source("nrc/config.R")
source("nrc/data_prep.R")
source("nrc/fit_models.R")

# Build protocol (same as MV script)
# protocol <- define_validation_protocol(...)

# Run CV
cv_results <- run_cv(
  protocol = protocol,
  cfg_dir = CFG_DIR,
  events_df = nrc_events,
  ops_features = ops_features,
  n_workers = 10L,
  output_path = "results_new/nrc/cv_results.parquet",
  checkpoint_dir = "results_new/nrc/cv_checkpoints",
  config_ids = c(1,2,3,4,5,6,7,8,9,10)
)

# t <- arrow::read_parquet("results_new/nrc/cv_results.parquet")
# skimr::skim(t)