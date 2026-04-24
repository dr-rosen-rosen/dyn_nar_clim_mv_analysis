# 2_rail_cv_binary.R
# Cross-Validation for Stage 1 (Binary Occurrence) Models
# Implements Group K-Fold CV (Strategy A) and Time-Series CV (Strategy B)
# for the logistic GLMM: I(outcome > 0) ~ covariates + (1 | org_id)
#
# See cross_validation_metrics.md for full design rationale.
#
source("common/data_prep.R")
source("common/temporal_features.R")
source("common/feature_layers.R")
source("common/layer_temporal.R")
source("common/layer_ews.R")
source("common/validation_protocol.R")
source("common/cv_runner.R")
source("rail/config.R")
source("rail/data_prep.R")
source("rail/fit_models.R")



# Run CV
cv_results <- run_cv(
  protocol = protocol,
  cfg_dir = CFG_DIR,
  events_df = rail_raw,
  ops_features = ops_features,
  n_workers = 20L,
  output_path = "results_new_new/rail/cv_results.parquet",
  checkpoint_dir = "results_new_new/rail/cv_checkpoints",
  config_ids = NULL #c(1) #c(1,2,3,4,5,6,7,8,9,10)
)


