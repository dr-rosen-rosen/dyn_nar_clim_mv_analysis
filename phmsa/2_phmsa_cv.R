# =============================================================================
# phmsa/2_phmsa_cv.R — PHMSA Cross-Validation
# =============================================================================
#
# Runs run_cv() for each pipeline type in PIPELINE_TYPES.
# CV uses binary outcomes only (injury_binary, fatality_binary) for Brier score.
#
# CV strategies:
#   group_kfold : K=5 folds stratified by operator — generalisation to held-out operators
#   timeseries  : rolling 24-month test windows with 6-month gap — temporal generalisation
#
# Requires: phmsa/0_main_phmsa.R sourced first.
# =============================================================================

cat("=============================================================\n")
cat("  PHMSA CROSS-VALIDATION\n")
cat("=============================================================\n\n")

cv_results_all <- vector("list", length(PIPELINE_TYPES))
names(cv_results_all) <- PIPELINE_TYPES

for (pt in PIPELINE_TYPES) {

  out <- cv_output_path(pt)
  chk <- file.path(CV_CHECKPOINT_DIR, pipeline_label(pt))
  cat(sprintf("--- Pipeline type: %s ---\n", pt))

  if (file.exists(out)) {
    cat(sprintf("  Output exists, skipping: %s\n", basename(out)))
    cv_results_all[[pt]] <- read_parquet(out) %>% mutate(.pipeline_type = pt)
    next
  }

  protocol_pt <- make_phmsa_protocol(pt)

  cv_pt <- tryCatch(
    run_cv(
      protocol       = protocol_pt,
      cfg_dir        = CFG_DIR,
      events_df      = NULL,          # events captured in closure
      ops_features   = ops_features,
      n_workers      = N_WORKERS,
      output_path    = out,
      checkpoint_dir = chk,
      config_ids     = NULL
    ),
    error = function(e) {
      message(sprintf("  CV FAILED for %s: %s", pt, conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(cv_pt)) {
    cv_results_all[[pt]] <- cv_pt %>% mutate(.pipeline_type = pt)
    cat(sprintf("  Done. Rows: %s | Saved: %s\n",
        format(nrow(cv_pt), big.mark = ","), basename(out)))
  }
}

# Combine all pipeline types
cv_combined <- bind_rows(cv_results_all)
combined_path <- file.path(OUTPUT_DIR, "cv_results_all_types.parquet")
write_parquet(cv_combined, combined_path)

cat(sprintf("\n--- CV complete ---\n"))
cat(sprintf("  Total rows across all pipeline types: %s\n",
    format(nrow(cv_combined), big.mark = ",")))
cat(sprintf("  Combined results: %s\n", combined_path))

# Quick delta-Brier summary across pipeline types
if (all(c("brier_full", "brier_no_climate", ".pipeline_type") %in% names(cv_combined))) {
  delta_summary <- cv_combined %>%
    group_by(.pipeline_type, outcome, strategy) %>%
    summarise(
      mean_delta_brier = mean(brier_full - brier_no_climate, na.rm = TRUE),
      n_configs        = n_distinct(config_id),
      .groups          = "drop"
    ) %>%
    arrange(.pipeline_type, outcome, strategy)

  cat("\n  Delta-Brier summary (full - no_climate; negative = climate helps):\n")
  print(delta_summary, n = 40)
}
