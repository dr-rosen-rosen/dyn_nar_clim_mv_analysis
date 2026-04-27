# =============================================================================
# aviation/2_aviation_cv.R — Aviation Cross-Validation
# =============================================================================
#
# Runs run_cv() for every combination in av_spec_grid, mirroring the
# multiverse loop structure. CV uses binary accident occurrence only
# (Brier score requires a binary outcome; hurdle/ordinal are MV-only).
#
# CV strategies:
#   group_kfold  : K=5 folds stratified by airport — assesses
#                  generalisation to held-out airports
#   timeseries   : rolling 24-month test windows with 6-month gap —
#                  assesses temporal generalisation
#
# Requires: aviation/0_main_aviation.R sourced first.
# =============================================================================

cat("=============================================================\n")
cat("  AVIATION CROSS-VALIDATION\n")
cat("=============================================================\n\n")

cv_results_all <- vector("list", nrow(av_spec_grid))

for (i in seq_len(nrow(av_spec_grid))) {

  spec <- av_spec_grid[i, ]
  lbl  <- spec_label(spec$atc_scope, spec$apt_dist_nm, spec$missing_climate)
  out  <- cv_output_path(spec$atc_scope, spec$apt_dist_nm, spec$missing_climate)
  chk  <- file.path(CV_CHECKPOINT_DIR, lbl)

  cat(sprintf("--- Spec %d/%d: %s ---\n", i, nrow(av_spec_grid), lbl))

  if (file.exists(out)) {
    cat(sprintf("  Output exists, skipping: %s\n", basename(out)))
    cv_results_all[[i]] <- read_parquet(out) %>% mutate(.spec = lbl)
    next
  }

  protocol_i <- make_av_protocol(
    atc_scope       = spec$atc_scope,
    apt_dist_nm     = spec$apt_dist_nm,
    missing_climate = spec$missing_climate
  )

  cv_i <- tryCatch(
    run_cv(
      protocol        = protocol_i,
      cfg_dir         = CFG_DIR,
      events_df       = asrs_meta,
      ops_features    = ops_features,
      n_workers       = N_WORKERS,
      output_path     = out,
      checkpoint_dir  = chk,
      config_ids      = NULL
    ),
    error = function(e) {
      message(sprintf("  CV FAILED for spec %s: %s", lbl, conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(cv_i)) {
    cv_results_all[[i]] <- cv_i %>% mutate(.spec = lbl)
    cat(sprintf("  Done. Rows: %s | Saved: %s\n",
        format(nrow(cv_i), big.mark = ","), basename(out)))
  }
}

# Combine all specs
cv_combined <- bind_rows(cv_results_all)
combined_path <- file.path(OUTPUT_DIR, "cv_results_all_specs.parquet")
write_parquet(cv_combined, combined_path)

cat(sprintf("\n--- CV complete ---\n"))
cat(sprintf("  Total rows across all specs: %s\n",
    format(nrow(cv_combined), big.mark = ",")))
cat(sprintf("  Combined results: %s\n", combined_path))

# Quick delta-Brier summary across specs
if (all(c("brier_full", "brier_no_climate", ".spec") %in% names(cv_combined))) {
  delta_summary <- cv_combined %>%
    group_by(.spec, outcome, strategy) %>%
    summarise(
      mean_delta_brier = mean(brier_full - brier_no_climate, na.rm = TRUE),
      n_configs        = n_distinct(config_id),
      .groups          = "drop"
    ) %>%
    arrange(.spec, outcome, strategy)

  cat("\n  Delta-Brier summary (full - no_climate; negative = climate helps):\n")
  print(delta_summary, n = 40)
}
