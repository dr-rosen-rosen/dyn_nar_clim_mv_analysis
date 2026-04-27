# =============================================================================
# aviation/1_aviation_multiverse.R — Aviation Multiverse Analysis
# =============================================================================
#
# Runs run_multiverse() for every combination in av_spec_grid.
# Each spec combination produces a separate output parquet tagged with
# its atc_scope × apt_dist_nm × missing_climate label.
#
# Requires: aviation/0_main_aviation.R sourced first.
# =============================================================================

cat("=============================================================\n")
cat("  AVIATION MULTIVERSE ANALYSIS\n")
cat("=============================================================\n\n")

mv_results_all <- vector("list", nrow(av_spec_grid))

for (i in seq_len(nrow(av_spec_grid))) {

  spec <- av_spec_grid[i, ]
  lbl  <- spec_label(spec$atc_scope, spec$apt_dist_nm, spec$missing_climate)
  out  <- mv_output_path(spec$atc_scope, spec$apt_dist_nm, spec$missing_climate)

  cat(sprintf("--- Spec %d/%d: %s ---\n", i, nrow(av_spec_grid), lbl))

  if (file.exists(out)) {
    cat(sprintf("  Output exists, skipping: %s\n", basename(out)))
    mv_results_all[[i]] <- read_parquet(out) %>% mutate(.spec = lbl)
    next
  }

  protocol_i <- make_av_protocol(
    atc_scope       = spec$atc_scope,
    apt_dist_nm     = spec$apt_dist_nm,
    missing_climate = spec$missing_climate
  )

  results_i <- tryCatch(
    run_multiverse(
      protocol     = protocol_i,
      cfg_dir      = CFG_DIR,
      events_df    = asrs_meta,      # asrs_meta passed as events_df
      ops_features = ops_features,
      n_workers    = N_WORKERS,
      output_path  = out,
      config_ids   = NULL
    ),
    error = function(e) {
      message(sprintf("  Multiverse FAILED for spec %s: %s", lbl, conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(results_i)) {
    mv_results_all[[i]] <- results_i %>% mutate(.spec = lbl)
    cat(sprintf("  Done. Rows: %s | Saved: %s\n",
        format(nrow(results_i), big.mark = ","), basename(out)))
  }
}

# Combine all specs into one summary parquet
mv_combined <- bind_rows(mv_results_all)
combined_path <- file.path(OUTPUT_DIR, "mv_results_all_specs.parquet")
write_parquet(mv_combined, combined_path)

cat(sprintf("\n--- Multiverse complete ---\n"))
cat(sprintf("  Total rows across all specs: %s\n",
    format(nrow(mv_combined), big.mark = ",")))
cat(sprintf("  Combined results: %s\n", combined_path))

if ("status" %in% names(mv_combined)) {
  cat("  Status breakdown:\n")
  print(count(mv_combined, .spec, status))
}
