# =============================================================================
# phmsa/1_phmsa_multiverse.R — PHMSA Multiverse Analysis
# =============================================================================
#
# Runs run_multiverse() for each pipeline type in PIPELINE_TYPES.
# Each type produces a separate output parquet.
#
# Requires: phmsa/0_main_phmsa.R sourced first.
# =============================================================================

cat("=============================================================\n")
cat("  PHMSA MULTIVERSE ANALYSIS\n")
cat("=============================================================\n\n")

mv_results_all <- vector("list", length(PIPELINE_TYPES))
names(mv_results_all) <- PIPELINE_TYPES

for (pt in PIPELINE_TYPES) {

  out <- mv_output_path(pt)
  cat(sprintf("--- Pipeline type: %s ---\n", pt))

  if (file.exists(out)) {
    cat(sprintf("  Output exists, skipping: %s\n", basename(out)))
    mv_results_all[[pt]] <- read_parquet(out) %>% mutate(.pipeline_type = pt)
    next
  }

  protocol_pt <- make_phmsa_protocol(pt)

  results_pt <- tryCatch(
    run_multiverse(
      protocol     = protocol_pt,
      cfg_dir      = CFG_DIR,
      events_df    = NULL,          # events captured in closure
      ops_features = ops_features,
      n_workers    = N_WORKERS,
      output_path  = out,
      config_ids   = NULL
    ),
    error = function(e) {
      message(sprintf("  Multiverse FAILED for %s: %s", pt, conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(results_pt)) {
    mv_results_all[[pt]] <- results_pt %>% mutate(.pipeline_type = pt)
    cat(sprintf("  Done. Rows: %s | Saved: %s\n",
        format(nrow(results_pt), big.mark = ","), basename(out)))
  }
}

# Combine all pipeline types
mv_combined <- bind_rows(mv_results_all)
combined_path <- file.path(OUTPUT_DIR, "mv_results_all_types.parquet")
write_parquet(mv_combined, combined_path)

cat(sprintf("\n--- Multiverse complete ---\n"))
cat(sprintf("  Total rows across all pipeline types: %s\n",
    format(nrow(mv_combined), big.mark = ",")))
cat(sprintf("  Combined results: %s\n", combined_path))

if ("status" %in% names(mv_combined)) {
  cat("  Status breakdown:\n")
  print(count(mv_combined, .pipeline_type, status))
}
