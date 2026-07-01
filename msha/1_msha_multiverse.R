# =============================================================================
# msha/1_msha_multiverse.R — MSHA Incident-Level Multiverse
# =============================================================================
#
# For each commodity (coal, mnm), sweeps every climate config × climate window
# × incident-level outcome (fatality_binary, injury_binary, days_lost).
#
# Source msha/0_main_msha.R FIRST (provides data, make_msha_protocol(),
# CFG_DIR, N_WORKERS, and path helpers).
# =============================================================================

results_mv <- list()

for (cm in COMMODITIES) {
  cat(sprintf("\n\n#################  MSHA %s — incident MV  #################\n",
              toupper(cm)))

  protocol     <- make_msha_protocol(cm)
  events_c     <- msha_events_all %>% filter(commodity == cm)
  ops_c        <- ops_all %>% filter(commodity == cm)
  output_pq    <- mv_output_path(cm)

  res_c <- run_multiverse(
    protocol     = protocol,
    cfg_dir      = CFG_DIR,
    events_df    = events_c,
    ops_features = ops_c,
    n_workers    = N_WORKERS,
    output_path  = output_pq,
    config_ids   = NULL
  )

  results_mv[[cm]] <- res_c %>% mutate(commodity = cm)
}

cat("\n=== MSHA Incident Multiverse complete ===\n")
mv_combined <- bind_rows(results_mv)
cat("\nStatus by commodity × outcome:\n")
print(mv_combined %>% count(commodity, outcome, status))
