# =============================================================================
# rail/phase1_panel_diagnostic.R — Phase 1 Diagnostic for Panel-Rate Modeling
# =============================================================================
#
# Single-spec sanity check for Option B (panel rate models). Runs ONE climate
# config, ONE climate window, fits panel-rate models for accidents and
# injuries, and prints a side-by-side comparison with the event-level
# conditional Poisson coefficient on the same data.
#
# This is a diagnostic — it does NOT replace the multiverse runner. The point
# is to verify:
#   (a) the panel construction is sane (% of org-months with valid climate,
#       % with events, exposure totals)
#   (b) the offset model fits and converges
#   (c) the climate coefficient on the panel-rate side has a plausible
#       magnitude/direction relative to the event-level severity model
#
# Run from project root: Rscript rail/phase1_panel_diagnostic.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(glmmTMB)
  library(broom.mixed)
})

# --- Source order matters ---
source("rail/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("rail/data_prep.R")        # make_ops_features_rolling, prepare_config_data
source("rail/panel_data_prep.R")
source("rail/fit_models.R")        # fit_single_rail_model (event-level)
source("rail/panel_fit_models.R")


# =============================================================================
# CONFIG — edit paths to match your environment
# =============================================================================

CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026"
EVENTS_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet"
OPS_PATH    <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"

CONFIG_ID   <- "0"
CLIMATE_WINDOW <- "overall_final_score_ewmaLAG_540d_hl270d"  # one of the WINDOW_SPECS combinations
PANEL_OUTCOMES <- c("rate_accidents", "rate_injuries")


# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("\n=== Loading data ===\n")

rail_events <- arrow::read_parquet(EVENTS_PATH) %>%
  rename(total_persons_killed  = `Total Persons Killed`,
         total_persons_injured = `Total Persons Injured`,
         total_damage_cost     = `Total Damage Cost`) %>%
  mutate(total_damage_cost = as.numeric(gsub("[^0-9.]", "", total_damage_cost))) %>%
  select(org_id, eid, event_date,
         total_persons_killed, total_persons_injured, total_damage_cost) %>%
  mutate(event_date = as.Date(event_date))

ops_raw <- read_csv(OPS_PATH, show_col_types = FALSE) %>%
  mutate(across(all_of(OPS_VARS), ~ ifelse(.x < 0, NA_real_, .x))) %>%
  mutate(yearmonth = as.Date(yearmonth))

ops_features <- make_ops_features_rolling(ops_raw) %>%
  distinct(org_id, yearmonth, .keep_all = TRUE)

cat(sprintf("  events: %d rows, %d orgs, %s to %s\n",
            nrow(rail_events), n_distinct(rail_events$org_id),
            min(rail_events$event_date), max(rail_events$event_date)))
cat(sprintf("  ops:    %d rows, %d orgs, %s to %s\n",
            nrow(ops_raw), n_distinct(ops_raw$org_id),
            min(ops_raw$yearmonth), max(ops_raw$yearmonth)))


# =============================================================================
# 2. BUILD PANEL
# =============================================================================

cat("\n=== Building panel for config", CONFIG_ID, "===\n")

cfg_path <- file.path(CFG_DIR, "_cfg", CONFIG_ID, "results.parquet")
panel <- prepare_rail_panel_data(
  parquet_path     = cfg_path,
  config_id        = CONFIG_ID,
  events_df        = rail_events,
  ops_raw          = ops_raw,
  ops_features     = ops_features,
  min_reports      = 50L,
  max_holdover_days = 365L,
  climate_base     = "overall_final_score"
)

# --- Panel diagnostics ---
cat("\n--- Panel structure ---\n")
cat(sprintf("  rows (org-months):       %d\n", nrow(panel)))
cat(sprintf("  unique orgs:             %d\n", n_distinct(panel$org_id)))
cat(sprintf("  date range:              %s to %s\n",
            min(panel$yearmonth), max(panel$yearmonth)))
cat(sprintf("  org-months with >=1 event: %d (%.1f%%)\n",
            sum(panel$n_accidents > 0), 100 * mean(panel$n_accidents > 0)))
cat(sprintf("  org-months with valid %s: %d (%.1f%%)\n",
            CLIMATE_WINDOW,
            sum(!is.na(panel[[CLIMATE_WINDOW]])),
            100 * mean(!is.na(panel[[CLIMATE_WINDOW]]))))
cat(sprintf("  org-months with non-NA train_miles>0: %d (%.1f%%)\n",
            sum(!is.na(panel$train_miles) & panel$train_miles > 0),
            100 * mean(!is.na(panel$train_miles) & panel$train_miles > 0)))

cat("\n--- Outcome totals across panel ---\n")
print(panel %>% summarise(
  n_accidents_total = sum(n_accidents),
  injured_total     = sum(sum_injured),
  killed_total      = sum(sum_killed),
  damage_total      = sum(sum_damage),
  exposure_train_miles = sum(train_miles, na.rm = TRUE),
  exposure_staff_hours = sum(staff_hours, na.rm = TRUE)
))


# =============================================================================
# 3. FIT PANEL-RATE MODELS
# =============================================================================

cat("\n=== Panel-rate fits ===\n")

climate_sd <- sd(panel[[CLIMATE_WINDOW]], na.rm = TRUE)
cat(sprintf("\n[climate] %s: mean=%.4f, sd=%.4f, range=[%.3f, %.3f]\n",
            CLIMATE_WINDOW,
            mean(panel[[CLIMATE_WINDOW]], na.rm = TRUE),
            climate_sd,
            min(panel[[CLIMATE_WINDOW]], na.rm = TRUE),
            max(panel[[CLIMATE_WINDOW]], na.rm = TRUE)))

panel_results <- map(PANEL_OUTCOMES, function(o) {
  cat(sprintf("\n--- %s ---\n", o))
  res <- fit_single_rail_panel_model(panel, CLIMATE_WINDOW, CONFIG_ID, outcome = o)
  cat(sprintf("status: %s | n_obs=%s | n_orgs=%s\n",
              res$status, res$n_obs, res$n_orgs))
  if (!is.null(res$climate_estimate) && !is.na(res$climate_estimate)) {
    rr_per_sd <- exp(res$climate_estimate * climate_sd)
    rr_lo     <- exp(res$climate_ci_low  * climate_sd)
    rr_hi     <- exp(res$climate_ci_high * climate_sd)
    cat(sprintf("CLIMATE coef (log rate ratio per unit):     %.4f  (SE %.4f, p=%.4g)\n",
                res$climate_estimate, res$climate_se, res$climate_pval))
    cat(sprintf("        rate ratio per 1-unit climate:      %.3f  [%.3f, %.3f]\n",
                exp(res$climate_estimate),
                exp(res$climate_ci_low), exp(res$climate_ci_high)))
    cat(sprintf("        rate ratio per +1-SD (sd=%.4f):    %.3f  [%.3f, %.3f]\n",
                climate_sd, rr_per_sd, rr_lo, rr_hi))
  }
  res
}) %>% setNames(PANEL_OUTCOMES)


# =============================================================================
# 4. EVENT-LEVEL COMPARISON (same config, same climate var)
# =============================================================================

cat("\n=== Event-level reference fits (same config, same climate var) ===\n")

event_df <- prepare_config_data(
  parquet_path = cfg_path,
  config_id    = CONFIG_ID,
  events_df    = rail_events,
  ops_features = ops_features,
  min_reports  = 50,
  min_n        = 1
)

event_outcomes_to_fit <- list(
  rate_accidents  = "injuries",   # closest event-level analogue (truncated Poisson)
  rate_injuries   = "injuries"
)

event_results <- map(unique(unlist(event_outcomes_to_fit)), function(o) {
  cat(sprintf("\n--- event-level outcome: %s ---\n", o))
  fit_single_rail_model(event_df, CLIMATE_WINDOW, CONFIG_ID, outcome = o)
}) %>% setNames(unique(unlist(event_outcomes_to_fit)))


# =============================================================================
# 5. SIDE-BY-SIDE
# =============================================================================

cat("\n=== Comparison: panel rate vs event-level severity ===\n")

# Defensive getter — returns NA_real_ for missing keys / NULL values.
.g <- function(lst, key) {
  v <- lst[[key]]
  if (is.null(v) || length(v) == 0) NA_real_ else v[[1]]
}
.gi <- function(lst, key) {
  v <- lst[[key]]
  if (is.null(v) || length(v) == 0) NA_integer_ else as.integer(v[[1]])
}

cmp <- tibble(
  panel_outcome = c("rate_accidents", "rate_injuries"),
  event_outcome = c("injuries (cond)", "injuries (cond)"),

  panel_climate_est = c(.g(panel_results$rate_accidents, "climate_estimate"),
                        .g(panel_results$rate_injuries,  "climate_estimate")),
  panel_climate_p   = c(.g(panel_results$rate_accidents, "climate_pval"),
                        .g(panel_results$rate_injuries,  "climate_pval")),
  panel_n_obs       = c(.gi(panel_results$rate_accidents, "n_obs"),
                        .gi(panel_results$rate_injuries,  "n_obs")),

  event_climate_est = c(.g(event_results$injuries, "climate_estimate_cond"),
                        .g(event_results$injuries, "climate_estimate_cond")),
  event_climate_p   = c(.g(event_results$injuries, "climate_pval_cond"),
                        .g(event_results$injuries, "climate_pval_cond")),
  event_status      = c(.g(event_results$injuries, "status") %||% NA,
                        .g(event_results$injuries, "status") %||% NA),
  event_n_obs       = c(.gi(event_results$injuries, "n_obs"),
                        .gi(event_results$injuries, "n_obs"))
)

print(cmp, n = Inf, width = Inf)

# If the event-level fit failed, surface the error message.
if (!is.null(event_results$injuries$status) &&
    event_results$injuries$status != "success") {
  cat(sprintf("\n[event-level injuries] status=%s | error=%s\n",
              event_results$injuries$status,
              event_results$injuries$error %||% "(no message)"))
}

cat("\nInterpretation guide:\n")
cat("  panel_climate_est > 0 -> poorer climate -> HIGHER event rate per unit exposure\n")
cat("  event_climate_est > 0 -> poorer climate -> MORE severe events given an event\n")
cat("  Sign agreement = consistent story. Disagreement = collider / selection signal.\n")

# Save for follow-up.
out_dir <- "results_new_new/rail/phase1_panel_diagnostic"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(panel, file.path(out_dir, "panel.parquet"))
saveRDS(panel_results, file.path(out_dir, "panel_results.rds"))
saveRDS(event_results, file.path(out_dir, "event_results.rds"))
write_csv(cmp, file.path(out_dir, "comparison.csv"))

cat(sprintf("\nSaved to: %s\n", out_dir))
