# =============================================================================
# nrc/phase1_panel_diagnostic.R — Phase 1 Diagnostic for NRC Panel-Rate Models
# =============================================================================
#
# Single-spec sanity check for the NRC panel-rate track. Runs ONE climate
# config, ONE climate window, fits panel-rate models for LERs and emergency
# declarations, and prints a side-by-side comparison with the event-level
# emerg_binary fit on the same data.
#
# Verifies:
#   (a) facility name normalization correctly bridges events <-> ops
#   (b) panel construction is sane (% facility-quarters with valid climate /
#       events / exposure)
#   (c) the offset model fits and converges
#   (d) the climate coefficient on the panel-rate side has a plausible
#       magnitude/direction relative to the event-level binary emergency model
#
# Run from project root: Rscript nrc/phase1_panel_diagnostic.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(glmmTMB)
  library(broom.mixed)
  library(lme4)        # required by nrc/fit_models.R (uses bare glmer())
})

# --- Source order matters ---
source("nrc/config.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("nrc/data_prep.R")          # prepare_nrc_config_data, join_nrc_ops
source("nrc/panel_data_prep.R")
source("nrc/fit_models.R")          # fit_binary_emerg, fit_power_loss_hurdle
source("nrc/panel_fit_models.R")


# =============================================================================
# CONFIG
# =============================================================================

CFG_DIR     <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026"
EVENTS_PATH <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet"
OPS_PATH    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet"

CONFIG_ID      <- "0"
CLIMATE_WINDOW <- "overall_final_score_ewmaLAG_540d_hl270d"
PANEL_OUTCOMES <- c("rate_lers", "rate_emerg", "rate_pct_power_loss")


# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("\n=== Loading data ===\n")

nrc_events <- arrow::read_parquet(EVENTS_PATH) %>%
  mutate(event_date = as.Date(event_date),
         eid = as.character(event_num))

ops_raw <- arrow::read_parquet(OPS_PATH) %>%
  mutate(quarter_start = as.Date(quarter_start))

cat(sprintf("  events: %d rows, %d facilities, %s to %s\n",
            nrow(nrc_events), n_distinct(nrc_events$facility),
            min(nrc_events$event_date, na.rm = TRUE),
            max(nrc_events$event_date, na.rm = TRUE)))
cat(sprintf("  ops:    %d rows, %d facility_units, %s to %s\n",
            nrow(ops_raw), n_distinct(ops_raw$facility_unit),
            min(ops_raw$quarter_start), max(ops_raw$quarter_start)))

# Sanity-check facility name normalization.
ev_sites  <- unique(normalize_nrc_facility(nrc_events$facility))
ops_sites <- unique(normalize_nrc_facility(ops_raw$facility_unit))
cat(sprintf("  facility-name overlap: %d/%d events sites match ops sites\n",
            sum(ev_sites %in% ops_sites), length(ev_sites)))


# =============================================================================
# 2. BUILD PANEL
# =============================================================================

cat("\n=== Building panel for config", CONFIG_ID, "===\n")

cfg_path <- file.path(CFG_DIR, "_cfg", CONFIG_ID, "results.parquet")
panel <- prepare_nrc_panel_data(
  parquet_path     = cfg_path,
  config_id        = CONFIG_ID,
  events_df        = nrc_events,
  ops_raw          = ops_raw,
  min_reports      = 75L,
  max_holdover_days = 365L,
  climate_base     = "overall_final_score"
)

cat("\n--- Panel structure ---\n")
cat(sprintf("  rows (facility-quarters):        %d\n", nrow(panel)))
cat(sprintf("  unique facilities:               %d\n", n_distinct(panel$facility_site)))
cat(sprintf("  date range:                      %s to %s\n",
            min(panel$quarter_start), max(panel$quarter_start)))
cat(sprintf("  facility-quarters with >=1 LER:  %d (%.1f%%)\n",
            sum(panel$n_lers > 0), 100 * mean(panel$n_lers > 0)))
cat(sprintf("  facility-quarters with >=1 emerg:%d (%.1f%%)\n",
            sum(panel$n_emerg > 0), 100 * mean(panel$n_emerg > 0)))
cat(sprintf("  fac-quarters with valid %s: %d (%.1f%%)\n",
            CLIMATE_WINDOW,
            sum(!is.na(panel[[CLIMATE_WINDOW]])),
            100 * mean(!is.na(panel[[CLIMATE_WINDOW]]))))
cat(sprintf("  fac-quarters with days_at_power_total>0: %d (%.1f%%)\n",
            sum(!is.na(panel$days_at_power_total) & panel$days_at_power_total > 0),
            100 * mean(!is.na(panel$days_at_power_total) & panel$days_at_power_total > 0)))

cat("\n--- Outcome totals across panel ---\n")
print(panel %>% summarise(
  n_lers_total              = sum(n_lers),
  n_emerg_total             = sum(n_emerg),
  sum_pct_power_loss_total  = sum(sum_pct_power_loss),
  exposure_days_at_power    = sum(days_at_power_total, na.rm = TRUE),
  exposure_days_at_full     = sum(days_at_full_total, na.rm = TRUE),
  mean_n_units              = mean(n_units, na.rm = TRUE),
  max_n_units               = max(n_units, na.rm = TRUE)
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
  res <- fit_single_nrc_panel_model(panel, CLIMATE_WINDOW, CONFIG_ID, outcome = o)
  cat(sprintf("status: %s | family=%s | n_obs=%s | n_orgs=%s\n",
              res$status, res$family %||% "?", res$n_obs, res$n_orgs))
  if (!is.na(res$tweedie_power %||% NA_real_)) {
    cat(sprintf("Tweedie variance power p = %.3f  (1=Poisson, 2=Gamma)\n",
                res$tweedie_power))
  }
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
  if (!is.null(res$error)) {
    cat(sprintf("error: %s\n", res$error))
  }
  res
}) %>% setNames(PANEL_OUTCOMES)


# =============================================================================
# 4. EVENT-LEVEL COMPARISON (same config, same climate var)
# =============================================================================

cat("\n=== Event-level reference fits (emerg_binary, same config) ===\n")

# We need the existing event-level prep, which expects ops_features rather
# than ops_raw. Just pass ops_raw — join_nrc_ops will compute between/within
# itself if not present.
event_df <- prepare_nrc_config_data(
  parquet_path = cfg_path,
  config_id    = CONFIG_ID,
  events_df    = nrc_events,
  ops_features = ops_raw,
  min_reports  = MIN_REPORTS_DEFAULT,
  min_n        = 1
)

if (!is.null(event_df) && CLIMATE_WINDOW %in% names(event_df)) {
  event_emerg <- fit_binary_emerg(event_df, CLIMATE_WINDOW, CONFIG_ID)
  cat("\n--- event-level outcome: emerg_binary ---\n")
  print(event_emerg, width = Inf)
  if (!is.null(event_emerg$status) && event_emerg$status == "failed") {
    cat("\n[event-level] fit returned status=failed.\n")
    cat(sprintf("  error field:    '%s'\n",    event_emerg$error    %||% ""))
    cat(sprintf("  warnings field: '%s'\n", event_emerg$warnings %||% ""))
    # Manual debug refit so we can see the actual error/warning.
    cat("\n[event-level] manual debug refit (no tryCatch swallowing)...\n")
    ops_cols <- c(paste0(NRC_OPS_VARS, "_between"), paste0(NRC_OPS_VARS, "_within"))
    req <- c(CLIMATE_WINDOW, "emerg_binary", "facility", "yearmonth_num_c", ops_cols)
    md <- event_df %>% filter(if_all(all_of(intersect(req, names(event_df))), ~ !is.na(.)))
    cat(sprintf("  rows=%d, facilities=%d, n_emerg=%d\n",
                nrow(md), n_distinct(md$facility), sum(md$emerg_binary)))
    fml <- as.formula(build_formula(MODEL_FORMULAS$emerg_binary, CLIMATE_WINDOW))
    cat("  formula: ", deparse1(fml), "\n", sep = "")
    dbg <- tryCatch(
      lme4::glmer(fml, data = md, family = binomial,
                  control = lme4::glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 2e5))),
      error   = function(e) { cat("  ERROR: ", conditionMessage(e), "\n"); NULL },
      warning = function(w) { cat("  WARNING: ", conditionMessage(w), "\n"); NULL }
    )
    if (!is.null(dbg)) {
      cat("  debug fit succeeded; coef summary:\n")
      print(summary(dbg)$coefficients[CLIMATE_WINDOW, , drop = FALSE])
    }
  }
} else {
  event_emerg <- NULL
  cat("\nEvent-level prep returned NULL or missing climate column — skipping comparison.\n")
}

# --- Event-level reference for pct_power_loss (hurdle: binary + gamma cond) ---
if (!is.null(event_df) && CLIMATE_WINDOW %in% names(event_df)) {
  cat("\n--- event-level outcome: pct_power_loss (hurdle) ---\n")
  event_pploss <- fit_power_loss_hurdle(event_df, CLIMATE_WINDOW, CONFIG_ID)
  print(event_pploss, width = Inf)
} else {
  event_pploss <- NULL
}


# =============================================================================
# 5. SIDE-BY-SIDE
# =============================================================================

cat("\n=== Comparison: panel rate vs event-level binary ===\n")

.g <- function(lst, key) {
  if (is.null(lst)) return(NA_real_)
  v <- lst[[key]]
  if (is.null(v) || length(v) == 0) NA_real_ else v[[1]]
}
.gi <- function(lst, key) {
  if (is.null(lst)) return(NA_integer_)
  v <- lst[[key]]
  if (is.null(v) || length(v) == 0) NA_integer_ else as.integer(v[[1]])
}

# fit_binary_emerg may return a tibble row or a list — coerce to list-by-name.
event_emerg_l <- if (is.null(event_emerg)) NULL else as.list(event_emerg)

cmp <- tibble(
  panel_outcome = c("rate_lers", "rate_emerg"),
  event_outcome = c("emerg_binary", "emerg_binary"),

  panel_climate_est = c(.g(panel_results$rate_lers,  "climate_estimate"),
                        .g(panel_results$rate_emerg, "climate_estimate")),
  panel_climate_p   = c(.g(panel_results$rate_lers,  "climate_pval"),
                        .g(panel_results$rate_emerg, "climate_pval")),
  panel_n_obs       = c(.gi(panel_results$rate_lers,  "n_obs"),
                        .gi(panel_results$rate_emerg, "n_obs")),

  event_climate_est = c(.g(event_emerg_l, "climate_estimate"),
                        .g(event_emerg_l, "climate_estimate")),
  event_climate_p   = c(.g(event_emerg_l, "climate_p"),
                        .g(event_emerg_l, "climate_p")),
  event_status      = c(.g(event_emerg_l, "status"),
                        .g(event_emerg_l, "status")),
  event_n_obs       = c(.gi(event_emerg_l, "n_obs"),
                        .gi(event_emerg_l, "n_obs"))
)

print(cmp, n = Inf, width = Inf)

# --- pct_power_loss panel (Tweedie) vs event-level hurdle (zi + cond) ---
event_pploss_l <- if (is.null(event_pploss)) NULL else as.list(event_pploss)
cmp_pploss <- tibble(
  panel_outcome = "rate_pct_power_loss",
  panel_climate_est = .g(panel_results$rate_pct_power_loss, "climate_estimate"),
  panel_climate_p   = .g(panel_results$rate_pct_power_loss, "climate_pval"),
  panel_tweedie_p   = .g(panel_results$rate_pct_power_loss, "tweedie_power"),
  panel_n_obs       = .gi(panel_results$rate_pct_power_loss, "n_obs"),
  event_zi_est      = .g(event_pploss_l, "climate_estimate_zi"),
  event_zi_p        = .g(event_pploss_l, "climate_pval_zi"),
  event_cond_est    = .g(event_pploss_l, "climate_estimate_cond"),
  event_cond_p      = .g(event_pploss_l, "climate_pval_cond"),
  event_n_obs       = .gi(event_pploss_l, "n_obs")
)
cat("\n=== Comparison: panel pct_power_loss (Tweedie) vs event-level hurdle ===\n")
print(cmp_pploss, n = Inf, width = Inf)

cat("\nInterpretation guide:\n")
cat("  panel_climate_est > 0 -> poorer climate -> HIGHER event rate per reactor-day\n")
cat("  event_climate_est > 0 -> poorer climate -> emergency MORE likely given an event\n")
cat("  Sign agreement = consistent story. Disagreement = collider / selection signal.\n")

# Save for follow-up.
out_dir <- "results_new_new/nrc/phase1_panel_diagnostic"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(panel, file.path(out_dir, "panel.parquet"))
saveRDS(panel_results, file.path(out_dir, "panel_results.rds"))
saveRDS(event_emerg,   file.path(out_dir, "event_emerg.rds"))
saveRDS(event_pploss,  file.path(out_dir, "event_pploss.rds"))
write_csv(cmp,        file.path(out_dir, "comparison.csv"))
write_csv(cmp_pploss, file.path(out_dir, "comparison_pploss.csv"))

cat(sprintf("\nSaved to: %s\n", out_dir))
