############ Lagged-outcome control: is the climate effect just temporal
############ autocorrelation of past events ("it's just past event rates")?
#
# The critique: events generate narratives that read as poor climate, and
# event rates are autocorrelated, so a climate->future-events association
# could be a spurious echo of an org's own recent event history.
#
# Test: for each main-text best-performing model, refit adding the org's
# OWN lagged event RATE on the model's natural scale -- log((count+0.5)/
# exposure) -- at lag 1 (prior period) and lag 1+2. If the climate
# coefficient survives controlling for the lagged outcome, climate carries
# predictive information beyond simple autoregression of the outcome itself.
#
# Design notes (for defensibility):
#   * Lag is taken over the previous OBSERVED org-period (panel ordered by
#     period within org). First 1 (or 2) periods per org therefore have no
#     lag and are dropped.
#   * All variants (base, +lag1, +lag1&2) are fit on the SAME common sample
#     (rows where BOTH lag1 and lag2 exist), so the change in the climate
#     coefficient reflects the added control, not a sample shift.
#   * Lagged log-rates are z-scored for interpretable competing coefficients,
#     exactly as the sentiment-control covariates were.
#   * The org random intercept already absorbs between-org mean rate; the
#     lagged term therefore tests WITHIN-org autoregressive structure.
#
# Outputs:
#   report_figures_manuscript/sensitivity_lagged_outcome_table.csv
#   report_figures_manuscript/sensitivity_lagged_outcome_forest_{log,rateratio}.pdf

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(glmmTMB)
  library(broom.mixed); library(slider); library(lubridate); library(stringr)
  library(readxl); library(here)
})

source("common/postprocessing.R")
source("common/configuration_concordance.R")
source("common/manuscript_figures.R")
source("rail/config.R"); source("common/data_prep.R")
source("common/panel_data_prep.R")
source("rail/data_prep.R"); source("rail/panel_data_prep.R"); source("rail/panel_fit_models.R")
source("nrc/config.R");  source("nrc/panel_data_prep.R"); source("nrc/panel_fit_models.R")
source("aviation/config.R"); source("aviation/data_prep.R")
source("aviation/panel_data_prep.R"); source("aviation/panel_fit_models.R")
# MSHA (coal): panel-prep + fit modules only (not msha/config.R).
source("msha/panel_data_prep.R"); source("msha/panel_fit_models.R")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
DPK <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim"

# =============================================================================
# 1. CHAMPIONS + INDUSTRY METADATA
# =============================================================================

industries <- list(
  rail = list(label = "Rail (FRA)",
              cfg_dir = file.path(DPK, "notebooks/checkpoints/rail_04-14-2026"),
              org_var = "org_id", period_var = "yearmonth"),
  nrc = list(label = "Nuclear (NRC)",
             cfg_dir = file.path(DPK, "notebooks/checkpoints/nrc_04-14-2026"),
             org_var = "facility_site", period_var = "quarter_start"),
  aviation = list(label = "Aviation (NTSB / AIDS / ASRS)",
                  cfg_dir = file.path(DPK, "notebooks/checkpoints/asrs_05-01-2026"),
                  org_var = "airport_id", period_var = "yearmonth"),
  msha = list(label = "Mining (MSHA coal)",
              cfg_dir = file.path(DPK, "notebooks/checkpoints/msha_06-01-2026"),
              org_var = "mine_id", period_var = "quarter_start")
)

champion_table <- build_champion_manuscript_table(
  industries = list(
    rail = list(label="Rail (FRA)", cv_results="results_new_new/rail/panel_cv_results.parquet",
                config_registry=file.path(industries$rail$cfg_dir,"config_registry.csv")),
    nrc = list(label="Nuclear (NRC)", cv_results="results_new_new/nrc/panel_cv_results.parquet",
               config_registry=file.path(industries$nrc$cfg_dir,"config_registry.csv")),
    aviation = list(label="Aviation (NTSB / AIDS / ASRS)", cv_results="results_new_new/aviation/panel_cv_results_quarterly.parquet",
                    config_registry=file.path(industries$aviation$cfg_dir,"config_registry.csv")),
    msha = list(label="Mining (MSHA coal)", cv_results="results_new_new/msha/panel_cv_results_coal_detectability_full.parquet",
                config_registry=file.path(industries$msha$cfg_dir,"config_registry.csv"))),
  report_dir = "report_figures_panel",
  cv_strategies = "timeseries") %>% filter(cv_strategy == "timeseries")

MAIN_CELLS <- c(
  "rail|rate_accidents","rail|rate_injuries","rail|rate_fatalities",
  "nrc|rate_lers","nrc|rate_emerg","nrc|rate_scrams","nrc|rate_pct_power_loss",
  "aviation|rate_aids_noharm","aviation|rate_aids_propdamage","aviation|rate_ntsb_serious_fatal",
  "msha|rate_t0","msha|rate_t1","msha|rate_t2","msha|rate_t3","msha|rate_days_lost")
main_cells <- champion_table %>%
  mutate(key = paste(industry, outcome, sep="|")) %>%
  filter(key %in% MAIN_CELLS)

# =============================================================================
# 2. LOAD DATA + panel closures (no VADER needed here)
# =============================================================================

# --- Rail ---
rail_events <- read_parquet(file.path(DPK,"data/processed/rail/events.parquet")) %>%
  rename(total_persons_killed=`Total Persons Killed`, total_persons_injured=`Total Persons Injured`,
         total_damage_cost=`Total Damage Cost`) %>%
  mutate(total_damage_cost=as.numeric(gsub("[^0-9.]","",total_damage_cost))) %>%
  select(org_id,eid,event_date,total_persons_killed,total_persons_injured,total_damage_cost) %>%
  mutate(event_date=as.Date(event_date))
rail_ops_raw <- readr::read_csv("data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv",
                                show_col_types=FALSE) %>%
  mutate(across(all_of(OPS_VARS), ~ ifelse(.x<0, NA_real_, .x))) %>% mutate(yearmonth=as.Date(yearmonth))
rail_ops_features <- make_ops_features_rolling(rail_ops_raw) %>% distinct(org_id,yearmonth,.keep_all=TRUE)

# --- NRC ---
nrc_events <- read_parquet(file.path(DPK,"data/processed/nrc/events.parquet")) %>%
  mutate(event_date=as.Date(event_date), eid=as.character(event_num))
nrc_ops_raw <- read_parquet(file.path(DPK,"data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet")) %>%
  mutate(quarter_start=as.Date(quarter_start))
nrc_findings <- read_parquet(file.path(DPK,"data/processed/nrc/findings_quarterly.parquet")) %>% mutate(quarter_start=as.Date(quarter_start))
nrc_am <- read_parquet(file.path(DPK,"data/processed/nrc/action_matrix_long.parquet")) %>% mutate(quarter_start=as.Date(quarter_start))

# --- Aviation ---
asrs_meta <- load_asrs_meta_panel(file.path(DPK,"data/processed/aviation/events.parquet"))
av_ops_features <- read_parquet("data/aviation/bts_t100/airport_month_ops.parquet")
ntsb_panel <- load_ntsb_panel_rate("data/aviation/ntsb_av_accident_data/events.xlsx",
                                   "data/aviation/ntsb_av_accident_data/events_pre2008.xlsx", apt_dist_nm=5L, period="quarter")
aids_panel <- load_aids_panel_rate(file.path(DPK,"data/processed/aviation/aids_events.parquet"),
                                   apt_dist_nm=5L, min_year=1988L, period="quarter")

# --- MSHA (coal) ---
msha_events <- read_parquet(file.path(DPK,"data/processed/msha/events.parquet")) %>%
  mutate(mine_id=as.character(mine_id), eid=as.character(eid),
         event_date=as.Date(accident_date), commodity=tolower(trimws(commodity))) %>%
  filter(!is.na(mine_id), !is.na(event_date), commodity=="coal")
msha_ops <- read_parquet(file.path(DPK,"data/processed/msha/ops_mine_quarter.parquet")) %>%
  mutate(mine_id=as.character(mine_id), commodity=tolower(trimws(commodity))) %>%
  filter(commodity=="coal")
msha_cov <- read_parquet(file.path(DPK,"data/processed/msha/covariates_mine_quarter.parquet")) %>%
  mutate(mine_id=as.character(mine_id))

build_panel <- function(industry, parquet_path, cid) {
  if (industry == "rail") {
    prepare_rail_panel_data(parquet_path=parquet_path, config_id=cid,
      events_df=rail_events, ops_raw=rail_ops_raw, ops_features=rail_ops_features,
      min_reports=50L, max_holdover_days=365L, climate_base="overall_final_score")
  } else if (industry == "nrc") {
    prepare_nrc_panel_data(parquet_path=parquet_path, config_id=cid,
      events_df=nrc_events, ops_raw=nrc_ops_raw, findings_df=nrc_findings,
      action_matrix_df=nrc_am, min_reports=MIN_REPORTS_DEFAULT,
      max_holdover_days=365L, climate_base="overall_final_score")
  } else if (industry == "aviation") {
    prepare_aviation_panel_data(parquet_path=parquet_path, config_id=cid,
      asrs_meta_df=asrs_meta, ntsb_panel_df=ntsb_panel, ops_features=av_ops_features,
      aids_panel_df=aids_panel, atc_scope="local_terminal", missing_climate="exclude",
      min_reports=MIN_REPORTS_DEFAULT, period="quarter", max_holdover_days=365L, climate_base="overall_final_score")
  } else {
    prepare_msha_panel_data(parquet_path=parquet_path, config_id=cid,
      events_df=msha_events, ops_raw=msha_ops, covariates_df=msha_cov,
      min_reports=30L, max_holdover_days=365L, climate_base="overall_final_score")
  }
}

.registry <- list(rail = PANEL_OUTCOME_VARS, nrc = PANEL_OUTCOME_VARS_NRC,
                  aviation = PANEL_OUTCOME_VARS_AVIATION, msha = PANEL_OUTCOME_VARS_MSHA)
.builder  <- list(rail = build_panel_formula, nrc = build_nrc_panel_formula,
                  aviation = build_aviation_panel_formula, msha = build_msha_panel_formula)
.resolve_family <- function(fam) switch(fam %||% "nbinom2",
  nbinom2 = glmmTMB::nbinom2(), nbinom1 = glmmTMB::nbinom1(),
  poisson = stats::poisson(link = "log"), tweedie = glmmTMB::tweedie(link = "log"),
  glmmTMB::nbinom2())

#' Fit the best-performing model (optionally augmented with control terms) on a
#' supplied (already sample-restricted) panel and return the climate coefficient.
fit_augmented <- function(industry, outcome, panel, climate_var,
                          org_var, extra_terms = character(0)) {
  cfg <- .registry[[industry]][[outcome]]
  fam <- .resolve_family(cfg$family)
  base_fml <- .builder[[industry]](outcome, climate_var)
  fml <- if (length(extra_terms) > 0)
    update(base_fml, as.formula(paste(". ~ . +", paste(extra_terms, collapse = " + "))))
  else base_fml

  vars <- intersect(all.vars(fml), names(panel))
  d <- panel %>% filter(if_all(all_of(vars), ~ !is.na(.x)))
  if (!is.null(cfg$offset) && cfg$offset %in% names(d)) {
    d <- d %>% filter(.data[[cfg$offset]] > 0)
  }
  if (nrow(d) < 100) return(NULL)

  fit <- tryCatch(suppressWarnings(glmmTMB::glmmTMB(fml, data = d, family = fam)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  fe <- tryCatch(broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE),
                 error = function(e) NULL)
  if (is.null(fe)) return(NULL)
  row <- fe %>% filter(term == climate_var)
  if (nrow(row) == 0) return(NULL)
  # Standardize climate coefficient to PER-SD (matches the main figure), so
  # sensitivity forests share the per-SD rate-ratio axis. p-value invariant.
  sigma <- sd(d[[climate_var]], na.rm = TRUE)
  if (!is.finite(sigma) || sigma == 0) sigma <- 1
  list(n_obs = nrow(d), n_orgs = dplyr::n_distinct(d[[org_var]]),
       climate_estimate = row$estimate[1] * sigma, climate_se = row$std.error[1] * sigma,
       climate_pval = row$p.value[1],
       climate_ci_low = row$conf.low[1] * sigma, climate_ci_high = row$conf.high[1] * sigma,
       status = if (isTRUE(fit$sdr$pdHess)) "success" else "convergence_warning")
}

# =============================================================================
# 3. FITTING LOOP
# =============================================================================

cat("=== Lagged-outcome control sensitivity ===\n")
rows <- list()

for (i in seq_len(nrow(main_cells))) {
  cell <- main_cells[i, ]
  ind  <- cell$industry; outc <- cell$outcome
  cid  <- as.character(cell$best_config_id); cv <- cell$best_climate_var
  org_var <- industries[[ind]]$org_var; per_var <- industries[[ind]]$period_var
  cfg  <- .registry[[ind]][[outc]]
  resp <- cfg$var; expo <- cfg$offset
  cat(sprintf("\n[%s | %s] cfg=%s cv=%s  resp=%s expo=%s\n",
              ind, outc, cid, cv, resp, expo %||% "<none>"))

  real_path <- file.path(industries[[ind]]$cfg_dir, "_cfg", cid, "results.parquet")
  panel_real <- tryCatch(build_panel(ind, real_path, cid),
                         error = function(e) { cat("  PANEL ERR:", conditionMessage(e), "\n"); NULL })
  if (is.null(panel_real)) next
  if (is.null(expo) || !expo %in% names(panel_real)) { cat("  no offset/exposure -- skipped\n"); next }

  # --- Build lagged log event-rate within org, ordered by period ---
  panel_lag <- panel_real %>%
    mutate(.org = .data[[org_var]], .per = .data[[per_var]],
           .lograte = ifelse(!is.na(.data[[resp]]) & !is.na(.data[[expo]]) & .data[[expo]] > 0,
                             log((.data[[resp]] + 0.5) / .data[[expo]]), NA_real_)) %>%
    arrange(.org, .per) %>%
    group_by(.org) %>%
    mutate(lag1_lograte = dplyr::lag(.lograte, 1),
           lag2_lograte = dplyr::lag(.lograte, 2)) %>%
    ungroup() %>%
    mutate(lag1 = as.numeric(scale(lag1_lograte)),
           lag2 = as.numeric(scale(lag2_lograte)))

  # Common sample: rows where BOTH lags exist (fair base-vs-augmented compare)
  panel_cs <- panel_lag %>% filter(!is.na(lag1), !is.na(lag2))
  cat(sprintf("  rows: full=%d  common-sample(lag1&2)=%d  orgs=%d\n",
              nrow(panel_lag), nrow(panel_cs), dplyr::n_distinct(panel_cs$.org)))
  if (nrow(panel_cs) < 100) { cat("  common sample too small -- skipped\n"); next }

  variants <- list(
    base       = character(0),
    plus_lag1  = "lag1",
    plus_lag12 = c("lag1", "lag2")
  )
  for (vn in names(variants)) {
    fit <- fit_augmented(ind, outc, panel_cs, cv, org_var, variants[[vn]])
    if (is.null(fit)) { cat(sprintf("  %-11s FIT FAILED\n", vn)); next }
    rows[[length(rows)+1L]] <- tibble(
      industry = ind, industry_label = cell$industry_label,
      outcome = outc, outcome_label = cell$outcome_label,
      variant = vn, best_config_id = cid, best_climate_var = cv,
      n_obs = fit$n_obs, n_orgs = fit$n_orgs,
      climate_estimate = fit$climate_estimate, climate_se = fit$climate_se,
      climate_pval = fit$climate_pval,
      climate_ci_low = fit$climate_ci_low, climate_ci_high = fit$climate_ci_high,
      status = fit$status)
    cat(sprintf("  %-11s beta=%+.3f  p=%.2g  (n=%d)\n",
                vn, fit$climate_estimate, fit$climate_pval, fit$n_obs))
  }
}

lagged_outcome_table <- bind_rows(rows)

# =============================================================================
# 4. OUTPUT
# =============================================================================

if (nrow(lagged_outcome_table) == 0) {
  cat("\nNo rows produced.\n")
} else {
  readr::write_csv(lagged_outcome_table,
                   "report_figures_manuscript/sensitivity_lagged_outcome_table.csv")

  cat("\n=== Climate coefficient: base vs +lagged outcome ===\n")
  print(lagged_outcome_table %>%
          arrange(industry, outcome, variant) %>%
          select(industry_label, outcome_label, variant, n_obs,
                  climate_estimate, climate_se, climate_pval))

  make_forest <- function(d, scale) {
    if (scale == "rate_ratio") {
      d <- d %>% mutate(.e = exp(climate_estimate), .lo = exp(climate_ci_low), .hi = exp(climate_ci_high))
      ref <- 1; xlab <- "Rate ratio per unit climate (95% CI)"
    } else {
      d <- d %>% mutate(.e = climate_estimate, .lo = climate_ci_low, .hi = climate_ci_high)
      ref <- 0; xlab <- "Climate coefficient (log rate-ratio, 95% CI)"
    }
    d <- d %>% mutate(.cell = sprintf("%s — %s", .short_industry_label(industry_label), outcome_label),
                      .cell = forcats::fct_inorder(.cell),
                      variant = factor(variant, levels = c("base","plus_lag1","plus_lag12")))
    p <- ggplot(d, aes(.e, .cell, color = variant, shape = variant)) +
      geom_vline(xintercept = ref, linetype = "dashed", color = "gray60") +
      geom_pointrange(aes(xmin = .lo, xmax = .hi),
                      position = position_dodge(width = 0.6), size = 0.4) +
      scale_color_manual(values = c(base="#000000", plus_lag1="#d6604d", plus_lag12="#2c7bb6"),
        labels = c(base="Best-performing model (no lag)",
                   plus_lag1="+ 1-period lagged outcome rate",
                   plus_lag12="+ 1- and 2-period lags"), name = NULL) +
      scale_shape_manual(values = c(base=16, plus_lag1=17, plus_lag12=15),
        labels = c(base="Best-performing model (no lag)",
                   plus_lag1="+ 1-period lagged outcome rate",
                   plus_lag12="+ 1- and 2-period lags"), name = NULL) +
      labs(x = xlab, y = NULL,
           title = "Autocorrelation robustness: does climate survive controlling for the org's own lagged event rate?",
           subtitle = "Lagged log event-rate (z-scored) within organization; all variants on a common sample. Bars are 95% CIs.") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(),
            plot.subtitle = element_text(color = "gray40"),
            legend.position = "bottom")
    if (scale == "rate_ratio") p <- p + scale_x_log10(labels = scales::label_number(drop0trailing = TRUE))
    p
  }

  ggsave("report_figures_manuscript/sensitivity_lagged_outcome_forest_log.pdf",
         make_forest(lagged_outcome_table, "log"), width = 10, height = 6)
  ggsave("report_figures_manuscript/sensitivity_lagged_outcome_forest_rateratio.pdf",
         make_forest(lagged_outcome_table, "rate_ratio"), width = 10, height = 6)

  cat("\nOutput:\n")
  cat("  report_figures_manuscript/sensitivity_lagged_outcome_table.csv\n")
  cat("  report_figures_manuscript/sensitivity_lagged_outcome_forest_{log,rateratio}.pdf\n")
}
