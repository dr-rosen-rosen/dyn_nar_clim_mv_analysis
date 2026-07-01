############ ED Figure 4 — cross-validation sign concordance.
#
# Does the climate -> outcome association hold under the more conservative
# organization-blocked CV (which prevents any single organization's history
# from appearing in both train and test folds), not just under time-series CV?
#
# For each main-text cell we take the best-performing specification selected
# UNDER EACH CV strategy, refit it, and extract the per-SD standardized climate
# coefficient (estimate x in-panel SD of the climate variable) with 95% CIs.
# We then plot the two strategies side by side per cell. The figure documents
# that the SIGN and approximate magnitude of the effect are concordant across
# CV strategies; the best specification (and therefore the exact per-SD value)
# can differ between strategies because each strategy selects independently.
#
# Reuses refit_champion_and_predict() + extract_champion_standardized_coefs()
# (the same machinery behind main-text Figure 1A), so model structure matches
# the paper specification exactly.
#
# Outputs:
#   report_figures_manuscript/extended_data_cv_sign_comparison_table.csv
#   report_figures_manuscript/extended_data_cv_sign_comparison_{log,rateratio}.pdf

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
# MSHA (coal): panel-prep + fit modules only (not msha/config.R; min_reports
# passed explicitly so the other arms' MIN_REPORTS_DEFAULT is left intact).
source("msha/panel_data_prep.R"); source("msha/panel_fit_models.R")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
DPK <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim"

# Main-text cells (detectability ladders + rail/NRC). EXCLUDE derived as the
# complement from the champion table below.
MAIN_CELLS <- c(
  "rail|rate_accidents","rail|rate_injuries","rail|rate_fatalities",
  "nrc|rate_lers","nrc|rate_emerg","nrc|rate_scrams","nrc|rate_pct_power_loss",
  "aviation|rate_aids_noharm","aviation|rate_aids_propdamage",
  "aviation|rate_ntsb_serious_fatal",
  "msha|rate_t0","msha|rate_t1","msha|rate_t2","msha|rate_t3","msha|rate_days_lost")

industries <- list(
  rail = list(label="Rail (FRA)", cfg_dir=file.path(DPK,"notebooks/checkpoints/rail_04-14-2026"),
              cv_results="results_new_new/rail/panel_cv_results.parquet"),
  nrc  = list(label="Nuclear (NRC)", cfg_dir=file.path(DPK,"notebooks/checkpoints/nrc_04-14-2026"),
              cv_results="results_new_new/nrc/panel_cv_results.parquet"),
  aviation = list(label="Aviation (NTSB / AIDS / ASRS)", cfg_dir=file.path(DPK,"notebooks/checkpoints/asrs_05-01-2026"),
                  cv_results="results_new_new/aviation/panel_cv_results_quarterly.parquet"),
  msha = list(label="Mining (MSHA coal)", cfg_dir=file.path(DPK,"notebooks/checkpoints/msha_06-01-2026"),
              cv_results="results_new_new/msha/panel_cv_results_coal_detectability_full.parquet")
)

champion_table <- build_champion_manuscript_table(
  industries = lapply(industries, function(x)
    list(label=x$label, cv_results=x$cv_results,
         config_registry=file.path(x$cfg_dir,"config_registry.csv"))),
  report_dir = "report_figures_panel",
  cv_strategies = c("timeseries","group_kfold"))

.ct_oc <- if ("outcome" %in% names(champion_table)) champion_table$outcome else champion_table$panel_outcome
EXCLUDE <- setdiff(unique(paste(champion_table$industry, .ct_oc, sep="|")), MAIN_CELLS)

# =============================================================================
# DATA LOADING + panel closures (copied from 5_manuscript_plots.R)
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
rail_panel_data_fn <- function(parquet_path, config_id)
  prepare_rail_panel_data(parquet_path=parquet_path, config_id=config_id,
    events_df=rail_events, ops_raw=rail_ops_raw, ops_features=rail_ops_features,
    min_reports=50L, max_holdover_days=365L, climate_base="overall_final_score")

# --- NRC ---
nrc_events <- read_parquet(file.path(DPK,"data/processed/nrc/events.parquet")) %>%
  mutate(event_date=as.Date(event_date), eid=as.character(event_num))
nrc_ops_raw <- read_parquet(file.path(DPK,"data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet")) %>%
  mutate(quarter_start=as.Date(quarter_start))
nrc_panel_data_fn <- function(parquet_path, config_id)
  prepare_nrc_panel_data(parquet_path=parquet_path, config_id=config_id,
    events_df=nrc_events, ops_raw=nrc_ops_raw, min_reports=MIN_REPORTS_DEFAULT,
    max_holdover_days=365L, climate_base="overall_final_score")

# --- Aviation ---
asrs_meta <- load_asrs_meta_panel(file.path(DPK,"data/processed/aviation/events.parquet"))
av_ops_features <- read_parquet("data/aviation/bts_t100/airport_month_ops.parquet")
ntsb_panel <- load_ntsb_panel_rate("data/aviation/ntsb_av_accident_data/events.xlsx",
                                   "data/aviation/ntsb_av_accident_data/events_pre2008.xlsx",
                                   apt_dist_nm=5L, period="quarter")
aids_panel <- load_aids_panel_rate(file.path(DPK,"data/processed/aviation/aids_events.parquet"),
                                   apt_dist_nm=5L, min_year=1988L, period="quarter")
aviation_panel_data_fn <- function(parquet_path, config_id)
  prepare_aviation_panel_data(parquet_path=parquet_path, config_id=config_id,
    asrs_meta_df=asrs_meta, ntsb_panel_df=ntsb_panel, ops_features=av_ops_features,
    aids_panel_df=aids_panel, atc_scope="local_terminal", missing_climate="exclude",
    min_reports=MIN_REPORTS_DEFAULT, period="quarter", max_holdover_days=365L,
    climate_base="overall_final_score")

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
msha_panel_data_fn <- function(parquet_path, config_id)
  prepare_msha_panel_data(parquet_path=parquet_path, config_id=config_id,
    events_df=msha_events, ops_raw=msha_ops, covariates_df=msha_cov,
    min_reports=30L, max_holdover_days=365L, climate_base="overall_final_score")

panel_fns <- list(rail=rail_panel_data_fn, nrc=nrc_panel_data_fn,
                  aviation=aviation_panel_data_fn, msha=msha_panel_data_fn)

spec_map <- list(
  rail = list(
    fit_fn = function(panel, cv, cid, outcome) fit_single_rail_panel_model(panel, cv, cid, outcome=outcome),
    var    = list(rate_accidents="n_accidents", rate_injuries="sum_injured", rate_fatalities="sum_killed"),
    offset = list(rate_accidents="train_miles", rate_injuries="staff_hours", rate_fatalities="train_miles"),
    bw     = list(rate_accidents=c("passenger_miles_between","passenger_miles_within","staff_hours_between","staff_hours_within"),
                  rate_injuries=c("train_miles_between","train_miles_within","passenger_miles_between","passenger_miles_within"),
                  rate_fatalities=c("passenger_miles_between","passenger_miles_within","staff_hours_between","staff_hours_within")),
    org="org_id",
    family=list(rate_accidents="nbinom2", rate_injuries="nbinom2", rate_fatalities="nbinom2")),
  nrc = list(
    fit_fn = function(panel, cv, cid, outcome) fit_single_nrc_panel_model(panel, cv, cid, outcome=outcome),
    var    = list(rate_lers="n_lers", rate_emerg="n_emerg", rate_scrams="n_scrams", rate_pct_power_loss="sum_pct_power_loss"),
    offset = list(rate_lers="days_at_power_total", rate_emerg="days_at_power_total",
                  rate_scrams="days_at_power_total", rate_pct_power_loss="days_at_power_total"),
    bw     = list(rate_lers=c("power_std_mean_between","power_std_mean_within"),
                  rate_emerg=c("power_std_mean_between","power_std_mean_within"),
                  rate_scrams=c("power_std_mean_between","power_std_mean_within"),
                  rate_pct_power_loss=c("power_std_mean_between","power_std_mean_within")),
    org="facility_site",
    family=list(rate_lers="nbinom2", rate_emerg="nbinom2", rate_scrams="nbinom2", rate_pct_power_loss="tweedie")),
  aviation = local({
    .o <- c("rate_aids_noharm","rate_aids_propdamage","rate_ntsb_serious_fatal","rate_ntsb_fatal","rate_ntsb_serious")
    .v <- c(n_aids_noharm="rate_aids_noharm", n_aids_propdamage="rate_aids_propdamage",
            n_ntsb_serious_fatal="rate_ntsb_serious_fatal", n_ntsb_fatal="rate_ntsb_fatal",
            n_ntsb_serious="rate_ntsb_serious")
    list(
      fit_fn = function(panel, cv, cid, outcome) fit_single_aviation_panel_model(panel, cv, cid, outcome=outcome),
      var    = setNames(as.list(names(.v)), unname(.v)),
      offset = setNames(rep(list("departures"), length(.o)), .o),
      bw     = setNames(rep(list(c("seats_between","seats_within","passengers_between","passengers_within")), length(.o)), .o),
      org="airport_id",
      family = setNames(rep(list("nbinom2"), length(.o)), .o))
  }),
  msha = list(
    fit_fn = function(panel, cv, cid, outcome) fit_single_msha_panel_model(panel, cv, cid, outcome=outcome),
    var    = list(rate_t0="n_t0", rate_t1="n_t1", rate_t2="n_t2", rate_t3="n_t3", rate_days_lost="sum_days_lost"),
    offset = list(rate_t0="hours_worked", rate_t1="hours_worked", rate_t2="hours_worked", rate_t3="hours_worked", rate_days_lost="hours_worked"),
    bw     = list(rate_t0=c("hours_within"), rate_t1=c("hours_within"), rate_t2=c("hours_within"), rate_t3=c("hours_within"), rate_days_lost=c("hours_within")),
    org="mine_id",
    family=list(rate_t0="nbinom2", rate_t1="nbinom2", rate_t2="nbinom2", rate_t3="nbinom2", rate_days_lost="tweedie")))

# =============================================================================
# REFIT each main cell under BOTH CV strategies; extract per-SD climate coef
# =============================================================================

refit_for_strategy <- function(strategy) {
  champs <- champion_table %>%
    filter(cv_strategy == strategy,
           !paste(industry, outcome, sep="|") %in% EXCLUDE)
  pd <- list()
  for (i in seq_len(nrow(champs))) {
    row <- champs[i, ]; ind <- row$industry; outc <- row$outcome
    sm <- spec_map[[ind]]
    if (is.null(sm$offset[[outc]])) next
    cat(sprintf("  [%s] %s — %s ... ", strategy, ind, outc))
    res <- tryCatch(refit_champion_and_predict(
      industry_key=ind, outcome=outc, cv_strategy=strategy,
      champion_table=champion_table, panel_data_fn=panel_fns[[ind]],
      cfg_dir=industries[[ind]]$cfg_dir, fit_fn=sm$fit_fn,
      outcome_offset=sm$offset[[outc]], outcome_bw_terms=sm$bw[[outc]],
      outcome_org_var=sm$org, outcome_var=sm$var[[outc]],
      outcome_family=sm$family[[outc]]),
      error=function(e){ cat("ERR:", conditionMessage(e), "\n"); NULL })
    if (!is.null(res)) { pd[[paste(ind, outc, sep="|")]] <- res; cat("ok\n") }
  }
  pd
}

cat("=== Refitting best models under each CV strategy ===\n")
pd_ts <- refit_for_strategy("timeseries")
pd_gk <- refit_for_strategy("group_kfold")

coefs_ts <- extract_champion_standardized_coefs(pd_ts, champion_table) %>%
  filter(variable_type == "climate") %>% mutate(cv_strategy = "timeseries")
coefs_gk <- extract_champion_standardized_coefs(pd_gk, champion_table) %>%
  filter(variable_type == "climate") %>% mutate(cv_strategy = "group_kfold")

cv_coefs <- bind_rows(coefs_ts, coefs_gk)

# =============================================================================
# OUTPUT
# =============================================================================

if (nrow(cv_coefs) == 0) {
  cat("\nNo coefficients produced.\n")
} else {
  readr::write_csv(cv_coefs, "report_figures_manuscript/extended_data_cv_sign_comparison_table.csv")

  cat("\n=== Per-SD climate coefficient by CV strategy ===\n")
  print(cv_coefs %>% arrange(industry, outcome, cv_strategy) %>%
          select(industry_label, outcome_label, cv_strategy,
                 estimate_std, ci_low_std, ci_high_std))

  # Sign concordance summary
  conc <- cv_coefs %>% select(industry, outcome, cv_strategy, estimate_std) %>%
    pivot_wider(names_from=cv_strategy, values_from=estimate_std) %>%
    filter(!is.na(timeseries), !is.na(group_kfold)) %>%
    mutate(same_sign = sign(timeseries) == sign(group_kfold))
  cat(sprintf("\nSign concordance: %d/%d cells agree in direction.\n",
              sum(conc$same_sign), nrow(conc)))

  make_forest <- function(d, scale) {
    if (scale == "rate_ratio") {
      d <- d %>% mutate(.e=exp(estimate_std), .lo=exp(ci_low_std), .hi=exp(ci_high_std))
      ref <- 1; xlab <- "Rate ratio per +1 SD climate (95% CI)"
    } else {
      d <- d %>% mutate(.e=estimate_std, .lo=ci_low_std, .hi=ci_high_std)
      ref <- 0; xlab <- "Per-SD climate coefficient (log rate-ratio, 95% CI)"
    }
    d <- d %>% mutate(.ind = .short_industry_label(industry_label),
                      .ind = factor(.ind, levels = sort(unique(.ind))),
                      .cell = factor(outcome_label, levels = rev(unique(outcome_label))),
                      cv_strategy=factor(cv_strategy, levels=c("timeseries","group_kfold")))
    p <- ggplot(d, aes(.e, .cell, color=cv_strategy, shape=cv_strategy)) +
      geom_vline(xintercept=ref, linetype="dashed", color="gray60") +
      geom_pointrange(aes(xmin=.lo, xmax=.hi), position=position_dodge(width=0.5), size=0.45) +
      facet_grid(rows=vars(.ind), scales="free_y", space="free_y", switch="y") +
      scale_color_manual(values=c(timeseries="#2c7bb6", group_kfold="#d6604d"),
        labels=c(timeseries="Time-series CV", group_kfold="Organization-blocked CV"), name=NULL) +
      scale_shape_manual(values=c(timeseries=16, group_kfold=17),
        labels=c(timeseries="Time-series CV", group_kfold="Organization-blocked CV"), name=NULL) +
      labs(x=xlab, y=NULL,
           title="Cross-validation robustness: per-SD climate effect under time-series vs organization-blocked CV",
           subtitle="Best-performing specification selected independently under each CV strategy; per-SD standardized. Bars are 95% CIs.") +
      theme_minimal(base_size=10) +
      theme(panel.grid.minor=element_blank(), plot.subtitle=element_text(color="gray40"),
            legend.position="bottom",
            panel.spacing.y=grid::unit(4,"pt"), strip.placement="outside",
            strip.text.y.left=element_text(angle=0, face="bold", hjust=1),
            strip.background=element_rect(fill="grey92", colour=NA))
    if (scale == "rate_ratio") p <- p + scale_x_log10(labels=scales::label_number(drop0trailing=TRUE))
    p
  }

  ggsave("report_figures_manuscript/extended_data_cv_sign_comparison_log.pdf",
         make_forest(cv_coefs, "log"), width=9, height=5)
  ggsave("report_figures_manuscript/extended_data_cv_sign_comparison_rateratio.pdf",
         make_forest(cv_coefs, "rate_ratio"), width=9, height=5)

  cat("\nOutput:\n")
  cat("  report_figures_manuscript/extended_data_cv_sign_comparison_table.csv\n")
  cat("  report_figures_manuscript/extended_data_cv_sign_comparison_{log,rateratio}.pdf\n")
}
