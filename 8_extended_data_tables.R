############ Extended Data Tables 1-7 (publication-ready CSVs) + crosswalk
#
# Assembles Extended Data tables from already-generated artifacts:
#   champion_summary.csv, table1_sources_long.csv, table1_panel_summary.csv,
#   figure1_coefs_standardized.csv, report_figures_panel/<ind>/champions_candidates.csv,
#   results_new_new/<ind>/panel_mv_results.parquet, config registries.
#
# Outputs (report_figures_manuscript/extended_data/):
#   ED_table1_sample_construction.csv
#   ED_table2_bestmodel_specs_primary.csv
#   ED_table3_robustness_coarse_granular.csv
#   ED_table4_robustness_grain_scope.csv
#   ED_table5_convergence_diagnostics.csv
#   ED_table6_candidate_pool_composition.csv
#   ED_table7_climate_vs_operational_coefs.csv
#   ED_crosswalk_maintext_to_extended_data.csv
#   ED_placeholder_checklist.md
#
# Run: Rscript 8_extended_data_tables.R

suppressPackageStartupMessages({library(tidyverse); library(arrow)})
`%||%` <- function(a,b) if (is.null(a)||length(a)==0) b else a

RFM <- "report_figures_manuscript"
ED  <- file.path(RFM, "extended_data")
dir.create(ED, recursive = TRUE, showWarnings = FALSE)
DPK <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim"

cfg_dirs <- list(
  rail     = file.path(DPK,"notebooks/checkpoints/rail_04-14-2026"),
  nrc      = file.path(DPK,"notebooks/checkpoints/nrc_04-14-2026"),
  aviation = file.path(DPK,"notebooks/checkpoints/asrs_05-01-2026"),
  msha     = file.path(DPK,"notebooks/checkpoints/msha_06-01-2026")
)
mv_paths <- list(
  rail="results_new_new/rail/panel_mv_results.parquet",
  nrc="results_new_new/nrc/panel_mv_results.parquet",
  aviation="results_new_new/aviation/panel_mv_results_quarterly.parquet",
  msha="results_new_new/msha/panel_mv_results_coal_detectability_full.parquet"
)

# ---- Classification of cells ----
# Under the restructured manuscript, ALL main-text outcomes are the detectability
# / consequence ladders (no "lesser outcomes" relegated to Extended Data).
# Extended Data holds supplemental ANALYSES (spec curves, sensitivity, CV-sign,
# convergence, candidate pools, climate-vs-ops) plus ROBUSTNESS cells: the coarse
# pre-decomposition mining outcomes and the granular aviation tiers, which are
# reported as sensitivity checks, not as primary outcomes.
PRIMARY <- c("rail|rate_accidents","rail|rate_injuries","rail|rate_fatalities",
             "nrc|rate_lers","nrc|rate_emerg","nrc|rate_scrams","nrc|rate_pct_power_loss",
             "aviation|rate_aids_noharm","aviation|rate_aids_propdamage",
             "aviation|rate_ntsb_serious_fatal",
             "msha|rate_t0","msha|rate_t1","msha|rate_t2","msha|rate_t3","msha|rate_days_lost")
# Robustness cells (Extended-Data analyses, NOT lesser primary outcomes):
ROBUSTNESS <- c("msha|rate_accidents","msha|rate_injuries","msha|rate_fatal",
                "aviation|rate_ntsb_fatal","aviation|rate_ntsb_serious","aviation|rate_ntsb_minor",
                "aviation|rate_aids_injury","aviation|rate_aids_harm","aviation|rate_aids_all")
# Legacy aliases kept so downstream references don't break (now empty):
RARE_AVIATION <- character(0)
EXTERNAL_NRC  <- character(0)

cell_class <- function(industry, outcome) {
  k <- paste(industry, outcome, sep="|")
  case_when(k %in% PRIMARY ~ "primary",
            k %in% ROBUSTNESS ~ "robustness",
            TRUE ~ "other")
}
ind_label <- function(x) recode(x, rail="Rail", nrc="Nuclear", aviation="Aviation", msha="Mining")
cv_label  <- function(x) recode(x, timeseries="Time-series", group_kfold="Organization-blocked")
unit_label <- function(x) recode(x, rail="railroad-month", nrc="facility-quarter", aviation="airport-quarter", msha="mine-quarter")
thr_label <- function(thk) case_when(thk==99999 ~ "all-segments", thk==3 ~ "top-3", thk==5 ~ "top-5", TRUE ~ as.character(thk))

# ---- Load core artifacts ----
champ <- readr::read_csv(file.path(RFM,"champion_summary.csv"), show_col_types=FALSE)
coefs <- readr::read_csv(file.path(RFM,"figure1_coefs_standardized.csv"), show_col_types=FALSE)
psum  <- readr::read_csv(file.path(RFM,"table1_panel_summary.csv"), show_col_types=FALSE)
srcs  <- readr::read_csv(file.path(RFM,"table1_sources_long.csv"), show_col_types=FALSE)

# Config registries (for thresholding lookup by config_id)
regs <- imap(cfg_dirs, ~ readr::read_csv(file.path(.x,"config_registry.csv"), show_col_types=FALSE) %>%
               mutate(config_id=as.character(config_id)))
# Vectorized + length-0 safe (called in both rowwise and plain mutate contexts).
get_thr <- function(industry, cid) {
  mapply(function(i, c) {
    if (is.na(i) || is.null(regs[[i]])) return(NA_character_)
    r <- regs[[i]] %>% filter(config_id == as.character(c))
    if (nrow(r) == 0) NA_character_ else thr_label(r$th__k[1])
  }, industry, cid, USE.NAMES = FALSE, SIMPLIFY = TRUE)
}

# MV status counts per industry x outcome
mv_status <- imap_dfr(mv_paths, function(p, ind) {
  if (!file.exists(p)) return(tibble())
  arrow::read_parquet(p) %>% count(outcome, status) %>%
    pivot_wider(names_from=status, values_from=n, values_fill=0) %>%
    mutate(industry=ind)
})
# Normalize status columns: add any entirely-missing column, AND coalesce
# per-cell NAs (introduced when an industry's pivot lacked a status that
# another industry had) to 0.
for (col in c("success","convergence_warning","failed")) {
  if (!col %in% names(mv_status)) mv_status[[col]] <- 0L
  mv_status[[col]] <- dplyr::coalesce(mv_status[[col]], 0L)
}
mv_status <- mv_status %>%
  mutate(total_specs = success + convergence_warning + failed,
         convergence_rate = round(100*success/total_specs,1)) %>%
  rename(n_success=success, n_convergence_warning=convergence_warning, n_failed=failed)


# =============================================================================
# ED TABLE 1 — Sample construction & panel characteristics
# =============================================================================
# Per-industry pre-filter info from sources (narratives row)
pre <- srcs %>% filter(source_type=="narratives") %>%
  transmute(industry, raw_records=n_records, n_orgs_prefilter=n_orgs,
            source_date_min=date_min, source_date_max=date_max)

ed1 <- psum %>%
  transmute(industry, outcome,
            cell_class = cell_class(industry, outcome),
            unit_of_analysis = unit_label(industry),
            panel_start=panel_min, panel_end=panel_max,
            n_orgs_postfilter=n_orgs, n_panel_cells=n_cells,
            n_nonzero_cells=n_nonzero_outcome, total_outcome,
            pct_zero_cells=pct_zero, exposure_offset=offset_var,
            outcome_var) %>%
  left_join(pre, by="industry") %>%
  mutate(industry=ind_label(industry)) %>%
  arrange(industry, desc(cell_class=="primary"), outcome) %>%
  select(industry, outcome, cell_class, unit_of_analysis,
         raw_records, n_orgs_prefilter, source_date_min, source_date_max,
         panel_start, panel_end, n_orgs_postfilter, n_panel_cells,
         n_nonzero_cells, total_outcome, pct_zero_cells,
         exposure_offset, outcome_var)
readr::write_csv(ed1, file.path(ED,"ED_table1_sample_construction.csv"))
cat(sprintf("ED T1: %d rows\n", nrow(ed1)))


# =============================================================================
# ED TABLE 2 — Detailed best-performing-model specs (PRIMARY cells, both CV)
# =============================================================================
ed2 <- champ %>%
  filter(paste(industry, outcome, sep="|") %in% PRIMARY) %>%
  rowwise() %>%
  mutate(thresholding = get_thr(industry, best_config_id)) %>%
  ungroup() %>%
  left_join(mv_status %>% select(industry, outcome, n_success, n_convergence_warning,
                                  n_failed, total_specs, convergence_rate),
            by=c("industry","outcome")) %>%
  transmute(industry=ind_label(industry), outcome, outcome_label,
            cv_strategy=cv_label(cv_strategy),
            best_config_id, embedding_model, sentiment_model=sent_method,
            composite_method=comp__method, thresholding,
            window_type, window_size, lag_days, halflife_days,
            climate_coef=round(climate_estimate,3),
            ci_low=round(climate_ci_low,3), ci_high=round(climate_ci_high,3),
            p_value=signif(climate_pval,3),
            best_delta_ll=round(best_delta_ll,4),
            pct_specs_positive_dll=pct_positive_dll, median_dll=round(median_dll,5),
            n_specs_converged=n_success, n_specs_excluded=n_convergence_warning+n_failed,
            n_specs_total=total_specs) %>%
  arrange(industry, outcome, cv_strategy)
readr::write_csv(ed2, file.path(ED,"ED_table2_bestmodel_specs_primary.csv"))
cat(sprintf("ED T2: %d rows\n", nrow(ed2)))


# =============================================================================
# ED TABLE 3 — Robustness: pre-decomposition coarse outcomes & finer tiers
# =============================================================================
# Reports the ROBUSTNESS cells (coarse mining accidents/injuries/fatalities that
# the detectability tiers refine; finer aviation casualty/AIDS tiers) so readers
# can confirm the main-text ladder is consistent with the un-decomposed view.
# These are robustness ANALYSES, not lesser primary outcomes.
ed3 <- champ %>%
  filter(paste(industry, outcome, sep="|") %in% ROBUSTNESS) %>%
  rowwise() %>% mutate(thresholding=get_thr(industry,best_config_id)) %>% ungroup() %>%
  left_join(psum %>% select(industry, outcome, n_nonzero_outcome), by=c("industry","outcome")) %>%
  left_join(mv_status %>% select(industry,outcome,n_success,total_specs), by=c("industry","outcome")) %>%
  transmute(industry=ind_label(industry), outcome, outcome_label,
            cv_strategy=cv_label(cv_strategy), best_config_id,
            climate_coef=round(climate_estimate,3),
            ci_low=round(climate_ci_low,3), ci_high=round(climate_ci_high,3),
            p_value=signif(climate_pval,3), best_delta_ll=round(best_delta_ll,4),
            pct_specs_positive_dll=pct_positive_dll,
            n_nonzero_cells=n_nonzero_outcome, n_specs_converged=n_success,
            robustness_note="Pre-decomposition coarse outcome (mining) or finer-grained casualty/AIDS tier (aviation). Reported as a robustness check on the main-text detectability/consequence ladder; not a separate primary outcome.") %>%
  arrange(industry, outcome, cv_strategy)
readr::write_csv(ed3, file.path(ED,"ED_table3_robustness_coarse_granular.csv"))
cat(sprintf("ED T3: %d rows\n", nrow(ed3)))


# =============================================================================
# ED TABLE 4 — Robustness: aviation time grain x ATC reporter scope
# =============================================================================
# The primary aviation analysis is airport-QUARTER at local+terminal ATC scope.
# This table shows the multiverse direction/breadth of the key aviation cells
# under the alternative grain (airport-month) and the narrower local-only scope,
# confirming the conclusions are not grain- or scope-specific. (Raw multiverse
# beta is on the windowed-climate scale; sign and %-positive are the comparable
# quantities across settings.)
av_settings <- c(
  "quarterly, local+terminal (primary)" = "results_new_new/aviation/panel_mv_results_quarterly.parquet",
  "monthly, local+terminal"             = "results_new_new/aviation/panel_mv_results_consolidated.parquet",
  "monthly, local-only ATC"             = "results_new_new/aviation/panel_mv_results_local_scope.parquet")
KEY_AV <- c("rate_aids_noharm","rate_aids_propdamage","rate_ntsb_serious_fatal")
ed4 <- imap_dfr(av_settings, function(p, setting) {
  if (!file.exists(p)) return(tibble())
  arrow::read_parquet(p) %>%
    filter(outcome %in% KEY_AV, status=="success", !is.na(climate_estimate)) %>%
    group_by(outcome) %>%
    summarise(setting=setting, n_specs=n(),
              median_beta=round(median(climate_estimate),3),
              pct_beta_positive=round(100*mean(climate_estimate>0)),
              pct_sig_positive=round(100*mean(climate_estimate>0 & climate_pval<0.05)),
              pct_sig_negative=round(100*mean(climate_estimate<0 & climate_pval<0.05)),
              .groups="drop")
}) %>%
  mutate(outcome_label = recode(outcome,
           rate_aids_noharm="No-harm events (AIDS)",
           rate_aids_propdamage="Property-damage events (AIDS)",
           rate_ntsb_serious_fatal="Serious/fatal accidents (NTSB)")) %>%
  transmute(outcome, outcome_label, setting, n_specs,
            median_beta, pct_beta_positive, pct_sig_positive, pct_sig_negative) %>%
  arrange(outcome_label, factor(setting, levels=names(av_settings)))
readr::write_csv(ed4, file.path(ED,"ED_table4_robustness_grain_scope.csv"))
cat(sprintf("ED T4: %d rows\n", nrow(ed4)))


# =============================================================================
# ED TABLE 5 — Convergence & exclusion diagnostics (all cells, MV)
# =============================================================================
ed5 <- mv_status %>%
  mutate(cell_class = cell_class(industry, outcome),
         industry=ind_label(industry)) %>%
  transmute(industry, outcome, cell_class,
            n_specs_total=total_specs, n_converged=n_success,
            n_convergence_warning, n_failed,
            n_included_in_robustness=n_success,
            convergence_rate_pct=convergence_rate) %>%
  arrange(industry, desc(cell_class=="primary"), outcome)
readr::write_csv(ed5, file.path(ED,"ED_table5_convergence_diagnostics.csv"))
cat(sprintf("ED T5: %d rows\n", nrow(ed5)))


# =============================================================================
# ED TABLE 6 — Candidate-champion pool composition (primary cells)
# =============================================================================
cand <- imap_dfr(cfg_dirs, function(dir, ind) {
  f <- file.path("report_figures_panel", ind, "champions_candidates.csv")
  if (!file.exists(f)) return(tibble())
  d <- readr::read_csv(f, show_col_types=FALSE) %>% mutate(industry=ind, config_id=as.character(config_id))
  # join thresholding from registry
  d %>% left_join(regs[[ind]] %>% select(config_id, th__k), by="config_id") %>%
    mutate(thresholding=thr_label(th__k))
})
mode1 <- function(x) { t <- sort(table(x), decreasing=TRUE); sprintf("%s (%d/%d)", names(t)[1], t[1], length(x)) }
ed6 <- cand %>%
  filter(paste(industry, outcome, sep="|") %in% PRIMARY) %>%
  group_by(industry, outcome, cv_strategy) %>%
  summarise(n_in_pool=n(),
            top_embedding=mode1(embedding_model),
            top_sentiment=mode1(sent_method),
            top_composite=mode1(comp__method),
            top_thresholding=mode1(thresholding),
            top_window_type=mode1(window_type),
            .groups="drop") %>%
  mutate(industry=ind_label(industry), cv_strategy=cv_label(cv_strategy)) %>%
  arrange(industry, outcome, cv_strategy)
readr::write_csv(ed6, file.path(ED,"ED_table6_candidate_pool_composition.csv"))
cat(sprintf("ED T6: %d rows\n", nrow(ed6)))


# =============================================================================
# ED TABLE 7 — Climate vs operational covariate coefficients (primary cells)
# =============================================================================
ed7 <- coefs %>%
  filter(paste(industry, outcome, sep="|") %in% PRIMARY,
         variable_type %in% c("climate","between","within")) %>%
  transmute(industry=ind_label(industry), outcome, outcome_label,
            term=variable_label, term_type=variable_type,
            std_coef=round(estimate_std,3),
            std_ci_low=round(ci_low_std,3), std_ci_high=round(ci_high_std,3),
            rate_ratio_per_sd=round(exp(estimate_std),3),
            rr_ci_low=round(exp(ci_low_std),3), rr_ci_high=round(exp(ci_high_std),3)) %>%
  arrange(industry, outcome, factor(term_type, levels=c("climate","within","between")))
readr::write_csv(ed7, file.path(ED,"ED_table7_climate_vs_operational_coefs.csv"))
cat(sprintf("ED T7: %d rows\n", nrow(ed7)))


# =============================================================================
# CROSSWALK — main-text claim -> Extended Data item
# =============================================================================
crosswalk <- tribble(
  ~main_text_claim, ~extended_data_item, ~artifact,
  "Full sample construction & panel composition", "ED Table 1", "ED_table1_sample_construction.csv",
  "Detailed best-performing-model specifications", "ED Table 2", "ED_table2_bestmodel_specs_primary.csv",
  "Detectability ladder consistent with pre-decomposition coarse outcomes", "ED Table 3 (robustness)", "ED_table3_robustness_coarse_granular.csv",
  "Aviation conclusions robust to time grain & ATC reporter scope", "ED Table 4 (robustness)", "ED_table4_robustness_grain_scope.csv",
  "High multiverse convergence rates", "ED Table 5", "ED_table5_convergence_diagnostics.csv",
  "Champion not an isolated multiverse corner (10% pool)", "ED Table 6", "ED_table6_candidate_pool_composition.csv",
  "Climate not absorbed by operational covariates", "ED Table 7", "ED_table7_climate_vs_operational_coefs.csv",
  "Multiverse support: consistency, coherence, CV sign stability", "ED Table (full multiverse-support) + ED Fig (per-industry CV spec curves)", "table2_multiverse_support_full.csv; ED_speccurve_cv_timeseries_{rail,nrc,aviation,msha}.pdf",
  "Organization-blocked CV robustness", "ED Figure (per-industry org-blocked spec curves)", "ED_speccurve_cv_groupkfold_{rail,nrc,aviation,msha}.pdf",
  "Climate effect concordant across CV strategies (per-SD)", "ED Figure (CV sign comparison)", "extended_data_cv_sign_comparison_{log,rateratio}.pdf; extended_data_cv_sign_comparison_table.csv",
  "Per-cell decision-importance decompositions", "ED Figure (variance partition, all cells)", "appendix_facet_importance_all_outcomes.pdf",
  "Traditional (coefficient) multiverse spec curves", "ED Figure (per-industry traditional spec curves)", "ED_speccurve_traditional_mv_{rail,nrc,aviation,msha}.pdf",
  "Robust to minimum-event inclusion threshold", "ED Figure (cutoff sensitivity)", "sensitivity_min_reports_forest_{log,rateratio}.pdf; sensitivity_min_reports_table.csv",
  "Robust to zero-score windowing convention", "ED Figure (zero-as-NA sensitivity)", "sensitivity_zero_as_na_forest_{log,rateratio}.pdf; sensitivity_zero_as_na_table.csv",
  "Not a sentiment/negativity detector", "ED Figure (sentiment-decomposition control)", "sensitivity_sentiment_control_forest_{log,rateratio}.pdf; sensitivity_sentiment_control_table.csv",
  "Not just outcome autocorrelation (lagged-event control)", "ED Figure (lagged-outcome control)", "sensitivity_lagged_outcome_forest_{log,rateratio}.pdf; sensitivity_lagged_outcome_table.csv",
  "No org-level reporting-rate confound", "ED Figure (report-rate scatter)", "sensitivity_report_rate_panels.pdf; sensitivity_report_rate_corr.csv",
  "Face validity of climate scoring", "Supp Table (content-validity samples)", "supplementary_content_validity_samples.csv/.md",
  "Zero-score mechanism (sentiment vs similarity)", "Supp Results / Methods", "zero_score_diagnostic_summary.csv; zero_score_diagnostic_examples.md",
  "Climate-score scale & identification", "Supp Results", "paper_repo_prep/METHODS_NOTE_climate_scale_and_identification.md",
  "Per-segment sentiment/similarity aggregation detail", "Supp Methods", "paper_repo_prep/METHODS_NOTE_sentiment_aggregation.md"
)
readr::write_csv(crosswalk, file.path(ED,"ED_crosswalk_maintext_to_extended_data.csv"))
cat(sprintf("Crosswalk: %d rows\n", nrow(crosswalk)))


# =============================================================================
# PLACEHOLDER CHECKLIST — unresolved values to confirm before submission
# =============================================================================
checklist <- c(
  "# Extended Data — Unresolved Placeholders Checklist","",
  "Confirm these exact values before archiving / submission:","",
  "## Counts that must reconcile across tables",
  "- [ ] n_orgs_postfilter in ED T1 matches the panel n_orgs used in fitting (note: earlier Table-1 vs sensitivity runs showed a 59-vs-65 NRC discrepancy depending on data-state; re-run Table 1 + sensitivity from the SAME final data state and confirm)",
  "- [ ] raw_records / n_orgs_prefilter in ED T1 are the intended pre-filter denominators (sources narratives row)",
  "- [ ] total_outcome in ED T1 matches sums used in the modeling panels",
  "",
  "## Sensitivity / alternative-explanation controls",
  "- [ ] lagged-outcome control (10_lagged_outcome_sensitivity.R): confirm climate sign + significance preserved in all 9 main cells; note the two cells with >25% attenuation (NRC LERs, aviation AIDS) remain significant",
  "- [ ] CV sign comparison (11_cv_sign_comparison.R): 7/9 cells sign-concordant across time-series vs organization-blocked CV. Decide how to present the two discordant cells: (a) NRC % power loss = two non-significant near-zero estimates (no real disagreement); (b) rail accidents = genuine CV-dependent sign flip (TS +, org-blocked -; both tiny per-SD). Rail accidents is the least CV-stable cell -- flag in text or footnote.",
  "- [ ] multiverse support (12_multiverse_support.R): tiers REMOVED. Table now reports consistency (%dll>0), coherence (%same-sign), and CV sign stability (credible-mass per-strategy modal sign + flip; best-spec sign match + significance). Confirm the CV-flip tallies (CV-stable / credible-mass / argmax-driven / both) match the 2x2 figure (13_multiverse_2x2.R).",
  "- [ ] spec curves restructured to 12 per-industry ED figures (ED_speccurve_{cv_timeseries,cv_groupkfold,traditional_mv}_{rail,nrc,aviation,msha}.pdf), restricted to the 15 main outcomes; main-text multiverse figures are figure4_multiverse_support.pdf and figure4_multiverse_2x2.pdf.",
  "",
  "## Specification details",
  "- [ ] thresholding label (all-segments / top-3 / top-5) correct for each champion config_id",
  "- [ ] n_specs_total = 810 for every cell (135 configs x 6 windows); flag any cell != 810",
  "- [ ] prob_above_col1 CV failure: decide whether to (a) fix + re-run CV, or (b) report MV-only as noted",
  "",
  "## Repository / data links (fill once minted)",
  "- [ ] Zenodo DOI for input data",
  "- [ ] Zenodo DOI for intermediate artifacts",
  "- [ ] Zenodo DOI + GitHub tag for analysis code",
  "- [ ] Zenodo DOI + GitHub tag for text-pipeline code",
  "",
  "## Figure/label consistency",
  "- [ ] Industry labels standardized (Rail / Nuclear / Aviation) across all ED items",
  "- [ ] CV labels standardized (Time-series / Organization-blocked)",
  "- [ ] Sign conventions consistent between main text and ED",
  "- [ ] 'best-performing model' terminology (not 'champion') in all captions"
)
writeLines(checklist, file.path(ED,"ED_placeholder_checklist.md"))
cat("Placeholder checklist written\n")


# =============================================================================
# ED FIGURES — copy the per-industry spec-curve PDFs into the ED package.
# 5_manuscript_plots.R now emits 12 figures (4 industries x {time-series CV,
# organization-blocked CV, traditional multiverse}), each restricted to the 15
# main-text outcomes and named ED_speccurve_<type>_<industry>.pdf. Copying makes
# the Extended Data folder self-contained / reproducible.
# =============================================================================
ed_speccurve_files <- list.files(RFM, pattern = "^ED_speccurve_.*\\.pdf$")
if (length(ed_speccurve_files) == 0) {
  cat(sprintf("(no ED_speccurve_*.pdf in %s — run 5_manuscript_plots.R first)\n", RFM))
}
for (f in ed_speccurve_files) {
  file.copy(file.path(RFM, f), file.path(ED, f), overwrite = TRUE)
  cat(sprintf("Copied ED figure: %s\n", f))
}
cat(sprintf("Copied %d per-industry spec-curve figures into the ED package\n",
            length(ed_speccurve_files)))

cat(sprintf("\n=== Extended Data written to %s/ ===\n", ED))
