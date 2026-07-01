############ Multiverse-support summary table + continuum figure (no tiering)
#
# For each primary industry x outcome cell we report two robustness axes plus
# cross-validation sign stability. The old 1/2/3 "support tiers" are removed.
#   * Consistency: % of specs with held-out Delta-loglik > 0 (timeseries CV)
#       <- panel_cv_results.parquet (delta_climate_vs_seasonal, timeseries)
#   * Coherence:   % of positive-gain specs whose full-data climate coefficient
#       shares the SIGN of the best-performing spec
#       <- CV (dLL>0) JOIN MV (sign)
#   * CV sign stability, TWO estimators (see 13_multiverse_2x2.R):
#       - credible-mass: per-CV-strategy modal full-data sign over credible
#         (dLL>0) specs; flip = modal signs differ across strategies
#         <- panel_cv_results (both strategies) JOIN MV sign
#       - best-spec: do the best specs selected under timeseries vs
#         organization-blocked CV agree in sign (and is the flip significant)?
#         <- extended_data_cv_sign_comparison_table.csv
#
# Outputs:
#   report_figures_manuscript/table2_multiverse_support_full.csv   (Extended Data)
#   report_figures_manuscript/table2_multiverse_support_main.csv   (main text, compact)
#   report_figures_manuscript/figure4_multiverse_support.pdf       (continuum, A+B)

suppressPackageStartupMessages({library(tidyverse); library(arrow); library(scales)})

INDS <- c("rail","nrc","aviation","msha")
IND_LABEL <- c(rail="Rail", nrc="Nuclear", aviation="Aviation", msha="Mining")
RFM <- "report_figures_manuscript"
EXCLUDE <- character(0)

# MSHA -> detectability-tier results; aviation -> quarterly grain. Rail/NRC bare.
.cv_path <- function(ind) switch(ind,
  msha     = "results_new_new/msha/panel_cv_results_coal_detectability_full.parquet",
  aviation = "results_new_new/aviation/panel_cv_results_quarterly.parquet",
  sprintf("results_new_new/%s/panel_cv_results.parquet", ind))
.mv_path <- function(ind) switch(ind,
  msha     = "results_new_new/msha/panel_mv_results_coal_detectability_full.parquet",
  aviation = "results_new_new/aviation/panel_mv_results_quarterly.parquet",
  sprintf("results_new_new/%s/panel_mv_results.parquet", ind))

OUTCOME_META <- tribble(
  ~industry, ~outcome,                  ~outcome_label,                ~outcome_class,
  "rail",    "rate_injuries",           "Injuries",                    "Harm",
  "rail",    "rate_fatalities",         "Fatalities",                  "Harm",
  "rail",    "rate_accidents",          "Accidents",                   "Reporting/classification-sensitive",
  "nrc",     "rate_lers",               "Licensee event reports",      "Reporting/classification-sensitive",
  "nrc",     "rate_emerg",              "Emergency declarations",      "Operational disruption",
  "nrc",     "rate_scrams",             "Scrams",                      "Operational disruption",
  "nrc",     "rate_pct_power_loss",     "Percent power loss",          "Operational disruption",
  "aviation","rate_aids_noharm",        "No-harm events (AIDS)",         "Reporting/surfacing",
  "aviation","rate_aids_propdamage",    "Property-damage events (AIDS)", "Harm",
  "aviation","rate_ntsb_serious_fatal", "Serious/fatal accidents (NTSB)","Harm",
  "msha",    "rate_t0",                 "No-injury reports",            "Reporting/surfacing",
  "msha",    "rate_t3",                 "Minor injury",                 "Harm",
  "msha",    "rate_t2",                 "Lost-time injury",             "Harm",
  "msha",    "rate_t1",                 "Severe injury (fatal/disability)","Harm",
  "msha",    "rate_days_lost",          "Lost workdays",                "Harm"
)

# =============================================================================
# 1. LOAD + CONSISTENCY (breadth) + COHERENCE
# =============================================================================

get_cv <- function(ind) read_parquet(.cv_path(ind)) %>% mutate(industry = ind)
get_mv <- function(ind) read_parquet(.mv_path(ind)) %>%
  mutate(industry = ind, config_id = as.character(config_id))

cv <- map_dfr(INDS, get_cv)
mv <- map_dfr(INDS, get_mv)
if (!"panel_outcome" %in% names(cv)) cv$panel_outcome <- cv$outcome

mv_sign <- mv %>% transmute(industry, config_id, climate_var, outcome,
                            mv_sign = sign(climate_estimate))

# timeseries delta (breadth + coherence); both-strategy delta (credible-mass flip)
delta <- cv %>%
  filter(model_label == "delta_climate_vs_seasonal", cv_strategy == "timeseries",
         !is.na(loglik_mean)) %>%
  mutate(config_id = as.character(config_id))
delta_both <- cv %>%
  filter(model_label == "delta_climate_vs_seasonal", !is.na(loglik_mean)) %>%
  mutate(config_id = as.character(config_id)) %>%
  left_join(mv_sign, by = c("industry","config_id","climate_var","panel_outcome"="outcome"))

best_sign <- delta %>%
  group_by(industry, panel_outcome) %>%
  slice_max(loglik_mean, n = 1, with_ties = FALSE) %>% ungroup() %>%
  left_join(mv_sign, by = c("industry","config_id","climate_var","panel_outcome"="outcome")) %>%
  transmute(industry, panel_outcome, best_sign = mv_sign)

breadth <- delta %>%
  left_join(mv_sign, by = c("industry","config_id","climate_var","panel_outcome"="outcome")) %>%
  left_join(best_sign, by = c("industry","panel_outcome")) %>%
  group_by(industry, outcome = panel_outcome) %>%
  summarise(n_spec        = n(),
            pct_dll_pos   = round(100 * mean(loglik_mean > 0), 0),
            median_dll    = round(median(loglik_mean), 3),
            best_dll      = round(max(loglik_mean), 3),
            pct_same_sign = round(100 * mean(mv_sign[loglik_mean > 0] ==
                                             best_sign[loglik_mean > 0], na.rm = TRUE), 0),
            .groups = "drop")

# =============================================================================
# 2. CV SIGN STABILITY — credible-mass (modal sign) + best-spec (sign match)
# =============================================================================

modal_sign <- function(s) { s <- s[!is.na(s)]; if (!length(s)) return(NA_real_)
  as.numeric(names(sort(table(s), decreasing = TRUE))[1]) }

cvflip <- delta_both %>% filter(loglik_mean > 0) %>%
  group_by(industry, outcome = panel_outcome, cv_strategy) %>%
  summarise(n_cred = n(), modal = modal_sign(mv_sign),
            frac_pos = mean(mv_sign > 0, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cv_strategy, values_from = c(n_cred, modal, frac_pos)) %>%
  mutate(credmass_flip = !is.na(modal_timeseries) & !is.na(modal_group_kfold) &
                         modal_timeseries != modal_group_kfold)

a_path <- file.path(RFM, "extended_data_cv_sign_comparison_table.csv")
cvsign <- readr::read_csv(a_path, show_col_types = FALSE)
if ("variable_type" %in% names(cvsign)) cvsign <- cvsign %>% filter(variable_type == "climate")

# org-blocked (group_kfold) best-spec direction
gk <- cvsign %>% filter(cv_strategy == "group_kfold") %>%
  transmute(industry, outcome,
            orgblocked_dir = dplyr::case_when(
              ci_low_std > 0 ~ "positive", ci_high_std < 0 ~ "protective",
              TRUE ~ "null/unstable"))

# best-spec sign concordance across timeseries vs group_kfold
flip_a <- cvsign %>%
  transmute(industry, outcome, cv_strategy,
            s = sign(estimate_std), sig = (ci_low_std > 0 | ci_high_std < 0)) %>%
  pivot_wider(names_from = cv_strategy, values_from = c(s, sig)) %>%
  mutate(.flip = !is.na(s_timeseries) & !is.na(s_group_kfold) &
                 s_timeseries != s_group_kfold) %>%
  transmute(industry, outcome,
            bestspec_match    = !.flip,
            bestspec_flip_sig = .flip & sig_timeseries & sig_group_kfold)

# =============================================================================
# 3. BEST PER-SD RATE RATIO (timeseries) + ASSEMBLE
# =============================================================================

rr <- readr::read_csv(file.path(RFM, "figure1_coefs_standardized.csv"), show_col_types = FALSE) %>%
  filter(variable_type == "climate") %>%
  transmute(industry, outcome,
            best_rr = exp(estimate_std), best_rr_lo = exp(ci_low_std),
            best_rr_hi = exp(ci_high_std))

tab <- OUTCOME_META %>%
  left_join(breadth, by = c("industry","outcome")) %>%
  left_join(rr,      by = c("industry","outcome")) %>%
  left_join(gk,      by = c("industry","outcome")) %>%
  left_join(cvflip,  by = c("industry","outcome")) %>%
  left_join(flip_a,  by = c("industry","outcome")) %>%
  mutate(key = paste(industry, outcome, sep = "|")) %>%
  filter(!key %in% EXCLUDE) %>%
  mutate(orgblocked_dir    = dplyr::coalesce(orgblocked_dir, "null/unstable"),
         credmass_flip     = dplyr::coalesce(credmass_flip, FALSE),
         bestspec_flip_sig = dplyr::coalesce(bestspec_flip_sig, FALSE),
         industry_label    = IND_LABEL[industry],
         best_rr_ci        = sprintf("[%.2f, %.2f]", best_rr_lo, best_rr_hi),
         cv_flip_status = factor(dplyr::case_when(
             bestspec_flip_sig &  credmass_flip ~ "Both flip",
             bestspec_flip_sig & !credmass_flip ~ "Argmax-driven flip",
            !bestspec_flip_sig &  credmass_flip ~ "Credible-mass flip",
             TRUE                               ~ "CV-stable"),
           levels = c("CV-stable","Credible-mass flip","Argmax-driven flip","Both flip"))) %>%
  arrange(industry, outcome)

# =============================================================================
# 4. WRITE TABLES (full -> ED; compact -> main)
# =============================================================================

sgn <- function(x) dplyr::case_when(x > 0 ~ "+", x < 0 ~ "-", TRUE ~ NA_character_)

full_tbl <- tab %>%
  transmute(Industry = industry_label, Outcome = outcome_label,
            `Outcome class` = outcome_class,
            `Best RR (per-SD)` = round(best_rr, 2), `95% CI` = best_rr_ci,
            `Best dll` = best_dll, `Median dll` = median_dll,
            `% dll>0` = pct_dll_pos, `% same-sign (dll>0)` = pct_same_sign,
            `Modal sign (TS)` = sgn(modal_timeseries),
            `Modal sign (GK)` = sgn(modal_group_kfold),
            `frac+ (TS)` = round(frac_pos_timeseries, 2),
            `frac+ (GK)` = round(frac_pos_group_kfold, 2),
            `N credible (TS)` = n_cred_timeseries,
            `N credible (GK)` = n_cred_group_kfold,
            `Credible-mass flip` = credmass_flip,
            `Best-spec sign match` = bestspec_match,
            `Best-spec flip (sig)` = bestspec_flip_sig,
            `Org-blocked direction` = orgblocked_dir)
readr::write_csv(full_tbl, file.path(RFM, "table2_multiverse_support_full.csv"))

main_tbl <- tab %>%
  transmute(Industry = industry_label, Outcome = outcome_label,
            `Outcome class` = outcome_class,
            `Best RR (per-SD)` = round(best_rr, 2),
            `% dll>0` = pct_dll_pos, `% same-sign (dll>0)` = pct_same_sign,
            `Modal sign TS/GK` = paste0(sgn(modal_timeseries), "/", sgn(modal_group_kfold)),
            `Credible-mass flip` = credmass_flip,
            `Best-spec sign match` = bestspec_match)
readr::write_csv(main_tbl, file.path(RFM, "table2_multiverse_support_main.csv"))

cat("=== Table 2 (full) ===\n"); print(as.data.frame(full_tbl))
cat(sprintf("\nCV-flip status: %s\n",
            paste(sprintf("%s=%d", names(table(tab$cv_flip_status)), table(tab$cv_flip_status)),
                  collapse = ", ")))

# =============================================================================
# 5. CONTINUUM FIGURE 4 — Panel A (% dll>0) + Panel B (% same-sign)
#    Colour = CV-strategy sign stability (no tiering).
# =============================================================================

ord <- tab %>% arrange(pct_same_sign, pct_dll_pos) %>%
  mutate(cell = sprintf("%s - %s", industry_label, outcome_label)) %>% pull(cell)

plt <- tab %>%
  mutate(cell = factor(sprintf("%s - %s", industry_label, outcome_label), levels = ord)) %>%
  select(cell, cv_flip_status, pct_dll_pos, pct_same_sign) %>%
  pivot_longer(c(pct_dll_pos, pct_same_sign), names_to = "panel", values_to = "value") %>%
  mutate(panel = factor(panel, levels = c("pct_dll_pos","pct_same_sign"),
                        labels = c("A. % specifications with held-out gain (dLL > 0)",
                                   "B. % same sign as best model (among dLL > 0)")))

ref_df <- tibble(panel = factor(c("A. % specifications with held-out gain (dLL > 0)",
                                  "B. % same sign as best model (among dLL > 0)"),
                                levels = levels(plt$panel)),
                 xint = c(50, 80))

flip_cols <- c("CV-stable" = "#4575b4", "Credible-mass flip" = "#fdae61",
               "Argmax-driven flip" = "#f46d43", "Both flip" = "#d73027")

p <- ggplot(plt, aes(value, cell, color = cv_flip_status)) +
  geom_vline(data = ref_df, aes(xintercept = xint), linetype = "dashed", color = "gray55") +
  geom_segment(aes(x = 0, xend = value, yend = cell), linewidth = 0.4, color = "gray80") +
  geom_point(size = 2.6) +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = flip_cols, name = "CV-strategy sign stability", drop = FALSE) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = NULL,
       title = "Multiverse support for narrative-derived safety climate across outcomes",
       subtitle = "Panel A: how broadly climate improves held-out prediction (dLL = held-out delta log-likelihood vs baseline).\nPanel B: directional coherence of predictive specifications. Colour = cross-validation sign stability (time-series vs\norganization-blocked). Dashed lines mark 50% (A) and 80% (B) heuristics; cells ordered by coherence.") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.subtitle = element_text(color = "gray40"),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", hjust = 0))

ggsave(file.path(RFM, "figure4_multiverse_support.pdf"), p, width = 10, height = 4.5)

cat("\nOutput:\n")
cat("  table2_multiverse_support_full.csv  (Extended Data)\n")
cat("  table2_multiverse_support_main.csv  (main text, compact)\n")
cat("  figure4_multiverse_support.pdf      (continuum, Panels A+B)\n")
