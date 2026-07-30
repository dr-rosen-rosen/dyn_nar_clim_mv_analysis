############ Multiverse support, 2x2 representation (consistency x coherence)
#
# Redo of the multiverse-support visual. Two orthogonal robustness axes, drawn
# as a continuum rather than collapsed to tiers:
#   * CONSISTENCY (x) = % of specs with held-out gain (dLL > 0), timeseries CV
#       <- panel_cv_results.parquet (delta_climate_vs_seasonal, timeseries)
#   * COHERENCE   (y) = % of positive-gain specs whose full-data climate sign
#       matches the best-performing spec   <- CV (dLL>0) JOIN MV (sign)
#
# CV-strategy sign stability is a SEPARATE signal from coherence (it varies the
# resampling scheme, not the specification), so it rides as a glyph, not a third
# axis. We report TWO estimators and never auto-demote on either:
#   (a) best-spec-per-strategy  -- argmax of held-out dLL refit under each
#       strategy, sign + significance  <- extended_data_cv_sign_comparison_table.csv
#   (b) credible-mass-per-strategy -- modal full-data sign over each strategy's
#       credible (dLL>0) specs; flip = modal signs disagree across strategies
#       <- panel_cv_results.parquet (per strategy) JOIN MV sign
# When (a) and (b) disagree, the flip is argmax-driven (the best spec sits where
# the credible mass does not); we mark that rather than demote the cell.
#
# NOTE on sign normalization: NOT applied. Every main-text cell is a single-
# family panel rate model (NB / Tweedie) with one oriented climate_estimate
# (+climate -> more events per exposure), so signs are already comparable within
# a cell. A zi-orientation guard would only be needed if a cell sourced the
# event-level hurdle track's zi component, which the main-text figure does not.
#
# Outputs:
#   report_figures_manuscript/figure4_multiverse_2x2.pdf
#   report_figures_manuscript/table2_multiverse_2x2_full.csv

suppressPackageStartupMessages({library(tidyverse); library(arrow)})

INDS <- c("rail","nrc","aviation","msha")
IND_LABEL <- c(rail="Rail", nrc="Nuclear", aviation="Aviation", msha="Mining")
IND_COL <- c(Rail="#2c7bb6", Nuclear="#1a9850", Aviation="#d6604d", Mining="#8856a7")
RFM <- "report_figures_manuscript"

# Standard 2x2 split (no data-tuned cutoffs). Consistency splits at the 50%
# midpoint of [0,100]. Coherence is the MODAL-SIGN consensus share of the
# credible set (2026-07 redefinition: the best-performing spec is selected by
# knife-edge margins, <=6e-4 dLL, and can contradict a stable consensus — see
# scrams), which is bounded to [50,100] by construction because the mode always
# holds the plurality; its principled midpoint is therefore 75. Sensitivity at
# the 2:1-odds cut (66.7%) is logged below; cells at 68-74% sit near the line.
CONSIST_CUT <- 50
COHER_CUT   <- 75
# Margin from 0.5 below which a credible-mass modal sign is treated as too
# balanced to call confidently (drives the "tentative" footnote, not the glyph).
BALANCE_MARGIN <- 0.10

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
# 1. LOAD + CONSISTENCY / COHERENCE  (matches 12_multiverse_support.R defns)
# =============================================================================

cv <- map_dfr(INDS, ~read_parquet(.cv_path(.x)) %>% mutate(industry = .x))
mv <- map_dfr(INDS, ~read_parquet(.mv_path(.x)) %>%
                mutate(industry = .x, config_id = as.character(config_id)))
if (!"panel_outcome" %in% names(cv)) cv$panel_outcome <- cv$outcome

mv_sign <- mv %>% transmute(industry, config_id, climate_var, outcome,
                            mv_sign = sign(climate_estimate))

delta <- cv %>%
  filter(model_label == "delta_climate_vs_seasonal", !is.na(loglik_mean)) %>%
  mutate(config_id = as.character(config_id)) %>%
  left_join(mv_sign, by = c("industry","config_id","climate_var",
                            "panel_outcome" = "outcome"))

# best ts spec sign per cell (for coherence) ---------------------------------
best_sign <- delta %>% filter(cv_strategy == "timeseries") %>%
  group_by(industry, panel_outcome) %>%
  slice_max(loglik_mean, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(industry, panel_outcome, best_sign = mv_sign)

axes <- delta %>% filter(cv_strategy == "timeseries") %>%
  left_join(best_sign, by = c("industry","panel_outcome")) %>%
  group_by(industry, outcome = panel_outcome) %>%
  summarise(n_spec        = n(),
            consistency   = round(100 * mean(loglik_mean > 0), 0),
            # Coherence = consensus share of the credible set's modal sign
            # (bounded [50,100]); the best spec no longer anchors the axis.
            coherence     = { s <- mv_sign[loglik_mean > 0]; s <- s[!is.na(s)]
                              if (!length(s)) NA_real_ else
                              round(100 * max(table(s)) / length(s), 0) },
            modal_sign_ts = { s <- mv_sign[loglik_mean > 0]; s <- s[!is.na(s)]
                              if (!length(s)) NA_real_ else
                              as.numeric(names(sort(table(s), decreasing = TRUE))[1]) },
            best_sign_val = dplyr::first(best_sign),
            .groups = "drop") %>%
  mutate(best_ne_modal = !is.na(best_sign_val) & !is.na(modal_sign_ts) &
                         best_sign_val != modal_sign_ts)

# =============================================================================
# 2. FLIP ESTIMATOR (b): credible-mass modal sign per CV strategy
# =============================================================================

modal_sign <- function(s) { s <- s[!is.na(s)]; if (!length(s)) return(NA_real_)
  as.numeric(names(sort(table(s), decreasing = TRUE))[1]) }

flip_b <- delta %>% filter(loglik_mean > 0) %>%
  group_by(industry, outcome = panel_outcome, cv_strategy) %>%
  summarise(n_cred = n(), modal = modal_sign(mv_sign),
            frac_pos = mean(mv_sign > 0, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cv_strategy,
              values_from = c(n_cred, modal, frac_pos)) %>%
  mutate(
    flip_b = !is.na(modal_timeseries) & !is.na(modal_group_kfold) &
             modal_timeseries != modal_group_kfold,
    # both per-strategy modal signs sit far enough from a 50/50 split to trust
    flip_b_confident = flip_b &
      pmin(abs(frac_pos_timeseries - 0.5), abs(frac_pos_group_kfold - 0.5)) >= BALANCE_MARGIN,
    thin_credible = pmin(n_cred_timeseries, n_cred_group_kfold) < 100)

# =============================================================================
# 3. FLIP ESTIMATOR (a): best-spec-per-strategy (from 11_), sign + significance
# =============================================================================

a_path <- file.path(RFM, "extended_data_cv_sign_comparison_table.csv")
if (file.exists(a_path)) {
  flip_a <- readr::read_csv(a_path, show_col_types = FALSE) %>%
    filter(variable_type == "climate") %>%
    transmute(industry, outcome, cv_strategy,
              s = sign(estimate_std), sig = (ci_low_std > 0 | ci_high_std < 0)) %>%
    pivot_wider(names_from = cv_strategy, values_from = c(s, sig)) %>%
    transmute(industry, outcome,
              flip_a     = !is.na(s_timeseries) & !is.na(s_group_kfold) &
                           s_timeseries != s_group_kfold,
              flip_a_sig = flip_a & sig_timeseries & sig_group_kfold)
} else {
  message("NOTE: ", a_path, " absent (run 11_); estimator (a) shown as NA.")
  flip_a <- tibble(industry = character(), outcome = character(),
                   flip_a = logical(), flip_a_sig = logical())
}

# =============================================================================
# 4. ASSEMBLE
# =============================================================================

tab <- OUTCOME_META %>%
  left_join(axes,   by = c("industry","outcome")) %>%
  left_join(flip_b, by = c("industry","outcome")) %>%
  left_join(flip_a, by = c("industry","outcome")) %>%
  mutate(across(c(flip_a, flip_a_sig, flip_b, flip_b_confident, thin_credible),
                ~ coalesce(.x, FALSE)),
         industry_label = factor(IND_LABEL[industry], levels = IND_LABEL),
         # descriptive quadrant (NOT a tier; no demotion)
         quadrant = case_when(
           consistency >= CONSIST_CUT & coherence >= COHER_CUT ~ "Robust & coherent",
           consistency >= CONSIST_CUT & coherence <  COHER_CUT ~ "Pervasive but sign-incoherent",
           consistency <  CONSIST_CUT & coherence >= COHER_CUT ~ "Coherent but narrow",
           TRUE                                                ~ "Weak / ambiguous"),
         # 4-level glyph reporting BOTH estimators
         flip_glyph = case_when(
            flip_a_sig &  flip_b ~ "Both estimators flip",
            flip_a_sig & !flip_b ~ "Argmax-driven flip (best-spec only)",
           !flip_a_sig &  flip_b ~ "Credible-mass flip only",
           TRUE                  ~ "CV-stable"),
         flip_glyph = factor(flip_glyph, levels = c(
           "CV-stable","Credible-mass flip only",
           "Argmax-driven flip (best-spec only)","Both estimators flip")),
         cell = sprintf("%s: %s", industry_label, outcome_label),
         # asterisk = best-performing spec's sign contradicts the modal sign
         plot_label = ifelse(best_ne_modal, paste0(outcome_label, "*"),
                             outcome_label))

readr::write_csv(
  tab %>% transmute(Industry = industry_label, Outcome = outcome_label,
    `Outcome class` = outcome_class, Quadrant = quadrant,
    Consistency = consistency, Coherence = coherence,
    `Best vs modal disagree` = best_ne_modal, `N spec` = n_spec,
    `Flip (best-spec a)` = flip_a, `Flip (best-spec, sig)` = flip_a_sig,
    `Flip (credible-mass b)` = flip_b, `Flip (b, confident)` = flip_b_confident,
    `frac_pos ts` = round(frac_pos_timeseries, 2),
    `frac_pos gk` = round(frac_pos_group_kfold, 2),
    `n credible ts` = n_cred_timeseries, `n credible gk` = n_cred_group_kfold,
    `Thin credible set` = thin_credible, Glyph = as.character(flip_glyph)),
  file.path(RFM, "table2_multiverse_2x2_full.csv"))

# =============================================================================
# 5. FIGURE — consistency x coherence, glyph = CV-flip status
# =============================================================================

# shapes: 16 filled = stable; 1 open ring = credible-mass flip; 13 circle-X =
# argmax-only; 8 star = both. Fillable not needed (colour carries industry).
glyph_shape <- c("CV-stable" = 16, "Credible-mass flip only" = 1,
                 "Argmax-driven flip (best-spec only)" = 13, "Both estimators flip" = 8)

quad_lab <- tibble(
  x = c(98, 98, 2, 2), y = c(99.5, 50.5, 99.5, 50.5),
  hjust = c(1, 1, 0, 0), vjust = c(1, 0, 1, 0),
  label = c("Robust & coherent","Pervasive but\nsign-incoherent",
            "Coherent but narrow","Weak / ambiguous"))

p <- ggplot(tab, aes(consistency, coherence)) +
  geom_vline(xintercept = CONSIST_CUT, linetype = "dashed", colour = "gray70") +
  geom_hline(yintercept = COHER_CUT,   linetype = "dashed", colour = "gray70") +
  geom_text(data = quad_lab, aes(x, y, label = label, hjust = hjust, vjust = vjust),
            inherit.aes = FALSE, colour = "gray60", size = 3, fontface = "italic",
            lineheight = 0.9) +
  geom_point(aes(colour = industry_label, shape = flip_glyph),
             size = 3.2, stroke = 1.1)

if (requireNamespace("ggrepel", quietly = TRUE)) {
  p <- p + ggrepel::geom_text_repel(aes(label = plot_label, colour = industry_label),
             size = 2.7, max.overlaps = 20, seed = 1, show.legend = FALSE,
             min.segment.length = 0, segment.colour = "gray75")
} else {
  p <- p + geom_text(aes(label = plot_label, colour = industry_label),
             size = 2.6, vjust = -0.9, show.legend = FALSE)
}

p <- p +
  scale_colour_manual(values = IND_COL, name = "Industry") +
  scale_shape_manual(values = glyph_shape, name = "CV-strategy sign stability",
                     drop = FALSE) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "Consistency  ->  % specifications with held-out gain (dLL > 0)",
       y = "Coherence  ->  % modal-sign consensus (among dLL > 0)",
       title = "Multiverse support for narrative-derived safety climate",
       subtitle = paste0(
         "Consistency x coherence. Consistency splits at its 50% midpoint; coherence ",
         "(bounded 50-100% by construction) at its 75% midpoint.",
         "\nGlyph = CV-strategy sign stability (time-series vs organization-blocked). ",
         "* = best-performing specification's sign contradicts the modal sign.")) +
  guides(colour = guide_legend(order = 1, override.aes = list(shape = 16, size = 3)),
         shape  = guide_legend(order = 2, override.aes = list(colour = "gray20"))) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.subtitle = element_text(color = "gray40", size = 8),
        legend.position = "right", legend.box = "vertical")

ggsave(file.path(RFM, "figure4_multiverse_2x2.pdf"), p, width = 10, height = 6)

# =============================================================================
# 6. CONSOLE SUMMARY + coherence-cut sensitivity
# =============================================================================

cat("=== Multiverse 2x2 summary ===\n")
print(as.data.frame(tab %>% arrange(desc(consistency)) %>%
  transmute(cell, consistency, coherence, quadrant,
            flip = as.character(flip_glyph),
            fpos_ts = round(frac_pos_timeseries, 2),
            fpos_gk = round(frac_pos_group_kfold, 2),
            thin = thin_credible)), row.names = FALSE)

cat("\n--- Cells near the axis cuts (quadrant-fragile under standard split) ---\n")
near_coh <- tab %>% filter(abs(coherence - COHER_CUT)     <= 10) %>% transmute(cell, coherence)   %>% arrange(coherence)
near_con <- tab %>% filter(abs(consistency - CONSIST_CUT) <= 10) %>% transmute(cell, consistency) %>% arrange(consistency)
cat(sprintf("coherence within %d+/-10:   ", COHER_CUT),
    paste(sprintf("%s(%d)", near_coh$cell, near_coh$coherence), collapse = ", "), "\n")
cat(sprintf("consistency within %d+/-10: ", CONSIST_CUT),
    paste(sprintf("%s(%d)", near_con$cell, near_con$consistency), collapse = ", "), "\n")

cat("\n--- Coherence-cut sensitivity: 75 (range midpoint) vs 66.7 (2:1 odds) ---\n")
moved <- tab %>%
  mutate(q75  = coherence >= 75, q667 = coherence >= 200/3) %>%
  filter(q75 != q667) %>% transmute(cell, coherence)
if (nrow(moved)) {
  cat("cells whose coherence classification changes between cuts:\n")
  print(as.data.frame(moved), row.names = FALSE)
} else cat("no cell changes classification between the two cuts\n")

cat("\n--- Estimator (a) vs (b) disagreement (argmax-driven flips) ---\n")
print(as.data.frame(tab %>% filter(flip_a_sig != flip_b) %>%
  transmute(cell, flip_a_sig, flip_b, flip_b_confident, thin_credible)), row.names = FALSE)

cat("\nOutput:\n")
cat("  ", file.path(RFM, "figure4_multiverse_2x2.pdf"), "\n")
cat("  ", file.path(RFM, "table2_multiverse_2x2_full.csv"), "\n")
