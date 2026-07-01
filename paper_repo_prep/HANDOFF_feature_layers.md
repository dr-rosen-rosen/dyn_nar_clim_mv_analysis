# Handoff — adding lexical/syntactic feature layers to the model

Purpose: let a fresh session start building lexical/syntactic-feature models
without re-deriving the existing infrastructure. The paper itself is at a
natural stopping point; this is the pivot to "what's next."

## The two codebases (and which one does what)

- **Python toolkit** — `DynamicNarrativeClimateToolkit/dynclim`. Scores report
  text → per-report `overall_final_score`. Each climate config writes
  `notebooks/checkpoints/<industry>_<date>/_cfg/<config_id>/results.parquet`
  with columns including `report_id` and `overall_final_score`.
- **R analysis** — `data_anlaysis/dyn_nar_clim_mv_analysis` (this repo). Builds
  org-period panels, windows the per-report score, fits panel-rate GLMMs, runs
  the multiverse + CV, and makes all figures/tables.

Checkpoint dirs in use:
`rail_04-14-2026`, `nrc_04-14-2026`, `asrs_05-01-2026`.

## The mechanism you'll reuse: windowing an arbitrary per-report value

The single most important trick (proven in `9_sentiment_control_sensitivity.R`
and `10_lagged_outcome_sensitivity.R`):

`prepare_<industry>_panel_data(parquet_path, config_id, ...,
climate_base="overall_final_score")` windows the per-report
`overall_final_score` column into org-period columns named like
`overall_final_score_sma_20` or `overall_final_score_ewmaLAG_720d_hl360d`.

To window ANY other per-report quantity with the IDENTICAL grid / org-selection
/ windowing, **substitute it into the parquet and re-run panel prep**:

```r
# write_substituted_parquet() in scripts 9 & 10:
d  <- read_parquet(".../_cfg/<cid>/results.parquet") %>% mutate(report_id = as.character(report_id))
v  <- feature_df %>% transmute(report_id = as.character(eid), .newval = <your_feature>)
d2 <- d %>% select(-overall_final_score) %>% left_join(v, by="report_id") %>%
        rename(overall_final_score = .newval)
write_parquet(d2, tmp); build_panel(industry, tmp, cid)   # extract the windowed column by its name
```

So: compute a per-report feature → join on `report_id` (== event id) →
window it exactly like climate → use as a covariate. No new windowing code.

## Where to compute the features (template already exists)

`9a_compute_whole_narrative_vader.py` is the template: it reads each industry's
narrative column, computes per-report features, writes
`whole_narrative_vader_<industry>.parquet` keyed by `eid`. Narrative columns:
- rail: `narrative`  · nrc: `event_text`  · aviation: `narrative`
(events parquets under `dynclim/data/processed/<industry>/`).

For lexical/syntactic features, mirror that script (spaCy / textstat / nltk):
e.g. readability (Flesch/FK), type-token ratio, mean dependency depth,
clause/sentence length, passive-voice rate, POS proportions, hedging/modal
counts, named-entity density. Emit one parquet per industry keyed by report id.

## How to enter a feature into the model

Two routes, pick per question:

1. **Competing-covariate test** (does climate survive controlling for syntax?):
   exactly the sentiment-control pattern. Window the feature (substitution
   trick), z-score it, add via `update(base_fml, . ~ . + feature_w)`, refit each
   main cell, compare the climate coefficient base vs +feature. Reuse
   `fit_augmented()` + the registry/builders verbatim (see below).

2. **Feature-as-predictor / new layer** (do syntax features predict outcomes on
   their own, or improve a composite?): window each feature, fit it as the focal
   predictor (swap `climate_var` for the windowed feature name), optionally feed
   the multiverse. Bigger lift if you want full 810-spec multiverse parity —
   that would mean emitting feature "configs" from the toolkit side.

### Registry + formula builders (the model skeleton)

- Registries: `PANEL_OUTCOME_VARS` (rail/`panel_fit_models.R`),
  `PANEL_OUTCOME_VARS_NRC`, `PANEL_OUTCOME_VARS_AVIATION`. Each cell:
  `var` (count), `offset` (exposure), `family` (nbinom2 / tweedie / binomial).
- Base formula: `build_panel_formula(outcome, climate_var)` /
  `build_nrc_panel_formula` / `build_aviation_panel_formula`. Structure =
  focal predictor + `yearmonth_num_c` + `sin_month` + `cos_month` +
  between/within ops terms + `offset(log(exposure))` + `(1 | org)`.
- Augment: `update(base_fml, as.formula(". ~ . + term1 + term2"))`.
- `fit_augmented()` (scripts 9/10) already wraps: build formula → complete-case
  filter → `offset>0` filter → `glmmTMB` → tidy → return climate coef + CI.

### Main cells (9) and exclusions

PRIMARY: rail {accidents, injuries, fatalities}; nrc {lers, emerg, scrams,
pct_power_loss}; aviation {accidents, aids_all}.
`EXCLUDE` (rare/external, skip): aviation {fatalities, inj_serious_fatal,
aids_incidents_only}; nrc {findings_all, findings_nongreen, prob_above_col1}.
Org/period vars: rail `org_id`/`yearmonth`, nrc `facility_site`/`quarter_start`,
aviation `airport_id`/`yearmonth`.

## Data-loading boilerplate

Don't rewrite it — copy the loading block + `panel_fns` + `spec_map` from
`5_manuscript_plots.R` (lines ~228–382) or the trimmed version in
`10_lagged_outcome_sensitivity.R`. `11_cv_sign_comparison.R` shows the full
refit-both-CV-strategies pattern.

## Multiverse / CV result stores (if you go to route 2)

- `results_new_new/<industry>/panel_cv_results.parquet` — held-out `loglik_mean`
  per spec; `model_label=="delta_climate_vs_seasonal"` is climate-vs-baseline
  gain. 810 specs/cell × 2 cv_strategy (timeseries, group_kfold).
- `results_new_new/<industry>/panel_mv_results.parquet` — full-data fit per spec
  WITH `climate_estimate` (coefficient sign). `12_multiverse_support.R` joins
  these two to compute % same-sign-among-gainers; reuse that join.

## Gotchas (learned the hard way)

- This repo runs from `/Users/.../data_anlaysis/dyn_nar_clim_mv_analysis`; the
  shell cwd resets to the worktree after each command — use absolute paths.
- Shell is zsh: unquoted `$VAR` does NOT word-split in `for` loops.
- PDFs: keep figure text ASCII (no Greek Δ, no em-dash "—") or glyph warnings /
  blank boxes appear. Use "dLL", " - ".
- Join key is the report/event id as **character** on both sides.
- Measurement-error framing: small-variance noisy predictors attenuate toward
  null — same logic will apply to noisy syntactic features.

## Current paper artifacts (stable; don't disturb)

Scripts 5–12 in repo root; outputs in `report_figures_manuscript/` (+ its
`extended_data/`). Methods notes + Supplementary Results + this handoff in
`paper_repo_prep/`. Multiverse-support tiers (Tier1: rail injuries, NRC LERs;
Tier3: rail/aviation accidents, NRC %power) in `table2_multiverse_support_*`.
