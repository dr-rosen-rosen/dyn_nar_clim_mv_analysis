# Narrative Safety-Climate — Multiverse Analysis

R analysis for the study relating **narrative-derived organizational safety
climate** to safety outcomes across **rail, nuclear power, coal mining, and
aviation**. It consumes the per-report climate scores and event/operational
data produced by the companion Python toolkit
([`DynamicNarrativeClimateToolkit`](https://github.com/dr-rosen-rosen/DynamicNarrativeClimateToolkit))
and runs the multiverse / cross-validation modeling that generates the
manuscript figures and tables.

## What it does

1. **Panel multiverse + cross-validation** (`<industry>/1_*_panel_multiverse.R`,
   `2_*_panel_cv.R`; runners in `common/panel_multiverse_runner.R`,
   `panel_cv_runner.R`). For each industry × outcome it fits one negative-binomial /
   Tweedie mixed model per specification (135 climate-scoring configurations × 6
   temporal windows = 810 specs) and evaluates held-out predictive value under
   both time-series and organization-blocked cross-validation.

2. **Manuscript & Extended-Data outputs** (numbered scripts, run after the
   panel results exist):
   - `5_manuscript_plots.R` — Figure 1 forest, partial dependence, per-industry
     specification curves, facet-importance
   - `12_multiverse_support.R`, `13_multiverse_2x2.R` — multiverse-support table
     + figures (consistency × coherence, CV sign stability)
   - `6_/9_/10_/11_` — sensitivity analyses (report-rate, min-reports,
     zero-as-NA, sentiment/verbosity control, lagged-outcome control, CV sign
     concordance)
   - `8_extended_data_tables.R` — assembles the Extended Data package

## Setup

Requires **R ≥ 4.2** (developed on 4.5). Install the packages used across the
pipeline:

```r
install.packages(c(
  "tidyverse", "arrow", "glmmTMB", "broom.mixed", "lme4", "ordinal",
  "furrr", "patchwork", "ggrepel", "slider", "lubridate", "readxl",
  "scales", "glue", "here"
))
```

Point the scripts at the toolkit's processed data (paths near the top of each
driver / `5_manuscript_plots.R`) or at the Zenodo data deposit.

## Running

```
# 1. per-industry panel multiverse + CV  ->  results_new_new/<industry>/
Rscript rail/1_rail_panel_multiverse.R   # and 2_rail_panel_cv.R; likewise nrc / aviation / msha

# 2. manuscript + extended-data figures/tables  ->  report_figures_manuscript/
Rscript 5_manuscript_plots.R
Rscript 12_multiverse_support.R
Rscript 13_multiverse_2x2.R
Rscript 8_extended_data_tables.R
```

## Layout

```
rail/ nrc/ aviation/ msha/   # per-industry: config, data prep, panel drivers, fit models
common/                      # shared runners, data prep, postprocessing, figure helpers
5_ .. 13_*.R                 # manuscript + extended-data scripts
attic/                       # retired exploratory / event-level code (gitignored)
```

## Data & reproducibility

Input data (`data/`), model results (`results_new_new/`), and figures
(`report_figures_manuscript/`) are **not** tracked in git — they are shared via
the Zenodo deposit. See `DATA_MANIFEST.md` for data sources, coverage, and
provenance. The state used for the published study is tagged `paper1-submission`
(matching the toolkit tag).
