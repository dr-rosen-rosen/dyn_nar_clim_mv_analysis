# Safety Climate Multiverse — Paper Companion

[![Zenodo DOI](https://zenodo.org/badge/DOI/PENDING.svg)](https://doi.org/PENDING)

R analysis code for:

> Rosen et al. (2026). _[Paper title]_. Nature Human Behaviour.

This repository contains all R code required to reproduce the figures,
tables, and statistical results in the manuscript. It is a frozen snapshot
of the working analysis repository at the time of submission; the working
repository continues to evolve under [link to working repo] but this
companion is pinned to commit `<COMMIT_SHA>` (2026-MM-DD).

The companion Python text-pipeline (sentence segmentation, embedding,
sentiment scoring, composite climate scores) is archived separately at
[link to dynclim-paper repo + Zenodo DOI].

---

## What's in this repository

```
.
├── README.md                       # this file
├── LICENSE                         # MIT
├── renv.lock                       # R package version snapshot
├── Methods.docx                    # final methods text (mirrors this README)
│
├── 4_plots_panel.R                 # Champion-spec selection driver
├── 5_manuscript_plots.R            # Main figures: 1a, 1b, 2, 3, 4
├── 6_sensitivity_plots.R           # Appendix sensitivity analyses
├── 7_table1_data_sources.R         # Table 1 + post-filter panel summary
│
├── common/                         # Cross-industry helpers
│   ├── best_model_analysis.R       # Champion-spec identification (compute_champions)
│   ├── configuration_concordance.R # Cross-industry concordance metrics
│   ├── data_prep.R                 # Generic event-level helpers
│   ├── generate_mv_report.R        # Quarto-driven champion reports
│   ├── manuscript_figures.R        # All Figure 1-4 builders
│   ├── panel_cv_runner.R           # Panel-rate CV engine (Δlog-lik)
│   ├── panel_data_prep.R           # Generic panel-grid + EWMA/SMA windowing
│   ├── panel_multiverse_runner.R   # Panel-rate multiverse engine (parallel)
│   ├── postprocessing.R            # Spec curve / config-linking helpers
│   ├── postprocessing_layers.R     # Layer-diagnostic plots
│   └── sensitivity_analyses.R      # Reporting-bias sensitivity helpers
│
├── rail/                           # Rail (FRA) industry module
│   ├── 1_rail_panel_multiverse.R   # Multiverse driver
│   ├── 2_rail_panel_cv.R           # Cross-validation driver
│   ├── config.R                    # Window specs, outcome registry
│   ├── data_prep.R                 # Operational-features rolling decomposition
│   ├── panel_data_prep.R           # Rail panel construction
│   └── panel_fit_models.R          # NB GLMM fits + outcome registry
│
├── nrc/                            # NRC (Nuclear) industry module
│   ├── 1_nrc_panel_multiverse.R
│   ├── 2_nrc_panel_cv.R
│   ├── config.R
│   ├── panel_data_prep.R           # Incl. findings + action-matrix outcomes
│   └── panel_fit_models.R          # NB + Tweedie + binomial outcome registry
│
├── aviation/                       # Aviation (NTSB/AIDS/ASRS) industry module
│   ├── 1_aviation_panel_multiverse.R
│   ├── 2_aviation_panel_cv.R
│   ├── 1_get_bts_t100.R            # BTS T-100 download utility
│   ├── config.R                    # ATC reporter-function constants
│   ├── data_prep.R                 # ASRS metadata + AIDS panel loaders
│   ├── panel_data_prep.R           # Aviation panel construction
│   └── panel_fit_models.R
│
├── data/                           # Raw inputs (downloaded from Zenodo)
│   ├── rail/
│   ├── nrc/
│   └── aviation/
│
├── results/                        # Multiverse + CV outputs (from this code)
│   ├── rail/
│   ├── nrc/
│   └── aviation/
│
├── report_figures_panel/           # Champion CSVs (from this code)
│   ├── rail/champions_best.csv
│   ├── nrc/champions_best.csv
│   └── aviation/champions_best.csv
│
└── report_figures_manuscript/      # Final figures + tables for the paper
    ├── figure1a_forest.pdf
    ├── figure1b_coef_grid.pdf
    ├── figure2_partial_dependence.pdf
    ├── figure3_spec_curves_main.pdf
    ├── figure4_facet_importance.pdf
    ├── table1_*.csv
    └── appendix_*.pdf
```

---

## Data

All raw and processed input data are archived on Zenodo:

> [Data DOI placeholder] — Rosen et al. (2026). Safety Climate Multiverse — Input Data.

Download the data archive, unpack it as `data/` at this repository's root
before running any scripts. Intermediate climate-score parquets (the output
of the Python text pipeline) are in a separate Zenodo deposit:

> [Climate-scores DOI placeholder] — Rosen et al. (2026). Climate Score Configuration Registry.

After download, your directory should look like:

```
./data/
├── rail/events.parquet
├── rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv
├── nrc/events.parquet
├── nrc/power_status_quarterly.parquet/
├── nrc/findings_quarterly.parquet
├── nrc/action_matrix_long.parquet
├── aviation/asrs_events.parquet
├── aviation/aids_events.parquet
├── aviation/ntsb_av_accident_data/events.xlsx
├── aviation/ntsb_av_accident_data/events_pre2008.xlsx
└── aviation/bts_t100/airport_month_ops.parquet

./climate_configs/
├── rail_04-14-2026/
│   ├── config_registry.csv
│   └── _cfg/{0..134}/results.parquet
├── nrc_04-14-2026/
│   ├── config_registry.csv
│   └── _cfg/{0..134}/results.parquet
└── asrs_05-01-2026/
    ├── config_registry.csv
    └── _cfg/{0..134}/results.parquet
```

The numbered driver scripts expect these paths. If you place data elsewhere,
edit the path constants at the top of each driver.

---

## System requirements

- **R**: version 4.4 or later
- **Memory**: ~32 GB recommended (the rail multiverse parallelizes over many workers and holds the panel in each)
- **CPUs**: 8+ cores recommended; the multiverse and CV drivers use `furrr` for parallel execution
- **Disk**: ~5 GB for intermediates
- **Total runtime**: approximately 6–10 hours end-to-end on a 2024 MacBook Pro M3 Max (4 cores active per industry × 3 industries × 135 configs × 6 windows × ~6 outcomes for multiverse, plus the same scope for CV)
  - Multiverse phase: ~3–4 hours
  - CV phase: ~2–3 hours
  - Figures and tables: ~30 minutes

---

## Reproduction

### Step 0: Install R packages

If you use `renv`:
```r
renv::restore()
```

Otherwise install manually (versions used at submission shown):

```r
install.packages(c(
  "tidyverse",      # 2.0.0
  "glmmTMB",        # 1.1.10
  "arrow",          # 17.0.0
  "furrr",          # 0.3.1
  "broom.mixed",    # 0.2.9.4
  "slider",         # 0.3.2
  "lubridate",      # 1.9.4
  "patchwork",      # 1.3.0
  "ggrepel",        # 0.9.5
  "RColorBrewer",   # 1.1-3
  "here",           # 1.0.1
  "readxl"          # 1.4.5
))
```

### Step 1: Multiverse fits (slow; ~3–4 hours)

Fit all 810 (config × window) × (industry × outcome) combinations:

```bash
Rscript rail/1_rail_panel_multiverse.R
Rscript nrc/1_nrc_panel_multiverse.R
Rscript aviation/1_aviation_panel_multiverse.R
```

Outputs:
- `results/rail/panel_mv_results.parquet`
- `results/nrc/panel_mv_results.parquet`
- `results/aviation/panel_mv_results.parquet`

Each driver writes per-config checkpoints in
`results/<industry>/panel_checkpoints/` so interrupted runs can resume.

### Step 2: Cross-validation (slow; ~2–3 hours)

Same scope as Step 1, but with 5-fold group k-fold and 5-split timeseries
CV per cell, computing Δlog-lik per held-out observation:

```bash
Rscript rail/2_rail_panel_cv.R
Rscript nrc/2_nrc_panel_cv.R
Rscript aviation/2_aviation_panel_cv.R
```

Outputs:
- `results/<industry>/panel_cv_results.parquet`

### Step 3: Champion-spec selection + industry reports

```bash
Rscript 4_plots_panel.R
```

This produces:
- `report_figures_panel/<industry>/champions_best.csv` — the headline
  champion spec per (outcome × CV strategy)
- `report_figures_panel/<industry>/champions_candidates.csv` — the
  top-10% candidate pool
- Quarto-rendered industry diagnostic reports

### Step 4: Manuscript figures

```bash
Rscript 5_manuscript_plots.R
```

Produces, in `report_figures_manuscript/`:
- `figure1a_forest.pdf` — Champion-coefficient forest plot across cells
- `figure1b_coef_grid.pdf` — Per-cell standardized-coefficient grid
- `figure2_partial_dependence.pdf` — Predicted-rate response vs climate
- `figure3_spec_curves_main.pdf` — 3×3 CV Δlog-lik spec curves
- `figure4_facet_importance.pdf` — ANOVA-on-Δlog-lik facet variance shares
- All `appendix_*.pdf` variants

### Step 5: Table 1 + panel summary

```bash
Rscript 7_table1_data_sources.R --with-panel
```

Produces:
- `report_figures_manuscript/table1_sources_long.csv` — one row per data source
- `report_figures_manuscript/table1_wide.csv` — one row per industry
- `report_figures_manuscript/table1_panel_summary.csv` — post-filter panel
  characteristics per (industry × outcome)

### Step 6 (optional): Sensitivity analyses

```bash
Rscript 6_sensitivity_plots.R
```

Produces appendix sensitivity figures examining the reporting-bias
hypothesis via between-organization mean climate vs. report-rate scatter.

---

## Output → Methods/Results mapping

| Methods/Results section | Code path | Output file(s) |
|---|---|---|
| Panel-rate framework (Methods) | `common/panel_data_prep.R`, `<industry>/panel_data_prep.R` | (intermediate panel objects) |
| Multiverse specification | `common/panel_multiverse_runner.R` + per-industry MV drivers | `results/<industry>/panel_mv_results.parquet` |
| Cross-validation | `common/panel_cv_runner.R` + per-industry CV drivers | `results/<industry>/panel_cv_results.parquet` |
| Champion-spec selection (Methods) | `common/best_model_analysis.R::compute_champions()` | `report_figures_panel/<industry>/champions_best.csv` |
| Table 1 (Results) | `7_table1_data_sources.R` | `report_figures_manuscript/table1_*.csv` |
| Figure 1A — Forest plot | `common/manuscript_figures.R::plot_champion_forest()` | `figure1a_forest.pdf` |
| Figure 1B — Coefficient grid | `common/manuscript_figures.R::plot_champion_coef_grid()` | `figure1b_coef_grid.pdf` |
| Figure 2 — Partial dependence | `common/manuscript_figures.R::plot_champion_partial_dependence()` | `figure2_partial_dependence.pdf` |
| Figure 3 — CV spec curves on Δlog-lik | `common/manuscript_figures.R::plot_cv_spec_curve_manuscript()` | `figure3_spec_curves_main.pdf` |
| Figure 4 — Facet importance | `common/manuscript_figures.R::plot_dll_facet_importance()` | `figure4_facet_importance.pdf` |
| Appendix — group-kfold spec curves | same as Figure 3, different CV strategy | `appendix_spec_curves_group_kfold.pdf` |
| Appendix — non-main outcomes | same as Figure 3, EXCLUDE_FROM_MAIN cells | `appendix_spec_curves_appendix_outcomes.pdf` |
| Appendix — traditional β spec curves | `common/manuscript_figures.R::plot_traditional_spec_curve_manuscript()` | `appendix_traditional_spec_curves.pdf` |
| Appendix — sensitivity scatters | `6_sensitivity_plots.R` | various `appendix_sensitivity_*.pdf` |

---

## Repository provenance

This frozen paper-companion repository was created from the working
analysis repository at:

> [working repo URL]@<COMMIT_SHA> (2026-MM-DD)

Files were curated according to the inventory documented in
`paper_repo_prep/FILE_INVENTORY.md` in the working repository. The
following modules from the working repo are intentionally **excluded**
from this paper companion as they were not used in the analyses reported
in the manuscript:

- PHMSA pipeline (`phmsa/`)
- Event-level (pre-panel-rate) multiverse and CV (`1_*_multiverse.R`,
  `2_*_cv.R` non-panel variants, and `common/multiverse_runner.R`,
  `common/cv_runner.R`)
- Layer-feature framework (`common/layer_*.R`, `common/feature_layers.R`,
  `common/temporal_features.R`)
- Exploratory diagnostics (`phase1_panel_diagnostic.R`, `facility_highlight_chart.R`, `0_audit_aviation.R`, `0_main_*.R` orchestrators)

These modules continue to evolve in the working repository for follow-up
work; the working repo URL is maintained for transparency.

---

## Citation

If you use this code, please cite both the paper and this archive:

```bibtex
@article{rosen2026safety,
  author = {Rosen, ...},
  title  = {[Paper title]},
  journal = {Nature Human Behaviour},
  year   = {2026},
  doi    = {[paper DOI]}
}

@software{rosen2026safety_code,
  author = {Rosen, ...},
  title  = {Safety Climate Multiverse — Paper Companion},
  year   = {2026},
  publisher = {Zenodo},
  doi    = {[code DOI]}
}
```

---

## License

MIT — see `LICENSE` for full text.

---

## Contact

For questions about the code or analyses, contact:
[corresponding author email].

For substantive questions about the construct or extensions to additional
industries, see the working repository for ongoing development.
