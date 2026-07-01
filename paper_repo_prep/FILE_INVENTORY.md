# File Inventory for the Paper Repo

This is the definitive "keep / drop" list for the frozen paper repository,
derived by transitively tracing `source()` calls and function references
from the paper's entry points:

- `4_plots_panel.R` (champion selection + Quarto reports)
- `5_manuscript_plots.R` (Figures 1, 2, 3, 4)
- `6_sensitivity_plots.R` (Appendix sensitivity figures)
- `7_table1_data_sources.R` (Table 1)
- `rail/1_rail_panel_multiverse.R`, `nrc/1_nrc_panel_multiverse.R`,
  `aviation/1_aviation_panel_multiverse.R` (multiverse fits)
- `rail/2_rail_panel_cv.R`, `nrc/2_nrc_panel_cv.R`,
  `aviation/2_aviation_panel_cv.R` (cross-validation)

Files marked KEEP are referenced — directly or transitively — by at least
one of the above. Files marked DROP are not referenced by any paper path.

## Top-level scripts

| File | Status | Reason |
|---|---|---|
| `3_plots.R` | **DROP** | Event-level report generator from the old (pre-panel-rate) pipeline. Not used by any paper figure. |
| `4_plots_panel.R` | KEEP | Generates champion CSVs via `generate_mv_report()`. Required prior to 5/6. |
| `5_manuscript_plots.R` | KEEP | Builds main-text figures 1A, 1B, 2, 3, 4 and appendix variants. |
| `6_sensitivity_plots.R` | KEEP | Appendix sensitivity analyses. |
| `7_table1_data_sources.R` | KEEP | Generates Table 1 and post-filter panel summary. |

## common/

| File | Status | Reason |
|---|---|---|
| `best_model_analysis.R` | KEEP | `compute_champions()` — used by `generate_mv_report.R`. |
| `configuration_concordance.R` | KEEP | Sourced by 4, 5, 6 and by `generate_mv_report.R`. |
| `cv_runner.R` | **DROP** | Event-level CV runner; superseded by `panel_cv_runner.R`. |
| `data_prep.R` | KEEP | Generic helpers; sourced by 5, 6, 7 and all per-industry drivers. |
| `feature_layers.R` | **DROP** | Feature-engineering layer framework not used by panel pipeline. |
| `generate_mv_report.R` | KEEP | Drives champion selection + Quarto reports from 4. |
| `layer_ews.R` | **DROP** | Early-warning-system layer; exploratory, not in paper. |
| `layer_template.R` | **DROP** | Layer-pattern template. |
| `layer_temporal.R` | **DROP** | Temporal-feature layer; exploratory. |
| `manuscript_figures.R` | KEEP | All Figure 1-4 builders and helpers. |
| `multiverse_runner.R` | **DROP** | Event-level MV runner; superseded by `panel_multiverse_runner.R`. |
| `panel_cv_runner.R` | KEEP | Panel-rate CV engine. |
| `panel_data_prep.R` | KEEP | Generic panel-grid + windowing helpers. |
| `panel_multiverse_runner.R` | KEEP | Panel-rate multiverse engine. |
| `postprocessing.R` | KEEP | Spec curve / config-linking helpers; sourced by 4, 5, 6, 7. |
| `postprocessing_layers.R` | KEEP | Layer-specific plotting; sourced by 4. (See note on `feature_layers.R` — if you remove that, audit whether `postprocessing_layers.R` still works in the report pipeline. If not, both can be dropped together, with the Quarto template adjusted.) |
| `sensitivity_analyses.R` | KEEP | Used by 6. |
| `spec_curve_common.R` | **DROP** | Helpers for old (event-level) spec curves. |
| `temporal_features.R` | **DROP** | Exploratory temporal-feature code. |
| `validation_protocol.R` | **DROP** | Exploratory; not in paper drivers. |

## rail/

| File | Status | Reason |
|---|---|---|
| `0_main_rail.R` | **DROP** | Orchestrates the old event-level pipeline. |
| `1_rail_multiverse.R` | **DROP** | Event-level MV; superseded by `1_rail_panel_multiverse.R`. |
| `1_rail_panel_multiverse.R` | KEEP | Paper multiverse driver. |
| `2_rail_cv.R` | **DROP** | Event-level CV. |
| `2_rail_panel_cv.R` | KEEP | Paper CV driver. |
| `config.R` | KEEP | Window specs, outcome registry, formula builders. |
| `data_prep.R` | KEEP | `make_ops_features_rolling()` used by panel pipeline. (Trim `prepare_config_data()` — dead.) |
| `fit_models.R` | **DROP** | Event-level fits; superseded by `panel_fit_models.R`. |
| `panel_data_prep.R` | KEEP | Panel construction. |
| `panel_fit_models.R` | KEEP | Panel-rate fit functions + outcome registry. |

## nrc/

| File | Status | Reason |
|---|---|---|
| `0_main_nrc.R` | **DROP** | Orchestrates the old event-level pipeline. |
| `1_nrc_multiverse.R` | **DROP** | Event-level MV. |
| `1_nrc_panel_multiverse.R` | KEEP | Paper multiverse driver. |
| `2_nrc_cv.R` | **DROP** | Event-level CV. |
| `2_nrc_panel_cv.R` | KEEP | Paper CV driver. |
| `config.R` | KEEP | NRC-specific config. |
| `data_prep.R` | **DROP** | Functions (`encode_nrc_outcomes`, `prepare_nrc_config_data`, `join_nrc_ops`) are all referenced only by `0_main_nrc.R` and `1_nrc_multiverse.R`. The panel pipeline uses `aggregate_nrc_outcomes()` defined in `panel_data_prep.R`, which is a refactored re-implementation; a comment in `panel_data_prep.R` references `data_prep.R::encode_nrc_outcomes()` for parser-correction provenance, but it's not actually imported. **Verify by grepping** before deleting. |
| `facility_highlight_chart.R` | **DROP** | Exploratory NRC visualization, not in paper. |
| `fit_models.R` | **DROP** | Event-level fits. |
| `panel_data_prep.R` | KEEP | Panel construction (incl. new findings/action-matrix outcomes). |
| `panel_fit_models.R` | KEEP | Panel-rate fits + outcome registry. |
| `phase1_panel_diagnostic.R` | **DROP** | Exploratory diagnostic, not in paper. |

## aviation/

| File | Status | Reason |
|---|---|---|
| `0_audit_aviation.R` | **DROP** | Exploratory data audit. |
| `0_main_aviation.R` | **DROP** | Orchestrates the old event-level pipeline. |
| `1_aviation_multiverse.R` | **DROP** | Event-level MV. |
| `1_aviation_panel_multiverse.R` | KEEP | Paper multiverse driver. |
| `1_get_bts_t100.R` | KEEP (optional) | BTS T-100 download utility. Include for replication if BTS source URL is volatile; can be dropped if T-100 parquet is archived on Zenodo. |
| `2_aviation_cv.R` | **DROP** | Event-level CV. |
| `2_aviation_panel_cv.R` | KEEP | Paper CV driver. |
| `config.R` | KEEP | ATC scope constants, function lists. |
| `data_prep.R` | KEEP | `load_asrs_meta_panel()` and `load_aids_panel_rate()` used. (See DEAD_CODE.md for which sub-functions to trim.) |
| `fit_models.R` | **DROP** | Event-level fits. |
| `panel_data_prep.R` | KEEP | Panel construction (incl. fixed ASRS facility-type normalization). |
| `panel_fit_models.R` | KEEP | Panel-rate fits + outcome registry. |
| `phase1_panel_diagnostic.R` | **DROP** | Exploratory. |

## phmsa/ — DROP ENTIRE DIRECTORY

| File | Status |
|---|---|
| `phmsa/*` | **DROP** (all files) |

PHMSA was scoped out of the paper. Industry was excluded from
`5_manuscript_plots.R` via the industries-list curation. The phmsa code
should not appear in the paper repo at all; remove the entire directory.

If `4_plots_panel.R` retains commented-out phmsa entries, delete those
lines outright in the paper-repo version.

## Other artifacts at the project root

| Artifact | Status | Reason |
|---|---|---|
| `Methods.docx` | KEEP | Source for paper methods text. |
| `Methods_revised_tracked.docx` | DROP | Internal review artifact. |
| `paper_repo_prep/` (this dir) | DROP from paper repo | Staging area only. |
| `data/` | KEEP (curated) | See data section below. |
| `results_new_new/` | KEEP (curated) | See results section below. |
| `report_figures_panel/` | KEEP (curated) | Champion CSVs needed by 5. |
| `report_figures_manuscript/` | KEEP (curated) | Final figure outputs. |

## Data & results inclusions for the paper repo

### `data/` (raw inputs not on Zenodo)
Many of the raw inputs are not in this repo currently — they live in the
parallel `DynamicNarrativeClimateToolkit` path under `dynclim/data/processed/`.
For the paper repo, either:

- **Option A** (recommended): copy the specific input parquets into a flat
  `data/` directory inside the paper repo (small inputs only), and document
  the larger files (e.g., BTS T-100, full NRC events) as Zenodo downloads.
- **Option B**: put everything on Zenodo and have the repo's setup script
  download into `data/` at first run.

Required input files for end-to-end reproduction:
- `data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv`
- `data/aviation/ntsb_av_accident_data/events.xlsx`
- `data/aviation/ntsb_av_accident_data/events_pre2008.xlsx`
- `data/aviation/bts_t100/airport_month_ops.parquet`
- `dynclim/data/processed/rail/events.parquet` (rename / move to `data/rail/events.parquet`)
- `dynclim/data/processed/nrc/events.parquet` (rename / move to `data/nrc/events.parquet`)
- `dynclim/data/processed/nrc/power_status_quarterly.parquet/` (move to `data/nrc/`)
- `dynclim/data/processed/nrc/findings_quarterly.parquet`
- `dynclim/data/processed/nrc/action_matrix_long.parquet`
- `dynclim/data/processed/aviation/events.parquet` (move to `data/aviation/asrs_events.parquet`)
- `dynclim/data/processed/aviation/aids_events.parquet` (move to `data/aviation/`)

### Climate config registries (these come from the toolkit)
- `dynclim/notebooks/checkpoints/rail_04-14-2026/config_registry.csv` and `_cfg/*/results.parquet`
- `dynclim/notebooks/checkpoints/nrc_04-14-2026/config_registry.csv` and `_cfg/*/results.parquet`
- `dynclim/notebooks/checkpoints/asrs_05-01-2026/config_registry.csv` and `_cfg/*/results.parquet`

These are the 135-config × 3-industry × climate-score parquets that the R
analysis joins on. They're produced by the toolkit. For the paper repo,
**Zenodo deposit them** rather than copy — they're large and stable.

### `results_new_new/` (multiverse + CV outputs from MV/CV runs)
Curated to:
- `results/rail/panel_mv_results.parquet`
- `results/rail/panel_cv_results.parquet`
- `results/nrc/panel_mv_results.parquet`
- `results/nrc/panel_cv_results.parquet`
- `results/aviation/panel_mv_results.parquet`
- `results/aviation/panel_cv_results.parquet`

Recommend **renaming** `results_new_new/` to `results/` in the paper repo
(the `_new_new` suffix is dev cruft and would confuse readers).

### `report_figures_panel/` (champion selection outputs)
Keep:
- `report_figures_panel/<industry>/champions_best.csv` (×3)
- `report_figures_panel/<industry>/champions_candidates.csv` (×3)
- Quarto-rendered industry reports (optional — for reviewers who want them)

Drop:
- Any `report_figures_panel/phmsa*` directories
- Intermediate Quarto temp files

### `report_figures_manuscript/` (the actual paper figures)
Keep everything in here that's referenced in the paper. After the final
end-to-end run produces them, this directory should contain:
- `figure1a_forest.pdf`
- `figure1b_coef_grid.pdf`
- `figure2_partial_dependence.pdf`
- `figure3_spec_curves_main.pdf`
- `figure4_facet_importance.pdf`
- All `appendix_*.pdf` variants
- `table1_sources_long.csv`, `table1_panel_summary.csv`, `table1_wide.csv`
- Supporting CSVs (`champion_summary.csv`, `figure1_coefs_standardized.csv`, `figure4_facet_importance_ts.csv`, etc.)
- Supporting parquets (`figure2_pd_data.parquet`,
  `figure3_dll_multiverse_ts.parquet`,
  `figure3_dll_multiverse_gk.parquet`,
  `appendix_beta_multiverse.parquet`)

Drop:
- `figure1_composite.pdf` (superseded by figure1a + figure1b)
- Any old appendix files no longer referenced (e.g., `appendix_spec_curves_all.pdf` was renamed to `appendix_spec_curves_appendix_outcomes.pdf`)
