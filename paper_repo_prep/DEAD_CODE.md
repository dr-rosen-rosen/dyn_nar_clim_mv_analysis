# Dead Code Within KEEP Files

Files marked KEEP in `FILE_INVENTORY.md` contain functions that are not
reached from any paper entry point. These are candidates for trimming
before freezing the paper repo. None of these deletions are required —
they just clean up.

**Verify each before deleting** by running:
```bash
cd <paper-repo-root>
grep -rn "<function_name>" --include="*.R" --include="*.qmd"
```
If the only matches are in the file defining the function (plus the
roxygen comment), it's safe to remove.

## aviation/data_prep.R

Used by paper drivers: `load_asrs_meta_panel()`, `load_aids_panel_rate()`.

Candidates for removal:

| Function | Where defined | Status |
|---|---|---|
| `load_asrs_meta()` | aviation/data_prep.R | Superseded by `load_asrs_meta_panel()`. The non-`_panel` variant is from the old event-level pipeline. |
| `encode_av_outcomes()` | aviation/data_prep.R | Only referenced by old event-level fits. Panel pipeline uses inline aggregation in `load_ntsb_panel_rate()` and `load_aids_panel_rate()`. |
| `load_ntsb_panel()` | aviation/data_prep.R | Superseded by `load_ntsb_panel_rate()` defined in `aviation/panel_data_prep.R`. |
| `join_av_ops()` | aviation/data_prep.R | Old event-level helper. |
| `project_climate_to_panel()` | aviation/data_prep.R | Earlier-prototype function; the working version is in `panel_data_prep.R`. |
| `prepare_av_config_data()` | aviation/data_prep.R | Old event-level data prep. |

After trimming, `aviation/data_prep.R` should contain only:
- `load_asrs_meta_panel()` (incl. the facility-type normalization fix)
- `load_aids_panel_rate()`
- Any module-level constants those two functions reference (ATC_FUNCTIONS_LOCAL, etc.)

## rail/data_prep.R

Used by paper drivers: `make_ops_features_rolling()`.

Candidate for removal:

| Function | Where defined | Status |
|---|---|---|
| `prepare_config_data()` | rail/data_prep.R | Old event-level config-data builder. Not used by `prepare_rail_panel_data()` in `rail/panel_data_prep.R`. |

After trimming, `rail/data_prep.R` should contain only `make_ops_features_rolling()` plus its dependencies.

## common/postprocessing.R

Large file with many helpers used across `4_plots_panel.R`, `5_manuscript_plots.R`, `generate_mv_report.R`, etc. **Do not aggressively prune.** Some functions are referenced through the Quarto report templates, not just direct `source()`. Recommended action: leave intact for the paper freeze and revisit if cleanup is wanted in the next iteration.

## common/generate_mv_report.R

Comments at lines 9 and 13 reference an old `shared/` path that no longer exists. These are documentation cruft only — the code uses dynamic path resolution via `.bma_path` and `concordance_path`. Safe to leave; no functional impact.

## common/postprocessing_layers.R

Only used by `4_plots_panel.R` for layer-based diagnostic plots in the Quarto report. If you drop `common/feature_layers.R`, `common/layer_*.R`, and `common/temporal_features.R` (all marked DROP in the inventory), you must also:

1. Check the Quarto report template (`common/mv_report_template.qmd` and `common/mv_report_condensed_template.qmd`) for any references to layer-plot functions.
2. Either remove those template sections, or keep `postprocessing_layers.R` and the layer files as a minimal set.

Recommendation: if the layer-based diagnostics are not discussed in the paper or appendix, remove the corresponding template sections and drop both `postprocessing_layers.R` and the `layer_*.R` files. If they are referenced anywhere in the paper, keep them.

## Driver-level dead code

### `4_plots_panel.R`

Lines 66–85 define `phmsa_industry()` and instantiate `phmsa_gd / phmsa_gt / phmsa_hl`. Delete those lines outright in the paper-repo version (PHMSA is excluded from the paper).

### `5_manuscript_plots.R`

No paper-specific dead code; the `EXCLUDE_FROM_MAIN` mechanism handles main-text vs appendix splits.

### `6_sensitivity_plots.R`

Verify no PHMSA references survive after copying.

### `7_table1_data_sources.R`

The `--with-panel` flag includes substantial NRC findings and action-matrix code. Since those outcomes are appendix-only in the paper, the panel summary section is needed to produce the appendix table. Keep as is.

## Files referenced by name but never imported

These references in code comments mention files that should NOT make it into the paper repo:

| Reference | Location | Action |
|---|---|---|
| `nrc/data_prep.R::encode_nrc_outcomes()` | comment in `nrc/panel_data_prep.R` line ~92 | Update comment to point to the local re-implementation in `aggregate_nrc_outcomes()` and remove the file-path reference. |
| `shared/postprocessing.R` | comment in `common/generate_mv_report.R` lines 9, 13 | Delete the stale comments. |

## Stale RDS / parquet / temp files to remove

The working repo accumulates intermediate files that should not appear in the paper repo:

```bash
# Run from paper-repo root after copying files in
find . -name "*.Rhistory" -delete
find . -name ".DS_Store" -delete
find . -name "*.rds" -path "*/results*/*" -mtime +60 -delete  # old run artifacts
find . -name "Rplots.pdf" -delete
```

Also check for and remove:
- Any `results/` or `results_*/` directories from runs that aren't the final paper run
- Any `report_figures_*/phmsa*` subdirectories
- Old Quarto cache directories (`*.quarto_cache/`)
- Test scripts named `test_*.R` or `scratch_*.R`
- Any `notes.md` / `TODO.md` files that don't reflect the paper state
