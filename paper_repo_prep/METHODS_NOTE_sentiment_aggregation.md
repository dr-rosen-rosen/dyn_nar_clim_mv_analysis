# Methods Note — Per-segment aggregation of sentiment and similarity

Pinned reference so this detail does not get re-litigated. Verified against
the toolkit source on 2026-05-XX (`dynclim/features/safety/toolkit.py`,
`dynclim/features/safety/unified_processor.py`).

## The two per-item signals

Every scale item produces two signals, aggregated from its segments:

- `sent_signal` — signed sentiment valence for that item
- `strength_signal` — semantic-similarity salience for that item
- `final_score = sent_signal × strength_signal`
  (`unified_processor.py`, ItemFeatureResult definition)

Report-level values are simple means across items:
`overall_sent = mean(item sent_signal)`,
`overall_strength = mean(item strength_signal)`,
`overall_final_score = mean(item final_score)`
(`unified_processor.py:1039–1040`).

## How segments are aggregated into each item signal

Both signals are computed by the composite scorer as `Σ wᵢ · xᵢ`, where
`xᵢ` is the per-segment quantity (sentiment for `sent_signal`, similarity
for `strength_signal`) and the weights `wᵢ` depend on the composite method:

| Composite method | Segment weights `wᵢ` | `sent_signal` is… | `strength_signal` is… |
|---|---|---|---|
| `weighted_average` | uniform, `wᵢ = 1/n` over thresholded-selected segments (`toolkit.py:898`) | unweighted mean sentiment of selected segments | unweighted mean similarity of selected segments |
| `attention_weight` | `wᵢ = softmax(simᵢ / τ)` (`toolkit.py:996`) | similarity-weighted mean sentiment | similarity-weighted mean similarity |
| `power_attention` | `wᵢ = simᵢᵖ / Σ simⱼᵖ` (`toolkit.py:1317`) | similarity-weighted mean sentiment | similarity-weighted mean similarity |

Notes:
- The `weighted_average` name is historical and slightly misleading: its
  segment weights are **uniform**, not similarity-proportional. The legacy
  `similarity_weight` / `sentiment_weight` config fields are explicitly
  "kept for backward compatibility; not used here" (`toolkit.py:881–883`).
- **Segment selection (thresholding) is always applied** and is
  composite-invariant: `n_segments` per item does not change across
  composite methods (verified empirically — 0% of reports show variation
  in `LSC_1_n_segments` across the five composite variants of a config that
  is otherwise identical).
- For `attention_weight` and `power_attention`, the segment **sentiments
  are reweighted by similarity**, so `overall_sent` varies with the
  composite method (verified: `overall_sent` differs across composite
  method for ~56% of NRC reports under an otherwise-identical config).

## What this means for the best-performing models in the paper

All three main-text best-performing models use `power_attention`
(rail injuries, NRC LERs, aviation broad-incidents). Therefore, for the
specifications actually reported, `overall_sent` is **both thresholded and
similarity-weighted** — it is not a raw measure of report sentiment.

## What is NOT saved

The persisted per-config output (`_cfg/<id>/results.parquet`) contains only:
`config_id`, `report_id`, `overall_final_score`, `overall_sent`,
`overall_strength`, processing metadata, and the per-item
`_sent` / `_strength` / `_final` / `_n_segments` columns.

There is **no genuinely-raw similarity column** and **no genuinely-raw
sentiment column** in the saved data. The `sims_all_mean` / `sims_sel_mean`
diagnostics computed in `_build_item_diagnostics()` are logging-only and are
not written to the parquet. Each config checkpoint directory contains only
`meta.json` and `results.parquet` (verified).

Consequence: any analysis requiring a raw, pipeline-independent sentiment
(or similarity) measure must **recompute it from the narrative text** — it
cannot be read from the saved outputs. This motivates the whole-narrative
VADER control used in the sentiment-decomposition sensitivity analysis
(`9_sentiment_control_sensitivity.R`): VADER scored on the full untrimmed
narrative is the only fully pipeline-independent measure of report-level
affective valence available, since `overall_sent` is entangled with both
thresholding and (for the power_attention champions) similarity weighting.

## One-line summary for the manuscript methods text

> Report-level sentiment and similarity signals are weighted means of
> per-segment values over segments surviving similarity thresholding; for
> attention- and power-attention composites the segment weights are
> similarity-derived, so the reported sentiment signal is itself a
> similarity-weighted, thresholded quantity rather than a raw measure of
> narrative sentiment.
