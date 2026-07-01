# Methods Note — Climate-score scale, effect reporting, and identification

Pinned reference for the recurring reviewer instinct: "the climate predictor
has a tiny numeric range yet you report large coefficients — is that
suspicious?" Short answer: no. The small scale is a units phenomenon, and if
anything it biases effects toward the null.

## Two rate-ratio scales — report per-SD, not per-unit

| Scale | What it is | Example (NRC LERs) | Use |
|---|---|---|---|
| Per-unit | exp(raw coefficient); effect of a +1.0 change in the climate score | RR = 0.001 (= exp(-6.57)) | **Not** for interpretation; a 1.0-unit change is ~70 SDs, far outside observed range |
| Per-SD | exp(coefficient x in-panel SD of climate); effect of a realistic +1-SD shift | RR = 0.92 | **Main-text reporting** |

The raw coefficient is large only because the predictor SD is small
(coefficient = effect per 1 unit; 1 unit is enormous relative to SD ~0.01-0.08).
`coefficient x SD` is invariant to predictor rescaling, so the per-SD effect
is the scale-free quantity. Per-SD rate ratios for the main cells all fall in
0.80-1.20 (8-20% effects) — modest and plausible, not extreme.

Main-text Figure 1A is the per-SD forest
(`figure1a_forest_persd_{log,rateratio}.pdf`). The per-unit forest is retained
in Extended Data (`extended_data_forest_perunit_*.pdf`).

## Why the small scale biases toward the null (not toward false positives)

The climate score is NLP-derived and carries measurement error. Classical
(non-differential) measurement error causes regression dilution: coefficients
attenuate toward zero, and the attenuation is worse when the true signal
variance is small relative to noise. A small-variance, noisily-measured
predictor therefore makes effects HARDER to detect. The reported associations
are best read as **conservative lower bounds**.

## Empirical evidence the variance is real (not degenerate or leverage-driven)

Characterized on the tightest-SD main cell (NRC LERs, windowed climate
`overall_final_score_ewmaLAG_720d_hl360d`, SD = 0.0138):

- **Continuous, not degenerate:** 3,142 distinct values across 5,655
  observations (56%); smooth quantile spread (5th -0.050, median -0.025,
  95th -0.007, max +0.015).
- **Predominantly within-organization:** variance decomposition ~88% within-org
  / ~12% between-org. The effect is identified from *longitudinal* change in a
  facility's climate over time, not cross-organization differences (which the
  facility random intercept absorbs anyway).
- **Not leverage-driven:** trimming the outer 5% of org-quarters reduces the SD
  only from 0.0138 to 0.0113 (ratio 0.82).
- **Well-identified:** per-SD CIs are tight (e.g., NRC LERs RR 0.92 [0.89, 0.95]),
  not the wide intervals expected under weak identification from a near-constant
  predictor.

## What would have been concerning — and is not

| Potential concern | Status |
|---|---|
| Degenerate / near-constant predictor | Ruled out (56% distinct values, smooth distribution) |
| A few high-leverage organizations | Ruled out (trim-stable; large org counts; effect holds across all 810 multiverse specs) |
| Weak identification (huge coef, huge SE) | Ruled out (tight per-SD CIs) |
| Mechanical predictor-outcome scale coupling | None (climate from narratives; outcome = event counts with an independent exposure offset) |

## Manuscript text (drop-in)

> Although the narrative climate score occupies a small numeric range, this
> reflects the scale of the composite measure rather than a limitation of
> identification. Effects are reported per standard deviation of climate; the
> climate score's variance is continuous and predominantly within-organization
> (~88% for the nuclear LER cell), per-SD effects are robust to trimming and
> stable across the specification multiverse, and confidence intervals are
> narrow. Because non-differential measurement error in a small-variance
> predictor attenuates coefficients toward the null, the reported associations
> are conservative.
