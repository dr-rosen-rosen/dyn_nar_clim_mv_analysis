# Supplementary Results — Robustness and alternative-explanation controls

This section reports the analyses that rule out the principal alternative
explanations for the narrative-climate → safety-outcome association. Each
subsection corresponds to an Extended Data figure (see crosswalk,
`ED_crosswalk_maintext_to_extended_data.csv`). Unless noted, all tests refit the
nine main-text best-performing models — the per-industry × outcome models
selected by maximum cross-validated improvement over a seasonal-plus-operations
baseline — and report the climate coefficient on the log rate-ratio scale with
95% confidence intervals; rate-ratio versions of every figure are provided
alongside the log-scale versions.

## S1. The effect is not an artifact of specification choice

The association is estimated within a specification multiverse of 810 climate
specifications per cell (135 climate configurations × 6 temporal windows) and
under two cross-validation strategies (time-series and organization-blocked).
The reported best-performing model is not an isolated corner of this space: it
sits within a broad pool of competitive specifications (ED Table 6), multiverse
convergence rates are high (ED Table 5), and the spec-curve distributions (ED
figures) show the sign and approximate magnitude of the climate effect are
stable across the multiverse rather than contingent on a single analytic path.

Comparing the best-performing specification selected independently under each
cross-validation strategy, the per-SD climate effect was sign-concordant in
seven of nine cells (ED Figure, CV sign comparison). The two exceptions differ
in kind: for nuclear percent-power-loss the two estimates are both small and
statistically non-significant (no substantive disagreement), whereas rail
accidents is the single cell whose direction depends on the cross-validation
scheme (time-series positive, organization-blocked negative; both per-SD
magnitudes small). We flag rail accidents accordingly as the least
CV-stable cell.

## S2. The effect is not a sentiment or verbosity detector

A natural concern is that an NLP-derived climate score simply measures how
negative or how long the source narratives are. We tested this directly by
refitting each best-performing model with two competing covariates windowed
identically to the climate variable: (i) whole-narrative VADER compound
sentiment, computed on the full report text independently of the climate
pipeline, and (ii) narrative length (word count). Both were z-scored.

The climate coefficient retained its sign and significance in all nine cells
when controlling for negativity and length jointly. In the best-powered cells
the estimate was essentially unchanged (e.g., nuclear LERs −6.05 → −6.11; rail
injuries −3.32 → −3.06; aviation incidents +2.39 → +3.03). Because the
climate score and whole-narrative sentiment are only negligibly correlated, the
semantic-similarity targeting that defines the climate measure carries
predictive information well beyond generic report negativity or verbosity
(ED Figure, sentiment-decomposition control;
`sensitivity_sentiment_control_table.csv`).

## S3. The effect is not merely outcome autocorrelation

Because safety narratives are written about events and event rates are serially
correlated, a climate → future-events association could in principle be an echo
of each organization's own recent event history. We tested this by refitting
each best-performing model with the organization's own lagged event rate — the
log rate at the prior one and two observed periods, z-scored — added as a
competing within-organization covariate. The organization random intercept
already absorbs between-organization differences in mean rate, so the lagged
term isolates the within-organization autoregressive channel that the critique
invokes. To separate the effect of the control from any change in sample, all
variants were fit on a common sample (periods for which both lags exist).

The climate coefficient retained its sign and statistical significance in every
one of the nine main-text cells. Attenuation of the coefficient between the
unadjusted model and the model adjusting for both lags was below 6% in six
cells. Two well-powered cells showed larger but partial attenuation — nuclear
LERs (−5.99 → −4.45; 26%; p = 2×10⁻⁸ → 3×10⁻⁵) and aviation incidents
(+2.36 → +1.41; 40%; p = 1×10⁻⁵ → 8×10⁻³) — both remaining clearly significant.
The two cells that were borderline or null at baseline (nuclear percent-power-
loss, aviation accidents) were unchanged by the control. The narrative climate
measure therefore predicts future safety outcomes beyond what an organization's
own recent event history explains (ED Figure, lagged-outcome control;
`sensitivity_lagged_outcome_table.csv`).

## S4. The effect is not an organization-level reporting-rate confound

A further concern is that organizations differing in how much they report could
generate both their climate scores and their event counts. At the
organization level the climate score is largely uncorrelated with reporting
rate: all nuclear and aviation cells show near-zero rank correlations
(|ρ| ≤ 0.17, all p > 0.13). Where a correlation is present it does not threaten
the interpretation — and for outcomes such as nuclear LERs the reporting-culture
mechanism (better climate → more candid self-reporting) makes the observed
negative climate coefficients conservative rather than inflated
(ED Figure, report-rate scatter; `sensitivity_report_rate_corr.csv`).

## S5. The effect is robust to inclusion and windowing conventions

The association is stable to two analytic conventions tested as paired
sensitivity refits: (i) the minimum-event inclusion threshold used to admit an
organization-period into the panel (ED Figure, cutoff sensitivity), and (ii)
whether zero climate scores are treated as numerical zeros or as missing in the
windowing step (ED Figure, zero-as-NA sensitivity). In both, the climate
coefficients move negligibly relative to their confidence intervals.

## S6. Scale and identification

The climate score occupies a small numeric range, which reflects the units of
the composite measure rather than a limitation of identification. Effects are
reported per standard deviation of climate; the score's variance is continuous
and predominantly within-organization, per-SD effects are robust to trimming and
stable across the multiverse, and confidence intervals are narrow. Because
non-differential measurement error in a small-variance predictor attenuates
coefficients toward the null, the reported associations are conservative lower
bounds (see `METHODS_NOTE_climate_scale_and_identification.md`).

---

### Provenance

| Subsection | Script | Table / figure artifact |
|---|---|---|
| S1 multiverse / CV | `5_manuscript_plots.R`, `11_cv_sign_comparison.R` | ED Tables 5–6; spec-curve ED figures; `extended_data_cv_sign_comparison_*` |
| S2 sentiment / length | `9a_compute_whole_narrative_vader.py`, `9_sentiment_control_sensitivity.R` | `sensitivity_sentiment_control_*` |
| S3 lagged outcome | `10_lagged_outcome_sensitivity.R` | `sensitivity_lagged_outcome_*` |
| S4 reporting rate | `6_sensitivity_plots.R` | `sensitivity_report_rate_*` |
| S5 cutoff / zero-as-NA | `6_sensitivity_plots.R` | `sensitivity_min_reports_*`, `sensitivity_zero_as_na_*` |
| S6 scale / identification | — | `METHODS_NOTE_climate_scale_and_identification.md` |
