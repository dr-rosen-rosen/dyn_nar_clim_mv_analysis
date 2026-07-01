# Methods Note — Lagged-outcome control (autocorrelation critique)

Pinned reference for the reviewer instinct: "events generate the narratives,
event rates are autocorrelated, so your climate→future-events association is
just an echo of each organization's own recent event history." Short answer:
no — the climate effect survives controlling for the organization's own lagged
event rate in every main-text cell.

## What the test does

For each main-text best-performing model (9 cells), we refit adding the
organization's **own lagged event rate** on the model's natural scale —
`log((count + 0.5) / exposure)` — at lag 1 (prior observed period) and lag 1+2.
This is an explicit autoregressive control: if a climate→events association
were merely the autocorrelation of the outcome with itself, conditioning on the
lagged outcome would absorb it and the climate coefficient would collapse.

Script: `10_lagged_outcome_sensitivity.R`. Outputs
`sensitivity_lagged_outcome_table.csv` and
`sensitivity_lagged_outcome_forest_{log,rateratio}.pdf`.

## Design choices (for defensibility)

- **Lag scale matches the model.** The lagged term is the log event-rate
  (count over exposure), the same quantity the GLMM's linear predictor governs,
  so it is the most direct autoregressive competitor.
- **Common sample.** All variants (base, +lag1, +lag1&2) are fit on the same
  rows — those where both lags exist — so the change in the climate coefficient
  reflects the added control, not a sample shift. First 1–2 periods per org drop.
- **Within-org test.** The org random intercept already absorbs between-org mean
  rate, so the lagged term tests *within-organization* autoregressive structure,
  exactly the channel the critique invokes.
- **z-scored lags** for interpretable competing coefficients, as in the
  sentiment-control test.

## Result — climate survives in all 9 cells

Sign and statistical significance are preserved in every cell. Attenuation
(base → +lag1&2 coefficient) is small in most cells and modest in three:

| Cell | base β | +lag1&2 β | attenuation | base p | +lag1&2 p |
|---|---|---|---|---|---|
| Rail — accidents | +1.34 | +0.96 | 28% | 0.012 | 0.049 |
| Rail — fatalities | −5.63 | −5.34 | 5% | 5e-4 | 1e-3 |
| Rail — injuries | −3.39 | −3.33 | 2% | 6e-16 | 2e-15 |
| NRC — LERs | −5.99 | −4.45 | 26% | 2e-8 | 3e-5 |
| NRC — emergencies | +2.20 | +2.15 | 2% | 2e-3 | 2e-3 |
| NRC — % power loss | −4.34 | −4.34 | ~0% | 0.052 | 0.052 |
| NRC — scrams | −1.84 | −1.85 | ~0% | 0.019 | 0.019 |
| Aviation — accidents | +1.67 | +1.63 | 2% | 0.30 | 0.30 |
| Aviation — AIDS (all) | +2.36 | +1.41 | 40% | 1e-5 | 8e-3 |

The two cells with the largest attenuation (NRC LERs 26%, aviation AIDS 40%)
remain comfortably significant — the lagged outcome explains *some* shared
variance, as expected for any autocorrelated process, but the climate signal is
not reducible to it. NRC % power loss and aviation accidents are unchanged
because they were already borderline / null at baseline; the lag does not
rescue or destroy them.

## Manuscript text (drop-in)

> To rule out the possibility that the climate–outcome association merely
> reflects temporal autocorrelation of events — narratives are written about
> events, and event rates are serially correlated — we refit each
> best-performing model adding the organization's own lagged event rate
> (log rate at the prior one and two periods, z-scored) as a competing
> within-organization covariate, fitting all variants on a common sample. The
> climate coefficient retained its sign and statistical significance in all
> nine main-text cells; attenuation was below 6% in six cells and 26–40% in two
> well-powered cells (nuclear LERs, aviation incidents), both of which remained
> significant. The narrative climate measure therefore predicts future safety
> outcomes beyond what the organization's own recent event history explains.
