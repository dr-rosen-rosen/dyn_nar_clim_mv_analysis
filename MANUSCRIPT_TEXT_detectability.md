# Manuscript text — detectability/consequence-ladder restructure

Draft prose to integrate into Methods, Results, and figure/table captions for the
restructured analysis (mining DEGREE_INJURY detectability tiers; aviation AIDS↔NTSB
consequence ladder at airport-quarter grain). Matches the existing Methods voice.
Square-bracketed notes flag spots to confirm against final numbers.

---

## METHODS — additions

### Outcome detectability tiering (new subsection, before the per-industry outcomes)

A central prediction of the reporting-filter account is that an organization's
safety climate should move *recorded* outcome counts most at the low-detectability
(discretionary) end of the severity range and least — or in the opposite direction —
at the high-detectability end, where capture is near-complete regardless of climate.
To test this directly rather than infer it across coarse outcomes, we decomposed each
industry's outcomes into an ordered **detectability/consequence ladder** and estimated
the climate coefficient separately for each rung on a common exposure denominator.
The estimand is therefore not a single coefficient but the ordered set of climate
coefficients across rungs, and specifically the presence of a sign change between the
no-/low-harm and high-harm ends.

*Mining outcomes.* MSHA Part 50 records assign every reported event a degree-of-injury
code, which we mapped to an ordered severity ladder: **no-injury reports**
(accident-only, code 00 — property/operational events with no injury), **minor injury**
(restricted-activity-only or medical-treatment-only, codes 05/06/10), **lost-time
injury** (days away from work, codes 03/04), and **severe injury** (permanent disability
or fatality, codes 01/02). We additionally modeled **lost workdays** (the summed
days-lost burden) as a continuous severity measure. Occupational illness (code 07),
non-occupational (08), and non-employee (09) events were excluded as governed by a
different reporting filter or population. Each rung was modeled as a mine-quarter rate
(negative binomial, log link) with an offset of log(employee-hours worked), an identical
right-hand side across rungs (safety climate, within-mine operational volatility,
centered year-quarter and harmonic seasonal terms), and a mine random intercept; lost
workdays used a Tweedie rate model. Modeling tiers as distinct outcomes within the same
multiverse contract keeps their climate coefficients directly comparable.

*Aviation outcomes.* Aviation has no single census with a within-record severity ladder;
the ladder is therefore cross-regime. Because the climate signal is derived from
voluntary, confidential ATC narratives (ASRS) while outcomes are filed by separate
agents under separate regimes, the within-corpus circularity that would compromise a
reporting-filter test on a single corpus does not arise. Two complementary axes were
used. On the **AIDS** side (FAA incidents/events below the NTSB accident threshold), we
separated **no-harm events** (no injury and no substantial aircraft damage — the
discretionary, detection-dependent reporting tier) from **property-damage events** (no
injury but substantial or destroyed aircraft — a higher-detectability, no-casualty
tier), using the per-event damage code. On the **NTSB** side (investigator-confirmed
accidents), the canonical casualty measure was the count of **serious/fatal accidents**
(highest injury serious or fatal). Each was modeled as an airport-quarter rate (negative
binomial, log link, offset = log(tower+TRACON operations), airport random intercept).

### Time grain (amend the windowing/aggregation paragraph)

Mining and nuclear outcomes were modeled at the facility-quarter grain. Aviation
outcomes were likewise aggregated to the **airport-quarter** grain (rather than
airport-month) to align with the nuclear cadence and to reduce the extreme
zero-inflation of the sparse casualty tiers while preserving dynamic (sub-annual)
resolution; climate scores, which use event-count and time-decay windows, are invariant
to this choice of evaluation grain. [Primary aviation spec: local+terminal ATC reporter
scope, events within 5 NM, airport-quarter. Robustness to airport-month grain and
tower-only scope is reported in Extended Data Table 4.]

---

## RESULTS — additions

### Within-industry detectability gradient (mining)

Decomposing the mining outcome onto its detectability ladder revealed the predicted
sign change. Better safety climate was associated with **more** recorded no-injury
reports (RR ≈ 1.20 per SD [CONFIRM]) — the discretionary, zero-harm reporting tier — and
with progressively weaker or reversed associations as severity rose: minor and lost-time
injuries showed small positive associations, while **severe injury** (fatal/permanent
disability) and **lost-workday burden** were protective (RR ≈ 0.94 [CONFIRM]). Because a
no-injury event carries no harm by construction, a positive climate coefficient there
cannot reflect risk reduction and can only reflect more complete surfacing of
discretionary reports — an identification that the within-injury rungs cannot provide.
This resolves the apparently paradoxical coarse-outcome pattern (recorded injuries rising
but fatalities falling with better climate): decomposed, the positive signal lives at the
low-severity, discretionary end and the protective signal at the severe end.

A second, finer feature reinforces the same mechanism *within* the lost-time class:
better climate was associated with a higher **incidence** of lost-time injuries
(RR ≈ 1.05) but a lower total lost-workday **burden** (RR ≈ 0.94), implying roughly
[~10%] fewer lost days per lost-time event per SD. That is, the additional lost-time
events surfaced under a stronger reporting climate are disproportionately the milder,
short-absence cases, even as the severe long-duration tail contracts — the injury-pyramid
signature of a healthy reporting culture (catch more small events; prevent the large
ones).

### Cross-industry contrast (coupled vs. decoupled reporting agent)

The reporting-filter signal was strongest where the organization that holds the safety
climate also makes the recording decision (mining), and absent where those agents are
decoupled (aviation). In mining, the no-injury surfacing tier was not only positive but
the most robustly supported cell in the study, improving held-out forward-in-time
prediction over a seasonal-plus-operational baseline in 100% of specifications under both
cross-validation strategies — establishing the surfacing as a real, predictive signal
rather than a secular reporting-drift artifact. In aviation, where ATC controllers' climate
is decoupled from the FAA/NTSB agents who file the outcomes, the discretionary no-harm tier
did not surface (it was protective-leaning/null in-sample and did not generalize
out-of-sample at either grain or reporter scope; Extended Data Table 4), while the
casualty-harm tiers showed weak protective associations. The contrast is itself evidence:
the filter operates through the recording organization's own discretion.

### Sensitivity to narrative sentiment and length (robustness)

Because the climate score and the candidate confounders are all derived from the same
narratives, we refit each champion model adding whole-narrative sentiment (VADER) and
narrative length as standardized covariates, singly and jointly. Under the fully-adjusted
model (both controls), the climate–outcome associations were essentially unchanged: every
main cell retained its direction and significance except NRC scrams, which moved just below
the significance threshold (per-SD RR 0.93→0.94), and the headline detectability cells —
including the mining no-injury surfacing tier (RR 1.20→1.19) — were unaffected. One cell,
rail accidents, illustrates a classic suppression pattern that the per-variant forest
(Extended Data) makes visible: the association is significant at baseline (RR 1.02), is
absorbed to null when sentiment alone is added (RR 0.99), and re-emerges — slightly
strengthened — when length is added alongside sentiment (RR 1.04). This is expected
collinearity among co-derived narrative features (sentiment partially proxies the climate
signal until length is also held constant); the fully-adjusted estimate, not the
single-control intermediate, is the appropriate reference. [Sentiment-/length-control
forest: Extended Data; lagged-outcome and minimum-reports robustness: Extended Data.]

---

## CAPTIONS — updates

**Figure 1 (forest).** Per-standard-deviation climate rate ratios (95% CI) across 15
outcome cells in four industries, ordered within each industry from
low-severity/discretionary (top) to high-severity/harm (bottom). Mining and aviation
outcomes are decomposed onto detectability/consequence ladders (Methods); rail and
nuclear retain their established outcomes. RR > 1 indicates more recorded events per unit
exposure with better climate (surfacing); RR < 1 indicates fewer (harm reduction).

**Figure 2 (partial dependence).** Predicted outcome rate vs. climate percentile for the
champion specification of each of the 15 main cells.

**Figure 3 (specification curves).** Held-out Δlog-likelihood (climate vs.
seasonal+operational baseline) across all specifications, per cell, time-series CV.

**Figure 4 (multiverse support).** Breadth (% specifications with held-out gain) and
directional coherence per cell, colored by support tier.

**Table 1.** Data sources, outcomes, and post-filter panel composition. Aviation panel is
airport-quarter; mining is mine-quarter on the detectability ladder.

**Table 2 (multiverse support).** Per-SD rate ratio, predictive breadth, directional
coherence, and support tier for each main outcome. The mining no-injury (surfacing) tier
is the sole Tier-1 reporting-sensitive cell; lost-workday burden and the aviation casualty
tiers are Tier 2 (harm); the within-injury tiers and aviation no-harm are Tier 3.

**Extended Data Tables 3–4 (robustness).** ED Table 3: the pre-decomposition coarse mining
outcomes and finer aviation tiers, confirming consistency with the main-text ladder. ED
Table 4: aviation conclusions under the alternative airport-month grain and tower-only ATC
reporter scope.
