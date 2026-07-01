# Main-text draft — Multiverse support for the narrative climate measure

Drop-in prose for the Results (and a Discussion fragment), adapted to the actual
tier assignments. Callouts: **[Figure 4]** = `figure4_multiverse_support.pdf`
(summarized Panels A+B); **[Table 2]** = `table2_multiverse_support_main.csv`
(compact tiered table; full version in Extended Data,
`table2_multiverse_support_full.csv`). Per-SD rate ratios (RR) are for a +1 SD
increase in the narrative climate indicator; RR < 1 = fewer events (protective
direction), RR > 1 = more events.

---

## Results — Grading the strength of evidence across the multiverse

The best-performing models provide interpretable estimates of the strongest
climate signal within each industry–outcome cell, but they do not by themselves
establish that the signal is robust to analytical choice. We therefore
interpreted these estimates alongside the full multiverse of 810 defensible
specifications per cell, focusing on three features: the proportion of
specifications in which adding climate improved held-out prediction over the
seasonal-plus-operational baseline (% Δlog-lik > 0), the directional consistency
of those predictive specifications (the percentage whose coefficient shared the
sign of the best-performing model), and whether the association held under
organization-blocked as well as time-series cross-validation **[Figure 4;
Table 2]**.

This evidence distinguished three levels of support. **Tier 1 — directionally
coherent and broadly multiverse-supported.** For rail injuries
(RR 0.80, 95% CI 0.75–0.84) and nuclear licensee event reports
(RR 0.92, 0.89–0.95), the best-performing estimate was substantively meaningful,
climate improved held-out prediction across a large or moderate share of
specifications (62% and 32%), every predictive specification agreed in sign with
the best model (100% same-sign), and the protective direction was reproduced
under organization-blocked cross-validation. We treat these as the strongest
evidence that narrative-derived climate carries stable predictive information.

**Tier 2 — directionally coherent but specification-sensitive.** Rail fatalities
(RR 0.90, 0.85–0.96), nuclear emergency declarations (RR 1.20, 1.07–1.34),
nuclear scrams (RR 0.93, 0.88–0.99), and FAA AIDS all-events
(RR 1.08, 1.04–1.11) showed theoretically interpretable best-performing
estimates and high directional consistency among predictive specifications
(76–85% same-sign), but predictive improvement was concentrated in a narrower
region of the specification space. These should be read as specification-
sensitive evidence: the signal appears real, but its detection depends more
strongly on measurement choices.

**Tier 3 — weak, ambiguous, or unstable.** Nuclear percent-power-loss
(RR 0.94, 0.88–1.00), NTSB aviation accidents (RR 1.04, 0.96–1.12), and rail
accidents formed boundary-condition cases. The first two had best-performing
estimates that were small and not statistically distinguishable from no effect,
with predictive gain in only ~20% of specifications. Rail accidents was the
single cell whose direction depended on the validation scheme — its predictive
specifications were sign-split (only 26% agreed with the best-performing model)
and the effect reversed between time-series and organization-blocked
cross-validation. We do not interpret these cells as evidence for or against the
climate measure; they mark where sparse events, outcome heterogeneity, or
measurement-channel limits weaken inference.

We therefore treat the multiverse not as a binary robustness check, but as a way
to grade the strength and portability of evidence for each outcome.

## Results — Robustness to alternative explanations

The Tier 1–2 associations were robust to the principal alternative explanations.
Climate retained its sign and significance when controlling for whole-narrative
sentiment and length (it is not a negativity or verbosity detector), for the
organization's own lagged event rate (not simple outcome autocorrelation), and
for organization-level reporting rates, and was insensitive to event-inclusion
and zero-score windowing conventions (Extended Data; Supplementary Results
S1–S6).

## Discussion fragment — Cross-validation divergence as a boundary condition

We interpreted time-series and organization-blocked cross-validation as
complementary tests rather than interchangeable robustness checks. Time-series
validation asks whether an organization's prior narrative climate improves
prediction of its own future outcomes — the leading-indicator use case;
organization-blocked validation asks whether the same relationship transfers
across organizational units. Divergence between them, including the rail-accident
sign reversal, is therefore better read not as simple instability but as evidence
that a narrative-derived climate indicator may validly track temporal change
within a reporting system even when the same score is not fully portable across
organizations. Cross-organization portability requires stronger assumptions about
measurement equivalence in narrative style, reporting thresholds, event
classification, and local risk context. Where validation strategies diverged, we
interpret the indicator as more appropriate for within-organization monitoring
than for ranking organizations against one another.

---

## Notes for integration

- **Direction labels.** Tier 2/3 cells with RR > 1 (nuclear emergency
  declarations, FAA AIDS) point in the "more events with higher climate score"
  direction. If the climate-score polarity and the NRC reporting-culture
  interpretation (better climate → more candid reporting) are stated earlier,
  frame these as visibility/reporting-consistent rather than as harm; otherwise
  describe direction neutrally and defer mechanism to the Discussion.
- **Numbers** are pulled from `table2_multiverse_support_full.csv` (time-series
  best-performing, per-SD). Regenerate via `12_multiverse_support.R` if the
  panel data state changes; re-sync these figures before submission.
- **Tier cutoffs** (state in Methods or Table 2 footnote): Tier 3 if % same-sign
  < 50, or a significant CV sign-flip, or (best RR non-significant and
  % Δll > 0 < 30); Tier 1 if best RR significant and % same-sign ≥ 90 and
  % Δll > 0 ≥ 30 and organization-blocked direction concordant; Tier 2 otherwise.
- **Outcome-class column** in Table 2 (Harm / Operational disruption /
  Reporting-classification-sensitive / Mixed-sparse) — confirm the
  NRC-emergency-declarations class label (currently "Operational disruption").
