# Multiverse Support Framework for Dynamic Safety Climate Manuscript

## Purpose

This note defines a three-tier framework for interpreting multiverse support in the manuscript and provides concrete recommendations for revising Table 2 and Figure 4. The goal is to distinguish clearly between:

1. the magnitude and direction of the best-performing model estimate;  
2. the extent to which the climate signal generalizes across the full specification space; and  
3. whether cross-validation strategy differences reflect limits of portability rather than simple failure of the measure.

The core principle is that best-performing models should be treated as interpretable summaries of the strongest signal within each industry–outcome cell, while robustness claims should be based on the full multiverse distribution.

---

## Recommended Three Levels of Multiverse Support

### Tier 1: Directionally Coherent and Multiverse-Supported

**Definition:**  
The best-performing model shows a substantively meaningful association, and the broader multiverse provides convergent evidence that climate improves prediction and points in the same substantive direction.

**Typical characteristics:**

- Best-performing rate ratio is substantively meaningful.
- A relatively large share of specifications show positive held-out gain.
- Median Δlog-likelihood is near zero or positive, rather than strongly negative.
- Among specifications with positive Δlog-likelihood, coefficient signs are mostly consistent with the best-performing estimate.
- Time-series and organization-blocked cross-validation are directionally similar, or any divergence is explainable and limited.

**Interpretation:**  
These cells provide the strongest evidence that the narrative-derived climate indicator carries robust predictive information about the outcome.

**Suggested manuscript language:**

> For these outcomes, the best-performing model estimate was supported by the broader multiverse: climate improved held-out prediction across a substantial portion of defensible specifications, and predictive specifications were directionally coherent. We therefore interpret these cells as the strongest evidence that narrative-derived climate carries stable predictive information.

**Likely examples:**  
Rail injuries may be the clearest Tier 1 case. Nuclear scrams may also fit depending on final sign-consistency and median Δlog-likelihood summaries.

---

### Tier 2: Directionally Coherent but Specification-Sensitive

**Definition:**  
The best-performing model shows a theoretically interpretable association, and the broader multiverse is directionally consistent among the specifications that perform well, but support is concentrated in a narrower region of the specification space.

**Typical characteristics:**

- Best-performing rate ratio is interpretable and consistent with the harm-versus-visibility framework.
- Some specifications show positive held-out gain, but the proportion is moderate or modest.
- Median Δlog-likelihood may be near zero or negative.
- Sign consistency among positive-gain specifications is relatively high.
- Results may depend on specific temporal windows, sentiment models, or aggregation approaches.

**Interpretation:**  
These results support the proposed interpretation but should be presented as more conditional. The climate signal appears real or plausible, but its detection depends more strongly on measurement choices.

**Suggested manuscript language:**

> Several cells showed theoretically coherent but more concentrated support. In these cases, the best-performing models aligned with the expected harm or visibility interpretation, and positive-gain specifications tended to point in the same direction, but predictive improvement was not broadly distributed across the full specification space. These findings should therefore be interpreted as specification-sensitive evidence rather than uniformly robust validation.

**Likely examples:**  
Rail fatalities, nuclear Licensee Event Reports, nuclear emergency declarations, nuclear percentage power loss, and FAA AIDS all-events may fall here, depending on final multiverse summaries.

---

### Tier 3: Ambiguous, Weak, or Unstable

**Definition:**  
The best-performing estimate is small, imprecise, sign-changing, or weakly supported by the multiverse. Cross-validation strategies may diverge sharply, and positive-gain specifications may lack directional consistency.

**Typical characteristics:**

- Best-performing rate ratio is close to 1.0, imprecise, or not substantively meaningful.
- Low proportion of specifications show positive held-out gain.
- Median Δlog-likelihood is negative or near zero.
- Sign consistency is weak or split.
- Time-series and organization-blocked cross-validation produce sign reversals or materially different conclusions.
- Outcome is sparse, heterogeneous, or strongly shaped by reporting/classification thresholds.

**Interpretation:**  
These cells should not be used as core evidence for or against the climate measure. They are better treated as boundary-condition cases showing where outcome heterogeneity, sparse events, or measurement-channel limitations weaken inference.

**Suggested manuscript language:**

> Other cells provided weak or ambiguous evidence. In these cases, the best-performing estimates were small, unstable, or poorly supported across the multiverse, and cross-validation strategies sometimes diverged. We therefore interpret these outcomes as boundary-condition cases rather than as strong evidence for a climate–outcome association.

**Likely examples:**  
Rail accidents and NTSB aviation accidents likely belong here.

---

## Recommended Table 2 Revision

### Current Role of Table 2

The current table summarizes best-performing specification estimates and multiverse support. This is useful, but it risks making the best-performing model look like the primary result. The revised version should make clear that best-performing estimates and multiverse evidence answer different questions.

### Recommended Table 2 Title

**Table 2. Best-performing effect estimates and multiverse support across primary industry–outcome cells.**

### Recommended Table 2 Caption

> For each primary industry × outcome cell, the table reports the best-performing time-series specification as an interpretable summary of the strongest detected signal, alongside summaries of support across the full multiverse of defensible specifications. Rate ratios are scaled to a one-standard-deviation increase in the narrative-derived safety-climate indicator. Positive Δlog-likelihood indicates improved held-out prediction relative to the seasonal-plus-operational baseline. Robustness should be interpreted from the full multiverse summaries rather than from the best-performing estimate alone.

### Recommended Columns

| Column | Purpose |
|---|---|
| Industry | Rail, nuclear, aviation |
| Outcome | Outcome name |
| Outcome class | Harm, operational disruption, reporting/classification-sensitive, mixed/sparse |
| Best RR | Rate ratio for a one-SD increase in climate from the best-performing time-series specification |
| Best RR 95% CI | Confidence interval for the best-performing specification |
| Best Δll | Highest held-out Δlog-likelihood achieved in the cell |
| % Δll > 0 | Percentage of specifications where adding climate improved held-out prediction |
| Median Δll | Median held-out gain across all specifications |
| % same sign among Δll > 0 | Among positive-gain specifications, percentage with the same coefficient sign as the best-performing model |
| Organization-blocked direction | Direction under organization-blocked CV: protective, positive, null/unstable, or sign-flip |
| Support tier | Tier 1, Tier 2, or Tier 3 |
| Interpretation | Very brief label, such as “strong protective,” “concentrated protective,” “visibility/reporting,” or “ambiguous” |

### Why Add “% Same Sign Among Δll > 0”?

This is one of the most useful additions. The proportion of specifications with Δll > 0 tells whether climate often improves prediction. It does not tell whether the predictive specifications point in the same substantive direction.

For example:

- 30% Δll > 0 and 95% same-sign support means the evidence is concentrated but directionally coherent.
- 30% Δll > 0 and 50% same-sign support means the result is unstable or ambiguous.
- 65% Δll > 0 and 90% same-sign support means strong multiverse support.

This metric helps separate “sparse but coherent” support from “genuinely unstable” support.

### Optional Simplified Version for Main Text

If the table becomes too wide for the main manuscript, use a compressed main-text table and move the full table to Extended Data.

Main-text columns could be:

| Industry | Outcome | Outcome class | Best RR | % Δll > 0 | % same sign among Δll > 0 | Support tier |

The full version, including CIs, median Δll, best Δll, and organization-blocked CV, can be placed in Extended Data.

---

## Recommended Figure 4 Revision

### Current Figure 4 Concept

The current figure is planned as specification curves showing Δlog-likelihood across all specifications in each primary industry × outcome cell. This is defensible but may be visually dense for a broad audience.

### Recommended Main-Text Figure 4

Use Figure 4 to answer two simple questions:

1. How often does climate improve held-out prediction across the specification space?
2. When climate improves prediction, does it point in the same direction as the best-performing model?

### Recommended Figure 4 Layout

**Figure 4. Multiverse support for narrative-derived safety climate across outcomes.**

#### Panel A: Proportion of specifications with positive held-out gain

- X-axis: primary outcomes, grouped by industry or outcome class.
- Y-axis: percentage of specifications with Δlog-likelihood > 0.
- Horizontal reference line at 50%.
- Points or bars for each outcome.
- Optional visual encoding by outcome class.

**Interpretation:**  
Shows how broadly predictive value is distributed across the multiverse.

#### Panel B: Directional consistency among predictive specifications

- X-axis: same outcomes in the same order as Panel A.
- Y-axis: percentage of positive-gain specifications with the same coefficient sign as the best-performing model.
- Horizontal reference line at 75% or 80%, if using a heuristic for directional coherence.
- Optional labels for sign direction: protective or positive/visibility.

**Interpretation:**  
Shows whether the specifications that improve prediction tell a coherent substantive story.

#### Panel C: Optional median Δlog-likelihood

If space allows, add a third panel:

- X-axis: outcomes.
- Y-axis: median Δlog-likelihood across all specifications.
- Horizontal reference line at 0.

**Interpretation:**  
Shows whether the typical specification helps, rather than only whether some region of the multiverse helps.

### Recommended Figure 4 Caption

> Multiverse support for the narrative-derived safety-climate indicator across primary industry–outcome cells. Panel A shows the percentage of defensible specifications in which adding the climate indicator improved held-out prediction relative to the seasonal-plus-operational baseline. Panel B shows directional consistency among predictive specifications, defined as the percentage of positive-gain specifications with the same coefficient sign as the best-performing time-series model. Panel C, if included, shows the median Δlog-likelihood across all specifications. Together, these summaries distinguish broadly supported effects from concentrated but directionally coherent signals and from weak or unstable cells.

---

## Alternative Figure 4: Specification Curves

If retaining specification curves in the main text, make them as interpretable as possible.

### Recommended Specification Curve Design

For each outcome:

- Plot all specifications ordered by Δlog-likelihood.
- Y-axis: Δlog-likelihood.
- X-axis: ordered specifications.
- Horizontal line at Δlog-likelihood = 0.
- Color points or segments by coefficient sign:
  - protective sign;
  - positive/visibility sign;
  - null or unstable.
- Mark the best-performing specification with a larger point.
- Add a small annotation showing:
  - % Δll > 0;
  - % same sign among Δll > 0;
  - support tier.

### Advantage

This preserves transparency and shows the full multiverse distribution.

### Disadvantage

It may be visually dense and less immediately interpretable for a broad journal audience. For Nature Human Behaviour, the summarized dot/bar approach may be stronger in the main text, with full specification curves in Extended Data.

---

## Recommended Main-Text Language for Multiverse Support

### Opening paragraph for multiverse section

> The best-performing models provide interpretable estimates of the strongest climate signal within each industry–outcome cell, but they do not by themselves establish that the signal is robust to analytical choice. We therefore interpreted these estimates alongside the full multiverse distribution, focusing on three features: the proportion of specifications in which climate improved held-out prediction, the median predictive gain across specifications, and the directional consistency of coefficients among specifications with positive gain.

### Tiered interpretation paragraph

> This evidence distinguished three levels of support. Some cells showed directionally coherent and multiverse-supported associations, in which the best-performing estimate was substantively meaningful and predictive specifications were broadly aligned. Other cells showed directionally coherent but specification-sensitive associations: the best-performing estimate was theoretically interpretable, but predictive gains were concentrated in narrower regions of the specification space. Finally, some cells were weak or ambiguous, with small effects, limited predictive gain, or sign reversals across validation strategies. We therefore treat the multiverse not as a binary robustness check, but as a way to grade the strength and portability of evidence for each outcome.

### Cross-validation strategy paragraph

> Time-series and organization-blocked cross-validation were interpreted as complementary tests rather than interchangeable robustness checks. Time-series validation asked whether an organization’s prior narrative climate improved prediction of its own future outcomes, aligning with the leading-indicator use case. Organization-blocked validation asked whether the same climate–outcome relationship transferred across organizational units. Divergence between these strategies, including sign reversals, was therefore interpreted as evidence that some climate signals are more stable as within-organization temporal indicators than as cross-organization comparative measures.

---

## Recommended Discussion Language on Sign Flips

> Divergence between time-series and organization-blocked validation reveals an important boundary condition for text-derived organizational measurement. A narrative-derived climate indicator may validly track temporal change within a reporting system even when the same score is not fully portable across organizations. Cross-organization portability requires stronger assumptions about measurement equivalence in narrative style, reporting thresholds, event classification, and local risk context. Where validation strategies diverged, we therefore interpret the result not simply as instability, but as evidence that the indicator is more appropriate for within-organization monitoring than for ranking organizations against one another.

---

## Placement Recommendations

### Main Results

Include:

- Best rate-ratio findings.
- Brief robustness paragraph controlling for sentiment, length, lagged outcome rate, reporting rate, inclusion rules, and zero-score conventions.
- Multiverse support tiers.
- Short explanation of CV-strategy divergence.

### Figure 4

Use a summarized multiverse-support figure in the main text.

### Extended Data

Include:

- Full specification curves for all cells.
- Organization-blocked CV results.
- Sign-consistency details.
- Sensitivity analyses S1–S6.
- Aviation-specific limitations and subgroup notes, if not fully addressed in the main text.

### Discussion

Return to the broader implication:

> Text-derived organizational measures depend on alignment among construct, reporting channel, organizational unit, and outcome. The multiverse framework helps identify where that alignment is strong, where the signal is detectable but specification-sensitive, and where inference is limited by sparse outcomes or incomplete measurement channels.

---

## Notes on Aviation-Specific Interpretation

The aviation analyses should be treated cautiously because the climate signal is derived from local airport and air-traffic-control reports rather than from the full aviation system. ATC is central to airport-proximal safety, but the available narrative channel excludes aircrew, air-carrier, maintenance, dispatch, and operator-level perspectives. This makes the aviation climate indicator a partial measure of airport-proximal safety discourse rather than a comprehensive measure of aviation safety climate.

Suggested limitation language:

> The aviation analyses have an additional measurement limitation. To align ASRS narratives with airport-month outcomes, the climate indicator was derived from reports associated with local airport and terminal air-traffic-control functions. This restriction improved unit alignment but excluded aircrew, air-carrier, maintenance, dispatch, and broader operator perspectives that are central to aviation safety climate. As a result, the aviation indicator should be interpreted as a partial measure of airport-proximal safety discourse rather than a comprehensive measure of aviation organizational climate. This narrower and more fragmented measurement channel likely contributed to the weaker and less stable aviation results relative to rail and nuclear power, where reporting units and outcome units were more directly aligned.

Broader conceptual implication:

> The aviation case illustrates a general boundary condition: unobtrusive organizational measurement is strongest when the available text channel corresponds closely to the organizational level and construct being inferred. When narratives capture only one role group within a distributed system, the resulting indicator may still carry useful signal, but it should not be interpreted as the climate of the entire system.
