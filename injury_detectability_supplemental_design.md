# Detectability / Reporting-Filter Supplemental Analysis — Design Spec (v0.2)

**Status:** design only; build deferred until after anchor-paper submission.
**Scope:** Primary build on MSHA Part 50 (coal + M/NM), acute injuries. §9 ports the design to aviation (ASRS climate → AIDS/NTSB outcomes). Rail and nuclear analogs sketched at §10. Occupational illness handled separately.
**Purpose:** Test whether the dynclim climate signal's *positive* association with low-severity counts is the fingerprint of a severity-graded reporting filter rather than a true risk increase — by estimating the climate coefficient separately across detectability tiers and checking for the predicted sign gradient. Each industry tests a different facet of the same mechanism (see §10 cross-industry logic).

---

## 1. Motivation and the estimand

The main coal results show better climate → fewer fatalities and lower severity, more total accidents, **and** higher total injuries. The injury arm is the contested one. The reporting-filter hypothesis (Azaroff et al. 2002 conceptual filters; Probst et al. 2008; Morantz 2013, 2014) predicts that an event's probability of entering the regulatory record rises with its *detectability* (severity), and that organizational factors like climate move the count most at the low-detectability end. Under that hypothesis the climate coefficient on injury counts should vary monotonically by tier:

- **High-detectability tier (fatalities, permanent disability):** coefficient reflects *true risk* → protective (negative). Near-complete capture, climate-independent reporting.
- **Low-detectability tier (no-lost-time, first-aid):** coefficient dominated by *reporting/surfacing* → positive. Discretionary capture, climate-sensitive reporting.
- **Lost-time middle tier:** intermediate.

The estimand is therefore not a single coefficient but the **ordered set of climate coefficients across tiers**, and specifically the presence of a *sign flip* (not mere attenuation) between the high- and low-detectability tiers.

This is the same structure as Morantz (2014), with the exposure swapped: she drives underreporting with audit intensity, we drive it with narrative-derived climate. Her detectability typology (built with M. Cullen) is the citable basis for the tier mapping.

---

## 2. DEGREE_INJURY → detectability tier mapping

Authoritative codes from `Accidents_Definition_File.txt` (field `DEGREEINJURYCD`):

| Code | Description | Tier | Rationale |
|------|-------------|------|-----------|
| 01 | Fatality | **T1 high** | Impossible to conceal; ~100% capture |
| 02 | Permanent total / permanent partial disability | **T1 high** | Medical + comp + legal trail; very high capture |
| 03 | Days away from work only | **T2 lost-time** | Visible via absence/comp; moderate capture |
| 04 | Days away + restricted activity | **T2 lost-time** | As above |
| 05 | Days restricted activity only | **T3 low** | Light-duty; no absence; discretionary |
| 06 | No days away, no restrictions | **T3 low** | Medical-treatment-only; highly discretionary |
| 10 | All other cases (incl. first aid) | **T3 low** | Maximally discretionary minor |
| 00 | Accident only (no injury) | **T0 (optional)** | Property-damage / no-injury; pure reporting signal |
| 07 | Occupational illness not degree 1–6 | **EXCLUDE** | Latent illness; different filter (long latency, dust disease); not an acute-severity rank |
| 08 | Injuries due to natural causes | **EXCLUDE** | Non-occupational etiology |
| 09 | Injuries involving non-employees | **EXCLUDE** | Not in the employee-hours exposure denominator; population mismatch |

**Mapping notes**
- T0 (code 00, accident-only) is the lowest-detectability anchor and connects this analysis to the accident-count outcome. Treat its inclusion as a multiverse switch, not a default.
- The T2 "lost-time" band corresponds to the National Research Council (1982) "intermediate" injuries conjectured to be least underreported — useful as a principled middle reference rather than an arbitrary cut.
- 07 (illness) is a candidate for a *separate* latent-illness analysis (pneumoconiosis dynamics in coal), explicitly out of scope here.

---

## 3. Three estimators (primary + two corroborating)

### 3.1 Primary — climate coefficient by tier (count models)
For each tier `t ∈ {T1, T2, T3}` (and optionally T0), fit a facility-quarter count model:

```
count_t ~ climate + layer_features + ops_controls + size/commodity/method controls
          + (1 | facility) + offset(log(employee_hours))
```

- Same RHS across tiers so coefficients are comparable.
- Estimand: the vector (β_climate^T1, β_climate^T2, β_climate^T3). Hypothesis = monotone increasing, with β^T1 ≤ 0 < β^T3.
- This is **one model per spec**, consistent with the MV runner contract. Each tier is a distinct outcome — i.e. tiers enter the existing **outcome dimension**, exactly like the NRC outcome set (`emerg_binary`, `pct_power_loss`, `ordinal_scram`).

### 3.2 Corroborating — detectability share (fractional outcome)
Per facility-quarter, define the **high-detectability share**:

```
det_share = (T1 + T2 cases) / (T1 + T2 + T3 cases)
```

Fit as a beta-family (or beta-binomial) GLMM:
```
det_share ~ climate + controls + (1 | facility)     # glmmTMB family = beta_family / betabinomial
```
- Hypothesis: **β_climate < 0** — better climate dilutes the severe-among-reported share by surfacing minor injuries into the denominator. This is Morantz's establishment-level "red-flag" covariate, repurposed as the single cleanest summary statistic.

### 3.3 Corroborating — severity-mix composition (ordinal, conditional on a report)
```
clmm(tier_ordinal ~ climate + controls + (1 | facility))   # tier_ordinal: T3 < T2 < T1
```
- Conditions on an injury being filed; asks whether climate shifts the *mix* toward less-severe.
- Hypothesis: better climate → proportional-odds shift toward T3.
- Note the selection caveat: conditions on report existence, so it answers a composition question, not a rate question. Secondary to 3.1.

---

## 4. Sign normalization (robustness summary integration)

This analysis spans count, beta, and ordinal families, so it inherits the existing cross-model sign-interpretation problem. Define a single **"surfacing direction"** so the robustness summary stays coherent:

- Count tiers (3.1): positive β_climate = surfacing (more minor reported). Report raw and a normalized `surfacing_sign = +1 * sign(β)` for T3, `-1 * sign(β)` for T1 (so "surfacing-consistent" is positive in both).
- det_share (3.2): negative β_climate = surfacing → flip sign in the normalized column.
- ordinal (3.3): shift toward T3 = surfacing → orient cumulative-odds so positive = toward minor.

Add a `surfacing_consistent` boolean to the robustness summary per spec; the headline result is the *fraction of specs across the multiverse showing the full T1≤0<T3 gradient*.

---

## 5. Multiverse dimensions (additions specific to this analysis)

| Dimension | Levels | Notes |
|-----------|--------|-------|
| `detectability_tiering` | 3-tier (default) / 2-tier traumatic-vs-minor (Morantz 2013 style) / Morantz-2014 detectability typology | 2-tier collapses T1+T2 vs T3 for a robustness anchor |
| `t0_inclusion` | exclude (default) / include accident-only (00) | Tests sensitivity to the pure no-injury signal |
| `illness_handling` | exclude 07 (default) / 07 as separate illness outcome | Never folds 07 into the acute ladder |
| `model_family` | poisson / negbin / hurdle (glmmTMB ziformula) | Reuse existing family dimension |
| `time_grain` | quarter (default) / year | Year-grain mitigates T1 sparsity (see §6) |
| `baseline_strategy` | `best_non_climate` (recommended) | Carry the NRC lesson: naive baselines can underperform on rare mining outcomes |

Layer features (temporal, EWS) remain configurable as in the shared `common/` feature-layer system.

---

## 6. Known constraints / honest caveats

1. **T1 sparsity.** Fatalities + permanent disability per facility-quarter are mostly 0; a count model is unstable. Two mitigations as multiverse levels: (a) `time_grain = year`; (b) recast T1 as **binary** (`any_severe`) via glmer logistic — paralleling the NRC `emerg_binary` choice. The robustness summary must not compare a logistic T1 coefficient to a count T3 coefficient on the raw scale; use the normalized surfacing direction (§4) and standardized/marginal effects, not raw βs.

2. **Identification is consistency, not proof.** A monotone gradient is *consistent with* a reporting filter but also with a world where good climate genuinely prevents catastrophic events more than minor ones. The load-bearing evidence is the **sign flip** — a *positive* climate coefficient on minor injuries is hard for any pure risk-reduction story to produce (risk reduction should lower all tiers). Triangulate: (3.1) sign flip + (3.2) negative det_share + (3.3) composition shift. State this limitation explicitly; do not claim the gradient alone demonstrates bias.

3. **Strongest future corroboration is within-facility.** The cleanest discriminator between surfacing and true risk is a within-mine pre/post around a dated climate change (the certification/decertification event-study from the unionization follow-on). Cross-sectional gradient + longitudinal surfacing together would be decisive. Flag as the natural next step, not part of this supplement.

4. **Coal vs M/NM.** The EIA-7A union covariate is coal-only; this detectability analysis itself runs on the full Part 50 population, but any joint model with union status is coal-restricted.

5. **Comp-data validation is weak.** Morantz (2014) and the Eastern Research Group (2013) report both caution that workers'-comp-to-MSHA match-rate comparisons are unreliable because comp records are themselves incomplete. Do **not** anchor the detectability ordering to external comp match rates; anchor it to the a priori severity/detectability logic and cite the typology.

---

## 7. Architecture placement

```
common/                      # shared, unchanged
  feature_layers/            # temporal + EWS layers reused as-is
msha/
  config.R                   # + detectability_tiering, t0_inclusion, illness_handling, time_grain
  data_prep.R                # + tier mapping (§2); exclude 07/08/09; build det_share; ordinal factor
  fit_models.R               # tier-stratified count + beta + clmm specs
```

- `data_prep.R` is where the tier recode lives (single source of truth for the 00–10 → T0/T1/T2/T3/EXCLUDE map). Emit the excluded-code counts to a QC log so 07/08/09 volumes are auditable.
- MV runner: tiers enter the **outcome** dimension; one model per (outcome-tier × spec).
- CV runner: run the nested M0–M4 incremental-value hierarchy **per tier**. Expectation under the hypothesis — climate adds CV predictive value for T3 and for det_share, with smaller/again-protective value for T1; report ΔBrier / Δlog-loss per tier using `best_non_climate` baselines.

---

## 8. Reference anchors

- Azaroff, Levenstein & Wegman (2002), *AJPH* 92(9):1421–1429 — conceptual filters (theoretical backbone).
- Probst, Brubaker & Barsotti (2008), *J. Appl. Psychol.* 93(5):1147–1154 — injury-rate underreporting × organizational safety climate; differential effect strongest on the discretionary outcomes.
- Probst & Estrada (2010), *Accid. Anal. Prev.* 42(5):1438–1444 — ~2.48 unreported per reported; worse climate → more underreporting.
- Galizzi, Miesmaa, Punnett & Slatin (2010), *Industrial Relations* 49(1):22–43, doi:10.1111/j.1468-232X.2009.00585.x — healthcare; severity-graded capture (≈63% of serious injuries reported). [verify page range 22–43 vs 22–42]
- Morantz (2013), *ILR Review* 66(1):88–116 — coal; traumatic↓/total↑ under unionization = the pattern being replicated.
- Morantz (2014), *Filing Not Found: Which Injuries Go Unreported to Worker Protection Agencies, and Why?* DOL Scholars Program — detectability typology on MSHA data; the methodological template. [gray literature]
- National Research Council (1982), *Toward Safer Underground Coal Mines* — "intermediate" injury conjecture (T2 reference).
- Eastern Research Group (2013); DOL OIG (2014) — MSHA Part 50 underreporting context; treat comp-match inferences with caution.
- Reason (1997), *Managing the Risks of Organizational Accidents* — reporting culture / just culture; voluntary-report volume as a positive safety attribute (aviation valence, §9).

---

## 9. Aviation variant (ASRS climate → AIDS/NTSB outcomes)

**Why a variant, not a copy.** Aviation has no single mandatory census with a within-record severity ladder. The climate signal is extracted from **ASRS** ATC narratives (voluntary, confidential, de-identified — linkable only at the ATC-facility / terminal level, not the individual operator). Outcomes come from a *different* reporting regime: **FAA AIDS** (officially filed incidents below the NTSB accident threshold) plus **NTSB** (accidents above it). Because exposure (ASRS) and outcome (AIDS/NTSB) are separate corpora filed by different agents, the within-corpus circularity that would invalidate a reporting-filter test on ASRS-alone does not arise.

**Unit & geography.** Airport / terminal area, airport-quarter panel, events within 5 NM of the airport. Requires the ASRS climate stream to be terminal-area (tower/TRACON) matched to the airport; en-route (ARTCC) reports break the unit alignment (see caveat 4).

**Detectability ladder = cross-regime, not within-code.**

| Tier | Source | Content | Detectability |
|------|--------|---------|---------------|
| T1 high | NTSB | accidents w/ fatal or serious injury, or aircraft destroyed | ~complete; can't hide |
| T2 mid | NTSB | accidents w/ minor/no injury + substantial damage | boundary band |
| T3 low | AIDS | incidents below NTSB accident threshold | discretionary / detection-dependent |

Maps onto NTSB injury coding (fatal/serious/minor/none) and damage coding (destroyed/substantial/minor/none); the **AIDS↔NTSB regime boundary is the principal detectability cut**.

**Estimand.** Same as mining: ATC-climate coefficient by tier, airport-quarter, offset = log(tower+TRACON operations) from FAA **ATADS/OPSNET**. Hypothesis = protective at T1, positive at T3.

**Four disanalogies that change interpretation (build the spec around these):**

1. **Reporting agent ≠ climate-bearing unit.** AIDS is filed by FAA inspectors, NTSB by NTSB — partly exogenous to the controllers whose climate is measured. Attenuates the surfacing channel; the positive-T3 prediction is *softer* than in mining.
2. **Valence flips.** A positive T3 coefficient may mean the system is *working* (reporting-culture-as-leading-indicator; Reason 1997), not an artifact to explain away. More flattering for dynclim (signal detects a recognized positive safety attribute), but "sign flip ⇒ reporting bias" no longer follows cleanly.
3. **Artifact-vs-feature discriminator (temporal; routes to Paper 2/EWS).** Do surfaced T3 incidents *forecast fewer subsequent T1 events* (healthy reporting culture catching/correcting = feature) or merely track contemporaneous climate with no downstream protective value (artifact)? The lead-lag test is the aviation-specific discriminator and belongs in the temporal-features/EWS paper.
4. **T1 sparsity is an order of magnitude worse than mining.** NTSB (esp. fatal) events within 5 NM per airport-quarter are vanishingly rare → forces airport-year aggregation, rare-events logistic / Firth, or pooled airport random effects. T1-as-count at quarter grain is a non-starter.
5. **AIDS regime drift confounds T3.** AIDS coverage/coding shifted over time (full narratives from 1995; portal records effectively from 2010; ASIAS integration). Incident-capture rate moves for non-climate reasons, sitting directly on the discretionary tier → strong period controls (year effects, possibly facility trends) are mandatory, not optional.

**Open scoping check.** Confirm the ASRS ATC climate source is terminal-area, matched to the 5-NM terminal framing.

---

## 10. Cross-industry logic; rail & nuclear analogs (sketch)

The four industries are not four copies of one test — each isolates a different facet of the same reporting-filter mechanism, which is the real strength of the multi-industry design:

- **Mining (MSHA):** within-census severity ladder (DEGREE_INJURY tiers). Tests the filter directly where the same org both has the climate and makes the recording decision.
- **Aviation (ASRS→AIDS/NTSB):** cross-regime ladder; adds the reporting-culture-as-feature reading and the temporal artifact-vs-feature discriminator.
- **Rail (FRA):** a sharp, externally-set, reindexed **monetary reporting threshold** → the cleanest manipulation/bunching test.
- **Nuclear (NRC):** an **independent machine-logged outcome stream** (operational/power-status data) → the falsification anchor where reporting bias *cannot* operate, so a climate–outcome association there is clean evidence of real risk tracking.

### 10.1 Rail (FRA Form 6180.54 + casualty data)
- **Filter mechanism:** 49 CFR 225.19 requires reporting only of equipment accidents with damage **above a monetary threshold** (CY2026 = $12,600), reindexed annually (ladder back to $6,500 in 1997). Two discretionary channels: (a) whether to report at all (binary, threshold), and (b) the railroad's *self-estimated* damage figure (continuous, manipulable near the cut).
- **Detectability ladder:** damage magnitude relative to threshold — far-above = high detectability; just-above = discretionary; casualty/injury severity (Form 55a) as a parallel ladder with FRA's documented intimidation/anti-retaliation history as the mechanism.
- **Distinctive test — bunching / density discontinuity.** Because the threshold is a bright line and damage is self-estimated, test for excess mass just above the threshold (McCrary-style density test) and ask whether **worse climate predicts more just-above-threshold bunching** (strategic damage estimation). The annual threshold reindexing gives within-railroad, over-time variation in the cutoff location — a quasi-experimental handle absent in the other industries.
- **Unit/offset:** reporting railroad (restrict to Class I for a stable panel), railroad-quarter; offset = train-miles or employee-hours (FRA-collected).
- **Caveats:** FRA public data contains only above-threshold events (below-threshold are railroad-internal "accountable" records, not submitted) → density test works on the right side of the cut only; threshold is national (not facility-varying) so identification rides on the time dimension; damage-estimate manipulation and genuine severity are hard to separate without the casualty cross-check.

### 10.2 Nuclear (NRC LERs + operational data)
- **The key structural difference:** the US operating fleet has ≈no core-damage accidents in the LER era, so the high-detectability "accident/fatality" tier is **empty** — there is no T1 to anchor a within-LER severity ladder the way mining/aviation do.
- **The compensating asset (which no other industry has):** Mike's operational pipeline (`nrc_operational_data.py`, power-status scraper, scram data) is an **objective, machine-logged event stream captured independently of the LER narrative reporting decision.** A scram / power reduction / ESF actuation appears in operational telemetry whether or not a clean LER is filed.
- **Reframe — falsification, not gradient.** Nuclear's distinctive role is as the **clean-identification anchor**: regress objectively-logged operational outcomes (scram, emergency declaration, power loss — the existing `ordinal_scram`, `emerg_binary`, `pct_power_loss`) on LER-narrative-derived climate. Because the outcome is machine-logged, a climate–outcome association there **cannot be a reporting artifact** → it is the strongest available rebuttal to the "your signal just measures reporting propensity" critique. This is the inverse of the mining test: mining asks "is the positive minor-injury coefficient a reporting artifact?"; nuclear asks "here is an outcome where artifact is impossible — does climate still predict it?"
- **Secondary — reporting-completeness (capture-recapture).** Treat operational telemetry as an independent second list: does better climate predict tighter correspondence between operationally-evident events and filed LERs (fewer machine-logged events lacking a corresponding LER)? That is the nuclear analog of "surfacing."
- **Valence caution (opposite of mining).** Nuclear reporting incentives run *toward* conservative over-reporting (failing to file a required LER is an enforceable violation), unlike mining where the incentive is to hide injuries (comp/experience-rating). The climate–reporting relationship may therefore be non-monotone or reversed; do not assume the mining valence. Connects to the known NRC quirk that operational/seasonal baselines can underperform intercept-only (`baseline_strategy = "best_non_climate"`).
- **Reportability discretion:** LER filing is governed by 10 CFR 50.72/50.73 criteria (guidance: NUREG-1022) with genuine interpretation latitude at the low-significance end (tech-spec violations, missed surveillances) — that latitude is the discretionary tier for the completeness analysis.
