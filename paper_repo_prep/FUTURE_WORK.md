# Future Work — Narrative-Derived Safety Climate

Running list of follow-up analyses, methods extensions, and applied
directions raised during preparation of the NHB submission. Organized by
where each item fits in the methodological framework:

- **[MV/CV]** — integrates naturally with the existing multiverse + cross-
  validation infrastructure; can be added as a new "design dimension" or
  outcome / cell without changing the model class.
- **[MODEL]** — requires a different model architecture; new infrastructure
  needed; warrants a dedicated methods paper.
- **[ADD-ON]** — runs *on top of* the current champion models to test
  whether a specific extension improves fit; appendix-scale rather than
  paper-scale.
- **[SCOPE]** — extends the research program to new constructs, industries,
  or settings.
- **[APPLIED]** — translational / deployment-oriented work.

Each item is tagged with rough effort: **(S)** sub-analysis (days),
**(M)** appendix-scale or dissertation chapter (weeks), **(L)** dedicated
paper (months).

---

## 1. Feature layer extensions (input side)

These add new types of information to the climate measurement pipeline.
The current paper uses semantic similarity + sentiment valence; these
extensions add complementary signal channels.

### 1.1 Domain-specific sentiment fine-tuning [MV/CV] (M)
Train a sentiment model on labeled safety-narrative segments to replace
the general-purpose sentiment models (VADER, RoBERTa-base, SiEBERT)
currently used. Variance-partition results in the current paper identify
sentiment as the highest-leverage component (~19% of Δll variance);
domain-specific fine-tuning is the highest-leverage technical next step.

**Integration**: drop in as a 4th level of the sentiment-model factor in
the multiverse, then test whether it concentrates the positive-Δll tail
of the spec curve relative to the general models.

### 1.2 Lexical / syntactic features [MV/CV or ADD-ON] (M)
Add features beyond embedding-based semantic similarity:
- Lexical: hedging language, modal verbs, agency-marking
  ("we identified" vs. "it was determined"), causal-attribution
  patterns, error-framing language.
- Syntactic: passive-voice prevalence, nominalization, sentence
  complexity, narrative voice / agency markers.

**Integration**: each lexical/syntactic feature can be added either as a
parallel measurement channel (test in the multiverse alongside sentiment)
or as an additive control in the champion specification (test ΔAIC /
Δll improvement). Recommend running both: MV/CV for measurement-side
sensitivity, add-on for incremental-fit assessment.

**Note**: code scaffolding for these layers exists in
`common/layer_*.R` and `common/feature_layers.R` from earlier
exploratory work — those modules were excluded from the paper repo but
preserved in the working repo for this purpose.

### 1.3 Temporal features [MODEL or ADD-ON] (M)
Features that capture *how* a narrative is positioned in time rather
than what it says:
- Inter-event interval at the reporting organization
- Trend in narrative volume (acceleration / deceleration)
- Seasonality / cyclical position of the narrative
- Time-since-last-event
- Time-since-last-elevated-severity-event

**Integration**: these are best added as covariates within the champion
panel-rate model rather than as MV factors — they're operational metadata,
not measurement choices. Test ΔAIC / Δll on top of current models.

### 1.4 Early-warning-system (EWS) features [MODEL] (L)
Statistical EWS literature (variance, autocorrelation lag-1, critical
slowing down, flickering) developed in ecology and finance for detecting
approaching regime shifts. Apply these to either the narrative-climate
trajectory or the operational-event trajectory per organization.

**Integration**: requires per-organization time-series construction at
sufficient resolution. Some orgs in the current panels would not have
enough data; EWS features may need to be limited to high-coverage
organizations. Best as its own paper.

**Note**: `common/layer_ews.R` exists as scaffolding.

---

## 2. Modeling approach extensions (methods side)

### 2.1 Distributed lag models within panel-rate framework [MODEL] (M-L)
Replace the windowed-scalar climate covariate (SMA-5, EWMA-360, etc.)
with a polynomial- or Almon-constrained distributed lag of climate over
a defined horizon. Lets the data estimate the impulse-response of climate
on outcomes rather than fixing it via window choice.

**Why this is the strongest near-term methods move**: addresses the
"window choice is a researcher degree of freedom" concern at the model
level rather than via multiverse sensitivity. The multiverse becomes
"what is the maximum lag horizon and constraint family" — a much smaller
researcher-decision space.

**Integration**: keep glmmTMB and the panel-rate framework. Refactor
multiverse runners to enumerate lag-structure choices instead of single-
window choices. Compute Δll the same way. Report estimated impulse
responses per (industry × outcome) cell.

### 2.2 Dynamic factor models (joint latent state) [MODEL] (L)
State-space model where narrative-derived features AND event outcomes
are both observations on a latent dynamic organizational state. Lets
the question "is climate a leading indicator of safety state, a
contemporaneous reflection, or a lagged response" be asked directly.

**Why this matters**: directly addresses the bidirectional puzzle
raised by the NRC findings divergence in the current paper. The
substantive interpretation of narrative climate as protective trait
vs. discursive response to events becomes testable rather than
speculative.

**Integration**: Stan / INLA / brms implementation. Smaller scope than
the current paper — probably one industry, deeper analysis. Substantial
methods scaffolding required.

### 2.3 Time-varying climate coefficients [MODEL] (L)
State-space framework allowing climate's effect on outcomes to vary
over time within an organization. Tests whether climate's predictive
value changes after major safety events, regulatory interventions, or
leadership changes.

**Integration**: BSTS / Kalman-filter framework; per-organization
fitting feasible for high-coverage orgs only. Most useful for the
question "does climate matter more or less when an org is under
operational stress?"

### 2.4 Panel Granger causality tests [ADD-ON or MODEL] (S-M)
Apply Dumitrescu-Hurlin or related panel-Granger tests to formally
evaluate "past climate helps predict outcomes beyond what past
outcomes already explain." Direct formalization of the lagged-outcome
control check we did during paper prep.

**Integration**: complement to the current panel-rate framework
rather than replacement. Reports a separate test statistic alongside
the model coefficient. Could go in an appendix of the current paper if
reviewer pressure warrants; otherwise saves for a follow-up.

### 2.5 Lagged-outcome formalization [ADD-ON] (S)
The exploratory check we ran during prep (climate β under lag-1,
lag-3, lag-12 outcome controls) is appendix-ready. If a reviewer
raises the "climate is just past events" concern, formalize as a
sensitivity table with all main-text cells and report ΔAIC + climate
coefficient attenuation.

**Integration**: minimal new infrastructure; extends
`6_sensitivity_plots.R` by ~50 lines.

### 2.6 Bidirectional / reverse-causation identification [MODEL] (L)
Use natural experiments (regulatory actions, leadership changes,
post-event policy interventions) as instruments for narrative climate
to identify causal direction. Most tractable in NRC where regulatory
action timestamps are public and discrete.

**Integration**: substantial new design work. Identification strategy
(instrument validity, exclusion restriction) needs careful argument.
Probably a co-authored paper with someone with applied IV experience.

---

## 3. Substantive scope extensions

### 3.1 Additional industries [MV/CV] (M each)
Add domains with similar narrative-reporting infrastructures:
- Maritime safety (USCG)
- Pipeline safety (PHMSA — already has scaffolding in the working repo)
- Mine safety (MSHA)
- Healthcare adverse events (state-mandated reporting; varies by state)
- Offshore oil and gas (BSEE)

**Integration**: each new industry follows the same
panel-data-prep → multiverse → CV → champion-selection pattern.
Industry-specific data prep is the only substantial new code.

### 3.2 Cross-construct extension [SCOPE] (L each)
Apply the measurement framework to other organizational constructs that
share the discursive-byproduct structure:
- Psychological safety (Edmondson tradition)
- Learning climate
- Ethical climate
- Organizational justice
- High-reliability organizing dimensions

**Integration**: each requires its own validated scale items as semantic
anchors, its own outcome measures, and its own validation work. The
methodological framework (multiverse + CV + champion selection)
transfers directly.

### 3.3 Externally-assessed outcomes — bidirectional analysis [MODEL+MV/CV] (L)
Follow-up dedicated to the NRC findings / action-matrix divergence
documented in the current paper's appendix. Combine dynamic factor
modeling (Section 2.2) with the cross-channel validation question
(when do self-reported and externally-observed measures converge or
diverge?).

**This is the most theoretically generative follow-up.** Establishes
narrative climate as a tool for diagnosing reporting-channel dynamics
rather than just a measurement substitute for surveys.

### 3.4 Lower-stakes settings [SCOPE] (M)
Test whether the narrative-climate signal exists in organizational
contexts with weaker reporting incentives — manufacturing safety
records, hospital incident reports, professional-services compliance
filings. Generalization-of-framework question.

### 3.5 Multi-channel validation as a paradigm [SCOPE] (L)
Develop the conceptual framework for when self-reported and externally-
observed measures of organizational constructs should converge or
diverge. Empirical contribution across multiple constructs and
channels.

---

## 4. Applied / translational directions

### 4.1 Real-time deployment infrastructure [APPLIED] (M-L)
Production pipeline: incoming narrative → climate score → trajectory
update → alerting threshold evaluation. Engineering work more than
research. Critical questions:
- Model versioning and drift monitoring
- Audit trail integrity
- Recalibration cadence against independent outcomes
- Inter-version reliability

### 4.2 Industry-specific window-length guidelines [APPLIED] (S-M)
Synthesis from the variance-partition finding (window is the 2nd most
important design dimension after sentiment, with strong industry-
dependent patterns). Develop concrete guidance documents for how to
choose window length given reporting cadence and event base rate.

### 4.3 Regulatory practice applications [APPLIED] (L)
Pilot deployment with one regulator (NRC or FAA most likely candidates
given precedent and data infrastructure). Demonstrate use as a
complement to existing oversight processes. Substantial relationship-
building and policy work.

---

## Suggested ordering for the next 2-3 papers

If I were sketching the publication arc following the current NHB
submission:

1. **Paper 2 — Methods extension**: Distributed lag models within the
   panel-rate framework (§2.1). Same three industries, refined
   methodological contribution. Demonstrates that the multiverse + CV
   paradigm extends naturally to richer model classes.

2. **Paper 3 — Bidirectional dynamics**: NRC findings + action matrix
   analysis with dynamic factor models (§2.2 + §3.3). Most substantively
   novel; addresses the construct-validity question raised by the
   current paper's appendix divergence.

3. **Paper 4 — Cross-construct extension**: Apply the framework to
   psychological safety or learning climate using a similar narrative-
   reporting infrastructure (§3.2). Demonstrates the paradigm
   generalizes beyond safety climate specifically.

The feature-layer extensions (§1.2, §1.3, §1.4) can be incorporated
into any of these papers as new factors in the multiverse, or split off
as a dedicated methods paper if the EWS layer in particular shows
sufficient signal to warrant its own treatment.

---

## Items that should NOT become future papers

Worth being honest about scope:

- **Single-industry deeper analyses** (e.g., "narrative climate in rail
  injuries — a deeper look"). Risk of diminishing returns; the current
  paper already does the rail injuries headline well. Would need a
  substantively new question to warrant.
- **Pure NLP comparison papers** (which embedding model is best, etc.).
  The variance-partition finding shows embedding choice matters
  relatively little; not enough signal for a paper.
- **Replication of the current paper at a different time window**.
  Useful as an exercise but not paper-grade contribution.
