# Data Manifest

Curated safety-event, safety-outcome, and operational-exposure data for a
four-industry study of narrative-derived organizational safety climate
(rail, nuclear power, coal mining, aviation).

Path roots used below:
- `[toolkit]` = `DynamicNarrativeClimateToolkit/dynclim` (Python processing repo)
- `[analysis]` = `dyn_nar_clim_mv_analysis` (R analysis repo)

For the Zenodo deposit these files are reorganized **by industry**; the paths
below identify the exact source of each file in the working repositories.

---

## Overview

Data are curated versions of publicly available safety event reports, safety
outcomes, and operational measures across rail, nuclear power, coal mining, and
aviation. Data are organized by industry. For each industry the collection
provides three linked components: (i) **event reports**, including the free-text
incident narratives from which organizational-safety-climate measures are
derived; (ii) **safety outcomes** — counts and severities of accidents,
injuries, and other adverse events; and (iii) **operational measures** that
quantify each organization's exposure and activity, used as modeling offsets and
covariates. All sources are U.S. federal safety-reporting systems. The curated
files standardize identifiers, dates, and outcome fields and aggregate
operational data to the analytic panel cadence. Where noted, raw source files
extend earlier than the modeling window, which is bounded by the availability of
matching operational-exposure data.

---

## Rail — Federal Railroad Administration (FRA)

Event narratives and casualty outcomes are drawn from the FRA Rail Equipment
Accident/Incident reporting system (Form 6180.54); operational exposure
(train-miles, passenger-miles, employee-hours) is from the FRA Injury/Illness
Summary operational source data (Form 55). **Unit of analysis: railroad-month.**

| Role | File | Coverage |
|---|---|---|
| Events (processed) | `[toolkit]/data/processed/rail/events.parquet` (30M) | 1982–2025 |
| Events (raw) | `[analysis]/data/rail/rail_raw.csv` | Form 54 |
| Operational (raw = input) | `[analysis]/data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv` (41M) | 1975–2025 |

Public source: FRA Office of Safety Analysis (safetydata.fra.dot.gov). Form 55
retrieved 2026-02-20. (An earlier pull `..._20260129.csv` also exists in the
working repo and is **not** used.)

---

## Nuclear power — Nuclear Regulatory Commission (NRC)

Event narratives and outcomes are from the NRC Licensee Event Report (LER)
system, retrieved via the Idaho National Laboratory LER Search
(lersearch.inl.gov): licensee event reports, reactor scrams, emergency-
classification declarations, and power-loss measures. Operational exposure
(reactor-days at power; daily power variability aggregated across units to the
site level) is from NRC daily power-reactor status data. **Unit of analysis:
site-quarter.**

| Role | File | Coverage |
|---|---|---|
| Events (processed) | `[toolkit]/data/processed/nrc/events.parquet` (5.8M) | 1994–2026 |
| Events (raw) | `[toolkit]/data/raw/nrc/nrc_events_raw.csv` (+ `failed_events.csv`) | LER search |
| Operational (processed) | `[toolkit]/data/processed/nrc/power_status_quarterly.parquet/…` (124K) | 2000–2025 |
| Aux outcomes* | `[toolkit]/data/processed/nrc/findings_quarterly.parquet`, `action_matrix_long.parquet` | from `ENSearchResults_02-16-2026.xlsx` |

Operational data (2000–2025) bound the nuclear analytic window. Public source:
NRC / INL LER Search; NRC enforcement/oversight exports (retrieved 2026-02-16).
*Inspection findings and action-matrix files are auxiliary — present in the
multiverse but **not** among the study's reported outcomes.

---

## Coal mining — Mine Safety and Health Administration (MSHA)

Event narratives and injury outcomes are from the MSHA accident/injury reporting
system (Form 7000-1) via MSHA Open Data; operational exposure (employee-hours)
is from the MSHA quarterly mine employment/production data; regulatory covariates
(citations, significant-and-substantial violations) from the MSHA
violations/inspections databases. Analysis restricted to **coal** operations
(commodity from the mines master table). **Unit of analysis: mine-quarter.**

| Role | File | Coverage |
|---|---|---|
| Events (processed) | `[toolkit]/data/processed/msha/events.parquet` (45M) | 2000–2026 |
| Events (raw) | `[toolkit]/data/raw/msha/Accidents.{zip,txt}` | Form 7000-1 |
| Operational (processed) | `[toolkit]/data/processed/msha/ops_mine_quarter.parquet` (36M) | 2000–2025 |
| Operational (raw) | `[toolkit]/data/raw/msha/MinesProdQuarterly.{zip,txt}` + `MinesProdYearly.{zip,txt}` | — |
| Covariates | `[toolkit]/data/processed/msha/covariates_mine_quarter.parquet` (9.3M) | violations / S&S |
| Covariates (raw) | `[toolkit]/data/raw/msha/Violations.zip`, `Inspections.zip` | — |
| Commodity master | `[toolkit]/data/processed/msha/mines_master.parquet` (raw: `Mines.zip`) | — |

Public source: MSHA Open Data (msha.gov/opendata; arlweb open-government datasets).

---

## Aviation — ASRS + NTSB + FAA AIDS + BTS T-100

The climate narrative source (ASRS) and the outcome registries (NTSB, AIDS) are
independent systems; operational exposure is from BTS T-100. Outcomes are linked
to the nearest FAA-coded airport within 5 nautical miles; analysis restricted to
1988 onward. **Unit of analysis: airport-quarter.**

| Role | File | Coverage |
|---|---|---|
| ASRS narratives (climate; processed) | `[toolkit]/data/processed/aviation/events.parquet` (54M) | 1988–2025 |
| ASRS (raw) | `[toolkit]/data/raw/aviation/asrs_dict_df.csv`, `asrs_raw/` | 1988–2026 |
| NTSB accidents (outcomes) | `[analysis]/data/aviation/ntsb_av_accident_data/events.xlsx` (8.5M) + `events_pre2008.xlsx` (19M) | 1948–2023 |
| NTSB (raw) | same dir: `avall.zip`, `Pre2008.zip` | — |
| FAA AIDS (outcomes; processed) | `[toolkit]/data/processed/aviation/aids_events.parquet` (12M) | 1945–2026 (analysis ≥1988) |
| FAA AIDS (raw) | `[analysis]/data/aviation/aids/raw/` | — |
| Operational (BTS T-100) | `[analysis]/data/aviation/bts_t100/airport_month_ops.parquet` (16M) | 1990–2023 |
| Operational (raw) | `[analysis]/data/aviation/bts_t100/raw/` | — |

BTS T-100 coverage (1990–2023) bounds the aviation analytic window. Public
sources: NASA ASRS (asrs.arc.nasa.gov); NTSB aviation accident database
(ntsb.gov); FAA AIDS / SFIS; U.S. Bureau of Transportation Statistics T-100
Segment (bts.gov).

---

## Notes for the deposit

- **Two working repositories.** Most *processed* files live under `[toolkit]`,
  while rail operational data, NTSB, AIDS-raw, and BTS live under `[analysis]`.
  A by-industry Zenodo layout should draw from both.
- **Source vs. analytic coverage.** Raw AIDS (to 1945) and NTSB pre-2008 (to
  1948) extend well before the modeling window (1988+). If the deposit ships
  trimmed files, update the coverage spans accordingly.
- **Retrieval dates** are embedded in filenames only for FRA Form 55 (2026-02-20)
  and the NRC enforcement export (2026-02-16); confirm download dates for
  ASRS / NTSB / AIDS / MSHA / BTS before citing versions.
- **Checksums** (md5/sha256) can be added per file on request for archival
  integrity.
- **Derived NLP features** (`linguistic_features_*`, `temporal_features_*`) are
  not listed here — they are model-derived intermediates, not source data.
