# Align Script Defaults With Manuscript Artifacts

**Problem (bit us in the 2026-07 revision rerun):** the per-industry driver
scripts' *default* invocations do not produce the artifacts the manuscript
actually consumes. The manuscript-final variants live behind env-var tags
that are documented only in script comments and in the champion/figure
scripts' hardcoded read paths:

| Industry | Default run produces | Manuscript actually reads |
|---|---|---|
| msha | 7-outcome coal+mnm baseline | `_coal_detectability_full` (8 outcomes incl. rate_t0, coal only; `MSHA_MV_TAG=detectability_full MSHA_MV_COMMODITIES=coal MSHA_MV_OUTCOMES=...+rate_t0`) |
| aviation | month grain, 15 outcomes | `_quarterly` (**`AV_PERIOD=quarter`**; CV restricted to the 5 primary outcomes) — the month-grain default equals the `_consolidated` ED variant |
| rail, nrc | baseline | baseline (aligned ✓) |

During the revision rerun this cost a redundant ~17h msha coal baseline
MV+CV (det_full is a superset of it) and a near-miss where month-grain
aviation results were briefly mislabeled as the quarterly primary (caught
by an estimate-correlation check: same-design runs correlate ≥0.999;
cross-grain ~0.95).

**Fix before posting the paper repo (pick one):**
1. *(preferred)* Make the default invocation of each `1_/2_` driver produce
   exactly the manuscript artifact (fold the tag env-vars into the script
   defaults; keep the old baselines available behind explicit tags instead),
   OR
2. Add a top-level `run_manuscript.sh` that encodes the full artifact →
   invocation map (tags, env vars, outcome restrictions) so the posted repo
   has ONE documented entry point that regenerates everything the paper uses.

Either way: the champion generator (`attic/_gen_champions.R`) and stages
5–13 hardcode the tagged filenames — whatever is chosen must keep those
paths working or update them in the same commit.
