#!/usr/bin/env python3
"""
9a_compute_whole_narrative_vader.py

Compute a pipeline-INDEPENDENT report-level sentiment for each industry by
running VADER on the ENTIRE untrimmed narrative (no segmentation, no
similarity thresholding, no similarity weighting). This is the clean
"raw sentiment" control for the sentiment-decomposition sensitivity
analysis: it answers "is the climate measure just a negativity detector?"

Why VADER (and why whole-narrative):
  - The saved per-config `overall_sent` is NOT raw: segments are filtered by
    similarity thresholding and, for attention/power composites (which all
    three best-performing models use), reweighted by similarity. It cannot
    serve as a clean sentiment control. See
    paper_repo_prep/METHODS_NOTE_sentiment_aggregation.md.
  - VADER is lexicon-based with no token limit, so it scores the full text
    of any length, and being a different model from the pipeline's
    transformer sentiment, surviving a VADER control is strong evidence the
    effect is not a sentiment-model artifact.

Outputs one parquet per industry:
  report_figures_manuscript/whole_narrative_vader_<industry>.parquet
  columns: eid, vader_compound, vader_pos, vader_neg, vader_neu,
           narrative_nchar, narrative_nword
"""

import os
import re
import pandas as pd
import pyarrow.parquet as pq
import pyarrow as pa
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

OUT_DIR = "report_figures_manuscript"
os.makedirs(OUT_DIR, exist_ok=True)

# (industry, events_parquet_path, narrative_col, id_col)
SOURCES = [
    ("rail",
     "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet",
     "narrative", "eid"),
    ("nrc",
     "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet",
     "event_text", "event_num"),
    ("aviation",
     "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet",
     "narrative", "eid"),
    ("msha",
     "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/events.parquet",
     "narrative", "eid"),
]

analyzer = SentimentIntensityAnalyzer()
_ws = re.compile(r"\s+")


def score_text(t):
    """VADER compound + components + length features for one narrative."""
    if t is None or (isinstance(t, float) and pd.isna(t)):
        return None
    s = str(t).strip()
    if not s:
        return None
    vs = analyzer.polarity_scores(s)
    nchar = len(s)
    nword = len(_ws.split(s))
    return (vs["compound"], vs["pos"], vs["neg"], vs["neu"], nchar, nword)


for industry, path, narr_col, id_col in SOURCES:
    print(f"\n=== {industry} ===")
    if not os.path.exists(path):
        print(f"  MISSING: {path}")
        continue
    df = pd.read_parquet(path)
    # Resolve id column — prefer eid if present
    if "eid" in df.columns:
        ids = df["eid"].astype(str)
    elif id_col in df.columns:
        ids = df[id_col].astype(str)
    else:
        raise SystemExit(f"No id column ({id_col}/eid) in {path}")
    if narr_col not in df.columns:
        raise SystemExit(f"No narrative column {narr_col} in {path}")

    print(f"  {len(df):,} reports; scoring whole-narrative VADER ...")
    rows = []
    narr = df[narr_col].tolist()
    idl = ids.tolist()
    n_scored = 0
    for eid, txt in zip(idl, narr):
        r = score_text(txt)
        if r is None:
            rows.append((eid, None, None, None, None, None, None))
        else:
            rows.append((eid, *r))
            n_scored += 1

    out = pd.DataFrame(rows, columns=[
        "eid", "vader_compound", "vader_pos", "vader_neg", "vader_neu",
        "narrative_nchar", "narrative_nword"
    ])
    # De-duplicate on eid (events files can carry duplicate ids); keep first
    out = out.drop_duplicates(subset="eid", keep="first")

    out_path = os.path.join(OUT_DIR, f"whole_narrative_vader_{industry}.parquet")
    pq.write_table(pa.Table.from_pandas(out, preserve_index=False), out_path)
    nz = out["vader_compound"].notna().sum()
    print(f"  scored {n_scored:,} non-empty; wrote {len(out):,} unique eids "
          f"({nz:,} with VADER) -> {out_path}")
    if nz > 0:
        print(f"  compound: mean={out['vader_compound'].mean():.3f} "
              f"sd={out['vader_compound'].std():.3f} "
              f"min={out['vader_compound'].min():.3f} "
              f"max={out['vader_compound'].max():.3f}")

print("\n=== Done ===")
