# Data Availability & Code Availability Statements

For inclusion in the manuscript at submission. Replace `[DOI: pending]`
placeholders with the actual Zenodo DOIs once deposits are minted.

NHB places these statements at the end of the Methods section (which is
itself at the end of the manuscript). Recommended length: each statement
is one paragraph, 80–160 words. Both statements should be complete
sentences that a reader can act on without consulting other parts of the
paper.

---

## Data Availability

> All input data analysed in this study are derived from publicly
> accessible safety reporting infrastructures maintained by U.S. federal
> regulators. The U.S. Federal Railroad Administration accident/incident
> reports (Form 54) and operational summaries (Form 55) are available at
> https://safetydata.fra.dot.gov/. The U.S. Nuclear Regulatory Commission
> Licensee Event Reports, daily power-status records, action-matrix
> assignments, and inspection findings are available at
> https://lersearch.inl.gov/ and https://www.nrc.gov/. The NASA Aviation
> Safety Reporting System (ASRS) database is available at
> https://asrs.arc.nasa.gov/. The U.S. National Transportation Safety
> Board accident database and the Federal Aviation Administration
> Accident/Incident Data System (AIDS) are available at
> https://www.ntsb.gov/ and https://www.faa.gov/, respectively. The
> Bureau of Transportation Statistics T-100 Segment data on airport
> operations are available at https://www.transtats.bts.gov/. The exact
> source files used to construct the panels for this analysis — including
> derived event narratives, operational covariates, and the climate-score
> configuration registries produced by the accompanying text-processing
> pipeline — are archived at Zenodo
> ([DOI: pending — input data deposit]). Intermediate analytical
> artifacts (multiverse and cross-validation results parquets, champion-
> spec selections, and figure-input data) are archived at Zenodo
> ([DOI: pending — intermediate artifacts deposit]) and permit
> reproduction of all figures and tables without re-running the model-
> fitting steps.

### Variant: shorter version (if word count is tight)

> All input data are derived from publicly accessible safety reporting
> infrastructures maintained by the U.S. Federal Railroad Administration,
> the U.S. Nuclear Regulatory Commission, the NASA Aviation Safety
> Reporting System, the U.S. National Transportation Safety Board, the
> Federal Aviation Administration, and the U.S. Bureau of Transportation
> Statistics. The exact source files used to construct the analysis
> panels are archived at Zenodo ([DOI: pending — input data deposit]),
> together with the climate-score configuration registries produced by
> the text-processing pipeline. Intermediate analytical artifacts
> (multiverse and cross-validation results, champion-spec selections,
> and figure-input data) are archived at Zenodo
> ([DOI: pending — intermediate artifacts deposit]).

---

## Code Availability

> All code required to reproduce the analyses and figures in this
> manuscript is publicly available. The R analysis code, including the
> multiverse and cross-validation drivers, panel-rate model specifications,
> champion-spec selection, and all figure- and table-generating scripts,
> is archived at Zenodo ([DOI: pending — analysis code]) and mirrored on
> GitHub at https://github.com/[org]/safety-climate-multiverse-paper
> (release tag `v1.0-nhb-submission`). The Python text-processing pipeline
> (sentence segmentation, sentence-transformer embedding, sentiment
> scoring, composite-score aggregation, and temporal windowing) is
> archived separately at Zenodo ([DOI: pending — text pipeline]) and
> mirrored at https://github.com/[org]/safety-climate-text-pipeline
> (release tag `v1.0-nhb-submission`). Both repositories are frozen
> snapshots of the working analysis and toolkit repositories at the time
> of submission; the working repositories continue to evolve and are
> available at [working analysis URL] and [working toolkit URL] under
> permissive open-source licenses (MIT). Comprehensive instructions for
> reproducing the analysis, including system requirements, dependency
> versions, and a methods-to-code mapping table, are provided in the
> README of the analysis-code archive.

### Variant: shorter version

> All code required to reproduce the analyses and figures is publicly
> available. The R analysis code is archived at Zenodo
> ([DOI: pending — analysis code]) and mirrored on GitHub
> (https://github.com/[org]/safety-climate-multiverse-paper, release
> tag `v1.0-nhb-submission`). The companion Python text-processing
> pipeline is archived at Zenodo ([DOI: pending — text pipeline]) and
> mirrored on GitHub (https://github.com/[org]/safety-climate-text-pipeline,
> release tag `v1.0-nhb-submission`). Both archives include comprehensive
> reproduction instructions and a methods-to-code mapping table in the
> README. The working analysis and toolkit repositories continue to evolve
> and are available at [working analysis URL] and [working toolkit URL]
> under the MIT license.

---

## Optional: Methods-section ↔ archive cross-references

If NHB editorial guidance allows, embed inline references to the
archives at the appropriate spots in the Methods text. Suggested
locations:

| Methods subsection | Suggested inline reference |
|---|---|
| Data sources | "(see Data Availability for archived source files)" |
| Climate measurement from text | "(implemented in the companion Python pipeline; see Code Availability)" |
| Multiverse specification | "(implemented in `common/panel_multiverse_runner.R` in the code archive)" |
| Cross-validation | "(implemented in `common/panel_cv_runner.R`; held-out log-likelihoods archived in the intermediate artifacts deposit)" |
| Champion-spec selection | "(implemented in `common/best_model_analysis.R`; champion CSVs archived in the intermediate artifacts deposit)" |

These inline references make the manuscript navigable for any reader
who wants to verify a specific claim against a specific code path —
which is the function NHB code-availability requirements are designed
to support.

---

## Reviewer access during peer review

For the initial submission, the Zenodo deposits should be created with
restricted-access tokens that can be shared in the cover letter for
double-anonymous review (or with the editor in single-anonymous review).
Once the paper is accepted, the deposits should be made fully public.

Suggested cover-letter language:

> The data and code supporting this manuscript are deposited at Zenodo
> and are currently accessible via the following private review links:
> - Input data: [link with token]
> - Intermediate artifacts: [link with token]
> - Analysis code: [link with token]
> - Text-processing pipeline: [link with token]
>
> Upon acceptance, all deposits will be made fully public with persistent
> DOIs, and the corresponding GitHub repositories will be tagged with a
> `v1.0-nhb-publication` release for permanent citation.

---

## Pre-deposit checklist

Before depositing on Zenodo, verify:

- [ ] All raw data files are present in the input-data deposit
- [ ] All paths in the code drivers resolve correctly against the
      data-deposit directory structure
- [ ] The README in the code archive matches the actual code structure
      (drivers, helpers, output paths)
- [ ] The multiverse and CV results parquets in the intermediate-artifacts
      deposit are from the FINAL run, not an earlier exploratory run
- [ ] The champion CSVs in `report_figures_panel/` reflect the final
      multiverse + CV results
- [ ] The final figures in `report_figures_manuscript/` correspond to
      what appears in the manuscript PDF
- [ ] License files are present in both code repos
- [ ] No personally-identifying information appears in any data file
      (verify event-narrative content where applicable)
- [ ] No internal-only paths (e.g., absolute paths to your machine) remain
      in any committed code

Run the following at the paper-repo root to check for stale absolute paths:
```bash
grep -rn "/Users/michaelrosen" --include="*.R" --include="*.qmd" --include="*.md"
```
Any matches should be relocated under `data/`, `results/`, etc., or
replaced with paths configurable via environment variables or top-of-file
constants.
