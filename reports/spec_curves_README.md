# spec_curves.qmd — Setup & Usage

## Requirements

```r
install.packages(c("quarto", "knitr", "glue"))
# quarto CLI: https://quarto.org/docs/get-started/
```

## Rendering

From your project root (or wherever spec_curves.qmd lives):

```bash
quarto render spec_curves.qmd
# → outputs spec_curves.html (single self-contained file)
```

Or from R:

```r
quarto::quarto_render("spec_curves.qmd")
```

## Configuring for your actual file paths

Everything you need to change is in the `setup` chunk at the top of the .qmd:

### 1. Plot directories

```r
PLOT_DIRS <- list(
  rail = "rail_analysis/plots",   # ← change to your actual path
  nrc  = "nrc_analysis/plots"
)
```

Paths are relative to wherever you render from (usually project root).
Use absolute paths if needed: `"/Users/mike/dynclim/nrc_analysis/plots"`.

### 2. Outcomes

```r
OUTCOMES <- list(
  rail = list(
    list(id = "hurdle_count",    label = "Event Count (Hurdle)"),
    list(id = "hurdle_severity", label = "Severity (Hurdle)")
    # add more outcomes here
  ),
  nrc = list(
    list(id = "ordinal_scram",   label = "Reactor Scram (Ordinal)"),
    list(id = "count_ler",       label = "LER Event Count")
    # add more outcomes here
  )
)
```

The `id` must match exactly what appears in your plot filenames.
The `label` is what shows in the document.

### 3. Filename templates

```r
PLOT_TEMPLATES <- list(
  multiverse = "spec_curve_multiverse_{outcome}.png",
  cv         = "spec_curve_cv_{outcome}.png"
)
```

Replace these patterns to match what your R plotting scripts save.
For example, if your scripts save `rail_hurdle_count_multiverse.pdf`, use:

```r
multiverse = "{outcome}_multiverse.pdf"
```

## Extending the document

To add more industries (e.g., healthcare), add entries to both `PLOT_DIRS` and `OUTCOMES`,
then add a new top-level section to the .qmd:

```markdown
## Healthcare

```{r healthcare-sections, results='asis'}
for (outcome in OUTCOMES$healthcare) {
  render_outcome_section("healthcare", outcome)
}
```
```

To add more plot types per outcome (e.g., a sensitivity plot), extend
`PLOT_TEMPLATES` and update `render_outcome_section()` accordingly.
