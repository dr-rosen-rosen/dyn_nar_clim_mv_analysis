############ Content-validity sampling: high vs low climate-score narratives
#
# For each industry's headline outcome (rail injuries, NRC LERs, aviation
# AIDS-all), this script samples five narratives from the top decile and
# five from the bottom decile of the champion-spec per-report climate
# score. The result is a 30-row supplementary table demonstrating
# face-level content validity of the climate measure.
#
# Sampling design choices (and why):
#   * Random sample from top/bottom decile (not top-5/bottom-5):
#     - Reduces cherry-picking concerns ("showcase extremes")
#     - Demonstrates the scoring distinguishes reliably across many cases
#     - Fixed seed for reproducibility
#   * One headline outcome per industry (champion config varies by outcome
#     within industry; using one per industry keeps the table interpretable)
#   * Windowing is NOT a factor in the ranking — windowing aggregates
#     ACROSS reports for the panel-rate analysis, but ranking individual
#     reports uses the champion's per-report overall_final_score directly
#
# Outputs:
#   report_figures_manuscript/supplementary_content_validity_samples.csv
#     — 30 rows, with full narrative text + a truncated display version
#   report_figures_manuscript/supplementary_content_validity_samples.md
#     — Markdown table ready for inclusion in supplementary materials
#
# Note: narratives may contain identifying information about specific
# organizations or events. Manual review is recommended before publication
# to ensure no PII or sensitive content appears in the supplementary table.

PLOT_BASE_SIZE <- 12
N_PER_DECILE   <- 5L
RANDOM_SEED    <- 42L
NARRATIVE_TRUNCATE_CHARS <- 600L  # ~100 words; tighten/loosen as needed

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(readxl)
})


# =============================================================================
# 1. CHAMPION-SPEC HEADLINE OUTCOME PER INDUSTRY
# =============================================================================

champion_specs <- tribble(
  ~industry,  ~outcome,        ~cfg_dir,                                                                                                                  ~source_path,                                                                                          ~narrative_col,  ~id_col,    ~org_col,         ~date_col,
  "rail",     "rate_injuries", "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026",            "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet",  "narrative",     "eid",      "org_id",         "event_date",
  "nrc",      "rate_lers",     "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026",             "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet",   "event_text",    "event_num", "facility",      "event_date",
  "aviation", "rate_aids_all", "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026",            "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet", "narrative",   "eid",      "airport_id",     "event_date"
)


# =============================================================================
# 2. PER-INDUSTRY SAMPLING
# =============================================================================

#' Sample n narratives from the top and bottom deciles of a champion's
#' per-report climate score.
#'
#' @param industry Industry key (rail / nrc / aviation).
#' @param champion_row One row from champions_best.csv (timeseries CV).
#' @param spec Row of champion_specs giving file paths and column names.
#' @param n Number per decile.
#' @param seed Random seed.
#' @return Tibble with one row per sampled narrative.
sample_decile_narratives <- function(industry, champion_row, spec,
                                       n = N_PER_DECILE, seed = RANDOM_SEED) {

  cid <- as.character(champion_row$best_config_id)
  parquet_path <- file.path(spec$cfg_dir, "_cfg", cid, "results.parquet")
  if (!file.exists(parquet_path)) {
    warning(sprintf("Champion config parquet not found: %s", parquet_path))
    return(NULL)
  }

  # Per-report climate scores from the champion pipeline.
  # results.parquet has one row per report with overall_final_score.
  scores <- arrow::read_parquet(parquet_path)
  if (!"overall_final_score" %in% names(scores)) {
    warning(sprintf("No overall_final_score column in %s", parquet_path))
    return(NULL)
  }
  # Standardize the report id column to "eid" — different industries
  # use "report_id" or other names in the toolkit output.
  if ("report_id" %in% names(scores) && !"eid" %in% names(scores)) {
    scores <- scores %>% rename(eid = report_id)
  }
  scores <- scores %>%
    select(eid, overall_final_score) %>%
    mutate(eid = as.character(eid)) %>%
    filter(!is.na(overall_final_score))

  # Filter to reports the pipeline actually scored substantively.
  # An overall_final_score of exactly 0 means no segments passed the
  # composite pipeline — typically very short narratives or narratives
  # without text that survived the segment thresholding. These are
  # effectively missing data for content-validity purposes; including
  # them would tie-break the top decile to a single value (zero), which
  # is exactly what we saw in the first-pass smoke test.
  n_before <- nrow(scores)
  scores <- scores %>% filter(overall_final_score != 0)
  cat(sprintf("    Filtered %d -> %d non-zero-scored reports (%.1f%% retained)\n",
              n_before, nrow(scores), 100 * nrow(scores) / n_before))

  # Source narratives.
  events <- arrow::read_parquet(spec$source_path)
  # Ensure id column matches what's in scores
  if (spec$id_col != "eid" && "eid" %in% names(events)) {
    # Use eid if present (preferred)
    events <- events %>% mutate(.id = as.character(eid))
  } else if (spec$id_col %in% names(events)) {
    events <- events %>% mutate(.id = as.character(.data[[spec$id_col]]))
  } else {
    stop(sprintf("Could not find id column in %s (looked for %s and eid)",
                  spec$source_path, spec$id_col))
  }

  # Pick narrative + metadata columns. Some industries (aviation) have
  # multiple narrative columns; pick the canonical one.
  needed_cols <- intersect(c(spec$narrative_col, spec$org_col, spec$date_col),
                            names(events))
  events_keep <- events %>%
    select(.id, all_of(needed_cols)) %>%
    rename(narrative_raw = !!sym(spec$narrative_col))
  if (spec$org_col %in% needed_cols) {
    events_keep <- events_keep %>% rename(org = !!sym(spec$org_col))
  } else {
    events_keep$org <- NA_character_
  }
  if (spec$date_col %in% needed_cols) {
    events_keep <- events_keep %>% rename(event_date = !!sym(spec$date_col))
  } else {
    events_keep$event_date <- as.Date(NA)
  }

  # Join scores → events on id
  joined <- scores %>%
    inner_join(events_keep, by = c("eid" = ".id"))

  cat(sprintf("  [%s] %d reports with scores; %d joined to narratives\n",
              industry, nrow(scores), nrow(joined)))

  if (nrow(joined) < (n * 5)) {
    warning(sprintf("Too few reports for stable decile sampling in %s (n=%d)",
                    industry, nrow(joined)))
  }

  # Compute deciles. ntile is robust to ties.
  joined <- joined %>%
    mutate(decile = dplyr::ntile(overall_final_score, 10))

  set.seed(seed)
  top    <- joined %>% filter(decile == 10) %>% slice_sample(n = n)
  bottom <- joined %>% filter(decile == 1)  %>% slice_sample(n = n)

  bind_rows(
    top    %>% mutate(decile_label = "top (10th)"),
    bottom %>% mutate(decile_label = "bottom (1st)")
  ) %>%
    mutate(
      industry            = industry,
      industry_label      = champion_row$industry_label,
      outcome             = spec$outcome,
      champion_config_id  = cid,
      narrative_raw       = as.character(narrative_raw),
      narrative_length    = nchar(narrative_raw),
      narrative_display   = ifelse(
        narrative_length > NARRATIVE_TRUNCATE_CHARS,
        paste0(substr(narrative_raw, 1, NARRATIVE_TRUNCATE_CHARS), " [...]"),
        narrative_raw
      )
    ) %>%
    select(industry, industry_label, outcome, champion_config_id,
            decile_label, climate_score = overall_final_score,
            event_date, org, eid,
            narrative_length, narrative_display, narrative_raw)
}


# =============================================================================
# 3. LOAD CHAMPIONS + LOOP
# =============================================================================

cat("=== Sampling content-validity examples ===\n")

samples <- list()
for (i in seq_len(nrow(champion_specs))) {
  spec <- champion_specs[i, ]
  ind  <- spec$industry

  # Load champion (timeseries CV)
  ch_path <- file.path("report_figures_panel", ind, "champions_best.csv")
  if (!file.exists(ch_path)) {
    warning(sprintf("champions_best.csv not found for %s — skipping", ind))
    next
  }
  champ <- readr::read_csv(ch_path, show_col_types = FALSE) %>%
    filter(outcome == spec$outcome, cv_strategy == "timeseries")
  if (nrow(champ) != 1) {
    warning(sprintf("Unexpected champion count for %s | %s — skipping",
                    ind, spec$outcome))
    next
  }
  # Attach a clean industry_label
  champ$industry_label <- switch(ind,
    "rail"     = "Rail (FRA)",
    "nrc"      = "Nuclear (NRC)",
    "aviation" = "Aviation (NTSB/AIDS/ASRS)",
    ind
  )

  samples[[ind]] <- sample_decile_narratives(ind, champ, spec)
}

result <- bind_rows(samples) %>%
  arrange(industry, decile_label, desc(climate_score))


# =============================================================================
# 4. WRITE OUTPUTS
# =============================================================================

dir.create("report_figures_manuscript", recursive = TRUE, showWarnings = FALSE)

# Full CSV — includes BOTH narrative_display (truncated) and narrative_raw (full)
csv_out <- "report_figures_manuscript/supplementary_content_validity_samples.csv"
readr::write_csv(result, csv_out)
cat(sprintf("\nWrote %s (%d rows)\n", csv_out, nrow(result)))

# Markdown table — uses the truncated display column, suitable for the
# supplementary materials document.
md_path <- "report_figures_manuscript/supplementary_content_validity_samples.md"
md_lines <- c(
  "# Supplementary Table: Content Validity Examples",
  "",
  sprintf("Random samples of five narratives from the top decile and five from the bottom decile of the per-report climate score, for each industry's headline outcome. Sampling uses the champion specification's full pipeline (embedding × sentiment × composite × thresholding); windowing is not a factor in the per-report ranking. Random seed = %d.",
          RANDOM_SEED),
  "",
  "Reports with an overall_final_score of exactly 0 (typically very short narratives or narratives with no segments surviving thresholding) are excluded from the ranking before deciles are computed. The remaining distribution of scores is bounded above by zero in most cases and skews negative across industries, reflecting that the source material consists of incident narratives — text written *about* things that went wrong. The score therefore captures *how* incidents are described, not whether they are framed positively in absolute terms. Higher-decile (top 10%) narratives represent the least critical, most constructive descriptions of incidents at their organization, while lower-decile (bottom 10%) narratives represent the most critical or fatalistic descriptions. This relative-position interpretation is what the climate measure operationalizes.",
  "",
  sprintf("Narratives truncated to %d characters for display. Full narratives are available in the accompanying CSV.",
          NARRATIVE_TRUNCATE_CHARS),
  ""
)

for (ind in unique(result$industry)) {
  ind_label <- result %>% filter(industry == ind) %>% pull(industry_label) %>% .[1]
  outc      <- result %>% filter(industry == ind) %>% pull(outcome) %>% .[1]
  cfg       <- result %>% filter(industry == ind) %>% pull(champion_config_id) %>% .[1]
  md_lines <- c(md_lines,
                sprintf("## %s — headline outcome: %s (champion config %s)",
                        ind_label, outc, cfg),
                "")
  for (dec in c("top (10th)", "bottom (1st)")) {
    md_lines <- c(md_lines, sprintf("### %s decile", dec), "")
    sub <- result %>% filter(industry == ind, decile_label == dec)
    for (j in seq_len(nrow(sub))) {
      r <- sub[j, ]
      md_lines <- c(md_lines,
        sprintf("**Score: %.4f | Date: %s | Org: %s | id: %s**",
                r$climate_score,
                as.character(r$event_date),
                as.character(r$org),
                substr(r$eid, 1, 40)),
        "",
        sprintf("> %s",
                gsub("\n+", " ", r$narrative_display)),
        ""
      )
    }
  }
}

writeLines(md_lines, md_path)
cat(sprintf("Wrote %s\n", md_path))

cat("\n=== Summary ===\n")
print(result %>%
        group_by(industry_label, decile_label) %>%
        summarise(n = n(),
                  score_min = min(climate_score),
                  score_max = max(climate_score),
                  mean_length = mean(narrative_length, na.rm = TRUE),
                  .groups = "drop"))
