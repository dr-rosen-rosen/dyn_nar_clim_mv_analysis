############ Zero-score diagnostic: where do exact-zero overall_final_scores
############ come from, and how prevalent are they per industry?
#
# Per-report climate scores from the champion specification are computed
# as a composite of segment-level semantic similarity and sentiment. For
# a non-trivial fraction of reports in each industry, the overall score
# is exactly zero. Inspection of the per-item diagnostic columns
# (overall_strength, overall_sent, per-item _sent / _strength) reveals
# that zero scores have a consistent signature:
#
#   - processing_success = TRUE (no pipeline error)
#   - overall_strength > 0 (semantic similarity computed normally; segments
#     match scale items as expected)
#   - overall_sent == 0 (sentiment is uniformly zero across all segments)
#   - All per-item _sent values are exactly 0
#
# This means the zero is not a failed pipeline run, missing data, or a
# segmentation problem — it is the sentiment model returning exactly zero
# (or near-zero values that compose to zero) across all segments of the
# narrative. Direct inspection of zero-score narratives shows they are
# highly procedural engineering/operational descriptions with no affective
# content. The choice of composite method matters here: with
# power_attention (champion for NRC LERs, rail injuries) the composite is
# Σ(w_i × sent_i), so zero sentiments produce a zero output regardless of
# similarity weights. With weighted_average composite, the composite would
# be 0.5 × mean_sim + 0.5 × mean_sent, and a zero sentiment column would
# still yield ~0.1 from the similarity component.
#
# Important: zero scores are NOT excluded from temporal windowing in the
# panel-rate analysis. The build_climate_panel() function in
# common/panel_data_prep.R filters only on is.na(), not on equality to
# zero. Therefore zeros enter the SMA/EWMA windows as literal zeros,
# pulling per-organization windowed climate scores toward zero in
# proportion to how procedural the organization's reporting style is.
# Whether to treat these as missing instead of as zero is a non-trivial
# modeling choice and is currently planned as a separate robustness
# check (treating zeros as missing, refitting champions, comparing climate
# coefficients).
#
# Outputs:
#   report_figures_manuscript/zero_score_diagnostic_summary.csv
#   report_figures_manuscript/zero_score_diagnostic_examples.md

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(stringr)
})

N_EXAMPLES_PER_END <- 3L
NARRATIVE_TRUNCATE <- 400L
RANDOM_SEED <- 42L

# Headline-outcome champion per industry (same as 8_content_validity_samples.R)
specs <- tribble(
  ~industry,  ~outcome,        ~cfg_dir,                                                                                                                  ~events_path,                                                                                          ~narrative_col,  ~id_col,
  "rail",     "rate_injuries", "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026",            "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet",  "narrative",     "eid",
  "nrc",      "rate_lers",     "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026",             "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet",   "event_text",    "event_num",
  "aviation", "rate_aids_all", "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026",            "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet", "narrative",   "eid"
)

dir.create("report_figures_manuscript", recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 1. PER-INDUSTRY SUMMARY
# =============================================================================

summarize_zeros <- function(industry, outcome, cfg_dir, events_path,
                              narrative_col, id_col) {

  # Load champion config id + sentiment / composite method (from
  # config_registry.csv) so we can attach the design info to the summary.
  champ_path <- file.path("report_figures_panel", industry, "champions_best.csv")
  ch <- readr::read_csv(champ_path, show_col_types = FALSE) %>%
    filter(outcome == !!outcome, cv_strategy == "timeseries")
  cid <- as.character(ch$best_config_id)

  reg <- readr::read_csv(file.path(cfg_dir, "config_registry.csv"),
                         show_col_types = FALSE) %>%
    filter(as.character(config_id) == cid)

  parquet_path <- file.path(cfg_dir, "_cfg", cid, "results.parquet")
  if (!file.exists(parquet_path)) {
    warning(sprintf("Missing %s", parquet_path))
    return(NULL)
  }

  p <- arrow::read_parquet(parquet_path)

  # Tag each report's "zero source":
  #   "sentiment_zero"     = overall_sent == 0 (the dominant case)
  #   "similarity_zero"    = overall_strength == 0 (segment-matching failure)
  #   "process_failure"    = processing_success == FALSE
  #   "non_zero"           = overall_final_score != 0
  d <- p %>%
    mutate(zero_class = case_when(
      !processing_success                              ~ "process_failure",
      overall_final_score != 0                          ~ "non_zero",
      overall_final_score == 0 & overall_strength == 0  ~ "similarity_zero",
      overall_final_score == 0 & overall_sent     == 0  ~ "sentiment_zero",
      overall_final_score == 0                          ~ "composite_zero_other",
      TRUE                                              ~ "unknown"
    ))

  totals <- d %>%
    count(zero_class) %>%
    mutate(pct = round(100 * n / sum(n), 2))

  # Helper to safely pull a count from the per-class totals
  cnt <- function(cls) totals$n[totals$zero_class == cls] %||% 0L
  pct <- function(cls) round(100 * cnt(cls) / nrow(d), 2)

  summary_row <- tibble(
    industry          = industry,
    outcome           = outcome,
    champion_cfg      = cid,
    embedding_model   = reg$embedding_model[1],
    sent_method       = reg$sent_method[1],
    comp_method       = reg$comp__method[1],
    n_reports         = nrow(d),
    n_zero            = sum(d$overall_final_score == 0, na.rm = TRUE),
    pct_zero          = round(100 * mean(d$overall_final_score == 0, na.rm = TRUE), 2),
    n_sentiment_zero      = cnt("sentiment_zero"),
    pct_sentiment_zero    = pct("sentiment_zero"),
    n_similarity_zero     = cnt("similarity_zero"),
    pct_similarity_zero   = pct("similarity_zero"),
    n_composite_zero_other = cnt("composite_zero_other"),
    pct_composite_zero_other = pct("composite_zero_other"),
    n_process_failure     = cnt("process_failure"),
    pct_process_failure   = pct("process_failure"),
    median_n_seg_at_zero =
      if (any(d$overall_final_score == 0)) {
        # Use LSC_1_n_segments as a proxy for total segments processed
        median(d$LSC_1_n_segments[d$overall_final_score == 0], na.rm = TRUE)
      } else NA_real_,
    median_n_seg_at_nonzero =
      if (any(d$overall_final_score != 0)) {
        median(d$LSC_1_n_segments[d$overall_final_score != 0], na.rm = TRUE)
      } else NA_real_,
    median_strength_at_zero =
      if (any(d$overall_final_score == 0)) {
        round(median(d$overall_strength[d$overall_final_score == 0], na.rm = TRUE), 4)
      } else NA_real_,
    median_strength_at_nonzero =
      if (any(d$overall_final_score != 0)) {
        round(median(d$overall_strength[d$overall_final_score != 0], na.rm = TRUE), 4)
      } else NA_real_
  )

  # Pull example narratives for the markdown output
  events <- arrow::read_parquet(events_path)
  if (!"eid" %in% names(events) && id_col %in% names(events)) {
    events <- events %>% mutate(eid = as.character(.data[[id_col]]))
  } else {
    events <- events %>% mutate(eid = as.character(eid))
  }
  events_keep <- events %>%
    select(eid, all_of(narrative_col)) %>%
    rename(narrative = !!narrative_col)

  set.seed(RANDOM_SEED)
  zero_examples <- d %>%
    filter(overall_final_score == 0) %>%
    mutate(eid = as.character(report_id)) %>%
    left_join(events_keep, by = "eid") %>%
    filter(!is.na(narrative), nchar(narrative) > 30) %>%
    slice_sample(n = N_EXAMPLES_PER_END) %>%
    transmute(industry = industry, decile = "zero (composite=0)",
              eid, climate_score = overall_final_score,
              overall_strength, overall_sent,
              narrative = substr(stringr::str_squish(narrative), 1, NARRATIVE_TRUNCATE))

  # Also pick non-zero contrast examples (one with very negative score, one
  # near zero but non-zero, one positive if any exist)
  nonzero <- d %>% filter(overall_final_score != 0) %>%
    mutate(eid = as.character(report_id))
  if (nrow(nonzero) > 0) {
    contrasts <- nonzero %>%
      mutate(pct_rank = percent_rank(overall_final_score)) %>%
      filter(pct_rank < 0.05 | pct_rank > 0.95) %>%
      slice_sample(n = N_EXAMPLES_PER_END) %>%
      left_join(events_keep, by = "eid") %>%
      filter(!is.na(narrative), nchar(narrative) > 30) %>%
      transmute(industry = industry, decile = "non-zero (contrast)",
                eid, climate_score = overall_final_score,
                overall_strength, overall_sent,
                narrative = substr(stringr::str_squish(narrative), 1, NARRATIVE_TRUNCATE))
  } else {
    contrasts <- zero_examples[0, ]
  }

  list(summary = summary_row,
       examples = bind_rows(zero_examples, contrasts))
}

`%||%` <- function(a, b) if (length(a) == 0 || is.null(a) || is.na(a)) b else a

# Run across industries
cat("=== Zero-score diagnostic across champion specs ===\n\n")
all_summaries <- list()
all_examples  <- list()
for (i in seq_len(nrow(specs))) {
  s <- specs[i, ]
  cat(sprintf("Processing %s (%s) ...\n", s$industry, s$outcome))
  res <- summarize_zeros(
    industry = s$industry, outcome = s$outcome,
    cfg_dir = s$cfg_dir, events_path = s$events_path,
    narrative_col = s$narrative_col, id_col = s$id_col
  )
  if (is.null(res)) next
  all_summaries[[i]] <- res$summary
  all_examples[[i]]  <- res$examples
}

summary_df  <- bind_rows(all_summaries)
examples_df <- bind_rows(all_examples)

# =============================================================================
# 2. OUTPUT
# =============================================================================

csv_path <- "report_figures_manuscript/zero_score_diagnostic_summary.csv"
readr::write_csv(summary_df, csv_path)
cat(sprintf("\nWrote %s\n", csv_path))

print(summary_df %>%
        select(industry, n_reports, pct_zero,
                pct_sentiment_zero, pct_similarity_zero,
                pct_composite_zero_other,
                median_n_seg_at_zero, median_strength_at_zero,
                sent_method, comp_method))

# Markdown output
md_path <- "report_figures_manuscript/zero_score_diagnostic_examples.md"
md_lines <- c(
  "# Zero-Score Diagnostic — Example Narratives",
  "",
  sprintf("For each industry's headline-outcome champion specification, this table shows three random zero-score narratives (composite climate score exactly = 0) and three non-zero contrast narratives from the score-distribution tails. Random seed = %d. Narratives are truncated to %d characters for display.",
          RANDOM_SEED, NARRATIVE_TRUNCATE),
  "",
  "## Summary of zero-score sources",
  "",
  paste("|", paste(c("Industry", "n reports", "% zero", "% sentiment-zero",
                     "median n_seg (zero)", "median strength (zero)",
                     "sentiment model", "composite"), collapse = " | "), "|"),
  paste("|", paste(rep("---", 8), collapse = " | "), "|")
)
for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  md_lines <- c(md_lines,
    sprintf("| %s | %d | %.1f%% | %.1f%% | %s | %s | %s | %s |",
            r$industry, r$n_reports, r$pct_zero, r$pct_sentiment_zero,
            as.character(r$median_n_seg_at_zero),
            as.character(r$median_strength_at_zero),
            r$sent_method, r$comp_method))
}
md_lines <- c(md_lines, "",
  "Reading: zero scores arise from two distinct failure modes that vary by industry. In rail and NRC (where sentiment-zeros dominate), segments are extracted normally and match the safety-climate scale items semantically, but the general-purpose sentiment model returns a uniformly neutral compound score on highly procedural / engineering language. With the power_attention composite (champion method for both rail injuries and NRC LERs), zero sentiments propagate to a zero composite regardless of similarity strength. This is the mechanism implied by the variance-partition finding that sentiment is the highest-leverage design dimension; domain-specific sentiment fine-tuning would address it.",
  "",
  "In aviation (where similarity-zeros dominate), the failure is upstream: the semantic-similarity step finds no segments matching the safety-climate scale items above the thresholding cutoff. The narrative content (often technical flight or ATC descriptions) does not align semantically with cross-industry safety-climate items. This points to a different methodological lever — industry-specific climate scales, or looser thresholding — rather than sentiment-side improvements.",
  "",
  "Both mechanisms produce identical downstream consequences (a composite climate score of exactly zero that enters the temporal windowing as a numerical zero rather than as missing data), but they imply different methodological remedies.",
  ""
)

for (ind in unique(examples_df$industry)) {
  md_lines <- c(md_lines, sprintf("## %s", ind), "")
  for (dec in c("zero (composite=0)", "non-zero (contrast)")) {
    md_lines <- c(md_lines, sprintf("### %s", dec), "")
    sub <- examples_df %>% filter(industry == ind, decile == dec)
    for (j in seq_len(nrow(sub))) {
      r <- sub[j, ]
      md_lines <- c(md_lines,
        sprintf("**Score: %.4f | strength: %.3f | sent: %.3f | eid: %s**",
                r$climate_score, r$overall_strength, r$overall_sent,
                substr(r$eid, 1, 40)),
        "",
        sprintf("> %s", r$narrative),
        ""
      )
    }
  }
}

writeLines(md_lines, md_path)
cat(sprintf("Wrote %s\n", md_path))

cat("\n=== Done ===\n")
