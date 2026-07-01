############ Table 1 — Data sources, outcomes, and operational covariates
#
# Builds Table 1 for the manuscript: one block per industry summarizing
#   - Narrative source (used for climate scoring)
#   - Outcome source(s)
#   - Operational / exposure source
# with raw record counts, date ranges, and organizational unit counts.
#
# Optionally also reports the post-filter panel counts (n cells, n orgs,
# n non-zero, date range) from a representative champion config.
#
# Outputs:
#   report_figures_manuscript/table1_sources_long.csv  — long format,
#       one row per (industry × source_type × source_name)
#   report_figures_manuscript/table1_panel_summary.csv — one row per
#       (industry × outcome) with filtered panel counts (if enabled)
#   report_figures_manuscript/table1_wide.csv          — wide pivoted
#       version, one row per industry
#
# Usage:
#   Rscript 7_table1_data_sources.R                # raw sources only
#   Rscript 7_table1_data_sources.R --with-panel   # + panel summary
# =============================================================================

INCLUDE_PANEL_SUMMARY <- "--with-panel" %in% commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(readxl)
  library(lubridate)
  library(here)
})

OUT_DIR <- "report_figures_manuscript"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 1. SOURCE-FILE REGISTRY (one row per file we want to summarize)
# =============================================================================

source_registry <- tribble(
  ~industry,  ~source_type,        ~source_name,                        ~reader,    ~path,
  "rail",     "narratives",        "FRA Accident/Incident Reports",     "parquet",  "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet",
  "rail",     "outcomes",          "FRA Accident/Incident Reports",     "parquet",  "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet",
  "rail",     "operational",       "FRA Form 55 (operational summary)", "csv",      "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv",

  "nrc",      "narratives",        "NRC Event Notification Reports",    "parquet",  "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet",
  "nrc",      "outcomes",          "NRC Event Notification Reports",    "parquet",  "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet",
  "nrc",      "operational",       "NRC reactor power status (quarterly)","parquet","/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet",

  "aviation", "narratives",        "ASRS (NASA Aviation Safety Reporting System)", "parquet", "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet",
  "aviation", "outcomes-ntsb-post","NTSB Accident Database (>=2008)",   "xlsx",     "data/aviation/ntsb_av_accident_data/events.xlsx",
  "aviation", "outcomes-ntsb-pre", "NTSB Accident Database (pre-2008)", "xlsx",     "data/aviation/ntsb_av_accident_data/events_pre2008.xlsx",
  "aviation", "outcomes-aids",     "FAA AIDS Incident Database",        "parquet",  "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/aids_events.parquet",
  "aviation", "operational",       "BTS T-100 segment data (airport × month)","parquet","data/aviation/bts_t100/airport_month_ops.parquet",

  "msha",     "narratives",        "MSHA Accident/Injury/Illness Reports", "parquet", "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/events.parquet",
  "msha",     "outcomes",          "MSHA Accident/Injury/Illness Reports", "parquet", "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/events.parquet",
  "msha",     "operational",       "MSHA Mine Employment/Production (quarterly)", "parquet", "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/ops_mine_quarter.parquet"
)


# =============================================================================
# 2. RAW-SOURCE SUMMARY
# =============================================================================
# For each file: n records, date range, n unique orgs (column varies by source).

#' Read a source file and return a tibble with one date column "x_date" and
#' one org column "x_org" so the summary loop can be uniform.
read_source <- function(reader, path) {
  if (!file.exists(path)) {
    warning(sprintf("Missing source file: %s", path))
    return(NULL)
  }
  d <- switch(reader,
    parquet = arrow::read_parquet(path),
    csv     = readr::read_csv(path, show_col_types = FALSE,
                               guess_max = 50000),
    xlsx    = suppressWarnings(as_tibble(readxl::read_excel(path))),
    stop(sprintf("Unknown reader: %s", reader))
  )
  as_tibble(d)
}

#' Try a vector of candidate column names; return the first that exists.
pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

#' Coerce to Date, tolerating POSIXct or character.
to_date_safe <- function(x) {
  if (inherits(x, "Date"))    return(x)
  if (inherits(x, "POSIXt"))  return(as.Date(x))
  suppressWarnings(as.Date(x))
}

summarize_raw_source <- function(reader, path, industry, source_type, source_name) {
  d <- read_source(reader, path)
  if (is.null(d) || nrow(d) == 0) {
    return(tibble(industry, source_type, source_name, source_path = path,
                   n_records = NA_integer_, date_min = as.Date(NA),
                   date_max = as.Date(NA), n_orgs = NA_integer_,
                   org_unit = NA_character_, notes = "file missing"))
  }

  # ---- Date column ----
  date_col <- pick_col(d, c("event_date", "ev_date", "accident_date", "yearmonth",
                             "quarter_start", "Date", "time_date"))
  if (is.null(date_col)) {
    date_min <- as.Date(NA); date_max <- as.Date(NA)
  } else {
    dvec <- to_date_safe(d[[date_col]])
    date_min <- suppressWarnings(min(dvec, na.rm = TRUE))
    date_max <- suppressWarnings(max(dvec, na.rm = TRUE))
    if (is.infinite(date_min)) date_min <- as.Date(NA)
    if (is.infinite(date_max)) date_max <- as.Date(NA)
  }

  # ---- Org column ----
  # Source-type-specific preferences first (NTSB has no org_id per se).
  org_candidates <- list(
    rail            = c("org_id"),
    nrc             = c("facility_unit", "facility"),
    aviation_asrs   = c("airport_org_id", "operator_org_id"),
    aviation_ntsb   = c(),  # NTSB events: no consistent org column
    aviation_aids   = c("airport_id"),
    aviation_ops    = c("airport_id"),
    msha            = c("mine_id"),
    fallback        = c("org_id", "airport_id", "facility", "operator", "mine_id")
  )

  org_key <- case_when(
    industry == "rail"                                            ~ "rail",
    industry == "nrc"                                             ~ "nrc",
    industry == "msha"                                            ~ "msha",
    industry == "aviation" & source_type == "narratives"          ~ "aviation_asrs",
    industry == "aviation" & startsWith(source_type, "outcomes-ntsb") ~ "aviation_ntsb",
    industry == "aviation" & source_type == "outcomes-aids"       ~ "aviation_aids",
    industry == "aviation" & source_type == "operational"         ~ "aviation_ops",
    TRUE                                                          ~ "fallback"
  )

  org_col <- pick_col(d, org_candidates[[org_key]])
  # NRC: always assemble facility × unit if both raw columns present —
  # this is what the panel uses as the org grouping. Falls back to plain
  # `facility_unit` (where it's already a column, e.g. the ops file) when
  # facility+unit aren't separately present.
  if (industry == "nrc" &&
      all(c("facility", "unit") %in% names(d))) {
    n_orgs   <- dplyr::n_distinct(paste(d$facility, d$unit))
    org_unit <- "facility × unit"
  } else if (is.null(org_col)) {
    n_orgs   <- NA_integer_
    org_unit <- NA_character_
  } else {
    n_orgs   <- dplyr::n_distinct(d[[org_col]])
    org_unit <- switch(org_key,
                        rail          = "railroad",
                        nrc           = "facility × unit",
                        msha          = "mine",
                        aviation_asrs = "ASRS-reported airport",
                        aviation_aids = "FAA airport",
                        aviation_ops  = "BTS airport",
                        "(unknown)")
  }

  tibble(industry, source_type, source_name, source_path = path,
         n_records = nrow(d),
         date_min, date_max,
         n_orgs    = n_orgs,
         org_unit  = org_unit,
         notes     = "")
}


cat("=== Building Table 1: raw-source summary ===\n")
raw_summary <- source_registry %>%
  pmap_dfr(function(industry, source_type, source_name, reader, path) {
    cat(sprintf("  [%s | %s] %s\n", industry, source_type,
                basename(path)))
    summarize_raw_source(reader, path, industry, source_type, source_name)
  })

readr::write_csv(raw_summary, file.path(OUT_DIR, "table1_sources_long.csv"))
cat(sprintf("  Wrote %s (%d rows)\n",
            file.path(OUT_DIR, "table1_sources_long.csv"),
            nrow(raw_summary)))


# =============================================================================
# 3. WIDE TABLE 1 — one row per industry, narrative/outcome/ops side by side
# =============================================================================
# Roll up to: industry, narrative_source/period/n, outcome_source(s)/period/n,
# operational_source/period/n.

fmt_period <- function(d1, d2) {
  if (is.na(d1) || is.na(d2)) return(NA_character_)
  sprintf("%s — %s", format(d1, "%Y-%m"), format(d2, "%Y-%m"))
}

build_wide_row <- function(rs, ind) {
  sub <- rs %>% filter(industry == ind)

  narr  <- sub %>% filter(source_type == "narratives")
  ops   <- sub %>% filter(source_type == "operational")
  outs  <- sub %>% filter(stringr::str_detect(source_type, "^outcomes"))

  outs_str_n <- outs %>%
    mutate(label = sprintf("%s (n=%s)", source_name,
                            format(n_records, big.mark = ","))) %>%
    pull(label) %>% paste(collapse = "; ")

  outs_periods <- outs %>%
    mutate(period = mapply(fmt_period, date_min, date_max)) %>%
    pull(period) %>% paste(collapse = "; ")

  tibble(
    industry              = ind,
    narratives_source     = narr$source_name[1] %||% NA_character_,
    narratives_n          = narr$n_records[1]  %||% NA_integer_,
    narratives_period     = fmt_period(narr$date_min[1], narr$date_max[1]),
    narratives_org_unit   = narr$org_unit[1]   %||% NA_character_,
    narratives_n_orgs     = narr$n_orgs[1]     %||% NA_integer_,
    outcomes_sources      = outs_str_n,
    outcomes_periods      = outs_periods,
    operational_source    = ops$source_name[1] %||% NA_character_,
    operational_n         = ops$n_records[1]   %||% NA_integer_,
    operational_period    = fmt_period(ops$date_min[1], ops$date_max[1]),
    operational_org_unit  = ops$org_unit[1]    %||% NA_character_,
    operational_n_orgs    = ops$n_orgs[1]      %||% NA_integer_
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

wide_table <- map_dfr(c("rail", "nrc", "aviation", "msha"),
                       ~ build_wide_row(raw_summary, .x))
readr::write_csv(wide_table, file.path(OUT_DIR, "table1_wide.csv"))
cat(sprintf("  Wrote %s\n", file.path(OUT_DIR, "table1_wide.csv")))


# =============================================================================
# 4. (OPTIONAL) POST-FILTER PANEL SUMMARY
# =============================================================================
# Builds the same panel each industry's analysis uses for the champion config,
# and reports filtered cell / org counts and outcome non-zero rates. Slow
# because it has to load all the dependent modules. Skipped unless run with
# --with-panel.

if (!INCLUDE_PANEL_SUMMARY) {
  cat("\n[skipped] panel summary (pass --with-panel to compute)\n")
  cat("\n=== Table 1 done ===\n")
  cat(sprintf("  Outputs in: %s/\n", OUT_DIR))
  cat("    table1_sources_long.csv\n    table1_wide.csv\n")
  quit(save = "no", status = 0)
}

cat("\n=== Building post-filter panel summary ===\n")

# Source per-industry modules (same as 5_manuscript_plots.R)
source("common/postprocessing.R")
source("common/data_prep.R")
source("common/panel_data_prep.R")
source("rail/config.R");     source("rail/data_prep.R")
source("rail/panel_data_prep.R")
source("nrc/config.R");      source("nrc/panel_data_prep.R")
source("aviation/config.R"); source("aviation/data_prep.R")
source("aviation/panel_data_prep.R")
# MSHA: panel-prep + fit modules only (not msha/config.R; min_reports passed
# explicitly so MIN_REPORTS_DEFAULT from aviation/config.R is left intact).
source("msha/panel_data_prep.R"); source("msha/panel_fit_models.R")

champion_table <- readr::read_csv(file.path(OUT_DIR, "champion_summary.csv"),
                                    show_col_types = FALSE) %>%
  filter(cv_strategy == "timeseries")

# Map (industry, outcome) -> outcome_count column name (matches 5_manuscript_plots.R spec_map)
outcome_var_map <- tribble(
  ~industry,  ~outcome,                       ~outcome_var,
  "rail",     "rate_accidents",               "n_accidents",
  "rail",     "rate_injuries",                "sum_injured",
  "rail",     "rate_fatalities",              "sum_killed",
  "nrc",      "rate_emerg",                   "n_emerg",
  "nrc",      "rate_lers",                    "n_lers",
  "nrc",      "rate_pct_power_loss",          "sum_pct_power_loss",
  "nrc",      "rate_scrams",                  "n_scrams",
  "aviation", "rate_aids_noharm",             "n_aids_noharm",
  "aviation", "rate_aids_propdamage",         "n_aids_propdamage",
  "aviation", "rate_ntsb_serious_fatal",      "n_ntsb_serious_fatal",
  "msha",     "rate_t0",                      "n_t0",
  "msha",     "rate_t1",                      "n_t1",
  "msha",     "rate_t2",                      "n_t2",
  "msha",     "rate_t3",                      "n_t3",
  "msha",     "rate_days_lost",               "sum_days_lost"
)

# ---- Per-industry panel-data closures (same as 5_manuscript_plots.R) ----

# Rail
RAIL_EVENTS <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/rail/events.parquet"
RAIL_OPS    <- "data/rail/Injury_Illness_Summary_-_Operational_Source_Data_(Form_55)_20260220.csv"
rail_events <- read_parquet(RAIL_EVENTS) %>%
  rename(total_persons_killed  = `Total Persons Killed`,
         total_persons_injured = `Total Persons Injured`,
         total_damage_cost     = `Total Damage Cost`) %>%
  mutate(total_damage_cost = as.numeric(gsub("[^0-9.]", "", total_damage_cost))) %>%
  select(org_id, eid, event_date,
         total_persons_killed, total_persons_injured, total_damage_cost) %>%
  mutate(event_date = as.Date(event_date))
rail_ops_raw <- readr::read_csv(RAIL_OPS, show_col_types = FALSE) %>%
  mutate(across(all_of(OPS_VARS), ~ ifelse(.x < 0, NA_real_, .x))) %>%
  mutate(yearmonth = as.Date(yearmonth))
rail_ops_features <- make_ops_features_rolling(rail_ops_raw) %>%
  distinct(org_id, yearmonth, .keep_all = TRUE)

rail_panel_data_fn <- function(parquet_path, config_id) {
  prepare_rail_panel_data(parquet_path  = parquet_path,
                           config_id     = config_id,
                           events_df     = rail_events,
                           ops_raw       = rail_ops_raw,
                           ops_features  = rail_ops_features,
                           min_reports   = 50L,
                           max_holdover_days = 365L,
                           climate_base  = "overall_final_score")
}

# NRC
NRC_EVENTS <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/events.parquet"
NRC_OPS    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/nrc/power_status_quarterly.parquet/power_status_quarterly.parquet"
nrc_events <- read_parquet(NRC_EVENTS) %>%
  mutate(event_date = as.Date(event_date), eid = as.character(event_num))
nrc_ops_raw <- read_parquet(NRC_OPS) %>%
  mutate(quarter_start = as.Date(quarter_start))

nrc_panel_data_fn <- function(parquet_path, config_id) {
  prepare_nrc_panel_data(parquet_path  = parquet_path,
                          config_id     = config_id,
                          events_df     = nrc_events,
                          ops_raw       = nrc_ops_raw,
                          min_reports   = MIN_REPORTS_DEFAULT,
                          max_holdover_days = 365L,
                          climate_base  = "overall_final_score")
}

# Aviation
AV_ASRS      <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/events.parquet"
AV_AIDS      <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/aviation/aids_events.parquet"
AV_NTSB_POST <- "data/aviation/ntsb_av_accident_data/events.xlsx"
AV_NTSB_PRE  <- "data/aviation/ntsb_av_accident_data/events_pre2008.xlsx"
AV_OPS       <- "data/aviation/bts_t100/airport_month_ops.parquet"
asrs_meta       <- load_asrs_meta_panel(AV_ASRS)
av_ops_features <- read_parquet(AV_OPS)
ntsb_panel      <- load_ntsb_panel_rate(AV_NTSB_POST, AV_NTSB_PRE, apt_dist_nm = 5L,
                                        period = "quarter")
aids_panel      <- load_aids_panel_rate(AV_AIDS, apt_dist_nm = 5L, min_year = 1988L,
                                        period = "quarter")

aviation_panel_data_fn <- function(parquet_path, config_id) {
  prepare_aviation_panel_data(parquet_path  = parquet_path,
                               config_id     = config_id,
                               asrs_meta_df  = asrs_meta,
                               ntsb_panel_df = ntsb_panel,
                               ops_features  = av_ops_features,
                               aids_panel_df = aids_panel,
                               atc_scope     = "local_terminal",
                               missing_climate = "exclude",
                               min_reports   = MIN_REPORTS_DEFAULT,
                               period        = "quarter",
                               max_holdover_days = 365L,
                               climate_base  = "overall_final_score")
}

# MSHA (coal)
MSHA_EVENTS <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/events.parquet"
MSHA_OPS    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/ops_mine_quarter.parquet"
MSHA_COV    <- "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/data/processed/msha/covariates_mine_quarter.parquet"
msha_events <- read_parquet(MSHA_EVENTS) %>%
  mutate(mine_id = as.character(mine_id), eid = as.character(eid),
         event_date = as.Date(accident_date),
         commodity = tolower(trimws(commodity))) %>%
  filter(!is.na(mine_id), !is.na(event_date), commodity == "coal")
msha_ops <- read_parquet(MSHA_OPS) %>%
  mutate(mine_id = as.character(mine_id), commodity = tolower(trimws(commodity))) %>%
  filter(commodity == "coal")
msha_cov <- read_parquet(MSHA_COV) %>% mutate(mine_id = as.character(mine_id))

msha_panel_data_fn <- function(parquet_path, config_id) {
  prepare_msha_panel_data(parquet_path  = parquet_path,
                          config_id     = config_id,
                          events_df     = msha_events,
                          ops_raw       = msha_ops,
                          covariates_df = msha_cov,
                          min_reports   = 30L,
                          max_holdover_days = 365L,
                          climate_base  = "overall_final_score")
}

industry_panel_fns <- list(
  rail     = rail_panel_data_fn,
  nrc      = nrc_panel_data_fn,
  aviation = aviation_panel_data_fn,
  msha     = msha_panel_data_fn
)
industry_cfg_dirs <- list(
  rail     = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/rail_04-14-2026",
  nrc      = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/nrc_04-14-2026",
  aviation = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/asrs_05-01-2026",
  msha     = "/Users/michaelrosen/Documents/dev/DynamicNarrativeClimateToolkit/dynclim/notebooks/checkpoints/msha_06-01-2026"
)
industry_org_var <- list(rail = "org_id", nrc = "facility_site", aviation = "airport_id",
                         msha = "mine_id")

#' Summarize one panel.
summarize_panel <- function(panel, outcome_col, offset_col, org_col) {
  if (!outcome_col %in% names(panel)) {
    return(tibble(n_cells = nrow(panel), n_orgs = n_distinct(panel[[org_col]]),
                   panel_min = as.Date(NA), panel_max = as.Date(NA),
                   n_nonzero_outcome = NA_integer_, total_outcome = NA_real_,
                   pct_zero = NA_real_,
                   notes = sprintf("outcome col '%s' not in panel",
                                    outcome_col)))
  }
  d <- panel %>%
    filter(!is.na(.data[[outcome_col]]),
            !is.na(.data[[offset_col]]),
            .data[[offset_col]] > 0)
  # Panel time column varies by industry: yearmonth (rail/aviation) or
  # quarter_start (NRC).
  time_col <- intersect(c("yearmonth", "quarter_start"), names(d))[[1]]
  tibble(
    n_cells           = nrow(d),
    n_orgs            = dplyr::n_distinct(d[[org_col]]),
    panel_min         = suppressWarnings(min(as.Date(d[[time_col]]), na.rm = TRUE)),
    panel_max         = suppressWarnings(max(as.Date(d[[time_col]]), na.rm = TRUE)),
    n_nonzero_outcome = sum(d[[outcome_col]] > 0),
    total_outcome     = sum(d[[outcome_col]], na.rm = TRUE),
    pct_zero          = round(100 * mean(d[[outcome_col]] == 0), 1),
    notes             = ""
  )
}

# Loop one panel per (industry × outcome × champion config)
panel_rows <- list()
for (i in seq_len(nrow(champion_table))) {
  ch  <- champion_table[i, ]
  ind <- ch$industry
  oc  <- ch$outcome
  cid <- as.character(ch$best_config_id)

  oc_var_row <- outcome_var_map %>%
    filter(industry == ind, outcome == oc)
  if (nrow(oc_var_row) != 1) {
    warning(sprintf("Skipping %s|%s — no outcome_var mapping", ind, oc))
    next
  }
  outcome_var <- oc_var_row$outcome_var[1]

  panel_fn <- industry_panel_fns[[ind]]
  cfg_dir  <- industry_cfg_dirs[[ind]]
  org_col  <- industry_org_var[[ind]]

  parquet_path <- file.path(cfg_dir, "_cfg", cid, "results.parquet")
  cat(sprintf("  [%s | %s] config %s ... ", ind, oc, cid))
  panel <- tryCatch(panel_fn(parquet_path = parquet_path, config_id = cid),
                    error = function(e) { cat("FAILED\n"); NULL })
  if (is.null(panel) || nrow(panel) == 0) {
    panel_rows[[length(panel_rows) + 1L]] <- tibble(
      industry = ind, outcome = oc, config_id = cid,
      outcome_var = outcome_var, offset_var = NA_character_,
      n_cells = NA_integer_, n_orgs = NA_integer_,
      panel_min = as.Date(NA), panel_max = as.Date(NA),
      n_nonzero_outcome = NA_integer_, total_outcome = NA_real_,
      pct_zero = NA_real_, notes = "panel prep failed/empty"
    )
    next
  }

  # Pick the offset column from the panel based on outcome (matches
  # 5_manuscript_plots.R's spec_map)
  # Offset is per-industry (rail injuries is the one cell-level exception).
  offset_var <- if (ind == "rail" && oc == "rate_injuries") "staff_hours" else
    switch(ind, rail = "train_miles", nrc = "days_at_power_total",
           aviation = "departures", msha = "hours_worked", NA_character_)

  summ <- summarize_panel(panel, outcome_var, offset_var, org_col)
  panel_rows[[length(panel_rows) + 1L]] <- tibble(
    industry = ind, outcome = oc, config_id = cid,
    outcome_var = outcome_var, offset_var = offset_var,
    summ
  )
  cat(sprintf("n_cells=%d n_orgs=%d non_zero=%d\n",
              summ$n_cells, summ$n_orgs, summ$n_nonzero_outcome))
}

panel_summary <- bind_rows(panel_rows)
readr::write_csv(panel_summary,
                 file.path(OUT_DIR, "table1_panel_summary.csv"))
cat(sprintf("  Wrote %s (%d rows)\n",
            file.path(OUT_DIR, "table1_panel_summary.csv"),
            nrow(panel_summary)))


cat("\n=== Table 1 done ===\n")
cat(sprintf("  Outputs in: %s/\n", OUT_DIR))
cat("    table1_sources_long.csv\n")
cat("    table1_wide.csv\n")
cat("    table1_panel_summary.csv\n")
