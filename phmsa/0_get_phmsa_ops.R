# =============================================================================
# phmsa/0_get_phmsa_ops.R — PHMSA Operator Mileage Processing  [DEPRECATED]
# =============================================================================
#
# *** DEPRECATED 2026-04-29 ***
# This script has been superseded by the Python ops pipeline at:
#   dynclim/data/phmsa_ops_pipeline.py
#
# The Python pipeline produces the same operator_annual_ops.parquet (same
# schema, same Mundlak between/within decomposition) and is the canonical
# preprocessing step going forward. This R script is kept as a fallback for
# users without Python and may be removed in a future cleanup.
#
# To regenerate the parquet, prefer:
#   python3 dynclim/data/phmsa_ops_pipeline.py \
#       --base-dir dynclim/data/raw/phmsa/operator_data \
#       --output   dynclim/data/processed/phmsa --summary
# =============================================================================
#
# Loads PHMSA annual report CSV files for each pipeline type, extracts the
# total mileage denominator per operator per year, computes the between/within
# decomposition of log(mileage), and saves a single parquet for all three types.
#
# Sources:
#   GD — annual_gas_distribution_2010_present 2/GD AR {year}.csv
#         Key column: MMILES_TOTAL
#   GT — annual_gas_transmission_gathering_2010_present/GT AR {year} Part A to D.csv
#         Key column: PARTDTOTALMILES  (Part D grand-total: transmission + gathering;
#         the only column populated consistently across 2017-2025. PARTB192MILESTOTAL
#         is empty pre-2021 and ~71% zero from 2021+ — Part 192 transmission only.)
#   HL — annual_hazardous_liquid_2010_present/HL AR {year} Part A to E.csv
#         Key column: PARTDTOTALMILES
#
# NOTE: gravity_regulated incidents (~540) have no corresponding annual report
# files in this dataset; they are excluded from ops coverage.
#
# Output: data/phmsa/operator_annual_ops.parquet
#   Columns: OPERATOR_ID, REPORT_YEAR, pipeline_type, total_miles,
#             total_miles_between, total_miles_within
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(arrow)
  library(stringr)
  library(slider)
  library(purrr)
})

# =============================================================================
# PATHS
# =============================================================================

GD_DIR  <- here::here("data/phmsa/operator_data/annual_gas_distribution_2010_present 2")
GT_DIR  <- here::here("data/phmsa/operator_data/annual_gas_transmission_gathering_2010_present")
HL_DIR  <- here::here("data/phmsa/operator_data/annual_hazardous_liquid_2010_present")
OUT_PATH <- here::here("data/phmsa/operator_annual_ops.parquet")

# Years with CSV files (xlsx available earlier but not parsed here)
GD_YEARS <- 2010:2025
GT_YEARS <- 2010:2025
HL_YEARS <- 2010:2024

# Between/within decomposition parameters (annual data — roll in years)
OPS_ROLL_K  <- 3L   # 3-year rolling window
OPS_LAG_K   <- 1L   # 1-year lag (use prior year's report)
OPS_MIN_HIST <- 2L  # minimum 2 years of history

cat("=============================================================\n")
cat("  PHMSA OPERATOR OPS PROCESSING\n")
cat("=============================================================\n\n")


# =============================================================================
# SECTION 1 — LOAD EACH PIPELINE TYPE
# =============================================================================

read_col_safe <- function(path, col_name) {
  # Read one column + identifiers from a PHMSA annual report CSV
  tryCatch({
    df <- read_csv(path,
      col_types = cols(
        OPERATOR_ID  = col_integer(),
        REPORT_YEAR  = col_integer(),
        !!col_name   := col_double(),
        .default     = col_skip()
      ),
      show_col_types = FALSE
    )
    if (!col_name %in% names(df)) {
      warning(sprintf("Column '%s' not found in %s", col_name, basename(path)))
      return(NULL)
    }
    df %>%
      filter(!is.na(OPERATOR_ID), !is.na(REPORT_YEAR)) %>%
      rename(total_miles = !!col_name)
  }, error = function(e) {
    warning(sprintf("Failed to read %s: %s", basename(path), conditionMessage(e)))
    NULL
  })
}


#' Read one column from a pre-2017 PHMSA annual-report xlsx file.
#'
#' Pre-2017 reports come as single-sheet xlsx with two documentation rows
#' before the header (row 1 = "One operator can have multiple reports per
#' year (...)", row 2 = "Records sorted by ..."). Real column names start at
#' row 3. The mile column names match the post-2017 CSVs.
read_xlsx_col_safe <- function(path, col_name) {
  tryCatch({
    df <- suppressWarnings(read_excel(path, skip = 2L,
                                       .name_repair = "minimal"))
    needed <- c("OPERATOR_ID", "REPORT_YEAR", col_name)
    missing <- setdiff(needed, names(df))
    if (length(missing) > 0) {
      warning(sprintf("xlsx %s missing columns: %s",
                      basename(path), paste(missing, collapse = ", ")))
      return(NULL)
    }
    df %>%
      transmute(
        OPERATOR_ID = suppressWarnings(as.integer(.data[["OPERATOR_ID"]])),
        REPORT_YEAR = suppressWarnings(as.integer(.data[["REPORT_YEAR"]])),
        total_miles = suppressWarnings(as.numeric(.data[[col_name]]))
      ) %>%
      filter(!is.na(OPERATOR_ID), !is.na(REPORT_YEAR))
  }, error = function(e) {
    warning(sprintf("Failed to read %s: %s", basename(path), conditionMessage(e)))
    NULL
  })
}


#' Resolve the source file for one (pipeline_type, year), preferring CSV when
#' present (post-2017) and falling back to xlsx (2010–2016).
#'
#' @return list(path, format) or NULL if neither file exists.
.resolve_phmsa_source <- function(pipeline_type, yr) {
  csv_path <- switch(pipeline_type,
    "GD" = file.path(GD_DIR, sprintf("GD AR %d.csv", yr)),
    "GT" = file.path(GT_DIR, sprintf("GT AR %d Part A to D.csv", yr)),
    "HL" = file.path(HL_DIR, sprintf("HL AR %d Part A to E.csv", yr)),
    stop("Unknown pipeline type: ", pipeline_type)
  )
  xlsx_path <- switch(pipeline_type,
    "GD" = file.path(GD_DIR, sprintf("annual_gas_distribution_%d.xlsx", yr)),
    "GT" = file.path(GT_DIR, sprintf("annual_gas_transmission_gathering_%d.xlsx", yr)),
    "HL" = file.path(HL_DIR, sprintf("annual_hazardous_liquid_%d.xlsx", yr))
  )
  if (file.exists(csv_path))  return(list(path = csv_path,  format = "csv"))
  if (file.exists(xlsx_path)) return(list(path = xlsx_path, format = "xlsx"))
  NULL
}


#' Read one (pipeline_type, year) regardless of source file format.
.read_phmsa_year <- function(pipeline_type, yr, col_name) {
  src <- .resolve_phmsa_source(pipeline_type, yr)
  if (is.null(src)) {
    message(sprintf("  Missing: %s %d (no csv or xlsx found)", pipeline_type, yr))
    return(NULL)
  }
  df <- if (src$format == "csv") {
    read_col_safe(src$path, col_name)
  } else {
    read_xlsx_col_safe(src$path, col_name)
  }
  if (!is.null(df)) df$REPORT_YEAR <- yr
  df
}

# --- Gas Distribution ---
# 2010-2016 are .xlsx; 2017+ are .csv. .read_phmsa_year() picks the right
# format automatically.
cat("--- Loading GD annual reports (csv 2017+ + xlsx 2010-2016) ---\n")
gd_raw <- map_dfr(GD_YEARS, ~ .read_phmsa_year("GD", .x, "MMILES_TOTAL")) %>%
  mutate(pipeline_type = "gas_distribution")
cat(sprintf("  GD rows: %s | operators: %d | years: %d-%d\n",
    format(nrow(gd_raw), big.mark = ","),
    n_distinct(gd_raw$OPERATOR_ID),
    min(gd_raw$REPORT_YEAR), max(gd_raw$REPORT_YEAR)))

# --- Gas Transmission ---
cat("--- Loading GT annual reports (csv 2017+ + xlsx 2010-2016) ---\n")
gt_raw <- map_dfr(GT_YEARS, ~ .read_phmsa_year("GT", .x, "PARTDTOTALMILES")) %>%
  mutate(pipeline_type = "gas_transmission")
cat(sprintf("  GT rows: %s | operators: %d | years: %d-%d\n",
    format(nrow(gt_raw), big.mark = ","),
    n_distinct(gt_raw$OPERATOR_ID),
    min(gt_raw$REPORT_YEAR), max(gt_raw$REPORT_YEAR)))

# --- Hazardous Liquid ---
cat("--- Loading HL annual reports (csv 2017+ + xlsx 2010-2016) ---\n")
hl_raw <- map_dfr(HL_YEARS, ~ .read_phmsa_year("HL", .x, "PARTDTOTALMILES")) %>%
  mutate(pipeline_type = "hazardous_liquid")
cat(sprintf("  HL rows: %s | operators: %d | years: %d-%d\n",
    format(nrow(hl_raw), big.mark = ","),
    n_distinct(hl_raw$OPERATOR_ID),
    min(hl_raw$REPORT_YEAR), max(hl_raw$REPORT_YEAR)))


# =============================================================================
# SECTION 2 — CLEAN AND STACK
# =============================================================================

cat("\n--- Combining and cleaning ---\n")

# Some operators file multiple supplemental reports per year — keep the
# row with the largest mileage (final/amended report supersedes original)
ops_raw <- bind_rows(gd_raw, gt_raw, hl_raw) %>%
  filter(!is.na(total_miles), total_miles >= 0) %>%
  group_by(OPERATOR_ID, REPORT_YEAR, pipeline_type) %>%
  slice_max(total_miles, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(pipeline_type, OPERATOR_ID, REPORT_YEAR)

cat(sprintf("  Combined: %s operator-year rows\n", format(nrow(ops_raw), big.mark = ",")))

# Distribution of total_miles by type
ops_raw %>%
  group_by(pipeline_type) %>%
  summarise(
    n_op_years   = n(),
    n_operators  = n_distinct(OPERATOR_ID),
    median_miles = round(median(total_miles, na.rm = TRUE), 1),
    max_miles    = round(max(total_miles, na.rm = TRUE)),
    pct_zero     = round(100 * mean(total_miles == 0), 1),
    .groups = "drop"
  ) %>%
  print()


# =============================================================================
# SECTION 3 — BETWEEN/WITHIN DECOMPOSITION (annual rolling)
# =============================================================================
# Mirrors make_ops_features_rolling() in rail/data_prep.R, adapted for
# annual (year-indexed) data rather than monthly.
#
# Steps per operator × pipeline_type:
#   1. log(1 + total_miles)
#   2. Lag 1 year
#   3. 3-year rolling mean of lagged values  → between (stable scale)
#   4. Deviation from rolling mean           → within (annual fluctuation)
#   5. Standardise across all operators; NA out rows with < OPS_MIN_HIST years
# =============================================================================

cat("\n--- Computing between/within features ---\n")

make_phmsa_ops_features <- function(df,
                                     roll_k   = OPS_ROLL_K,
                                     lag_k    = OPS_LAG_K,
                                     min_hist = OPS_MIN_HIST) {
  df %>%
    arrange(pipeline_type, OPERATOR_ID, REPORT_YEAR) %>%
    group_by(pipeline_type, OPERATOR_ID) %>%
    mutate(
      miles_log      = log1p(total_miles),
      miles_lag      = dplyr::lag(miles_log, lag_k),
      miles_between_raw = slider::slide_dbl(
        miles_lag, mean, na.rm = TRUE,
        .before = roll_k - 1L, .complete = FALSE
      ),
      miles_within_raw  = miles_lag - miles_between_raw,
      .hist_n           = row_number()
    ) %>%
    ungroup() %>%
    mutate(
      total_miles_between = ifelse(
        .hist_n <= min_hist, NA_real_,
        as.vector(scale(miles_between_raw))
      ),
      total_miles_within = ifelse(
        .hist_n <= min_hist, NA_real_,
        as.vector(scale(miles_within_raw))
      )
    ) %>%
    select(-miles_log, -miles_lag, -miles_between_raw,
           -miles_within_raw, -.hist_n)
}

ops_features <- make_phmsa_ops_features(ops_raw)

# Sanity check
ops_check <- ops_features %>%
  filter(!is.na(total_miles_between)) %>%
  group_by(pipeline_type) %>%
  summarise(
    n_op_years        = n(),
    n_operators       = n_distinct(OPERATOR_ID),
    pct_na_between    = round(100 * mean(is.na(total_miles_between)), 1),
    .groups = "drop"
  )
cat("  Feature coverage by pipeline type:\n")
print(ops_check)


# =============================================================================
# SECTION 4 — MATCH RATE CHECK VS EVENTS
# =============================================================================

cat("\n--- Checking match rate vs events.parquet ---\n")

events <- read_parquet(here::here("data/phmsa/events.parquet")) %>%
  mutate(
    OPERATOR_ID = as.integer(operator_raw_id),
    event_year  = as.integer(format(as.Date(event_date), "%Y")),
    join_year   = event_year - 1L    # use prior year's report
  ) %>%
  filter(!is.na(OPERATOR_ID), !is.na(event_year))

# Join events to ops by OPERATOR_ID + join_year = REPORT_YEAR
events_joined <- events %>%
  left_join(
    ops_features %>% select(OPERATOR_ID, REPORT_YEAR, pipeline_type,
                             total_miles, total_miles_between, total_miles_within),
    by = c("OPERATOR_ID", "join_year" = "REPORT_YEAR",
           "pipeline_type")
  )

match_summary <- events_joined %>%
  group_by(pipeline_type) %>%
  summarise(
    n_events    = n(),
    n_matched   = sum(!is.na(total_miles)),
    pct_matched = round(100 * mean(!is.na(total_miles)), 1),
    .groups = "drop"
  )
cat("  Match rate (prior-year ops join):\n")
print(match_summary)


# =============================================================================
# SECTION 5 — SAVE
# =============================================================================

cat("\n--- Saving ---\n")
write_parquet(ops_features, OUT_PATH)
cat(sprintf("  Saved: %s\n", OUT_PATH))
cat(sprintf("  Rows: %s | Columns: %s\n",
    format(nrow(ops_features), big.mark = ","),
    paste(names(ops_features), collapse = ", ")))

cat("\n=============================================================\n")
cat("  PHMSA OPS PROCESSING COMPLETE\n")
cat("=============================================================\n")
