# =============================================================================
# aviation/1_get_bts_t100.R — BTS T100 Operational Data Download & Processing
# =============================================================================
#
# Downloads BTS Form 41 T100 Domestic Segment data (US carriers), aggregates
# to airport × month, and computes the between/within decomposition of
# log(departures) used as the operational covariate in the aviation model.
#
# Output: data/aviation/bts_t100/airport_month_ops.parquet
#
# MANUAL DOWNLOAD FALLBACK:
#   If the programmatic download fails, download CSV files from:
#     https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FIL
#   Select fields: YEAR, MONTH, UNIQUE_CARRIER, ORIGIN,
#                  DEPARTURES_PERFORMED, SEATS, PASSENGERS
#   Download one file per year (or use "All Years") and place the CSV(s) in:
#     data/aviation/bts_t100/raw/
#   Then run this script from the PROCESS section onward (skip download).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(arrow)
  library(httr2)
  library(lubridate)
  library(stringr)
  library(slider)
  library(purrr)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

RAW_DIR     <- here::here("data/aviation/bts_t100/raw")
OUT_PATH    <- here::here("data/aviation/bts_t100/airport_month_ops.parquet")

# Year range: match ASRS coverage (1990 = first full year with reasonable ATC volume)
YEARS       <- 1990:2023

# Operational variables to carry through
OPS_VARS    <- c("departures", "seats", "passengers")

# Between/within decomposition parameters (mirrors rail/config.R)
OPS_ROLL_K  <- 12L   # 12-month rolling window
OPS_LAG_K   <- 1L    # 1-month lag
OPS_MIN_HIST <- 6L   # minimum 6 months before features are valid

# BTS T100 Domestic Segment — field names in the downloaded CSV
BTS_FIELDS  <- c(
  "YEAR", "MONTH", "UNIQUE_CARRIER", "ORIGIN",
  "DEPARTURES_PERFORMED", "SEATS", "PASSENGERS"
)

dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

cat("=============================================================\n")
cat("  BTS T100 DOWNLOAD & PROCESSING\n")
cat("=============================================================\n\n")


# =============================================================================
# SECTION 1 — DOWNLOAD (programmatic via BTS TranStats POST API)
# =============================================================================
# BTS TranStats serves T100 data through a form POST to their download endpoint.
# Each request fetches one year; results are returned as a zipped CSV.
#
# Table_ID 258 = T100 Domestic Segment (US Carriers Only).
# If the download returns an error page instead of data, fall back to the
# manual download instructions at the top of this file.
# =============================================================================

BTS_ENDPOINT <- "https://www.transtats.bts.gov/DownLoad_Table.asp"
BTS_TABLE_ID <- "258"

bts_field_string <- paste(
  "YEAR", "MONTH", "UNIQUE_CARRIER", "ORIGIN",
  "DEPARTURES_PERFORMED", "SEATS", "PASSENGERS",
  sep = "%2C"
)

download_bts_year <- function(year) {
  out_zip <- file.path(RAW_DIR, sprintf("t100_domestic_%d.zip", year))
  out_csv <- file.path(RAW_DIR, sprintf("t100_domestic_%d.csv", year))

  if (file.exists(out_csv)) {
    message(sprintf("  %d: cached, skipping download", year))
    return(invisible(out_csv))
  }

  message(sprintf("  %d: downloading...", year), appendLF = FALSE)

  resp <- tryCatch(
    request(BTS_ENDPOINT) %>%
      req_method("POST") %>%
      req_body_form(
        Table_ID        = BTS_TABLE_ID,
        UserTableName   = "T_T100D_SEGMENT_US_CARRIER_ONLY",
        DBShortName     = "",
        RawDataTable    = "T_T100D_SEGMENT_US_CARRIER_ONLY",
        sqlstr          = sprintf(
          "SELECT %s FROM T_T100D_SEGMENT_US_CARRIER_ONLY WHERE YEAR=%d",
          paste(BTS_FIELDS, collapse = ","), year
        ),
        varlist         = bts_field_string,
        grouplist       = "",
        suml            = "",
        sumregion       = "",
        filter1         = "title=",
        filter2         = "title=",
        geo             = "All",
        time            = sprintf("%d", year),
        timename        = "Year",
        GEOGRAPHY       = "All",
        ITYP_CD         = "All",
        VarDesc         = "Air Carrier Statistics",
        TransModalYear  = "",
        TransModYtD     = "",
        ItineraryID     = ""
      ) %>%
      req_timeout(120) %>%
      req_perform(),
    error = function(e) {
      message(sprintf(" FAILED: %s", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(resp) || resp_status(resp) != 200) {
    message(sprintf(" HTTP error — check BTS site or use manual download for %d", year))
    return(invisible(NULL))
  }

  # Write zip and unzip
  writeBin(resp_body_raw(resp), out_zip)

  unzip_result <- tryCatch(
    unzip(out_zip, exdir = RAW_DIR),
    error = function(e) NULL
  )

  if (!is.null(unzip_result)) {
    # BTS names the CSV inside the zip; rename to our convention
    csv_in_zip <- unzip_result[grepl("\\.csv$", unzip_result, ignore.case = TRUE)][1]
    if (!is.na(csv_in_zip) && file.exists(csv_in_zip)) {
      file.rename(csv_in_zip, out_csv)
    }
    file.remove(out_zip)
  }

  if (file.exists(out_csv)) {
    size_mb <- round(file.info(out_csv)$size / 1e6, 1)
    message(sprintf(" done (%s MB)", size_mb))
  } else {
    message(" download succeeded but CSV not found — check zip contents")
  }

  invisible(out_csv)
}

# Only attempt download if no CSVs already present
existing_csvs <- list.files(RAW_DIR, pattern = "\\.csv$", full.names = TRUE)

if (length(existing_csvs) == 0) {
  cat("--- Downloading BTS T100 data (one request per year) ---\n")
  walk(YEARS, download_bts_year)
  existing_csvs <- list.files(RAW_DIR, pattern = "\\.csv$", full.names = TRUE)
} else {
  cat(sprintf("--- Found %d CSV file(s) in raw dir, skipping download ---\n",
              length(existing_csvs)))
}

if (length(existing_csvs) == 0) {
  stop(
    "No CSV files found in ", RAW_DIR, "\n",
    "Download manually from:\n",
    "  https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FIL\n",
    "Fields: YEAR, MONTH, UNIQUE_CARRIER, ORIGIN, ",
    "DEPARTURES_PERFORMED, SEATS, PASSENGERS\n",
    "Place CSV(s) in: ", RAW_DIR
  )
}


# =============================================================================
# SECTION 2 — READ AND AGGREGATE TO AIRPORT × MONTH
# =============================================================================

cat("\n--- Reading and aggregating T100 files ---\n")

# Year is not a column in the BTS CSV — extract it from the filename.
# Expected pattern: "T_T100D_SEGMENT_US_CARRIER_ONLY 2004.csv"
extract_year_from_path <- function(path) {
  yr <- str_extract(basename(path), "\\d{4}(?=\\.csv$)")
  if (is.na(yr)) stop(sprintf("Cannot extract year from filename: %s", basename(path)))
  as.integer(yr)
}

read_t100_csv <- function(path) {
  yr <- extract_year_from_path(path)

  df <- read_csv(
    path,
    col_types = cols(
      MONTH                = col_integer(),
      UNIQUE_CARRIER       = col_character(),
      ORIGIN               = col_character(),
      DEPARTURES_PERFORMED = col_double(),
      SEATS                = col_double(),
      PASSENGERS           = col_double(),
      .default             = col_skip()
    ),
    show_col_types = FALSE
  )

  df %>%
    mutate(YEAR = yr) %>%
    filter(!is.na(MONTH), !is.na(ORIGIN), ORIGIN != "")
}

# Only read files whose filename year falls within YEARS range
existing_csvs <- existing_csvs[
  !is.na(str_extract(basename(existing_csvs), "\\d{4}(?=\\.csv$)")) &
  as.integer(str_extract(basename(existing_csvs), "\\d{4}(?=\\.csv$)")) %in% YEARS
]

cat(sprintf("  Files to read: %d (%d–%d)\n",
    length(existing_csvs),
    min(as.integer(str_extract(basename(existing_csvs), "\\d{4}(?=\\.csv$)"))),
    max(as.integer(str_extract(basename(existing_csvs), "\\d{4}(?=\\.csv$)")))
))

raw_t100 <- map_dfr(existing_csvs, read_t100_csv)

cat(sprintf("  Raw rows loaded: %s\n", format(nrow(raw_t100), big.mark = ",")))
cat(sprintf("  Year range: %d – %d\n", min(raw_t100$YEAR), max(raw_t100$YEAR)))
cat(sprintf("  Unique airports: %d\n", n_distinct(raw_t100$ORIGIN)))

# Aggregate by airport × month (sum across all carriers)
airport_month_raw <- raw_t100 %>%
  group_by(airport_id = str_trim(str_to_upper(ORIGIN)), year = YEAR, month = MONTH) %>%
  summarise(
    departures = sum(DEPARTURES_PERFORMED, na.rm = TRUE),
    seats      = sum(SEATS,               na.rm = TRUE),
    passengers = sum(PASSENGERS,           na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(yearmonth = as.Date(sprintf("%04d-%02d-01", year, month)))

cat(sprintf("  Airport × month rows: %s\n", format(nrow(airport_month_raw), big.mark = ",")))
cat(sprintf("  Airports with any departures: %d\n",
    n_distinct(airport_month_raw$airport_id[airport_month_raw$departures > 0])))


# =============================================================================
# SECTION 3 — BETWEEN/WITHIN DECOMPOSITION
# =============================================================================
# Mirrors the make_ops_features_rolling() function in rail/data_prep.R.
# For each variable: log(1 + x) → lag 1 month → 12-month rolling mean
# (between) → deviation from mean (within) → standardise across airports.
# =============================================================================

cat("\n--- Computing between/within operational features ---\n")

make_aviation_ops_features <- function(df,
                                        vars     = OPS_VARS,
                                        lag_k    = OPS_LAG_K,
                                        roll_k   = OPS_ROLL_K,
                                        min_hist = OPS_MIN_HIST) {

  df <- df %>%
    arrange(airport_id, yearmonth) %>%
    group_by(airport_id)

  # Step 1: log(1+x)
  for (v in vars) {
    df <- df %>% mutate(!!paste0(v, "_log") := log1p(.data[[v]]))
  }

  # Step 2: lag
  for (v in vars) {
    df <- df %>%
      mutate(!!paste0(v, "_lag") := dplyr::lag(.data[[paste0(v, "_log")]], lag_k))
  }

  # Step 3: rolling mean of lagged values (between)
  for (v in vars) {
    df <- df %>%
      mutate(
        !!paste0(v, "_between_raw") := slider::slide_dbl(
          .data[[paste0(v, "_lag")]], mean, na.rm = TRUE,
          .before = roll_k - 1L, .complete = FALSE
        )
      )
  }

  # Step 4: within = lagged - rolling mean
  for (v in vars) {
    df <- df %>%
      mutate(
        !!paste0(v, "_within_raw") :=
          .data[[paste0(v, "_lag")]] - .data[[paste0(v, "_between_raw")]]
      )
  }

  # Step 5: history counter (for min_hist gating)
  df <- df %>% mutate(.hist_n = row_number()) %>% ungroup()

  # Step 6: standardise across all airports then gate short histories
  for (v in vars) {
    between_raw <- paste0(v, "_between_raw")
    within_raw  <- paste0(v, "_within_raw")
    between_col <- paste0(v, "_between")
    within_col  <- paste0(v, "_within")

    df <- df %>%
      mutate(
        !!between_col := ifelse(
          .hist_n <= min_hist, NA_real_,
          as.vector(scale(.data[[between_raw]]))
        ),
        !!within_col := ifelse(
          .hist_n <= min_hist, NA_real_,
          as.vector(scale(.data[[within_raw]]))
        )
      )
  }

  # Drop intermediates
  df %>% select(
    -ends_with("_log"), -ends_with("_lag"),
    -ends_with("_between_raw"), -ends_with("_within_raw"),
    -.hist_n
  )
}

airport_month_ops <- make_aviation_ops_features(airport_month_raw)

# Quick sanity check
ops_check <- airport_month_ops %>%
  filter(!is.na(departures_between)) %>%
  summarise(
    n_airports     = n_distinct(airport_id),
    n_airport_months = n(),
    pct_na_between = round(100 * mean(is.na(departures_between)), 1)
  )
cat("  Features summary:\n")
print(as.data.frame(ops_check))


# =============================================================================
# SECTION 4 — COVERAGE DIAGNOSTICS
# =============================================================================

cat("\n--- Coverage diagnostics ---\n")

# Load audit feasibility list to check overlap
feasibility_path <- here::here("aviation/audit_output/linked_airport_feasibility.csv")
if (file.exists(feasibility_path)) {
  feasible_airports <- read_csv(feasibility_path, show_col_types = FALSE) %>%
    pull(airport_id) %>%
    str_to_upper()

  t100_airports <- str_to_upper(unique(airport_month_ops$airport_id))

  in_t100   <- intersect(feasible_airports, t100_airports)
  not_t100  <- setdiff(feasible_airports, t100_airports)

  cat(sprintf("  Linked airports (from audit):          %d\n", length(feasible_airports)))
  cat(sprintf("  Linked airports also in T100:          %d\n", length(in_t100)))
  cat(sprintf("  Linked airports NOT in T100:           %d\n", length(not_t100)))

  if (length(not_t100) > 0) {
    cat(sprintf("  Missing from T100 (GA/military likely): %s\n",
                paste(head(sort(not_t100), 20), collapse = ", ")))
  }

  # T100 departures stats for airports that make our min-reports threshold
  top_airports <- feasible_airports[feasible_airports %in% t100_airports]
  ops_top <- airport_month_ops %>%
    filter(airport_id %in% top_airports) %>%
    group_by(airport_id) %>%
    summarise(
      mean_monthly_deps = round(mean(departures, na.rm = TRUE)),
      total_months      = n(),
      pct_months_data   = round(100 * mean(!is.na(departures_between))),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_monthly_deps))

  cat("\n  Monthly departure stats for linked + T100 airports (top 25):\n")
  print(head(ops_top, 25))
}


# =============================================================================
# SECTION 5 — SAVE OUTPUT
# =============================================================================

cat("\n--- Saving output ---\n")

dir.create(dirname(OUT_PATH), showWarnings = FALSE, recursive = TRUE)
write_parquet(airport_month_ops, OUT_PATH)

cat(sprintf("  Saved: %s\n", OUT_PATH))
cat(sprintf("  Rows: %s | Columns: %d\n",
    format(nrow(airport_month_ops), big.mark = ","),
    ncol(airport_month_ops)))
cat(sprintf("  Columns: %s\n", paste(names(airport_month_ops), collapse = ", ")))

cat("\n=============================================================\n")
cat("  BTS T100 PROCESSING COMPLETE\n")
cat("=============================================================\n")
