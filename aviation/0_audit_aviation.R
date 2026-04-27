# =============================================================================
# aviation/0_audit_aviation.R — Pre-Analysis Data Audit
# =============================================================================
#
# Assesses feasibility of the aviation safety climate analysis before building
# the full pipeline. Answers:
#   1. ASRS: Which function values identify ATC reporters?
#   2. ASRS: How dense is the airport × month panel for ATC reports?
#   3. NTSB: What is the distribution of apt_dist, ev_type, ev_highest_injury?
#   4. NTSB: Airport-month accident base rate and severity breakdown.
#   5. Linkage: Airport code format comparison and overlap between datasets.
#   6. Panel feasibility: Airports with enough ASRS reports for windowing.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(ggplot2)
})

# =============================================================================
# PATHS
# =============================================================================

ASRS_CSV_PATH   <- here::here("data/aviation/asrs_dict_df.csv")
NTSB_POST_PATH  <- here::here("data/aviation/ntsb_av_accident_data/events.xlsx")
NTSB_PRE_PATH   <- here::here("data/aviation/ntsb_av_accident_data/events_pre2008.xlsx")
AUDIT_OUT_DIR   <- here::here("aviation/audit_output")

dir.create(AUDIT_OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("=============================================================\n")
cat("  AVIATION DATA AUDIT\n")
cat("=============================================================\n\n")


# =============================================================================
# SECTION 1 — ASRS DATA
# =============================================================================

cat("--- SECTION 1: ASRS data loading (selecting key columns) ---\n")

# Read only the columns needed for the audit to keep memory manageable.
# The full file is ~1.2 GB / 898 columns; we need ~7 columns.
asrs_raw <- read_csv(
  ASRS_CSV_PATH,
  col_select = c(
    acn,
    time_date,
    place_locale_reference,
    place_state_reference,
    person_1function,
    person_2function,
    person_1reporter_organization
  ),
  col_types  = cols(.default = "c"),
  show_col_types = FALSE
)

cat(sprintf("  Rows loaded: %s\n\n", format(nrow(asrs_raw), big.mark = ",")))


# ---------------------------------------------------------------------------
# 1a. person_1function distribution — identify ATC positions
# ---------------------------------------------------------------------------

cat("--- 1a. person_1function value counts (all, sorted desc) ---\n")

func_counts <- asrs_raw %>%
  count(person_1function, sort = TRUE) %>%
  mutate(pct = round(100 * n / nrow(asrs_raw), 2))

print(func_counts, n = 40)

# ATC positions based on ASRS coding
ATC_FUNCTIONS_LOCAL    <- c("Local", "Ground", "Flight Data / Clearance Delivery",
                             "Handoff / Assist")
ATC_FUNCTIONS_TERMINAL <- c("Approach", "Departure")
ATC_FUNCTIONS_ENROUTE  <- c("Enroute")

ATC_FUNCTIONS_ALL <- c(ATC_FUNCTIONS_LOCAL, ATC_FUNCTIONS_TERMINAL, ATC_FUNCTIONS_ENROUTE)

# Exact-match on the full semicolon-delimited string (a report can list multiple functions)
is_atc_func <- function(x, targets) {
  sapply(x, function(v) {
    parts <- str_split(v, ";\\s*")[[1]]
    any(trimws(parts) %in% targets)
  })
}

asrs_raw <- asrs_raw %>%
  mutate(
    atc_local    = is_atc_func(person_1function, ATC_FUNCTIONS_LOCAL),
    atc_terminal = is_atc_func(person_1function, ATC_FUNCTIONS_TERMINAL),
    atc_enroute  = is_atc_func(person_1function, ATC_FUNCTIONS_ENROUTE),
    atc_any      = atc_local | atc_terminal | atc_enroute
  )

cat("\n--- 1b. ATC reporter counts by tier ---\n")
atc_summary <- tribble(
  ~tier,       ~n,
  "Local (tower)",        sum(asrs_raw$atc_local,    na.rm = TRUE),
  "Terminal (TRACON)",    sum(asrs_raw$atc_terminal, na.rm = TRUE),
  "Enroute (ARTCC)",      sum(asrs_raw$atc_enroute,  na.rm = TRUE),
  "Any ATC",              sum(asrs_raw$atc_any,       na.rm = TRUE),
  "Non-ATC",              sum(!asrs_raw$atc_any,      na.rm = TRUE)
) %>% mutate(pct_of_total = round(100 * n / nrow(asrs_raw), 2))
print(atc_summary)


# ---------------------------------------------------------------------------
# 1c. Airport code format in ASRS (place_locale_reference)
# ---------------------------------------------------------------------------

cat("\n--- 1c. Airport code format (place_locale_reference) in ATC subset ---\n")

asrs_atc <- asrs_raw %>% filter(atc_any)

# Some records list multiple airports with "; " separator — parse first code
asrs_atc <- asrs_atc %>%
  mutate(
    airport_primary = str_trim(str_split_fixed(place_locale_reference, ";", 2)[, 1]),
    airport_primary = na_if(airport_primary, ""),
    airport_primary = na_if(airport_primary, "NA")
  )

code_lengths <- asrs_atc %>%
  filter(!is.na(airport_primary)) %>%
  mutate(code_len = nchar(airport_primary)) %>%
  count(code_len, sort = TRUE)

cat("  Code length distribution (should be 3 = FAA, 4 = ICAO):\n")
print(code_lengths)

cat(sprintf("  Records with airport code: %s / %s (%.1f%%)\n",
    format(sum(!is.na(asrs_atc$airport_primary)), big.mark = ","),
    format(nrow(asrs_atc), big.mark = ","),
    100 * mean(!is.na(asrs_atc$airport_primary))))

# Top 30 airports in ATC subset
top_airports_asrs <- asrs_atc %>%
  filter(!is.na(airport_primary)) %>%
  count(airport_primary, sort = TRUE) %>%
  head(30)

cat("\n  Top 30 airports in ATC reports:\n")
print(top_airports_asrs)


# ---------------------------------------------------------------------------
# 1d. Time coverage and date parsing
# ---------------------------------------------------------------------------

cat("\n--- 1d. ASRS time_date format and coverage ---\n")

# time_date is YYYYMM (character "198801")
asrs_atc <- asrs_atc %>%
  mutate(
    time_date_chr = str_pad(time_date, 6, side = "left", pad = "0"),
    asrs_year  = as.integer(str_sub(time_date_chr, 1, 4)),
    asrs_month = as.integer(str_sub(time_date_chr, 5, 6)),
    asrs_ym    = as.Date(paste0(asrs_year, "-", str_pad(asrs_month, 2, pad = "0"), "-01"))
  ) %>%
  filter(!is.na(asrs_ym), asrs_year >= 1988, asrs_year <= 2030,
         asrs_month >= 1, asrs_month <= 12)

cat(sprintf("  Date range: %s to %s\n",
    min(asrs_atc$asrs_ym, na.rm = TRUE),
    max(asrs_atc$asrs_ym, na.rm = TRUE)))

annual_asrs_atc <- asrs_atc %>%
  count(asrs_year) %>%
  filter(asrs_year >= 1990)
cat("  Annual ATC report counts:\n")
print(annual_asrs_atc, n = 40)


# ---------------------------------------------------------------------------
# 1e. Airport × month panel density
# ---------------------------------------------------------------------------

cat("\n--- 1e. Airport x month panel density (ATC reports) ---\n")

airport_month_asrs <- asrs_atc %>%
  filter(!is.na(airport_primary), !is.na(asrs_ym)) %>%
  count(airport_primary, asrs_ym, name = "n_reports")

# Distribution of reports per cell
density_summary <- airport_month_asrs %>%
  summarise(
    n_cells          = n(),
    n_airports       = n_distinct(airport_primary),
    n_months_total   = n_distinct(asrs_ym),
    median_reports   = median(n_reports),
    mean_reports     = round(mean(n_reports), 2),
    pct_cells_ge_1   = round(100 * mean(n_reports >= 1), 1),
    pct_cells_ge_3   = round(100 * mean(n_reports >= 3), 1),
    pct_cells_ge_5   = round(100 * mean(n_reports >= 5), 1)
  )
cat("  Airport x month cell summary:\n")
print(as.data.frame(density_summary))

# Reports per airport (total across all months)
airport_totals_asrs <- airport_month_asrs %>%
  group_by(airport_primary) %>%
  summarise(
    total_reports   = sum(n_reports),
    n_months_active = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_reports))

cat("\n  Reports per airport (top 30):\n")
print(head(airport_totals_asrs, 30))

cat(sprintf("\n  Airports with >= 75 ATC reports total: %d\n",
    sum(airport_totals_asrs$total_reports >= 75)))
cat(sprintf("  Airports with >= 50 ATC reports total: %d\n",
    sum(airport_totals_asrs$total_reports >= 50)))
cat(sprintf("  Airports with >= 30 ATC reports total: %d\n",
    sum(airport_totals_asrs$total_reports >= 30)))


# =============================================================================
# SECTION 2 — NTSB DATA
# =============================================================================

cat("\n=============================================================\n")
cat("--- SECTION 2: NTSB accident data ---\n")

ntsb_post <- read_excel(NTSB_POST_PATH) %>%
  mutate(source = "post2008")

ntsb_pre  <- read_excel(NTSB_PRE_PATH) %>%
  mutate(source = "pre2008")

# Harmonize column types before binding
shared_cols <- intersect(names(ntsb_post), names(ntsb_pre))
ntsb_post_shared <- ntsb_post %>% select(all_of(shared_cols)) %>%
  mutate(across(everything(), as.character))
ntsb_pre_shared  <- ntsb_pre  %>% select(all_of(shared_cols)) %>%
  mutate(across(everything(), as.character))

ntsb <- bind_rows(ntsb_post_shared, ntsb_pre_shared) %>%
  mutate(
    ev_date   = as.Date(ev_date),
    ev_year   = as.integer(ev_year),
    ev_month  = as.integer(ev_month),
    apt_dist  = as.numeric(apt_dist),
    inj_tot_f = as.integer(inj_tot_f),
    inj_tot_s = as.integer(inj_tot_s),
    inj_tot_m = as.integer(inj_tot_m),
    inj_tot_t = as.integer(inj_tot_t)
  )

cat(sprintf("  Total NTSB rows (pre + post 2008): %s\n",
    format(nrow(ntsb), big.mark = ",")))
cat(sprintf("  Date range: %s to %s\n",
    min(ntsb$ev_date, na.rm = TRUE),
    max(ntsb$ev_date, na.rm = TRUE)))

cat("\n  ev_type distribution:\n")
print(count(ntsb, ev_type, sort = TRUE))

cat("\n  ev_highest_injury distribution:\n")
print(count(ntsb, ev_highest_injury, sort = TRUE))


# ---------------------------------------------------------------------------
# 2a. Airport proximity filter
# ---------------------------------------------------------------------------

cat("\n--- 2a. apt_dist distribution for accidents (ev_type == 'ACC') ---\n")

acc <- ntsb %>% filter(ev_type == "ACC")
cat(sprintf("  Total accidents: %s\n", format(nrow(acc), big.mark = ",")))
cat(sprintf("  With ev_nr_apt_id not NA: %s (%.1f%%)\n",
    format(sum(!is.na(acc$ev_nr_apt_id)), big.mark = ","),
    100 * mean(!is.na(acc$ev_nr_apt_id))))
cat(sprintf("  With apt_dist not NA: %s (%.1f%%)\n",
    format(sum(!is.na(acc$apt_dist)), big.mark = ","),
    100 * mean(!is.na(acc$apt_dist))))

dist_cuts <- c(0, 1, 2, 5, 10, 15, 25, 50, Inf)
dist_labels <- c("<1", "1-2", "2-5", "5-10", "10-15", "15-25", "25-50", "50+")

acc_dist <- acc %>%
  filter(!is.na(apt_dist)) %>%
  mutate(dist_bin = cut(apt_dist, breaks = dist_cuts, labels = dist_labels,
                        right = FALSE, include.lowest = TRUE)) %>%
  count(dist_bin) %>%
  mutate(pct = round(100 * n / sum(n), 1), cumulative_pct = round(cumsum(pct), 1))

cat("\n  apt_dist breakdown for accidents (NM from nearest airport):\n")
print(acc_dist)

# Recommend cutoffs based on data
for (cutoff in c(5, 10, 15)) {
  n_within <- sum(acc$apt_dist <= cutoff, na.rm = TRUE)
  n_with_id <- sum(!is.na(acc$ev_nr_apt_id) & (is.na(acc$apt_dist) | acc$apt_dist <= cutoff))
  cat(sprintf("  apt_dist <= %d NM: %s accidents | with apt_id: %s\n",
      cutoff,
      format(n_within, big.mark = ","),
      format(n_with_id, big.mark = ",")))
}


# ---------------------------------------------------------------------------
# 2b. Airport code format in NTSB
# ---------------------------------------------------------------------------

cat("\n--- 2b. NTSB airport code format (ev_nr_apt_id) ---\n")

ntsb_code_lengths <- acc %>%
  filter(!is.na(ev_nr_apt_id)) %>%
  mutate(code_len = nchar(str_trim(ev_nr_apt_id))) %>%
  count(code_len, sort = TRUE)
cat("  ev_nr_apt_id code length distribution:\n")
print(ntsb_code_lengths)

top_airports_ntsb <- acc %>%
  filter(!is.na(ev_nr_apt_id)) %>%
  count(ev_nr_apt_id, sort = TRUE) %>%
  head(30)
cat("\n  Top 30 airports in NTSB accidents:\n")
print(top_airports_ntsb)


# ---------------------------------------------------------------------------
# 2c. Airport × month panel — accident base rates
# ---------------------------------------------------------------------------

cat("\n--- 2c. Accident base rates at airport-month level ---\n")

# Near-airport accidents: apt_dist <= 15 NM (broad; we'll multiverse the cutoff)
acc_near <- acc %>%
  filter(!is.na(ev_nr_apt_id)) %>%
  filter(is.na(apt_dist) | apt_dist <= 15) %>%
  mutate(
    ntsb_ym     = as.Date(paste0(ev_year, "-", str_pad(ev_month, 2, pad = "0"), "-01")),
    airport_id  = str_trim(ev_nr_apt_id),
    has_fatal   = as.integer(!is.na(inj_tot_f) & inj_tot_f > 0),
    has_serious = as.integer(!is.na(inj_tot_s) & inj_tot_s > 0),
    sev_ord = case_when(
      !is.na(inj_tot_f) & inj_tot_f > 0  ~ 3L,
      !is.na(inj_tot_s) & inj_tot_s > 0  ~ 2L,
      !is.na(inj_tot_m) & inj_tot_m > 0  ~ 1L,
      TRUE                                ~ 0L
    )
  )

cat(sprintf("  Near-airport accidents (apt_dist <= 15 NM or missing dist with apt_id): %s\n",
    format(nrow(acc_near), big.mark = ",")))
cat(sprintf("  Date range: %s to %s\n",
    min(acc_near$ntsb_ym, na.rm = TRUE),
    max(acc_near$ntsb_ym, na.rm = TRUE)))

# Summarise to airport × month
airport_month_ntsb <- acc_near %>%
  group_by(airport_id, ntsb_ym) %>%
  summarise(
    n_accidents    = n(),
    n_fatal        = sum(has_fatal),
    n_serious      = sum(has_serious),
    n_inj_f        = sum(inj_tot_f, na.rm = TRUE),
    n_inj_s        = sum(inj_tot_s, na.rm = TRUE),
    max_sev        = max(sev_ord),
    .groups = "drop"
  )

cat(sprintf("\n  Airport x month cells with >= 1 accident: %s\n",
    format(nrow(airport_month_ntsb), big.mark = ",")))
cat(sprintf("  Unique airports in panel: %s\n",
    format(n_distinct(airport_month_ntsb$airport_id), big.mark = ",")))

# Airport-level annual accident rate (accidents per airport-year in panel)
airport_acc_totals <- airport_month_ntsb %>%
  group_by(airport_id) %>%
  summarise(
    total_accidents = sum(n_accidents),
    n_months        = n_distinct(ntsb_ym),
    n_fatal_months  = sum(n_fatal > 0),
    .groups = "drop"
  ) %>%
  arrange(desc(total_accidents))

cat("\n  Top 20 airports by accident count (near-airport):\n")
print(head(airport_acc_totals, 20))

cat("\n  Severity breakdown of near-airport accidents:\n")
print(count(acc_near, ev_highest_injury, sort = TRUE))


# =============================================================================
# SECTION 3 — LINKAGE: CODE OVERLAP
# =============================================================================

cat("\n=============================================================\n")
cat("--- SECTION 3: Airport code overlap (ASRS ↔ NTSB) ---\n")

asrs_airports <- unique(asrs_atc$airport_primary[!is.na(asrs_atc$airport_primary)])
ntsb_airports <- unique(str_trim(acc_near$airport_id[!is.na(acc_near$airport_id)]))

# Normalise to uppercase for comparison
asrs_up  <- toupper(asrs_airports)
ntsb_up  <- toupper(ntsb_airports)

in_both  <- intersect(asrs_up, ntsb_up)
only_asrs <- setdiff(asrs_up,  ntsb_up)
only_ntsb <- setdiff(ntsb_up,  asrs_up)

cat(sprintf("  Airports in ASRS ATC subset:           %d\n", length(asrs_up)))
cat(sprintf("  Airports in NTSB near-airport panel:   %d\n", length(ntsb_up)))
cat(sprintf("  Airports in BOTH (direct code match):  %d\n", length(in_both)))
cat(sprintf("  Only in ASRS:                          %d\n", length(only_asrs)))
cat(sprintf("  Only in NTSB:                          %d\n", length(only_ntsb)))

cat("\n  Matched airports (first 40):\n")
cat(paste(head(sort(in_both), 40), collapse = ", "), "\n")

# Accidents at matched vs. unmatched airports
acc_in_linked  <- acc_near %>% filter(toupper(str_trim(airport_id)) %in% in_both)
cat(sprintf("\n  NTSB accidents at linkable airports: %s / %s (%.1f%%)\n",
    format(nrow(acc_in_linked), big.mark = ","),
    format(nrow(acc_near), big.mark = ","),
    100 * nrow(acc_in_linked) / nrow(acc_near)))


# =============================================================================
# SECTION 4 — LINKED PANEL FEASIBILITY
# =============================================================================

cat("\n=============================================================\n")
cat("--- SECTION 4: Linked panel feasibility ---\n")

# Build the airport-month panel for matched airports only
# ASRS side: aggregate reports by airport × month (all ATC tiers)
asrs_panel <- asrs_atc %>%
  filter(
    !is.na(airport_primary),
    toupper(airport_primary) %in% in_both,
    !is.na(asrs_ym)
  ) %>%
  group_by(airport_id = toupper(airport_primary), yearmonth = asrs_ym) %>%
  summarise(
    n_atc_any      = n(),
    n_atc_local    = sum(atc_local),
    n_atc_terminal = sum(atc_terminal),
    n_atc_enroute  = sum(atc_enroute),
    .groups = "drop"
  )

# NTSB side: aggregate accidents by airport × month
ntsb_panel <- airport_month_ntsb %>%
  filter(toupper(airport_id) %in% in_both) %>%
  mutate(airport_id = toupper(airport_id)) %>%
  rename(yearmonth = ntsb_ym)

# Full joint panel: all yearmonths for matched airports
all_ym <- seq.Date(
  from = as.Date("1988-01-01"),
  to   = max(c(max(asrs_panel$yearmonth), max(ntsb_panel$yearmonth)), na.rm = TRUE),
  by   = "month"
)

full_panel <- expand_grid(
  airport_id = in_both,
  yearmonth  = all_ym
) %>%
  left_join(asrs_panel,  by = c("airport_id", "yearmonth")) %>%
  left_join(ntsb_panel,  by = c("airport_id", "yearmonth")) %>%
  mutate(
    n_atc_any      = replace_na(n_atc_any, 0L),
    n_atc_local    = replace_na(n_atc_local, 0L),
    n_atc_terminal = replace_na(n_atc_terminal, 0L),
    n_accidents    = replace_na(n_accidents, 0L),
    accident_binary = as.integer(n_accidents > 0)
  )

cat(sprintf("  Full panel rows (matched airports × all months): %s\n",
    format(nrow(full_panel), big.mark = ",")))
cat(sprintf("  Panel date range: %s to %s\n",
    min(full_panel$yearmonth), max(full_panel$yearmonth)))

# Months with at least one ATC report
cat(sprintf("\n  Months with >= 1 ATC report (any tier):  %s (%.1f%%)\n",
    format(sum(full_panel$n_atc_any > 0), big.mark = ","),
    100 * mean(full_panel$n_atc_any > 0)))
cat(sprintf("  Months with >= 1 local ATC report:       %s (%.1f%%)\n",
    format(sum(full_panel$n_atc_local > 0), big.mark = ","),
    100 * mean(full_panel$n_atc_local > 0)))

# Accident base rate in panel
cat(sprintf("\n  Accident base rate in panel:             %.2f%% of airport-months\n",
    100 * mean(full_panel$accident_binary)))
cat(sprintf("  Accident base rate (months w/ ATC data): %.2f%%\n",
    100 * mean(full_panel$accident_binary[full_panel$n_atc_any > 0])))

# Per-airport: does it have both enough ASRS reports AND at least one accident?
airport_feasibility <- full_panel %>%
  group_by(airport_id) %>%
  summarise(
    total_atc_reports  = sum(n_atc_any),
    total_local_rpts   = sum(n_atc_local),
    total_accidents    = sum(n_accidents),
    n_months           = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_atc_reports))

cat("\n  Per-airport panel stats (top 30 by ATC reports):\n")
print(head(airport_feasibility, 30))

for (min_rpts in c(30, 50, 75)) {
  feasible <- airport_feasibility %>%
    filter(total_atc_reports >= min_rpts, total_accidents >= 1)
  cat(sprintf("\n  Airports with >= %d ATC reports AND >= 1 accident: %d\n",
      min_rpts, nrow(feasible)))
}

# Local-only feasibility
cat("\n  Feasibility with LOCAL reporters only:\n")
for (min_rpts in c(15, 30, 50)) {
  feasible_local <- airport_feasibility %>%
    filter(total_local_rpts >= min_rpts, total_accidents >= 1)
  cat(sprintf("  Airports with >= %d local ATC reports AND >= 1 accident: %d\n",
      min_rpts, nrow(feasible_local)))
}


# =============================================================================
# SECTION 5 — SAVE OUTPUTS
# =============================================================================

cat("\n=============================================================\n")
cat("--- SECTION 5: Saving audit outputs ---\n")

write_csv(func_counts,          file.path(AUDIT_OUT_DIR, "asrs_function_counts.csv"))
write_csv(atc_summary,          file.path(AUDIT_OUT_DIR, "asrs_atc_tier_summary.csv"))
write_csv(top_airports_asrs,    file.path(AUDIT_OUT_DIR, "asrs_top_airports.csv"))
write_csv(annual_asrs_atc,      file.path(AUDIT_OUT_DIR, "asrs_annual_atc_counts.csv"))
write_csv(airport_totals_asrs,  file.path(AUDIT_OUT_DIR, "asrs_airport_totals.csv"))
write_csv(acc_dist,             file.path(AUDIT_OUT_DIR, "ntsb_apt_dist_distribution.csv"))
write_csv(airport_acc_totals,   file.path(AUDIT_OUT_DIR, "ntsb_airport_accident_totals.csv"))
write_csv(airport_feasibility,  file.path(AUDIT_OUT_DIR, "linked_airport_feasibility.csv"))
write_csv(full_panel %>% filter(airport_id %in% head(airport_feasibility$airport_id, 50)),
          file.path(AUDIT_OUT_DIR, "linked_panel_sample.csv"))

# Quick diagnostic plot: ATC reports vs. accident count per airport
p_scatter <- airport_feasibility %>%
  filter(total_atc_reports > 0) %>%
  ggplot(aes(x = total_atc_reports, y = total_accidents)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "ATC Report Volume vs. Accident Count per Airport",
    subtitle = "Matched airports (ASRS ↔ NTSB); log-log scale",
    x = "Total ATC ASRS reports (log scale)",
    y = "Total NTSB accidents within 15 NM (log scale)"
  ) +
  theme_minimal()

ggsave(file.path(AUDIT_OUT_DIR, "atc_reports_vs_accidents.png"),
       p_scatter, width = 7, height = 5, dpi = 150)

# Time series: matched-airport accidents and ATC reports by year
p_ts <- full_panel %>%
  mutate(year = year(yearmonth)) %>%
  group_by(year) %>%
  summarise(
    atc_reports = sum(n_atc_any),
    accidents   = sum(n_accidents),
    .groups = "drop"
  ) %>%
  filter(year >= 1990, year <= 2024) %>%
  pivot_longer(cols = c(atc_reports, accidents), names_to = "series", values_to = "n") %>%
  ggplot(aes(x = year, y = n, colour = series)) +
  geom_line(linewidth = 1) +
  facet_wrap(~series, scales = "free_y", ncol = 1) +
  labs(title = "Annual ATC Reports (ASRS) and Accidents (NTSB) — matched airports",
       x = NULL, y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(AUDIT_OUT_DIR, "annual_timeseries.png"),
       p_ts, width = 7, height = 6, dpi = 150)

cat(sprintf("\n  Outputs written to: %s\n", AUDIT_OUT_DIR))
cat("\n=============================================================\n")
cat("  AUDIT COMPLETE\n")
cat("=============================================================\n")
