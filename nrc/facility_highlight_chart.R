# facility_highlight_chart.R
# ============================================================================
# Facility Highlight Run Chart — NRC Safety Climate Case Study Visualization
# ============================================================================
#
# Generates a "spaghetti plot" of windowed safety climate scores over time
# for all NRC facilities, with one target facility highlighted.
#
# Integrates directly with the multiverse pipeline:
#   - Uses prepare_nrc_config_data() for data loading + join
#   - Uses create_all_windows() for windowing (SMA / EWMA)
#   - Reads config parquets from the same _cfg/{id}/results.parquet structure
#
# Usage:
#   source("common/config.R")
#   source("common/data_prep.R")
#   source("facility_highlight_chart.R")
#
#   # Single score run chart
#   p <- facility_highlight_chart(
#     cfg_dir       = "path/to/config_dir",
#     config_id     = "0",
#     nrc_events    = nrc_events,
#     facility_name = "st. lucie",
#     climate_var   = "overall_final_score_sma_5",
#     save_path     = "st_lucie_highlight.pdf"
#   )
#
#   # Multi-domain panel
#   p <- facility_highlight_panel(
#     cfg_dir       = "path/to/config_dir",
#     config_id     = "0",
#     nrc_events    = nrc_events,
#     facility_name = "st. lucie",
#     save_path     = "st_lucie_domains.pdf"
#   )
#
# Dependencies: tidyverse, ggplot2, arrow, glue, patchwork
#   + common/config.R, common/data_prep.R must be sourced first
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(glue)
})

# ============================================================================
# FACILITY NAME MATCHING
# ============================================================================

#' Normalize a facility name for matching
#' Lowercase, strip trailing unit numbers, collapse whitespace
normalize_facility_name <- function(name) {
  s <- tolower(trimws(as.character(name)))
  # Remove trailing unit designators: " 1", " 2", " unit 1", etc.
  s <- str_trim(str_remove(s, "\\s+(unit\\s*)?\\d+\\s*$"))
  # Collapse multiple spaces
  s <- str_replace_all(s, "\\s+", " ")
  s
}

#' Find the best match for a facility name in the data
#' Tries exact match, then substring, then falls back with suggestions
find_facility <- function(df, facility_name, facility_col = "facility") {
  target <- normalize_facility_name(facility_name)
  all_facs <- sort(unique(normalize_facility_name(df[[facility_col]])))

  # Exact

  if (target %in% all_facs) return(target)

  # Substring match
  matches <- all_facs[str_detect(all_facs, fixed(target)) |
                      str_detect(target, fixed(all_facs))]
  if (length(matches) == 1) {
    message(glue("  Matched '{facility_name}' -> '{matches}'"))
    return(matches)
  }
  if (length(matches) > 1) {
    message(glue("  Multiple matches for '{facility_name}': {paste(matches, collapse=', ')}"))
    message(glue("  Using first match: '{matches[1]}'"))
    return(matches[1])
  }

  # Approximate (agrep)
  approx <- agrep(target, all_facs, max.distance = 0.3, value = TRUE)
  if (length(approx) > 0) {
    message(glue("  Approximate match for '{facility_name}': {paste(approx, collapse=', ')}"))
    return(approx[1])
  }

  stop(glue(
    "Facility '{facility_name}' not found in data.\n",
    "  Available facilities ({length(all_facs)}): {paste(head(all_facs, 15), collapse=', ')}..."
  ))
}


# ============================================================================
# CORE: PREPARE DATA FOR A SINGLE CONFIG + WINDOW
# ============================================================================

#' Prepare windowed facility-level data for plotting
#'
#' This is the key function: it calls prepare_nrc_config_data() to load
#' the config parquet, join to events, and create all windows — exactly
#' the same data prep as the multiverse analysis.
#'
#' @param cfg_dir Base config directory (parent of _cfg/)
#' @param config_id Configuration ID (e.g., "0", "12")
#' @param nrc_events Event-level data frame
#' @param ops_features Operational features (or NULL — fine for plotting)
#' @param climate_var Windowed climate variable name (e.g., "overall_final_score_sma_5")
#'   If NULL, returns the full prepped data frame so you can pick interactively.
#' @param min_reports Minimum events per facility to include
#' @param power_reactors_only If TRUE (default), filter to power reactor facilities
#'   only. Uses ops_features site names if available; otherwise falls back to the
#'   rx_type field in nrc_events (keeping BWR, PWR, etc. and dropping non-reactor
#'   facilities like materials licensees, hospitals, fuel fabricators).
#' @return Tibble with facility, event_date, climate_var value, and metadata
prepare_highlight_data <- function(cfg_dir,
                                   config_id,
                                   nrc_events,
                                   ops_features = NULL,
                                   climate_var = NULL,
                                   min_reports = 10,
                                   power_reactors_only = TRUE) {

  parquet_path <- file.path(cfg_dir, "_cfg", config_id, "results.parquet")
  if (!file.exists(parquet_path)) {
    stop(glue("Config parquet not found: {parquet_path}"))
  }

  # --- Pre-filter nrc_events to power reactors if requested ---
  # This happens BEFORE passing to prepare_nrc_config_data so that
  # the min_reports filter also only counts power reactor events.
  events_filtered <- nrc_events

  if (power_reactors_only) {
    n_before <- nrow(events_filtered)

    if (!is.null(ops_features) && nrow(ops_features) > 0) {
      # Method 1: Use ops data to identify power reactor site names
      # (same approach as the multiverse pipeline)
      ops_sites <- ops_features %>%
        mutate(.fac = tolower(trimws(facility_unit))) %>%
        mutate(.site = str_trim(str_remove(.fac, "\\s*[-]?\\s*\\d+\\s*$"))) %>%
        pull(.site) %>%
        unique()

      events_filtered <- events_filtered %>%
        filter(tolower(trimws(facility)) %in% ops_sites)

      message(glue("  Power reactor filter (via ops): {nrow(events_filtered)}/{n_before} events retained"))

    } else if ("rx_type" %in% names(events_filtered)) {
      # Method 2: Use rx_type field to identify reactor events
      # rx_type contains strings like "[1] B&W ...", "[1] GE BWR...", "[1] W-4LP..."
      # Power reactor events have non-NA rx_type with reactor designators;
      # non-reactor facilities have NA or empty rx_type.
      events_filtered <- events_filtered %>%
        filter(!is.na(rx_type) & trimws(rx_type) != "")

      message(glue("  Power reactor filter (via rx_type): {nrow(events_filtered)}/{n_before} events retained"))

    } else {
      message("  No ops_features or rx_type column available — skipping power reactor filter")
    }
  }

  message(glue("Loading config {config_id} and preparing windows..."))

  # Use the same prep function as the multiverse pipeline
  df <- prepare_nrc_config_data(
    parquet_path  = parquet_path,
    config_id     = config_id,
    nrc_events    = events_filtered,
    ops_features  = ops_features,
    min_reports   = min_reports,
    min_n         = 1
  )

  if (is.null(df) || nrow(df) == 0) {
    stop(glue("No data after preparation for config {config_id}"))
  }

  # Report available climate vars if none specified
  if (is.null(climate_var)) {
    cvars <- get_climate_vars(df)
    message(glue("Available climate variables ({length(cvars)}):"))
    for (cv in cvars) message(glue("  {cv}"))
    return(df)
  }

  if (!climate_var %in% names(df)) {
    cvars <- get_climate_vars(df)
    stop(glue(
      "Climate variable '{climate_var}' not found.\n",
      "Available: {paste(head(cvars, 10), collapse=', ')}..."
    ))
  }

  # Return with the essential columns
  df %>%
    select(
      facility, event_date, config_id,
      all_of(climate_var),
      any_of(c("overall_final_score",
               names(df)[str_detect(names(df), "_domain_score$")]))
    ) %>%
    rename(climate_score = all_of(climate_var)) %>%
    filter(!is.na(climate_score))
}


# ============================================================================
# MAIN PLOTTING FUNCTION — SINGLE VARIABLE
# ============================================================================

#' Create a facility highlight run chart
#'
#' Plots all facilities as thin transparent grey lines with one target
#' facility highlighted in red. Optionally adds a fleet-wide median
#' trend line.
#'
#' @param cfg_dir Base config directory (parent of _cfg/)
#' @param config_id Configuration ID
#' @param nrc_events Event-level data frame
#' @param facility_name Character vector of facility names to highlight (fuzzy matched).
#'   Can also be a named vector where names are facilities and values are colors,
#'   e.g. c("st. lucie" = "#CC0000", "turkey point" = "#0066CC").
#'   A single string still works as before.
#' @param climate_var Windowed climate variable name
#' @param ops_features Operational features (or NULL)
#' @param min_reports Minimum events per facility to include
#' @param show_fleet_median Show a dashed fleet-wide median trend line
#' @param fleet_median_window Width of rolling median in days (default 365)
#' @param annotation_dates Named list of annotation dates (label = "YYYY-MM-DD")
#' @param title Plot title (auto-generated if NULL)
#' @param subtitle Plot subtitle (auto-generated if NULL)
#' @param ylabel Y-axis label (auto-generated if NULL)
#' @param bg_color Background line color (default grey)
#' @param bg_alpha Background line transparency
#' @param bg_linewidth Background line width
#' @param hl_colors Default color palette for highlighted facilities.
#'   Used when facility_name is an unnamed vector. Recycled if needed.
#' @param hl_alpha Highlight line transparency
#' @param hl_linewidth Highlight line width
#' @param save_path Path to save figure (PDF/SVG); NULL to skip
#' @param width Figure width in inches
#' @param height Figure height in inches
#' @param return_data If TRUE, return list(plot, data) instead of just plot
#' @return ggplot object (or list if return_data=TRUE)
facility_highlight_chart <- function(
    cfg_dir,
    config_id,
    nrc_events,
    facility_name,
    climate_var,
    ops_features     = NULL,
    min_reports      = 10,
    power_reactors_only = TRUE,
    show_fleet_median = TRUE,
    fleet_median_window = 365,
    annotation_dates = NULL,
    title            = NULL,
    subtitle         = NULL,
    ylabel           = NULL,
    bg_color         = "grey60",
    bg_alpha         = 0.15,
    bg_linewidth     = 0.5,
    hl_colors        = c("#CC0000", "#0066CC", "#009933", "#CC6600", "#6600CC"),
    hl_alpha         = 0.95,
    hl_linewidth     = 1.8,
    save_path        = NULL,
    width            = 10,
    height           = 5,
    return_data      = FALSE
) {

  # --- Prepare data ---
  df <- prepare_highlight_data(
    cfg_dir      = cfg_dir,
    config_id    = config_id,
    nrc_events   = nrc_events,
    ops_features = ops_features,
    climate_var  = climate_var,
    min_reports  = min_reports,
    power_reactors_only = power_reactors_only
  )

  # --- Resolve facilities (supports multiple) ---
  df <- df %>% mutate(facility_norm = normalize_facility_name(facility))

  # Parse facility_name: named vector = custom colors, unnamed = use palette
  if (!is.null(names(facility_name)) && all(nchar(names(facility_name)) > 0)) {
    fac_colors <- facility_name
    facility_name <- names(fac_colors)
  } else {
    facility_name <- as.character(facility_name)
    fac_colors <- setNames(
      rep_len(hl_colors, length(facility_name)),
      facility_name
    )
  }

  # Resolve each facility name against the data
  resolved <- vapply(facility_name, function(fn) {
    find_facility(df, fn)
  }, character(1))

  # Color mapping keyed on resolved names
  color_map <- setNames(as.character(fac_colors), resolved)
  target_facs <- unname(resolved)

  # Split data
  df_targets <- df %>% filter(facility_norm %in% target_facs)
  df_bg      <- df %>% filter(!facility_norm %in% target_facs)

  for (fac in target_facs) {
    n_fac <- sum(df_targets$facility_norm == fac)
    message(glue("  Highlight '{fac}': {n_fac} data points"))
  }
  message(glue("  Background: {n_distinct(df_bg$facility_norm)} other facilities"))

  if (nrow(df_targets) == 0) {
    stop("No data for any highlighted facility. Try reducing min_reports.")
  }

  # --- Build plot ---
  p <- ggplot() +
    geom_line(
      data = df_bg,
      aes(x = event_date, y = climate_score, group = facility_norm),
      color = bg_color, alpha = bg_alpha, linewidth = bg_linewidth
    )

  # Fleet-wide median trend
  if (show_fleet_median && nrow(df) > 10) {
    fleet_med <- df %>%
      mutate(date_floor = lubridate::floor_date(event_date, "quarter")) %>%
      group_by(date_floor) %>%
      summarise(
        fleet_median = median(climate_score, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 3)

    if (nrow(fleet_med) > 2) {
      p <- p +
        geom_line(
          data = fleet_med,
          aes(x = date_floor, y = fleet_median),
          color = "grey30", linewidth = 1.0, linetype = "dashed", alpha = 0.6
        )
    }
  }

  # Highlighted facilities — one layer per facility
  for (fac in target_facs) {
    fac_data  <- df_targets %>% filter(facility_norm == fac)
    fac_col   <- color_map[[fac]]
    fac_label <- str_to_title(fac)

    p <- p +
      geom_line(
        data = fac_data,
        aes(x = event_date, y = climate_score, color = !!fac_label),
        alpha = hl_alpha, linewidth = hl_linewidth, show.legend = TRUE
      ) +
      geom_point(
        data = fac_data,
        aes(x = event_date, y = climate_score),
        color = fac_col, alpha = hl_alpha, size = 1.2, show.legend = FALSE
      )
  }

  # Manual color scale for the legend
  legend_labels <- str_to_title(target_facs)
  legend_colors <- unname(color_map[target_facs])
  p <- p + scale_color_manual(
    name   = NULL,
    values = setNames(legend_colors, legend_labels)
  )

  # Annotation lines for known events
  if (!is.null(annotation_dates)) {
    for (i in seq_along(annotation_dates)) {
      lbl <- names(annotation_dates)[i]
      dt  <- as.Date(annotation_dates[[i]])
      p <- p +
        geom_vline(xintercept = dt, color = "grey40",
                   alpha = 0.6, linewidth = 0.7, linetype = "dotted") +
        annotate("text", x = dt, y = Inf, label = paste0("  ", lbl),
                 hjust = 0, vjust = 1.2, size = 2.5, color = "grey30",
                 angle = 90, fontface = "italic")
    }
  }

  # --- Labels ---
  # Parse the climate_var name for human-readable labels
  window_label <- climate_var %>%
    str_remove("^overall_final_score_") %>%
    str_replace("sma_(\\d+)", "SMA(\\1)") %>%
    str_replace("ewmaLAG_(\\d+)d_hl(\\d+)d", "EWMA(lag=\\1d, hl=\\2d)")

  if (is.null(title)) {
    fac_labels <- paste(str_to_title(target_facs), collapse = " & ")
    title <- glue("{fac_labels} vs. Fleet — Safety Climate Score")
  }
  if (is.null(subtitle)) {
    subtitle <- glue("Config: {config_id} | Window: {window_label}")
  }
  if (is.null(ylabel)) {
    ylabel <- "Windowed Climate Score"
  }

  p <- p +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 minor_breaks = NULL) +
    labs(
      title    = title,
      subtitle = subtitle,
      x = NULL,
      y = ylabel
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(size = 9, color = "grey40"),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.3, color = "grey90"),
      legend.position    = "top",
      legend.justification = "left"
    )

  # --- Save ---
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = width, height = height, dpi = 300)
    message(glue("  Saved to: {save_path}"))
  }

  if (return_data) {
    return(list(plot = p, data = df, target_facilities = target_facs,
                color_map = color_map))
  }
  return(p)
}


# ============================================================================
# MULTI-DOMAIN PANEL VERSION
# ============================================================================

#' Create a multi-domain panel of facility highlight charts
#'
#' One subplot per safety climate domain, all sharing the same x-axis.
#' Uses the domain score columns from the config parquet.
#'
#' @param cfg_dir Base config directory
#' @param config_id Configuration ID
#' @param nrc_events Event-level data frame
#' @param facility_name Character vector of facility names to highlight.
#'   Same format as facility_highlight_chart(): unnamed vector or named with colors.
#' @param climate_var Windowed climate variable (determines which window spec to use)
#' @param domains Character vector of domain abbreviations to plot.
#'   If NULL, auto-detects all *_domain_score columns.
#' @param ops_features Operational features (or NULL)
#' @param min_reports Minimum events per facility
#' @param save_path Path to save (PDF/SVG)
#' @param width Figure width
#' @param height_per_panel Height per panel in inches
#' @return patchwork composite plot
facility_highlight_panel <- function(
    cfg_dir,
    config_id,
    nrc_events,
    facility_name,
    climate_var = "overall_final_score_sma_5",
    domains         = NULL,
    ops_features    = NULL,
    min_reports     = 10,
    power_reactors_only = TRUE,
    show_fleet_median = TRUE,
    annotation_dates = NULL,
    bg_color    = "grey60",
    bg_alpha    = 0.15,
    hl_color    = "#CC0000",
    save_path   = NULL,
    width       = 10,
    height_per_panel = 3.5
) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("The patchwork package is required for panel plots: install.packages('patchwork')")
  }

  # --- Pre-filter to power reactors (same logic as prepare_highlight_data) ---
  events_filtered <- nrc_events
  if (power_reactors_only) {
    n_before <- nrow(events_filtered)
    if (!is.null(ops_features) && nrow(ops_features) > 0) {
      ops_sites <- ops_features %>%
        mutate(.fac = tolower(trimws(facility_unit))) %>%
        mutate(.site = str_trim(str_remove(.fac, "\\s*[-]?\\s*\\d+\\s*$"))) %>%
        pull(.site) %>%
        unique()
      events_filtered <- events_filtered %>%
        filter(tolower(trimws(facility)) %in% ops_sites)
    } else if ("rx_type" %in% names(events_filtered)) {
      events_filtered <- events_filtered %>%
        filter(!is.na(rx_type) & trimws(rx_type) != "")
    }
    message(glue("  Power reactor filter: {nrow(events_filtered)}/{n_before} events retained"))
  }

  # --- Load full prepped data (with all domain scores) ---
  parquet_path <- file.path(cfg_dir, "_cfg", config_id, "results.parquet")
  if (!file.exists(parquet_path)) {
    stop(glue("Config parquet not found: {parquet_path}"))
  }

  message(glue("Loading config {config_id} for panel plot..."))
  df_full <- prepare_nrc_config_data(
    parquet_path  = parquet_path,
    config_id     = config_id,
    nrc_events    = events_filtered,
    ops_features  = ops_features,
    min_reports   = min_reports,
    min_n         = 1
  )

  if (is.null(df_full) || nrow(df_full) == 0) {
    stop(glue("No data after preparation for config {config_id}"))
  }

  # --- Detect domain score columns ---
  domain_cols <- names(df_full)[str_detect(names(df_full), "_domain_score$")]
  if (is.null(domains)) {
    domains <- str_remove(domain_cols, "_domain_score$")
  }

  if (length(domains) == 0) {
    stop("No domain score columns found in the data.")
  }

  # --- Determine window spec from climate_var ---
  # We need to replicate the windowing for each domain's base score
  # The windowing function expects a climate_var_base; for domains,
  # we'll apply the same window spec to each domain score.

  # Parse the window spec from the climate_var name
  window_spec <- parse_window_spec(climate_var)

  # --- Resolve facilities (multi-facility support) ---
  df_full <- df_full %>% mutate(facility_norm = normalize_facility_name(facility))

  # Parse facility colors (same logic as main chart)
  if (!is.null(names(facility_name)) && all(nchar(names(facility_name)) > 0)) {
    fac_colors <- facility_name
    facility_name <- names(fac_colors)
  } else {
    facility_name <- as.character(facility_name)
    default_hl_colors <- c("#CC0000", "#0066CC", "#009933", "#CC6600", "#6600CC")
    fac_colors <- setNames(rep_len(default_hl_colors, length(facility_name)), facility_name)
  }

  resolved <- vapply(facility_name, function(fn) find_facility(df_full, fn), character(1))
  color_map <- setNames(as.character(fac_colors), resolved)
  target_facs <- unname(resolved)

  # --- Build one panel per domain ---
  panels <- list()

  for (dom in domains) {
    base_col <- paste0(dom, "_domain_score")
    if (!base_col %in% names(df_full)) {
      message(glue("  Skipping domain {dom}: column {base_col} not found"))
      next
    }

    # Apply the same windowing to this domain's base score
    windowed_col <- apply_single_window(df_full, base_col, window_spec)

    if (is.null(windowed_col)) next

    df_panel <- df_full %>%
      mutate(climate_score = windowed_col) %>%
      filter(!is.na(climate_score))

    df_targets_panel <- df_panel %>% filter(facility_norm %in% target_facs)
    df_bg <- df_panel %>% filter(!facility_norm %in% target_facs)

    if (nrow(df_targets_panel) == 0) {
      message(glue("  Skipping domain {dom}: no data for highlighted facilities"))
      next
    }

    p <- ggplot() +
      geom_line(
        data = df_bg,
        aes(x = event_date, y = climate_score, group = facility_norm),
        color = bg_color, alpha = bg_alpha, linewidth = 0.4
      )

    if (show_fleet_median) {
      fleet_med <- df_panel %>%
        mutate(date_floor = lubridate::floor_date(event_date, "quarter")) %>%
        group_by(date_floor) %>%
        summarise(fleet_median = median(climate_score, na.rm = TRUE),
                  n = n(), .groups = "drop") %>%
        filter(n >= 3)
      if (nrow(fleet_med) > 2) {
        p <- p +
          geom_line(data = fleet_med,
                    aes(x = date_floor, y = fleet_median),
                    color = "grey30", linewidth = 0.8, linetype = "dashed", alpha = 0.5)
      }
    }

    # One layer per highlighted facility
    for (fac in target_facs) {
      fac_data <- df_targets_panel %>% filter(facility_norm == fac)
      if (nrow(fac_data) == 0) next
      fac_col   <- color_map[[fac]]
      fac_label <- str_to_title(fac)

      p <- p +
        geom_line(data = fac_data,
                  aes(x = event_date, y = climate_score, color = !!fac_label),
                  alpha = 0.95, linewidth = 1.5) +
        geom_point(data = fac_data,
                   aes(x = event_date, y = climate_score),
                   color = fac_col, alpha = 0.95, size = 0.9, show.legend = FALSE)
    }

    # Manual color scale
    legend_labels <- str_to_title(target_facs)
    legend_colors <- unname(color_map[target_facs])
    p <- p + scale_color_manual(
      name = NULL,
      values = setNames(legend_colors, legend_labels)
    )

    # Annotation lines
    if (!is.null(annotation_dates)) {
      for (i in seq_along(annotation_dates)) {
        dt <- as.Date(annotation_dates[[i]])
        p <- p +
          geom_vline(xintercept = dt, color = "grey40",
                     alpha = 0.5, linewidth = 0.5, linetype = "dotted")
      }
    }

    # Show legend only on first panel
    show_legend <- (dom == domains[1])

    p <- p +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      labs(y = dom) +
      theme_minimal(base_size = 10) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x  = if (dom == tail(domains, 1)) {
          element_text(angle = 45, hjust = 1)
        } else {
          element_blank()
        },
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2, color = "grey90"),
        legend.position    = if (show_legend) "top" else "none",
        legend.justification = "left",
        plot.margin        = margin(2, 5, 2, 5)
      )

    panels[[dom]] <- p
  }

  if (length(panels) == 0) {
    stop("No valid panels generated.")
  }

  # --- Compose with patchwork ---
  window_label <- climate_var %>%
    str_remove("^overall_final_score_") %>%
    str_replace("sma_(\\d+)", "SMA(\\1)") %>%
    str_replace("ewmaLAG_(\\d+)d_hl(\\d+)d", "EWMA(lag=\\1d, hl=\\2d)")

  fac_labels <- paste(str_to_title(target_facs), collapse = " & ")
  combined <- patchwork::wrap_plots(panels, ncol = 1) +
    patchwork::plot_annotation(
      title    = glue("{fac_labels} vs. Fleet — Domain-Level Safety Climate"),
      subtitle = glue("Config: {config_id} | Window: {window_label}"),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "grey40")
      )
    )

  if (!is.null(save_path)) {
    total_height <- height_per_panel * length(panels) + 1.5
    ggsave(save_path, combined, width = width, height = total_height, dpi = 300)
    message(glue("  Saved panel figure to: {save_path}"))
  }

  return(combined)
}


# ============================================================================
# HELPERS: WINDOW SPEC PARSING + SINGLE-COLUMN WINDOWING
# ============================================================================

#' Parse a window specification from a climate variable name
#' Returns a list with type ("sma" or "ewma") and relevant parameters
parse_window_spec <- function(climate_var) {
  if (str_detect(climate_var, "_sma_\\d+")) {
    win_size <- as.integer(str_extract(climate_var, "(?<=_sma_)\\d+"))
    return(list(type = "sma", window_size = win_size))
  }
  if (str_detect(climate_var, "_ewmaLAG_")) {
    lag_d <- as.integer(str_extract(climate_var, "(?<=_ewmaLAG_)\\d+"))
    hl_d  <- as.integer(str_extract(climate_var, "(?<=_hl)\\d+"))
    return(list(type = "ewma", lag_days = lag_d, halflife_days = hl_d))
  }
  # Fallback: assume it's the base (unwindowed) score
  return(list(type = "none"))
}


#' Apply a single window specification to a column in the data
#' Returns a numeric vector of windowed values (same length as df)
apply_single_window <- function(df, base_col, window_spec) {
  if (window_spec$type == "none") {
    return(df[[base_col]])
  }

  # Need facility + event_date for grouping
  if (!all(c("facility", "event_date") %in% names(df))) {
    warning("Cannot apply windowing: missing facility or event_date columns")
    return(df[[base_col]])
  }

  result <- df %>%
    arrange(facility, event_date) %>%
    group_by(facility)

  if (window_spec$type == "sma") {
    result <- result %>%
      mutate(
        .windowed = slider::slide_dbl(
          lag(.data[[base_col]], 1),
          ~ mean(.x, na.rm = TRUE),
          .before = window_spec$window_size - 1,
          .complete = TRUE
        )
      )
  } else if (window_spec$type == "ewma") {
    result <- result %>%
      mutate(
        .windowed = ewma_time_decay_irregular_lag(
          .data[[base_col]], event_date,
          window_spec$lag_days, window_spec$halflife_days, min_n = 1
        )
      )
  }

  result <- result %>% ungroup()
  return(result$.windowed)
}


# ============================================================================
# CONVENIENCE: LIST AVAILABLE CONFIGS + WINDOWS
# ============================================================================

#' List available configuration IDs from a config directory
list_configs <- function(cfg_dir) {
  cfg_path <- file.path(cfg_dir, "_cfg")
  if (!dir.exists(cfg_path)) {
    stop(glue("Config directory not found: {cfg_path}"))
  }
  ids <- list.dirs(cfg_path, full.names = FALSE, recursive = FALSE)
  ids <- ids[ids != "" & !startsWith(ids, ".")]
  message(glue("Found {length(ids)} configurations"))
  return(sort(ids))
}

#' List available windowed climate variables for a config
#' (Convenience wrapper — calls prepare_highlight_data with climate_var=NULL)
list_climate_vars <- function(cfg_dir, config_id, nrc_events,
                              ops_features = NULL, min_reports = 10) {
  df <- prepare_highlight_data(
    cfg_dir      = cfg_dir,
    config_id    = config_id,
    nrc_events   = nrc_events,
    ops_features = ops_features,
    climate_var  = NULL,
    min_reports  = min_reports
  )
  return(get_climate_vars(df))
}
