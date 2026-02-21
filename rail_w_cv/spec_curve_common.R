# ==============================================================================
# SPECIFICATION CURVE PLOTTING — SHARED MODULE
# ==============================================================================
#
# Harmonized dot-line specification curve for both:
#   - Cross-validation multiverse (Brier score / delta-Brier)
#   - Regular multiverse (effect estimates + CI)
#
# Configuration facets are displayed as dot-line indicator panels below the
# main curve, following the Steegen et al. style.  Each facet row shows
# horizontal reference lines for every level; a dot marks the level active
# for that specification.  Window type is split into two sub-panels (SMA and
# EWMA) so duration and type are jointly visible.
#
# Dependencies: tidyverse, patchwork
# Mike Rose — Safety Climate Analysis
# ==============================================================================

library(tidyverse)
library(patchwork)

# ==============================================================================
# 1.  DERIVE DISPLAY-READY FACET COLUMNS
# ==============================================================================

#' Add derived configuration columns used by the indicator panels
#'
#' Creates:
#'   - thresholding  : "none", "top_k = 3", "top_k = 5"
#'   - composite     : "weighted_avg", "attention (t = 0.15)", etc.
#'   - sma_duration  : window size label when window_type == "sma", else NA
#'   - ewma_duration : halflife label when window_type == "ewma", else NA
#'
#' @param df  Data frame with raw config columns (window_type, window_size,
#'            halflife_days, th__k, comp__method, comp__temperature, comp__power)
#' @return df with additional columns
derive_facet_columns <- function(df) {

  df <- df %>%
    mutate(
      # --- Thresholding (collapsed) ----------------------------------------
      thresholding = case_when(
        is.na(th__k) | th__k == 0       ~ "none",
        th__k == 3                       ~ "top_k = 3",
        th__k == 5                       ~ "top_k = 5",
        TRUE                             ~ paste0("top_k = ", th__k)
      ),

      # --- Composite method (collapsed) ------------------------------------
      composite = case_when(
        comp__method == "weighted_average"
          ~ "weighted avg",
        comp__method == "attention_weight" & !is.na(comp__temperature)
          ~ sprintf("attention (t = %s)", format_num(comp__temperature)),
        comp__method == "power_attention" & !is.na(comp__power)
          ~ sprintf("power (p = %s)", format_num(comp__power)),
        TRUE
          ~ as.character(comp__method)
      ),

      # --- Window sub-panels -----------------------------------------------
      # Coalesce window_size and halflife_days into a single duration value
      window_duration = coalesce(window_size, halflife_days),

      sma_duration = if_else(
        window_type == "sma" & !is.na(window_size),
        as.character(window_size),
        NA_character_
      ),
      ewma_duration = if_else(
        window_type == "ewma" & !is.na(halflife_days),
        as.character(halflife_days),
        NA_character_
      )
    )

  df
}


# Small formatter: drop trailing zeros (0.15 → "0.15", 1.00 → "1", 2.0 → "2")
format_num <- function(x) {
  sub("0+$", "", sub("\\.$", "", format(x, nsmall = 2)))
}


# ==============================================================================
# 2.  DOT-LINE INDICATOR PANEL (single facet)
# ==============================================================================

#' Build one dot-line indicator panel
#'
#' @param plot_data  Data frame that MUST contain columns `spec_rank` and
#'                   the column named in `var`.
#' @param var        Name of the column to plot.
#' @param label      Y-axis label for this facet row.
#' @param levels_order  Optional character vector giving the desired ordering
#'                      of factor levels (bottom → top).  If NULL, levels are
#'                      sorted with `sort()`.
#' @param dot_color  Colour of the indicator dots (default: "grey25").
#' @param dot_size   Size of dots (default: 1.2).
#' @param dot_alpha  Alpha of dots (default: 0.7).
#' @param na_action  What to do when the value is NA for a spec:
#'                   "hide" (default) = no dot; "show" = show at a special
#'                   "(none)" level.
#' @return A ggplot object (one panel row).
make_indicator_panel <- function(plot_data,
                                 var,
                                 label,
                                 levels_order = NULL,
                                 dot_color    = "grey25",
                                 dot_size     = 1.2,
                                 dot_alpha    = 0.70,
                                 na_action    = "hide") {

  d <- plot_data %>%
    select(spec_rank, value = all_of(var))

  # Handle NAs

  if (na_action == "hide") {
    d <- d %>% filter(!is.na(value))
  } else {
    d <- d %>% mutate(value = replace_na(as.character(value), "(none)"))
  }

  d <- d %>% mutate(value = as.character(value))

  # Determine levels
  if (is.null(levels_order)) {
    all_levels <- sort(unique(d$value))
  } else {
    all_levels <- levels_order
  }

  d <- d %>% mutate(value = factor(value, levels = all_levels))

  # Reference lines data (one per level)
  ref_lines <- tibble(
    y      = factor(all_levels, levels = all_levels),
    xstart = 0.5,
    xend   = max(plot_data$spec_rank, na.rm = TRUE) + 0.5
  )

  ggplot() +
    # Faint reference lines for every level
    geom_segment(
      data = ref_lines,
      aes(x = xstart, xend = xend, y = y, yend = y),
      color = "grey85", linewidth = 0.3
    ) +
    # Dots marking active level per specification
    geom_point(
      data = d,
      aes(x = spec_rank, y = value),
      color = dot_color, size = dot_size, alpha = dot_alpha,
      shape = 16
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0.005, 0.005)),
      limits = c(0.5, max(plot_data$spec_rank, na.rm = TRUE) + 0.5)
    ) +
    labs(x = NULL, y = label) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x      = element_blank(),
      axis.ticks.x      = element_blank(),
      axis.text.y       = element_text(size = 8),
      axis.title.y      = element_text(size = 9, face = "bold", angle = 0,
                                        hjust = 1, vjust = 0.5),
      panel.grid        = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin       = margin(1, 5, 1, 5)
    )
}


# ==============================================================================
# 3.  SPLIT-WINDOW INDICATOR PANELS
# ==============================================================================

#' Build the SMA and EWMA indicator panels.
#'
#' Each panel shows dots only for specifications that use that window type.
#' Levels are the distinct duration values (window_size for SMA, halflife for
#' EWMA) — currently 3 each.
#'
#' @param plot_data  Data frame with spec_rank, sma_duration, ewma_duration
#' @return A named list with elements `sma` and `ewma` (ggplot objects).
make_window_panels <- function(plot_data,
                               dot_color = "grey25",
                               dot_size  = 1.2,
                               dot_alpha = 0.70) {

  sma_levels  <- sort(unique(na.omit(plot_data$sma_duration)))
  ewma_levels <- sort(unique(na.omit(plot_data$ewma_duration)))

  # Attempt numeric sort if levels look numeric
  sma_levels  <- try_numeric_sort(sma_levels)
  ewma_levels <- try_numeric_sort(ewma_levels)

  p_sma <- make_indicator_panel(
    plot_data,
    var          = "sma_duration",
    label        = "SMA\nWindow",
    levels_order = sma_levels,
    dot_color    = dot_color,
    dot_size     = dot_size,
    dot_alpha    = dot_alpha,
    na_action    = "hide"
  )

  p_ewma <- make_indicator_panel(
    plot_data,
    var          = "ewma_duration",
    label        = "EWMA\nHalflife",
    levels_order = ewma_levels,
    dot_color    = dot_color,
    dot_size     = dot_size,
    dot_alpha    = dot_alpha,
    na_action    = "hide"
  )

  list(sma = p_sma, ewma = p_ewma)
}


# Helper: if all values can be coerced to numeric, sort numerically
try_numeric_sort <- function(x) {
  nums <- suppressWarnings(as.numeric(x))
  if (all(!is.na(nums))) {
    x[order(nums)]
  } else {
    sort(x)
  }
}


# ==============================================================================
# 4.  MAIN CURVE PANEL (generic top panel)
# ==============================================================================

#' Build the main specification curve panel.
#'
#' Supports two modes:
#'
#'   mode = "estimate"  — effect estimate with 95 % CI ribbon, coloured by
#'                         significance.  (Regular multiverse.)
#'   mode = "brier"     — Brier score with ± SD error bars, optional baseline
#'                         reference lines.  (CV multiverse.)
#'   mode = "delta"     — Delta-Brier, coloured by sign.
#'
#' @param plot_data   Data frame already sorted and ranked (must have spec_rank).
#' @param mode        One of "estimate", "brier", "delta".
#' @param y_var       Column name for the y-axis value.
#' @param se_var      Column name for SE (mode = "estimate") or SD (mode = "brier").
#' @param pval_var    Column name for p-value (mode = "estimate").
#' @param title       Plot title.
#' @param subtitle    Plot subtitle.
#' @param y_lab       Y-axis label.
#' @param baseline_y  Optional numeric: draw a reference line (e.g., no-climate
#'                    baseline Brier score).
#' @param baseline_label Label for the baseline reference line.
#' @param intercept_y Optional numeric: second reference line (intercept-only).
#' @param intercept_label Label for the intercept reference line.
#' @return A ggplot object.
make_curve_panel <- function(plot_data,
                             mode           = "estimate",
                             y_var          = NULL,
                             se_var         = NULL,
                             pval_var       = NULL,
                             title          = NULL,
                             subtitle       = NULL,
                             y_lab          = NULL,
                             baseline_y     = NULL,
                             baseline_label = "No-climate baseline",
                             intercept_y    = NULL,
                             intercept_label = "Intercept-only") {

  n_specs <- nrow(plot_data)

  # ---- Mode: effect estimate with CI and significance ----------------------
  if (mode == "estimate") {

    y_var    <- y_var    %||% "estimate"
    se_var   <- se_var   %||% "se"
    pval_var <- pval_var %||% "pval"

    plot_data <- plot_data %>%
      mutate(
        significant = .data[[pval_var]] < 0.05,
        ci_lower    = .data[[y_var]] - 1.96 * .data[[se_var]],
        ci_upper    = .data[[y_var]] + 1.96 * .data[[se_var]]
      )

    p <- ggplot(plot_data, aes(x = spec_rank, y = .data[[y_var]])) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red",
                 linewidth = 0.5) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                  alpha = 0.15, fill = "grey50") +
      geom_point(aes(color = significant), size = 0.8, alpha = 0.7) +
      scale_color_manual(
        values = c("TRUE" = "#2166AC", "FALSE" = "grey70"),
        labels = c("TRUE" = "p < 0.05", "FALSE" = "ns"),
        name   = NULL
      )

    y_lab <- y_lab %||% "Effect Estimate (95% CI)"

  # ---- Mode: Brier score ---------------------------------------------------
  } else if (mode == "brier") {

    y_var  <- y_var  %||% "brier_score_mean"
    se_var <- se_var %||% "brier_score_sd"

    p <- ggplot(plot_data, aes(x = spec_rank, y = .data[[y_var]])) +
      geom_errorbar(
        aes(ymin = .data[[y_var]] - .data[[se_var]],
            ymax = .data[[y_var]] + .data[[se_var]]),
        width = 0, alpha = 0.15, color = "#2166AC"
      ) +
      geom_point(size = 0.8, alpha = 0.7, color = "#2166AC")

    # Baseline reference lines
    if (!is.null(baseline_y) && !is.na(baseline_y)) {
      p <- p +
        geom_hline(yintercept = baseline_y, linetype = "dashed",
                   color = "#B2182B", linewidth = 0.5) +
        annotate("text", x = n_specs * 0.02, y = baseline_y,
                 label = baseline_label, hjust = 0, vjust = -0.5,
                 color = "#B2182B", size = 3)
    }

    if (!is.null(intercept_y) && !is.na(intercept_y)) {
      p <- p +
        geom_hline(yintercept = intercept_y, linetype = "dotted",
                   color = "#999999", linewidth = 0.4) +
        annotate("text", x = n_specs * 0.02, y = intercept_y,
                 label = intercept_label, hjust = 0, vjust = -0.5,
                 color = "#999999", size = 2.5)
    }

    y_lab <- y_lab %||% "Brier Score (mean ± SD across folds)"

  # ---- Mode: delta-Brier ---------------------------------------------------
  } else if (mode == "delta") {

    y_var <- y_var %||% "delta_brier"

    plot_data <- plot_data %>%
      mutate(helps = .data[[y_var]] < 0)

    n_helps   <- sum(plot_data$helps, na.rm = TRUE)
    pct_helps <- 100 * n_helps / n_specs

    p <- ggplot(plot_data, aes(x = spec_rank, y = .data[[y_var]])) +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
      geom_point(aes(color = helps), size = 0.8, alpha = 0.7) +
      scale_color_manual(
        values = c("TRUE" = "#2166AC", "FALSE" = "#B2182B"),
        labels = c("TRUE" = "Climate helps", "FALSE" = "Climate hurts"),
        name   = NULL
      ) +
      annotate("text", x = n_specs * 0.98,
               y = min(plot_data[[y_var]], na.rm = TRUE) * 0.4,
               label = sprintf("Helps: %d/%d (%.0f%%)",
                               n_helps, n_specs, pct_helps),
               hjust = 1, size = 3.2, color = "#2166AC") +
      annotate("text", x = n_specs * 0.98,
               y = max(plot_data[[y_var]], na.rm = TRUE) * 0.4,
               label = sprintf("Hurts: %d/%d (%.0f%%)",
                               n_specs - n_helps, n_specs, 100 - pct_helps),
               hjust = 1, size = 3.2, color = "#B2182B")

    y_lab <- y_lab %||% "Delta Brier (full − no-climate)"

  } else {
    stop("mode must be one of: 'estimate', 'brier', 'delta'")
  }

  # Common styling
  p <- p +
    scale_x_continuous(
      expand = expansion(mult = c(0.005, 0.005)),
      limits = c(0.5, max(plot_data$spec_rank, na.rm = TRUE) + 0.5)
    ) +
    labs(title = title, subtitle = subtitle, x = NULL, y = y_lab) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x       = element_blank(),
      axis.ticks.x      = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 12),
      plot.subtitle      = element_text(size = 9, color = "grey40"),
      legend.position    = "top",
      plot.margin        = margin(5, 5, 0, 5)
    )

  p
}


# ==============================================================================
# 5.  ASSEMBLE FULL SPECIFICATION CURVE
# ==============================================================================

#' Build a complete specification curve plot with dot-line indicator panels.
#'
#' This is the main user-facing function.  It:
#'
#'   1. Derives display facet columns (thresholding, composite, split windows).
#'   2. Sorts and ranks specifications.
#'   3. Builds the top curve panel (estimate, Brier, or delta-Brier).
#'   4. Builds indicator panels for each configuration facet.
#'   5. Stacks them with patchwork.
#'
#' @param data  Data frame.  For mode = "estimate" it should already have
#'              config columns joined.  For mode = "brier" / "delta" it should
#'              be the CV results table (with window_type, window_size,
#'              halflife_days, config_id, and the config columns joined).
#' @param mode  "estimate", "brier", or "delta".
#' @param y_var,se_var,pval_var  Column names for the curve panel (see
#'              make_curve_panel docs).  Sensible defaults per mode.
#' @param sort_var  Column to sort specifications by (defaults to y_var).
#' @param sort_dir  "asc" or "desc" (default: "asc" — lowest first).
#' @param title,subtitle,y_lab  Passed to the curve panel.
#' @param baseline_y,intercept_y  Reference lines for Brier mode.
#' @param facets  Character vector of facet names to show.
#'               Allowed values: "embedding_model", "sent_method",
#'               "thresholding", "composite", "window" (split SMA/EWMA).
#'               Default shows all.
#' @param curve_height   Relative height of the curve panel (default 3).
#' @param indicator_height  Relative height per indicator row (default 0.8).
#' @param dot_color,dot_size,dot_alpha  Dot aesthetics.
#' @return A list with `plot` (patchwork), `curve`, `panels`, `data`.
#' @export
plot_spec_curve <- function(data,
                            mode              = "estimate",
                            y_var             = NULL,
                            se_var            = NULL,
                            pval_var          = NULL,
                            sort_var          = NULL,
                            sort_dir          = "asc",
                            title             = NULL,
                            subtitle          = NULL,
                            y_lab             = NULL,
                            baseline_y        = NULL,
                            baseline_label    = "No-climate baseline",
                            intercept_y       = NULL,
                            intercept_label   = "Intercept-only",
                            facets            = c("embedding_model",
                                                  "sent_method",
                                                  "thresholding",
                                                  "composite",
                                                  "window"),
                            curve_height      = 3,
                            indicator_height  = 0.8,
                            dot_color         = "grey25",
                            dot_size          = 1.2,
                            dot_alpha         = 0.70) {

  # -- Default y_var per mode ------------------------------------------------
  if (is.null(y_var)) {
    y_var <- switch(mode,
      estimate = "climate_estimate_cond",
      brier    = "brier_score_mean",
      delta    = "delta_brier"
    )
  }
  sort_var <- sort_var %||% y_var

  # -- Derive facet columns --------------------------------------------------
  data <- derive_facet_columns(data)

  # -- Sort and rank ---------------------------------------------------------
  if (sort_dir == "asc") {
    data <- data %>% arrange(.data[[sort_var]])
  } else {
    data <- data %>% arrange(desc(.data[[sort_var]]))
  }
  data <- data %>% mutate(spec_rank = row_number())

  n_specs <- nrow(data)

  # -- Auto title / subtitle -------------------------------------------------
  if (is.null(title)) {
    title <- switch(mode,
      estimate = "Specification Curve: Climate Effect Estimates",
      brier    = "Specification Curve: Brier Scores",
      delta    = "Specification Curve: Delta-Brier"
    )
  }
  if (is.null(subtitle)) {
    subtitle <- sprintf("%d specifications ordered by %s", n_specs, sort_var)
  }

  # -- Build curve panel -----------------------------------------------------
  p_curve <- make_curve_panel(
    data,
    mode            = mode,
    y_var           = y_var,
    se_var          = se_var,
    pval_var        = pval_var,
    title           = title,
    subtitle        = subtitle,
    y_lab           = y_lab,
    baseline_y      = baseline_y,
    baseline_label  = baseline_label,
    intercept_y     = intercept_y,
    intercept_label = intercept_label
  )

  # -- Build indicator panels ------------------------------------------------
  panels <- list()
  panel_heights <- c()

  facet_specs <- list(
    embedding_model = list(var = "embedding_model",  label = "Embedding\nModel"),
    sent_method     = list(var = "sent_method",      label = "Sentiment\nModel"),
    thresholding    = list(var = "thresholding",     label = "Threshold"),
    composite       = list(var = "composite",        label = "Composite\nMethod")
  )

  for (f in facets) {

    if (f == "window") {
      # Split window panels
      wp <- make_window_panels(data,
                               dot_color = dot_color,
                               dot_size  = dot_size,
                               dot_alpha = dot_alpha)
      panels[["sma_window"]]  <- wp$sma
      panels[["ewma_window"]] <- wp$ewma
      panel_heights <- c(panel_heights, indicator_height, indicator_height)

    } else if (f %in% names(facet_specs)) {
      spec <- facet_specs[[f]]

      # Skip if column missing or invariant
      if (!spec$var %in% names(data)) next
      if (n_distinct(data[[spec$var]], na.rm = TRUE) <= 1) next

      panels[[f]] <- make_indicator_panel(
        data,
        var       = spec$var,
        label     = spec$label,
        dot_color = dot_color,
        dot_size  = dot_size,
        dot_alpha = dot_alpha
      )
      panel_heights <- c(panel_heights, indicator_height)

    } else {
      # Generic: try to show it as a plain indicator
      if (f %in% names(data) && n_distinct(data[[f]], na.rm = TRUE) > 1) {
        nice_label <- f %>%
          str_replace_all("__", ": ") %>%
          str_replace_all("_", " ") %>%
          str_to_title()
        panels[[f]] <- make_indicator_panel(
          data, var = f, label = nice_label,
          dot_color = dot_color, dot_size = dot_size, dot_alpha = dot_alpha
        )
        panel_heights <- c(panel_heights, indicator_height)
      }
    }
  }

  # -- X-axis label panel ----------------------------------------------------
  p_xlab <- ggplot(data, aes(x = spec_rank, y = 1)) +
    geom_blank() +
    scale_x_continuous(
      expand = expansion(mult = c(0.005, 0.005)),
      limits = c(0.5, n_specs + 0.5)
    ) +
    labs(x = sprintf("Specification (ranked by %s)", sort_var), y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid   = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin  = margin(0, 5, 5, 5)
    )

  # -- Stack with patchwork --------------------------------------------------
  heights <- c(curve_height, panel_heights, 0.25)
  plot_list <- c(list(p_curve), panels, list(p_xlab))

  combined <- wrap_plots(plotlist = plot_list, ncol = 1, heights = heights)

  list(
    plot   = combined,
    curve  = p_curve,
    panels = panels,
    data   = data
  )
}


# ==============================================================================
# 6.  CONVENIENCE WRAPPERS
# ==============================================================================

# ---- Regular multiverse (effect estimates) ---------------------------------

#' Specification curve for one outcome / component of the regular multiverse
#'
#' @param results_with_config  Data frame with config + model results joined.
#' @param outcome_filter  Outcome name (e.g., "injuries").
#' @param component  "cond" or "zi".
#' @param ...  Additional arguments passed to plot_spec_curve().
#' @return list(plot, curve, panels, data)
plot_spec_curve_estimate <- function(results_with_config,
                                     outcome_filter = NULL,
                                     component      = "cond",
                                     ...) {

  d <- results_with_config
  if (!is.null(outcome_filter)) {
    d <- d %>% filter(outcome == outcome_filter)
  }

  est_var  <- paste0("climate_estimate_", component)
  se_var   <- paste0("climate_se_", component)
  pval_var <- paste0("climate_pval_", component)
  comp_label <- if (component == "cond") "Conditional" else "Zero-Inflation"

  outcome_label <- if (!is.null(outcome_filter)) {
    paste0(tools::toTitleCase(outcome_filter), " — ")
  } else ""

  d <- d %>% filter(!is.na(.data[[est_var]]))

  plot_spec_curve(
    d,
    mode     = "estimate",
    y_var    = est_var,
    se_var   = se_var,
    pval_var = pval_var,
    title    = paste0(outcome_label, "Climate Effect (", comp_label, " Model)"),
    y_lab    = "Climate Effect Estimate (95% CI)",
    ...
  )
}


# ---- CV multiverse: Brier score -------------------------------------------

#' Specification curve for Brier scores from CV results
#'
#' @param cv_results  CV results tibble.
#' @param strategy  "timeseries" or "group_kfold".
#' @param show_baselines  Show no-climate and intercept-only reference lines.
#' @param ...  Additional arguments passed to plot_spec_curve().
#' @return list(plot, curve, panels, data)
plot_spec_curve_brier <- function(cv_results,
                                   strategy       = "timeseries",
                                   show_baselines = TRUE,
                                   ...) {

  full_specs <- cv_results %>%
    filter(
      cv_strategy == strategy,
      model_label == climate_var_tested,
      !is.na(brier_score_mean)
    )

  if (nrow(full_specs) == 0) {
    warning("No full-model specs found for strategy: ", strategy)
    return(NULL)
  }

  baseline_y  <- NULL
  intercept_y <- NULL

  if (show_baselines) {
    baseline_y <- cv_results %>%
      filter(cv_strategy == strategy, model_label == "no_climate") %>%
      pull(brier_score_mean) %>%
      median(na.rm = TRUE)

    intercept_y <- cv_results %>%
      filter(cv_strategy == strategy, model_label == "intercept_only") %>%
      pull(brier_score_mean) %>%
      median(na.rm = TRUE)
  }

  strategy_label <- if (strategy == "timeseries") {
    "Time-Series CV"
  } else {
    "Group K-Fold CV"
  }

  plot_spec_curve(
    full_specs,
    mode        = "brier",
    title       = sprintf("Specification Curve: Brier Score [%s]", strategy_label),
    y_lab       = "Brier Score (mean ± SD across folds)",
    baseline_y  = baseline_y,
    intercept_y = intercept_y,
    ...
  )
}


# ---- CV multiverse: delta-Brier -------------------------------------------

#' Specification curve for delta-Brier from CV results
#'
#' @param cv_results  CV results tibble.
#' @param strategy  "timeseries" or "group_kfold".
#' @param ...  Additional arguments passed to plot_spec_curve().
#' @return list(plot, curve, panels, data)
plot_spec_curve_delta <- function(cv_results,
                                   strategy = "timeseries",
                                   ...) {

  plot_data <- cv_results %>%
    filter(
      cv_strategy == strategy,
      model_label == climate_var_tested,
      !is.na(delta_brier)
    )

  if (nrow(plot_data) == 0) {
    warning("No delta-Brier data for strategy: ", strategy)
    return(NULL)
  }

  strategy_label <- if (strategy == "timeseries") {
    "Time-Series CV"
  } else {
    "Group K-Fold CV"
  }

  plot_spec_curve(
    plot_data,
    mode  = "delta",
    title = sprintf("Delta-Brier: Climate Predictive Value [%s]", strategy_label),
    y_lab = "Delta Brier (full − no-climate)",
    ...
  )
}


# ==============================================================================
# 7.  BATCH PLOTTING HELPERS
# ==============================================================================

#' Generate and save specification curves for all strategies in CV results
#'
#' @param cv_results_path  Path to CV results parquet.
#' @param output_dir  Directory for saved plots.
#' @param outcome  Outcome label for filenames.
#' @param width,height  Plot dimensions.
#' @param ...  Passed to plot_spec_curve_brier / plot_spec_curve_delta.
generate_cv_spec_curves <- function(cv_results_path,
                                     output_dir = "results/cv/plots",
                                     outcome    = "injuries",
                                     width      = 14,
                                     height     = 12,
                                     ...) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  cv_results <- arrow::read_parquet(cv_results_path)
  cat(sprintf("Loaded %d rows from %s\n", nrow(cv_results), cv_results_path))

  strategies <- unique(cv_results$cv_strategy)

  for (strat in strategies) {
    strat_short <- if (strat == "timeseries") "ts" else "gkf"

    # Brier spec curve
    cat(sprintf("  Brier spec curve [%s]...\n", strat))
    res <- tryCatch(
      plot_spec_curve_brier(cv_results, strategy = strat, ...),
      error = function(e) { warning(e$message); NULL }
    )
    if (!is.null(res)) {
      fpath <- file.path(output_dir,
                         sprintf("%s_spec_curve_brier_%s", outcome, strat_short))
      ggsave(paste0(fpath, ".pdf"), res$plot, width = width, height = height)
      ggsave(paste0(fpath, ".png"), res$plot, width = width, height = height,
             dpi = 150)
      cat(sprintf("    Saved: %s.{pdf,png}\n", fpath))
    }

    # Delta-Brier spec curve
    cat(sprintf("  Delta-Brier spec curve [%s]...\n", strat))
    res_d <- tryCatch(
      plot_spec_curve_delta(cv_results, strategy = strat, ...),
      error = function(e) { warning(e$message); NULL }
    )
    if (!is.null(res_d)) {
      fpath <- file.path(output_dir,
                         sprintf("%s_spec_curve_delta_%s", outcome, strat_short))
      ggsave(paste0(fpath, ".pdf"), res_d$plot, width = width, height = height)
      ggsave(paste0(fpath, ".png"), res_d$plot, width = width, height = height,
             dpi = 150)
      cat(sprintf("    Saved: %s.{pdf,png}\n", fpath))
    }
  }

  cat("Done.\n")
}


#' Generate specification curves for all outcomes × components (regular MV)
#'
#' @param results_with_config  Data frame with config + results.
#' @param output_dir  Directory for saved plots.
#' @param width,height  Plot dimensions.
#' @param ...  Passed to plot_spec_curve_estimate.
generate_mv_spec_curves <- function(results_with_config,
                                     output_dir = "results/multiverse/plots",
                                     width      = 14,
                                     height     = 12,
                                     ...) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  outcomes <- unique(results_with_config$outcome)

  for (out in outcomes) {
    for (comp in c("cond", "zi")) {

      est_var <- paste0("climate_estimate_", comp)
      sub_d <- results_with_config %>%
        filter(outcome == out, !is.na(.data[[est_var]]))

      if (nrow(sub_d) == 0) {
        cat(sprintf("  Skipping %s/%s (no data)\n", out, comp))
        next
      }

      cat(sprintf("  %s / %s ...\n", out, comp))
      res <- tryCatch(
        plot_spec_curve_estimate(results_with_config,
                                 outcome_filter = out,
                                 component      = comp,
                                 ...),
        error = function(e) { warning(e$message); NULL }
      )

      if (!is.null(res)) {
        fpath <- file.path(output_dir, sprintf("%s_%s_spec_curve", out, comp))
        ggsave(paste0(fpath, ".pdf"), res$plot, width = width, height = height)
        ggsave(paste0(fpath, ".png"), res$plot, width = width, height = height,
               dpi = 150)
        cat(sprintf("    Saved: %s.{pdf,png}\n", fpath))
      }
    }
  }

  cat("Done.\n")
}
source("3_rail_multiverse_postprocessing_w_ops.R")
# Link to config data
mv_results_rail <- link_results_to_config_railway(
  arrow::read_parquet(glue::glue("results/cv/{outcome}/rail_{outcome}_cv_results.parquet")),,
  config_registry_path = here::here(cfg_dir,'config_registry.csv')
) |>
  mutate(
    th__k = if_else(comp__apply_over == 'all', 'none',th__k)
  )

res <- plot_spec_curve_brier(cv_results = mv_results_rail,
                             strategy = 'timeseries')
res$plot
