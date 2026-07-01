# =============================================================================
# common/manuscript_figures.R — Single-best-spec figures for the manuscript
# =============================================================================
#
# Builds the headline figures and tables for the simplified manuscript:
#
#   build_champion_manuscript_table() — one row per (industry × outcome × CV
#     strategy) with the champion spec's climate estimate, p-value, Δll, and
#     "% multiverse positive" robustness summary
#
#   refit_champion_and_predict() — re-fits the champion panel-rate model on
#     the full panel data for one (industry × outcome) and computes
#     predicted rates across a climate-score grid. Building block for the
#     partial-dependence plot.
#
#   plot_champion_forest() — Figure 1: forest plot of climate coefficients
#     with 95% CI across all (industry × outcome) cells. Lead manuscript
#     figure.
#
#   plot_champion_partial_dependence() — Figure 2: faceted grid of
#     predicted rate vs climate score for each champion model.
#
# Designed for the three-industry manuscript (rail, NRC, aviation) — PHMSA
# is excluded from the driver but the functions work on any industry list.
#
# Dependencies: dplyr, ggplot2, glmmTMB, broom.mixed, patchwork, scales.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(arrow)
  library(stringr)
  library(glmmTMB)
  library(broom.mixed)
})


# Outcome labels: canonical definition lives in common/postprocessing.R
# (sourced more universally). Defined here only as a fallback for contexts that
# source manuscript_figures.R without postprocessing.R.
if (!exists("pretty_outcome")) {
  OUTCOME_LABELS <- c(
    rate_accidents = "Accidents", rate_injuries = "Injuries", rate_fatalities = "Fatalities",
    rate_lers = "Licensee event reports", rate_emerg = "Emergency declarations",
    rate_scrams = "Scrams", rate_pct_power_loss = "Percent power loss",
    rate_aids_noharm = "No-harm events (AIDS)", rate_aids_propdamage = "Property-damage events (AIDS)",
    rate_ntsb_serious_fatal = "Serious/fatal accidents (NTSB)", rate_ntsb_fatal = "Fatal accidents (NTSB)",
    rate_t0 = "No-injury reports", rate_t1 = "Severe injury (fatal/disability)",
    rate_t2 = "Lost-time injury", rate_t3 = "Minor injury", rate_days_lost = "Lost workdays")
  pretty_outcome <- function(outcome) {
    out <- unname(OUTCOME_LABELS[outcome]); miss <- is.na(out)
    if (any(miss)) out[miss] <- tools::toTitleCase(str_replace_all(outcome[miss], "_", " "))
    out
  }
}


# =============================================================================
# 1. CHAMPION SUMMARY TABLE
# =============================================================================

#' Build a tidy table of champion specs ready for the forest plot.
#'
#' For each (industry × outcome × cv_strategy), pulls the champion-best row
#' from `champions_best.csv` (written by best_model_analysis.R via
#' generate_industry_figures), and joins the multiverse-robustness summary
#' (% of specifications with Δll > 0 in that CV strategy).
#'
#' @param industries Named list of industry configs (same as 4_plots_panel.R).
#' @param report_dir Top-level report_figures dir (default
#'   "report_figures_panel").
#' @param cv_strategies Strategies to include (default: c("timeseries",
#'   "group_kfold")). The forest plot reads from this table.
#' @return Tibble with columns: industry, industry_label, outcome,
#'   outcome_label, cv_strategy, best_config_id, best_climate_var,
#'   best_delta_ll, climate_estimate, climate_pval, climate_ci_low,
#'   climate_ci_high, n_multiverse, pct_positive_dll, plus champion-spec
#'   facet columns (embedding_model, sent_method, comp__method, window_type,
#'   window_size, lag_days, halflife_days).
build_champion_manuscript_table <- function(industries,
                                              report_dir = "report_figures_panel",
                                              cv_strategies = c("timeseries",
                                                                "group_kfold")) {
  rows <- list()
  for (ind in names(industries)) {
    cp_path <- file.path(report_dir, ind, "champions_best.csv")
    if (!file.exists(cp_path)) {
      warning(sprintf("No champions_best.csv for industry %s", ind))
      next
    }
    champ <- readr::read_csv(cp_path, show_col_types = FALSE)
    champ$industry       <- ind
    champ$industry_label <- industries[[ind]]$label %||% toupper(ind)

    # Pull CI for the climate coefficient — already in MV results, joined
    # in compute_champions(). Coefficient SE → 95% CI via normal approximation.
    if (!"climate_ci_low" %in% names(champ) &&
        all(c("climate_estimate", "climate_pval") %in% names(champ))) {
      # Derive SE from the two-sided p-value, then 95% CI. This is only
      # used if the upstream champions_best.csv didn't preserve CIs.
      z <- qnorm(1 - champ$climate_pval / 2)
      se <- abs(champ$climate_estimate) / z
      champ$climate_ci_low  <- champ$climate_estimate - 1.96 * se
      champ$climate_ci_high <- champ$climate_estimate + 1.96 * se
    }

    # Multiverse robustness: % of specs with Δll > 0 in each CV strategy
    cv_path <- industries[[ind]]$cv_results
    if (!is.null(cv_path) && file.exists(cv_path)) {
      cv <- arrow::read_parquet(cv_path)
      if (!"panel_outcome" %in% names(cv) && "outcome" %in% names(cv)) {
        cv <- cv %>% mutate(panel_outcome = outcome)
      }
      mv_summary <- cv %>%
        filter(model_label == "delta_climate_vs_seasonal",
               !is.na(loglik_mean)) %>%
        group_by(panel_outcome, cv_strategy) %>%
        summarise(n_multiverse     = n(),
                  pct_positive_dll = round(100 * mean(loglik_mean > 0), 1),
                  median_dll       = median(loglik_mean, na.rm = TRUE),
                  .groups = "drop") %>%
        rename(outcome = panel_outcome)
      champ <- champ %>%
        left_join(mv_summary, by = c("outcome", "cv_strategy"))
    }

    rows[[ind]] <- champ
  }
  out <- bind_rows(rows) %>% filter(cv_strategy %in% cv_strategies)

  # Pretty outcome label (central map; descriptive mining labels, AIDS/NTSB casing)
  out <- out %>% mutate(outcome_label = pretty_outcome(outcome))

  out
}


# =============================================================================
# 2. CHAMPION REFIT + PARTIAL DEPENDENCE
# =============================================================================

#' Refit the champion model on the full panel and compute predictions across
#' the climate-score range.
#'
#' @param industry_key Industry key (e.g., "rail").
#' @param outcome Panel outcome name (e.g., "rate_injuries").
#' @param cv_strategy Which CV strategy's champion to use.
#' @param champion_table Output of build_champion_manuscript_table().
#' @param panel_data_fn Function(parquet_path, config_id) -> panel tibble.
#'   Same closure used by the multiverse driver for this industry.
#' @param cfg_dir Climate config directory for this industry.
#' @param fit_fn Industry-specific fit function:
#'   function(panel, climate_var, config_id, outcome) -> fitted glmmTMB list
#'   that includes $fit (the glmmTMB object).
#'   If NULL, we refit using a generic formula derived from the panel-rate
#'   modeling registry (PANEL_OUTCOME_VARS_<IND>).
#' @param outcome_var Industry-specific outcome COLUMN name (e.g.,
#'   "sum_fatalities" for aviation, "sum_killed" for rail). Required because
#'   the same panel-outcome key (e.g., "rate_fatalities") maps to different
#'   underlying count columns across industries.
#' @param grid_n Number of climate-score grid points (default 80).
#' @param hold_at "median" or "mean" for non-climate covariates. Default median.
#' @return List with $champ_row (one row from champion_table), $fit (glmmTMB
#'   model), $pd_data (tibble with climate, predicted_log_rate,
#'   predicted_rate, ci_low, ci_high, climate_percentile).
refit_champion_and_predict <- function(industry_key,
                                        outcome,
                                        cv_strategy,
                                        champion_table,
                                        panel_data_fn,
                                        cfg_dir,
                                        fit_fn,
                                        outcome_offset,
                                        outcome_bw_terms,
                                        outcome_org_var,
                                        outcome_var      = NULL,
                                        outcome_family   = "nbinom2",
                                        grid_n           = 80L,
                                        hold_at          = c("median", "mean")) {

  hold_at <- match.arg(hold_at)

  champ_row <- champion_table %>%
    filter(industry == industry_key, outcome == !!outcome,
           cv_strategy == !!cv_strategy)
  if (nrow(champ_row) != 1) {
    stop(sprintf("Expected exactly 1 champion for (%s, %s, %s); got %d",
                 industry_key, outcome, cv_strategy, nrow(champ_row)))
  }
  champ_row <- champ_row[1, ]
  cid <- as.character(champ_row$best_config_id)
  cv  <- champ_row$best_climate_var

  # 1. Re-prepare the panel for this config
  parquet_path <- file.path(cfg_dir, "_cfg", cid, "results.parquet")
  panel <- panel_data_fn(parquet_path = parquet_path, config_id = cid)
  if (is.null(panel) || nrow(panel) == 0) {
    stop(sprintf("Panel prep returned empty for %s config %s",
                 industry_key, cid))
  }

  # 2. Re-fit (use the industry's fit_fn if provided, else generic NB/Tweedie)
  res <- fit_fn(panel, cv, cid, outcome)
  if (is.null(res$fit) && !is.null(res$status) && res$status == "failed") {
    stop(sprintf("Champion refit failed for (%s, %s): %s",
                 industry_key, outcome, res$error %||% ""))
  }

  # Some industry fit_fns return only summary list, not the model itself.
  # Fall back to fitting directly here.
  if (is.null(res$fit)) {
    fam <- switch(outcome_family,
                  nbinom2 = glmmTMB::nbinom2(),
                  tweedie = glmmTMB::tweedie(link = "log"),
                  poisson = stats::poisson(link = "log"))
    re_term <- sprintf("(1 | %s)", outcome_org_var)
    # Reconstruct formula. Try a common shape; let R complain if columns missing.
    rhs <- c(cv,
             intersect(c("yearmonth_num_c", "sin_month", "cos_month"),
                       names(panel)),
             intersect(outcome_bw_terms, names(panel)),
             sprintf("offset(log(%s))", outcome_offset),
             re_term)
    yvar <- outcome_var %||% panel_outcome_to_var(outcome)
    fml <- as.formula(paste(yvar, "~", paste(rhs, collapse = " + ")))
    # complete-case panel
    cc_cols <- c(yvar, cv,
                 intersect(c("yearmonth_num_c", "sin_month", "cos_month"),
                            names(panel)),
                 intersect(outcome_bw_terms, names(panel)),
                 outcome_org_var, outcome_offset)
    d <- panel %>%
      filter(if_all(all_of(cc_cols), ~ !is.na(.x)),
             .data[[outcome_offset]] > 0)
    fit <- glmmTMB::glmmTMB(fml, data = d, family = fam)
    res$fit <- fit
    res$panel_data <- d
  } else {
    res$panel_data <- panel
  }

  # 3. Build prediction grid across the climate-score range
  d <- res$panel_data
  clim_vals <- d[[cv]]
  clim_vals <- clim_vals[is.finite(clim_vals)]
  clim_grid <- seq(quantile(clim_vals, 0.02, na.rm = TRUE),
                   quantile(clim_vals, 0.98, na.rm = TRUE),
                   length.out = grid_n)

  hold_fn <- if (hold_at == "median") median else mean
  newd <- tibble(.x = clim_grid)
  newd[[cv]] <- clim_grid
  # Temporal controls
  for (tc in intersect(c("yearmonth_num_c", "sin_month", "cos_month"),
                        names(d))) {
    newd[[tc]] <- hold_fn(d[[tc]], na.rm = TRUE)
  }
  # Ops covariates
  for (bw in intersect(outcome_bw_terms, names(d))) {
    newd[[bw]] <- hold_fn(d[[bw]], na.rm = TRUE)
  }
  # Exposure (offset) — set to median so back-transformed rate is "per
  # median-org-period exposure"; partial-dependence plot will divide by
  # this to give per-unit-exposure rate when desired.
  newd[[outcome_offset]] <- hold_fn(d[[outcome_offset]], na.rm = TRUE)
  # Random-intercept group — use the first level (re.form = NA in predict
  # marginalizes the RE anyway, so this is just for type-checking)
  newd[[outcome_org_var]] <- d[[outcome_org_var]][1]

  # 4. Predict on link scale with SE (marginalize RE: re.form = NA)
  pred <- predict(res$fit, newdata = newd, type = "link",
                  re.form = NA, se.fit = TRUE)
  link  <- as.numeric(pred$fit)
  link_se <- as.numeric(pred$se.fit)

  exposure_median <- hold_fn(d[[outcome_offset]], na.rm = TRUE)
  # Convert link → response, then divide by exposure to get rate per unit
  pd <- tibble(
    climate        = clim_grid,
    climate_pctile = ecdf(clim_vals)(clim_grid) * 100,
    pred_log_rate  = link - log(exposure_median),
    pred_rate      = exp(link - log(exposure_median)),
    ci_low         = exp(link - 1.96 * link_se - log(exposure_median)),
    ci_high        = exp(link + 1.96 * link_se - log(exposure_median))
  )

  list(champ_row = champ_row,
       fit       = res$fit,
       pd_data   = pd,
       panel     = d,
       clim_var  = cv,
       cfg_id    = cid)
}


#' Helper: map a panel outcome name to its outcome-count variable name.
#' This consults the per-industry PANEL_OUTCOME_VARS_<X> registry in the
#' caller's environment, or falls back to a small hardcoded map.
panel_outcome_to_var <- function(outcome) {
  m <- list(
    rate_accidents         = "n_accidents",
    rate_injuries          = "sum_injured",
    rate_fatalities        = "sum_killed",
    rate_lers              = "n_lers",
    rate_emerg             = "n_emerg",
    rate_scrams            = "n_scrams",
    rate_pct_power_loss    = "sum_pct_power_loss",
    rate_aids_all          = "n_aids_all",
    rate_aids_incidents_only = "n_aids_incidents",
    rate_inj_serious_fatal = "sum_serious_fatal"
  )
  # Aviation rate_accidents / rate_fatalities use different cols than rail
  # — caller should provide a map override when needed.
  if (outcome %in% names(m)) m[[outcome]] else outcome
}


# =============================================================================
# 3. FOREST PLOT (FIGURE 1)
# =============================================================================

#' Forest plot of best-performing-model climate coefficients with 95% CI,
#' annotated with Δll and % multiverse positive.
#'
#' @param champion_table Output of build_champion_manuscript_table().
#' @param cv_strategy Restrict to one CV strategy (e.g., "timeseries"). NULL
#'   = both, with strategy as a row facet.
#' @param order_by "industry" (default) or "estimate" or "abs_estimate".
#' @param exclude_cells Character vector of "industry|outcome" strings to
#'   drop from this plot. Used to move rare-event cells to an appendix
#'   variant of the figure.
#' @param scale "log" (default) plots the climate coefficient on the log
#'   rate-ratio scale with a reference line at 0; "rate_ratio" plots
#'   exp(coefficient) with a reference line at 1 and a log-scaled x-axis.
#'   The two are produced as SEPARATE figures (see the driver).
#' @return ggplot object.
plot_champion_forest <- function(champion_table,
                                  cv_strategy = "timeseries",
                                  order_by    = c("industry", "estimate",
                                                  "abs_estimate"),
                                  exclude_cells = character(0),
                                  scale = c("log", "rate_ratio")) {
  order_by <- match.arg(order_by)
  scale    <- match.arg(scale)
  d <- champion_table
  if (!is.null(cv_strategy)) d <- d %>% filter(cv_strategy == !!cv_strategy)
  if (length(exclude_cells) > 0) {
    d <- d %>% filter(!paste(industry, outcome, sep = "|") %in% exclude_cells)
  }

  d <- d %>%
    mutate(.row_label = sprintf("%s — %s", industry_label, outcome_label),
           .sig       = case_when(
             climate_pval < 0.001 ~ "***",
             climate_pval < 0.01  ~ "**",
             climate_pval < 0.05  ~ "*",
             TRUE                 ~ ""
           ),
           .direction = ifelse(climate_estimate > 0, "positive", "negative"))

  d <- switch(order_by,
              # Within industry, order by severity rank (surfacing -> harm), not
              # alphabetically, so the detectability gradient reads top-to-bottom.
              industry      = d %>% mutate(.ord = outcome_rank(outcome)) %>%
                                    arrange(industry_label, .ord, outcome_label),
              estimate      = d %>% arrange(climate_estimate),
              abs_estimate  = d %>% arrange(desc(abs(climate_estimate))))

  d <- d %>% mutate(.row_label = factor(.row_label, levels = rev(.row_label)))

  # When ordered by industry, group rows into per-industry facet bands for
  # readability (only valid for industry ordering — estimate/abs_estimate need a
  # single global axis).
  do_facet <- order_by == "industry"
  if (do_facet) {
    d <- d %>% mutate(industry_label = factor(industry_label,
                                              levels = sort(unique(industry_label))))
  }

  # Map columns + reference line + axis label per display scale.
  if (scale == "rate_ratio") {
    d <- d %>% mutate(.est  = exp(climate_estimate),
                      .lo   = exp(climate_ci_low),
                      .hi   = exp(climate_ci_high))
    ref_line <- 1
    x_lab <- "Rate ratio per unit climate (best-performing model, 95% CI)"
  } else {
    d <- d %>% mutate(.est  = climate_estimate,
                      .lo   = climate_ci_low,
                      .hi   = climate_ci_high)
    ref_line <- 0
    x_lab <- "Climate coefficient (log rate-ratio, 95% CI)"
  }

  p <- ggplot(d, aes(x = .est, y = .row_label, color = .direction)) +
    geom_vline(xintercept = ref_line, linetype = "dashed", color = "gray50") +
    geom_pointrange(aes(xmin = .lo, xmax = .hi), size = 0.6) +
    geom_text(aes(label = .sig, x = .hi),
              hjust = -0.3, size = 4, color = "gray30",
              show.legend = FALSE) +
    scale_color_manual(values = c("positive" = "#d6604d",
                                   "negative" = "#4393c3"),
                        guide  = "none") +
    labs(
      x = x_lab,
      y = NULL,
      title = "Climate effect at the best-performing model",
      subtitle = if (!is.null(cv_strategy))
                   sprintf("Best-performing model identified by max Delta log-lik in %s CV",
                           cv_strategy)
                 else "Both CV strategies"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          plot.title.position = "plot")

  if (do_facet) {
    p <- p +
      facet_grid(rows = vars(industry_label), scales = "free_y", space = "free_y",
                 switch = "y") +
      scale_y_discrete(labels = function(x) sub("^.* — ", "", x)) +
      theme(panel.spacing.y = grid::unit(5, "pt"),
            strip.placement = "outside",
            strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1),
            strip.background = element_rect(fill = "grey92", colour = NA))
  }

  # Rate-ratio scale reads most naturally on a log x-axis (multiplicative
  # symmetry: 0.5x and 2x are equidistant from 1). Plain-number labels
  # (no scientific notation) are easier to read.
  if (scale == "rate_ratio") {
    p <- p + scale_x_log10(labels = scales::label_number(drop0trailing = TRUE))
  }
  p
}


#' Companion table-as-figure for the forest plot: a kable-style overlay
#' showing Δll and % multiverse positive next to each row.
#'
#' @param champion_table Output of build_champion_manuscript_table().
#' @param cv_strategy CV strategy to filter to.
#' @param scale "log" (default) reports the climate coefficient and CI on
#'   the log rate-ratio scale; "rate_ratio" reports exp(coefficient) and
#'   exp(CI) for interpretability. Produced as SEPARATE tables (see driver).
build_champion_annotation_table <- function(champion_table,
                                              cv_strategy = "timeseries",
                                              scale = c("log", "rate_ratio")) {
  scale <- match.arg(scale)
  d <- champion_table %>% filter(cv_strategy == !!cv_strategy)

  base_tbl <- d %>%
    transmute(industry_label, outcome_label,
              `Best Δll`             = round(best_delta_ll, 4),
              .est = climate_estimate, .lo = climate_ci_low,
              .hi = climate_ci_high,
              `p`                    = signif(climate_pval, 2),
              `% MV +`               = paste0(pct_positive_dll, "%"),
              `Best-performing config` = sprintf("emb=%s; sent=%s; comp=%s",
                                                embedding_model %||% "",
                                                sent_method %||% "",
                                                comp__method %||% ""),
              `Window`               = case_when(
                window_type == "sma"  ~ sprintf("SMA-%s", window_size),
                window_type == "ewma" ~ sprintf("EWMA lag=%sd hl=%sd",
                                                 lag_days, halflife_days),
                TRUE ~ window_type
              ))

  if (scale == "rate_ratio") {
    base_tbl %>%
      mutate(`Rate ratio`     = round(exp(.est), 3),
             `95% CI (RR)`    = sprintf("[%.2f, %.2f]", exp(.lo), exp(.hi))) %>%
      select(industry_label, outcome_label, `Best Δll`,
             `Rate ratio`, `95% CI (RR)`, `p`, `% MV +`,
             `Best-performing config`, `Window`)
  } else {
    base_tbl %>%
      mutate(`Climate estimate` = round(.est, 3),
             `95% CI`           = sprintf("[%.2f, %.2f]", .lo, .hi)) %>%
      select(industry_label, outcome_label, `Best Δll`,
             `Climate estimate`, `95% CI`, `p`, `% MV +`,
             `Best-performing config`, `Window`)
  }
}


#' Per-SD forest plot of the best-performing-model climate effect.
#'
#' This is the MAIN-TEXT Figure 1A. Unlike plot_champion_forest() (which
#' exponentiates the raw per-unit coefficient — uninterpretable because a
#' 1.0-unit change in the climate score is far outside its observed range),
#' this plots the effect of a realistic +1-SD change in climate. Inputs are
#' the standardized coefficients from extract_champion_standardized_coefs(),
#' whose estimate_std = raw coefficient x in-panel SD of climate.
#'
#' @param coefs_long Output of extract_champion_standardized_coefs().
#' @param scale "rate_ratio" (default; exp of the per-SD coefficient, ref=1,
#'   log x-axis) or "log" (per-SD coefficient, ref=0).
#' @param exclude_cells Character vector of "industry|outcome" to drop.
#' @return ggplot object.
plot_bestmodel_forest_per_sd <- function(coefs_long,
                                          scale = c("rate_ratio", "log"),
                                          exclude_cells = character(0)) {
  scale <- match.arg(scale)
  d <- coefs_long %>% filter(variable_type == "climate")
  if (length(exclude_cells) > 0) {
    d <- d %>% filter(!paste(industry, outcome, sep = "|") %in% exclude_cells)
  }

  if (scale == "rate_ratio") {
    d <- d %>% mutate(.e = exp(estimate_std), .lo = exp(ci_low_std), .hi = exp(ci_high_std))
    ref <- 1
    x_lab <- "Rate ratio per +1 SD of climate (95% CI)"
  } else {
    d <- d %>% mutate(.e = estimate_std, .lo = ci_low_std, .hi = ci_high_std)
    ref <- 0
    x_lab <- "Standardized climate coefficient (log rate-ratio per SD, 95% CI)"
  }

  # Group rows by industry via faceting (one band per industry, with a strip
  # header), so the now-longer cell list reads clearly. Row keys stay unique
  # ("industry — outcome") to avoid collisions when an outcome name (e.g. "Rate
  # Accidents") recurs across industries; the industry prefix is stripped from
  # the visible tick label since the strip already names the industry.
  d <- d %>%
    mutate(.row = sprintf("%s — %s", industry_label, outcome_label),
           .dir = ifelse(estimate_std > 0, "positive", "negative"),
           .det = detectability_class(industry, outcome),
           industry_label = factor(industry_label, levels = sort(unique(industry_label))),
           .ord = outcome_rank(outcome)) %>%
    arrange(industry_label, .ord, outcome_label) %>%
    mutate(.row = factor(.row, levels = rev(.row)))

  # Detectability swatch sits in a gutter just left of every CI (a multiplicative
  # pad on the log axis, an additive pad on the linear/log-coefficient axis).
  rng <- range(c(d$.lo, d$.hi), na.rm = TRUE)
  gut <- if (scale == "rate_ratio") rng[1] * 0.82 else rng[1] - 0.12 * diff(rng)

  p <- ggplot(d, aes(.e, .row, color = .dir)) +
    geom_vline(xintercept = ref, linetype = "dashed", color = "gray50") +
    geom_pointrange(aes(xmin = .lo, xmax = .hi), size = 0.6) +
    # row-level detectability tile; constant border colour overrides the
    # inherited .dir mapping so the swatch fill alone carries detectability.
    geom_point(aes(x = gut, fill = .det), shape = 22, size = 3.8,
               colour = "grey35", stroke = 0.3) +
    facet_grid(rows = vars(industry_label), scales = "free_y", space = "free_y",
               switch = "y") +
    scale_y_discrete(labels = function(x) sub("^.* — ", "", x)) +
    scale_color_manual(values = c(positive = "#d6604d", negative = "#4393c3"),
                        guide = "none") +
    scale_fill_manual(values = DETECTABILITY_COL, name = "Detectability",
                      drop = FALSE, na.translate = FALSE) +
    guides(fill = guide_legend(ncol = 3, override.aes = list(size = 4, shape = 22))) +
    labs(x = x_lab, y = NULL,
         title = "Climate effect per standard deviation of climate (best-performing model)",
         subtitle = paste0("Effect of a realistic (1-SD) shift in organizational climate; ",
                           "reference line = no effect. Red = elevated, blue = protective.\n",
                           "Left-margin swatch = detectability (likelihood a missed report ",
                           "would be caught): Low / Moderate / High.")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(), plot.title.position = "plot",
          panel.spacing.y = grid::unit(5, "pt"),
          legend.position = "bottom",
          legend.text = element_text(size = 9), legend.title = element_text(size = 9),
          plot.subtitle = element_text(color = "gray40"),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1),
          strip.background = element_rect(fill = "grey92", colour = NA))

  if (scale == "rate_ratio") {
    # Per-SD rate ratios cluster tightly around 1, so the default log10 breaks
    # (powers of 10) leave only the "1" tick in range — no scale above 1.
    # Use a geometric ladder that brackets the observed CI range with ticks on
    # BOTH sides of the reference line.
    rng    <- range(c(d$.lo, d$.hi), na.rm = TRUE)
    ladder <- c(0.1, 0.25, 0.33, 0.5, 0.67, 0.8, 0.9, 1, 1.1, 1.25, 1.5, 2, 3, 4, 10)
    brks   <- ladder[ladder >= rng[1] * 0.97 & ladder <= rng[2] * 1.03]
    if (length(brks) < 3) brks <- scales::breaks_log(6)(rng)
    p <- p + scale_x_log10(breaks = brks,
                           labels = scales::label_number(drop0trailing = TRUE))
  }
  p
}


# =============================================================================
# 3b. PANEL-CELL DIAGNOSTIC FIGURE
# =============================================================================

#' Build the panel-cell diagnostic table: median n_obs, pct_zero, and
#' derived n_nonzero per (industry × outcome). Used to identify rare-event
#' cells with too few non-zero observations for stable inference.
#'
#' @param mv_paths Named list of MV parquet paths keyed by industry.
#' @return Tibble with industry, outcome, median_n_obs, median_pct_zero,
#'   median_n_nonzero.
build_panel_diagnostic_table <- function(mv_paths) {
  out <- purrr::map_dfr(names(mv_paths), function(ind) {
    if (!file.exists(mv_paths[[ind]])) return(NULL)
    mv <- arrow::read_parquet(mv_paths[[ind]])
    mv %>%
      filter(status %in% c("success", "convergence_warning")) %>%
      group_by(outcome) %>%
      summarise(
        median_n_obs     = median(n_obs, na.rm = TRUE),
        median_pct_zero  = median(pct_zero_outcome, na.rm = TRUE),
        median_n_nonzero = median(n_obs * (1 - pct_zero_outcome / 100),
                                   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(industry = ind)
  })
  out %>% relocate(industry)
}


#' Diagnostic figure: shows where each cell sits in
#' (pct_zero, n_nonzero, CI width) space.
#'
#' Cells in the lower-left of the second panel (high pct_zero, low n_nonzero,
#' wide CIs) are the rare-event cells dropped from the main forest plot.
#'
#' @param champion_table Output of build_champion_manuscript_table().
#' @param diag_table Output of build_panel_diagnostic_table().
#' @param cv_strategy CV strategy to use (default "timeseries").
#' @param rare_cells Optional vector of "industry|outcome" rare-event cells
#'   to highlight with a label.
#' @return patchwork object (two panels).
plot_panel_diagnostic <- function(champion_table,
                                    diag_table,
                                    cv_strategy = "timeseries",
                                    rare_cells  = character(0)) {

  ct <- champion_table %>%
    filter(cv_strategy == !!cv_strategy) %>%
    mutate(ci_width = climate_ci_high - climate_ci_low,
            .cell = paste(industry, outcome, sep = "|"))

  d <- ct %>%
    left_join(diag_table, by = c("industry", "outcome")) %>%
    mutate(.lab = sprintf("%s — %s",
                          industry_label,
                          outcome_label),
           .rare = .cell %in% rare_cells)

  base_theme <- theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())

  # Panel A: pct_zero vs n_obs (log), bubbles colored by industry, shaped by rare-event status
  pA <- ggplot(d, aes(x = median_pct_zero,
                       y = median_n_obs,
                       color = industry_label,
                       shape = .rare)) +
    geom_point(size = 4, alpha = 0.85) +
    scale_y_log10(labels = scales::label_comma()) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4),
                        labels = c("FALSE" = "Main figure",
                                    "TRUE"  = "Rare-event (appendix)"),
                        name = NULL) +
    ggrepel::geom_text_repel(aes(label = ifelse(.rare,
                                                 paste0(industry_label, " ",
                                                         outcome_label),
                                                 "")),
                              size = 3, max.overlaps = Inf,
                              segment.alpha = 0.4,
                              show.legend = FALSE) +
    labs(
      x = "Panel-cell zero rate (% of org-period observations with 0 events)",
      y = "Median panel observations (log scale)",
      color = "Industry",
      title = "A. Sample size vs. zero-inflation"
    ) +
    base_theme

  # Panel B: n_nonzero (log) vs CI width
  pB <- ggplot(d, aes(x = median_n_nonzero,
                       y = ci_width,
                       color = industry_label,
                       shape = .rare)) +
    geom_point(size = 4, alpha = 0.85) +
    scale_x_log10(labels = scales::label_comma()) +
    scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4),
                        labels = c("FALSE" = "Main figure",
                                    "TRUE"  = "Rare-event (appendix)"),
                        name = NULL) +
    ggrepel::geom_text_repel(aes(label = ifelse(.rare,
                                                 paste0(industry_label, " ",
                                                         outcome_label),
                                                 "")),
                              size = 3, max.overlaps = Inf,
                              segment.alpha = 0.4,
                              show.legend = FALSE) +
    labs(
      x = "Median non-zero panel observations (log scale)",
      y = "Best-performing-model 95% CI width (log scale)",
      color = "Industry",
      title = "B. Estimate precision vs. non-zero count"
    ) +
    base_theme

  (pA | pB) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = "Panel-cell diagnostics: zero-inflation, sample size, and precision",
      subtitle = paste0(
        "Each point is one (industry × outcome) cell. Non-zero count = ",
        "n_obs * (1 - pct_zero). The two rare-event aviation cells have ",
        "~20-40 non-zero observations and correspondingly wide CIs (>100 ",
        "log-units); they are reported in the appendix only."
      ),
      theme = theme(plot.subtitle = element_text(color = "gray40"))
    )
}


# =============================================================================
# 3c. STANDARDIZED-COEFFICIENT GRID (FIGURE 1 SUB-PANELS)
# =============================================================================

#' Extract champion-spec standardized coefficients across all predictors.
#'
#' Reads from the refit fits in pd_results (which already have the panel +
#' fit objects). For each (industry × outcome) cell, returns one row per
#' predictor (climate + ops between/within), with the coefficient
#' standardized by the in-panel SD of that predictor so that effects are
#' comparable across cells.
#'
#' For Mundlak between/within ops covariates the panel already has them
#' z-scored, so σ ≈ 1 and the standardized coefficient ≈ the raw coefficient.
#' For the climate variable, σ varies per panel.
#'
#' @param pd_results Named list keyed by "industry|outcome" with $fit,
#'   $panel, $clim_var, $champ_row (from refit_champion_and_predict).
#' @param champion_table Output of build_champion_manuscript_table().
#' @param drop_temporal Drop temporal-control coefficients (yearmonth_num_c,
#'   sin_month, cos_month) from the output. Default TRUE.
#' @return Tibble: industry, outcome, industry_label, outcome_label,
#'   variable, variable_label, variable_type, estimate_std, ci_low_std,
#'   ci_high_std, sigma_predictor.
extract_champion_standardized_coefs <- function(pd_results,
                                                  champion_table,
                                                  drop_temporal = TRUE) {

  rows <- list()
  for (k in names(pd_results)) {
    parts <- strsplit(k, "\\|")[[1]]
    ind   <- parts[1]
    outc  <- parts[2]
    res   <- pd_results[[k]]
    fit   <- res$fit
    panel <- res$panel
    clim_var <- res$clim_var

    # Industry / outcome labels from the champion row
    ind_lab  <- res$champ_row$industry_label
    outc_lab <- res$champ_row$outcome_label

    tidy_fe <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)

    for (i in seq_len(nrow(tidy_fe))) {
      term <- tidy_fe$term[i]
      if (term == "(Intercept)") next
      if (drop_temporal && term %in% c("yearmonth_num_c",
                                         "sin_month", "cos_month")) next
      if (!term %in% names(panel)) next

      sigma <- sd(panel[[term]], na.rm = TRUE)
      if (!is.finite(sigma) || sigma == 0) sigma <- 1

      est_std   <- tidy_fe$estimate[i]   * sigma
      se_std    <- tidy_fe$std.error[i]  * sigma
      ci_low    <- est_std - 1.96 * se_std
      ci_high   <- est_std + 1.96 * se_std

      var_type <- dplyr::case_when(
        term == clim_var          ~ "climate",
        grepl("_between$", term)  ~ "between",
        grepl("_within$",  term)  ~ "within",
        TRUE                       ~ "other"
      )

      var_label <- if (term == clim_var) {
        "Climate"
      } else {
        # Convert "passenger_miles_between" → "Passenger Miles (between-org)"
        base <- sub("(_between|_within)$", "", term)
        suffix <- dplyr::case_when(
          grepl("_between$", term) ~ " (between-org)",
          grepl("_within$",  term) ~ " (within-org)",
          TRUE                      ~ ""
        )
        paste0(stringr::str_replace_all(base, "_", " ") |> tools::toTitleCase(),
                suffix)
      }

      rows[[length(rows) + 1L]] <- tibble(
        industry        = ind,
        outcome         = outc,
        industry_label  = ind_lab,
        outcome_label   = outc_lab,
        variable        = term,
        variable_label  = var_label,
        variable_type   = var_type,
        estimate_std    = est_std,
        ci_low_std      = ci_low,
        ci_high_std     = ci_high,
        sigma_predictor = sigma
      )
    }
  }
  bind_rows(rows)
}


#' Per-cell coefficient grid: climate alongside between/within ops covariates.
#'
#' @param coefs_long Output of extract_champion_standardized_coefs().
#' @param exclude_cells Character vector of "industry|outcome" to drop.
#' @param ncol Grid columns (default 3).
#' @return ggplot object.
#' Short industry label used for compact facet strips.
#' Maps the longer "Rail (FRA)" / "Aviation (NTSB / AIDS / ASRS)" labels to
#' single-word industry names that fit on one line of a facet strip.
.short_industry_label <- function(industry_label) {
  case_when(
    grepl("Rail",     industry_label, ignore.case = TRUE) ~ "Rail",
    grepl("Nuclear",  industry_label, ignore.case = TRUE) ~ "Nuclear",
    grepl("Aviation", industry_label, ignore.case = TRUE) ~ "Aviation",
    TRUE                                                   ~ industry_label
  )
}

plot_champion_coef_grid <- function(coefs_long,
                                      exclude_cells = character(0),
                                      ncol = 3L,
                                      scale = c("log", "rate_ratio")) {
  scale <- match.arg(scale)
  d <- coefs_long
  if (length(exclude_cells) > 0) {
    d <- d %>% filter(!paste(industry, outcome, sep = "|") %in% exclude_cells)
  }

  d <- d %>%
    # Two-line facet title: short industry on line 1, outcome on line 2.
    # The previous version used the long industry_label which truncated even
    # with a newline; using a short single-word industry name (Rail /
    # Nuclear / Aviation) keeps each line short enough to render fully.
    mutate(.cell = sprintf("%s\n%s",
                            .short_industry_label(industry_label),
                            outcome_label),
           # Order rows within each cell so climate sits at the top
           .order_key = ifelse(variable_type == "climate", 0,
                                rank(abs(estimate_std))) + 0.1,
           variable_label = forcats::fct_reorder2(variable_label,
                                                    .cell,
                                                    .order_key))

  # Map to display scale: log keeps standardized coefficients; rate_ratio
  # exponentiates the standardized coefficient and CI (= rate ratio per SD
  # of predictor) with a reference at 1 and a log-scaled x-axis.
  if (scale == "rate_ratio") {
    d <- d %>% mutate(.est = exp(estimate_std),
                      .lo  = exp(ci_low_std),
                      .hi  = exp(ci_high_std))
    ref_line <- 1
    x_lab <- "Rate ratio per SD of predictor"
    subtitle <- "Effects standardized by predictor SD then exponentiated; reference line at 1"
  } else {
    d <- d %>% mutate(.est = estimate_std,
                      .lo  = ci_low_std,
                      .hi  = ci_high_std)
    ref_line <- 0
    x_lab <- "Standardized coefficient (log rate-ratio per SD of predictor)"
    subtitle <- "All effects standardized by predictor SD; reference line at 0"
  }

  p <- ggplot(d, aes(x = .est, y = variable_label,
                color = variable_type)) +
    geom_vline(xintercept = ref_line, linetype = "dashed", color = "gray60") +
    geom_pointrange(aes(xmin = .lo, xmax = .hi),
                    size = 0.35) +
    facet_wrap(~ .cell, scales = "free", ncol = ncol) +
    scale_color_manual(
      values = c("climate" = "#d6604d",
                  "between" = "#1b9e77",   # green for between-org
                  "within"  = "#2c7bb6",   # blue for within-org
                  "other"   = "gray60"),
      labels = c("climate" = "Climate (standardized)",
                  "between" = "Operational, between-org",
                  "within"  = "Operational, within-org",
                  "other"   = "Other"),
      name   = NULL
    ) +
    labs(
      x = x_lab,
      y = NULL,
      title = "Best-performing-model coefficients: climate alongside operational covariates",
      subtitle = subtitle
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position  = "bottom",
          # Two-line strip needs substantial vertical room. Larger top/bottom
          # margins on the strip text + extra panel spacing prevents the
          # second line from being clipped by the facet boundary.
          strip.text       = element_text(face = "bold", size = 9,
                                            lineheight = 1.0,
                                            margin = margin(6, 4, 6, 4)),
          strip.background = element_rect(fill = "gray95", color = NA),
          panel.spacing.y  = unit(1.0, "lines"),
          panel.spacing.x  = unit(0.8, "lines"),
          panel.grid.minor = element_blank())

  # Rate-ratio scale reads most naturally on a log x-axis; plain-number
  # labels (no scientific notation) are easier to read.
  if (scale == "rate_ratio") {
    p <- p + scale_x_log10(labels = scales::label_number(drop0trailing = TRUE))
  }
  p
}


#' Composite Figure 1: forest plot on top, coefficient grid below.
#'
#' @param forest_plot ggplot from plot_champion_forest().
#' @param coef_grid_plot ggplot from plot_champion_coef_grid().
#' @param heights Relative heights for forest/grid (default c(1, 2)).
#' @return patchwork object.
plot_figure1_composite <- function(forest_plot, coef_grid_plot,
                                     heights = c(1, 2)) {
  forest_plot / coef_grid_plot +
    patchwork::plot_layout(heights = heights) +
    patchwork::plot_annotation(tag_levels = "A",
                                theme = theme(plot.tag = element_text(face = "bold")))
}


# =============================================================================
# 4. PARTIAL-DEPENDENCE PLOT (FIGURE 2)
# =============================================================================

#' Faceted partial-dependence grid: predicted rate vs climate score for each
#' (industry × outcome) champion model.
#'
#' @param pd_results Named list keyed by "industry|outcome" with elements
#'   from refit_champion_and_predict().
#' @param free_y If TRUE, each panel has its own y-axis. Default TRUE.
#' @param x_scale "raw" (climate score natural scale) or "percentile" (0-100
#'   climate-distribution rank). Default "percentile".
#' @param y_scale "rate" (predicted rate per unit exposure) or "rate_ratio"
#'   (predicted rate divided by the predicted rate at median climate;
#'   unitless, standardized across cells). Default "rate_ratio".
#' @param exclude_cells Character vector of "industry|outcome" to drop.
#' @return ggplot object.
plot_champion_partial_dependence <- function(pd_results,
                                              free_y  = TRUE,
                                              x_scale = c("percentile", "raw"),
                                              y_scale = c("rate_ratio", "rate"),
                                              exclude_cells = character(0)) {
  x_scale <- match.arg(x_scale)
  y_scale <- match.arg(y_scale)

  keys <- setdiff(names(pd_results), exclude_cells)

  all_pd <- purrr::map_dfr(keys, function(k) {
    parts <- stringr::str_split(k, "\\|")[[1]]
    pd <- pd_results[[k]]$pd_data

    if (y_scale == "rate_ratio") {
      # Anchor: predicted rate at the 50th percentile climate score (nearest
      # grid point). This is a more robust reference than the min/max edge.
      med_idx  <- which.min(abs(pd$climate_pctile - 50))
      ref_rate <- pd$pred_rate[med_idx]
      pd <- pd %>% mutate(
        plot_y      = pred_rate / ref_rate,
        plot_y_low  = ci_low    / ref_rate,
        plot_y_high = ci_high   / ref_rate
      )
    } else {
      pd <- pd %>% mutate(
        plot_y      = pred_rate,
        plot_y_low  = ci_low,
        plot_y_high = ci_high
      )
    }

    pd %>%
      mutate(industry       = parts[1],
              outcome        = parts[2],
              industry_label = pd_results[[k]]$champ_row$industry_label,
              outcome_label  = pd_results[[k]]$champ_row$outcome_label,
              facet_label    = sprintf("%s\n%s",
                                        industry_label, outcome_label))
  })

  if (x_scale == "percentile") {
    all_pd <- all_pd %>% mutate(.x = climate_pctile)
    x_lab <- "Climate-score percentile within panel"
  } else {
    all_pd <- all_pd %>% mutate(.x = climate)
    x_lab <- "Climate score"
  }

  if (y_scale == "rate_ratio") {
    y_lab <- "Predicted rate ratio (vs. median-climate baseline)"
    sub_lab <- "Rate ratio = predicted_rate(climate) / predicted_rate(median climate). 1.0 = no effect. Shaded band = 95% CI."
  } else {
    y_lab <- "Predicted rate per unit exposure"
    sub_lab <- "Shaded band = 95% CI; other covariates held at panel median."
  }

  p <- ggplot(all_pd, aes(x = .x, y = plot_y)) +
    geom_ribbon(aes(ymin = plot_y_low, ymax = plot_y_high),
                alpha = 0.18, fill = "#2c7bb6") +
    geom_line(color = "#2c7bb6", linewidth = 0.75)

  if (y_scale == "rate_ratio") {
    p <- p + geom_hline(yintercept = 1, linetype = "dashed",
                         color = "gray45", linewidth = 0.4)
  }

  p +
    facet_wrap(~ facet_label,
                scales = if (free_y) "free_y" else "fixed") +
    labs(x = x_lab, y = y_lab,
         title = "Predicted outcome rate vs. climate score (best-performing model)",
         subtitle = sub_lab) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_text(color = "gray40"))
}


# =============================================================================
# 5. MANUSCRIPT FACET HELPER (used by Figures 3 + 4)
# =============================================================================

#' Derive manuscript-level facet columns from raw config + climate_var fields.
#'
#' Collapses the registry's individual parameter columns into the five
#' decision strips reported in the manuscript:
#'
#'   window       — SMA-5, SMA-10, SMA-20, EWMA-360, EWMA-540, EWMA-720
#'                  (EWMA half-life is fixed at lag/2, so a single label per
#'                  lag suffices.)
#'   embedding    — embedding_model (3 levels).
#'   sentiment    — sent_method (3 levels).
#'   composite    — weighted_avg, attention(τ=0.15), attention(τ=1.0),
#'                  power(p=0.5), power(p=2.0). Combines comp__method with
#'                  comp__temperature (attention) / comp__power (power).
#'   thresholding — none (all-segments), top-3, top-5. Combines th__k with
#'                  comp__apply_over (the registry maps 1-to-1).
#'
#' @param df Tibble that already has columns: climate_var, embedding_model,
#'   sent_method, th__k, comp__method, comp__power, comp__temperature,
#'   comp__apply_over (i.e. CV/MV results joined to config_registry).
#' @return Tibble with five added factor columns: window, embedding,
#'   sentiment, composite, thresholding.
add_manuscript_facets <- function(df) {
  required <- c("climate_var", "embedding_model", "sent_method",
                "th__k", "comp__method", "comp__power",
                "comp__temperature", "comp__apply_over")
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) {
    stop(sprintf("add_manuscript_facets: missing columns: %s",
                  paste(miss, collapse = ", ")))
  }

  window_levels <- c("SMA-5", "SMA-10", "SMA-20",
                     "EWMA-360", "EWMA-540", "EWMA-720")
  composite_levels <- c("weighted_avg",
                        "attention(tau=0.15)", "attention(tau=1.0)",
                        "power(p=0.5)", "power(p=2.0)")
  thresholding_levels <- c("all-segments", "top-3", "top-5")

  out <- df %>%
    mutate(
      # ----- WINDOW (one strip combining window type, size, lag_days) -----
      window = case_when(
        stringr::str_detect(climate_var, "_sma_5$")      ~ "SMA-5",
        stringr::str_detect(climate_var, "_sma_10$")     ~ "SMA-10",
        stringr::str_detect(climate_var, "_sma_20$")     ~ "SMA-20",
        stringr::str_detect(climate_var, "ewmaLAG_360d") ~ "EWMA-360",
        stringr::str_detect(climate_var, "ewmaLAG_540d") ~ "EWMA-540",
        stringr::str_detect(climate_var, "ewmaLAG_720d") ~ "EWMA-720",
        TRUE                                              ~ NA_character_
      ),
      window = factor(window, levels = window_levels),

      # ----- EMBEDDING -----
      embedding = factor(embedding_model),

      # ----- SENTIMENT -----
      sentiment = factor(sent_method),

      # ----- COMPOSITE (one strip combining comp__method + sub-params) -----
      composite = case_when(
        comp__method == "weighted_average"  ~ "weighted_avg",
        comp__method == "attention_weight"  &
          abs(comp__temperature - 0.15) < 1e-6 ~ "attention(tau=0.15)",
        comp__method == "attention_weight"  &
          abs(comp__temperature - 1.0)  < 1e-6 ~ "attention(tau=1.0)",
        comp__method == "power_attention"   &
          abs(comp__power - 0.5) < 1e-6        ~ "power(p=0.5)",
        comp__method == "power_attention"   &
          abs(comp__power - 2.0) < 1e-6        ~ "power(p=2.0)",
        TRUE                                   ~ NA_character_
      ),
      composite = factor(composite, levels = composite_levels),

      # ----- THRESHOLDING (one strip combining th__k + apply_over) -----
      thresholding = case_when(
        th__k == 99999 | comp__apply_over == "all" ~ "all-segments",
        th__k == 3                                  ~ "top-3",
        th__k == 5                                  ~ "top-5",
        TRUE                                        ~ NA_character_
      ),
      thresholding = factor(thresholding, levels = thresholding_levels)
    )

  # Sanity-check: anything NA after the map is unmapped
  unmapped <- out %>%
    filter(is.na(window) | is.na(composite) | is.na(thresholding))
  if (nrow(unmapped) > 0) {
    warning(sprintf(
      "add_manuscript_facets: %d rows did not match any facet bucket.",
      nrow(unmapped)
    ))
  }
  out
}


# =============================================================================
# 6. CV SPECIFICATION CURVES ON Δll (FIGURE 3)
# =============================================================================

#' Assemble a tidy multiverse table of Δll across all (industry × outcome ×
#' cv_strategy × config × climate_var) cells, joined to manuscript facets.
#'
#' @param industries Same named list passed to build_champion_manuscript_table.
#' @param cv_strategy "timeseries" (default) or "group_kfold". Filters the
#'   returned rows.
#' @return Tibble: industry, industry_label, outcome (panel_outcome),
#'   outcome_label, config_id, climate_var, delta_ll, plus the five
#'   manuscript facet factors.
build_dll_multiverse_table <- function(industries,
                                        cv_strategy = "timeseries") {
  parts <- list()
  for (ind in names(industries)) {
    cv_path  <- industries[[ind]]$cv_results
    reg_path <- industries[[ind]]$config_registry
    if (is.null(cv_path) || !file.exists(cv_path)) {
      warning(sprintf("CV parquet missing for %s", ind))
      next
    }
    if (is.null(reg_path) || !file.exists(reg_path)) {
      warning(sprintf("Config registry missing for %s", ind))
      next
    }

    cv  <- arrow::read_parquet(cv_path)
    reg <- readr::read_csv(reg_path, show_col_types = FALSE)

    if (!"panel_outcome" %in% names(cv) && "outcome" %in% names(cv)) {
      cv <- cv %>% mutate(panel_outcome = outcome)
    }

    dll <- cv %>%
      filter(model_label == "delta_climate_vs_seasonal",
              cv_strategy == !!cv_strategy,
              !is.na(loglik_mean)) %>%
      transmute(industry        = ind,
                industry_label  = industries[[ind]]$label %||% toupper(ind),
                outcome         = panel_outcome,
                config_id       = as.character(config_id),
                climate_var     = climate_var,
                delta_ll        = loglik_mean,
                cv_strategy     = cv_strategy)

    reg <- reg %>% mutate(config_id = as.character(config_id))
    dll <- dll %>% left_join(reg, by = "config_id")

    parts[[ind]] <- dll
  }
  out <- bind_rows(parts)

  if (nrow(out) == 0) return(out)

  out <- out %>%
    add_manuscript_facets() %>%
    mutate(outcome_label = pretty_outcome(outcome))
  out
}


#' Spec curve on Δll for the manuscript, faceted by (industry × outcome) with
#' five colored decision strips below each curve.
#'
#' @param dll_table Output of build_dll_multiverse_table().
#' @param cells Character vector of "industry|outcome" keys to render. If
#'   NULL, renders every cell present in the table.
#' @param ncol Grid columns (default: length(cells), one row).
#' @param strip_height Relative height of each decision strip in the
#'   patchwork stack (default 0.18).
#' @return patchwork object (one column per cell, curve on top, then
#'   {window, embedding, sentiment, composite, thresholding} strips).
plot_cv_spec_curve_manuscript <- function(dll_table,
                                            cells = NULL,
                                            ncol  = NULL,
                                            strip_height = 0.18) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork required for plot_cv_spec_curve_manuscript")
  }

  if (is.null(cells)) {
    cells <- dll_table %>%
      mutate(key = paste(industry, outcome, sep = "|")) %>%
      pull(key) %>% unique()
  }

  # Color palettes (one per strip; muted distinct).
  pal_window     <- c("SMA-5" = "#a6cee3", "SMA-10" = "#1f78b4",
                       "SMA-20" = "#08306b",
                       "EWMA-360" = "#b2df8a", "EWMA-540" = "#33a02c",
                       "EWMA-720" = "#1b5e20")
  pal_embed      <- RColorBrewer::brewer.pal(3, "Set2")
  pal_sentiment  <- RColorBrewer::brewer.pal(3, "Set3")[1:3]
  pal_composite  <- c("weighted_avg" = "#7570b3",
                       "attention(tau=0.15)" = "#fdb462",
                       "attention(tau=1.0)"  = "#e7298a",
                       "power(p=0.5)"        = "#66c2a5",
                       "power(p=2.0)"        = "#1b9e77")
  pal_threshold  <- c("all-segments" = "#cccccc",
                       "top-3"        = "#fc8d59",
                       "top-5"        = "#b30000")

  build_cell <- function(key) {
    parts <- stringr::str_split(key, "\\|")[[1]]
    ind <- parts[1]; oc <- parts[2]
    d <- dll_table %>% filter(industry == ind, outcome == oc) %>%
      arrange(delta_ll) %>% mutate(rank = row_number())
    if (nrow(d) == 0) return(NULL)

    title <- sprintf("%s - %s",
                     d$industry_label[1], d$outcome_label[1])
    pct_pos <- mean(d$delta_ll > 0) * 100

    p_curve <- ggplot(d, aes(x = rank, y = delta_ll)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                  color = "gray40", linewidth = 0.4) +
      geom_point(aes(color = delta_ll > 0), size = 0.65, alpha = 0.7) +
      scale_color_manual(values = c("TRUE" = "#1b9e77",
                                     "FALSE" = "#b2182b"),
                          guide  = "none") +
      labs(title = title,
           subtitle = sprintf("%.1f%% of %d specs have delta_ll > 0",
                              pct_pos, nrow(d)),
           x = NULL, y = "Delta log-lik (climate vs seasonal+ops)") +
      theme_minimal(base_size = 10) +
      theme(plot.title    = element_text(face = "bold", size = 10),
            plot.subtitle = element_text(color = "gray40", size = 8),
            panel.grid.minor = element_blank(),
            axis.text.x   = element_blank(),
            axis.ticks.x  = element_blank())

    # Each strip uses a uniquely-named scale via `aes(fill = ...)` so that
    # patchwork's `guides = "collect"` can deduplicate identical legends
    # across panels. Within a single column of panels the strips have
    # different fill scales (window vs embedding vs ...), so they each get
    # their own collected legend at the bottom.
    make_strip <- function(var_sym, label, palette) {
      ggplot(d, aes(x = rank, y = 1, fill = !!var_sym)) +
        geom_tile() +
        scale_fill_manual(values = palette, name = label,
                           drop = FALSE, na.value = "white") +
        scale_y_continuous(expand = c(0, 0),
                            breaks = 1, labels = label) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 9) +
        theme(panel.grid       = element_blank(),
              axis.text.x      = element_blank(),
              axis.ticks       = element_blank(),
              axis.text.y      = element_text(size = 7),
              legend.position  = "bottom",
              legend.key.size  = unit(0.3, "cm"),
              legend.text      = element_text(size = 7),
              legend.title     = element_text(size = 7,
                                                face = "bold"))
    }

    # Use ggnewscale::new_scale_fill() to give each strip its own fill
    # legend within the per-panel stack, but mark them with consistent names
    # so patchwork collects them across panels.
    p_window <- make_strip(rlang::sym("window"),       "Window",       pal_window)
    p_embed  <- make_strip(rlang::sym("embedding"),    "Embedding",
                            setNames(pal_embed,     levels(d$embedding)))
    p_sent   <- make_strip(rlang::sym("sentiment"),    "Sentiment",
                            setNames(pal_sentiment, levels(d$sentiment)))
    p_comp   <- make_strip(rlang::sym("composite"),    "Composite",    pal_composite)
    p_thresh <- make_strip(rlang::sym("thresholding"), "Thresholding", pal_threshold)

    # IMPORTANT: do NOT apply `guides = "collect"` at this inner level.
    # If we did, each per-cell composite would bake its own legend block at
    # the bottom, and the outer wrap_plots() collect step below would no
    # longer see the individual strip guides — it would just see N finished
    # composites with N copies of the legend. Letting the strips keep their
    # own per-plot guides (legend.position = "bottom" on each) means the
    # outer collect walks all the way down through this nested structure
    # and pulls every unique fill scale into a single shared legend block
    # at the bottom of the FULL figure.
    p_curve / p_window / p_embed / p_sent / p_comp / p_thresh +
      patchwork::plot_layout(
        heights = c(1, strip_height, strip_height, strip_height,
                    strip_height, strip_height))
  }

  panels <- purrr::compact(purrr::map(cells, build_cell))
  if (length(panels) == 0) {
    stop("No cells matched; check 'cells' argument and dll_table contents.")
  }

  if (is.null(ncol)) ncol <- length(panels)

  # Outer-level guides="collect" walks the full nested patchwork tree and
  # collects unique fill-scale legends (one per design dimension) into a
  # single shared block at the bottom of the whole figure.
  #
  # Legend layout: each of the five design dimensions becomes its own
  # vertical column with the dimension name as the bolded header and the
  # levels stacked underneath. The five columns sit side-by-side along the
  # bottom of the figure, replicating the visual continuum-of-colors feel
  # of the per-cell strips and making efficient use of horizontal space.
  #
  #   Window      Embedding     Sentiment   Composite            Thresholding
  #   [SMA-5]     [MiniLM]      [VADER]     [weighted_avg]       [all-segments]
  #   [SMA-10]    [RoBERTa-L]   [RoBERTa]   [attention(t=0.15)]  [top-3]
  #   [SMA-20]    [BGE-M3]      [SiEBERT]   [attention(t=1.0)]   [top-5]
  #   [EWMA-360]                            [power(p=0.5)]
  #   [EWMA-540]                            [power(p=2.0)]
  #   [EWMA-720]
  #
  #   - legend.direction = "vertical" stacks chips top-to-bottom within
  #     each dimension (so the ordered scales — window, thresholding —
  #     read as a color continuum).
  #   - legend.box = "horizontal" arranges the five dimensions side-by-side.
  #   - legend.box.just = "top" aligns the bolded dimension headers along
  #     the top, so the legend titles form a horizontal row.
  patchwork::wrap_plots(panels, ncol = ncol, guides = "collect") &
    theme(legend.position  = "bottom",
          legend.direction = "vertical",
          legend.box       = "horizontal",
          legend.box.just  = "top",
          legend.spacing.x = unit(1.0, "lines"),
          legend.margin    = margin(4, 6, 4, 6),
          legend.title     = element_text(size = 8, face = "bold"),
          legend.text      = element_text(size = 7),
          legend.key.size  = unit(0.35, "cm"))
}


# =============================================================================
# 7. ANOVA-ON-Δll FACET IMPORTANCE (FIGURE 4)
# =============================================================================

#' For each cell, decompose variance in Δll across the five manuscript
#' facets via a Type-II ANOVA (sum-of-squares partition).
#'
#' Each row of dll_table is one specification; the response is delta_ll, and
#' the predictors are the five categorical facets. We fit lm(delta_ll ~
#' window + embedding + sentiment + composite + thresholding) per cell and
#' read out the proportion of total sum-of-squares attributable to each
#' factor (the residual term captures interactions + noise).
#'
#' @param dll_table Output of build_dll_multiverse_table().
#' @return Tibble: industry, industry_label, outcome, outcome_label,
#'   facet ("window"|"embedding"|"sentiment"|"composite"|"thresholding"|
#'   "residual"), ss, pct_var, n_specs.
compute_dll_facet_importance <- function(dll_table) {
  facets <- c("window", "embedding", "sentiment", "composite", "thresholding")

  dll_table %>%
    group_by(industry, industry_label, outcome, outcome_label) %>%
    group_modify(function(d, key) {
      if (nrow(d) < 10) {
        return(tibble(facet = character(0), ss = numeric(0),
                       pct_var = numeric(0), n_specs = integer(0)))
      }
      # Drop any rows missing facet values; ensure factors have ≥ 2 levels
      d2 <- d %>% drop_na(all_of(c(facets, "delta_ll")))
      use_facets <- facets[purrr::map_lgl(facets, function(f) {
        nlevels(droplevels(d2[[f]])) >= 2
      })]
      if (length(use_facets) == 0) {
        return(tibble(facet = character(0), ss = numeric(0),
                       pct_var = numeric(0), n_specs = integer(0)))
      }
      form <- as.formula(paste("delta_ll ~", paste(use_facets,
                                                      collapse = " + ")))
      fit  <- lm(form, data = d2)
      av   <- anova(fit)
      tibble(facet   = c(rownames(av)),
             ss      = av$`Sum Sq`,
             pct_var = 100 * av$`Sum Sq` / sum(av$`Sum Sq`),
             n_specs = nrow(d2))
    }) %>%
    ungroup() %>%
    mutate(facet = recode(facet, "Residuals" = "residual"))
}


#' Stacked bar chart: per-cell, share of Δll variance explained by each
#' facet (and the residual).
#'
#' @param importance_table Output of compute_dll_facet_importance().
#' @param exclude_residual If TRUE (default FALSE) drop the residual bar so
#'   the stack sums to "explained variance only".
#' @return ggplot object.
plot_dll_facet_importance <- function(importance_table,
                                        exclude_residual = FALSE) {
  facet_levels <- c("window", "embedding", "sentiment",
                    "composite", "thresholding", "residual")
  pal <- c(
    window       = "#1f78b4",
    embedding    = "#33a02c",
    sentiment    = "#e31a1c",
    composite    = "#ff7f00",
    thresholding = "#6a3d9a",
    residual     = "#bdbdbd"
  )

  d <- importance_table %>%
    mutate(facet = factor(facet, levels = facet_levels)) %>%
    arrange(industry, outcome, facet)

  if (exclude_residual) {
    d <- d %>%
      filter(facet != "residual") %>%
      group_by(industry, outcome) %>%
      mutate(pct_var = 100 * pct_var / sum(pct_var)) %>%
      ungroup()
    subtitle <- "Bars rescaled to 100% of EXPLAINED variance only (residual omitted)."
  } else {
    subtitle <- "Bars sum to 100% of total delta_ll variance; 'residual' = interactions + noise."
  }

  d <- d %>%
    mutate(cell_label = sprintf("%s - %s",
                                  industry_label, outcome_label))

  ggplot(d, aes(x = pct_var, y = cell_label, fill = facet)) +
    geom_col() +
    scale_fill_manual(values = pal, name = NULL,
                       breaks = facet_levels) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = "Share of delta_ll variance (%)",
         y = NULL,
         title = "Which design choice moves delta_ll the most?",
         subtitle = subtitle) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          plot.title.position = "plot",
          plot.subtitle = element_text(color = "gray40"),
          legend.position = "bottom")
}


# =============================================================================
# 8. TRADITIONAL SPECIFICATION CURVES (climate β across specs) — APPENDIX
# =============================================================================
#
# Companion to the Δlog-lik spec curves (Figure 3). The traditional spec
# curve plots the climate coefficient estimate (with 95% CI) sorted across
# all 810 specifications, with the same 5 facet strips below the curve.
# Reads directly from the multiverse results parquet rather than the
# cross-validated Δll table.

#' Assemble a tidy multiverse table of climate β across all (industry ×
#' outcome × config × climate_var) cells, joined to manuscript facets.
#'
#' @param industries Same named list passed to other manuscript builders.
#' @return Tibble: industry, industry_label, outcome (panel_outcome),
#'   outcome_label, config_id, climate_var, climate_estimate, climate_se,
#'   climate_ci_low, climate_ci_high, climate_pval, plus the five manuscript
#'   facet factors.
build_beta_multiverse_table <- function(industries) {
  parts <- list()
  for (ind in names(industries)) {
    mv_path  <- industries[[ind]]$mv_results
    reg_path <- industries[[ind]]$config_registry
    if (is.null(mv_path) || !file.exists(mv_path)) {
      warning(sprintf("MV parquet missing for %s", ind))
      next
    }
    if (is.null(reg_path) || !file.exists(reg_path)) {
      warning(sprintf("Config registry missing for %s", ind))
      next
    }

    mv  <- arrow::read_parquet(mv_path)
    reg <- readr::read_csv(reg_path, show_col_types = FALSE)

    mv_sel <- mv %>%
      filter(status %in% c("success", "convergence_warning"),
              !is.na(climate_estimate)) %>%
      transmute(industry         = ind,
                industry_label   = industries[[ind]]$label %||% toupper(ind),
                outcome          = outcome,
                config_id        = as.character(config_id),
                climate_var      = climate_var,
                climate_estimate = climate_estimate,
                climate_se       = climate_se,
                climate_ci_low   = climate_ci_low,
                climate_ci_high  = climate_ci_high,
                climate_pval     = climate_pval,
                status           = status)

    reg <- reg %>% mutate(config_id = as.character(config_id))
    mv_sel <- mv_sel %>% left_join(reg, by = "config_id")

    parts[[ind]] <- mv_sel
  }
  out <- bind_rows(parts)
  if (nrow(out) == 0) return(out)

  out <- out %>%
    add_manuscript_facets() %>%
    mutate(outcome_label = pretty_outcome(outcome))
  out
}


#' Traditional specification curve: per cell, plot climate β (with CI ribbon)
#' across all specifications sorted ascending. Five colored decision strips
#' beneath each curve mirror the Δll spec curves.
#'
#' @param beta_table Output of build_beta_multiverse_table().
#' @param cells Character vector of "industry|outcome" keys to render.
#' @param ncol Grid columns.
#' @param strip_height Relative height of each decision strip in the
#'   patchwork stack (default 0.18).
#' @return patchwork object.
plot_traditional_spec_curve_manuscript <- function(beta_table,
                                                     cells = NULL,
                                                     ncol  = NULL,
                                                     strip_height = 0.18) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork required for plot_traditional_spec_curve_manuscript")
  }

  if (is.null(cells)) {
    cells <- beta_table %>%
      mutate(key = paste(industry, outcome, sep = "|")) %>%
      pull(key) %>% unique()
  }

  # Use the same color palettes as the Δll spec curves so the appendix
  # figure visually corresponds 1:1 to the main-text figure.
  pal_window     <- c("SMA-5" = "#a6cee3", "SMA-10" = "#1f78b4",
                       "SMA-20" = "#08306b",
                       "EWMA-360" = "#b2df8a", "EWMA-540" = "#33a02c",
                       "EWMA-720" = "#1b5e20")
  pal_embed      <- RColorBrewer::brewer.pal(3, "Set2")
  pal_sentiment  <- RColorBrewer::brewer.pal(3, "Set3")[1:3]
  pal_composite  <- c("weighted_avg" = "#7570b3",
                       "attention(tau=0.15)" = "#fdb462",
                       "attention(tau=1.0)"  = "#e7298a",
                       "power(p=0.5)"        = "#66c2a5",
                       "power(p=2.0)"        = "#1b9e77")
  pal_threshold  <- c("all-segments" = "#cccccc",
                       "top-3"        = "#fc8d59",
                       "top-5"        = "#b30000")

  build_cell <- function(key) {
    parts <- stringr::str_split(key, "\\|")[[1]]
    ind <- parts[1]; oc <- parts[2]
    d <- beta_table %>% filter(industry == ind, outcome == oc) %>%
      arrange(climate_estimate) %>% mutate(rank = row_number())
    if (nrow(d) == 0) return(NULL)

    title <- sprintf("%s - %s",
                     .short_industry_label(d$industry_label[1]),
                     d$outcome_label[1])
    pct_sig_neg <- 100 * mean(d$climate_pval < 0.05 & d$climate_estimate < 0,
                              na.rm = TRUE)
    pct_sig_pos <- 100 * mean(d$climate_pval < 0.05 & d$climate_estimate > 0,
                              na.rm = TRUE)

    p_curve <- ggplot(d, aes(x = rank, y = climate_estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                  color = "gray40", linewidth = 0.4) +
      geom_ribbon(aes(ymin = climate_ci_low, ymax = climate_ci_high),
                   fill = "gray70", alpha = 0.4) +
      geom_point(aes(color = climate_estimate < 0), size = 0.6, alpha = 0.7) +
      scale_color_manual(values = c("TRUE" = "#1b9e77",
                                     "FALSE" = "#b2182b"),
                          guide  = "none") +
      labs(title = title,
           subtitle = sprintf("%.1f%% sig. neg. | %.1f%% sig. pos. (n=%d specs)",
                              pct_sig_neg, pct_sig_pos, nrow(d)),
           x = NULL, y = "Climate beta (log rate-ratio)") +
      theme_minimal(base_size = 10) +
      theme(plot.title    = element_text(face = "bold", size = 10),
            plot.subtitle = element_text(color = "gray40", size = 8),
            panel.grid.minor = element_blank(),
            axis.text.x   = element_blank(),
            axis.ticks.x  = element_blank())

    make_strip <- function(var_sym, label, palette) {
      ggplot(d, aes(x = rank, y = 1, fill = !!var_sym)) +
        geom_tile() +
        scale_fill_manual(values = palette, name = label,
                           drop = FALSE, na.value = "white") +
        scale_y_continuous(expand = c(0, 0),
                            breaks = 1, labels = label) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 9) +
        theme(panel.grid       = element_blank(),
              axis.text.x      = element_blank(),
              axis.ticks       = element_blank(),
              axis.text.y      = element_text(size = 7),
              legend.position  = "bottom",
              legend.key.size  = unit(0.3, "cm"),
              legend.text      = element_text(size = 7),
              legend.title     = element_text(size = 7,
                                                face = "bold"))
    }

    p_window <- make_strip(rlang::sym("window"),       "Window",       pal_window)
    p_embed  <- make_strip(rlang::sym("embedding"),    "Embedding",
                            setNames(pal_embed,     levels(d$embedding)))
    p_sent   <- make_strip(rlang::sym("sentiment"),    "Sentiment",
                            setNames(pal_sentiment, levels(d$sentiment)))
    p_comp   <- make_strip(rlang::sym("composite"),    "Composite",    pal_composite)
    p_thresh <- make_strip(rlang::sym("thresholding"), "Thresholding", pal_threshold)

    # No inner `guides = "collect"` — outer wrap_plots() collects across the
    # whole nested structure so the legend appears once at the bottom of
    # the FULL figure, not under each per-cell composite. See the
    # corresponding comment in plot_cv_spec_curve_manuscript() for detail.
    p_curve / p_window / p_embed / p_sent / p_comp / p_thresh +
      patchwork::plot_layout(
        heights = c(1, strip_height, strip_height, strip_height,
                    strip_height, strip_height))
  }

  panels <- purrr::compact(purrr::map(cells, build_cell))
  if (length(panels) == 0) {
    stop("No cells matched; check 'cells' argument and beta_table contents.")
  }

  if (is.null(ncol)) ncol <- length(panels)

  # Legend layout matches plot_cv_spec_curve_manuscript(): each design
  # dimension is its own vertical column with the dimension name as a
  # bolded header and the levels stacked underneath as a color continuum.
  # The five columns sit side-by-side across the bottom of the figure.
  patchwork::wrap_plots(panels, ncol = ncol, guides = "collect") &
    theme(legend.position  = "bottom",
          legend.direction = "vertical",
          legend.box       = "horizontal",
          legend.box.just  = "top",
          legend.spacing.x = unit(1.0, "lines"),
          legend.margin    = margin(4, 6, 4, 6),
          legend.title     = element_text(size = 8, face = "bold"),
          legend.text      = element_text(size = 7),
          legend.key.size  = unit(0.35, "cm"))
}
