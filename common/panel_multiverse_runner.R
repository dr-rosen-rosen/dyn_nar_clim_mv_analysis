# =============================================================================
# common/panel_multiverse_runner.R — Panel-Rate Multiverse Runner
# =============================================================================
#
# Parallel to common/multiverse_runner.R but for the panel-rate track. Fits
# one model per (climate_config × climate_var × outcome). Industry-specific
# data prep and fit functions are provided by the caller; no feature-layer
# system here (the panel track keeps it simple — layers can be added later).
#
# Each fit_fn returns a NAMED LIST (not a tibble) with at minimum these keys:
#   config_id, climate_var, outcome, status, n_obs, climate_estimate,
#   climate_se, climate_pval, climate_ci_low, climate_ci_high
# Extra keys are passed through; rows are stacked via dplyr::bind_rows after
# coercion to tibble.
#
# Dependencies: dplyr, purrr, furrr, arrow, glue, tibble
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(furrr)
  library(arrow)
  library(glue)
  library(tibble)
})


#' Coerce a fit_fn result (list or tibble) to a single-row tibble.
#'
#' Scalar list elements become columns. Anything non-scalar is dropped with
#' a warning rather than failing the whole run.
.fit_result_to_row <- function(x) {
  if (is.data.frame(x)) return(tibble::as_tibble(x))
  if (is.list(x)) {
    keep <- vapply(x, function(v) length(v) == 1L && is.atomic(v), logical(1))
    tibble::as_tibble(x[keep])
  } else {
    stop("fit result must be a list or data frame")
  }
}


#' Analyze a single climate config on the panel-rate track.
#'
#' For each climate_var present in the prepared panel, fits one model per
#' outcome via the supplied fit_fns.
#'
#' @param config_id Climate config ID string.
#' @param panel_data_fn Function(parquet_path, config_id) -> panel tibble.
#'   Should encapsulate any global state (events, ops) via closure.
#' @param fit_fns Named list of fit functions, keyed by outcome name. Each is
#'   called as fit_fn(panel, climate_var, config_id, outcome).
#' @param parquet_path Path to the climate config's per-event scores parquet.
#' @param outcomes Character vector of outcome names to fit.
#' @param climate_var_base Base climate column name (default
#'   "overall_final_score"); used to discover windowed climate columns.
#' @return Tibble of result rows.
analyze_single_panel_config <- function(config_id,
                                        panel_data_fn,
                                        fit_fns,
                                        parquet_path,
                                        outcomes,
                                        climate_var_base = "overall_final_score") {

  panel <- tryCatch(
    panel_data_fn(parquet_path = parquet_path, config_id = config_id),
    error = function(e) {
      warning(glue("Panel prep failed for config {config_id}: {conditionMessage(e)}"))
      NULL
    }
  )

  if (is.null(panel) || nrow(panel) == 0) {
    return(tibble(
      config_id = config_id, status = "failed",
      error = "panel prep returned NULL or empty"
    ))
  }

  pattern <- paste0("^", climate_var_base, "_(sma_|ewmaLAG_)")
  climate_vars <- grep(pattern, names(panel), value = TRUE)

  if (length(climate_vars) == 0) {
    return(tibble(
      config_id = config_id, status = "failed",
      error = "no climate windowed columns found in panel"
    ))
  }

  rows <- list()
  idx <- 0L
  for (cv in climate_vars) {
    for (outcome in outcomes) {
      idx <- idx + 1L
      if (!outcome %in% names(fit_fns)) {
        rows[[idx]] <- tibble(config_id = config_id, climate_var = cv,
                              outcome = outcome, status = "failed",
                              error = "no fit_fn registered for outcome")
        next
      }
      r <- tryCatch({
        res <- fit_fns[[outcome]](panel, cv, config_id, outcome)
        row <- .fit_result_to_row(res)
        # Ensure the protocol-level identifiers are present.
        row %>%
          mutate(
            config_id   = as.character(config_id),
            climate_var = as.character(cv),
            outcome     = as.character(outcome),
            .before     = 1
          )
      }, error = function(e) {
        tibble(config_id = config_id, climate_var = cv, outcome = outcome,
               status = "failed", error = conditionMessage(e))
      })
      rows[[idx]] <- r
    }
  }

  bind_rows(rows)
}


#' Run the full panel-rate multiverse in parallel.
#'
#' @param cfg_dir Base directory containing climate configs (subdir layout:
#'   cfg_dir/_cfg/{id}/results.parquet).
#' @param panel_data_fn Function(parquet_path, config_id) -> panel tibble.
#' @param fit_fns Named list of fit functions, keyed by outcome name.
#' @param outcomes Character vector of outcomes to fit.
#' @param config_ids Optional override of which configs to process (NULL =
#'   discover all).
#' @param n_workers Number of parallel workers.
#' @param output_path Optional path to write combined results parquet.
#' @param checkpoint_dir Optional directory for per-config checkpoints.
#' @param worker_source_files Character vector of files to source inside each
#'   worker (so fit_fns and panel_data_fn closures resolve).
#' @param worker_globals Named list of globals to expose to each worker.
#' @return Tibble of combined results.
run_panel_multiverse <- function(cfg_dir,
                                  panel_data_fn,
                                  fit_fns,
                                  outcomes,
                                  config_ids = NULL,
                                  n_workers = 4L,
                                  output_path = NULL,
                                  checkpoint_dir = NULL,
                                  worker_source_files = character(0),
                                  worker_globals = list()) {

  if (is.null(config_ids)) {
    cfg_path <- file.path(cfg_dir, "_cfg")
    if (!dir.exists(cfg_path)) {
      stop(glue("Config directory not found: {cfg_path}"))
    }
    ids <- list.dirs(cfg_path, full.names = FALSE, recursive = FALSE)
    config_ids <- ids[ids != "" & !startsWith(ids, ".")]
  }

  message(glue("\n{paste(rep('=', 60), collapse='')}\n",
               "Panel Multiverse: {length(config_ids)} configs × ",
               "{length(outcomes)} outcomes\n",
               "{paste(rep('=', 60), collapse='')}"))

  if (!is.null(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    existing <- tools::file_path_sans_ext(
      list.files(checkpoint_dir, pattern = "\\.parquet$")
    )
    remaining <- setdiff(config_ids, existing)
    if (length(existing) > 0) {
      message(glue("  Found {length(existing)} checkpoints; ",
                   "{length(remaining)} remaining"))
    }
    config_ids <- remaining
    if (length(config_ids) == 0) {
      message("  All configs checkpointed; loading existing results.")
      files <- list.files(checkpoint_dir, pattern = "\\.parquet$",
                          full.names = TRUE)
      return(map_dfr(files, ~ as_tibble(arrow::read_parquet(.x))))
    }
  }

  plan(multisession, workers = n_workers)
  on.exit(plan(sequential), add = TRUE)

  start_time <- Sys.time()

  results <- future_map(config_ids, function(cid) {
    for (f in worker_source_files) source(f, local = FALSE)

    parquet_path <- file.path(cfg_dir, "_cfg", cid, "results.parquet")
    df <- analyze_single_panel_config(
      config_id = cid,
      panel_data_fn = panel_data_fn,
      fit_fns = fit_fns,
      parquet_path = parquet_path,
      outcomes = outcomes
    )

    if (!is.null(checkpoint_dir)) {
      cp_path <- file.path(checkpoint_dir, glue("{cid}.parquet"))
      tryCatch(arrow::write_parquet(df, cp_path),
               error = function(e) warning(glue("checkpoint failed for {cid}")))
    }
    df
  },
  .options = furrr_options(
    seed = TRUE,
    packages = c("dplyr", "purrr", "tidyr", "tibble", "glue", "arrow",
                 "slider", "lubridate", "stringr",
                 "glmmTMB", "broom.mixed"),
    globals = c(as.list(globalenv()), worker_globals)
  ),
  .progress = TRUE)

  combined <- bind_rows(results)
  elapsed <- difftime(Sys.time(), start_time, units = "mins")

  message(glue("\nCompleted in {round(as.numeric(elapsed), 1)} minutes"))
  message(glue("Rows: {nrow(combined)} | success: ",
               "{sum(combined$status == 'success', na.rm=TRUE)} | failed: ",
               "{sum(combined$status == 'failed', na.rm=TRUE)}"))

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(combined, output_path)
    message(glue("Saved: {output_path}"))
  }

  combined
}
