# =============================================================================
# layer_template.R — Template for Adding a New Feature Layer
# =============================================================================
#
# Copy this file and customize to add a new feature family to the multiverse
# pipeline. Replace all instances of "TEMPLATE" / "template" with your
# layer name, and implement the three core functions:
#   1. config_grid: What parameter combinations does this layer test?
#   2. compute:     How are features computed/loaded for a given config?
#   3. formula_terms: Which columns enter the model formula?
#
# CHECKLIST for a new feature layer:
#   [ ] Choose: is this layer config-DEPENDENT (computed on climate scores)
#       or config-INDEPENDENT (precomputed from raw data)?
#   [ ] Define your column prefix (must be unique across layers)
#   [ ] Define your config grid
#   [ ] Implement compute()
#   [ ] Decide which features are model terms vs. just available for joins
#   [ ] Source this file in the industry's setup script
#   [ ] Add the layer to the industry's create_layer_registry() call
#   [ ] Add the layer to the model_hierarchy if it should be tested incrementally
#
# =============================================================================

# source("common/feature_layers.R")  # provides define_feature_layer()

#' Create the TEMPLATE feature layer
#'
#' @param data_dir Directory containing precomputed feature files (if independent)
#' @param param1 Example parameter for the config grid
#' @param param2 Another example parameter
#' @param formula_vars Which feature columns to add as model terms
#' @return A feature_layer object
create_template_layer <- function(
    data_dir = NULL,
    param1 = c("option_a", "option_b"),
    param2 = c(10L, 20L),
    formula_vars = c("template_feature1", "template_feature2")
) {

  # Build the config grid: every combination of parameters to test
  grid <- expand.grid(
    template_param1 = param1,
    template_param2 = param2,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      # REQUIRED: must have a column named {layer_name}_config_id
      template_config_id = glue::glue("template_{template_param1}_p{template_param2}")
    )

  define_feature_layer(
    name = "template",
    prefix = "template_",

    # Set TRUE if features depend on climate composite scores
    # Set FALSE if features are precomputed from raw text/timestamps
    depends_on_climate = FALSE,

    description = "Template layer — replace with your description",

    config_grid = function() {
      grid
    },

    compute = function(df, layer_config, ...) {
      # layer_config is a single-row tibble from config_grid()
      # df is the prepared data frame (with eid, org_id, event_date, etc.)

      cfg_id <- layer_config[["template_config_id"]]
      p1 <- layer_config[["template_param1"]]
      p2 <- layer_config[["template_param2"]]

      # --- OPTION A: Load precomputed features (for independent layers) ---
      # path <- file.path(data_dir, glue::glue("template_{cfg_id}.parquet"))
      # result <- arrow::read_parquet(path) %>% as_tibble()

      # --- OPTION B: Compute features in R (for dependent layers) ---
      # result <- df %>%
      #   group_by(org_id) %>%
      #   mutate(
      #     template_feature1 = some_function(.data[[composite_col]], p1, p2),
      #     template_feature2 = another_function(.data[[composite_col]])
      #   ) %>%
      #   ungroup() %>%
      #   select(eid, starts_with("template_"))

      # Placeholder: return empty features
      tibble::tibble(eid = df$eid[0])
    },

    formula_terms = function(layer_config) {
      cfg_id <- layer_config[["template_config_id"]]
      # Return the variable names that should be added to the model formula
      # Return character(0) if this layer provides features only for downstream use
      formula_vars
    }
  )
}


# =============================================================================
# EXAMPLE: Lexical/Syntactic feature layer (sketch)
# =============================================================================
#
# This is what a real lexical feature layer might look like.
# It would be precomputed in Python (config-independent) and produce features
# like sentence complexity, vocabulary richness, hedging frequency, etc.
#
# create_lexical_layer <- function(
#     lexical_dir = NULL,
#     formula_vars = c("lex_avg_sentence_len", "lex_type_token_ratio",
#                      "lex_hedge_rate", "lex_negation_rate")
# ) {
#   # Scan for available lexical config parquets
#   if (!is.null(lexical_dir) && dir.exists(lexical_dir)) {
#     files <- list.files(lexical_dir, pattern = "^lexical_.*\\.parquet$",
#                         full.names = TRUE)
#     available <- tibble(
#       lexical_config_id = tools::file_path_sans_ext(basename(files)) %>%
#         sub("^lexical_", "", .),
#       path = files
#     )
#   } else {
#     available <- tibble(lexical_config_id = "none", path = character(0))
#   }
#
#   cache <- new.env(parent = emptyenv())
#
#   define_feature_layer(
#     name = "lexical",
#     prefix = "lex_",
#     depends_on_climate = FALSE,
#     description = "Lexical/syntactic features from narrative text",
#     config_grid = function() { available %>% select(lexical_config_id) },
#     compute = function(df, layer_config, ...) {
#       cfg_id <- layer_config[["lexical_config_id"]]
#       if (cfg_id == "none") return(tibble(eid = character(0)))
#       if (!is.null(cache[[cfg_id]])) return(cache[[cfg_id]])
#       path <- available %>% filter(lexical_config_id == cfg_id) %>% pull(path)
#       result <- arrow::read_parquet(path[1]) %>% as_tibble()
#       cache[[cfg_id]] <- result
#       result
#     },
#     formula_terms = function(layer_config) { formula_vars }
#   )
# }
