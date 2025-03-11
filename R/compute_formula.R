#' Compute Formula-Based Signal Calculation
#'
#' This method computes a transformation between multiple signals for each ticker and date in a `meta_dataframe`.
#' Unlike `compute_window`, this function does not apply any rolling or seasonal windowing; instead, the calculation
#' is applied directly to each observation based on a user-defined formula.
#'
#' @param features_m_df A `meta_dataframe` object.
#' @param formula A `character` string specifying the arithmetic formula to apply.
#'   The formula should reference column names in `features_m_df` and can include `+`, `-`, `*`, `/`, and parentheses.
#'   Example: "price / earnings", "revenue - expenses", "log(market_cap)".
#' @param ignore_NA A `character vector` specifying which variables should be ignored in case of NA. The user can specify:
#'   - A list of column names to replace NA values in those columns only.
#' @param feature_name A `character` specifying the name of the new feature column.
#'
#' @return A `meta_dataframe` object with an added column containing the computed values based on the formula.
#'
#' @export
setGeneric("compute_formula", function(features_m_df, formula, ignore_NA = NULL, feature_name) {
  standardGeneric("compute_formula")
})

setMethod("compute_formula",
          signature(features_m_df = "meta_dataframe", formula = "character"),
          function(features_m_df, formula, ignore_NA = NULL, feature_name) {

            #Extract data
            #################
              ##Extract necessary data
              meta_dataframe_workflow <- features_m_df@workflow
              meta_dataframe_name <- features_m_df@meta_dataframe_name
              pre_silver_features_m_df <- features_m_df@data

              ##Parse formula to identify column names
              formula_expr <- rlang::parse_expr(formula)
              formula_vars <- all.vars(formula_expr)
              formula_tokens <- all.names(formula_expr)

              ##Identify operations in the formula
              contains_add_subtract <- any(formula_tokens %in% c("+", "-"))
              contains_mult_div <- any(formula_tokens %in% c("*", "/"))
              contains_other_functions <- any(formula_tokens %in% c("log", "exp", "sqrt", "^"))

            #################

            #Check errors
            #################

              ##Check if all referenced columns exist
              missing_vars <- setdiff(formula_vars, names(pre_silver_features_m_df))
              if (length(missing_vars) > 0) {
                stop("The following columns are missing in the data: ", paste(missing_vars, collapse = ", "))
              }
              ##Check if all ignore_NA columns exist
              if (!is.null(ignore_NA)) {
                missing_ignore_NA_vars <- setdiff(ignore_NA, names(pre_silver_features_m_df))
                if (length(missing_ignore_NA_vars) > 0) {
                  stop("The following columns are missing in the data: ", paste(missing_ignore_NA_vars, collapse = ", "))
                }
              }
              ##Check that ignore_NA and formula_vars are not exactly the same (order may differ)
              if (!is.null(ignore_NA)) {
                if (identical(sort(ignore_NA), sort(formula_vars))) {
                  stop("The ignore_NA columns and formula columns are the same.")
                }
                if ((contains_add_subtract && contains_mult_div) || contains_other_functions) {
                  stop("When ignore_NA is specified, only basic arithmethic operations that do not mix addition/subtraction with multiplication/division are allowed.")
                }
              }

            #################

            #Apply formula
            ################

              ##Handle NA values dynamically using dplyr
              modified_data <- pre_silver_features_m_df

              if (is.character(ignore_NA) && !(length(ignore_NA) == 1 && is.null(ignore_NA))) {
                cols_to_replace <- intersect(ignore_NA, formula_vars)
                if (contains_add_subtract) {
                  modified_data <- modified_data %>%
                    dplyr::mutate(dplyr::across(dplyr::all_of(cols_to_replace), ~ifelse(is.na(.), 0, .)))
                } else if (contains_mult_div) {
                  modified_data <- modified_data %>%
                    dplyr::mutate(dplyr::across(dplyr::all_of(cols_to_replace), ~ifelse(is.na(.), 1, .)))
                }
              }

              ##Evaluate the formula dynamically with error handling
              computed_values <- tryCatch({
                modified_data %>%
                  dplyr::mutate(result = eval(formula_expr, .))
              }, error = function(e) {
                stop("Formula evaluation failed: ", e$message)
              }, warning = function(w) {
                warning("Formula evaluation produced a warning: ", w$message)
                # Only replace problematic rows with NA
                modified_data %>%
                  dplyr::mutate(
                    result = dplyr::if_else(is.na(eval(formula_expr, .)), NA_real_, eval(formula_expr, .))
                  )
              })

            #################

            #Finalize
            #################

              ##Add computed values to meta_dataframe
              pre_silver_features_m_df <- pre_silver_features_m_df %>%
                dplyr::mutate(!!rlang::sym(feature_name) := computed_values$result)

              ##Update workflow
              new_workflow <- list(
                list(
                  timestamp = Sys.time(),
                  formula = formula,
                  ignore_NA = ignore_NA,
                  feature_name = feature_name,
                  call = match.call()
                )
              )

              ##Recreate meta_dataframe
              pre_silver_features_m_df <- create_meta_dataframe(
                pre_silver_features_m_df,
                meta_dataframe_name = meta_dataframe_name,
                workflow = c(meta_dataframe_workflow, new_workflow),
                type = "generic"
              )

              # Rename workflow entry
              names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <-
                paste0("compute_", feature_name)

            return(pre_silver_features_m_df)
          })
