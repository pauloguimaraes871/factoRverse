#' Run Time-wise Preprocessing Backtest
#'
#' This method performs a time-wise backtest preprocessing on a
#' \code{raw_features_m_df} object using the recipe stored in a
#' \code{pp_backtest_config} object. For each date, the recipe is prepped on the
#' subset of data available at the specific date. This ensures that no future
#' information is used during preprocessing.
#'
#' Parallel processing is achieved using \code{furrr::future_map}; ensure that an appropriate
#' future plan is set (e.g., \code{future::plan(future::multisession)}).
#'
#' @param raw_features_m_df A \code{raw_features_m_df} object.
#' @param pp_backtest_config A \code{pp_backtest_config} object that contains a recipe in its \code{recipe} slot.
#' @param verbose A logical indicating whether to print messages during the process. Default is \code{FALSE}.
#'
#' @return A time-wise preprocessed \code{meta_dataframe}.
#'
#' @examples
#' \dontrun{
#'   # Assume raw_obj is an instance of raw_features_m_df and config_obj is a
#'   # pp_backtest_config object (created via create_pp_backtest_config) that contains a recipe.
#'   preprocessed_df <- run_pp_backtest(raw_features_m_df = raw_obj, config_obj = config_obj)
#' }
#'
#' @export
setGeneric("run_pp_backtest", function(raw_features_m_df, config_obj, ...) {
  standardGeneric("run_pp_backtest")
})

setMethod("run_pp_backtest",
          signature(raw_features_m_df = "raw_features_m_df", config_obj = "pp_backtest_config"),
          function(raw_features_m_df, config_obj, verbose, parallel = TRUE) {

            #Extract objects
            #################
              ##Recipe
              recipe <- config_obj@recipe

              ##Raw features
              meta_dataframe_workflow <- raw_features_m_df@workflow
              meta_dataframe_name <- raw_features_m_df@meta_dataframe_name
              pre_silver_features_m_df <- raw_features_m_df@data
            #################

            #Process each date in parallel.
            #################
            dates_m_vector <- pre_silver_features_m_df %>% dplyr::pull(dates) %>% unique()

            ##Preprocess using furrr:: or purrr::

              ###Define a function to process time-wise
              process_date <- function(current_date) {
                pre_silver_features_m_d_ref <- pre_silver_features_m_df %>% dplyr::filter(dates == current_date)

                if (nrow(pre_silver_features_m_d_ref) < 2) {
                  warning("Not enough data to prep the recipe for date: ", current_date)
                  return(NULL)
                }

                # Prep and bake the recipe
                rec_prepped <- recipes::prep(recipe, training = pre_silver_features_m_d_ref,
                                             retain = TRUE, verbose = verbose)

                baked_data <- recipes::bake(rec_prepped, new_data = pre_silver_features_m_d_ref)

                return(baked_data)
              }

              ###Apply preprocessing using parallel or sequential approach
              if (parallel) {
                preprocessed_pre_silver_features_m_d_ref_list <-
                  furrr::future_map(dates_m_vector, process_date, .options = furrr::furrr_options(seed = TRUE))
              } else {
                preprocessed_pre_silver_features_m_d_ref_list <-
                  purrr::map(dates_m_vector, process_date)
              }

            #################

            #Combine the processed rows into a single meta_dataframe
            #################

              ##Remove NULL results
              preprocessed_pre_silver_features_m_d_ref_list <-
                preprocessed_pre_silver_features_m_d_ref_list[!sapply(preprocessed_pre_silver_features_m_d_ref_list, is.null)]

              ##If all dates failed, return an empty dataframe
              if (length(preprocessed_pre_silver_features_m_d_ref_list) == 0) {
                stop("All preprocessing steps failed due to insufficient data.")
              }

              ##Combine and sort
              preprocessed_features_m_df <- dplyr::bind_rows(preprocessed_pre_silver_features_m_d_ref_list) %>%
                dplyr::arrange(id) %>% as.data.frame()

              ###Ensure consistent columns across processed datasets by introducing missing factors as 0
                ###Identify columns created by step_dummy for handling different factor levels across time
                dummy_steps <- purrr::keep(recipe$steps, ~ inherits(.x, "step_dummy"))
                ###Extract the factor cols
                factor_columns <- unique(unlist(purrr::map(dummy_steps, function(step) {
                  step_tidy <- recipes::tidy(step)
                  step_tidy$terms  # Extracts the actual names of the dummy variables
                })))
                ####Get dummy_columns_to_fill
                all_columns <- unique(unlist(lapply(preprocessed_pre_silver_features_m_d_ref_list, colnames)))
                dummy_columns_to_fill <- all_columns[stringr::str_detect(all_columns, paste0("^", factor_columns, "_"))]
                if (length(dummy_columns_to_fill) > 0) {
                  ####Identify dates where NA exists in dummy columns and replace with 0
                  preprocessed_features_m_df <- preprocessed_features_m_df %>%
                    dplyr::mutate(dplyr::across(dplyr::all_of(dummy_columns_to_fill), ~ ifelse(is.na(.), 0, .)))
                }


            #################

            #Return the preprocessed meta_dataframe
            #################
            preprocessed_features_m_df <- create_meta_dataframe(preprocessed_features_m_df,
                                                                meta_dataframe_name = meta_dataframe_name,
                                                                workflow = c(meta_dataframe_workflow, list(recipe)),
                                                                type = "signals")


            ##Rename
            names(preprocessed_features_m_df@workflow)[length(preprocessed_features_m_df@workflow)] <-
              paste0("preprocessing_recipe", "_", preprocessed_features_m_df@current_date)

            return(preprocessed_features_m_df)


          })
