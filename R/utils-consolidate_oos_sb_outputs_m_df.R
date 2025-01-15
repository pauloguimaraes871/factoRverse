#' Consolidate Out-of-Sample Signal-Blending Outputs into a Meta Dataframe
#'
#' @description
#' This function consolidates out-of-sample (OOS) predictions from a list of `sb_backtest_results` objects into a single meta dataframe.
#' It supports optional winsorization and normalization of predictions, as well as passthrough of specific features to the output dataframe.
#'
#' @param base_sb_backtest_results_list A list of `sb_backtest_results` objects, each containing OOS predictions and associated metadata.
#' @param winsorize_predictions Logical; if `TRUE`, performs winsorization on predictions to mitigate the effect of outliers. Default is `TRUE`.
#' @param normalize_predictions Logical; if `TRUE`, normalizes predictions to ensure comparability across models. Default is `TRUE`.
#' @param winsorization_probs A numeric vector of length 2 specifying the lower and upper quantile probabilities for winsorization. Default is `c(0.025, 0.975)`.
#' @param features_passthrough_and_positions Character; specifies which features from `features_m_df` to include in the output. Options are `"none"`, `"all"`, or specific feature names with their positions. Default is `"none"`.
#' @param features_m_df A meta dataframe of features to include in the passthrough. Only required if `features_passthrough_and_positions` is not `"none"`. Default is `NULL`.
#'
#' @details
#' The function performs a series of validation checks to ensure the consistency of input data:
#' - All elements in `base_sb_backtest_results_list` must have the same `oos_predictions_m_df` structure, with matching `id` values across all elements.
#' - The list must have unique, non-empty names to be used as column identifiers in the consolidated dataframe.
#' - If `features_passthrough_and_positions` is not `"none"`, a `features_m_df` must be provided.
#'
#' After performing these validations, the function consolidates predictions into a single meta dataframe.
#' Optional winsorization and normalization can be applied to the predictions.
#' Additionally, the specified features can be passed through to the output dataframe.
#'
#' @return A `meta_dataframe` object containing the consolidated predictions, optionally with normalized and winsorized values, and passthrough features.
#'
#' @examples
#' \dontrun{
#' # Example with default options
#' consolidated_df <- consolidate_oos_sb_outputs_m_df(
#'   base_sb_backtest_results_list = list_of_sb_results,
#'   features_passthrough_and_positions = "all",
#'   features_m_df = features_df
#' )
#' }
#'
#' @seealso
#' [sb_backtest_results-class] for the definition of `sb_backtest_results` objects.
#' [create_meta_dataframe()] for constructing a meta dataframe.
#'
#' @export
consolidate_oos_sb_outputs_m_df <- function(base_sb_backtest_results_list,
                                            winsorize_predictions = TRUE, normalize_predictions= TRUE, winsorization_probs = c(0.025,0.975),
                                            features_passthrough_and_positions = "none", features_m_df = NULL) {

  #Initial checks
  #########################
    ##Check if input is a list
    if (!is.list(base_sb_backtest_results_list)) {
      stop("Input must be a list of S4 objects.")
    }

    ##Check if all oos_predictions_m_df share the same ids
    if (!all(sapply(base_base_sb_backtest_results_list,
                    function(x) identical(x@oos_sb_outputs_m_df@data$id, base_base_sb_backtest_results_list[[1]]@oos_sb_outputs_m_df@data$id)))){

      ##Check the reason for non-compliance
      ###Check if all have the same features_m_df object
      check_if_all_have_same_features_m_df_object_names <- !all(sapply(base_base_sb_backtest_results_list,
                                                                       function(x) x@sb_backtest_workflow$features_object_name) == base_base_sb_backtest_results_list[[1]]@sb_backtest_workflow$features_object_name)

      ###Check if all have same training_sample_size and validation_sample_size
      check_if_all_have_same_training_scheme <- !all(sapply(base_base_sb_backtest_results_list,
                                                            function(x) x@sb_backtest_workflow$training_sample_size + x@sb_backtest_workflow$validation_sample_size) ==
                                                       base_base_sb_backtest_results_list[[1]]@sb_backtest_workflow$training_sample_size + base_base_sb_backtest_results_list[[1]]@sb_backtest_workflow$validation_sample_size)

      ###Case 1: All have different features_m_df object and different training schemes
      if (all(check_if_all_have_same_features_m_df_object_names, check_if_all_have_same_training_scheme)){
        warning("All base_sb_backtest_results must have the same features_m_df object and also the same training scheme (training_sample_size + validation_sample_size).")
        return(base_base_sb_backtest_results_list)
      }
      ###Case 2: All have different features_m_df
      if (check_if_all_have_same_features_m_df_object_names){
        warning("All base_sb_backtest_results must have the same features_m_df object.")
        return(base_base_sb_backtest_results_list)
      }
      ###Case 3: All have different training schemes
      if (check_if_all_have_same_training_scheme){
        warning("All base_sb_backtest_results must have the same training scheme (training_sample_size + validation_sample_size).")
        return(base_base_sb_backtest_results_list)
      }
    }

    ##Check if elements of lists in oos_predictions_list match
    elements <- lapply(base_sb_backtest_results_list, function(x) x@oos_sb_outputs_m_df@data %>% dplyr::select(id))
    if(!all(purrr::map_lgl(elements, ~ identical(.x, elements[[1]])))){
      stop("Elements of oos_sb_outputs_m_df in each sb_backtest_results object must be the same.")
    }

    ##Check if the list has names
    if (is.null(names(base_sb_backtest_results_list)) || any(names(base_sb_backtest_results_list) == "")) {
      stop("All elements in the list must have names to be used as column names in the final data frame.")
    }

    ##Check if length of oos_predictions_list match
    if(!all(sapply(base_sb_backtest_results_list, function(x) nrow(x@oos_sb_outputs_m_df@data)) == nrow(base_sb_backtest_results_list[[1]]@oos_sb_outputs_m_df@data))){
      stop("Number of rows of oos_sb_outputs_m_df in each sb_backtest_results object must be the same.")
    }

    ##Check if features_m_df is provided when features_passthrough_and_positions is not none
    if (features_passthrough_and_positions != "none" && is.null(features_m_df)){
      stop("features_m_df can't be NULL when features_passthrough_and_positions is not none")
    }
  #########################

  #Join oos_preds
  #########################
    ##Create base obj
    oos_predictions_m_df <- purrr::reduce(
      base_sb_backtest_results_list,
      function(sb_results_1, sb_results_2){
        #Combines data.frames sequentially one by one
        dplyr::left_join(sb_results_1@oos_sb_outputs_m_df@data,
                         sb_results_2@oos_sb_outputs_m_df@data %>%
                           dplyr::select(id, pred) %>% #Eliminate error, target, tickers and dates cols
                           dplyr::rename_with(~ paste0(sb_results@backtest_identifier), .cols = "pred"),
                         by = "id")
      },
      #Initialize the reduction
      .init = base_sb_backtest_results_list[[1]]@oos_sb_outputs_m_df@data %>%
        dplyr::select(id, tickers, dates, pred) %>%
        dplyr::rename_with(~ paste0(base_sb_backtest_results_list[[1]]@backtest_identifier), .cols = "pred")
    )

      ###Check if 'id', 'tickers', and 'dates' exist
      required_columns <- c("id", "tickers", "dates")
      if (!all(required_columns %in% colnames(oos_predictions_m_df))) {
        stop("The merged data frame does not contain the required 'id', 'tickers', or 'dates' columns.")
      }

    ##Transform to meta dataframe
    oos_predictions_m_df <- create_meta_dataframe(oos_predictions_m_df)

    # Perform Winsorization and Normalization
    if(winsorize_predictions) oos_predictions_m_df <- winsorize_panel_data(oos_predictions_m_df, probs = winsorization_probs)
    if(normalize_predictions) oos_predictions_m_df <- normalize_panel_data(oos_predictions_m_df)
  #########################

  # Add Pass-through features
  #########################
  if (features_passthrough_and_positions == "none") {
    # If none, do nothing
    oos_predictions_and_features_m_df <- oos_predictions_m_df
    oos_predictions_and_features_m_df@meta_dataframe_name <- paste0(config@config_name, "_bpreds")
    oos_predictions_and_features_m_df@workflow <- c(oos_predictions_and_features_m_df@workflow, "passthrough_none")

  } else {

    if (features_passthrough_and_positions == "all") {
      ##If all, pass everything except for tickers and dates
      oos_predictions_and_features_m_df <- oos_predictions_m_df
      oos_predictions_and_features_m_df@data <- dplyr::left_join(oos_predictions_m_df@data,
                                                                 dplyr::select(features_m_df@data, -tickers, -dates), by = "id")
      oos_predictions_and_features_m_df@meta_dataframe_name <- paste0(config@config_name, "_bpreds")
      oos_predictions_and_features_m_df@workflow <- c(oos_predictions_and_features_m_df@workflow, "passthrough_all")

    } else {
      ##If specific features, pass only those

        ###Adjust features_m_df according to features_passthrough_and_positions
        selected_and_correct_features_m_df <- select_and_correct_signals(features_m_df@data,
                                                                         chosen_signals_and_positions = features_passthrough_and_positions)$selected_signals_corrected_positions_m_df

        ###Join
        oos_predictions_and_features_m_df <- oos_predictions_m_df
        oos_predictions_and_features_m_df@data <- dplyr::left_join(oos_predictions_m_df@data,
                                                                   dplyr::select(selected_and_correct_features_m_df, -tickers, -dates), by = "id")
        oos_predictions_and_features_m_df@meta_dataframe_name <- paste0(config@config_name, "_bpreds")
        oos_predictions_and_features_m_df@workflow <- c(oos_predictions_and_features_m_df@workflow, features_passthrough_and_positions)
    }
  }

  #########################
  return(oos_predictions_and_features_m_df)
}



