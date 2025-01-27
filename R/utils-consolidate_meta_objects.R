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
#' @param features_passthrough Character; specifies which features from `features_m_df` to include in the output. Options are `"none"`, `"all"`, or specific feature names. Default is `"none"`.
#' @param features_m_df A meta dataframe of features to include in the passthrough. Only required if `features_passthrough` is not `"none"`. Default is `NULL`.
#'
#' @details
#' The function performs a series of validation checks to ensure the consistency of input data:
#' - All elements in `base_sb_backtest_results_list` must have the same `oos_predictions_m_df` structure, with matching `id` values across all elements.
#' - The list must have unique, non-empty names to be used as column identifiers in the consolidated dataframe.
#' - If `features_passthrough` is not `"none"`, a `features_m_df` must be provided.
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
#'   features_passthrough = "all",
#'   features_m_df = features_df
#' )
#' }
#'
#' @seealso
#' [sb_backtest_results-class] for the definition of `sb_backtest_results` objects.
#' [create_meta_dataframe()] for constructing a meta dataframe.
#'
consolidate_oos_sb_outputs_m_df <- function(base_sb_backtest_results_list,
                                            winsorize_predictions = TRUE, normalize_predictions = TRUE, winsorization_probs = c(0.025,0.975),
                                            features_passthrough_and_positions = "none", features_m_df = NULL) {

  #Initial checks
  #########################
    ##Check if input is a list
    if (!is.list(base_sb_backtest_results_list)) {
      stop("Input must be a list of S4 objects.")
    }

    ##Check if elements of lists in oos_predictions_list match
    elements <- lapply(base_sb_backtest_results_list, function(x) x@oos_sb_outputs_m_df@data %>% dplyr::select(id))
    if (!all(purrr::map_lgl(elements, ~ identical(.x, elements[[1]])))){
      stop("Elements of oos_sb_outputs_m_df in each sb_backtest_results object must be the same.")
    }

    ##Check if there are avaialable features_m_df ids
    if (!is.null(features_m_df)) {
      if (any(!base_sb_backtest_results_list[[1]]@oos_sb_outputs_m_df@data$id %in% features_m_df@data$id))
        stop("Not all ids in base_sb_backtest_results are available in features_m_df")
    }

    ##Check if backtest_ids are in fact unique
    if (length(sapply(base_sb_backtest_results_list, function(x) x@backtest_identifier)) !=
        length(unique(sapply(base_sb_backtest_results_list, function(x) x@backtest_identifier)))) {
      stop("Backtest identifiers must be unique.")
    }

    if (length(unique(names(base_sb_backtest_results_list))) != length(base_sb_backtest_results_list)){
      stop("Names of sb_backtest_results objects must be unique.")
    }

    ##Check if length of oos_predictions_list match
    if (!all(sapply(base_sb_backtest_results_list, function(x) nrow(x@oos_sb_outputs_m_df@data)) == nrow(base_sb_backtest_results_list[[1]]@oos_sb_outputs_m_df@data))){
      stop("Number of rows of oos_sb_outputs_m_df in each sb_backtest_results object must be the same.")
    }

    ##Check if features_m_df is provided if features_passthrough_and_positions is not 'none'
    if (length(features_passthrough_and_positions) == 1 && features_passthrough_and_positions != "none" && is.null(features_m_df)){
      stop("features_m_df must be provided if features_passthrough_and_positions is not 'none'.")
    }


  #########################

  #Join oos_preds
  #########################
    ##Create base obj
    oos_predictions_m_df <- purrr::reduce(
      # Use all but the first S4 object in the iteration
      .x = base_sb_backtest_results_list[-1],

      # Start with the data frame from the first object
      .init = base_sb_backtest_results_list[[1]]@oos_sb_outputs_m_df@data %>%
        dplyr::select(id, tickers, dates, pred) %>%
        dplyr::rename_with(~ paste0(base_sb_backtest_results_list[[1]]@backtest_identifier), .cols = "pred"),

      # For each iteration, 'df_acc' is a data.frame, 'sb_obj' is the next S4 object
      .f = function(df_acc, sb_obj) {
        dplyr::left_join(
          df_acc,
          sb_obj@oos_sb_outputs_m_df@data %>%
            dplyr::select(id, pred) %>%
            dplyr::rename_with(~ paste0(sb_obj@backtest_identifier), .cols = "pred"),
          by = "id"
        )
      }
    )

      ###Check if 'id', 'tickers', and 'dates' exist
      required_columns <- c("id", "tickers", "dates")
      if (!all(required_columns %in% colnames(oos_predictions_m_df))) {
        stop("The merged data frame does not contain the required 'id', 'tickers', or 'dates' columns.")
      }

      ###Check for backtest_identifier columns
      if (any(!names(base_sb_backtest_results_list) %in% colnames(oos_predictions_m_df))){
        stop("All backtests should be in oos_predictions_m_df.")
      }


    ##Transform to meta dataframe
    oos_predictions_m_df <- create_meta_dataframe(oos_predictions_m_df, type = "signals")

    # Perform Winsorization and Normalization
    if (winsorize_predictions) oos_predictions_m_df <- winsorize_panel_data(oos_predictions_m_df, probs = winsorization_probs)
    if (normalize_predictions) oos_predictions_m_df <- normalize_panel_data(oos_predictions_m_df)
  #########################

  # Add Pass-through features
  #########################
  if (length(features_passthrough_and_positions) == 1 && features_passthrough_and_positions == "none") {
    # If none, do nothing
    oos_predictions_and_features_m_df <- oos_predictions_m_df
  } else {
    ##If specific features, pass only those

      ###Adjust features_m_df according to features_passthrough_and_positions
      selected_and_correct_features_m_df <- features_m_df@data %>% dplyr::select(id, tickers, dates, names(features_passthrough_and_positions))

      ###Join
      oos_predictions_and_features_m_df <- oos_predictions_m_df
      oos_predictions_and_features_m_df@data <- dplyr::left_join(oos_predictions_m_df@data,
                                                                   dplyr::select(selected_and_correct_features_m_df, -tickers, -dates), by = "id")

  }


  #########################
  return(oos_predictions_and_features_m_df)
}


#' @title Consolidate Backtest Returns (XTS)
#'
#' @description
#' This function consolidates two XTS objects containing backtest returns by merging them.
#' If one of the inputs is NULL, the function returns the other non-NULL object.
#' If both are non-NULL, it merges them using a left join and removes any missing values.
#'
#' @param meta_backtest_returns_m_xts An XTS object representing meta backtest returns.
#' @param base_backtest_returns_m_xts An XTS object representing base backtest returns.
#'
#' @return A consolidated XTS object containing the merged data with updated metadata.
#' If `meta_backtest_returns_m_xts` is NULL, NULL is returned.
#'
#' @examples
#' consolidated <- consolidate_backtest_returns_xts(meta_xts, base_xts)
#'
consolidate_backtest_returns_m_xts <- function(meta_backtest_returns_m_xts, base_backtest_returns_m_xts) {
  # Return NULL if meta_backtest_returns_m_xts is NULL
  if (is.null(meta_backtest_returns_m_xts)) return(NULL)

  # Return meta_backtest_returns_m_xts if base_backtest_returns_m_xts is NULL
  if (is.null(base_backtest_returns_m_xts)) return(meta_backtest_returns_m_xts)

  # Merge meta and base backtest returns using a left join
  return(create_meta_xts(merge(meta_backtest_returns_m_xts@data, base_backtest_returns_m_xts@data, join = "left") %>% na.omit(), #Join
                               meta_xts_name = paste0(meta_backtest_returns_m_xts@meta_xts_name, "_", base_backtest_returns_m_xts@meta_xts_name), #Rename
                               type = "assets")
        )
}


#' @title Consolidate Benchmark Returns (XTS)
#'
#' @description
#' This function consolidates two benchmark return XTS objects.
#' If one is NULL, it returns the other. If both are present, they are merged.
#'
#' @param meta_benchmark_returns_m_xts An XTS object containing meta benchmark returns.
#' @param base_benchmark_returns_m_xts An XTS object containing base benchmark returns.
#'
#' @return A combined XTS object with updated metadata or NULL.
#'
#' @examples
#' result <- consolidate_benchmark_returns_xts(meta_xts, base_xts)
consolidate_benchmark_returns_m_xts <- function(meta_benchmark_returns_m_xts, base_benchmark_returns_m_xts) {
  # Case 1 & 2: Return the non-NULL object if either is NULL, or NULL if both are NULL
  if (is.null(meta_benchmark_returns_m_xts)) return(base_benchmark_returns_m_xts)
  if (is.null(base_benchmark_returns_m_xts)) return(meta_benchmark_returns_m_xts)

  # Case 3: Merge both if neither is NULL
  return(create_meta_xts(merge(meta_benchmark_returns_m_xts@data, base_benchmark_returns_m_xts@data, join = "left") %>% na.omit(), #Join
                         meta_xts_name = paste0(meta_benchmark_returns_m_xts@meta_xts_name, "_", base_benchmark_returns_m_xts@meta_xts_name), #Rename
                         type = "assets")
         )

}

#' @title Consolidate Meta and Base Dataframes
#'
#' @description
#' Merges meta and base dataframes, handling cases where either dataframe is NULL.
#' If both are present, they are combined using a full join and sorted by ID.
#'
#' @param meta_generic_m_df A meta dataframe object.
#' @param base_generic_m_df A base dataframe object.
#' @param type A character string indicating the type of data (e.g., "groups").
#'
#' @return A consolidated dataframe combining meta and base data, or NULL if meta is NULL.
#'
#' @examples
#' consolidated_df <- consolidate_generic_meta_dataframes(meta_df, base_df, "groups")
#'
consolidate_generic_meta_dataframes <- function(meta_generic_m_df, base_generic_m_df, type) {
  if (is.null(meta_generic_m_df)){
    ##If meta_generic_m_df is NULL, just pass NULL
    adapted_generic_m_df <- NULL
  } else if (is.null(base_generic_m_df))  {
    adapted_generic_m_df <- meta_generic_m_df
  } else {
    ##Else Full Join them. If base is NULL, it will return only meta_generic_m_df
    adapted_generic_m_df <- dplyr::bind_rows(meta_generic_m_df@data, base_generic_m_df@data) %>% dplyr::arrange(id) %>%
      create_meta_dataframe(meta_dataframe_name = paste0(meta_generic_m_df@meta_dataframe_name, "_", base_generic_m_df@meta_dataframe_name), type = "groups")
  }
  return(adapted_generic_m_df)
}

#' @title Derive Adapted Custom Signal Universe Metrics
#'
#' @description
#' This function consolidates and adapts custom signal universe metrics.
#' It processes evaluation metrics across multiple backtest results and integrates them with meta-level data.
#'
#' @param meta_custom_objective A character string representing the custom objective (e.g., 'min_rss').
#' @param base_sb_backtest_results_list A list of backtest results for processing.
#' @param meta_custom_signal_universe_metrics_m_df Meta-level metrics dataframe.
#' @param base_custom_signal_universe_metrics_m_df Base-level metrics dataframe.
#'
#' @return A combined and processed dataframe of signal universe metrics.
#'
derive_adapted_custom_signal_universe_m_df <- function(meta_custom_objective, base_sb_backtest_results_list,
                                                       meta_custom_signal_universe_metrics_m_df, base_custom_signal_universe_metrics_m_df
                                                        ) {

  ###Derive consolidated_eval_metrics_m_df
  all_base_consolidated_eval_metrics_m_df <- purrr::reduce(
    ###Use reduce to join all oos_testing_eval_metrics_m_df into a consolidate object
    purrr::map(
      ####Apply a map function to transform each oos_testing_eval_metrics_m_xts into a meta_dataframe-like object
      base_sb_backtest_results_list,
      function(x){
        x@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() %>% #First extract all oos_testing eval metrics xts
          tibble::rownames_to_column(var = "dates") %>% #Rename to dates column
          dplyr::mutate(dates = as.Date(dates) + months(x@sb_backtest_workflow$target_fwd)) %>% #Adjust dates in order to reflect when info was actually available
          dplyr::mutate(tickers = x@backtest_identifier, .before = dates) %>% #Add tickers
          dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers) #Add id
      }
    ),
    function(oos_testing_eval_m_df_1, oos_testing_eval_m_df_2){
      ###Use bind_rows to join all oos_testing_eval_metrics_m_df sequentially
      dplyr::bind_rows(
        oos_testing_eval_m_df_1, oos_testing_eval_m_df_2
      )
    }
  ) %>% dplyr::arrange(id)

  ###Check if meta_custom_signal_universe_metrics_m_df is NULL
  if (is.null(meta_custom_signal_universe_metrics_m_df)){
    ##Check if max/min oos_testing_eval_metrics is the objective
    oos_testing_eval_metrics <- c("rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")
    if (stringr::str_remove(stringr::str_remove(meta_custom_objective, "min_"), "max_") %in% oos_testing_eval_metrics){
      adapted_custom_signal_universe_metrics_m_df <- create_meta_dataframe(all_base_consolidated_eval_metrics_m_df)
    } else {
      adapted_custom_signal_universe_metrics_m_df <- NULL
    }
  } else {
    adapted_custom_signal_universe_metrics_m_df <- dplyr::bind_rows(meta_custom_signal_universe_metrics_m_df@data, base_custom_signal_universe_metrics_m_df@data) %>% dplyr::arrange(id) %>%
      create_meta_dataframe(meta_dataframe_name = paste0(meta_custom_signal_universe_metrics_m_df@meta_dataframe_name, "_", base_custom_signal_universe_metrics_m_df@meta_dataframe_name))
  }

  return(adapted_custom_signal_universe_metrics_m_df)
}









