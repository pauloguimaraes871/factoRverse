#' Consolidate Out-of-Sample Signal-Blending Outputs into a Meta Dataframe
#'
#' @description
#' This function consolidates out-of-sample (OOS) predictions from a list of `sb_backtest_results` objects into a single meta dataframe.
#' It supports optional winsorization and normalization of predictions, as well as passthrough of specific features to the output dataframe.
#'
#' @param base_sb_backtest_outputs_list A list of `sb_backtest_results` objects, each containing OOS predictions and associated metadata.
#' @param winsorize_predictions Logical; if `TRUE`, performs winsorization on predictions to mitigate the effect of outliers. Default is `TRUE`.
#' @param normalize_predictions Logical; if `TRUE`, normalizes predictions to ensure comparability across models. Default is `TRUE`.
#' @param winsorization_probs A numeric vector of length 2 specifying the lower and upper quantile probabilities for winsorization. Default is `c(0.025, 0.975)`.
#' @param features_passthrough_and_positions Character; specifies which features from `features_m_df` to include in the output. Options are `"none"`, `"all"`, or specific feature names. Default is `"none"`.
#' @param features_m_df A meta dataframe of features to include in the passthrough. Only required if `features_passthrough` is not `"none"`. Default is `NULL`.
#' @param parallel Logical; if `TRUE`, parallel processing is used to speed up the consolidation process. Default is `TRUE`.
#' @param verbose Logical; if `TRUE`, progress messages are printed. Default is `TRUE`.
#'
#'
#' @details
#' The function performs a series of validation checks to ensure the consistency of input data:
#' - All elements in `base_sb_backtest_outputs_list` must have the same `oos_predictions_m_df` structure, with matching `id` values across all elements.
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
#'   base_sb_backtest_outputs_list = list_of_sb_results,
#'   features_passthrough = "all",
#'   features_m_df = features_df
#' )
#' }
#'
#' @seealso
#' [sb_backtest_results-class] for the definition of `sb_backtest_results` objects.
#' [create_meta_dataframe()] for constructing a meta dataframe.
#'
consolidate_oos_sb_outputs_m_df <- function(base_sb_backtest_outputs_list,
                                            winsorize_predictions = TRUE, normalize_predictions = TRUE, winsorization_probs = c(0.025,0.975),
                                            features_passthrough_and_positions = "none", features_m_df = NULL, parallel = TRUE, verbose = TRUE) {

  #Initial checks
  #########################
    ##Check if input is a list
    if (!is.list(base_sb_backtest_outputs_list)) {
      stop("Input must be a list of S4 objects.")
    }

    ##Check if elements of lists in oos_predictions_list match
    elements <- lapply(base_sb_backtest_outputs_list, function(x) x@oos_sb_outputs_m_df@data %>% dplyr::select(id))
    if (!all(purrr::map_lgl(elements, ~ identical(.x, elements[[1]])))){
      stop("Elements of oos_sb_outputs_m_df in each sb_backtest_results object must be the same.")
    }

    ##Check if there are avaialable features_m_df ids
    if (!is.null(features_m_df)) {
      if (any(!base_sb_backtest_outputs_list[[1]]@oos_sb_outputs_m_df@data$id %in% features_m_df@data$id))
        stop("Not all ids in base_sb_backtest_results are available in features_m_df")
    }

    ##Check if backtest_ids are in fact unique
    if (length(sapply(base_sb_backtest_outputs_list, function(x) x@backtest_identifier)) !=
        length(unique(sapply(base_sb_backtest_outputs_list, function(x) x@backtest_identifier)))) {
      stop("Backtest identifiers must be unique.")
    }

    if (length(unique(names(base_sb_backtest_outputs_list))) != length(base_sb_backtest_outputs_list)){
      stop("Names of sb_backtest_results objects must be unique.")
    }

    ##Check if length of oos_predictions_list match
    if (!all(sapply(base_sb_backtest_outputs_list, function(x) nrow(x@oos_sb_outputs_m_df@data)) == nrow(base_sb_backtest_outputs_list[[1]]@oos_sb_outputs_m_df@data))){
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
      .x = base_sb_backtest_outputs_list[-1],

      # Start with the data frame from the first object
      .init = base_sb_backtest_outputs_list[[1]]@oos_sb_outputs_m_df@data %>%
        dplyr::select(id, tickers, dates, pred) %>%
        dplyr::rename_with(~ paste0(base_sb_backtest_outputs_list[[1]]@backtest_identifier), .cols = "pred"),

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
      if (any(!names(base_sb_backtest_outputs_list) %in% colnames(oos_predictions_m_df))){
        stop("All backtests should be in oos_predictions_m_df.")
      }


    ##Transform to meta dataframe
    oos_predictions_m_df <- create_meta_dataframe(oos_predictions_m_df, type = "signals")

    ##Perform Winsorization and Normalization
    if (any(winsorize_predictions, normalize_predictions)) {

      ###Define recipe
      oos_preds_recipe <- recipes::recipe(oos_predictions_m_df@data) %>%
        recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
        recipes::update_role(recipes::all_numeric(), new_role = "predictor")

        ####Winsorize Step
        if (winsorize_predictions) {
          oos_preds_recipe <- oos_preds_recipe %>%
            step_winsorize(recipes::all_numeric_predictors(), probs = winsorization_probs) # Winsorize
        }
        ####Normalize Step
        if (normalize_predictions) {
          oos_preds_recipe <- oos_preds_recipe %>%
            recipes::step_range(recipes::all_numeric_predictors(), min = -1, max = 1) # Normalize

          #####Insert NaN replacement step using step_mutate (when a prediction is constant for a given date)
            ######Identify numeric predictors
            numeric_predictors <- oos_predictions_m_df@data %>%
              dplyr::select(dplyr::where(is.numeric)) %>%
              names()

            ######Build mutation expressions
            nan_replacement_exprs <- purrr::map(numeric_predictors, function(var) {
              rlang::expr(!!rlang::sym(var) := ifelse(is.nan(!!rlang::sym(var)), 0, !!rlang::sym(var)))
            })

            #####Append the step_mutate dynamically
            oos_preds_recipe <- do.call(recipes::step_mutate, c(list(recipe = oos_preds_recipe), nan_replacement_exprs))
        }

      ###Apply recipe
      withCallingHandlers( #Enable control over warning messages
        {
          oos_predictions_m_df <- map_recipe_timewise(
            oos_predictions_m_df,
            recipe = oos_preds_recipe,
            parallel = parallel,
            verbose = FALSE
          )
        },
        warning = function(w) {
          if (grepl("returned NaN.*step_zv\\(\\)", conditionMessage(w))) {
            warning("An oos_prediction column has constant values for a given date. It will be replaced by 0 when normalizing.")
            invokeRestart("muffleWarning")
          }
        }
      )

    }

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

#' @title Consolidate Generic Meta XTS
#'
#' @description
#' Merges meta and base XTS objects, handling cases where either object is NULL.
#' If both are present, they are combined using bind_rows.
#'
#' @param main_generic_m_xts An XTS object representing the most important object.
#' @param supplemental_generic_m_xts An XTS object representing supplementary.
#' @param type A character string indicating the type of data (e.g., "groups").
#' @param consolidate_name A logical indicating whether to consolidate the name of the meta dataframe.
#' @param require_main A logical indicating whether to return NULL if main_generic_m_df is NULL.
#' @param operation A character indicating whether to 'merge' or 'bind_rows' the data.
#'
#' @return A consolidated xts combining main and new data or the appropriate object depending on require_main
#'
consolidate_generic_meta_xts <- function(main_generic_m_xts, supplemental_generic_m_xts, type = "returns", consolidate_name = TRUE, require_main = TRUE,
                                         operation = "merge"){

  #Checks if main and new are being provided and return according to require_main
  #############
    ##main_generic_m_xts
    if (is.null(main_generic_m_xts)){
      ###If main_generic_m_xts is NULL, just pass NULL or supplemental_generic_m_xts depending on require_main
      if (require_main) return(NULL) else return(supplemental_generic_m_xts)
    }
    ##supplemental_generic_m_xts
      ###If supplemental_generic_m_xts is NULL, just pass main_generic_m_xts
      if (is.null(supplemental_generic_m_xts)) return(main_generic_m_xts)

  #############
  #Merge
  if (operation == "merge"){
    ###Check that there is an intersection in dates
    if (!any(zoo::index(main_generic_m_xts@data) %in% zoo::index(supplemental_generic_m_xts@data))){
      stop("There is no intersection between the dates of the two objects. Please check and try again.")
    }

    ###Merge main and new generic meta XTS objects using a left join
    consolidate_meta_xts <- create_meta_xts(merge(main_generic_m_xts@data, supplemental_generic_m_xts@data, join = "left") %>% stats::na.omit(), #Join
                                            meta_xts_name =
                                              #Dynamically build the object name
                                              if (consolidate_name){
                                                paste0(main_generic_m_xts@meta_xts_name, "_", supplemental_generic_m_xts@meta_xts_name)
                                              } else {
                                                main_generic_m_xts@meta_xts_name
                                              }, type = type, asset_type = if (type == "returns") main_generic_m_xts@asset_type else NULL
                                            )

    return(consolidate_meta_xts)

  }
  #Bind
  if (operation == "bind_rows"){
    ##Bind main and new generic meta XTS objects
      ###Check that there is no intersection between dates of each object
      if (any(zoo::index(main_generic_m_xts@data) %in% zoo::index(supplemental_generic_m_xts@data))){
        stop("There is an intersection between the dates of the two objects. Please check and try again.")
      }

      ###Check that binding the two dates sequence will generate a fully filled sequence for returns_m_xts
      if (!inherits(main_generic_m_xts, "metrics_meta_xts")){
        beggining_date <- min(min(zoo::index(main_generic_m_xts@data)), min(zoo::index(supplemental_generic_m_xts@data)))
        end_date <- max(max(zoo::index(main_generic_m_xts@data)), max(zoo::index(supplemental_generic_m_xts@data)))
        expected_date_sequence <- seq.Date(from = beggining_date, to = end_date, by = "month")
        actual_date_sequence <- zoo::index(rbind(main_generic_m_xts@data, supplemental_generic_m_xts@data))
        if (length(expected_date_sequence) != length(actual_date_sequence) || !all(expected_date_sequence == actual_date_sequence)){
          stop("The two objects do not have a fully filled date sequence. Please check and try again.")
        }
      }

      ###Check if colnames match exactly
      if (!identical(colnames(main_generic_m_xts@data), colnames(supplemental_generic_m_xts@data))){
        stop("Column names do not match exactly. Please check and try again.")
      }

      ###Ensure that rbind is time ordered just to be sure
      tmp_xts <- rbind(main_generic_m_xts@data, supplemental_generic_m_xts@data)
      tmp_xts <- tmp_xts[order(zoo::index(tmp_xts)), ] #Sort by index in ascending order

    ##Create the consolidated meta xts
    consolidate_meta_xts <- create_meta_xts(tmp_xts, #Row-Binded
                                            meta_xts_name =
                                              #Dynamically build the object name
                                              if (consolidate_name){
                                                paste0(main_generic_m_xts@meta_xts_name, "_", supplemental_generic_m_xts@meta_xts_name)
                                              } else {
                                                main_generic_m_xts@meta_xts_name
                                              }, type = type, asset_type = if (type == "returns") main_generic_m_xts@asset_type else NULL
    )

    return(consolidate_meta_xts)

  }

}

#' @title Consolidate Generic Meta Dataframes
#'
#' @description
#' Merges meta and base dataframes, handling cases where either dataframe is NULL.
#' If both are present, they are combined using a full join and sorted by ID.
#'
#' @param main_generic_m_df A meta dataframe object.
#' @param supplemental_generic_m_df A base dataframe object.
#' @param type A character string indicating the type of data (e.g., "groups").
#' @param consolidate_name A logical indicating whether to consolidate the name of the meta dataframe.
#' @param require_main A logical indicating whether to return NULL if main_generic_m_df is NULL.
#'
#' @return A consolidated dataframe combining main and new data or the appropriate object depending on require_main
#'
#'
consolidate_generic_meta_dataframes <- function(main_generic_m_df, supplemental_generic_m_df, type, consolidate_name = TRUE, require_main = TRUE) {

  #Checks if main and new are being provided and return according to require_main
  #############

    ##main_generic_m_df
    if (is.null(main_generic_m_df)){
      ###If main_generic_m_df is NULL, just pass NULL or supplemental_generic_m_df
      if (require_main) return(NULL) else return(supplemental_generic_m_df)
    }
    ##supplemental_generic_m_df
    if (is.null(supplemental_generic_m_df)) return(main_generic_m_df)
      ###If supplemental_generic_m_df is NULL, just pass main_generic_m_df

  #############

  #Consolidate if both exist
  #############
    ###Check if colnames match exactly
    if (!identical(colnames(main_generic_m_df@data), colnames(supplemental_generic_m_df@data))){
      stop("Column names do not match exactly. Please check and try again.")
    }

    ##Full Join them. If base is NULL, it will return only main_generic_m_df
    consolidated_generic_m_df <- dplyr::bind_rows(main_generic_m_df@data, supplemental_generic_m_df@data) %>% dplyr::arrange(id) %>%
      create_meta_dataframe(meta_dataframe_name =
                            #Dynamically build the object name
                            if (consolidate_name){
                              paste0(main_generic_m_df@meta_dataframe_name, "_", supplemental_generic_m_df@meta_dataframe_name)
                            } else {
                              main_generic_m_df@meta_dataframe_name
                            }, type = type,
                            port_backtest_workflow = if (type == "stock_universe") main_generic_m_df@port_backtest_workflow else NULL,
                            ss_backtest_workflow = if (type == "signal_universe") main_generic_m_df@ss_backtest_workflow else NULL,
                            sb_backtest_workflow = if (type == "oos_sb_outputs") supplemental_generic_m_df@sb_backtest_workflow else NULL
                            )

  return(consolidated_generic_m_df)
}

#' @title Consolidate Newly Produced Backtest Results
#'
#' @description
#' This function takes a named list of newly produced backtest results (the "new" objects),
#' and merges each with the corresponding old object from an explicitly provided list
#' (`old_backtest_outputs_list`). The old object is treated as the "main" data source,
#' and the new object as the "supplemental" data to be appended.
#'
#' Specifically:
#' \itemize{
#'   \item For objects whose name ends with \code{"_m_df"}, the function uses
#'         \code{\link{consolidate_generic_meta_dataframes}}.
#'   \item For objects whose name ends with \code{"_m_xts"}, the function uses
#'         \code{\link{consolidate_generic_meta_xts}}, with an operation of
#'         \code{"bind_rows"}.
#' }
#'
#' @param new_backtest_outputs_list A named list of **new** backtest objects (S4).
#'   Each element's name (e.g. \code{"port_weights_m_df"}) must match the slot name
#'   used in the old backtest. These objects are the "additional" data.
#'
#' @param old_backtest_results An **old** port_backtest_results objects (S4).
#'   These objects are considered the "main" data source.
#'
#' @return A named list of **consolidated** S4 objects. The returned list has
#' the same names as \code{new_backtest_outputs_list}, but each item now contains
#' **both** old and new data merged together in \code{new_obj@data}.
#'
#' @details
#' \enumerate{
#'   \item Looks up an old object in \code{old_backtest_outputs_list} by the same key.
#'   \item If the name ends with \code{"_m_df"}, calls
#'       \code{consolidate_generic_meta_dataframes(main_generic_m_df = old_obj,
#'       supplemental_generic_m_df = new_obj, ...)}.
#'   \item If the name ends with \code{"_m_xts"}, calls
#'       \code{consolidate_generic_meta_xts(main_generic_m_xts = old_obj,
#'       supplemental_generic_m_xts = new_obj, ...)}.
#'   \item Updates \code{new_obj@data} slot with the merged data.
#'   \item Returns the updated \code{new_obj} in the output list.
#' }
#'
#' An error is thrown if no matching old object is found, or if the slot name does not
#' match the \code{"_m_df"} or \code{"_m_xts"} pattern.
#'
#' @seealso
#' \code{\link{consolidate_generic_meta_dataframes}},
#' \code{\link{consolidate_generic_meta_xts}}
#'
#' @examples
#' \dontrun{
#'   # Suppose you have:
#'   #   - old_results (port_backtest_results) with existing data
#'   #   - a new list 'new_list' with updated S4 objects
#'
#'   # Construct the 'old_backtest_outputs_list':
#'   old_list <- list(
#'     port_weights_m_df   = old_results@port_weights_m_df,
#'     stock_universe_m_df = old_results@stock_universe_m_df,
#'     port_returns_m_xts  = old_results@port_returns_m_xts,
#'     port_costs_m_xts    = old_results@port_costs_m_xts
#'     # add more as needed...
#'   )
#'
#'   # Then call:
#'   updated_list <- consolidate_backtest_results(
#'     new_backtest_outputs_list = new_list,
#'     old_backtest_outputs_list = old_list
#'   )
#' }
#'
consolidate_backtest_results <- function(new_backtest_outputs_list, old_backtest_results){

  #Consolidate using purrr
  updated_results_list <- purrr::imap(
    .x = new_backtest_outputs_list,
    .f = function(new_obj, slot_name){

      ##Retrieve the old object by the same slot name
      old_obj <- methods::slot(old_backtest_results, slot_name)

      ##For oos_testing_eval_metrics_m_xts, best_hyperparameters_m_xts,
      ##validation_eval_metrics_hyper_choice_m_xts, chosen_eval_metric_validation,
      ##it is possible that old obj is NULL

      ##Check if it exists
      if (!slot_name %in% c("oos_testing_eval_metrics_m_xts", "best_hyperparameters_m_xts",
                            "validation_eval_metrics_hyper_choice_m_xts", "chosen_eval_metric_validation",
                            "port_metrics_m_xts") &&
          is.null(old_obj)) {
        stop(sprintf("No old object named '%s' in 'old_backtest_outputs_list'.", slot_name))
      }

      ##Consolidate data
      if (grepl("_m_df$", slot_name)) {

        ###Get correct type
        if (stringr::str_remove(class(old_obj), "_m_df") %in% c(
          "signal_universe", "stock_universe", "oos_sb_outputs", "groups", "target",
          "weights", "priors", "signals", "features", "feature_importance", "raw"
        )){
          type <- stringr::str_remove(class(old_obj), "_m_df")
        } else {
          type <- "generic"
        }

        ###Use the "dataframes" consolidation
        updated_obj <- consolidate_generic_meta_dataframes(
          main_generic_m_df = old_obj,  # the 'main' object
          supplemental_generic_m_df  = new_obj,  # the 'additional' object
          type = type,
          consolidate_name = FALSE
        )

      } else if (grepl("_m_xts$", slot_name)) {

        ###Use the "xts" consolidation
        updated_obj <- consolidate_generic_meta_xts(
          main_generic_m_xts       = old_obj,
          supplemental_generic_m_xts = new_obj,
          type = stringr::str_remove(class(old_obj), "_meta_xts"),
          consolidate_name = FALSE,
          operation = "bind_rows"
        )

      } else {
        ###If neither, warn and return the new object unchanged
        stop(sprintf("Slot '%s' doesn't match _m_df nor _m_xts suffix."))
      }

        ####Update workflow
        if (!is.null(new_obj)){
        new_entry <- list(
          list(
            new_date = new_obj@current_date,
            timestamp = Sys.time(),
            incremental_obj_workflow = new_obj@workflow
          )
        )

        updated_workflow <- c(old_obj@workflow, new_entry) #Add to the old workflow
        names(updated_workflow)[length(updated_workflow)] <- paste0("update_", new_obj@current_date)


        ####Update the new object's workflow
        updated_obj@workflow <- updated_workflow
        }

        ####Return the updated object
        updated_obj
    }
  )
  return(updated_results_list)
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
          dplyr::mutate(dates = as.Date(dates) + months(x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$target_fwd)) %>% #Adjust dates in order to reflect when info was actually available
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

    ## If base custom_signal_universe_metrics_m_df is NULL, just pass meta
    if (is.null(base_custom_signal_universe_metrics_m_df)){
      adapted_custom_signal_universe_metrics_m_df <- meta_custom_signal_universe_metrics_m_df
    } else {
      ##Bind meta and base custom_signal_universe_metrics_m_df
      adapted_custom_signal_universe_metrics_m_df <-
        dplyr::bind_rows(meta_custom_signal_universe_metrics_m_df@data, base_custom_signal_universe_metrics_m_df@data) %>%
        dplyr::arrange(id) %>%
        create_meta_dataframe(meta_dataframe_name = paste0(
          meta_custom_signal_universe_metrics_m_df@meta_dataframe_name, "_", base_custom_signal_universe_metrics_m_df@meta_dataframe_name)
          )
    }

  }

  return(adapted_custom_signal_universe_metrics_m_df)
}



#' Consolidate Meta Backtest Results
#'
#' This function takes a collection of \code{sb_backtest_results} objects along with
#' optional meta-backtest information, and consolidates out-of-sample and validation
#' metrics into both tabular and time-series formats. Specifically, it:
#' \itemize{
#'   \item Identifies backtests that include validation metrics.
#'   \item Calculates common testing date ranges across all backtests.
#'   \item Reshapes both out-of-sample and validation metrics into a consistent \code{long} format,
#'         as well as into time-series \code{xts} objects (\code{time_series_oos_testing_metrics} and
#'         \code{time_series_validation_metrics}).
#'   \item Combines full-sample metrics and those restricted to common dates, while preserving a
#'         \code{testing_dates_range} column in the returned data frames.
#' }
#'
#' @param all_sb_backtest_results A list of \code{sb_backtest_results} objects that you wish to consolidate.
#' @param meta_sb_name An optional character string representing the name of the meta backtest results.
#'   If \code{NULL}, meta-related naming will be skipped.
#' @param base_sb_names A character vector containing the names of base backtest results.
#'
#' @details
#' The function first determines which backtest objects contain validation metrics by inspecting
#' the \code{sb_algorithm} in each workflow. It then computes the intersection of testing dates
#' across all backtests and extracts both out-of-sample and validation metrics into separate
#' data frames. Key columns, such as \code{testing_dates_range}, are included to indicate the
#' date span over which metrics were calculated. Subsequently, the metrics are reshaped into
#' both \code{long} and \code{wide} formats to facilitate time-series representations in \code{xts}.
#'
#' @return A named list containing the following elements:
#' \itemize{
#'   \item \code{all_dates_oos_testing_metrics}: A data frame of out-of-sample metrics for the full date range.
#'   \item \code{common_dates_oos_testing_metrics}: A data frame of out-of-sample metrics restricted to the
#'         common date range found across all backtests.
#'   \item \code{mean_validation_metrics}: A data frame of validation metrics for the backtests that provide them.
#'   \item \code{time_series_oos_testing_metrics}: A named list of \code{xts} objects containing
#'         out-of-sample testing metrics (by metric).
#'   \item \code{time_series_validation_metrics}: A named list of \code{xts} objects containing
#'         validation metrics (by metric).
#' }
#'
#' @examples
#' \dontrun{
#' # Suppose you have a list of sb_backtest_results named base_list and a meta backtest object:
#' my_meta_results <- consolidate_sb_metabacktest_results(
#'   all_sb_backtest_results = c(base_list, meta_result),
#'   meta_sb_name            = \"metaLearner\",
#'   base_sb_names           = names(base_list)
#' )
#'
#' # Access the consolidated metrics
#' my_meta_results$all_dates_oos_testing_metrics
#' my_meta_results$time_series_oos_testing_metrics
#' }
#'
consolidate_sb_metabacktest_results <- function(all_sb_backtest_results, meta_sb_name, base_sb_names) {

  #Initial prep
  ##################
    ##Get all names
    if (!is.null(meta_sb_name)){
      all_sb_names <- c(base_sb_names, meta_sb_name)
    } else {
      all_sb_names <- base_sb_names
    }

    ##Identify which backtests have validation
    sb_names_with_validation <- ifelse(
      !sapply(all_sb_backtest_results, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$sb_algorithm) %in%
        c("ols", "ew", "sw", "rp", "hrp", "mvo", "mmaf"),
      1, 0
    )
    sb_names_with_validation <- all_sb_names[sb_names_with_validation == 1]

    ##Determine the common testing dates
    common_testing_dates_range <- as.Date(
      Reduce(
        intersect,
        lapply(all_sb_backtest_results, function(x) sort(unique(x@oos_sb_outputs_m_df@data$dates)))
      )
    )

    ##Initialize lists and data frames
    oos_metrics_list <- list()
    oos_metrics_common_dates_list <- list()
    validation_metrics_list <- list()
    all_oos_metrics_long_df <- data.frame()
    all_validation_metrics_long_df <- data.frame()
    oos_metric_names <- NULL
    validation_metric_names <- NULL

  ##################

  #Get time series of validation and oos metrics and build: all_dates_oos_testing_metrics, common_dates_oos_testing_metrics and mean_validation_metrics
  ###################
    ##Loop through all sb_backtest_results
    for (i in seq_along(all_sb_backtest_results)) {

      sb_backtest_result <- all_sb_backtest_results[[i]]
      sb_name <- all_sb_names[i]

      ###Out-of-sample testing eval metrics
      if (!is.null(sb_backtest_result@oos_testing_eval_metrics_m_xts)){
      oos_metrics_time_series <- sb_backtest_result@oos_testing_eval_metrics_m_xts@data %>%
        as.data.frame()
      oos_metrics_time_series$dates <- zoo::index(sb_backtest_result@oos_testing_eval_metrics_m_xts@data) %>%
        as.Date()
      oos_metrics_time_series$sb_backtest <- sb_name


      ###Reshape to long format
      oos_metrics_long <- tidyr::pivot_longer(
        data = oos_metrics_time_series,
        cols = -c(dates, sb_backtest),
        names_to = "Metric",
        values_to = "Value"
      ) %>% as.data.frame()
      all_oos_metrics_long_df <- rbind(all_oos_metrics_long_df, oos_metrics_long)

      ###Get consolidated OOS metrics
      oos_metrics <- sb_backtest_result@consolidated_eval_metrics %>%
        dplyr::select(metric, cons_oos)

      ###Subset for common testing dates
      oos_metrics_common_dates <- oos_metrics_time_series[
        which(rownames(oos_metrics_time_series) %in% common_testing_dates_range),
        , drop = FALSE]

      ###Build full-sample metrics data frame with testing_dates_range
      oos_metrics_df <- cbind(
        data.frame(
          sb_backtest = sb_name,
          testing_dates_range = paste0(
            min(as.Date(sb_backtest_result@oos_sb_outputs_m_df@data$dates)),
            "-",
            max(as.Date(sb_backtest_result@oos_sb_outputs_m_df@data$dates))
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        as.data.frame(oos_metrics)
      )

      ###Build common-date metrics data frame with testing_dates_range
      oos_metrics_common_dates_df <- cbind(
        data.frame(
          sb_backtest = sb_name,
          testing_dates_range = paste0(
            min(as.Date(common_testing_dates_range)),
            "-",
            max(as.Date(common_testing_dates_range))
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        oos_metrics_common_dates %>%
          dplyr::select(-sb_backtest, -dates) %>%
          colMeans() %>% #Calculate mean of oos testing dates
          as.data.frame() %>%
          t()
      )

      ###Eliminate rownames
      rownames(oos_metrics_df) <- NULL
      rownames(oos_metrics_common_dates_df) <- NULL
      oos_metric_names <- unique(c(oos_metric_names, names(oos_metrics_df)))

      oos_metrics_list[[i]] <- oos_metrics_df
      oos_metrics_common_dates_list[[i]] <- oos_metrics_common_dates_df
      }

      ###Validation metrics (if available)
      validation_metrics <- if (!is.null(sb_backtest_result@validation_eval_metrics_hyper_choice_m_xts)) {
        sb_backtest_result@validation_eval_metrics_hyper_choice_m_xts@data
      } else {
        NULL
      }

      ###For those that have validation
      if (!is.null(validation_metrics) && nrow(validation_metrics) > 0) {
        validation_metrics_time_series <- validation_metrics %>% as.data.frame()
        validation_metrics_time_series$dates <- zoo::index(
          sb_backtest_result@validation_eval_metrics_hyper_choice_m_xts@data
        ) %>% as.Date()
        validation_metrics_time_series$sb_backtest <- sb_name

        ###Reshape to long format for validation
        validation_metrics_long <- tidyr::pivot_longer(
          data = validation_metrics_time_series,
          cols = -c(dates, sb_backtest),
          names_to = "Metric",
          values_to = "Value"
        ) %>% as.data.frame()
        all_validation_metrics_long_df <- rbind(all_validation_metrics_long_df, validation_metrics_long)

        ###Average consolidated validation metrics
        validation_metrics_average <- sb_backtest_result@consolidated_eval_metrics %>%
          dplyr::select(metric, avg_val)

        validation_metrics_df <- cbind(
          data.frame(
            sb_backtest = sb_name,
            check.names = FALSE,
            stringsAsFactors = FALSE
          ),
          as.data.frame(validation_metrics_average)
        )

        ###Remove rownames and add
        rownames(validation_metrics_df) <- NULL
        validation_metric_names <- unique(c(validation_metric_names, names(validation_metrics_df)))
        validation_metrics_list[[i]] <- validation_metrics_df
      }
    }

    ##Combine consolidated metrics
    all_dates_oos_testing_metrics <- do.call(rbind, oos_metrics_list)
    common_dates_oos_testing_metrics <- do.call(rbind, oos_metrics_common_dates_list)
    mean_validation_metrics <- do.call(rbind, validation_metrics_list)

    rownames(all_dates_oos_testing_metrics) <- NULL
    rownames(common_dates_oos_testing_metrics) <- NULL
    rownames(mean_validation_metrics) <- NULL

    all_dates_oos_testing_metrics[is.nan(as.matrix(all_dates_oos_testing_metrics))] <- NA
    common_dates_oos_testing_metrics[is.nan(as.matrix(common_dates_oos_testing_metrics))] <- NA

    ##Convert numeric columns
    num_cols_oos <- sapply(all_dates_oos_testing_metrics, is.numeric)
    all_dates_oos_testing_metrics[, num_cols_oos] <- sapply(all_dates_oos_testing_metrics[, num_cols_oos], as.numeric)

    num_cols_common <- sapply(common_dates_oos_testing_metrics, is.numeric)
    common_dates_oos_testing_metrics[, num_cols_common] <- sapply(common_dates_oos_testing_metrics[, num_cols_common], as.numeric)

    #Do the same for validation metrics
    if (!is.null(mean_validation_metrics)){
    mean_validation_metrics[is.nan(as.matrix(mean_validation_metrics))] <- NA
      ##Convert numeric cols
      num_cols_val <- sapply(mean_validation_metrics, is.numeric)
      mean_validation_metrics[, num_cols_val] <- sapply(mean_validation_metrics[, num_cols_val], as.numeric)
    }

  ###################

  #Create time series objects for OOS (all time series of oos testing metrics)
  ###################
    ##Init obj
    time_series_oos_testing_metrics <- list()
    time_series_metric_names <- unique(all_oos_metrics_long_df$Metric)

    ##Loop through each metric
    for (metric in time_series_metric_names) {
      ###Create time series object for each metric
      metric_df <- subset(all_oos_metrics_long_df, Metric == metric)
      metric_wide_df <- tidyr::pivot_wider(
        data = metric_df,
        id_cols = dates,
        names_from = sb_backtest,
        values_from = Value
      ) %>% as.data.frame()

      metric_wide_df <- metric_wide_df[order(as.Date(metric_wide_df$dates)), ]
      metric_dates <- as.Date(metric_wide_df$dates)

      metric_wide_xts <- xts::xts(
        x = metric_wide_df %>% dplyr::select(-dates),
        order.by = metric_dates
      )

      ###Define name depending on meta_sb_name
      if (!is.null(meta_sb_name)){
        meta_xts_name <- paste0("testing_", meta_sb_name, "_", metric)
      } else {
        meta_xts_name <- paste0("testing_", metric)
      }

      time_series_oos_testing_metrics[[metric]] <- create_meta_xts(
        metric_wide_xts,
        type = "metrics",
        meta_xts_name = meta_xts_name,
        metric_name = metric,
        source = colnames(metric_wide_xts)
      )
    }

    ##Create time series objects for validation
    time_series_validation_metrics <- list()
    if (nrow(all_validation_metrics_long_df) > 0) {
      validation_time_series_metric_names <- unique(all_validation_metrics_long_df$Metric)

      for (metric in validation_time_series_metric_names) {
        metric_df <- subset(all_validation_metrics_long_df, Metric == metric)
        val_wide_df <- tidyr::pivot_wider(
          data = metric_df,
          id_cols = dates,
          names_from = sb_backtest,
          values_from = Value
        ) %>% as.data.frame()

        val_wide_df <- val_wide_df[order(as.Date(val_wide_df$dates)), ]
        val_dates <- as.Date(val_wide_df$dates)

        val_wide_xts <- xts::xts(
          x = val_wide_df %>% dplyr::select(-dates),
          order.by = val_dates
        )

        #Define name depending on meta_sb_name
        if (!is.null(meta_sb_name)){
          meta_xts_name <- paste0("val_", meta_sb_name, "_", metric)
        } else {
          meta_xts_name <- paste0("val_", metric)
        }


        time_series_validation_metrics[[metric]] <- create_meta_xts(
          val_wide_xts,
          type = "metrics",
          meta_xts_name = meta_xts_name,
          metric_name = metric,
          source = sb_names_with_validation
        )
      }
    }
  ###################

  # Return all objects needed for the final sb_metabacktest_results
  return(list(
    all_dates_oos_testing_metrics = all_dates_oos_testing_metrics,
    common_dates_oos_testing_metrics = common_dates_oos_testing_metrics,
    mean_validation_metrics = mean_validation_metrics,
    time_series_oos_testing_metrics = time_series_oos_testing_metrics,
    time_series_validation_metrics = time_series_validation_metrics
  ))

}



#' Plot Various Consolidated Backtest Results
#'
#' This function creates a variety of diagnostic and comparative plots for both
#' out-of-sample (OOS) and validation metrics obtained from a set of backtest results.
#' The type of plot depends on the value supplied to \code{plot_name}.
#'
#' @param combined_metrics A named list containing out-of-sample testing metrics, typically the
#'   \code{combined_oos_testing_metrics} slot from an \code{sb_metabacktest_results} object.
#'   Must have elements \code{all_dates_oos_testing_metrics} and \code{common_dates_oos_testing_metrics}.
#' @param mean_validation_metrics A data frame containing aggregated validation metrics for each
#'   backtest, usually found in the \code{mean_validation_metrics} slot of
#'   \code{sb_metabacktest_results}.
#' @param time_series_oos_testing_metrics A named list of time-series objects (\code{meta_xts})
#'   capturing OOS testing metrics. Each entry corresponds to a distinct metric.
#' @param time_series_validation_metrics A named list of time-series objects (\code{meta_xts})
#'   capturing validation metrics. Each entry corresponds to a distinct metric.
#' @param base_learners A list of backtest objects used primarily when plotting the
#'   \code{\"Prediction Error Correlation\"}, since it needs the error outputs of each
#'   learner to build the correlation matrix.
#' @param plot_name A character string indicating which plot to generate. Options include:
#'   \describe{
#'     \item{\code{\"Combined and Consolidated OOS Testing Metrics - All Dates\"}}{Shows
#'       a bar chart of OOS testing metrics across all backtests for the full date range.}
#'     \item{\code{\"Combined and Averaged OOS Testing Metrics - Common Dates\"}}{Shows
#'       a bar chart of OOS testing metrics across all backtests restricted to the common
#'       date range.}
#'     \item{\code{\"Time Series OOS Testing Metrics\"}}{Plots time-series OOS metrics for
#'       each backtest, typically over the entire out-of-sample period.}
#'     \item{\code{\"Mean Validation Metrics Comparison\"}}{Creates a bar chart comparing
#'       average validation metrics across backtests.}
#'     \item{\code{\"Time Series Validation Metrics\"}}{Plots validation metrics in a time
#'       series format, if such data exist.}
#'     \item{\code{\"Prediction Error Correlation\"}}{Builds a correlation heatmap of
#'       prediction errors among the selected backtest learners.}
#'   }
#'
#' @details
#' When creating each plot, the function re-labels lengthy backtest identifiers with numeric
#' labels for clarity. It also prints a legend that maps the numeric label back to the
#' underlying identifier. Note that most plot types rely on subsets of the data contained in
#' \code{combined_metrics}, \code{mean_validation_metrics}, \code{time_series_oos_testing_metrics},
#' and \code{time_series_validation_metrics}.
#'
#' For the \code{\"Prediction Error Correlation\"} plot:
#' \itemize{
#'   \item The user can select a subset of backtests (by indices) or all of them.
#'   \item A correlation matrix is constructed from the merged error columns of each selected
#'         backtest, and a heatmap is displayed where only the lower triangle is filled.
#'   \item A legend maps each column index to the corresponding backtest identifier.
#' }
#'
#' @return Called for its side effects of displaying plots. For some cases, a ggplot2 plot object
#'   is returned invisibly (e.g., in the \code{\"Prediction Error Correlation\"} case).
#'
#' @examples
#' \dontrun{
#' # Suppose you have computed sb_metabacktest_results named my_results:
#' #   combined_metrics <- my_results@combined_oos_testing_metrics
#' #   mean_val         <- my_results@mean_validation_metrics
#' #   ts_oos           <- my_results@time_series_oos_testing_metrics
#' #   ts_val           <- my_results@time_series_validation_metrics
#'
#' # To plot consolidated OOS testing metrics for all dates:
#' plot_consolidated_sb_backtest_results(
#'   combined_metrics               = combined_metrics,
#'   mean_validation_metrics        = mean_val,
#'   time_series_oos_testing_metrics= ts_oos,
#'   time_series_validation_metrics = ts_val,
#'   base_learners                  = list_of_backtests,  # Only needed if you want error correlation
#'   plot_name                      = \"Combined and Consolidated OOS Testing Metrics - All Dates\"
#' )
#' }
#'
plot_consolidated_sb_backtest_results <- function(combined_metrics, mean_validation_metrics,
                                                  time_series_oos_testing_metrics, time_series_validation_metrics,
                                                  base_learners,
                                                  plot_name) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # Define color palette
  neon_blue <- "#00BFFF"
  neon_pink <- "#FF1493"
  neon_yellow <- "#FFFF00"
  neon_purple <- "#8A2BE2"
  neon_orange <- "#FF4500"
  neon_green <- "#39FF14"
  blue_bg <- "#001f3f"
  faint_blue <- "#003366"
  black <- "#000000"
  white <- "#FFFFFF"
  neon_hot_pink <- "#FF69B4"
  neon_lime_green <- "#32CD32"
  neon_bright_orange <- "#FFA500"
  neon_rose_pink        <- "#FF6EC7"
  pastel_neon_peach     <- "#FFB347"
  pastel_neon_mint      <- "#77DD77"
  pastel_neon_sky_blue  <- "#AEC6CF"
  pastel_lavender       <- "#CBAACB"
  pastel_blush_pink     <- "#FFD1DC"
  powder_neon_blue      <- "#B0E0E6"
  pastel_neon_soft_yellow <- "#FDFD96"
  lavender_mist         <- "#E6E6FA"
  coral_neon            <- "#FF7F50"
  turquoise_neon        <- "#40E0D0"
  electric_cyan         <- "#7DF9FF"
  bright_neon_green     <- "#66FF66"
  vivid_magenta_pink    <- "#FF66CC"
  soft_neon_orange      <- "#FF9966"
  pastel_aqua_green     <- "#99FFCC"
  pastel_violet         <- "#CC99FF"
  luminous_yellow       <- "#FFFF66"
  light_neon_fuchsia    <- "#FF99CC"
  pastel_cyan           <- "#99FFFF"

  # Extended neon palette
  extended_neon_palette <- c(
    neon_blue,
    neon_pink,
    neon_yellow,
    neon_purple,
    neon_orange,
    neon_green,
    neon_hot_pink,
    neon_lime_green,
    neon_bright_orange,
    neon_rose_pink,
    pastel_neon_peach,
    pastel_neon_mint,
    pastel_neon_sky_blue,
    pastel_lavender,
    pastel_blush_pink,
    powder_neon_blue,
    pastel_neon_soft_yellow,
    lavender_mist,
    coral_neon,
    turquoise_neon,
    electric_cyan,
    bright_neon_green,
    vivid_magenta_pink,
    soft_neon_orange,
    pastel_aqua_green,
    pastel_violet,
    luminous_yellow,
    light_neon_fuchsia,
    pastel_cyan
  )

  # Generate the selected plot
  if (plot_name == "Combined and Consolidated OOS Testing Metrics - All Dates") {
    # Plot consolidated OOS testing metrics for all models (base and meta learners)

    # Prepare data
    # Extract full periods and common dates metrics
    all_dates_metrics <- combined_metrics$all_dates_oos_testing_metrics
    all_dates_metrics$period <- "All Dates"


    # Replace long sb_backtest identifiers with labels
    all_backtests <- unique(all_dates_metrics$sb_backtest)
    labels <- seq_along(all_backtests)
    legend_df <- data.frame(backtest = all_backtests, label = labels)
    all_dates_metrics$backtest_label <- legend_df$label[match(all_dates_metrics$sb_backtest, legend_df$backtest)]

    # Create the plot
    p <- ggplot2::ggplot(
      all_dates_metrics,
      ggplot2::aes(
        x    = factor(backtest_label),
        y    = cons_oos,
        fill = factor(backtest_label)
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~ metric, scales = "free_y") +
      ggplot2::labs(
        title = "Combined and Consolidated OOS Testing Metrics - All Dates",
        x     = "Model (Backtest Label)",
        y     = "Metric Value",
        fill  = "Backtest Label"
      ) +
      ggplot2::scale_fill_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background       = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background      = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title            = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text             = ggplot2::element_text(color = white),
        axis.title            = ggplot2::element_text(color = white),
        strip.text            = ggplot2::element_text(color = white, face = "bold"),
        legend.title          = ggplot2::element_text(color = white),
        legend.text           = ggplot2::element_text(color = white),
        panel.grid.major      = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor      = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Print the legend mapping Backtest labels to identifiers
    cat("\nLegend:\n")
    for (i in seq_along(labels)) {
      cat(paste(labels[i], ":", all_backtests[i], "\n"))
    }

    print(p)
    return(invisible(p))

  } else if (plot_name == "Combined and Averaged OOS Testing Metrics - Common Dates") {

    # Prepare data
    common_dates_raw <- combined_metrics$common_dates_oos_testing_metrics
    common_dates_metrics <- tidyr::pivot_longer(
      common_dates_raw,
      cols      = -c(sb_backtest, testing_dates_range),
      names_to  = "metric",
      values_to = "cons_oos"
    )
    common_dates_metrics$Period <- "Common Dates"

    # Use the same labeling logic
    all_backtests <- unique(common_dates_metrics$sb_backtest)
    labels <- seq_along(all_backtests)
    legend_df <- data.frame(backtest = all_backtests, label = labels)
    common_dates_metrics$backtest_label <- legend_df$label[match(common_dates_metrics$sb_backtest, legend_df$backtest)]

    # Plot (Common Dates)
    p <- ggplot2::ggplot(
      common_dates_metrics,
      ggplot2::aes(
        x    = factor(backtest_label),
        y    = cons_oos,
        fill = factor(backtest_label)
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~ metric, scales = "free_y") +
      ggplot2::labs(
        title = "Combined and Averaged OOS Testing Metrics - Common Dates",
        x     = "Model (Backtest Label)",
        y     = "Metric Value",
        fill  = "Backtest Label"
      ) +
      ggplot2::scale_fill_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background       = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background      = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title            = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text             = ggplot2::element_text(color = white),
        axis.title            = ggplot2::element_text(color = white),
        strip.text            = ggplot2::element_text(color = white, face = "bold"),
        legend.title          = ggplot2::element_text(color = white),
        legend.text           = ggplot2::element_text(color = white),
        panel.grid.major      = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor      = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Show a quick legend
    cat("\nLegend (Common Dates):\n")
    for (i in seq_along(labels)) {
      cat(paste0(labels[i], " : ", all_backtests[i], "\n"))
    }

    print(p)
    return(invisible(p))


  } else if (plot_name == "Time Series OOS Testing Metrics") {
    # Plot time series OOS testing metrics for each model

    # Prepare data
    # time_series_oos_testing_metrics is a list of data frames for each metric
    # Combine them into one data frame for plotting

    metrics_list <- time_series_oos_testing_metrics
    metric_names <- names(metrics_list)

    plot_data <- data.frame()

    for (metric_name in metric_names) {
      metric_df <- metrics_list[[metric_name]]@data %>% as.data.frame()
      metric_long <- tidyr::pivot_longer(
        data = as.data.frame(metric_df),
        cols = dplyr::everything(),
        names_to = "backtest",
        values_to = "value"
      ) %>% as.data.frame()

      ###Reorder according to original colnames
      ordered_backtests <- colnames(metric_df)
      metric_long <- metric_long %>% dplyr::mutate(order = match(backtest, ordered_backtests)) %>%
        dplyr::arrange(order) %>%
        dplyr::select(-order)

     ###Add dates and metrics
      metric_long$dates <- as.Date(zoo::index(metrics_list[[metric_name]]@data))
      metric_long$metric <- metric_name
      plot_data <- rbind(plot_data, metric_long)
    }

    # Replace long backtest identifiers with labels
    all_backtests <- unique(plot_data$backtest)
    labels <- seq_along(all_backtests)
    legend <- data.frame(
      backtest = all_backtests,
      label = labels
    )
    plot_data$backtest_label <- legend$label[match(plot_data$backtest, legend$backtest)]

    # Create the plot
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = dates, y = value, color = as.factor(backtest_label))
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~metric, scales = "free_y") +
      ggplot2::labs(
        title = "Time Series OOS Testing Metrics",
        x = "Date",
        y = "Metric Value",
        color = "Model (Backtest Label)"
      ) +
      ggplot2::scale_color_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Print the legend mapping Backtest labels to identifiers
    cat("\nLegend:\n")
    for (i in seq_along(labels)) {
      cat(paste(labels[i], ":", all_backtests[i], "\n"))
    }

    suppressWarnings(print(p))
    return(invisible(p))

  } else if (plot_name == "Mean Validation Metrics Comparison") {

    # Check if there are any mean validation metrics to plot
    if (length(mean_validation_metrics) == 0) {
      cat("No mean validation metrics to plot.\n")
      return(invisible(NULL))
    }

    # Plot mean validation metrics for each model

    # Prepare data
    data_df <- mean_validation_metrics

    # Replace long backtest identifiers with labels
    if ("sb_backtest" %in% names(data_df)) {
      all_backtests <- unique(data_df$sb_backtest)
      labels <- seq_along(all_backtests)
      legend <- data.frame(
        backtest = all_backtests,
        label = labels
      )
      data_df$backtest_label <- legend$label[match(data_df$sb_backtest, legend$backtest)]
    } else {
      legend <- NULL
    }

    # Melt data for plotting
    plot_data <- tidyr::pivot_longer(
      data = data_df,
      cols = -c(sb_backtest, backtest_label, metric),
      names_to = "variable",
      values_to = "avg_val"
    )


    # Create the plot
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x    = as.factor(backtest_label),
        y    = avg_val,
        fill = factor(backtest_label)
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~ metric, scales = "free_y") +
      ggplot2::labs(
        title = "Mean Validation Metrics Comparison",
        x = "Model (Backtest Label)",
        y = "Metric Value",
        fill = "Metric"
      ) +
      ggplot2::scale_fill_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background       = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background      = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title            = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text             = ggplot2::element_text(color = white),
        axis.title            = ggplot2::element_text(color = white),
        strip.text            = ggplot2::element_text(color = white, face = "bold"),
        legend.title          = ggplot2::element_text(color = white),
        legend.text           = ggplot2::element_text(color = white),
        panel.grid.major      = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor      = ggplot2::element_line(color = faint_blue, size = 0.1)
      )


    # Print the legend mapping Backtest labels to identifiers
    if (!is.null(legend)) {
      cat("\nLegend:\n")
      for (i in seq_along(labels)) {
        cat(paste(labels[i], ":", all_backtests[i], "\n"))
      }
    }

    print(p)
    return(invisible(p))

  } else if (plot_name == "Time Series Validation Metrics") {

    # Check if there are any mean validation metrics to plot
    if (length(mean_validation_metrics) == 0) {
      cat("No mean validation metrics to plot.\n")
      return(invisible(NULL))
    }

    # Plot time series validation metrics for each model

    # Prepare data
    metrics_list <- time_series_validation_metrics
    metric_names <- names(metrics_list)

    plot_data <- data.frame()

    for (metric_name in metric_names) {
      ##Manipulate metric data.frame to long format
      metric_df <- metrics_list[[metric_name]]@data %>% as.data.frame()
      metric_long <- tidyr::pivot_longer(
        data = as.data.frame(metric_df),
        cols = dplyr::everything(),
        names_to = "sb_backtest",
        values_to = "value"
      ) %>% as.data.frame()

      ###Reorder according to original colnames
      ordered_backtests <- colnames(metric_df)
      metric_long <- metric_long %>% dplyr::mutate(order = match(sb_backtest, ordered_backtests)) %>%
        dplyr::arrange(order) %>%
        dplyr::select(-order)

      ###Add dates and metrics
      metric_long$dates <- as.Date(zoo::index(metrics_list[[metric_name]]@data))
      metric_long$metric <- metric_name
      plot_data <- rbind(plot_data, metric_long)

    }

    # Replace long Backtest identifiers with labels
    all_backtests <- unique(plot_data$sb_backtest)
    labels <- seq_along(all_backtests)
    legend <- data.frame(
      backtest = all_backtests,
      label = labels
    )
    plot_data$backtest_label <- legend$label[match(plot_data$sb_backtest, legend$backtest)]

    # Create the plot
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = dates, y = value, color = as.factor(backtest_label))
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~metric, scales = "free_y") +
      ggplot2::labs(
        title = "Time Series Validation Metrics",
        x = "Date",
        y = "Metric Value",
        color = "Model (Backtest Label)"
      ) +
      ggplot2::scale_color_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )


    # Print the legend mapping Backtest labels to identifiers
    cat("\nLegend:\n")
    for (i in seq_along(labels)) {
      cat(paste(labels[i], ":", all_backtests[i], "\n"))
    }

    suppressWarnings(print(p))
    return(invisible(p))

  } else if (plot_name == "Prediction Error Correlation") {

    #Get Cov Matrix of Errors
    oos_errors_df <- purrr::reduce(
      base_learners[-1], #Remove first to avoid duplication
      function(x, y) {
        y_df <- y@oos_sb_outputs_m_df@data %>%
          dplyr::select(id, error) %>%
          dplyr::rename(!!y@backtest_identifier := error)  # Rename before joining

        dplyr::left_join(x, y_df, by = "id")  # Avoids column duplication
      },
      .init = base_learners[[1]]@oos_sb_outputs_m_df@data %>%
        dplyr::select(id, error) %>%
        dplyr::rename(!!base_learners[[1]]@backtest_identifier := error) #Initialization is needed because a data.frame must be returned
    ) %>% dplyr::select(-id)

    # Generate Correlation Matrix
    oos_error_cor_matrix <- stats::cor(oos_errors_df)
    backtest_names <- colnames(oos_error_cor_matrix)  # Extract backtest identifiers

    # Create index mapping for backtests
    backtest_indices <- setNames(seq_along(backtest_names), backtest_names)

    # Selection
    cat("\nEnter 'all' for all backtests,\nOR indices (e.g. '1,3'):\n")
    selection <- readline(prompt = "Your choice: ")

    # Process user selection
    if (nzchar(selection) && tolower(selection) != "all") {
      parts <- strsplit(selection, ",")[[1]]
      parts <- trimws(parts)
      all_numeric <- suppressWarnings(!any(is.na(as.numeric(parts))))
      if (all_numeric) {
        indices <- as.numeric(parts)
        if (any(indices < 1 | indices > length(backtest_names)))
          stop("Some indices are out of range.")
        backtests_to_plot <- backtest_names[indices]  # Convert indices to names
      } else {
        stop("Please enter numeric indices only.")
      }
    } else {
      backtests_to_plot <- backtest_names
    }

    # Subset the Correlation Matrix
    sub_mat <- oos_error_cor_matrix[backtests_to_plot, backtests_to_plot, drop = FALSE]
    sub_mat[upper.tri(sub_mat, diag = FALSE)] <- NA  # Keep lower triangle

    df_cor <- as.data.frame(sub_mat)
    df_cor$BacktestRow <- rownames(df_cor)

    df_long <- df_cor %>%
      tidyr::pivot_longer(
        cols      = -BacktestRow,
        names_to  = "BacktestCol",
        values_to = "Correlation"
      )

    # Replace long backtest names with index numbers in the plot
    df_long$BacktestRow <- as.character(backtest_indices[df_long$BacktestRow])
    df_long$BacktestCol <- as.character(backtest_indices[df_long$BacktestCol])

    # Generate the heatmap plot
    p <- ggplot2::ggplot(
      df_long,
      ggplot2::aes(
        x = factor(.data$BacktestCol, levels = as.character(backtest_indices[backtests_to_plot])),
        y = factor(.data$BacktestRow, levels = rev(as.character(backtest_indices[backtests_to_plot]))),
        fill = .data$Correlation
      )
    ) +
      ggplot2::geom_tile(color = "white") +

      # Add text labels
      ggplot2::geom_text(
        ggplot2::aes(label = round(.data$Correlation, 2)),
        color = "black",
        na.rm = TRUE,
        size = 3
      ) +

      ggplot2::scale_fill_gradient2(
        low      = neon_pink,
        mid      = white,
        high     = neon_green,
        midpoint = 0,
        limits   = c(-1, 1)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        axis.text.x      = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, color = white),
        axis.text.y      = ggplot2::element_text(color = white),
        axis.title       = ggplot2::element_blank(),
        plot.title       = ggplot2::element_text(color = white, size = 14, face = "bold"),
        legend.position  = "right",
        legend.title     = ggplot2::element_text(color = white),
        legend.text      = ggplot2::element_text(color = white)
      ) +
      ggplot2::labs(
        title = paste("Base Learners Backtest Correlation Heatmap")
      )

    print(p)

    #Print legend
    cat("Legend:\n")
    for (i in seq_along(backtest_names)) {
      cat(paste0(i, ": ", backtest_names[i], "\n"))
    }

    return(invisible(p))


  }

  }










