#' Create Heuristic Ensembles from Signal Blending Backtest Results
#'
#' This function creates ensemble configurations by combining multiple signal blending
#' backtest results. It aggregates out-of-sample (OOS) predictions across different
#' configurations, ensuring that the dates and target variables align. The ensemble
#' can be constructed using either equal weighting or optimal weighting based on a
#' specified evaluation metric.
#'
#' @param sb_backtest_results_list A list of `sb_backtest_results` objects, each containing
#'   out-of-sample predictions, true values, evaluation metrics, and workflow information.
#'   Example structure:
#'   \code{list(
#'     rf_config = sb_backtest_results_object1,
#'     svm_config = sb_backtest_results_object2
#'   )}
#' @param ensemble_eval_metric A character string specifying the evaluation metric used
#'   to determine optimal weights. Must be one of `"rss"`, `"cp"`, `"rmse"`, `"mae"`,
#'   `"mphe"`, `"mpe"`, `"mape"`, `"hr"`, or `"mb"`. Defaults to `"rmse"`.
#' @param ensemble_huber_delta (Optional) Numeric value specifying the delta parameter
#'   for the Huber loss function. If `NULL`, the mean of the `huber_delta` values from
#'   all backtest results is used.
#' @param ensemble_quantile_tau (Optional) Numeric value specifying the tau parameter
#'   for the quantile loss function. If `NULL`, the mean of the `quantile_tau` values
#'   from all backtest results is used.
#'
#' @return A named list containing two `sb_backtest_results` objects:
#'   \describe{
#'     \item{`ew_ensemble_config`}{Ensemble configuration using equal weights.}
#'     \item{`optimal_ensemble_config`}{Ensemble configuration using optimal weights based on the specified evaluation metric.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates the `ensemble_eval_metric` parameter.
#'   \item Extracts OOS predictions and ensures that all configurations have matching dates and target variables.
#'   \item Calculates equal weights and optimal weights for aggregating predictions.
#'   \item Aggregates predictions using both equal and optimal weights.
#'   \item Computes evaluation metrics and errors for each ensemble configuration.
#'   \item Constructs and returns `sb_backtest_results` objects for both ensemble configurations.
#' }
#'
#' @export
convert_oos_predictions_lists_to_m_df <- function(sb_backtest_results_list, winsorize_predictions = TRUE, normalize_predictions= TRUE, winsorization_probs = c(0.025,0.975)) {

  # Check if input is a list
  if (!is.list(sb_backtest_results_list)) {
    stop("Input must be a list of S4 objects.")
  }

  # Check if the list has names
  if (is.null(names(sb_backtest_results_list)) || any(names(sb_backtest_results_list) == "")) {
    stop("All elements in the list must have names to be used as column names in the final data frame.")
  }

  # Check if length of oos_predictions_list match
  if(!all(sapply(sb_backtest_results_list, function(x) nrow(x@oos_sb_outputs_m_df@data)) == nrow(sb_backtest_results_list[[1]]@oos_sb_outputs_m_df@data))){
    stop("Number of rows of oos_sb_outputs_m_df in each sb_backtest_results object must be the same.")
  }

  #Check if elements of lists in oos_predictions_list match
  elements <- lapply(sb_backtest_results_list, function(x) x@oos_sb_outputs_m_df@data %>% dplyr::select(id))
  if(!all(purrr::map_lgl(elements, ~ identical(.x, elements[[1]])))){
    stop("Elements of oos_sb_outputs_m_df in each sb_backtest_results object must be the same.")
  }

  # Function to Convert a Single S4 Object to a Data Frame with Model Name as Column
  convert_single_s4_to_df <- function(s4_obj, model_name) {
    # Verify that the object is of class 'sb_backtest_results'
    if (!methods::is(s4_obj, "sb_backtest_results")) {
      stop(paste("All objects in the list must be of class 'sb_backtest_results'. Object named", model_name, "is not."))
    }

    # Check if the 'oos_sb_outputs_m_df' slot exists
    if (!"oos_sb_outputs_m_df" %in% methods::slotNames(s4_obj)) {
      stop(paste("The S4 object named", model_name, "does not contain the 'oos_sb_outputs_m_df' slot."))
    }

    # Extract the 'oos_prediction_list' slot
    oos_sb_outputs_m_df <- s4_obj@oos_sb_outputs_m_df@data

    # Check if 'oos_prediction_list' is a list
    if (!is.data.frame(oos_sb_outputs_m_df)) {
      stop(paste("The 'oos_sb_outputs_m_df' slot in object named", model_name, "must be a data.frame."))
    }

    # Convert the list to a (meta) data frame
    m_df <- convert_oos_list_to_m_df(oos_obj_list = oos_pred_list)

    # Rename the 'value' column to the model's name
    colnames(m_df)[4] <- model_name

    return(m_df)
  }

  # Iterate Over Each S4 Object in the List and Convert to Data Frame
  prediction_dfs <- purrr::imap(sb_backtest_results_list, ~ convert_single_s4_to_df(.x, .y))

  # Merge All Prediction Data Frames by 'id' Using Full Join to Ensure All Combinations are Included
  final_predictions_df <- purrr::reduce(prediction_dfs, dplyr::full_join, by = c("id", "tickers", "dates"))

  # Check if 'id', 'tickers', and 'dates' exist
  required_columns <- c("id", "tickers", "dates")
  if (!all(required_columns %in% colnames(final_predictions_df))) {
    stop("The merged data frame does not contain the required 'id', 'tickers', or 'dates' columns.")
  }

  # Arrange the data frame for better readability
  final_predictions_df <- final_predictions_df %>%
    dplyr::arrange(tickers, dates)

  #Transform to meta dataframe
  predictions_m_df <- create_meta_dataframe(final_predictions_df)

  # Perform Winsorization and Normalization
  if(winsorize_predictions) predictions_m_df <- winsorize_panel_data(predictions_m_df, probs = winsorization_probs)
  if(normalize_predictions) predictions_m_df <- normalize_panel_data(predictions_m_df)


  return(predictions_m_df)
}

#' Convert OOS Lists to Meta Data Frame
#' @export
convert_oos_list_to_m_df <- function(oos_obj_list){

  #Convert to list
  m_df <- purrr::map_dfr(names(oos_obj_list), function(date_name) {
    oos_vector <- oos_obj_list[[date_name]]

    # Ensure that oos_vector is a named numeric vector
    if (!is.numeric(oos_vector) || is.null(names(oos_vector))) {
      stop(paste("Each element in 'oos_list' must be a named numeric vector. Issue found in date:", date_name))
    }

    data.frame(
      tickers = names(oos_vector),
      dates = as.Date(date_name),  # Adjust the format if necessary
      value = as.numeric(oos_vector),
      stringsAsFactors = FALSE
    )
  })

  # Create a unique identifier by combining 'tickers' and 'dates'
  m_df <- m_df %>%
    dplyr::mutate(id = paste(tickers, dates, sep = "-")) %>%
    dplyr::select(id, tickers, dates, value)

  return(m_df)
}

