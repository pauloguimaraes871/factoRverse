#' Create Heuristic Ensembles from Machine Learning Backtest Results
#'
#' This function creates ensemble configurations by combining multiple machine learning
#' backtest results. It aggregates out-of-sample (OOS) predictions across different
#' configurations, ensuring that the dates and target variables align. The ensemble
#' can be constructed using either equal weighting or optimal weighting based on a
#' specified evaluation metric.
#'
#' @param ml_backtest_results_list A list of `ml_backtest_results` objects, each containing
#'   out-of-sample predictions, true values, evaluation metrics, and workflow information.
#'   Example structure:
#'   \code{list(
#'     rf_config = ml_backtest_results_object1,
#'     svm_config = ml_backtest_results_object2
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
#' @return A named list containing two `ml_backtest_results` objects:
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
#'   \item Constructs and returns `ml_backtest_results` objects for both ensemble configurations.
#' }
#'
#' @export
convert_oos_predictions_list_to_m_df <- function(ml_backtest_results_list, winsorize_predictions = TRUE, normalize_predictions= TRUE, winsorization_probs = c(0.025,0.975)) {

  # Check if input is a list
  if (!is.list(ml_backtest_results_list)) {
    stop("Input must be a list of S4 objects.")
  }

  # Check if the list has names
  if (is.null(names(ml_backtest_results_list)) || any(names(ml_backtest_results_list) == "")) {
    stop("All elements in the list must have names to be used as column names in the final data frame.")
  }

  # Check if length of oos_predictions_list match
  if(!all(sapply(ml_backtest_results_list, function(x) length(x@oos_prediction_list)) == length(ml_backtest_results_list[[1]]@oos_prediction_list))){
    stop("Length of oos_prediction_list in each ml_backtest_results object must be the same.")
  }

  #Check if elements of lists in oos_predictions_list match
  elements <- lapply(ml_backtest_results_list, function(x) names(unlist(x@oos_prediction_list)))
  if(!all(purrr::map_lgl(elements, ~ identical(.x, elements[[1]])))){
    stop("Elements of lists in oos_prediction_list in each ml_backtest_results object must be the same.")
  }

  # Function to Convert a Single S4 Object to a Data Frame with Model Name as Column
  convert_single_s4_to_df <- function(s4_obj, model_name) {
    # Verify that the object is of class 'ml_backtest_results'
    if (!methods::is(s4_obj, "ml_backtest_results")) {
      stop(paste("All objects in the list must be of class 'ml_backtest_results'. Object named", model_name, "is not."))
    }

    # Check if the 'oos_prediction_list' slot exists
    if (!"oos_prediction_list" %in% methods::slotNames(s4_obj)) {
      stop(paste("The S4 object named", model_name, "does not contain the 'oos_prediction_list' slot."))
    }

    # Extract the 'oos_prediction_list' slot
    oos_pred_list <- s4_obj@oos_prediction_list

    # Check if 'oos_prediction_list' is a list
    if (!is.list(oos_pred_list)) {
      stop(paste("The 'oos_prediction_list' slot in object named", model_name, "must be a list."))
    }

    # Convert the list to a data frame
    df <- purrr::map_dfr(names(oos_pred_list), function(date_name) {
      pred_vector <- oos_pred_list[[date_name]]

      # Ensure that pred_vector is a named numeric vector
      if (!is.numeric(pred_vector) || is.null(names(pred_vector))) {
        stop(paste("Each element in 'oos_prediction_list' must be a named numeric vector. Issue found in date:", date_name, "in model:", model_name))
      }

      data.frame(
        tickers = names(pred_vector),
        dates = as.Date(date_name),  # Adjust the format if necessary
        prediction = as.numeric(pred_vector),
        stringsAsFactors = FALSE
      )
    })

    # Create a unique identifier by combining 'tickers' and 'dates'
    df <- df %>%
      dplyr::mutate(id = paste(tickers, dates, sep = "-")) %>%
      dplyr::select(id, tickers, dates, prediction)

    # Rename the 'prediction' column to the model's name
    colnames(df)[4] <- model_name

    return(df)
  }

  # Iterate Over Each S4 Object in the List and Convert to Data Frame
  prediction_dfs <- purrr::imap(ml_backtest_results_list, ~ convert_single_s4_to_df(.x, .y))

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
