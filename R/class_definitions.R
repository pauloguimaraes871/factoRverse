#' Define the `meta_dataframe` S4 Class
#'
#' This class represents a metadata-enhanced data frame. It extends the functionality
#' of a standard data frame by including additional metadata slots. The class is designed
#' to ensure that the input data frame adheres to specific structural requirements, including
#' unique identifiers, valid date formats, and unique column names.
#'
#' @slot data A \code{data.frame} containing the actual data.
#' @slot workflow A \code{list} for storing metadata about the data manipulation workflow.
#' @slot signals A \code{character} vector containing the names of columns that represent signals.
#' @slot unique_dates A \code{numeric} value representing the count of unique dates in the data.
#' @slot unique_tickers A \code{numeric} value representing the count of unique tickers in the data.
#' @slot n_obs A \code{numeric} value representing the total number of observations in the data.
#'
#' @details
#' The \code{meta_dataframe} class ensures that the data frame is structured correctly with the required columns,
#' and includes metadata about the data. The \code{signals} slot holds the names of columns representing various signals.
#' The \code{unique_dates}, \code{unique_tickers}, and \code{n_obs} slots store the metadata related to the number of unique dates,
#' tickers, and total observations respectively.
#'
#' @examples
#' # Define a sample data frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value = c(10, 20)
#' )
#'
#' # Create a meta_dataframe object
#' meta_df <- create_meta_dataframe(df)
#'
#' # Print the meta_dataframe object
#' print(meta_df)
#'
#' @export
setClass("meta_dataframe",
         slots = c(
           data = "data.frame",        # Slot for the data frame
           workflow = "list",          # Slot for storing metadata about the data manipulation workflow
           signals = "character",      # Slot for storing column names
           unique_dates = "numeric",   # Slot for storing count of unique dates
           unique_tickers = "numeric", # Slot for storing count of unique tickers
           n_obs = "numeric"           # Slot for storing total number of observations
         ))



# Define the is_meta_dataframe function
#' Check if an object is a meta_dataframe
#'
#' @param x The object to check.
#' @return TRUE if x is of class "meta_dataframe", FALSE otherwise.
#' @export
is_meta_dataframe <- function(x) {
  inherits(x, "meta_dataframe")
}


#' Define the `refit_ml_model` S4 Class
#'
#' This class represents a refitted machine learning model. It encapsulates the algorithm used, hyperparameters,
#' feature data, target variable, and the fitted model object.
#'
#' @slot ml_algorithm A character string specifying the machine learning algorithm used (e.g., "ols", "glmnet", "rf", "xgb", "nn").
#' @slot best_hyperparameters A list containing hyperparameters relevant to the specified machine learning algorithm.
#' @slot model The fitted model object, which varies based on the algorithm used.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{refit()}}{Refits the model based on the specified algorithm and hyperparameters.}
#'   \item{\code{predict(new_features)}}{Generates predictions using the fitted model on new feature data.}
#' }
#'
#' @export
setClass(
  "refit_ml_model",
  slots = list(
    model = "ANY",
    ml_algorithm = "character",
    best_hyperparameters = "numeric",
    custom_objective = "ANY",
    huber_delta = "numeric",
    keras_architecture_parameters = "ANY"
  )
)



#' @title S4 Class for Machine Learning Walk-Forward Validation Results
#'
#' @description This class encapsulates the results of a time series validation on a machine learning model,
#' including out-of-sample predictions, errors, evaluation metrics, model details and metadata.
#'
#' @slot oos_prediction_list A list containing out-of-sample predictions.
#' @slot oos_error_list A list containing out-of-sample error metrics.
#' @slot oos_y_list A list containing the actual values for out-of-sample data.
#' @slot oos_testing_eval_metrics A list of evaluation metrics for testing.
#' @slot final_model An object representing the final fitted model.
#' @slot chosen_eval_metric_validation A list of chosen evaluation metrics for validation (if applicable).
#' @slot best_hyperparameters A data frame containing the best hyperparameter choices.
#' @slot validation_eval_metrics_hyper_choice A list of evaluation metrics based on hyperparameter choices.
#' @slot plots A list of plots generated during the model evaluation.
#' @slot metadata A list containing metadata about the model and data used.
#'
#' @export
setClass("ml_walk_forward_validation_results",
         slots = list(
           oos_prediction_list = "list",
           oos_error_list = "list",
           oos_y_list = "list",
           oos_testing_eval_metrics = "list",
           final_model = "ANY",  # Adjust type based on your model
           chosen_eval_metric_validation = "list",
           best_hyperparameters = "data.frame",
           validation_eval_metrics_hyper_choice = "list",
           plots = "list",
           metadata = "list"
         ))
