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
    model_class = "character",
    ml_algorithm = "character",
    best_hyperparameters = "numeric",
    custom_objective = "ANY",
    huber_delta = "numeric",
    keras_architecture_parameters = "ANY"
  )
)



#' S4 Class for Time Series Walk-Forward Validation Results of Machine-Learning Models
#'
#' This S4 class encapsulates the results and parameters from performing walk-forward
#' validation on time series data using machine learning algorithms. It includes
#' information about the model, data, tuning process, and performance metrics.
#'
#' @slot oos_prediction_list A list containing out-of-sample predictions indexed by testing dates.
#' @slot oos_error_list A list of out-of-sample errors indexed by testing dates.
#' @slot oos_y_list A list containing the actual target values for the out-of-sample period, indexed by testing dates.
#' @slot oos_testing_eval_metrics A list of evaluation metrics for the out-of-sample testing samples.
#' @slot final_model The final refitted machine learning model with best hyperparameters found after tuning. Possibly a object of refit_ml_model S4 class.
#' @slot chosen_eval_metric_validation A list of data.frames with the chosen evaluation metric calculated for the hyperparameter grid.
#' @slot best_hyperparameters A data frame containing the best hyperparameters selected during tuning for each rebalancing period.
#' @slot validation_eval_metrics_hyper_choice All evaluation metrics calculated for the set of best hyperparameters.
#' @slot metadata A list containing metadata about the walk-forward validation process. It includes:
#' \itemize{
#'   \item \strong{ml_algorithm}: A character string specifying the machine learning algorithm used.
#'   \item \strong{custom_objective}: A character string indicating the custom loss function applied (e.g., "squared_error").
#'   \item \strong{dates_covered}: A vector of dates representing the time period covered by the analysis.
#'   \item \strong{n_dates}: An integer indicating the total number of dates in the covered period.
#'   \item \strong{training_sample_size}: An integer representing the size of the training samples used.
#'   \item \strong{validation_sample_size}: An integer indicating the size of the validation samples used.
#'   \item \strong{testing_sample_size}: An integer indicating the size of the testing samples used.
#'   \item \strong{dates_testing_sample}: A vector of dates corresponding to the testing samples.
#'   \item \strong{first_rebalance_date}: A date indicating the first date when the model was rebalanced.
#'   \item \strong{rebalance_dates}: A vector of dates when the model was rebalanced.
#'   \item \strong{split_method}: A character string indicating the method used for splitting the data (e.g., "expanding" or "rolling").
#'   \item \strong{ids}: A vector of identifiers from the features data frame.
#'   \item \strong{nobs}: An integer representing the total number of observations in the features data frame.
#'   \item \strong{tickers}: A vector of unique stock tickers from the features data frame.
#'   \item \strong{n_stocks}: An integer indicating the number of unique stocks in the features data frame.
#'   \item \strong{target_fwd_name}: A character string naming the target variable for forward prediction.
#'   \item \strong{target_fwd}: A vector of forward target values.
#'   \item \strong{target_workflow}: A description of the workflow used for the target variable.
#'   \item \strong{target_object}: A character string capturing the name of the target data frame passed to the function.
#'   \item \strong{features}: A character vector of feature names extracted from the features data frame.
#'   \item \strong{features_workflow}: A description of the workflow used for the features.
#'   \item \strong{features_object}: A character string capturing the name of the features data frame passed to the function.
#'   \item \strong{tuning_method}: A character string indicating the method used for hyperparameter tuning (e.g., "grid_search").
#'   \item \strong{n_iter}: An integer specifying the number of iterations for tuning methods that require it.
#'   \item \strong{k_iter}: An integer indicating the number of times to sample the evaluation function during tuning.
#'   \item \strong{acq}: A character string specifying the acquisition function used in Bayesian optimization.
#'   \item \strong{init_points}: An integer indicating the number of initial random points for Bayesian optimization.
#'   \item \strong{hyper_grid_domain_list}: A list containing hyperparameter definitions for tuning.
#'   \item \strong{chosen_eval_metric}: A character string representing the evaluation metric chosen for optimization.
#'   \item \strong{huber_delta}: A numeric value indicating the boundary for the Huber loss function.
#'   \item \strong{quantile_tau}: A numeric value representing the target quantile for quantile loss.
#'   \item \strong{early_stop}: A criteria indicating if early stopping was used during training.
#'   \item \strong{keras_architecture_parameters}: A list containing parameters for the Keras model architecture.
#'   \item \strong{completion_time}: The system time when the validation process was completed.
#'   \item \strong{elapsed_time}: A numeric value representing the total time taken for the validation process.
#'   \item \strong{parallel}: A logical value indicating whether the process was run in parallel (TRUE or FALSE).
#'   \item \strong{call}: The matched call used to create the S4 object, capturing the function call context.
#' }
#'
#'
#' @return An S4 object of class `ml_wf_val_results` containing all the specified results and metadata.
#'
#'
#'@export
setClass(
  "ml_wf_val_results",
  slots = list(
    oos_prediction_list = "list",
    oos_error_list = "list",
    oos_y_list = "list",
    oos_testing_eval_metrics = "list",
    final_model = "ANY",  # Replace with specific class if known
    chosen_eval_metric_validation = "ANY",
    best_hyperparameters = "ANY",  # Replace with specific class if needed
    validation_eval_metrics_hyper_choice = "ANY",
    metadata = "list"
  )
)






##########################
#########Acessors#########
##########################



# Define generic accessor methods for ml_wf_val_results
##########################################################
setGeneric("get_oos_prediction_list", function(object) standardGeneric("get_oos_prediction_list"))
setGeneric("get_oos_error_list", function(object) standardGeneric("get_oos_error_list"))
setGeneric("get_oos_y_list", function(object) standardGeneric("get_oos_y_list"))
setGeneric("get_oos_testing_eval_metrics", function(object) standardGeneric("get_oos_testing_eval_metrics"))
setGeneric("get_final_model", function(object) standardGeneric("get_final_model"))
setGeneric("get_chosen_eval_metric_validation", function(object) standardGeneric("get_chosen_eval_metric_validation"))
setGeneric("get_best_hyperparameters", function(object) standardGeneric("get_best_hyperparameters"))
setGeneric("get_validation_eval_metrics_hyper_choice", function(object) standardGeneric("get_validation_eval_metrics_hyper_choice"))
setGeneric("get_metadata", function(object) standardGeneric("get_metadata"))

# Define methods for accessing the slots
setMethod("get_oos_prediction_list", "ml_wf_val_results", function(object) {
  return(object@oos_prediction_list)
})

setMethod("get_oos_error_list", "ml_wf_val_results", function(object) {
  return(object@oos_error_list)
})

setMethod("get_oos_y_list", "ml_wf_val_results", function(object) {
  return(object@oos_y_list)
})

setMethod("get_oos_testing_eval_metrics", "ml_wf_val_results", function(object) {
  return(object@oos_testing_eval_metrics)
})

setMethod("get_final_model", "ml_wf_val_results", function(object) {
  return(object@final_model)
})

setMethod("get_chosen_eval_metric_validation", "ml_wf_val_results", function(object) {
  return(object@chosen_eval_metric_validation)
})

setMethod("get_best_hyperparameters", "ml_wf_val_results", function(object) {
  return(object@best_hyperparameters)
})

setMethod("get_validation_eval_metrics_hyper_choice", "ml_wf_val_results", function(object) {
  return(object@validation_eval_metrics_hyper_choice)
})

setMethod("get_metadata", "ml_wf_val_results", function(object) {
  return(object@metadata)
})
##########################################################


# Define generic accessor methods for meta_dataframe
#########################################################
setGeneric("get_data", function(object) standardGeneric("get_data"))
setGeneric("get_workflow", function(object) standardGeneric("get_workflow"))

# Define methods for accessing the slots
setMethod("get_data", "meta_dataframe", function(object) {
  return(object@data)
})

setMethod("get_workflow", "meta_dataframe", function(object) {
  return(object@workflow)
})
#########################################################




# Define generic accessor methods for refit_ml_model
#########################################################
setGeneric("get_ml_algorithm", function(object) standardGeneric("get_ml_algorithm"))
setGeneric("get_best_hyperparameters", function(object) standardGeneric("get_best_hyperparameters"))
setGeneric("get_model", function(object) standardGeneric("get_model"))

# Define methods for accessing the slots
setMethod("get_ml_algorithm", "refit_ml_model", function(object) {
  return(object@ml_algorithm)
})

setMethod("get_best_hyperparameters", "refit_ml_model", function(object) {
  return(object@best_hyperparameters)
})

setMethod("get_model", "refit_ml_model", function(object) {
  return(object@model)
})
#########################################################
