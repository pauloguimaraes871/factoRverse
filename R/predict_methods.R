#' Predict Method for refit_ml_model Class
#'
#' This method generates predictions using a refitted machine learning model
#' based on the provided new feature data. It accommodates different machine
#' learning algorithms and applies the appropriate prediction logic.
#'
#' @param object An instance of the `refit_ml_model` class containing the
#'   refitted model and its parameters.
#' @param new_features_m_df A data frame or an object of class `meta_dataframe`
#'   containing new feature data for which predictions are to be made. The
#'   data frame must be structured correctly and should not include the first
#'   three columns, which are reserved for identifiers.
#'
#' @return A numeric vector of predictions for the new feature data.
#'
#' @details The function first validates that `new_features_m_df` is coercible
#' to a `meta_dataframe`. It extracts the relevant data from the input and
#' uses the appropriate prediction method based on the specified machine
#' learning algorithm (e.g., OLS, GLMNET, RF, XGB, NN).
#'
#' @examples
#' # Assuming `refit_model` is an instance of `refit_ml_model`
#' # and `new_data` is a properly structured data frame:
#' predictions <- predict(refit_model, new_data)
#'
#' @export
setMethod("predict", "refit_ml_model", function(object, new_features_m_df) {

  #Validate input
  if (!is_coercible_to_meta_dataframe(new_features_m_df)) {
    stop("new_features_m_df must be coercible to meta_dataframe.")
  }

  #Prepare new data
  #Extract data.frame in case of meta_dataframe obj
  if(is_meta_dataframe(new_features_m_df)){
    new_features_m_df <- new_features_m_df@data[,-c(1:3)] #get data
  } else {
    new_data <- new_features_m_df[,-c(1:3)]
  }

  #Get parameters
  ml_algorithm <- object@ml_algorithm
  optimal_hyper <- object@best_hyperparameters
  if(ml_algorithm == "glmnet"){
    best_lam = as.numeric(optimal_hyper["best_lam"])
  } else {
    NULL
  }
  refit_model <- object@model

  #Choose predict method base on ml_algorithm
  predictions <- switch(
    ml_algorithm, #Depending on the algorithm
    ols = as.numeric(stats::predict(refit_model, newdata = as.data.frame(new_data))), #prediction for new data OLS
    glmnet = as.numeric(stats::predict(refit_model, newx = as.matrix(new_data), s = best_lam)), #prediction for new data GLM
    rf = as.numeric(stats::predict(refit_model, data = janitor::clean_names(new_data))$predictions), #prediction for RF
    xgb = as.numeric(stats::predict(refit_model, newdata = as.matrix(new_data))), #predictions for XGB
    nn = as.numeric(stats::predict(refit_model, x = as.matrix(new_data)))
  )

  return(predictions)
})



#' Predict Method for ml_wf_val_results Class
#'
#' This method generates predictions using a machine learning model that has been
#' validated through walk-forward validation. It uses the provided new feature
#' data and applies the appropriate prediction logic based on the underlying
#' model and its hyperparameters.
#'
#' @param object An instance of the `ml_wf_val_results` class containing the
#'   validated model, metadata, and best hyperparameters.
#' @param new_features_m_df A data frame or an object of class `meta_dataframe`
#'   containing new feature data for which predictions are to be made. The
#'   data frame must be structured correctly and should not include the first
#'   three columns, which are reserved for identifiers.
#'
#' @return A numeric vector of predictions for the new feature data.
#'
#' @details The function validates that `new_features_m_df` is coercible to a
#' `meta_dataframe`. It extracts the relevant data and uses the appropriate
#' prediction method based on the specified machine learning algorithm (e.g., OLS,
#' GLMNET, RF, XGB, NN). The method retrieves the refitted model and the best
#' hyperparameters for making predictions.
#'
#' @examples
#' # Assuming `ml_wf_model` is an instance of `ml_wf_val_results`
#' # and `new_data` is a properly structured data frame:
#' predictions <- predict(ml_wf_model, new_data)
#'
#' @export
setMethod("predict", "ml_wf_val_results", function(object, new_features_m_df) {

  #Validate input
  if (!is_coercible_to_meta_dataframe(new_features_m_df)) {
    stop("new_features_m_df must be coercible to meta_dataframe.")
  }

  #Prepare new data
  #Extract data.frame in case of meta_dataframe obj
  if(is_meta_dataframe(new_features_m_df)){
    new_features_m_df <- new_features_m_df@data[,-c(1:3)] #get data
  } else {
    new_data <- new_features_m_df[,-c(1:3)]
  }

  #Get objects of ml_walk_forward_validation_results
    ml_algorithm <- ml_wf_val_results@metadata$ml_algorithm #Get ML algo
    refit_model <- ml_walk_forward_validation_results@final_model #Get refitted model
    best_hyperparameters <- ml_walk_forward_validation_results@best_hyperparameters #Best hyper
    ##Extract best_lam for glmnet
    best_lam <- ifelse(ml_walk_forward_validation_results@metadata$ml_algorithm == "glmnet",
                       as.numeric(best_hyperparameters[nrow(best_hyperparameters), "best_lam"]),
                       NULL)

    #Choose predict method base on ml_algorithm
    predictions <- switch(
      ml_algorithm, #Depending on the algorithm
      ols = as.numeric(stats::predict(refit_model, newdata = as.data.frame(new_data))), #prediction for new data OLS
      glmnet = as.numeric(stats::predict(refit_model, newx = as.matrix(new_data), s = best_lam)), #prediction for new data GLM
      rf = as.numeric(stats::predict(refit_model, data = janitor::clean_names(new_data))$predictions), #prediction for RF
      xgb = as.numeric(stats::predict(refit_model, newdata = as.matrix(new_data))), #predictions for XGB
      nn = as.numeric(stats::predict(refit_model, x = as.matrix(new_data)))
    )

  return(predictions)
})
