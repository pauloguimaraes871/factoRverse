#' Predict method for signal_port class
#'
#' This method generates predictions using a signal_port obejct
#' based on the provided new feature data. It accommodates different port construction methods.
#' The function handles signal weighting and quantile winsorization.
#'
#' @param object An instance of the `signal_port` class containing the
#' signal portfolio and respective weights.
#' @param new_features_m_df A data frame or an object of class `meta_dataframe`
#'   containing new feature data for which predictions are to be made. The
#'   data frame must be structured correctly and should not include the first
#'   three columns, which are reserved for identifiers.
#' @param upper_quantile_winsorization Numeric value for upper winsorization
#' @param lower_quantile_winsorization Numeric value for lower winsorization
#' @export
#'
#' @export
setMethod("predict", "signal_port", function(object, new_features_m_df_clean,
                                             upper_quantile_winsorization, lower_quantile_winsorization) {


  #Check if all signals are present in new_features_m_df_clean
  if(!all(object@eligible_assets %in% colnames(new_features_m_df_clean))){
    stop("Not all eligible assets are present in new_features_m_df_clean")
  }

  #Eliminate signals not in eligible_signals
  selected_signals_corrected_positions_m_df <- new_features_m_df_clean %>% dplyr::select(dplyr::all_of(object@eligible_assets))

  #Get signal weights
  signal_weights <- object@weights

  ##Calculate preds
  ############################
  preds <- selected_signals_corrected_positions_m_df %>% apply(1, function(row){ #To each row
    sum(row * signal_weights) #Multiply by corresponding weights and sum
  }) %>% #Signal transform
    signal_transform(upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)
  ############################

  return(preds)
})


#' Predict Method for sb_model Class
#'
#' This method generates predictions using a refitted sb model
#' based on the provided new feature data. It accommodates different machine
#' learning algorithms and applies the appropriate prediction logic.
#'
#' @param object An instance of the `sb_model` class containing the
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
#' # Assuming `refit_model` is an instance of `sb_model`
#' # and `new_data` is a properly structured data frame:
#' predictions <- predict(refit_model, new_data)
#'
#' @export
setMethod("predict", "sb_model", function(object, new_features_m_df,
                                          lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975) {

  #Validate input
  if (!is_coercible_to_meta_dataframe(new_features_m_df)) {
    stop("new_features_m_df must be coercible to meta_dataframe.")
  }

  #Prepare new data
  ################
  #Extract data.frame in case of meta_dataframe obj
  if(is_meta_dataframe(new_features_m_df)){
    new_data <- new_features_m_df@data[,-c(1:3)] #get data
  } else {
    new_data <- new_features_m_df[,-c(1:3)]
  }
  ################

  #Get parameters
  ################
  sb_algorithm <- object@sb_algorithm
  optimal_hyper <- object@best_hyperparameters
  if(sb_algorithm == "glmnet"){
    best_lam = as.numeric(optimal_hyper["best_lam"])
  } else {
    NULL
  }
  sb_model_fit <- object@model
  ################

  #Choose predict method base on sb_algorithm
  ################
  ###Make a general case for signal port
  if(sb_algorithm %in% c("ew", "sw", "mvo", "rp", "custom_weights")) sb_algorithm <- "signal_port"

  ###Generate predictions
  predictions <- switch(
    sb_algorithm, #Depending on the algorithm
    ols = as.numeric(predict(sb_model_fit, newdata = as.data.frame(new_data))), #prediction for new data OLS
    glmnet = as.numeric(predict(sb_model_fit, newx = as.matrix(new_data), s = best_lam)), #prediction for new data GLM
    rf = as.numeric(predict(sb_model_fit, data = janitor::clean_names(new_data))$predictions), #prediction for RF
    xgb = as.numeric(predict(sb_model_fit, newdata = as.matrix(new_data))), #predictions for XGB
    nn = as.numeric(predict(sb_model_fit, x = as.matrix(new_data))), #predictions for NN
    signal_port = as.numeric(predict(sb_model_fit, new_features_m_df_clean = new_data,
                                     lower_quantile_winsorization = lower_quantile_winsorization,
                                     upper_quantile_winsorization = upper_quantile_winsorization)) #Predictions for signal ports
  )
  ################

  return(predictions)
})



#' Predict Method for sb_backtest_results Class
#'
#' This method generates predictions using a sb model that has been
#' validated through walk-forward validation. It uses the provided new feature
#' data and applies the appropriate prediction logic based on the underlying
#' model and its hyperparameters.
#'
#' @param object An instance of the `sb_backtest_results` class containing the
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
#' # Assuming `ml_wf_model` is an instance of `sb_backtest_results`
#' # and `new_data` is a properly structured data frame:
#' predictions <- predict(ml_wf_model, new_data)
#'
#' @export
setMethod("predict", "sb_backtest_results", function(object, new_features_m_df,
                                                     lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975) {


  #Get objects of sb_backtest_workflow
  sb_model_refit <- sb_backtest_results@final_sb_model #Get refitted model

  #Get predictions
  predictions <- predict(sb_model_refit, new_features_m_df = new_features_m_df,
                         lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization)

  return(predictions)
})







