#' Predict with Machine Learning Models
#'
#' This function generates predictions from a refitted machine learning model based on the specified algorithm and hyperparameters.
#'
#' @param ml_algorithm A character string specifying the machine learning algorithm used. Options are "ols", "glmnet", "rf", "xgb", and "nn".
#' @param refit_model The refitted model object to be used for making predictions. The type of model object depends on the algorithm specified.
#' @param best_lam A numeric value for the lambda parameter. Only used for "glmnet" models, where it specifies the lambda value for prediction.
#' @param ml_walk_forward_validation_results Results object from ml_walk_forward_validation
#' @param new_features_m_d_ref A data frame or matrix containing the new feature data for prediction. The first three columns are assumed to be non-predictive and are excluded from the predictions.
#'
#' @return A numeric vector of predictions.
#' @details
#' The function uses the appropriate prediction method based on the `ml_algorithm` specified. It assumes that:
#' \itemize{
#'   \item For "ols", the model is a linear regression model from the `stats` package.
#'   \item For "glmnet", the model is an elastic net model from the `glmnet` package. The `best_lam` parameter should be specified for lambda.
#'   \item For "rf", the model is a random forest model from the `ranger` package. The new data is cleaned using `janitor::clean_names()`.
#'   \item For "xgb", the model is an XGBoost model from the `xgboost` package.
#'   \item For "nn", the model is a neural network model from the `keras` package.
#' }
#' The function excludes the first three columns of `new_features_m_d_ref` for prediction purposes.
#'
predict_ml_model <- function(ml_algorithm = NULL, refit_model = NULL, best_lam = NULL,
                             ml_walk_forward_validation_results = NULL,
                             new_features_m_d_ref){

  #Check for one of ml_walk_forward_validation_results or refit model
  if(is.null(ml_walk_forward_validation_results) & is.null(refit_model) & is.null(ml_algorithm)){
    stop("If ml_walk_forward_validation_results is NULL, then refit model and ml_algorithm should be provided")
  }

  #Check for best_lam if ml_walk_forward_validation_results is not provided
  if(is.null(ml_walk_forward_validation_results)){
    if(ml_algorithm == "glmnet" & is.null(best_lam)){
      stop("best_lam should be provided in case of glmnet")
    }
  }

  #Get parameters if ml_walk_forward_validation_results is provided
  if(!is.null(ml_walk_forward_validation_results)){
  ml_algorithm <- ml_walk_forward_validation_results$metadata$ml_algorithm #Get ML algo
  refit_model <- ml_walk_forward_validation_results$final_model #Get refitted model
  best_hyperparameters <- ml_walk_forward_validation_results$best_hyperparameters #Best hyper
    ##Extract best_lam for glmnet
    best_lam <- ifelse(ml_algorithm == "glmnet",
                       as.numeric(best_hyperparameters[nrow(best_hyperparameters), "best_lam"]),
                       NULL)
  }


 #Choose predict method base on ml_algorithm
  predictions <- switch(
    ml_algorithm, #Depending on the algorithm
    ols = as.numeric(stats::predict(refit_model, newdata = as.data.frame(new_features_m_d_ref[,-c(1:3)]))), #prediction for new data OLS
    glmnet = as.numeric(stats::predict(refit_model, newx = as.matrix(new_features_m_d_ref[,-c(1:3)]), s = best_lam)), #prediction for new data GLM
    rf = as.numeric(stats::predict(refit_model, data = janitor::clean_names(new_features_m_d_ref[,-c(1:3)]))$predictions), #prediction for RF
    xgb = as.numeric(stats::predict(refit_model, newdata = as.matrix(new_features_m_d_ref[,-c(1:3)]))), #predictions for XGB
    nn = as.numeric(stats::predict(refit_model, x = as.matrix(new_features_m_d_ref[,-c(1:3)])))
  )


  #Return predictions
  return(predictions)


}
