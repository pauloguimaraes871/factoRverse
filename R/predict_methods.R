# Define the print method for refit_ml_model
############################################
setMethod("predict", "refit_ml_model", function(object, new_features_m_df, ...) {

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



# Define the print method for ml_wf_val_results
############################################
setMethod("predict", "ml_wf_val_results", function(object, new_features_m_df, ...) {

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
