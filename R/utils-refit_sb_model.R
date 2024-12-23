refit_sb_model <- function(sb_algorithm, target_fwd_name, full_data_m_refit_clean = NULL, features_m_refit, target_m_refit, optimal_hyper = NULL,
                           signal_universe_m_df = NULL){

  ###Define the heuristic_sb_metric for optimization in heuristic portfolios
  if(sb_algorithm %in% c("sw", "mto")){
    ###Identify objective (min or max) and heuristic metric
    objective <- ifelse(stringr::str_detect(custom_objective, "max_"), "max", "min")
    heuristic_sb_metric <- stringr::str_remove(custom_objective, paste0(objective, "_"))

    signal_universe_m_df[, "final_signal"] <- signal_transform(
      if(objective == "max") signal_universe_m_d_ref[, heuristic_sb_metric] else signal_universe_m_d_ref[, heuristic_sb_metric]*(-1),
    )


  ###Fit sb model based on sb_algorithm
  sb_model <- switch(sb_algorithm,
                     ols = stats::lm(paste(target_fwd_name,'~.'), data = full_data_m_refit_clean),

                     glmnet::glmnet(features_m_refit[,-c(1:3)], target_m_refit, #Features and target
                                    #Hyperparameters
                                    alpha = optimal_hyper["alpha"],
                                    lambda.min.ratio = optimal_hyper["lambda.min.ratio"],
                                    verbose = FALSE
                     ),

                     rf = ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(full_data_m_refit_clean), #Features and target
                                         #Hyperparameters
                                         mtry = optimal_hyper["mtry"] * (ncol(full_data_m_refit_clean) - 1),
                                         num.trees = optimal_hyper["num.trees"],
                                         max.depth = optimal_hyper["max.depth"],
                                         min.bucket = optimal_hyper["min.bucket"],
                                         verbose = FALSE
                     ),

                     xgb = xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(features_m_refit[,-c(1:3)]), #Features and target
                                                                          label = target_m_refit),
                                              objective = custom_objective_translated,
                                              huber_slope = huber_delta,
                                              #quantile_alpha = quantile_tau,
                                              #Hyperparameters
                                              min_child_weight = optimal_hyper["min_child_weight"],
                                              max_depth = round(optimal_hyper["max_depth"],0),
                                              subsample = optimal_hyper["subsample"],
                                              colsample_bytree = optimal_hyper["colsample_bytree"],
                                              eta = optimal_hyper["eta"],
                                              alpha = optimal_hyper["alpha"],
                                              gamma = optimal_hyper["gamma"],
                                              nrounds = if(is.null(early_stop)){
                                                c(optimal_hyper["nrounds"])
                                              } else {
                                                c(optimal_hyper["best_iteration"])
                                              },
                                              verbose = FALSE
                     ),

                     nn = fit_keras_model(features_matrix_train_clean = features_m_refit[,-c(1:3)], #Feature
                                          target_vector_train = target_m_refit, #Target
                                          custom_objective = custom_objective_translated, #No need for switch
                                          huber_slope = huber_delta, #Huber loss
                                          chosen_eval_metric_translated = chosen_eval_metric_translated, #Is this really necessary?

                                          #Keras Parameters
                                          #Architecture
                                          keras_architecture_parameters = keras_architecture_parameters,

                                          #Hyperparameters
                                          #Training
                                          number_of_epochs = if(is.null(early_stop)){
                                            c(optimal_hyper["number_of_epochs"])
                                          } else {
                                            c(optimal_hyper["best_iteration"])
                                          },
                                          size_of_batch = optimal_hyper["size_of_batch"],
                                          lr = optimal_hyper["lr"],

                                          #Regularization
                                          regularizer_l1 = optimal_hyper["regularizer_l1"],
                                          regularizer_l2 = optimal_hyper["regularizer_l2"],
                                          droprate = optimal_hyper["droprate"],


                                          verbose = FALSE
                     )$model_nn, #This is a wrapper for keras

                     ew = set_portfolio_weights(universe_m_d_ref = signal_universe_m_df), #Universe of signals
                     sw = set_portfolio_weights(universe_m_d_ref = signal_universe_m_df)



                     )

)

}
