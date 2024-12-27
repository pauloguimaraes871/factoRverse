#' @title Fit SB Model
#' @description
#' Fits a Statistical/Beta (SB) model based on the specified \code{sb_algorithm}, preparing and
#' training the model with given data, hyperparameters, and constraints. This function
#' acts as a dispatcher, calling different modeling workflows (OLS, GLMNET, Ranger, XGBoost,
#' Keras, heuristic portfolios, Risk Parity, or MVO) depending on \code{sb_algorithm}.



fit_sb_model <- function(sb_algorithm, #SB Algorithm
                         target_fwd_name, full_data_m_refit_clean = NULL, features_m_refit, target_m_refit, #Data
                         custom_objective_translated, huber_delta, quantile_tau, early_stop, keras_architecture_parameters, #Model Parameters
                         optimal_hyper = NULL, chosen_eval_metric_translated, #Validation Parameters
                         most_recent_signal_universe_m_d_ref, selected_backtest_returns_corrected_positions_xts_upd_ref,  selected_market_factor_proxy_xts_upd_ref, #Signal Universe
                         covariance_matrix_sample_size = 36, covariance_estimation_method = "sample", active_returns = TRUE, #COV (for RP and MVO)
                         rp_method = "cyclical-spinu", n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", concentration_constraint_policy, #RP/MVO
                         upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose){ #MISC

  ###Define the heuristic_sb_metric for optimization in heuristic portfolios
  if(sb_algorithm %in% c("sw", "mvo")){
    ###Identify objective (min or max) and heuristic metric
    objective <- ifelse(stringr::str_detect(custom_objective_translated, "max_"), "max", "min")
    heuristic_sb_metric <- most_recent_signal_universe_m_d_ref %>% dplyr::pull(stringr::str_remove(custom_objective_translated, paste0(objective, "_")))

    ###Calculate final signal base on user choice for custom objective
    most_recent_signal_universe_m_d_ref[, "final_signal"] <- signal_transform(
      if(objective == "max") heuristic_sb_metric else heuristic_sb_metric*(-1),
      upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
    )
  }

  ###Fit sb model based on sb_algorithm
  sb_model <- switch(sb_algorithm,
                     ##OLS
                     ols = stats::lm(paste(target_fwd_name,'~.'), data = full_data_m_refit_clean),

                     ##GLMNET
                     glmnet::glmnet(features_m_refit[,-c(1:3)], target_m_refit, #Features and target
                                    #Hyperparameters
                                    alpha = optimal_hyper["alpha"],
                                    lambda.min.ratio = optimal_hyper["lambda.min.ratio"],
                                    verbose = verbose
                     ),

                     ##Ranger
                     rf = ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(full_data_m_refit_clean), #Features and target
                                         #Hyperparameters
                                         mtry = optimal_hyper["mtry"] * (ncol(full_data_m_refit_clean) - 1),
                                         num.trees = optimal_hyper["num.trees"],
                                         max.depth = optimal_hyper["max.depth"],
                                         min.bucket = optimal_hyper["min.bucket"],
                                         verbose = verbose
                     ),
                     ##XGB
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
                                              verbose = verbose
                     ),
                     ##Keras
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


                                          verbose = verbose
                     )$model_nn, #This is a wrapper for keras

                     ##Equal-Weighted Signals
                     ew = set_portfolio_weights(port_construction_method = "ew",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref), #Universe of signals

                     ##Signal-Weighted Signals
                     sw = set_portfolio_weights(port_construction_method = "sw",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref), #Universe of signals

                     ##Risk-Parity
                     rp = set_portfolio_weights(port_construction_method = "rp",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                returns_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
                                                selected_benchmark_upd_ref = selected_market_factor_proxy_xts_upd_ref,
                                                covariance_matrix_sample_size = covariance_matrix_sample_size,
                                                covariance_estimation_method = covariance_estimation_method,
                                                active_returns = active_returns
                                                ),
                     ##MVO
                     mvo = set_portfolio_weights(port_construction_method = "mvo",
                                                 universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                 returns_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
                                                 selected_benchmark_upd_ref = selected_market_factor_proxy_xts_upd_ref,
                                                 covariance_matrix_sample_size = covariance_matrix_sample_size,
                                                 covariance_estimation_method = covariance_estimation_method,
                                                 active_returns = active_returns,
                                                 rp_method = rp_method,
                                                 n_random_ports = n_random_ports,
                                                 random_ports_method = random_ports_method,
                                                 opt_objective = opt_objective,
                                                 concentration_constraint_policy = concentration_constraint_policy
                                                 )

                     )

}
