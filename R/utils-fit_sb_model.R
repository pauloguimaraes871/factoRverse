#' @title Fit Signal Blending (SB) Model
#' @description
#' Fits a Signal Blending (SB) model based on the specified \code{sb_algorithm}, preparing and
#' training the model with the given data, hyperparameters, and constraints. This function
#' dispatches to various modeling workflows, including OLS, GLMNET, Ranger (RF), XGBoost,
#' Keras (NN), heuristic portfolios, Risk Parity, or Mean-Variance Optimization (MVO),
#' depending on the input.
#'
#' @param sb_algorithm A \code{character} specifying the signal blending algorithm. Options include:
#'   \code{"ols"}, \code{"glmnet"}, \code{"rf"}, \code{"xgb"}, \code{"nn"}, \code{"ew"}, \code{"sw"}, \code{"rp"}, \code{"mvo"}.
#' @param target_fwd_name A \code{character} indicating the target variable's name.
#' @param selected_full_data_corrected_positions_m_refit_clean A cleaned meta-dataframe for refitting the model.
#' @param selected_features_corrected_positions_m_refit A matrix or dataframe containing the features for model refitting.
#' @param target_m_refit A vector containing the target variable for model refitting.
#' @param custom_objective_translated A \code{character} specifying the custom objective function for optimization.
#' @param huber_delta A numeric value specifying the delta parameter for Huber loss (used in XGBoost and NN).
#' @param quantile_tau A numeric value specifying the quantile level (used in custom objectives).
#' @param early_stop A numeric value specifying the early stopping criteria (if applicable).
#' @param keras_architecture_parameters A list containing Keras neural network architecture specifications.
#' @param optimal_hyper A named list of optimal hyperparameters for the specified \code{sb_algorithm}.
#' @param chosen_eval_metric_translated A \code{character} specifying the evaluation metric for validation.
#' @param most_recent_signal_universe_m_d_ref A meta-dataframe representing the most recent signal universe.
#' @param selected_backtest_returns_corrected_positions_m_xts_upd_ref An \code{xts} object containing backtested returns for corrected positions.
#' @param selected_cov_matrix_benchmark_m_xts_upd_ref An \code{xts} object representing the selected market factor proxy.
#' @param cov_matrix_sample_size A numeric value specifying the sample size for covariance matrix estimation.
#' @param cov_estimation_method A \code{character} specifying the method for covariance estimation (e.g., \code{"sample"}).
#' @param active_returns A logical value indicating whether to use active returns (default: \code{TRUE}).
#' @param rp_method A \code{character} specifying the method for Risk Parity optimization.
#' @param n_random_ports A numeric value specifying the number of random portfolios to generate (for MVO).
#' @param random_ports_method A \code{character} specifying the method for generating random portfolios.
#' @param opt_objective A \code{character} specifying the optimization objective (e.g., \code{"sharpe"}).
#' @param opt_method A \code{character} specifying the optimization method (e.g., \code{"random"}).
#' @param concentration_constraint_policy A policy object defining concentration constraints.
#' @param upper_quantile_winsorization A numeric value specifying the upper winsorization quantile.
#' @param lower_quantile_winsorization A numeric value specifying the lower winsorization quantile.
#' @param verbose A logical value indicating whether to enable verbose output during model training.
#'
#' @return An S4 object of class \code{sb_model}, encapsulating the trained model, algorithm, and associated metadata.
#' @export
fit_sb_model <- function(sb_algorithm, #SB Algorithm
                         target_fwd_name,  selected_features_corrected_positions_m_refit, target_m_refit,
                         selected_full_data_corrected_positions_m_refit_clean = NULL, #Data
                         custom_objective_translated, huber_delta, quantile_tau, early_stop, keras_architecture_parameters, #Model Parameters
                         optimal_hyper = NULL, chosen_eval_metric_translated, #Validation Parameters
                         most_recent_signal_universe_m_d_ref, most_recent_custom_signal_weights_m_d_ref = NULL, selected_backtest_returns_corrected_positions_m_xts_upd_ref, #Signal Universe
                         cov_matrix_sample_size = 36, cov_estimation_method = "sample", active_returns = TRUE, selected_cov_matrix_benchmark_m_xts_upd_ref, groups_m_d_ref, #COV (for RP and MVO)
                         rp_method = "cyclical-spinu", n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random", #RP/MVO Methods
                         concentration_constraint_policy, #Concentration Constraint
                         upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose){ #MISC


  ###Define the heuristic_sb_metric for optimization in heuristic portfolios
  ######################
  if(sb_algorithm %in% c("sw", "mvo")){
    ###Identify objective (min or max) and heuristic metric
    objective <- ifelse(stringr::str_detect(custom_objective_translated, "max_"), "max", "min")
    heuristic_sb_metric <- most_recent_signal_universe_m_d_ref %>% dplyr::pull(stringr::str_remove(custom_objective_translated, paste0(objective, "_")))

    ###Calculate exp ret score base on user choice for custom objective
    most_recent_signal_universe_m_d_ref[, "exp_ret_score"] <- signal_transform(
      if(objective == "max") heuristic_sb_metric else heuristic_sb_metric*(-1),
      upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
    )
  }
  ######################

  ###Fit sb model based on sb_algorithm
  ######################
  sb_model <- switch(sb_algorithm,
                     ##OLS
                     ols = stats::lm(paste(target_fwd_name,'~.'), data = selected_full_data_corrected_positions_m_refit_clean),

                     ##GLMNET
                     glmnet::glmnet(selected_features_corrected_positions_m_refit[,-c(1:3)], target_m_refit, #Features and target
                                    #Hyperparameters
                                    alpha = optimal_hyper["alpha"],
                                    lambda.min.ratio = optimal_hyper["lambda.min.ratio"],
                                    verbose = verbose
                     ),

                     ##Ranger
                     rf = ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(selected_full_data_corrected_positions_m_refit_clean), #Features and target
                                         #Hyperparameters
                                         mtry = optimal_hyper["mtry"] * (ncol(selected_full_data_corrected_positions_m_refit_clean) - 1),
                                         num.trees = optimal_hyper["num.trees"],
                                         max.depth = optimal_hyper["max.depth"],
                                         min.bucket = optimal_hyper["min.bucket"],
                                         verbose = verbose
                     ),
                     ##XGB
                     xgb = xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(selected_features_corrected_positions_m_refit[,-c(1:3)]), #Features and target
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
                     nn = fit_keras_model(features_matrix_train_clean = selected_features_corrected_positions_m_refit[,-c(1:3)], #Feature
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

                     ##Custom Weights
                     custom_weights = set_portfolio_weights(port_construction_method = "custom_weights",
                                                            universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                            custom_weights_m_d_ref = most_recent_custom_signal_weights_m_d_ref),

                     ##Equal-Weighted Signals
                     ew = set_portfolio_weights(port_construction_method = "ew",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref), #Universe of signals

                     ##Signal-Weighted Signals
                     sw = set_portfolio_weights(port_construction_method = "sw",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref), #Universe of signals

                     ##Risk-Parity
                     rp = set_portfolio_weights(port_construction_method = "rp",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                cov_matrix_sample_size = cov_matrix_sample_size,
                                                cov_estimation_method = cov_estimation_method,
                                                active_returns = active_returns,
                                                groups_m_d_ref = groups_m_d_ref,
                                                rp_method = rp_method
                     ),
                     ##MVO
                     mvo = set_portfolio_weights(port_construction_method = "mvo",
                                                 universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                 returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                 selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                 cov_matrix_sample_size = cov_matrix_sample_size,
                                                 cov_estimation_method = cov_estimation_method,
                                                 active_returns = active_returns,
                                                 groups_m_d_ref = groups_m_d_ref,
                                                 random_ports_method = random_ports_method,
                                                 n_random_ports = n_random_ports,
                                                 opt_objective = opt_objective,
                                                 opt_method = opt_method,
                                                 concentration_constraint_policy = concentration_constraint_policy
                     )

  )
  ######################

  ###Transform port_obj into signal_port
  ######################
  if(sb_algorithm %in% c("ew", "sw", "rp", "mvo","custom_weights")){
    sb_model <- new( # Convert port_obj to signal_port
      "signal_port",
      universe_m_d_ref = sb_model@universe_m_d_ref,
      port_construction_method = sb_model@port_construction_method,
      eligible_assets = sb_model@eligible_assets,
      exp_ret_score = sb_model@exp_ret_score,
      covariance_matrix = sb_model@covariance_matrix,
      correlation_matrix = sb_model@correlation_matrix,
      weights = sb_model@weights,
      rel_risk_contr = sb_model@rel_risk_contr,
      mvo_port_spec = sb_model@mvo_port_spec,
      random_port_weights = sb_model@random_port_weights,
      ind_max_weights = sb_model@ind_max_weights,
      ind_min_weights = sb_model@ind_min_weights,
      groups = sb_model@groups,
      port_name = sb_model@port_name,
      heuristic_sb_metric = if (sb_model@port_construction_method %in% c("sw", "mvo")) custom_objective_translated else NULL
    )
  }


  ######################

  ###Create S4 Sb Model Object
  #Create S4 Object
  if(!sb_algorithm == "custom_weights"){
    eligible_signals <- most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  } else {
    eligible_signals <- most_recent_custom_signal_weights_m_d_ref %>% dplyr::filter(weights > 0) %>% dplyr::pull(tickers)
  }


  sb_model_fit <- new("sb_model",
                      model = sb_model,
                      eligible_signals = eligible_signals,
                      model_class = class(sb_model),
                      sb_algorithm = sb_algorithm,
                      best_hyperparameters = if(sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights")) NULL else optimal_hyper,
                      custom_objective = custom_objective_translated,
                      huber_delta = huber_delta,
                      keras_architecture_parameters = keras_architecture_parameters
  )

}
