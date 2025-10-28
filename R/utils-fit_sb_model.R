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
#' @param most_recent_custom_signal_weights_m_d_ref A meta-dataframe containing custom signal weights.
#' @param selected_backtest_returns_corrected_positions_m_xts_upd_ref An \code{xts} object containing backtested returns for corrected positions.
#' @param selected_cov_matrix_benchmark_m_xts_upd_ref An \code{xts} object representing the selected market factor proxy.
#' @param cov_matrix_sample_size A numeric value specifying the sample size for covariance matrix estimation.
#' @param cov_estimation_method A \code{character} specifying the method for covariance estimation (e.g., \code{"sample"}).
#' @param active_returns A logical value indicating whether to use active returns (default: \code{TRUE}).
#' @param groups_m_d_ref A meta-dataframe containing group information for the assets.
#' @param bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref A 'xts' object containg returns data for selected_benchmark. Only used in heuristics methods
#' @param selected_benchmark A character vector indicating the selected benchmark tickers. Only used in heuristics methods
#' @param rp_method A \code{character} specifying the method for Risk Parity optimization.
#' @param exp_ret_score_tilt Character argument specififying whether tilt must be applied during of after risk-parity weights
#' @param exp_ret_score_tilt_eta Numeric. The intensity of the tilt effect when using `exp_ret_score_tilt`. Higher values increase the tilt effect.
#' @param linkage Character. Linkage method for hierarchical clustering in Risk Parity. Defaults to `"single"`.
#' @param n_random_ports A numeric value specifying the number of random portfolios to generate (for MVO).
#' @param random_ports_method A \code{character} specifying the method for generating random portfolios.
#' @param ridge_pen Numeric. Ridge penalty for MVO optimization to improve numerical stability. Defaults to `NULL`.
#' @param target_port_m_d_ref Optional. A data frame containing columns for id, tickers, dates, and target portfolio weights.
#' @param opt_objective A \code{character} specifying the optimization objective (e.g., \code{"sharpe"}).
#' @param opt_method A \code{character} specifying the optimization method (e.g., \code{"random"}).
#' @param n_resamples Number of resamples for resampled MVO. Defaults to \code{0}.
#' @param exp_ret_score_jitter Numeric. Standard deviation of jitter added to expected return
#' scores during resampling. Defaults to \code{0}.
#' @param cov_eigval_jitter Numeric. Standard deviation of jitter added to covariance
#' matrix eigenvalues during resampling. Defaults to \code{0}.
#' @param concentration_constraint_policy A policy object defining concentration constraints.
#' @param mmaf_method Character. Method for Micro-Macro Allocation Framework. Options are `"bottom_up"` or `"top_down"`. Defaults to `"bottom_up"`.
#' @param top_down_proxy_port_method Character. Method for constructing the top-down proxy portfolio in MMAF. Options are `"ew"`, `"sw"`, `"rp"`, or `"mvo"`.
#' @param mmaf_group_col Character. Column name in `groups_m_d_ref` used to define groups for MMAF.
#' @param micro_port_construction_method Character micro method used to allocate within groups (e.g., `"ew"`, `"rp"`, `"hrp"`, `"mvo"`).
#' @param macro_port_construction_method Character macro method used to allocate across groups
#'   (e.g., `"ew"`, `"rp"`, `"hrp"`, `"mvo"`). For a strictly *neutral* top-down
#'   sector allocation, prefer `"ew"`, `"rp"` or `"hrp"` and keep `macro_exp_ret_score_tilt = NULL`.
#' @param macro_linkage Character, passed to macro hierarchical methods when applicable.
#' @param macro_concentration_constraint_policy Optional list with group-level weight caps.
#' @param macro_n_random_ports Integer, number of random portfolios at macro level.
#' @param macro_random_ports_method Character, sampling method (macro).
#' @param macro_opt_objective Character, optimization objective at macro.
#' @param macro_opt_method Character, optimizer selector at macro.
#' @param macro_ridge_pen Numeric or `NULL`, ridge penalty for macro MVO.
#' @param macro_n_resamples Integer, number of resamples for macro MVO.
#' @param macro_exp_ret_score_jitter Numeric, jitter on sector ER (macro MVO).
#' @param macro_cov_eigval_jitter Numeric, jitter on macro covariance eigenvalues.
#' @param macro_rp_method Character, risk parity method at macro.
#' @param macro_exp_ret_score_tilt Optional numeric vector/column name for RP tilt (macro).
#' @param macro_exp_ret_score_tilt_eta Optional numeric, tilt intensity for macro RP.
#' @param upper_quantile_winsorization A numeric value specifying the upper winsorization quantile.
#' @param lower_quantile_winsorization A numeric value specifying the lower winsorization quantile.
#' @param verbose A logical value indicating whether to enable verbose output during model training.
#'
#' @return An S4 object of class \code{sb_model}, encapsulating the trained model, algorithm, and associated metadata.
fit_sb_model <- function(sb_algorithm, #SB Algorithm
                         target_fwd_name,  selected_features_corrected_positions_m_refit, target_m_refit,
                         selected_full_data_corrected_positions_m_refit_clean = NULL, #Data
                         custom_objective_translated, huber_delta, quantile_tau, early_stop, keras_architecture_parameters, #Model Parameters
                         optimal_hyper = NULL, chosen_eval_metric_translated, #Validation Parameters
                         most_recent_signal_universe_m_d_ref, most_recent_custom_signal_weights_m_d_ref = NULL, selected_backtest_returns_corrected_positions_m_xts_upd_ref, #Signal Universe
                         cov_matrix_sample_size = 36, cov_estimation_method = "sample", active_returns = TRUE, selected_cov_matrix_benchmark_m_xts_upd_ref, groups_m_d_ref, #COV (for RP and MVO)
                         bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref = NULL, selected_benchmark = NULL,
                         rp_method = "cyclical-spinu", exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL, linkage = "single", #RP/HRP
                         n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random", ridge_pen = NULL,
                         n_resamples = 0, exp_ret_score_jitter = 0, cov_eigval_jitter = 0, target_port_m_d_ref = NULL,#MVO Methods
                         concentration_constraint_policy = NULL, #Concentration Constraint
                         ## MMAF
                         mmaf_method = "bottom_up", top_down_proxy_port_method, mmaf_group_col,
                         micro_port_construction_method = NULL, #Micro portfolio construction method
                         macro_port_construction_method = NULL, macro_concentration_constraint_policy = NULL,
                         macro_n_random_ports = 2000, macro_random_ports_method = "sample",
                         macro_opt_objective = "sharpe", macro_opt_method = "random", macro_ridge_pen = NULL,
                         macro_n_resamples = 0, macro_exp_ret_score_jitter = 0, macro_cov_eigval_jitter = 0,
                         macro_rp_method = "cyclical-spinu", macro_exp_ret_score_tilt = NULL,  macro_exp_ret_score_tilt_eta = NULL,
                         macro_linkage = "single",
                         upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose){ #MISC


  ###Modify signal universe to append needed cols for heuristic methods a la classify_investment_universe
  ######################
    ####Add target weights if MVO, ridge_pen and target_port_m_d_ref not NULL
    if ((sb_algorithm == "mvo") ||
        (sb_algorithm == "mmaf" && (micro_port_construction_method == "mvo" || macro_port_construction_method == "mvo"))) {
      #### If ridge_pen and target_port_m_d_ref are not NULL
      if (!is.null(ridge_pen) && !is.null(target_port_m_d_ref)) {
        ##### Defensive checks
        if (!is.null(target_port_m_d_ref) && length(unique(dplyr::pull(target_port_m_d_ref, dates))) != 1){
          stop("target_port_m_d_ref should have only one date")
        }
        if (!all(c("id", "tickers", "dates", "target_weights") == colnames(target_port_m_d_ref))) {
          stop("target_port_m_d_ref must contain columns: id, tickers, dates, target_weights")
        }
        most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref %>%
          dplyr::left_join(target_port_m_d_ref %>% dplyr::select(-id, -dates), by = "tickers")
        ##### If there is a 'exp_ret_score' column, position target_weights before it
        if ("exp_ret_score" %in% colnames(most_recent_signal_universe_m_d_ref)) {
          most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref %>%
            dplyr::relocate(target_weights, .before = exp_ret_score)
        }
      }
    }
    #### Define the heuristic_sb_metric for optimization in heuristic portfolios
    if (
      #### First condition: sw or mvo
      (sb_algorithm %in% c("sw", "mvo")) ||
      #### Second condition: rp or hrp with exp_ret_score_tilt
      ((sb_algorithm %in% c("rp", "hrp") && (!is.null(exp_ret_score_tilt)) && (!is.null(exp_ret_score_tilt_eta)))) ||
      #### Third condition: mmaf
      (sb_algorithm == "mmaf")
      ){

      ###Creates object to identify need of exp_ret_score
      needs_exp_ret_score <- TRUE

      ###Identify objective (min or max) and heuristic metric
      objective <- ifelse(stringr::str_detect(custom_objective_translated, "max_"), "max", "min")
      heuristic_sb_metric <- most_recent_signal_universe_m_d_ref %>% dplyr::pull(stringr::str_remove(custom_objective_translated, paste0(objective, "_")))

      ###Calculate exp ret score base on user choice for custom objective
      most_recent_signal_universe_m_d_ref[, "exp_ret_score"] <- signal_transform(
        if(objective == "max") heuristic_sb_metric else heuristic_sb_metric*(-1),
        upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
      )
    } else {
        needs_exp_ret_score <- FALSE
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
                                          custom_objective_translated = custom_objective_translated, #No need for switch
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
                                                            custom_weights_m_d_ref = most_recent_custom_signal_weights_m_d_ref,
                                                            eligible_returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                            selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                            cov_matrix_sample_size = cov_matrix_sample_size,
                                                            cov_estimation_method = cov_estimation_method,
                                                            active_returns = active_returns,
                                                            groups_m_d_ref = groups_m_d_ref,
                                                            bench_assets_returns_m_xts_upd_ref = bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                            selected_benchmark = selected_benchmark
                                                            ),

                     ##Equal-Weighted Signals
                     ew = set_portfolio_weights(port_construction_method = "ew",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                eligible_returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                cov_matrix_sample_size = cov_matrix_sample_size,
                                                cov_estimation_method = cov_estimation_method,
                                                active_returns = active_returns,
                                                groups_m_d_ref = groups_m_d_ref,
                                                bench_assets_returns_m_xts_upd_ref = bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                selected_benchmark = selected_benchmark
                                                ),

                     ##Signal-Weighted Signals
                     sw = set_portfolio_weights(port_construction_method = "sw",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                eligible_returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                cov_matrix_sample_size = cov_matrix_sample_size,
                                                cov_estimation_method = cov_estimation_method,
                                                active_returns = active_returns,
                                                groups_m_d_ref = groups_m_d_ref,
                                                bench_assets_returns_m_xts_upd_ref = bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                selected_benchmark = selected_benchmark
                                                ),

                     ##Risk-Parity
                     rp = set_portfolio_weights(port_construction_method = "rp",
                                                universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                eligible_returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                cov_matrix_sample_size = cov_matrix_sample_size,
                                                cov_estimation_method = cov_estimation_method,
                                                exp_ret_score_tilt = exp_ret_score_tilt,
                                                exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                                active_returns = active_returns,
                                                groups_m_d_ref = groups_m_d_ref,
                                                rp_method = rp_method,
                                                concentration_constraint_policy = concentration_constraint_policy,
                                                bench_assets_returns_m_xts_upd_ref = bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                selected_benchmark = selected_benchmark
                     ),
                     ## Hierarchical Risk Parity
                     hrp = set_portfolio_weights(port_construction_method = "hrp",
                                                 universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                 eligible_returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                 selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                 cov_matrix_sample_size = cov_matrix_sample_size,
                                                 cov_estimation_method = cov_estimation_method,
                                                 exp_ret_score_tilt = exp_ret_score_tilt,
                                                 exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                                 active_returns = active_returns,
                                                 groups_m_d_ref = groups_m_d_ref,
                                                 linkage = linkage,
                                                 bench_assets_returns_m_xts_upd_ref = bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                 selected_benchmark = selected_benchmark
                     ),
                     ##MVO
                     mvo = set_portfolio_weights(port_construction_method = "mvo",
                                                 universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                 eligible_returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                 selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                 cov_matrix_sample_size = cov_matrix_sample_size,
                                                 cov_estimation_method = cov_estimation_method,
                                                 active_returns = active_returns,
                                                 groups_m_d_ref = groups_m_d_ref,
                                                 random_ports_method = random_ports_method,
                                                 n_random_ports = n_random_ports,
                                                 opt_objective = opt_objective,
                                                 opt_method = opt_method,
                                                 ridge_pen = ridge_pen,
                                                 n_resamples = n_resamples,
                                                 exp_ret_score_jitter = exp_ret_score_jitter,
                                                 cov_eigval_jitter = cov_eigval_jitter,
                                                 concentration_constraint_policy = concentration_constraint_policy,
                                                 bench_assets_returns_m_xts_upd_ref = bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                 selected_benchmark = selected_benchmark
                     ),
                     ##MMAF
                     mmaf = set_portfolio_weights(port_construction_method = "mmaf",
                                                  universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                                  eligible_returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                  selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                                  cov_matrix_sample_size = cov_matrix_sample_size,
                                                  cov_estimation_method = cov_estimation_method,
                                                  active_returns = active_returns,
                                                  groups_m_d_ref = groups_m_d_ref,
                                                  mmaf_group_col = mmaf_group_col,
                                                  mmaf_method = mmaf_method,
                                                  top_down_proxy_port_method = top_down_proxy_port_method,
                                                  micro_port_construction_method = micro_port_construction_method,
                                                  macro_port_construction_method = macro_port_construction_method,
                                                  #Micro parameters
                                                  random_ports_method = random_ports_method,
                                                  n_random_ports = n_random_ports,
                                                  opt_objective = opt_objective,
                                                  opt_method = opt_method,
                                                  ridge_pen = ridge_pen,
                                                  n_resamples = n_resamples,
                                                  exp_ret_score_jitter = exp_ret_score_jitter,
                                                  cov_eigval_jitter = cov_eigval_jitter,
                                                  concentration_constraint_policy = concentration_constraint_policy,
                                                  rp_method = rp_method,
                                                  linkage = linkage,
                                                  exp_ret_score_tilt = exp_ret_score_tilt,
                                                  exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                                  #Macro parameters
                                                  macro_concentration_constraint_policy = macro_concentration_constraint_policy,
                                                  macro_cap_weighting_metric = macro_cap_weighting_metric,
                                                  macro_n_random_ports = macro_n_random_ports,
                                                  macro_random_ports_method = macro_random_ports_method,
                                                  macro_opt_objective = macro_opt_objective,
                                                  macro_opt_method = macro_opt_method,
                                                  macro_ridge_pen = macro_ridge_pen,
                                                  macro_n_resamples = macro_n_resamples,
                                                  macro_exp_ret_score_jitter = macro_exp_ret_score_jitter,
                                                  macro_cov_eigval_jitter = macro_cov_eigval_jitter,
                                                  macro_rp_method = macro_rp_method,
                                                  macro_exp_ret_score_tilt = macro_exp_ret_score_tilt,
                                                  macro_exp_ret_score_tilt_eta = macro_exp_ret_score_tilt_eta,
                                                  macro_linkage = macro_linkage,
                                                  bench_assets_returns_m_xts_upd_ref = bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref,
                                                  selected_benchmark = selected_benchmark
                     )

  )
  ######################

  ###Transform port_obj into signal_port
  ######################
  if(sb_algorithm %in% c("ew", "sw", "rp", "hrp", "mvo", "mmaf", "custom_weights")){
    sb_model <- methods::new( # Convert port_obj to signal_port
      "signal_port",
      universe_m_d_ref = sb_model@universe_m_d_ref,
      port_construction_method = sb_model@port_construction_method,
      eligible_assets = sb_model@eligible_assets,
      exp_ret_score = sb_model@exp_ret_score,
      covariance_matrix = sb_model@covariance_matrix,
      correlation_matrix = sb_model@correlation_matrix,
      weights = sb_model@weights,
      rel_risk_contr = sb_model@rel_risk_contr,
      clusters = sb_model@clusters,
      mvo_port_spec = sb_model@mvo_port_spec,
      random_port_weights = sb_model@random_port_weights,
      ind_max_weights = sb_model@ind_max_weights,
      ind_min_weights = sb_model@ind_min_weights,
      groups = sb_model@groups,
      mmaf_method = sb_model@mmaf_method,
      mmaf_group_col = sb_model@mmaf_group_col,
      group_cov_matrix = sb_model@group_cov_matrix,
      micro = sb_model@micro,
      macro = sb_model@macro,
      port_stats = sb_model@port_stats,
      port_name = sb_model@port_name,
      heuristic_sb_metric = if (needs_exp_ret_score) custom_objective_translated else NULL
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


  sb_model_fit <- methods::new("sb_model",
                                model = sb_model,
                                eligible_signals = eligible_signals,
                                model_class = class(sb_model),
                                sb_algorithm = sb_algorithm,
                                best_hyperparameters = if(sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights")) NULL else optimal_hyper,
                                custom_objective = custom_objective_translated,
                                huber_delta = huber_delta,
                                keras_architecture_parameters = keras_architecture_parameters
  )

  return(sb_model_fit)

}
