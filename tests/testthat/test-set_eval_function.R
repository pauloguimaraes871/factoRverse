#Integration tests for hyperparameter tuning
test_that("set_eval_function correctly sets a glmnet model", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "glmnet"
  tuning_method = "grid_search"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1
  quantile_tau = 0.5
  early_stop <- NULL
  n_iter <- NULL
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(alpha = c(0.1, 1), lambda.min.ratio = c(0.1, 0.2))
  keras_architecture_parameters <- list(units = NULL, n_layers = NULL, activation = NULL, batch_norm_option = NULL, nn_optimizer = NULL)
  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL


  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )

  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  ml_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                              features_validation_sample = ts_splits$validation$features_validation_sample,
                              target_validation_sample = ts_splits$validation$target_validation_sample,
                              target_fwd_name = target_fwd_name, #User defined
                              sb_algorithm = sb_algorithm, #User defined
                              tuning_method = tuning_method, #User defined
                              chosen_eval_metric_translated = chosen_eval_metric_translated,
                              chosen_eval_metric = chosen_eval_metric, #User defined
                              huber_delta = huber_delta,
                              quantile_tau = quantile_tau,
                              early_stop = early_stop,
                              custom_objective = custom_objective_translated,
                              alpha = hyperparameters_grid$alpha[1],
                              lambda = hyperparameters_grid$lambda[1],
                              verbose = TRUE,
                              return_all_info = TRUE
  )



  #Check if set_eval_function correctly sets a ranger model
  check_if_ml_algo_is_used <- all(class(ml_model_fit_results$ml_model) == c("elnet", "glmnet"))
  expect_true(check_if_ml_algo_is_used)


  #Check if all features are used
  check_number_of_features_used <- ml_model_fit_results$ml_model$dim[1] == ncol(ts_splits$training$full_data_training_sample_clean) - 1
  expect_true(check_number_of_features_used)


  #Checks with another glmnet model
  set.seed(123)
  benchmark_model <- glmnet::glmnet(as.matrix(ts_splits$training$features_training_sample[,-c(1:3)]), #train matrix
                                    ts_splits$training$target_training_sample, #target vector
                                    alpha = hyperparameters_grid$alpha[1], #alpha hyperparameter
                                    lambda.min.ratio = hyperparameters_grid$lambda.min.ratio[1])



  benchmark_model$call <- NULL
  ml_model_fit_results$ml_model$call <- NULL

  expect_equal(ml_model_fit_results$ml_model,benchmark_model)

  #Check that best_lam is correctly set
  benchmark_predict <- predict(benchmark_model, newx = as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]))
  scores <- vector(length = ncol(benchmark_predict))
  for(i in 1:ncol(benchmark_predict)){
    scores[i] <- calculate_eval_metrics(pred = benchmark_predict[,i], target = ts_splits$validation$target_validation_sample,
                           huber_delta = huber_delta, quantile_tau = quantile_tau, chosen_eval_metric = chosen_eval_metric)$Score
  }
  expect_equal(ml_model_fit_results$best_lam,
               benchmark_model$lambda[which.max(scores)])

  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made1 <-
    all(ml_model_fit_results$pred == as.numeric(predict(ml_model_fit_results$ml_model,
                                                        newx = as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]),
                                                s = ml_model_fit_results$best_lam)))

  expect_true(check_if_predictions_are_correctly_made1)

  check_if_predictions_are_correctly_made2 <-
    as.numeric(ml_model_fit_results$pred) - as.numeric(predict(glmnet::glmnet(as.matrix(ts_splits$training$features_training_sample[,-c(1:3)]), #train matrix
                                                                       ts_splits$training$target_training_sample, #target vector
                                                                       alpha = hyperparameters_grid$alpha[1], #alpha hyperparameter
                                                                       lambda = benchmark_model$lambda[which.max(scores)]),
                                                        newx = as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)])
                                                        ))


  expect_equal(mean(check_if_predictions_are_correctly_made2), 0, tolerance = 1e-3)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(ml_model_fit_results$df_eval_metrics == calculate_eval_metrics(pred = ml_model_fit_results$pred,
                                                                                                            target = ts_splits$validation$target_validation_sample,
                                                                                                            huber_delta = huber_delta,
                                                                                                            quantile_tau = quantile_tau,
                                                                                                            chosen_eval_metric = chosen_eval_metric))

  expect_true(check_if_metrics_are_correctly_made)


})

test_that("set_eval_function correctly sets a ranger model", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "rf"
  tuning_method = "grid_search"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1
  quantile_tau = 0.5
  early_stop <- NULL
  n_iter <- NULL
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(mtry = c(0.1, 1), num.trees = c(200, 500),
                                 max.depth = c(2), min.bucket = c(1, 10,15))
  keras_architecture_parameters <- list(units = NULL, n_layers = NULL, activation = NULL, batch_norm_option = NULL, nn_optimizer = NULL)
  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL


  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )

  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                    dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                    split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  sb_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                              features_validation_sample = ts_splits$validation$features_validation_sample,
                              target_validation_sample = ts_splits$validation$target_validation_sample,
                              target_fwd_name = target_fwd_name, #User defined
                              sb_algorithm = sb_algorithm, #User defined
                              tuning_method = tuning_method, #User defined
                              chosen_eval_metric_translated = chosen_eval_metric_translated,
                              chosen_eval_metric = chosen_eval_metric, #User defined
                              huber_delta = huber_delta,
                              quantile_tau = quantile_tau,
                              early_stop = early_stop,
                              custom_objective = custom_objective_translated,
                              max.depth = hyperparameters_grid$max.depth[1],
                              num.trees = hyperparameters_grid$num.trees[1],
                              mtry = hyperparameters_grid$mtry[1],
                              min.bucket = hyperparameters_grid$min.bucket[1],
                              verbose = TRUE,
                              return_all_info = TRUE
                              )



  #Check if set_eval_function correctly sets a ranger model
  check_if_sb_algo_is_used <- class(sb_model_fit_results$ml_model) == "ranger"
  expect_true(check_if_sb_algo_is_used)

  #Check if hyperparameters are correctly set
  check_if_hyperparameters_have_been_applied <-
    all(c(hyperparameters_grid$num.trees[1] == sb_model_fit_results$ml_model$num.trees,
          as.integer(hyperparameters_grid$mtry[1]*sb_model_fit_results$ml_model$num.independent.variables) == sb_model_fit_results$ml_model$mtry))
  expect_true(check_if_hyperparameters_have_been_applied)


  #Check if all features are used
  check_number_of_features_used <- sb_model_fit_results$ml_model$num.independent.variables == ncol(ts_splits$training$full_data_training_sample_clean) - 1
  expect_true(check_number_of_features_used)

  #Check if dependent_variable is adequately set
  check_dependent_variable_used <- sb_model_fit_results$ml_model$dependent.variable.name == target_fwd_name
  expect_true(check_dependent_variable_used)

  #Checks with another ranger model
  set.seed(123)
  benchmark_model <- ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(ts_splits$training$full_data_training_sample_clean),
                                      mtry =  hyperparameters_grid$mtry[1] * (ncol(ts_splits$training$full_data_training_sample_clean)-1),
                                      max.depth = hyperparameters_grid$max.depth[1],
                                      num.trees = hyperparameters_grid$num.trees[1])

  benchmark_model$call <- NULL
  sb_model_fit_results$ml_model$call <- NULL

  expect_equal(sb_model_fit_results$ml_model,benchmark_model)


  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made <-
    all(sb_model_fit_results$pred == as.numeric(predict(sb_model_fit_results$ml_model, data = janitor::clean_names(ts_splits$validation$features_validation_sample[,-c(1:3)]))$predictions))

  expect_true(check_if_predictions_are_correctly_made)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(sb_model_fit_results$df_eval_metrics == calculate_eval_metrics(pred = sb_model_fit_results$pred,
                                                                                                        target = ts_splits$validation$target_validation_sample,
                                                                                                        huber_delta = huber_delta,
                                                                                                        quantile_tau = quantile_tau,
                                                                                                        chosen_eval_metric = chosen_eval_metric))

  expect_true(check_if_metrics_are_correctly_made)


})

test_that("set_eval_function correctly sets a xgboost model (custom_objective = squared_error) and no early_stop", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "xgb"
  tuning_method = "grid_search"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1
  quantile_tau = 0.5
  early_stop <- NULL
  n_iter <- NULL
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(min_child_weight = c(2), max_depth = c(2), subsample = c(1), colsample_bytree = c(0.5, 0.8), eta = c(0.02),
                                 alpha = c(1), gamma = c(0), nrounds = c(50))
  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL


  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )

  keras_architecture_parameters <- list(units = NULL, n_layers = NULL, activation = NULL, batch_norm_option = NULL, nn_optimizer = NULL)


  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric
  chosen_eval_metric_translated<- adjusted_metrics$chosen_eval_metric_translated

  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  suppressWarnings(
  sb_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                              features_validation_sample = ts_splits$validation$features_validation_sample,
                              target_validation_sample = ts_splits$validation$target_validation_sample,
                              target_fwd_name = target_fwd_name, #User defined
                              huber_delta = huber_delta,
                              quantile_tau = quantile_tau,
                              early_stop = early_stop,
                              custom_objective_translated = custom_objective_translated,
                              chosen_eval_metric = chosen_eval_metric,
                              chosen_eval_metric_translated = chosen_eval_metric_translated,
                              min_child_weight = hyperparameters_grid$min_child_weight[1],
                              max_depth = hyperparameters_grid$max_depth[1],
                              subsample = hyperparameters_grid$subsample[1],
                              colsample_bytree = hyperparameters_grid$colsample_bytree[1],
                              eta = hyperparameters_grid$eta[1],
                              alpha = hyperparameters_grid$alpha[1],
                              gamma = hyperparameters_grid$gamma[1],
                              nrounds = hyperparameters_grid$nrounds[1],
                              verbose = FALSE,
                              return_all_info = TRUE
  )
  )



  #Check if set_eval_function correctly sets a ranger model
  check_if_sb_algo_is_used <- class(sb_model_fit_results$ml_model) == "xgb.Booster"
  expect_true(check_if_sb_algo_is_used)

  #Check if hyperparameters are correctly set
  check_if_hyperparameters_have_been_applied <-
    all(c(hyperparameters_grid$nrounds[1] == sb_model_fit_results$ml_model$niter,
          hyperparameters_grid$eta[1] == sb_model_fit_results$ml_model$params$eta,
          hyperparameters_grid$alpha[1] == sb_model_fit_results$ml_model$params$alpha,
          hyperparameters_grid$gamma[1] == sb_model_fit_results$ml_model$params$gamma,
          hyperparameters_grid$subsample[1] == sb_model_fit_results$ml_model$params$subsample,
          hyperparameters_grid$colsample_bytree[1] == sb_model_fit_results$ml_model$params$colsample_bytree,
          hyperparameters_grid$min_child_weight[1] == sb_model_fit_results$ml_model$params$min_child_weight,
          hyperparameters_grid$max_depth[1] == sb_model_fit_results$ml_model$params$max_depth))


  expect_true(check_if_hyperparameters_have_been_applied)


  #Check if all features are used
  check_features_used <- all(sb_model_fit_results$ml_model$feature_names == colnames(ts_splits$training$full_data_training_sample_clean)[-1])
  expect_true(check_features_used)

  #Check if early stop is correctly
  check_early_stop_usage <- sb_model_fit_results$ml_model$best_iter
  expect_null(check_early_stop_usage)

  #Checks with another xgb model
  set.seed(123)
  suppressWarnings(
  benchmark_model <- xgboost::xgboost(data = xgboost::xgb.DMatrix(data = as.matrix(ts_splits$training$features_training_sample[,-c(1:3)]), label = ts_splits$training$target_training_sample),
                                      eta = hyperparameters_grid$eta[1],
                                      min_child_weight = hyperparameters_grid$min_child_weight[1],
                                      max_depth = hyperparameters_grid$max_depth[1],
                                      nrounds = hyperparameters_grid$nrounds[1],
                                      subsample = hyperparameters_grid$subsample[1],
                                      colsample_bytree = hyperparameters_grid$colsample_bytree[1],
                                      alpha = hyperparameters_grid$alpha[1],
                                      gamma = hyperparameters_grid$gamma[1],
                                      verbose = FALSE
                                      )
  )


  benchmark_model$call <- NULL
  sb_model_fit_results$ml_model$call <- NULL

  expect_equal(sb_model_fit_results$ml_model$raw,benchmark_model$raw)
  expect_equal(sb_model_fit_results$ml_model$evaluation_log$train_rmse,
               benchmark_model$evaluation_log$train_rmse)
  expect_equal(sb_model_fit_results$ml_model$evaluation_log$train_rmse,
               benchmark_model$evaluation_log$train_rmse)


  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made <-
    all(sb_model_fit_results$pred == as.numeric(predict(sb_model_fit_results$ml_model, newdata = as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]))))

  expect_true(check_if_predictions_are_correctly_made)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(sb_model_fit_results$df_eval_metrics == calculate_eval_metrics(pred = sb_model_fit_results$pred,
                                                                                                            target = ts_splits$validation$target_validation_sample,
                                                                                                            huber_delta = huber_delta,
                                                                                                            quantile_tau = quantile_tau,
                                                                                                            chosen_eval_metric = chosen_eval_metric))

  expect_true(check_if_metrics_are_correctly_made)


})

test_that("set_eval_function correctly sets a xgboost model (custom_objective = pseudo_huber_error) and with early_stop", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "xgb"
  tuning_method = "grid_search"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- NULL
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(min_child_weight = c(2), max_depth = c(2), subsample = c(1), colsample_bytree = c(0.5, 0.8), eta = c(0.02),
                                 alpha = c(1), gamma = c(0), nrounds = c(500))

  keras_architecture_parameters <- list(units = NULL, n_layers = NULL, activation = NULL, batch_norm_option = NULL, nn_optimizer = NULL)

  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL


  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )

  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated

  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  suppressWarnings(
    sb_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                chosen_eval_metric = chosen_eval_metric, #For tuning
                                chosen_eval_metric_translated = chosen_eval_metric_translated, #For early stop
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective_translated = custom_objective_translated,
                                min_child_weight = hyperparameters_grid$min_child_weight[1],
                                max_depth = hyperparameters_grid$max_depth[1],
                                subsample = hyperparameters_grid$subsample[1],
                                colsample_bytree = hyperparameters_grid$colsample_bytree[1],
                                eta = hyperparameters_grid$eta[1],
                                alpha = hyperparameters_grid$alpha[1],
                                gamma = hyperparameters_grid$gamma[1],
                                nrounds = hyperparameters_grid$nrounds[1],
                                verbose = FALSE,
                                return_all_info = TRUE
    )
  )



  #Check if set_eval_function correctly sets a ranger model
  check_if_sb_algo_is_used <- class(sb_model_fit_results$ml_model) == "xgb.Booster"
  expect_true(check_if_sb_algo_is_used)

  #Check if hyperparameters are correctly set
  check_if_hyperparameters_have_been_applied <-
    all(c(hyperparameters_grid$eta[1] == sb_model_fit_results$ml_model$params$eta,
          hyperparameters_grid$alpha[1] == sb_model_fit_results$ml_model$params$alpha,
          hyperparameters_grid$gamma[1] == sb_model_fit_results$ml_model$params$gamma,
          hyperparameters_grid$subsample[1] == sb_model_fit_results$ml_model$params$subsample,
          hyperparameters_grid$colsample_bytree[1] == sb_model_fit_results$ml_model$params$colsample_bytree,
          hyperparameters_grid$min_child_weight[1] == sb_model_fit_results$ml_model$params$min_child_weight,
          hyperparameters_grid$max_depth[1] == sb_model_fit_results$ml_model$params$max_depth))


  expect_true(check_if_hyperparameters_have_been_applied)


  #Check if all features are used
  check_features_used <- all(sb_model_fit_results$ml_model$feature_names == colnames(ts_splits$training$full_data_training_sample_clean)[-1])
  expect_true(check_features_used)

  #Check if early stop is correctly implemented
  check_early_stop_usage <- !is.null(sb_model_fit_results$ml_model$best_iter) #Best iter should no be null
  expect_true(check_early_stop_usage)

  check_best_iter_lower_nrounds <- sb_model_fit_results$ml_model$best_iteration < hyperparameters_grid$nrounds[1] #Best iter should be lower than nrounds when nrounds is big
  expect_true(check_best_iter_lower_nrounds)

  check_train_validation_are_different <- all(sb_model_fit_results$ml_model$evaluation_log$validation_mphe != sb_model_fit_results$ml_model$evaluation_log$train_mphe) #Validaiton and train should be diff
  expect_true(check_train_validation_are_different)

  check_early_stop_is_at_lowest_val <- which.min(sb_model_fit_results$ml_model$evaluation_log$validation_mphe) == sb_model_fit_results$ml_model$best_iter #Best iter should min val mphe
  check_best_iter_has_best_score <- sb_model_fit_results$ml_model$best_score == min(sb_model_fit_results$ml_model$evaluation_log$validation_mphe) #Best score is min of val list
  expect_true(check_best_iter_has_best_score)

  check_train_lower_val <- length(which(sb_model_fit_results$ml_model$evaluation_log$train_mphe <= sb_model_fit_results$ml_model$evaluation_log$validation_mphe))/sb_model_fit_results$ml_model$niter
  expect_gt(check_train_lower_val, 0.50) #At least 50% of training eval metric should be lower than validation

  check_best_iter_train_greater_best_iter <- which.min(sb_model_fit_results$ml_model$evaluation_log$train_mphe) >= sb_model_fit_results$ml_model$best_iteration #Lowest train eval should happen after best val
  expect_true(check_best_iter_train_greater_best_iter)


  #Checks with another xgb model
  set.seed(123)
  suppressWarnings(
    benchmark_model <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(ts_splits$training$features_training_sample[,-c(1:3)]), label = ts_splits$training$target_training_sample),
                                        eta = hyperparameters_grid$eta[1],
                                        min_child_weight = hyperparameters_grid$min_child_weight[1],
                                        max_depth = hyperparameters_grid$max_depth[1],
                                        nrounds = hyperparameters_grid$nrounds[1],
                                        subsample = hyperparameters_grid$subsample[1],
                                        colsample_bytree = hyperparameters_grid$colsample_bytree[1],
                                        alpha = hyperparameters_grid$alpha[1],
                                        gamma = hyperparameters_grid$gamma[1],
                                        objective = custom_objective_translated,
                                        eval_metric = chosen_eval_metric_translated,
                                        huber_slope = huber_delta,
                                        early_stopping_rounds = early_stop,
                                        watchlist = list(train = xgboost::xgb.DMatrix(data = as.matrix(ts_splits$training$features_training_sample[,-c(1:3)]),
                                                                                      label = ts_splits$training$target_training_sample),
                                                         validation = xgboost::xgb.DMatrix(data = as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]),
                                                                                           label = ts_splits$validation$target_validation_sample)
                                                         ),

                                        verbose = FALSE
                                        )


    )



  expect_equal(sb_model_fit_results$ml_model$evaluation_log$train_mphe,benchmark_model$evaluation_log$train_mphe)

  expect_equal(sb_model_fit_results$ml_model$evaluation_log$validation_mphe, benchmark_model$evaluation_log$validation_mphe)

  expect_equal(sb_model_fit_results$ml_model$best_iteration, benchmark_model$best_iteration)


  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made <-
    all(sb_model_fit_results$pred == as.numeric(predict(sb_model_fit_results$ml_model, newdata = as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]))))

  expect_true(check_if_predictions_are_correctly_made)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(sb_model_fit_results$df_eval_metrics[1:10] == calculate_eval_metrics(pred = sb_model_fit_results$pred,
                                                                                                            target = ts_splits$validation$target_validation_sample,
                                                                                                            huber_delta = huber_delta,
                                                                                                            quantile_tau = quantile_tau,
                                                                                                            chosen_eval_metric = chosen_eval_metric))

  expect_true(check_if_metrics_are_correctly_made)


})

test_that("set_eval_function correctly sets a keras nn1 model (custom_objective = squared_error) and no early_stop", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "grid_search"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1
  quantile_tau = 0.5
  early_stop <- NULL
  n_iter <- NULL
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(regularizer_l1 = c(2), regularizer_l2 = c(5), droprate = c(0.5), lr = c(0.2), size_of_batch = 512, number_of_epochs = 100)
  keras_architecture_parameters <- list(units = 32, n_layers = 1, activation = 'relu',  nn_optimizer = 'Adam', batch_norm_option = TRUE)

  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL

  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )


  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  tensorflow::set_random_seed(123)
  suppressWarnings(
    sb_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                chosen_eval_metric_translated = chosen_eval_metric_translated,
                                chosen_eval_metric = chosen_eval_metric, #User defined
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective_translated = custom_objective_translated,

                                #Keras infrastructure
                                keras_architecture_parameters = keras_architecture_parameters,

                                #Hyperparameters
                                regularizer_l1 = hyperparameters_grid$regularizer_l1[1],
                                regularizer_l2 = hyperparameters_grid$regularizer_l2[1],
                                droprate = hyperparameters_grid$droprate[1],
                                lr = hyperparameters_grid$lr[1],
                                number_of_epochs = hyperparameters_grid$number_of_epochs[1],
                                size_of_batch = hyperparameters_grid$size_of_batch[1],
                                verbose = FALSE,
                                return_all_info = TRUE
    )
  )





  #Check if set_eval_function correctly sets a nn model
  check_if_sb_algo_is_used <- class(sb_model_fit_results$fit_nn) == "keras_training_history"
  expect_true(check_if_sb_algo_is_used)

  #Check if architecture is correctly set
  config <- sb_model_fit_results$model_nn$get_config()
  compile_config <- sb_model_fit_results$model_nn$get_compile_config()
  config_string <- as.character(keras::get_config(sb_model_fit_results$model_nn))
  #Input shape
  n_feat <- ncol(ts_splits$training$full_data_training_sample_clean) - 1
  check_if_n_feat_is_correct <- config$layers[[1]]$config$batch_input_shape[[2]] == n_feat
  expect_true(check_if_n_feat_is_correct)
  #Activation
  check_if_activation_is_correct <- config$layers[[2]]$config$activation == keras_architecture_parameters$activation
  expect_true(check_if_activation_is_correct)
  #Units
  check_if_units_is_correct <- config$layers[[2]]$config$units == keras_architecture_parameters$units
  expect_true(check_if_units_is_correct)
  #BatchNorm
  check_if_batchnorm_is_on <- config$layers[[3]]$class_name == if(keras_architecture_parameters$batch_norm_option) "BatchNormalization"
  expect_true(check_if_batchnorm_is_on)
  #Optimizer
  check_if_optimizer_is_correct <- compile_config$optimizer$class_name == keras_architecture_parameters$nn_optimizer
  expect_true(check_if_optimizer_is_correct)

  #Check if hyperparametesr are correctly set
  check_if_reg_l1_is_correct <- config$layers[[2]]$config$kernel_regularizer$config$l1 == hyperparameters_grid$regularizer_l1
  expect_true(check_if_reg_l1_is_correct)
  check_if_reg_l2_is_correct <- config$layers[[2]]$config$kernel_regularizer$config$l2 == hyperparameters_grid$regularizer_l2
  expect_true(check_if_reg_l2_is_correct)
  check_if_droprate_is_correct <-  config$layers[[4]]$config$rate == hyperparameters_grid$droprate
  expect_true(check_if_droprate_is_correct)
  check_if_lr_is_correct <- all.equal(compile_config$optimizer$config$learning_rate, as.numeric(hyperparameters_grid$lr))
  expect_true(check_if_lr_is_correct)
  check_if_number_epochs_is_correct <- sb_model_fit_results$fit_nn$params$epochs == hyperparameters_grid$number_of_epochs
  expect_true(check_if_number_epochs_is_correct)
  check_if_batch_size_is_correct <- (hyperparameters_grid$size_of_batch -
    nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps + #Number of samples in each step if training sample was size_of_batch * 2
    nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps) == hyperparameters_grid$size_of_batch
  expect_true(check_if_batch_size_is_correct)

  #Check if custom objective is set
  expect_equal(custom_objective_translated, compile_config$loss)


  #Check if early stop is correctly
  check_early_stop_usage <- sb_model_fit_results$df_eval_metrics$best_iter
  expect_null(check_early_stop_usage)

  #Checks with another nn model
  set.seed(123)
  tensorflow::set_random_seed(123)
  benchmark_model <- keras::keras_model_sequential()
  benchmark_model %>%
    keras::layer_dense(units = keras_architecture_parameters$units[1],
                       activation = keras_architecture_parameters$activation[1],
                       input_shape = ncol(ts_splits$training$features_training_sample[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1], l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%
    keras::layer_dense(units = 1)

  benchmark_model %>% keras::compile(
      loss = custom_objective_translated,
      optimizer = keras::optimizer_adam(learning_rate = hyperparameters_grid$lr[1]),
      metrics = chosen_eval_metric_translated$metric
    )

  benchmark_fit <- benchmark_model %>%
    keras::fit(x = as.matrix(ts_splits$training$full_data_training_sample_clean[,-1]),
               y = ts_splits$training$full_data_training_sample_clean[,1],
               epochs = hyperparameters_grid$number_of_epochs,
               batch_size = hyperparameters_grid$size_of_batch,
               verbose = FALSE)


  #Get configs
  benchmark_config <- benchmark_model$get_config()
  benchmark_compile_config <- benchmark_model$get_compile_config()


  #Input shape
  expect_equal(config$layers[[1]]$config$batch_input_shape[[2]][1], benchmark_config$layers[[1]]$config$batch_input_shape[[2]][1])
  #Units
  expect_equal(config$layers[[2]]$config$units, benchmark_config$layers[[2]]$config$units)
  #Activation
  expect_equal(config$layers[[2]]$config$activation, benchmark_config$layers[[2]]$config$activation)
  #BatchNorm
  expect_equal(config$layers[[3]]$class_name, benchmark_config$layers[[3]]$class_name)
  #Optimizer
  expect_equal(compile_config$optimizer$class_name, benchmark_compile_config$optimizer$class_name)
  #Hyperparameters
  expect_equal(c(config$layers[[2]]$config$kernel_regularizer$config$l1,
                 config$layers[[2]]$config$kernel_regularizer$config$l2,
                 config$layers[[4]]$config$rate,
                 compile_config$optimizer$config$learning_rate,
                 sb_model_fit_results$fit_nn$params$epochs),

              c(benchmark_config$layers[[2]]$config$kernel_regularizer$config$l1,
                benchmark_config$layers[[2]]$config$kernel_regularizer$config$l2,
                benchmark_config$layers[[4]]$config$rate,
                benchmark_compile_config$optimizer$config$learning_rate,
                benchmark_fit$params$epochs)
      )

  #Custom obj
  expect_equal(compile_config$loss, benchmark_compile_config$loss)
  #Metric
  expect_equal(compile_config$metrics, benchmark_compile_config$metrics)

  #Compare loss metrics
  expect_equal(sb_model_fit_results$fit_nn$metrics$loss, benchmark_fit$metrics$loss)


  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made <-
    all(sb_model_fit_results$pred ==
          as.numeric(
            predict(sb_model_fit_results$model_nn,
                    as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]))
            )
        )

  expect_true(check_if_predictions_are_correctly_made)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(sb_model_fit_results$df_eval_metrics == calculate_eval_metrics(pred = sb_model_fit_results$pred,
                                                                                                            target = ts_splits$validation$target_validation_sample,
                                                                                                            huber_delta = huber_delta,
                                                                                                            quantile_tau = quantile_tau,
                                                                                                            chosen_eval_metric = chosen_eval_metric)
                                             )

  expect_true(check_if_metrics_are_correctly_made)


})

test_that("set_eval_function correctly sets a keras nn2 model (custom_objective = squared_error) and early_stop", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "random_search"
  chosen_eval_metric = "mphe"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.5
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(regularizer_l1 = list(distribution_choice = "constant", value = c(1)),
                                 regularizer_l2 = list(distribution_choice = "uniform", pars = c(min = 1, max = 1)),
                                 droprate = list(distribution_choice = "uniform", pars = c(min = 0.5, max = 0.7)),
                                 lr = list(distribution_choice = "uniform", pars = c(min = 0.02, max = 0.02)),
                                 size_of_batch = list(distribution_choice = "constant", value = 512L),
                                 number_of_epochs = list(distribution_choice = "constant", value = 100L))

  keras_architecture_parameters <- list(units = c(32,16), n_layers = 2, activation = c('relu', 'relu'),  nn_optimizer = 'Adam', batch_norm_option = c(TRUE, TRUE))


  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL

  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )


  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  tensorflow::set_random_seed(123)
  suppressWarnings(
    sb_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                chosen_eval_metric_translated = chosen_eval_metric_translated,
                                chosen_eval_metric = chosen_eval_metric, #User defined
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective_translated = custom_objective_translated,

                                #Keras infrastructure
                                keras_architecture_parameters = keras_architecture_parameters,

                                #Hyperparameters
                                regularizer_l1 = hyperparameters_grid$regularizer_l1[1],
                                regularizer_l2 = hyperparameters_grid$regularizer_l2[1],
                                droprate = hyperparameters_grid$droprate[1],
                                lr = hyperparameters_grid$lr[1],
                                number_of_epochs = hyperparameters_grid$number_of_epochs[1],
                                size_of_batch = hyperparameters_grid$size_of_batch[1],
                                verbose = FALSE,
                                return_all_info = TRUE
    )
  )


  #Check if set_eval_function correctly sets a nn model
  check_if_sb_algo_is_used <- class(sb_model_fit_results$fit_nn) == "keras_training_history"
  expect_true(check_if_sb_algo_is_used)

  #Check if architecture is correctly set
  config <- sb_model_fit_results$model_nn$get_config()
  compile_config <- sb_model_fit_results$model_nn$get_compile_config()
  config_string <- as.character(keras::get_config(sb_model_fit_results$model_nn))
  #Input shape
  n_feat <- ncol(ts_splits$training$full_data_training_sample_clean) - 1
  check_if_n_feat_is_correct <- config$layers[[1]]$config$batch_input_shape[[2]] == n_feat
  expect_true(check_if_n_feat_is_correct)
  #Activation
  check_if_activation_is_correct <- all(c(config$layers[[2]]$config$activation, config$layers[[5]]$config$activation) == keras_architecture_parameters$activation)
  expect_true(check_if_activation_is_correct)
  #Units
  check_if_units_is_correct <- all(c(config$layers[[2]]$config$units, config$layers[[5]]$config$units) == keras_architecture_parameters$units)
  expect_true(check_if_units_is_correct)
  #BatchNorm
  check_if_batchnorm_is_on <- all(c(config$layers[[3]]$class_name == if(keras_architecture_parameters$batch_norm_option[1]) "BatchNormalization",
                                    config$layers[[6]]$class_name == if(keras_architecture_parameters$batch_norm_option[2]) "BatchNormalization"))

  expect_true(check_if_batchnorm_is_on)
  #Optimizer
  check_if_optimizer_is_correct <- compile_config$optimizer$class_name == keras_architecture_parameters$nn_optimizer
  expect_true(check_if_optimizer_is_correct)

  #Check if hyperparametesr are correctly set
  check_if_reg_l1_is_correct <- unique(c(config$layers[[2]]$config$kernel_regularizer$config$l1, config$layers[[5]]$config$kernel_regularizer$config$l1)) ==
                                      hyperparameters_grid$regularizer_l1[1]
  expect_true(check_if_reg_l1_is_correct)
  check_if_reg_l2_is_correct <- unique(c(config$layers[[2]]$config$kernel_regularizer$config$l2, config$layers[[5]]$config$kernel_regularizer$config$l2)) ==
                                      hyperparameters_grid$regularizer_l2[1]
  expect_true(check_if_reg_l2_is_correct)
  check_if_droprate_is_correct <- unique(c(config$layers[[4]]$config$rate, config$layers[[7]]$config$rate)) == hyperparameters_grid$droprate[1]
  expect_true(check_if_droprate_is_correct)
  check_if_lr_is_correct <- all.equal(compile_config$optimizer$config$learning_rate, as.numeric(hyperparameters_grid$lr[1]), tolerance = 1e-03)
  expect_true(check_if_lr_is_correct)
  check_if_number_epochs_is_correct <- sb_model_fit_results$fit_nn$params$epochs == hyperparameters_grid$number_of_epochs[1]
  expect_true(check_if_number_epochs_is_correct)
  check_if_batch_size_is_correct <- all((hyperparameters_grid$size_of_batch[1] -
                                       nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps + #Number of samples in each step if training sample was size_of_batch * 2
                                       nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps) == hyperparameters_grid$size_of_batch)
  expect_true(check_if_batch_size_is_correct)



  #Check if custom objective is set
  expect_equal(custom_objective_translated, compile_config$loss)


  #Check if early stop is correctly used
  check_early_stop_usage <- sb_model_fit_results$df_eval_metrics$best_iteration
  expect_true(!is.null(check_early_stop_usage))
  expect_gt(range(sb_model_fit_results$fit_nn$metrics$loss)[2] - range(sb_model_fit_results$fit_nn$metrics$loss)[1],
            range(sb_model_fit_results$fit_nn$metrics$val_loss)[2] - range(sb_model_fit_results$fit_nn$metrics$val_loss)[1]
  )

  expect_gt(range(sb_model_fit_results$fit_nn$metrics$huber_loss)[2] - range(sb_model_fit_results$fit_nn$metrics$huber_loss)[1],
            range(sb_model_fit_results$fit_nn$metrics$val_huber_loss)[2] - range(sb_model_fit_results$fit_nn$metrics$val_huber_loss)[1]
  )
  #Monitored metrics is lower for best iteration
  expect_gt(sb_model_fit_results$fit_nn$metrics[chosen_eval_metric_translated$name][[1]][1],
            sb_model_fit_results$fit_nn$metrics[chosen_eval_metric_translated$name][[1]][sb_model_fit_results$df_eval_metrics$best_iteration])


  #Checks with another nn model
  set.seed(123)
  tensorflow::set_random_seed(123)
  benchmark_model <- keras::keras_model_sequential()
  benchmark_model %>%
    keras::layer_dense(units = keras_architecture_parameters$units[1],
                       activation = keras_architecture_parameters$activation[1],
                       input_shape = ncol(ts_splits$training$features_training_sample[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%
    keras::layer_dense(units = keras_architecture_parameters$units[2],
                       activation = keras_architecture_parameters$activation[2],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])
                       ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%
    keras::layer_dense(units = 1)

  benchmark_model %>% keras::compile(
    loss = custom_objective_translated,
    optimizer = keras::optimizer_adam(learning_rate = hyperparameters_grid$lr[1]),
    metrics = chosen_eval_metric_translated$metric
  )

  benchmark_fit <- benchmark_model %>%
    keras::fit(x = as.matrix(ts_splits$training$full_data_training_sample_clean[,-1]),
               y = ts_splits$training$full_data_training_sample_clean[,1],
               epochs = hyperparameters_grid$number_of_epochs[1],
               batch_size = hyperparameters_grid$size_of_batch[1],
               callbacks = list(callback_early_stopping(monitor = chosen_eval_metric_translated$name,
                                                        patience = early_stop,
                                                        restore_best_weights = TRUE,
                                                        mode = chosen_eval_metric_translated$mode)), #early stop
               validation_data = list(as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]),
                                      ts_splits$validation$target_validation_sample),

               verbose = FALSE)


  #Get configs
  benchmark_config <- benchmark_model$get_config()
  benchmark_compile_config <- benchmark_model$get_compile_config()


  #Input shape
  expect_equal(config$layers[[1]]$config$batch_input_shape[[2]][1], benchmark_config$layers[[1]]$config$batch_input_shape[[2]][1])
  #Units
  expect_equal(c(config$layers[[2]]$config$units, config$layers[[5]]$config$units),
               c(benchmark_config$layers[[2]]$config$units, benchmark_config$layers[[5]]$config$units))
  #Activation
  expect_equal(c(config$layers[[2]]$config$activation, config$layers[[5]]$config$activation),
               c(benchmark_config$layers[[2]]$config$activation, benchmark_config$layers[[5]]$config$activation))
  #BatchNorm
  expect_equal(c(config$layers[[3]]$class_name, config$layers[[6]]$class_name),
               c(benchmark_config$layers[[3]]$class_name, benchmark_config$layers[[6]]$class_name))
  #Optimizer
  expect_equal(compile_config$optimizer$class_name, benchmark_compile_config$optimizer$class_name)
  #Hyperparameters
  expect_equal(c(config$layers[[2]]$config$kernel_regularizer$config$l1,
                 config$layers[[2]]$config$kernel_regularizer$config$l2,
                 config$layers[[4]]$config$rate,
                 config$layers[[5]]$config$kernel_regularizer$config$l1,
                 config$layers[[5]]$config$kernel_regularizer$config$l2,
                 config$layers[[7]]$config$rate,
                 compile_config$optimizer$config$learning_rate,
                 sb_model_fit_results$fit_nn$params$epochs),

               c(benchmark_config$layers[[2]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[2]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[4]]$config$rate,
                 benchmark_config$layers[[5]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[5]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[7]]$config$rate,
                 benchmark_compile_config$optimizer$config$learning_rate,
                 benchmark_fit$params$epochs)
  )

  #Custom obj
  expect_equal(compile_config$loss, benchmark_compile_config$loss)
  #Metric
  expect_equal(compile_config$metrics, benchmark_compile_config$metrics)

  #Compare loss metrics
  expect_equal(sb_model_fit_results$fit_nn$metrics$loss, benchmark_fit$metrics$loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$huber_loss, benchmark_fit$metrics$huber_loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$val_loss, benchmark_fit$metrics$val_loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$val_huber_loss, benchmark_fit$metrics$val_huber_loss)


  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made <-
    all(sb_model_fit_results$pred ==
          as.numeric(
            predict(sb_model_fit_results$model_nn,
                    as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]))
          )
    )

  expect_true(check_if_predictions_are_correctly_made)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(sb_model_fit_results$df_eval_metrics ==
                                               calculate_eval_metrics(pred = sb_model_fit_results$pred,
                                                                      target = ts_splits$validation$target_validation_sample,
                                                                      huber_delta = huber_delta,
                                                                      quantile_tau = quantile_tau,
                                                                      chosen_eval_metric = chosen_eval_metric,
                                                                      early_stop = early_stop,
                                                                      best_iteration = which.min(benchmark_fit$metrics$val_huber_loss)
                                                                      )
  )

  expect_true(check_if_metrics_are_correctly_made)


})

test_that("set_eval_function correctly sets a keras nn3 model (custom_objective = pseudo_huber_error) and early_stop", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "random_search"
  custom_objective = "pseudo_huber_error"
  chosen_eval_metric <- NULL
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.5
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(regularizer_l1 = list(distribution_choice = "constant", value = c(1)),
                                 regularizer_l2 = list(distribution_choice = "uniform", pars = c(min = 1, max = 1)),
                                 droprate = list(distribution_choice = "uniform", pars = c(min = 0.5, max = 0.7)),
                                 lr = list(distribution_choice = "uniform", pars = c(min = 0.02, max = 0.02)),
                                 size_of_batch = list(distribution_choice = "constant", value = 512L),
                                 number_of_epochs = list(distribution_choice = "constant", value = 100L))

  keras_architecture_parameters <- list(units = c(32,16,8), n_layers = 3,
                                        activation = c("relu", "relu", "relu"),  nn_optimizer = 'Adam',
                                        batch_norm_option = c(TRUE, TRUE, TRUE))


  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL

  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )



  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  tensorflow::set_random_seed(123)
  suppressWarnings(
    sb_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                chosen_eval_metric_translated = chosen_eval_metric_translated,
                                chosen_eval_metric = chosen_eval_metric, #User defined
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective_translated = custom_objective_translated,

                                #Keras infrastructure
                                keras_architecture_parameters = keras_architecture_parameters,

                                #Hyperparameters
                                regularizer_l1 = hyperparameters_grid$regularizer_l1[1],
                                regularizer_l2 = hyperparameters_grid$regularizer_l2[1],
                                droprate = hyperparameters_grid$droprate[1],
                                lr = hyperparameters_grid$lr[1],
                                number_of_epochs = hyperparameters_grid$number_of_epochs[1],
                                size_of_batch = hyperparameters_grid$size_of_batch[1],
                                verbose = FALSE,
                                return_all_info = TRUE
    )
  )


  #Check if set_eval_function correctly sets a nn model
  check_if_sb_algo_is_used <- class(sb_model_fit_results$fit_nn) == "keras_training_history"
  expect_true(check_if_sb_algo_is_used)

  #Check if architecture is correctly set
  config <- sb_model_fit_results$model_nn$get_config()
  compile_config <- sb_model_fit_results$model_nn$get_compile_config()
  config_string <- as.character(keras::get_config(sb_model_fit_results$model_nn))
  #Input shape
  n_feat <- ncol(ts_splits$training$full_data_training_sample_clean) - 1
  check_if_n_feat_is_correct <- config$layers[[1]]$config$batch_input_shape[[2]] == n_feat
  expect_true(check_if_n_feat_is_correct)
  #Activation
  check_if_activation_is_correct <- all(c(config$layers[[2]]$config$activation,
                                          config$layers[[5]]$config$activation,
                                          config$layers[[8]]$config$activation) == keras_architecture_parameters$activation)
  expect_true(check_if_activation_is_correct)
  #Units
  check_if_units_is_correct <- all(c(config$layers[[2]]$config$units,
                                     config$layers[[5]]$config$units,
                                     config$layers[[8]]$config$units) == keras_architecture_parameters$units)
  expect_true(check_if_units_is_correct)
  #BatchNorm
  check_if_batchnorm_is_on <- all(c(config$layers[[3]]$class_name == if(keras_architecture_parameters$batch_norm_option[1]) "BatchNormalization",
                                    config$layers[[6]]$class_name == if(keras_architecture_parameters$batch_norm_option[2]) "BatchNormalization",
                                    config$layers[[9]]$class_name == if(keras_architecture_parameters$batch_norm_option[3]) "BatchNormalization"
                                    ))

  expect_true(check_if_batchnorm_is_on)
  #Optimizer
  check_if_optimizer_is_correct <- compile_config$optimizer$class_name == keras_architecture_parameters$nn_optimizer
  expect_true(check_if_optimizer_is_correct)

  #Check if hyperparametesr are correctly set
  check_if_reg_l1_is_correct <- unique(c(config$layers[[2]]$config$kernel_regularizer$config$l1,
                                         config$layers[[5]]$config$kernel_regularizer$config$l1,
                                         config$layers[[8]]$config$kernel_regularizer$config$l1)) ==
    hyperparameters_grid$regularizer_l1[1]
  expect_true(check_if_reg_l1_is_correct)
  check_if_reg_l2_is_correct <- unique(c(config$layers[[2]]$config$kernel_regularizer$config$l2,
                                         config$layers[[5]]$config$kernel_regularizer$config$l2,
                                         config$layers[[8]]$config$kernel_regularizer$config$l2)) ==
    hyperparameters_grid$regularizer_l2[1]
  expect_true(check_if_reg_l2_is_correct)
  check_if_droprate_is_correct <- unique(c(config$layers[[4]]$config$rate,
                                           config$layers[[7]]$config$rate,
                                           config$layers[[10]]$config$rate)) == hyperparameters_grid$droprate[1]
  expect_true(check_if_droprate_is_correct)
  check_if_lr_is_correct <- all.equal(compile_config$optimizer$config$learning_rate,
                                      as.numeric(hyperparameters_grid$lr[1]), tolerance = 1e-03)
  expect_true(check_if_lr_is_correct)
  check_if_number_epochs_is_correct <- sb_model_fit_results$fit_nn$params$epochs == hyperparameters_grid$number_of_epochs[1]
  expect_true(check_if_number_epochs_is_correct)
  check_if_batch_size_is_correct <- all((hyperparameters_grid$size_of_batch[1] -
                                           nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps + #Number of samples in each step if training sample was size_of_batch * 2
                                           nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps) == hyperparameters_grid$size_of_batch)
  expect_true(check_if_batch_size_is_correct)



  #Check if custom objective is set
  expect_equal(custom_objective_translated$get_config()$name, compile_config$loss$config$name)
  expect_equal(custom_objective_translated$get_config()$delta, compile_config$loss$config$delta)


  #Check if early stop is correctly used
  check_early_stop_usage <- sb_model_fit_results$df_eval_metrics$best_iteration
  expect_true(!is.null(check_early_stop_usage))
  expect_gt(range(sb_model_fit_results$fit_nn$metrics$loss)[2] - range(sb_model_fit_results$fit_nn$metrics$loss)[1],
            range(sb_model_fit_results$fit_nn$metrics$val_loss)[2] - range(sb_model_fit_results$fit_nn$metrics$val_loss)[1]
  )

  expect_gt(range(sb_model_fit_results$fit_nn$metrics$huber_loss)[2] - range(sb_model_fit_results$fit_nn$metrics$huber_loss)[1],
            range(sb_model_fit_results$fit_nn$metrics$val_huber_loss)[2] - range(sb_model_fit_results$fit_nn$metrics$val_huber_loss)[1]
  )
  #Monitored metrics is lower for best iteration
  expect_gt(sb_model_fit_results$fit_nn$metrics[chosen_eval_metric_translated$name][[1]][1],
            sb_model_fit_results$fit_nn$metrics[chosen_eval_metric_translated$name][[1]][sb_model_fit_results$df_eval_metrics$best_iteration])


  #Checks with another nn model
  set.seed(123)
  tensorflow::set_random_seed(123)
  benchmark_model <- keras::keras_model_sequential()
  benchmark_model %>%
    keras::layer_dense(units = keras_architecture_parameters$units[1],
                       activation = keras_architecture_parameters$activation[1],
                       input_shape = ncol(ts_splits$training$features_training_sample[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[2],
                       activation = keras_architecture_parameters$activation[2],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[3],
                       activation = keras_architecture_parameters$activation[3],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = 1)

  benchmark_model %>% keras::compile(
    loss = custom_objective_translated,
    optimizer = keras::optimizer_adam(learning_rate = hyperparameters_grid$lr[1]),
    metrics = chosen_eval_metric_translated$metric
  )

  benchmark_fit <- benchmark_model %>%
    keras::fit(x = as.matrix(ts_splits$training$full_data_training_sample_clean[,-1]),
               y = ts_splits$training$full_data_training_sample_clean[,1],
               epochs = hyperparameters_grid$number_of_epochs[1],
               batch_size = hyperparameters_grid$size_of_batch[1],
               callbacks = list(callback_early_stopping(monitor = chosen_eval_metric_translated$name,
                                                        patience = early_stop,
                                                        restore_best_weights = TRUE,
                                                        mode = chosen_eval_metric_translated$mode)), #early stop
               validation_data = list(as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]),
                                      ts_splits$validation$target_validation_sample),

               verbose = FALSE)


  #Get configs
  benchmark_config <- benchmark_model$get_config()
  benchmark_compile_config <- benchmark_model$get_compile_config()


  #Input shape
  expect_equal(config$layers[[1]]$config$batch_input_shape[[2]][1], benchmark_config$layers[[1]]$config$batch_input_shape[[2]][1])
  #Units
  expect_equal(c(config$layers[[2]]$config$units,
                 config$layers[[5]]$config$units,
                 config$layers[[8]]$config$units),
               c(benchmark_config$layers[[2]]$config$units,
                 benchmark_config$layers[[5]]$config$units,
                 benchmark_config$layers[[8]]$config$units))
  #Activation
  expect_equal(c(config$layers[[2]]$config$activation,
                 config$layers[[5]]$config$activation,
                 config$layers[[8]]$config$activation),
               c(benchmark_config$layers[[2]]$config$activation,
                 benchmark_config$layers[[5]]$config$activation,
                 benchmark_config$layers[[8]]$config$activation))
  #BatchNorm
  expect_equal(c(config$layers[[3]]$class_name,
                 config$layers[[6]]$class_name,
                 config$layers[[9]]$class_name),
               c(benchmark_config$layers[[3]]$class_name,
                 benchmark_config$layers[[6]]$class_name,
                 benchmark_config$layers[[9]]$class_name))
  #Optimizer
  expect_equal(compile_config$optimizer$class_name, benchmark_compile_config$optimizer$class_name)
  #Hyperparameters
  expect_equal(c(config$layers[[2]]$config$kernel_regularizer$config$l1,
                 config$layers[[2]]$config$kernel_regularizer$config$l2,
                 config$layers[[4]]$config$rate,
                 config$layers[[5]]$config$kernel_regularizer$config$l1,
                 config$layers[[5]]$config$kernel_regularizer$config$l2,
                 config$layers[[7]]$config$rate,
                 config$layers[[8]]$config$kernel_regularizer$config$l1,
                 config$layers[[8]]$config$kernel_regularizer$config$l2,
                 config$layers[[10]]$config$rate,

                 compile_config$optimizer$config$learning_rate,
                 sb_model_fit_results$fit_nn$params$epochs),

               c(benchmark_config$layers[[2]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[2]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[4]]$config$rate,
                 benchmark_config$layers[[5]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[5]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[7]]$config$rate,
                 benchmark_config$layers[[8]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[8]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[10]]$config$rate,


                 benchmark_compile_config$optimizer$config$learning_rate,
                 benchmark_fit$params$epochs)
  )

  #Custom obj
  expect_equal(compile_config$loss, benchmark_compile_config$loss)
  #Metric
  expect_equal(compile_config$metrics, benchmark_compile_config$metrics)

  #Compare loss metrics
  expect_equal(sb_model_fit_results$fit_nn$metrics$loss, benchmark_fit$metrics$loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$huber_loss, benchmark_fit$metrics$huber_loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$val_loss, benchmark_fit$metrics$val_loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$val_huber_loss, benchmark_fit$metrics$val_huber_loss)


  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made <-
    all(sb_model_fit_results$pred ==
          as.numeric(
            predict(sb_model_fit_results$model_nn,
                    as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]))
          )
    )

  expect_true(check_if_predictions_are_correctly_made)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(sb_model_fit_results$df_eval_metrics ==
                                               calculate_eval_metrics(pred = sb_model_fit_results$pred,
                                                                      target = ts_splits$validation$target_validation_sample,
                                                                      huber_delta = huber_delta,
                                                                      quantile_tau = quantile_tau,
                                                                      chosen_eval_metric = chosen_eval_metric,
                                                                      early_stop = early_stop,
                                                                      best_iteration = which.min(benchmark_fit$metrics$val_huber_loss)
                                               )
  )

  expect_true(check_if_metrics_are_correctly_made)


})

test_that("set_eval_function correctly sets a keras nn4 model (custom_objective = squared_error) and early_stop (mphe)", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "random_search"
  custom_objective = "squared_error"
  chosen_eval_metric <- NULL
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.5
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(regularizer_l1 = list(distribution_choice = "constant", value = c(1)),
                                 regularizer_l2 = list(distribution_choice = "uniform", pars = c(min = 1, max = 1)),
                                 droprate = list(distribution_choice = "uniform", pars = c(min = 0.5, max = 0.7)),
                                 lr = list(distribution_choice = "uniform", pars = c(min = 0.02, max = 0.02)),
                                 size_of_batch = list(distribution_choice = "constant", value = 512L),
                                 number_of_epochs = list(distribution_choice = "constant", value = 100L))

  keras_architecture_parameters <- list(units = c(64,32,16,8), n_layers = 4,
                                        activation = c("relu", "relu", "relu", "relu"),  nn_optimizer = 'Adam',
                                        batch_norm_option = c(TRUE, TRUE, TRUE, TRUE))


  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL

  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )


  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  tensorflow::set_random_seed(123)
  suppressWarnings(
    sb_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                chosen_eval_metric_translated = chosen_eval_metric_translated,
                                chosen_eval_metric = chosen_eval_metric, #User defined
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective_translated = custom_objective_translated,

                                #Keras infrastructure
                                keras_architecture_parameters = keras_architecture_parameters,

                                #Hyperparameters
                                regularizer_l1 = hyperparameters_grid$regularizer_l1[1],
                                regularizer_l2 = hyperparameters_grid$regularizer_l2[1],
                                droprate = hyperparameters_grid$droprate[1],
                                lr = hyperparameters_grid$lr[1],
                                number_of_epochs = hyperparameters_grid$number_of_epochs[1],
                                size_of_batch = hyperparameters_grid$size_of_batch[1],
                                verbose = FALSE,
                                return_all_info = TRUE
    )
  )


  #Check if set_eval_function correctly sets a nn model
  check_if_sb_algo_is_used <- class(sb_model_fit_results$fit_nn) == "keras_training_history"
  expect_true(check_if_sb_algo_is_used)

  #Check if architecture is correctly set
  config <- sb_model_fit_results$model_nn$get_config()
  compile_config <- sb_model_fit_results$model_nn$get_compile_config()
  config_string <- as.character(keras::get_config(sb_model_fit_results$model_nn))
  #Input shape
  n_feat <- ncol(ts_splits$training$full_data_training_sample_clean) - 1
  check_if_n_feat_is_correct <- config$layers[[1]]$config$batch_input_shape[[2]] == n_feat
  expect_true(check_if_n_feat_is_correct)
  #Activation
  check_if_activation_is_correct <- all(c(config$layers[[2]]$config$activation,
                                          config$layers[[5]]$config$activation,
                                          config$layers[[8]]$config$activation,
                                          config$layers[[11]]$config$activation
                                          ) == keras_architecture_parameters$activation)
  expect_true(check_if_activation_is_correct)
  #Units
  check_if_units_is_correct <- all(c(config$layers[[2]]$config$units,
                                     config$layers[[5]]$config$units,
                                     config$layers[[8]]$config$units,
                                     config$layers[[11]]$config$units) == keras_architecture_parameters$units)
  expect_true(check_if_units_is_correct)
  #BatchNorm
  check_if_batchnorm_is_on <- all(c(config$layers[[3]]$class_name == if(keras_architecture_parameters$batch_norm_option[1]) "BatchNormalization",
                                    config$layers[[6]]$class_name == if(keras_architecture_parameters$batch_norm_option[2]) "BatchNormalization",
                                    config$layers[[9]]$class_name == if(keras_architecture_parameters$batch_norm_option[3]) "BatchNormalization",
                                    config$layers[[12]]$class_name == if(keras_architecture_parameters$batch_norm_option[4]) "BatchNormalization"
  ))

  expect_true(check_if_batchnorm_is_on)
  #Optimizer
  check_if_optimizer_is_correct <- compile_config$optimizer$class_name == keras_architecture_parameters$nn_optimizer
  expect_true(check_if_optimizer_is_correct)

  #Check if hyperparametesr are correctly set
  check_if_reg_l1_is_correct <- unique(c(config$layers[[2]]$config$kernel_regularizer$config$l1,
                                         config$layers[[5]]$config$kernel_regularizer$config$l1,
                                         config$layers[[8]]$config$kernel_regularizer$config$l1,
                                         config$layers[[11]]$config$kernel_regularizer$config$l1)) ==
    hyperparameters_grid$regularizer_l1[1]
  expect_true(check_if_reg_l1_is_correct)
  check_if_reg_l2_is_correct <- unique(c(config$layers[[2]]$config$kernel_regularizer$config$l2,
                                         config$layers[[5]]$config$kernel_regularizer$config$l2,
                                         config$layers[[8]]$config$kernel_regularizer$config$l2,
                                         config$layers[[11]]$config$kernel_regularizer$config$l2
  )) ==
    hyperparameters_grid$regularizer_l2[1]
  expect_true(check_if_reg_l2_is_correct)
  check_if_droprate_is_correct <- unique(c(config$layers[[4]]$config$rate,
                                           config$layers[[7]]$config$rate,
                                           config$layers[[10]]$config$rate,
                                           config$layers[[13]]$config$rate
  )) == hyperparameters_grid$droprate[1]
  expect_true(check_if_droprate_is_correct)
  check_if_lr_is_correct <- all.equal(compile_config$optimizer$config$learning_rate,
                                      as.numeric(hyperparameters_grid$lr[1]), tolerance = 1e-03)
  expect_true(check_if_lr_is_correct)
  check_if_number_epochs_is_correct <- sb_model_fit_results$fit_nn$params$epochs == hyperparameters_grid$number_of_epochs[1]
  expect_true(check_if_number_epochs_is_correct)
  check_if_batch_size_is_correct <- all((hyperparameters_grid$size_of_batch[1] -
                                           nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps + #Number of samples in each step if training sample was size_of_batch * 2
                                           nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps) == hyperparameters_grid$size_of_batch)
  expect_true(check_if_batch_size_is_correct)



  #Check if custom objective is set
  expect_equal(custom_objective_translated, compile_config$loss)


  #Check if early stop is correctly used
  check_early_stop_usage <- sb_model_fit_results$df_eval_metrics$best_iteration
  expect_true(!is.null(check_early_stop_usage))
  expect_gt(range(sb_model_fit_results$fit_nn$metrics$loss)[2] - range(sb_model_fit_results$fit_nn$metrics$loss)[1],
            range(sb_model_fit_results$fit_nn$metrics$val_loss)[2] - range(sb_model_fit_results$fit_nn$metrics$val_loss)[1]
  )

  expect_gt(range(sb_model_fit_results$fit_nn$metrics$mean_squared_error)[2] - range(sb_model_fit_results$fit_nn$metrics$mean_squared_error)[1],
            range(sb_model_fit_results$fit_nn$metrics$val_mean_squared_error)[2] - range(sb_model_fit_results$fit_nn$metrics$val_mean_squared_error)[1]
  )
  #Monitored metrics is lower for best iteration
  expect_gt(sb_model_fit_results$fit_nn$metrics[chosen_eval_metric_translated$name][[1]][1],
            sb_model_fit_results$fit_nn$metrics[chosen_eval_metric_translated$name][[1]][sb_model_fit_results$df_eval_metrics$best_iteration])


  #Checks with another nn model
  set.seed(123)
  tensorflow::set_random_seed(123)
  benchmark_model <- keras::keras_model_sequential()
  benchmark_model %>%
    keras::layer_dense(units = keras_architecture_parameters$units[1],
                       activation = keras_architecture_parameters$activation[1],
                       input_shape = ncol(ts_splits$training$features_training_sample[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[2],
                       activation = keras_architecture_parameters$activation[2],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[3],
                       activation = keras_architecture_parameters$activation[3],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[4],
                       activation = keras_architecture_parameters$activation[4],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%


    keras::layer_dense(units = 1)

  benchmark_model %>% keras::compile(
    loss = custom_objective_translated,
    optimizer = keras::optimizer_adam(learning_rate = hyperparameters_grid$lr[1]),
    metrics = chosen_eval_metric_translated$metric
  )

  benchmark_fit <- benchmark_model %>%
    keras::fit(x = as.matrix(ts_splits$training$full_data_training_sample_clean[,-1]),
               y = ts_splits$training$full_data_training_sample_clean[,1],
               epochs = hyperparameters_grid$number_of_epochs[1],
               batch_size = hyperparameters_grid$size_of_batch[1],
               callbacks = list(callback_early_stopping(monitor = chosen_eval_metric_translated$name,
                                                        patience = early_stop,
                                                        restore_best_weights = TRUE,
                                                        mode = chosen_eval_metric_translated$mode)), #early stop
               validation_data = list(as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]),
                                      ts_splits$validation$target_validation_sample),

               verbose = FALSE)


  #Get configs
  benchmark_config <- benchmark_model$get_config()
  benchmark_compile_config <- benchmark_model$get_compile_config()


  #Input shape
  expect_equal(config$layers[[1]]$config$batch_input_shape[[2]][1], benchmark_config$layers[[1]]$config$batch_input_shape[[2]][1])
  #Units
  expect_equal(c(config$layers[[2]]$config$units,
                 config$layers[[5]]$config$units,
                 config$layers[[8]]$config$units,
                 config$layers[[11]]$config$units
  ),
               c(benchmark_config$layers[[2]]$config$units,
                 benchmark_config$layers[[5]]$config$units,
                 benchmark_config$layers[[8]]$config$units,
                 benchmark_config$layers[[11]]$config$units
                 ))
  #Activation
  expect_equal(c(config$layers[[2]]$config$activation,
                 config$layers[[5]]$config$activation,
                 config$layers[[8]]$config$activation,
                 config$layers[[11]]$config$activation
  ),
               c(benchmark_config$layers[[2]]$config$activation,
                 benchmark_config$layers[[5]]$config$activation,
                 benchmark_config$layers[[8]]$config$activation,
                 benchmark_config$layers[[11]]$config$activation
               ))
  #BatchNorm
  expect_equal(c(config$layers[[3]]$class_name,
                 config$layers[[6]]$class_name,
                 config$layers[[9]]$class_name,
                 config$layers[[12]]$class_name
  ),
               c(benchmark_config$layers[[3]]$class_name,
                 benchmark_config$layers[[6]]$class_name,
                 benchmark_config$layers[[9]]$class_name,
                 benchmark_config$layers[[12]]$class_name
               ))
  #Optimizer
  expect_equal(compile_config$optimizer$class_name, benchmark_compile_config$optimizer$class_name)
  #Hyperparameters
  expect_equal(c(config$layers[[2]]$config$kernel_regularizer$config$l1,
                 config$layers[[2]]$config$kernel_regularizer$config$l2,
                 config$layers[[4]]$config$rate,
                 config$layers[[5]]$config$kernel_regularizer$config$l1,
                 config$layers[[5]]$config$kernel_regularizer$config$l2,
                 config$layers[[7]]$config$rate,
                 config$layers[[8]]$config$kernel_regularizer$config$l1,
                 config$layers[[8]]$config$kernel_regularizer$config$l2,
                 config$layers[[10]]$config$rate,
                 config$layers[[11]]$config$kernel_regularizer$config$l1,
                 config$layers[[11]]$config$kernel_regularizer$config$l2,
                 config$layers[[13]]$config$rate,


                 compile_config$optimizer$config$learning_rate,
                 sb_model_fit_results$fit_nn$params$epochs),

               c(benchmark_config$layers[[2]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[2]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[4]]$config$rate,
                 benchmark_config$layers[[5]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[5]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[7]]$config$rate,
                 benchmark_config$layers[[8]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[8]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[10]]$config$rate,
                 benchmark_config$layers[[11]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[11]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[13]]$config$rate,



                 benchmark_compile_config$optimizer$config$learning_rate,
                 benchmark_fit$params$epochs)
  )

  #Custom obj
  expect_equal(compile_config$loss, benchmark_compile_config$loss)
  #Metric
  expect_equal(compile_config$metrics, benchmark_compile_config$metrics)

  #Compare loss metrics
  expect_equal(sb_model_fit_results$fit_nn$metrics$loss, benchmark_fit$metrics$loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$mean_squared_error, benchmark_fit$metrics$mean_squared_error)
  expect_equal(sb_model_fit_results$fit_nn$metrics$val_loss, benchmark_fit$metrics$val_loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$val_mean_squared_error, benchmark_fit$metrics$val_mean_squared_error)


  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made <-
    all(sb_model_fit_results$pred ==
          as.numeric(
            predict(sb_model_fit_results$model_nn,
                    as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]))
          )
    )

  expect_true(check_if_predictions_are_correctly_made)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(sb_model_fit_results$df_eval_metrics ==
                                               calculate_eval_metrics(pred = sb_model_fit_results$pred,
                                                                      target = ts_splits$validation$target_validation_sample,
                                                                      huber_delta = huber_delta,
                                                                      quantile_tau = quantile_tau,
                                                                      chosen_eval_metric = chosen_eval_metric,
                                                                      early_stop = early_stop,
                                                                      best_iteration = which.min(benchmark_fit$metrics$val_mean_squared_error)
                                               )
  )

  expect_true(check_if_metrics_are_correctly_made)


})

test_that("set_eval_function correctly sets a keras nn5 model (custom_objective = squared_error) and early_stop (mse)", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "random_search"
  custom_objective = "squared_error"
  chosen_eval_metric <- NULL
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.5
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(regularizer_l1 = list(distribution_choice = "constant", value = c(1)),
                                 regularizer_l2 = list(distribution_choice = "uniform", pars = c(min = 1, max = 1)),
                                 droprate = list(distribution_choice = "uniform", pars = c(min = 0.5, max = 0.7)),
                                 lr = list(distribution_choice = "uniform", pars = c(min = 0.02, max = 0.02)),
                                 size_of_batch = list(distribution_choice = "constant", value = 512L),
                                 number_of_epochs = list(distribution_choice = "constant", value = 100L))

  keras_architecture_parameters <- list(units = c(128,64,32,16,8), n_layers = 5,
                                        activation = c("relu", "relu", "relu", "relu", "relu"),  nn_optimizer = 'Adam',
                                        batch_norm_option = c(TRUE, TRUE, TRUE, TRUE, TRUE))


  #Heuristic SB part
  cov_matrix_sample_size <- 36
  cov_estimation_method <- "sample"
  cov_matrix_benchmark <- NULL
  active_returns <- TRUE
  rp_method <- "cyclical-spinu"
  n_random_ports <- 2000
  random_ports_method <- "sample"
  opt_objective <- "sharpe"
  concentration_constraint_policy <- NULL
  tickers <- colnames(toy_preprocessed_features)[-c(1:3)]
  dates <- unique(toy_preprocessed_features$dates) %>% sort()
  signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::mutate(id = paste0(Var1, "-", Var2), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::mutate(is_eligible = 1) %>%
    dplyr::arrange(id)
  backtest_returns_xts <- NULL
  benchmark_returns_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL

  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_xts = backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
    )
  )


  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)


  #Checks tuning
  set.seed(123)
  tensorflow::set_random_seed(123)
  suppressWarnings(
    sb_model_fit_results <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                chosen_eval_metric_translated = chosen_eval_metric_translated,
                                chosen_eval_metric = chosen_eval_metric, #User defined
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective_translated = custom_objective_translated,

                                #Keras infrastructure
                                keras_architecture_parameters = keras_architecture_parameters,

                                #Hyperparameters
                                regularizer_l1 = hyperparameters_grid$regularizer_l1[1],
                                regularizer_l2 = hyperparameters_grid$regularizer_l2[1],
                                droprate = hyperparameters_grid$droprate[1],
                                lr = hyperparameters_grid$lr[1],
                                number_of_epochs = hyperparameters_grid$number_of_epochs[1],
                                size_of_batch = hyperparameters_grid$size_of_batch[1],
                                verbose = FALSE,
                                return_all_info = TRUE
    )
  )


  #Check if set_eval_function correctly sets a nn model
  check_if_sb_algo_is_used <- class(sb_model_fit_results$fit_nn) == "keras_training_history"
  expect_true(check_if_sb_algo_is_used)

  #Check if architecture is correctly set
  config <- sb_model_fit_results$model_nn$get_config()
  compile_config <- sb_model_fit_results$model_nn$get_compile_config()
  config_string <- as.character(keras::get_config(sb_model_fit_results$model_nn))
  #Input shape
  n_feat <- ncol(ts_splits$training$full_data_training_sample_clean) - 1
  check_if_n_feat_is_correct <- config$layers[[1]]$config$batch_input_shape[[2]] == n_feat
  expect_true(check_if_n_feat_is_correct)
  #Activation
  check_if_activation_is_correct <- all(c(config$layers[[2]]$config$activation,
                                          config$layers[[5]]$config$activation,
                                          config$layers[[8]]$config$activation,
                                          config$layers[[11]]$config$activation,
                                          config$layers[[14]]$config$activation

  ) == keras_architecture_parameters$activation)
  expect_true(check_if_activation_is_correct)
  #Units
  check_if_units_is_correct <- all(c(config$layers[[2]]$config$units,
                                     config$layers[[5]]$config$units,
                                     config$layers[[8]]$config$units,
                                     config$layers[[11]]$config$units,
                                     config$layers[[14]]$config$units
  ) == keras_architecture_parameters$units)
  expect_true(check_if_units_is_correct)
  #BatchNorm
  check_if_batchnorm_is_on <- all(c(config$layers[[3]]$class_name == if(keras_architecture_parameters$batch_norm_option[1]) "BatchNormalization",
                                    config$layers[[6]]$class_name == if(keras_architecture_parameters$batch_norm_option[2]) "BatchNormalization",
                                    config$layers[[9]]$class_name == if(keras_architecture_parameters$batch_norm_option[3]) "BatchNormalization",
                                    config$layers[[12]]$class_name == if(keras_architecture_parameters$batch_norm_option[4]) "BatchNormalization",
                                    config$layers[[15]]$class_name == if(keras_architecture_parameters$batch_norm_option[4]) "BatchNormalization"


  ))

  expect_true(check_if_batchnorm_is_on)
  #Optimizer
  check_if_optimizer_is_correct <- compile_config$optimizer$class_name == keras_architecture_parameters$nn_optimizer
  expect_true(check_if_optimizer_is_correct)

  #Check if hyperparametesr are correctly set
  check_if_reg_l1_is_correct <- unique(c(config$layers[[2]]$config$kernel_regularizer$config$l1,
                                         config$layers[[5]]$config$kernel_regularizer$config$l1,
                                         config$layers[[8]]$config$kernel_regularizer$config$l1,
                                         config$layers[[11]]$config$kernel_regularizer$config$l1,
                                         config$layers[[14]]$config$kernel_regularizer$config$l1
  )) ==
    hyperparameters_grid$regularizer_l1[1]
  expect_true(check_if_reg_l1_is_correct)
  check_if_reg_l2_is_correct <- unique(c(config$layers[[2]]$config$kernel_regularizer$config$l2,
                                         config$layers[[5]]$config$kernel_regularizer$config$l2,
                                         config$layers[[8]]$config$kernel_regularizer$config$l2,
                                         config$layers[[11]]$config$kernel_regularizer$config$l2,
                                         config$layers[[14]]$config$kernel_regularizer$config$l2

  )) ==
    hyperparameters_grid$regularizer_l2[1]
  expect_true(check_if_reg_l2_is_correct)
  check_if_droprate_is_correct <- unique(c(config$layers[[4]]$config$rate,
                                           config$layers[[7]]$config$rate,
                                           config$layers[[10]]$config$rate,
                                           config$layers[[13]]$config$rate,
                                           config$layers[[16]]$config$rate


  )) == hyperparameters_grid$droprate[1]
  expect_true(check_if_droprate_is_correct)
  check_if_lr_is_correct <- all.equal(compile_config$optimizer$config$learning_rate,
                                      as.numeric(hyperparameters_grid$lr[1]), tolerance = 1e-03)
  expect_true(check_if_lr_is_correct)
  check_if_number_epochs_is_correct <- sb_model_fit_results$fit_nn$params$epochs == hyperparameters_grid$number_of_epochs[1]
  expect_true(check_if_number_epochs_is_correct)
  check_if_batch_size_is_correct <- all((hyperparameters_grid$size_of_batch[1] -
                                           nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps + #Number of samples in each step if training sample was size_of_batch * 2
                                           nrow(ts_splits$training$full_data_training_sample_clean)/sb_model_fit_results$fit_nn$params$steps) == hyperparameters_grid$size_of_batch)
  expect_true(check_if_batch_size_is_correct)



  #Check if custom objective is set
  expect_equal(custom_objective_translated, compile_config$loss)


  #Check if early stop is correctly used
  check_early_stop_usage <- sb_model_fit_results$df_eval_metrics$best_iteration
  expect_true(!is.null(check_early_stop_usage))
  expect_gt(range(sb_model_fit_results$fit_nn$metrics$loss)[2] - range(sb_model_fit_results$fit_nn$metrics$loss)[1],
            range(sb_model_fit_results$fit_nn$metrics$val_loss)[2] - range(sb_model_fit_results$fit_nn$metrics$val_loss)[1]
  )

  expect_gt(range(sb_model_fit_results$fit_nn$metrics$mean_squared_error)[2] - range(sb_model_fit_results$fit_nn$metrics$mean_squared_error)[1],
            range(sb_model_fit_results$fit_nn$metrics$val_mean_squared_error)[2] - range(sb_model_fit_results$fit_nn$metrics$val_mean_squared_error)[1]
  )
  #Monitored metrics is lower for best iteration
  expect_gt(sb_model_fit_results$fit_nn$metrics[chosen_eval_metric_translated$name][[1]][1],
            sb_model_fit_results$fit_nn$metrics[chosen_eval_metric_translated$name][[1]][sb_model_fit_results$df_eval_metrics$best_iteration])


  #Checks with another nn model
  set.seed(123)
  tensorflow::set_random_seed(123)
  benchmark_model <- keras::keras_model_sequential()
  benchmark_model %>%
    keras::layer_dense(units = keras_architecture_parameters$units[1],
                       activation = keras_architecture_parameters$activation[1],
                       input_shape = ncol(ts_splits$training$features_training_sample[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[2],
                       activation = keras_architecture_parameters$activation[2],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[3],
                       activation = keras_architecture_parameters$activation[3],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[4],
                       activation = keras_architecture_parameters$activation[4],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%

    keras::layer_dense(units = keras_architecture_parameters$units[5],
                       activation = keras_architecture_parameters$activation[5],
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = hyperparameters_grid$regularizer_l1[1],
                                                                     l2 = hyperparameters_grid$regularizer_l2[1])) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = hyperparameters_grid$droprate[1]) %>%
    keras::layer_dense(units = 1)

  benchmark_model %>% keras::compile(
    loss = custom_objective_translated,
    optimizer = keras::optimizer_adam(learning_rate = hyperparameters_grid$lr[1]),
    metrics = chosen_eval_metric_translated$metric
  )

  benchmark_fit <- benchmark_model %>%
    keras::fit(x = as.matrix(ts_splits$training$full_data_training_sample_clean[,-1]),
               y = ts_splits$training$full_data_training_sample_clean[,1],
               epochs = hyperparameters_grid$number_of_epochs[1],
               batch_size = hyperparameters_grid$size_of_batch[1],
               callbacks = list(callback_early_stopping(monitor = chosen_eval_metric_translated$name,
                                                        patience = early_stop,
                                                        restore_best_weights = TRUE,
                                                        mode = chosen_eval_metric_translated$mode)), #early stop
               validation_data = list(as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]),
                                      ts_splits$validation$target_validation_sample),

               verbose = FALSE)


  #Get configs
  benchmark_config <- benchmark_model$get_config()
  benchmark_compile_config <- benchmark_model$get_compile_config()


  #Input shape
  expect_equal(config$layers[[1]]$config$batch_input_shape[[2]][1], benchmark_config$layers[[1]]$config$batch_input_shape[[2]][1])
  #Units
  expect_equal(c(config$layers[[2]]$config$units,
                 config$layers[[5]]$config$units,
                 config$layers[[8]]$config$units,
                 config$layers[[11]]$config$units,
                 config$layers[[14]]$config$units
  ),
  c(benchmark_config$layers[[2]]$config$units,
    benchmark_config$layers[[5]]$config$units,
    benchmark_config$layers[[8]]$config$units,
    benchmark_config$layers[[11]]$config$units,
    benchmark_config$layers[[14]]$config$units
  )
  )
  #Activation
  expect_equal(c(config$layers[[2]]$config$activation,
                 config$layers[[5]]$config$activation,
                 config$layers[[8]]$config$activation,
                 config$layers[[11]]$config$activation,
                 config$layers[[14]]$config$activation

  ),
  c(benchmark_config$layers[[2]]$config$activation,
    benchmark_config$layers[[5]]$config$activation,
    benchmark_config$layers[[8]]$config$activation,
    benchmark_config$layers[[11]]$config$activation,
    benchmark_config$layers[[14]]$config$activation
  ))
  #BatchNorm
  expect_equal(c(config$layers[[3]]$class_name,
                 config$layers[[6]]$class_name,
                 config$layers[[9]]$class_name,
                 config$layers[[12]]$class_name,
                 config$layers[[15]]$class_name


  ),
  c(benchmark_config$layers[[3]]$class_name,
    benchmark_config$layers[[6]]$class_name,
    benchmark_config$layers[[9]]$class_name,
    benchmark_config$layers[[12]]$class_name,
    benchmark_config$layers[[15]]$class_name
  ))
  #Optimizer
  expect_equal(compile_config$optimizer$class_name, benchmark_compile_config$optimizer$class_name)
  #Hyperparameters
  expect_equal(c(config$layers[[2]]$config$kernel_regularizer$config$l1,
                 config$layers[[2]]$config$kernel_regularizer$config$l2,
                 config$layers[[4]]$config$rate,
                 config$layers[[5]]$config$kernel_regularizer$config$l1,
                 config$layers[[5]]$config$kernel_regularizer$config$l2,
                 config$layers[[7]]$config$rate,
                 config$layers[[8]]$config$kernel_regularizer$config$l1,
                 config$layers[[8]]$config$kernel_regularizer$config$l2,
                 config$layers[[10]]$config$rate,
                 config$layers[[11]]$config$kernel_regularizer$config$l1,
                 config$layers[[11]]$config$kernel_regularizer$config$l2,
                 config$layers[[13]]$config$rate,
                 config$layers[[14]]$config$kernel_regularizer$config$l1,
                 config$layers[[14]]$config$kernel_regularizer$config$l2,
                 config$layers[[16]]$config$rate,




                 compile_config$optimizer$config$learning_rate,
                 sb_model_fit_results$fit_nn$params$epochs),

               c(benchmark_config$layers[[2]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[2]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[4]]$config$rate,
                 benchmark_config$layers[[5]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[5]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[7]]$config$rate,
                 benchmark_config$layers[[8]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[8]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[10]]$config$rate,
                 benchmark_config$layers[[11]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[11]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[13]]$config$rate,
                 benchmark_config$layers[[14]]$config$kernel_regularizer$config$l1,
                 benchmark_config$layers[[14]]$config$kernel_regularizer$config$l2,
                 benchmark_config$layers[[16]]$config$rate,




                 benchmark_compile_config$optimizer$config$learning_rate,
                 benchmark_fit$params$epochs)
  )

  #Custom obj
  expect_equal(compile_config$loss, benchmark_compile_config$loss)
  #Metric
  expect_equal(compile_config$metrics, benchmark_compile_config$metrics)

  #Compare loss metrics
  expect_equal(sb_model_fit_results$fit_nn$metrics$loss, benchmark_fit$metrics$loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$mean_squared_error, benchmark_fit$metrics$mean_squared_error)
  expect_equal(sb_model_fit_results$fit_nn$metrics$val_loss, benchmark_fit$metrics$val_loss)
  expect_equal(sb_model_fit_results$fit_nn$metrics$val_mean_squared_error, benchmark_fit$metrics$val_mean_squared_error)


  #Check if predictions and errors are correctly made
  check_if_predictions_are_correctly_made <-
    all(sb_model_fit_results$pred ==
          as.numeric(
            predict(sb_model_fit_results$model_nn,
                    as.matrix(ts_splits$validation$features_validation_sample[,-c(1:3)]))
          )
    )

  expect_true(check_if_predictions_are_correctly_made)

  #Check if metrics are correctly made
  check_if_metrics_are_correctly_made <- all(sb_model_fit_results$df_eval_metrics ==
                                               calculate_eval_metrics(pred = sb_model_fit_results$pred,
                                                                      target = ts_splits$validation$target_validation_sample,
                                                                      huber_delta = huber_delta,
                                                                      quantile_tau = quantile_tau,
                                                                      early_stop = early_stop,
                                                                      chosen_eval_metric = chosen_eval_metric,
                                                                      best_iteration = which.min(benchmark_fit$metrics$val_mean_squared_error)
                                               )
  )

  expect_true(check_if_metrics_are_correctly_made)


})
