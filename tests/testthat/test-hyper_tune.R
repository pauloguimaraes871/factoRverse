test_that("all hyperparameters are explored in grid search", {

  #Grid Search
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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

  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
                         )

  #Check that hyperparameters are all tested
  expect_equal(
    c(unique(hyper_tune_results$chosen_eval_metric_validation_current_date$mtry),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$num.trees),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$max.depth),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$min.bucket)
      ),
      c(hyper_grid_domain_list$mtry,
        hyper_grid_domain_list$num.trees,
        hyper_grid_domain_list$max.depth,
        hyper_grid_domain_list$min.bucket
  )
  )


})

test_that("all hyperparameters are explored in random search", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "xgb"
  tuning_method = "random_search"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(min_child_weight = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                 max_depth = list(distribution_choice = "constant", value = 3L),
                                 subsample = list(distribution_choice = "uniform", pars = c(min = 0.2, max = 0.4)),
                                 colsample_bytree = list(distribution_choice = "constant", value = c(0.75, 0.90)),
                                 eta = list(distribution_choice = "constant", value = 0.2),
                                 alpha = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                 gamma = list(distribution_choice = "constant", value = 0),
                                 nrounds = list(distribution_choice = "lognormal", pars = c(meanlog = 2L, sdlog = 1L)))


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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
  set.seed(123)
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tuning
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )



  #Check that hyperparameters are all tested
  expect_equal(
    c(unique(hyper_tune_results$chosen_eval_metric_validation_current_date$min_child_weight),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$max_depth),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$subsample),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$colsample_bytree),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$eta),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$alpha),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$gamma),
      unique(hyper_tune_results$chosen_eval_metric_validation_current_date$nrounds)
    ),
    c(unique(hyperparameters_grid$min_child_weight),
      unique(hyperparameters_grid$max_depth),
      unique(hyperparameters_grid$subsample),
      unique(hyperparameters_grid$colsample_bytree),
      unique(hyperparameters_grid$eta),
      unique(hyperparameters_grid$alpha),
      unique(hyperparameters_grid$gamma),
      unique(hyperparameters_grid$nrounds)

    )
  )



})

test_that("best_iteration is included in chosen_eval_metric_validation", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "xgb"
  tuning_method = "random_search"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(min_child_weight = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                 max_depth = list(distribution_choice = "constant", value = 3L),
                                 subsample = list(distribution_choice = "uniform", pars = c(min = 0.2, max = 0.4)),
                                 colsample_bytree = list(distribution_choice = "constant", value = c(0.75, 0.90)),
                                 eta = list(distribution_choice = "constant", value = 0.2),
                                 alpha = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                 gamma = list(distribution_choice = "constant", value = 0),
                                 nrounds = list(distribution_choice = "lognormal", pars = c(meanlog = 2L, sdlog = 1L)))


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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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


  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tuning
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )



  #Check that best_iteration is present
  expect_true(!is.null(hyper_tune_results$chosen_eval_metric_validation_current_date$best_iteration))


})

test_that("random_search/grid_search: hyper_tuning works for glmnet when Parallel = FALSE", {

  #GLMNET
  ########################

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
  hyper_grid_domain_list <- list(alpha = c(0.1, 1), lambda.min.ratio = c(0.01, 0.2, 0.5))

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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
  )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
  )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
  )

  #Hyper tuning
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )

  #Compare hyper tuning
  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm
  )


  hyper_eval_test <- list()
  for(i in seq_len(length(hyperparameters_grid$alpha))){
    hyper_eval_test[[i]] <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                ml_algorithm = ml_algorithm, #User defined
                                tuning_method = tuning_method, #User defined
                                chosen_eval_metric_translated = chosen_eval_metric_translated,
                                chosen_eval_metric = chosen_eval_metric, #User defined
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective = custom_objective_translated,
                                alpha = hyperparameters_grid$alpha[i],
                                lambda.min.ratio = hyperparameters_grid$lambda.min.ratio[i],
                                verbose = TRUE,
                                return_all_info = FALSE
    )

  }

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    c(sapply(hyperparameters_grid, function(x) x[best_hyper_ref]), best_lam = hyper_eval_test[[best_hyper_ref]]$best_lam),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]][,-11],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date
  )


  ########################

})

test_that("random_search/grid_search: hyper_tuning works for random_forest when Parallel = FALSE", {

  #RANDOM FOREST
  ########################

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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
                                        )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
                                 )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
                           )

  #Hyper tuning
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )

  #Compare hyper tuning
  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm
  )


  hyper_eval_test <- list()
  set.seed(123)
  for(i in seq_len(length(hyperparameters_grid$mtry))){
    hyper_eval_test[[i]] <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                      features_validation_sample = ts_splits$validation$features_validation_sample,
                                      target_validation_sample = ts_splits$validation$target_validation_sample,
                                      target_fwd_name = target_fwd_name, #User defined
                                      ml_algorithm = ml_algorithm, #User defined
                                      tuning_method = tuning_method, #User defined
                                      chosen_eval_metric_translated = chosen_eval_metric_translated,
                                      chosen_eval_metric = chosen_eval_metric, #User defined
                                      huber_delta = huber_delta,
                                      quantile_tau = quantile_tau,
                                      early_stop = early_stop,
                                      custom_objective = custom_objective_translated,
                                      max.depth = hyperparameters_grid$max.depth[i],
                                      num.trees = hyperparameters_grid$num.trees[i],
                                      mtry = hyperparameters_grid$mtry[i],
                                      min.bucket = hyperparameters_grid$min.bucket[i],
                                      verbose = TRUE,
                                      return_all_info = FALSE
    )

  }

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
    )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    sapply(hyperparameters_grid, function(x) x[best_hyper_ref]),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date
  )


  ########################

})

test_that("random_search/grid_search: hyper_tuning works for XGB when Parallel = FALSE", {

  #XGB
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "xgb"
  tuning_method = "random_search"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(min_child_weight = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                 max_depth = list(distribution_choice = "constant", value = 3L),
                                 subsample = list(distribution_choice = "uniform", pars = c(min = 0.2, max = 0.4)),
                                 colsample_bytree = list(distribution_choice = "constant", value = c(0.75, 0.90)),
                                 eta = list(distribution_choice = "constant", value = 0.2),
                                 alpha = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                 gamma = list(distribution_choice = "constant", value = 0),
                                 nrounds = list(distribution_choice = "lognormal", pars = c(meanlog = 2L, sdlog = 1L)))


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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )



  #Compare hyper tuning
  #Create tuning list
  set.seed(123)
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)
  hyper_eval_test <- list()
  for(i in seq_len(length(hyperparameters_grid$min_child_weight))){
    hyper_eval_test[[i]] <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                chosen_eval_metric = chosen_eval_metric, #For tuning
                                chosen_eval_metric_translated = chosen_eval_metric_translated, #For early stop
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective_translated = custom_objective_translated,
                                min_child_weight = hyperparameters_grid$min_child_weight[i],
                                max_depth = hyperparameters_grid$max_depth[i],
                                subsample = hyperparameters_grid$subsample[i],
                                colsample_bytree = hyperparameters_grid$colsample_bytree[i],
                                eta = hyperparameters_grid$eta[i],
                                alpha = hyperparameters_grid$alpha[i],
                                gamma = hyperparameters_grid$gamma[i],
                                nrounds = hyperparameters_grid$nrounds[i],
                                verbose = FALSE,
                                return_all_info = FALSE
    )

  }

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    c(sapply(hyperparameters_grid, function(x) x[best_hyper_ref]), best_iteration = hyper_eval_test[[best_hyper_ref]]$best_iteration),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]][,-11],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date
  )

  #######################
})

test_that("random_search/grid_search: hyper_tuning works for NN when Parallel = FALSE", {
  skip_if_no_tensorflow()

  #NN2
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "random_search"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  set.seed(123)
  tensorflow::set_random_seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )



  #Compare hyper tuning
  #Create tuning list
  set.seed(123)
  tensorflow::set_random_seed(123)
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)
  hyper_eval_test <- list()
  for(i in seq_len(length(hyperparameters_grid$regularizer_l1))){
    hyper_eval_test[[i]] <- FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
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
                                regularizer_l1 = hyperparameters_grid$regularizer_l1[i],
                                regularizer_l2 = hyperparameters_grid$regularizer_l2[i],
                                droprate = hyperparameters_grid$droprate[i],
                                lr = hyperparameters_grid$lr[i],
                                number_of_epochs = hyperparameters_grid$number_of_epochs[i],
                                size_of_batch = hyperparameters_grid$size_of_batch[i],
                                verbose = FALSE,
                                return_all_info = FALSE
    )

  }

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    c(sapply(hyperparameters_grid, function(x) x[best_hyper_ref]), best_iteration = hyper_eval_test[[best_hyper_ref]]$best_iteration),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]][,-11],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date
  )

  #######################
})

test_that("random_search/grid_search: hyper_tuning works for glmnet when Parallel = TRUE", {

  #GLMNET - Plan(Multisession)
  ########################
  future::plan("multisession")

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
  parallel = TRUE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(alpha = c(0.1, 1), lambda.min.ratio = c(0.2, 0.5))
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
  )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
  )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
  )

  #Hyper tuning
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )

  #Compare hyper tuning
  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm
  )


  hyper_eval_test <- list()
  set.seed(123)
  hyper_eval_test <-
    foreach::foreach(i = seq_len(length(hyperparameters_grid$alpha)), .options.future = list(seed = TRUE)) %dofuture% {
      FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
          features_validation_sample = ts_splits$validation$features_validation_sample,
          target_validation_sample = ts_splits$validation$target_validation_sample,
          target_fwd_name = target_fwd_name, #User defined
          ml_algorithm = ml_algorithm, #User defined
          tuning_method = tuning_method, #User defined
          chosen_eval_metric_translated = chosen_eval_metric_translated,
          chosen_eval_metric = chosen_eval_metric, #User defined
          huber_delta = huber_delta,
          quantile_tau = quantile_tau,
          early_stop = early_stop,
          custom_objective = custom_objective_translated,
          alpha = hyperparameters_grid$alpha[i],
          lambda.min.ratio = hyperparameters_grid$lambda.min.ratio[i],
          verbose = TRUE,
          return_all_info = FALSE
      )

    }

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    c(sapply(hyperparameters_grid, function(x) x[best_hyper_ref]), best_lam = hyper_eval_test[[best_hyper_ref]]$best_lam),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]][,-11],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date
  )
  future::plan("sequential")


  ########################

})

test_that("random_search/grid_search: hyper_tuning works for random_forest when Parallel = TRUE", {

  #RANDOM FOREST - Plan(Sequential)
  ########################
  future::plan("sequential")

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
  parallel = TRUE
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
  )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
  )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
  )

  #Hyper tuning
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )

  #Compare hyper tuning
  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm
  )


  hyper_eval_test <- list()
  set.seed(123)
  hyper_eval_test <-
  suppressWarnings(
  foreach::foreach(i = seq_len(length(hyperparameters_grid$mtry)), .options.future = list(seed = TRUE)) %dofuture% {
                                FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,

                                features_validation_sample = ts_splits$validation$features_validation_sample,
                                target_validation_sample = ts_splits$validation$target_validation_sample,
                                target_fwd_name = target_fwd_name, #User defined
                                ml_algorithm = ml_algorithm, #User defined
                                tuning_method = tuning_method, #User defined
                                chosen_eval_metric_translated = chosen_eval_metric_translated,
                                chosen_eval_metric = chosen_eval_metric, #User defined
                                huber_delta = huber_delta,
                                quantile_tau = quantile_tau,
                                early_stop = early_stop,
                                custom_objective = custom_objective_translated,
                                max.depth = hyperparameters_grid$max.depth[i],
                                num.trees = hyperparameters_grid$num.trees[i],
                                mtry = hyperparameters_grid$mtry[i],
                                min.bucket = hyperparameters_grid$min.bucket[i],
                                verbose = TRUE,
                                return_all_info = FALSE
    )

  }
  )

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    sapply(hyperparameters_grid, function(x) x[best_hyper_ref]),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date
  )
  future::plan("sequential")


  ########################


  #RANDOM FOREST - Plan(Multisession)
  ########################
  future::plan("multisession")

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
  parallel = TRUE
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
  )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
  )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
  )

  #Hyper tuning
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )

  #Compare hyper tuning
  #Create tuning list
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm
  )


  hyper_eval_test <- list()
  set.seed(123)
  hyper_eval_test <-
    foreach::foreach(i = seq_len(length(hyperparameters_grid$mtry)), .options.future = list(seed = TRUE)) %dofuture% {
      FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
          features_validation_sample = ts_splits$validation$features_validation_sample,
          target_validation_sample = ts_splits$validation$target_validation_sample,
          target_fwd_name = target_fwd_name, #User defined
          ml_algorithm = ml_algorithm, #User defined
          tuning_method = tuning_method, #User defined
          chosen_eval_metric_translated = chosen_eval_metric_translated,
          chosen_eval_metric = chosen_eval_metric, #User defined
          huber_delta = huber_delta,
          quantile_tau = quantile_tau,
          early_stop = early_stop,
          custom_objective = custom_objective_translated,
          max.depth = hyperparameters_grid$max.depth[i],
          num.trees = hyperparameters_grid$num.trees[i],
          mtry = hyperparameters_grid$mtry[i],
          min.bucket = hyperparameters_grid$min.bucket[i],
          verbose = TRUE,
          return_all_info = FALSE
      )

    }

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    sapply(hyperparameters_grid, function(x) x[best_hyper_ref]),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date
  )
  future::plan("sequential")


  ########################

})

test_that("random_search/grid_search: hyper_tuning works for XGB when Parallel = TRUE", {

   #XGB Plan(Multisession)
  ########################
  future::plan("multisession")
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "xgb"
  tuning_method = "random_search"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = TRUE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(min_child_weight = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                 max_depth = list(distribution_choice = "constant", value = 3L),
                                 subsample = list(distribution_choice = "uniform", pars = c(min = 0.2, max = 0.4)),
                                 colsample_bytree = list(distribution_choice = "constant", value = c(0.75, 0.90)),
                                 eta = list(distribution_choice = "constant", value = 0.2),
                                 alpha = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                 gamma = list(distribution_choice = "constant", value = 0),
                                 nrounds = list(distribution_choice = "lognormal", pars = c(meanlog = 2L, sdlog = 1L)))


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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )



  #Compare hyper tuning
  #Create tuning list
  set.seed(123)
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)
  hyper_eval_test <- list()
  hyper_eval_test <-
    foreach::foreach(i = seq_len(length(hyperparameters_grid$min_child_weight)), .options.future = list(seed = TRUE)) %dofuture% {

      FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
          features_validation_sample = ts_splits$validation$features_validation_sample,
          target_validation_sample = ts_splits$validation$target_validation_sample,
          target_fwd_name = target_fwd_name, #User defined
          chosen_eval_metric = chosen_eval_metric, #For tuning
          chosen_eval_metric_translated = chosen_eval_metric_translated, #For early stop
          huber_delta = huber_delta,
          quantile_tau = quantile_tau,
          early_stop = early_stop,
          custom_objective_translated = custom_objective_translated,
          min_child_weight = hyperparameters_grid$min_child_weight[i],
          max_depth = hyperparameters_grid$max_depth[i],
          subsample = hyperparameters_grid$subsample[i],
          colsample_bytree = hyperparameters_grid$colsample_bytree[i],
          eta = hyperparameters_grid$eta[i],
          alpha = hyperparameters_grid$alpha[i],
          gamma = hyperparameters_grid$gamma[i],
          nrounds = hyperparameters_grid$nrounds[i],
          verbose = FALSE,
          return_all_info = FALSE
      )

    }

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    c(sapply(hyperparameters_grid, function(x) x[best_hyper_ref]), best_iteration = hyper_eval_test[[best_hyper_ref]]$best_iteration),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]][,-11],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date
  )

  future::plan("sequential")

  #######################
})

test_that("Skipped: random_search/grid_search: hyper_tuning works for NN when Parallel = TRUE",{
skip()
  #NN2 (Plan = Sequential)
  ########################
  future::plan("sequential")
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "random_search"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  set.seed(123)
  tensorflow::set_random_seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )



  #Compare hyper tuning
  #Create tuning list
  set.seed(123)
  tensorflow::set_random_seed(123)
  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)
  hyper_eval_test <- list()
  suppressMessages(
  hyper_eval_test <-
    foreach::foreach(i = seq_len(length(hyperparameters_grid$regularizer_l1)), .options.future = list(seed = TRUE)) %dofuture% {

      FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
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
          regularizer_l1 = hyperparameters_grid$regularizer_l1[i],
          regularizer_l2 = hyperparameters_grid$regularizer_l2[i],
          droprate = hyperparameters_grid$droprate[i],
          lr = hyperparameters_grid$lr[i],
          number_of_epochs = hyperparameters_grid$number_of_epochs[i],
          size_of_batch = hyperparameters_grid$size_of_batch[i],
          verbose = FALSE,
          return_all_info = FALSE
      )

    }
  )

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric,
    tolerance = 1e-2
  )

  #Check if hyperparameters choice match
  best_hyper_ref <- which.min(as.numeric(sapply(hyper_eval_test, function(x) x[chosen_eval_metric])))
  expect_equal(
    c(sapply(hyperparameters_grid, function(x) x[best_hyper_ref]), best_iteration = hyper_eval_test[[best_hyper_ref]]$best_iteration),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    hyper_eval_test[[best_hyper_ref]][,-11],
    hyper_tune_results$validation_eval_metrics_hyper_choice_current_date,
    tolerance = 1e-5
  )

  #######################

  #NN2 (Plan = Multisession)
  ########################
  future::plan("multisession")
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
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- NULL
  acq <- NULL
  init_points = NULL
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(regularizer_l1 = 1 ,
                                 regularizer_l2 = c(1),
                                 droprate = c(0.50, 0.60),
                                 lr = c(0.02, 0.1),
                                 size_of_batch = 512L,
                                 number_of_epochs = 100L)


  keras_architecture_parameters <- list(units = c(32,16), n_layers = 2, activation = c('relu', 'relu'),  nn_optimizer = 'Adam', batch_norm_option = c(TRUE, TRUE))


  #Check Inputs
  expect_no_error(
    suppressWarnings(
      check_inputs_sb_backtest(features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets,
                             training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
                             validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method,
                             chosen_eval_metric = chosen_eval_metric,
                             ml_algorithm = ml_algorithm, custom_objective = custom_objective, huber_delta = huber_delta, quantile_tau = quantile_tau,
                             hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                             n_iter = n_iter, k_iter = k_iter, acq = acq, init_points = init_points, early_stop = early_stop, keras_architecture_parameters,
                             parallel = parallel
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  hyper_tune_results_list <- list()
  #Run hyper tune 10 times to check for differences
  tensorflow::set_random_seed(123)
  for(l in 1:10){
  hyper_tune_results_list[[l]] <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )
  }

  hyperparameters_grid <- create_expanded_hyper_grid_list(hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                                                          n_iter = n_iter, ml_algorithm = sb_algorithm)

  #Compare hyper tuning
  list_of_hyper_eval_test <- list()
  tensorflow::set_random_seed(123)
  for(s in 1:10){
    #Create tuning list
  hyper_eval_test <- list()
  suppressMessages(
  hyper_eval_test <-
    #For some reason, results here are not reproducible when running in parallel
    foreach::foreach(i = seq_len(length(hyperparameters_grid$regularizer_l1)), .options.future = list(seed = TRUE)) %dofuture% {

      FUN(full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
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
          regularizer_l1 = hyperparameters_grid$regularizer_l1[i],
          regularizer_l2 = hyperparameters_grid$regularizer_l2[i],
          droprate = hyperparameters_grid$droprate[i],
          lr = hyperparameters_grid$lr[i],
          number_of_epochs = hyperparameters_grid$number_of_epochs[i],
          size_of_batch = hyperparameters_grid$size_of_batch[i],
          verbose = FALSE,
          return_all_info = FALSE
      )

    }
  )

  list_of_hyper_eval_test[[s]] <- hyper_eval_test

  }

  hyper_eval_test[[1]] <- sapply(list_of_hyper_eval_test, function(x) x[[1]])
  hyper_eval_test[[2]] <- sapply(list_of_hyper_eval_test, function(x) x[[2]])
  hyper_eval_test[[3]] <- sapply(list_of_hyper_eval_test, function(x) x[[3]])
  hyper_eval_test[[4]] <- sapply(list_of_hyper_eval_test, function(x) x[[4]])



  #Check if same chosen_eval_metrics are calculated
  chosen_eval_metrics_first_round <- unlist(sapply(hyper_tune_results_list, function(x) x$chosen_eval_metric_validation_current_date[1,])["chosen_eval_metric",])
  chosen_eval_metrics_second_round <- unlist(sapply(hyper_tune_results_list, function(x) x$chosen_eval_metric_validation_current_date[2,])["chosen_eval_metric",])
  chosen_eval_metrics_third_round <- unlist(sapply(hyper_tune_results_list, function(x) x$chosen_eval_metric_validation_current_date[3,])["chosen_eval_metric",])
  chosen_eval_metrics_fourth_round <- unlist(sapply(hyper_tune_results_list, function(x) x$chosen_eval_metric_validation_current_date[4,])["chosen_eval_metric",])


  chosen_eval_metrics_test_first_round <- unlist(hyper_eval_test[[1]][chosen_eval_metric,])
  chosen_eval_metrics_test_second_round <- unlist(hyper_eval_test[[2]][chosen_eval_metric,])
  chosen_eval_metrics_test_third_round <- unlist(hyper_eval_test[[3]][chosen_eval_metric,])
  chosen_eval_metrics_test_fourth_round <- unlist(hyper_eval_test[[4]][chosen_eval_metric,])



  #Check if chosen_eval_metrics come from the same distribution
  expect_gte(
    ks.test(chosen_eval_metrics_first_round, chosen_eval_metrics_test_first_round)$p.value,
    0.05
  )
  expect_gte(
    ks.test(chosen_eval_metrics_second_round, chosen_eval_metrics_test_second_round)$p.value,
    0.05
  )
  expect_gte(
    ks.test(chosen_eval_metrics_third_round, chosen_eval_metrics_test_third_round)$p.value,
    0.05
  )
  expect_gte(
    ks.test(chosen_eval_metrics_fourth_round, chosen_eval_metrics_test_fourth_round)$p.value,
    0.05
  )



  #Check if hyperparameters choice match (except early stop)
  hyperparameters_chosen <- t(sapply(hyper_tune_results_list, function(x) x$optimal_hyper))
  hyperparameters_chosen_test <- data.frame(regularizer_l1 = rep(NA,10), regularizer_l2 = rep(NA,10), droprate = rep(NA,10), lr = rep(NA,10),
                                            size_of_batch = rep(NA, 10), number_of_epochs = rep(NA, 10), best_iteration = rep(NA,10))
    for(i in 1:10){
      hyperparameters_chosen_test[i,c(1:6)] <-
      #For each run, get best hyperparameters
      sapply(hyperparameters_grid, function(x) x[
        which.min(c(as.numeric(hyper_eval_test[[1]][chosen_eval_metric,i]),
                    as.numeric(hyper_eval_test[[2]][chosen_eval_metric,i]),
                    as.numeric(hyper_eval_test[[3]][chosen_eval_metric,i]),
                    as.numeric(hyper_eval_test[[4]][chosen_eval_metric,i])
                    ))
      ]
      )

      #Get best iteration
      hyperparameters_chosen_test$best_iteration[i] <-
       c(as.numeric(hyper_eval_test[[1]]['best_iteration',i]),
         as.numeric(hyper_eval_test[[2]]['best_iteration',i]),
         as.numeric(hyper_eval_test[[3]]['best_iteration',i]),
         as.numeric(hyper_eval_test[[4]]['best_iteration',i])
         )[
          which.min(c(as.numeric(hyper_eval_test[[1]][chosen_eval_metric,i]),
                      as.numeric(hyper_eval_test[[2]][chosen_eval_metric,i]),
                      as.numeric(hyper_eval_test[[3]][chosen_eval_metric,i]),
                      as.numeric(hyper_eval_test[[4]][chosen_eval_metric,i])
                      ))
        ]


    }


  #Check if same metrics are calculated for the best hyperparameters
  eval_metrics_hyper_choice <- t(sapply(hyper_tune_results_list, function(x) x$validation_eval_metrics_hyper_choice_current_date))
  eval_metrics_hyper_choice_test <- data.frame(Score = rep(NA,25), rss = rep(NA,25), cp = rep(NA,25), rmse = rep(NA,25), mae = rep(NA, 25),
                                               mphe = rep(NA, 25), mpe = rep(NA,25), mape = rep(NA,25), hr = rep(NA,25), mb = rep(NA,25))
  for(i in 1:10){
    eval_metrics_hyper_choice_test[i,] <-
    rbind(unlist(hyper_eval_test[[1]][-11,i]),
          unlist(hyper_eval_test[[2]][-11,i]),
          unlist(hyper_eval_test[[3]][-11,i]),
          unlist(hyper_eval_test[[4]][-11,i])
          )[
      which.min(c(as.numeric(hyper_eval_test[[1]][chosen_eval_metric,i]),
                  as.numeric(hyper_eval_test[[2]][chosen_eval_metric,i]),
                  as.numeric(hyper_eval_test[[3]][chosen_eval_metric,i]),
                  as.numeric(hyper_eval_test[[4]][chosen_eval_metric,i])
                  )),
    ]

  }

  #Check if chosen_eval_metrics come from the same distribution
  for(i in colnames(eval_metrics_hyper_choice_test)){
    expect_gte(
      ks.test(unlist(eval_metrics_hyper_choice[,i]), eval_metrics_hyper_choice_test[,i])$p.value,
      0.05
    )
  }


  future::plan("sequential")
  #######################


})

test_that("bayesian_opt: hyper_tuning works for glmnet when Parallel = FALSE", {

  #GLMNET
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "glmnet"
  tuning_method = "bayesian_opt"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.7
  early_stop <- NULL
  n_iter <- 2
  k_iter <- 2
  acq <- "ucb"
  init_points = 5
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(alpha = c(0.1, 0.7), lambda.min.ratio = c(0.2, 0.3))
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
  signal_themes_m_df <- NULL
  custom_signal_weights_m_df <- NULL
  gsm_algorithm <- "ols"
  .test_seed <- NULL


  #Check Inputs
  expect_no_error(
      check_inputs_sb_backtest(
        features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
        validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
        rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
        custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
        chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
        init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
      )
  )

  #Translate metrics
  adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective,
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
  )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
  )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
  )

  #Hyper tuning
  set.seed(123)
  suppressWarnings(
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter, acq = acq,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
                                   )
  )


  #Compare hyper tuning via bayesian opt
  test_eval_function <- function(alpha, lambda.min.ratio){
    #Set objects in GLM format
    features_matrix_train_clean <- ts_splits$training$full_data_training_sample_clean[,-which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training features matrix
    target_vector_train <- ts_splits$training$full_data_training_sample_clean[, which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training target vector
    features_validation_sample_clean <- ts_splits$validation$features_validation_sample[,-c(1:3)]


    #Fit GLM model
    glmnet_fit <- glmnet::glmnet(as.matrix(features_matrix_train_clean), #train matrix
                                 target_vector_train, #target vector
                                 alpha = alpha, #alpha hyperparameter
                                 lambda.min.ratio = lambda.min.ratio) #lambda.min.ratio hyperparameter


    #Get best lambda
    best_lam <- get_best_lambda(glmnet_fit = glmnet_fit, lambda_seq = glmnet_fit$lambda, #Glmnet Specific
                                features_validation_sample_clean = features_validation_sample_clean, target_validation_sample = ts_splits$validation$target_validation_sample,  #Val Data
                                huber_delta = huber_delta, quantile_tau = quantile_tau, chosen_eval_metric = chosen_eval_metric) #Eval Metrics Parameters

    #Predict with best lam
    pred <- stats::predict(glmnet_fit,#GLM model
                           newx = as.matrix(features_validation_sample_clean),  #Features test
                           s = best_lam) #Predict with best_lam

    #Target
    target <- ts_splits$validation$target_validation_sample

    #Error
    error <- target - pred



    #Calculate eval metrics
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)


    #Return List
    return(list(
      Score = -validation_sample_rmse,
      rss = validation_sample_rss, #RSS
      cp = validation_sample_cp, #CP
      rmse = validation_sample_rmse, #RMSE
      mae = validation_sample_mae, #MAE
      mphe = validation_sample_mphe, #MPHE
      mpe = validation_sample_mpe, #Pinball
      mape = validation_sample_mape, #MAPE
      hr = validation_sample_hr, #Hit Rate
      mb = validation_sample_mb, #Bias:
      best_lam = best_lam
    )
    )

  }


  set.seed(123)
  bayes_tune_test <- ParBayesianOptimization::bayesOpt(
    FUN = test_eval_function, #FUN
    bounds = hyper_grid_domain_list, #Boundaries
    initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
    acq = acq, #Acquisition function to be used
    iters.n = n_iter, #Number of times BO is to be repeated
    iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
    verbose = verbose, #Display msgs?
    parallel = parallel #Parallel?
  )


  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(abs(bayes_tune_test$scoreSummary$Score)),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Did results used same range of hyperparameters?
  expect_equal(
    cbind(alpha = bayes_tune_test$scoreSummary$alpha, lambda.min.ratio = bayes_tune_test$scoreSummary$lambda.min.ratio),
    cbind(alpha = hyper_tune_results$chosen_eval_metric_validation_current_date$alpha, lambda.min.ratio = hyper_tune_results$chosen_eval_metric_validation_current_date$lambda.min.ratio)
  )

  #Check if hyperparameters choice match
  expect_equal(
    c(unlist(ParBayesianOptimization::getBestPars(bayes_tune_test)), best_lam = bayes_tune_test$scoreSummary$best_lam[which.max(bayes_tune_test$scoreSummary$Score)]),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    as.numeric(bayes_tune_test$scoreSummary[which.max(bayes_tune_test$scoreSummary$Score),
                                            c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")]),
    as.numeric(hyper_tune_results$validation_eval_metrics_hyper_choice_current_date)
  )


  ########################

})

test_that("bayesian_opt: hyper_tuning works for random_forest when Parallel = FALSE", {

  #RANDOM FOREST
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "rf"
  tuning_method = "bayesian_opt"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.7
  early_stop <- NULL
  n_iter <- 2
  k_iter <- 2
  acq <- "ucb"
  init_points = 5
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(mtry = c(0.1, 0.7), num.trees = c(200L, 300L),
                                 max.depth = c(4L, 6L), min.bucket = c(1, 3))
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
  )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
  )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
  )

  #Hyper tuning
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter, acq = acq,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )

  #Compare hyper tuning via bayesian opt
  test_eval_function <- function(mtry, max.depth, min.bucket, num.trees){
    #Fit RF model
    rf_fit <- ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(ts_splits$training$full_data_training_sample_clean), #Names need to be clean
                             mtry = mtry * (ncol(ts_splits$training$full_data_training_sample_clean) - 1), #Proportion of variables used to forecast
                             num.trees = num.trees, #Number of trees
                             max.depth = max.depth, #Max Depth of tree
                             min.bucket = min.bucket, #Min Size of Terminal Node
                             verbose = verbose
    )
    #Format
    features_validation_sample_clean <- ts_splits$validation$features_validation_sample[,-c(1:3)]

    #Predict
    pred <- stats::predict(rf_fit,#RF model
                           data = janitor::clean_names(features_validation_sample_clean) #Features val
    )$predictions


    #Target
    target <- ts_splits$validation$target_validation_sample

    #Error
    error <- target - pred


    #Calculate eval metrics
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)


    #Return List
    return(list(
      Score = -validation_sample_rmse,
      rss = validation_sample_rss, #RSS
      cp = validation_sample_cp, #CP
      rmse = validation_sample_rmse, #RMSE
      mae = validation_sample_mae, #MAE
      mphe = validation_sample_mphe, #MPHE
      mpe = validation_sample_mpe, #Pinball
      mape = validation_sample_mape, #MAPE
      hr = validation_sample_hr, #Hit Rate
      mb = validation_sample_mb #Bias:
      )
    )

  }


  set.seed(123)
  bayes_tune_test <- ParBayesianOptimization::bayesOpt(
    FUN = test_eval_function, #FUN
    bounds = hyper_grid_domain_list, #Boundaries
    initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
    acq = acq, #Acquisition function to be used
    iters.n = n_iter, #Number of times BO is to be repeated
    iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
    verbose = verbose, #Display msgs?
    parallel = parallel #Parallel?
  )


  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(abs(bayes_tune_test$scoreSummary$Score)),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Did results used same range of hyperparameters?
  expect_equal(
    cbind(mtry = bayes_tune_test$scoreSummary$mtry, num.trees = bayes_tune_test$scoreSummary$num.trees,
          max.depth = bayes_tune_test$scoreSummary$max.depth, min.bucket = bayes_tune_test$scoreSummary$min.bucket),
    cbind(mtry = hyper_tune_results$chosen_eval_metric_validation_current_date$mtry, num.trees = hyper_tune_results$chosen_eval_metric_validation_current_date$num.trees,
          max.depth = hyper_tune_results$chosen_eval_metric_validation_current_date$max.depth, min.bucket = hyper_tune_results$chosen_eval_metric_validation_current_date$min.bucket)
  )

  #Check if hyperparameters choice match
  expect_equal(
    unlist(ParBayesianOptimization::getBestPars(bayes_tune_test)),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    as.numeric(bayes_tune_test$scoreSummary[which.max(bayes_tune_test$scoreSummary$Score),
                                 c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")]),
    as.numeric(hyper_tune_results$validation_eval_metrics_hyper_choice_current_date)
  )


  ########################

})

test_that("bayesian_opt: hyper_tuning works for XGB (custom_obj = pseudo-huber error) when Parallel = FALSE", {
  #XGB
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "xgb"
  tuning_method = "bayesian_opt"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- 2
  acq <- "ucb"
  init_points = 9
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(min_child_weight = c(1, 3),
                                 max_depth = c(3L, 5L),
                                 subsample = c(0.2, 0.4),
                                 colsample_bytree = c(0.75, 0.90),
                                 eta = c(0.05, 0.2),
                                 alpha = c(1,3),
                                 gamma = c(1,3),
                                 nrounds = c(200L, 500L)
                                 )


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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter, acq = acq,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )



  #Compare hyper tuning via bayesian opt
  test_eval_function <- function(min_child_weight, max_depth, subsample, colsample_bytree, eta, alpha, gamma, nrounds){
    #Set objects in XGB Format
    features_matrix_train_clean <- ts_splits$training$full_data_training_sample_clean[,-which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training features matrix
    target_vector_train <- ts_splits$training$full_data_training_sample_clean[, which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training target vector
    features_validation_sample_clean = ts_splits$validation$features_validation_sample[,-c(1:3)]

    full_data_training_sample_clean_xgb <- xgboost::xgb.DMatrix(data = as.matrix(features_matrix_train_clean), #Already withou 3 first columns
                                                                label = target_vector_train)

    full_data_val_clean_xgb <- xgboost::xgb.DMatrix(data = as.matrix(features_validation_sample_clean),
                                                    label =  ts_splits$validation$target_validation_sample)


    #Fit XGB model
    xgb_fit <- xgboost::xgb.train(data = full_data_training_sample_clean_xgb,
                                  eta = eta, #Learning Rate
                                  early_stopping_rounds = early_stop, #Number of rounds to early stop
                                  min_child_weight = min_child_weight, #Minimum sum of instance weight (hessian) needed in a child
                                  max_depth = round(max_depth, 0), #Max tree depth
                                  nrounds = nrounds, #Number of trees (boosting interations)
                                  subsample = subsample, #Subsample ratio of training instance
                                  colsample_bytree = colsample_bytree, #Col subsample
                                  alpha = alpha, #L1 regularization on weights
                                  gamma = gamma, #Min loss reduction to make a further partition
                                  print_every_n = 25,
                                  verbose = verbose,
                                  eval_metric = chosen_eval_metric_translated, #Set eval metric for ealy stop
                                  #Set custom objective
                                  objective = custom_objective_translated,
                                  #Watchlist,
                                  watchlist = list(train = full_data_training_sample_clean_xgb,
                                                   validation = full_data_val_clean_xgb),

                                  huber_slope = huber_delta #Huber delta
                                  #quantile_alpha = quantile_tau #Tau for quantile regression

    )


    #Predict
    pred <- stats::predict(xgb_fit,#XGB model
                           newdata = as.matrix(features_validation_sample_clean) #Features val
    )

    #Target
    target <- ts_splits$validation$target_validation_sample

    #Error
    error <- target - pred


    #Calculate eval metrics
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)


    #Return List
    return(list(
      Score = switch(chosen_eval_metric,
                     rss = validation_sample_rss, #RSS
                     cp = validation_sample_cp, #CP
                     rmse = -validation_sample_rmse, #RMSE
                     mae = -validation_sample_mae, #MAE
                     mphe = -validation_sample_mphe, #MPHE
                     mpe = -validation_sample_mpe, #Pinball
                     mape = -validation_sample_mape, #MAPE
                     hr = validation_sample_hr, #Hit Rate
                     mb = validation_sample_mb #Bias
                     ),

      rss = validation_sample_rss, #RSS
      cp = validation_sample_cp, #CP
      rmse = validation_sample_rmse, #RMSE
      mae = validation_sample_mae, #MAE
      mphe = validation_sample_mphe, #MPHE
      mpe = validation_sample_mpe, #Pinball
      mape = validation_sample_mape, #MAPE
      hr = validation_sample_hr, #Hit Rate
      mb = validation_sample_mb,
      best_iteration = xgb_fit$best_iteration
      )
    )

  }


  set.seed(123)
  bayes_tune_test <- ParBayesianOptimization::bayesOpt(
    FUN = test_eval_function, #FUN
    bounds = hyper_grid_domain_list, #Boundaries
    initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
    acq = acq, #Acquisition function to be used
    iters.n = n_iter, #Number of times BO is to be repeated
    iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
    verbose = verbose, #Display msgs?
    parallel = parallel #Parallel?
  )


  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(abs(bayes_tune_test$scoreSummary$Score)),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Did results used same range of hyperparameters?
  expect_equal(
    cbind(min_child_weight = bayes_tune_test$scoreSummary$min_child_weight, max_depth = bayes_tune_test$scoreSummary$max_depth,
          subsample = bayes_tune_test$scoreSummary$subsample, colsample_bytree = bayes_tune_test$scoreSummary$colsample_bytree,
          eta = bayes_tune_test$scoreSummary$eta, alpha = bayes_tune_test$scoreSummary$alpha,
          gamma = bayes_tune_test$scoreSummary$gamma, nrounds = bayes_tune_test$scoreSummary$nrounds,
          best_iteration = bayes_tune_test$scoreSummary$best_iteration)
    ,
    cbind(min_child_weight = hyper_tune_results$chosen_eval_metric_validation_current_date$min_child_weight, max_depth = hyper_tune_results$chosen_eval_metric_validation_current_date$max_depth,
          subsample = hyper_tune_results$chosen_eval_metric_validation_current_date$subsample, colsample_bytree = hyper_tune_results$chosen_eval_metric_validation_current_date$colsample_bytree,
          eta = hyper_tune_results$chosen_eval_metric_validation_current_date$eta, alpha = hyper_tune_results$chosen_eval_metric_validation_current_date$alpha,
          gamma = hyper_tune_results$chosen_eval_metric_validation_current_date$gamma, nrounds = hyper_tune_results$chosen_eval_metric_validation_current_date$nrounds,
          best_iteration = hyper_tune_results$chosen_eval_metric_validation_current_date$best_iteration)
    )

  #Check if hyperparameters choice match
  expect_equal(
    unlist(ParBayesianOptimization::getBestPars(bayes_tune_test)),
    hyper_tune_results$optimal_hyper[-9]
  )

  #Check if best_iteration match
  expect_equal(
    bayes_tune_test$scoreSummary$best_iteration[which.max(bayes_tune_test$scoreSummary$Score)],
    as.numeric(hyper_tune_results$optimal_hyper[9])
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    as.numeric(bayes_tune_test$scoreSummary[which.max(bayes_tune_test$scoreSummary$Score),
                                            c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")]),
    as.numeric(hyper_tune_results$validation_eval_metrics_hyper_choice_current_date)
  )

  #######################
})

test_that("bayesian_opt: hyper_tuning works for NN (custom_obj = pseudo-huber error) when Parallel = FALSE", {
  skip_if_no_tensorflow()

  #NN2
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "bayesian_opt"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 4
  k_iter <- 4
  acq <- "ucb"
  init_points = 7
  parallel = FALSE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(regularizer_l1 = c(1, 3),
                                 regularizer_l2 = c(1, 3),
                                 droprate = c(0.5, 0.7),
                                 lr = c(0.05, 0.2),
                                 size_of_batch = c(512L,512L),
                                 number_of_epochs = c(100L, 100L))


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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  set.seed(123)
  tensorflow::set_random_seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter, acq = acq,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )




  #Compare hyper tuning via bayesian opt
  test_eval_function <- function(regularizer_l1, regularizer_l2, droprate, lr, number_of_epochs, size_of_batch){
    #Set objects in XGB Format
    features_matrix_train_clean <- ts_splits$training$full_data_training_sample_clean[,-which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training features matrix
    target_vector_train <- ts_splits$training$full_data_training_sample_clean[, which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training target vector
    features_validation_sample_clean = ts_splits$validation$features_validation_sample[,-c(1:3)]
    target_vector_validation <- ts_splits$validation$target_validation_sample


    #Fit NN model
    model_nn <- keras::keras_model_sequential()
    model_nn %>%
      keras::layer_dense(units = keras_architecture_parameters$units[1],
                         activation = keras_architecture_parameters$activation[1], #Units and activation may vary by layer
                         input_shape =  ncol(features_matrix_train_clean), #Shape = # of features
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>% #L1 and L2 Regularization
      keras::layer_batch_normalization() %>% #Batch normalization
      keras::layer_dropout(rate = droprate) %>% #Adds dropout

      keras::layer_dense(units = keras_architecture_parameters$units[2],
                         activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>% #Batch normalization
      keras::layer_dropout(rate = droprate) %>% #Adds dropout

      keras::layer_dense(units = 1) #No activation means linear: f(x) = x

    model_nn %>% keras::compile(
      loss = custom_objective_translated,
      optimizer = keras::optimizer_adam(learning_rate = lr),
      metrics = chosen_eval_metric_translated$metric
    )

    fit_nn <- model_nn %>%
      keras::fit(x = as.matrix(features_matrix_train_clean), #Training features
                 y = target_vector_train, #Training label
                 epochs = number_of_epochs, #Number of epochs
                 batch_size = size_of_batch, #Batch size (should be a multiple of 2)
                 verbose = verbose,
                 callbacks = list(keras::callback_early_stopping(monitor = chosen_eval_metric_translated$name,
                                                                 patience = early_stop, #Early stop (nº epochs with no improvement)
                                                                 restore_best_weights = TRUE, #Restore best weights after stopping
                                                                 mode = chosen_eval_metric_translated$mode)), #Min for RMSE, MAE and HUBER
                 validation_data = list(as.matrix(features_validation_sample_clean),target_vector_validation) #Validation data
      )



    #Predict
    pred <- stats::predict(model_nn,#NN model
                           as.matrix(features_validation_sample_clean) #Features val
    )

    #Target
    target <- target_vector_validation

    #Error
    error <- target - pred


    #Calculate eval metrics
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)


    #Return List
    return(list(
      Score = switch(chosen_eval_metric,
                     rss = validation_sample_rss, #RSS
                     cp = validation_sample_cp, #CP
                     rmse = -validation_sample_rmse, #RMSE
                     mae = -validation_sample_mae, #MAE
                     mphe = -validation_sample_mphe, #MPHE
                     mpe = -validation_sample_mpe, #Pinball
                     mape = -validation_sample_mape, #MAPE
                     hr = validation_sample_hr, #Hit Rate
                     mb = validation_sample_mb #Bias
      ),

      rss = validation_sample_rss, #RSS
      cp = validation_sample_cp, #CP
      rmse = validation_sample_rmse, #RMSE
      mae = validation_sample_mae, #MAE
      mphe = validation_sample_mphe, #MPHE
      mpe = validation_sample_mpe, #Pinball
      mape = validation_sample_mape, #MAPE
      hr = validation_sample_hr, #Hit Rate
      mb = validation_sample_mb,
      best_iteration = which.min(fit_nn$metrics[[chosen_eval_metric_translated$name]])
    )
    )

  }


  set.seed(123)
  tensorflow::set_random_seed(123)
  bayes_tune_test <- ParBayesianOptimization::bayesOpt(
    FUN = test_eval_function, #FUN
    bounds = hyper_grid_domain_list, #Boundaries
    initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
    acq = acq, #Acquisition function to be used
    iters.n = n_iter, #Number of times BO is to be repeated
    iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
    verbose = verbose, #Display msgs?
    parallel = parallel #Parallel?
  )


  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(abs(bayes_tune_test$scoreSummary$Score)),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric,
    tolerance = 1e-3
  )

  #Did results used same range of hyperparameters?
  expect_equal(
    cbind(regularizer_l1 = bayes_tune_test$scoreSummary$regularizer_l1, regularizer_l2 = bayes_tune_test$scoreSummary$regularizer_l2,
          droprate = bayes_tune_test$scoreSummary$droprate, lr = bayes_tune_test$scoreSummary$lr,
          size_of_batch = bayes_tune_test$scoreSummary$size_of_batch, number_of_epochs = bayes_tune_test$scoreSummary$number_of_epochs,
          best_iteration = bayes_tune_test$scoreSummary$best_iteration)
    ,
    cbind(regularizer_l1 = hyper_tune_results$chosen_eval_metric_validation_current_date$regularizer_l1, regularizer_l2 = hyper_tune_results$chosen_eval_metric_validation_current_date$regularizer_l2,
          droprate = hyper_tune_results$chosen_eval_metric_validation_current_date$droprate, lr = hyper_tune_results$chosen_eval_metric_validation_current_date$lr,
          size_of_batch = hyper_tune_results$chosen_eval_metric_validation_current_date$size_of_batch, number_of_epochs = hyper_tune_results$chosen_eval_metric_validation_current_date$number_of_epochs,
          best_iteration = hyper_tune_results$chosen_eval_metric_validation_current_date$best_iteration)
  )

  #Check if hyperparameters choice match
  expect_equal(
    unlist(ParBayesianOptimization::getBestPars(bayes_tune_test)),
    hyper_tune_results$optimal_hyper[-7]
  )

  #Check if best_iteration match
  expect_equal(
    bayes_tune_test$scoreSummary$best_iteration[which.max(bayes_tune_test$scoreSummary$Score)],
    as.numeric(hyper_tune_results$optimal_hyper[7])
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    as.numeric(bayes_tune_test$scoreSummary[which.max(bayes_tune_test$scoreSummary$Score),
                                            c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")]),
    as.numeric(hyper_tune_results$validation_eval_metrics_hyper_choice_current_date)
  )

  #######################
})

test_that("bayesian_opt: hyper_tuning works for glmnet when Parallel = TRUE", {

  doFuture::registerDoFuture()
  future::plan("multisession")
  #GLMNET
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "glmnet"
  tuning_method = "bayesian_opt"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.7
  early_stop <- NULL
  n_iter <- 2
  k_iter <- 2
  acq <- "ucb"
  init_points = 5
  parallel = TRUE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(alpha = c(0.1, 0.7), lambda.min.ratio = c(0.2, 0.3))
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
  )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
  )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
  )

  #Hyper tuning
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter, acq = acq,
                                   init_points = init_points, k_iter = k_iter,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )

  #Compare hyper tuning via bayesian opt
  test_eval_function <- function(alpha, lambda.min.ratio){
    #Set objects in GLM format
    features_matrix_train_clean <- ts_splits$training$full_data_training_sample_clean[,-which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training features matrix
    target_vector_train <- ts_splits$training$full_data_training_sample_clean[, which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training target vector
    features_validation_sample_clean <- ts_splits$validation$features_validation_sample[,-c(1:3)]


    #Fit GLM model
    glmnet_fit <- glmnet::glmnet(as.matrix(features_matrix_train_clean), #train matrix
                                 target_vector_train, #target vector
                                 alpha = alpha, #alpha hyperparameter
                                 lambda.min.ratio = lambda.min.ratio) #lambda hyperparameter


    #Get best lambda
    best_lam <- glmnet_fit$lambda[which.max( #Which max score?
      sapply(
        apply(stats::predict(glmnet_fit, newx = as.matrix(features_validation_sample_clean)) #Predict to find best_lam
              , 2, function(x) calculate_eval_metrics(pred = x, target = ts_splits$validation$target_validation_sample,
                                                      huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                      chosen_eval_metric = chosen_eval_metric)), #Calculate eval metrics for all lambdas
        function(x) x$Score #Takes only score value

      )
    )]

    #Predict with best lam
    pred <- stats::predict(glmnet_fit,#GLM model
                           newx = as.matrix(features_validation_sample_clean),  #Features test
                           s = best_lam) #Predict with best_lam

    #Target
    target <- ts_splits$validation$target_validation_sample

    #Error
    error <- target - pred



    #Calculate eval metrics
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)


    #Return List
    return(list(
      Score = -validation_sample_rmse,
      rss = validation_sample_rss, #RSS
      cp = validation_sample_cp, #CP
      rmse = validation_sample_rmse, #RMSE
      mae = validation_sample_mae, #MAE
      mphe = validation_sample_mphe, #MPHE
      mpe = validation_sample_mpe, #Pinball
      mape = validation_sample_mape, #MAPE
      hr = validation_sample_hr, #Hit Rate
      mb = validation_sample_mb, #Bias:
      best_lam = best_lam
    )
    )

  }


  set.seed(123)
  bayes_tune_test <- doFuture::withDoRNG(ParBayesianOptimization::bayesOpt(
    FUN = test_eval_function, #FUN
    bounds = hyper_grid_domain_list, #Boundaries
    initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
    acq = acq, #Acquisition function to be used
    iters.n = n_iter, #Number of times BO is to be repeated
    iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
    verbose = verbose, #Display msgs?
    parallel = parallel #Parallel?
  )
  )


  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(abs(bayes_tune_test$scoreSummary$Score)),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Did results used same range of hyperparameters?
  expect_equal(
    cbind(alpha = bayes_tune_test$scoreSummary$alpha, lambda.min.ratio = bayes_tune_test$scoreSummary$lambda.min.ratio),
    cbind(alpha = hyper_tune_results$chosen_eval_metric_validation_current_date$alpha, lambda.min.ratio = hyper_tune_results$chosen_eval_metric_validation_current_date$lambda.min.ratio)
  )

  #Check if hyperparameters choice match
  expect_equal(
    c(unlist(ParBayesianOptimization::getBestPars(bayes_tune_test)), best_lam = bayes_tune_test$scoreSummary$best_lam[which.max(bayes_tune_test$scoreSummary$Score)]),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    as.numeric(bayes_tune_test$scoreSummary[which.max(bayes_tune_test$scoreSummary$Score),
                                            c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")]),
    as.numeric(hyper_tune_results$validation_eval_metrics_hyper_choice_current_date)
  )

  foreach::registerDoSEQ()
  future::plan("sequential")

  ########################

})

test_that("bayesian_opt: hyper_tuning works for random_forest when Parallel = TRUE", {

  doFuture::registerDoFuture()
  future::plan("multisession")
  #RANDOM FOREST
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "rf"
  tuning_method = "bayesian_opt"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.7
  early_stop <- NULL
  n_iter <- 2
  k_iter <- 2
  acq <- "ucb"
  init_points = 5
  parallel = TRUE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(mtry = c(0.1, 0.7), num.trees = c(200L, 300L),
                                 max.depth = c(4L, 6L), min.bucket = c(1, 3))
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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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
                                        early_stop = early_stop, huber_delta = huber_delta, verbose = verbose
  )

  custom_objective_translated <- adjusted_metrics$custom_objective_translated
  chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
  chosen_eval_metric <- adjusted_metrics$chosen_eval_metric



  #Splits data
  ts_splits <- time_series_split(toy_dates[training_sample_size+validation_sample_size], features_m_df = toy_preprocessed_features,
                                 target_m_df = toy_preprocessed_targets,
                                 dates_m_vector = toy_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                 split_method = split_method, target_fwd = target_fwd, target_fwd_name = target_fwd_name
  )



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method
  )

  #Hyper tuning
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter, acq = acq,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )

  #Compare hyper tuning via bayesian opt
  test_eval_function <- function(mtry, max.depth, min.bucket, num.trees){
    #Fit RF model
    rf_fit <- ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(ts_splits$training$full_data_training_sample_clean), #Names need to be clean
                             mtry = mtry * (ncol(ts_splits$training$full_data_training_sample_clean) - 1), #Proportion of variables used to forecast
                             num.trees = num.trees, #Number of trees
                             max.depth = max.depth, #Max Depth of tree
                             min.bucket = min.bucket, #Min Size of Terminal Node
                             verbose = verbose
    )
    #Format
    features_validation_sample_clean <- ts_splits$validation$features_validation_sample[,-c(1:3)]

    #Predict
    pred <- stats::predict(rf_fit,#RF model
                           data = janitor::clean_names(features_validation_sample_clean) #Features val
    )$predictions


    #Target
    target <- ts_splits$validation$target_validation_sample

    #Error
    error <- target - pred


    #Calculate eval metrics
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)


    #Return List
    return(list(
      Score = -validation_sample_rmse,
      rss = validation_sample_rss, #RSS
      cp = validation_sample_cp, #CP
      rmse = validation_sample_rmse, #RMSE
      mae = validation_sample_mae, #MAE
      mphe = validation_sample_mphe, #MPHE
      mpe = validation_sample_mpe, #Pinball
      mape = validation_sample_mape, #MAPE
      hr = validation_sample_hr, #Hit Rate
      mb = validation_sample_mb #Bias:
    )
    )

  }

  set.seed(123)
  bayes_tune_test <- doFuture::withDoRNG(ParBayesianOptimization::bayesOpt(
    FUN = test_eval_function, #FUN
    bounds = hyper_grid_domain_list, #Boundaries
    initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
    acq = acq, #Acquisition function to be used
    iters.n = n_iter, #Number of times BO is to be repeated
    iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
    verbose = verbose, #Display msgs?
    parallel = parallel #Parallel?
  )
  )

  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(abs(bayes_tune_test$scoreSummary$Score)),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Did results used same range of hyperparameters?
  expect_equal(
    cbind(mtry = bayes_tune_test$scoreSummary$mtry, num.trees = bayes_tune_test$scoreSummary$num.trees,
          max.depth = bayes_tune_test$scoreSummary$max.depth, min.bucket = bayes_tune_test$scoreSummary$min.bucket),
    cbind(mtry = hyper_tune_results$chosen_eval_metric_validation_current_date$mtry, num.trees = hyper_tune_results$chosen_eval_metric_validation_current_date$num.trees,
          max.depth = hyper_tune_results$chosen_eval_metric_validation_current_date$max.depth, min.bucket = hyper_tune_results$chosen_eval_metric_validation_current_date$min.bucket)
  )

  #Check if hyperparameters choice match
  expect_equal(
    unlist(ParBayesianOptimization::getBestPars(bayes_tune_test)),
    hyper_tune_results$optimal_hyper
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    as.numeric(bayes_tune_test$scoreSummary[which.max(bayes_tune_test$scoreSummary$Score),
                                            c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")]),
    as.numeric(hyper_tune_results$validation_eval_metrics_hyper_choice_current_date)
  )

  foreach::registerDoSEQ()
  future::plan("sequential")

  ########################

})

test_that("bayesian_opt: hyper_tuning works for XGB (custom_obj = pseudo-huber error) when Parallel = TRUE", {

  doFuture::registerDoFuture()
  future::plan("multisession")
  #XGB
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "xgb"
  tuning_method = "bayesian_opt"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 2
  k_iter <- 2
  acq <- "ucb"
  init_points = 9
  parallel = TRUE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(min_child_weight = c(1, 3),
                                 max_depth = c(3L, 5L),
                                 subsample = c(0.2, 0.4),
                                 colsample_bytree = c(0.75, 0.90),
                                 eta = c(0.05, 0.2),
                                 alpha = c(1,3),
                                 gamma = c(1,3),
                                 nrounds = c(200L, 500L)
  )


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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  set.seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter, acq = acq,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )



  #Compare hyper tuning via bayesian opt
  test_eval_function <- function(min_child_weight, max_depth, subsample, colsample_bytree, eta, alpha, gamma, nrounds){
    #Set objects in XGB Format
    features_matrix_train_clean <- ts_splits$training$full_data_training_sample_clean[,-which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training features matrix
    target_vector_train <- ts_splits$training$full_data_training_sample_clean[, which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training target vector
    features_validation_sample_clean = ts_splits$validation$features_validation_sample[,-c(1:3)]

    full_data_training_sample_clean_xgb <- xgboost::xgb.DMatrix(data = as.matrix(features_matrix_train_clean), #Already withou 3 first columns
                                                                label = target_vector_train)

    full_data_val_clean_xgb <- xgboost::xgb.DMatrix(data = as.matrix(features_validation_sample_clean),
                                                    label =  ts_splits$validation$target_validation_sample)


    #Fit XGB model
    xgb_fit <- xgboost::xgb.train(data = full_data_training_sample_clean_xgb,
                                  eta = eta, #Learning Rate
                                  early_stopping_rounds = early_stop, #Number of rounds to early stop
                                  min_child_weight = min_child_weight, #Minimum sum of instance weight (hessian) needed in a child
                                  max_depth = round(max_depth, 0), #Max tree depth
                                  nrounds = nrounds, #Number of trees (boosting interations)
                                  subsample = subsample, #Subsample ratio of training instance
                                  colsample_bytree = colsample_bytree, #Col subsample
                                  alpha = alpha, #L1 regularization on weights
                                  gamma = gamma, #Min loss reduction to make a further partition
                                  print_every_n = 25,
                                  verbose = verbose,
                                  eval_metric = chosen_eval_metric_translated, #Set eval metric for ealy stop
                                  #Set custom objective
                                  objective = custom_objective_translated,
                                  #Watchlist,
                                  watchlist = list(train = full_data_training_sample_clean_xgb,
                                                   validation = full_data_val_clean_xgb),

                                  huber_slope = huber_delta #Huber delta
                                  #quantile_alpha = quantile_tau #Tau for quantile regression

    )


    #Predict
    pred <- stats::predict(xgb_fit,#XGB model
                           newdata = as.matrix(features_validation_sample_clean) #Features val
    )

    #Target
    target <- ts_splits$validation$target_validation_sample

    #Error
    error <- target - pred


    #Calculate eval metrics
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)


    #Return List
    return(list(
      Score = switch(chosen_eval_metric,
                     rss = validation_sample_rss, #RSS
                     cp = validation_sample_cp, #CP
                     rmse = -validation_sample_rmse, #RMSE
                     mae = -validation_sample_mae, #MAE
                     mphe = -validation_sample_mphe, #MPHE
                     mpe = -validation_sample_mpe, #Pinball
                     mape = -validation_sample_mape, #MAPE
                     hr = validation_sample_hr, #Hit Rate
                     mb = validation_sample_mb #Bias
      ),

      rss = validation_sample_rss, #RSS
      cp = validation_sample_cp, #CP
      rmse = validation_sample_rmse, #RMSE
      mae = validation_sample_mae, #MAE
      mphe = validation_sample_mphe, #MPHE
      mpe = validation_sample_mpe, #Pinball
      mape = validation_sample_mape, #MAPE
      hr = validation_sample_hr, #Hit Rate
      mb = validation_sample_mb,
      best_iteration = xgb_fit$best_iteration
    )
    )

  }


  set.seed(123)
  bayes_tune_test <- doFuture::withDoRNG(ParBayesianOptimization::bayesOpt(
    FUN = test_eval_function, #FUN
    bounds = hyper_grid_domain_list, #Boundaries
    initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
    acq = acq, #Acquisition function to be used
    iters.n = n_iter, #Number of times BO is to be repeated
    iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
    verbose = verbose, #Display msgs?
    parallel = parallel #Parallel?
  ))


  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(abs(bayes_tune_test$scoreSummary$Score)),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric
  )

  #Did results used same range of hyperparameters?
  expect_equal(
    cbind(min_child_weight = bayes_tune_test$scoreSummary$min_child_weight, max_depth = bayes_tune_test$scoreSummary$max_depth,
          subsample = bayes_tune_test$scoreSummary$subsample, colsample_bytree = bayes_tune_test$scoreSummary$colsample_bytree,
          eta = bayes_tune_test$scoreSummary$eta, alpha = bayes_tune_test$scoreSummary$alpha,
          gamma = bayes_tune_test$scoreSummary$gamma, nrounds = bayes_tune_test$scoreSummary$nrounds,
          best_iteration = bayes_tune_test$scoreSummary$best_iteration)
    ,
    cbind(min_child_weight = hyper_tune_results$chosen_eval_metric_validation_current_date$min_child_weight, max_depth = hyper_tune_results$chosen_eval_metric_validation_current_date$max_depth,
          subsample = hyper_tune_results$chosen_eval_metric_validation_current_date$subsample, colsample_bytree = hyper_tune_results$chosen_eval_metric_validation_current_date$colsample_bytree,
          eta = hyper_tune_results$chosen_eval_metric_validation_current_date$eta, alpha = hyper_tune_results$chosen_eval_metric_validation_current_date$alpha,
          gamma = hyper_tune_results$chosen_eval_metric_validation_current_date$gamma, nrounds = hyper_tune_results$chosen_eval_metric_validation_current_date$nrounds,
          best_iteration = hyper_tune_results$chosen_eval_metric_validation_current_date$best_iteration)
  )

  #Check if hyperparameters choice match
  expect_equal(
    unlist(ParBayesianOptimization::getBestPars(bayes_tune_test)),
    hyper_tune_results$optimal_hyper[-9]
  )

  #Check if best_iteration match
  expect_equal(
    bayes_tune_test$scoreSummary$best_iteration[which.max(bayes_tune_test$scoreSummary$Score)],
    as.numeric(hyper_tune_results$optimal_hyper[9])
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    as.numeric(bayes_tune_test$scoreSummary[which.max(bayes_tune_test$scoreSummary$Score),
                                            c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")]),
    as.numeric(hyper_tune_results$validation_eval_metrics_hyper_choice_current_date)
  )

  #######################
})

test_that("Skipped: bayesian_opt: hyper_tuning works for NN (custom_obj = pseudo-huber error) when Parallel = TRUE", {
skip()
  doFuture::registerDoFuture()
  future::plan("multisession")
  #NN2
  ########################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #User inputs
  target_fwd_name = "fwd_premium_3m"
  sb_algorithm = "nn"
  tuning_method = "bayesian_opt"
  chosen_eval_metric = "mphe"
  custom_objective = "pseudo_huber_error"
  split_method = "expanding"
  target_fwd <- 3
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta = 1.2
  quantile_tau = 0.5
  early_stop <- 25
  n_iter <- 4
  k_iter <- 4
  acq <- "ucb"
  init_points = 7
  parallel = TRUE
  verbose <- FALSE
  rebalancing_months <- 6
  hyper_grid_domain_list <- list(regularizer_l1 = c(1, 3),
                                 regularizer_l2 = c(1, 3),
                                 droprate = c(0.5, 0.7),
                                 lr = c(0.05, 0.2),
                                 size_of_batch = c(512L,512L),
                                 number_of_epochs = c(100L, 100L))


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
  backtest_returns_m_xts <- NULL
  benchmark_returns_m_xts <- NULL
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
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
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



  #Sets eval function
  FUN <- set_eval_function(ml_algorithm = sb_algorithm,
                           tuning_method = tuning_method)

  #Hyper tune
  set.seed(123)
  tensorflow::set_random_seed(123)
  hyper_tune_results <- hyper_tune(tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
                                   full_data_training_sample_clean = ts_splits$training$full_data_training_sample_clean,
                                   features_validation_sample = ts_splits$validation$features_validation_sample, target_validation_sample = ts_splits$validation$target_validation_sample,
                                   eval_function = FUN, custom_objective_translated = custom_objective_translated,
                                   chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
                                   chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                                   hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
                                   init_points = init_points, k_iter = k_iter, acq = acq,
                                   keras_architecture_parameters = keras_architecture_parameters,
                                   parallel = parallel,
                                   verbose = verbose
  )




  #Compare hyper tuning via bayesian opt
  test_eval_function <- function(regularizer_l1, regularizer_l2, droprate, lr, number_of_epochs, size_of_batch){
    #Set objects in XGB Format
    features_matrix_train_clean <- ts_splits$training$full_data_training_sample_clean[,-which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training features matrix
    target_vector_train <- ts_splits$training$full_data_training_sample_clean[, which(names(ts_splits$training$full_data_training_sample_clean) == target_fwd_name)] #Get training target vector
    features_validation_sample_clean = ts_splits$validation$features_validation_sample[,-c(1:3)]
    target_vector_validation <- ts_splits$validation$target_validation_sample


    #Fit NN model
    model_nn <- keras::keras_model_sequential()
    model_nn %>%
      keras::layer_dense(units = keras_architecture_parameters$units[1],
                         activation = keras_architecture_parameters$activation[1], #Units and activation may vary by layer
                         input_shape =  ncol(features_matrix_train_clean), #Shape = # of features
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>% #L1 and L2 Regularization
      keras::layer_batch_normalization() %>% #Batch normalization
      keras::layer_dropout(rate = droprate) %>% #Adds dropout

      keras::layer_dense(units = keras_architecture_parameters$units[2],
                         activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>% #Batch normalization
      keras::layer_dropout(rate = droprate) %>% #Adds dropout

      keras::layer_dense(units = 1) #No activation means linear: f(x) = x

    model_nn %>% keras::compile(
      loss = custom_objective_translated,
      optimizer = keras::optimizer_adam(learning_rate = lr),
      metrics = chosen_eval_metric_translated$metric
    )

    fit_nn <- model_nn %>%
      keras::fit(x = as.matrix(features_matrix_train_clean), #Training features
                 y = target_vector_train, #Training label
                 epochs = number_of_epochs, #Number of epochs
                 batch_size = size_of_batch, #Batch size (should be a multiple of 2)
                 verbose = verbose,
                 callbacks = list(keras::callback_early_stopping(monitor = chosen_eval_metric_translated$name,
                                                                 patience = early_stop, #Early stop (nº epochs with no improvement)
                                                                 restore_best_weights = TRUE, #Restore best weights after stopping
                                                                 mode = chosen_eval_metric_translated$mode)), #Min for RMSE, MAE and HUBER
                 validation_data = list(as.matrix(features_validation_sample_clean),target_vector_validation) #Validation data
      )



    #Predict
    pred <- stats::predict(model_nn,#NN model
                           as.matrix(features_validation_sample_clean) #Features val
    )

    #Target
    target <- target_vector_validation

    #Error
    error <- target - pred


    #Calculate eval metrics
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)


    #Return List
    return(list(
      Score = switch(chosen_eval_metric,
                     rss = validation_sample_rss, #RSS
                     cp = validation_sample_cp, #CP
                     rmse = -validation_sample_rmse, #RMSE
                     mae = -validation_sample_mae, #MAE
                     mphe = -validation_sample_mphe, #MPHE
                     mpe = -validation_sample_mpe, #Pinball
                     mape = -validation_sample_mape, #MAPE
                     hr = validation_sample_hr, #Hit Rate
                     mb = validation_sample_mb #Bias:
      ),

      rss = validation_sample_rss, #RSS
      cp = validation_sample_cp, #CP
      rmse = validation_sample_rmse, #RMSE
      mae = validation_sample_mae, #MAE
      mphe = validation_sample_mphe, #MPHE
      mpe = validation_sample_mpe, #Pinball
      mape = validation_sample_mape, #MAPE
      hr = validation_sample_hr, #Hit Rate
      mb = validation_sample_mb,
      best_iteration = which.min(fit_nn$metrics[[chosen_eval_metric_translated$name]])
    )
    )

  }


  set.seed(123)
  tensorflow::set_random_seed(123)
  bayes_tune_test <- doFuture::withDoRNG(ParBayesianOptimization::bayesOpt(
    FUN = test_eval_function, #FUN
    bounds = hyper_grid_domain_list, #Boundaries
    initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
    acq = acq, #Acquisition function to be used
    iters.n = n_iter, #Number of times BO is to be repeated
    iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
    verbose = verbose, #Display msgs?
    parallel = parallel #Parallel?
  )
  )


  #Check if same chosen_eval_metrics are calculated
  expect_equal(
    as.numeric(abs(bayes_tune_test$scoreSummary$Score)),
    hyper_tune_results$chosen_eval_metric_validation_current_date$chosen_eval_metric,
    tolerance = 1e-3
  )

  #Did results used same range of hyperparameters?
  expect_equal(
    cbind(regularizer_l1 = bayes_tune_test$scoreSummary$regularizer_l1, regularizer_l2 = bayes_tune_test$scoreSummary$regularizer_l2,
          droprate = bayes_tune_test$scoreSummary$droprate, lr = bayes_tune_test$scoreSummary$lr,
          size_of_batch = bayes_tune_test$scoreSummary$size_of_batch, number_of_epochs = bayes_tune_test$scoreSummary$number_of_epochs,
          best_iteration = bayes_tune_test$scoreSummary$best_iteration)
    ,
    cbind(regularizer_l1 = hyper_tune_results$chosen_eval_metric_validation_current_date$regularizer_l1, regularizer_l2 = hyper_tune_results$chosen_eval_metric_validation_current_date$regularizer_l2,
          droprate = hyper_tune_results$chosen_eval_metric_validation_current_date$droprate, lr = hyper_tune_results$chosen_eval_metric_validation_current_date$lr,
          size_of_batch = hyper_tune_results$chosen_eval_metric_validation_current_date$size_of_batch, number_of_epochs = hyper_tune_results$chosen_eval_metric_validation_current_date$number_of_epochs,
          best_iteration = hyper_tune_results$chosen_eval_metric_validation_current_date$best_iteration)
  )

  #Check if hyperparameters choice match
  expect_equal(
    unlist(ParBayesianOptimization::getBestPars(bayes_tune_test)),
    hyper_tune_results$optimal_hyper[-7]
  )

  #Check if best_iteration match
  expect_equal(
    bayes_tune_test$scoreSummary$best_iteration[which.max(bayes_tune_test$scoreSummary$Score)],
    as.numeric(hyper_tune_results$optimal_hyper[7])
  )

  #Check if same metrics are calculated for the best hyperparameters
  expect_equal(
    as.numeric(bayes_tune_test$scoreSummary[which.max(bayes_tune_test$scoreSummary$Score),
                                            c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")]),
    as.numeric(hyper_tune_results$validation_eval_metrics_hyper_choice_current_date)
  )

  #######################
  foreach::registerDoSEQ()
  future::plan("sequential")
})
