#Grid Search Strategy
test_that("add_hyperparameter works for grid_search", {
  # Create an initial grid_search object
  grid_search_obj <- create_tuning_strategy(
    tuning_method = "grid_search",
    validation_sample_size = 1000,
    chosen_eval_metric = "rmse"
  )

  # Add hyperparameters to the grid_search_strategy object
  grid_search_obj <- add_hyperparameter(
    grid_search_obj,
    hyperparameter = c("alpha", "lambda.min.ratio"),
    grid = list(c(0.1, 0.2, 0.3), c(0.01, 0.1))
  )

  expect_equal(names(grid_search_obj@hyper_grid_domain@hyperparameter_list), c("alpha", "lambda.min.ratio"))
  expect_equal(grid_search_obj@hyper_grid_domain@hyperparameter_list$alpha, c(0.1, 0.2, 0.3))
  expect_equal(grid_search_obj@hyper_grid_domain@hyperparameter_list$lambda.min.ratio, c(0.01, 0.1))

  # Change hyperparameters to the grid_search_strategy object
  grid_search_obj <- add_hyperparameter(
    grid_search_obj,
    hyperparameter = c("alpha"),
    grid = 0.2
  )

  expect_equal(names(grid_search_obj@hyper_grid_domain@hyperparameter_list), c("lambda.min.ratio", "alpha"))
  expect_equal(grid_search_obj@hyper_grid_domain@hyperparameter_list$alpha, 0.2)
  expect_equal(grid_search_obj@hyper_grid_domain@hyperparameter_list$lambda.min.ratio, c(0.01, 0.1))

})

test_that("add_hyperparameter works for grid_search inside ml_backtest_config", {

  ml_backtest_config <- create_ml_backtest_config(
    ml_algorithm = "glmnet",
    target_fwd_name = "fwd_premium_3m"
  )

  ml_backtest_config <- add_tuning_strategy(
    ml_backtest_config,
    tuning_method = "grid_search",
    chosen_eval_metric = "mphe",
    validation_sample_size = 1000
  )

  ml_backtest_config <- add_hyperparameter(
    ml_backtest_config,
    hyperparameter = c("alpha", "lambda.min.ratio"),
    grid = list(c(0.1, 0.2, 0.3), c(0.01, 0.1))
  )

  expect_equal(names(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list), c("alpha", "lambda.min.ratio"))
  expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$alpha, c(0.1, 0.2, 0.3))
  expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$lambda.min.ratio, c(0.01, 0.1))


  ml_backtest_config <- add_hyperparameter(
    ml_backtest_config,
    hyperparameter = c("alpha", "lambda.min.ratio"),
    grid = list(c(0.5, 0.2, 0.3), c(0.01, 0.1))
  )

  expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$alpha, c(0.5, 0.2, 0.3))

})

test_that("add_hyperparameter throws error for missing grid in grid_search", {
  grid_search_obj <- create_tuning_strategy(
    tuning_method = "grid_search",
    chosen_eval_metric = "mphe",
    validation_sample_size = 1000
  )

  expect_error(add_hyperparameter(grid_search_obj, hyperparameter = "alpha"))
})

test_that("Invalid value for hypers for grid_search", {
  expect_error(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "glmnet",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        validation_sample_size = 10,
        chosen_eval_metric = "rmse"
      ) %>%
      add_hyperparameter(
        hyperparameter = "alpha",
        grid = c(-0.1, 0.5)
      )
  )

  expect_error(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "glmnet",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        validation_sample_size = 10,
        chosen_eval_metric = "rmse"
      ) %>%
      add_hyperparameter(
        hyperparameter = "lambda.min.ratio",
        grid = c(0.5, 1)
      )
  )

  expect_error(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "xgb",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        validation_sample_size = 20,
        chosen_eval_metric = "rmse"
      ) %>%
      add_hyperparameter(
        hyperparameter = "eta",
        grid = c(-0.2, 0.8)
      )
  )

  expect_error(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "rf",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        validation_sample_size = 20,
        chosen_eval_metric = "rmse"
      ) %>%
      add_hyperparameter(
        hyperparameter = "max.depth",
        grid = c(0.2, 0.8)
      ), "max.depth should be a positive integer without decimals"
  )

  expect_error(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "nn",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        validation_sample_size = 20,
        chosen_eval_metric = "rmse"
      ) %>%
      add_hyperparameter(
        hyperparameter = "number_of_epochs",
        grid = c(5, 6.3)
      ), "number_of_epochs should be a positive integer without decimals"
  )

})

#Random Search Strategy
test_that("add_hyperparameter works for random_search", {
  # Create an initial random_search object
  random_search_obj <- create_tuning_strategy(
    tuning_method = "random_search",
    validation_sample_size = 1000,
    chosen_eval_metric = "rmse",
    early_stop = 15,
    n_iter = 20
  )

  # Add hyperparameters for random_search_strategy
  random_search_obj <- add_hyperparameter(
    random_search_obj,
    hyperparameter = c("max_depth", "min_child_weight"),
    distribution_choice = c("uniform", "constant"),
    pars = list(c(min = 1, max = 10), 3)
  )

  expect_equal(names(random_search_obj@hyper_grid_domain@hyperparameter_list), c("max_depth", "min_child_weight"))
  expect_equal(random_search_obj@hyper_grid_domain@hyperparameter_list$max_depth$pars, c(min = 1, max = 10))
  expect_equal(random_search_obj@hyper_grid_domain@hyperparameter_list$min_child_weight$value, 3)
})

test_that("add_hyperparameter works for random_search inside ml_backtest_config", {


  # Create an initial ml_backtest_config object
  ml_backtest_config_obj <- create_ml_backtest_config(
    ml_algorithm = "glmnet",
    custom_objective = "squared_error",
    target_fwd_name = "fwd_premium_3m") %>%

    add_tuning_strategy(
      tuning_method = "random_search",
      validation_sample_size = 10,
      chosen_eval_metric = "rmse",
      n_iter = 20) %>%

    add_hyperparameter(
      hyperparameter = c("alpha", "lambda.min.ratio"),
      distribution_choice = c("uniform", "constant"),
      pars = list(c(min = 0, max = 1), 0.1)
    )

  expect_equal(ml_backtest_config_obj@tuning_strategy@hyper_grid_domain@hyperparameter_list$alpha, list(distribution_choice = "uniform", pars = c(min = 0, max = 1)))
  expect_equal(ml_backtest_config_obj@tuning_strategy@hyper_grid_domain@hyperparameter_list$lambda.min.ratio, list(distribution_choice = "constant", value = c(0.1)))


  #Change hyperparameters

  # Add hyperparameters for random_search_strategy
  ml_backtest_config_obj <- add_hyperparameter(
    ml_backtest_config_obj,
    hyperparameter = c("alpha"),
    distribution_choice = c("constant"),
    pars = 1
  )


  expect_equal(ml_backtest_config_obj@tuning_strategy@hyper_grid_domain@hyperparameter_list$alpha, list(distribution_choice = "constant", value = 1))
  expect_equal(ml_backtest_config_obj@tuning_strategy@hyper_grid_domain@hyperparameter_list$lambda.min.ratio, list(distribution_choice = "constant", value = c(0.1)))

})

test_that("add_hyperparameter throws error for missing distribution_choice in random_search", {
  random_search_obj <- create_tuning_strategy(
    tuning_method = "random_search",
    validation_sample_size = 1000,
    chosen_eval_metric = "rmse",
    n_iter = 20
  )

  expect_error(add_hyperparameter(random_search_obj, hyperparameter = "max_depth", grid = c(1,2,3,4,5)))

})

test_that("Invalid value in random_search for rf", {

  expect_error(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "rf",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "random_search",
        validation_sample_size = 15,
        chosen_eval_metric = "rmse",
        n_iter = 10
      ) %>%
      add_hyperparameter(
        hyperparameter = c("num.trees", "mtry", "max.depth", "min.bucket"),
        distribution_choice = c("uniform", "uniform", "uniform", "uniform"),
        pars = list(
          c(min = 10, max = 100),
          c(min = 0.1, max = 0.9),
          c(min = 1L, max = 5L),
          c(min = 1, max = 10)
        )
      ),
    "pars should be set as integers for num.trees"
  )

  expect_warning(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "rf",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "random_search",
        validation_sample_size = 15,
        chosen_eval_metric = "rmse",
        n_iter = 10
      ) %>%
      add_hyperparameter(
        hyperparameter = c("num.trees", "mtry", "max.depth", "min.bucket"),
        distribution_choice = c("uniform", "uniform", "uniform", "uniform"),
        pars = list(
          c(min = -10L, max = 100L),
          c(min = 0.1, max = 0.9),
          c(min = 1L, max = 5L),
          c(min = 1, max = 10)
        )
      ),
    "min below lower range for num.trees"
  )


  expect_warning(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "glmnet",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "random_search",
        validation_sample_size = 10,
        chosen_eval_metric = "rmse",
        n_iter = 5
      ) %>%
      add_hyperparameter(
        hyperparameter = "alpha",
        distribution_choice = "uniform",
        pars = c(min = 0, max = 1.5)
      ),
    "max above upper range for alpha"
  )

  expect_warning(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "xgb",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "random_search",
        validation_sample_size = 10,
        chosen_eval_metric = "rmse",
        n_iter = 5
      ) %>%
      add_hyperparameter(
        hyperparameter = "eta",
        distribution_choice = "normal",
        pars = c(mean = 0.8, sd = 0.5)
      )
  )


})


#Bayesian Opt Strategy
test_that("add_hyperparameter works for bayesian_opt", {
  # Create an initial bayesian_opt object
  bayesian_opt_obj <- create_tuning_strategy(
    tuning_method = "bayesian_opt",
    validation_sample_size = 1000,
    chosen_eval_metric = "mae",
    n_iter = 50,
    acq = "ei",
    init_points = 5,
    k_iter = 3
  )

  # Add hyperparameters for bayesian_opt_strategy
  bayesian_opt_obj <- add_hyperparameter(
    bayesian_opt_obj,
    hyperparameter = c("mtry", "num.trees"),
    bounds = list(c(1, 10), c(100, 1000))
  )

  expect_equal(names(bayesian_opt_obj@hyper_grid_domain@hyperparameter_list), c("mtry", "num.trees"))
  expect_equal(bayesian_opt_obj@hyper_grid_domain@hyperparameter_list$mtry, c(1, 10))
  expect_equal(bayesian_opt_obj@hyper_grid_domain@hyperparameter_list$num.trees, c(100, 1000))
})

test_that("add_hyperparameter works for bayesian_opt inside ml_backtest_config", {


  ml_backtest_config <- create_ml_backtest_config(
    ml_algorithm = "nn",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2,
    custom_objective = "pseudo_huber_error"
  ) %>%
    add_keras_architecture(
      nn_optimizer = "Adam",
      units = c(32),
      activation = "relu",
      batch_norm_option = c(TRUE)
    ) %>%
    add_tuning_strategy(
      tuning_method = "bayesian_opt",
      chosen_eval_metric = "mphe",
      validation_sample_size = 10,
      early_stop = 20,
      n_iter = 20,
      k_iter = 3,
      init_points = 5,
      acq = "ei"
    ) %>%
    add_hyperparameter(
      hyperparameter = c("regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs"),
      bounds = list(c(1, 3), c(4, 6), c(0.1, 0.3), c(0.001, 0.1), c(64L, 128L), c(20L, 30L))
    )


    expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$regularizer_l1, c(1, 3))
    expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$regularizer_l2, c(4, 6))
    expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$droprate, c(0.1, 0.3))
    expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$lr, c(0.001, 0.1))
    expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$size_of_batch, c(64, 128))
    expect_equal(ml_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list$number_of_epochs, c(20, 30))

    expect_equal(ml_backtest_config@tuning_strategy@chosen_eval_metric, "mphe")
    expect_equal(ml_backtest_config@tuning_strategy@early_stop, 20)
    expect_equal(ml_backtest_config@target_fwd_name, "fwd_return_1m")
    expect_equal(ml_backtest_config@huber_delta, 1.2)
    expect_equal(ml_backtest_config@custom_objective, "pseudo_huber_error")
    expect_equal(ml_backtest_config@keras_architecture_parameters@nn_optimizer, "Adam")
    expect_equal(ml_backtest_config@keras_architecture_parameters@units, c(32))
    expect_equal(ml_backtest_config@keras_architecture_parameters@activation, "relu")
    expect_equal(ml_backtest_config@keras_architecture_parameters@batch_norm_option, c(TRUE))


})

test_that("add_hyperparameter throws error for missing bounds in bayesian_opt", {
  bayesian_opt_obj <- create_tuning_strategy(
    tuning_method = "bayesian_opt",
    chosen_eval_metric = "rmse",
    validation_sample_size = 1000,
    n_iter = 50,
    acq = "ei",
    init_points = 5,
    k_iter = 3
  )

  expect_error(add_hyperparameter(bayesian_opt_obj, hyperparameter = "mtry"))
})

#Metabacktest
test_that("create_metabacktest_config works", {

  ml_backtest_1 <- create_ml_backtest_config(
    ml_algorithm = "rf",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2
  ) %>% add_tuning_strategy(
    tuning_method = "bayesian_opt",
    chosen_eval_metric = "mphe",
    validation_sample_size = 10,
    n_iter = 20,
    k_iter = 3,
    init_points = 5,
    acq = "ei"
  ) %>% add_hyperparameter(
    hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
    bounds = list(c(0, 1), c(100L, 1000L), c(1L, 6L), c(1L, 10L))
  )

  ml_backtest_2 <- create_ml_backtest_config(
    ml_algorithm = "rf",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2
  ) %>% add_tuning_strategy(
    tuning_method = "grid_search",
    chosen_eval_metric = "rmse",
    validation_sample_size = 10
  ) %>% add_hyperparameter(
    hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
    grid = list(c(0, 0.5, 1), c(100, 500, 1000), c(1, 3, 6), c(1, 5, 10))
  )

  ml_backtest_3 <- create_ml_backtest_config(
    ml_algorithm = "xgb",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2
  ) %>% add_tuning_strategy(
    tuning_method = "random_search",
    chosen_eval_metric = "rmse",
    validation_sample_size = 10,
    n_iter = 10
  ) %>% add_hyperparameter(
    hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds"),
    distribution_choice = c("uniform", "uniform", "uniform", "uniform", "uniform", "uniform", "uniform", "uniform"),
    pars = list(c(min = 0, max = 1), c(min = 1L, max = 6L), c(min = 0.5, max = 1), c(min = 0.5, max = 1), c(min = 0.001, max = 0.1),
                c(min = 1, max = 3), c(min = 0, max = 1), c(min = 100L, max = 1000L))
  )

  metabacktest_config <- create_ml_metabacktest_config(
    ml_backtest_configs = list(ml_backtest_1, ml_backtest_2, ml_backtest_3)
  )

  expect_equal(sapply(metabacktest_config@ml_backtest_configs, function(x) x@ml_algorithm) %>% unname(), c("rf", "rf", "xgb"))
  expect_equal(sapply(metabacktest_config@ml_backtest_configs, function(x) x@target_fwd_name) %>% unname(), c("fwd_return_1m", "fwd_return_1m", "fwd_return_1m"))
  expect_equal(sapply(metabacktest_config@ml_backtest_configs, function(x) x@huber_delta) %>% unname(), c(1.2, 1.2, 1.2))
  expect_equal(sapply(metabacktest_config@ml_backtest_configs, function(x) x@tuning_strategy@tuning_method) %>% unname(), c("bayesian_opt", "grid_search", "random_search"))
  expect_equal(sapply(metabacktest_config@ml_backtest_configs, function(x) x@tuning_strategy@chosen_eval_metric) %>% unname(), c("mphe", "rmse", "rmse"))
  expect_equal(sapply(metabacktest_config@ml_backtest_configs, function(x) x@tuning_strategy@validation_sample_size) %>% unname(), c(10, 10, 10))
  expect_equal(names(metabacktest_config@ml_backtest_configs), c("ml_backtest_1", "ml_backtest_2", "ml_backtest_3"))

  expect_equal(c(metabacktest_config@ml_backtest_configs[[1]]@tuning_strategy@n_iter, 0, metabacktest_config@ml_backtest_configs[[3]]@tuning_strategy@n_iter) ,c(20, 0, 10))
  expect_equal(metabacktest_config@ml_backtest_configs[[1]]@tuning_strategy@acq, "ei")
  expect_equal(sapply(metabacktest_config@ml_backtest_configs, function(x) names(x@tuning_strategy@hyper_grid_domain@hyperparameter_list)) %>% unname(),

               list(c("mtry", "num.trees", "max.depth", "min.bucket"), c("mtry", "num.trees", "max.depth", "min.bucket"),
                    c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds")))


})

test_that("create_metabacktest_config works when combining tuning_strategy and ml_backtest_configs", {

  rf_config_1 <- create_ml_backtest_config(
    ml_algorithm = "rf",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2
  )

  rf_config_2 <- create_ml_backtest_config(
    ml_algorithm = "rf",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.5
  )

  xgb_config <- create_ml_backtest_config(
    ml_algorithm = "xgb",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2
  )

  rf_tuning_strategy <- create_tuning_strategy(
    tuning_method = "bayesian_opt",
    chosen_eval_metric = "mphe",
    validation_sample_size = 10,
    n_iter = 20,
    k_iter = 3,
    init_points = 5,
    acq = "ei"
  ) %>% add_hyperparameter(
    hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
    bounds = list(c(0, 1), c(100L, 1000L), c(1L, 6L), c(1L, 10L))
  )

  xgb_tuning_strategy_1 <- create_tuning_strategy(
    tuning_method = "grid_search",
    chosen_eval_metric = "rmse",
    validation_sample_size = 10
  ) %>% add_hyperparameter(
    hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds"),
    grid = list(c(0, 1), c(1, 4), c(0, 0.5), c(0.5, 1), c(0.2,0.3), c(2,3,4,6), gamma = c(10,2,34,5), nrounds = 100)
  )

  xgb_tuning_strategy_2 <- create_tuning_strategy(
    tuning_method = "grid_search",
    chosen_eval_metric = "mphe",
    validation_sample_size = 25
  ) %>% add_hyperparameter(
    hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds"),
    grid = list(c(0, 1), c(1, 4), c(0, 0.5), c(0.5, 1), c(0.2,0.3), c(2,3,4,6), gamma = c(10,2,34,5), nrounds = 100)
  )


  metabacktest_config <- create_ml_metabacktest_config(
    ml_backtest_configs = list(rf_config_1, rf_config_2, xgb_config),
    tuning_strategies = list(rf_tuning_strategy, xgb_tuning_strategy_1 , xgb_tuning_strategy_2)
  )


  expect_equal(length(metabacktest_config@ml_backtest_configs), 4)
  expect_equal(metabacktest_config@ml_backtest_configs[[1]]@ml_algorithm, "rf")
  expect_equal(metabacktest_config@ml_backtest_configs[[2]]@ml_algorithm, "rf")
  expect_equal(metabacktest_config@ml_backtest_configs[[3]]@ml_algorithm, "xgb")
  expect_equal(metabacktest_config@ml_backtest_configs[[4]]@ml_algorithm, "xgb")
  expect_equal(names(metabacktest_config@ml_backtest_configs), c("rf_config_1_rf_tuning_strategy", "rf_config_2_rf_tuning_strategy",
               "xgb_config_xgb_tuning_strategy_1", "xgb_config_xgb_tuning_strategy_2"))


})

test_that("create_metabacktest_config throws an error when trying to add a config with incomplete hyperparameters", {

  ml_backtest_1 <- create_ml_backtest_config(
    ml_algorithm = "rf",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2
  ) %>% add_tuning_strategy(
    tuning_method = "bayesian_opt",
    chosen_eval_metric = "mphe",
    validation_sample_size = 10,
    n_iter = 20,
    k_iter = 3,
    init_points = 5,
    acq = "ei"
  ) %>% add_hyperparameter(
    hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
    bounds = list(c(0, 1), c(100L, 1000L), c(1L, 6L), c(1L, 10L))
  )

  ml_backtest_2 <- create_ml_backtest_config(
    ml_algorithm = "rf",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2
  ) %>% add_tuning_strategy(
    tuning_method = "grid_search",
    chosen_eval_metric = "rmse",
    validation_sample_size = 10
  ) %>% add_hyperparameter(
    hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
    grid = list(c(0, 0.5, 1), c(100, 500, 1000), c(1, 3, 6), c(1, 5, 10))
  )

  ml_backtest_3 <- create_ml_backtest_config(
    ml_algorithm = "xgb",
    target_fwd_name = "fwd_return_1m",
    huber_delta = 1.2
  ) %>% add_tuning_strategy(
    tuning_method = "random_search",
    chosen_eval_metric = "rmse",
    validation_sample_size = 10,
    n_iter = 10
  ) %>% add_hyperparameter(
    hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma"),
    distribution_choice = c("uniform", "uniform", "uniform", "uniform", "uniform", "uniform", "uniform"),
    pars = list(c(min = 0, max = 1), c(min = 1L, max = 6L), c(min = 0.5, max = 1), c(min = 0.5, max = 1), c(min = 0.001, max = 0.1),
                c(min = 1, max = 3), c(min = 0, max = 1))
  )

  expect_error(
  create_ml_metabacktest_config(
    ml_backtest_configs = list(ml_backtest_1, ml_backtest_2, ml_backtest_3)
  ), "In config 3, missing hyperparameters for algorithm 'xgb': nrounds."
  )

})

test_that("ml_metabacktest_config throws an error when target_fwd_name differs", {

  config1 <- create_ml_backtest_config(
    target_fwd_name = "target_1m",
    ml_algorithm = "glmnet"
  ) %>% add_tuning_strategy(
    tuning_method = "grid_search",
    chosen_eval_metric = "mphe",
    validation_sample_size = 1000
  ) %>% add_hyperparameter(
    hyperparameter = c("alpha", "lambda.min.ratio"),
    grid = list(c(0, 0.5, 1), c(0.1, 0.2, 0.9))
  )

  config2 <- create_ml_backtest_config(
    target_fwd_name = "target_2m",
    ml_algorithm = "glmnet"
  ) %>% add_tuning_strategy(
    tuning_method = "grid_search",
    chosen_eval_metric = "mphe",
    validation_sample_size = 1000
  ) %>% add_hyperparameter(
    hyperparameter = c("alpha", "lambda.min.ratio"),
    grid = list(c(0, 0.5, 1), c(0.1, 0.2, 0.9))
  )

  expect_error(
    create_ml_metabacktest_config(
      ml_backtest_configs = list(config1, config2)
    ),
    "The 'target_fwd_name' must match across all 'ml_backtest_config' elements."
  )
})


#General errors
test_that("add_hyperparameters throws an error when choosing wrong hyperparameters, chosen_eval_metric, tuning_method, ml_algorithm, incompatible hyperparameter format,
          wrong custom objective etc", {


  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "glmnet",
      target_fwd_name = "fwd_premium_3m"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        chosen_eval_metric = "mphe",
        validation_sample_size = 1000
      ) %>%
      add_hyperparameter(
        hyperparameter = "max_depth",
        grid = c(0.1, 0.2, 0.3)
      ),  "hyperparameters do not match ml_algorithm choice for 'glmnet'"
  )


  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "glmnet",
      target_fwd_name = "fwd_premium"
    ) ,  "target_fwd_name is not in the right pattern"
  )

  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "glmnet",
      target_fwd_name = "fwd_premium_3m",
      custom_object = "pseudo_huber_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        chosen_eval_metric = "mphe",
        validation_sample_size = 1000
      ) %>%
      add_hyperparameter(
        hyperparameter = "alpha",
        grid = c(0.1, 0.2, 0.3)
      ),  "Custom objectives are only allowed for 'xgb' or 'nn' algorithms."
  )





  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "glmnet",
      target_fwd_name = "fwd_premium_3m",
      quantile_tau = 1.2
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        chosen_eval_metric = "mphe",
        validation_sample_size = 1000
      ) %>%
      add_hyperparameter(
        hyperparameter = "alpha",
        grid = c(0.1, 0.2, 0.3)
      ),  "quantile_tau must be between 0 and 1."
  )


  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "ranger",
      target_fwd_name = "fwd_premium_3m"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        chosen_eval_metric = "mphe",
        validation_sample_size = 1000
      ) %>%
      add_hyperparameter(
        hyperparameter = "max_depth",
        grid = c(0.1, 0.2, 0.3)
      )
  )

  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "rf",
      target_fwd_name = "fwd_premium_3m"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid",
        chosen_eval_metric = "mphe",
        validation_sample_size = 1000
      ) %>%
      add_hyperparameter(
        hyperparameter = "max_depth",
        grid = c(0.1, 0.2, 0.3)
      )
  )


  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "rf",
      target_fwd_name = "fwd_premium_3m"
    ) %>%
      add_tuning_strategy(
        tuning_method = "random_search",
        chosen_eval_metric = "mphe",
        validation_sample_size = 1000,
        n_iter = 10
      ) %>%
      add_hyperparameter(
        hyperparameter = "max_depth",
        distribution_choice = "uniform",
        pars = c(mean = 0.1, sd = 0.2)
      ),  "For 'uniform', pars must contain 'min' and 'max'"
  )

  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "rf",
      target_fwd_name = "fwd_premium_3m"
    ) %>%
      add_tuning_strategy(
        tuning_method = "random_search",
        chosen_eval_metric = "mphe",
        validation_sample_size = 1000,
        n_iter = 10
      ) %>%
      add_hyperparameter(
        hyperparameter = "max_depth",
        grid = c(mean = 0.1, sd = 0.2)
      ),  "All hyperparameters should have matching distribution_choice and pars."
  )

  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "rf",
      target_fwd_name = "fwd_premium_3m"
    ) %>%
      add_tuning_strategy(
        tuning_method = "grid_search",
        chosen_eval_metric = "abracadabra",
        validation_sample_size = 1000
      ) %>%
      add_hyperparameter(
        hyperparameter = "max.depth",
        grid = c(0.2, 0.3)
      ),  "Invalid chosen_eval_metric. Choose from 'rss', 'rmse', 'cp', 'mae', 'mphe', 'mpe', 'mape', 'hr', 'mb'."
  )

})

test_that("add_tuning_strategy throws an error when trying to set early_stop for wrong ml algo", {

  expect_error(
    create_ml_backtest_config(
      ml_algorithm = "rf",
      target_fwd_name = "fwd_return_1m",
      huber_delta = 1.2
    ) %>% add_tuning_strategy(
      tuning_method = "bayesian_opt",
      chosen_eval_metric = "mphe",
      validation_sample_size = 10,
      early_stop = 20,
      n_iter = 20,
      k_iter = 3,
      init_points = 5,
      acq = "ei"
    ), "Invalid early_stop. Early stop is only allowed for 'xgb' or 'nn' algorithms."
  )


})

test_that("Invalid droprate value in bayesian_opt for nn", {
  expect_error(
    create_ml_backtest_config(
      target_fwd_name = "fwd_premium_3m",
      ml_algorithm = "nn",
      custom_objective = "squared_error"
    ) %>%
      add_tuning_strategy(
        tuning_method = "bayesian_opt",
        validation_sample_size = 25,
        chosen_eval_metric = "rmse",
        n_iter = 10,
        k_iter = 5,
        init_points = 3,
        acq = "ucb"
      ) %>%
      add_hyperparameter(
        hyperparameter = "droprate",
        bounds = c(0.0, 1.1)
      )
  )


})

#Keras
test_that("create_keras_architecture_parameters works", {

  keras <- create_keras_architecture(nn_optimizer = "Adam", units = c(32,16), activation = c("relu", "relu"), batch_norm_option = c(TRUE, TRUE))

  expect_equal(keras@units, c(32,16))
  expect_equal(keras@batch_norm_option, c(TRUE,TRUE))
  expect_equal(keras@activation, c("relu","relu"))
  expect_equal(keras@nn_optimizer, "Adam")
  expect_equal(keras@n_layers, 2)

})

test_that("add_keras_architecture_parameters works", {

  keras <- create_keras_architecture(nn_optimizer = "Adam", units = c(32,16), activation = c("relu", "relu"), batch_norm_option = c(TRUE, TRUE))
  keras <- add_keras_layer(keras, units = 8, activation = "tanh", batch_norm_option = FALSE)

  expect_equal(keras@units, c(32,16,8))
  expect_equal(keras@batch_norm_option, c(TRUE,TRUE,FALSE))
  expect_equal(keras@activation, c("relu","relu","tanh"))
  expect_equal(keras@nn_optimizer, "Adam")
  expect_equal(keras@n_layers, 3)

})



###########################################
##add_liquidity_constraint
##########################################

test_that("add_liquidity_constraint works with only liquidity_floor_rule", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_floor_rule = "micro_caps")

  expect_equal(port@liquidity_constraint_policy$liquidity_floor_rule, "micro_caps")

})

test_that("add_liquidity_constraint works with only liquidity_cap_rules", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.05))

  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_classification, "micro_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_cap, 0.05)
  expect_null(port@liquidity_constraint_policy$liquidity_floor_rule)

})


test_that("add_liquidity_constraint works with more than one liquidity_cap_rules", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.05, small_caps = 0.1))

  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_classification, "micro_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_cap, 0.05)
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_classification, "small_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_cap, 0.1)
  expect_null(port@liquidity_constraint_policy$liquidity_floor_rule)

})

test_that("add_liquidity_constraint works with liquidity_floor_rule and liquidity_cap_rules", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.05, small_caps = 0.1), liquidity_floor_rule = "micro_caps")

  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_classification, "micro_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_cap, 0.05)
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_classification, "small_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_cap, 0.1)
  expect_equal(port@liquidity_constraint_policy$liquidity_floor_rule, "micro_caps")

})

test_that("add_liquidity_constraint works when adding new rules", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.05, small_caps = 0.1), liquidity_floor_rule = "micro_caps")
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(mid_caps = 0.5))


  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_classification, "micro_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_cap, 0.05)
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_classification, "small_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_cap, 0.1)
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_3$liquidity_classification, "mid_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_3$liquidity_cap, 0.5)

  expect_equal(port@liquidity_constraint_policy$liquidity_floor_rule, "micro_caps")

})

test_that("add_liquidity_constraint works when overwritting old rule", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.05, small_caps = 0.1))
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.25), liquidity_floor_rule = "small_caps")


  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_classification, "small_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_cap, 0.1)
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_classification, "micro_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_cap, 0.25)

  expect_equal(port@liquidity_constraint_policy$liquidity_floor_rule, "small_caps")

})


test_that("add_liquidity_constraint_policy modifies liquidity_floor_rule while keeping liquidity_cap_rules unchanged", {
  port <- create_portfolio_policies()

  # Initial rules
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.05, small_caps = 0.1), liquidity_floor_rule = "micro_caps")

  # Modify only liquidity_floor_rule
  port <- add_liquidity_constraint_policy(port, liquidity_floor_rule = "small_caps")

  # Check that the liquidity_cap_rules remain the same
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_classification, "micro_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_cap, 0.05)
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_classification, "small_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_cap, 0.1)

  # Check that the liquidity_floor_rule has been updated
  expect_equal(port@liquidity_constraint_policy$liquidity_floor_rule, "small_caps")
})


test_that("add_liquidity_constraint_policy modifies liquidity_cap_rules while keeping liquidity_floor_rule unchanged", {
  port <- create_portfolio_policies()

  # Initial rules
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.05), liquidity_floor_rule = "micro_caps")

  # Modify only liquidity_cap_rules
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(small_caps = 0.1))

  # Check that the liquidity_floor_rule remains the same
  expect_equal(port@liquidity_constraint_policy$liquidity_floor_rule, "micro_caps")

  # Check that the new liquidity_cap_rules are added correctly
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_classification, "micro_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_1$liquidity_cap, 0.05)
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_classification, "small_caps")
  expect_equal(port@liquidity_constraint_policy$liquidity_cap_rule_2$liquidity_cap, 0.1)
})



test_that("add_turnover_constraint works with one rule", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.25), liquidity_floor_rule = "small_caps")
  port <- add_turnover_constraint_policy(port, turnover_rules = list(liquidity_classification = "small_caps", turnover_cap = 0.25, top_stock_quantile_buffer = 0.66))

  expect_equal(port@turnover_constraint_policy$buffer_zone_1,
               list(liquidity_classification = "small_caps", turnover_cap = 0.25, top_stock_quantile_buffer = 0.66))


})


test_that("add_turnover_constraint works with two or more rule", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.25), liquidity_floor_rule = "small_caps")
  port <- add_turnover_constraint_policy(port, turnover_rules = list(
                                           list(liquidity_classification = "small_caps", turnover_cap = 0.25, top_stock_quantile_buffer = 0.66),
                                           list(liquidity_classification = "micro_caps", turnover_cap = 0.10, top_stock_quantile_buffer = 0.66)
                                           ))

  expect_equal(port@turnover_constraint_policy$buffer_zone_1,
               list(liquidity_classification = "small_caps", turnover_cap = 0.25, top_stock_quantile_buffer = 0.66))

  expect_equal(port@turnover_constraint_policy$buffer_zone_2,
               list(liquidity_classification = "micro_caps", turnover_cap = 0.10, top_stock_quantile_buffer = 0.66))

})


test_that("add_turnover_constraint works with adding rule, while overwritting an old one", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.25), liquidity_floor_rule = "small_caps")
  port <- add_turnover_constraint_policy(port, turnover_rules = list(
    list(liquidity_classification = "small_caps", turnover_cap = 0.25, top_stock_quantile_buffer = 0.66),
    list(liquidity_classification = "micro_caps", turnover_cap = 0.10, top_stock_quantile_buffer = 0.66)
  ))


  port <- add_turnover_constraint_policy(port, turnover_rules = list(
    list(liquidity_classification = "small_caps", turnover_cap = 0.20, top_stock_quantile_buffer = 0.75),
    list(liquidity_classification = "mid_caps", turnover_cap = 0.50, top_stock_quantile_buffer = 0.66)
  ))

  expect_equal(port@turnover_constraint_policy$buffer_zone_1,
               list(liquidity_classification = "micro_caps", turnover_cap = 0.1, top_stock_quantile_buffer = 0.66))

  expect_equal(port@turnover_constraint_policy$buffer_zone_2,
               list(liquidity_classification = "small_caps", turnover_cap = 0.2, top_stock_quantile_buffer = 0.75))

  expect_equal(port@turnover_constraint_policy$buffer_zone_3,
               list(liquidity_classification = "mid_caps", turnover_cap = 0.5, top_stock_quantile_buffer = 0.66))


})


test_that("add_turnover_constraint_policy can be used multiple times on the same object", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call: Add the initial turnover constraint policy
  turnover_rules_1 <- list(
    list(liquidity_classification = "micro_caps", turnover_cap = 0.1, top_stock_quantile_buffer = 0.05)
  )
  portfolio_policies_obj <- add_turnover_constraint_policy(portfolio_policies_obj, turnover_rules_1)

  # Second call: Add a new turnover constraint policy
  turnover_rules_2 <- list(
    list(liquidity_classification = "small_caps", turnover_cap = 0.2, top_stock_quantile_buffer = 0.1)
  )
  result <- add_turnover_constraint_policy(portfolio_policies_obj, turnover_rules_2)

  # Ensure the new rules are added correctly
  expect_equal(result@turnover_constraint_policy$buffer_zone_1$liquidity_classification, "micro_caps")
  expect_equal(result@turnover_constraint_policy$buffer_zone_1$turnover_cap, 0.1)
  expect_equal(result@turnover_constraint_policy$buffer_zone_1$top_stock_quantile_buffer, 0.05)
  expect_equal(result@turnover_constraint_policy$buffer_zone_2$liquidity_classification, "small_caps")
  expect_equal(result@turnover_constraint_policy$buffer_zone_2$turnover_cap, 0.2)
  expect_equal(result@turnover_constraint_policy$buffer_zone_2$top_stock_quantile_buffer, 0.1)
})

test_that("add_turnover_constraint_policy modifies one argument while keeping others unchanged", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call: Add the initial turnover constraint policy
  turnover_rules <- list(
    list(liquidity_classification = "micro_caps", turnover_cap = 0.1, top_stock_quantile_buffer = 0.05)
  )
  portfolio_policies_obj <- add_turnover_constraint_policy(portfolio_policies_obj, turnover_rules)


  turnover_rules_modified <- list(
    list(liquidity_classification = "small_caps", turnover_cap = 0.15, top_stock_quantile_buffer = 0.10)  # Only modify turnover_cap
  )
  result <- add_turnover_constraint_policy(portfolio_policies_obj, turnover_rules_modified)

  # Ensure that turnover_cap is updated but top_stock_quantile_buffer remains the same
  expect_equal(result@turnover_constraint_policy$buffer_zone_1$liquidity_classification, "micro_caps")
  expect_equal(result@turnover_constraint_policy$buffer_zone_1$turnover_cap, 0.1)
  expect_equal(result@turnover_constraint_policy$buffer_zone_1$top_stock_quantile_buffer, 0.05)

  # Ensure that turnover_cap is updated but top_stock_quantile_buffer remains the same
  expect_equal(result@turnover_constraint_policy$buffer_zone_2$liquidity_classification, "small_caps")
  expect_equal(result@turnover_constraint_policy$buffer_zone_2$turnover_cap, 0.15)
  expect_equal(result@turnover_constraint_policy$buffer_zone_2$top_stock_quantile_buffer, 0.10)

})



test_that("add_turnover_constraint_policy throws an error for invalid liquidity_classification", {
  portfolio_policies_obj <- create_portfolio_policies()

  turnover_rules <- list(
    list(liquidity_classification = "invalid_class", turnover_cap = 0.1, top_stock_quantile_buffer = 0.05)
  )

  expect_error(
    add_turnover_constraint_policy(portfolio_policies_obj, turnover_rules),
    "Error: liquidity_classification must be one of: micro_caps, small_caps, mid_caps, large_caps, mega_caps."
  )
})

test_that("add_turnover_constraint_policy throws an error for invalid turnover_cap and top_stock_quantile_buffer", {
  portfolio_policies_obj <- create_portfolio_policies()

  # Invalid turnover_cap
  turnover_rules <- list(
    list(liquidity_classification = "micro_caps", turnover_cap = "invalid", top_stock_quantile_buffer = 0.05)
  )

  expect_error(
    add_turnover_constraint_policy(portfolio_policies_obj, turnover_rules),
    "Error: turnover_cap must be numeric"
  )

  # Invalid top_stock_quantile_buffer
  turnover_rules <- list(
    list(liquidity_classification = "micro_caps", turnover_cap = 0.1, top_stock_quantile_buffer = "invalid")
  )

  expect_error(
    add_turnover_constraint_policy(portfolio_policies_obj, turnover_rules),
    "Error: top_stock_quantile_buffer must be numeric"
  )
})



test_that("add_concentration_constraint works with adding rule", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.25), liquidity_floor_rule = "small_caps")
  port <- add_turnover_constraint_policy(port, turnover_rules = list(
    list(liquidity_classification = "small_caps", turnover_cap = 0.25, top_stock_quantile_buffer = 0.66),
    list(liquidity_classification = "micro_caps", turnover_cap = 0.10, top_stock_quantile_buffer = 0.66)
  ))
  port <- add_concentration_constraint_policy(port, benchmark = "IBOV", max_abs_active_individual_weight = 0.02, max_abs_active_group_weight = c(Sector = 0.10, Subsector = 0.50))

  expect_equal(port@concentration_constraint_policy$max_abs_active_group_weight, c(Sector = 0.10, Subsector = 0.50))
  expect_equal(port@concentration_constraint_policy$max_abs_active_individual_weight, 0.02)
  expect_equal(port@concentration_constraint_policy$benchmark, "IBOV")

})

test_that("add_concentration_constraint_policy works with valid inputs", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_concentration_constraint_policy(
    portfolio_policies_obj,
    benchmark = "IBOV",
    max_abs_active_individual_weight = 0.05,
    max_abs_active_group_weight = c(Sector = 0.1, Subsector = 0.5)
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@concentration_constraint_policy$benchmark, "IBOV")
  expect_equal(result@concentration_constraint_policy$max_abs_active_individual_weight, 0.05)
  expect_equal(result@concentration_constraint_policy$max_abs_active_group_weight, c(Sector = 0.1, Subsector = 0.5))
})


test_that("add_concentration_constraint_policy throws an error for invalid benchmark type", {
  portfolio_policies_obj <- create_portfolio_policies()

  expect_error(
    add_concentration_constraint_policy(
      portfolio_policies_obj,
      benchmark = 123,  # Invalid type
      max_abs_active_individual_weight = 0.05,
      max_abs_active_group_weight = c(Sector = 0.1, Subsector = 0.5)
    ),
    "benchmark should be a character"
  )
})



test_that("add_concentration_constraint works when overwritting an old one", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.25), liquidity_floor_rule = "small_caps")
  port <- add_turnover_constraint_policy(port, turnover_rules = list(
    list(liquidity_classification = "small_caps", turnover_cap = 0.25, top_stock_quantile_buffer = 0.66),
    list(liquidity_classification = "micro_caps", turnover_cap = 0.10, top_stock_quantile_buffer = 0.66)
  ))
  port <- add_concentration_constraint_policy(port, benchmark = "IBOV", max_abs_active_individual_weight = 0.02, max_abs_active_group_weight = c(Sector = 0.10, Subsector = 0.50))
  port <- add_concentration_constraint_policy(port, benchmark = "SMLL", max_abs_active_individual_weight = 0.03, max_abs_active_group_weight = c(Setor = 0.20))

  expect_equal(port@concentration_constraint_policy$max_abs_active_group_weight, c(Setor = 0.20))
  expect_equal(port@concentration_constraint_policy$max_abs_active_individual_weight, 0.03)
  expect_equal(port@concentration_constraint_policy$benchmark, "SMLL")

})


test_that("add_concentration_constraint_policy preserves existing policies", {
  portfolio_policies_obj <- create_portfolio_policies()

  # Add a signal selection policy
  portfolio_policies_obj@signal_selection_policy <- list(chosen_signals = c("signal_1"))

  # Add concentration constraint policy
  result <- add_concentration_constraint_policy(
    portfolio_policies_obj,
    benchmark = "IBOV",
    max_abs_active_individual_weight = 0.05,
    max_abs_active_group_weight = c(Sector = 0.1, Subsector = 0.5)
  )

  # Ensure concentration constraint policy is added correctly
  expect_equal(result@concentration_constraint_policy$benchmark, "IBOV")
  expect_equal(result@concentration_constraint_policy$max_abs_active_individual_weight, 0.05)
  expect_equal(result@concentration_constraint_policy$max_abs_active_group_weight, c(Sector = 0.1, Subsector = 0.5))

  # Ensure existing signal selection policy is preserved
  expect_equal(result@signal_selection_policy$chosen_signals, c("signal_1"))
})


test_that("add_concentration_constraint_policy can be used multiple times on the same object", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call: Add the initial concentration constraint policy
  portfolio_policies_obj <- add_concentration_constraint_policy(
    portfolio_policies_obj,
    benchmark = "IBOV",
    max_abs_active_individual_weight = 0.05,
    max_abs_active_group_weight = c(Sector = 0.1)
  )

  # Second call: Update the policy with new values
  result <- add_concentration_constraint_policy(
    portfolio_policies_obj,
    benchmark = "SP500",  # New benchmark
    max_abs_active_individual_weight = 0.1,  # Updated individual weight
    max_abs_active_group_weight = c(Sector = 0.2, Subsector = 0.3)  # Updated group weights
  )

  # Ensure the new values overwrite the old ones
  expect_equal(result@concentration_constraint_policy$benchmark, "SP500")
  expect_equal(result@concentration_constraint_policy$max_abs_active_individual_weight, 0.1)
  expect_equal(result@concentration_constraint_policy$max_abs_active_group_weight, c(Sector = 0.2, Subsector = 0.3))
})


test_that("add_signal_selection_policy works", {

  port <- create_portfolio_policies()
  port <- add_liquidity_constraint_policy(port, liquidity_cap_rules = c(micro_caps = 0.25), liquidity_floor_rule = "small_caps")
  port <- add_turnover_constraint_policy(port, turnover_rules = list(
    list(liquidity_classification = "small_caps", turnover_cap = 0.25, top_stock_quantile_buffer = 0.66),
    list(liquidity_classification = "micro_caps", turnover_cap = 0.10, top_stock_quantile_buffer = 0.66)
  ))
  port <- add_concentration_constraint_policy(port, benchmark = "IBOV", max_abs_active_individual_weight = 0.02, max_abs_active_group_weight = c(Sector = 0.10, Subsector = 0.50))
  port <- add_signal_selection_policy(port, chosen_signals = c("eps_yield", "roe_12m", "g_eps_36m", "sharpe_6m", "vol_36m"),
                                            signal_positions = c("long", "long", "long", "long", "short"),
                                            signal_blending_method = "SW",
                                            chosen_sb_metric = "IR",
                                            signal_significance_threshold = 0.05
                                      )

  expect_equal(port@signal_selection_policy$chosen_signals,  c("eps_yield", "roe_12m", "g_eps_36m", "sharpe_6m", "vol_36m"))
  expect_equal(port@signal_selection_policy$signal_positions,  c("long", "long", "long", "long", "short"))
  expect_equal(port@signal_selection_policy$signal_blending_method,  "SW")
  expect_equal(port@signal_selection_policy$chosen_sb_metric, "IR")
  expect_equal(port@signal_selection_policy$signal_significance_threshold, 0.05)

})



# Test: Valid Inputs with `p_correction_method = "bayesian"` and Valid `priors_type`
test_that("add_signal_selection_policy works with bayesian p_correction_method and valid priors_type", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long"),
    p_correction_method = "bayesian",
    priors_type = "uninformative"
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$p_correction_method, "bayesian")
  expect_equal(result@signal_selection_policy$priors_type, "uninformative")
})

# Test: Invalid `priors_informative_data` with `p_correction_method = "bayesian"`
test_that("add_signal_selection_policy throws error when priors_informative_data is missing for bayesian p_correction_method and valid priors_type", {
  portfolio_policies_obj <- create_portfolio_policies()

  # Expect error when priors_informative_data is NULL with bayesian p_correction_method and valid priors_type
  expect_error(add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long"),
    p_correction_method = "bayesian",
    priors_type = "all"  # Valid priors_type that requires priors_informative_data
  ), "priors_informative_data can't be NULL if p_correction_method is bayesian and priors_type is not uninformative or user")
})

# Test: Valid `priors_informative_data` with `p_correction_method = "bayesian"` and Valid `priors_type`
test_that("add_signal_selection_policy works with bayesian p_correction_method and valid priors_informative_data", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long"),
    p_correction_method = "bayesian",
    priors_type = "all",
    priors_informative_data = "jkp_emerging"
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$p_correction_method, "bayesian")
  expect_equal(result@signal_selection_policy$priors_type, "all")
  expect_equal(result@signal_selection_policy$priors_informative_data, "jkp_emerging")
})

# Test: Default `p_correction_method`
test_that("add_signal_selection_policy uses default p_correction_method when not provided", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long")
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$p_correction_method, "none")
})

# Test: Partial Inputs Provided (Mix of User Inputs and Defaults)
test_that("add_signal_selection_policy uses defaults for missing arguments", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long")
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$chosen_signals, c("signal_1"))
  expect_equal(result@signal_selection_policy$signal_positions, c("long"))
  expect_equal(result@signal_selection_policy$p_correction_method, "none")
  expect_equal(result@signal_selection_policy$signal_significance_threshold, 0.05)
  expect_equal(result@signal_selection_policy$sb_benchmark_weighting_method, "theme_sb")
})

# Test: No Arguments Provided (Should Use Defaults)
test_that("add_signal_selection_policy uses defaults when no arguments are provided", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_signal_selection_policy(portfolio_policies_obj)

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$p_correction_method, "none")
  expect_equal(result@signal_selection_policy$sb_benchmark_weighting_method, "theme_sb")
  expect_equal(result@signal_selection_policy$signal_significance_threshold, 0.05)
})

# Test: Invalid `chosen_signals` (Not a Character Vector)
test_that("add_signal_selection_policy throws error for invalid chosen_signals", {
  portfolio_policies_obj <- create_portfolio_policies()

  expect_error(add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = 123
  ), "chosen_signals should be a character vector.")
})

# Test: Invalid `signal_positions` Length Mismatch
test_that("add_signal_selection_policy throws error for length mismatch between signal_positions and chosen_signals", {
  portfolio_policies_obj <- create_portfolio_policies()

  expect_error(add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1", "signal_2"),
    signal_positions = c("long")
  ), "lengths of signal_positions and chosen_signals should match.")
})

# Test: Incomplete Policy in `portfolio_policies_obj`
test_that("add_signal_selection_policy updates and preserves existing policies in portfolio_policies_obj", {
  portfolio_policies_obj <- create_portfolio_policies()
  portfolio_policies_obj@signal_selection_policy <- list(
    chosen_signals = c("signal_1"),
    p_correction_method = "holm"
  )

  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    signal_positions = c("long")
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$chosen_signals, c("signal_1"))
  expect_equal(result@signal_selection_policy$p_correction_method, "holm")
  expect_equal(result@signal_selection_policy$signal_positions, c("long"))
  expect_equal(result@signal_selection_policy$signal_significance_threshold, 0.05)
})

# Test: Custom Default `p_correction_method` from `portfolio_policies_obj`
test_that("add_signal_selection_policy uses existing p_correction_method from portfolio_policies_obj", {
  portfolio_policies_obj <- create_portfolio_policies()
  portfolio_policies_obj@signal_selection_policy <- list(
    p_correction_method = "holm"
  )

  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long")
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$p_correction_method, "holm")
})

# Test: No `priors_type` Provided with `p_correction_method = "bayesian"`
test_that("add_signal_selection_policy defaults priors_type to uninformative if not provided with bayesian p_correction_method", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long"),
    p_correction_method = "bayesian"
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$priors_type, "uninformative")
})

test_that("add_signal_selection_policy updates chosen_signals without affecting other fields in the second call", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call
  portfolio_policies_obj <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long"),
    p_correction_method = "holm"
  )

  # Second call
  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_2", "signal_3"),
    signal_positions = c("long", "long")
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$chosen_signals, c("signal_2", "signal_3"))
  expect_equal(result@signal_selection_policy$signal_positions, c("long", "long"))  # Unchanged
  expect_equal(result@signal_selection_policy$p_correction_method, "holm")  # Unchanged
})



test_that("add_signal_selection_policy updates signal_positions without affecting other fields in the second call", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call
  portfolio_policies_obj <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("short"),
    p_correction_method = "holm"
  )

  # Second call
  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    signal_positions = c("long")
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$signal_positions, c("long"))
  expect_equal(result@signal_selection_policy$chosen_signals, c("signal_1"))  # Unchanged
  expect_equal(result@signal_selection_policy$p_correction_method, "holm")  # Unchanged
})

test_that("add_signal_selection_policy updates p_correction_method without affecting other fields in the second call", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call
  portfolio_policies_obj <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long"),
    p_correction_method = "holm"
  )

  # Second call
  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    p_correction_method = "bayesian"
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$p_correction_method, "bayesian")
  expect_equal(result@signal_selection_policy$chosen_signals, c("signal_1"))  # Unchanged
  expect_equal(result@signal_selection_policy$signal_positions, c("long"))  # Unchanged
})

test_that("add_signal_selection_policy updates signal_blending_method and chosen_sb_metric in the second call", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call
  portfolio_policies_obj <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long"),
    signal_blending_method = "EW"
  )

  # Second call
  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    signal_blending_method = "SW",
    chosen_sb_metric = "alpha"
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$signal_blending_method, "SW")
  expect_equal(result@signal_selection_policy$chosen_sb_metric, "alpha")
  expect_equal(result@signal_selection_policy$chosen_signals, c("signal_1"))  # Unchanged
  expect_equal(result@signal_selection_policy$signal_positions, c("long"))  # Unchanged
})

test_that("add_signal_selection_policy updates max_abs_active_individual_weight and enforces MTO signal_blending_method in the second call", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call
  portfolio_policies_obj <- add_signal_selection_policy(
    portfolio_policies_obj,
    signal_blending_method = "SW",
    chosen_sb_metric = "alpha"

  )

  # Second call
  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    signal_blending_method = "MTO",
    max_abs_active_individual_weight = 0.05
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$signal_blending_method, "MTO")
  expect_equal(result@signal_selection_policy$max_abs_active_individual_weight, 0.05)
})


test_that("add_signal_selection_policy updates priors_type and priors_informative_data with bayesian p_correction_method in the second call", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call
  portfolio_policies_obj <- add_signal_selection_policy(
    portfolio_policies_obj,
    p_correction_method = "bayesian"
  )

  # Second call
  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    priors_type = "all",
    priors_informative_data = "jkp_emerging"
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$p_correction_method, "bayesian")
  expect_equal(result@signal_selection_policy$priors_type, "all")
  expect_equal(result@signal_selection_policy$priors_informative_data, "jkp_emerging")
})


test_that("add_signal_selection_policy updates data_availability_cutoff without affecting other fields in the second call", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call
  portfolio_policies_obj <- add_signal_selection_policy(
    portfolio_policies_obj,
    chosen_signals = c("signal_1"),
    signal_positions = c("long"),
    data_availability_cutoff = 30
  )

  # Second call
  result <- add_signal_selection_policy(
    portfolio_policies_obj,
    data_availability_cutoff = 60
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(result@signal_selection_policy$data_availability_cutoff, 60)
  expect_equal(result@signal_selection_policy$chosen_signals, c("signal_1"))  # Unchanged
  expect_equal(result@signal_selection_policy$signal_positions, c("long"))  # Unchanged
})



test_that("add_liquidity_floor_cutoffs works with valid inputs", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(1, 5, 10, 50, 100)
  )

  expect_s4_class(result, "portfolio_policies")
  expect_equal(unname(result@liquidity_floor_cutoffs$micro_caps["mean_volfin_3m"]), 1)
  expect_equal(unname(result@liquidity_floor_cutoffs$small_caps["mean_volfin_3m"]), 5)
  expect_equal(unname(result@liquidity_floor_cutoffs$mid_caps["mean_volfin_3m"]), 10)
  expect_equal(unname(result@liquidity_floor_cutoffs$large_caps["mean_volfin_3m"]), 50)
  expect_equal(unname(result@liquidity_floor_cutoffs$mega_caps["mean_volfin_3m"]), 100)

  # Ensure numeric values are stored
  expect_type(result@liquidity_floor_cutoffs$micro_caps, "double")
  expect_type(result@liquidity_floor_cutoffs$small_caps, "double")
  expect_type(result@liquidity_floor_cutoffs$mid_caps, "double")
  expect_type(result@liquidity_floor_cutoffs$large_caps, "double")
  expect_type(result@liquidity_floor_cutoffs$mega_caps, "double")
})


test_that("add_liquidity_floor_cutoffs throws an error for non-numeric cutoffs", {
  portfolio_policies_obj <- create_portfolio_policies()

  expect_error(add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c("a", "b", "c", "d", "e")
  ), "cutoffs must be a numeric vector of length 5")
})

test_that("add_liquidity_floor_cutoffs throws an error for negative or zero cutoff values", {
  portfolio_policies_obj <- create_portfolio_policies()

  expect_error(add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(-1, 5, 10, 50, 100)
  ), "cutoffs must be a numeric vector of length 5, and all values must be positive")
})

test_that("add_liquidity_floor_cutoffs updates an existing liquidity metric", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call: add initial metric and cutoffs
  portfolio_policies_obj <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(1, 5, 10, 50, 100)
  )

  # Second call: update the same metric with new cutoffs
  result <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(2, 6, 11, 51, 101)
  )

  expect_equal(unname(result@liquidity_floor_cutoffs$micro_caps["mean_volfin_3m"]), 2)
  expect_equal(unname(result@liquidity_floor_cutoffs$small_caps["mean_volfin_3m"]), 6)
  expect_equal(unname(result@liquidity_floor_cutoffs$mid_caps["mean_volfin_3m"]), 11)
  expect_equal(unname(result@liquidity_floor_cutoffs$large_caps["mean_volfin_3m"]), 51)
  expect_equal(unname(result@liquidity_floor_cutoffs$mega_caps["mean_volfin_3m"]), 101)
})


test_that("add_liquidity_floor_cutoffs adds multiple liquidity metrics", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First call: add the first liquidity metric
  portfolio_policies_obj <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(1, 5, 10, 50, 100)
  )

  # Second call: add a second liquidity metric
  result <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "presence",
    cutoffs = c(97.5, 99, 100, 100, 100)
  )

  expect_s4_class(result, "portfolio_policies")

  # Check values for both metrics
  expect_equal(unname(result@liquidity_floor_cutoffs$micro_caps["mean_volfin_3m"]), 1)
  expect_equal(unname(result@liquidity_floor_cutoffs$micro_caps["presence"]), 97.5)

  expect_equal(unname(result@liquidity_floor_cutoffs$small_caps["mean_volfin_3m"]), 5)
  expect_equal(unname(result@liquidity_floor_cutoffs$small_caps["presence"]), 99)

  expect_equal(unname(result@liquidity_floor_cutoffs$mid_caps["mean_volfin_3m"]), 10)
  expect_equal(unname(result@liquidity_floor_cutoffs$mid_caps["presence"]), 100)

  expect_equal(unname(result@liquidity_floor_cutoffs$large_caps["mean_volfin_3m"]), 50)
  expect_equal(unname(result@liquidity_floor_cutoffs$large_caps["presence"]), 100)

  expect_equal(unname(result@liquidity_floor_cutoffs$mega_caps["mean_volfin_3m"]), 100)
  expect_equal(unname(result@liquidity_floor_cutoffs$mega_caps["presence"]), 100)

  # Ensure numeric values are stored for both metrics
  expect_type(result@liquidity_floor_cutoffs$micro_caps, "double")
  expect_type(result@liquidity_floor_cutoffs$small_caps, "double")
})


test_that("add_liquidity_floor_cutoffs initializes empty liquidity_floor_cutoffs", {
  portfolio_policies_obj <- create_portfolio_policies()

  result <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(1, 5, 10, 50, 100)
  )

  expect_s4_class(result, "portfolio_policies")

  # Ensure all market cap categories are initialized
  expect_true(!is.null(result@liquidity_floor_cutoffs$micro_caps))
  expect_true(!is.null(result@liquidity_floor_cutoffs$small_caps))
  expect_true(!is.null(result@liquidity_floor_cutoffs$mid_caps))
  expect_true(!is.null(result@liquidity_floor_cutoffs$large_caps))
  expect_true(!is.null(result@liquidity_floor_cutoffs$mega_caps))
})

test_that("add_liquidity_floor_cutoffs adds two metrics simultaneously", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First metric
  portfolio_policies_obj <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(1, 5, 10, 50, 100)
  )

  # Second metric
  result <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "presence",
    cutoffs = c(97.5, 99, 100, 100, 100)
  )

  # Check the first metric
  expect_equal(unname(result@liquidity_floor_cutoffs$micro_caps["mean_volfin_3m"]), 1)
  expect_equal(unname(result@liquidity_floor_cutoffs$small_caps["mean_volfin_3m"]), 5)
  expect_equal(unname(result@liquidity_floor_cutoffs$mid_caps["mean_volfin_3m"]), 10)
  expect_equal(unname(result@liquidity_floor_cutoffs$large_caps["mean_volfin_3m"]), 50)
  expect_equal(unname(result@liquidity_floor_cutoffs$mega_caps["mean_volfin_3m"]), 100)

  # Check the second metric
  expect_equal(unname(result@liquidity_floor_cutoffs$micro_caps["presence"]), 97.5)
  expect_equal(unname(result@liquidity_floor_cutoffs$small_caps["presence"]), 99)
  expect_equal(unname(result@liquidity_floor_cutoffs$mid_caps["presence"]), 100)
  expect_equal(unname(result@liquidity_floor_cutoffs$large_caps["presence"]), 100)
  expect_equal(unname(result@liquidity_floor_cutoffs$mega_caps["presence"]), 100)
})


test_that("add_liquidity_floor_cutoffs updates one metric without affecting the other", {
  portfolio_policies_obj <- create_portfolio_policies()

  # First metric
  portfolio_policies_obj <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(1, 5, 10, 50, 100)
  )

  # Second metric
  portfolio_policies_obj <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "presence",
    cutoffs = c(97.5, 99, 100, 100, 100)
  )

  # Update the first metric
  result <- add_liquidity_floor_cutoffs(
    portfolio_policies_obj,
    liquidity_metric = "mean_volfin_3m",
    cutoffs = c(2, 6, 11, 51, 101)
  )

  # Check that the first metric has been updated
  expect_equal(unname(result@liquidity_floor_cutoffs$micro_caps["mean_volfin_3m"]), 2)
  expect_equal(unname(result@liquidity_floor_cutoffs$small_caps["mean_volfin_3m"]), 6)
  expect_equal(unname(result@liquidity_floor_cutoffs$mid_caps["mean_volfin_3m"]), 11)
  expect_equal(unname(result@liquidity_floor_cutoffs$large_caps["mean_volfin_3m"]), 51)
  expect_equal(unname(result@liquidity_floor_cutoffs$mega_caps["mean_volfin_3m"]), 101)

  # Check that the second metric remains unchanged
  expect_equal(unname(result@liquidity_floor_cutoffs$micro_caps["presence"]), 97.5)
  expect_equal(unname(result@liquidity_floor_cutoffs$small_caps["presence"]), 99)
  expect_equal(unname(result@liquidity_floor_cutoffs$mid_caps["presence"]), 100)
  expect_equal(unname(result@liquidity_floor_cutoffs$large_caps["presence"]), 100)
  expect_equal(unname(result@liquidity_floor_cutoffs$mega_caps["presence"]), 100)
})

