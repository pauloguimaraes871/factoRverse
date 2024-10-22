#Hyper grid domain
test_that("create_hyper_grid_domain works", {

  expect_no_error(create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet"))

  expect_no_error(
    create_hyper_grid_domain(tuning_method = "random_search",
                             ml_algorithm = "glmnet",
                             hyperparameter = c("alpha", "lambda.min.ratio"),
                             distribution_choice = c("uniform", "uniform"),
                             pars = list(c(min = 0, max = 1), c(min = 0, max = 1))
    )
  )

  hyper_grid_domain <- create_hyper_grid_domain(
    ml_algorithm = "glmnet",
    tuning_method = "random_search",
    hyperparameter = c("alpha", "lambda.min.ratio"),
    distribution_choice = c("uniform", "uniform"),
    pars = list(c(min = 0, max = 1), c(min = 0, max = 0.9))
  )

  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, list(distribution_choice = "uniform", pars = c(min = 0, max = 1)))
  expect_equal(hyper_grid_domain@hyperparameter_list$lambda.min.ratio, list(distribution_choice = "uniform", pars = c(min = 0, max = 0.9)))


  hyper_grid_domain <- create_hyper_grid_domain(tuning_method = "grid_search", ml_algorithm = "glmnet",
                                                hyperparameter = c("alpha", "lambda.min.ratio"),
                                                grid = list(c(0.1, 0.5, 0.9), c(0.1, 0.75, 0.9))
                                                )

  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, c(0.1, 0.5, 0.9))
  expect_equal(hyper_grid_domain@hyperparameter_list$lambda.min.ratio, c(0.1, 0.75, 0.9))


  hyper_grid_domain <- create_hyper_grid_domain(tuning_method = "bayesian_opt", ml_algorithm = "glmnet",
                                                hyperparameter = c("alpha", "lambda.min.ratio"),
                                                bounds = list(c(0.1, 0.5), c(0.1, 0.9))
  )

  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, c(0.1, 0.5))
  expect_equal(hyper_grid_domain@hyperparameter_list$lambda.min.ratio, c(0.1, 0.9))

  hyper_grid_domain <- create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet",
                                                hyperparameter = c("alpha", "lambda.min.ratio"),
                                                distribution_choice = c("uniform", "constant"),
                                                pars = list(c(min = 0, max = 1), c(value = 3))
                                                )

  expect_equal(hyper_grid_domain@hyperparameter_list, list(alpha = list(distribution_choice = "uniform", pars = c(min=0,max=1)),
                                                           lambda.min.ratio = list(distribution_choice = "constant", value = 3))
               )


})

test_that("add_hyperparameters works to add and overwrite a hyper_grid_domain obj", {

  hyper_grid_domain <- create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet",
                                                hyperparameter = "alpha",
                                                distribution_choice = "uniform",
                                                pars = c(min = 0, max = 1)
                                                )

  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain, hyperparameter = "alpha",
                                          distribution_choice = "uniform",
                                          pars = c(min = 0, max = 0.75)
                                          )


  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, list(distribution_choice = "uniform", pars = c(min = 0, max = 0.75)))

  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain, hyperparameter = "alpha", distribution_choice = "uniform", pars = c(min = 0, max = 0.50))
  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain, hyperparameter = "lambda.min.ratio", distribution_choice = "uniform", pars = c(min = 0, max = 0.70))

  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, list(distribution_choice = "uniform", pars = c(min = 0, max = 0.50)))
  expect_equal(hyper_grid_domain@hyperparameter_list$lambda.min.ratio, list(distribution_choice = "uniform", pars = c(min = 0, max = 0.70)))


  hyper_grid_domain <- create_hyper_grid_domain(tuning_method = "grid_search", ml_algorithm = "rf",
                                                hyperparameter = c("mtry", "num.trees", "max.depth"),
                                                grid = list(2, c(1,2,3), c(2,3,2,4))
  )

  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain, hyperparameter = c("mtry","min.bucket"),
                                          grid = list(c(2,3,4), c(1,2,3,2,5))
  )
  expect_equal(hyper_grid_domain@hyperparameter_list$min.bucket, c(1,2,3,2,5))
  expect_equal(hyper_grid_domain@hyperparameter_list$mtry, c(2,3,4))
  expect_equal(hyper_grid_domain@hyperparameter_list$num.trees, c(1,2,3))
  expect_equal(hyper_grid_domain@hyperparameter_list$max.depth, c(2,3,2,4))


  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain, hyperparameter = c("min.bucket"),
                                          grid = c(22,3,5)
  )
  expect_equal(hyper_grid_domain@hyperparameter_list$num.trees, c(1,2,3))

})

test_that("add_hyperparameters throws an error when choosing wrong hyperparameters, tuning_method or ml_algorithm or when adding an incompatible hyperparameter format", {

  expect_error(create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet",
                                                hyperparameter = "max_depth",
                                                distribution_choice = "uniform",
                                                pars = c(min = 0, max = 1)),
               "hyperparameters do not match ml_algorithm choice for 'glmnet'")

  expect_error(create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "ranger",
                                        hyperparameter = "max_depth",
                                        distribution_choice = "uniform",
                                        pars = c(min = 0, max = 1)),
               "Invalid choice for ml_algorithm. Should be one of glmnet, rf, xgb or nn."
               )



  expect_error(create_hyper_grid_domain(tuning_method = "random", ml_algorithm = "glmnet",
                                        hyperparameter = "alpha",
                                        distribution_choice = "uniform",
                                        pars = c(min = 0, max = 1)),
               "Invalid tuning_method. Only 'grid_search', 'random_search', and 'bayesian_opt' are supported."
               )

  expect_error(create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet",
                                        hyperparameter = "alpha",
                                        distribution_choice = "uniform",
                                        pars = c(a = 0, max = 1)),
               "For 'uniform', pars must contain 'min' and 'max'."
               )

  hyper_grid <- create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "rf",
                                         hyperparameter = "max.depth",
                                         distribution_choice = "uniform",
                                         pars = c(min = 0, max = 1))

   expect_error(add_hyperparameter(hyper_grid, hyperparameter = "lambda.min.ratio", grid = c(1,2,3,4,5)),
                "distribution_choice and pars can't be missing when tuning_method is random_search")


})

#Grid Search Strategy
test_that("add_hyperparameter works for grid_search", {
  # Create an initial grid_search object
  grid_search_obj <- create_hyperparameter_tuning_strategy(
    tuning_method = "grid_search",
    ml_algorithm = "glmnet",
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

test_that("add_hyperparameter throws error for missing grid in grid_search", {
  grid_search_obj <- create_hyperparameter_tuning_strategy(
    tuning_method = "grid_search",
    ml_algorithm = "glmnet",
    chosen_eval_metric = "mphe",
    validation_sample_size = 1000
  )

  expect_error(add_hyperparameter(grid_search_obj, hyperparameter = "alpha"), "grid can't be missing when tuning method is grid_search")
})

#Random Search Strategy
test_that("add_hyperparameter works for random_search", {
  # Create an initial random_search object
  random_search_obj <- create_hyperparameter_tuning_strategy(
    tuning_method = "random_search",
    ml_algorithm = "xgb",
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

test_that("add_hyperparameter throws error for missing distribution_choice in random_search", {
  random_search_obj <- create_hyperparameter_tuning_strategy(
    tuning_method = "random_search",
    ml_algorithm = "xgb",
    validation_sample_size = 1000,
    chosen_eval_metric = "rmse",
    n_iter = 20
  )

  expect_error(add_hyperparameter(random_search_obj, hyperparameter = "max_depth", grid = c(1,2,3,4,5)), "distribution_choice and pars can't be missing when tuning_method is random_search")
})

#Bayesian Opt Strategy
test_that("add_hyperparameter works for bayesian_opt", {
  # Create an initial bayesian_opt object
  bayesian_opt_obj <- create_hyperparameter_tuning_strategy(
    tuning_method = "bayesian_opt",
    ml_algorithm = "rf",
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

test_that("add_hyperparameter throws error for missing bounds in bayesian_opt", {
  bayesian_opt_obj <- create_hyperparameter_tuning_strategy(
    tuning_method = "bayesian_opt",
    ml_algorithm = "rf",
    chosen_eval_metric = "rmse",
    validation_sample_size = 1000,
    n_iter = 50,
    acq = "ei",
    init_points = 5,
    k_iter = 3
  )

  expect_error(add_hyperparameter(bayesian_opt_obj, hyperparameter = "mtry"), "bounds can't be missing when tuning_method is bayesian_opt")
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
##Create ml_experiment
##########################################

# Test: Create `ml_experiment` Object with Default Values
# Test that an `ml_experiment` object can be successfully created with default values where applicable.
test_that("create_ml_experiment works with default values", {
  ml_exp <- create_ml_experiment(
    target_fwd_name = "target_variable",
    ml_algorithm = "rf",
    hyperparameter_tuning_strategy =
      create_hyperparameter_tuning_strategy(ml_algorithm = "rf", tuning_method = "grid_search",
                                            validation_sample_size = 10, chosen_eval_metric = "rmse")
  )

  expect_s4_class(ml_exp, "ml_experiment")
  expect_equal(ml_exp@target_fwd_name, "target_variable")
  expect_equal(ml_exp@ml_algorithm, "rf")
  expect_equal(ml_exp@custom_objective, "squared_error")
  expect_equal(ml_exp@quantile_tau, 0.5)
  expect_equal(ml_exp@huber_delta, 1)
})


# Test: Valid tuning method
test_that("create_ml_parameters accepts valid tuning method", {
  ml_params <- create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "xgb",
    tuning_method = "grid_search"
  )

  expect_equal(ml_params@tuning_method, "grid_search")
})

# Test: Invalid tuning method
test_that("create_ml_parameters throws an error for invalid tuning method", {
  expect_error(create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "xgb",
    tuning_method = "invalid_method"
  ), "Invalid tuning method")
})

# Test: Valid chosen evaluation metric
test_that("create_ml_parameters accepts valid chosen_eval_metric", {
  ml_params <- create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "xgb",
    chosen_eval_metric = "rmse"
  )

  expect_equal(ml_params@chosen_eval_metric, "rmse")
})

# Test: Invalid chosen evaluation metric
test_that("create_ml_parameters throws an error for invalid chosen_eval_metric", {
  expect_error(create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "xgb",
    chosen_eval_metric = "invalid_metric"
  ), "chosen_eval_metric choice not supported.")
})

# Test: Quantile tau range
test_that("create_ml_parameters ensures quantile_tau is between 0 and 1", {
  expect_error(create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "xgb",
    quantile_tau = 1.5
  ), "quantile_tau should be > 0 and less than 1.")

  expect_error(create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "xgb",
    quantile_tau = -0.5
  ), "quantile_tau should be > 0 and less than 1.")
})

# Test: Custom objective only for xgb or nn algorithms
test_that("create_ml_parameters throws an error if custom_objective is used with unsupported ml_algorithm", {
  expect_error(create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "rf",
    custom_objective = "absolute_error"
  ), "Invalid custom_objective. Custom objectives are only allowed for 'xgb' or 'nn' algorithms.")
})

# Test: Valid custom objective
test_that("create_ml_parameters accepts valid custom_objective for xgb or nn", {
  ml_params <- create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "xgb",
    custom_objective = "absolute_error"
  )

  expect_equal(ml_params@custom_objective, "absolute_error")
})

test_that("create_ml_parameters works with keras_architecture_parameters and hyper_grid_domain", {

  hyper_grid <- create_hyper_grid_domain(tuning_method = "grid_search",
                                         ml_algorithm = "glmnet", hyperparameters = list(alpha = c(0.2, 0.5, 0.9)))
  hyper_grid <- add_hyperparameter(hyper_grid, hyperparameter = list(alpha = c(0.2, 0.75, 0.9), lambda.min.ratio = .3))

  keras_architecture <- create_keras_architecture(nn_optimizer = "Adam", units = 32, activation = "relu", batch_norm_option = TRUE)

  keras_architecture <- add_layer(keras_architecture, units = 16, batch_norm_option = FALSE, activation = "relu")

  ml_params <- create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "glmnet",
    chosen_eval_metric = "rmse",
    hyper_grid_domain = hyper_grid
  )

  expect_true(is_hyper_grid_domain(ml_params@hyper_grid_domain))

  ml_params <- create_ml_parameters(
    target_fwd_name = "target_variable",
    ml_algorithm = "nn",
    chosen_eval_metric = "rmse",
    keras_architecture_parameters = keras_architecture
  )

  expect_true(is_keras_architecture_parameters(ml_params@keras_architecture_parameters))

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

