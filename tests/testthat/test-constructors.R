test_that("create_hyper_grid_domain works", {


  expect_no_error(create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet"))

  expect_no_error(
    create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet",
                                           hyperparameters = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0, max = 1)),
                                                                  lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0, max = 1))
                                                                               ))
    )


  hyper_grid_domain <-    create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet",
                                                   hyperparameters = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0, max = 1)),
                                                                          lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0, max = 0.9))
                                                   ))

  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, list(distribution_choice = "uniform", pars = c(min = 0, max = 1)))
  expect_equal(hyper_grid_domain@hyperparameter_list$lambda.min.ratio, list(distribution_choice = "uniform", pars = c(min = 0, max = 0.9)))


  hyper_grid_domain <- create_hyper_grid_domain(tuning_method = "grid_search", ml_algorithm = "glmnet",
                                                   hyperparameters = list(alpha = c(0.1, 0.5, 0.9),
                                                                          lambda.min.ratio = c(0.1, 0.75, 0.9))
                                                   )

  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, c(0.1, 0.5, 0.9))
  expect_equal(hyper_grid_domain@hyperparameter_list$lambda.min.ratio, c(0.1, 0.75, 0.9))


})

test_that("add_hyperparameters works to add and overwrite", {

  hyper_grid_domain <- create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet",
                                                hyperparameters = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0, max = 1))
                                                   ))

  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain, new_hyperparameters = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0, max = 0.75))
                                          ))


  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, list(distribution_choice = "uniform", pars = c(min = 0, max = 0.75)))

  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain, new_hyperparameters = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0, max = 0.50))))
  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain, new_hyperparameters = list(lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0, max = 0.70))))


  expect_equal(hyper_grid_domain@hyperparameter_list$alpha, list(distribution_choice = "uniform", pars = c(min = 0, max = 0.50)))
  expect_equal(hyper_grid_domain@hyperparameter_list$lambda.min.ratio, list(distribution_choice = "uniform", pars = c(min = 0, max = 0.70)))


})



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
  keras <- add_layer(keras, units = 8, activation = "tanh", batch_norm_option = FALSE)

  expect_equal(keras@units, c(32,16,8))
  expect_equal(keras@batch_norm_option, c(TRUE,TRUE,FALSE))
  expect_equal(keras@activation, c("relu","relu","tanh"))
  expect_equal(keras@nn_optimizer, "Adam")
  expect_equal(keras@n_layers, 3)

})



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
                                            signal_blending_method = ""
                                      )


})



