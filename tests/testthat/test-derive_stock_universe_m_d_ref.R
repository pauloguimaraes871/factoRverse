test_that("derive_stock_universe_m_d_ref works for 'long' chosen_score_metric_and_position", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Alpha = "long")

  #Expected
  expected_results <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, Alpha) %>%
    dplyr::mutate(exp_ret_score_raw =
                    signal_transform(Alpha,
                                     upper_quantile_winsorization = upper_quantile_winsorization,
                                     lower_quantile_winsorization = lower_quantile_winsorization),
                  exp_ret_score = exp_ret_score_raw
                  ) %>%
    dplyr::select(-Alpha)

  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization),
    expected_results)

})

test_that("derive_stock_universe_m_d_ref works for 'short' chosen_score_metric_and_position", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Beta = "short")

  #Expected
  expected_results <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, Beta) %>%
    dplyr::mutate(exp_ret_score_raw =
                    signal_transform(Beta*-1,
                                     upper_quantile_winsorization = upper_quantile_winsorization,
                                     lower_quantile_winsorization = lower_quantile_winsorization),
                  exp_ret_score = exp_ret_score_raw
    ) %>%
    dplyr::select(-Beta)

  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization), expected_results)

})

test_that("derive_stock_universe_m_d_ref works with scaling for 'long'", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Alpha = "long")

  #Scaler
  scaler_m_d_ref <- signals_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(dy = c(1, 10, 5, 2))
  scaler_shrinkage <- 0.5

  #Expected
  expected_results <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, Alpha) %>%
    dplyr::mutate(exp_ret_score_raw =
                    signal_transform(Alpha,
                                     upper_quantile_winsorization = upper_quantile_winsorization,
                                     lower_quantile_winsorization = lower_quantile_winsorization),
                  scaler = signal_transform(scaler_m_d_ref$dy,
                                            lower_quantile_winsorization = lower_quantile_winsorization,
                                            upper_quantile_winsorization = upper_quantile_winsorization),
                  scaler = scaler * (1 - scaler_shrinkage) + scaler_shrinkage * 1,
                  exp_ret_score = exp_ret_score_raw * scaler
                  ) %>%
    dplyr::select(-Alpha)

  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization,
                                  scaler_m_d_ref = scaler_m_d_ref,
                                  scaler_shrinkage = scaler_shrinkage,
                                  chosen_scaler = "dy"),
    expected_results)



})

test_that("derive_stock_universe_m_d_ref works with scaling for 'short'", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Beta = "short")

  #Scaler
  scaler_m_d_ref <- signals_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(dy = c(1, 10, 5, 2))
  scaler_shrinkage <- 0.5

  #Expected
  expected_results <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, Beta) %>%
    dplyr::mutate(exp_ret_score_raw =
                    signal_transform(Beta*-1,
                                     upper_quantile_winsorization = upper_quantile_winsorization,
                                     lower_quantile_winsorization = lower_quantile_winsorization),
                  scaler = signal_transform(scaler_m_d_ref$dy,
                                            lower_quantile_winsorization = lower_quantile_winsorization,
                                            upper_quantile_winsorization = upper_quantile_winsorization),
                  scaler = scaler * (1 - scaler_shrinkage) + scaler_shrinkage * 1,
                  exp_ret_score = exp_ret_score_raw * scaler
    ) %>%
    dplyr::select(-Beta)

  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization,
                                  scaler_m_d_ref = scaler_m_d_ref,
                                  scaler_shrinkage = scaler_shrinkage,
                                  chosen_scaler = "dy"),
    expected_results)
})

test_that("derive_stock_universe_m_d_ref preserves dummies for both 'long' or 'short' (ie keeps 1,0)", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date) %>%
    dplyr::mutate(Dummy = c(1, 0, 1, 0))

  #Chosen Score
  chosen_score_metric_and_position <- c(Dummy = "long")

  #Expected
  expected_results <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, Dummy) %>%
    dplyr::mutate(exp_ret_score_raw = Dummy,
                  exp_ret_score = exp_ret_score_raw
    ) %>%
    dplyr::select(-Dummy)

  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization),
    expected_results)


  #For short, result should be -1 for dummy = 1 and 0 for dummy = 0
  chosen_score_metric_and_position <- c(Dummy = "short")

  expected_results <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, Dummy) %>%
    dplyr::mutate(exp_ret_score_raw = Dummy * -1,
                  exp_ret_score = exp_ret_score_raw
    ) %>%
    dplyr::select(-Dummy)

  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization),
    expected_results)


})

test_that("derive_stock_universe_m_d_ref answers to changes in shrinkage (ie 0, 0.5, 1)", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Alpha = "long")

  #Scaler
  scaler_m_d_ref <- signals_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(dy = c(1, 10, 5, 2))

  #Shrinkage values
  shrinkage_values <- c(0, 0.5, 1)

  results_list <- lapply(shrinkage_values, function(shrinkage) {
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization,
                                  scaler_m_d_ref = scaler_m_d_ref,
                                  scaler_shrinkage = shrinkage,
                                  chosen_scaler = "dy")
  })

  names(results_list) <- paste0("shrinkage_", shrinkage_values)

  # Check that results differ as expected
  expect_false(identical(results_list$shrinkage_0$exp_ret_score, results_list$shrinkage_0.5$exp_ret_score))
  expect_false(identical(results_list$shrinkage_0.5$exp_ret_score, results_list$shrinkage_1$exp_ret_score))

  # Check that shrinkage of 1 results in no scaling effect (all scores should be equal to raw scores)
  expect_equal(results_list$shrinkage_1$exp_ret_score, results_list$shrinkage_1$exp_ret_score_raw)

  # Check that shrinkage of 0 makes stocks with high dy have higher exp_ret_score than raw scores
  expect_true(all(results_list$shrinkage_0$exp_ret_score[scaler_m_d_ref$dy == max(scaler_m_d_ref$dy)] >=
                    results_list$shrinkage_0$exp_ret_score_raw[scaler_m_d_ref$dy == max(scaler_m_d_ref$dy)]))
  # Check that shrinkage of 0 makes stocks with low dy have lower exp_ret_score than raw scores
  expect_true(all(results_list$shrinkage_0$exp_ret_score[scaler_m_d_ref$dy == min(scaler_m_d_ref$dy)] <=
                    results_list$shrinkage_0$exp_ret_score_raw[scaler_m_d_ref$dy == min(scaler_m_d_ref$dy)]))

})

test_that("derive_stock_universe_m_d_ref works for oos_predictions_m_df", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  ols_config <- create_sb_backtest_config(sb_algorithm = "ols", training_sample_size = 6, rebalancing_months = 6,
                                          target_fwd_name = c("fwd_premium_3m"))

  target_m_df <- target_m_df %>% create_meta_dataframe()
  features_m_df <- signals_m_df %>% create_meta_dataframe()

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    sb_backtest_results <- run_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      config = ols_config,
      parallel = FALSE,
      verbose = TRUE
    )}))

  #Get OOS Preds
  oos_preds_m_df <- sb_backtest_results@oos_sb_outputs_m_df@data

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = c(roe_3m = "long"),
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "ew",
                             chosen_scaler = NULL, scaler_m_df = NULL, scaler_shrinkage = NULL, use_raw_for_eligibility = NULL,
                             exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             min_eligible_assets_fallback = NULL, ridge_pen = NULL, macro_ridge_pen = NULL,
                             micro_port_construction_method = NULL, macro_port_construction_method = NULL,
                             macro_concentration_constraint_policy = NULL,
                             eligibility_quantile_range = c(0.75, 0.90), selected_benchmark = "ibov",
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = NULL, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
                             )


  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  oos_preds_m_d_ref <- sb_backtest_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)


  #Expected
  expected_results <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>%
    dplyr::left_join(oos_preds_m_d_ref %>% dplyr::filter(dates == current_date) %>% dplyr::select(-dates, -tickers, -error, -target), by = "id") %>%
    dplyr::mutate(exp_ret_score_raw =
                    signal_transform(pred,
                                     upper_quantile_winsorization = upper_quantile_winsorization,
                                     lower_quantile_winsorization = lower_quantile_winsorization),
                  exp_ret_score = exp_ret_score_raw
                  ) %>%
    dplyr::select(-pred)


  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = NULL,
                                  oos_predictions_m_d_ref = oos_preds_m_d_ref,
                                  lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization),
    expected_results)


})

test_that("derive_stock_universe_m_d_ref throws errors for incorrect selections", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Alma = "long")

  #Expect error
  expect_error(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization),
    "The chosen score column 'Alma' is not found in signals_m_d_ref.")

  #Chosen Score not provided
  expect_error(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = NULL,
                                  lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization),
    "Either oos_predictions_m_d_ref or chosen_score_metric_and_position must be provided.")

  #Both provided
  chosen_score_metric_and_position <- c(Alpha = "long")

  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Both provided
  ols_config <- create_sb_backtest_config(sb_algorithm = "ols", training_sample_size = 6, rebalancing_months = 6,
                                          target_fwd_name = c("fwd_premium_3m"))

  target_m_df <- target_m_df %>% create_meta_dataframe()
  features_m_df <- signals_m_df %>% create_meta_dataframe()

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    sb_backtest_results <- run_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      config = ols_config,
      parallel = FALSE,
      verbose = TRUE
    )}))

  #Get OOS Preds
  oos_preds_m_df <- sb_backtest_results@oos_sb_outputs_m_df@data

  expect_error(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  oos_predictions_m_d_ref = oos_preds_m_df,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization),
    "Only one of oos_predictions_m_d_ref or chosen_score_metric_and_position should be provided.")


})

test_that("derive_stock_universe_m_d_ref throws errors when chosen_scaler is not in scaler_m_d_ref", {


  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Alpha = "long")

  #Scaler
  scaler_m_d_ref <- signals_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(dy = c(1, 10, 5, 2))
  scaler_shrinkage <- 0.5
  chosen_scaler <- "non_existent_column"

  expect_error(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization,
                                  scaler_m_d_ref = scaler_m_d_ref,
                                  scaler_shrinkage = scaler_shrinkage,
                                  chosen_scaler = chosen_scaler),
    "chosen_scaler must be the name of a column in scaler_m_d_ref.")



})

test_that("derive_stock_universe_m_d_ref throws errors when scaler_shrinkage is not between 0 and 1", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Alpha = "long")

  #Scaler
  scaler_m_d_ref <- signals_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(dy = c(1, 10, 5, 2))
  chosen_scaler <- "dy"

  #Shrinkage values
  invalid_shrinkage_values <- c(-0.1, 1.1)

  for (shrinkage in invalid_shrinkage_values) {
    expect_error(
      derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                    chosen_score_metric_and_position = chosen_score_metric_and_position,
                                    lower_quantile_winsorization = lower_quantile_winsorization,
                                    upper_quantile_winsorization = upper_quantile_winsorization,
                                    scaler_m_d_ref = scaler_m_d_ref,
                                    scaler_shrinkage = shrinkage,
                                    chosen_scaler = chosen_scaler),
      "scaler_shrinkage must be a numeric value between 0 and 1.")
  }

})

test_that("derive_stock_universe_m_d_ref throws an error when scaler_m_d_ref is provided without a chosen_scaler", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)

  #Chosen Score
  chosen_score_metric_and_position <- c(Alpha = "long")

  #Scaler
  scaler_m_d_ref <- signals_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(dy = c(1, 10, 5, 2))
  scaler_shrinkage <- 0.5

  expect_error(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization,
                                  scaler_m_d_ref = scaler_m_d_ref,
                                  scaler_shrinkage = scaler_shrinkage,
                                  chosen_scaler = NULL),
    "If scaler_m_d_ref is provided, a chosen_scaler must be provided."
  )

})

test_that("derive_stock_universe_m_d_ref throws an error when trying to scale dummy variables", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date) %>%
    dplyr::mutate(Dummy = c(1, 0, 1, 0))

  #Chosen Score
  chosen_score_metric_and_position <- c(Dummy = "long")

  #Scaler
  scaler_m_d_ref <- signals_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(dy = c(1, 10, 5, 2))
  scaler_shrinkage <- 0.5
  chosen_scaler <- "dy"

  expect_error(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                  chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization,
                                  upper_quantile_winsorization = upper_quantile_winsorization,
                                  scaler_m_d_ref = scaler_m_d_ref,
                                  scaler_shrinkage = scaler_shrinkage,
                                  chosen_scaler = chosen_scaler),
    "Scaler provided but chosen score is a binary dummy. Prefer user AND rules for this use case."
  )


})
