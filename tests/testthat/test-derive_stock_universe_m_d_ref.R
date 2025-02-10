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
    dplyr::mutate(exp_ret_score = signal_transform(Alpha, upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)) %>%
    dplyr::select(-Alpha)

  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization), expected_results)

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
    dplyr::mutate(exp_ret_score = signal_transform(Beta*-1, upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)) %>%
    dplyr::select(-Beta)

  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
                                  lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization), expected_results)

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
                             eligibility_quantile_range = c(0.75, 0.90), selected_benchmark = "ibov",
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = NULL, benchmark_weights_m_df = NULL, volatility_m_df = volatility_m_df,
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
    dplyr::mutate(exp_ret_score = signal_transform(pred, upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)) %>%
    dplyr::select(-pred)


  expect_equal(
    derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = NULL,
                                  oos_predictions_m_d_ref = oos_preds_m_d_ref,
                                  lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization),
    expected_results)


})
