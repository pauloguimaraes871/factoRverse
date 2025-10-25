test_that("roll_fwd_port_weights works with artificial obj", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(Gamma = "long")
  fwd_return_m_df <- target_m_df %>% dplyr::select(id, tickers, dates, fwd_return_1m)
  transaction_costs_parameters$strategy_aum <- 1

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 3, port_construction_method = "sw",
                             min_eligible_assets_fallback = NULL, ridge_pen = NULL, macro_ridge_pen = NULL,
                             scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             micro_port_construction_method = NULL, macro_port_construction_method = NULL,
                             use_raw_for_eligibility = NULL, macro_concentration_constraint_policy = NULL,
                             exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_parameters,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::select(-smll)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- 0

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Set Portfolio Weights
  sw_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "sw")

  #Allocate Port
  port_alloc_results <- allocate_port(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                      updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                      stock_universe_m_d_ref = sw_port@universe_m_d_ref@data,
                                      liquidity_m_d_ref = liquidity_m_d_ref,
                                      volatility_m_d_ref = volatility_m_d_ref,
                                      main_liquidity_metric = "mean_volfin_3m",
                                      transaction_costs_parameters = transaction_costs_parameters,
                                      selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
                                      verbose = FALSE)

  ##Clean fwd 1m
  clean_fwd_return_1m_m_d_ref <- fwd_return_m_d_ref
  fwd_selected_benchmark_return <- benchmark_returns_m_xts$ibov[5] %>% as.numeric()

  ##Returns
  returns <- calculate_port_returns(
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost
  )

  ##Rolled port
  results <- roll_fwd_port_weights(
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref
  )

  #Check if the results are correct
  expect_equal(results$updated_port_weights, c(0.5499, 0.1638, 0.1231, 0.1631), tolerance = 1e-2)


})

test_that("calculate_port_metrics works with toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  transaction_costs_parameters$strategy_aum <- 1000
  liquidity_constraint_policy$liquidity_floor_rule <- "small_caps"
  liquidity_constraint_policy$liquidity_cap_rules <- NULL


  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 3, port_construction_method = "cw",
                             min_eligible_assets_fallback = NULL, ridge_pen = NULL, macro_ridge_pen = NULL,
                             scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             micro_port_construction_method = NULL, macro_port_construction_method = NULL,
                             use_raw_for_eligibility = NULL, macro_concentration_constraint_policy = NULL,
                             exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = liquidity_constraint_policy, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_parameters,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )


  #Current date
  current_date <- "2023-03-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2023-02-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- 0

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #cw_port
  cw_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "cw",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  #Allocate Port
  port_alloc_results <- allocate_port(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                      updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                      stock_universe_m_d_ref = cw_port@universe_m_d_ref@data,
                                      liquidity_m_d_ref = liquidity_m_d_ref,
                                      volatility_m_d_ref = volatility_m_d_ref,
                                      main_liquidity_metric = "mean_volfin_3m",
                                      transaction_costs_parameters = transaction_costs_parameters,
                                      selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
                                      verbose = FALSE)


  ##Clean fwd 1m
  clean_fwd_return_1m_m_d_ref <- fwd_return_m_d_ref
  fwd_selected_benchmark_return <- benchmark_returns_m_xts$ibov[7] %>% as.numeric()

  ##Results
  returns <- calculate_port_returns(
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost
  )


  ##Rolled port
  results <- roll_fwd_port_weights(
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref
  )

  #Check that all zero-weighted  are zero in the following period
  non_constituents <- port_alloc_results$port_weights_m_d_ref %>% dplyr::filter(eop_port_weights == 0) %>% dplyr::pull(tickers)
  expect_equal(results %>% dplyr::filter(tickers %in% non_constituents) %>% dplyr::pull(updated_port_weights) %>% unique(), 0)

  #Check that all non zero-weighted are not zero in the following period
  constituents <- port_alloc_results$port_weights_m_d_ref %>% dplyr::filter(eop_port_weights > 0) %>% dplyr::pull(tickers)
  expect_true(all((results %>% dplyr::filter(tickers %in% constituents) %>% dplyr::pull(updated_port_weights)) != 0))

  #Check if the results are correct
  expect_equal(results[which(results$tickers == "ABCB4"),4],
               (0.004277075 * (1 - 2.5116822/100))/sum(results$updated_port_weights * (1 + clean_fwd_return_1m_m_d_ref$fwd_return_1m/100)), tolerance = 1e-2)

  #Check that updates weights sum to 1
  expect_equal(sum(results$updated_port_weights), 1)

  #Row numbers match
  expect_equal(nrow(results) , nrow(port_alloc_results$port_weights_m_d_ref))



})

















