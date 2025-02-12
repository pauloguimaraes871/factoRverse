test_that("calculate_port_returns with toy port data with benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(Gamma = "long")
  fwd_return_m_df <- target_m_df %>% dplyr::select(id, tickers, dates, fwd_return_1m)

  #Custom Stock Metrics M DF
  custom_stock_metrics_m_df <- signals_m_df %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(pe = rnorm(n = nrow(.), mean = 0, sd = 1),
                  roe_3m = rnorm(n = nrow(.), mean = 0, sd = 1)
    )

  transaction_costs_parameters$strategy_aum <- 1

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 3, port_construction_method = "sw",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_parameters,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = custom_stock_metrics_m_df, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  custom_stock_metrics_m_d_ref <- custom_stock_metrics_m_df %>% dplyr::filter(dates == current_date)
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

  ##Results
  results <- calculate_port_returns(
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost
  )

  #raw return
  expect_equal(results$fwd_raw_return, 1.3115, tolerance = 1e-2)
  expect_equal(results$fwd_raw_active_return, 2.2990, tolerance = 1e-2)

  #net return
  expect_equal(results$fwd_net_return, 0.641637, tolerance = 1e-2)
  expect_equal(results$fwd_net_active_return, 1.629137, tolerance = 1e-2)

  #fwd selected bench
  expect_equal(results$fwd_selected_bench_return, fwd_selected_benchmark_return, tolerance = 1e-2)

})

test_that("calculate_port_returns with toy port data without benchmark and a single asset portfolio", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(Gamma = "long")
  fwd_return_m_df <- target_m_df %>% dplyr::select(id, tickers, dates, fwd_return_1m)

  #Custom Stock Metrics M DF
  custom_stock_metrics_m_df <- signals_m_df %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(pe = rnorm(n = nrow(.), mean = 0, sd = 1),
                  roe_3m = rnorm(n = nrow(.), mean = 0, sd = 1)
    )

  transaction_costs_parameters$strategy_aum <- 1

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 3, port_construction_method = "sw",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_parameters,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = custom_stock_metrics_m_df, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  custom_stock_metrics_m_d_ref <- custom_stock_metrics_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- NULL
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
    concentration_constraint_policy = NULL,
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
  fwd_selected_benchmark_return <- NULL

  ##Results
  results <- calculate_port_returns(
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost
  )

  #raw return
  expect_equal(results$fwd_raw_return, clean_fwd_return_1m_m_d_ref$fwd_return_1m[1], tolerance = 1e-2)
  expect_null(results$fwd_raw_active_return)

  #net return
  expect_equal(results$fwd_net_return, clean_fwd_return_1m_m_d_ref$fwd_return_1m[1] - port_alloc_results$port_costs_d_ref$total_cost, tolerance = 1e-2)
  expect_null(results$fwd_net_active_return)

  #fwd selected bench
  expect_null(results$fwd_selected_bench_return)

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
  results <- calculate_port_returns(
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost
  )

  #raw return
  expect_equal(results$fwd_raw_return, 5.0672, tolerance = 1e-2)
  expect_equal(results$fwd_raw_active_return, 1.5572, tolerance = 1e-2)

  #net return
  expect_equal(results$fwd_net_return, 4.6362, tolerance = 1e-2)
  expect_equal(results$fwd_net_active_return, 1.1272, tolerance = 1e-2)

  #fwd selected bench
  expect_equal(results$fwd_selected_bench_return, fwd_selected_benchmark_return, tolerance = 1e-2)


})

