
test_that("run_port_backtest works for a simple ew single signal strategy with only a liquidity_floor_rule constraint and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "ew",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts["2022-10-15/2023-04-15"])
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m))


  #Run port_backtest
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  ew_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "ew"
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = ew_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )



  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  ew_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "ew"
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = ew_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, ew_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               signals_m_d_ref$roe_3m %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               ew_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               ew_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are equal in rebalancing months
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15", eop_port_weights > 0) %>% dplyr::pull(eop_port_weights) %>% unique() %>% length(),
               1
  )

  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15", eop_port_weights > 0) %>% dplyr::pull(eop_port_weights) %>% unique() %>% length(),
               1
  )


  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))


  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, ew_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "ew")
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))



  #Summary, plot and print
  expect_no_error(print(results))
  expect_no_error(print(port_config))

})

test_that("run_port_backtest works for a simple sw single signal strategy with only a liquidity_floor_rule constraint and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
                                             ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m))


  #Run port_backtest
  expect_warning(
  results <- run_port_backtest(signals_m_df = signals_m_df,
                               fwd_return_m_df = fwd_return_m_df,
                               liquidity_m_df = liquidity_m_df,
                               volatility_m_df = volatility_m_df,
                               config = port_config,
                               benchmark_weights_m_df = benchmark_weights_m_df,
                               benchmark_returns_m_xts = benchmark_returns_m_xts,
                               custom_stock_metrics_m_df = port_metrics_m_df,
                               verbose = TRUE),
  "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  sw_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "sw"
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = sw_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
                           )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )



  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  sw_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "sw"
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = sw_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, sw_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                        dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                        dplyr::filter(is_eligible == 1))
                        , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               signals_m_d_ref$roe_3m %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
               )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               sw_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
               )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               sw_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))

  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, sw_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "sw")
  expect_equal(results@final_stock_port@exp_ret_score, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
    #Port Ret
    expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
    expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
    #Port Costs
    expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
    expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
    #Port Metrics
    expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
    expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
    expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a simple sw single signal strategy with more than one rebalancing month", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = c(3,4),
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m))


  #Run port_backtest
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  sw_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "sw"
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = sw_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
  )

  #Set Port Weights
  sw_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "sw"
  )

  #port_allocation
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = sw_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )




  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_3 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  sw_port_3 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_3,
    port_construction_method = "sw"
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = sw_port_3@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, sw_port_3@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               signals_m_d_ref$roe_3m %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               sw_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               sw_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               sw_port_3@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))

  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, sw_port_3@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "sw")
  expect_equal(results@final_stock_port@exp_ret_score, stock_universe_m_d_ref_3 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_3 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a simple cs single signal strategy with only a liquidity_floor_rule constraint and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "cs",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m))


  #Run port_backtest
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  cs_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "cs",
    liquidity_m_d_ref = liquidity_m_d_ref,
    cap_weighting_metric = "mean_volfin_3m"
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = cs_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )



  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  cs_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "cs",
    liquidity_m_d_ref = liquidity_m_d_ref,
    cap_weighting_metric = "mean_volfin_3m"
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = cs_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, cs_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               signals_m_d_ref$roe_3m %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               cs_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               cs_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are higher for high market-caps
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(is_eligible == 1, liquidity_classification %in% c("mega_caps", "large_caps")) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(is_eligible == 1, !liquidity_classification %in% c("mega_caps", "large_caps"))  %>% dplyr::pull(weights) %>% mean()
  )

  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))

  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, cs_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "cs")
  expect_equal(results@final_stock_port@exp_ret_score, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a simple rp single signal strategy with only a liquidity_floor_rule constraint and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "rp",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_cov_est_method(cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE)


  #meta_dataframes and xts
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  expect_warning(
    daily_stock_returns_m_xts <-  create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3"),
    "There are NA values in the time series."
  )
  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )

  #Run port_backtest
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 stock_groups_m_df = stock_groups_m_df,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  rp_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = rp_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  rp_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = rp_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, rp_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               signals_m_d_ref$roe_3m %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               rp_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               rp_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are somewhat higher for high roe
  high_roe_ids <- signals_m_d_ref %>% dplyr::filter(roe_3m >= quantile(roe_3m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(is_eligible == 1, id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(is_eligible == 1, !id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))

  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, rp_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "rp")
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for cov
  expect_equal(results@final_stock_port@covariance_matrix, rp_port_2@covariance_matrix)

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a rp strategy with exp_ret_score_tilt (inner) + scaler DY, a liquidity_floor_rule constraint and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             chosen_scaler = "dy_med_36m",
                                             scaler_shrinkage = 0.5,
                                             use_raw_for_eligibility = TRUE,
                                             rebalancing_months = 4,
                                             port_construction_method = "rp",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_cov_est_method(cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE) %>%
    add_rp_parameters(exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = 0.25)


  #meta_dataframes and xts
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m, dy_med_36m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  expect_warning(
    daily_stock_returns_m_xts <-  create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3"),
    "There are NA values in the time series."
  )
  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )
  scaler_m_df <- signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m) %>% create_meta_dataframe()

  #Run port_backtest
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 scaler_m_df = scaler_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    use_raw_for_eligibility = port_config@use_raw_for_eligibility,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  ##Test that disagreements arise because of differences in dy_med_36m
  stock_universe_m_d_ref_1_contrafactual <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    use_raw_for_eligibility = FALSE,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
  )

  expect_true(
    stock_universe_m_d_ref_1 %>% dplyr::left_join( #This use raw for eligibility
      stock_universe_m_d_ref_1_contrafactual %>% dplyr::select(id, is_eligible),
      by = "id"
    ) %>%
      dplyr::filter(is_eligible.x == 0, #Uses raw for eligibility
                    is_eligible.y == 1) %>%
      dplyr::summarize(
        mean_exp_ret_score = mean(exp_ret_score),
        mean_exp_ret_score_raw = mean(exp_ret_score_raw)
      ) %>%
      dplyr::mutate(check = mean_exp_ret_score > mean_exp_ret_score_raw) %>%
      dplyr::pull(check)
  )


  expect_true(
    stock_universe_m_d_ref_1 %>% dplyr::left_join( #This use raw for eligibility
      stock_universe_m_d_ref_1_contrafactual %>% dplyr::select(id, is_eligible),
      by = "id"
    ) %>%
      dplyr::filter(is_eligible.x == 1, #Uses raw for eligibility
                    is_eligible.y == 0) %>%
      dplyr::summarize(
        mean_exp_ret_score = mean(exp_ret_score),
        mean_exp_ret_score_raw = mean(exp_ret_score_raw)
      ) %>%
      dplyr::mutate(check = mean_exp_ret_score < mean_exp_ret_score_raw) %>%
      dplyr::pull(check)
  )



  #Set Port Weights
  rp_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    exp_ret_score_tilt = port_config@rp_parameters@exp_ret_score_tilt,
    exp_ret_score_tilt_eta = port_config@rp_parameters@exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  rp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1 %>% dplyr::mutate(exp_ret_score = exp_ret_score_raw),
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    rp_port_1@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    rp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  ## Test that running rp without exp_ret_score_tilt will give a portfolio with lower mean roe_3m
  rp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    rp_port_1@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    rp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = rp_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    use_raw_for_eligibility = port_config@use_raw_for_eligibility,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  rp_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    exp_ret_score_tilt = port_config@rp_parameters@exp_ret_score_tilt,
    exp_ret_score_tilt_eta = port_config@rp_parameters@exp_ret_score_tilt_eta,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  rp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2 %>% dplyr::mutate(exp_ret_score = exp_ret_score_raw),
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    rp_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    rp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  ## Test that running rp without exp_ret_score_tilt will give a portfolio with lower mean roe_3m
  rp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    rp_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    rp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )


  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = rp_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, rp_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               rp_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               rp_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are somewhat higher for high roe
  high_roe_ids <- signals_m_d_ref %>% dplyr::filter(roe_3m >= quantile(roe_3m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check that weights are somewhat higher for high dy
  high_dy_ids <- signals_m_d_ref %>% dplyr::filter(dy_med_36m >= quantile(dy_med_36m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% high_dy_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% high_dy_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m and dy is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))
  expect_true(all(results@port_metrics_m_xts@data$dy_med_36m > results@port_metrics_m_xts@data$bench_dy_med_36m))


  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, rp_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "rp")
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for cov
  expect_equal(results@final_stock_port@covariance_matrix, rp_port_2@covariance_matrix)

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a hrp strategy with exp_ret_score_tilt (final) + scaler DY, a liquidity_floor_rule constraint and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             chosen_scaler = "dy_med_36m",
                                             scaler_shrinkage = 0.5,
                                             use_raw_for_eligibility = TRUE,
                                             rebalancing_months = 4,
                                             port_construction_method = "hrp",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_cov_est_method(cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE) %>%
    add_hrp_parameters(exp_ret_score_tilt = "final", exp_ret_score_tilt_eta = 0.5)


  #meta_dataframes and xts
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m, dy_med_36m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  expect_warning(
    daily_stock_returns_m_xts <-  create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3"),
    "There are NA values in the time series."
  )
  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )
  scaler_m_df <- signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m) %>% create_meta_dataframe()

  #Run port_backtest
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 scaler_m_df = scaler_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    use_raw_for_eligibility = port_config@use_raw_for_eligibility,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  ##Test that disagreements arise because of differences in dy_med_36m
  stock_universe_m_d_ref_1_contrafactual <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    use_raw_for_eligibility = FALSE,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  expect_true(
    stock_universe_m_d_ref_1 %>% dplyr::left_join( #This use raw for eligibility
      stock_universe_m_d_ref_1_contrafactual %>% dplyr::select(id, is_eligible),
      by = "id"
    ) %>%
      dplyr::filter(is_eligible.x == 0, #Uses raw for eligibility
                    is_eligible.y == 1) %>%
      dplyr::summarize(
        mean_exp_ret_score = mean(exp_ret_score),
        mean_exp_ret_score_raw = mean(exp_ret_score_raw)
      ) %>%
      dplyr::mutate(check = mean_exp_ret_score > mean_exp_ret_score_raw) %>%
      dplyr::pull(check)
  )


  expect_true(
    stock_universe_m_d_ref_1 %>% dplyr::left_join( #This use raw for eligibility
      stock_universe_m_d_ref_1_contrafactual %>% dplyr::select(id, is_eligible),
      by = "id"
    ) %>%
      dplyr::filter(is_eligible.x == 1, #Uses raw for eligibility
                    is_eligible.y == 0) %>%
      dplyr::summarize(
        mean_exp_ret_score = mean(exp_ret_score),
        mean_exp_ret_score_raw = mean(exp_ret_score_raw)
      ) %>%
      dplyr::mutate(check = mean_exp_ret_score < mean_exp_ret_score_raw) %>%
      dplyr::pull(check)
  )



  #Set Port Weights
  hrp_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    exp_ret_score_tilt = port_config@hrp_parameters@exp_ret_score_tilt,
    exp_ret_score_tilt_eta = port_config@hrp_parameters@exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  hrp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1 %>% dplyr::mutate(exp_ret_score = exp_ret_score_raw),
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    hrp_port_1@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    hrp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  ## Test that running rp without exp_ret_score_tilt will give a portfolio with lower mean roe_3m
  hrp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    hrp_port_1@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    hrp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = hrp_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    use_raw_for_eligibility = port_config@use_raw_for_eligibility,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  hrp_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    exp_ret_score_tilt = port_config@hrp_parameters@exp_ret_score_tilt,
    exp_ret_score_tilt_eta = port_config@hrp_parameters@exp_ret_score_tilt_eta,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  hrp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2 %>% dplyr::mutate(exp_ret_score = exp_ret_score_raw),
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    hrp_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    hrp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  ## Test that running rp without exp_ret_score_tilt will give a portfolio with lower mean roe_3m
  hrp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    hrp_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    hrp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )

  ## Test that runnin with stronger tilt will bias even more towards roe_3m
  hrp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    exp_ret_score_tilt = "final",
    exp_ret_score_tilt_eta = 1.0,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_lt(
    hrp_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    hrp_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )



  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = hrp_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, hrp_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               hrp_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               hrp_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are somewhat higher for high roe
  high_roe_ids <- signals_m_d_ref %>% dplyr::filter(roe_3m >= quantile(roe_3m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check that weights are somewhat higher for high dy
  high_dy_ids <- signals_m_d_ref %>% dplyr::filter(dy_med_36m >= quantile(dy_med_36m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% high_dy_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% high_dy_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m and dy is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))
  expect_true(all(results@port_metrics_m_xts@data$dy_med_36m > results@port_metrics_m_xts@data$bench_dy_med_36m))


  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, hrp_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "hrp")
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for cov
  expect_equal(results@final_stock_port@covariance_matrix, hrp_port_2@covariance_matrix)

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a mmaf top_down strategy and selected benchmark (macro = constrained mvo, micro hrp)", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))
  macro_ridge_pen <- 50
  exp_ret_score_tilt_eta <- 5

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             chosen_scaler = "dy_med_36m",
                                             scaler_shrinkage = 0.5,
                                             use_raw_for_eligibility = TRUE,
                                             rebalancing_months = 4,
                                             port_construction_method = "mmaf",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_cov_est_method(cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE) %>%
    add_mmaf_parameters(mmaf_group_col = "macro_sector", mmaf_method = "top_down", top_down_proxy_port_method = "rp",
                        macro_port_construction_method = "mvo", micro_port_construction_method = "hrp") %>%
    add_mvo_parameters(n_random_ports = 500, ridge_pen = macro_ridge_pen, n_resamples = 3,
                       exp_ret_score_jitter = 0.2, cov_eigval_jitter = 0.3,
                       level = "macro") %>%
    add_hrp_parameters(exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                       level = "micro") %>%
    add_concentration_constraint_policy(benchmark = "ibov",
                                        max_abs_active_group_weight = c("macro_sector" = 0.1))


  #meta_dataframes and xts
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m, dy_med_36m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  expect_warning(
    daily_stock_returns_m_xts <-  create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3"),
    "There are NA values in the time series."
  )
  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )
  scaler_m_df <- signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m) %>% create_meta_dataframe()
  target_port_m_df <- benchmark_weights_m_df@data %>% dplyr::select(id, tickers, dates, ibov) %>%
    dplyr::rename(target_weights = ibov) %>%
    create_meta_dataframe()

  #Run port_backtest
  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 scaler_m_df = scaler_m_df,
                                 target_port_m_df = target_port_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 .test_seed = 123,
                                 verbose = TRUE)
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)
  target_port_m_d_ref <- target_port_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)
  macro_concentration_constraint_policy <- port_config@concentration_constraint_policy
  macro_concentration_constraint_policy@max_abs_active_individual_weight <- macro_concentration_constraint_policy@max_abs_active_group_weight
  macro_concentration_constraint_policy@max_abs_active_group_weight <- NULL

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    is_mmaf = TRUE,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    use_raw_for_eligibility = port_config@use_raw_for_eligibility,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    ridge_pen = port_config@mmaf_parameters@macro_port_config@mvo_parameters@ridge_pen,
    target_port_m_d_ref = target_port_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    selected_benchmark = "ibov"
  )

  ##Test that is_mmaf worked
  expect_true("ibov_bench_weights" %in% names(stock_universe_m_d_ref_1))
  expect_true("target_weights" %in% names(stock_universe_m_d_ref_1))

  ##Test that disagreements arise because of differences in dy_med_36m
  stock_universe_m_d_ref_1_contrafactual <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    is_mmaf = TRUE,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    use_raw_for_eligibility = FALSE,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    ridge_pen = port_config@mmaf_parameters@macro_port_config@mvo_parameters@ridge_pen,
    target_port_m_d_ref = target_port_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    selected_benchmark = "ibov"
  )


  expect_true(
    stock_universe_m_d_ref_1 %>% dplyr::left_join( #This use raw for eligibility
      stock_universe_m_d_ref_1_contrafactual %>% dplyr::select(id, is_eligible),
      by = "id"
    ) %>%
      dplyr::filter(is_eligible.x == 0, #Uses raw for eligibility
                    is_eligible.y == 1) %>%
      dplyr::summarize(
        mean_exp_ret_score = mean(exp_ret_score),
        mean_exp_ret_score_raw = mean(exp_ret_score_raw)
      ) %>%
      dplyr::mutate(check = mean_exp_ret_score > mean_exp_ret_score_raw) %>%
      dplyr::pull(check)
  )


  expect_true(
    stock_universe_m_d_ref_1 %>% dplyr::left_join( #This use raw for eligibility
      stock_universe_m_d_ref_1_contrafactual %>% dplyr::select(id, is_eligible),
      by = "id"
    ) %>%
      dplyr::filter(is_eligible.x == 1, #Uses raw for eligibility
                    is_eligible.y == 0) %>%
      dplyr::summarize(
        mean_exp_ret_score = mean(exp_ret_score),
        mean_exp_ret_score_raw = mean(exp_ret_score_raw)
      ) %>%
      dplyr::mutate(check = mean_exp_ret_score < mean_exp_ret_score_raw) %>%
      dplyr::pull(check)
  )



  #Set Port Weights
  set.seed(123)
  mmaf_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    macro_opt_method = "random", macro_rp_method = "sample",
    macro_n_random_ports = 500, macro_opt_objective = "sharpe",
    macro_ridge_pen = macro_ridge_pen, macro_n_resamples = 3,
    macro_exp_ret_score_jitter = 0.2, macro_cov_eigval_jitter = 0.3,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = "rp",
    mmaf_method = "top_down",
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "mvo",
    micro_port_construction_method = "hrp",
    exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  ## Test that there is resampling evidence
  expect_true("base_weights" %in% names(mmaf_port_1@macro@universe_m_d_ref@data))

  ## Test that weight restrictions were obeyed
  expect_true(all(c("max_weight", "min_weight") %in% names(mmaf_port_1@macro@universe_m_d_ref@data)))
  expect_true(
  mmaf_port_1@macro@universe_m_d_ref@data %>%
    dplyr::mutate(check1 = weights < ibov_bench_weights +
                    macro_concentration_constraint_policy@max_abs_active_individual_weight,
                  check2 = weights > pmax(ibov_bench_weights -
                    macro_concentration_constraint_policy@max_abs_active_individual_weight, 0),
                  check3 = check1 & check2
                  ) %>%
    dplyr::pull(check3) %>% all()
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  set.seed(123)
  mmaf_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    macro_opt_method = "random", macro_rp_method = "sample",
    macro_n_random_ports = 500, macro_opt_objective = "sharpe",
    macro_ridge_pen = macro_ridge_pen, macro_n_resamples = 3,
    macro_exp_ret_score_jitter = 0.2, macro_cov_eigval_jitter = 0.3,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = "rp",
    mmaf_method = "top_down",
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "mvo",
    micro_port_construction_method = "hrp",
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    mmaf_port_1@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    mmaf_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  ## Test that running rp without exp_ret_score_tilt will give a portfolio with lower mean roe_3m
  expect_gt(
    mmaf_port_1@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    mmaf_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )

  ## Test that running without ridge pen will provide a portfolio with more differences to target_weights
  #Set Port Weights
  set.seed(123)
  mmaf_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    macro_opt_method = "random", macro_rp_method = "sample",
    macro_n_random_ports = 500, macro_opt_objective = "sharpe",
    macro_n_resamples = 3,
    macro_exp_ret_score_jitter = 0.2, macro_cov_eigval_jitter = 0.3,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = "rp",
    mmaf_method = "top_down",
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "mvo",
    micro_port_construction_method = "hrp",
    exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_true("target_weights" %in% names(mmaf_port_1@universe_m_d_ref@data))
  expect_true("target_weights" %in% names(mmaf_port_1@macro@universe_m_d_ref@data))
  expect_equal(
  mmaf_port_1@universe_m_d_ref@data %>%
    dplyr::group_by(macro_sector) %>%
    dplyr::summarize(sum_target_weights = sum(target_weights)) %>%
    as.data.frame() %>%
    dplyr::rename(tickers = macro_sector, target_weights = sum_target_weights),
  mmaf_port_1@macro@universe_m_d_ref@data %>%
    dplyr::select(tickers, target_weights) %>%
    as.data.frame()
  )

  expect_gt(
    mmaf_port_contrafactual@macro@universe_m_d_ref@data %>%
      dplyr::mutate(abs_diff_to_target = abs(weights - target_weights)) %>%
      dplyr::pull(abs_diff_to_target) %>%
      mean(),
    mmaf_port_1@macro@universe_m_d_ref@data %>%
      dplyr::mutate(abs_diff_to_target = abs(weights - target_weights)) %>%
      dplyr::pull(abs_diff_to_target) %>%
      mean()
    )

  #port_allocation
  suppressWarnings(
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mmaf_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
    )
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)
  target_port_m_d_ref <- target_port_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)
  target_port_m_d_ref <- target_port_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    is_mmaf = TRUE,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    use_raw_for_eligibility = port_config@use_raw_for_eligibility,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    ridge_pen = port_config@mmaf_parameters@macro_port_config@mvo_parameters@ridge_pen,
    target_port_m_d_ref = target_port_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    selected_benchmark = "ibov"
  )

  ##Test that is_mmaf worked
  expect_true("ibov_bench_weights" %in% names(stock_universe_m_d_ref_2))
  expect_true("target_weights" %in% names(stock_universe_m_d_ref_2))

  ##Test that disagreements arise because of differences in dy_med_36m
  stock_universe_m_d_ref_2_contrafactual <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    is_mmaf = TRUE,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    use_raw_for_eligibility = FALSE,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    ridge_pen = port_config@mmaf_parameters@macro_port_config@mvo_parameters@ridge_pen,
    target_port_m_d_ref = target_port_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    selected_benchmark = "ibov"
  )


  expect_true(
    stock_universe_m_d_ref_2 %>% dplyr::left_join( #This use raw for eligibility
      stock_universe_m_d_ref_2_contrafactual %>% dplyr::select(id, is_eligible),
      by = "id"
    ) %>%
      dplyr::filter(is_eligible.x == 0, #Uses raw for eligibility
                    is_eligible.y == 1) %>%
      dplyr::summarize(
        mean_exp_ret_score = mean(exp_ret_score),
        mean_exp_ret_score_raw = mean(exp_ret_score_raw)
      ) %>%
      dplyr::mutate(check = mean_exp_ret_score > mean_exp_ret_score_raw) %>%
      dplyr::pull(check)
  )


  expect_true(
    stock_universe_m_d_ref_2 %>% dplyr::left_join( #This use raw for eligibility
      stock_universe_m_d_ref_2_contrafactual %>% dplyr::select(id, is_eligible),
      by = "id"
    ) %>%
      dplyr::filter(is_eligible.x == 1, #Uses raw for eligibility
                    is_eligible.y == 0) %>%
      dplyr::summarize(
        mean_exp_ret_score = mean(exp_ret_score),
        mean_exp_ret_score_raw = mean(exp_ret_score_raw)
      ) %>%
      dplyr::mutate(check = mean_exp_ret_score < mean_exp_ret_score_raw) %>%
      dplyr::pull(check)
  )


  #Set Port Weights
  set.seed(123)
  mmaf_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    macro_opt_method = "random", macro_rp_method = "sample",
    macro_n_random_ports = 500, macro_opt_objective = "sharpe",
    macro_ridge_pen = macro_ridge_pen, macro_n_resamples = 3,
    macro_exp_ret_score_jitter = 0.2, macro_cov_eigval_jitter = 0.3,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = "rp",
    mmaf_method = "top_down",
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "mvo",
    micro_port_construction_method = "hrp",
    exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  ## Test that there is resampling evidence
  expect_true("base_weights" %in% names(mmaf_port_2@macro@universe_m_d_ref@data))

  ## Test that weight restrictions were obeyed
  expect_true(all(c("max_weight", "min_weight") %in% names(mmaf_port_2@macro@universe_m_d_ref@data)))
  expect_true(
    mmaf_port_2@macro@universe_m_d_ref@data %>%
      dplyr::mutate(check1 = weights < ibov_bench_weights +
                      macro_concentration_constraint_policy@max_abs_active_individual_weight,
                    check2 = weights > pmax(ibov_bench_weights -
                                              macro_concentration_constraint_policy@max_abs_active_individual_weight, 0),
                    check3 = check1 & check2
      ) %>%
      dplyr::pull(check3) %>% all()
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  set.seed(123)
  mmaf_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    macro_opt_method = "random", macro_rp_method = "sample",
    macro_n_random_ports = 500, macro_opt_objective = "sharpe",
    macro_ridge_pen = macro_ridge_pen, macro_n_resamples = 3,
    macro_exp_ret_score_jitter = 0.2, macro_cov_eigval_jitter = 0.3,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = "rp",
    mmaf_method = "top_down",
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "mvo",
    micro_port_construction_method = "hrp",
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_gt(
    mmaf_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    mmaf_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  ## Test that running rp without exp_ret_score_tilt will give a portfolio with lower mean roe_3m
  expect_gt(
    mmaf_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    mmaf_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )

  ## Test that running without ridge pen will provide a portfolio with more differences to target_weights
  #Set Port Weights
  set.seed(123)
  mmaf_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    macro_opt_method = "random", macro_rp_method = "sample",
    macro_n_random_ports = 500, macro_opt_objective = "sharpe",
    macro_n_resamples = 3,
    macro_exp_ret_score_jitter = 0.2, macro_cov_eigval_jitter = 0.3,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = "rp",
    mmaf_method = "top_down",
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "mvo",
    micro_port_construction_method = "hrp",
    exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  expect_true("target_weights" %in% names(mmaf_port_2@universe_m_d_ref@data))
  expect_true("target_weights" %in% names(mmaf_port_2@macro@universe_m_d_ref@data))
  expect_equal(
    mmaf_port_2@universe_m_d_ref@data %>%
      dplyr::group_by(macro_sector) %>%
      dplyr::summarize(sum_target_weights = sum(target_weights)) %>%
      as.data.frame() %>%
      dplyr::rename(tickers = macro_sector, target_weights = sum_target_weights),
    mmaf_port_2@macro@universe_m_d_ref@data %>%
      dplyr::select(tickers, target_weights) %>%
      as.data.frame()
  )

  expect_gt(
    mmaf_port_contrafactual@macro@universe_m_d_ref@data %>%
      dplyr::mutate(abs_diff_to_target = abs(weights - target_weights)) %>%
      dplyr::pull(abs_diff_to_target) %>%
      mean(),
    mmaf_port_2@macro@universe_m_d_ref@data %>%
      dplyr::mutate(abs_diff_to_target = abs(weights - target_weights)) %>%
      dplyr::pull(abs_diff_to_target) %>%
      mean()
  )




  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mmaf_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, mmaf_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               mmaf_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               mmaf_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are somewhat higher for high roe
  high_roe_ids <- signals_m_d_ref %>% dplyr::filter(roe_3m >= quantile(roe_3m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check that weights are somewhat higher for high dy
  high_dy_ids <- signals_m_d_ref %>% dplyr::filter(dy_med_36m >= quantile(dy_med_36m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% high_dy_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% high_dy_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m and dy is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))
  expect_true(all(results@port_metrics_m_xts@data$dy_med_36m > results@port_metrics_m_xts@data$bench_dy_med_36m))


  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, mmaf_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "mmaf")
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for cov
  expect_equal(results@final_stock_port@covariance_matrix, mmaf_port_2@covariance_matrix)

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a mmaf bottom_up strategy and selected benchmark (macro = constrained rp, constrained rp)", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))
  exp_ret_score_tilt_eta <- 5
  macro_exp_ret_score_tilt_eta <- 2

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             chosen_scaler = "dy_med_36m",
                                             scaler_shrinkage = 0.5,
                                             use_raw_for_eligibility = TRUE,
                                             rebalancing_months = 4,
                                             port_construction_method = "mmaf",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps",
                                    liquidity_cap_rules = c(small_caps = 0.02)) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_cov_est_method(cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE) %>%
    add_mmaf_parameters(mmaf_group_col = "macro_sector", mmaf_method = "bottom_up",
                        macro_port_construction_method = "rp", micro_port_construction_method = "rp") %>%
    add_rp_parameters(exp_ret_score_tilt_eta = macro_exp_ret_score_tilt_eta, exp_ret_score_tilt = "inner",
                      level = "macro") %>%
    add_rp_parameters(exp_ret_score_tilt_eta = exp_ret_score_tilt_eta, exp_ret_score_tilt = "inner",
                       level = "micro") %>%
    add_concentration_constraint_policy(benchmark = "ibov",
                                        max_abs_active_individual_weight = 0.04,
                                        max_abs_active_group_weight = c("macro_sector" = 0.1))


  #meta_dataframes and xts
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m, dy_med_36m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  expect_warning(
    daily_stock_returns_m_xts <-  create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3"),
    "There are NA values in the time series."
  )
  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )
  scaler_m_df <- signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m) %>% create_meta_dataframe()

  #Run port_backtest
  expect_warning(
  expect_warning(
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 scaler_m_df = scaler_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 .test_seed = 123,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  ),
  "For bottom_up, micro-level concentration constraints might not hold globally."),
  "For bottom_up, micro-level concentration constraints might not hold globally.")

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  concentration_constraint_policy <- port_config@concentration_constraint_policy
  macro_concentration_constraint_policy <- concentration_constraint_policy
  macro_concentration_constraint_policy@max_abs_active_individual_weight <- macro_concentration_constraint_policy@max_abs_active_group_weight
  macro_concentration_constraint_policy@max_abs_active_group_weight <- NULL

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    is_mmaf = FALSE,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    use_raw_for_eligibility = port_config@use_raw_for_eligibility,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(concentration_constraint_policy),
    selected_benchmark = "ibov",
    groups_m_d_ref = stock_groups_m_d_ref
  )

  ##Test that is_mmaf worked
  expect_true("ibov_bench_weights" %in% names(stock_universe_m_d_ref_1))

  #Set Port Weights
  set.seed(123)
  mmaf_port_1 <- suppressWarnings(set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
    macro_exp_ret_score_tilt = "inner", macro_exp_ret_score_tilt_eta = macro_exp_ret_score_tilt_eta,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = NULL,
    mmaf_method = "bottom_up",
    concentration_constraint_policy = as.list(concentration_constraint_policy),
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "rp",
    micro_port_construction_method = "rp",
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  ))

  ## Test that constraints were applied to both bottom up and top down
  expect_true(all(c("max_weight", "min_weight") %in% names(mmaf_port_1@micro$bottom_up@universe_m_d_ref@data)))
  expect_equal(
    mmaf_port_1@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::filter(weights > max_weight + 1e-08) %>%
      nrow(),
    0
  )
  expect_equal(
    mmaf_port_1@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::filter(weights < min_weight - 1e-08) %>%
      nrow(),
    0
  )
  expect_equal(
    mmaf_port_1@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::filter(liquidity_classification == "small_caps") %>%
      dplyr::filter(weights > ibov_bench_weights +
                      as.list(port_config@liquidity_constraint_policy)$liquidity_cap_rules["small_caps"] +
                      1e-08) %>%
      nrow(),
    0
  )

  ## Test that weight restrictions were obeyed
  expect_true(all(c("max_weight", "min_weight") %in% names(mmaf_port_1@macro@universe_m_d_ref@data)))
  expect_true(
    mmaf_port_1@macro@universe_m_d_ref@data %>%
      dplyr::mutate(check1 = weights < ibov_bench_weights +
                      macro_concentration_constraint_policy@max_abs_active_individual_weight + 1e-08,
                    check2 = weights > pmax(ibov_bench_weights -
                                              macro_concentration_constraint_policy@max_abs_active_individual_weight, 0) - 1e-08,
                    check3 = check1 & check2
      ) %>%
      dplyr::pull(check3) %>% all()
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  set.seed(123)
  mmaf_port_contrafactual <- suppressWarnings(set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = NULL,
    mmaf_method = "bottom_up",
    concentration_constraint_policy = as.list(concentration_constraint_policy),
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "rp",
    micro_port_construction_method = "rp",
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  ))

  expect_gt(
    mmaf_port_1@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    mmaf_port_contrafactual@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  expect_gt(
    mmaf_port_1@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    mmaf_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  ## Test that running rp without exp_ret_score_tilt will give a portfolio with lower mean roe_3m
  expect_gt(
    mmaf_port_1@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    mmaf_port_contrafactual@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )
  expect_gt(
    mmaf_port_1@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    mmaf_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mmaf_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  scaler_m_d_ref <- scaler_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    scaler_m_d_ref = scaler_m_d_ref,
    chosen_scaler = port_config@chosen_scaler,
    scaler_shrinkage = port_config@scaler_shrinkage,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    is_mmaf = FALSE,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    use_raw_for_eligibility = port_config@use_raw_for_eligibility,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(concentration_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    selected_benchmark = "ibov"
  )

  ##Test that is_mmaf worked
  expect_true("ibov_bench_weights" %in% names(stock_universe_m_d_ref_2))


  #Set Port Weights
  set.seed(123)
  mmaf_port_2 <- suppressWarnings(set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
    macro_exp_ret_score_tilt = "inner", macro_exp_ret_score_tilt_eta = macro_exp_ret_score_tilt_eta,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = NULL,
    mmaf_method = "bottom_up",
    concentration_constraint_policy = as.list(concentration_constraint_policy),
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "rp",
    micro_port_construction_method = "rp",
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  ))

  ## Test that constraints were applied to both bottom up and top down
  expect_true(all(c("max_weight", "min_weight") %in% names(mmaf_port_2@micro$bottom_up@universe_m_d_ref@data)))
  expect_equal(
    mmaf_port_2@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::filter(weights > max_weight + 1e-08) %>%
      nrow(),
    0
  )
  expect_equal(
    mmaf_port_2@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::filter(weights < min_weight - 1e-08) %>%
      nrow(),
    0
  )
  expect_equal(
    mmaf_port_2@micro$bottom_up@universe_m_d_ref@data %>%
      dplyr::filter(liquidity_classification == "small_caps") %>%
      dplyr::filter(weights > ibov_bench_weights +
                      as.list(port_config@liquidity_constraint_policy)$liquidity_cap_rules["small_caps"] +
                      1e-08) %>%
      nrow(),
    0
  )

  ## Test that weight restrictions were obeyed
  expect_true(all(c("max_weight", "min_weight") %in% names(mmaf_port_2@macro@universe_m_d_ref@data)))
  expect_true(
    mmaf_port_2@macro@universe_m_d_ref@data %>%
      dplyr::mutate(check1 = weights < ibov_bench_weights +
                      macro_concentration_constraint_policy@max_abs_active_individual_weight + 1e-08,
                    check2 = weights > pmax(ibov_bench_weights -
                                              macro_concentration_constraint_policy@max_abs_active_individual_weight, 0) - 1e-08,
                    check3 = check1 & check2
      ) %>%
      dplyr::pull(check3) %>% all()
  )


  ## Test that exp_ret_score_tilt increases dy_med_36m
  set.seed(123)
  mmaf_port_contrafactual <- suppressWarnings(set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = NULL,
    mmaf_method = "bottom_up",
    concentration_constraint_policy = as.list(concentration_constraint_policy),
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "rp",
    micro_port_construction_method = "rp",
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  ))


  expect_gt(
    mmaf_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum(),
    mmaf_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, dy_med_36m), by = "id") %>%
      dplyr::mutate(dy_med_36m_mult = dy_med_36m * weights) %>%
      dplyr::pull(dy_med_36m_mult) %>%
      sum()
  )

  ## Test that running rp without exp_ret_score_tilt will give a portfolio with lower mean roe_3m
  expect_gt(
    mmaf_port_2@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum(),
    mmaf_port_contrafactual@universe_m_d_ref@data %>%
      dplyr::left_join(signals_m_d_ref %>% dplyr::select(id, roe_3m), by = "id") %>%
      dplyr::mutate(roe_3m_mult = roe_3m * weights) %>%
      dplyr::pull(roe_3m_mult) %>%
      sum()
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mmaf_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, mmaf_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               mmaf_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               mmaf_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are somewhat higher for high roe
  high_roe_ids <- signals_m_d_ref %>% dplyr::filter(roe_3m >= quantile(roe_3m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% high_roe_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check that weights are somewhat higher for high dy
  high_dy_ids <- signals_m_d_ref %>% dplyr::filter(dy_med_36m >= quantile(dy_med_36m, .67)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% high_dy_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% high_dy_ids) %>% dplyr::pull(weights) %>% mean()
  )

  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that roe_3m and dy is higher for port than for bench
  expect_true(all(results@port_metrics_m_xts@data$roe_3m > results@port_metrics_m_xts@data$bench_roe_3m))
  expect_true(all(results@port_metrics_m_xts@data$dy_med_36m > results@port_metrics_m_xts@data$bench_dy_med_36m))


  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, mmaf_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "mmaf")
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for cov
  expect_equal(results@final_stock_port@covariance_matrix, mmaf_port_2@covariance_matrix)

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a oos_predictions blended strategy with only a liquidity_floor_rule constraint and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  target_m_df <- create_meta_dataframe(fwd_return_m_df@data, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m, mom_res_12m, roe_3m))

  #Create sb_backtest_config
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", rebalancing_months = 4,
                                             training_sample_size = 3, target_fwd_name = "fwd_return_1m") %>%
    add_tuning_strategy(tuning_method = "grid_search", chosen_eval_metric = "rmse", validation_sample_size = 2) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"),
                       grid = list(c(0, 0.5, 1), seq(0.1, 0.9, length=10)))

  #run_sb_backtest
  expect_warning(
  sb_results <- run_sb_backtest(
    features_m_df = signals_m_df,
    target_m_df = target_m_df,
    config = glmnet_config,
    parallel = TRUE
  ), "Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest."
  )

  #Create port_backtest_config
  port_config <- create_port_backtest_config(eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>% add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #Run port_backtest
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 sb_backtest_results = sb_results,
                                 config = port_config,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  sw_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "sw"
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = sw_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  sw_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "sw"
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = sw_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, sw_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               oos_predictions_m_d_ref$pred %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               sw_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               sw_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are higher for stocks with a higer oos_pred
  higher_pred_id <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(pred >= quantile(pred, 0.67)) %>% dplyr::arrange(desc(pred)) %>% dplyr::pull(id)
  expect_gt(
    results@stock_universe_m_df@data %>% dplyr::filter(id %in% higher_pred_id) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(!id %in% higher_pred_id)  %>% dplyr::pull(weights) %>% mean()
  )

  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that dy_med_36m (high importance in predictive model) is higher for port than for bench
  expect_gt(results@port_metrics_m_xts@data$dy_med_36m %>% mean(), results@port_metrics_m_xts@data$bench_dy_med_36m %>% mean())
  #Check that roe_3m (high importance in predictive model) is higher for port than for bench
  expect_gt(results@port_metrics_m_xts@data$roe_3m %>% mean(), results@port_metrics_m_xts@data$bench_roe_3m %>% mean())
  #Check that mom_res_12m (little importance in predictive model) is lower for port than for bench
  expect_lt(results@port_metrics_m_xts@data$mom_res_12m %>% mean(), results@port_metrics_m_xts@data$bench_mom_res_12m %>% mean())



  #Check for stock port
  expect_equal(results@final_stock_port@type, "signal_blend")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, sw_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "sw")
  expect_equal(results@final_stock_port@exp_ret_score, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest works for a oos_predictions blended strategy and 'mvo' with liquidity, turnover and concentration constraints and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  target_m_df <- create_meta_dataframe(fwd_return_m_df@data, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m, mom_res_12m, roe_3m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  expect_warning(daily_stock_returns_m_xts <- create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3"),
  "There are NA values in the time series.")

  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )


  #Create sb_backtest_config
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", rebalancing_months = 4,
                                             training_sample_size = 3, target_fwd_name = "fwd_return_1m") %>%
    add_tuning_strategy(tuning_method = "grid_search", chosen_eval_metric = "rmse", validation_sample_size = 2) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"),
                       grid = list(c(0, 0.5, 1), seq(0.1, 0.9, length=10)))

  #run_sb_backtest
  expect_warning(
    sb_results <- run_sb_backtest(
      features_m_df = signals_m_df,
      target_m_df = target_m_df,
      config = glmnet_config,
      parallel = TRUE
    ),
    "Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest."
  )

  #Create port_backtest_config
  port_config <- create_port_backtest_config(eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "mvo",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>% add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "micro_caps", liquidity_cap_rules = c(micro_caps = 0.01, small_caps = 0.02)) %>%
    add_turnover_constraint_policy(quantile_range_buffer = 0.1, turnover_cap_rules = c(micro_caps = 0.01, small_caps = 0.02)) %>%
    add_concentration_constraint_policy(max_abs_active_individual_weight = 0.03, max_abs_active_group_weight = c(sectors = 0.10, macro_sector = 0.05)) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_mvo_parameters(n_random_ports = 500, opt_objective = "sharpe") %>%
    add_cov_est_method(cov_estimation_method = "shrink_cc", cov_matrix_sample_size = 52, active_returns = TRUE)


  #Run port_backtest
  set.seed(123)
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 config = port_config,
                                 sb_backtest_results = sb_results,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  set.seed(123)
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)



  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  mvo_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  mvo_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, mvo_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 97.5 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 97.5) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               oos_predictions_m_d_ref$pred %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               mvo_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               mvo_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights respect concentration_constraint
  expect_true(all(
    results@stock_universe_m_df@data$weights <=
    results@stock_universe_m_df@data$ibov_bench_weights + port_config@concentration_constraint_policy@max_abs_active_individual_weight
    ))

  expect_true(all(
    results@stock_universe_m_df@data$weights >=
      pmax(0, results@stock_universe_m_df@data$ibov_bench_weights - port_config@concentration_constraint_policy@max_abs_active_individual_weight)
  ))


  #Check that sector weights respect sector_concentration_constraint
  sectors_benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates %in% c("2023-02-15", "2023-04-15")) %>% dplyr::left_join(stock_groups_m_df@data %>% dplyr::select(-tickers, -dates), by = "id") %>%
    dplyr::group_by(sectors, dates) %>% dplyr::summarise(bench_total = sum(ibov))

  sectors_port_weights_m_d_ref <-  results@stock_universe_m_df@data %>%
    dplyr::group_by(sectors,dates) %>% dplyr::summarise(port_total = sum(weights))

  expect_equal(sectors_benchmark_weights_m_d_ref$bench_total %>% sum(), 2) #two periods = 100% + 100%
  expect_equal(sectors_port_weights_m_d_ref$port_total %>% sum(), 2)

  expect_true(all(
    sectors_port_weights_m_d_ref$port_total <= sectors_benchmark_weights_m_d_ref$bench_total + port_config@concentration_constraint_policy@max_abs_active_group_weight[1])
  )
  expect_true(all(
    sectors_port_weights_m_d_ref$port_total >= pmax(0, sectors_benchmark_weights_m_d_ref$bench_total - port_config@concentration_constraint_policy@max_abs_active_group_weight[1]))
  )

  #Check that macro_sector weights respect sector_concentration_constraint
  macro_sector_benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates %in% c("2023-02-15", "2023-04-15")) %>% dplyr::left_join(stock_groups_m_df@data %>% dplyr::select(-tickers, -dates), by = "id") %>%
    dplyr::group_by(macro_sector, dates) %>% dplyr::summarise(bench_total = sum(ibov))

  macro_sector_port_weights_m_d_ref <-  results@stock_universe_m_df@data %>%
    dplyr::group_by(macro_sector, dates) %>% dplyr::summarise(port_total = sum(weights))

  expect_equal(macro_sector_benchmark_weights_m_d_ref$bench_total %>% sum(), 2) #two periods = 100% + 100%
  expect_equal(macro_sector_port_weights_m_d_ref$port_total %>% sum(), 2)

  expect_true(all(
    macro_sector_port_weights_m_d_ref$port_total <= macro_sector_benchmark_weights_m_d_ref$bench_total + port_config@concentration_constraint_policy@max_abs_active_group_weight[2])
  )
  expect_true(all(
    macro_sector_port_weights_m_d_ref$port_total >= pmax(0, macro_sector_benchmark_weights_m_d_ref$bench_total - port_config@concentration_constraint_policy@max_abs_active_group_weight[2]))
  )

  #Check that weights respect liquidity cap
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "small_caps") %>% dplyr::pull(weights) <=
    results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "small_caps") %>% dplyr::pull(ibov_bench_weights) + port_config@liquidity_constraint_policy@liquidity_cap_rules[2]
  ))
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "micro_caps") %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "micro_caps") %>% dplyr::pull(ibov_bench_weights) + port_config@liquidity_constraint_policy@liquidity_cap_rules[1]
  ))

  #Check that weights respect turnover constraint
  #Inclusion of buffered stocks
  expect_equal(
    results@stock_universe_m_df@data %>%
    dplyr::filter(exp_ret_score >= quantile(exp_ret_score, 0.57), exp_ret_score <= quantile(exp_ret_score, 0.67),
                  liquidity_classification == "micro_caps", bop_port_weights > 0) %>% dplyr::pull(buffer_zone_1) %>% unique(),
    1)

  #Weights respect cap
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(weights) <=
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(bop_port_weights) +
      port_config@turnover_constraint_policy@turnover_cap_rules[1]
  ))
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(weights) >=
    pmax(results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(bop_port_weights) -
      port_config@turnover_constraint_policy@turnover_cap_rules[1], 0)
  ))

  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(bop_port_weights) +
      port_config@turnover_constraint_policy@turnover_cap_rules[2]
  ))
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(weights) >=
      pmax(results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(bop_port_weights) -
             port_config@turnover_constraint_policy@turnover_cap_rules[2], 0)
  ))


  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that dy_med_36m (high importance in predictive model) is higher for port than for bench
  expect_gt(results@port_metrics_m_xts@data$dy_med_36m %>% mean(), results@port_metrics_m_xts@data$bench_dy_med_36m %>% mean())
  #Check that roe_3m (high importance in predictive model) is higher for port than for bench
  expect_gt(results@port_metrics_m_xts@data$roe_3m %>% mean(), results@port_metrics_m_xts@data$bench_roe_3m %>% mean())
  #Check that mom_res_12m (little importance in predictive model) is lower for port than for bench
  expect_lt(results@port_metrics_m_xts@data$mom_res_12m %>% mean(), results@port_metrics_m_xts@data$bench_mom_res_12m %>% mean())


  #Check for stock port
  expect_equal(results@final_stock_port@type, "signal_blend")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, mvo_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "mvo")
  expect_equal(results@final_stock_port@exp_ret_score, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))

  #backtest identifier
  expect_equal(results@port_backtest_workflow[[1]]$sb_backtest_identifier, sb_results@backtest_identifier)

})

test_that("run_port_backtest works for a oos_predictions blended strategy and 'mvo' with resamples, ridge penalty and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  target_m_df <- create_meta_dataframe(fwd_return_m_df@data, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m, mom_res_12m, roe_3m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  expect_warning(daily_stock_returns_m_xts <- create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3"),
                 "There are NA values in the time series.")
  target_port_m_df <- benchmark_weights_m_df@data %>% dplyr::select(id, tickers, dates, ibov) %>%
    dplyr::rename(target_weights = ibov)
  target_port_m_df <- create_meta_dataframe(target_port_m_df)

  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )


  #Create sb_backtest_config
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", rebalancing_months = 4,
                                             training_sample_size = 3, target_fwd_name = "fwd_return_1m") %>%
    add_tuning_strategy(tuning_method = "grid_search", chosen_eval_metric = "rmse", validation_sample_size = 2) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"),
                       grid = list(c(0, 0.5, 1), seq(0.1, 0.9, length=10)))

  #run_sb_backtest
  expect_warning(
    sb_results <- run_sb_backtest(
      features_m_df = signals_m_df,
      target_m_df = target_m_df,
      config = glmnet_config,
      parallel = TRUE
    ),
    "Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest."
  )

  #Create port_backtest_config
  port_config <- create_port_backtest_config(eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "mvo",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>% add_liquidity_floor_cutoffs(
    metric_name = c("mean_volfin_3m", "presence"),
    metric_cutoffs = list(
      c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
      c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
    )
  ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "micro_caps", liquidity_cap_rules = c(micro_caps = 0.01, small_caps = 0.02)) %>%
    add_turnover_constraint_policy(quantile_range_buffer = 0.1, turnover_cap_rules = c(micro_caps = 0.01, small_caps = 0.02)) %>%
    add_concentration_constraint_policy(max_abs_active_individual_weight = 0.1, max_abs_active_group_weight = c(sectors = 0.20, macro_sector = 0.10)) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_mvo_parameters(n_random_ports = 500, opt_objective = "sharpe", n_resamples = 3, ridge_pen = 50, exp_ret_score_jitter = 0.2, cov_eigval_jitter = 0.1) %>%
    add_cov_est_method(cov_estimation_method = "shrink_cc", cov_matrix_sample_size = 52, active_returns = TRUE)


  #Run port_backtest
  set.seed(123)
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 config = port_config,
                                 sb_backtest_results = sb_results,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 target_port_m_df = target_port_m_df,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 .test_seed = 123,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  set.seed(123)
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  target_port_m_d_ref <- target_port_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    ridge_pen = 50,
    liquidity_m_d_ref = liquidity_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    groups_m_d_ref = stock_groups_m_d_ref,
    target_port_m_d_ref = target_port_m_d_ref,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  set.seed(123)
  mvo_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    ridge_pen = 50,
    n_resamples = 3,
    exp_ret_score_jitter = 0.2,
    cov_eigval_jitter = 0.1,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

    ##Run counterfactual
    set.seed(123)
    mvo_port_counterfactual_1 <- set_portfolio_weights(
      universe_m_d_ref = stock_universe_m_d_ref_1,
      port_construction_method = "mvo",
      liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
      concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
      turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
      groups_m_d_ref = stock_groups_m_d_ref,
      n_resamples = 3,
      exp_ret_score_jitter = 0.2,
      cov_eigval_jitter = 0.1,
      returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
      selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
      active_returns = port_config@cov_est_method@active_returns,
      cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
      cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
      n_random_ports = port_config@mvo_parameters@n_random_ports
    )

    ## Test that mvo_port_1 weights are closer to target_port_m_df than counterfactual
    expect_true(
      sum((mvo_port_1@universe_m_d_ref@data$weights - mvo_port_1@universe_m_d_ref@data$target_weights)^2) <
        sum((mvo_port_counterfactual_1@universe_m_d_ref@data$weights - mvo_port_counterfactual_1@universe_m_d_ref@data$target_weights)^2)
    )



  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  target_port_m_d_ref <- target_port_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  target_port_m_d_ref <- target_port_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    ridge_pen = 50,
    target_port_m_d_ref = target_port_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  set.seed(123)
  mvo_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    ridge_pen = 50,
    n_resamples = 3,
    exp_ret_score_jitter = 0.2,
    cov_eigval_jitter = 0.1,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

  ##Run counterfactual
  set.seed(123)
  mvo_port_counterfactual_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    n_resamples = 3,
    exp_ret_score_jitter = 0.2,
    cov_eigval_jitter = 0.1,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

  ## Test that mvo_port_1 weights are closer to target_port_m_df than counterfactual
  expect_true(
    sum((mvo_port_2@universe_m_d_ref@data$weights - mvo_port_2@universe_m_d_ref@data$target_weights)^2) <
      sum((mvo_port_counterfactual_2@universe_m_d_ref@data$weights - mvo_port_counterfactual_2@universe_m_d_ref@data$target_weights)^2)
  )


  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, mvo_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 97.5 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 97.5) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               oos_predictions_m_d_ref$pred %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               mvo_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               mvo_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights respect concentration_constraint
  expect_true(all(
    results@stock_universe_m_df@data$weights <=
      results@stock_universe_m_df@data$ibov_bench_weights + port_config@concentration_constraint_policy@max_abs_active_individual_weight
  ))

  expect_true(all(
    results@stock_universe_m_df@data$weights >=
      pmax(0, results@stock_universe_m_df@data$ibov_bench_weights - port_config@concentration_constraint_policy@max_abs_active_individual_weight)
  ))


  #Check that sector weights respect sector_concentration_constraint
  sectors_benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates %in% c("2023-02-15", "2023-04-15")) %>% dplyr::left_join(stock_groups_m_df@data %>% dplyr::select(-tickers, -dates), by = "id") %>%
    dplyr::group_by(sectors, dates) %>% dplyr::summarise(bench_total = sum(ibov))

  sectors_port_weights_m_d_ref <-  results@stock_universe_m_df@data %>%
    dplyr::group_by(sectors,dates) %>% dplyr::summarise(port_total = sum(weights))

  expect_equal(sectors_benchmark_weights_m_d_ref$bench_total %>% sum(), 2) #two periods = 100% + 100%
  expect_equal(sectors_port_weights_m_d_ref$port_total %>% sum(), 2)

  expect_true(all(
    sectors_port_weights_m_d_ref$port_total <= sectors_benchmark_weights_m_d_ref$bench_total + port_config@concentration_constraint_policy@max_abs_active_group_weight[1])
  )
  expect_true(all(
    sectors_port_weights_m_d_ref$port_total >= pmax(0, sectors_benchmark_weights_m_d_ref$bench_total - port_config@concentration_constraint_policy@max_abs_active_group_weight[1]))
  )

  #Check that macro_sector weights respect sector_concentration_constraint
  macro_sector_benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates %in% c("2023-02-15", "2023-04-15")) %>% dplyr::left_join(stock_groups_m_df@data %>% dplyr::select(-tickers, -dates), by = "id") %>%
    dplyr::group_by(macro_sector, dates) %>% dplyr::summarise(bench_total = sum(ibov))

  macro_sector_port_weights_m_d_ref <-  results@stock_universe_m_df@data %>%
    dplyr::group_by(macro_sector, dates) %>% dplyr::summarise(port_total = sum(weights))

  expect_equal(macro_sector_benchmark_weights_m_d_ref$bench_total %>% sum(), 2) #two periods = 100% + 100%
  expect_equal(macro_sector_port_weights_m_d_ref$port_total %>% sum(), 2)

  expect_true(all(
    macro_sector_port_weights_m_d_ref$port_total <= macro_sector_benchmark_weights_m_d_ref$bench_total + port_config@concentration_constraint_policy@max_abs_active_group_weight[2])
  )
  expect_true(all(
    macro_sector_port_weights_m_d_ref$port_total >= pmax(0, macro_sector_benchmark_weights_m_d_ref$bench_total - port_config@concentration_constraint_policy@max_abs_active_group_weight[2]))
  )

  #Check that weights respect liquidity cap
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "small_caps") %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "small_caps") %>% dplyr::pull(ibov_bench_weights) + port_config@liquidity_constraint_policy@liquidity_cap_rules[2]
  ))
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "micro_caps") %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "micro_caps") %>% dplyr::pull(ibov_bench_weights) + port_config@liquidity_constraint_policy@liquidity_cap_rules[1]
  ))

  #Check that weights respect turnover constraint
  #Inclusion of buffered stocks
  expect_equal(
    results@stock_universe_m_df@data %>%
      dplyr::filter(exp_ret_score >= quantile(exp_ret_score, 0.57), exp_ret_score <= quantile(exp_ret_score, 0.67),
                    liquidity_classification == "micro_caps", bop_port_weights > 0) %>% dplyr::pull(buffer_zone_1) %>% unique(),
    1)

  #Weights respect cap
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(bop_port_weights) +
      port_config@turnover_constraint_policy@turnover_cap_rules[1]
  ))
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(weights) >=
      pmax(results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(bop_port_weights) -
             port_config@turnover_constraint_policy@turnover_cap_rules[1], 0)
  ))

  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(bop_port_weights) +
      port_config@turnover_constraint_policy@turnover_cap_rules[2]
  ))
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(weights) >=
      pmax(results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(bop_port_weights) -
             port_config@turnover_constraint_policy@turnover_cap_rules[2], 0)
  ))


  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that dy_med_36m (high importance in predictive model) is higher for port than for bench
  expect_gt(results@port_metrics_m_xts@data$dy_med_36m %>% mean(), results@port_metrics_m_xts@data$bench_dy_med_36m %>% mean())
  #Check that roe_3m (high importance in predictive model) is higher for port than for bench
  expect_gt(results@port_metrics_m_xts@data$roe_3m %>% mean(), results@port_metrics_m_xts@data$bench_roe_3m %>% mean())
  #Check that mom_res_12m (little importance in predictive model) is lower for port than for bench
  expect_lt(results@port_metrics_m_xts@data$mom_res_12m %>% mean(), results@port_metrics_m_xts@data$bench_mom_res_12m %>% mean())

  #Check for presence of target weights and multiple resamples
  expect_true("target_weights" %in% colnames(results@final_stock_universe_m_d_ref@data))
  expect_true("base_weights" %in% colnames(results@final_stock_universe_m_d_ref@data))

  #Check for stock port
  expect_equal(results@final_stock_port@type, "signal_blend")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, mvo_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "mvo")
  expect_equal(results@final_stock_port@exp_ret_score, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))

  #backtest identifier
  expect_equal(results@port_backtest_workflow[[1]]$sb_backtest_identifier, sb_results@backtest_identifier)

})

test_that("run_port_backtest works for a META-LEVEL oos_predictions blended strategy and 'mvo' with liquidity, turnover and concentration constraints and selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  target_m_df <- create_meta_dataframe(fwd_return_m_df@data, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m, mom_res_12m, roe_3m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  expect_warning(daily_stock_returns_m_xts <- create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3"),
                 "There are NA values in the time series.")

  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )


  #Create sb_backtest_configs
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", rebalancing_months = 4,
                                             training_sample_size = 2, target_fwd_name = "fwd_return_1m", config_name = "ronaldo") %>%
    add_tuning_strategy(tuning_method = "grid_search", chosen_eval_metric = "rmse", validation_sample_size = 1) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"),
                       grid = list(c(0, 0.5, 1), seq(0.1, 0.9, length=10)))

  ols_config <- create_sb_backtest_config(sb_algorithm = "ols", rebalancing_months = 4,
                                          training_sample_size = 3, target_fwd_name = "fwd_return_1m", config_name = "romario")

  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "glmnet", rebalancing_months = 4,
                                                   training_sample_size = 2, target_fwd_name = "fwd_return_1m") %>%
    add_tuning_strategy(tuning_method = "grid_search", chosen_eval_metric = "rmse", validation_sample_size = 1) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"),
                       grid = list(c(0, 0.5, 1), seq(0.1, 0.9, length=10)))

  meta_backtest_config <- create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                                        features_passthrough = "none", config_name = "pele")

  #run_sb_backtest
  expect_warning(
    glmnet_results <- run_sb_backtest(
      features_m_df = signals_m_df,
      target_m_df = target_m_df,
      config = glmnet_config,
      parallel = TRUE
    ),
    "Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest."
  )

  expect_warning(
    ols_results <- run_sb_backtest(
      features_m_df = signals_m_df,
      target_m_df = target_m_df,
      config = ols_config,
      parallel = TRUE
    ),
    "Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest."
  )

  #run meta sb
  meta_sb_results <- run_sb_backtest(
    features_m_df = signals_m_df,
    target_m_df = target_m_df,
    config = meta_backtest_config,
    base_sb_backtest_results_list = list(glmnet_results, ols_results),
    parallel = TRUE)


  #Create port_backtest_config
  port_config <- create_port_backtest_config(eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "mvo",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>% add_liquidity_floor_cutoffs(
    metric_name = c("mean_volfin_3m", "presence"),
    metric_cutoffs = list(
      c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
      c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
    )
  ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "micro_caps", liquidity_cap_rules = c(micro_caps = 0.01, small_caps = 0.02)) %>%
    add_turnover_constraint_policy(quantile_range_buffer = 0.1, turnover_cap_rules = c(micro_caps = 0.01, small_caps = 0.02)) %>%
    add_concentration_constraint_policy(max_abs_active_individual_weight = 0.03, max_abs_active_group_weight = c(sectors = 0.10, macro_sector = 0.05)) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_mvo_parameters(n_random_ports = 500, opt_objective = "sharpe") %>%
    add_cov_est_method(cov_estimation_method = "shrink_cc", cov_matrix_sample_size = 52, active_returns = TRUE)


  #Run port_backtest
  set.seed(123)
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 config = port_config,
                                 sb_backtest_results = meta_sb_results,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  set.seed(123)
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- meta_sb_results@meta_sb_backtest_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)



  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  mvo_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- meta_sb_results@meta_sb_backtest_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- meta_sb_results@meta_sb_backtest_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  mvo_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, mvo_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 97.5 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 97.5) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               oos_predictions_m_d_ref$pred %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               mvo_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               mvo_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights respect concentration_constraint
  expect_true(all(
    results@stock_universe_m_df@data$weights <=
      results@stock_universe_m_df@data$ibov_bench_weights + port_config@concentration_constraint_policy@max_abs_active_individual_weight
  ))

  expect_true(all(
    results@stock_universe_m_df@data$weights >=
      pmax(0, results@stock_universe_m_df@data$ibov_bench_weights - port_config@concentration_constraint_policy@max_abs_active_individual_weight)
  ))


  #Check that sector weights respect sector_concentration_constraint
  sectors_benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates %in% c("2023-02-15", "2023-04-15")) %>% dplyr::left_join(stock_groups_m_df@data %>% dplyr::select(-tickers, -dates), by = "id") %>%
    dplyr::group_by(sectors, dates) %>% dplyr::summarise(bench_total = sum(ibov))

  sectors_port_weights_m_d_ref <-  results@stock_universe_m_df@data %>%
    dplyr::group_by(sectors,dates) %>% dplyr::summarise(port_total = sum(weights))

  expect_equal(sectors_benchmark_weights_m_d_ref$bench_total %>% sum(), 2) #two periods = 100% + 100%
  expect_equal(sectors_port_weights_m_d_ref$port_total %>% sum(), 2)

  expect_true(all(
    sectors_port_weights_m_d_ref$port_total <= sectors_benchmark_weights_m_d_ref$bench_total + port_config@concentration_constraint_policy@max_abs_active_group_weight[1])
  )
  expect_true(all(
    sectors_port_weights_m_d_ref$port_total >= pmax(0, sectors_benchmark_weights_m_d_ref$bench_total - port_config@concentration_constraint_policy@max_abs_active_group_weight[1]))
  )

  #Check that macro_sector weights respect sector_concentration_constraint
  macro_sector_benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates %in% c("2023-02-15", "2023-04-15")) %>% dplyr::left_join(stock_groups_m_df@data %>% dplyr::select(-tickers, -dates), by = "id") %>%
    dplyr::group_by(macro_sector, dates) %>% dplyr::summarise(bench_total = sum(ibov))

  macro_sector_port_weights_m_d_ref <-  results@stock_universe_m_df@data %>%
    dplyr::group_by(macro_sector, dates) %>% dplyr::summarise(port_total = sum(weights))

  expect_equal(macro_sector_benchmark_weights_m_d_ref$bench_total %>% sum(), 2) #two periods = 100% + 100%
  expect_equal(macro_sector_port_weights_m_d_ref$port_total %>% sum(), 2)

  expect_true(all(
    macro_sector_port_weights_m_d_ref$port_total <= macro_sector_benchmark_weights_m_d_ref$bench_total + port_config@concentration_constraint_policy@max_abs_active_group_weight[2])
  )
  expect_true(all(
    macro_sector_port_weights_m_d_ref$port_total >= pmax(0, macro_sector_benchmark_weights_m_d_ref$bench_total - port_config@concentration_constraint_policy@max_abs_active_group_weight[2]))
  )

  #Check that weights respect liquidity cap
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "small_caps") %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "small_caps") %>% dplyr::pull(ibov_bench_weights) + port_config@liquidity_constraint_policy@liquidity_cap_rules[2]
  ))
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "micro_caps") %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(liquidity_classification == "micro_caps") %>% dplyr::pull(ibov_bench_weights) + port_config@liquidity_constraint_policy@liquidity_cap_rules[1]
  ))

  #Check that weights respect turnover constraint
  #Inclusion of buffered stocks
  expect_equal(
    results@stock_universe_m_df@data %>%
      dplyr::filter(exp_ret_score >= quantile(exp_ret_score, 0.57), exp_ret_score <= quantile(exp_ret_score, 0.67),
                    liquidity_classification == "micro_caps", bop_port_weights > 0) %>% dplyr::pull(buffer_zone_1) %>% unique(),
    1)

  #Weights respect cap
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(bop_port_weights) +
      port_config@turnover_constraint_policy@turnover_cap_rules[1]
  ))
  ##Note: JHSF3 is an interesting case of a stock that would a turnover cap min weight of ~0.2. However, a stock can never have
  ##a constrained minimum weight that is greater than benchmark weight (this would mean that the constraint is making one have a strictly positive
  ##active weight)
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(weights) >=
      pmin(
      pmax(results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_1 == 1) %>%
             dplyr::pull(bop_port_weights) - port_config@turnover_constraint_policy@turnover_cap_rules[1],
           0),
      0
      )
  ))

  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(weights) <=
      results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(bop_port_weights) +
      port_config@turnover_constraint_policy@turnover_cap_rules[2]
  ))
  expect_true(all(
    results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(weights) >=
      pmax(results@stock_universe_m_df@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(bop_port_weights) -
             port_config@turnover_constraint_policy@turnover_cap_rules[2], 0)
  ))


  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check that dy_med_36m (high importance in predictive model) is higher for port than for bench
  expect_gt(results@port_metrics_m_xts@data$dy_med_36m %>% mean(), results@port_metrics_m_xts@data$bench_dy_med_36m %>% mean())
  #Check that roe_3m (high importance in predictive model) is higher for port than for bench
  expect_gt(results@port_metrics_m_xts@data$roe_3m %>% mean(), results@port_metrics_m_xts@data$bench_roe_3m %>% mean())
  #Check that mom_res_12m (little importance in predictive model) is lower for port than for bench
  expect_lt(results@port_metrics_m_xts@data$mom_res_12m %>% mean(), results@port_metrics_m_xts@data$bench_mom_res_12m %>% mean())


  #Check for stock port
  expect_equal(results@final_stock_port@type, "signal_blend")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, mvo_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "mvo")
  expect_equal(results@final_stock_port@exp_ret_score, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))

  #backtest identifier
  expect_equal(results@port_backtest_workflow[[1]]$sb_backtest_identifier, meta_sb_results@backtest_identifier)
  expect_equal(results@port_backtest_workflow[[1]]$oos_predictions_object_name,
               meta_sb_results@meta_sb_backtest_results@oos_sb_outputs_m_df@meta_dataframe_name)

})

test_that("run_port_backtest works for a benchmark-agnostic oos_predictions blended strategy and 'mvo' with liquidity, turnover and user_rules, but no selected benchmark", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  target_m_df <- create_meta_dataframe(fwd_return_m_df@data, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, dy_med_36m, mom_res_12m, roe_3m))
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  daily_stock_returns_m_xts <- suppressWarnings(
    create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks", meta_xts_name = "B3")
  )
  daily_benchmark_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )
  too_big_stocks <- benchmark_weights_m_df@data %>% dplyr::slice_max(order_by = ibov, n = 50) %>% dplyr::pull(id)
  user_defined_AND_rules_m_df <- create_meta_dataframe(
    benchmark_weights_m_df@data %>%
      dplyr::mutate(small_classification = dplyr::case_when(
        ibov < 0.01 ~ "small",
        ibov >= 0.01 & ibov < 0.05 ~ "mid",
        ibov >= 0.05 ~ "big"
      )) %>%
      dplyr::mutate(is_small = dplyr::if_else(!id %in% too_big_stocks, 1, 0)) %>%
      dplyr::select(-ibov)
  )


  #Create sb_backtest_config
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", rebalancing_months = 4,
                                             training_sample_size = 3, target_fwd_name = "fwd_return_1m") %>%
    add_tuning_strategy(tuning_method = "grid_search", chosen_eval_metric = "rmse", validation_sample_size = 2) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"),
                       grid = list(c(0, 0.5, 1), seq(0.1, 0.9, length=10)))


  expect_warning(
    sb_results <- run_sb_backtest(
      features_m_df = signals_m_df,
      target_m_df = target_m_df,
      config = glmnet_config,
      parallel = TRUE
    ), "Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest."
  )


  #Create port_backtest_config
  port_config <- create_port_backtest_config(eligibility_quantile_range = c(0.67, 1.0),
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "mvo",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "micro_caps") %>%
    add_turnover_constraint_policy(quantile_range_buffer = 0.1, turnover_cap_rules = c(micro_caps = 0.01, small_caps = 0.02)) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000) %>%
    add_mvo_parameters(n_random_ports = 500, opt_objective = "return") %>%
    add_cov_est_method(cov_estimation_method = "shrink_id", cov_matrix_sample_size = 52, active_returns = FALSE)


  #Run port_backtest
  expect_warning(
  expect_warning(
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 config = port_config,
                                 sb_backtest_results = sb_results,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 user_defined_AND_rules_m_df = user_defined_AND_rules_m_df,
                                 verbose = TRUE,
                                 .test_seed = 123),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  ), "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint."
  ),
  "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  user_defined_AND_rules_m_d_ref <- user_defined_AND_rules_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    user_defined_AND_rules_m_d_ref = user_defined_AND_rules_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy)
  )

  #Set Port Weights
  set.seed(123)
  mvo_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = NULL,
    opt_objective = "return",
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

  #port_allocation
  expect_warning(
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = NULL
  ), "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint."
  )

  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = NULL,
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = NULL
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = NULL,
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  oos_predictions_m_d_ref <- sb_results@oos_sb_outputs_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts@data[which(zoo::index(daily_stock_returns_m_xts@data) <= current_date),]
  daily_bench_returns_m_xts_upd_ref <- daily_benchmark_returns_m_xts_mocked@data[which(zoo::index(daily_benchmark_returns_m_xts_mocked@data) <= current_date),]
  stock_groups_m_d_ref <- stock_groups_m_df@data %>% dplyr::filter(dates == current_date)
  user_defined_AND_rules_m_d_ref <- user_defined_AND_rules_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = oos_predictions_m_d_ref,
    chosen_score_metric_and_position = NULL,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    user_defined_AND_rules_m_d_ref = user_defined_AND_rules_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy)
  )

  #Set Port Weights
  set.seed(123)
  mvo_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = NULL,
    opt_objective = "return",
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )

  #port_allocation
  expect_warning(
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = NULL
  ), "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint."
  )

  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = NULL,
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, mvo_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check for buffered stocks
  buffer_1_stocks <- results@final_stock_universe_m_d_ref@data %>%
    dplyr::filter(bop_port_weights > 0, liquidity_classification == "micro_caps", exp_ret_score >= quantile(exp_ret_score, 0.57)) %>% dplyr::pull(tickers)
  buffer_2_stocks <- results@final_stock_universe_m_d_ref@data %>%
    dplyr::filter(bop_port_weights > 0, liquidity_classification == "small_caps", exp_ret_score >= quantile(exp_ret_score, 0.57)) %>% dplyr::pull(tickers)

  expect_equal(results@final_stock_universe_m_d_ref@data %>% dplyr::filter(buffer_zone_1 == 1) %>% dplyr::pull(tickers), buffer_1_stocks)
  expect_equal(results@final_stock_universe_m_d_ref@data %>% dplyr::filter(buffer_zone_2 == 1) %>% dplyr::pull(tickers), buffer_2_stocks)



  #Check that all with presence < 97.5 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 97.5) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               oos_predictions_m_d_ref$pred %>% signal_transform(lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975)
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               mvo_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               mvo_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check if all eligible are in user_defined_AND_rules_m_d_ref
  expect_equal(results@stock_universe_m_df@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(is_small) %>% unique(), 1)

  #Check if all that are not in user_defined_AND_rules_m_d_ref are not eligible
  expect_equal(results@stock_universe_m_df@data %>% dplyr::filter(is_small == 0) %>% dplyr::pull(is_eligible) %>% unique(), 0)


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )


  #Check for stock port
  expect_equal(results@final_stock_port@type, "signal_blend")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, mvo_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "mvo")
  expect_equal(results@final_stock_port@exp_ret_score, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))


})

test_that("run_port_backtest work for a benchmark-sensitive cohort of cw, cs and sw ports", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data)

  #Create port_backtest_config 1
  chosen_score_metric_and_position <- c(book_yield = "long")
  sw_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 2,
                                             rebalancing_months = 4,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "sw_book_yield"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Run port_backtest
  expect_warning(
    sw_results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = sw_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )



  #Create port_backtest_config 2
  chosen_score_metric_and_position <- c(book_yield = "long")
  cs_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                           eligibility_quantile_range = c(0.67, 1.0),
                                           selected_benchmark = "ibov",
                                           initial_buffer_period = 2,
                                           rebalancing_months = 4,
                                           port_construction_method = "cs",
                                           main_liquidity_metric = "mean_volfin_3m",
                                           config_name = "cs_book_yield"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Run port_backtest
  skimmed_port_metrics_m_df <- port_metrics_m_df
  skimmed_port_metrics_m_df@data <- port_metrics_m_df@data %>% dplyr::select(-vol_36m)

  expect_warning(
    cs_results <- run_port_backtest(signals_m_df = signals_m_df,
                                    fwd_return_m_df = fwd_return_m_df,
                                    liquidity_m_df = liquidity_m_df,
                                    volatility_m_df = volatility_m_df,
                                    config = cs_config,
                                    benchmark_weights_m_df = benchmark_weights_m_df,
                                    benchmark_returns_m_xts = benchmark_returns_m_xts,
                                    custom_stock_metrics_m_df = skimmed_port_metrics_m_df,
                                    verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Create port_backtest_config 3
  chosen_score_metric_and_position <- c(vol_36m = "short")
  cw_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                           eligibility_quantile_range = c(0.67, 1.0),
                                           selected_benchmark = "ibov",
                                           initial_buffer_period = 2,
                                           rebalancing_months = 4,
                                           port_construction_method = "cw",
                                           main_liquidity_metric = "mean_volfin_3m",
                                           config_name = "cw_book_yield"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Run port_backtest
  expect_warning(
    cw_results <- run_port_backtest(signals_m_df = signals_m_df,
                                    fwd_return_m_df = fwd_return_m_df,
                                    liquidity_m_df = liquidity_m_df,
                                    volatility_m_df = volatility_m_df,
                                    config = cw_config,
                                    benchmark_weights_m_df = benchmark_weights_m_df,
                                    benchmark_returns_m_xts = benchmark_returns_m_xts,
                                    custom_stock_metrics_m_df = port_metrics_m_df,
                                    verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Create cohort
  port_cohort <- create_port_backtest_cohort(list(sw_results, cs_results, cw_results),
                                             cohort_name = "generic_cohort")

  #Check that there are 3 backtest results
  expect_equal(length(port_cohort@port_backtest_results_list), 3)

  #Check that port_weights are according to expectation
  expect_equal(port_cohort@port_weights_m_df@data$`c:sw_book_yield_s:not_identified_f:not_identified`, sw_results@port_weights_m_df@data$eop_port_weights)
  expect_equal(port_cohort@port_weights_m_df@data$`c:cs_book_yield_s:not_identified_f:not_identified`, cs_results@port_weights_m_df@data$eop_port_weights)
  expect_equal(port_cohort@port_weights_m_df@data$`c:cw_book_yield_s:not_identified_f:not_identified`, cw_results@port_weights_m_df@data$eop_port_weights)
  expect_equal(port_cohort@port_weights_m_df@data$bench_weights, cw_results@port_weights_m_df@data$bench_weights)
  expect_equal(port_cohort@port_weights_m_df@data$bench_weights, sw_results@port_weights_m_df@data$bench_weights)
  expect_equal(port_cohort@port_weights_m_df@data$bench_weights, cs_results@port_weights_m_df@data$bench_weights)

  expect_true(all(cs_results@port_weights_m_df@data$id %in% port_cohort@port_weights_m_df@data$id))

  #Check that port returns are according to expectation
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c.sw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$selected_bench_return, sw_results@port_returns_m_xts@data$selected_bench_return)
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$selected_bench_return, cs_results@port_returns_m_xts@data$selected_bench_return)
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$selected_bench_return, cw_results@port_returns_m_xts@data$selected_bench_return)

  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c.sw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$selected_bench_return, sw_results@port_returns_m_xts@data$selected_bench_return)
  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$selected_bench_return, cs_results@port_returns_m_xts@data$selected_bench_return)
  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$selected_bench_return, cw_results@port_returns_m_xts@data$selected_bench_return)

  expect_equal(port_cohort@port_returns_m_xts_list$raw_active_returns_m_xts@data$c.sw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_returns_m_xts@data$raw_active_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$raw_active_returns_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_returns_m_xts@data$raw_active_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$raw_active_returns_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_returns_m_xts@data$raw_active_return %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_returns_m_xts_list$net_active_returns_m_xts@data$c.sw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_returns_m_xts@data$net_active_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$net_active_returns_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_returns_m_xts@data$net_active_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$net_active_returns_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_returns_m_xts@data$net_active_return %>% as.data.frame() %>% unname())


  #Check that port costs are accordng to expectation
  expect_equal(port_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c.sw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c.sw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c.sw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_costs_m_xts_list$turnover_m_xts@data$c.sw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$turnover_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$turnover_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())

  #Port Metrics
  #Check that length is 3 (no sw)
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data %>% ncol(), 3)
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_metrics_m_xts@data$book_yield %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_metrics_m_xts@data$book_yield %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data$bench_book_yield %>% as.data.frame() %>% unname(),
               cs_results@port_metrics_m_xts@data$bench_book_yield %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data$bench_book_yield %>% as.data.frame() %>% unname(),
               cw_results@port_metrics_m_xts@data$bench_book_yield %>% as.data.frame() %>% unname())


  expect_equal(port_cohort@port_metrics_m_xts_list$dy_med_36m_m_xts@data %>% ncol(), 3)
  expect_equal(port_cohort@port_metrics_m_xts_list$dy_med_36m_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_metrics_m_xts@data$dy_med_36m %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_metrics_m_xts_list$dy_med_36m_m_xts@data$c.cw_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_metrics_m_xts@data$dy_med_36m %>% as.data.frame() %>% unname())

  #For vol_36m, there is data only for cs
  expect_equal(port_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data %>% ncol(), 2)
  expect_equal(port_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data$c.cs_book_yield_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_metrics_m_xts@data$vol_36m %>% as.data.frame() %>% unname())

  #Check that all metrics are present
  expect_equal(stringr::str_remove(names(port_cohort@port_metrics_m_xts_list), "_m_xts"), colnames(port_metrics_m_df@data)[-c(1:3)])


})

test_that("run_port_backtest work for a benchmark-agnostic long-short cohort", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id,tickers,dates,vol_36m))

  #Create port_backtest_config 1
  chosen_score_metric_and_position <- c(vol_36m = "long")
  long_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                           eligibility_quantile_range = c(0.67, 1.0),
                                           initial_buffer_period = 2,
                                           rebalancing_months = 4,
                                           port_construction_method = "sw",
                                           main_liquidity_metric = "mean_volfin_3m",
                                           config_name = "long_vol_36m"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Run port_backtest
  expect_warning(
    long_results <- run_port_backtest(signals_m_df = signals_m_df,
                                      fwd_return_m_df = fwd_return_m_df,
                                      liquidity_m_df = liquidity_m_df,
                                      volatility_m_df = volatility_m_df,
                                      config = long_config,
                                      custom_stock_metrics_m_df = port_metrics_m_df,
                                      verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )



  #Create port_backtest_config 2
  chosen_score_metric_and_position <- c(vol_36m = "short")
  short_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             initial_buffer_period = 2,
                                             rebalancing_months = 4,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "short_vol_36m"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Run port_backtest
  expect_warning(
    short_results <- run_port_backtest(signals_m_df = signals_m_df,
                                      fwd_return_m_df = fwd_return_m_df,
                                      liquidity_m_df = liquidity_m_df,
                                      volatility_m_df = volatility_m_df,
                                      config = short_config,
                                      custom_stock_metrics_m_df = port_metrics_m_df,
                                      verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Create cohort
  ls_cohort <- create_port_backtest_cohort(list(long_results, short_results), cohort_name = "vol_ls_cohort")

  #Check that there are 2 backtest results
  expect_equal(length(ls_cohort@port_backtest_results_list), 2)

  #Check that port_weights are according to expectation
  expect_equal(ls_cohort@port_weights_m_df@data$c__long_vol_36m_s__not_identified_f__not_identified, long_results@port_weights_m_df@data$eop_port_weights)
  expect_equal(ls_cohort@port_weights_m_df@data$c__short_vol_36m_s__not_identified_f__not_identified, short_results@port_weights_m_df@data$eop_port_weights)
  expect_null(ls_cohort@port_weights_m_df@data$bench_weights)

  expect_true(all(long_results@port_weights_m_df@data$id %in% ls_cohort@port_weights_m_df@data$id))

  #Check that port returns are according to expectation
  expect_equal(ls_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c__long_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               long_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c__short_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               short_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())
  expect_null(ls_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$selected_bench_return)

  expect_equal(ls_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c__long_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               long_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c__short_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               short_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())
  expect_null(ls_cohort@port_returns_m_xts_list$net_returns_m_xts@data$selected_bench_return)

  #Only raw and net returns xts
  expect_equal(names(ls_cohort@port_returns_m_xts_list), c("raw_returns_m_xts", "net_returns_m_xts"))


  #Check that port costs are accordng to expectation
  expect_equal(ls_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c__long_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               long_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c__short_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               short_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())

  expect_equal(ls_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c__long_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               long_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c__short_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               short_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())

  expect_equal(ls_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c__long_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               long_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c__short_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               short_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())

  expect_equal(ls_cohort@port_costs_m_xts_list$turnover_m_xts@data$c__long_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               long_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_costs_m_xts_list$turnover_m_xts@data$c__short_vol_36m_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               short_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())


  #Port Metrics
  #Check that length is 2 (no sw)
  expect_equal(ls_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data %>% ncol(), 2)
  expect_gt(ls_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data$c__long_vol_36m_s__not_identified_f__not_identified %>% mean(),
            ls_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data$c__short_vol_36m_s__not_identified_f__not_identified %>% mean())

  #Check that all metrics are present
  expect_equal(stringr::str_remove(names(ls_cohort@port_metrics_m_xts_list), "_m_xts"), colnames(port_metrics_m_df@data)[-c(1:3)])

})

test_that("run_port_backtest works for sector ports", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Add a random sector
  signals_m_df <- signals_m_df %>%
    dplyr::mutate(financeiro = dplyr::if_else(tickers %in% c("ABCB4", "BBAS3", "ITUB4", "SANB11", "BMGB4", "BRBI11",
                                                             "BBDC4", "BRSR6", "BBDC3", "ITUB3", "BPAC11", "BPAC3"),
                                              1,0))


  #Create port_backtest_config
  chosen_score_metric_and_position <- c(financeiro = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "ew",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts["2022-10-15/2023-04-15"])
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m))


  #Run port_backtest
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected results
  current_date <- "2023-02-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)

  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- signals_m_df@data %>% dplyr::filter(dates == "2023-01-15") %>%
    dplyr::select(id, tickers, dates) %>% dplyr::mutate(bop_port_weights = 0)

  #Derive Universe
  stock_universe_m_d_ref_1 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Check that all financeiro is 1 for pre eligible and all is eligible is 1 for financeiro
  financeiro_id <- signals_m_d_ref %>% dplyr::filter(financeiro == 1) %>% dplyr::pull(id)
  expect_equal(
    stock_universe_m_d_ref_1 %>% dplyr::filter(id %in% financeiro_id) %>% dplyr::pull(pre_eligible_assets) %>% unique(),
    1)
  expect_equal(
    stock_universe_m_d_ref_1 %>% dplyr::filter(!id %in% financeiro_id) %>% dplyr::pull(pre_eligible_assets) %>% unique(),
    0)
  expect_true(all((stock_universe_m_d_ref_1 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(id)) %in% financeiro_id))
  expect_equal(stock_universe_m_d_ref_1 %>% dplyr::pull(exp_ret_score) %>% unique() %>% length(), 2)


  #Set Port Weights
  ew_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "ew"
  )
  expect_equal(ew_port_1@universe_m_d_ref@data %>% dplyr::filter(weights > 0) %>% dplyr::pull(weights) %>% unique() %>% length(),
              1)
  expect_equal(ew_port_1@universe_m_d_ref@data %>% dplyr::filter(!id %in% financeiro_id) %>% dplyr::pull(weights) %>% unique(), 0)


  #port_allocation
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = ew_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  expect_equal(port_allocation_1$port_weights_m_d_ref %>% dplyr::filter(eop_port_weights > 0) %>% dplyr::pull(eop_port_weights) %>% unique() %>% length(),
               1)
  expect_equal(port_allocation_1$port_weights_m_d_ref %>% dplyr::filter(eop_port_weights > 0, id %in% financeiro_id) %>% dplyr::pull(eop_port_weights) %>% sum(),
               1)
  expect_equal(port_allocation_1$port_weights_m_d_ref %>% dplyr::filter(!id %in% financeiro_id) %>% dplyr::pull(eop_port_weights) %>% sum(),
               0)



  #Port Metric
  port_metric_1 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_1 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-03-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_1$port_weights_m_d_ref,
                           total_cost = port_allocation_1$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #2nd date
  current_date <- "2023-03-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_1$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Roll portfolio
  port_allocation_2 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  #Port Metric
  port_metric_2 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )



  port_roll_2 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-04-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_2$port_weights_m_d_ref,
                           total_cost = port_allocation_2$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #3rd date
  current_date <- "2023-04-15"
  signals_m_d_ref <- signals_m_df@data %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df@data %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df@data %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df@data %>% dplyr::filter(dates == current_date)
  port_metrics_m_d_ref <- port_metrics_m_df@data %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df@data %>% dplyr::filter(dates == current_date)


  #placeholder
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  updated_port_weights_m_lstd_ref <- port_roll_2$rolled_fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = updated_port_weights)

  #Derive Universe
  stock_universe_m_d_ref_2 <- derive_stock_universe_m_d_ref(
    signals_m_d_ref = signals_m_d_ref,
    oos_predictions_m_d_ref = NULL,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    lower_quantile_winsorization = 0.025,
    upper_quantile_winsorization = 0.975
  ) %>% classify_investment_universe(
    eligibility_quantile_range = c(0.67, 1.0),
    min_eligible_assets_fallback = NULL,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = port_config@liquidity_floor_cutoffs,
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Check that all financeiro is 1 for pre eligible and all is eligible is 1 for financeiro
  financeiro_id <- signals_m_d_ref %>% dplyr::filter(financeiro == 1) %>% dplyr::pull(id)
  expect_equal(
    stock_universe_m_d_ref_2 %>% dplyr::filter(id %in% financeiro_id) %>% dplyr::pull(pre_eligible_assets) %>% unique(),
    1)
  expect_equal(
    stock_universe_m_d_ref_2 %>% dplyr::filter(!id %in% financeiro_id) %>% dplyr::pull(pre_eligible_assets) %>% unique(),
    0)
  expect_true(all((stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(id)) %in% financeiro_id))
  expect_equal(stock_universe_m_d_ref_2 %>% dplyr::pull(exp_ret_score) %>% unique() %>% length(), 2)


  #Set Port Weights
  ew_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "ew"
  )

  expect_equal(ew_port_2@universe_m_d_ref@data %>% dplyr::filter(weights > 0) %>% dplyr::pull(weights) %>% unique() %>% length(),
               1)
  expect_equal(ew_port_2@universe_m_d_ref@data %>% dplyr::filter(!id %in% financeiro_id) %>% dplyr::pull(weights) %>% unique(), 0)


  #port_allocation
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = ew_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
  )

  expect_equal(port_allocation_3$port_weights_m_d_ref %>% dplyr::filter(eop_port_weights > 0) %>% dplyr::pull(eop_port_weights) %>% unique() %>% length(),
               1)
  expect_equal(port_allocation_3$port_weights_m_d_ref %>% dplyr::filter(eop_port_weights > 0, id %in% financeiro_id) %>% dplyr::pull(eop_port_weights) %>% sum(),
               1)
  expect_equal(port_allocation_3$port_weights_m_d_ref %>% dplyr::filter(!id %in% financeiro_id) %>% dplyr::pull(eop_port_weights) %>% sum(),
               0)


  #Port Metric
  port_metric_3 <- calculate_port_metrics(
    port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
    custom_stock_metrics_m_d_ref = port_metrics_m_d_ref
  )

  #Roll portfolio
  port_roll_3 <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                           fwd_selected_benchmark_return = benchmark_returns_m_xts@data["2023-05-15", "ibov"] %>% as.numeric(),
                           port_weights_m_d_ref = port_allocation_3$port_weights_m_d_ref,
                           total_cost = port_allocation_3$port_costs_d_ref$total_cost,
                           verbose = TRUE
  )

  #Check that financeiro is never chosen
  overall_non_financeiro_id <- signals_m_df@data %>% dplyr::filter(!financeiro == 1) %>% dplyr::pull(id)
  expect_equal(results@stock_universe_m_df@data %>% dplyr::filter(id %in% overall_non_financeiro_id) %>% dplyr::pull(weights) %>% unique(), 0)
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(id %in% overall_non_financeiro_id) %>% dplyr::pull(eop_port_weights) %>% unique(), 0)


  #Check if stock universe is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data, ew_port_2@universe_m_d_ref@data)

  #Check if there are micro caps in stock universe
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(liquidity_classification %in% c("nano_caps", "micro_caps")) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check that all with presence < 100 are not eligible
  expect_equal(nrow(results@stock_universe_m_df@data %>%
                      dplyr::filter(presence < 100) %>%
                      dplyr::filter(is_eligible == 1))
               , 0)

  #Check if exp_ret_score is as expected
  expect_equal(results@final_stock_universe_m_d_ref@data$exp_ret_score,
               signals_m_d_ref$financeiro #Just keep 1 and 0
  )

  #Check for port_returns
  expect_equal(results@port_returns_m_xts@data[1,] %>% as.numeric(),
               port_roll_1$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )
  expect_equal(results@port_returns_m_xts@data[2,] %>% as.numeric(),
               port_roll_2$fwd_port_returns_d_ref[1,] %>% as.numeric()
  )

  #Check for port_weights
  expect_equal(results@port_weights_m_df@data,
               rbind(port_allocation_1$port_weights_m_d_ref, port_allocation_2$port_weights_m_d_ref, port_allocation_3$port_weights_m_d_ref) %>%
                 dplyr::arrange(id)

  )

  #Check for port_weights for stocks
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(eop_port_weights),
               ew_port_1@universe_m_d_ref@data %>% dplyr::pull(weights)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-03-15") %>% dplyr::pull(eop_port_weights),
               port_allocation_2$port_weights_m_d_ref$eop_port_weights
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15") %>% dplyr::pull(eop_port_weights),
               ew_port_2@universe_m_d_ref@data %>% dplyr::pull(weights)
  )

  #Check that weights are equal in rebalancing months
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15", eop_port_weights > 0) %>% dplyr::pull(eop_port_weights) %>% unique() %>% length(),
               1
  )

  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-04-15", eop_port_weights > 0) %>% dplyr::pull(eop_port_weights) %>% unique() %>% length(),
               1
  )


  #Check for port_weights for benchmark
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )
  expect_equal(results@port_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(bench_weights),
               benchmark_weights_m_df@data %>% dplyr::filter(dates == "2023-02-15") %>% dplyr::pull(ibov)
  )


  #Check for port_costs
  expect_equal(results@port_costs_m_xts@data[1,] %>% as.numeric(),
               port_allocation_1$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[2,] %>% as.numeric(),
               port_allocation_2$port_costs_d_ref %>% as.numeric()
  )

  expect_equal(results@port_costs_m_xts@data[3,] %>% as.numeric(),
               port_allocation_3$port_costs_d_ref %>% as.numeric()
  )

  #Check for port_metric
  expect_equal(results@port_metrics_m_xts@data[1,] %>% as.numeric(),
               port_metric_1 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[2,] %>% as.numeric(),
               port_metric_2 %>% as.numeric()
  )
  expect_equal(results@port_metrics_m_xts@data[3,] %>% as.numeric(),
               port_metric_3 %>% as.numeric()
  )

  #Check for stock port
  expect_equal(results@final_stock_port@type, "single_signal")
  expect_equal(results@final_stock_port@main_liquidity_metric, "mean_volfin_3m")
  expect_equal(results@final_stock_port@universe_m_d_ref@data, ew_port_2@universe_m_d_ref@data)
  expect_equal(results@final_stock_port@port_construction_method, "ew")
  expect_equal(results@final_stock_port@eligible_assets, stock_universe_m_d_ref_2 %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  #Check for transactions_log
  expect_equal(results@transactions_log@data$`2023-02-15`, port_allocation_1$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-03-15`, port_allocation_2$transactions_log_m_d_ref)
  expect_equal(results@transactions_log@data$`2023-04-15`, port_allocation_3$transactions_log_m_d_ref)

  #Check for dates in m_xts
  #Port Ret
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[1]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_returns_m_xts@data)[2]), as.Date(c("2023-04-15")))
  #Port Costs
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[1]), as.Date(c("2023-02-16")))
  expect_equal(as.Date(zoo::index(results@port_costs_m_xts@data)[2]), as.Date(c("2023-03-16")))
  #Port Metrics
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[1]), as.Date(c("2023-02-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[2]), as.Date(c("2023-03-15")))
  expect_equal(as.Date(zoo::index(results@port_metrics_m_xts@data)[3]), as.Date(c("2023-04-15")))



  #Summary, plot and print
  expect_no_error(print(results))
  expect_no_error(print(port_config))

})


#ERRORS
test_that("run_port_backtest throws error for incompatible port_backtests", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id,tickers,dates,vol_36m))

  #Create port_backtest_config 1
  chosen_score_metric_and_position <- c(vol_36m = "long")
  config1 <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             initial_buffer_period = 2,
                                             rebalancing_months = 4,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "long_vol_36m"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Run port_backtest
  expect_warning(
    config1_results <- run_port_backtest(signals_m_df = signals_m_df,
                                      fwd_return_m_df = fwd_return_m_df,
                                      liquidity_m_df = liquidity_m_df,
                                      volatility_m_df = volatility_m_df,
                                      config = config1,
                                      custom_stock_metrics_m_df = port_metrics_m_df,
                                      verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )



  #Create port_backtest_config 2
  chosen_score_metric_and_position <- c(vol_36m = "short")
  config2 <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                              eligibility_quantile_range = c(0.67, 1.0),
                                              initial_buffer_period = 2,
                                              rebalancing_months = 4,
                                              port_construction_method = "sw",
                                              main_liquidity_metric = "mean_volfin_3m",
                                              config_name = "short_vol_36m"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Run port_backtest
  expect_warning(
    config2_results <- run_port_backtest(signals_m_df = signals_m_df,
                                       fwd_return_m_df = fwd_return_m_df,
                                       liquidity_m_df = liquidity_m_df,
                                       volatility_m_df = volatility_m_df,
                                       config = config2,
                                       custom_stock_metrics_m_df = port_metrics_m_df,
                                       verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )


  #Create cohort
  #Different selected_benchmark
  wrong_config1_results <- config1_results
  wrong_config1_results@port_backtest_workflow[[length(wrong_config1_results@port_backtest_workflow)]]$selected_benchmark <- "ibov"

  expect_error(create_port_backtest_cohort(list(wrong_config1_results, config2_results), cohort_name = "wrong_cohort"),
               "All backtests must use the same benchmark.")

  #Different initial buffer period
  wrong_config1_results <- config1_results
  wrong_config1_results@port_backtest_workflow$`2023-04-15`$initial_buffer_period <- 3

  expect_error(create_port_backtest_cohort(list(wrong_config1_results, config2_results), cohort_name = "wrong_cohort"),
               "Incompatibility found in parameter: initial_buffer_period for backtest result at index 2")

  #Different signals_obj name
  wrong_config1_results <- config1_results
  wrong_config1_results@port_backtest_workflow$`2023-04-15`$signals_object_name <- "signals123"

  expect_error(create_port_backtest_cohort(list(wrong_config1_results, config2_results), cohort_name = "wrong_cohort"),
               "Incompatibility found in parameter: signals_object_name for backtest result at index 2")

  #Repeated backtests
  expect_error(create_port_backtest_cohort(list(config1_results, config1_results), cohort_name = "wrong_cohort"),
               "Backtest identifiers must be unique.")

  #Wrong ids in port weights
  wrong_config1_results <- config1_results
  wrong_config1_results@port_weights_m_df@data$id[4] <- "AALR4-2023-02-15"

  expect_error(create_port_backtest_cohort(list(wrong_config1_results, config2_results), cohort_name = "wrong_cohort"),
               "Mismatch in id, tickers, or dates in port_weights_m_df of backtest result at index 2")

  #Wrong date in port_returns
  wrong_config1_results <- config1_results
  wrong_config1_results@port_returns_m_xts@data <- xts::xts(as.data.frame(wrong_config1_results@port_returns_m_xts@data),
                                                            order.by = as.Date(c("2022-11-15", "2023-01-15", "2023-02-15", "2023-03-15", "2023-04-15")))

  expect_error(create_port_backtest_cohort(list(wrong_config1_results, config2_results), cohort_name = "wrong_cohort"),
               "Dates do not match across port_returns_m_xts for column: raw_return")

  #Wrong date in port_metrics
  wrong_config1_results <- config1_results
  wrong_config1_results@port_metrics_m_xts@data <- xts::xts(as.data.frame(wrong_config1_results@port_metrics_m_xts@data),
                                                            order.by = as.Date(c("2022-11-15", "2022-12-16", "2023-01-15", "2023-02-15", "2023-03-15", "2023-04-15")))

  expect_error(create_port_backtest_cohort(list(wrong_config1_results, config2_results), cohort_name = "wrong_cohort"),
               "Dates do not match across port_metrics_m_xts for metric: vol_36m")

})

test_that("run_port_backtest throws error for wrong normalization in port_backtests", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id,tickers,dates,vol_36m))

  #Create port_backtest_config 1
  chosen_score_metric_and_position <- c(vol_36m = "long")
  config1 <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                         eligibility_quantile_range = c(0.67, 1.0),
                                         initial_buffer_period = 2,
                                         rebalancing_months = 4,
                                         port_construction_method = "sw",
                                         main_liquidity_metric = "mean_volfin_3m",
                                         config_name = "long_vol_36m"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  norm_recipe <- recipes::recipe(signals_m_df@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    step_winsorize(recipes::all_numeric_predictors())
  wrong_signals_m_df <- map_recipe_timewise(signals_m_df, norm_recipe)

  #Run port_backtest
  expect_warning(
  run_port_backtest(signals_m_df = signals_m_df,
                                         fwd_return_m_df = fwd_return_m_df,
                                         liquidity_m_df = liquidity_m_df,
                                         volatility_m_df = volatility_m_df,
                                         config = config1,
                                         custom_stock_metrics_m_df = port_metrics_m_df,
                                         verbose = TRUE),
  "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  expect_warning(
    run_port_backtest(signals_m_df = wrong_signals_m_df,
                      fwd_return_m_df = fwd_return_m_df,
                      liquidity_m_df = liquidity_m_df,
                      volatility_m_df = volatility_m_df,
                      config = config1,
                      custom_stock_metrics_m_df = port_metrics_m_df,
                      verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  norm_recipe <- recipes::recipe(fwd_return_m_df@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::step_normalize(recipes::all_numeric_predictors(), na_rm = TRUE)
  suppressWarnings(
  wrong_fwd_return_m_df <- map_recipe_timewise(fwd_return_m_df, norm_recipe, type = "target")
  )

  #Run port_backtest
  expect_error(
    expect_warning(
    run_port_backtest(signals_m_df = signals_m_df,
                      fwd_return_m_df = wrong_fwd_return_m_df,
                      liquidity_m_df = liquidity_m_df,
                      volatility_m_df = volatility_m_df,
                      config = config1,
                      custom_stock_metrics_m_df = port_metrics_m_df,
                      verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest.")
    ,"Normalization found in fwd_return_m_df workflow."
  )

  norm_recipe <- recipes::recipe(liquidity_m_df@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::step_normalize(recipes::all_numeric_predictors(), na_rm = TRUE)
    wrong_liquidity_m_df <- map_recipe_timewise(liquidity_m_df, norm_recipe, type = "generic")


  #Run port_backtest
  expect_error(
    expect_warning(
    run_port_backtest(signals_m_df = signals_m_df,
                      fwd_return_m_df = fwd_return_m_df,
                      liquidity_m_df = wrong_liquidity_m_df,
                      volatility_m_df = volatility_m_df,
                      config = config1,
                      custom_stock_metrics_m_df = port_metrics_m_df,
                      verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
    ),
    "Normalization found in liquidity_m_df workflow."
  )



  norm_recipe <- recipes::recipe(volatility_m_df@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::step_normalize(recipes::all_numeric_predictors(), na_rm = TRUE)
  suppressWarnings(
    wrong_volatility_m_df <- map_recipe_timewise(volatility_m_df, norm_recipe, type = "generic")
  )

  #Run port_backtest
  expect_error(
    expect_warning(
    run_port_backtest(signals_m_df = signals_m_df,
                      fwd_return_m_df = fwd_return_m_df,
                      liquidity_m_df = liquidity_m_df,
                      volatility_m_df = wrong_volatility_m_df,
                      config = config1,
                      custom_stock_metrics_m_df = port_metrics_m_df,
                      verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
    ),
    "Normalization found in volatility_workflow workflow."
  )

})

test_that("run_port_backtest throws an error for selected benchmark missing", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id,tickers,dates,vol_36m))

  #Create port_backtest_config 1
  chosen_score_metric_and_position <- c(vol_36m = "long")
  config1 <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                         eligibility_quantile_range = c(0.67, 1.0),
                                         initial_buffer_period = 2,
                                         rebalancing_months = 4,
                                         port_construction_method = "sw",
                                         main_liquidity_metric = "mean_volfin_3m",
                                         config_name = "long_vol_36m"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Run port_backtest
  expect_error(
    expect_warning(
    run_port_backtest(signals_m_df = signals_m_df,
                      fwd_return_m_df = fwd_return_m_df,
                      liquidity_m_df = liquidity_m_df,
                      volatility_m_df = volatility_m_df,
                      benchmark_weights_m_df = benchmark_weights_m_df,
                      config = config1,
                      custom_stock_metrics_m_df = port_metrics_m_df,
                      verbose = TRUE)
  , "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest.")
  , "selected_benchmark must be provided with benchmark_weights_m_df."
  )


  #Run port_backtest
  expect_error(
    expect_warning(
    run_port_backtest(signals_m_df = signals_m_df,
                      fwd_return_m_df = fwd_return_m_df,
                      liquidity_m_df = liquidity_m_df,
                      volatility_m_df = volatility_m_df,
                      benchmark_returns_m_xts = benchmark_returns_m_xts,
                      config = config1,
                      custom_stock_metrics_m_df = port_metrics_m_df,
                      verbose = TRUE)
    , "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest.")
    ,"selected_benchmark must be provided with benchmark_returns_m_xts."
  )

})

#UPDATE
test_that("update_port_backtest works for a simple sw single signal strategy with a selected benchmark and
          user_defined_OR_rules, with new month a rebalancing month", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes at 2023-03-15
  #Suppose a esg focused portfolio
  set.seed(123)
  user_defined_OR_rules_m_df_total <- signals_m_df %>% dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(esg_score = sample(c("esg", "non-esg"), dplyr::n(), replace = TRUE)) %>%
    dplyr::mutate(esg = sample(c(1,0), dplyr::n(), replace = TRUE)) %>% create_meta_dataframe()

  signals_m_df <- create_meta_dataframe(signals_m_df %>% dplyr::filter(!dates == "2023-04-15"), type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df %>% dplyr::filter(!dates == "2023-04-15") %>%
                                             dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-03-15", NA_real_, fwd_return_1m))
                                           , type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "liq")
  volatility_m_df <- create_meta_dataframe(volatility_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "vol")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts["2022-10-15/2023-03-15"], asset_type = "benchmark", meta_xts_name = "bench_returns")
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "bench_weights")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data, "stock_metrics")
  user_defined_OR_rules_m_df <- create_meta_dataframe(user_defined_OR_rules_m_df_total@data %>% dplyr::filter(!dates == "2023-04-15"))



  #Run port_backtest
  expect_warning(
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 user_defined_OR_rules_m_df = user_defined_OR_rules_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  ),
  "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint."
  )

  #Check some not yet tested points
  #All OR are eligible
  expect_equal(
  results@stock_universe_m_df@data %>%
    dplyr::filter(id %in% (user_defined_OR_rules_m_df@data %>% dplyr::filter(esg == 1) %>% dplyr::pull(id))) %>%
    dplyr::pull(is_eligible) %>% unique(),
  1)

  #Even those with low roe
  expect_equal(
    results@stock_universe_m_df@data %>%
      dplyr::filter(id %in% (user_defined_OR_rules_m_df@data %>% dplyr::filter(esg == 1) %>% dplyr::pull(id)),
                    pre_eligible_assets == 0
                    ) %>%
      dplyr::pull(is_eligible) %>% unique(),
    1)

  #A new batch of data arrives
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))
  #meta_dataframes at 2023-04-15
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df, meta_dataframe_name = "liq")
  volatility_m_df <- create_meta_dataframe(volatility_m_df, meta_dataframe_name = "vol")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark", meta_xts_name = "bench_returns")
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, meta_dataframe_name = "bench_weights")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data, "stock_metrics")
  user_defined_OR_rules_m_df <- create_meta_dataframe(user_defined_OR_rules_m_df_total@data)

  #Run port_backtest
  expect_warning(
  expect_warning(
    expect_warning(
    new_results <- run_port_backtest(signals_m_df = signals_m_df,
                                     fwd_return_m_df = fwd_return_m_df,
                                     liquidity_m_df = liquidity_m_df,
                                     volatility_m_df = volatility_m_df,
                                     config = port_config,
                                     benchmark_weights_m_df = benchmark_weights_m_df,
                                     user_defined_OR_rules_m_df = user_defined_OR_rules_m_df,
                                     benchmark_returns_m_xts = benchmark_returns_m_xts,
                                     custom_stock_metrics_m_df = port_metrics_m_df,
                                     verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."),
    "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint."
    ),
  "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint."
  )


  #Update results
  suppressWarnings(
  updated_results <- update_port_backtest(signals_m_df = signals_m_df,
                                          fwd_return_m_df = fwd_return_m_df,
                                          liquidity_m_df = liquidity_m_df,
                                          volatility_m_df = volatility_m_df,
                                          old_results = results,
                                          benchmark_weights_m_df = benchmark_weights_m_df,
                                          benchmark_returns_m_xts = benchmark_returns_m_xts,
                                          custom_stock_metrics_m_df = port_metrics_m_df,
                                          user_defined_OR_rules_m_df = user_defined_OR_rules_m_df
                                          )
  )

  #Check that updated objects match new results
  expect_equal(new_results@port_weights_m_df@data, updated_results@port_weights_m_df@data)
  expect_equal(new_results@port_costs_m_xts@data, updated_results@port_costs_m_xts@data)
  expect_equal(new_results@port_returns_m_xts@data, updated_results@port_returns_m_xts@data)
  expect_equal(new_results@port_metrics_m_xts@data, updated_results@port_metrics_m_xts@data)
  expect_equal(new_results@transactions_log@data, updated_results@transactions_log@data)
  expect_equal(new_results@stock_universe_m_df@data, updated_results@stock_universe_m_df@data)
  expect_equal(new_results@final_stock_port, updated_results@final_stock_port)
  expect_equal(updated_results@port_backtest_config@initial_buffer_period, length(unique(signals_m_df@data$dates)) - 1)

})

test_that("update_port_backtest works for a simple cs single signal strategy with a selected benchmark,
          with new month a post-rebalancing month AND 2 UPDATES", {

            #Create signals_m_d_ref
            load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

            #Create port_backtest_config
            chosen_score_metric_and_position <- c(roe_3m = "long")
            port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                                       eligibility_quantile_range = c(0.67, 1.0),
                                                       selected_benchmark = "ibov",
                                                       initial_buffer_period = 4,
                                                       rebalancing_months = 2,
                                                       port_construction_method = "cs",
                                                       main_liquidity_metric = "mean_volfin_3m",
                                                       config_name = "guara_model"
            ) %>%
              add_liquidity_floor_cutoffs(
                metric_name = c("mean_volfin_3m", "presence"),
                metric_cutoffs = list(
                  c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
                  c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
                )
              ) %>%
              add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
              add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

            #meta_dataframes at 2023-02-15
            signals_m_df <- create_meta_dataframe(signals_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), type = "signals", meta_dataframe_name = "signals")
            fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")) %>%
                                                       dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-02-15", NA_real_, fwd_return_1m))
                                                     , type = "target", meta_dataframe_name = "fwd")
            liquidity_m_df <- create_meta_dataframe(liquidity_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), meta_dataframe_name = "liq")
            volatility_m_df <- create_meta_dataframe(volatility_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), meta_dataframe_name = "vol")
            benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts["2022-10-15/2023-02-15"], asset_type = "benchmark", meta_xts_name = "bench_returns")
            benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), meta_dataframe_name = "bench_weights")
            port_metrics_m_df <- create_meta_dataframe(signals_m_df@data, "stock_metrics")


            #Run port_backtest
            expect_warning(
              results <- run_port_backtest(signals_m_df = signals_m_df,
                                           fwd_return_m_df = fwd_return_m_df,
                                           liquidity_m_df = liquidity_m_df,
                                           volatility_m_df = volatility_m_df,
                                           benchmark_weights_m_df = benchmark_weights_m_df,
                                           config = port_config,
                                           benchmark_returns_m_xts = benchmark_returns_m_xts,
                                           custom_stock_metrics_m_df = port_metrics_m_df,
                                           verbose = TRUE),
              "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
            )


            #A new batch of data arrives
            load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))
            #meta_dataframes at 2023-04-15
            signals_m_df <- create_meta_dataframe(signals_m_df %>% dplyr::filter(!dates %in% c("2023-04-15")), type = "signals", meta_dataframe_name = "signals")
            fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df %>% dplyr::filter(!dates %in% c("2023-04-15")) %>%
                                                       dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-03-15", NA_real_, fwd_return_1m))
                                                     , type = "target", meta_dataframe_name = "fwd")
            liquidity_m_df <- create_meta_dataframe(liquidity_m_df %>% dplyr::filter(!dates %in% c("2023-04-15")), meta_dataframe_name = "liq")
            volatility_m_df <- create_meta_dataframe(volatility_m_df %>% dplyr::filter(!dates %in% c("2023-04-15")), meta_dataframe_name = "vol")
            benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts["2022-10-15/2023-03-15"], asset_type = "benchmark", meta_xts_name = "bench_returns")
            benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df %>% dplyr::filter(!dates %in% c("2023-04-15")), meta_dataframe_name = "bench_weights")
            port_metrics_m_df <- create_meta_dataframe(signals_m_df@data, "stock_metrics")

            #Run port_backtest
            expect_warning(
              new_results <- run_port_backtest(signals_m_df = signals_m_df,
                                               fwd_return_m_df = fwd_return_m_df,
                                               liquidity_m_df = liquidity_m_df,
                                               volatility_m_df = volatility_m_df,
                                               benchmark_weights_m_df = benchmark_weights_m_df,
                                               config = port_config,
                                               benchmark_returns_m_xts = benchmark_returns_m_xts,
                                               custom_stock_metrics_m_df = port_metrics_m_df,
                                               verbose = TRUE),
              "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
            )

            #Update results
            expect_warning(
              updated_results <- update_port_backtest(signals_m_df = signals_m_df,
                                                      fwd_return_m_df = fwd_return_m_df,
                                                      liquidity_m_df = liquidity_m_df,
                                                      volatility_m_df = volatility_m_df,
                                                      benchmark_weights_m_df = benchmark_weights_m_df,
                                                      old_results = results,
                                                      benchmark_returns_m_xts = benchmark_returns_m_xts,
                                                      custom_stock_metrics_m_df = port_metrics_m_df
              ), "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
            )

            #Check that updated objects match new results
            expect_equal(new_results@port_weights_m_df@data, updated_results@port_weights_m_df@data)
            expect_equal(new_results@port_costs_m_xts@data, updated_results@port_costs_m_xts@data)
            expect_equal(new_results@port_returns_m_xts@data, updated_results@port_returns_m_xts@data)
            expect_equal(new_results@port_metrics_m_xts@data, updated_results@port_metrics_m_xts@data)
            expect_equal(new_results@transactions_log@data, updated_results@transactions_log@data)
            expect_equal(new_results@stock_universe_m_df@data, updated_results@stock_universe_m_df@data)
            expect_equal(new_results@final_stock_port, updated_results@final_stock_port)
            expect_equal(updated_results@port_backtest_config@initial_buffer_period, length(unique(signals_m_df@data$dates)) - 1)


            #New batch again
            load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))
            #meta_dataframes at 2023-04-15
            signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals", meta_dataframe_name = "signals")
            fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target", meta_dataframe_name = "fwd")
            liquidity_m_df <- create_meta_dataframe(liquidity_m_df, meta_dataframe_name = "liq")
            volatility_m_df <- create_meta_dataframe(volatility_m_df, meta_dataframe_name = "vol")
            benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark", meta_xts_name = "bench_returns")
            benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, meta_dataframe_name = "bench_weights")
            port_metrics_m_df <- create_meta_dataframe(signals_m_df@data, "stock_metrics")

            #Run port_backtest
            expect_warning(
              new_results2 <- run_port_backtest(signals_m_df = signals_m_df,
                                               fwd_return_m_df = fwd_return_m_df,
                                               liquidity_m_df = liquidity_m_df,
                                               volatility_m_df = volatility_m_df,
                                               benchmark_weights_m_df = benchmark_weights_m_df,
                                               config = port_config,
                                               benchmark_returns_m_xts = benchmark_returns_m_xts,
                                               custom_stock_metrics_m_df = port_metrics_m_df,
                                               verbose = TRUE),
              "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
            )

            #Update results 2
            expect_warning(
              updated_results2 <- update_port_backtest(signals_m_df = signals_m_df,
                                                      fwd_return_m_df = fwd_return_m_df,
                                                      liquidity_m_df = liquidity_m_df,
                                                      volatility_m_df = volatility_m_df,
                                                      benchmark_weights_m_df = benchmark_weights_m_df,
                                                      old_results = updated_results,
                                                      benchmark_returns_m_xts = benchmark_returns_m_xts,
                                                      custom_stock_metrics_m_df = port_metrics_m_df
              ), "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
            )


            #Check that updated objects match new results
            expect_equal(new_results2@port_weights_m_df@data, updated_results2@port_weights_m_df@data)
            expect_equal(new_results2@port_costs_m_xts@data, updated_results2@port_costs_m_xts@data)
            expect_equal(new_results2@port_returns_m_xts@data, updated_results2@port_returns_m_xts@data)
            expect_equal(new_results2@port_metrics_m_xts@data, updated_results2@port_metrics_m_xts@data)
            expect_equal(new_results2@transactions_log@data, updated_results2@transactions_log@data)
            expect_equal(new_results2@stock_universe_m_df@data, updated_results2@stock_universe_m_df@data)
            expect_equal(new_results2@final_stock_port, updated_results2@final_stock_port)
            expect_equal(updated_results2@port_backtest_config@initial_buffer_period, length(unique(signals_m_df@data$dates)) - 1)

})

test_that("update_port_backtest_works for a meta_sb_backtest AND 2 UPDATES", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #meta_dataframes at 2023-04-15
  target_m_df <- toy_preprocessed_targets
  original_fwd_return_m_df <- target_m_df %>% dplyr::select(id, tickers, dates, fwd_return_1m) %>%
    dplyr::mutate(fwd_return_1m = fwd_return_1m + rnorm(nrow(.), 10, 1))

  target_m_df <- create_meta_dataframe(
    target_m_df %>% dplyr::filter(dates <= as.Date("2023-04-15")) %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-04-15", NA_real_, fwd_return_1m),
                    fwd_premium_1m = dplyr::if_else(dates == "2023-04-15", NA_real_, fwd_premium_1m),
                    fwd_return_3m = dplyr::if_else(dates >= as.Date("2023-02-15"), NA_real_, fwd_return_3m),
                    fwd_premium_3m = dplyr::if_else(dates >= as.Date("2023-02-15"), NA_real_, fwd_premium_3m)
      ),
    meta_dataframe_name = "target", type = "target"
  )

  fwd_return_m_df <- create_meta_dataframe(
    original_fwd_return_m_df %>% dplyr::filter(dates <= as.Date("2023-04-15")) %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-04-15", NA_real_, fwd_return_1m)),
    meta_dataframe_name = "fwd_return", type = "target"
  )

  original_liquidity_m_df <- create_meta_dataframe(
    toy_preprocessed_features %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(mean_volfin_3m = rnorm(nrow(.), 200000, 50000)),
    meta_dataframe_name = "liq"
  )
  liquidity_m_df <- original_liquidity_m_df
  liquidity_m_df@data <- original_liquidity_m_df@data %>%
    dplyr::filter(!dates > as.Date("2023-04-15"))
  liquidity_m_df@current_date <- as.Date("2023-04-15")

  original_volatility_m_df <- create_meta_dataframe(
    toy_preprocessed_features %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(daily_vol = rlnorm(nrow(.), 1, 1)),
    meta_dataframe_name = "vol"
  )
  volatility_m_df <- original_volatility_m_df
  volatility_m_df@data <- original_volatility_m_df@data %>%
    dplyr::filter(!dates > as.Date("2023-04-15"))
  volatility_m_df@current_date <- as.Date("2023-04-15")

  set.seed(123)
  original_benchmark_returns_m_xts <- create_meta_xts(
    xts::xts(data.frame(ibov = rnorm(length(unique(toy_preprocessed_features$dates)), -10, 1)),
             order.by = sort(unique(toy_preprocessed_features$dates))), type = "returns", asset_type = "benchmarks"
  )
  benchmark_returns_m_xts <- create_meta_xts(
    original_benchmark_returns_m_xts@data["2022-07-15/2023-04-15"], type = "returns", asset_type = "benchmarks"
  )

  features_m_df <- create_meta_dataframe(
    toy_preprocessed_features %>% dplyr::filter(dates <= "2023-04-15"), type = "features", meta_dataframe_name = "signals")

  scaler_m_df <- create_meta_dataframe(
    features_m_df@data %>%
      dplyr::select(id, tickers, dates, book_yield)
  )
  chosen_scaler = "book_yield"
  scaler_shrinkage = 0.5

  benchmark_weights_m_df <- create_meta_dataframe(
    features_m_df@data %>% dplyr::select(id, tickers, dates) %>% dplyr::group_by(dates) %>%
      dplyr::mutate(ibov = 1/dplyr::n()) %>%
      dplyr::ungroup(),
    meta_dataframe_name = "ibov_weight"
  )

  target_port_m_df <- create_meta_dataframe(
    benchmark_weights_m_df@data %>% dplyr::rename(target_weights = ibov)
  )

  port_metrics_m_df <- create_meta_dataframe(features_m_df@data %>%
                                               dplyr::select(id, tickers, dates, roe_3m), meta_dataframe_name = "metrics")


  #Characteristics portfolio
  characteristics_ports <- c(
    book_yield = "long",
    asset_turnover_12m = "long",
    eps_yield = "long",
    mom_res_12m = "long",
    roe_3m = "long",
    sharpe_6m = "long",
    idio_vol_mrkt_ewma = "short",
    sectors_c1Agro = "long"
  )

  #Create config list
  port_backtest_config_list <- purrr::imap(characteristics_ports, function(pos, metric_name) {
    create_port_backtest_config(
      eligibility_quantile_range = c(0.67, 1),
      selected_benchmark = "ibov",
      initial_buffer_period = 1,
      chosen_score_metric_and_position = stats::setNames(pos, metric_name),
      rebalancing_months = 4,
      port_construction_method = "sw",
      main_liquidity_metric = "mean_volfin_3m",
      config_name = metric_name
    ) %>%
      add_liquidity_floor_cutoffs(
        metric_name = c("mean_volfin_3m"),
        metric_cutoffs = list(
          c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000)
        )) %>%
      add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
      add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)
  })

  #Run!
  future::plan("sequential")

  suppressWarnings(
    port_backtest_cohort <- purrr::map(port_backtest_config_list, function(port_config) {
      run_port_backtest(
        signals_m_df = features_m_df,
        fwd_return_m_df = fwd_return_m_df,
        liquidity_m_df = liquidity_m_df,
        benchmark_weights_m_df = benchmark_weights_m_df,
        volatility_m_df = volatility_m_df,
        config = port_config,
        benchmark_returns_m_xts = benchmark_returns_m_xts,
        custom_stock_metrics_m_df = port_metrics_m_df,
        verbose = TRUE
      )
    }) %>% create_port_backtest_cohort(cohort_name = "sw_signals")
  )


  #SS Configuration
  chosen_signals_and_positions <- c(book_yield = "long", eps_yield = "long", roe_3m = "long", sharpe_6m = "long", idio_vol_mrkt_ewma = "short")

  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 3, rebalancing_months = 11,
                                                     split_method = "expanding", config_name = "frequentist_ss", active_returns = TRUE,
                                                     chosen_signals_and_positions = chosen_signals_and_positions
  ) %>%
    add_alpha_test_strategy(model_structure = "no_pooled",
                            signal_significance_threshold = 0.50, p_correction_method = "none",
                            market_factor_proxy = "ibov", enable_theme_representativeness = TRUE)


  signal_themes_m_df <- create_meta_dataframe(
    expand.grid(c("book_yield", "eps_yield", "roe_3m", "sharpe_6m", "low_idio_vol_mrkt_ewma"), unique(features_m_df@data$dates)) %>%
      dplyr::rename(tickers = Var1, dates = Var2) %>%
      dplyr::mutate(tickers = as.character(tickers)) %>%
      dplyr::mutate(theme = dplyr::case_when(tickers %in% c("book_yield", "eps_yield") ~ "value",
                                             tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "quality",
                                             tickers %in% c("sharpe_6m") ~ "momentum")) %>%
      dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers) %>%
      dplyr::arrange(id),
    type = "groups", meta_dataframe_name = "themes")


  #This is for NA warning of NAs at the end of run_ss_backtest
  suppressWarnings(
    ss_results <-
      run_ss_backtest(frequentist_ss_config,
                      signals_m_df = features_m_df, port_backtest_cohort = port_backtest_cohort, benchmark_returns_m_xts = benchmark_returns_m_xts,
                      signal_themes_m_df = signal_themes_m_df,
                      verbose = TRUE)
  )
  #SB Backtest
  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", target_fwd_name = "fwd_premium_1m",
                                         training_sample_size = 4, rebalancing_months = 6, config_name = "rf") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"), grid = list(c(0.1, 0.9), c(100, 500), 3, 5))


  expect_warning(
    rf_results <- run_sb_backtest(config = rf_config, features_m_df = features_m_df, target_m_df = target_m_df,
                                  ss_backtest_results = ss_results, .test_seed = 123),
    "Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest."
  )

  mvo_config <- create_sb_backtest_config(sb_algorithm = "mvo", target_fwd_name = "fwd_premium_1m",
                                          training_sample_size = 6, rebalancing_months = 6, config_name = "mvo",
                                          custom_objective = "max_info_ratio") %>%
    add_cov_est_method(cov_estimation_method = "shrink_id", cov_matrix_sample_size = 3, cov_matrix_benchmark = "ibov", active_retuerns = TRUE) %>%
    add_concentration_constraint_policy(benchmark = "theme_sb", max_abs_active_group_weight = c(theme = 0.2))

  expect_warning(
    mvo_results <- run_sb_backtest(config = mvo_config, features_m_df = features_m_df, target_m_df = target_m_df,
                                   port_backtest_cohort = port_backtest_cohort, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                   signal_themes_m_df = signal_themes_m_df,
                                   ss_backtest_results = ss_results, .test_seed = 123),
    "Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest."
  )


  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "sw", training_sample_size = 2, target_fwd_name = "fwd_premium_1m",
                                                   rebalancing_months = 5, config_name = "meta", custom_objective = "min_rmse")

  meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = "none",
                                  config_name = "meta_rf_ols")


  set.seed(123)
    sb_metabacktest_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      base_sb_backtest_results_list = list(rf_results, mvo_results),
      config = meta_config,
      parallel = FALSE,
      verbose = TRUE)

  #Build portfolio for meta sb
  meta_port_config <- create_port_backtest_config(
    eligibility_quantile_range = c(0.67, 1),
    selected_benchmark = "ibov",
    initial_buffer_period = 7,
    rebalancing_months = 4,
    port_construction_method = "sw",
    main_liquidity_metric = "mean_volfin_3m",
    config_name = "meta_portfolio",
    chosen_scaler = chosen_scaler,
    scaler_shrinkage = scaler_shrinkage,
    use_raw_for_eligibility = TRUE
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000)
      )) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)


  #Build portfolio for metaport
  suppressWarnings(
  sb_metaport_results <- run_port_backtest(
    signals_m_df = features_m_df,
    fwd_return_m_df = fwd_return_m_df,
    liquidity_m_df = liquidity_m_df,
    sb_backtest_results = sb_metabacktest_results,
    benchmark_weights_m_df = benchmark_weights_m_df,
    volatility_m_df = volatility_m_df,
    config = meta_port_config,
    benchmark_returns_m_xts = benchmark_returns_m_xts,
    .test_seed = 123,
    target_port_m_df = target_port_m_df,
    scaler_m_df = scaler_m_df,
    verbose = TRUE
  )
  )

  ################
  ####Update 1####
  ################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #meta_dataframes at 2023-05-15
  target_m_df <- toy_preprocessed_targets
  target_m_df <- create_meta_dataframe(
    target_m_df %>% dplyr::filter(dates <= as.Date("2023-05-15")) %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-05-15", NA_real_, fwd_return_1m),
                    fwd_premium_1m = dplyr::if_else(dates == "2023-05-15", NA_real_, fwd_premium_1m),
                    fwd_return_3m = dplyr::if_else(dates >= as.Date("2023-03-15"), NA_real_, fwd_return_3m),
                    fwd_premium_3m = dplyr::if_else(dates >= as.Date("2023-03-15"), NA_real_, fwd_premium_3m)
      ),
    meta_dataframe_name = "target", type = "target"
  )

  fwd_return_m_df <- create_meta_dataframe(
    original_fwd_return_m_df %>% dplyr::filter(dates <= as.Date("2023-05-15")) %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-05-15", NA_real_, fwd_return_1m)),
    meta_dataframe_name = "fwd_return", type = "target"
  )
  fwd_return_m_df@current_date <- as.Date("2023-05-15")

  liquidity_m_df <- original_liquidity_m_df
  liquidity_m_df@data <- original_liquidity_m_df@data %>%
    dplyr::filter(!dates > as.Date("2023-05-15"))
  liquidity_m_df@current_date <- as.Date("2023-05-15")


  volatility_m_df <- original_volatility_m_df
  volatility_m_df@data <- original_volatility_m_df@data %>%
    dplyr::filter(!dates > as.Date("2023-05-15"))
  volatility_m_df@current_date <- as.Date("2023-05-15")

  features_m_df <- create_meta_dataframe(
    toy_preprocessed_features %>% dplyr::filter(dates <= "2023-05-15"), type = "features", meta_dataframe_name = "signals")
  scaler_m_df <- create_meta_dataframe(
    features_m_df@data %>%
      dplyr::select(id, tickers, dates, book_yield)
  )

  benchmark_weights_m_df <- create_meta_dataframe(
    features_m_df@data %>% dplyr::select(id, tickers, dates) %>% dplyr::group_by(dates) %>%
      dplyr::mutate(ibov = 1/dplyr::n()) %>%
      dplyr::ungroup(),
    meta_dataframe_name = "ibov_weight"
  )
  benchmark_returns_m_xts <- create_meta_xts(
    original_benchmark_returns_m_xts@data["2022-07-15/2023-05-15"], type = "returns", asset_type = "benchmarks"
  )

  port_metrics_m_df <- create_meta_dataframe(features_m_df@data %>%
                                               dplyr::select(id, tickers, dates, roe_3m), meta_dataframe_name = "metrics")

  #Run!
  future::plan("sequential")

  suppressWarnings(
    updated_port_backtest_cohort <- purrr::map(port_backtest_cohort@port_backtest_results_list, function(port_results) {
      update_port_backtest(
        signals_m_df = features_m_df,
        fwd_return_m_df = fwd_return_m_df,
        liquidity_m_df = liquidity_m_df,
        benchmark_weights_m_df = benchmark_weights_m_df,
        volatility_m_df = volatility_m_df,
        old_results = port_results,
        benchmark_returns_m_xts = benchmark_returns_m_xts,
        custom_stock_metrics_m_df = port_metrics_m_df,
        verbose = TRUE
      )
    }) %>% create_port_backtest_cohort(cohort_name = "sw_signals")
  )

  signal_themes_m_df <- create_meta_dataframe(
    expand.grid(c("book_yield", "eps_yield", "roe_3m", "sharpe_6m", "low_idio_vol_mrkt_ewma"), unique(features_m_df@data$dates)) %>%
      dplyr::rename(tickers = Var1, dates = Var2) %>%
      dplyr::mutate(tickers = as.character(tickers)) %>%
      dplyr::mutate(theme = dplyr::case_when(tickers %in% c("book_yield", "eps_yield") ~ "value",
                                             tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "quality",
                                             tickers %in% c("sharpe_6m") ~ "momentum")) %>%
      dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers) %>%
      dplyr::arrange(id),
    type = "groups", meta_dataframe_name = "themes")


  #This is for NA warning of NAs at the end of run_ss_backtest
  suppressWarnings(
    updated_ss_results <-
      update_ss_backtest(updated_port_backtest_cohort = updated_port_backtest_cohort,
                         signals_m_df = features_m_df,
                         benchmark_returns_m_xts = benchmark_returns_m_xts,
                         signal_themes_m_df = signal_themes_m_df,
                         old_results = ss_results,
                         verbose = TRUE)
  )

  #Update sb backtest
  suppressWarnings(
    updated_rf_results <-
      update_sb_backtest(features_m_df = features_m_df,
                         target_m_df = target_m_df,
                         old_results = rf_results,
                         updated_ss_backtest_results = updated_ss_results,
                         .test_seed = 123
      )
    )

  suppressWarnings(
    updated_mvo_results <-
      update_sb_backtest(features_m_df = features_m_df,
                         target_m_df = target_m_df,
                         old_results = mvo_results,
                         updated_port_backtest_cohort = updated_port_backtest_cohort,
                         benchmark_returns_m_xts = benchmark_returns_m_xts,
                         signal_themes_m_df = signal_themes_m_df,
                         updated_ss_backtest_results = updated_ss_results,
                         .test_seed = 123
      )
    )

  #Update meta
  set.seed(123)
  suppressWarnings(
    updated_sb_metabacktest_results <- update_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      updated_base_sb_backtest_results = list(updated_rf_results, updated_mvo_results),
      old_results = sb_metabacktest_results,
      parallel = FALSE,
      verbose = TRUE)
  )

  #Update meta port
  expect_warning(
  updated_sb_meta_port_results <-
    update_port_backtest(
      signals_m_df = features_m_df,
      fwd_return_m_df = fwd_return_m_df,
      liquidity_m_df = liquidity_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      volatility_m_df = volatility_m_df,
      old_results = sb_metaport_results,
      scaler_m_df = scaler_m_df,
      updated_sb_backtest_results = updated_sb_metabacktest_results,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      verbose = TRUE
    ),
  "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  ################
  ####Update 2####
  ################

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #meta_dataframes at 2023-06-15
  target_m_df <- toy_preprocessed_targets
  target_m_df <- create_meta_dataframe(
    target_m_df %>% dplyr::filter(dates <= as.Date("2023-06-15")) %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-06-15", NA_real_, fwd_return_1m),
                    fwd_premium_1m = dplyr::if_else(dates == "2023-06-15", NA_real_, fwd_premium_1m),
                    fwd_return_3m = dplyr::if_else(dates >= as.Date("2023-04-15"), NA_real_, fwd_return_3m),
                    fwd_premium_3m = dplyr::if_else(dates >= as.Date("2023-04-15"), NA_real_, fwd_premium_3m)
      ),
    meta_dataframe_name = "target", type = "target"
  )
  fwd_return_m_df <- create_meta_dataframe(
    original_fwd_return_m_df %>% dplyr::filter(dates <= as.Date("2023-06-15")) %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-06-15", NA_real_, fwd_return_1m)),
    meta_dataframe_name = "fwd_return", type = "target"
  )

  liquidity_m_df <- original_liquidity_m_df
  liquidity_m_df@data <- original_liquidity_m_df@data %>%
    dplyr::filter(!dates > as.Date("2023-06-15"))
  liquidity_m_df@current_date <- as.Date("2023-06-15")


  volatility_m_df <- original_volatility_m_df
  volatility_m_df@data <- original_volatility_m_df@data %>%
    dplyr::filter(!dates > as.Date("2023-06-15"))
  volatility_m_df@current_date <- as.Date("2023-06-15")

  features_m_df <- create_meta_dataframe(
    toy_preprocessed_features %>% dplyr::filter(dates <= "2023-06-15"), type = "features", meta_dataframe_name = "signals")

  scaler_m_df <- create_meta_dataframe(
    features_m_df@data %>%
      dplyr::select(id, tickers, dates, book_yield)
  )

  benchmark_weights_m_df <- create_meta_dataframe(
    features_m_df@data %>% dplyr::select(id, tickers, dates) %>% dplyr::group_by(dates) %>%
      dplyr::mutate(ibov = 1/dplyr::n()) %>%
      dplyr::ungroup(),
    meta_dataframe_name = "ibov_weight"
  )

  benchmark_returns_m_xts <- create_meta_xts(
    original_benchmark_returns_m_xts@data["2022-07-15/2023-06-15"], type = "returns", asset_type = "benchmarks"
  )

  port_metrics_m_df <- create_meta_dataframe(features_m_df@data %>%
                                               dplyr::select(id, tickers, dates, roe_3m), meta_dataframe_name = "metrics")

  #Run!
  future::plan("sequential")

  suppressWarnings(
    updated_port_backtest_cohort_2 <- purrr::map(updated_port_backtest_cohort@port_backtest_results_list, function(port_results) {
      update_port_backtest(
        signals_m_df = features_m_df,
        fwd_return_m_df = fwd_return_m_df,
        liquidity_m_df = liquidity_m_df,
        benchmark_weights_m_df = benchmark_weights_m_df,
        volatility_m_df = volatility_m_df,
        old_results = port_results,
        benchmark_returns_m_xts = benchmark_returns_m_xts,
        custom_stock_metrics_m_df = port_metrics_m_df,
        verbose = FALSE
      )
    }) %>% create_port_backtest_cohort(cohort_name = "sw_signals")
  )

  signal_themes_m_df <- create_meta_dataframe(
    expand.grid(c("book_yield", "eps_yield", "roe_3m", "sharpe_6m", "low_idio_vol_mrkt_ewma"), unique(features_m_df@data$dates)) %>%
      dplyr::rename(tickers = Var1, dates = Var2) %>%
      dplyr::mutate(tickers = as.character(tickers)) %>%
      dplyr::mutate(theme = dplyr::case_when(tickers %in% c("book_yield", "eps_yield") ~ "value",
                                             tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "quality",
                                             tickers %in% c("sharpe_6m") ~ "momentum")) %>%
      dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers) %>%
      dplyr::arrange(id),
    type = "groups", meta_dataframe_name = "themes")


  #This is for NA warning of NAs at the end of run_ss_backtest
  suppressWarnings(
    updated_ss_results_2 <-
      update_ss_backtest(updated_port_backtest_cohort = updated_port_backtest_cohort_2,
                         signals_m_df = features_m_df,
                         benchmark_returns_m_xts = benchmark_returns_m_xts,
                         signal_themes_m_df = signal_themes_m_df,
                         old_results = updated_ss_results,
                         verbose = TRUE)
  )

  #Update sb backtest
  suppressWarnings(
    updated_rf_results_2 <-
      update_sb_backtest(features_m_df = features_m_df,
                         target_m_df = target_m_df,
                         old_results = updated_rf_results,
                         updated_ss_backtest_results = updated_ss_results_2,
                         .test_seed = 123
      )
    )

  suppressWarnings(
    updated_mvo_results_2 <-
      update_sb_backtest(features_m_df = features_m_df,
                         target_m_df = target_m_df,
                         old_results = updated_mvo_results,
                         benchmark_returns_m_xts = benchmark_returns_m_xts,
                         signal_themes_m_df = signal_themes_m_df,
                         updated_port_backtest_cohort = updated_port_backtest_cohort_2,
                         updated_ss_backtest_results = updated_ss_results_2,
                         .test_seed = 123
      )
    )

  #Update meta
  set.seed(123)
  suppressWarnings(
    updated_sb_metabacktest_results_2 <- update_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      updated_base_sb_backtest_results = list(updated_rf_results_2, updated_mvo_results_2),
      old_results = updated_sb_metabacktest_results,
      parallel = FALSE,
      verbose = TRUE)
  )

  #Update meta port
  expect_warning(
    updated_sb_meta_port_results_2 <-
      update_port_backtest(
        signals_m_df = features_m_df,
        fwd_return_m_df = fwd_return_m_df,
        liquidity_m_df = liquidity_m_df,
        benchmark_weights_m_df = benchmark_weights_m_df,
        volatility_m_df = volatility_m_df,
        old_results = updated_sb_meta_port_results,
        scaler_m_df = scaler_m_df,
        updated_sb_backtest_results = updated_sb_metabacktest_results_2,
        benchmark_returns_m_xts = benchmark_returns_m_xts,
        verbose = TRUE
      ),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  )

  #Expected Results
  #################
  suppressWarnings(
    new_port_backtest_cohort <- purrr::map(port_backtest_config_list, function(port_config) {
      run_port_backtest(
        signals_m_df = features_m_df,
        fwd_return_m_df = fwd_return_m_df,
        liquidity_m_df = liquidity_m_df,
        benchmark_weights_m_df = benchmark_weights_m_df,
        volatility_m_df = volatility_m_df,
        config = port_config,
        benchmark_returns_m_xts = benchmark_returns_m_xts,
        custom_stock_metrics_m_df = port_metrics_m_df,
        verbose = FALSE
      )
    }) %>% create_port_backtest_cohort(cohort_name = "sw_signals")
  )


  #SS Configuration
  suppressWarnings(
    new_ss_results <-
      run_ss_backtest(frequentist_ss_config,
                      signals_m_df = features_m_df, port_backtest_cohort = new_port_backtest_cohort,
                      benchmark_returns_m_xts = benchmark_returns_m_xts,
                      signal_themes_m_df = signal_themes_m_df,
                      verbose = TRUE)
  )

  #SB Backtest
  suppressWarnings(
    new_rf_results <- run_sb_backtest(config = rf_config, features_m_df = features_m_df, target_m_df = target_m_df,
                                      ss_backtest_results = new_ss_results, .test_seed = 123)
  )

  suppressWarnings(
    new_mvo_results <- run_sb_backtest(config = mvo_config, features_m_df = features_m_df, target_m_df = target_m_df,
                                       port_backtest_cohort = new_port_backtest_cohort, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                       signal_themes_m_df = signal_themes_m_df,
                                       ss_backtest_results = new_ss_results, .test_seed = 123)
  )

  set.seed(123)
  suppressWarnings(
    new_sb_metabacktest_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      base_sb_backtest_results_list = list(new_rf_results, new_mvo_results),
      config = meta_config,
      parallel = FALSE,
      verbose = FALSE)
  )


  #Build portfolio for metaport
  suppressWarnings(
    new_sb_metaport_results <- run_port_backtest(
      signals_m_df = features_m_df,
      fwd_return_m_df = fwd_return_m_df,
      liquidity_m_df = liquidity_m_df,
      sb_backtest_results = new_sb_metabacktest_results,
      benchmark_weights_m_df = benchmark_weights_m_df,
      volatility_m_df = volatility_m_df,
      scaler_m_df = scaler_m_df,
      config = meta_port_config,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      verbose = TRUE
    )
  )


  expect_equal(updated_sb_meta_port_results_2@port_weights_m_df@data, new_sb_metaport_results@port_weights_m_df@data)
  expect_equal(updated_sb_meta_port_results_2@transactions_log, new_sb_metaport_results@transactions_log)
  expect_equal(updated_sb_meta_port_results_2@port_costs_m_xts@data, new_sb_metaport_results@port_costs_m_xts@data)
  expect_equal(updated_sb_meta_port_results_2@port_returns_m_xts@data, new_sb_metaport_results@port_returns_m_xts@data)
  expect_equal(updated_sb_meta_port_results_2@stock_universe_m_df@data, new_sb_metaport_results@stock_universe_m_df@data)


})



test_that("update_port_backtest throws errors for uncompatible objects", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 3,
                                             rebalancing_months = 4,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes at 2023-03-15
  #Suppose a esg focused portfolio
  set.seed(123)
  user_defined_OR_rules_m_df_total <- signals_m_df %>% dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(esg_score = sample(c("esg", "non-esg"), dplyr::n(), replace = TRUE)) %>%
    dplyr::mutate(esg = sample(c(1,0), dplyr::n(), replace = TRUE)) %>% create_meta_dataframe()

  signals_m_df <- create_meta_dataframe(signals_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")) %>%
                                             dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-02-15", NA_real_, fwd_return_1m))
                                           , type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), meta_dataframe_name = "liq")
  volatility_m_df <- create_meta_dataframe(volatility_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), meta_dataframe_name = "vol")
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), meta_dataframe_name = "bench_weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts["2022-10-15/2023-02-15"], asset_type = "benchmark", meta_xts_name = "bench_returns")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data, "stock_metrics")
  user_defined_OR_rules_m_df <- create_meta_dataframe(user_defined_OR_rules_m_df_total@data %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")))


  #Run port_backtest
  expect_warning(
  expect_warning(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 config = port_config,
                                 user_defined_OR_rules_m_df = user_defined_OR_rules_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE),
    "Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest."
  ), "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint.")

  #Update port_backtest
  #A new batch of data arrives
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))
  #meta_dataframes at 2023-04-15
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df, meta_dataframe_name = "liq")
  volatility_m_df <- create_meta_dataframe(volatility_m_df, meta_dataframe_name = "vol")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark", meta_xts_name = "bench_returns")
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, meta_dataframe_name = "bench_weights")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data, "stock_metrics")
  user_defined_OR_rules_m_df <- create_meta_dataframe(user_defined_OR_rules_m_df_total@data)


  expect_error(
    update_port_backtest(signals_m_df = signals_m_df,
                         fwd_return_m_df = fwd_return_m_df,
                         liquidity_m_df = liquidity_m_df,
                         volatility_m_df = volatility_m_df,
                         benchmark_weights_m_df = benchmark_weights_m_df,
                         old_results = results,
                         benchmark_returns_m_xts = benchmark_returns_m_xts,
                         custom_stock_metrics_m_df = port_metrics_m_df,
                         user_defined_OR_rules_m_df = user_defined_OR_rules_m_df
    ),
    "The current_date in the new signals_m_df is not equal to the current_date in the old_results \\+ 1 month"
  )




})
