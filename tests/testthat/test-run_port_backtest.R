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
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::select(id, tickers, dates, roe_3m))


  #Run port_backtest
  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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
  suppressWarnings(
  results <- run_port_backtest(signals_m_df = signals_m_df,
                               fwd_return_m_df = fwd_return_m_df,
                               liquidity_m_df = liquidity_m_df,
                               volatility_m_df = volatility_m_df,
                               config = port_config,
                               benchmark_weights_m_df = benchmark_weights_m_df,
                               benchmark_returns_m_xts = benchmark_returns_m_xts,
                               custom_stock_metrics_m_df = port_metrics_m_df,
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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
  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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

  #Run port_backtest
  suppressWarnings(
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
    groups_m_d_ref = stock_groups_m_d_ref
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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

  #Check that weights are somewhat lower for high vol
  high_vol_ids <- signals_m_d_ref %>% dplyr::filter(vol_36m >= quantile(vol_36m, .67)) %>% dplyr::pull(id)
  expect_lt(
    results@stock_universe_m_df@data %>% dplyr::filter(is_eligible == 1, id %in% high_vol_ids) %>% dplyr::pull(weights) %>% mean(),
    results@stock_universe_m_df@data %>% dplyr::filter(is_eligible == 1, !id %in% high_vol_ids) %>% dplyr::pull(weights) %>% mean()
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
  suppressWarnings(
  sb_results <- run_sb_backtest(
    features_m_df = signals_m_df,
    target_m_df = target_m_df,
    config = glmnet_config,
    parallel = TRUE
  )
  )

  #Create port_backtest_config
  port_config <- create_port_backtest_config(eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             sb_backtest_results = sb_results,
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>% add_sb_backtest_results(sb_results) %>%
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
  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 config = port_config,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy)
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


  #Create sb_backtest_config
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", rebalancing_months = 4,
                                             training_sample_size = 3, target_fwd_name = "fwd_return_1m") %>%
    add_tuning_strategy(tuning_method = "grid_search", chosen_eval_metric = "rmse", validation_sample_size = 2) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"),
                       grid = list(c(0, 0.5, 1), seq(0.1, 0.9, length=10)))

  #run_sb_backtest
  suppressWarnings(
    sb_results <- run_sb_backtest(
      features_m_df = signals_m_df,
      target_m_df = target_m_df,
      config = glmnet_config,
      parallel = TRUE
    )
  )

  #Create port_backtest_config
  port_config <- create_port_backtest_config(eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             sb_backtest_results = sb_results,
                                             initial_buffer_period = 5,
                                             rebalancing_months = 4,
                                             port_construction_method = "mvo",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
  ) %>% add_sb_backtest_results(sb_results) %>%
    add_liquidity_floor_cutoffs(
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
  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 config = port_config,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_benchmark_returns_m_xts_mocked,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 verbose = TRUE)
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
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy)
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
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy)
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

  #run_sb_backtest
  suppressWarnings(
    sb_results <- run_sb_backtest(
      features_m_df = signals_m_df,
      target_m_df = target_m_df,
      config = glmnet_config,
      parallel = TRUE
    )
  )

  #Create port_backtest_config
  port_config <- create_port_backtest_config(eligibility_quantile_range = c(0.67, 1.0),
                                             sb_backtest_results = sb_results,
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
  set.seed(123)
  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 config = port_config,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 stock_groups_m_df = stock_groups_m_df,
                                 benchmark_weights_m_df = NULL,
                                 benchmark_returns_m_xts = NULL,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = NULL,
                                 custom_stock_metrics_m_df = port_metrics_m_df,
                                 user_defined_AND_rules_m_df = user_defined_AND_rules_m_df,
                                 verbose = TRUE)
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
  suppressWarnings(
  port_allocation_1 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_1@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = NULL
  )
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
  suppressWarnings(
  port_allocation_3 <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    stock_universe_m_d_ref = mvo_port_2@universe_m_d_ref@data,
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = "mean_volfin_3m",
    transaction_cost_parameters <- as.list(port_config@transaction_costs_parameters),
    selected_benchmark_weights_m_d_ref = NULL
  )
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
  suppressWarnings(
    sw_results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = sw_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 verbose = TRUE)
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

  suppressWarnings(
    cs_results <- run_port_backtest(signals_m_df = signals_m_df,
                                    fwd_return_m_df = fwd_return_m_df,
                                    liquidity_m_df = liquidity_m_df,
                                    volatility_m_df = volatility_m_df,
                                    config = cs_config,
                                    benchmark_weights_m_df = benchmark_weights_m_df,
                                    benchmark_returns_m_xts = benchmark_returns_m_xts,
                                    custom_stock_metrics_m_df = skimmed_port_metrics_m_df,
                                    verbose = TRUE)
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
  suppressWarnings(
    cw_results <- run_port_backtest(signals_m_df = signals_m_df,
                                    fwd_return_m_df = fwd_return_m_df,
                                    liquidity_m_df = liquidity_m_df,
                                    volatility_m_df = volatility_m_df,
                                    config = cw_config,
                                    benchmark_weights_m_df = benchmark_weights_m_df,
                                    benchmark_returns_m_xts = benchmark_returns_m_xts,
                                    custom_stock_metrics_m_df = port_metrics_m_df,
                                    verbose = TRUE)
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
  suppressWarnings(
    long_results <- run_port_backtest(signals_m_df = signals_m_df,
                                      fwd_return_m_df = fwd_return_m_df,
                                      liquidity_m_df = liquidity_m_df,
                                      volatility_m_df = volatility_m_df,
                                      config = long_config,
                                      custom_stock_metrics_m_df = port_metrics_m_df,
                                      verbose = TRUE)
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
  suppressWarnings(
    short_results <- run_port_backtest(signals_m_df = signals_m_df,
                                      fwd_return_m_df = fwd_return_m_df,
                                      liquidity_m_df = liquidity_m_df,
                                      volatility_m_df = volatility_m_df,
                                      config = short_config,
                                      custom_stock_metrics_m_df = port_metrics_m_df,
                                      verbose = TRUE)
  )

  #Create cohort
  ls_cohort <- create_port_backtest_cohort(list(long_results, short_results), cohort_name = "vol_ls_cohort")

  #Check that there are 2 backtest results
  expect_equal(length(ls_cohort@port_backtest_results_list), 2)

  #Check that port_weights are according to expectation
  expect_equal(ls_cohort@port_weights_m_df@data$`c:long_vol_36m_s:not_identified_f:not_identified`, long_results@port_weights_m_df@data$eop_port_weights)
  expect_equal(ls_cohort@port_weights_m_df@data$`c:short_vol_36m_s:not_identified_f:not_identified`, short_results@port_weights_m_df@data$eop_port_weights)
  expect_null(ls_cohort@port_weights_m_df@data$bench_weights)

  expect_true(all(long_results@port_weights_m_df@data$id %in% ls_cohort@port_weights_m_df@data$id))

  #Check that port returns are according to expectation
  expect_equal(ls_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c.long_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               long_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c.short_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               short_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())
  expect_null(ls_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$selected_bench_return)

  expect_equal(ls_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c.long_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               long_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c.short_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               short_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())
  expect_null(ls_cohort@port_returns_m_xts_list$net_returns_m_xts@data$selected_bench_return)

  #Only raw and net returns xts
  expect_equal(names(ls_cohort@port_returns_m_xts_list), c("raw_returns_m_xts", "net_returns_m_xts"))


  #Check that port costs are accordng to expectation
  expect_equal(ls_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c.long_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               long_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c.short_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               short_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())

  expect_equal(ls_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c.long_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               long_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c.short_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               short_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())

  expect_equal(ls_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c.long_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               long_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c.short_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               short_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())

  expect_equal(ls_cohort@port_costs_m_xts_list$turnover_m_xts@data$c.long_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               long_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())
  expect_equal(ls_cohort@port_costs_m_xts_list$turnover_m_xts@data$c.short_vol_36m_s.not_identified_f.not_identified %>% as.data.frame() %>% unname(),
               short_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())


  #Port Metrics
  #Check that length is 2 (no sw)
  expect_equal(ls_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data %>% ncol(), 2)
  expect_gt(ls_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data$c.long_vol_36m_s.not_identified_f.not_identified %>% mean(),
            ls_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data$c.short_vol_36m_s.not_identified_f.not_identified %>% mean())

  #Check that all metrics are present
  expect_equal(stringr::str_remove(names(ls_cohort@port_metrics_m_xts_list), "_m_xts"), colnames(port_metrics_m_df@data)[-c(1:3)])

})
