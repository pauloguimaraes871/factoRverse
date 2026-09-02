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
    port_construction_method = "ew",
    selected_benchmark = "ibov"
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
    port_construction_method = "ew",
    selected_benchmark = "ibov"
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


  #Port stats
  rebal_dates <- results@port_backtest_workflow[[length(results@port_backtest_workflow)]]$rebalance_dates
  port_stats_1 <- data.frame(
    id = paste0(c("raw_return-", "net_return-"), rebal_dates[1]),
    tickers = c("raw_return", "net_return"),
    dates = rebal_dates[1],
    ew_port_1@port_stats %>% dplyr::rename(IR = info_ratio)
  )
  port_stats_2 <- summarize_performance(
      selected_backtest_returns_corrected_positions_m_xts_upd_ref =
        results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), c("raw_return", "net_return")],
      selected_market_factor_proxy_m_xts_upd_ref =
        results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), "selected_bench_return"],
      model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = FALSE,
      selected_signal_themes_m_d_ref = NULL, active_returns = TRUE, verbose = FALSE
    )$signal_universe_m_d_ref %>%
        dplyr::left_join(
          data.frame(
            tickers = c("raw_return", "net_return"),
            ew_port_2@port_stats %>% dplyr::rename(IR = info_ratio)
          ), by = "tickers"
      )

  port_stats <- dplyr::bind_rows(port_stats_1, port_stats_2) %>%
    dplyr::arrange(id)
  port_stats <- port_stats[, names(port_stats_2)]

  expect_equal(
    results@port_stats_m_df@data, port_stats
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
  #expect_no_error(print(results))
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
    port_construction_method = "sw",
    selected_benchmark = "ibov"
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
    port_construction_method = "sw",
    selected_benchmark = "ibov"
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

   #Port stats
  rebal_dates <- results@port_backtest_workflow[[length(results@port_backtest_workflow)]]$rebalance_dates
  port_stats_1 <- data.frame(
    id = paste0(c("raw_return-", "net_return-"), rebal_dates[1]),
    tickers = c("raw_return", "net_return"),
    dates = rebal_dates[1],
    sw_port_1@port_stats %>% dplyr::rename(IR = info_ratio)
  )
  port_stats_2 <- summarize_performance(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), c("raw_return", "net_return")],
    selected_market_factor_proxy_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), "selected_bench_return"],
    model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = FALSE,
    selected_signal_themes_m_d_ref = NULL, active_returns = TRUE, verbose = FALSE
  )$signal_universe_m_d_ref %>%
    dplyr::left_join(
      data.frame(
        tickers = c("raw_return", "net_return"),
        sw_port_2@port_stats %>% dplyr::rename(IR = info_ratio)
      ), by = "tickers"
    )

  port_stats <- dplyr::bind_rows(port_stats_1, port_stats_2) %>%
    dplyr::arrange(id)
  port_stats <- port_stats[, names(port_stats_2)]

  expect_equal(
    results@port_stats_m_df@data, port_stats
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
    port_construction_method = "sw",
    selected_benchmark = "ibov"
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
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    selected_benchmark = "ibov"
  )

  #Set Port Weights
  sw_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "sw",
    selected_benchmark = "ibov"
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
    port_construction_method = "sw",
    selected_benchmark = "ibov"
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

  rebal_dates <- results@port_backtest_workflow[[length(results@port_backtest_workflow)]]$rebalance_dates
  port_stats_1 <- data.frame(
    id = paste0(c("raw_return-", "net_return-"), rebal_dates[1]),
    tickers = c("raw_return", "net_return"),
    dates = rebal_dates[1],
    sw_port_1@port_stats %>% dplyr::rename(IR = info_ratio)
  )
  port_stats_2 <- data.frame(
    id = paste0(c("raw_return-", "net_return-"), rebal_dates[2]),
    tickers = c("raw_return", "net_return"),
    dates = rebal_dates[2],
    sw_port_2@port_stats %>% dplyr::rename(IR = info_ratio)
  )

  port_stats_3 <- summarize_performance(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[3]),
                                      c("raw_return", "net_return")],
    selected_market_factor_proxy_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[3]),
                                      "selected_bench_return"],
    model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = FALSE,
    selected_signal_themes_m_d_ref = NULL, active_returns = TRUE, verbose = FALSE
  )$signal_universe_m_d_ref %>%
    dplyr::left_join(
      data.frame(
        tickers = c("raw_return", "net_return"),
        sw_port_3@port_stats %>% dplyr::rename(IR = info_ratio)
      ), by = "tickers"
    ) %>%
    dplyr::arrange(id)

  port_stats <- dplyr::bind_rows(port_stats_1, port_stats_2, port_stats_3) %>%
    dplyr::arrange(id)
  port_stats <- port_stats[, names(port_stats_3)]

  expect_equal(
    results@port_stats_m_df@data, port_stats
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
    cap_weighting_metric = "mean_volfin_3m",
    selected_benchmark = "ibov"
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
    cap_weighting_metric = "mean_volfin_3m",
    selected_benchmark = "ibov"
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

  rebal_dates <- results@port_backtest_workflow[[length(results@port_backtest_workflow)]]$rebalance_dates
  port_stats_1 <- data.frame(
    id = paste0(c("raw_return-", "net_return-"), rebal_dates[1]),
    tickers = c("raw_return", "net_return"),
    dates = rebal_dates[1],
    cs_port_1@port_stats %>% dplyr::rename(IR = info_ratio)
  )
  port_stats_2 <- summarize_performance(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), c("raw_return", "net_return")],
    selected_market_factor_proxy_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), "selected_bench_return"],
    model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = FALSE,
    selected_signal_themes_m_d_ref = NULL, active_returns = TRUE, verbose = FALSE
  )$signal_universe_m_d_ref %>%
    dplyr::left_join(
      data.frame(
        tickers = c("raw_return", "net_return"),
        cs_port_2@port_stats %>% dplyr::rename(IR = info_ratio)
      ), by = "tickers"
    ) %>%
    dplyr::arrange(id)

  port_stats <- dplyr::bind_rows(port_stats_1, port_stats_2) %>%
    dplyr::arrange(id)
  port_stats <- port_stats[, names(port_stats_2)]

  expect_equal(
    results@port_stats_m_df@data, port_stats
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
  suppressWarnings(
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
  suppressWarnings(
    rp_port_1 <- set_portfolio_weights(
      universe_m_d_ref = stock_universe_m_d_ref_1,
      port_construction_method = "rp",
      groups_m_d_ref = stock_groups_m_d_ref,
      eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
      exp_ret_score_tilt = port_config@rp_parameters@exp_ret_score_tilt,
      exp_ret_score_tilt_eta = port_config@rp_parameters@exp_ret_score_tilt_eta,
      selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
      selected_benchmark = "ibov",
      cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
    )
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  suppressWarnings(
    rp_port_contrafactual <- set_portfolio_weights(
      universe_m_d_ref = stock_universe_m_d_ref_1 %>% dplyr::mutate(exp_ret_score = exp_ret_score_raw),
      port_construction_method = "rp",
      groups_m_d_ref = stock_groups_m_d_ref,
      selected_benchmark = "ibov",
      eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
      selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
      cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
    )
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
    selected_benchmark = "ibov",
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    selected_benchmark = "ibov",
    exp_ret_score_tilt = port_config@rp_parameters@exp_ret_score_tilt,
    exp_ret_score_tilt_eta = port_config@rp_parameters@exp_ret_score_tilt_eta,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  rp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2 %>% dplyr::mutate(exp_ret_score = exp_ret_score_raw),
    port_construction_method = "rp",
    groups_m_d_ref = stock_groups_m_d_ref,
    selected_benchmark = "ibov",
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    selected_benchmark = "ibov",
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    exp_ret_score_tilt = port_config@hrp_parameters@exp_ret_score_tilt,
    exp_ret_score_tilt_eta = port_config@hrp_parameters@exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  hrp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1 %>% dplyr::mutate(exp_ret_score = exp_ret_score_raw),
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
  )

  ## Test that exp_ret_score_tilt increases dy_med_36m
  hrp_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2 %>% dplyr::mutate(exp_ret_score = exp_ret_score_raw),
    port_construction_method = "hrp",
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
  set.seed(123)
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
                                 parallel = FALSE,
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
  suppressWarnings(
  mmaf_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref =
      daily_stock_returns_m_xts_upd_ref,
    macro_opt_method = "random", macro_rp_method = "sample",
    macro_n_random_ports = 500, macro_opt_objective = "sharpe",
    macro_ridge_pen = macro_ridge_pen, macro_n_resamples = 3,
    macro_exp_ret_score_jitter = 0.2, macro_cov_eigval_jitter = 0.3,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = "rp",
    mmaf_method = "top_down",
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_m_d_ref = liquidity_m_d_ref,
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "mvo",
    micro_port_construction_method = "hrp",
    exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov",
    bench_assets_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    verbose = TRUE
    )
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
  suppressWarnings(
  mmaf_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov", bench_assets_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref
    )
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
  suppressWarnings(
  mmaf_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov", bench_assets_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref
  )
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
  suppressWarnings(
  mmaf_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    macro_opt_method = "random", macro_rp_method = "sample",
    macro_n_random_ports = 500, macro_opt_objective = "sharpe",
    macro_ridge_pen = macro_ridge_pen, macro_n_resamples = 3,
    macro_exp_ret_score_jitter = 0.2, macro_cov_eigval_jitter = 0.3,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = "rp",
    mmaf_method = "top_down",
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "mvo",
    micro_port_construction_method = "hrp",
    liquidity_m_d_ref = liquidity_m_d_ref,
    exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov", bench_assets_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref
  )
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
  suppressWarnings(
  mmaf_port_contrafactual <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov", bench_assets_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref
  )
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

  mmaf_port_contrafactual <- suppressWarnings(set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mmaf",
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov", bench_assets_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref
  )
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


  #Port stats
  rebal_dates <- results@port_backtest_workflow[[length(results@port_backtest_workflow)]]$rebalance_dates
  port_stats_1 <- data.frame(
    id = paste0(c("raw_return-", "net_return-"), rebal_dates[1]),
    tickers = c("raw_return", "net_return"),
    dates = rebal_dates[1],
    mmaf_port_1@port_stats %>% dplyr::rename(IR = info_ratio)
  )
  port_stats_2 <- summarize_performance(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), c("raw_return", "net_return")],
    selected_market_factor_proxy_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), "selected_bench_return"],
    model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = FALSE,
    selected_signal_themes_m_d_ref = NULL, active_returns = TRUE, verbose = FALSE
  )$signal_universe_m_d_ref %>%
    dplyr::left_join(
      data.frame(
        tickers = c("raw_return", "net_return"),
        mmaf_port_2@port_stats %>% dplyr::rename(IR = info_ratio)
      ), by = "tickers"
    ) %>%
    dplyr::arrange(id)
  port_stats <- dplyr::bind_rows(port_stats_1, port_stats_2) %>%
    dplyr::arrange(id)
  port_stats <- port_stats[, names(port_stats_2)]

  expect_equal(
    results@port_stats_m_df@data,
    port_stats)

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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = NULL,
    mmaf_method = "bottom_up",
    concentration_constraint_policy = as.list(concentration_constraint_policy),
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "rp",
    micro_port_construction_method = "rp",
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
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
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    mmaf_group_col = "macro_sector", top_down_proxy_port_method = NULL,
    mmaf_method = "bottom_up",
    concentration_constraint_policy = as.list(concentration_constraint_policy),
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    macro_concentration_constraint_policy = as.list(macro_concentration_constraint_policy),
    macro_port_construction_method = "rp",
    micro_port_construction_method = "rp",
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    cov_estimation_method = "ewma", cov_matrix_sample_size = 52, active_returns = TRUE,
    selected_benchmark = "ibov"
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
    port_construction_method = "sw",
    selected_benchmark = "ibov"
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
    port_construction_method = "sw",
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports,
    selected_benchmark = "ibov"
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
      eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
      selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
      active_returns = port_config@cov_est_method@active_returns,
      cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
      cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
      n_random_ports = port_config@mvo_parameters@n_random_ports,
      selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports,
    selected_benchmark = "ibov"
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
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = daily_bench_returns_m_xts_upd_ref[, "ibov"],
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports,
    selected_benchmark = "ibov"
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
  ),
  "Weights for group 'Utilidade Pública' sum to zero. Fallback to equal weights."
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
  suppressWarnings(
  mvo_port_1 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_1,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = NULL,
    opt_objective = "return",
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )
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
  suppressWarnings(
  mvo_port_2 <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref_2,
    port_construction_method = "mvo",
    liquidity_constraint_policy = as.list(port_config@liquidity_constraint_policy),
    concentration_constraint_policy = as.list(port_config@concentration_constraint_policy),
    turnover_constraint_policy = as.list(port_config@turnover_constraint_policy),
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    selected_benchmark_m_xts_upd_ref = NULL,
    opt_objective = "return",
    active_returns = port_config@cov_est_method@active_returns,
    cov_estimation_method = port_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = port_config@cov_est_method@cov_matrix_sample_size,
    n_random_ports = port_config@mvo_parameters@n_random_ports
  )
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

  #Check for port stats
  rebal_dates <- results@port_backtest_workflow[[length(results@port_backtest_workflow)]]$rebalance_dates
  port_stats_1 <- data.frame(
    id = paste0(c("raw_return-", "net_return-"), rebal_dates[1]),
    tickers = c("raw_return", "net_return"),
    dates = rebal_dates[1],
    mvo_port_1@port_stats %>% dplyr::rename(SR = sharpe)
  )
  port_stats_2 <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref =
      results@port_returns_m_xts@data[which(zoo::index(results@port_returns_m_xts@data) <= rebal_dates[2]), c("raw_return", "net_return")],
    selected_market_factor_proxy_m_xts_upd_ref = NULL, active_returns = FALSE
  ) %>%
    dplyr::left_join(
      data.frame(
        tickers = c("raw_return", "net_return"),
        mvo_port_2@port_stats %>% dplyr::rename(SR = sharpe)
      ), by = "tickers"
    )

  port_stats <- dplyr::bind_rows(port_stats_1, port_stats_2) %>%
    dplyr::arrange(id)
  port_stats <- port_stats[, names(port_stats_2)]

  expect_equal(
    results@port_stats_m_df@data, port_stats
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
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")

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
                                    stock_groups_m_df = stock_groups_m_df,
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
  expect_equal(port_cohort@port_weights_m_df@data$c__sw_book_yield_s__not_identified_f__not_identified,
               sw_results@port_weights_m_df@data$eop_port_weights)
  expect_equal(port_cohort@port_weights_m_df@data$c__cs_book_yield_s__not_identified_f__not_identified,
               cs_results@port_weights_m_df@data$eop_port_weights)
  expect_equal(port_cohort@port_weights_m_df@data$c__cw_book_yield_s__not_identified_f__not_identified,
               cw_results@port_weights_m_df@data$eop_port_weights)
  expect_equal(port_cohort@port_weights_m_df@data$bench_weights, cw_results@port_weights_m_df@data$bench_weights)
  expect_equal(port_cohort@port_weights_m_df@data$bench_weights, sw_results@port_weights_m_df@data$bench_weights)
  expect_equal(port_cohort@port_weights_m_df@data$bench_weights, cs_results@port_weights_m_df@data$bench_weights)

  expect_true(all(cs_results@port_weights_m_df@data$id %in% port_cohort@port_weights_m_df@data$id))

  #Check that port returns are according to expectation
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_returns_m_xts@data$raw_return %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$selected_bench_return, sw_results@port_returns_m_xts@data$selected_bench_return)
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$selected_bench_return, cs_results@port_returns_m_xts@data$selected_bench_return)
  expect_equal(port_cohort@port_returns_m_xts_list$raw_returns_m_xts@data$selected_bench_return, cw_results@port_returns_m_xts@data$selected_bench_return)

  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_returns_m_xts@data$net_return %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$selected_bench_return, sw_results@port_returns_m_xts@data$selected_bench_return)
  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$selected_bench_return, cs_results@port_returns_m_xts@data$selected_bench_return)
  expect_equal(port_cohort@port_returns_m_xts_list$net_returns_m_xts@data$selected_bench_return, cw_results@port_returns_m_xts@data$selected_bench_return)

  expect_equal(port_cohort@port_returns_m_xts_list$raw_active_returns_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified  %>% as.data.frame() %>% unname(),
               sw_results@port_returns_m_xts@data$raw_active_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$raw_active_returns_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_returns_m_xts@data$raw_active_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$raw_active_returns_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_returns_m_xts@data$raw_active_return %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_returns_m_xts_list$net_active_returns_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_returns_m_xts@data$net_active_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$net_active_returns_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_returns_m_xts@data$net_active_return %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_returns_m_xts_list$net_active_returns_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_returns_m_xts@data$net_active_return %>% as.data.frame() %>% unname())


  #Check that port costs are accordng to expectation
  expect_equal(port_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$direct_cost_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_costs_m_xts@data$direct_cost %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$market_impact_cost_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_costs_m_xts@data$market_impact_cost %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$total_cost_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_costs_m_xts@data$total_cost %>% as.data.frame() %>% unname())

  expect_equal(port_cohort@port_costs_m_xts_list$turnover_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               sw_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$turnover_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_costs_m_xts_list$turnover_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_costs_m_xts@data$turnover %>% as.data.frame() %>% unname())

  #Port Metrics
  #Check that length is 3 (no sw)
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data %>% ncol(), 3)
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_metrics_m_xts@data$book_yield %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_metrics_m_xts@data$book_yield %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data$bench_book_yield %>% as.data.frame() %>% unname(),
               cs_results@port_metrics_m_xts@data$bench_book_yield %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_metrics_m_xts_list$book_yield_m_xts@data$bench_book_yield %>% as.data.frame() %>% unname(),
               cw_results@port_metrics_m_xts@data$bench_book_yield %>% as.data.frame() %>% unname())


  expect_equal(port_cohort@port_metrics_m_xts_list$dy_med_36m_m_xts@data %>% ncol(), 3)
  expect_equal(port_cohort@port_metrics_m_xts_list$dy_med_36m_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cs_results@port_metrics_m_xts@data$dy_med_36m %>% as.data.frame() %>% unname())
  expect_equal(port_cohort@port_metrics_m_xts_list$dy_med_36m_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_metrics_m_xts@data$dy_med_36m %>% as.data.frame() %>% unname())

  #For vol_36m, there is data only for cw
  expect_equal(port_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data %>% ncol(), 2)
  expect_equal(port_cohort@port_metrics_m_xts_list$vol_36m_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.data.frame() %>% unname(),
               cw_results@port_metrics_m_xts@data$vol_36m %>% as.data.frame() %>% unname())

  #Check that all metrics are present
  expect_equal(stringr::str_remove(names(port_cohort@port_metrics_m_xts_list), "_m_xts"), colnames(port_metrics_m_df@data)[-c(1:3)])

  #Check that port stats are as expected
  expect_equal(purrr::map_dbl(
    port_cohort@port_stats_m_xts_nested_list$net_return,
    function(x) ncol(x@data)
  ) %>% unique(), c(3, 1))

  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$net_return$info_ratio_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.numeric(),
    cw_results@port_stats_m_df@data %>% dplyr::filter(tickers == "net_return") %>% dplyr::pull(info_ratio) %>% as.numeric()
  )
  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$net_return$info_ratio_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.numeric(),
    cs_results@port_stats_m_df@data %>% dplyr::filter(tickers == "net_return") %>% dplyr::pull(info_ratio) %>% as.numeric()
  )
  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$net_return$info_ratio_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.numeric(),
    sw_results@port_stats_m_df@data %>% dplyr::filter(tickers == "net_return") %>% dplyr::pull(info_ratio) %>% as.numeric()
  )

  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$raw_return$info_ratio_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.numeric(),
    cw_results@port_stats_m_df@data %>% dplyr::filter(tickers == "raw_return") %>% dplyr::pull(info_ratio) %>% as.numeric()
  )
  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$raw_return$info_ratio_m_xts@data$c__cs_book_yield_s__not_identified_f__not_identified %>% as.numeric(),
    cs_results@port_stats_m_df@data %>% dplyr::filter(tickers == "raw_return") %>% dplyr::pull(info_ratio) %>% as.numeric()
  )
  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$raw_return$info_ratio_m_xts@data$c__sw_book_yield_s__not_identified_f__not_identified %>% as.numeric(),
    sw_results@port_stats_m_df@data %>% dplyr::filter(tickers == "raw_return") %>% dplyr::pull(info_ratio) %>% as.numeric()
  )

  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$net_return %>% names() %>% stringr::str_remove("_m_xts"),
    names(cw_results@port_stats_m_df@data)[-c(1:3)] %>% sort()
  )

  #Only cw has group cols
  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$net_return$group_info_ratio_m_xts@data %>% names(),
    "c__cw_book_yield_s__not_identified_f__not_identified"
  )
  expect_equal(
    port_cohort@port_stats_m_xts_nested_list$net_return$n_groups_m_xts@data$c__cw_book_yield_s__not_identified_f__not_identified %>% as.numeric(),
    cw_results@port_stats_m_df@data %>% dplyr::filter(tickers == "net_return") %>% dplyr::pull(n_groups)
  )



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
    port_construction_method = "ew",
    selected_benchmark = "ibov"
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
    port_construction_method = "ew",
    selected_benchmark = "ibov"
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
test_that("update_port_backtest works for a simple sw single signal strategy with a selected benchmark and user_defined_OR_rules, with new month a rebalancing month", {

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
  expect_equal(new_results@port_stats_m_df@data, updated_results@port_stats_m_df@data)
  expect_equal(new_results@transactions_log@data, updated_results@transactions_log@data)
  expect_equal(new_results@stock_universe_m_df@data, updated_results@stock_universe_m_df@data)
  expect_equal(new_results@final_stock_port, updated_results@final_stock_port)
  expect_equal(updated_results@port_backtest_config@initial_buffer_period, length(unique(signals_m_df@data$dates)) - 1)


})

test_that("update_port_backtest works for a simple cs single signal strategy with a selected benchmark, with new month a post-rebalancing month AND 2 UPDATES", {

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
            expect_equal(new_results@port_stats_m_df@data, updated_results@port_stats_m_df@data)
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
            expect_equal(new_results2@port_stats_m_df@data, updated_results2@port_stats_m_df@data)
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
  expect_equal(updated_sb_meta_port_results_2@port_stats_m_df@data, new_sb_metaport_results@port_stats_m_df@data)
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

#SLSAF
test_that("run_port_backtest works for a slsaf strategy with a signal-weighted long leg", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    eligibility_quantile_range = c(0.67, 1.0),
    selected_benchmark = "ibov",
    initial_buffer_period = 5,
    rebalancing_months = 4,
    port_construction_method = "slsaf",
    main_liquidity_metric = "mean_volfin_3m",
    config_name = "slsaf_model"
  ) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts["2022-10-15/2023-04-15"])

  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 verbose = FALSE)
  )

  expect_s4_class(results, "port_backtest_results")
  expect_equal(results@port_construction_method, "slsaf")

  #The final portfolio is a valid slsaf portfolio with both legs reported
  final_port <- results@final_stock_port
  expect_equal(final_port@port_construction_method, "slsaf")
  expect_named(final_port@micro, c("long", "short"))
  expect_s4_class(final_port@micro$long, "port")
  expect_s4_class(final_port@micro$short, "port")

  #The diagnostics reach port_stats_m_df, which is what makes the endogenous active
  #budget visible per rebalance date
  port_stats_m_df <- results@port_stats_m_df@data
  expect_true(all(c("slsaf_short_budget", "slsaf_active_budget", "slsaf_n_long",
                    "slsaf_n_short", "slsaf_n_zeroed") %in% names(port_stats_m_df)))
  expect_true(all(port_stats_m_df$slsaf_active_budget <= port_stats_m_df$slsaf_short_budget + 1e-8))
  expect_true(all(port_stats_m_df$slsaf_n_long > 0))

  #Every rebalance date must satisfy the construction invariants
  stock_universe_m_df <- results@stock_universe_m_df@data
  rebalance_dates <- unique(stock_universe_m_df$dates)

  for (rebalance_date in rebalance_dates){

    universe_m_d_ref <- stock_universe_m_df %>% dplyr::filter(dates == rebalance_date)

    bench_weights <- universe_m_d_ref$ibov_bench_weights
    weights <- universe_m_d_ref$weights
    long  <- which(universe_m_d_ref$is_long_candidate == 1L)
    short <- which(universe_m_d_ref$is_short_candidate == 1L)

    ##The overlay is self-financing and the portfolio is long-only
    expect_equal(sum(weights), 1, tolerance = 1e-6)
    expect_true(all(weights >= 0))

    ##A disliked constituent is never overweighted, an eligible name never underweighted
    expect_true(all(weights[short] <= bench_weights[short] + 1e-8))
    expect_true(all(weights[long] >= bench_weights[long] - 1e-8))

    ##The whole benchmark is represented, which is what makes an underweight expressible
    constituents <- which(bench_weights > 0)
    expect_true(all(universe_m_d_ref$is_eligible[constituents] == 1))
  }

  #Both budget series must be available to the budget plot: the gap between them is the
  #capping loss, which is what that plot exists to show. The plot method itself cannot be
  #asserted on, because every plot method in this package prompts and returns NULL.
  expect_true(all(c("slsaf_short_budget", "slsaf_active_budget") %in% names(port_stats_m_df)))
  expect_false(isTRUE(all.equal(port_stats_m_df$slsaf_short_budget,
                                port_stats_m_df$slsaf_active_budget)))
})

test_that("run_port_backtest works for a slsaf strategy with a risk-parity long leg", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    eligibility_quantile_range = c(0.67, 1.0),
    selected_benchmark = "ibov",
    initial_buffer_period = 5,
    rebalancing_months = 4,
    port_construction_method = "slsaf",
    main_liquidity_metric = "mean_volfin_3m",
    config_name = "slsaf_rp_model"
  ) %>%
    add_slsaf_parameters(long_port_construction_method = "rp",
                         badness_tilt_eta = 2,
                         max_short_budget = 0.20) %>%
    add_cov_est_method(cov_estimation_method = "ewma", cov_matrix_sample_size = 52,
                       active_returns = TRUE) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 50000, mid_caps = 100000, large_caps = 200000, mega_caps = 500000),
        c(micro_caps = 97.5, small_caps = 100, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "micro_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark")
  #Groups are needed to fill NAs in the daily returns sample during covariance estimation
  stock_groups_m_df <- create_meta_dataframe(stock_groups_m_df, type = "groups")
  daily_stock_returns_m_xts <- suppressWarnings(
    create_meta_xts(daily_stock_returns_m_xts, type = "returns", asset_type = "stocks",
                    meta_xts_name = "B3")
  )
  daily_bench_returns_m_xts_mocked <- suppressWarnings(
    create_meta_xts(xts::xts(data.frame(
      ibov = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      smll = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5),
      idiv = rnorm(n = nrow(daily_stock_returns_m_xts@data), mean = 0, sd = 0.5)
    ), order.by = zoo::index(daily_stock_returns_m_xts@data)
    ), type = "returns", asset_type = "benchmark", meta_xts_name = "B3")
  )

  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df,
                                 fwd_return_m_df = fwd_return_m_df,
                                 liquidity_m_df = liquidity_m_df,
                                 volatility_m_df = volatility_m_df,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts,
                                 stock_groups_m_df = stock_groups_m_df,
                                 daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                                 daily_bench_returns_m_xts = daily_bench_returns_m_xts_mocked,
                                 verbose = FALSE)
  )

  expect_s4_class(results, "port_backtest_results")

  #The long leg was built with a covariance matrix, so risk contributions exist
  final_port <- results@final_stock_port
  expect_equal(final_port@micro$long@port_construction_method, "rp")
  expect_false(is.null(final_port@micro$long@covariance_matrix))

  #The ceiling binds the realized active budget at every rebalance date
  port_stats_m_df <- results@port_stats_m_df@data
  expect_true(all(port_stats_m_df$slsaf_active_budget <= 0.20 + 1e-8))

  #And the construction still holds end to end
  stock_universe_m_df <- results@stock_universe_m_df@data
  for (rebalance_date in unique(stock_universe_m_df$dates)){
    universe_m_d_ref <- stock_universe_m_df %>% dplyr::filter(dates == rebalance_date)
    expect_equal(sum(universe_m_d_ref$weights), 1, tolerance = 1e-6)
    expect_true(all(universe_m_d_ref$weights >= 0))
  }
})

#SLSAF updates
test_that("update_port_backtest works for a slsaf strategy, with new month a rebalancing month", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #April is month 4, so the incoming month triggers a rebalancing and the slsaf
  #construction runs on the new date
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    eligibility_quantile_range = c(0.67, 1.0),
    selected_benchmark = "ibov",
    initial_buffer_period = 5,
    rebalancing_months = 4,
    port_construction_method = "slsaf",
    main_liquidity_metric = "mean_volfin_3m",
    config_name = "slsaf_update_model"
  ) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes truncated at 2023-03-15
  signals_m_df_old <- create_meta_dataframe(
    signals_m_df %>% dplyr::filter(!dates == "2023-04-15"), type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df_old <- create_meta_dataframe(
    fwd_return_m_df %>% dplyr::filter(!dates == "2023-04-15") %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-03-15", NA_real_, fwd_return_1m)),
    type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df_old <- create_meta_dataframe(
    liquidity_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "liq")
  volatility_m_df_old <- create_meta_dataframe(
    volatility_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "vol")
  benchmark_returns_m_xts_old <- create_meta_xts(
    benchmark_returns_m_xts["2022-10-15/2023-03-15"], asset_type = "benchmark",
    meta_xts_name = "bench_returns")
  benchmark_weights_m_df_old <- create_meta_dataframe(
    benchmark_weights_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "bench_weights")

  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df_old,
                                 fwd_return_m_df = fwd_return_m_df_old,
                                 liquidity_m_df = liquidity_m_df_old,
                                 volatility_m_df = volatility_m_df_old,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df_old,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts_old,
                                 verbose = FALSE)
  )

  #A new batch of data arrives
  signals_m_df_new <- create_meta_dataframe(signals_m_df, type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df_new <- create_meta_dataframe(fwd_return_m_df, type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df_new <- create_meta_dataframe(liquidity_m_df, meta_dataframe_name = "liq")
  volatility_m_df_new <- create_meta_dataframe(volatility_m_df, meta_dataframe_name = "vol")
  benchmark_returns_m_xts_new <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark",
                                                 meta_xts_name = "bench_returns")
  benchmark_weights_m_df_new <- create_meta_dataframe(benchmark_weights_m_df, meta_dataframe_name = "bench_weights")

  #The same backtest run once over the whole panel
  suppressWarnings(
    new_results <- run_port_backtest(signals_m_df = signals_m_df_new,
                                     fwd_return_m_df = fwd_return_m_df_new,
                                     liquidity_m_df = liquidity_m_df_new,
                                     volatility_m_df = volatility_m_df_new,
                                     config = port_config,
                                     benchmark_weights_m_df = benchmark_weights_m_df_new,
                                     benchmark_returns_m_xts = benchmark_returns_m_xts_new,
                                     verbose = FALSE)
  )

  #The truncated backtest brought forward
  suppressWarnings(
    updated_results <- update_port_backtest(signals_m_df = signals_m_df_new,
                                            fwd_return_m_df = fwd_return_m_df_new,
                                            liquidity_m_df = liquidity_m_df_new,
                                            volatility_m_df = volatility_m_df_new,
                                            old_results = results,
                                            benchmark_weights_m_df = benchmark_weights_m_df_new,
                                            benchmark_returns_m_xts = benchmark_returns_m_xts_new)
  )

  #Updating must be indistinguishable from running the whole panel at once
  expect_equal(new_results@port_weights_m_df@data, updated_results@port_weights_m_df@data)
  expect_equal(new_results@port_costs_m_xts@data, updated_results@port_costs_m_xts@data)
  expect_equal(new_results@port_returns_m_xts@data, updated_results@port_returns_m_xts@data)
  expect_equal(new_results@port_stats_m_df@data, updated_results@port_stats_m_df@data)
  expect_equal(new_results@transactions_log@data, updated_results@transactions_log@data)
  expect_equal(new_results@stock_universe_m_df@data, updated_results@stock_universe_m_df@data)
  expect_equal(new_results@final_stock_port, updated_results@final_stock_port)
  expect_equal(updated_results@port_backtest_config@initial_buffer_period,
               length(unique(signals_m_df_new@data$dates)) - 1)

  #consolidate_backtest_results() binds old and new runs with bind_rows(), which fills
  #absent columns with NA rather than failing. slsaf is the first method to add columns
  #to both consolidated frames, so assert they survived the merge intact.
  consolidated_universe_m_df <- updated_results@stock_universe_m_df@data
  expect_true(all(c("is_long_candidate", "is_short_candidate") %in% names(consolidated_universe_m_df)))
  expect_false(any(is.na(consolidated_universe_m_df$is_long_candidate)))
  expect_false(any(is.na(consolidated_universe_m_df$is_short_candidate)))

  consolidated_port_stats_m_df <- updated_results@port_stats_m_df@data
  slsaf_stats_cols <- c("slsaf_short_budget", "slsaf_active_budget", "slsaf_n_long",
                        "slsaf_n_short", "slsaf_n_zeroed")
  expect_true(all(slsaf_stats_cols %in% names(consolidated_port_stats_m_df)))
  expect_false(any(is.na(consolidated_port_stats_m_df[, slsaf_stats_cols])))

  #The incoming month must actually have been rebuilt, otherwise this test would pass
  #without ever running the slsaf construction on new data
  expect_true(as.Date("2023-04-15") %in% unique(consolidated_universe_m_df$dates))
  expect_gte(length(unique(consolidated_universe_m_df$dates)), 2)

  #And the construction still holds on every rebalance date of the consolidated run
  for (rebalance_date in unique(consolidated_universe_m_df$dates)){

    universe_m_d_ref <- consolidated_universe_m_df %>% dplyr::filter(dates == rebalance_date)

    bench_weights <- universe_m_d_ref$ibov_bench_weights
    weights <- universe_m_d_ref$weights
    long  <- which(universe_m_d_ref$is_long_candidate == 1L)
    short <- which(universe_m_d_ref$is_short_candidate == 1L)

    expect_equal(sum(weights), 1, tolerance = 1e-6)
    expect_true(all(weights >= 0))
    expect_true(all(weights[short] <= bench_weights[short] + 1e-8))
    expect_true(all(weights[long] >= bench_weights[long] - 1e-8))
  }
})

test_that("update_port_backtest works for a slsaf strategy, with new month a post-rebalancing month", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #February is month 2, so the incoming April date is NOT a rebalancing month and the
  #update exercises the pickup branch, where no portfolio is rebuilt
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    eligibility_quantile_range = c(0.67, 1.0),
    selected_benchmark = "ibov",
    initial_buffer_period = 4,
    rebalancing_months = 2,
    port_construction_method = "slsaf",
    main_liquidity_metric = "mean_volfin_3m",
    config_name = "slsaf_pickup_model"
  ) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes truncated at 2023-02-15
  signals_m_df_old <- create_meta_dataframe(
    signals_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")),
    type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df_old <- create_meta_dataframe(
    fwd_return_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")) %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-02-15", NA_real_, fwd_return_1m)),
    type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df_old <- create_meta_dataframe(
    liquidity_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), meta_dataframe_name = "liq")
  volatility_m_df_old <- create_meta_dataframe(
    volatility_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")), meta_dataframe_name = "vol")
  benchmark_returns_m_xts_old <- create_meta_xts(
    benchmark_returns_m_xts["2022-10-15/2023-02-15"], asset_type = "benchmark",
    meta_xts_name = "bench_returns")
  benchmark_weights_m_df_old <- create_meta_dataframe(
    benchmark_weights_m_df %>% dplyr::filter(!dates %in% c("2023-03-15", "2023-04-15")),
    meta_dataframe_name = "bench_weights")

  suppressWarnings(
    results <- run_port_backtest(signals_m_df = signals_m_df_old,
                                 fwd_return_m_df = fwd_return_m_df_old,
                                 liquidity_m_df = liquidity_m_df_old,
                                 volatility_m_df = volatility_m_df_old,
                                 config = port_config,
                                 benchmark_weights_m_df = benchmark_weights_m_df_old,
                                 benchmark_returns_m_xts = benchmark_returns_m_xts_old,
                                 verbose = FALSE)
  )

  #A new batch of data arrives, one month at a time
  signals_m_df_mid <- create_meta_dataframe(
    signals_m_df %>% dplyr::filter(!dates == "2023-04-15"), type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df_mid <- create_meta_dataframe(
    fwd_return_m_df %>% dplyr::filter(!dates == "2023-04-15") %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == "2023-03-15", NA_real_, fwd_return_1m)),
    type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df_mid <- create_meta_dataframe(
    liquidity_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "liq")
  volatility_m_df_mid <- create_meta_dataframe(
    volatility_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "vol")
  benchmark_returns_m_xts_mid <- create_meta_xts(
    benchmark_returns_m_xts["2022-10-15/2023-03-15"], asset_type = "benchmark",
    meta_xts_name = "bench_returns")
  benchmark_weights_m_df_mid <- create_meta_dataframe(
    benchmark_weights_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "bench_weights")

  #The same backtest run once over the whole panel
  suppressWarnings(
    new_results <- run_port_backtest(signals_m_df = signals_m_df_mid,
                                     fwd_return_m_df = fwd_return_m_df_mid,
                                     liquidity_m_df = liquidity_m_df_mid,
                                     volatility_m_df = volatility_m_df_mid,
                                     config = port_config,
                                     benchmark_weights_m_df = benchmark_weights_m_df_mid,
                                     benchmark_returns_m_xts = benchmark_returns_m_xts_mid,
                                     verbose = FALSE)
  )

  #The truncated backtest brought forward into a non-rebalancing month
  suppressWarnings(
    updated_results <- update_port_backtest(signals_m_df = signals_m_df_mid,
                                            fwd_return_m_df = fwd_return_m_df_mid,
                                            liquidity_m_df = liquidity_m_df_mid,
                                            volatility_m_df = volatility_m_df_mid,
                                            old_results = results,
                                            benchmark_weights_m_df = benchmark_weights_m_df_mid,
                                            benchmark_returns_m_xts = benchmark_returns_m_xts_mid)
  )

  #Updating must be indistinguishable from running the whole panel at once, including
  #when no portfolio is rebuilt and the old one is simply carried forward
  expect_equal(new_results@port_weights_m_df@data, updated_results@port_weights_m_df@data)
  expect_equal(new_results@port_costs_m_xts@data, updated_results@port_costs_m_xts@data)
  expect_equal(new_results@port_returns_m_xts@data, updated_results@port_returns_m_xts@data)
  expect_equal(new_results@port_stats_m_df@data, updated_results@port_stats_m_df@data)
  expect_equal(new_results@transactions_log@data, updated_results@transactions_log@data)
  expect_equal(new_results@stock_universe_m_df@data, updated_results@stock_universe_m_df@data)
  expect_equal(new_results@final_stock_port, updated_results@final_stock_port)

  #The pickup branch is where a schema mismatch would go unnoticed, since no new
  #slsaf universe or diagnostics are produced for the carried-forward date
  consolidated_universe_m_df <- updated_results@stock_universe_m_df@data

  #Confirm this really is the pickup branch: the incoming month was carried forward
  #rather than rebuilt, while earlier dates did run the slsaf construction
  expect_false(as.Date("2023-03-15") %in% unique(consolidated_universe_m_df$dates))
  expect_gte(length(unique(consolidated_universe_m_df$dates)), 1)
  expect_true(all(c("is_long_candidate", "is_short_candidate") %in% names(consolidated_universe_m_df)))
  expect_false(any(is.na(consolidated_universe_m_df$is_long_candidate)))
  expect_false(any(is.na(consolidated_universe_m_df$is_short_candidate)))

  consolidated_port_stats_m_df <- updated_results@port_stats_m_df@data
  slsaf_stats_cols <- c("slsaf_short_budget", "slsaf_active_budget", "slsaf_n_long",
                        "slsaf_n_short", "slsaf_n_zeroed")
  expect_true(all(slsaf_stats_cols %in% names(consolidated_port_stats_m_df)))
  expect_false(any(is.na(consolidated_port_stats_m_df[, slsaf_stats_cols])))

  #A second update, now bringing in a rebalancing month on top of a pickup
  signals_m_df_new <- create_meta_dataframe(signals_m_df, type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df_new <- create_meta_dataframe(fwd_return_m_df, type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df_new <- create_meta_dataframe(liquidity_m_df, meta_dataframe_name = "liq")
  volatility_m_df_new <- create_meta_dataframe(volatility_m_df, meta_dataframe_name = "vol")
  benchmark_returns_m_xts_new <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark",
                                                 meta_xts_name = "bench_returns")
  benchmark_weights_m_df_new <- create_meta_dataframe(benchmark_weights_m_df, meta_dataframe_name = "bench_weights")

  suppressWarnings(
    new_results2 <- run_port_backtest(signals_m_df = signals_m_df_new,
                                      fwd_return_m_df = fwd_return_m_df_new,
                                      liquidity_m_df = liquidity_m_df_new,
                                      volatility_m_df = volatility_m_df_new,
                                      config = port_config,
                                      benchmark_weights_m_df = benchmark_weights_m_df_new,
                                      benchmark_returns_m_xts = benchmark_returns_m_xts_new,
                                      verbose = FALSE)
  )

  suppressWarnings(
    updated_results2 <- update_port_backtest(signals_m_df = signals_m_df_new,
                                             fwd_return_m_df = fwd_return_m_df_new,
                                             liquidity_m_df = liquidity_m_df_new,
                                             volatility_m_df = volatility_m_df_new,
                                             old_results = updated_results,
                                             benchmark_weights_m_df = benchmark_weights_m_df_new,
                                             benchmark_returns_m_xts = benchmark_returns_m_xts_new)
  )

  #Two successive updates must still reproduce the single full run exactly
  expect_equal(new_results2@port_weights_m_df@data, updated_results2@port_weights_m_df@data)
  expect_equal(new_results2@port_costs_m_xts@data, updated_results2@port_costs_m_xts@data)
  expect_equal(new_results2@port_returns_m_xts@data, updated_results2@port_returns_m_xts@data)
  expect_equal(new_results2@port_stats_m_df@data, updated_results2@port_stats_m_df@data)
  expect_equal(new_results2@transactions_log@data, updated_results2@transactions_log@data)
  expect_equal(new_results2@stock_universe_m_df@data, updated_results2@stock_universe_m_df@data)
  expect_equal(new_results2@final_stock_port, updated_results2@final_stock_port)
})

test_that("run_port_backtest_internal runs a custom_weights portfolio with no score source", {

  #This is the shape the meta-portfolio backtest produces: weights are decided upstream and
  #pushed through to stocks, so there is no expected-return score anywhere in the run.
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- suppressMessages(create_meta_xts(benchmark_returns_m_xts))

  #An equal split over ten names quoted on every date, so the weights sum to one throughout
  n_dates <- length(unique(signals_m_df@data$dates))
  ticker_counts <- table(signals_m_df@data$tickers)
  held <- sort(names(ticker_counts)[ticker_counts == n_dates])[1:10]
  custom_stock_weights_m_df <- signals_m_df@data %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(weights = ifelse(tickers %in% held, 1 / length(held), 0)) %>%
    dplyr::arrange(id)

  results <- suppressWarnings(suppressMessages(run_port_backtest_internal(
    signals_m_df = signals_m_df@data,
    oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = NULL,
    rebalancing_months = c(1, 4), initial_buffer_period = 2,
    port_construction_method = "custom_weights",
    selected_benchmark = "ibov",
    mmaf_group_col = NULL,
    exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
    liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL,
    concentration_constraint_policy = NULL,
    liquidity_m_df = liquidity_m_df@data, main_liquidity_metric = "mean_volfin_3m",
    volatility_m_df = volatility_m_df@data, fwd_return_m_df = fwd_return_m_df@data,
    benchmark_weights_m_df = benchmark_weights_m_df@data,
    benchmark_returns_m_xts = benchmark_returns_m_xts@data,
    transaction_costs_parameters = list(direct_transaction_cost = 0.07, alpha = 1,
                                        lambda = "dynamic", strategy_aum = 25000),
    custom_stock_weights_m_df = custom_stock_weights_m_df,
    verbose = FALSE, parallel = FALSE
  )))

  expect_s4_class(results, "port_backtest_results")

  #Eligibility followed the supplied weights: exactly the held names, on every rebalance date
  universe <- results@stock_universe_m_df@data
  expect_equal(unique(tapply(universe$is_eligible, universe$dates, sum)), length(held),
               ignore_attr = TRUE)
  expect_setequal(unique(universe$tickers[universe$is_eligible == 1]), held)
  expect_equal(universe$pre_eligible_assets, universe$is_eligible)

  #No expected-return view was invented anywhere
  expect_true(all(is.na(universe$exp_ret_score)))
  stats <- results@port_stats_m_df@data
  expect_true(all(is.na(stats$act_exp_ret)))
  expect_true(all(is.na(stats$IR)))

  #while everything derived from realized returns and trades still works
  expect_true(any(is.finite(stats$info_ratio)))
  expect_true(any(results@port_costs_m_xts@data$total_cost > 0, na.rm = TRUE))
  expect_true(any(is.finite(results@port_returns_m_xts@data$net_return)))

  #and the portfolio actually holds the names it was told to hold
  held_weights <- results@port_weights_m_df@data %>% dplyr::filter(eop_port_weights > 0)
  expect_setequal(unique(held_weights$tickers), held)
})


# Meta portfolio backtest -------------------------------------------------
# Allocating across a cohort of already-backtested portfolios, dispatched on
# port_metabacktest_config through the same generic.

# Fixtures ----------------------------------------------------------------
# Building the cohort means running three real backtests, so it is built once and reused.

port_meta_cache <- new.env(parent = emptyenv())


port_meta_inputs <- function() {
  if (!is.null(port_meta_cache$inputs)) return(port_meta_cache$inputs)

  load(paste(testthat::test_path(), "/testdata/", "toy_preprocessed_port_obj.RData", sep = ""))

  inputs <- list(
    signals_m_df = create_meta_dataframe(signals_m_df, type = "signals"),
    fwd_return_m_df = create_meta_dataframe(fwd_return_m_df, type = "target"),
    liquidity_m_df = create_meta_dataframe(liquidity_m_df),
    volatility_m_df = create_meta_dataframe(volatility_m_df),
    benchmark_weights_m_df = create_meta_dataframe(benchmark_weights_m_df, type = "weights"),
    benchmark_returns_m_xts = suppressMessages(create_meta_xts(benchmark_returns_m_xts))
  )

  port_meta_cache$inputs <- inputs
  inputs
}


port_meta_cohort <- function() {
  if (!is.null(port_meta_cache$cohort)) return(port_meta_cache$cohort)

  inputs <- port_meta_inputs()

  build_config <- function(method, name) {
    create_port_backtest_config(
      chosen_score_metric_and_position = c(book_yield = "long"),
      eligibility_quantile_range = c(0.67, 1.0), selected_benchmark = "ibov",
      initial_buffer_period = 2, rebalancing_months = c(1, 4),
      port_construction_method = method, main_liquidity_metric = "mean_volfin_3m",
      config_name = name) %>%
      add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                       lambda = "dynamic", strategy_aum = 25000)
  }
  run_one <- function(config) suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))

  cohort <- suppressWarnings(suppressMessages(create_port_backtest_cohort(
    list(run_one(build_config("ew", "ew_by")),
         run_one(build_config("sw", "sw_by")),
         run_one(build_config("cw", "cw_by"))),
    cohort_name = "meta_test_cohort")))

  port_meta_cache$cohort <- cohort
  cohort
}


port_meta_config <- function(port_construction_method = "sw",
                                 meta_score = c(ann_info_ratio = "long"),
                                 eligibility_quantile_range = c(0, 1),
                                 initial_buffer_period = 4,
                                 active_returns = TRUE,
                                 config_name = "meta_test") {

  ## The toy sample runs seven months, so only a couple of portfolio returns exist by the first
  ## meta rebalance. That is fewer observations than portfolios, which the validator warns about;
  ## the tests suppress it deliberately and one asserts it.
  inner <- create_port_backtest_config(
    chosen_score_metric_and_position = meta_score,
    eligibility_quantile_range = eligibility_quantile_range,
    initial_buffer_period = initial_buffer_period, rebalancing_months = c(1, 4),
    selected_benchmark = "ibov",
    cov_est_method = create_cov_est_method(
      cov_estimation_method = "sample", cov_matrix_sample_size = 2,
      active_returns = active_returns, cov_matrix_benchmark = "ibov"),
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = port_construction_method,
    config_name = config_name) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  suppressMessages(create_port_metabacktest_config(inner, config_name = config_name,
                                                   verbose = FALSE))
}


run_port_meta_backtest <- function(config = port_meta_config(), cohort = port_meta_cohort()) {
  inputs <- port_meta_inputs()
  suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, port_backtest_cohort = cohort,
    benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))
}


# Standard behaviour ------------------------------------------------------

testthat::test_that("run_port_backtest dispatches on port_metabacktest_config and fills every slot", {
  results <- run_port_meta_backtest()

  testthat::expect_s4_class(results, "port_metabacktest_results")
  testthat::expect_s4_class(results@meta_port_backtest_results, "port_backtest_results")
  testthat::expect_s4_class(results@port_backtest_cohort, "port_backtest_cohort")
  testthat::expect_s4_class(results@port_universe_m_df, "port_universe_m_df")
  testthat::expect_s4_class(results@meta_port_weights_m_df, "meta_dataframe")
  testthat::expect_s4_class(results@projected_stock_weights_m_df, "meta_dataframe")
  testthat::expect_s4_class(results@meta_port_stats_m_df, "meta_dataframe")
  testthat::expect_s4_class(results@final_meta_port, "port")

  ## The identifier names the allocation rule and the cohort it ran over
  testthat::expect_equal(results@backtest_identifier, "mc__meta_test_ch__meta_test_cohort")
})

testthat::test_that("meta weights are set on the configured schedule and sum to one", {
  results <- run_port_meta_backtest()
  meta_weights <- results@meta_port_weights_m_df@data

  ## Buffer 4 into a seven-date sample, rebalancing in January and April
  inputs <- port_meta_inputs()
  all_dates <- sort(unique(inputs$signals_m_df@data$dates))
  dates_backtest <- all_dates[4:length(all_dates)]
  expected_dates <- sort(unique(c(
    min(dates_backtest),
    dates_backtest[lubridate::month(dates_backtest) %in% c(1, 4)])))

  testthat::expect_setequal(unique(meta_weights$dates), expected_dates)

  sums <- tapply(meta_weights$weights, meta_weights$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(expected_dates)), tolerance = 1e-10)

  ## Every base portfolio is named, and signal weighting spreads them unevenly
  backtest_ids <- vapply(port_meta_cohort()@port_backtest_results_list,
                         function(x) x@backtest_identifier, character(1))
  testthat::expect_setequal(unique(meta_weights$tickers), unname(backtest_ids))
  testthat::expect_gt(stats::sd(meta_weights$weights[meta_weights$dates == expected_dates[1]]), 0)
})

testthat::test_that("the projected stock weights satisfy the panel contract", {
  results <- run_port_meta_backtest()
  projected <- results@projected_stock_weights_m_df@data
  inputs <- port_meta_inputs()

  sums <- tapply(projected$weights, projected$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-10)
  testthat::expect_true(all(inputs$signals_m_df@data$id %in% projected$id))
  testthat::expect_true(all(projected$weights >= 0 & projected$weights <= 1))
})


# The economic check ------------------------------------------------------

testthat::test_that("allocating everything to one base portfolio reproduces its own weights", {
  ## The strongest end-to-end check available: a quantile range that admits only the
  ## top-scoring portfolio, weighted equally, must project to exactly that portfolio's weights.
  ## If any step of the chain crossed portfolios or mis-scaled, this fails.
  concentrated <- port_meta_config(port_construction_method = "ew",
                                       eligibility_quantile_range = c(0.9, 1.0))
  results <- run_port_meta_backtest(config = concentrated)

  meta_weights <- results@meta_port_weights_m_df@data
  projected <- results@projected_stock_weights_m_df@data
  base_weights <- port_meta_cohort()@port_weights_m_df@data

  for (current_date in sort(unique(meta_weights$dates))) {
    date_weights <- meta_weights %>% dplyr::filter(dates == current_date)

    ## Exactly one portfolio was funded
    funded <- date_weights$tickers[date_weights$weights > 0]
    testthat::expect_length(funded, 1L)
    testthat::expect_equal(date_weights$weights[date_weights$weights > 0], 1, tolerance = 1e-10)

    ## and the projection is that portfolio's own weight vector, untouched
    expected <- base_weights %>%
      dplyr::filter(dates == current_date) %>%
      dplyr::select(id, expected = dplyr::all_of(funded))
    got <- projected %>%
      dplyr::filter(dates == current_date) %>%
      dplyr::select(id, weights)
    joined <- dplyr::inner_join(expected, got, by = "id")

    testthat::expect_equal(nrow(joined), nrow(expected))
    testthat::expect_equal(joined$weights, joined$expected, tolerance = 1e-12)
  }
})


# The stock-level result --------------------------------------------------

testthat::test_that("the stock-level backtest reports real returns and costs but no return view", {
  results <- run_port_meta_backtest()
  inner <- results@meta_port_backtest_results

  ## Trades were priced and returns were earned
  testthat::expect_true(any(is.finite(inner@port_returns_m_xts@data$net_return)))
  testthat::expect_true(any(inner@port_costs_m_xts@data$total_cost > 0, na.rm = TRUE))

  ## The weights were supplied, so there is no expected-return view at this level
  inner_stats <- inner@port_stats_m_df@data
  testthat::expect_true(all(is.na(inner_stats$act_exp_ret)))
  testthat::expect_true(all(is.na(inner_stats$IR)))
  testthat::expect_true(all(is.na(inner@stock_universe_m_df@data$exp_ret_score)))

  ## while everything measured from realized returns still works
  testthat::expect_true(any(is.finite(inner_stats$info_ratio)))
})

testthat::test_that("the expected-return view lives at the meta level instead", {
  results <- run_port_meta_backtest()
  meta_stats <- results@meta_port_stats_m_df@data

  testthat::expect_true(any(is.finite(meta_stats$exp_ret)))
  testthat::expect_equal(unique(meta_stats$tickers), "meta_port")
  testthat::expect_setequal(meta_stats$dates,
                            unique(results@meta_port_weights_m_df@data$dates))
})


# The covariance basis ----------------------------------------------------

testthat::test_that("meta-level risk is absolute whatever the configured covariance basis", {
  ## calculate_port_stats() re-estimates its own covariance with active returns switched off, so
  ## cov_est_method@active_returns reaches only the weights of the covariance-based methods and
  ## never the reported analytics. Under signal weighting it therefore changes nothing at all.
  active <- run_port_meta_backtest(config = port_meta_config(active_returns = TRUE))
  absolute <- run_port_meta_backtest(config = port_meta_config(active_returns = FALSE))

  testthat::expect_equal(active@meta_port_stats_m_df@data$risk,
                         absolute@meta_port_stats_m_df@data$risk)
  testthat::expect_equal(active@meta_port_weights_m_df@data$weights,
                         absolute@meta_port_weights_m_df@data$weights)

  ## and the figure is genuinely populated rather than trivially equal through being missing
  testthat::expect_true(any(is.finite(active@meta_port_stats_m_df@data$risk)))
})


# Validation propagates ---------------------------------------------------

testthat::test_that("inconsistent inputs are caught before anything is run", {
  inputs <- port_meta_inputs()

  ## A meta score no column of the derived universe carries
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_port_meta_backtest(
      config = port_meta_config(meta_score = c(nonsense = "long"))))),
    "is not a column of the derived port_universe_m_df"
  )

  ## A buffer that puts the first meta rebalance where no realized statistic exists yet
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_port_meta_backtest(
      config = port_meta_config(initial_buffer_period = 2)))),
    "missing at"
  )

  ## Data the cohort was not built from
  wrong_signals <- inputs$signals_m_df
  wrong_signals@meta_dataframe_name <- "some_other_signals"
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_port_backtest(
      signals_m_df = wrong_signals, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = port_meta_config(), port_backtest_cohort = port_meta_cohort(),
      verbose = FALSE, parallel = FALSE))),
    "Object name mismatch"
  )

  ## A cohort is not optional
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_port_backtest(
      signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = port_meta_config(), verbose = FALSE, parallel = FALSE))),
    "port_backtest_cohort must be provided"
  )
})

test_that("the meta backtest reproduces a step-by-step reconstruction of its own chain", {

  #Reconstructing the chain independently, in the style of the tests above: derive the universe,
  #score it, classify it, set the weights, project them, and compare each stage against what the
  #method produced. A wrong argument anywhere in the wiring shows up here.
  results <- run_port_meta_backtest()
  cohort <- port_meta_cohort()
  config <- port_meta_config()
  inner_config <- config@meta_port_backtest_config
  inputs <- port_meta_inputs()

  #Stage 1: the port universe
  expected_universe <- suppressWarnings(suppressMessages(derive_port_universe_m_df(
    port_backtest_cohort = cohort,
    return_basis = config@return_basis,
    cost_lookback = config@cost_lookback,
    verbose = FALSE
  )))
  expect_equal(results@port_universe_m_df@data, expected_universe@data)

  #Stage 2: the meta weights, rebuilt one rebalance date at a time
  base_returns_xts <- cohort@port_returns_m_xts_list$net_returns_m_xts@data
  base_portfolio_names <- sort(unique(expected_universe@data$tickers))
  bench_returns_xts <- base_returns_xts[, "selected_bench_return", drop = FALSE]
  base_returns_xts <- base_returns_xts[, base_portfolio_names, drop = FALSE]
  cov_est_method <- inner_config@cov_est_method

  meta_rebalance_dates <- sort(unique(results@meta_port_weights_m_df@data$dates))
  reconstructed_weights <- list()

  for (i in seq_along(meta_rebalance_dates)) {

    current_date <- meta_rebalance_dates[i]

    universe_m_d_ref <- expected_universe@data %>% dplyr::filter(dates == current_date)

    scored_m_d_ref <- derive_stock_universe_m_d_ref(
      signals_m_d_ref = universe_m_d_ref,
      oos_predictions_m_d_ref = NULL,
      chosen_score_metric_and_position = inner_config@chosen_score_metric_and_position,
      chosen_scaler = inner_config@chosen_scaler,
      scaler_m_d_ref = NULL,
      scaler_shrinkage = if (is.null(inner_config@scaler_shrinkage)) 0 else inner_config@scaler_shrinkage,
      lower_quantile_winsorization = 0.025,
      upper_quantile_winsorization = 0.975
    )

    classified_m_d_ref <- classify_investment_universe(
      universe_m_d_ref = scored_m_d_ref,
      eligibility_quantile_range = inner_config@eligibility_quantile_range,
      min_eligible_assets_fallback = inner_config@min_eligible_assets_fallback,
      use_raw_for_eligibility = FALSE,
      asset_object = "stocks",
      verbose = FALSE
    )

    #Point-in-time return sample, exactly as the method subsets it
    returns_upd_ref <- base_returns_xts[zoo::index(base_returns_xts) <= current_date, , drop = FALSE]
    bench_upd_ref <- bench_returns_xts[zoo::index(bench_returns_xts) <= current_date, , drop = FALSE]

    meta_port <- suppressWarnings(suppressMessages(set_portfolio_weights(
      universe_m_d_ref = classified_m_d_ref,
      port_construction_method = inner_config@port_construction_method,
      covariance_matrix = NULL,
      eligible_returns_m_xts_upd_ref = returns_upd_ref,
      selected_benchmark_m_xts_upd_ref = bench_upd_ref,
      active_returns = cov_est_method@active_returns,
      cov_estimation_method = cov_est_method@cov_estimation_method,
      cov_matrix_sample_size = cov_est_method@cov_matrix_sample_size,
      top_down_proxy_port_method = "ew", mmaf_group_col = NULL,
      selected_benchmark = NULL,
      lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
      parallel = FALSE, verbose = FALSE
    )))

    reconstructed_weights[[i]] <- meta_port@universe_m_d_ref@data %>%
      dplyr::select(id, tickers, dates, weights)
  }

  reconstructed_weights <- do.call(rbind, reconstructed_weights) %>%
    dplyr::arrange(id) %>%
    as.data.frame()
  rownames(reconstructed_weights) <- NULL

  expect_equal(results@meta_port_weights_m_df@data, reconstructed_weights)

  #The reconstruction must be a real allocation, not a degenerate one that would match trivially
  expect_gt(stats::sd(reconstructed_weights$weights), 0)

  #Stage 3: the projection onto stocks
  expected_projection <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = reconstructed_weights,
    port_backtest_cohort = cohort,
    signals_m_df = inputs$signals_m_df,
    verbose = FALSE
  ))
  expect_equal(results@projected_stock_weights_m_df@data, expected_projection@data)

  #Stage 4: the stock-level backtest run on those projected weights
  expected_inner <- suppressWarnings(suppressMessages(run_port_backtest_internal(
    signals_m_df = inputs$signals_m_df@data,
    oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = NULL,
    rebalancing_months = inner_config@rebalancing_months,
    initial_buffer_period = inner_config@initial_buffer_period,
    port_construction_method = "custom_weights",
    selected_benchmark = inner_config@selected_benchmark,
    eligibility_quantile_range = inner_config@eligibility_quantile_range,
    exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL, mmaf_group_col = NULL,
    cov_estimation_method = cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = cov_est_method@cov_matrix_sample_size,
    active_returns = cov_est_method@active_returns,
    cov_matrix_benchmark = cov_est_method@cov_matrix_benchmark,
    daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts@data,
    liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL,
    concentration_constraint_policy = NULL,
    liquidity_m_df = inputs$liquidity_m_df@data,
    main_liquidity_metric = inner_config@main_liquidity_metric,
    liquidity_floor_cutoffs = inner_config@liquidity_floor_cutoffs,
    volatility_m_df = inputs$volatility_m_df@data,
    fwd_return_m_df = inputs$fwd_return_m_df@data,
    stock_groups_m_df = NULL,
    benchmark_weights_m_df = inputs$benchmark_weights_m_df@data,
    transaction_costs_parameters = as.list(inner_config@transaction_costs_parameters),
    custom_stock_weights_m_df = expected_projection@data,
    custom_stock_metrics_m_df = NULL,
    lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
    verbose = FALSE, parallel = FALSE
  )))

  expect_equal(results@meta_port_backtest_results@port_weights_m_df@data,
               expected_inner@port_weights_m_df@data)
  expect_equal(results@meta_port_backtest_results@port_returns_m_xts@data,
               expected_inner@port_returns_m_xts@data)
  expect_equal(results@meta_port_backtest_results@port_costs_m_xts@data,
               expected_inner@port_costs_m_xts@data)
  expect_equal(results@meta_port_backtest_results@port_stats_m_df@data,
               expected_inner@port_stats_m_df@data)

  #and the returns are real numbers rather than an all-NA series matching itself
  expect_true(any(is.finite(expected_inner@port_returns_m_xts@data$net_return)))
})


# A custom_weights portfolio through the public method --------------------

test_that("run_port_backtest accepts a custom_weights config directly", {

  #port_backtest_config refused this value until the engine could reach the path, so the public
  #method had never run with one. Everything below the config was already exercised through
  #run_port_backtest_internal; this covers the method that assembles its arguments.
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- suppressMessages(create_meta_xts(benchmark_returns_m_xts))

  port_config <- create_port_backtest_config(
    chosen_score_metric_and_position = NULL,
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = 2, rebalancing_months = c(1, 4),
    selected_benchmark = "ibov",
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "custom_weights",
    config_name = "cw_direct") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  expect_equal(port_config@port_construction_method, "custom_weights")

  #A score alongside supplied weights is contradictory. That check existed but was unreachable
  #behind the blanket refusal, so it is exercised here for the first time.
  expect_error(
    create_port_backtest_config(
      chosen_score_metric_and_position = c(book_yield = "long"),
      eligibility_quantile_range = c(0, 1),
      initial_buffer_period = 2, rebalancing_months = c(1, 4),
      main_liquidity_metric = "mean_volfin_3m",
      port_construction_method = "custom_weights", config_name = "bad"),
    "must be NULL when port_construction_method is custom_weights"
  )

  #An equal split over ten names quoted on every date
  n_dates <- length(unique(signals_m_df@data$dates))
  ticker_counts <- table(signals_m_df@data$tickers)
  held <- sort(names(ticker_counts)[ticker_counts == n_dates])[1:10]
  custom_stock_weights_m_df <- signals_m_df@data %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(weights = ifelse(tickers %in% held, 1 / length(held), 0)) %>%
    dplyr::arrange(id) %>%
    create_meta_dataframe(meta_dataframe_name = "cw", type = "weights")

  results <- suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = signals_m_df, fwd_return_m_df = fwd_return_m_df,
    liquidity_m_df = liquidity_m_df, volatility_m_df = volatility_m_df,
    config = port_config,
    benchmark_weights_m_df = benchmark_weights_m_df,
    benchmark_returns_m_xts = benchmark_returns_m_xts,
    custom_stock_weights_m_df = custom_stock_weights_m_df,
    verbose = FALSE, parallel = FALSE)))

  expect_s4_class(results, "port_backtest_results")

  universe <- results@stock_universe_m_df@data
  expect_equal(unique(tapply(universe$is_eligible, universe$dates, sum)), length(held),
               ignore_attr = TRUE)
  expect_true(all(is.na(universe$exp_ret_score)))

  weights <- results@port_weights_m_df@data %>% dplyr::filter(eop_port_weights > 0)
  expect_setequal(unique(weights$tickers), held)
  expect_true(any(is.finite(results@port_returns_m_xts@data$net_return)))
})


# The risk-targeted meta backtest -----------------------------------------

risk_targeted_cache <- new.env(parent = emptyenv())
risk_targeted_residual <- "BOVA11"


risk_targeted_inputs <- function() {
  if (!is.null(risk_targeted_cache$inputs)) return(risk_targeted_cache$inputs)

  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #The residual replicates the benchmark rather than standing in for cash, which is the whole
  #point of a tracking-error target. An index-tracking residual makes tracking error scale
  #linearly toward zero as the sleeve is cut. A constant-return residual has a tracking error
  #equal to the benchmark's own volatility, so blending toward it turns the portfolio into a large
  #underweight of the market and raises tracking error instead of lowering it.
  add_residual <- function(df, values) {
    own_dates <- sort(unique(df$dates))
    extra <- data.frame(id = paste0(risk_targeted_residual, "-", own_dates),
                        tickers = risk_targeted_residual, dates = own_dates,
                        stringsAsFactors = FALSE)
    for (nm in setdiff(names(df), names(extra))) {
      extra[[nm]] <- if (!is.null(values[[nm]])) values[[nm]] else values[["default"]]
    }
    out <- rbind(df, extra[, names(df)])
    out[order(out$id), ]
  }

  #The daily index, built from the benchmark weights applied to the daily stock returns point in
  #time, so the daily series is consistent with the panel the covariance is estimated from rather
  #than an unrelated fabrication
  daily_dates <- zoo::index(daily_stock_returns_m_xts)
  daily_matrix <- as.matrix(daily_stock_returns_m_xts)
  bench_dates <- sort(unique(benchmark_weights_m_df$dates))
  bench_by_date <- split(benchmark_weights_m_df, as.character(benchmark_weights_m_df$dates))

  daily_index_return <- vapply(seq_along(daily_dates), function(i) {
    applicable <- bench_dates[bench_dates <= daily_dates[i]]
    reference <- if (length(applicable) == 0) bench_dates[1] else max(applicable)
    weights_df <- bench_by_date[[as.character(reference)]]
    weights_df <- weights_df[is.finite(weights_df$ibov) & weights_df$ibov > 0, , drop = FALSE]
    held <- intersect(weights_df$tickers, colnames(daily_matrix))
    if (length(held) == 0) return(0)
    w <- weights_df$ibov[match(held, weights_df$tickers)]
    r <- daily_matrix[i, held]
    ok <- is.finite(r) & is.finite(w)
    if (!any(ok)) return(0)
    sum(w[ok] * r[ok]) / sum(w[ok])
  }, numeric(1))

  #A replication is never perfect, and the tracking difference is not cosmetic: a residual that
  #tracks exactly has zero variance in active space, which makes the active-return correlation
  #matrix singular
  set.seed(20260826)
  residual_daily <- daily_index_return + stats::rnorm(length(daily_index_return), 0, 0.05)

  daily_aug <- cbind(daily_stock_returns_m_xts, BOVA11 = residual_daily)
  daily_bench <- xts::xts(data.frame(
    ibov = daily_index_return,
    smll = daily_index_return + stats::rnorm(length(daily_index_return), 0, 0.2),
    idiv = daily_index_return + stats::rnorm(length(daily_index_return), 0, 0.2)
  ), order.by = daily_dates)

  #fwd_return_1m at date t is the return from t to t+1, and run_port_backtest reads the benchmark
  #return at t+1 to form the active return, so the residual's forward return has to be the
  #benchmark's return one date ahead for the two to cancel. Its last date has no forward return
  #either, exactly as the stocks do not.
  monthly_dates <- sort(unique(fwd_return_m_df$dates))
  ibov_monthly <- stats::setNames(as.numeric(benchmark_returns_m_xts[, "ibov"]),
                                  as.character(zoo::index(benchmark_returns_m_xts)))
  residual_fwd <- vapply(monthly_dates, function(d) {
    later <- monthly_dates[monthly_dates > d]
    if (length(later) == 0) return(NA_real_)
    unname(ibov_monthly[as.character(min(later))])
  }, numeric(1))
  residual_fwd <- residual_fwd + stats::rnorm(length(residual_fwd), 0, 0.05)

  fwd_aug <- add_residual(fwd_return_m_df, list(default = 0))
  is_residual <- fwd_aug$tickers == risk_targeted_residual
  fwd_aug$fwd_return_1m[is_residual] <-
    residual_fwd[match(fwd_aug$dates[is_residual], monthly_dates)]

  #An index ETF is not a member of a stock sector, and these are character columns, so writing a
  #number into them put the residual in a phantom sector named "1". It gets its own label instead.
  groups_aug <- add_residual(stock_groups_m_df, list(default = "Index ETF"))

  #Comparable to the most liquid names in the panel rather than orders of magnitude beyond them.
  #presence is a percentage, so 100 is its ceiling, not 1e9.
  liquidity_aug <- add_residual(liquidity_m_df, list(
    default = max(liquidity_m_df$mean_volfin_3m, na.rm = TRUE),
    presence = 100))

  #Its volatility is the index's, since it replicates the index
  residual_daily_vol <- stats::sd(residual_daily, na.rm = TRUE)
  volatility_aug <- add_residual(volatility_m_df, list(
    default = residual_daily_vol * sqrt(252),
    daily_vol = residual_daily_vol))

  #The residual must never be picked up by the risky sleeve's own selection, so it sits at the
  #floor of every signal rather than at zero, which could rank it mid-panel
  signal_cols <- setdiff(names(signals_m_df), c("id", "tickers", "dates"))
  signal_floors <- lapply(signal_cols, function(nm) {
    column <- signals_m_df[[nm]]
    if (is.numeric(column)) min(column, na.rm = TRUE) else column[1]
  })
  names(signal_floors) <- signal_cols
  signal_floors$default <- 0

  inputs <- list(
    signals_m_df = create_meta_dataframe(add_residual(signals_m_df, signal_floors),
                                         type = "signals"),
    fwd_return_m_df = create_meta_dataframe(fwd_aug, type = "target"),
    liquidity_m_df = create_meta_dataframe(liquidity_aug),
    volatility_m_df = create_meta_dataframe(volatility_aug),
    benchmark_weights_m_df = create_meta_dataframe(
      add_residual(benchmark_weights_m_df, list(default = 0)), type = "weights"),
    benchmark_returns_m_xts = suppressMessages(create_meta_xts(benchmark_returns_m_xts)),
    stock_groups_m_df = create_meta_dataframe(groups_aug, type = "groups"),
    daily_stock_returns_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      daily_aug, type = "returns", asset_type = "stocks", meta_xts_name = "B3"))),
    daily_bench_returns_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      daily_bench, type = "returns", asset_type = "benchmark", meta_xts_name = "B3")))
  )

  risk_targeted_cache$inputs <- inputs
  inputs
}


risk_targeted_cohort <- function() {
  if (!is.null(risk_targeted_cache$cohort)) return(risk_targeted_cache$cohort)

  inputs <- risk_targeted_inputs()
  config <- create_port_backtest_config(
    chosen_score_metric_and_position = c(book_yield = "long"),
    eligibility_quantile_range = c(0.67, 1.0), selected_benchmark = "ibov",
    initial_buffer_period = 2, rebalancing_months = c(1, 4),
    port_construction_method = "sw", main_liquidity_metric = "mean_volfin_3m",
    config_name = "risky") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  risky <- suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))

  cohort <- suppressWarnings(suppressMessages(
    create_port_backtest_cohort(list(risky), cohort_name = "risk_targeted_cohort")))

  risk_targeted_cache$cohort <- cohort
  cohort
}


risk_targeted_config <- function(target = 10, p = 1, min_weight = 0.2, max_weight = 1,
                                 exposure_method = "none", exposure_window = NULL,
                                 exposure_center = 1, exposure_sensitivity = NULL,
                                 exposure_bounds = c(0, 1)) {
  inner <- create_port_backtest_config(
    chosen_score_metric_and_position = NULL,
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = 4, rebalancing_months = c(1, 4),
    selected_benchmark = "ibov",
    cov_est_method = create_cov_est_method("sample", 2, TRUE, "ibov"),
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "custom_weights", config_name = "te_managed") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  suppressMessages(create_port_metabacktest_config(
    inner, type = "risk_targeted", config_name = "te_managed",
    #The toy daily panel runs 228 days, so the stock-level analytics window has to fit inside it
    stock_cov_matrix_sample_size = 60, verbose = FALSE)) %>%
    add_risk_target_parameters(
      residual_ticker = risk_targeted_residual, target = target,
      target_metric = "tracking_error", p = p,
      min_weight = min_weight, max_weight = max_weight,
      exposure_method = exposure_method, exposure_window = exposure_window,
      exposure_center = exposure_center, exposure_sensitivity = exposure_sensitivity,
      exposure_bounds = exposure_bounds,
      vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL))
}


#A metric on the sleeve whose sign flips between the two meta rebalance dates, so a trend rule
#has something to react to rather than sitting on one side throughout
risk_targeted_exposure_metric <- function(values = NULL) {
  own_dates <- sort(unique(risk_targeted_inputs()$signals_m_df@data$dates))
  if (is.null(values)) values <- ifelse(own_dates < as.Date("2023-03-01"), 5, -5)
  ##Named for the sleeve it leans on. derive_exposure_signal() now requires that, since carrying
  ##one ticker is not the same as carrying the right one.
  sleeve_name <- risk_targeted_cohort()@port_backtest_results_list[[1]]@backtest_identifier
  df <- data.frame(id = paste0(sleeve_name, "-", own_dates), tickers = sleeve_name,
                   dates = own_dates,
                   trailing_return = values, stringsAsFactors = FALSE)
  df
}


run_risk_targeted <- function(config = risk_targeted_config(), exposure_m_df = NULL) {
  inputs <- risk_targeted_inputs()
  suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, port_backtest_cohort = risk_targeted_cohort(),
    benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = inputs$daily_bench_returns_m_xts,
    stock_groups_m_df = inputs$stock_groups_m_df,
    exposure_m_df = exposure_m_df,
    verbose = FALSE, parallel = FALSE)))
}


test_that("a risk-targeted meta backtest returns the risk-targeted subclass with both levels filled", {
  results <- run_risk_targeted()

  expect_s4_class(results, "risk_target_metabacktest_results")
  expect_true(is(results, "port_metabacktest_results"))
  expect_equal(results@residual_ticker, risk_targeted_residual)
  expect_s4_class(results@risk_target_parameters, "risk_target_parameters")
  expect_s4_class(results@meta_port_backtest_results, "port_backtest_results")
})

test_that("the weight on the risky sleeve is the target over its estimated risk", {
  results <- run_risk_targeted()
  meta_stats <- results@meta_port_stats_m_df@data

  #The rule itself, reproduced from the reported risk
  expect_equal(meta_stats$risky_weight,
               pmin(pmax(10 / meta_stats$sleeve_risk, 0.2), 1), tolerance = 1e-10)

  #implied_risk equals the target wherever the bounds did not bind
  unclipped <- meta_stats$risky_weight > 0.2 & meta_stats$risky_weight < 1
  expect_true(any(unclipped))
  expect_equal(meta_stats$implied_risk[unclipped],
               rep(10, sum(unclipped)), tolerance = 1e-10)
})

test_that("the reported risk matches an independent estimate at each rebalance date", {
  results <- run_risk_targeted()
  inputs <- risk_targeted_inputs()
  cohort <- risk_targeted_cohort()
  meta_stats <- results@meta_port_stats_m_df@data
  risk_target_params <- results@risk_target_parameters

  for (i in seq_len(nrow(meta_stats))) {
    current_date <- meta_stats$dates[i]
    expected <- suppressWarnings(suppressMessages(estimate_sleeve_risk(
      current_date = current_date,
      risk_target_params = risk_target_params,
      risky_port_backtest_results = cohort@port_backtest_results_list[[1]],
      daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts@data,
      selected_benchmark = "ibov",
      stock_groups_m_d_ref = inputs$stock_groups_m_df@data %>%
        dplyr::filter(as.Date(dates) == current_date),
      return_basis = "net")))
    expect_equal(meta_stats$sleeve_risk[i], expected, tolerance = 1e-10)
  }
})

test_that("a higher exponent de-risks harder for the same estimated risk", {
  linear <- run_risk_targeted(risk_targeted_config(p = 1))
  quadratic <- run_risk_targeted(risk_targeted_config(p = 2))

  linear_stats <- linear@meta_port_stats_m_df@data
  quadratic_stats <- quadratic@meta_port_stats_m_df@data

  #The risk estimate is the same; only the response to it changes
  expect_equal(linear_stats$sleeve_risk, quadratic_stats$sleeve_risk)
  expect_true(all(quadratic_stats$risky_weight <= linear_stats$risky_weight))
  expect_true(any(quadratic_stats$risky_weight < linear_stats$risky_weight))
})

test_that("the bounds clip the weight and the residual absorbs the rest", {
  #A target far below the sleeve's own risk would ask for a tiny position; the floor stops it
  floored <- run_risk_targeted(risk_targeted_config(target = 1, min_weight = 0.6))
  meta_stats <- floored@meta_port_stats_m_df@data
  expect_true(all(meta_stats$risky_weight == 0.6))

  meta_weights <- floored@meta_port_weights_m_df@data
  residual <- meta_weights %>% dplyr::filter(tickers == risk_targeted_residual)
  expect_true(all(abs(residual$weights - 0.4) < 1e-10))
})

test_that("the projected weights carry the residual and stay fully invested", {
  results <- run_risk_targeted()
  inputs <- risk_targeted_inputs()
  projected <- results@projected_stock_weights_m_df@data

  sums <- tapply(projected$weights, projected$dates, sum)
  expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-10)
  expect_true(all(inputs$signals_m_df@data$id %in% projected$id))
  expect_true(all(projected$weights >= 0 & projected$weights <= 1))

  #The residual's projected weight is exactly one minus the risky weight on rebalance dates
  meta_stats <- results@meta_port_stats_m_df@data
  for (i in seq_len(nrow(meta_stats))) {
    residual_row <- projected %>%
      dplyr::filter(tickers == risk_targeted_residual, dates == meta_stats$dates[i])
    expect_equal(residual_row$weights, 1 - meta_stats$risky_weight[i], tolerance = 1e-10)
  }
})

test_that("the residual is actually held and traded in the stock-level run", {
  results <- run_risk_targeted()
  inner <- results@meta_port_backtest_results

  held <- inner@port_weights_m_df@data %>%
    dplyr::filter(tickers == risk_targeted_residual, eop_port_weights > 0)
  expect_gt(nrow(held), 0L)

  #Priced like any other position, so the run produces real returns and costs
  expect_true(any(is.finite(inner@port_returns_m_xts@data$net_return)))
  expect_true(any(inner@port_costs_m_xts@data$total_cost > 0, na.rm = TRUE))
  residual_transactions <- do.call(rbind, inner@transactions_log@data)
  expect_gt(nrow(residual_transactions %>%
                     dplyr::filter(tickers == risk_targeted_residual, eop_port_weights > 0)),
            0)
  expect_gt(nrow(residual_transactions %>%
                   dplyr::filter(tickers == risk_targeted_residual, total_cost > 0)),
            0)
  expect_gt(nrow(residual_transactions %>%
                   dplyr::filter(tickers == risk_targeted_residual, order > 0)),
            0)


  #and no expected-return view was invented at stock level
  expect_true(all(is.na(inner@stock_universe_m_df@data$exp_ret_score)))
  expect_gt(nrow(inner@stock_universe_m_df@data %>%
                   dplyr::filter(tickers == risk_targeted_residual)),
            0)

  residual_pos <- which(rownames(inner@final_stock_port@covariance_matrix) == risk_targeted_residual)
  expect_true(residual_pos > 0)
  expect_equal(stats::cov2cor(inner@final_stock_port@covariance_matrix)[residual_pos,residual_pos],
               1)

  #Groups
  expect_false(is.na(
    inner@final_stock_universe_m_d_ref@data %>%
      dplyr::filter(tickers == risk_targeted_residual) %>%
      dplyr::pull(sectors)
  ))

  #Bench weight
  expect_equal(
    inner@final_stock_universe_m_d_ref@data %>%
      dplyr::filter(tickers == risk_targeted_residual) %>%
      dplyr::pull(ibov_bench_weights),
    0
  )

  #TE. Compared on the active return series rather than on ann_track_err from port_stats_m_df:
  #that column is a rolling figure over each portfolio's own return history, and the sleeve's
  #history starts three months before the meta portfolio's, so reading both at a shared date
  #compares different windows. It also carries one row per return basis, so pulling the column
  #without filtering mixes net and raw.
  meta_returns <- inner@port_returns_m_xts@data
  sleeve_returns <-
    results@port_backtest_cohort@port_backtest_results_list[[1]]@port_returns_m_xts@data
  shared_dates <- intersect(as.character(zoo::index(meta_returns)),
                            as.character(zoo::index(sleeve_returns)))
  expect_gt(length(shared_dates), 1)

  meta_active <- as.numeric(
    meta_returns[as.character(zoo::index(meta_returns)) %in% shared_dates, "net_active_return"])
  sleeve_active <- as.numeric(
    sleeve_returns[as.character(zoo::index(sleeve_returns)) %in% shared_dates,
                   "net_active_return"])

  ##Holding less of the sleeve has to bring the portfolio closer to the benchmark
  expect_lt(stats::sd(meta_active, na.rm = TRUE), stats::sd(sleeve_active, na.rm = TRUE))
  expect_lt(mean(abs(meta_active), na.rm = TRUE), mean(abs(sleeve_active), na.rm = TRUE))


})

test_that("a risk-targeted config is refused when its inputs do not support it", {
  inputs <- risk_targeted_inputs()

  #ex_ante estimation has nothing to work from without daily returns
  expect_error(
    suppressWarnings(suppressMessages(run_port_backtest(
      signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = risk_targeted_config(), port_backtest_cohort = risk_targeted_cohort(),
      benchmark_weights_m_df = inputs$benchmark_weights_m_df,
      benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
      stock_groups_m_df = inputs$stock_groups_m_df,
      verbose = FALSE, parallel = FALSE))),
    "daily_stock_returns_m_xts must be supplied"
  )

  #A residual that is not tradable in the stock universe
  absent <- suppressMessages(create_port_metabacktest_config(
    risk_targeted_config()@meta_port_backtest_config, type = "risk_targeted",
    config_name = "absent", stock_cov_matrix_sample_size = 60, verbose = FALSE)) %>%
    add_risk_target_parameters(residual_ticker = "NOT_A_STOCK", target = 10,
                       vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL))

  expect_error(
    suppressWarnings(suppressMessages(run_port_backtest(
      signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = absent, port_backtest_cohort = risk_targeted_cohort(),
      benchmark_weights_m_df = inputs$benchmark_weights_m_df,
      benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
      daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = inputs$daily_bench_returns_m_xts,
      stock_groups_m_df = inputs$stock_groups_m_df,
      verbose = FALSE, parallel = FALSE))),
    "not a row of signals_m_df"
  )
})


# The exposure signal end to end ------------------------------------------

test_that("the reported weight is the exposure times the risk ratio, reconstructed", {

  #The whole rule, rebuilt from its two halves and compared against what the run produced.
  #A wrong sign, a mismatched date, or the exposure being dropped would all show up here.
  config <- risk_targeted_config(exposure_method = "trend", exposure_center = 0.75,
                                 exposure_sensitivity = 0.25)
  exposure_metric <- risk_targeted_exposure_metric()
  results <- run_risk_targeted(config, exposure_m_df = exposure_metric)

  meta_stats <- results@meta_port_stats_m_df@data
  risk_target_params <- results@risk_target_parameters

  expected_exposure <- suppressMessages(derive_exposure_signal(
    metric_m_df = exposure_metric, method = "trend",
    center = 0.75, sensitivity = 0.25, verbose = FALSE))

  for (i in seq_len(nrow(meta_stats))) {
    current_date <- meta_stats$dates[i]
    exposure <- expected_exposure$exposure[expected_exposure$dates == current_date]

    expect_length(exposure, 1L)
    expect_equal(meta_stats$exposure[i], exposure, tolerance = 1e-12)
    expect_equal(meta_stats$risky_weight[i],
                 risk_to_weight(meta_stats$sleeve_risk[i], risk_target_params, exposure),
                 tolerance = 1e-12)
    expect_equal(meta_stats$risky_weight[i],
                 risk_target_params@target/meta_stats$sleeve_risk[i]*meta_stats$exposure[i],
                 tolerance = 1e-12)
  }
})

test_that("a trend that flips actually moves the allocation", {

  #The metric is positive at the first rebalance and negative at the second, so a trend rule
  #must lean differently at the two. Without this the test above could pass on a constant signal.
  config <- risk_targeted_config(exposure_method = "trend", exposure_center = 0.75,
                                 exposure_sensitivity = 0.25)
  results <- run_risk_targeted(config, exposure_m_df = risk_targeted_exposure_metric())
  meta_stats <- results@meta_port_stats_m_df@data

  expect_gt(nrow(meta_stats), 1L)
  expect_equal(meta_stats$exposure[1], 1.0, tolerance = 1e-12)
  expect_equal(meta_stats$exposure[nrow(meta_stats)], 0.5, tolerance = 1e-12)

  #and the exposure and the risk estimate are separable in the output, so a move can be
  #attributed to one or the other rather than read as a single opaque number
  expect_true(all(c("exposure", "sleeve_risk", "risky_weight") %in% names(meta_stats)))
})

test_that("a flat exposure of one reproduces the run without any exposure signal", {

  #The addition is a strict generalisation: leaving the signal out must be the same as supplying
  #one that never leans.
  without <- run_risk_targeted(risk_targeted_config())
  flat_metric <- risk_targeted_exposure_metric(values = 1)
  with_flat <- run_risk_targeted(
    risk_targeted_config(exposure_method = "as_is"), exposure_m_df = flat_metric)

  without_stats <- without@meta_port_stats_m_df@data
  flat_stats <- with_flat@meta_port_stats_m_df@data

  expect_equal(without_stats$exposure, rep(1, nrow(without_stats)))
  expect_equal(flat_stats$risky_weight, without_stats$risky_weight, tolerance = 1e-12)
  expect_equal(flat_stats$sleeve_risk, without_stats$sleeve_risk, tolerance = 1e-12)

  #and the stock-level results are identical too, not merely the meta-level ones
  expect_equal(with_flat@projected_stock_weights_m_df@data,
               without@projected_stock_weights_m_df@data)
  expect_equal(with_flat@meta_port_backtest_results@port_returns_m_xts@data,
               without@meta_port_backtest_results@port_returns_m_xts@data)
})

test_that("leaning less carries through to the stocks and to the intended risk", {

  #Half the exposure must halve the sleeve's stock weights and leave the rest to the residual
  half <- run_risk_targeted(
    risk_targeted_config(exposure_method = "as_is", min_weight = 0),
    exposure_m_df = risk_targeted_exposure_metric(values = 0.5))
  full <- run_risk_targeted(
    risk_targeted_config(exposure_method = "as_is", min_weight = 0),
    exposure_m_df = risk_targeted_exposure_metric(values = 1))

  half_stats <- half@meta_port_stats_m_df@data
  full_stats <- full@meta_port_stats_m_df@data

  expect_equal(half_stats$risky_weight, full_stats$risky_weight / 2, tolerance = 1e-12)

  #implied_risk is exposure times the target when the bounds do not bind, since leaning less is
  #meant to carry less risk rather than to miss the target
  unclipped <- full_stats$risky_weight < 1 & half_stats$risky_weight > 0
  expect_true(any(unclipped))
  expect_equal(half_stats$implied_risk[unclipped],
               0.5 * half_stats$target[unclipped], tolerance = 1e-10)

  #and the residual takes what the sleeve gave up
  residual_half <- half@meta_port_weights_m_df@data %>%
    dplyr::filter(tickers == risk_targeted_residual)
  residual_full <- full@meta_port_weights_m_df@data %>%
    dplyr::filter(tickers == risk_targeted_residual)
  expect_true(all(residual_half$weights > residual_full$weights))
})

test_that("the exposure is bounded before the risk ratio scales it", {

  #An as_is metric outside the unit interval is clipped by exposure_bounds, not left to blow up
  #the weight and then be caught by min_weight and max_weight
  results <- run_risk_targeted(
    risk_targeted_config(exposure_method = "as_is", exposure_bounds = c(0.25, 0.75)),
    exposure_m_df = risk_targeted_exposure_metric(values = 5))

  meta_stats <- results@meta_port_stats_m_df@data
  expect_true(all(meta_stats$exposure == 0.75))
})

test_that("a missing or short exposure signal is refused, and says which half is missing", {
  inputs <- risk_targeted_inputs()

  #Asking for a signal without supplying the metric it comes from
  expect_error(
    suppressWarnings(suppressMessages(run_risk_targeted(
      risk_targeted_config(exposure_method = "trend", exposure_sensitivity = 0.25)))),
    "exposure_m_df must be supplied"
  )

  #A signal whose trailing window does not reach the first rebalance date. The risk estimate
  #exists on those dates, so the message must point at the exposure rather than at the risk.
  short_window <- risk_targeted_config(exposure_method = "ts_adjusted", exposure_window = 6,
                                       exposure_sensitivity = -0.25)
  expect_error(
    suppressWarnings(suppressMessages(run_risk_targeted(
      short_window, exposure_m_df = risk_targeted_exposure_metric(values = seq_len(7))))),
    "it is the exposure signal that is missing"
  )
})


# Updating a meta portfolio backtest --------------------------------------
# A meta backtest sits on a cohort, so rolling it forward means rolling every base portfolio
# forward first. The fixtures below build the same panel twice, once stopping a month short and
# once complete, so the short run can be updated into the complete one and the result compared
# against a reconstruction of every stage.

meta_update_cache <- new.env(parent = emptyenv())

meta_update_last_date <- as.Date("2023-04-15")


## The panel, either complete or stopping a month short. Truncating the forward return at the last
## retained date mirrors reality: at that point next month's return is not yet observable.
meta_update_inputs <- function(short = TRUE) {

  key <- if (short) "inputs_short" else "inputs_full"
  if (!is.null(meta_update_cache[[key]])) return(meta_update_cache[[key]])

  load(paste(testthat::test_path(), "/testdata/", "toy_preprocessed_port_obj.RData", sep = ""))

  if (short) {
    keep <- function(df) df %>% dplyr::filter(dates != meta_update_last_date)
    signals_m_df <- keep(signals_m_df)
    liquidity_m_df <- keep(liquidity_m_df)
    volatility_m_df <- keep(volatility_m_df)
    benchmark_weights_m_df <- keep(benchmark_weights_m_df)
    fwd_return_m_df <- keep(fwd_return_m_df) %>%
      dplyr::mutate(fwd_return_1m = dplyr::if_else(
        dates == max(dates), NA_real_, fwd_return_1m))
    benchmark_returns_m_xts <- benchmark_returns_m_xts["2022-10-15/2023-03-15"]
  }

  inputs <- list(
    signals_m_df = create_meta_dataframe(signals_m_df, type = "signals",
                                         meta_dataframe_name = "signals"),
    fwd_return_m_df = create_meta_dataframe(fwd_return_m_df, type = "target",
                                            meta_dataframe_name = "fwd"),
    liquidity_m_df = create_meta_dataframe(liquidity_m_df, meta_dataframe_name = "liq"),
    volatility_m_df = create_meta_dataframe(volatility_m_df, meta_dataframe_name = "vol"),
    benchmark_weights_m_df = create_meta_dataframe(benchmark_weights_m_df, type = "weights",
                                                   meta_dataframe_name = "bench_weights"),
    benchmark_returns_m_xts = suppressMessages(create_meta_xts(
      benchmark_returns_m_xts, asset_type = "benchmark", meta_xts_name = "bench_returns"))
  )

  meta_update_cache[[key]] <- inputs
  inputs
}


meta_update_base_config <- function(method, name) {
  create_port_backtest_config(
    chosen_score_metric_and_position = c(book_yield = "long"),
    eligibility_quantile_range = c(0.67, 1.0), selected_benchmark = "ibov",
    initial_buffer_period = 2, rebalancing_months = c(1, 4),
    port_construction_method = method, main_liquidity_metric = "mean_volfin_3m",
    config_name = name) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)
}


## The cohort stopping a month short, which the meta backtest under test allocates across
meta_update_short_cohort <- function() {
  if (!is.null(meta_update_cache$cohort_short)) return(meta_update_cache$cohort_short)

  inputs <- meta_update_inputs(short = TRUE)
  run_one <- function(config) suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))

  cohort <- suppressWarnings(suppressMessages(create_port_backtest_cohort(
    list(run_one(meta_update_base_config("ew", "ew_upd")),
         run_one(meta_update_base_config("sw", "sw_upd"))),
    cohort_name = "meta_update_cohort")))

  meta_update_cache$cohort_short <- cohort
  cohort
}


## Each base portfolio rolled forward one month through update_port_backtest, which is the input
## the meta update requires. Built with the same method the meta update is being tested against,
## deliberately: if the base update were broken this cohort would be wrong and the meta
## reconstruction below would not match either.
meta_update_full_cohort <- function() {
  if (!is.null(meta_update_cache$cohort_full)) return(meta_update_cache$cohort_full)

  short_cohort <- meta_update_short_cohort()
  full_inputs <- meta_update_inputs(short = FALSE)

  updated_list <- lapply(short_cohort@port_backtest_results_list, function(old_base) {
    suppressWarnings(suppressMessages(update_port_backtest(
      signals_m_df = full_inputs$signals_m_df,
      fwd_return_m_df = full_inputs$fwd_return_m_df,
      liquidity_m_df = full_inputs$liquidity_m_df,
      volatility_m_df = full_inputs$volatility_m_df,
      old_results = old_base,
      benchmark_weights_m_df = full_inputs$benchmark_weights_m_df,
      benchmark_returns_m_xts = full_inputs$benchmark_returns_m_xts,
      verbose = FALSE, parallel = FALSE)))
  })

  cohort <- suppressWarnings(suppressMessages(create_port_backtest_cohort(
    updated_list, cohort_name = "meta_update_cohort")))

  meta_update_cache$cohort_full <- cohort
  cohort
}


meta_update_config <- function(config_name = "meta_update") {
  inner <- create_port_backtest_config(
    chosen_score_metric_and_position = c(ann_info_ratio = "long"),
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = 4, rebalancing_months = c(1, 2, 3, 4),
    selected_benchmark = "ibov",
    cov_est_method = create_cov_est_method("sample", 2, TRUE, "ibov"),
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "sw", config_name = config_name) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  suppressMessages(create_port_metabacktest_config(inner, config_name = config_name,
                                                   verbose = FALSE))
}


meta_update_short_run <- function() {
  if (!is.null(meta_update_cache$run_short)) return(meta_update_cache$run_short)
  inputs <- meta_update_inputs(short = TRUE)
  results <- suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = meta_update_config(), port_backtest_cohort = meta_update_short_cohort(),
    benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))
  meta_update_cache$run_short <- results
  results
}


meta_update_run <- function() {
  if (!is.null(meta_update_cache$run_updated)) return(meta_update_cache$run_updated)
  full_inputs <- meta_update_inputs(short = FALSE)
  results <- suppressWarnings(suppressMessages(update_port_backtest(
    signals_m_df = full_inputs$signals_m_df,
    fwd_return_m_df = full_inputs$fwd_return_m_df,
    liquidity_m_df = full_inputs$liquidity_m_df,
    volatility_m_df = full_inputs$volatility_m_df,
    old_results = meta_update_short_run(),
    updated_port_backtest_cohort = meta_update_full_cohort(),
    benchmark_weights_m_df = full_inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = full_inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))
  meta_update_cache$run_updated <- results
  results
}


test_that("the meta update reproduces a step-by-step reconstruction of its own chain", {

  #The same reconstruction the non-update test does, but over the window the update recomputes.
  #Every stage is rebuilt from the functions the method calls, so a wrong argument anywhere in
  #the update wiring shows up as a mismatched stage rather than as a plausible-looking number.
  old_results <- meta_update_short_run()
  updated <- meta_update_run()
  full_cohort <- meta_update_full_cohort()
  full_inputs <- meta_update_inputs(short = FALSE)

  config <- meta_update_config()
  inner_config <- config@meta_port_backtest_config
  old_inner_workflow <- old_results@meta_port_backtest_results@port_backtest_workflow[[
    length(old_results@meta_port_backtest_results@port_backtest_workflow)]]

  #Stage 1: the port universe, now derived from the updated cohort
  expected_universe <- suppressWarnings(suppressMessages(derive_port_universe_m_df(
    port_backtest_cohort = full_cohort,
    return_basis = config@return_basis,
    cost_lookback = config@cost_lookback,
    verbose = FALSE)))
  expect_equal(updated@port_universe_m_df@data, expected_universe@data)

  #The universe must actually reach the new month, or nothing below is testing the update
  expect_true(meta_update_last_date %in% expected_universe@data$dates)

  #Stage 2: the recomputed window. The update sets the buffer to the old number of dates, so the
  #recomputation starts at the last old date, whose forward returns are now populated.
  recomputed_buffer <- old_inner_workflow$n_dates
  dates_vector <- sort(unique(full_inputs$signals_m_df@data$dates))
  dates_backtest <- dates_vector[recomputed_buffer:length(dates_vector)]
  recomputed_dates <- sort(unique(c(
    min(dates_backtest),
    dates_backtest[lubridate::month(dates_backtest) %in% inner_config@rebalancing_months])))

  #The window has to cover more than the new date alone, or the overlap handling is untested
  expect_gt(length(recomputed_dates), 1L)

  base_returns_xts <- full_cohort@port_returns_m_xts_list$net_returns_m_xts@data
  base_names <- sort(unique(expected_universe@data$tickers))
  bench_returns_xts <- base_returns_xts[, "selected_bench_return", drop = FALSE]
  base_returns_xts <- base_returns_xts[, base_names, drop = FALSE]
  cov_est_method <- inner_config@cov_est_method

  reconstructed <- list()
  for (i in seq_along(recomputed_dates)) {
    current_date <- recomputed_dates[i]

    universe_m_d_ref <- expected_universe@data %>% dplyr::filter(dates == current_date)

    scored_m_d_ref <- derive_stock_universe_m_d_ref(
      signals_m_d_ref = universe_m_d_ref,
      oos_predictions_m_d_ref = NULL,
      chosen_score_metric_and_position = inner_config@chosen_score_metric_and_position,
      chosen_scaler = inner_config@chosen_scaler,
      scaler_m_d_ref = NULL,
      scaler_shrinkage = if (is.null(inner_config@scaler_shrinkage)) 0 else inner_config@scaler_shrinkage,
      lower_quantile_winsorization = 0.025,
      upper_quantile_winsorization = 0.975)

    classified_m_d_ref <- classify_investment_universe(
      universe_m_d_ref = scored_m_d_ref,
      eligibility_quantile_range = inner_config@eligibility_quantile_range,
      min_eligible_assets_fallback = inner_config@min_eligible_assets_fallback,
      use_raw_for_eligibility = FALSE,
      asset_object = "stocks",
      verbose = FALSE)

    returns_upd_ref <- base_returns_xts[zoo::index(base_returns_xts) <= current_date, , drop = FALSE]
    bench_upd_ref <- bench_returns_xts[zoo::index(bench_returns_xts) <= current_date, , drop = FALSE]

    meta_port <- suppressWarnings(suppressMessages(set_portfolio_weights(
      universe_m_d_ref = classified_m_d_ref,
      port_construction_method = inner_config@port_construction_method,
      covariance_matrix = NULL,
      eligible_returns_m_xts_upd_ref = returns_upd_ref,
      selected_benchmark_m_xts_upd_ref = bench_upd_ref,
      active_returns = cov_est_method@active_returns,
      cov_estimation_method = cov_est_method@cov_estimation_method,
      cov_matrix_sample_size = cov_est_method@cov_matrix_sample_size,
      top_down_proxy_port_method = "ew", mmaf_group_col = NULL,
      selected_benchmark = NULL,
      lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
      parallel = FALSE, verbose = FALSE)))

    reconstructed[[i]] <- meta_port@universe_m_d_ref@data %>%
      dplyr::select(id, tickers, dates, weights)
  }
  reconstructed <- do.call(rbind, reconstructed)

  #Stage 3: the consolidation. Old rows outside the recomputed window survive; rows inside it are
  #replaced by the recomputed ones, because those are the figures formed with the forward returns
  #that only became available with the new month.
  old_weights <- old_results@meta_port_weights_m_df@data
  expected_weights <- dplyr::bind_rows(
    old_weights[!old_weights$id %in% reconstructed$id, , drop = FALSE], reconstructed)
  expected_weights <- expected_weights[order(expected_weights$id), , drop = FALSE]
  rownames(expected_weights) <- NULL

  expect_equal(updated@meta_port_weights_m_df@data, expected_weights)

  #and it is a real allocation, not a degenerate one that would match trivially
  expect_gt(stats::sd(expected_weights$weights), 0)

  #Stage 4: the projection onto stocks, rebuilt from the consolidated meta weights
  expected_projection <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = expected_weights,
    port_backtest_cohort = full_cohort,
    signals_m_df = full_inputs$signals_m_df,
    verbose = FALSE))
  expect_equal(updated@projected_stock_weights_m_df@data, expected_projection@data)
})


test_that("the meta update preserves the old history and appends the new month", {

  old_results <- meta_update_short_run()
  updated <- meta_update_run()

  old_dates <- sort(unique(old_results@meta_port_weights_m_df@data$dates))
  new_dates <- sort(unique(updated@meta_port_weights_m_df@data$dates))

  #Nothing is lost, and exactly the new month is gained
  expect_true(all(old_dates %in% new_dates))
  expect_equal(max(new_dates), meta_update_last_date)
  expect_false(meta_update_last_date %in% old_dates)

  #The stock-level returns are the old series extended, not recomputed from scratch. The last old
  #date is recomputed, so it is the earlier dates that must survive untouched.
  old_returns <- old_results@meta_port_backtest_results@port_returns_m_xts@data
  new_returns <- updated@meta_port_backtest_results@port_returns_m_xts@data
  untouched <- zoo::index(old_returns) < max(zoo::index(old_returns))
  expect_true(any(untouched))
  expect_equal(as.numeric(old_returns[untouched, "net_return"]),
               as.numeric(new_returns[zoo::index(new_returns) %in%
                                        zoo::index(old_returns)[untouched], "net_return"]),
               tolerance = 1e-10)
  expect_gt(nrow(new_returns), nrow(old_returns))

  #The workflow records the update rather than replacing the original run
  workflow <- updated@meta_port_backtest_results@port_backtest_workflow
  expect_gt(length(workflow),
            length(old_results@meta_port_backtest_results@port_backtest_workflow))
  expect_true(any(grepl("^update_", names(workflow))))

  #The transactions log is extended too, since costs are charged on the new trades
  expect_gt(length(updated@meta_port_backtest_results@transactions_log@data),
            length(old_results@meta_port_backtest_results@transactions_log@data))
})


test_that("the meta update refuses a cohort that is not the one-month continuation", {

  full_inputs <- meta_update_inputs(short = FALSE)
  old_results <- meta_update_short_run()

  run_update <- function(cohort) {
    suppressWarnings(suppressMessages(update_port_backtest(
      signals_m_df = full_inputs$signals_m_df,
      fwd_return_m_df = full_inputs$fwd_return_m_df,
      liquidity_m_df = full_inputs$liquidity_m_df,
      volatility_m_df = full_inputs$volatility_m_df,
      old_results = old_results, updated_port_backtest_cohort = cohort,
      benchmark_weights_m_df = full_inputs$benchmark_weights_m_df,
      benchmark_returns_m_xts = full_inputs$benchmark_returns_m_xts,
      verbose = FALSE, parallel = FALSE)))
  }

  #A cohort that was never rolled forward would silently produce meta weights for the new month
  #from statistics that stop a month short of it
  expect_error(run_update(meta_update_short_cohort()),
               "must be updated before the meta backtest")

  #and one holding different portfolios is not the same allocation problem at all
  wrong_members <- meta_update_full_cohort()
  wrong_members@port_backtest_results_list <- wrong_members@port_backtest_results_list[1]
  expect_error(run_update(wrong_members), "do not match the ones the old meta backtest")

  #The cohort is required, since there is nothing to allocate over without it
  expect_error(
    suppressWarnings(suppressMessages(update_port_backtest(
      signals_m_df = full_inputs$signals_m_df,
      fwd_return_m_df = full_inputs$fwd_return_m_df,
      liquidity_m_df = full_inputs$liquidity_m_df,
      volatility_m_df = full_inputs$volatility_m_df,
      old_results = old_results, verbose = FALSE, parallel = FALSE))),
    "updated_port_backtest_cohort must be provided")
})


test_that("the risk-targeted meta backtest reproduces a step-by-step reconstruction of its chain", {

  #The counterpart of the multi-portfolio reconstruction above, for the path where the weight
  #comes from the targeting rule rather than from ranking a cross-section. Every stage is rebuilt
  #from the functions the method calls, so a wrong argument anywhere in the wiring shows up as a
  #mismatched stage rather than as a plausible-looking number.
  results <- run_risk_targeted()
  cohort <- risk_targeted_cohort()
  config <- risk_targeted_config()
  inner_config <- config@meta_port_backtest_config
  inputs <- risk_targeted_inputs()
  risk_target_params <- results@risk_target_parameters
  sleeve <- cohort@port_backtest_results_list[[1]]

  #Stage 1: the port universe. A lone sleeve is the required shape here, not too small a cohort.
  expected_universe <- suppressWarnings(suppressMessages(derive_port_universe_m_df(
    port_backtest_cohort = cohort,
    return_basis = config@return_basis,
    cost_lookback = config@cost_lookback,
    allow_single_portfolio = TRUE,
    verbose = FALSE)))
  expect_equal(results@port_universe_m_df@data, expected_universe@data)

  #Stage 2: the rule, rebuilt one rebalance date at a time from its two halves
  meta_stats <- results@meta_port_stats_m_df@data
  rebalance_dates <- sort(unique(meta_stats$dates))
  expect_gt(length(rebalance_dates), 1L)

  reconstructed_risk <- numeric(length(rebalance_dates))
  reconstructed_weight <- numeric(length(rebalance_dates))

  for (i in seq_along(rebalance_dates)) {
    current_date <- rebalance_dates[i]

    reconstructed_risk[i] <- suppressWarnings(suppressMessages(estimate_sleeve_risk(
      current_date = current_date,
      risk_target_params = risk_target_params,
      risky_port_backtest_results = sleeve,
      daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts@data,
      selected_benchmark = inner_config@selected_benchmark,
      stock_groups_m_d_ref = inputs$stock_groups_m_df@data %>%
        dplyr::filter(as.Date(dates) == current_date),
      return_basis = config@return_basis)))

    #No exposure signal is configured here, so the multiplier is one and the weight is the
    #risk ratio alone. risk_to_weight() applies the exponent and the bounds.
    reconstructed_weight[i] <- risk_to_weight(reconstructed_risk[i], risk_target_params, 1)
  }

  expect_equal(meta_stats$sleeve_risk, reconstructed_risk, tolerance = 1e-10)
  expect_equal(meta_stats$risky_weight, reconstructed_weight, tolerance = 1e-10)

  #The reconstruction must move, or a constant weight would match trivially
  expect_gt(stats::sd(reconstructed_weight), 0)

  #Stage 3: the meta weights, which are the rule for the sleeve and the remainder for the residual
  sleeve_name <- sleeve@backtest_identifier
  expected_weights <- dplyr::bind_rows(
    data.frame(tickers = sleeve_name, dates = rebalance_dates,
               weights = reconstructed_weight, stringsAsFactors = FALSE),
    data.frame(tickers = risk_targeted_residual, dates = rebalance_dates,
               weights = 1 - reconstructed_weight, stringsAsFactors = FALSE)) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates)) %>%
    dplyr::select(id, tickers, dates, weights) %>%
    dplyr::arrange(id) %>%
    as.data.frame()
  rownames(expected_weights) <- NULL

  expect_equal(results@meta_port_weights_m_df@data, expected_weights)

  #The two sleeves are fully invested at every date, which is what makes the residual the
  #complement of the rule rather than an independent position
  sums <- tapply(expected_weights$weights, expected_weights$dates, sum)
  expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-12)

  #Stage 4: the projection onto stocks, which has to carry the residual through as a holding
  expected_projection <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = expected_weights,
    port_backtest_cohort = cohort,
    signals_m_df = inputs$signals_m_df,
    residual_ticker = risk_targeted_residual,
    verbose = FALSE))
  expect_equal(results@projected_stock_weights_m_df@data, expected_projection@data)

  #and the residual really is held, or the blend never happened
  residual_rows <- expected_projection@data %>%
    dplyr::filter(tickers == risk_targeted_residual, weights > 0)
  expect_gt(nrow(residual_rows), 0)

  #Stage 5: the stock-level backtest run on those projected weights
  expected_inner <- suppressWarnings(suppressMessages(run_port_backtest_internal(
    signals_m_df = inputs$signals_m_df@data,
    oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = NULL,
    rebalancing_months = inner_config@rebalancing_months,
    initial_buffer_period = inner_config@initial_buffer_period,
    port_construction_method = "custom_weights",
    selected_benchmark = inner_config@selected_benchmark,
    eligibility_quantile_range = inner_config@eligibility_quantile_range,
    exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL, mmaf_group_col = NULL,
    cov_estimation_method = inner_config@cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = inner_config@cov_est_method@cov_matrix_sample_size,
    active_returns = inner_config@cov_est_method@active_returns,
    cov_matrix_benchmark = inner_config@cov_est_method@cov_matrix_benchmark,
    daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts@data,
    daily_bench_returns_m_xts = inputs$daily_bench_returns_m_xts@data,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts@data,
    liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL,
    concentration_constraint_policy = NULL,
    liquidity_m_df = inputs$liquidity_m_df@data,
    main_liquidity_metric = inner_config@main_liquidity_metric,
    liquidity_floor_cutoffs = inner_config@liquidity_floor_cutoffs,
    volatility_m_df = inputs$volatility_m_df@data,
    fwd_return_m_df = inputs$fwd_return_m_df@data,
    stock_groups_m_df = inputs$stock_groups_m_df@data,
    benchmark_weights_m_df = inputs$benchmark_weights_m_df@data,
    transaction_costs_parameters = as.list(inner_config@transaction_costs_parameters),
    custom_stock_weights_m_df = expected_projection@data,
    custom_stock_metrics_m_df = NULL,
    lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
    verbose = FALSE, parallel = FALSE)))

  expect_equal(results@meta_port_backtest_results@port_weights_m_df@data,
               expected_inner@port_weights_m_df@data)
  expect_equal(results@meta_port_backtest_results@port_returns_m_xts@data,
               expected_inner@port_returns_m_xts@data)
  expect_equal(results@meta_port_backtest_results@port_costs_m_xts@data,
               expected_inner@port_costs_m_xts@data)

  #and the returns are real numbers rather than an all-NA series matching itself
  expect_true(any(is.finite(expected_inner@port_returns_m_xts@data$net_return)))
})


test_that("the exposure signal enters the reconstruction as its own factor", {

  #The same chain, but with a signal configured, so the weight is the product of two things that
  #have to be reproduced separately. Without this the reconstruction above could pass on a rule
  #whose exposure half was silently dropped.
  config <- risk_targeted_config(exposure_method = "trend", exposure_center = 0.75,
                                 exposure_sensitivity = 0.25)
  exposure_metric <- risk_targeted_exposure_metric()
  results <- run_risk_targeted(config, exposure_m_df = exposure_metric)

  cohort <- risk_targeted_cohort()
  inputs <- risk_targeted_inputs()
  inner_config <- config@meta_port_backtest_config
  risk_target_params <- results@risk_target_parameters
  sleeve <- cohort@port_backtest_results_list[[1]]
  meta_stats <- results@meta_port_stats_m_df@data

  expected_exposure <- suppressMessages(derive_exposure_signal(
    metric_m_df = exposure_metric, method = "trend",
    center = 0.75, sensitivity = 0.25, verbose = FALSE))

  for (i in seq_len(nrow(meta_stats))) {
    current_date <- meta_stats$dates[i]

    risk <- suppressWarnings(suppressMessages(estimate_sleeve_risk(
      current_date = current_date,
      risk_target_params = risk_target_params,
      risky_port_backtest_results = sleeve,
      daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts@data,
      selected_benchmark = inner_config@selected_benchmark,
      stock_groups_m_d_ref = inputs$stock_groups_m_df@data %>%
        dplyr::filter(as.Date(dates) == current_date),
      return_basis = "net")))

    exposure <- expected_exposure$exposure[expected_exposure$dates == current_date]
    expect_length(exposure, 1L)

    expect_equal(meta_stats$sleeve_risk[i], risk, tolerance = 1e-10)
    expect_equal(meta_stats$exposure[i], exposure, tolerance = 1e-12)
    expect_equal(meta_stats$risky_weight[i],
                 risk_to_weight(risk, risk_target_params, exposure), tolerance = 1e-12)
  }

  #The signal has to have leaned differently across the sample, or it is not being tested
  expect_gt(stats::sd(meta_stats$exposure), 0)
})


# Updating a risk-targeted meta backtest ----------------------------------
# The same exercise as the multi-portfolio update, on the path where the weight comes from the
# targeting rule. The sleeve has to be rolled forward first, exactly as the base portfolios are.

rt_update_cache <- new.env(parent = emptyenv())


## The risk-targeted panel, either complete or stopping a month short. Built the same way
## risk_targeted_inputs() builds its own, so the residual is a tradable row of every object.
rt_update_inputs <- function(short = TRUE) {

  key <- if (short) "inputs_short" else "inputs_full"
  if (!is.null(rt_update_cache[[key]])) return(rt_update_cache[[key]])

  full <- risk_targeted_inputs()
  if (!short) {
    rt_update_cache[[key]] <- full
    return(full)
  }

  drop_last <- function(meta_df, type = "generic") {
    trimmed <- meta_df@data %>% dplyr::filter(dates != meta_update_last_date)
    suppressWarnings(suppressMessages(create_meta_dataframe(
      trimmed, meta_dataframe_name = meta_df@meta_dataframe_name, type = type)))
  }

  ##The last retained date has no forward return yet, which is the state the update starts from
  fwd_trimmed <- full$fwd_return_m_df@data %>%
    dplyr::filter(dates != meta_update_last_date) %>%
    dplyr::mutate(fwd_return_1m = dplyr::if_else(dates == max(dates), NA_real_, fwd_return_1m))

  daily_full <- full$daily_stock_returns_m_xts@data
  daily_bench_full <- full$daily_bench_returns_m_xts@data
  cutoff <- as.Date("2023-03-15")

  inputs <- list(
    signals_m_df = drop_last(full$signals_m_df, "signals"),
    fwd_return_m_df = suppressWarnings(suppressMessages(create_meta_dataframe(
      fwd_trimmed, meta_dataframe_name = full$fwd_return_m_df@meta_dataframe_name,
      type = "target"))),
    liquidity_m_df = drop_last(full$liquidity_m_df, "generic"),
    volatility_m_df = drop_last(full$volatility_m_df, "generic"),
    benchmark_weights_m_df = drop_last(full$benchmark_weights_m_df, "weights"),
    stock_groups_m_df = drop_last(full$stock_groups_m_df, "groups"),
    benchmark_returns_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      full$benchmark_returns_m_xts@data[zoo::index(full$benchmark_returns_m_xts@data) <= cutoff]))),
    daily_stock_returns_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      daily_full[zoo::index(daily_full) <= cutoff], type = "returns",
      asset_type = "stocks", meta_xts_name = "B3"))),
    daily_bench_returns_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      daily_bench_full[zoo::index(daily_bench_full) <= cutoff], type = "returns",
      asset_type = "benchmark", meta_xts_name = "B3")))
  )

  rt_update_cache[[key]] <- inputs
  inputs
}


rt_update_sleeve_config <- function() {
  create_port_backtest_config(
    chosen_score_metric_and_position = c(book_yield = "long"),
    eligibility_quantile_range = c(0.67, 1.0), selected_benchmark = "ibov",
    initial_buffer_period = 2, rebalancing_months = c(1, 4),
    port_construction_method = "sw", main_liquidity_metric = "mean_volfin_3m",
    config_name = "risky_upd") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)
}


rt_update_short_cohort <- function() {
  if (!is.null(rt_update_cache$cohort_short)) return(rt_update_cache$cohort_short)
  inputs <- rt_update_inputs(short = TRUE)
  sleeve <- suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = rt_update_sleeve_config(),
    benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))
  cohort <- suppressWarnings(suppressMessages(create_port_backtest_cohort(
    list(sleeve), cohort_name = "rt_update_cohort")))
  rt_update_cache$cohort_short <- cohort
  cohort
}


rt_update_full_cohort <- function() {
  if (!is.null(rt_update_cache$cohort_full)) return(rt_update_cache$cohort_full)
  full_inputs <- rt_update_inputs(short = FALSE)
  updated <- suppressWarnings(suppressMessages(update_port_backtest(
    signals_m_df = full_inputs$signals_m_df,
    fwd_return_m_df = full_inputs$fwd_return_m_df,
    liquidity_m_df = full_inputs$liquidity_m_df,
    volatility_m_df = full_inputs$volatility_m_df,
    old_results = rt_update_short_cohort()@port_backtest_results_list[[1]],
    benchmark_weights_m_df = full_inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = full_inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))
  cohort <- suppressWarnings(suppressMessages(create_port_backtest_cohort(
    list(updated), cohort_name = "rt_update_cohort")))
  rt_update_cache$cohort_full <- cohort
  cohort
}


## Rebalancing every month from January, so the recomputed window overlaps a date the short run
## already produced and the consolidation has something to replace
rt_update_config <- function(target = 10) {
  inner <- create_port_backtest_config(
    chosen_score_metric_and_position = NULL,
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = 4, rebalancing_months = c(1, 2, 3, 4),
    selected_benchmark = "ibov",
    cov_est_method = create_cov_est_method("sample", 2, TRUE, "ibov"),
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "custom_weights", config_name = "rt_update") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  suppressMessages(create_port_metabacktest_config(
    inner, type = "risk_targeted", config_name = "rt_update",
    stock_cov_matrix_sample_size = 60, verbose = FALSE)) %>%
    add_risk_target_parameters(
      residual_ticker = risk_targeted_residual, target = target,
      target_metric = "tracking_error", p = 1, min_weight = 0.2, max_weight = 1,
      vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL))
}


rt_update_short_run <- function() {
  if (!is.null(rt_update_cache$run_short)) return(rt_update_cache$run_short)
  inputs <- rt_update_inputs(short = TRUE)
  results <- suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = rt_update_config(), port_backtest_cohort = rt_update_short_cohort(),
    benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = inputs$daily_bench_returns_m_xts,
    stock_groups_m_df = inputs$stock_groups_m_df,
    verbose = FALSE, parallel = FALSE)))
  rt_update_cache$run_short <- results
  results
}


rt_update_run <- function() {
  if (!is.null(rt_update_cache$run_updated)) return(rt_update_cache$run_updated)
  full_inputs <- rt_update_inputs(short = FALSE)
  results <- suppressWarnings(suppressMessages(update_port_backtest(
    signals_m_df = full_inputs$signals_m_df,
    fwd_return_m_df = full_inputs$fwd_return_m_df,
    liquidity_m_df = full_inputs$liquidity_m_df,
    volatility_m_df = full_inputs$volatility_m_df,
    old_results = rt_update_short_run(),
    updated_port_backtest_cohort = rt_update_full_cohort(),
    benchmark_weights_m_df = full_inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = full_inputs$benchmark_returns_m_xts,
    daily_stock_returns_m_xts = full_inputs$daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = full_inputs$daily_bench_returns_m_xts,
    stock_groups_m_df = full_inputs$stock_groups_m_df,
    verbose = FALSE, parallel = FALSE)))
  rt_update_cache$run_updated <- results
  results
}


test_that("the risk-targeted update reproduces a step-by-step reconstruction of its chain", {

  old_results <- rt_update_short_run()
  updated <- rt_update_run()
  full_cohort <- rt_update_full_cohort()
  full_inputs <- rt_update_inputs(short = FALSE)

  config <- rt_update_config()
  inner_config <- config@meta_port_backtest_config
  risk_target_params <- updated@risk_target_parameters
  sleeve <- full_cohort@port_backtest_results_list[[1]]

  #The subclass survives the update rather than degrading to the parent
  expect_s4_class(updated, "risk_target_metabacktest_results")
  expect_equal(updated@residual_ticker, risk_targeted_residual)

  #Stage 1: the universe, from the rolled-forward sleeve
  expected_universe <- suppressWarnings(suppressMessages(derive_port_universe_m_df(
    port_backtest_cohort = full_cohort,
    return_basis = config@return_basis,
    cost_lookback = config@cost_lookback,
    allow_single_portfolio = TRUE,
    verbose = FALSE)))
  expect_equal(updated@port_universe_m_df@data, expected_universe@data)
  expect_true(meta_update_last_date %in% expected_universe@data$dates)

  #Stage 2: the recomputed window, resolved the way the update resolves it
  old_inner_workflow <- old_results@meta_port_backtest_results@port_backtest_workflow[[
    length(old_results@meta_port_backtest_results@port_backtest_workflow)]]
  dates_vector <- sort(unique(full_inputs$signals_m_df@data$dates))
  dates_backtest <- dates_vector[old_inner_workflow$n_dates:length(dates_vector)]
  recomputed_dates <- sort(unique(c(
    min(dates_backtest),
    dates_backtest[lubridate::month(dates_backtest) %in% inner_config@rebalancing_months])))

  #and it must overlap the old run, or the consolidation is untested
  old_dates <- sort(unique(old_results@meta_port_stats_m_df@data$dates))
  expect_true(any(recomputed_dates %in% old_dates))

  #Stage 3: the rule over that window, rebuilt from its two halves
  reconstructed_weight <- numeric(length(recomputed_dates))
  for (i in seq_along(recomputed_dates)) {
    current_date <- recomputed_dates[i]
    risk <- suppressWarnings(suppressMessages(estimate_sleeve_risk(
      current_date = current_date,
      risk_target_params = risk_target_params,
      risky_port_backtest_results = sleeve,
      daily_stock_returns_m_xts = full_inputs$daily_stock_returns_m_xts@data,
      selected_benchmark = inner_config@selected_benchmark,
      stock_groups_m_d_ref = full_inputs$stock_groups_m_df@data %>%
        dplyr::filter(as.Date(dates) == current_date),
      return_basis = config@return_basis)))
    reconstructed_weight[i] <- risk_to_weight(risk, risk_target_params, 1)
  }

  new_stats <- updated@meta_port_stats_m_df@data %>%
    dplyr::filter(dates %in% recomputed_dates) %>%
    dplyr::arrange(dates)
  expect_equal(new_stats$risky_weight, reconstructed_weight, tolerance = 1e-10)

  #Stage 4: the consolidated meta weights. The recomputed window replaces the old rows inside it.
  sleeve_name <- sleeve@backtest_identifier
  recomputed_weights <- dplyr::bind_rows(
    data.frame(tickers = sleeve_name, dates = recomputed_dates,
               weights = reconstructed_weight, stringsAsFactors = FALSE),
    data.frame(tickers = risk_targeted_residual, dates = recomputed_dates,
               weights = 1 - reconstructed_weight, stringsAsFactors = FALSE)) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates)) %>%
    dplyr::select(id, tickers, dates, weights) %>%
    as.data.frame()

  old_weights <- old_results@meta_port_weights_m_df@data
  expected_weights <- dplyr::bind_rows(
    old_weights[!old_weights$id %in% recomputed_weights$id, , drop = FALSE], recomputed_weights)
  expected_weights <- expected_weights[order(expected_weights$id), , drop = FALSE]
  rownames(expected_weights) <- NULL

  expect_equal(updated@meta_port_weights_m_df@data, expected_weights)

  #Stage 5: the projection, which must still carry the residual
  expected_projection <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = expected_weights,
    port_backtest_cohort = full_cohort,
    signals_m_df = full_inputs$signals_m_df,
    residual_ticker = risk_targeted_residual,
    verbose = FALSE))
  expect_equal(updated@projected_stock_weights_m_df@data, expected_projection@data)
})


test_that("the risk-targeted update extends the history and keeps targeting", {

  old_results <- rt_update_short_run()
  updated <- rt_update_run()

  old_dates <- sort(unique(old_results@meta_port_stats_m_df@data$dates))
  new_dates <- sort(unique(updated@meta_port_stats_m_df@data$dates))

  expect_true(all(old_dates %in% new_dates))
  expect_equal(max(new_dates), meta_update_last_date)
  expect_false(meta_update_last_date %in% old_dates)

  #The targeting identity still holds on the new month wherever no bound binds, which is what
  #says the rule is still being applied rather than the weights being carried forward
  stats_new <- updated@meta_port_stats_m_df@data
  params <- updated@risk_target_parameters
  unclipped <- stats_new$risky_weight > params@min_weight &
    stats_new$risky_weight < params@max_weight
  expect_true(any(unclipped))
  expect_equal(stats_new$implied_risk[unclipped], stats_new$target[unclipped],
               tolerance = 1e-10)

  #The stock-level series is extended rather than rebuilt
  old_returns <- old_results@meta_port_backtest_results@port_returns_m_xts@data
  new_returns <- updated@meta_port_backtest_results@port_returns_m_xts@data
  expect_gt(nrow(new_returns), nrow(old_returns))
  untouched <- zoo::index(old_returns) < max(zoo::index(old_returns))
  expect_true(any(untouched))
  expect_equal(as.numeric(old_returns[untouched, "net_return"]),
               as.numeric(new_returns[zoo::index(new_returns) %in%
                                        zoo::index(old_returns)[untouched], "net_return"]),
               tolerance = 1e-10)

  #and the residual is still held at the end, so the blend survived the update
  final_weights <- updated@meta_port_weights_m_df@data %>%
    dplyr::filter(dates == meta_update_last_date)
  expect_setequal(final_weights$tickers,
                  c(rt_update_full_cohort()@port_backtest_results_list[[1]]@backtest_identifier,
                    risk_targeted_residual))
  expect_equal(sum(final_weights$weights), 1, tolerance = 1e-12)
})


# The residual is a replication, and its inputs look like an asset's -------

test_that("the residual replicates the benchmark rather than standing in for cash", {

  #The distinction decides whether the whole rule works. An index-tracking residual makes tracking
  #error scale linearly toward zero as the sleeve is cut; a constant-return residual has a
  #tracking error equal to the benchmark's own volatility, so blending toward it turns the
  #portfolio into a large underweight of the market and raises tracking error instead.
  inputs <- risk_targeted_inputs()
  load(paste(test_path(), "/testdata/", "toy_preprocessed_port_obj.RData", sep = ""))

  residual_fwd <- inputs$fwd_return_m_df@data %>%
    dplyr::filter(tickers == risk_targeted_residual) %>%
    dplyr::arrange(dates)
  index_monthly <- stats::setNames(as.numeric(benchmark_returns_m_xts[, "ibov"]),
                                   as.character(zoo::index(benchmark_returns_m_xts)))

  ##fwd_return_1m at t is the return from t to t+1, and the engine reads the benchmark return at
  ##t+1 to form the active return, so the two have to be aligned that way to cancel
  own_dates <- residual_fwd$dates
  expected_next <- vapply(own_dates, function(d) {
    later <- own_dates[own_dates > d]
    if (length(later) == 0) return(NA_real_)
    unname(index_monthly[[as.character(min(later))]])
  }, numeric(1))

  tracking_difference <- residual_fwd$fwd_return_1m - expected_next
  residual_te <- stats::sd(tracking_difference, na.rm = TRUE) * sqrt(12)

  ##A replication tracks closely. The benchmark's own annualised volatility over this sample is
  ##about 19 percent, which is what a constant-return residual would report, so the gap between
  ##the two is the whole point.
  expect_lt(residual_te, 1)
  expect_gt(residual_te, 0)

  index_vol <- stats::sd(as.numeric(benchmark_returns_m_xts[, "ibov"]), na.rm = TRUE) * sqrt(12)
  expect_gt(index_vol / residual_te, 10)

  ##and the last date carries no forward return, exactly as the stocks do not
  expect_true(is.na(residual_fwd$fwd_return_1m[which.max(residual_fwd$dates)]))
})

test_that("the residual tracks the benchmark on daily data too", {

  #The ex-ante risk estimate reads daily returns, so a residual that tracks monthly but not daily
  #would still make the estimated tracking error wrong
  inputs <- risk_targeted_inputs()
  daily <- inputs$daily_stock_returns_m_xts@data
  bench_daily <- inputs$daily_bench_returns_m_xts@data

  residual_daily <- as.numeric(daily[, risk_targeted_residual])
  index_daily <- as.numeric(bench_daily[, "ibov"])

  expect_gt(stats::cor(residual_daily, index_daily), 0.95)

  ##The tracking difference must not be zero: a residual that replicates exactly has no variance
  ##in active space, which makes the active-return correlation matrix singular
  expect_gt(stats::sd(residual_daily - index_daily), 0)

  ##and the daily index is the benchmark weights applied to the daily stock returns, not an
  ##unrelated series, so it has to be far less volatile than a typical single stock
  stock_vols <- apply(daily[, setdiff(colnames(daily), risk_targeted_residual)], 2,
                      stats::sd, na.rm = TRUE)
  expect_lt(stats::sd(index_daily), stats::median(stock_vols, na.rm = TRUE))
})

test_that("the residual's inputs are comparable to the other assets", {

  #The residual is held like any other position, so anything the engine reads about it has to be
  #on the same scale as the rest of the panel. Sentinel values such as 1e9 do not error, they just
  #make it the most liquid asset by three orders of magnitude and distort anything that ranks or
  #screens on those columns.
  inputs <- risk_targeted_inputs()

  compare_to_panel <- function(meta_df, object_name) {
    data <- meta_df@data
    residual_rows <- data[data$tickers == risk_targeted_residual, , drop = FALSE]
    other_rows <- data[data$tickers != risk_targeted_residual, , drop = FALSE]
    for (column in setdiff(names(data), c("id", "tickers", "dates"))) {
      residual_values <- residual_rows[[column]]
      other_values <- other_rows[[column]]

      ##Type first: writing a number into a character column puts the residual in a group of its
      ##own invention rather than in a real one
      expect_equal(class(residual_values), class(other_values),
                   info = paste(object_name, column, "type"))

      if (is.numeric(other_values)) {
        expect_true(all(is.finite(residual_values)),
                    info = paste(object_name, column, "finite"))
        expect_lte(max(residual_values, na.rm = TRUE), max(other_values, na.rm = TRUE))
        ##Volatility is the exception: a diversified index is legitimately less volatile than
        ##any single constituent, so the floor is zero rather than the panel minimum
        expect_gt(min(residual_values, na.rm = TRUE), 0)
      }
    }
  }

  compare_to_panel(inputs$liquidity_m_df, "liquidity_m_df")
  compare_to_panel(inputs$volatility_m_df, "volatility_m_df")
  compare_to_panel(inputs$stock_groups_m_df, "stock_groups_m_df")

  ##presence is a percentage, so its ceiling is 100 rather than whatever sentinel is convenient
  liquidity <- inputs$liquidity_m_df@data
  expect_lte(max(liquidity$presence[liquidity$tickers == risk_targeted_residual]), 100)

  ##The residual is not a benchmark constituent, so its benchmark weight has to be exactly zero.
  ##Anything else would net against its own position and silently shrink the active bet.
  bench <- inputs$benchmark_weights_m_df@data
  expect_true(all(bench$ibov[bench$tickers == risk_targeted_residual] == 0))

  ##and the benchmark still sums to one over the constituents that remain
  sums <- tapply(bench$ibov, bench$dates, sum)
  expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-8)
})

test_that("the residual is never selected by the risky sleeve itself", {
  #The sleeve allocates across stocks; the residual is the thing the sleeve is blended against. If
  #the sleeve could hold it, the blend would be double counted.
  sleeve <- risk_targeted_cohort()@port_backtest_results_list[[1]]
  held <- sleeve@port_weights_m_df@data %>% dplyr::filter(eop_port_weights > 0)
  expect_false(risk_targeted_residual %in% unique(held$tickers))
})


# What the targeting rule is supposed to buy ------------------------------

test_that("blending toward the residual lowers realised tracking error", {

  #The point of the exercise. With a residual that tracks the benchmark, holding less of the
  #sleeve has to bring the portfolio closer to the benchmark, not further from it. The assertion
  #fails outright when the residual is cash-like, which is how the fixture's original
  #constant-return residual was caught: a constant-return residual has a tracking error equal to
  #the benchmark's own volatility, so blending toward it is a large underweight of the market.
  results <- run_risk_targeted()
  inner <- results@meta_port_backtest_results
  sleeve <- results@port_backtest_cohort@port_backtest_results_list[[1]]

  #The comparison is made on the active return series rather than on ann_track_err from
  #port_stats_m_df. That column is a rolling figure over each portfolio's own history, and the
  #sleeve's history starts three months before the meta portfolio's, so reading both at a shared
  #date compares different windows. It also carries one row per return basis, so pulling the
  #column without filtering mixes net and raw.
  meta_returns <- inner@port_returns_m_xts@data
  sleeve_returns <- sleeve@port_returns_m_xts@data
  shared <- intersect(as.character(zoo::index(meta_returns)),
                      as.character(zoo::index(sleeve_returns)))
  expect_gt(length(shared), 1)

  meta_active <- as.numeric(
    meta_returns[as.character(zoo::index(meta_returns)) %in% shared, "net_active_return"])
  sleeve_active <- as.numeric(
    sleeve_returns[as.character(zoo::index(sleeve_returns)) %in% shared, "net_active_return"])

  expect_lt(stats::sd(meta_active, na.rm = TRUE), stats::sd(sleeve_active, na.rm = TRUE))

  ##Mean absolute active return says the same thing with far less sensitivity to the handful of
  ##observations a toy panel provides
  expect_lt(mean(abs(meta_active), na.rm = TRUE), mean(abs(sleeve_active), na.rm = TRUE))

  ##and on raw active returns too, so the result is not an artefact of the two cost profiles
  meta_raw <- as.numeric(
    meta_returns[as.character(zoo::index(meta_returns)) %in% shared, "raw_active_return"])
  sleeve_raw <- as.numeric(
    sleeve_returns[as.character(zoo::index(sleeve_returns)) %in% shared, "raw_active_return"])
  expect_lt(mean(abs(meta_raw), na.rm = TRUE), mean(abs(sleeve_raw), na.rm = TRUE))

  ##The sleeve was genuinely cut, or the comparison is vacuous
  risky_weight <- results@meta_port_stats_m_df@data$risky_weight
  expect_true(all(risky_weight < 1))
})


# A two-portfolio cohort --------------------------------------------------
# The smallest cross-section the multi-portfolio path accepts, and the one where the cross
# sectional machinery behaves least like an allocation: signal_transform runs over a pair the same
# way it runs over a hundred names, so the weights it produces barely depend on how far apart the
# two scores are.

port_meta_pair_cohort <- function() {
  if (!is.null(port_meta_cache$pair_cohort)) return(port_meta_cache$pair_cohort)

  inputs <- port_meta_inputs()
  build_config <- function(method, name) {
    create_port_backtest_config(
      chosen_score_metric_and_position = c(book_yield = "long"),
      eligibility_quantile_range = c(0.67, 1.0), selected_benchmark = "ibov",
      initial_buffer_period = 2, rebalancing_months = c(1, 4),
      port_construction_method = method, main_liquidity_metric = "mean_volfin_3m",
      config_name = name) %>%
      add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                       lambda = "dynamic", strategy_aum = 25000)
  }
  run_one <- function(config) suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))

  cohort <- suppressWarnings(suppressMessages(create_port_backtest_cohort(
    list(run_one(build_config("ew", "ew_pair")), run_one(build_config("sw", "sw_pair"))),
    cohort_name = "meta_pair_cohort")))

  port_meta_cache$pair_cohort <- cohort
  cohort
}


test_that("a two-portfolio meta backtest reproduces a step-by-step reconstruction of its chain", {

  #The same reconstruction the three-portfolio test does, over the smallest cohort the path
  #accepts. Nothing about the chain should change when the cross-section is a pair, and this is
  #where an off-by-one in the ranking or the covariance would be easiest to hide.
  cohort <- port_meta_pair_cohort()
  config <- port_meta_config()
  inner_config <- config@meta_port_backtest_config
  inputs <- port_meta_inputs()

  results <- suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, port_backtest_cohort = cohort,
    benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))

  #Stage 1: the port universe
  expected_universe <- suppressWarnings(suppressMessages(derive_port_universe_m_df(
    port_backtest_cohort = cohort,
    return_basis = config@return_basis,
    cost_lookback = config@cost_lookback,
    verbose = FALSE)))
  expect_equal(results@port_universe_m_df@data, expected_universe@data)
  expect_equal(length(unique(expected_universe@data$tickers)), 2L)

  #Stage 2: the meta weights, rebuilt one rebalance date at a time
  base_returns_xts <- cohort@port_returns_m_xts_list$net_returns_m_xts@data
  base_portfolio_names <- sort(unique(expected_universe@data$tickers))
  bench_returns_xts <- base_returns_xts[, "selected_bench_return", drop = FALSE]
  base_returns_xts <- base_returns_xts[, base_portfolio_names, drop = FALSE]
  cov_est_method <- inner_config@cov_est_method

  meta_rebalance_dates <- sort(unique(results@meta_port_weights_m_df@data$dates))
  reconstructed_weights <- list()

  for (i in seq_along(meta_rebalance_dates)) {
    current_date <- meta_rebalance_dates[i]
    universe_m_d_ref <- expected_universe@data %>% dplyr::filter(dates == current_date)

    scored_m_d_ref <- derive_stock_universe_m_d_ref(
      signals_m_d_ref = universe_m_d_ref,
      oos_predictions_m_d_ref = NULL,
      chosen_score_metric_and_position = inner_config@chosen_score_metric_and_position,
      chosen_scaler = inner_config@chosen_scaler,
      scaler_m_d_ref = NULL,
      scaler_shrinkage = if (is.null(inner_config@scaler_shrinkage)) 0 else inner_config@scaler_shrinkage,
      lower_quantile_winsorization = 0.025,
      upper_quantile_winsorization = 0.975)

    classified_m_d_ref <- classify_investment_universe(
      universe_m_d_ref = scored_m_d_ref,
      eligibility_quantile_range = inner_config@eligibility_quantile_range,
      min_eligible_assets_fallback = inner_config@min_eligible_assets_fallback,
      use_raw_for_eligibility = FALSE,
      asset_object = "stocks",
      verbose = FALSE)

    returns_upd_ref <- base_returns_xts[zoo::index(base_returns_xts) <= current_date, , drop = FALSE]
    bench_upd_ref <- bench_returns_xts[zoo::index(bench_returns_xts) <= current_date, , drop = FALSE]

    meta_port <- suppressWarnings(suppressMessages(set_portfolio_weights(
      universe_m_d_ref = classified_m_d_ref,
      port_construction_method = inner_config@port_construction_method,
      covariance_matrix = NULL,
      eligible_returns_m_xts_upd_ref = returns_upd_ref,
      selected_benchmark_m_xts_upd_ref = bench_upd_ref,
      active_returns = cov_est_method@active_returns,
      cov_estimation_method = cov_est_method@cov_estimation_method,
      cov_matrix_sample_size = cov_est_method@cov_matrix_sample_size,
      top_down_proxy_port_method = "ew", mmaf_group_col = NULL,
      selected_benchmark = NULL,
      lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
      parallel = FALSE, verbose = FALSE)))

    reconstructed_weights[[i]] <- meta_port@universe_m_d_ref@data %>%
      dplyr::select(id, tickers, dates, weights)
  }

  reconstructed_weights <- do.call(rbind, reconstructed_weights) %>%
    dplyr::arrange(id) %>%
    as.data.frame()
  rownames(reconstructed_weights) <- NULL

  expect_equal(results@meta_port_weights_m_df@data, reconstructed_weights)

  ##Both sleeves are held and the pair is fully invested
  sums <- tapply(reconstructed_weights$weights, reconstructed_weights$dates, sum)
  expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-10)
  expect_equal(length(unique(reconstructed_weights$tickers)), 2L)

  #Stage 3: the projection onto stocks
  expected_projection <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = reconstructed_weights,
    port_backtest_cohort = cohort,
    signals_m_df = inputs$signals_m_df,
    verbose = FALSE))
  expect_equal(results@projected_stock_weights_m_df@data, expected_projection@data)

  #Stage 4: the stock-level backtest run on those projected weights
  expected_inner <- suppressWarnings(suppressMessages(run_port_backtest_internal(
    signals_m_df = inputs$signals_m_df@data,
    oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = NULL,
    rebalancing_months = inner_config@rebalancing_months,
    initial_buffer_period = inner_config@initial_buffer_period,
    port_construction_method = "custom_weights",
    selected_benchmark = inner_config@selected_benchmark,
    eligibility_quantile_range = inner_config@eligibility_quantile_range,
    exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL, mmaf_group_col = NULL,
    cov_estimation_method = cov_est_method@cov_estimation_method,
    cov_matrix_sample_size = cov_est_method@cov_matrix_sample_size,
    active_returns = cov_est_method@active_returns,
    cov_matrix_benchmark = cov_est_method@cov_matrix_benchmark,
    daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts@data,
    liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL,
    concentration_constraint_policy = NULL,
    liquidity_m_df = inputs$liquidity_m_df@data,
    main_liquidity_metric = inner_config@main_liquidity_metric,
    liquidity_floor_cutoffs = inner_config@liquidity_floor_cutoffs,
    volatility_m_df = inputs$volatility_m_df@data,
    fwd_return_m_df = inputs$fwd_return_m_df@data,
    stock_groups_m_df = NULL,
    benchmark_weights_m_df = inputs$benchmark_weights_m_df@data,
    transaction_costs_parameters = as.list(inner_config@transaction_costs_parameters),
    custom_stock_weights_m_df = expected_projection@data,
    custom_stock_metrics_m_df = NULL,
    lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
    verbose = FALSE, parallel = FALSE)))

  expect_equal(results@meta_port_backtest_results@port_weights_m_df@data,
               expected_inner@port_weights_m_df@data)
  expect_equal(results@meta_port_backtest_results@port_returns_m_xts@data,
               expected_inner@port_returns_m_xts@data)
  expect_equal(results@meta_port_backtest_results@port_costs_m_xts@data,
               expected_inner@port_costs_m_xts@data)

  expect_true(any(is.finite(expected_inner@port_returns_m_xts@data$net_return)))
})

test_that("a pair under signal weighting is warned about, because the gap barely reaches it", {
  #signal_transform runs over a pair the same way it runs over a full cross-section, so the two
  #weights are nearly fixed however far apart the scores are. That is worth a warning rather than
  #a refusal, since the allocation is still well defined.
  inputs <- port_meta_inputs()
  expect_warning(
    suppressMessages(run_port_backtest(
      signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = port_meta_config(), port_backtest_cohort = port_meta_pair_cohort(),
      benchmark_weights_m_df = inputs$benchmark_weights_m_df,
      benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
      verbose = FALSE, parallel = FALSE)),
    "gap"
  )
})


# The meta level is not benchmark-relative --------------------------------

test_that("the meta universe carries no benchmark weights, and the benchmark reaches the covariance", {

  #A base portfolio is not a benchmark constituent, so there is nothing to net its weight against
  #and the meta allocation is not benchmark-relative: selected_benchmark is NULL when the meta
  #weights are set. The benchmark series is not ignored, though. It still feeds the covariance
  #estimate when active returns are configured, which is what the covariance-based methods
  #construct from.
  cohort <- port_meta_cohort()
  config <- port_meta_config()

  universe <- suppressWarnings(suppressMessages(derive_port_universe_m_df(
    port_backtest_cohort = cohort, return_basis = config@return_basis,
    cost_lookback = config@cost_lookback, verbose = FALSE)))

  ##Nothing in the meta universe describes a benchmark position
  expect_false(any(grepl("bench", names(universe@data), ignore.case = TRUE)))

  ##nor in the meta weights the run produces, unlike a stock-level port_weights_m_df, which
  ##carries bench_weights alongside eop_port_weights whenever a benchmark is set
  results <- run_port_meta_backtest()
  expect_false(any(grepl("bench", names(results@meta_port_weights_m_df@data),
                         ignore.case = TRUE)))
  expect_true("bench_weights" %in%
                names(results@meta_port_backtest_results@port_weights_m_df@data))

  ##Under risk parity the covariance is what the weights are built from, and the benchmark reaches
  ##it through active returns, so the two bases must not produce the same allocation
  active <- run_port_meta_backtest(config = port_meta_config(
    port_construction_method = "rp", active_returns = TRUE))
  absolute <- run_port_meta_backtest(config = port_meta_config(
    port_construction_method = "rp", active_returns = FALSE))

  expect_false(isTRUE(all.equal(active@meta_port_weights_m_df@data$weights,
                                absolute@meta_port_weights_m_df@data$weights)))

  ##and both are real allocations rather than one of them collapsing
  expect_true(all(is.finite(active@meta_port_weights_m_df@data$weights)))
  expect_true(all(is.finite(absolute@meta_port_weights_m_df@data$weights)))
})


# Every meta plot renders -------------------------------------------------
# The fixtures live in this file, so the smoke tests do too. A plot method is only exercised when
# it is drawn: a column renamed out from under it, or a slot that is NULL on one path and not the
# other, shows up here and nowhere else.

test_that("every multi-portfolio meta plot renders", {
  results <- run_port_meta_backtest()

  ##A null device, so the tests draw without writing files
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  plot_names <- c(
    "Meta Weights Over Time",
    "Meta vs Base Cumulative Returns",
    "Meta vs Base Costs and Turnover",
    "Meta vs Base Portfolio Stats",
    "Meta Score vs Meta Weight")

  for (plot_name in plot_names) {
    expect_no_error(suppressWarnings(suppressMessages(
      plot(results, plot_id = plot_name))))
  }

  ##and by index, since the menu resolves either way
  expect_no_error(suppressWarnings(suppressMessages(plot(results, plot_id = 1))))

  ##An unknown name is refused rather than silently drawing nothing
  expect_error(plot(results, plot_id = "Not A Plot"), "Invalid 'plot_id'")
})

test_that("every risk-targeted meta plot renders", {
  results <- run_risk_targeted()

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  plot_names <- c(
    "Meta Weights Over Time",
    "Meta vs Base Cumulative Returns",
    "Meta vs Base Costs and Turnover",
    "Meta vs Base Portfolio Stats",
    "Capital Market Line",
    "Risky Weight vs Sleeve Risk",
    "Realised vs Target Risk")

  for (plot_name in plot_names) {
    expect_no_error(suppressWarnings(suppressMessages(
      plot(results, plot_id = plot_name, rolling_window = 2))))
  }
})

test_that("the exposure plot renders only when a signal was used", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  ##With a signal there are two factors to separate, so the plot has something to say
  with_signal <- run_risk_targeted(
    risk_targeted_config(exposure_method = "trend", exposure_center = 0.75,
                         exposure_sensitivity = 0.25),
    exposure_m_df = risk_targeted_exposure_metric())
  expect_no_error(suppressWarnings(suppressMessages(
    plot(with_signal, plot_id = "Exposure and Risk Ratio", rolling_window = 2))))
})

test_that("the cumulative return plot starts every series at the meta portfolio's own first date", {

  #The meta portfolio starts later than its bases, and compounding each from its own start would
  #hand the bases a head start that has nothing to do with the allocation.
  results <- run_port_meta_backtest()
  meta_start <- min(zoo::index(results@meta_port_backtest_results@port_returns_m_xts@data))
  base_returns <- results@port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts@data
  base_start <- min(zoo::index(base_returns))

  ##The premise of the fix: the bases really do start earlier, so the plot has to cut them
  expect_lt(base_start, meta_start)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(suppressWarnings(suppressMessages(
    plot(results, plot_id = "Meta vs Base Cumulative Returns"))))
})

test_that("sleeve labels are numbered keys with the mapping kept alongside", {

  #Backtest identifiers run to fifty characters, which crowds a legend out of the panel, so the
  #plots key them by number and print the mapping to the console
  labels <- .meta_sleeve_labels(c("c__long_name_two", "c__long_name_one"))

  expect_equal(unname(labels$map), c("1", "2"))
  ##Sorted, so the key a portfolio gets does not depend on the order the cohort was built in
  expect_equal(names(labels$map), c("c__long_name_one", "c__long_name_two"))
  expect_equal(labels$labels, c(1L, 2L))
  expect_equal(labels$names, c("c__long_name_one", "c__long_name_two"))
})


# Adding the passive ETF is what lowers tracking error --------------------

test_that("tracking error falls monotonically as the passive ETF takes more of the portfolio", {

  #The claim the whole risk-targeted path rests on, demonstrated rather than asserted at a single
  #weight. Pinning min_weight and max_weight together fixes the split, so the only thing that
  #changes across the runs is how much of the portfolio sits in the ETF. If the residual really
  #replicates the benchmark, realised tracking error has to fall as its share rises.
  fixed_weight_run <- function(weight) {
    run_risk_targeted(risk_targeted_config(min_weight = weight, max_weight = weight))
  }

  risky_shares <- c(1, 0.6, 0.2)
  runs <- lapply(risky_shares, fixed_weight_run)

  ##The rule really was pinned, so each run holds the share it was asked to hold
  for (i in seq_along(risky_shares)) {
    expect_equal(unique(runs[[i]]@meta_port_stats_m_df@data$risky_weight), risky_shares[i],
                 tolerance = 1e-10)
  }

  sleeve <- runs[[1]]@port_backtest_cohort@port_backtest_results_list[[1]]
  sleeve_returns <- sleeve@port_returns_m_xts@data

  ##Compared over the dates every run covers, so the windows are identical
  shared <- Reduce(intersect, c(
    list(as.character(zoo::index(sleeve_returns))),
    lapply(runs, function(r)
      as.character(zoo::index(r@meta_port_backtest_results@port_returns_m_xts@data)))))
  expect_gt(length(shared), 1)

  active_of <- function(returns_xts, column = "net_active_return") {
    as.numeric(returns_xts[as.character(zoo::index(returns_xts)) %in% shared, column])
  }
  tracking_error <- function(values) stats::sd(values, na.rm = TRUE) * sqrt(12)

  realised <- vapply(runs, function(r)
    tracking_error(active_of(r@meta_port_backtest_results@port_returns_m_xts@data)), numeric(1))

  ##Falling in the ETF's share, which is the point
  expect_true(all(diff(realised) < 0))

  ##and the fall is material rather than a rounding artefact
  expect_lt(realised[length(realised)], realised[1] / 2)

  ##At a full risky weight the meta portfolio is the sleeve, so it has to track the sleeve's own
  ##active returns closely. Costs differ slightly because the blend is rebalanced through a
  ##different set of trades, so this is a correlation rather than an equality.
  sleeve_active <- active_of(sleeve_returns)
  expect_gt(stats::cor(active_of(runs[[1]]@meta_port_backtest_results@port_returns_m_xts@data),
                       sleeve_active), 0.95)

  ##and the same ordering holds on raw active returns, so it is not a cost artefact
  realised_raw <- vapply(runs, function(r)
    tracking_error(active_of(r@meta_port_backtest_results@port_returns_m_xts@data,
                             "raw_active_return")), numeric(1))
  expect_true(all(diff(realised_raw) < 0))
})

test_that("the residual is not a base portfolio, so it has no row in the port universe", {

  #The meta weights span the sleeve and the residual, while port_universe_m_df spans only the
  #cohort. The asymmetry is deliberate: the universe carries the realised and ex-ante statistics
  #of already-backtested portfolios, and the residual has no backtest behind it, so every column
  #would be missing. Anything that reads the two together has to be told about the residual
  #separately, which is why project_meta_weights_to_stocks takes residual_ticker as an argument.
  results <- run_risk_targeted()

  universe_tickers <- unique(results@port_universe_m_df@data$tickers)
  weight_tickers <- unique(results@meta_port_weights_m_df@data$tickers)

  expect_false(risk_targeted_residual %in% universe_tickers)
  expect_true(risk_targeted_residual %in% weight_tickers)
  expect_true(all(universe_tickers %in% weight_tickers))

  ##The universe is exactly the cohort, no more and no less
  cohort_names <- vapply(results@port_backtest_cohort@port_backtest_results_list,
                         function(base) base@backtest_identifier, character(1))
  expect_setequal(universe_tickers, cohort_names)

  ##and the residual still reaches the portfolio, through the projection rather than the universe
  projected <- results@projected_stock_weights_m_df@data
  expect_true(any(projected$tickers == risk_targeted_residual & projected$weights > 0))
})


# The residual is checked against the benchmark, not assumed to track it ---

test_that("a cash-like residual is caught by the tracking-error check", {

  #The check that this branch is not dead. A constant-return residual has a tracking error equal to
  #the benchmark's own volatility, so it is the furthest any asset can sit from the index while
  #still being paired with a tracking-error target. Blending toward it raises tracking error rather
  #than lowering it, and nothing else in the run would say so.
  inputs <- risk_targeted_inputs()
  cash_fwd <- inputs$fwd_return_m_df@data
  is_residual <- cash_fwd$tickers == risk_targeted_residual
  last_date <- max(cash_fwd$dates)
  cash_fwd$fwd_return_1m[is_residual] <- 0.8
  cash_fwd$fwd_return_1m[is_residual & cash_fwd$dates == last_date] <- NA_real_

  cash_inputs <- inputs
  cash_inputs$fwd_return_m_df <- suppressWarnings(suppressMessages(create_meta_dataframe(
    cash_fwd, meta_dataframe_name = inputs$fwd_return_m_df@meta_dataframe_name, type = "target")))

  expect_warning(
    suppressMessages(run_port_backtest(
      signals_m_df = cash_inputs$signals_m_df, fwd_return_m_df = cash_inputs$fwd_return_m_df,
      liquidity_m_df = cash_inputs$liquidity_m_df, volatility_m_df = cash_inputs$volatility_m_df,
      config = risk_targeted_config(), port_backtest_cohort = risk_targeted_cohort(),
      benchmark_weights_m_df = cash_inputs$benchmark_weights_m_df,
      benchmark_returns_m_xts = cash_inputs$benchmark_returns_m_xts,
      daily_stock_returns_m_xts = cash_inputs$daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = cash_inputs$daily_bench_returns_m_xts,
      stock_groups_m_df = cash_inputs$stock_groups_m_df,
      verbose = FALSE, parallel = FALSE)),
    "does not track")
})

test_that("a replicating residual passes the check without complaint", {

  #The other half of the guard. A warning that fires for every configuration is no guard at all,
  #which is what the previous version amounted to: it read a workflow element nothing ever wrote,
  #so the NULL branch was the only one reachable.
  inputs <- risk_targeted_inputs()

  warnings_raised <- character()
  withCallingHandlers(
    suppressMessages(run_port_backtest(
      signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = risk_targeted_config(), port_backtest_cohort = risk_targeted_cohort(),
      benchmark_weights_m_df = inputs$benchmark_weights_m_df,
      benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
      daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = inputs$daily_bench_returns_m_xts,
      stock_groups_m_df = inputs$stock_groups_m_df,
      verbose = FALSE, parallel = FALSE)),
    warning = function(w) {
      warnings_raised <<- c(warnings_raised, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

  expect_false(any(grepl("does not track", warnings_raised)))
  expect_false(any(grepl("whether the residual tracks", warnings_raised)))
})


test_that("supplied weights cannot carry constraint policies that would be ignored", {

  #The custom-weight route returns before the eligibility cascade, so a policy attached alongside
  #it never runs. A constraint that does not bind and one that was silently discarded look the
  #same from the outside, which is what makes this worth refusing at construction.
  base_config <- function(...) {
    create_port_backtest_config(
      chosen_score_metric_and_position = NULL,
      eligibility_quantile_range = c(0, 1),
      initial_buffer_period = 2, rebalancing_months = c(1, 4),
      selected_benchmark = "ibov",
      main_liquidity_metric = "mean_volfin_3m",
      port_construction_method = "custom_weights", config_name = "cw", ...)
  }

  expect_error(
    base_config() %>%
      add_liquidity_floor_cutoffs(
        metric_name = "mean_volfin_3m",
        metric_cutoffs = list(c(micro_caps = 1, small_caps = 50000, mid_caps = 100000,
                                large_caps = 200000, mega_caps = 500000))) %>%
      add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps"),
    "cannot be combined with port_construction_method 'custom_weights'")

  #and the same config is fine without one
  expect_s4_class(base_config(), "port_backtest_config")
})

test_that("the risk-targeted route checks its inputs against the cohort", {

  #These identity checks used to sit after the meta-score section, which the risk-targeted route
  #returned before reaching. A cohort built from one set of inputs could then be executed against
  #another without complaint.
  inputs <- risk_targeted_inputs()
  wrong_signals <- inputs$signals_m_df
  wrong_signals@meta_dataframe_name <- "some_other_signals"

  expect_error(
    suppressWarnings(suppressMessages(run_port_backtest(
      signals_m_df = wrong_signals, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = risk_targeted_config(), port_backtest_cohort = risk_targeted_cohort(),
      benchmark_weights_m_df = inputs$benchmark_weights_m_df,
      benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
      daily_stock_returns_m_xts = inputs$daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = inputs$daily_bench_returns_m_xts,
      stock_groups_m_df = inputs$stock_groups_m_df,
      verbose = FALSE, parallel = FALSE))),
    "Object name mismatch")
})
