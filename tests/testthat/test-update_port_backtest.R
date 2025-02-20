test_that("update_port_backtest works for a regular single_signal (n_update = 1)", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df %>% dplyr::filter(!dates == "2023-04-15"), type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df %>% dplyr::filter(!dates == "2023-04-15"), type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "liq")
  volatility_m_df <- create_meta_dataframe(volatility_m_df %>% dplyr::filter(!dates == "2023-04-15"), meta_dataframe_name = "vol")
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df %>% dplyr::filter(!dates == "2023-04-15"), type = "weights", meta_dataframe_name = "bench_weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts["2022-10-15/2023-04-15"], asset_type = "benchmark", meta_xts_name = "bench_returns")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data %>% dplyr::filter(!dates == "2023-04-15"), "stock_metrics")

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



  #Run a new backtest
  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals", meta_dataframe_name = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target", meta_dataframe_name = "fwd")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df, meta_dataframe_name = "liq")
  volatility_m_df <- create_meta_dataframe(volatility_m_df, meta_dataframe_name = "vol")
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights", meta_dataframe_name = "bench_weights")
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts, asset_type = "benchmark", meta_xts_name = "bench_returns")
  port_metrics_m_df <- create_meta_dataframe(signals_m_df@data, "stock_metrics")

  #Create port_backtest_config 1
  updated_sw_results <- update_port_backtest(signals_m_df = signals_m_df,
                                             fwd_return_m_df = fwd_return_m_df,
                                             liquidity_m_df = liquidity_m_df,
                                             volatility_m_df = volatility_m_df,
                                             results = sw_results,
                                             benchmark_weights_m_df = benchmark_weights_m_df,
                                             benchmark_returns_m_xts = benchmark_returns_m_xts,
                                             verbose = TRUE,
                                             n_update = 1
                                             )




})
