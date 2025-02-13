test_that("run_port_backtest works for a simple sw single signal strategy", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Create port_backtest_config
  chosen_score_metric_and_position <- c(roe_3m = "long")
  port_config <- create_port_backtest_config(chosen_score_metric_and_position = chosen_score_metric_and_position,
                                             eligibility_quantile_range = c(0.67, 1.0),
                                             selected_benchmark = "ibov",
                                             initial_buffer_period = 4,
                                             rebalancing_months = 6,
                                             port_construction_method = "sw",
                                             main_liquidity_metric = "mean_volfin_3m",
                                             config_name = "guara_model"
                                             ) %>%
    add_liquidity_floor_cutoffs(
      metric_name = c("mean_volfin_3m", "presence"),
      metric_cutoffs = list(
        c(micro_caps = 1, small_caps = 100, mid_caps = 5000, large_caps = 80000, mega_caps = 1000000),
        c(micro_caps = 97.5, small_caps = 99, mid_caps = 100, large_caps = 100, mega_caps = 100)
      )
    ) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1, lambda = "dynamic", strategy_aum = 25000)

  #meta_dataframes
  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_returns_m_xts <- create_meta_xts(benchmark_returns_m_xts)

  #Run port_backtest
  results <- run_port_backtest(signals_m_df = signals_m_df,
                               fwd_return_m_df = fwd_return_m_df,
                               port_config = port_config,
                               liquidity_m_df = liquidity_m_df,
                               volatility_m_df = volatility_m_df,
                               config = port_config,
                               benchmark_returns_m_xts = benchmark_returns_m_xts,
                               verbose = TRUE)

  #Expected results
  colnames(results@port_weights_m_df@data)












})
