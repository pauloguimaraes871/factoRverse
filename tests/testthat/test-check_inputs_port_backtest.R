test_that("check_inputs_port_backtest throws an error when trying to choose a signal not present in signals_m_df", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))
  daily_stock_returns_m_xts <- daily_returns_m_xts
  n <- nrow(daily_stock_returns_m_xts)
  daily_benchmark_returns_m_xts <-
    xts::xts(data.frame(ibov = rnorm(n,
                                     mean = mean(daily_stock_returns_m_xts$`Stock D`, na.rm = TRUE),
                                     sd = sd(daily_stock_returns_m_xts$`Stock D`, na.rm = TRUE)),
                        smll = rnorm(n,
                                     mean = mean(daily_stock_returns_m_xts$`Stock D`, na.rm = TRUE),
                                     sd = sd(daily_stock_returns_m_xts$`Stock D`, na.rm = TRUE))),
             order.by = zoo::index(daily_stock_returns_m_xts))
  transaction_costs_parameters <- transaction_costs_list
  remove(transaction_costs_list)
  remove(n)





  chosen_score_metric_and_position <- c(roe_3m = "long", vol_36m = "short")

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = wrong_chosen_score_metric_and_position,
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    eligibility_quantile_range = c(0.5, 0.75),
    daily_stock_returns_m_xts = daily_returns_m_xts,
    daily_benchmark_returns_m_xts = daily_benchmark_returns_m_xts

)

}
