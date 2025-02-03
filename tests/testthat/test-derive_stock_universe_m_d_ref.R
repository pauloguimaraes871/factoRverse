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
