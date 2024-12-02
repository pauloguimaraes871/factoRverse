test_that("check_inputs_ss_backtest throws an error when trying to choose a signal not present in signals_m_df", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals <- c(signal_selection_policy$chosen_signals, "Delta")
  signal_positions <- c(signal_selection_policy$signal_positions, Delta = "short")

  expect_error(check_inputs_ss_backtest(chosen_signals = chosen_signals, signal_positions = signal_positions, signals_m_df = signals_m_df),
               "signal selection not avaiable in signals_m_df")

})

test_that("check_inputs_ss_backtest throws an error when trying to choose a signal not present in signals_m_df ", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals <- c(signal_selection_policy$chosen_signals, "vega")
  signal_positions <- signal_selection_policy$signal_positions
  data_availability_cutoff <- signal_selection_policy$data_availability_cutoff
  priors_m_df <- priors_m_df_list$jkp_emerging
  p_correction_method <- signal_selection_policy$p_correction_method
  rebalancing_months <- 6



  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals = chosen_signals, signal_positions = signal_positions,
                             backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                             signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "signal selection not avaiable in signals_m_df"
  )


})

test_that("check_inputs_ss_backtest throws an error when when chosen_signals do not match signal_positions", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals <- signal_selection_policy$chosen_signals[-2]
  signal_positions <- signal_selection_policy$signal_positions
  data_availability_cutoff <- signal_selection_policy$data_availability_cutoff
  priors_m_df <- priors_m_df_list$jkp_emerging
  p_correction_method <- signal_selection_policy$p_correction_method
  rebalancing_months <- 6



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals = chosen_signals, signal_positions = signal_positions,
                                            backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                            signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                                            rebalancing_months = 6),
                                          "all chosen signals should have a matching position in signal_positions.")

})

test_that("check_inputs_ss_backtest thrown an error when trying to choose a signal more than once", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals <- c(signal_selection_policy$chosen_signals, "Beta")
  signal_positions <- c(signal_selection_policy$signal_positions, Beta = "long")
  data_availability_cutoff <- signal_selection_policy$data_availability_cutoff
  priors_m_df <- priors_m_df_list$jkp_emerging
  p_correction_method <- signal_selection_policy$p_correction_method
  rebalancing_months <- 6

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals = chosen_signals, signal_positions = signal_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "each signal must be chosen only once")



})
