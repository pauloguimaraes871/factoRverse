test_that("check_inputs_ss_backtest throws an error when trying to choose a signal not present in signals_m_df", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", Delta = "short")

  expect_error(check_inputs_ss_backtest(chosen_signals_and_positions = chosen_signals_and_positions,
                                        signals_m_df = signals_m_df),
               "signal selection not avaiable in signals_m_df")

})

test_that("check_inputs_ss_backtest throws an error when trying to choose a signal not present in signals_m_df ", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", vega = "short")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6



  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                             enable_theme_representativeness = TRUE, benchmark_returns_df = benchmark_returns_df,
                             signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "signal selection not avaiable in signals_m_df"
  )


})

test_that("check_inputs_ss_backtest thrown an error when trying to choose a signal more than once", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", Beta = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6

  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "each signal must be chosen only once")


})

test_that("check_inputs_ss_backtest thrown an error when signal_themes_m_df has wrong format", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", Beta = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  signal_themes_m_df$theme[7:12] <- paste0("high_",  signal_themes_m_df$theme[7:12])

  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "No underscores allowed in signal_themes_m_df theme names")



  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", Beta = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  colnames(signal_themes_m_df)[4] <- "themes"

  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'")



  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = NULL, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "signal_themes_m_df must be provided if enable_theme_representativeness is TRUE")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  signal_themes_m_df[7:12,"tickers"] <- "Beta"
  signal_themes_m_df[7:12,"id"] <- paste0(signal_themes_m_df[7:12,"tickers"], "-", signal_themes_m_df[7:12,"dates"])

  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen_signals_and_positions with their corrected position should be present in signal_themes_m_df")

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  benchmark_returns_df <- benchmark_returns_df[1:5,]
  backtest_returns_df <- backtest_returns_df[1:5,]

  expect_error(check_inputs_ss_backtest(initial_sample_size = 3, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all dates in signals_m_df must be present in backtest_returns_df")

})

test_that("check_inputs_ss_backtest thrown an error when backtest_return_df or benchmark_return_df has wrong format", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  backtest_returns_df[3,] <- backtest_returns_df[5,]



  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6))


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  benchmark_returns_df[3,] <- benchmark_returns_df[5,]


  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "dates in benchmark_returns_df and backtest_returns_df must be the same")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 10
  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(initial_sample_size = 12, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_df must have at least data_availability_cutoff rows")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(initial_sample_size = 12, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_df must have at least initial_sample_size rows")



  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  backtest_returns_df <-  backtest_returns_df[c(3:6),]




  expect_error(check_inputs_ss_backtest(initial_sample_size = 3, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6))


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  backtest_returns_df <-  backtest_returns_df[c(3:6),]
  benchmark_returns_df <- benchmark_returns_df[c(3:6),]


  expect_error(check_inputs_ss_backtest(initial_sample_size = 3, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, market_factor_proxy = "IBOV"),
               "all dates in signals_m_df must be present in backtest_returns_df")

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "long", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen_signals_and_positions with their corrected position should be present in backtest_returns_df")



})

test_that("check_inputs_ss_backtest thrown an error when priors_m_df has wrong format", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6




  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_df = backtest_returns_df, data_availability_cutoff = data_availability_cutoff,
                                        benchmark_returns_df = benchmark_returns_df, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "No underscores allowed in signal_themes_m_df theme names")



})
