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

  p_correction_method <- "none"
  rebalancing_months <- 6



  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_xts = backtest_returns_xts,
                             enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                             signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "signal selection not avaiable in signals_m_df"
  )


})

test_that("check_inputs_ss_backtest throws an error when trying to run ss_backtest with only one signal ", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long")

  p_correction_method <- "none"
  rebalancing_months <- 6



  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_xts = backtest_returns_xts,
                             enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                             signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "More than one signal must be provided in order to run a ss_backtest"
  )


})

test_that("check_inputs_ss_backtest throws a warning when trying to run ss_backtest with categorical variables ", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short")

  signals_m_df$Alpha <- sample(c(0,1), size = nrow(signals_m_df), replace = TRUE)

  p_correction_method <- "none"
  rebalancing_months <- 6



  expect_warning(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                             enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                             market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                             signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                             signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "Categorical signals included in chosen_signals_and_positions."
  )


})

test_that("check_inputs_ss_backtest throws an error when forced_signals not present in signals_m_df ", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short")


  p_correction_method <- "none"
  rebalancing_months <- 6



  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_xts = backtest_returns_xts, forced_signals = c(Tau = "long"), initial_sample_size = 3,
                             enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                             market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                             signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                             signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "forced_signals not available in signals_m_df"
  )


})

test_that("check_inputs_ss_backtest thrown an error when trying to choose a signal more than once", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", Beta = "long")

  backtest_returns_xts$Beta <- backtest_returns_xts$Alpha
  signal_themes_m_df_beta <- signal_themes_m_df %>% dplyr::filter(tickers == "Alpha") %>% dplyr::mutate(tickers = "Beta")
  signal_themes_m_df <- dplyr::bind_rows(signal_themes_m_df, signal_themes_m_df_beta)

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6

  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                             enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                             market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                             signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                             signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "each signal must be chosen only once"
  )

})

test_that("check_inputs_ss_backtest thrown an error when backtest_return_xts or benchmark_return_xts has wrong format", {

  #Test for NAs in backtest_returns_xts
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  backtest_returns_xts[3,2] <- NA



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                            backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                            enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                            market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                            signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                            signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                            rebalancing_months = 6),
               "backtest_returns_xts must not have any NA"
               )

  #Test for consecutive dates
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  correct_dates <- zoo::index(backtest_returns_xts)
  wrong_dates <- correct_dates
  wrong_dates[2] <- correct_dates[1]

  backtest_returns_xts <- xts::xts(backtest_returns_xts, order.by = wrong_dates)
  signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-04-15") #Remove the date that is not in the backtest_returns_xts

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_xts must have consecutive dates"
  )

  #Test for consecutive dates
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  correct_dates <- zoo::index(backtest_returns_xts)

  wrong_backtest_returns_xts <- backtest_returns_xts[-2,]
  signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-04-15") #Remove the date that is not in the backtest_returns_xts

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = wrong_backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_xts must have consecutive dates"
  )

  #Test for backtest_returns_xts not having the same dates as signals_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_backtest_returns_xts <- backtest_returns_xts[-2,]

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = wrong_backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all dates in signals_m_df must be present in backtest_returns_xts"
  )

  #backtest_returns_xts can have more dates than signals_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-03-15")

  expect_no_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6)
  )



  #Dates in backtest_returns_xts and benchmark_returns_xts must match
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  short_benchmark_returns_xts <- benchmark_returns_xts[-1,]


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = short_benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "dates in benchmark_returns_xts and backtest_returns_xts must be the same")

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  short_backtest_returns_xts <- backtest_returns_xts[-1,]
  signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-03-15")


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = short_backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "dates in benchmark_returns_xts and backtest_returns_xts must be the same")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 10
  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(initial_sample_size = 12, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_xts must have at least data_availability_cutoff rows")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(initial_sample_size = 12, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_xts must have at least initial_sample_size rows")



  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  backtest_returns_xts <-  backtest_returns_xts[c(3:6),]




  expect_error(check_inputs_ss_backtest(initial_sample_size = 3, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6))


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6
  backtest_returns_xts <-  backtest_returns_xts[c(3:6),]
  benchmark_returns_xts <- benchmark_returns_xts[c(3:6),]


  expect_error(check_inputs_ss_backtest(initial_sample_size = 3, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, market_factor_proxy = "IBOV"),
               "all dates in signals_m_df must be present in backtest_returns_xts")

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "long", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen_signals_and_positions with their corrected position should be present in backtest_returns_xts")



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
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
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
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'")



  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
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
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
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
  benchmark_returns_xts <- benchmark_returns_xts[1:5,]
  backtest_returns_xts <- backtest_returns_xts[1:5,]

  expect_error(check_inputs_ss_backtest(initial_sample_size = 3, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all dates in signals_m_df must be present in backtest_returns_xts")

})



test_that("check_inputs_ss_backtest thrown an error when priors_m_df has wrong format", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  data_availability_cutoff <- 2
  p_correction_method <- "none"
  rebalancing_months <- 6




  expect_error(check_inputs_ss_backtest(initial_sample_size = 6, signals_m_df = signals_m_df,  chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts,
                                        benchmark_returns_xts = benchmark_returns_xts, enable_theme_representativeness = TRUE,
                                        signal_significance_threshold = 0.05,
                                        signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "No underscores allowed in signal_themes_m_df theme names")



})
