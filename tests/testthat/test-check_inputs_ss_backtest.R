test_that("check_inputs_ss_backtest throws an error when chosen_signals_and_positions is wrong", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", Delta = "short")

  expect_error(check_inputs_ss_backtest(chosen_signals_and_positions = chosen_signals_and_positions, forced_signals = NULL,
                                        signals_m_df = signals_m_df),
               "signal selection not avaiable in signals_m_df")


  chosen_signals_and_positions <- c(Alpha = "long", low_Beta = "long", Gamma = "long")

  expect_error(check_inputs_ss_backtest(chosen_signals_and_positions = chosen_signals_and_positions, forced_signals = NULL,
                                        signals_m_df = signals_m_df),
               "chosen_signals_and_positions should not contain 'low_'.")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", vega = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6



  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL,
                             enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "signal selection not avaiable in signals_m_df"
  )


})

test_that("check_inputs_ss_backtest throws an error when signals_m_df contain low_", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  p_correction_method <- "none"
  rebalancing_months <- 6

  colnames(signals_m_df)[4] <- c("low_Alpha")


  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL,
                             enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "signals_m_df column names should not contain 'low_'."
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
                             backtest_returns_m_xts = backtest_returns_m_xts,
                             enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
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
                             backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                             enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                             signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                             signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "Categorical signals included in chosen_signals_and_positions."
  )


})

test_that("check_inputs_ss_backtest throws an error when forced_signals are wrong ", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short")


  p_correction_method <- "none"
  rebalancing_months <- 6

  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = c(Tau = "long"), initial_sample_size = 3,
                             enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                             signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                             signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "forced_signals not available in signals_m_df"
  )

  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = c(Alpha = "neutral"), initial_sample_size = 3,
                             enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                             signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                             signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "forced_signals should be 'force'"
  )

  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = c(Alpha = "long", Alpha = "short"), initial_sample_size = 3,
                             enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                             signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                             signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "each forced signal must be chosen only once"
  )


})

test_that("check_inputs_ss_backtest thrown an error when trying to choose a signal more than once", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long", Beta = "long")

  backtest_returns_m_xts$Beta <- backtest_returns_m_xts$Alpha
  signal_themes_m_df_beta <- signal_themes_m_df %>% dplyr::filter(tickers == "Alpha") %>% dplyr::mutate(tickers = "Beta")
  signal_themes_m_df <- dplyr::bind_rows(signal_themes_m_df, signal_themes_m_df_beta)


  p_correction_method <- "none"
  rebalancing_months <- 6

  expect_error(
    check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                             backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                             enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                             signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                             signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                             rebalancing_months = 6),
    "each signal must be chosen only once"
  )

})

test_that("check_inputs_ss_backtest thrown an error when backtest_return_m_xts or benchmark_return_m_xts have wrong format", {

  #all dates of backtest after initial sample size must be present in signals
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6

  signals_m_df <- signals_m_df %>% dplyr::filter(!dates %in% c("2001-08-15"))



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all backtest_dates from initial_sample_size onwards must be present in signals_m_df"
  )

  #Test for NAs in backtest_returns_m_xts
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  backtest_returns_m_xts[3,2] <- NA



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                            backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                            enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                            market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                            signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                            signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                            rebalancing_months = 6),
               "backtest_returns_m_xts must not have any NA"
               )

  #Test for consecutive dates
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  correct_dates <- zoo::index(backtest_returns_m_xts)
  wrong_dates <- correct_dates
  wrong_dates[2] <- correct_dates[1]

  backtest_returns_m_xts <- xts::xts(backtest_returns_m_xts, order.by = wrong_dates)
  signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-04-15") #Remove the date that is not in the backtest_returns_m_xts

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_m_xts must have consecutive dates"
  )

  #Test for consecutive dates
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  correct_dates <- zoo::index(backtest_returns_m_xts)

  wrong_backtest_returns_m_xts <- backtest_returns_m_xts[-2,]
  signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-04-15") #Remove the date that is not in the backtest_returns_m_xts

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = wrong_backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_m_xts must have consecutive dates"
  )

  #Test for backtest_returns_m_xts not having the same dates as signals_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_backtest_returns_m_xts <- backtest_returns_m_xts[-c(2:3),]

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = wrong_backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 1,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "There is only one date in backtest_returns_m_xts before the first training date"
  )

  #backtest_returns_m_xts can have more dates than signals_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-03-15")

  expect_no_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                           backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                           enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                           market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                           signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                           signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                           rebalancing_months = 6)
  )



  #Dates in backtest_returns_m_xts and benchmark_returns_m_xts must match
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  short_benchmark_returns_m_xts <- benchmark_returns_m_xts[-1,]


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = short_benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "dates in benchmark_returns_m_xts and backtest_returns_m_xts must be the same")

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  short_backtest_returns_m_xts <- backtest_returns_m_xts[-1,]
  signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-03-15")


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = short_backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "dates in benchmark_returns_m_xts and backtest_returns_m_xts must be the same")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 30,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "There is only one date in backtest_returns_m_xts before the first training date")



  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 1,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "There is only one date in backtest_returns_m_xts before the first training date")



  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "long", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen_signals_and_positions with their corrected position should be present in backtest_returns_m_xts")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts
  wrong_benchmark_returns_m_xts[3,2] <- NA

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = wrong_benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "benchmark_returns_m_xts must not have any NA values")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "sep500", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "market_factor_proxy must be present in benchmark_returns_m_xts")


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  expect_error(check_inputs_ss_backtest(chosen_signals_and_positions = chosen_signals_and_positions, forced_signals = NULL,
                                        signals_m_df = signals_m_df, initial_sample_size = "two"),
               "initial_sample_size must be numeric")

  wrong_backtest_returns_m_xts <- backtest_returns_m_xts[-4, ]

  expect_error(check_inputs_ss_backtest(chosen_signals_and_positions = chosen_signals_and_positions, forced_signals = NULL,
                                        signals_m_df = signals_m_df, initial_sample_size = 3,
                                        backtest_returns_m_xts = wrong_backtest_returns_m_xts),
               "backtest_returns_m_xts must have consecutive dates")

  expect_error(check_inputs_ss_backtest(chosen_signals_and_positions = chosen_signals_and_positions, forced_signals = NULL,
                                        signals_m_df = signals_m_df, initial_sample_size = 3,
                                        backtest_returns_m_xts = as.data.frame(backtest_returns_m_xts)),
               "backtest_returns_m_xts must be a xts object")




})

test_that("check_inputs_ss_backtest thrown an error when signal_themes_m_df has wrong format", {

  #No underscore allowed
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  p_correction_method <- "none"
  rebalancing_months <- 6
  signal_themes_m_df$theme[7:12] <- paste0("high_",  signal_themes_m_df$theme[7:12])

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "No underscores allowed in signal_themes_m_df theme names")



  #Col names
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  colnames(signal_themes_m_df)[4] <- "themes"

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'")


  #Enable theme representativeness
  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = NULL, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "signal_themes_m_df must be provided if enable_theme_representativeness is TRUE")


  #Chosen signals and positions
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")


  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_signal_themes_m_df <- signal_themes_m_df %>% dplyr::filter(!tickers %in% "Gamma")

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = wrong_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen_signals_and_positions with their corrected position should be present in signal_themes_m_df")


  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Vega = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  signals_m_df <- signals_m_df %>% dplyr::mutate(Vega = rnorm(n = nrow(signals_m_df)))
  backtest_returns_m_xts$Vega <- rnorm(n = nrow(backtest_returns_m_xts))


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen_signals_and_positions with their corrected position should be present in signal_themes_m_df")



  #Check for dates in signal_themes_m_df and signals_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")


  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_signal_themes_m_df <- signal_themes_m_df %>%
    dplyr::mutate(dates = dplyr::if_else(dates == as.Date("2001-04-15"), as.Date("2002-07-15"), dates)) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates)) %>%
    dplyr::arrange(id)


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = wrong_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "dates in signal_themes_m_df and signals_m_df must be the same")




  #Check for dates in signal_themes_m_df and signals_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")


  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_signal_themes_m_df <- signal_themes_m_df[-3,]


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = wrong_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "chosen_signals_and_positions must have a theme classification for every date")


  #Check for NA in signal_themes_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")


  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_signal_themes_m_df <- signal_themes_m_df
  wrong_signal_themes_m_df[3,4] <- NA

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = wrong_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "signal_themes_m_df should not have NAs")


})

test_that("check_inputs_ss_backtest thrown an error when priors_m_df has wrong format", {


  #Check for wrong colnames
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_priors_m_df <- priors_m_df %>% dplyr::rename(active_return = return)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "priors_m_df must have columns 'id', 'tickers', 'dates', 'return', 'market_factor_proxy' and 'theme'")


  #Check for theme matching in prior_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_priors_m_df <- priors_m_df %>% dplyr::mutate(theme = dplyr::if_else(theme == "value" & dates == "2001-04-15", "momentum", theme)) #Eliminate value theme in 2001-04-15


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "priors_m_df themes must contemplate all themes of chosen_signals_and_positions throughout all backtest dates")

  #Check for theme matching in prior_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_priors_m_df <- priors_m_df %>% dplyr::mutate(theme = dplyr::if_else(tickers == "delta" & dates == "2001-04-15", "skewness", theme)) #Crate a skewness theme


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "themes in priors_m_df and signal_themes_m_df should match")


  #Check for theme matching in prior_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_priors_m_df <- priors_m_df
  wrong_priors_m_df[3,4] <- NA
  adj_signal_themes_m_df <- signal_themes_m_df %>% dplyr::mutate(theme = dplyr::if_else(theme == "growth", "value", theme))

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "priors_m_df should not have NAs")



})

test_that("check_inputs_ss_backtest thrown an error when custom_signal_universe_metrics_m_df has wrong format", {


  #Check not contemplating chosen signals
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  custom_signal_universe_metrics_m_df <- signal_themes_m_df %>% dplyr::select(id, tickers, dates) %>% dplyr::filter(!dates %in% c("2001-05-15", "2001-07-15"),
                                                                                                                    !tickers == "Delta") %>%
    dplyr::mutate(pe = rnorm(nrow(.)), pb = rnorm(nrow(.)), ps = rnorm(nrow(.)), roe = rnorm(nrow(.)), roa = rnorm(nrow(.)), debt_to_equity = rnorm(nrow(.))) %>%
    dplyr::arrange(id)

  wrong_custom_signal_universe_metrics_m_df <- custom_signal_universe_metrics_m_df %>% dplyr::filter(!tickers == "Alpha") #Eliminate Alpha


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen signals should be contemplated in custom_signal_universe_metrics_m_df")


  #Check for first date not being ocntemplated
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  custom_signal_universe_metrics_m_df <- signal_themes_m_df %>% dplyr::select(id, tickers, dates) %>% dplyr::filter(!dates %in% c("2001-05-15", "2001-07-15"),
                                                                                                                    !tickers == "Delta") %>%
    dplyr::mutate(pe = rnorm(nrow(.)), pb = rnorm(nrow(.)), ps = rnorm(nrow(.)), roe = rnorm(nrow(.)), roa = rnorm(nrow(.)), debt_to_equity = rnorm(nrow(.))) %>%
    dplyr::arrange(id)

  wrong_custom_signal_universe_metrics_m_df <- custom_signal_universe_metrics_m_df %>% dplyr::filter(!dates < "2001-05-15") #Eliminate Alpha


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 2,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "first rebalancing date should be contemplated in custom_signal_universe_metrics_m_df")


  #Check for chosen_signals not being present in all dates
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  custom_signal_universe_metrics_m_df <- signal_themes_m_df %>% dplyr::select(id, tickers, dates) %>%
    dplyr::filter(!dates %in% c("2001-05-15", "2001-07-15"),!tickers == "Delta") %>%
    dplyr::mutate(pe = rnorm(nrow(.)), pb = rnorm(nrow(.)), ps = rnorm(nrow(.)), roe = rnorm(nrow(.)), roa = rnorm(nrow(.)), debt_to_equity = rnorm(nrow(.))) %>%
    dplyr::arrange(id)

  wrong_custom_signal_universe_metrics_m_df <- custom_signal_universe_metrics_m_df[-2, ]


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 2,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6))

  #Check for NA
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  custom_signal_universe_metrics_m_df <- signal_themes_m_df %>% dplyr::select(id, tickers, dates) %>%
    dplyr::filter(!dates %in% c("2001-05-15", "2001-07-15"),!tickers == "Delta") %>%
    dplyr::mutate(pe = rnorm(nrow(.)), pb = rnorm(nrow(.)), ps = rnorm(nrow(.)), roe = rnorm(nrow(.)), roa = rnorm(nrow(.)), debt_to_equity = rnorm(nrow(.))) %>%
    dplyr::arrange(id)

  wrong_custom_signal_universe_metrics_m_df <- custom_signal_universe_metrics_m_df
  wrong_custom_signal_universe_metrics_m_df[3,4] <- NA


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 2,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "custom_signal_universe_metrics_m_df should not have NAs"
               )

  #Check for heuristic sb
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  custom_signal_universe_metrics_m_df <- signal_themes_m_df %>% dplyr::select(id, tickers, dates) %>%
    dplyr::filter(!dates %in% c("2001-05-15", "2001-07-15"),!tickers == "Delta") %>%
    dplyr::mutate(pe = rnorm(nrow(.)), pb = rnorm(nrow(.)), ps = rnorm(nrow(.)), roe = rnorm(nrow(.)), roa = rnorm(nrow(.)), debt_to_equity = rnorm(nrow(.))) %>%
    dplyr::arrange(id)

  wrong_custom_signal_universe_metrics_m_df <- custom_signal_universe_metrics_m_df
  colnames(wrong_custom_signal_universe_metrics_m_df)[4] <- "arith_mean_ret"

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 2,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "custom_signal_universe_metrics_m_df should not have colnames that match the usual output from summarize_performance"
  )



})

test_that("check_inputs_ss_backtest throws an error when user_priors has wrong format (Spec 1)", {

  #Model Spec 1
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "bayesian"
  rebalancing_months <- 6


  #Create priors for model
  wrong_user_priors <- c(
    # Prior for Intercept
    brms::set_prior("normal(0.0012, 0.0016)", class = "Intercept"), #ok

    # Prior for market_factor_proxy coefficient
    #brms::set_prior("normal(0.0003, 0.0003)", class = "b", coef = "market_factor_proxy"), #ok

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"), #ok

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"), #ok

    # Prior for sd of Intercept at theme level
    brms::set_prior("student_t(30, 0, 0.0011)", class = "sd", group = "theme", coef = "Intercept"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        theme_level_intercept = "random", theme_level_slope = "fixed", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "Expected 7 rows for theme-level model specification 'random_intercept_fixed_slope', but got 6.")


  #Create priors for model
  wrong_user_priors <- c(
    # Prior for Intercept
    brms::set_prior("normal(0.0012, 0.0016)", class = "Intercept"), #ok

    # Prior for market_factor_proxy coefficient
    brms::set_prior("normal(0.0003, 0.0003)", class = "sd", coef = "market_factor_proxy"), #ok

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"), #ok

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"), #ok

    # Prior for sd of Intercept at theme level
    brms::set_prior("student_t(30, 0, 0.0011)", class = "sd", group = "theme", coef = "Intercept"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        theme_level_intercept = "random", theme_level_slope = "fixed", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "user_priors structure is invalid for theme-level model specification 'random_intercept_fixed_slope'.")



})

test_that("check_inputs_ss_backtest throws an error when user_priors has wrong format (Spec 2)", {

  #Model Spec 2
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "bayesian"
  rebalancing_months <- 6


  #Create priors for model
  wrong_user_priors <- c(
      # Prior for Value and Mom
      brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
      #brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "thememomentum"),

      # Prior for market_factor_proxy coefficient
      brms::set_prior("normal(0.0003, 0.0003)", class = "b", coef = "market_factor_proxy"),

      # Prior for sd of Intercept at theme:tickers level
      brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

      # Prior for sd of market_factor_proxy at theme:tickers level
      brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

      # Prior for residual error (sigma)
      brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

      # LKJ prior for correlations
      brms::set_prior("lkj(2)", class = "cor")
    )



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        theme_level_intercept = "theme_specific", theme_level_slope = "fixed", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "Expected 7 rows for theme-level model specification 'theme_specific_intercept_fixed_slope', but got 6.")


  #Create priors for model
  wrong_user_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "themegrowth"),

    # Prior for market_factor_proxy coefficient
    brms::set_prior("normal(0.0003, 0.0003)", class = "b", coef = "market_factor_proxy"),

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        theme_level_intercept = "theme_specific", theme_level_slope = "fixed", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "user_priors structure is invalid for theme-level model specification 'theme_specific_intercept_fixed_slope'")



})

test_that("check_inputs_ss_backtest throws an error when user_priors has wrong format (Spec 3)", {

  #Model Spec 3
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "bayesian"
  rebalancing_months <- 6


  #Create priors for model
  wrong_user_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "thememomentum"),
    #brms::set_prior("normal(0.03, 0.002)", class = "b", coef = "themevalue:market_factor_proxy"),
    brms::set_prior("normal(0.0000, 0.004)", class = "b", coef = "thememomentum:market_factor_proxy"),


    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        theme_level_intercept = "theme_specific", theme_level_slope = "theme_specific", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "Expected 8 rows for theme-level model specification 'theme_specific_intercept_theme_specific_slope', but got 7.")


  #Create priors for model
  wrong_user_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "thememomentum"),
    brms::set_prior("normal(0.03, 0.002)", class = "b", coef = "themegrowth:market_factor_proxy"),
    brms::set_prior("normal(0.0000, 0.004)", class = "b", coef = "thememomentum:market_factor_proxy"),


    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        theme_level_intercept = "theme_specific", theme_level_slope = "theme_specific", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "user_priors structure is invalid for theme-level model specification 'theme_specific_intercept_theme_specific_slope'.")



})

test_that("check_inputs_ss_backtest throws an error when user_priors has wrong format (Spec 4)", {

  #Model Spec 4
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "bayesian"
  rebalancing_months <- 6


  #Create priors for model
  wrong_user_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themedefensive"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "thememomentum"),
    brms::set_prior("normal(0.03, 0.002)", class = "b", coef = "themevalue:market_factor_proxy"),
    brms::set_prior("normal(0.0000, 0.004)", class = "b", coef = "thememomentum:market_factor_proxy"),

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )




  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        theme_level_intercept = "fixed", theme_level_slope = "fixed", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "Expected 6 rows for theme-level model specification 'fixed_intercept_fixed_slope', but got 9.")



  #Create priors for model
  wrong_user_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "Intercept"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "themevalue"),

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )



  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        theme_level_intercept = "fixed", theme_level_slope = "fixed", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "user_priors structure is invalid for theme-level model specification 'fixed_intercept_fixed_slope'.")

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  user_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "thememomentum"),
    brms::set_prior("normal(0.03, 0.002)", class = "b", coef = "themevalue:market_factor_proxy"),
    brms::set_prior("normal(0.0000, 0.004)", class = "b", coef = "thememomentum:market_factor_proxy"),

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = "bayesian",
                                        user_priors = user_priors,
                                        theme_level_intercept = "Random", theme_level_slope = "Fix",
                                        rebalancing_months = 6),
               "Invalid model specification at theme-level")




})

test_that("check_inputs_ss_backtest have wrong lmer_control", {

  #Check for wrong colnames
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_lmer_control <- list(optCtrl = list(maxfun = 1000))


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = wrong_lmer_control, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "lmer_control should have only 'lmer_optimizer', 'lmer_optimization_objective' or 'hierarchical_p_value_method' as names.")


  wrong_lmer_control <- list(lmer_optimizer = "mqo")


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = wrong_lmer_control, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "lmer_optimizer should be one of 'Nelder_Mead', 'bobyqa', 'nlminbwrap' or 'nloptwrap'")


  wrong_lmer_control <- list(lmer_optimization_objective = "lik")


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = wrong_lmer_control, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "lmer_optimization_objective should be one of 'likelihood' or 'REML'")


  wrong_lmer_control <- list(hierarchical_p_value_method = "lik")


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = wrong_lmer_control, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "hierarchical_p_value_method should be one of 'Satterthwaite', 'Kenward-Roger'  or 'REML'")



  lmer_control <- list(hierarchical_p_value_method = "Kenward-Roger")


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = lmer_control, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, theme_level_intercept = NULL, theme_level_slope = "fixed"),
               "For 'partial_pooled' model structure, 'theme_level_intercept' and 'theme_level_slope' must be provided.")

  })

test_that("check_inputs_ss_backtest throws error when p_correction arguments are wrong", {

  #Check for wrong colnames
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "NONE"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "p_correction_method must be one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'bayesian' or 'none'")


  p_correction_method <- "bayesian"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "no_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "bayesian p_correction_method is currently only available for partial_pooled model_structure")


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 1.1, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "signal_significance_threshold must be between 0 and 1")





})

test_that("check_inputs_ss_backtest have wrong bayesian spec", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "bayesian"
  rebalancing_months <- 6

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL),
               "Currently, bayesian fit requires user_priors or priors_m_df.")



  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short")

  p_correction_method <- "bayesian"
  rebalancing_months <- 6

  prior_derivation_control <- c(t_df = 3)

  adj_signal_themes_m_df <- signal_themes_m_df %>% dplyr::mutate(theme = dplyr::if_else(tickers == "Delta", "momentum", theme))

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL, prior_derivation_control = prior_derivation_control),
               "prior_derivation_control should have only 'half_t_df' as names.")

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short")

  p_correction_method <- "bayesian"
  rebalancing_months <- 6

  brms_control <- c(Chain = 3)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL, prior_derivation_control = NULL, brms_control = brms_control),
               "brms_control must be a list containing 'chains', 'iter', 'warmup', 'thin', 'seed' and/or 'adapt_delta'.")



  brms_control <- list(chains = -3, iter = 1000)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL, prior_derivation_control = NULL, brms_control = brms_control),
               "chains must be a positive number.")



  brms_control <- list(chains = 3, iter = -1000)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL, prior_derivation_control = NULL, brms_control = brms_control),
               "iter must be a positive number.")


  brms_control <- list(chains = 3, warmup = -1000)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL, prior_derivation_control = NULL, brms_control = brms_control),
               "warmup must be a positive number.")


  brms_control <- list(chains = 3, thin = -1000)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL, prior_derivation_control = NULL, brms_control = brms_control),
               "thin must be a positive number.")


  brms_control <- list(adapt_delta = 3)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL, prior_derivation_control = NULL, brms_control = brms_control),
               "adapt_delta should be between 0 and 1.")


  brms_control <- list(warmup = 1000, iter = 100)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = FALSE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = adj_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6, user_priors = NULL, prior_derivation_control = NULL, brms_control = brms_control),
               "warmup must be less than iter")



})

test_that("check_inputs_ss_backtest throws error when p_correction arguments are wrong", {

  #Check for wrong colnames
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = "6"),
               "rebalancing_months should be numeric.")


  colnames(benchmark_returns_m_xts)[1] <- 3
  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = 3, model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "market_factor_proxy must be character")

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_m_xts = backtest_returns_m_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_m_xts = benchmark_returns_m_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = "TRUE",
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "active_returns must be logical")


})

