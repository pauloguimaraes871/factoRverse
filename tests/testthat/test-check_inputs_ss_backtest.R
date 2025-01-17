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


  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_backtest_returns_xts <- backtest_returns_xts[-c(2:3),]

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = wrong_backtest_returns_xts, forced_signals = NULL, initial_sample_size = 1,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all backtest_dates derived from signals_m_df must be present in backtest_returns_xts"
  )

  #backtest_returns_xts can have more dates than signals_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")


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

  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 30,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "backtest_returns_xts must have at least initial_sample_size rows")



  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 1,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "There is only one date in backtest_returns_xts before the first training date")



  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "long", Gamma = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen_signals_and_positions with their corrected position should be present in backtest_returns_xts")



})

test_that("check_inputs_ss_backtest thrown an error when signal_themes_m_df has wrong format", {

  #No underscore allowed
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  p_correction_method <- "none"
  rebalancing_months <- 6
  signal_themes_m_df$theme[7:12] <- paste0("high_",  signal_themes_m_df$theme[7:12])

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'")


  #Enable theme representativeness
  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
  signal_themes_m_df[7:12,"tickers"] <- "Beta"
  signal_themes_m_df[7:12,"id"] <- paste0(signal_themes_m_df[7:12,"tickers"], "-", signal_themes_m_df[7:12,"dates"])

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "all chosen_signals_and_positions with their corrected position should be present in signal_themes_m_df")


  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Vega = "long")


  p_correction_method <- "none"
  rebalancing_months <- 6
  signals_m_df <- signals_m_df %>% dplyr::mutate(Vega = rnorm(n = nrow(signals_m_df)))
  backtest_returns_xts$Vega <- rnorm(n = nrow(backtest_returns_xts))


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
  signal_themes_m_df$dates[which(signal_themes_m_df$dates == "2001-04-15")] <- "2001-07-15"


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "dates in signal_themes_m_df and signals_m_df must be the same")




  #Check for dates in signal_themes_m_df and signals_m_df
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")


  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_signal_themes_m_df <- signal_themes_m_df[-3,]


  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = wrong_signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "chosen_signals_and_positions must have a theme classification for every date")

})

test_that("check_inputs_ss_backtest thrown an error when priors_m_df has wrong format", {


  #Check for wrong colnames
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")

  p_correction_method <- "none"
  rebalancing_months <- 6
  wrong_priors_m_df <- priors_m_df %>% dplyr::rename(active_return = return)

  expect_error(check_inputs_ss_backtest(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = wrong_priors_m_df, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "themes in priors_m_df and signal_themes_m_df should match")


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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 2,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 2,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6))


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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
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
                                        backtest_returns_xts = backtest_returns_xts, forced_signals = NULL, initial_sample_size = 3,
                                        enable_theme_representativeness = TRUE, benchmark_returns_xts = benchmark_returns_xts,
                                        theme_level_intercept = "fixed", theme_level_slope = "fixed", user_priors = wrong_user_priors,
                                        market_factor_proxy = "IBOV", model_structure = "partial_pooled", lmer_control = NULL, active_returns = TRUE,
                                        signal_significance_threshold = 0.05, priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                                        signal_themes_m_df = signal_themes_m_df, p_correction_method = p_correction_method,
                                        rebalancing_months = 6),
               "user_priors structure is invalid for theme-level model specification 'fixed_intercept_fixed_slope'.")



})


