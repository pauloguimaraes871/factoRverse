#Artificial data
test_that("select_and_correct_signals correctly subsets signals_m_df", {

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #Subseted signals
  subsetted_signals <- colnames(select_and_correct_signals(chosen_signals_and_positions,
                                                           backtest_returns_xts = backtest_returns_xts,
                                                           signals_m_df = signals_m_df)$selected_signals_corrected_positions_m_df)

  expect_equal(c("id", "tickers", "dates", "Alpha", "low_Beta", "Gamma"),subsetted_signals)

})

test_that("select_and_correct_signals correctly subsets signals_m_df when chosen_signals are less than all options", {

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long")

  #Subseted signals
  subsetted_signals <- colnames(select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_xts = backtest_returns_xts,
    signals_m_df = signals_m_df)$selected_signals_corrected_positions_m_df)

  expect_equal(c("id", "tickers", "dates", "Alpha", "Gamma"),subsetted_signals)

})

test_that("select_and_correct_signals correctly inverts signs of short positions", {

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Beta = "short")

    #Subseted signals
  low_beta <- select_and_correct_signals(signals_m_df = signals_m_df,
                                         chosen_signals_and_positions = chosen_signals_and_positions,
                                         backtest_returns_xts = backtest_returns_xts)$selected_signals_corrected_positions_m_df$low_Beta

  expect_equal(signals_m_df$Beta*-1 ,low_beta)

})

test_that("select_and_correct_signals correctly subsets backtest_returns_xts", {

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #Subseted backtests
  subsetted_backtests <- colnames(
    select_and_correct_signals(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                               backtest_returns_xts = backtest_returns_xts)$selected_backtest_returns_corrected_positions_xts
  )

  subsetted_signals <- colnames(
    select_and_correct_signals(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                                                          backtest_returns_xts = backtest_returns_xts)$selected_signals_corrected_positions_m_df
  )


  expect_equal(c("Alpha", "low_Beta", "Gamma"),subsetted_backtests)
  expect_equal(subsetted_backtests, subsetted_signals[-c(1:3)])

})

test_that("select_and_correct_signals correctly subsets backtest_returns_xts when chosen_signals are less than all options", {

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long")

#Subseted backtests
  subsetted_backtests <- colnames(
    select_and_correct_signals(chosen_signals_and_positions = chosen_signals_and_positions,
                               signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)$selected_backtest_returns_corrected_positions_xts
  )

  subsetted_signals<- colnames(
    select_and_correct_signals(chosen_signals_and_positions = chosen_signals_and_positions,
                               signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)$selected_signals_corrected_positions_m_df
  )


  expect_equal(c("Alpha", "Gamma"),subsetted_backtests)
  expect_equal(subsetted_backtests, subsetted_signals[-c(1:3)])

})

test_that("select_and_correct_signals correctly subsets backtest_returns_xts when only one signal in chosen_signals", {

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Beta = "short")

  #Subseted backtests
  subsetted_backtests <- colnames(
    select_and_correct_signals(chosen_signals_and_positions = chosen_signals_and_positions,
                               signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)$selected_backtest_returns_corrected_positions_xts
  )

  subsetted_signals<- colnames(
    select_and_correct_signals(chosen_signals_and_positions = chosen_signals_and_positions,
                               signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)$selected_signals_corrected_positions_m_df
  )


  expect_equal(c("low_Beta"),subsetted_backtests)
  expect_equal(subsetted_backtests, subsetted_signals[-c(1:3)])

})

test_that("select_and_correct_signals correctly chooses short option in backtest_returns_xts", {

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  test_backtest_returns_xts <- backtest_returns_xts
  test_backtest_returns_xts$Beta <- rnorm(n = nrow(test_backtest_returns_xts), mean = 0, sd = 1)

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #Subseted backtests
  subsetted_backtests <- colnames(
    select_and_correct_signals(chosen_signals_and_positions = chosen_signals_and_positions,
                               signals_m_df = signals_m_df, backtest_returns_xts = test_backtest_returns_xts)$selected_backtest_returns_corrected_positions_xts
  )

  expect_false("Beta" %in% subsetted_backtests)

})

##Real data
test_that("select_and_correct_signals correctly subsets signals_m_df when chosen_signals are less than all options for real data", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  chosen_signals_and_positions <- c(book_yield = "long", dps_yield = "long", roe_3m = "long", sharpe_6m = "short")

  #Subseted signals
  subsetted_signals <- colnames(select_and_correct_signals(chosen_signals_and_positions = chosen_signals_and_positions,
                                                           signals_m_df = toy_preprocessed_features)$selected_signals_corrected_positions_m_df)


  #Check for correct subset
  expect_equal(c("id", "tickers", "dates", "book_yield", "dps_yield", "roe_3m", "low_sharpe_6m"),subsetted_signals)
  #Check for correct signal
  expect_equal(
    select_and_correct_signals(chosen_signals_and_positions = chosen_signals_and_positions, signals_m_df = toy_preprocessed_features)$selected_signals_corrected_positions_m_df$dps_yield,
    toy_preprocessed_features$dps_yield)

  expect_equal(
    select_and_correct_signals(chosen_signals_and_positions = chosen_signals_and_positions, signals_m_df = toy_preprocessed_features)$selected_signals_corrected_positions_m_df$low_sharpe_6m,
    toy_preprocessed_features$sharpe_6m*-1)


})

##Error check
test_that("check_inputs_ss_backtest throws an error when trying to choose a signal not present in backtest_returns_xts ", {

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")
  backtest_returns_xts <- backtest_returns_xts[,-1]

  expect_error(
    select_and_correct_signals(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                               backtest_returns_xts = backtest_returns_xts),
    "all chosen signals should have a matching position in backtest_returns_xts"
  )

  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")
  colnames(backtest_returns_xts)[3] <- "Beta"



  expect_error(
    select_and_correct_signals(signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions,
                               backtest_returns_xts = backtest_returns_xts),
    "all chosen signals should have a matching position in backtest_returns_xts"
  )



})




