test_that("select_and_correct_signals correctly subsets signals_m_upd_ref", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date = "2001-07-15"

  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]

  #Subseted signals
  subsetted_signals <- colnames(select_and_correct_signals(signal_selection_policy = signal_selection_policy,
                                                           signals_m_upd_ref = signals_m_upd_ref)$selected_signals_corrected_positions_m_upd_ref)

  expect_equal(c("id", "tickers", "dates", "Alpha", "low_Beta", "Gamma"),subsetted_signals)

})

test_that("select_and_correct_signals correctly subsets signals_m_upd_ref when chosen_signals are less than all options", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  test_signal_selection_policy <- signal_selection_policy
  test_signal_selection_policy$chosen_signals <- signal_selection_policy$chosen_signals[-2]
  test_signal_selection_policy$signal_positions <- signal_selection_policy$signal_positions[-2]

  current_date = "2001-07-15"

  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]

  #Subseted signals
  subsetted_signals <- colnames(select_and_correct_signals(signal_selection_policy = test_signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref)$selected_signals_corrected_positions_m_upd_ref)

  expect_equal(c("id", "tickers", "dates", "Alpha", "Gamma"),subsetted_signals)

})

test_that("select_and_correct_signals correctly inverts signs of short positions", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  test_signal_selection_policy <- signal_selection_policy
  test_signal_selection_policy$chosen_signals <- signal_selection_policy$chosen_signals[2]
  test_signal_selection_policy$signal_positions <- signal_selection_policy$signal_positions[2]

  current_date = "2001-07-15"

  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]

  #Subseted signals
  low_beta <- select_and_correct_signals(signal_selection_policy = test_signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref)$selected_signals_corrected_positions_m_upd_ref$low_Beta

  expect_equal(signals_m_upd_ref$Beta*-1 ,low_beta)

})

test_that("select_and_correct_signals correctly subsets backtest_returns_upd_ref", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date = "2001-07-15"

  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]

  #Subseted backtests
  subsetted_backtests <- colnames(
    select_and_correct_signals(signal_selection_policy = signal_selection_policy,
    signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)$selected_signals_backtest_returns_upd_ref
      )

  subsetted_signals<- colnames(
    select_and_correct_signals(signal_selection_policy = signal_selection_policy,
                               signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)$selected_signals_corrected_positions_m_upd_ref
  )


  expect_equal(c("dates", "Alpha", "low_Beta", "Gamma"),subsetted_backtests)
  expect_equal(subsetted_backtests[-1], subsetted_signals[-c(1:3)])

})

test_that("select_and_correct_signals correctly subsets backtest_returns_upd_ref when chosen_signals are less than all options", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date = "2001-07-15"
  test_signal_selection_policy <- signal_selection_policy
  test_signal_selection_policy$chosen_signals <- signal_selection_policy$chosen_signals[-2]
  test_signal_selection_policy$signal_positions <- signal_selection_policy$signal_positions[-2]


  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]

  #Subseted backtests
  subsetted_backtests <- colnames(
    select_and_correct_signals(signal_selection_policy = test_signal_selection_policy,
                               signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)$selected_signals_backtest_returns_upd_ref
  )

  subsetted_signals<- colnames(
    select_and_correct_signals(signal_selection_policy = test_signal_selection_policy,
                               signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)$selected_signals_corrected_positions_m_upd_ref
  )


  expect_equal(c("dates", "Alpha", "Gamma"),subsetted_backtests)
  expect_equal(subsetted_backtests[-1], subsetted_signals[-c(1:3)])

})

test_that("select_and_correct_signals correctly subsets backtest_returns_upd_ref when only one signal in chosen_signals", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date = "2001-07-15"
  test_signal_selection_policy <- signal_selection_policy
  test_signal_selection_policy$chosen_signals <- signal_selection_policy$chosen_signals[2]
  test_signal_selection_policy$signal_positions <- signal_selection_policy$signal_positions[2]

  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]

  #Subseted backtests
  subsetted_backtests <- colnames(
    select_and_correct_signals(signal_selection_policy = test_signal_selection_policy,
                               signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)$selected_signals_backtest_returns_upd_ref
  )

  subsetted_signals<- colnames(
    select_and_correct_signals(signal_selection_policy = test_signal_selection_policy,
                               signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)$selected_signals_corrected_positions_m_upd_ref
  )


  expect_equal(c("dates", "low_Beta"),subsetted_backtests)
  expect_equal(subsetted_backtests[-1], subsetted_signals[-c(1:3)])

})


test_that("select_and_correct_signals correctly chooses short option in backtest_returns_upd_ref", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date = "2001-07-15"
  test_signal_selection_policy <- signal_selection_policy


  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  test_backtest_returns_df <- backtest_returns_df
  test_backtest_returns_df$Beta <- rnorm(n = nrow(test_backtest_returns_df), mean = 0, sd = 1)
  backtest_returns_upd_ref <- test_backtest_returns_df[which(test_backtest_returns_df$dates <= current_date), ]

  #Subseted backtests
  subsetted_backtests <- colnames(
    select_and_correct_signals(signal_selection_policy = test_signal_selection_policy,
                               signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)$selected_signals_backtest_returns_upd_ref
  )

  expect_false("Beta" %in% subsetted_backtests)

})

test_that("select_and_correct_signals throws an error when trying to choose a signal not present in signals_m_df", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  test_signal_selection_policy <- signal_selection_policy
  test_signal_selection_policy$chosen_signals <- c(signal_selection_policy$chosen_signals, "Delta")
  test_signal_selection_policy$signal_positions <- c(signal_selection_policy$signal_positions, Delta = "short")

  current_date = "2001-07-15"

  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]

  expect_error(select_and_correct_signals(signal_selection_policy = test_signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref),
               "signal selection not avaiable in signals_m_df")


})

test_that("select_and_correct_signals throws an error when trying to choose a signal not present in backtest_returns_df ", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date = "2001-07-15"


  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), -1]

 expect_error(select_and_correct_signals(signal_selection_policy = signal_selection_policy,
                                          signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref),
              "all chosen signals should have a matching position in backtest_returns_df"
              )



})

test_that("select_and_correct_signals throws an error when when chosen_signals do not match signal_positions", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  test_signal_selection_policy <- signal_selection_policy
  test_signal_selection_policy$chosen_signals <- signal_selection_policy$chosen_signals[-2]

  current_date = "2001-07-15"

  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]


  expect_error(select_and_correct_signals(signal_selection_policy = test_signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref,
                                          "all chosen signals should have a matching position in signal_positions."))

})

test_that("select_and_correct_signals thrown an error when trying to choose a signal more than once", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date = "2001-07-15"
  test_signal_selection_policy <- signal_selection_policy
  test_signal_selection_policy$chosen_signals <- c(test_signal_selection_policy$chosen_signals, "Beta")
  test_signal_selection_policy$signal_positions <- c(signal_selection_policy$signal_positions, Beta = "long")


  #Generate signal matrix part
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]

  expect_error(select_and_correct_signals(signal_selection_policy = test_signal_selection_policy,
                                          signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref),
               "each signal must be chosen only once")



})
