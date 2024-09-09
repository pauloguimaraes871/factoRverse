test_that("clean_returns_sample works for a monthly series without needing to fill and no holidays", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-06-15"

  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(signal_selection_policy = signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref,
                                                                   backtest_returns_upd_ref = backtest_returns_upd_ref)
  selected_signals_backtest_returns_upd_ref <- selected_signals_and_backtest_list$selected_signals_backtest_returns_upd_ref

  #Define signal eligibilirt
  signal_eligibility_results_list <- define_signal_eligibility(
    selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
    selected_benchmark_returns_upd_ref = selected_benchmark_returns_upd_ref,
    signal_selection_policy = signal_selection_policy,
    signals_groups_m_d_ref = signals_groups_m_d_ref
  )
  eligible_universe <- signal_eligibility_results_list$signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
  returns_sample <- selected_signals_backtest_returns_upd_ref[, c("dates", eligible_universe$tickers)]

  expected_results <- returns_sample

  results <- clean_returns_sample(returns_sample = returns_sample,
                                  groups_m_d_ref = signals_groups_m_d_ref)

  expect_equal(expected_results, results)

})

test_that("clean_returns_sample works for a daily series WITH holidays and NAs (one NA filled by sector and the other by row median)", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- groups_m_df_list$stocks[which(groups_m_df_list$stocks$dates == current_date), ]
  stocks_groups_m_d_ref$tickers <- c("Stock_A", "Stock_B", "Stock_C", "Stock_D", "Stock_E")
  covariance_matrix_sample_size <- 200
  daily_active_returns_upd_ref <- daily_active_returns_df[which(daily_active_returns_df$dates <= current_date), ]

  #eligible stocks
  eligible_stocks <- c("Stock_A", "Stock_B", "Stock_C", "Stock_E")

  returns_sample <- daily_active_returns_upd_ref[, c("dates", eligible_stocks)]

  #min date
  min_date <- returns_sample$dates[length(returns_sample$dates) - covariance_matrix_sample_size]
  returns_sample <- returns_sample[which(returns_sample$dates >= min_date),]

  #remove holidays
  holidays <- which(returns_sample$dates %in% as.Date(c("2000-09-07", "2000-11-15", "2000-12-24", "2000-12-25", "2000-12-31", "2001-01-01")))
  expected_results <- returns_sample[-holidays, ]

  #Fill NAs
  expected_results$Stock_C[which(is.na(expected_results$Stock_C))] <- expected_results$Stock_B[which(is.na(expected_results$Stock_C))]
  expected_results$Stock_A[which(is.na(expected_results$Stock_A))] <- apply(expected_results[which(is.na(expected_results$Stock_A)),-1], 1, function(x) median(x, na.rm = TRUE))

  rownames(expected_results) <- NULL

  results <- clean_returns_sample(returns_sample = returns_sample,
                                  groups_m_d_ref = stocks_groups_m_d_ref,
                                  fill_by = "Sector"
  )

  rownames(results) <- NULL

  expect_equal(expected_results, results)


})

test_that("clean_returns_sample works for a daily series WITH holidays and NAs (filled by row median) and fill_by = NULL", {
#Load
load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

current_date <- "2001-04-15"

#Generate return sample
signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
stocks_groups_m_d_ref <- groups_m_df_list$stocks[which(groups_m_df_list$stocks$dates == current_date), ]
stocks_groups_m_d_ref$tickers <- c("Stock_A", "Stock_B", "Stock_C", "Stock_D", "Stock_E")
stocks_groups_m_d_ref$Sector[4:5] <- "Financials"
covariance_matrix_sample_size <- 50
stocks_groups_m_d_ref$Subsector <- NULL
daily_active_returns_upd_ref <- daily_active_returns_df[which(daily_active_returns_df$dates <= current_date), ]

#eligible stocks
eligible_stocks <- c("Stock_A", "Stock_B", "Stock_C", "Stock_D", "Stock_E")

returns_sample <- daily_active_returns_upd_ref[, c("dates", eligible_stocks)]

#min date
min_date <- returns_sample$dates[length(returns_sample$dates) - covariance_matrix_sample_size]
returns_sample <- returns_sample[which(returns_sample$dates >= min_date),]

#remove holidays
holidays <- NULL
expected_results <- returns_sample

#Fill NAs
expected_results$Stock_C[which(is.na(expected_results$Stock_C))] <- apply(data.frame(B = expected_results$Stock_B[which(is.na(expected_results$Stock_C))],
                                                                                     E = expected_results$Stock_E[which(is.na(expected_results$Stock_C))],
                                                                                     D = expected_results$Stock_D[which(is.na(expected_results$Stock_C))]
),
1, function(x) median(x))

expected_results$Stock_A[which(is.na(expected_results$Stock_A))] <- apply(expected_results[which(is.na(expected_results$Stock_A)),-1], 1, function(x) median(x, na.rm = TRUE))

rownames(expected_results) <- NULL

results <- clean_returns_sample(returns_sample = returns_sample,
                                groups_m_d_ref = stocks_groups_m_d_ref,
                                fill_by = NULL
)

rownames(results) <- NULL

expect_equal(expected_results, results)
})
