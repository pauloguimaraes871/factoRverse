test_that("clean_returns_sample works for a monthly series without needing to fill and no holidays", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  current_date <- "2001-06-15"

  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  backtest_returns_m_xts_upd_ref <- backtest_returns_m_xts[which(zoo::index(backtest_returns_m_xts) <= current_date), ]
  selected_benchmark_returns_m_xts_upd_ref <- benchmark_returns_m_xts[which(zoo::index(benchmark_returns_m_xts) <= current_date), concentration_constraint_policy$benchmark]
  signal_themes_m_d_ref <- signal_themes_m_df %>% dplyr::filter(dates == current_date)


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(signals_m_df = signals_m_d_ref,
                                                                   chosen_signals_and_positions = c(Alpha = "long", Beta = "short", Gamma = "long"),
                                                                   backtest_returns_m_xts = backtest_returns_m_xts_upd_ref,
                                                                   signal_themes_m_df = signal_themes_m_d_ref)

  selected_signals_backtest_returns_m_xts_upd_ref <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_signal_themes_m_d_ref <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #Define signal eligibilirt
  signal_eligibility_results_list <- define_signal_eligibility(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_signals_backtest_returns_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_benchmark_returns_m_xts_upd_ref,
    selected_signal_themes_m_d_ref = signal_themes_m_d_ref
  )

  eligible_universe <- signal_eligibility_results_list$signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
  returns_sample <- selected_signals_backtest_returns_m_xts_upd_ref[, eligible_universe$tickers]

  expected_results <- returns_sample

  results <- clean_returns_sample(returns_m_xts_sample = returns_sample,
                                  groups_m_d_ref = selected_signal_themes_m_d_ref)

  expect_equal(expected_results, results)

})

test_that("clean_returns_sample works for a daily series WITH holidays and NAs with only one sector (useful for mmaf)", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  covariance_matrix_sample_size <- 200
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]

  #eligible stocks
  eligible_stocks <- c("Stock B", "Stock C")

  returns_sample <- daily_stock_returns_m_xts_upd_ref[, eligible_stocks]

  #min date
  dates <- zoo::index(returns_sample) %>% as.Date()
  min_date <- dates[length(dates) - covariance_matrix_sample_size]
  returns_sample <- returns_sample[which(zoo::index(returns_sample) >= min_date),]

  #remove holidays
  holidays <- as.Date(c("2000-11-15", "2000-12-24", "2000-12-25", "2000-12-31", "2001-01-01"))
  no_holidays <- setdiff(zoo::index(returns_sample), holidays) %>% as.Date()
  expected_results <- returns_sample[no_holidays]

  #Fill NAs with row median
  row_medians <- apply(expected_results, 1, function(x) median(x, na.rm = TRUE))
  expected_results$`Stock C`[which(is.na(expected_results$`Stock C`))] <- row_medians[which(is.na(expected_results$`Stock C`))]
  #expected_results$`Stock_B`[which(is.na(expected_results$`Stock B`))] <- row_medians[which(is.na(expected_results$`Stock B`))]
  #expected_results$`Stock C`[which(is.na(expected_results$`Stock C`))] <- row_medians[which(is.na(expected_results$`Stock C`))]
  #expected_results$`Stock E`[which(is.na(expected_results$`Stock E`))] <- row_medians[which(is.na(expected_results$`Stock E`))]

  rownames(expected_results) <- NULL

  results <- clean_returns_sample(returns_m_xts_sample = returns_sample,
                                  groups_m_d_ref = stocks_groups_m_d_ref,
                                  fill_by = "Sector"
  )

  rownames(results) <- NULL

  expect_equal(results, expected_results)


})

test_that("clean_returns_sample works for a daily series WITH holidays and NAs (one NA filled by sector and the other by row median)", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  covariance_matrix_sample_size <- 200
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]

  #eligible stocks
  eligible_stocks <- c("Stock A", "Stock B", "Stock C", "Stock E")

  returns_sample <- daily_stock_returns_m_xts_upd_ref[, eligible_stocks]

  #min date
  dates <- zoo::index(returns_sample) %>% as.Date()
  min_date <- dates[length(dates) - covariance_matrix_sample_size]
  returns_sample <- returns_sample[which(zoo::index(returns_sample) >= min_date),]

  #remove holidays
  holidays <- as.Date(c("2000-11-15", "2000-12-24", "2000-12-25", "2000-12-31", "2001-01-01"))
  no_holidays <- setdiff(zoo::index(returns_sample), holidays) %>% as.Date()
  expected_results <- returns_sample[no_holidays]

  #Fill NAs with groups
  financials_group_medians <- apply(expected_results[, c("Stock B", "Stock C")], 1, function(x) median(x, na.rm = TRUE))
  expected_results$`Stock C`[which(is.na(expected_results$`Stock C`))] <- financials_group_medians[which(is.na(expected_results$`Stock C`))]
  #expected_results$`Stock B`[which(is.na(expected_results$`Stock B`))] <- financials_group_medians[which(is.na(expected_results$`Stock B`))]

  #Fill NAs with row median
  row_medians <- apply(expected_results, 1, function(x) median(x, na.rm = TRUE))
  expected_results$`Stock A`[which(is.na(expected_results$`Stock A`))] <- row_medians[which(is.na(expected_results$`Stock A`))]
  #expected_results$`Stock_B`[which(is.na(expected_results$`Stock B`))] <- row_medians[which(is.na(expected_results$`Stock B`))]
  #expected_results$`Stock C`[which(is.na(expected_results$`Stock C`))] <- row_medians[which(is.na(expected_results$`Stock C`))]
  #expected_results$`Stock E`[which(is.na(expected_results$`Stock E`))] <- row_medians[which(is.na(expected_results$`Stock E`))]

  rownames(expected_results) <- NULL

  results <- clean_returns_sample(returns_m_xts_sample = returns_sample,
                                  groups_m_d_ref = stocks_groups_m_d_ref,
                                  fill_by = "Sector"
  )

  rownames(results) <- NULL

  expect_equal(results, expected_results)


})

test_that("clean_returns_sample works for a daily series without holidays and NAs (filled by row median) and fill_by = NULL", {
  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  stocks_groups_m_d_ref$Sector[4:5] <- "Financials"
  covariance_matrix_sample_size <- 50
  stocks_groups_m_d_ref$Subsector <- NULL
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]

  #eligible stocks
  eligible_stocks <- c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E")

  returns_sample <- daily_stock_returns_m_xts_upd_ref[, eligible_stocks]

  #min date
  dates <- zoo::index(returns_sample) %>% as.Date()
  min_date <- dates[length(dates) - covariance_matrix_sample_size]
  returns_sample <- returns_sample[which(zoo::index(returns_sample) >= min_date),]

  #remove holidays
  holidays <- NULL
  expected_results <- returns_sample

  #Fill NAs
  expected_results$`Stock C`[which(is.na(expected_results$`Stock C`))] <- apply(data.frame(B = expected_results$`Stock B`[which(is.na(expected_results$`Stock C`))],
                                                                                           E = expected_results$`Stock E`[which(is.na(expected_results$`Stock C`))],
                                                                                           D = expected_results$`Stock D`[which(is.na(expected_results$`Stock C`))]
  ),
  1, function(x) median(x))

  expected_results$`Stock A`[which(is.na(expected_results$`Stock A`))] <- apply(expected_results[which(is.na(expected_results$`Stock A`)),-1], 1, function(x) median(x, na.rm = TRUE))

  rownames(expected_results) <- NULL

  results <- clean_returns_sample(returns_m_xts_sample = returns_sample,
                                  groups_m_d_ref = stocks_groups_m_d_ref,
                                  fill_by = NULL
  )

  rownames(results) <- NULL

  expect_equal(expected_results, results)

})


test_that("clean_returns_sample works toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  cleaned_returns_sample <- clean_returns_sample(returns_m_xts_sample = daily_stock_returns_m_xts_upd_ref,
                                                 groups_m_d_ref = stock_groups_m_d_ref)

  #Test if a given row was filled as expected
  stocks_in_eqma3b_sector <- stock_groups_m_d_ref %>% dplyr::filter(macro_sector == "Doméstico Defensivo") %>% dplyr::pull(tickers)
  expect_equal(cleaned_returns_sample[4,"EQMA3B"] %>% as.numeric(),
               median(daily_stock_returns_m_xts_upd_ref[4,which(colnames(daily_stock_returns_m_xts_upd_ref) %in% stocks_in_eqma3b_sector)], na.rm = TRUE))

})



