test_that("estimate_covariance_matrix works for SAM", {
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

  #exoected_results
  expected_results <- daily_active_returns_upd_ref[, c("dates", eligible_stocks)]

  #min date
  min_date <- expected_results$dates[length(expected_results$dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(expected_results$dates >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)


  expected_results <- cov(expected_results[,-1])

  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_upd_ref = daily_active_returns_upd_ref,
    covariance_matrix_sample_size = covariance_matrix_sample_size,
    covariance_estimation_method = "SAM",
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

test_that("estimate_covariance_matrix works for EWMA", {
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

  #exoected_results
  expected_results <- daily_active_returns_upd_ref[, c("dates", eligible_stocks)]

  #min date
  min_date <- expected_results$dates[length(expected_results$dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(expected_results$dates >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)


  expected_results <- PerformanceAnalytics::M2.ewma(as.matrix(expected_results[,-1]))

  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_upd_ref = daily_active_returns_upd_ref,
    covariance_matrix_sample_size = covariance_matrix_sample_size,
    covariance_estimation_method = "EWMA",
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

test_that("estimate_covariance_matrix works for Shrink CC", {
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

  #exoected_results
  expected_results <- daily_active_returns_upd_ref[, c("dates", eligible_stocks)]

  #min date
  min_date <- expected_results$dates[length(expected_results$dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(expected_results$dates >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)


  expected_results <- PerformanceAnalytics::M2.shrink(as.matrix(expected_results[,-1]), target = 4)
  expected_results <- expected_results$M2sh

  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_upd_ref = daily_active_returns_upd_ref,
    covariance_matrix_sample_size = covariance_matrix_sample_size,
    covariance_estimation_method = "Shrink_CC",
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

test_that("estimate_covariance_matrix works for PCA1", {
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

  #exoected_results
  expected_results <- daily_active_returns_upd_ref[, c("dates", eligible_stocks)]

  #min date
  min_date <- expected_results$dates[length(expected_results$dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(expected_results$dates >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)

  #how many factors
  number_of_factors <- which(cumsum(stats::prcomp(cov(expected_results[,-1]))$sdev/sum(stats::prcomp(cov(expected_results[,-1]))$sdev)) >= 0.90)[1]
  expected_results <- PortfolioAnalytics::extractCovariance(
    PortfolioAnalytics::statistical.factor.model(xts::xts(expected_results[,-1], order.by = expected_results$dates), number_of_factors))


  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_upd_ref = daily_active_returns_upd_ref,
    covariance_matrix_sample_size = covariance_matrix_sample_size,
    covariance_estimation_method = "PCA1",
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

test_that("estimate_covariance_matrix works for PCA2", {
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

  #exoected_results
  expected_results <- daily_active_returns_upd_ref[, c("dates", eligible_stocks)]

  #min date
  min_date <- expected_results$dates[length(expected_results$dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(expected_results$dates >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_sample = expected_results, groups_m_d_ref = stocks_groups_m_d_ref)

  #how many factors
  number_of_factors <- log(4)
  expected_results <- PortfolioAnalytics::extractCovariance(
    PortfolioAnalytics::statistical.factor.model(xts::xts(expected_results[,-1], order.by = expected_results$dates), number_of_factors))
  colnames(expected_results) <- eligible_stocks


  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_upd_ref = daily_active_returns_upd_ref,
    covariance_matrix_sample_size = covariance_matrix_sample_size,
    covariance_estimation_method = "PCA2",
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})
