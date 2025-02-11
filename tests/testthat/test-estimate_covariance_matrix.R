test_that("estimate_covariance_matrix works for sample and raw_returns", {
  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  covariance_matrix_sample_size <- 200
  daily_stock_returns_m_xts <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]

  #eligible stocks
  eligible_stocks <- c("Stock A", "Stock B", "Stock C", "Stock E")

  #exoected_results
  expected_results <- daily_stock_returns_m_xts[,eligible_stocks]

  #min date
  dates <- zoo::index(expected_results) %>% as.Date()
  min_date <- dates[length(dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(zoo::index(expected_results) >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_m_xts_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)


  expected_results <- cov(expected_results)

  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts,
    cov_matrix_sample_size = covariance_matrix_sample_size,
    cov_estimation_method = "sample",
    active_returns = FALSE,
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

test_that("estimate_covariance_matrix works for ewma and active returns", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  covariance_matrix_sample_size <- 200
  daily_stock_returns_m_xts <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]


  #eligible stocks
  eligible_stocks <- c("Stock A", "Stock B", "Stock C", "Stock E")

  #exoected_results
  expected_results <- daily_stock_returns_m_xts[,eligible_stocks]

  #min date
  dates <- zoo::index(expected_results) %>% as.Date()
  min_date <- dates[length(dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(zoo::index(expected_results) >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_m_xts_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)

  selected_benchmark_returns_m_xts_upd_ref <-
    daily_benchmark_returns_m_xts[which(zoo::index(daily_benchmark_returns_m_xts) %in% zoo::index(expected_results)), "ibov" ]

  #calculate active
  expected_results$`Stock A` <- ((1 + expected_results$`Stock A`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock B` <- ((1 + expected_results$`Stock B`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock C` <- ((1 + expected_results$`Stock C`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock E` <- ((1 + expected_results$`Stock E`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100

  expected_results <- PerformanceAnalytics::M2.ewma(as.matrix(expected_results))

  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts,
    cov_matrix_sample_size = covariance_matrix_sample_size,
    cov_estimation_method = "ewma",
    active_returns = TRUE,
    selected_benchmark_m_xts_upd_ref = selected_benchmark_returns_m_xts_upd_ref,
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

test_that("estimate_covariance_matrix works for shrink_cc and active returns", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  covariance_matrix_sample_size <- 200
  daily_stock_returns_m_xts <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]


  #eligible stocks
  eligible_stocks <- c("Stock A", "Stock B", "Stock C", "Stock E")

  #exoected_results
  expected_results <- daily_stock_returns_m_xts[,eligible_stocks]

  #min date
  dates <- zoo::index(expected_results) %>% as.Date()
  min_date <- dates[length(dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(zoo::index(expected_results) >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_m_xts_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)

  selected_benchmark_returns_m_xts_upd_ref <-
    daily_benchmark_returns_m_xts[which(zoo::index(daily_benchmark_returns_m_xts) %in% zoo::index(expected_results)), "ibov" ]

  #calculate active
  expected_results$`Stock A` <- ((1 + expected_results$`Stock A`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock B` <- ((1 + expected_results$`Stock B`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock C` <- ((1 + expected_results$`Stock C`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock E` <- ((1 + expected_results$`Stock E`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100

  expected_results <- PerformanceAnalytics::M2.shrink(as.matrix(expected_results), target = 4)
  expected_results <- expected_results$M2sh

  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts,
    cov_matrix_sample_size = covariance_matrix_sample_size,
    cov_estimation_method = "shrink_cc",
    active_returns = TRUE,
    selected_benchmark_m_xts_upd_ref = selected_benchmark_returns_m_xts_upd_ref,
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

test_that("estimate_covariance_matrix works for pca1 and active_returns", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  covariance_matrix_sample_size <- 200
  daily_stock_returns_m_xts <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]


  #eligible stocks
  eligible_stocks <- c("Stock A", "Stock B", "Stock C", "Stock E")

  #exoected_results
  expected_results <- daily_stock_returns_m_xts[,eligible_stocks]

  #min date
  dates <- zoo::index(expected_results) %>% as.Date()
  min_date <- dates[length(dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(zoo::index(expected_results) >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_m_xts_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)

  selected_benchmark_returns_m_xts_upd_ref <-
    daily_benchmark_returns_m_xts[which(zoo::index(daily_benchmark_returns_m_xts) %in% zoo::index(expected_results)), "ibov" ]

  #calculate active
  expected_results$`Stock A` <- ((1 + expected_results$`Stock A`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock B` <- ((1 + expected_results$`Stock B`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock C` <- ((1 + expected_results$`Stock C`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock E` <- ((1 + expected_results$`Stock E`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100


  #how many factors
  number_of_factors <- which(cumsum(stats::prcomp(cov(expected_results))$sdev/sum(stats::prcomp(cov(expected_results))$sdev)) >= 0.90)[1]
  expected_results <- PortfolioAnalytics::extractCovariance(PortfolioAnalytics::statistical.factor.model(expected_results, number_of_factors))

  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts,
    cov_matrix_sample_size = covariance_matrix_sample_size,
    cov_estimation_method = "pca1",
    active_returns = TRUE,
    selected_benchmark_m_xts_upd_ref = selected_benchmark_returns_m_xts_upd_ref,
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

test_that("estimate_covariance_matrix works for pca2 and active_returns", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-04-15"

  #Generate return sample
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  covariance_matrix_sample_size <- 200
  daily_stock_returns_m_xts <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]


  #eligible stocks
  eligible_stocks <- c("Stock A", "Stock B", "Stock C", "Stock E")

  #exoected_results
  expected_results <- daily_stock_returns_m_xts[,eligible_stocks]

  #min date
  dates <- zoo::index(expected_results) %>% as.Date()
  min_date <- dates[length(dates) - covariance_matrix_sample_size]
  expected_results <- expected_results[which(zoo::index(expected_results) >= min_date),]

  #clean
  expected_results <- clean_returns_sample(returns_m_xts_sample = expected_results,
                                           groups_m_d_ref = stocks_groups_m_d_ref)

  selected_benchmark_returns_m_xts_upd_ref <-
    daily_benchmark_returns_m_xts[which(zoo::index(daily_benchmark_returns_m_xts) %in% zoo::index(expected_results)), "ibov" ]

  #calculate active
  expected_results$`Stock A` <- ((1 + expected_results$`Stock A`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock B` <- ((1 + expected_results$`Stock B`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock C` <- ((1 + expected_results$`Stock C`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100
  expected_results$`Stock E` <- ((1 + expected_results$`Stock E`/100)/(1 + selected_benchmark_returns_m_xts_upd_ref$ibov/100) - 1)*100


  #how many factors
  number_of_factors <- round(log(4))
  expected_results <- PortfolioAnalytics::extractCovariance(PortfolioAnalytics::statistical.factor.model(expected_results, number_of_factors))
  rownames(expected_results) <- eligible_stocks
  colnames(expected_results) <- eligible_stocks

  results <- estimate_covariance_matrix(
    tickers = eligible_stocks,
    returns_m_xts_upd_ref = daily_stock_returns_m_xts,
    cov_matrix_sample_size = covariance_matrix_sample_size,
    cov_estimation_method = "pca2",
    active_returns = TRUE,
    selected_benchmark_m_xts_upd_ref = selected_benchmark_returns_m_xts_upd_ref,
    groups_m_d_ref = stocks_groups_m_d_ref
  )

  expect_equal(expected_results, results)

})

