test_that("create_meta_xts works for a xts object with no type specification a decimal warning", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)

  mocked_backtest_returns_m_xts <- xts::as.xts(data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.035),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.025),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.005, sd = 0.03),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 0.015, sd = 0.035),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.02),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 0.025, sd = 0.035),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.0075)
  ), order.by = unique(signals_m_df$dates))


  expect_message(
    #Message for detected frequency
  expect_message(
    #Message for asset type
  expect_warning(
    #Warning for decimal form
  results <- create_meta_xts(mocked_backtest_returns_m_xts)
  ), "Asset_type not identified for 'returns_meta_xts' subclass"
  ), "Detected frequency is: monthly"
  )

  #Expect results
  expect_equal(results@data, mocked_backtest_returns_m_xts)
  expect_equal(results@asset_type, "not_identified")
  expect_equal(results@meta_xts_name, "not_identified")
  expect_equal(results@workflow, NULL)
  expect_equal(results@n_dates, length(unique(signals_m_df$dates)))
  expect_equal(results@frequency, "monthly")
  expect_equal(results@assets, c("book_yield", "dy_med_36m", "eps_yield", "mom_res_12m", "roe_3m", "sharpe_6m", "low_vol_36m"))
  expect_equal(results@metric_name, "returns")


})

test_that("create_meta_xts works for a xts object with more specification and no decimal warning", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)

  mocked_backtest_returns_m_xts <- xts::as.xts(data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 1, sd = 0.35),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.75, sd = 0.25),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 5, sd = 3),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 1.5, sd = 0.35),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 1, sd = 2),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 2.5, sd = 0.35),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 7.5, sd = 0.75)
  ), order.by = unique(signals_m_df$dates))


  expect_message(
    #Message for detected frequency
    expect_no_warning(
      #Warning for decimal form
      results <- create_meta_xts(mocked_backtest_returns_m_xts, type = "returns", asset_type = "signals", meta_xts_name = "mocked",
                                 metric_name = "monthly_raw_returns")
    ), "Detected frequency is: monthly"
  )

  #Expect results
  expect_equal(results@data, mocked_backtest_returns_m_xts)
  expect_equal(results@asset_type, "signals")
  expect_equal(results@meta_xts_name, "mocked")
  expect_equal(results@workflow, NULL)
  expect_equal(results@n_dates, length(unique(signals_m_df$dates)))
  expect_equal(results@frequency, "monthly")
  expect_equal(results@assets, c("book_yield", "dy_med_36m", "eps_yield", "mom_res_12m", "roe_3m", "sharpe_6m", "low_vol_36m"))
  expect_equal(results@metric_name, "monthly_raw_returns")


})

test_that("create_meta_xts works for a data.frame object wide format", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)
  mocked_backtest_returns_m_xts <- data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 1, sd = 0.35),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.75, sd = 0.25),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 5, sd = 3),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 1.5, sd = 0.35),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 1, sd = 2),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 2.5, sd = 0.35),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 7.5, sd = 0.75)
  )

  expect_message(
    #Message for detected frequency
    expect_no_warning(
      #Warning for decimal form
      results <- create_meta_xts(mocked_backtest_returns_m_xts, type = "returns", asset_type = "signals", meta_xts_name = "mocked",
                                 metric_name = "monthly_raw_returns", dates = unique(signals_m_df$dates))
    ), "Detected frequency is: monthly"
  )

  #Expect results
  expect_equal(results@data, xts::as.xts(mocked_backtest_returns_m_xts, order.by = unique(signals_m_df$dates)))
  expect_equal(results@asset_type, "signals")
  expect_equal(results@meta_xts_name, "mocked")
  expect_equal(results@workflow, NULL)
  expect_equal(results@n_dates, length(unique(signals_m_df$dates)))
  expect_equal(results@frequency, "monthly")
  expect_equal(results@assets, c("book_yield", "dy_med_36m", "eps_yield", "mom_res_12m", "roe_3m", "sharpe_6m", "low_vol_36m"))
  expect_equal(results@metric_name, "monthly_raw_returns")

})

test_that("create_meta_xts works for a data.frame object with long format (including presence of NAs)", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  signals_m_df[1, "book_yield"] <- NA #Create a NA to mimick an IPO
  signals_m_df[c(107, 108), "dy_med_36m"] <- NA #Create a NA to mimick a delisting

  results <- create_meta_xts(data = signals_m_df %>% dplyr::select(id, tickers, dates, book_yield, dy_med_36m),
                             type = "metrics", data_format = "long", meta_xts_name = "signals_m_df"
  )

  #Check names match columns
  expect_equal(names(results), c("book_yield", "dy_med_36m"))

  #Check data is wide
  expect_equal(colnames(results$book_yield@data), signals_m_df$tickers %>% unique())
  expect_equal(colnames(results$dy_med_36m@data), signals_m_df$tickers %>% unique())

  #Check dates are right
  expect_equal(zoo::index(results$book_yield@data) %>% as.character(), signals_m_df$dates %>% unique() %>% as.character())
  expect_equal(zoo::index(results$dy_med_36m@data) %>% as.character(), signals_m_df$dates %>% unique() %>% as.character())

  #Check for random inputs
  expect_equal(signals_m_df %>% dplyr::filter(tickers == "AMBP3") %>% dplyr::pull(book_yield),
               results$book_yield@data$AMBP3 %>% as.vector()
  )
  expect_equal(signals_m_df %>% dplyr::filter(tickers == "ZAMP3") %>% dplyr::pull(dy_med_36m),
               results$dy_med_36m@data$ZAMP3 %>% as.vector()
  )
  #This one is originally delisted
  expect_equal(c(signals_m_df %>% dplyr::filter(tickers == "MERC3") %>% dplyr::pull(dy_med_36m), NA, NA, NA),
               results$dy_med_36m@data$MERC3 %>% as.vector()
  )
  #This one is faked IPO
  expect_equal(signals_m_df %>% dplyr::filter(tickers == "AALR3") %>% dplyr::pull(book_yield),
               results$book_yield@data$AALR3 %>% as.vector()
  )
  #This one is faked delisting
  expect_equal(signals_m_df %>% dplyr::filter(tickers == "AHEB3") %>% dplyr::pull(dy_med_36m),
               results$dy_med_36m@data$AHEB3 %>% as.vector()
  )




})

test_that("create_meta_xts fails for not providing dates, tickers or metric_name correctly", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  expect_error(
    create_meta_xts(data = signals_m_df %>% dplyr::select(id, tickers, -dates, book_yield, dy_med_36m),
                    type = "metrics", data_format = "long", meta_xts_name = "signals_m_df"
    ), "Error: For long format, the data.frame must contain 'tickers' and 'dates' columns.")

  expect_error(
    create_meta_xts(data = signals_m_df %>% dplyr::select(id, -tickers, dates, book_yield, dy_med_36m),
                    type = "metrics", data_format = "long", meta_xts_name = "signals_m_df"
    ), "Error: For long format, the data.frame must contain 'tickers' and 'dates' columns.")


  expect_error(
    create_meta_xts(data = signals_m_df %>% dplyr::select(id, tickers, dates, book_yield, dy_med_36m),
                    type = "metrics", data_format = "long", meta_xts_name = "signals_m_df", metric_name = c("ok", "bro", "yeah")
    ), "Error: When data_format is 'long' and metric_name is provided as a vector, its length must equal the number of feature columns.")


  set.seed(123)
  mocked_backtest_returns_m_xts <- data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 1, sd = 0.35),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.75, sd = 0.25),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 5, sd = 3),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 1.5, sd = 0.35),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 1, sd = 2),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 2.5, sd = 0.35),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 7.5, sd = 0.75)
  )

  expect_error(
    create_meta_xts(mocked_backtest_returns_m_xts, type = "returns", asset_type = "signals", meta_xts_name = "mocked",
                    metric_name = "monthly_raw_returns"),
    "Error: No valid dates found. Please provide a 'dates' column or pass a 'dates' argument."
  )



})
