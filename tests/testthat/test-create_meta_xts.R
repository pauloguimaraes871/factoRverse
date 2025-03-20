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

test_that("create_meta_xts works for a meta_dataframe", {

  #Initiate a meta_dataframe with daily returns
  dates <- seq.Date(from = as.Date("2002-01-01"), to = as.Date("2002-02-28"), by = "days")

  set.seed(123)
  happy_stock <- rnorm(length(dates), 0, 1) #No NAS
  ipo_stock <- c(rep(NA, 20), rnorm(length(dates) - 20, 0, 1)) #IPO
  delisted_stock <- c(rnorm(length(dates) - 22, 0, 1), rep(NA, 22)) #Delisted
  iliquid_stock <- c(0, 0, NA, rnorm(length(dates) - 6, 0, 1), 0, NA, 0) #IPO and Delisted
  wrong_stock <- rnorm(length(dates), 0, 1) #No NAS before initial listing and after unlisting

  #Combine dates and tickers (happy, ipo, delisted and illiquid) into a grid)
  daily_returns_m_df <- expand.grid(c("happy", "ipo", "delisted", "illiquid", "wrong"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = c(delisted_stock, happy_stock, iliquid_stock, ipo_stock, wrong_stock))
  daily_returns_m_df$tickers <- as.character(daily_returns_m_df$tickers)
  daily_returns_m_df <- create_meta_dataframe(daily_returns_m_df, type = "raw", meta_dataframe_name = "daily_bronze")

  #Create a features_m_df
  features_m_df <- daily_returns_m_df@data %>% dplyr::filter(dates %in% c("2002-01-15", "2002-02-15")) %>%
    dplyr::mutate(book_yield = rnorm(nrow(.)), dy_med_36m = rnorm(nrow(.)))

  #For each row in features_m_df, check if corresponding ret is NA and replace it by NA if it is
  features_m_df <- features_m_df %>% dplyr::mutate(book_yield = ifelse(is.na(ret), NA, book_yield),
                                                   dy_med_36m = ifelse(is.na(ret), NA, dy_med_36m)) %>%
    create_meta_dataframe(type = "raw", meta_dataframe_name = "bronze")

  date_first_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_first_quote = as.Date(c("2000-01-12", "2002-01-21", "2000-01-05", "2000-02-25", "2002-01-06"))
  )

  date_last_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_last_quote = as.Date(c("2002-02-28", "2002-02-28", "2002-02-05", "2002-02-25", "2002-02-06"))
  )

  #Create tickers catalog
  tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = daily_returns_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #read catalog
  pre_silver_daily_returns_m_df <- read_tickers_catalog(
    daily_returns_m_df,
    tickers_catalog = tickers_catalog
  )

  #Check that read_tickers_catalog correctly assigned NA to happy, ipo, delisted and wrong
  first_date_happy <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["happy"]) %>% dplyr::slice_min(dates,n = 1)
  expect_equal(first_date_happy$dates, dates[1])

  first_date_ipo <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["ipo"]) %>% dplyr::slice_min(dates,n = 1)
  expect_equal(first_date_ipo$dates, date_first_quote$date_first_quote[2])

  first_date_delisted <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["delisted"]) %>% dplyr::slice_min(dates,n = 1)
  expect_equal(first_date_delisted$dates, first_date_happy$dates) #Delisted and happy have the same first date

  first_date_illiquid <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["illiquid"]) %>% dplyr::slice_min(dates,n = 1)
  expect_equal(first_date_delisted$dates, first_date_happy$dates) #Illiquid and happy have the same first date

  first_date_wrong <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["wrong"]) %>% dplyr::slice_min(dates,n = 1)
  expect_equal(first_date_wrong$dates, date_first_quote$date_first_quote[5])

  last_date_happy <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["happy"]) %>% dplyr::slice_max(dates,n = 1)
  expect_equal(last_date_happy$dates, date_last_quote$date_last_quote[1])

  last_date_ipo <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["ipo"]) %>% dplyr::slice_max(dates,n = 1)
  expect_equal(last_date_ipo$dates, last_date_happy$dates)

  last_date_delisted <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["delisted"]) %>% dplyr::slice_max(dates,n = 1)
  expect_equal(last_date_delisted$dates, date_last_quote$date_last_quote[3] + 10) #stock is only considered delisted after 10 days without trading

  last_date_illiquid <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["illiquid"]) %>% dplyr::slice_max(dates,n = 1)
  expect_equal(last_date_illiquid$dates, last_date_happy$dates)

  last_date_wrong <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers %in% tickers_catalog@perm_id["wrong"]) %>% dplyr::slice_max(dates,n = 1)
  expect_equal(last_date_wrong$dates, date_last_quote$date_last_quote[5] + 10) #stock is only considered delisted after 10 days without trading

  #Create meta xts
  suppressWarnings(
    meta_xts <- create_meta_xts(data = pre_silver_daily_returns_m_df)
  )

  #Check that illiquid matches pre_silver_daily
  expect_equal(meta_xts@data[, tickers_catalog@perm_id["illiquid"]] %>% as.numeric(),
               iliquid_stock)

  #Check that delisted has only NAs after date_last_quote
  expect_equal(meta_xts@data[, tickers_catalog@perm_id["delisted"]] %>% as.numeric(),
               delisted_stock)

  #Check that ipo has only NAs before date_first_quote
  expect_equal(meta_xts@data[, tickers_catalog@perm_id["ipo"]] %>% as.numeric(),
               ipo_stock)

  #Check that WRONG has only NAs before date_first_quote and after date_last_quote + 10
  expect_false(identical(meta_xts@data[, tickers_catalog@perm_id["wrong"]] %>% as.numeric(), wrong_stock))
  expect_equal(meta_xts@data["2002-01-01/2002-01-05", tickers_catalog@perm_id["wrong"]] %>% as.numeric() %>% unique(),
               NA_real_)
  expect_equal(meta_xts@data["2002-02-17/2002-02-28", tickers_catalog@perm_id["wrong"]] %>% as.numeric() %>% unique(),
               NA_real_)

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
    "Error: No valid dates found. Please provide a 'dates' column, valid rownames, or pass a 'dates' argument."
  )



})
