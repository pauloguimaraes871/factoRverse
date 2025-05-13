test_that("create_target_m_df works for a base case with happy, ipo, delisted, illiquid and wrong stocks for a 3m horizon", {

  #Initiate a meta_dataframe with daily returns
  dates <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2002-02-15"), by = "days")

  set.seed(123)
  happy_stock <- rnorm(length(dates), 0, 1) #No NAS
  ipo_stock <- c(rep(NA, 50), rnorm(length(dates) - 50, 0, 1)) #IPO
  delisted_stock <- c(rnorm(length(dates) - 100, 0, 1), rep(NA, 100)) #Delisted
  iliquid_stock <- rnorm(length(dates), 0, 1) #Illiq
  iliquid_stock[sample(size = 80, x = length(iliquid_stock))] <- sample(size = 80, c(0, NA), replace = TRUE)
  wrong_stock <- rnorm(length(dates), 0, 1) #No NAS before initial listing and after unlisting

  #Combine dates and tickers (happy, ipo, delisted and illiquid) into a grid)
  daily_returns_m_df <- expand.grid(c("happy", "ipo", "delisted", "illiquid", "wrong"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = c(delisted_stock, happy_stock, iliquid_stock, ipo_stock, wrong_stock))
  daily_returns_m_df$tickers <- as.character(daily_returns_m_df$tickers)
  daily_returns_m_df <- create_meta_dataframe(daily_returns_m_df, type = "raw", meta_dataframe_name = "daily_bronze")

  #Mock daily bench returns
  daily_bench_returns_m_df <- expand.grid(c("ibov", "smll", "idiv"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = rnorm(nrow(.)))
  daily_bench_returns_m_df$tickers <- as.character(daily_bench_returns_m_df$tickers)
  daily_bench_returns_m_df <- create_meta_dataframe(daily_bench_returns_m_df, type = "raw", meta_dataframe_name = "bench_bronze")

  suppressWarnings(
  daily_bench_returns_m_xts <- create_meta_xts(daily_bench_returns_m_df)
  )

  #Create a feat_m_df
  features_dates <- lubridate::add_with_rollback(as.Date("1999-12-15"), months(1:26))

  feat_m_df <- daily_returns_m_df@data %>%
    dplyr::filter(dates %in% seq.Date(from = as.Date("2000-01-15"), to = as.Date("2002-02-15"), by = "months")) %>%
    dplyr::mutate(book_yield = rnorm(nrow(.)), dy_med_36m = rnorm(nrow(.)))

  #For each row in features_m_df, check if corresponding ret is NA and replace it by NA if it is
  feat_m_df <- feat_m_df %>% dplyr::mutate(book_yield = ifelse(is.na(ret), NA, book_yield),
                                                   dy_med_36m = ifelse(is.na(ret), NA, dy_med_36m)) %>%
    create_meta_dataframe(type = "raw", meta_dataframe_name = "bronze")

  date_first_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_first_quote = as.Date(c("1999-01-12", "2000-02-20", "1999-01-05", "1999-01-12", "2001-01-06"))
  )

  date_last_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_last_quote = as.Date(c("2002-02-15", "2002-02-15", "2001-11-07", "2002-02-15", "2002-02-04"))
  )

  #Create tickers catalog
  tickers_catalog_features <- create_tickers_catalog(
    raw_features_m_df = feat_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #read catalog
  pre_silver_daily_returns_m_df <- read_tickers_catalog(
    daily_returns_m_df,
    tickers_catalog = tickers_catalog_features
  )
  pre_silver_features_m_df <- read_tickers_catalog(
    feat_m_df,
    tickers_catalog = tickers_catalog_features
  )

  #Create target_m_df
  suppressWarnings(
  results <- create_target_m_df(
    daily_returns_m_df = pre_silver_daily_returns_m_df,
    features_m_df = pre_silver_features_m_df,
    daily_bench_returns_m_xts = daily_bench_returns_m_xts,
    past_ret_column = "ret",
    selected_bench = "ibov",
    fwd_horizon = 3,
    active_returns = TRUE,
    parallel = FALSE
  )
  )


  #Check that all expected ids are contemplated
  expect_equal(
    results@data %>% dplyr::pull(id),
    pre_silver_features_m_df@data %>% dplyr::pull(id)
  )

  #Check that all dates are contemplated
  expect_equal(
    results@data %>% dplyr::pull(dates),
    pre_silver_features_m_df@data %>% dplyr::pull(dates)
  )

  #Check that calculation is correct for some specific cases
  ticker_1 <- tickers_catalog_features@perm_id["delisted"] %>% unname() #delisted
  ticker_2 <- tickers_catalog_features@perm_id["happy"] %>% unname() #happy
  ticker_3 <- tickers_catalog_features@perm_id["ipo"] %>% unname() #happy
  ticker_4 <- tickers_catalog_features@perm_id["illiquid"] %>% unname() #happy
  ticker_5 <- tickers_catalog_features@perm_id["wrong"] %>% unname() #happy


  #Get obs for next 3 months beginning in 2000-01-15
  ticker_1_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-04-15"))
  ticker_2_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-04-15"))
  ticker_4_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-04-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2000-01-15/2000-04-15", "ibov"]

  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret)  %>%
    dplyr::bind_rows(ticker_4_fwd_ret)
  #replace na with 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    first_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  tickers_universe_m_d_ref <- summarize_performance(first_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = TRUE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2000-01-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)

  first_results <- results@data %>% dplyr::filter(dates == as.Date("2000-01-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(first_results)

  standardize_na <- function(df) {
    df[] <- lapply(df, function(col) ifelse(is.na(col), NA_real_, col))
    return(df)
  }

  expect_equal(standardize_na(first_results), standardize_na(tickers_universe_m_d_ref))


  #Check that ipo and wrong are not present
  expect_false(unname(tickers_catalog_features@perm_id["ipo"]) %in% first_results$tickers)
  expect_false(unname(tickers_catalog_features@perm_id["wrong"]) %in% first_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["delisted"]) %in% first_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% first_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% first_results$tickers)


  #Get obs for next 3 months beginning in 2001-01-15
  ticker_1_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_2_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_3_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_4_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_5_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_5, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2001-01-15/2001-04-15", "ibov"]


  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret) %>% dplyr::bind_rows(ticker_3_fwd_ret) %>%
    dplyr::bind_rows(ticker_4_fwd_ret) %>% dplyr::bind_rows(ticker_5_fwd_ret)

  #replace na to 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    second_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  tickers_universe_m_d_ref <- summarize_performance(second_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = TRUE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2001-01-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  second_results <- results@data %>% dplyr::filter(dates == as.Date("2001-01-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(second_results)
  expect_equal(second_results, tickers_universe_m_d_ref)

  #Check that all are present
  expect_true(unname(tickers_catalog_features@perm_id["ipo"]) %in% second_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["wrong"]) %in% second_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["delisted"]) %in% second_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% second_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% second_results$tickers)





  #Get obs for next 3 months beginning in 2001-11-15
  ticker_1_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_2_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_3_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_4_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_5_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_5, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2001-11-15/2002-02-15", "ibov"]

  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret) %>% dplyr::bind_rows(ticker_3_fwd_ret) %>%
    dplyr::bind_rows(ticker_4_fwd_ret) %>% dplyr::bind_rows(ticker_5_fwd_ret)
  #replace NA with 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    third_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  #Replace NAs in delisted with bench ret
  third_fwd_ret_xts@data[c(4:93), 1] <- bench_fwd[4:93]
  third_fwd_ret_xts@data[93, 5] <- bench_fwd[93]

  tickers_universe_m_d_ref <- summarize_performance(third_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = TRUE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2001-11-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  third_results <- results@data %>% dplyr::filter(dates == as.Date("2001-11-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(third_results)
  expect_equal(standardize_na(third_results), standardize_na(tickers_universe_m_d_ref))

  #Check that all are present
  expect_true(unname(tickers_catalog_features@perm_id["ipo"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["wrong"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["delisted"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% third_results$tickers)



  #Check that last tiem period has NAs
  fourth_results <- results@data %>% dplyr::filter(dates == as.Date("2001-12-15"))


  #Check that is all NAs
  expect_true(all(is.na(fourth_results[,-c(1:3)])))

})

test_that("create_target_m_df works for a base case with happy, ipo, delisted, illiquid and wrong stocks for a 1m horizon in parallel", {

  #Initiate a meta_dataframe with daily returns
  dates <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2002-02-15"), by = "days")

  set.seed(123)
  happy_stock <- rnorm(length(dates), 0, 1) #No NAS
  ipo_stock <- c(rep(NA, 50), rnorm(length(dates) - 50, 0, 1)) #IPO
  delisted_stock <- c(rnorm(length(dates) - 100, 0, 1), rep(NA, 100)) #Delisted
  iliquid_stock <- rnorm(length(dates), 0, 1) #Illiq
  iliquid_stock[sample(size = 80, x = length(iliquid_stock))] <- sample(size = 80, c(0, NA), replace = TRUE)
  wrong_stock <- rnorm(length(dates), 0, 1) #No NAS before initial listing and after unlisting

  #Combine dates and tickers (happy, ipo, delisted and illiquid) into a grid)
  daily_returns_m_df <- expand.grid(c("happy", "ipo", "delisted", "illiquid", "wrong"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = c(delisted_stock, happy_stock, iliquid_stock, ipo_stock, wrong_stock))
  daily_returns_m_df$tickers <- as.character(daily_returns_m_df$tickers)
  daily_returns_m_df <- create_meta_dataframe(daily_returns_m_df, type = "raw", meta_dataframe_name = "daily_bronze")

  #Mock daily bench returns
  daily_bench_returns_m_df <- expand.grid(c("ibov", "smll", "idiv"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = rnorm(nrow(.)))
  daily_bench_returns_m_df$tickers <- as.character(daily_bench_returns_m_df$tickers)
  daily_bench_returns_m_df <- create_meta_dataframe(daily_bench_returns_m_df, type = "raw", meta_dataframe_name = "bench_bronze")

  suppressWarnings(
    daily_bench_returns_m_xts <- create_meta_xts(daily_bench_returns_m_df)
  )

  #Create a feat_m_df
  features_dates <- lubridate::add_with_rollback(as.Date("1999-12-15"), months(1:26))

  feat_m_df <- daily_returns_m_df@data %>%
    dplyr::filter(dates %in% seq.Date(from = as.Date("2000-01-15"), to = as.Date("2002-02-15"), by = "months")) %>%
    dplyr::mutate(book_yield = rnorm(nrow(.)), dy_med_36m = rnorm(nrow(.)))

  #For each row in features_m_df, check if corresponding ret is NA and replace it by NA if it is
  feat_m_df <- feat_m_df %>% dplyr::mutate(book_yield = ifelse(is.na(ret), NA, book_yield),
                                           dy_med_36m = ifelse(is.na(ret), NA, dy_med_36m)) %>%
    create_meta_dataframe(type = "raw", meta_dataframe_name = "bronze")

  date_first_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_first_quote = as.Date(c("1999-01-12", "2000-02-20", "1999-01-05", "1999-01-12", "2001-01-06"))
  )

  date_last_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_last_quote = as.Date(c("2002-02-15", "2002-02-15", "2001-11-07", "2002-02-15", "2002-02-04"))
  )

  #Create tickers catalog
  tickers_catalog_features <- create_tickers_catalog(
    raw_features_m_df = feat_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #read catalog
  pre_silver_daily_returns_m_df <- read_tickers_catalog(
    daily_returns_m_df,
    tickers_catalog = tickers_catalog_features
  )
  pre_silver_features_m_df <- read_tickers_catalog(
    feat_m_df,
    tickers_catalog = tickers_catalog_features
  )

  #Create target_m_df
  suppressWarnings(
    results <- create_target_m_df(
      daily_returns_m_df = pre_silver_daily_returns_m_df,
      features_m_df = pre_silver_features_m_df,
      daily_bench_returns_m_xts = daily_bench_returns_m_xts,
      past_ret_column = "ret",
      selected_bench = "ibov",
      fwd_horizon = 1,
      active_returns = FALSE,
      parallel = TRUE
    )
  )


  #Check that all expected ids are contemplated
  expect_equal(
    results@data %>% dplyr::pull(id),
    pre_silver_features_m_df@data %>% dplyr::pull(id)
  )

  #Check that all dates are contemplated
  expect_equal(
    results@data %>% dplyr::pull(dates),
    pre_silver_features_m_df@data %>% dplyr::pull(dates)
  )

  #Check that calculation is correct for some specific cases
  ticker_1 <- tickers_catalog_features@perm_id["delisted"] %>% unname() #delisted
  ticker_2 <- tickers_catalog_features@perm_id["happy"] %>% unname() #happy
  ticker_3 <- tickers_catalog_features@perm_id["ipo"] %>% unname() #happy
  ticker_4 <- tickers_catalog_features@perm_id["illiquid"] %>% unname() #happy
  ticker_5 <- tickers_catalog_features@perm_id["wrong"] %>% unname() #happy


  #Get obs for next 1 months beginning in 2000-01-15
  ticker_1_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-02-15"))
  ticker_2_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-02-15"))
  ticker_4_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-02-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2000-01-15/2000-02-15", "ibov"]

  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret)  %>%
    dplyr::bind_rows(ticker_4_fwd_ret)
  #replace na with 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    first_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  tickers_universe_m_d_ref <- summarize_performance(first_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = FALSE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2000-01-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)

  first_results <- results@data %>% dplyr::filter(dates == as.Date("2000-01-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(first_results)

  standardize_na <- function(df) {
    df[] <- lapply(df, function(col) ifelse(is.na(col), NA_real_, col))
    return(df)
  }

  expect_equal(standardize_na(first_results), standardize_na(tickers_universe_m_d_ref))


  #Check that ipo and wrong are not present
  expect_false(unname(tickers_catalog_features@perm_id["ipo"]) %in% first_results$tickers)
  expect_false(unname(tickers_catalog_features@perm_id["wrong"]) %in% first_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["delisted"]) %in% first_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% first_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% first_results$tickers)


  #Get obs for next 1 months beginning in 2001-01-15
  ticker_1_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-02-15"))
  ticker_2_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-02-15"))
  ticker_3_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-02-15"))
  ticker_4_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-02-15"))
  ticker_5_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_5, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-02-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2001-01-15/2001-02-15", "ibov"]


  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret) %>% dplyr::bind_rows(ticker_3_fwd_ret) %>%
    dplyr::bind_rows(ticker_4_fwd_ret) %>% dplyr::bind_rows(ticker_5_fwd_ret)

  #replace na to 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    second_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  tickers_universe_m_d_ref <- summarize_performance(second_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = FALSE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2001-01-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  second_results <- results@data %>% dplyr::filter(dates == as.Date("2001-01-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(second_results)
  expect_equal(second_results, tickers_universe_m_d_ref)

  #Check that all are present
  expect_true(unname(tickers_catalog_features@perm_id["ipo"]) %in% second_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["wrong"]) %in% second_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["delisted"]) %in% second_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% second_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% second_results$tickers)





  #Get obs for next 1 months beginning in 2001-11-15
  ticker_1_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2001-11-15"), dates <= as.Date("2001-12-15"))
  ticker_2_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2001-11-15"), dates <= as.Date("2001-12-15"))
  ticker_3_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2001-11-15"), dates <= as.Date("2001-12-15"))
  ticker_4_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2001-11-15"), dates <= as.Date("2001-12-15"))
  ticker_5_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_5, dates >= as.Date("2001-11-15"), dates <= as.Date("2001-12-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2001-11-15/2001-12-15", "ibov"]

  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret) %>% dplyr::bind_rows(ticker_3_fwd_ret) %>%
    dplyr::bind_rows(ticker_4_fwd_ret) %>% dplyr::bind_rows(ticker_5_fwd_ret)
  #replace NA with 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    third_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  #Replace NAs in delisted with bench ret
  third_fwd_ret_xts@data[c(4:31), 1] <- bench_fwd[4:31]

  tickers_universe_m_d_ref <- summarize_performance(third_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = FALSE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2001-11-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  third_results <- results@data %>% dplyr::filter(dates == as.Date("2001-11-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(third_results)
  expect_equal(standardize_na(third_results), standardize_na(tickers_universe_m_d_ref))

  #Check that all are present
  expect_true(unname(tickers_catalog_features@perm_id["ipo"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["wrong"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["delisted"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% third_results$tickers)



  #Check that fourth result does not contain delisted
  fourth_results <- results@data %>% dplyr::filter(dates == as.Date("2002-01-15"))
  expect_true(unname(tickers_catalog_features@perm_id["ipo"]) %in% fourth_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["wrong"]) %in% fourth_results$tickers)
  expect_false(unname(tickers_catalog_features@perm_id["delisted"]) %in% fourth_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% fourth_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% fourth_results$tickers)
  expect_false(all(is.na(fourth_results)))

  #Check that is all NAs for last date and wrong is not present
  fifth_results <- results@data %>% dplyr::filter(dates == as.Date("2002-02-15"))
  expect_true(unname(tickers_catalog_features@perm_id["ipo"]) %in% fifth_results$tickers)
  expect_false(unname(tickers_catalog_features@perm_id["wrong"]) %in% fifth_results$tickers)
  expect_false(unname(tickers_catalog_features@perm_id["delisted"]) %in% fifth_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% fifth_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% fifth_results$tickers)
  expect_true(all(is.na(fifth_results[,-c(1:3)])))



})

test_that("create_target_m_df works in an update workflow", {

  #Initiate a meta_dataframe with daily returns
  dates <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2002-02-15"), by = "days")

  set.seed(123)
  happy_stock <- rnorm(length(dates), 0, 1) #No NAS
  ipo_stock <- c(rep(NA, 50), rnorm(length(dates) - 50, 0, 1)) #IPO
  delisted_stock <- c(rnorm(length(dates) - 100, 0, 1), rep(NA, 100)) #Delisted
  iliquid_stock <- rnorm(length(dates), 0, 1) #Illiq
  iliquid_stock[sample(size = 80, x = length(iliquid_stock))] <- sample(size = 80, c(0, NA), replace = TRUE)
  wrong_stock <- rnorm(length(dates), 0, 1) #No NAS before initial listing and after unlisting

  #Combine dates and tickers (happy, ipo, delisted and illiquid) into a grid)
  daily_returns_m_df <- expand.grid(c("happy", "ipo", "delisted", "illiquid", "wrong"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = c(delisted_stock, happy_stock, iliquid_stock, ipo_stock, wrong_stock))
  daily_returns_m_df$tickers <- as.character(daily_returns_m_df$tickers)
  daily_returns_m_df <- create_meta_dataframe(daily_returns_m_df, type = "raw", meta_dataframe_name = "daily_bronze")

  #Mock daily bench returns
  daily_bench_returns_m_df <- expand.grid(c("ibov", "smll", "idiv"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = rnorm(nrow(.)))
  daily_bench_returns_m_df$tickers <- as.character(daily_bench_returns_m_df$tickers)
  daily_bench_returns_m_df <- create_meta_dataframe(daily_bench_returns_m_df, type = "raw", meta_dataframe_name = "bench_bronze")

  #Create a feat_m_df
  features_dates <- lubridate::add_with_rollback(as.Date("1999-12-15"), months(1:26))

  feat_m_df <- daily_returns_m_df@data %>%
    dplyr::filter(dates %in% seq.Date(from = as.Date("2000-01-15"), to = as.Date("2002-02-15"), by = "months")) %>%
    dplyr::mutate(book_yield = rnorm(nrow(.)), dy_med_36m = rnorm(nrow(.)))

  #For each row in features_m_df, check if corresponding ret is NA and replace it by NA if it is
  feat_m_df <- feat_m_df %>% dplyr::mutate(book_yield = ifelse(is.na(ret), NA, book_yield),
                                           dy_med_36m = ifelse(is.na(ret), NA, dy_med_36m)) %>%
    create_meta_dataframe(type = "raw", meta_dataframe_name = "bronze")

  date_first_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_first_quote = as.Date(c("1999-01-12", "2000-02-20", "1999-01-05", "1999-01-12", "2001-01-06"))
  )

  date_last_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_last_quote = as.Date(c("2002-02-15", "2002-02-15", "2001-11-07", "2002-02-15", "2002-02-04"))
  )

  #Create tickers catalog
  tickers_catalog_features <- create_tickers_catalog(
    raw_features_m_df = feat_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #read catalog
  pre_silver_daily_returns_m_df <- read_tickers_catalog(
    daily_returns_m_df,
    tickers_catalog = tickers_catalog_features
  )
  pre_silver_features_m_df <- read_tickers_catalog(
    feat_m_df,
    tickers_catalog = tickers_catalog_features
  )

  #A new batch arrives
  #Initiate a meta_dataframe with daily returns
  dates <- seq.Date(from = as.Date("2002-02-16"), to = as.Date("2002-03-15"), by = "days")

  set.seed(123)
  happy_stock2 <- rnorm(length(dates), 0, 1) #No NAS
  ipo_stock <- rnorm(length(dates), 0 , 1)  #IPO
  another_ipo <- c(rep(NA, 20), rnorm(length(dates) - 20, 0, 1)) #New IPO
  delisted <- rep(NA, length(dates))
  iliquid_stock <- rnorm(length(dates), 0, 1) #Illiq
  iliquid_stock[sample(size = 5, x = length(iliquid_stock))] <- sample(size = 5, c(0, NA), replace = TRUE)
  wrong_stock <- rnorm(length(dates), 0, 1) #No NAS before initial listing and after unlisting

  #Combine dates and tickers
  new_daily_returns_m_df <- expand.grid(c("happy2", "ipo", "new_ipo", "delisted", "illiquid", "wrong"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = c(delisted, happy_stock2, iliquid_stock, ipo_stock, another_ipo, wrong_stock))
  new_daily_returns_m_df$tickers <- as.character(new_daily_returns_m_df$tickers)
  new_daily_returns_m_df <- create_meta_dataframe(new_daily_returns_m_df, type = "raw", meta_dataframe_name = "new_daily_bronze")


  #Mock daily bench returns
  new_daily_bench_returns_m_df <- expand.grid(c("ibov", "smll", "idiv"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = rnorm(nrow(.)))
  new_daily_bench_returns_m_df$tickers <- as.character(new_daily_bench_returns_m_df$tickers)
  new_daily_bench_returns_m_df <- create_meta_dataframe(new_daily_bench_returns_m_df, type = "raw", meta_dataframe_name = "bench_bronze")

  #Mock daily bench returns
  new_daily_bench_returns_m_df <- expand.grid(c("ibov", "smll", "idiv"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = rnorm(nrow(.)))
  new_daily_bench_returns_m_df$tickers <- as.character(new_daily_bench_returns_m_df$tickers)
  new_daily_bench_returns_m_df <- create_meta_dataframe(new_daily_bench_returns_m_df, type = "raw", meta_dataframe_name = "bench_bronze")

  #Create a feat_m_df
  new_feat_m_df <- new_daily_returns_m_df@data %>%
    dplyr::filter(dates %in% as.Date("2002-03-15")) %>%
    dplyr::mutate(book_yield = rnorm(nrow(.)), dy_med_36m = rnorm(nrow(.)))

  #For each row in features_m_df, check if corresponding ret is NA and replace it by NA if it is
  new_feat_m_df <- new_feat_m_df %>% dplyr::mutate(book_yield = ifelse(is.na(ret), NA, book_yield),
                                           dy_med_36m = ifelse(is.na(ret), NA, dy_med_36m)) %>%
    create_meta_dataframe(type = "raw", meta_dataframe_name = "bronze")

  date_first_quote <- data.frame(
    tickers = c("happy2", "ipo", "new_ipo", "delisted", "illiquid", "wrong"),
    date_first_quote = as.Date(c("1999-01-12", "2000-02-20", "2002-03-08", "1999-01-05", "1999-01-12", "2001-01-06"))
  )

  date_last_quote <- data.frame(
    tickers = c("happy2", "ipo", "new_ipo", "delisted", "illiquid", "wrong"),
    date_last_quote = as.Date(c("2002-03-15", "2002-03-02", "2002-03-15", "2001-11-07", "2002-03-10", "2002-02-04"))
  )

  #Create tickers catalog
  new_tickers_catalog_features <- create_tickers_catalog(
    raw_features_m_df = new_daily_returns_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )
  new_tickers_catalog_features <- create_tickers_catalog(
    raw_features_m_df = new_feat_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("happy2"),
    old_tickers = c("happy"),
    change_date = as.Date(c("2002-03-02"))
  )

  # Update catalog
  updated_catalog_daily <- update_tickers_catalog(
    old_tickers_catalog = tickers_catalog_features,
    new_tickers_catalog = new_tickers_catalog_features,
    ticker_changes = ticker_changes
  )

  updated_catalog_feat <- update_tickers_catalog(
    old_tickers_catalog = tickers_catalog_features,
    new_tickers_catalog = new_tickers_catalog_features,
    ticker_changes = ticker_changes
  )

  #read catalog
  second_pre_silver_daily_returns_m_df <- read_tickers_catalog(
    new_daily_returns_m_df,
    tickers_catalog = updated_catalog_daily
  )
  second_pre_silver_features_m_df <- read_tickers_catalog(
    new_feat_m_df,
    tickers_catalog = updated_catalog_feat
  )

  #Update pre-silver
  update_presilver_daily_m_df <- update_meta_dataframe(
    old_features_m_df = pre_silver_daily_returns_m_df,
    new_features_m_df = second_pre_silver_daily_returns_m_df,
    batch_type = "daily"
  )
  update_presilver_feat_m_df <- update_meta_dataframe(
    old_features_m_df = pre_silver_features_m_df,
    new_features_m_df = second_pre_silver_features_m_df
  )
  updated_bench_returns_m_df <- daily_bench_returns_m_df@data %>%
    dplyr::bind_rows(new_daily_bench_returns_m_df@data) %>%
    dplyr::arrange(id) %>%
    create_meta_dataframe()


  suppressWarnings(
    daily_bench_returns_m_xts <- create_meta_xts(updated_bench_returns_m_df)
  )

  #Create target_m_df
  suppressWarnings(
    results <- create_target_m_df(
      daily_returns_m_df = update_presilver_daily_m_df,
      features_m_df = update_presilver_feat_m_df,
      daily_bench_returns_m_xts = daily_bench_returns_m_xts,
      past_ret_column = "ret",
      selected_bench = "ibov",
      fwd_horizon = 3,
      active_returns = TRUE,
      parallel = FALSE
    )
  )


  #Check that all expected ids are contemplated
  expect_equal(
    results@data %>% dplyr::pull(id),
    update_presilver_feat_m_df@data %>% dplyr::pull(id)
  )

  #Check that all dates are contemplated
  expect_equal(
    results@data %>% dplyr::pull(dates),
    update_presilver_feat_m_df@data %>% dplyr::pull(dates)
  )

  #Check that calculation is correct for some specific cases
  ticker_1 <- updated_catalog_daily@perm_id["happy2"] %>% unname() #delisted
  ticker_2 <- updated_catalog_daily@perm_id["ipo"] %>% unname() #happy
  ticker_3 <- updated_catalog_daily@perm_id["delisted"] %>% unname() #happy
  ticker_4 <- updated_catalog_daily@perm_id["illiquid"] %>% unname() #happy
  ticker_5 <- updated_catalog_daily@perm_id["wrong"] %>% unname() #happy
  ticker_6 <- updated_catalog_daily@perm_id["new_ipo"] %>% unname() #happy


  #Get obs for next 3 months beginning in 2000-01-15
  ticker_1_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-04-15"))
  ticker_3_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-04-15"))
  ticker_4_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2000-01-15"), dates <= as.Date("2000-04-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2000-01-15/2000-04-15", "ibov"]

  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>%
    dplyr::bind_rows(ticker_3_fwd_ret) %>% dplyr::bind_rows(ticker_4_fwd_ret)
  #replace na with 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    first_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  tickers_universe_m_d_ref <- summarize_performance(first_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = TRUE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2000-01-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)

  first_results <- results@data %>% dplyr::filter(dates == as.Date("2000-01-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(first_results)

  standardize_na <- function(df) {
    df[] <- lapply(df, function(col) ifelse(is.na(col), NA_real_, col))
    return(df)
  }

  expect_equal(standardize_na(first_results), standardize_na(tickers_universe_m_d_ref))


  #Check that ipo and wrong are not present
  expect_false(unname(updated_catalog_daily@perm_id["ipo"]) %in% first_results$tickers)
  expect_false(unname(updated_catalog_daily@perm_id["wrong"]) %in% first_results$tickers)
  expect_false(unname(updated_catalog_daily@perm_id["new_ipo"]) %in% first_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["delisted"]) %in% first_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["illiquid"]) %in% first_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["happy2"]) %in% first_results$tickers)


  #Get obs for next 3 months beginning in 2001-01-15
  ticker_1_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_2_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_3_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_4_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_5_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_5, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))
  ticker_6_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_6, dates >= as.Date("2001-01-15"), dates <= as.Date("2001-04-15"))


  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2001-01-15/2001-04-15", "ibov"]


  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret) %>% dplyr::bind_rows(ticker_3_fwd_ret) %>%
    dplyr::bind_rows(ticker_4_fwd_ret) %>% dplyr::bind_rows(ticker_5_fwd_ret)

  #replace na to 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    second_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  tickers_universe_m_d_ref <- summarize_performance(second_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = TRUE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2001-01-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  second_results <- results@data %>% dplyr::filter(dates == as.Date("2001-01-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(second_results)
  expect_equal(second_results, tickers_universe_m_d_ref)

  #Check that all are present
  expect_true(unname(updated_catalog_daily@perm_id["ipo"]) %in% second_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["wrong"]) %in% second_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["delisted"]) %in% second_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["illiquid"]) %in% second_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["happy"]) %in% second_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["happy2"]) %in% second_results$tickers)
  expect_false(unname(updated_catalog_daily@perm_id["new_ipo"]) %in% second_results$tickers)


  #Get obs for next 3 months beginning in 2001-11-15
  ticker_1_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_2_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_3_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_4_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_5_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_5, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))
  ticker_6_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_6, dates >= as.Date("2001-11-15"), dates <= as.Date("2002-02-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2001-11-15/2002-02-15", "ibov"]

  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret) %>% dplyr::bind_rows(ticker_3_fwd_ret) %>%
    dplyr::bind_rows(ticker_4_fwd_ret) %>% dplyr::bind_rows(ticker_5_fwd_ret)
  #replace NA with 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    third_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  #Replace NAs in delisted with bench ret
  third_fwd_ret_xts@data[c(4:93), "h0ec000433"] <- bench_fwd[4:93]
  third_fwd_ret_xts@data[93, "h4c37ff6e6"] <- bench_fwd[93]

  tickers_universe_m_d_ref <- summarize_performance(third_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = TRUE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2001-11-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  third_results <- results@data %>% dplyr::filter(dates == as.Date("2001-11-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(third_results)
  expect_equal(standardize_na(third_results), standardize_na(tickers_universe_m_d_ref))

  #Check that all are present
  expect_true(unname(updated_catalog_daily@perm_id["ipo"]) %in% third_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["wrong"]) %in% third_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["delisted"]) %in% third_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["illiquid"]) %in% third_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["happy"]) %in% third_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["happy2"]) %in% third_results$tickers)
  expect_false(unname(updated_catalog_daily@perm_id["new_ipo"]) %in% third_results$tickers)

  #Check 2001-12-15 (now it has data)
  fourth_results <- results@data %>% dplyr::filter(dates == as.Date("2001-12-15"))

  #Get obs for next 3 months beginning in 2001-11-15
  ticker_1_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2001-12-15"), dates <= as.Date("2002-03-15"))
  ticker_2_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2001-12-15"), dates <= as.Date("2002-03-15"))
  ticker_3_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2001-12-15"), dates <= as.Date("2002-03-15"))
  ticker_4_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2001-12-15"), dates <= as.Date("2002-03-15"))
  ticker_5_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_5, dates >= as.Date("2001-12-15"), dates <= as.Date("2002-03-15"))
  ticker_6_fwd_ret <- update_presilver_daily_m_df@data %>%
    dplyr::filter(tickers == ticker_6, dates >= as.Date("2001-12-15"), dates <= as.Date("2002-03-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2001-12-15/2002-03-15", "ibov"]

  #get meta_xts
  joined_tickers_df <- ticker_1_fwd_ret %>% dplyr::bind_rows(ticker_2_fwd_ret) %>% dplyr::bind_rows(ticker_3_fwd_ret) %>%
    dplyr::bind_rows(ticker_4_fwd_ret) %>% dplyr::bind_rows(ticker_5_fwd_ret)
  #replace NA with 0
  joined_tickers_df <- joined_tickers_df %>% dplyr::mutate(
    ret = ifelse(is.na(ret), 0, ret)
  )

  suppressWarnings(
    fourth_fwd_ret_xts <- create_meta_xts(joined_tickers_df, data_format = "long")
  )

  fourth_fwd_ret_xts@data[c(63:91), "h4c37ff6e6"] <- bench_fwd[c(63:91)]
  fourth_fwd_ret_xts@data[c(89:91), "h9f6bf2400"] <- bench_fwd[c(89:91)] #fill for ipo (was delisted)


  tickers_universe_m_d_ref <- summarize_performance(fourth_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = TRUE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2001-12-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  fourth_results <- results@data %>% dplyr::filter(dates == as.Date("2001-12-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(fourth_results)
  expect_equal(standardize_na(fourth_results), standardize_na(tickers_universe_m_d_ref))

  #Check that all are present
  expect_true(unname(updated_catalog_daily@perm_id["ipo"]) %in% fourth_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["wrong"]) %in% fourth_results$tickers)
  expect_false(unname(updated_catalog_daily@perm_id["delisted"]) %in% fourth_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["illiquid"]) %in% fourth_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["happy"]) %in% fourth_results$tickers)
  expect_true(unname(updated_catalog_daily@perm_id["happy2"]) %in% fourth_results$tickers)
  expect_false(unname(updated_catalog_daily@perm_id["new_ipo"]) %in% fourth_results$tickers)


  #Check 2002-03-15" (now it has data)
  fifth_results <- results@data %>% dplyr::filter(dates == as.Date("2002-03-15"))

  #Check that is all NAs
  expect_true(all(is.na(fifth_results[,-c(1:3)])))


  ##Check that for 1m fwd return there are no NAs in 2002-02-15
  #Create target_m_df
  suppressWarnings(
    results2 <- create_target_m_df(
      daily_returns_m_df = update_presilver_daily_m_df,
      features_m_df = update_presilver_feat_m_df,
      daily_bench_returns_m_xts = daily_bench_returns_m_xts,
      past_ret_column = "ret",
      selected_bench = "ibov",
      fwd_horizon = 1,
      active_returns = TRUE,
      parallel = FALSE
    )
  )

  #Check 2002-02-15" (now it has data)
  sixth_results <- results@data %>% dplyr::filter(dates == as.Date("2002-02-15"))
  sixth_results2 <- results2@data %>% dplyr::filter(dates == as.Date("2002-02-15"))

  expect_false(all(is.na(sixth_results2[,-c(1:3)])))
  expect_true(all(is.na(sixth_results[,-c(1:3)])))



})

test_that("create_target_m_df throws an error when objects do not match expectations", {

  #Initiate a meta_dataframe with daily returns
  dates <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2002-02-15"), by = "days")

  set.seed(123)
  happy_stock <- rnorm(length(dates), 0, 1) #No NAS
  ipo_stock <- c(rep(NA, 50), rnorm(length(dates) - 50, 0, 1)) #IPO
  delisted_stock <- c(rnorm(length(dates) - 100, 0, 1), rep(NA, 100)) #Delisted
  iliquid_stock <- rnorm(length(dates), 0, 1) #Illiq
  iliquid_stock[sample(size = 80, x = length(iliquid_stock))] <- sample(size = 80, c(0, NA), replace = TRUE)
  wrong_stock <- rnorm(length(dates), 0, 1) #No NAS before initial listing and after unlisting

  #Combine dates and tickers (happy, ipo, delisted and illiquid) into a grid)
  daily_returns_m_df <- expand.grid(c("happy", "ipo", "delisted", "illiquid", "wrong"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = c(delisted_stock, happy_stock, iliquid_stock, ipo_stock, wrong_stock))
  daily_returns_m_df$tickers <- as.character(daily_returns_m_df$tickers)
  daily_returns_m_df <- create_meta_dataframe(daily_returns_m_df, type = "raw", meta_dataframe_name = "daily_bronze")

  #Mock daily bench returns
  daily_bench_returns_m_df <- expand.grid(c("ibov", "smll", "idiv"), dates) %>%
    dplyr::mutate(id = paste(Var1, Var2, sep = "-"), .before = Var1) %>%
    dplyr::rename(tickers = Var1, dates = Var2) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(ret = rnorm(nrow(.)))
  daily_bench_returns_m_df$tickers <- as.character(daily_bench_returns_m_df$tickers)
  daily_bench_returns_m_df <- create_meta_dataframe(daily_bench_returns_m_df, type = "raw", meta_dataframe_name = "bench_bronze")

  suppressWarnings(
    daily_bench_returns_m_xts <- create_meta_xts(daily_bench_returns_m_df)
  )

  #Create a feat_m_df
  features_dates <- lubridate::add_with_rollback(as.Date("1999-12-15"), months(1:26))

  feat_m_df <- daily_returns_m_df@data %>%
    dplyr::filter(dates %in% seq.Date(from = as.Date("2000-01-15"), to = as.Date("2002-02-15"), by = "months")) %>%
    dplyr::mutate(book_yield = rnorm(nrow(.)), dy_med_36m = rnorm(nrow(.)))

  #For each row in features_m_df, check if corresponding ret is NA and replace it by NA if it is
  feat_m_df <- feat_m_df %>% dplyr::mutate(book_yield = ifelse(is.na(ret), NA, book_yield),
                                           dy_med_36m = ifelse(is.na(ret), NA, dy_med_36m)) %>%
    create_meta_dataframe(type = "raw", meta_dataframe_name = "bronze")

  date_first_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_first_quote = as.Date(c("1999-01-12", "2000-02-20", "1999-01-05", "1999-01-12", "2001-01-06"))
  )

  date_last_quote <- data.frame(
    tickers = c("happy", "ipo", "delisted", "illiquid", "wrong"),
    date_last_quote = as.Date(c("2002-02-15", "2002-02-15", "2001-11-07", "2002-02-15", "2002-02-04"))
  )

  #Create tickers catalog
  tickers_catalog_features <- create_tickers_catalog(
    raw_features_m_df = feat_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #read catalog
  pre_silver_daily_returns_m_df <- read_tickers_catalog(
    daily_returns_m_df,
    tickers_catalog = tickers_catalog_features
  )
  pre_silver_features_m_df <- read_tickers_catalog(
    feat_m_df,
    tickers_catalog = tickers_catalog_features
  )

  #Wrong current_date
  wrong_feat_m_df <- pre_silver_features_m_df
  wrong_feat_m_df@current_date <- as.Date("2002-01-15")

  #Create target_m_df
  expect_error(
  suppressWarnings(
    create_target_m_df(
      daily_returns_m_df = pre_silver_daily_returns_m_df,
      features_m_df = wrong_feat_m_df,
      daily_bench_returns_m_xts = daily_bench_returns_m_xts,
      past_ret_column = "ret",
      selected_bench = "ibov",
      fwd_horizon = 3,
      active_returns = TRUE,
      parallel = FALSE
    )
  ), "features_m_df and daily_returns_m_df must have the same current_date."
  )


  #Wrong ids
  wrong_feat_m_df <- pre_silver_features_m_df
  wrong_feat_m_df@data$id[10] <- "wrong_id"

  #Create target_m_df
  expect_error(
    suppressWarnings(
      create_target_m_df(
        daily_returns_m_df = pre_silver_daily_returns_m_df,
        features_m_df = wrong_feat_m_df,
        daily_bench_returns_m_xts = daily_bench_returns_m_xts,
        past_ret_column = "ret",
        selected_bench = "ibov",
        fwd_horizon = 3,
        active_returns = TRUE,
        parallel = FALSE
      )
    ), "Some ids from features_m_df do not exist in daily_returns_m_df"
  )


  #Wrong benchmark
  wrong_daily_bench_returns_m_xts <- daily_bench_returns_m_xts
  wrong_daily_bench_returns_m_xts@data <- wrong_daily_bench_returns_m_xts@data[-5,]

  #Create target_m_df
  expect_error(
    suppressWarnings(
      create_target_m_df(
        daily_returns_m_df = pre_silver_daily_returns_m_df,
        features_m_df = pre_silver_features_m_df,
        daily_bench_returns_m_xts = wrong_daily_bench_returns_m_xts,
        past_ret_column = "ret",
        selected_bench = "ibov",
        fwd_horizon = 3,
        active_returns = TRUE,
        parallel = FALSE
      )
    ), "Dates from daily_returns_m_df and daily_bench_returns_m_xts should match."
  )

  #Wrong bench
  wrong_daily_bench_returns_m_xts <- daily_bench_returns_m_xts
  wrong_daily_bench_returns_m_xts@data <- rbind(wrong_daily_bench_returns_m_xts@data, wrong_daily_bench_returns_m_xts@data[5,])

  #Create target_m_df
  expect_error(
    suppressWarnings(
      create_target_m_df(
        daily_returns_m_df = pre_silver_daily_returns_m_df,
        features_m_df = pre_silver_features_m_df,
        daily_bench_returns_m_xts = wrong_daily_bench_returns_m_xts,
        past_ret_column = "ret",
        selected_bench = "ibov",
        fwd_horizon = 3,
        active_returns = TRUE,
        parallel = FALSE
      )
    ), "Dates from daily_returns_m_df and daily_bench_returns_m_xts should match."
  )


  #Wrong selected benchmark

  #Create target_m_df
  expect_error(
    suppressWarnings(
      create_target_m_df(
        daily_returns_m_df = pre_silver_daily_returns_m_df,
        features_m_df = pre_silver_features_m_df,
        daily_bench_returns_m_xts = daily_bench_returns_m_xts,
        past_ret_column = "ret",
        selected_bench = "IBOV",
        fwd_horizon = 3,
        active_returns = TRUE,
        parallel = FALSE
      )
    ), "selected_bench must be a column in benchmark_returns_m_xts."
  )


  #Wrong daily_bench_returns_m_xts
  wrong_daily_bench_returns_m_xts <- daily_bench_returns_m_xts
  wrong_daily_bench_returns_m_xts@data$ibov <- NA

  #Wrong past ret col

  #Create target_m_df
  expect_error(
    suppressWarnings(
      create_target_m_df(
        daily_returns_m_df = pre_silver_daily_returns_m_df,
        features_m_df = pre_silver_features_m_df,
        daily_bench_returns_m_xts = daily_bench_returns_m_xts,
        past_ret_column = "RET",
        selected_bench = "ibov",
        fwd_horizon = 3,
        active_returns = TRUE,
        parallel = FALSE
      )
    ), "past_ret_column must be a column in daily_returns_m_df."
  )


  #Wrong daily_bench_returns_m_xts
  wrong_daily_bench_returns_m_xts <- daily_bench_returns_m_xts
  wrong_daily_bench_returns_m_xts@data$ibov <- NA

  #Create target_m_df
  expect_error(
    suppressWarnings(
      create_target_m_df(
        daily_returns_m_df = pre_silver_daily_returns_m_df,
        features_m_df = pre_silver_features_m_df,
        daily_bench_returns_m_xts = wrong_daily_bench_returns_m_xts,
        past_ret_column = "ret",
        selected_bench = "ibov",
        fwd_horizon = 3,
        active_returns = TRUE,
        parallel = FALSE
      )
    ), "There are NAs in selected_daily_returns_m_d_fwd or selected_daily_bench_returns_m_xts_fwd."
  )

  #Wrong fwd_horizon

  #Create target_m_df
  expect_error(
    suppressWarnings(
      create_target_m_df(
        daily_returns_m_df = pre_silver_daily_returns_m_df,
        features_m_df = pre_silver_features_m_df,
        daily_bench_returns_m_xts = wrong_daily_bench_returns_m_xts,
        past_ret_column = "ret",
        selected_bench = "ibov",
        fwd_horizon = 0,
        active_returns = TRUE,
        parallel = FALSE
      )
    ), "fwd_horizon must be greater than 0."
  )


})
