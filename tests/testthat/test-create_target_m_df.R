test_that("create_target_m_df works for a base case", {

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
  tickers_catalog_daily <- create_tickers_catalog(
    raw_features_m_df = daily_returns_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )
  tickers_catalog_features <- create_tickers_catalog(
    raw_features_m_df = feat_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #read catalog
  pre_silver_daily_returns_m_df <- read_tickers_catalog(
    daily_returns_m_df,
    tickers_catalog = tickers_catalog_daily
  )
  pre_silver_features_m_df <- read_tickers_catalog(
    feat_m_df,
    tickers_catalog = tickers_catalog_features
  )

  #Create target_m_df
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

  #Check that there are no NAs present
  expect_equal(
    sum(is.na(results@data$target)),
    0
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
  ticker_1 <- tickers_catalog_daily@perm_id["delisted"] %>% unname() #delisted
  ticker_2 <- tickers_catalog_daily@perm_id["happy"] %>% unname() #happy
  ticker_3 <- tickers_catalog_daily@perm_id["ipo"] %>% unname() #happy
  ticker_4 <- tickers_catalog_daily@perm_id["illiquid"] %>% unname() #happy
  ticker_5 <- tickers_catalog_daily@perm_id["wrong"] %>% unname() #happy


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





  #Get obs for next 3 months beginning in 2001-09-15
  ticker_1_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_1, dates >= as.Date("2001-10-15"), dates <= as.Date("2002-01-15"))
  ticker_2_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_2, dates >= as.Date("2001-10-15"), dates <= as.Date("2002-01-15"))
  ticker_3_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_3, dates >= as.Date("2001-10-15"), dates <= as.Date("2002-01-15"))
  ticker_4_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_4, dates >= as.Date("2001-10-15"), dates <= as.Date("2002-01-15"))
  ticker_5_fwd_ret <- pre_silver_daily_returns_m_df@data %>%
    dplyr::filter(tickers == ticker_5, dates >= as.Date("2001-10-15"), dates <= as.Date("2002-01-15"))

  #Get bench returns for next 3 months
  bench_fwd <- daily_bench_returns_m_xts@data["2001-10-15/2002-01-15", "ibov"]

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

  tickers_universe_m_d_ref <- summarize_performance(third_fwd_ret_xts@data, bench_fwd,
                                                    model_structure = "no_pooled",
                                                    active_returns = TRUE)$signal_universe_m_d_ref

  #Check if results match
  tickers_universe_m_d_ref <- tickers_universe_m_d_ref %>% dplyr::mutate(
    dates = as.Date("2001-10-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  third_results <- results@data %>% dplyr::filter(dates == as.Date("2001-10-15"))
  colnames(tickers_universe_m_d_ref) <- colnames(third_results)
  expect_equal(standardize_na(third_results), standardize_na(tickers_universe_m_d_ref))

  #Check that all are present
  expect_true(unname(tickers_catalog_features@perm_id["ipo"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["wrong"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["delisted"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["illiquid"]) %in% third_results$tickers)
  expect_true(unname(tickers_catalog_features@perm_id["happy"]) %in% third_results$tickers)


  #Check that delisted stock return is only computed until delisting
  delisted_xts <- third_fwd_ret_xts@data[,1]
  delisted_xts <- delisted_xts[!is.na(delisted_xts)]
  delisted_bench <- bench_fwd[seq_len(nrow(delisted_xts)),]

  delisted_results <- summarize_performance(delisted_xts, delisted_bench,
                                            model_structure = "no_pooled",
                                            active_returns = TRUE)$signal_universe_m_d_ref

  delisted_results <- delisted_results %>% dplyr::mutate(
    dates = as.Date("2001-10-15"),
    id = paste0(tickers, "-", dates)
  ) %>% dplyr::arrange(id)


  #Check if results match
  colnames(delisted_results) <- colnames(third_results)

  all.equal(standardize_na(third_results[1,]), standardize_na(delisted_results))

})
