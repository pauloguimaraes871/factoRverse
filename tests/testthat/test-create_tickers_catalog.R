test_that("create_tickers_log works for a clean example dataset (no delisted, no untraded cias)", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, 3, 7, 9, 10, 4, 9), nrow = 3, ncol = 3),
      matrix(c(4, 5, 6, 7, 2, -3, 5, 4, -2), nrow = 3, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2), nrow = 3, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0), nrow = 3, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15"))
  )

  ## call function
  results <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  ## expected results
  expect_equal(
    c(
      ABEV3 = substr(digest::digest(paste0(date_first_quote$tickers[3], "_", format(date_first_quote$date_first_quote[3], "%Y%m%d")), algo = "md5"), 1, 10),
      PETR4 = substr(digest::digest(paste0(date_first_quote$tickers[1], "_", format(date_first_quote$date_first_quote[1], "%Y%m%d")), algo = "md5"), 1, 10),
      VALE3 = substr(digest::digest(paste0(date_first_quote$tickers[2], "_", format(date_first_quote$date_first_quote[2], "%Y%m%d")), algo = "md5"), 1, 10)
    ),
    results@perm_id
  )

  ## Check that the function is returning the correct number of tickers
  expect_equal(
    3,
    length(results@tickers)
  )

  ## Check that there are no untraded cias
  expect_equal(
    0,
    length(results@untraded)
  )

  ## Check that there are no delisted cias
  expect_equal(
    0,
    length(results@delisted)
  )

  ## Check that all are listed
  expect_equal(
    c("ABEV3", "PETR4", "VALE3"),
    results@listed
  )
})

test_that("create_tickers_log works for delisted and untraded cias", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  ## call function
  results <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  ## expected results
  expect_equal(
    c(
      ABEV3 = substr(digest::digest(paste0(date_first_quote$tickers[3], "_", format(date_first_quote$date_first_quote[3], "%Y%m%d")), algo = "md5"), 1, 10),
      CAFE3 = substr(digest::digest(paste0(date_first_quote$tickers[4], "_", NA), algo = "md5"), 1, 10),
      ENAT3 = substr(digest::digest(paste0(date_first_quote$tickers[5], "_", format(date_first_quote$date_first_quote[5], "%Y%m%d")), algo = "md5"), 1, 10),
      PETR4 = substr(digest::digest(paste0(date_first_quote$tickers[1], "_", format(date_first_quote$date_first_quote[1], "%Y%m%d")), algo = "md5"), 1, 10),
      VALE3 = substr(digest::digest(paste0(date_first_quote$tickers[2], "_", format(date_first_quote$date_first_quote[2], "%Y%m%d")), algo = "md5"), 1, 10)
    ),
    results@perm_id
  )

  ## Check that the function is returning the correct number of tickers
  expect_equal(
    5,
    length(results@tickers)
  )

  ## Check that CAFE3 is untraded
  expect_equal(
    "CAFE3",
    results@untraded
  )

  ## Check that ENAT3 is delisted cias
  expect_equal(
    "ENAT3",
    results@delisted
  )

  ## Check that others are listed
  expect_equal(
    c("ABEV3", "PETR4", "VALE3"),
    results@listed
  )
})

test_that("create_tickers_log gives same perm id for a ticker-initial_date combination", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  ## call function
  results <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  # Second call
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  raw_features_m_df_scrambled <- raw_features_m_df
  raw_features_m_df_scrambled@data <- raw_features_m_df_scrambled@data[sample(1:nrow(raw_features_m_df_scrambled@data)), ]


  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )


  results2 <- create_tickers_catalog(raw_features_m_df = raw_features_m_df_scrambled, date_first_quote = date_first_quote, date_last_quote = date_last_quote)



  ## check that perm ids are the same
  expect_equal(results@perm_id, results2@perm_id[order(names(results2@perm_id))])
})

test_that("create_tickers_log throws an error for incorrect data type", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  ## call function
  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df@data, date_first_quote = date_first_quote, date_last_quote = date_last_quote)
  )
})

test_that("create_tickers_log throws an error for incorrect data type", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  ## wrong features
  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df@data, date_first_quote = date_first_quote, date_last_quote = date_last_quote)
  )
  ## wrong date_last_quote
  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_final_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )
  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "date_first_quote must have columns 'tickers' and 'date_first_quote', and date_last_quote must have 'tickers' and 'date_last_quote'."
  )

  ## wrong date_first_quote
  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_initial_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", NA, "1999-05-15"))
  )
  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "date_first_quote must have columns 'tickers' and 'date_first_quote', and date_last_quote must have 'tickers' and 'date_last_quote'."
  )
})

test_that("create_tickers_log throws an error for ticker mismatch", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  ## wrong tickers IN DATE_FIRST_QUOTE
  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "Mismatch in tickers between raw_features_m_df, date_first_quote, and date_last_quote."
  )

  ## wrong tickers IN DATE_LAST_QUOTE
  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-03-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "Mismatch in tickers between raw_features_m_df, date_first_quote, and date_last_quote."
  )


  # different tickers in date_first_quote and date_last_quote
  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "CAFE3", "ENAT3", "GRND3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-05-15", NA, "1999-05-15", "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "CAFE3", "ENAT3", "GRND3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "Mismatch in tickers between raw_features_m_df, date_first_quote, and date_last_quote."
  )

  # different length
  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-03-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "Mismatch in tickers between raw_features_m_df, date_first_quote, and date_last_quote."
  )
})

test_that("create_tickers_log throws an error for duplicate tickers", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "PETR4", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-03-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "1995-03-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  ## wrong tickers
  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "Duplicate tickers found in date_first_quote or date_last_quote."
  )
})

test_that("create_tickers_log throws an error when only one date is NA", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-03-15", "1995-05-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-04-15"))
  )

  ## wrong tickers
  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "date_first_quote and date_last_quote must both be NA or neither."
  )
})

test_that("create_tickers_log throws an error when date_last_quote before date_first_quote", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-03-15", "2001-06-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", NA, "2001-04-15"))
  )

  expect_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "date_last_quote must be greater than or equal to date_first_quote for all tickers."
  )

  # no problem for =
  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-03-15", "2001-05-15", NA, "1999-05-15"))
  )

  expect_no_error(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)
  )
})

test_that("create_tickers_log throws an error when most common date is not the last_date", {
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 9, 10, 4, NA, 0, 3, 9, 1, NA, NA), nrow = 5, ncol = 3),
      matrix(c(4, 5, 6, NA, 2, -3, 5, NA, NA, NA, NA, 6, NA, NA, NA), nrow = 5, ncol = 3),
      matrix(c(8, 9, 10, 11, -2, -3, 4, 4, 2, 3, 4, NA, 9, NA, 4), nrow = 5, ncol = 3),
      matrix(c(3, 7, 9, 8, -1, 0, 5, -2, 0, 5, 6, 7, 2, NA, 4), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta", "Gamma", "Delta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-03-15", "2001-06-15", NA, "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CAFE3", "ENAT3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", NA, "2001-06-15"))
  )

  expect_warning(
    create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote),
    "Most common date in date_last_quote is not the last date in raw_features_m_df."
  )
})
