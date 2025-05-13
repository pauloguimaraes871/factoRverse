test_that("update_meta_dataframe works for a typical workflow (4 batches)", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA, "2000-02-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "TRPL4"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-13"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    data = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, 1, NA, 1, 3), nrow = 8, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA, 5, 4), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010615"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA, "2001-06-11", "2001-06-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  # Update catalog
  first_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Second pre silver
  second_pre_silver_features_m_df <- read_tickers_catalog(
    data = new_raw_features_m_df,
    tickers_catalog = first_updated_catalog
  )

  #Update pre-silver
  first_update_presilver_features_m_df <- update_meta_dataframe(
    old_features_m_df = first_pre_silver_features_m_df,
    new_features_m_df = second_pre_silver_features_m_df
  )

  #Check current date
  expect_equal(first_update_presilver_features_m_df@current_date, new_raw_features_m_df@current_date)

  #Check that all rows are present
  expect_equal(nrow(first_update_presilver_features_m_df@data),
               nrow(first_pre_silver_features_m_df@data) + nrow(second_pre_silver_features_m_df@data))
  #Check that all ids are contained
  expect_true(all(first_pre_silver_features_m_df@data$id %in% first_update_presilver_features_m_df@data$id))
  expect_true(all(second_pre_silver_features_m_df@data$id %in% first_update_presilver_features_m_df@data$id))
  expect_true(all(first_update_presilver_features_m_df@data$id %in%
                    c(first_pre_silver_features_m_df@data$id, second_pre_silver_features_m_df@data$id)))

  #Check that all tickers are present
  expect_true(all(first_pre_silver_features_m_df@data$tickers %in% first_update_presilver_features_m_df@data$tickers))
  expect_true(all(second_pre_silver_features_m_df@data$tickers %in% first_update_presilver_features_m_df@data$tickers))
  expect_true(all(first_update_presilver_features_m_df@data$tickers %in%
                    c(first_pre_silver_features_m_df@data$tickers, second_pre_silver_features_m_df@data$tickers)))

  #Check that all dates are present
  expect_true(all(first_pre_silver_features_m_df@data$dates %in% first_update_presilver_features_m_df@data$dates))
  expect_true(all(second_pre_silver_features_m_df@data$dates %in% first_update_presilver_features_m_df@data$dates))
  expect_true(all(first_update_presilver_features_m_df@data$dates %in%
                    c(first_pre_silver_features_m_df@data$dates, second_pre_silver_features_m_df@data$dates)))

  #Check that workflows are as expected
  expect_equal(
    first_update_presilver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`,
    first_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`
  )

  expect_equal(
    first_update_presilver_features_m_df@workflow$`update_2001-06-15`$batch_workflow$`read_tickers_catalog_2001-06-15`,
    second_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`
  )

  expect_equal(
    first_update_presilver_features_m_df@workflow$`update_2001-06-15`$new_date,
    second_pre_silver_features_m_df@current_date
  )

  # Another batch of data arrives and BRAV3 is now CALM3
  another_new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(10, -2, 3, -4, NA, NA, 9, 1), nrow = 8, ncol = 1),
      matrix(c(40, NA, 6, 7, NA, NA, 25, 4), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3"),
    dates = as.Date(c("2001-07-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010715"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "CALM3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "DMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c("2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15", "2001-05-15", NA, "2001-07-15", "2001-07-14"))
  )

  # Another one
  another_new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = another_new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create a second ticker_change
  new_ticker_changes <- data.frame(
    new_tickers = c("CALM3", "DMED3"),
    old_tickers = c("BRAV3", "CMED3"),
    change_date = as.Date(c("2001-07-09", "2001-06-16"))
  )

  # Update catalog
  second_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = first_updated_catalog,
    new_tickers_catalog = another_new_tickers_catalog,
    ticker_changes = new_ticker_changes
  )

  # Third pre silver
  third_pre_silver_features_m_df <- read_tickers_catalog(
    data = another_new_raw_features_m_df,
    tickers_catalog = second_updated_catalog
  )

  #Update pre-silver
  second_update_presilver_features_m_df <- update_meta_dataframe(
    old_features_m_df = first_update_presilver_features_m_df,
    new_features_m_df = third_pre_silver_features_m_df
  )

  #Check current date
  expect_equal(second_update_presilver_features_m_df@current_date, another_new_raw_features_m_df@current_date)
  #Check that all rows are present
  expect_equal(nrow(second_update_presilver_features_m_df@data),
               nrow(first_pre_silver_features_m_df@data) + nrow(second_pre_silver_features_m_df@data) + nrow(third_pre_silver_features_m_df@data))
  #Check that all ids are contained
  expect_true(all(first_pre_silver_features_m_df@data$id %in% second_update_presilver_features_m_df@data$id))
  expect_true(all(second_pre_silver_features_m_df@data$id %in% second_update_presilver_features_m_df@data$id))
  expect_true(all(third_pre_silver_features_m_df@data$id %in% second_update_presilver_features_m_df@data$id))

  expect_true(all(second_update_presilver_features_m_df@data$id %in%
                    c(first_pre_silver_features_m_df@data$id, second_pre_silver_features_m_df@data$id, third_pre_silver_features_m_df@data$id)))

  #Check that all tickers are present
  expect_true(all(first_pre_silver_features_m_df@data$tickers %in% second_update_presilver_features_m_df@data$tickers))
  expect_true(all(second_pre_silver_features_m_df@data$tickers %in% second_update_presilver_features_m_df@data$tickers))
  expect_true(all(third_pre_silver_features_m_df@data$tickers %in% second_update_presilver_features_m_df@data$tickers))

  expect_true(all(second_update_presilver_features_m_df@data$tickers %in%
                    c(first_pre_silver_features_m_df@data$tickers, second_pre_silver_features_m_df@data$tickers, third_pre_silver_features_m_df)))

  #Check that all dates are present
  expect_true(all(first_pre_silver_features_m_df@data$dates %in% second_update_presilver_features_m_df@data$dates))
  expect_true(all(second_pre_silver_features_m_df@data$dates %in% second_update_presilver_features_m_df@data$dates))
  expect_true(all(third_pre_silver_features_m_df@data$dates %in% second_update_presilver_features_m_df@data$dates))

  expect_true(all(second_update_presilver_features_m_df@data$dates %in%
                    c(first_pre_silver_features_m_df@data$dates, second_pre_silver_features_m_df@data$dates, third_pre_silver_features_m_df@data$dates)))

  #Check that workflows are as expected
  expect_equal(
    second_update_presilver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`,
    first_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`
  )

  expect_equal(
    second_update_presilver_features_m_df@workflow$`update_2001-06-15`$batch_workflow$`read_tickers_catalog_2001-06-15`,
    second_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`
  )
  expect_equal(
    second_update_presilver_features_m_df@workflow$`update_2001-06-15`$new_date,
    second_pre_silver_features_m_df@current_date
  )

  expect_equal(
    second_update_presilver_features_m_df@workflow$`update_2001-07-15`$batch_workflow$`read_tickers_catalog_2001-07-15`,
    third_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-07-15`
  )
  expect_equal(
    second_update_presilver_features_m_df@workflow$`update_2001-07-15`$new_date,
    third_pre_silver_features_m_df@current_date
  )

  # A new batch once again arrives
  once_again_new_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(12, -20, 30, 0, NA, NA, 9, 12, 3, 5), nrow = 10, ncol = 1),
      matrix(c(4, NA, -6, NA, NA, NA, 250, 4, 0, 1), nrow = 10, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3", "AGRO3", "KEPL3"),
    dates = as.Date(c("2001-08-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010815"
  )

  date_first_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "ABEV3", "CALM3", "ENAT3", "CAFE3", "KEPL3", "VALE3", "ISAE4", "DMED3"),
    date_first_quote = as.Date(c("2001-08-10", "1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "2001-08-10", "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "KEPL3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c("2001-08-15", "2001-08-15", "2001-08-15", "2001-08-15", "2001-08-15", "2001-05-15", NA, "2001-08-15", "2001-08-15", "2001-08-14"))
  )

  # Create another catalog
  another_new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = once_again_new_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Update catalog
  third_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = second_updated_catalog,
    new_tickers_catalog = another_new_tickers_catalog,
    ticker_changes = NULL
  )

  # Fourth pre silver
  fourth_pre_silver_features_m_df <- read_tickers_catalog(
    data = once_again_new_features_m_df,
    tickers_catalog = third_updated_catalog
  )

  #Update pre-silver
  third_update_presilver_features_m_df <- update_meta_dataframe(
    old_features_m_df = second_update_presilver_features_m_df,
    new_features_m_df = fourth_pre_silver_features_m_df
  )

  #Check current date
  expect_equal(third_update_presilver_features_m_df@current_date, once_again_new_features_m_df@current_date)

  #Check that all rows are present
  expect_equal(nrow(third_update_presilver_features_m_df@data),
               nrow(first_pre_silver_features_m_df@data) + nrow(second_pre_silver_features_m_df@data)
               + nrow(third_pre_silver_features_m_df@data) + nrow(fourth_pre_silver_features_m_df@data))

  #Check that all ids are contained
  expect_true(all(first_pre_silver_features_m_df@data$id %in% third_update_presilver_features_m_df@data$id))
  expect_true(all(second_pre_silver_features_m_df@data$id %in% third_update_presilver_features_m_df@data$id))
  expect_true(all(third_pre_silver_features_m_df@data$id %in% third_update_presilver_features_m_df@data$id))
  expect_true(all(fourth_pre_silver_features_m_df@data$id %in% third_update_presilver_features_m_df@data$id))

  expect_true(all(third_update_presilver_features_m_df@data$id %in%
                    c(first_pre_silver_features_m_df@data$id, second_pre_silver_features_m_df@data$id,
                      third_pre_silver_features_m_df@data$id, fourth_pre_silver_features_m_df@data$id)))

  #Check that all tickers are present
  expect_true(all(first_pre_silver_features_m_df@data$tickers %in% third_update_presilver_features_m_df@data$tickers))
  expect_true(all(second_pre_silver_features_m_df@data$tickers %in% third_update_presilver_features_m_df@data$tickers))
  expect_true(all(third_pre_silver_features_m_df@data$tickers %in% third_update_presilver_features_m_df@data$tickers))
  expect_true(all(fourth_pre_silver_features_m_df@data$tickers %in% third_update_presilver_features_m_df@data$tickers))

  expect_true(all(third_update_presilver_features_m_df@data$tickers %in%
                    c(first_pre_silver_features_m_df@data$tickers, second_pre_silver_features_m_df@data$tickers,
                      third_pre_silver_features_m_df, fourth_pre_silver_features_m_df@data$tickers)))

  #Check that all dates are present
  expect_true(all(first_pre_silver_features_m_df@data$dates %in% third_update_presilver_features_m_df@data$dates))
  expect_true(all(second_pre_silver_features_m_df@data$dates %in% third_update_presilver_features_m_df@data$dates))
  expect_true(all(third_pre_silver_features_m_df@data$dates %in% third_update_presilver_features_m_df@data$dates))
  expect_true(all(fourth_pre_silver_features_m_df@data$dates %in% third_update_presilver_features_m_df@data$dates))

  expect_true(all(third_update_presilver_features_m_df@data$dates %in%
                    c(first_pre_silver_features_m_df@data$dates, second_pre_silver_features_m_df@data$dates,
                      third_pre_silver_features_m_df@data$dates, fourth_pre_silver_features_m_df@data$dates)))

  #Check that workflows are as expected
  expect_equal(
    third_update_presilver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`,
    first_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`
  )

  expect_equal(
    third_update_presilver_features_m_df@workflow$`update_2001-06-15`$batch_workflow$`read_tickers_catalog_2001-06-15`,
    second_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`
  )
  expect_equal(
    third_update_presilver_features_m_df@workflow$`update_2001-06-15`$new_date,
    second_pre_silver_features_m_df@current_date
  )
  expect_equal(
    third_update_presilver_features_m_df@workflow$`update_2001-07-15`$batch_workflow$`read_tickers_catalog_2001-07-15`,
    third_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-07-15`
  )
  expect_equal(
    third_update_presilver_features_m_df@workflow$`update_2001-07-15`$new_date,
    third_pre_silver_features_m_df@current_date
  )

  expect_equal(
    third_update_presilver_features_m_df@workflow$`update_2001-08-15`$batch_workflow$`read_tickers_catalog_2001-08-15`,
    fourth_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-08-15`
  )
  expect_equal(
    third_update_presilver_features_m_df@workflow$`update_2001-08-15`$new_date,
    fourth_pre_silver_features_m_df@current_date
  )

  # One more batch arrives
  last_new_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, -2, 30, 1, -90, 5, 9, 12, 3, 5), nrow = 10, ncol = 1),
      matrix(c(40, NA, -60, 2, 3, 1, 250, 4, 0, 1), nrow = 10, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "LEIT4", "ISAE4", "DMED3", "AGRO3", "KEPL3"),
    dates = as.Date(c("2001-09-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010915"
  )

  date_first_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "ABEV3", "CALM3", "ENAT3", "LEIT4", "KEPL3", "VALE3", "ISAE4", "DMED3"),
    date_first_quote = as.Date(c("2001-08-10", "1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "2001-08-10", "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "LEIT4", "KEPL3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c("2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-05-15", NA, "2001-09-15", "2001-09-15", "2001-09-14"))
  )

  # Create another catalog
  once_again_new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = last_new_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create tickers change
  ticker_changes <- data.frame(
    new_tickers = c("LEIT4"),
    old_tickers = c("CAFE3"),
    change_date = as.Date("2001-08-29")
  )

  # Update catalog
  suppressWarnings(
    last_updated_catalog <- update_tickers_catalog(
      old_tickers_catalog = third_updated_catalog,
      new_tickers_catalog = once_again_new_tickers_catalog,
      ticker_changes = ticker_changes
    )
  )

  # Fifth pre silver
  fifth_pre_silver_features_m_df <- read_tickers_catalog(
    data = last_new_features_m_df,
    tickers_catalog = last_updated_catalog
  )

  #Update pre-silver
  fourth_update_presilver_features_m_df <- update_meta_dataframe(
    old_features_m_df = third_update_presilver_features_m_df,
    new_features_m_df = fifth_pre_silver_features_m_df
  )


  #Check current date
  expect_equal(fourth_update_presilver_features_m_df@current_date, last_new_features_m_df@current_date)

  #Check that all rows are present
  expect_equal(nrow(fourth_update_presilver_features_m_df@data),
               nrow(first_pre_silver_features_m_df@data) + nrow(second_pre_silver_features_m_df@data)
               + nrow(third_pre_silver_features_m_df@data) + nrow(fourth_pre_silver_features_m_df@data)
               + nrow(fifth_pre_silver_features_m_df@data))

  #Check that all ids are contained
  expect_true(all(first_pre_silver_features_m_df@data$id %in% fourth_update_presilver_features_m_df@data$id))
  expect_true(all(second_pre_silver_features_m_df@data$id %in% fourth_update_presilver_features_m_df@data$id))
  expect_true(all(third_pre_silver_features_m_df@data$id %in% fourth_update_presilver_features_m_df@data$id))
  expect_true(all(fourth_pre_silver_features_m_df@data$id %in% fourth_update_presilver_features_m_df@data$id))
  expect_true(all(fifth_pre_silver_features_m_df@data$id %in% fourth_update_presilver_features_m_df@data$id))


  expect_true(all(fourth_update_presilver_features_m_df@data$id %in%
                    c(first_pre_silver_features_m_df@data$id, second_pre_silver_features_m_df@data$id,
                      third_pre_silver_features_m_df@data$id, fourth_pre_silver_features_m_df@data$id,
                      fifth_pre_silver_features_m_df@data$id)))


  #Check that all tickers are present
  expect_true(all(first_pre_silver_features_m_df@data$tickers %in% fourth_update_presilver_features_m_df@data$tickers))
  expect_true(all(second_pre_silver_features_m_df@data$tickers %in% fourth_update_presilver_features_m_df@data$tickers))
  expect_true(all(third_pre_silver_features_m_df@data$tickers %in% fourth_update_presilver_features_m_df@data$tickers))
  expect_true(all(fourth_pre_silver_features_m_df@data$tickers %in% fourth_update_presilver_features_m_df@data$tickers))
  expect_true(all(fifth_pre_silver_features_m_df@data$tickers %in% fourth_update_presilver_features_m_df@data$tickers))

  expect_true(all(fourth_update_presilver_features_m_df@data$tickers %in%
                    c(first_pre_silver_features_m_df@data$tickers, second_pre_silver_features_m_df@data$tickers,
                      third_pre_silver_features_m_df, fourth_pre_silver_features_m_df@data$tickers,
                      fifth_pre_silver_features_m_df@data$tickers)))


  #Check that all dates are present
  expect_true(all(first_pre_silver_features_m_df@data$dates %in% fourth_update_presilver_features_m_df@data$dates))
  expect_true(all(second_pre_silver_features_m_df@data$dates %in% fourth_update_presilver_features_m_df@data$dates))
  expect_true(all(third_pre_silver_features_m_df@data$dates %in% fourth_update_presilver_features_m_df@data$dates))
  expect_true(all(fourth_pre_silver_features_m_df@data$dates %in% fourth_update_presilver_features_m_df@data$dates))
  expect_true(all(fifth_pre_silver_features_m_df@data$dates %in% fourth_update_presilver_features_m_df@data$dates))

  expect_true(all(fourth_update_presilver_features_m_df@data$dates %in%
                    c(first_pre_silver_features_m_df@data$dates, second_pre_silver_features_m_df@data$dates,
                      third_pre_silver_features_m_df@data$dates, fourth_pre_silver_features_m_df@data$dates,
                      fifth_pre_silver_features_m_df@data$dates)))

  #Check that workflows are as expected
  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`,
    first_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`
  )

  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`update_2001-06-15`$batch_workflow$`read_tickers_catalog_2001-06-15`,
    second_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`
  )
  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`update_2001-06-15`$new_date,
    second_pre_silver_features_m_df@current_date
  )
  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`update_2001-07-15`$batch_workflow$`read_tickers_catalog_2001-07-15`,
    third_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-07-15`
  )
  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`update_2001-07-15`$new_date,
    third_pre_silver_features_m_df@current_date
  )

  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`update_2001-08-15`$batch_workflow$`read_tickers_catalog_2001-08-15`,
    fourth_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-08-15`
  )
  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`update_2001-08-15`$new_date,
    fourth_pre_silver_features_m_df@current_date
  )

  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`update_2001-09-15`$batch_workflow$`read_tickers_catalog_2001-09-15`,
    fifth_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-09-15`
  )
  expect_equal(
    fourth_update_presilver_features_m_df@workflow$`update_2001-09-15`$new_date,
    fifth_pre_silver_features_m_df@current_date
  )


})

test_that("update_meta_dataframe throws an error when columns do not match", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA, "2000-02-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "TRPL4"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-13"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    data = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, 1, NA, 1, 3), nrow = 8, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA, 5, 4), nrow = 8, ncol = 1),
      matrix(c(1, 2, 3, 4, 1, NA, 1, 3), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma"),
    meta_dataframe_name = "bronze_20010615"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA, "2001-06-11", "2001-06-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  # Update catalog
  first_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Second pre silver
  second_pre_silver_features_m_df <- read_tickers_catalog(
    data = new_raw_features_m_df,
    tickers_catalog = first_updated_catalog
  )

  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = second_pre_silver_features_m_df
    ), "Column names between old_features_m_df and new_features_m_df do not match."
  )


})

test_that("update_meta_dataframe throws an error when there are common ids or tickers", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA, "2000-02-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "TRPL4"),
    date_last_quote = as.Date(c(NA, "2001-05-14", "2001-05-14", "2001-05-14", "2001-05-14", "2001-05-14", "2001-05-11"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    data = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, NA, NA, 1, 3), nrow = 8, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA, 5, 4), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010615"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA, "2001-06-11", "2001-06-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  # Update catalog
  first_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Second pre silver
  second_pre_silver_features_m_df <- read_tickers_catalog(
    data = new_raw_features_m_df,
    tickers_catalog = first_updated_catalog
  )

  #Force date
  second_pre_silver_features_m_df@data$dates[1] <- c("2001-05-15")

  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = second_pre_silver_features_m_df
    ), "There are common dates between old_features_m_df and new_features_m_df."
  )

  #Force id
  second_pre_silver_features_m_df@data$dates[1] <- c("2001-06-15")
  second_pre_silver_features_m_df@data$id[1] <- "h0165b9220-2001-05-15"


  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = second_pre_silver_features_m_df
    ), "There are common ids between old_features_m_df and new_features_m_df."
  )

})

test_that("update_meta_dataframe throws an error when there are two dates in new batch", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA, "2000-02-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "TRPL4"),
    date_last_quote = as.Date(c(NA, "2001-05-14", "2001-05-14", "2001-05-14", "2001-05-14", "2001-05-14", "2001-05-11"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    data = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, NA, NA, 1, 3, 1, 2, 3, 4, NA, NA, 1, 3), nrow = 8, ncol = 2),
      matrix(c(4, NA, 6, 7, NA, NA, 5, 4, 1, 2, 3, 4, NA, NA, 1, 3), nrow = 8, ncol = 2)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15", "2001-07-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010615"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_last_quote = as.Date(c("2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15", "2001-05-15", NA, "2001-07-11", "2001-07-15"))
  )


  #Force pre silver
  class(new_raw_features_m_df) <- "meta_dataframe"


  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = new_raw_features_m_df
    ), "Number of unique dates in new_features_m_df is not equal to 1."
  )


})

test_that("update_meta_dataframe throws an error when new current_date is > 1", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA, "2000-02-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "TRPL4"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-13"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    data = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, 1, NA, 1, 3), nrow = 8, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA, 5, 4), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010615"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA, "2001-06-11", "2001-06-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  # Update catalog
  first_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Second pre silver
  second_pre_silver_features_m_df <- read_tickers_catalog(
    data = new_raw_features_m_df,
    tickers_catalog = first_updated_catalog
  )

  second_pre_silver_features_m_df@current_date <- as.Date("2001-07-15")

  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = second_pre_silver_features_m_df
    ), "Current date in new_features_m_df should be 1 months ahead of current_date in old_features_m_df."
  )


})

test_that("update_meta_dataframe throws an error when new name is not right", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA, "2000-02-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "TRPL4"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-13"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    data = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, 1, NA, 1, 3), nrow = 8, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA, 5, 4), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010615"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA, "2001-06-11", "2001-06-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  # Update catalog
  first_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Second pre silver
  second_pre_silver_features_m_df <- read_tickers_catalog(
    data = new_raw_features_m_df,
    tickers_catalog = first_updated_catalog
  )

  second_pre_silver_features_m_df@meta_dataframe_name <- "golden"

  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = second_pre_silver_features_m_df
    ), "old_features_m_df name is not contained in new_features_m_df name."
  )


})

test_that("update_meta_dataframe throws an error when col classes are not right", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA, "2000-02-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "TRPL4"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-13"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    data = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, 1, NA, 1, 3), nrow = 8, ncol = 1),
      matrix(c("4", "NA", "6", "7", "NA", "NA", "5", "4"), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010615"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA, "2001-06-11", "2001-06-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  # Update catalog
  first_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Second pre silver
  second_pre_silver_features_m_df <- read_tickers_catalog(
    data = new_raw_features_m_df,
    tickers_catalog = first_updated_catalog
  )


  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = second_pre_silver_features_m_df
    ), "Column classes between old_features_m_df and new_features_m_df do not match."
  )


})

test_that("update_meta_dataframe throws an error when trying to mix raw_features_m_df", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, 1, NA, 1, 3), nrow = 8, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA, 5, 4), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010615"
  )

  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = old_raw_features_m_df,
      new_features_m_df = new_raw_features_m_df
    ), "old_features_m_df and new_features_m_df should not be of class raw_features_m_df."
  )

})

test_that("update_meta_dataframe throws an error when columns do not match", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(0, 1, 2, NA, 3, -1, NA, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA, 2, 3, 1), nrow = 7, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA, 2, 5, 0), nrow = 7, ncol = 3)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze"
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3", "TRPL4"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA, "2000-02-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "TRPL4"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-13"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    data = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, 2, 3, 4, 1, NA, 1, 3), nrow = 8, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA, 5, 4), nrow = 8, ncol = 1)
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    dates = as.Date(c("2001-06-15")),
    features_names = c("Alpha", "Beta"),
    meta_dataframe_name = "bronze_20010615"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA, "2001-06-11", "2001-06-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  # Update catalog
  first_updated_catalog <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Second pre silver
  second_pre_silver_features_m_df <- read_tickers_catalog(
    data = new_raw_features_m_df,
    tickers_catalog = first_updated_catalog
  )

  second_pre_silver_features_m_df@workflow <- NULL

  #Update pre-silver
  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = second_pre_silver_features_m_df
    ), "new_features_m_df should contain a read_tickers_catalog workflow."
  )

  first_pre_silver_features_m_df@workflow <- NULL


  expect_error(
    update_meta_dataframe(
      old_features_m_df = first_pre_silver_features_m_df,
      new_features_m_df = second_pre_silver_features_m_df
    ), "old_features_m_df should contain a read_tickers_catalog workflow."
  )

})
