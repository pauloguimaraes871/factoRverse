test_that("read_tickers_catalog works for a first meta_dataframe with only untraded", {

  # Initial raw_features_m_df
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, NA, 9, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA), nrow = 6, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA), nrow = 6, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-05", "2001-05-15"))
  )

  tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  results <- read_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    tickers_catalog = tickers_catalog
  )

  #Check that there are no untraded
  expect_false(tickers_catalog@perm_id["CAFE3"] %in% results@data$tickers)

  #Check that there are ENAT3 rows in 05
  expect_equal(
    results@data %>% dplyr::filter(tickers == tickers_catalog@perm_id["ENAT3"]) %>% dplyr::pull(dates),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15"))
  )

  #Check that removals were not cause by data inconsistency
  expect_equal(
    unique(results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$untrd_not_only_NA), 0
  )
  expect_equal(
    unique(results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$untrd_only_NA), 3
  )

  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_not_only_NA,
    0
  )

  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_only_NA,
    0
  )

  #Check that removed_rows + kept_rows = nrow(raw_features_m_df)
  expect_equal(nrow(results@data) + 3, nrow(raw_features_m_df@data))

  #Check that, except for CAFE3, all tickers have 3 rows each
  expect_equal(
    results@data %>% dplyr::filter(!tickers %in% tickers_catalog@perm_id[c("CAFE3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    rep(3, 5)
  )

  #Check that ENAT3 has 3 and CAFE3 0
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("ENAT3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    3
  )
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("CAFE3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    integer(0)
  )



})

test_that("read_tickers_catalog works for a first meta_dataframe with only delisted", {

  # Initial raw_features_m_df
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, NA, 9, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA), nrow = 6, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA), nrow = 6, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", "1999-06-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-05", "2001-04-15"))
  )

  tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  results <- read_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    tickers_catalog = tickers_catalog
  )

  #Check that CAFE3 is contained
  expect_true(tickers_catalog@perm_id["CAFE3"] %in% results@data$tickers)

  #Check that there are no ENAT3 rows in 04 and 05
  expect_equal(
    results@data %>% dplyr::filter(tickers == tickers_catalog@perm_id["ENAT3"]) %>% dplyr::pull(dates),
    as.Date(c("2001-03-15", "2001-04-15"))
  )

  #Check that removals were not cause by data inconsistency
  expect_equal(
    unique(results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$untrd_not_only_NA), 0
  )
  expect_equal(
    unique(results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$untrd_only_NA), 0
  )

  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_only_NA,
    1
  )

  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_not_only_NA,
    0
  )

  #Check that removed_rows + kept_rows = nrow(raw_features_m_df)
  expect_equal(nrow(results@data) + 1, nrow(raw_features_m_df@data))

  #Check that, except for ENAT3, all tickers have 3 rows each
  expect_equal(
    results@data %>% dplyr::filter(!tickers %in% tickers_catalog@perm_id[c("ENAT3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    rep(3, 5)
  )

  #Check that ENAT3 has 2 and CAFE3 3
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("ENAT3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    2
  )
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("CAFE3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
   3
  )



})

test_that("read_tickers_catalog works for a first meta_dataframe with no delisted and no untraded", {

  # Initial raw_features_m_df
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, NA, 9, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA), nrow = 6, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA), nrow = 6, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", "1999-06-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-05", "2001-05-15"))
  )

  tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  results <- read_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    tickers_catalog = tickers_catalog
  )

  #Check that CAFE3 is contained
  expect_true(tickers_catalog@perm_id["CAFE3"] %in% results@data$tickers)

  #Check that there are ENAT3 rows in 04 and 05
  expect_equal(
    results@data %>% dplyr::filter(tickers == tickers_catalog@perm_id["ENAT3"]) %>% dplyr::pull(dates),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15"))
  )

  #Check that there are no removals
  expect_equal(
    nrow(results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary), 0
  )

  #Check that removed_rows + kept_rows = nrow(raw_features_m_df)
  expect_equal(nrow(results@data) + 0, nrow(raw_features_m_df@data))

  #Check that all tickers have 3 rows each
  expect_equal(
    results@data %>% dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    rep(3, 6)
  )

  #Check that ENAT3 has 3 and CAFE3 3
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("ENAT3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    3
  )
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("CAFE3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    3
  )



})

test_that("read_tickers_catalog works for a first meta_datafame with untraded and delisted with no data inconsistency", {

  # Initial raw_features_m_df
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, NA, 9, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA), nrow = 6, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA), nrow = 6, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-05", "2001-04-15"))
  )

  tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  results <- read_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    tickers_catalog = tickers_catalog
  )

  #Check that there are no untraded
  expect_false(tickers_catalog@perm_id["CAFE3"] %in% results@data$tickers)

  #Check that there are no ENAT3 rows in 05
  expect_equal(
    results@data %>% dplyr::filter(tickers == tickers_catalog@perm_id["ENAT3"]) %>% dplyr::pull(dates),
    as.Date(c("2001-03-15", "2001-04-15"))
  )

  #Check that removals were not cause by data inconsistency
  expect_equal(
    unique(results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$untrd_not_only_NA), 0
  )
  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$untrd_only_NA, c(3,0)
  )

  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_only_NA,
    c(0,1)
  )

  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_not_only_NA,
    c(0,0)
  )

  #Check that removed_rows + kept_rows = nrow(raw_features_m_df)
  expect_equal(nrow(results@data) + 4, nrow(raw_features_m_df@data))

  #Check that, except for CAFE3 and ENAT3, all tickers have 3 rows each
  expect_equal(
    results@data %>% dplyr::filter(!tickers %in% tickers_catalog@perm_id[c("CAFE3", "ENAT3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    rep(3, 4)
  )

  #Check that ENAT3 has 2 and CAFE3 0
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("ENAT3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    2
  )
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("CAFE3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    integer(0)
  )
})

test_that("read_tickers_catalog works for a first meta_datafame with untraded and delisted with data inconsistency", {

  # Initial raw_features_m_df
  raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, 4, 9, 4, -1, 0, 3, NA, 1, NA, -4, 3, 2, NA), nrow = 6, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, 0, 19, 5, 1, 0, NA, NA, 1, -1, NA, NA, NA, NA), nrow = 6, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3", "CAFE3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15", NA))
  )

  date_last_quote <- data.frame(
    tickers = c("CAFE3", "PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-05", "2001-03-15"))
  )

  tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  results <- read_tickers_catalog(
    raw_features_m_df = raw_features_m_df,
    tickers_catalog = tickers_catalog
  )

  #Check that there are no untraded
  expect_false(tickers_catalog@perm_id["CAFE3"] %in% results@data$tickers)

  #Check that there are no ENAT3 rows in 05
  expect_equal(
    results@data %>% dplyr::filter(tickers == tickers_catalog@perm_id["ENAT3"]) %>% dplyr::pull(dates),
    as.Date(c("2001-03-15"))
  )

  #Check that removals were caused by data inconsistency
  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$untrd_only_NA, c(2,0)
  )
  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$untrd_not_only_NA, c(1,0)
  )

  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_only_NA,
    c(0,0)
  )

  expect_equal(
    results@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_not_only_NA,
    c(0,2)
  )

  #Check that removed_rows + kept_rows = nrow(raw_features_m_df)
  expect_equal(nrow(results@data) + 5, nrow(raw_features_m_df@data))

  #Check that, except for CAFE3 and ENAT3, all tickers have 3 rows each
  expect_equal(
    results@data %>% dplyr::filter(!tickers %in% tickers_catalog@perm_id[c("CAFE3", "ENAT3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    rep(3, 4)
  )

  #Check that ENAT3 has 1 and CAFE3 0
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("ENAT3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    1
  )
  expect_equal(
    results@data %>% dplyr::filter(tickers %in% tickers_catalog@perm_id[c("CAFE3")]) %>%
      dplyr::group_by(tickers) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n),
    integer(0)
  )
})

test_that("read_tickers_catalog works for a batch meta_dataframe with two ticker changes + IPO", {

  # Create a catalog
  first_raw_features_m_df <- create_meta_dataframe(
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

  first_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = first_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #First pre silver
  first_pre_silver_features_m_df <- read_tickers_catalog(
    raw_features_m_df = first_raw_features_m_df,
    tickers_catalog = first_tickers_catalog
  )

  #Check that there are no delisting removals, only untraded
  expect_equal(first_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_only_NA, 0)
  expect_equal(first_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_not_only_NA, 0)
  expect_equal(first_pre_silver_features_m_df@data %>% dplyr::group_by(tickers) %>% dplyr::summarize(count = dplyr::n()) %>% dplyr::pull(count) %>% unique(),3)
  expect_false(first_tickers_catalog@perm_id["CAFE3"] %in% first_pre_silver_features_m_df@data$tickers)
  expect_true(first_tickers_catalog@perm_id["TRPL4"] %in% first_pre_silver_features_m_df@data$tickers)


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
  new_catalog <- update_tickers_catalog(
    old_tickers_catalog = first_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  # Get pre silver for new batch
  new_pre_silver_features_m_df <- read_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    tickers_catalog = new_catalog
  )

  #Check that ENAT3 was delisted and CAFE3 is untraded (not present), CMED3, ISAE4, BRAV3 were added, TRPL4 and RRRP3 are ALSO present (because they changed ticker)
  expect_true(new_catalog@perm_id["CMED3"] %in% new_pre_silver_features_m_df@data$tickers)
  expect_true(new_catalog@perm_id["ISAE4"] %in% new_pre_silver_features_m_df@data$tickers)
  expect_true(new_catalog@perm_id["BRAV3"] %in% new_pre_silver_features_m_df@data$tickers)
  expect_true(new_catalog@perm_id["RRRP3"] %in% new_pre_silver_features_m_df@data$tickers)
  expect_true(new_catalog@perm_id["TRPL4"] %in% new_pre_silver_features_m_df@data$tickers)
  expect_false(new_catalog@perm_id["ENAT3"] %in% new_pre_silver_features_m_df@data$tickers)
  expect_false(new_catalog@perm_id["CAFE3"] %in% new_pre_silver_features_m_df@data$tickers)

  expect_equal(
    new_pre_silver_features_m_df@data %>% dplyr::group_by(tickers) %>% dplyr::summarize(count = dplyr::n()) %>% dplyr::pull(count) %>% unique(),
    1
  )
  expect_equal(new_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$untrd_only_NA, c(1,0))
  expect_equal(new_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$untrd_not_only_NA, c(0,0))
  expect_equal(new_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$out_trd_rg_only_NA, c(0,1))
  expect_equal(new_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$out_trd_rg_not_only_NA, c(0,0))

})

test_that("read_tickers_catalog works for a ticker changing ticker and being simultaneously delisted", {
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
    raw_features_m_df = old_raw_features_m_df,
    tickers_catalog = old_tickers_catalog
  )

  #Check that there are no delisting removals, only untraded
  expect_equal(first_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_only_NA, 0)
  expect_equal(first_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-05-15`$row_removal_summary$out_trd_rg_not_only_NA, 0)
  expect_equal(first_pre_silver_features_m_df@data %>% dplyr::group_by(tickers) %>% dplyr::summarize(count = dplyr::n()) %>% dplyr::pull(count) %>% unique(),3)
  expect_false(old_tickers_catalog@perm_id["CAFE3"] %in% first_pre_silver_features_m_df@data$tickers)
  expect_true(old_tickers_catalog@perm_id["TRPL4"] %in% first_pre_silver_features_m_df@data$tickers)
  expect_true(old_tickers_catalog@perm_id["ENAT3"] %in% first_pre_silver_features_m_df@data$tickers)

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
    raw_features_m_df = new_raw_features_m_df,
    tickers_catalog = first_updated_catalog
  )

  #Check that there are no delisting removals, only untraded
  expect_equal(second_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$out_trd_rg_only_NA, c(0, 0))
  expect_equal(second_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$out_trd_rg_not_only_NA, c(0,1))
  expect_equal(second_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$untrd_only_NA, c(1,0))
  expect_equal(second_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$untrd_not_only_NA, c(0,0))
  expect_equal(second_pre_silver_features_m_df@data %>% dplyr::filter(!tickers %in% first_updated_catalog@perm_id["ENAT3"]) %>%
                 dplyr::group_by(tickers) %>% dplyr::summarize(count = dplyr::n()) %>% dplyr::pull(count) %>% unique(),1)
  expect_equal(second_pre_silver_features_m_df@data %>% dplyr::filter(tickers %in% first_updated_catalog@perm_id["ENAT3"]) %>% nrow(), 0)

  expect_false(first_updated_catalog@perm_id["CAFE3"] %in% second_pre_silver_features_m_df@data$tickers)
  expect_false(first_updated_catalog@perm_id["ENAT3"] %in% second_pre_silver_features_m_df@data$tickers)
  expect_true(first_updated_catalog@perm_id["TRPL4"] %in% second_pre_silver_features_m_df@data$tickers)
  expect_true(first_updated_catalog@perm_id["ISAE4"] %in% second_pre_silver_features_m_df@data$tickers)
  expect_true(first_updated_catalog@perm_id["CMED3"] %in% second_pre_silver_features_m_df@data$tickers)
  expect_true(first_updated_catalog@perm_id["BRAV3"] %in% second_pre_silver_features_m_df@data$tickers)


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
  new_tickers_catalog <- create_tickers_catalog(
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
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = new_ticker_changes
  )

  #Third pre silver
  third_pre_silver_features_m_df <- read_tickers_catalog(
    raw_features_m_df = another_new_raw_features_m_df,
    tickers_catalog = second_updated_catalog
  )

  #Check that there are no delisting removals, only untraded
  expect_equal(third_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-07-15`$row_removal_summary$out_trd_rg_only_NA, c(0, 1))
  expect_equal(third_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-07-15`$row_removal_summary$out_trd_rg_not_only_NA, c(0,0))
  expect_equal(third_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-07-15`$row_removal_summary$untrd_only_NA, c(1,0))
  expect_equal(third_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-07-15`$row_removal_summary$untrd_not_only_NA, c(0,0))
  expect_equal(third_pre_silver_features_m_df@data %>% dplyr::filter(!tickers %in% second_updated_catalog@perm_id["ENAT3"]) %>%
                 dplyr::group_by(tickers) %>% dplyr::summarize(count = dplyr::n()) %>% dplyr::pull(count) %>% unique(),1)
  expect_equal(third_pre_silver_features_m_df@data %>% dplyr::filter(tickers %in% second_updated_catalog@perm_id["ENAT3"]) %>% nrow(), 0)

  expect_false(second_updated_catalog@perm_id["CAFE3"] %in% third_pre_silver_features_m_df@data$tickers)
  expect_false(second_updated_catalog@perm_id["ENAT3"] %in% third_pre_silver_features_m_df@data$tickers)
  expect_true(second_updated_catalog@perm_id["TRPL4"] %in% third_pre_silver_features_m_df@data$tickers)
  expect_true(second_updated_catalog@perm_id["ISAE4"] %in% third_pre_silver_features_m_df@data$tickers)
  expect_true(second_updated_catalog@perm_id["CMED3"] %in% third_pre_silver_features_m_df@data$tickers)
  expect_true(second_updated_catalog@perm_id["BRAV3"] %in% third_pre_silver_features_m_df@data$tickers)

  # A new bacht once again arrives
  once_again_new_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(12, -20, 30, 0, NA, NA, 9, 12, 3, 5), nrow = 10, ncol = 1),
      matrix(c(4, NA, -6, NA, NA, NA, 250, 4, 0, 1), nrow = 10, ncol = 1),
      matrix(c(40, NA, 6, 9, NA, NA, 2, NA, 2, 3), nrow = 10, ncol = 1) # A new column should not change anything
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3", "AGRO3", "KEPL3"),
    dates = as.Date(c("2001-08-15")),
    features_names = c("Alpha", "Beta", "Gamma"),
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

  #Fourth pre silver
  fourth_pre_silver_features_m_df <- read_tickers_catalog(
    raw_features_m_df = once_again_new_features_m_df,
    tickers_catalog = third_updated_catalog
  )

  #Check that there are no delisting removals, only untraded
  expect_equal(fourth_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-08-15`$row_removal_summary$out_trd_rg_only_NA, c(0, 1))
  expect_equal(fourth_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-08-15`$row_removal_summary$out_trd_rg_not_only_NA, c(0,0))
  expect_equal(fourth_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-08-15`$row_removal_summary$untrd_only_NA, c(1,0))
  expect_equal(fourth_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-08-15`$row_removal_summary$untrd_not_only_NA, c(0,0))
  expect_equal(fourth_pre_silver_features_m_df@data %>% dplyr::filter(!tickers %in% third_updated_catalog@perm_id["ENAT3"]) %>%
                 dplyr::group_by(tickers) %>% dplyr::summarize(count = dplyr::n()) %>% dplyr::pull(count) %>% unique(),1)
  expect_equal(fourth_pre_silver_features_m_df@data %>% dplyr::filter(tickers %in% third_updated_catalog@perm_id["ENAT3"]) %>% nrow(), 0)

  expect_false(third_updated_catalog@perm_id["CAFE3"] %in% fourth_pre_silver_features_m_df@data$tickers)
  expect_false(third_updated_catalog@perm_id["ENAT3"] %in% fourth_pre_silver_features_m_df@data$tickers)
  expect_true(third_updated_catalog@perm_id["TRPL4"] %in% fourth_pre_silver_features_m_df@data$tickers)
  expect_true(third_updated_catalog@perm_id["ISAE4"] %in% fourth_pre_silver_features_m_df@data$tickers)
  expect_true(third_updated_catalog@perm_id["CMED3"] %in% fourth_pre_silver_features_m_df@data$tickers)
  expect_true(third_updated_catalog@perm_id["BRAV3"] %in% fourth_pre_silver_features_m_df@data$tickers)
  expect_true(third_updated_catalog@perm_id["AGRO3"] %in% fourth_pre_silver_features_m_df@data$tickers)
  expect_true(third_updated_catalog@perm_id["KEPL3"] %in% fourth_pre_silver_features_m_df@data$tickers)
  expect_true(third_updated_catalog@perm_id["CALM3"] %in% fourth_pre_silver_features_m_df@data$tickers)



  # One more
  the_last_new_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(12, -20, 30, 0, NA, NA, 9, 12, 3, 5), nrow = 10, ncol = 1),
      matrix(c(4, NA, -6, NA, NA, NA, 250, 4, 0, 1), nrow = 10, ncol = 1),
      matrix(c(40, NA, 6, 9, NA, NA, 2, NA, 2, 3), nrow = 10, ncol = 1) # A new column should not change anything
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "PRFG3", "DMED3", "AGRO3", "KEPL3"),
    dates = as.Date(c("2001-09-15")),
    features_names = c("Alpha", "Beta", "Gamma"),
    meta_dataframe_name = "bronze_20010915"
  )

  date_first_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "ABEV3", "CALM3", "ENAT3", "CAFE3", "KEPL3", "VALE3", "PRFG3", "DMED3"),
    date_first_quote = as.Date(c("2001-08-10", "1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "2001-08-10", "1995-04-15", "2000-02-15", "2001-06-16"))
  )

  date_last_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "KEPL3", "PRFG3", "DMED3"),
    date_last_quote = as.Date(c("2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-05-15", NA, "2001-09-15", "2001-08-30", "2001-09-01"))
  )

  # Create another catalog
  the_last_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = the_last_new_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  ticker_changes <- data.frame(
    new_tickers = c("PRFG3"),
    old_tickers = c("ISAE4"),
    change_date = as.Date("2001-08-20")
  )

  # Update catalog
  expect_warning(
     fourth_updated_catalog <- update_tickers_catalog(
      old_tickers_catalog = third_updated_catalog,
      new_tickers_catalog = the_last_tickers_catalog,
      ticker_changes = ticker_changes
    )
  )

  #Last pre silver
  last_pre_silver_features_m_df <- read_tickers_catalog(
    raw_features_m_df = the_last_new_features_m_df,
    tickers_catalog = fourth_updated_catalog
  )

  #Check that there are no delisting removals, only untraded
  expect_equal(last_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-09-15`$row_removal_summary$out_trd_rg_only_NA, c(0, 0, 1,0))
  expect_equal(last_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-09-15`$row_removal_summary$out_trd_rg_not_only_NA, c(0,1,0,1))
  expect_equal(last_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-09-15`$row_removal_summary$untrd_only_NA, c(1,0,0,0))
  expect_equal(last_pre_silver_features_m_df@workflow$`read_tickers_catalog_2001-09-15`$row_removal_summary$untrd_not_only_NA, c(0,0,0,0))
  expect_equal(last_pre_silver_features_m_df@data %>% dplyr::filter(!tickers %in% fourth_updated_catalog@perm_id[c("ENAT3", "PRFG3", "DMED3")]) %>%
                 dplyr::group_by(tickers) %>% dplyr::summarize(count = dplyr::n()) %>% dplyr::pull(count) %>% unique(),1)
  expect_equal(last_pre_silver_features_m_df@data %>% dplyr::filter(tickers %in% fourth_updated_catalog@perm_id["ENAT3"]) %>% nrow(), 0)
  expect_equal(last_pre_silver_features_m_df@data %>% dplyr::filter(tickers %in% fourth_updated_catalog@perm_id["PRFG3"]) %>% nrow(), 0)
  expect_equal(last_pre_silver_features_m_df@data %>% dplyr::filter(tickers %in% fourth_updated_catalog@perm_id["DMED3"]) %>% nrow(), 0)



  expect_false(fourth_updated_catalog@perm_id["CAFE3"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_false(fourth_updated_catalog@perm_id["TRPL4"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_false(fourth_updated_catalog@perm_id["ISAE4"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_false(fourth_updated_catalog@perm_id["CMED3"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_false(fourth_updated_catalog@perm_id["DMED3"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_false(fourth_updated_catalog@perm_id["ENAT3"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_false(fourth_updated_catalog@perm_id["PRFG3"] %in% last_pre_silver_features_m_df@data$tickers)


  expect_true(fourth_updated_catalog@perm_id["BRAV3"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_true(fourth_updated_catalog@perm_id["AGRO3"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_true(fourth_updated_catalog@perm_id["KEPL3"] %in% last_pre_silver_features_m_df@data$tickers)
  expect_true(fourth_updated_catalog@perm_id["CALM3"] %in% last_pre_silver_features_m_df@data$tickers)

})

test_that("read_tickers_catalog works for an untraded changing ticker (no new perm_id assigned)", {

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
  first_update <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
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
  new_tickers_catalog <- create_tickers_catalog(
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
  second_update <- update_tickers_catalog(
    old_tickers_catalog = first_update,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = new_ticker_changes
  )

  # A new batch once again arrives
  once_again_new_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(12, -20, 30, 0, NA, NA, 9, 12, 3, 5), nrow = 10, ncol = 1),
      matrix(c(4, NA, -6, NA, NA, NA, 250, 4, 0, 1), nrow = 10, ncol = 1),
      matrix(c(40, NA, 6, 9, NA, NA, 2, NA, 2, 3), nrow = 10, ncol = 1) # A new column should not change anything
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3", "AGRO3", "KEPL3"),
    dates = as.Date(c("2001-08-15")),
    features_names = c("Alpha", "Beta", "Gamma"),
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
  third_update <- update_tickers_catalog(
    old_tickers_catalog = second_update,
    new_tickers_catalog = another_new_tickers_catalog,
    ticker_changes = NULL
  )

  # One more batch arrives
  once_again_new_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(1, -2, 30, 1, -90, 5, 9, 12, 3, 5), nrow = 10, ncol = 1),
      matrix(c(40, NA, -60, 2, 3, 1, 250, 4, 0, 1), nrow = 10, ncol = 1),
      matrix(c(4, 4, 6, 9, 33, 2, 2, 0, 2, 3), nrow = 10, ncol = 1) # A new column should not change anything
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "LEIT4", "ISAE4", "DMED3", "AGRO3", "KEPL3"),
    dates = as.Date(c("2001-09-15")),
    features_names = c("Alpha", "Beta", "Gamma"),
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
    raw_features_m_df = once_again_new_features_m_df,
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
    last_update <- update_tickers_catalog(
      old_tickers_catalog = third_update,
      new_tickers_catalog = once_again_new_tickers_catalog,
      ticker_changes = ticker_changes
    )
  )

  #RESULTS
  results <- read_tickers_catalog(
    raw_features_m_df = once_again_new_features_m_df,
    tickers_catalog = last_update
  )

  #Expect that LEIT4/CAFE3 are not present
  expect_false(last_update@perm_id["LEIT4"] %in% results@data$tickers)
  expect_false(last_update@perm_id["CAFE3"] %in% results@data$tickers)
  expect_true(last_update@perm_id["DMED3"] %in% results@data$tickers)
  expect_equal(results@workflow$`read_tickers_catalog_2001-09-15`$row_removal_summary$untrd_not_only_NA, c(0,1))


})

test_that("read_tickers_catalog works for an untraded IPO ticker", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, NA, 9, 4, -1, 0, 3, NA, 1, NA, -4), nrow = 5, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(1, 2, 3, 4, NA, NA), nrow = 6, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA), nrow = 6, ncol = 1)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    as.Date(c("2001-06-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- NULL

  # Update catalog
  suppressWarnings(
      updated_catalog <- update_tickers_catalog(
        old_tickers_catalog = old_tickers_catalog,
        new_tickers_catalog = new_tickers_catalog,
        ticker_changes = ticker_changes
      )
  )

  #@RESULTS
  results <- read_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    tickers_catalog = updated_catalog
  )

  expect_false(new_tickers_catalog@perm_id["CAFE3"] %in% results@data$tickers)
  expect_equal(results@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$untrd_only_NA, c(1,0))




})

test_that("read_tickers_catalog works for an delisted IPO ticker", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, NA, 9, 4, -1, 0, 3, NA, 1, NA, -4), nrow = 5, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA), nrow = 5, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("ABEV3", "VALE3", "PETR4", "RRRP3", "ENAT3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15", "1999-05-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3"),
    date_last_quote = as.Date(c("2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15"))
  )

  old_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # A new batch of data arrives
  new_raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(1, 2, 3, 4, NA, NA), nrow = 6, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA), nrow = 6, ncol = 1)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    as.Date(c("2001-06-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-06-01", "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", "2001-06-02"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- NULL

  # Update catalog
  expect_warning(
    expect_no_error(
      updated <- update_tickers_catalog(
        old_tickers_catalog = old_tickers_catalog,
        new_tickers_catalog = new_tickers_catalog,
        ticker_changes = ticker_changes
      ))
  )

  #results
  results <- read_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    tickers_catalog = updated
  )

  #Check
  expect_false("CAFE3" %in% results@data$tickers)
  expect_equal(results@workflow$`read_tickers_catalog_2001-06-15`$row_removal_summary$out_trd_rg_only_NA, c(1,1))


})

test_that("read_tickers_catalog works real data", {

  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  #Apply function
  results <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog)

  #Check that untraded are NOT present
  expect_true(all(!tickers_catalog@untraded %in% lookup_catalog(tickers_catalog, perm_id_to_lookup = results@data$ticker)))
  expect_true(all(!tickers_catalog@catalog$tickers[which(is.na(tickers_catalog@catalog$date_last_quote))]  %in% names(results@data$ticker)))
  expect_true(all(!tickers_catalog@catalog$tickers[which(is.na(tickers_catalog@catalog$date_first_quote))]  %in% names(results@data$ticker)))

  #Check for correct count of delisted
  delisted_features_m_df <- raw_features_m_df@data %>% dplyr::filter(tickers %in% tickers_catalog@delisted)

  delisted_only_NA <- delisted_features_m_df$tickers[delisted_features_m_df[,-c(1:3)] %>% apply(1, function(x) all(is.na(x)))] %>% unique()
  expect_equal(results@workflow$`read_tickers_catalog_2023-09-15`$row_removal_summary %>% dplyr::filter(tickers %in% delisted_only_NA) %>%
                 dplyr::pull(out_trd_rg_only_NA),
               rep(3, length(delisted_only_NA)))

  delisted_not_only_NA <- delisted_features_m_df$tickers[delisted_features_m_df[,-c(1:3)] %>% apply(1, function(x) !all(is.na(x)))] %>% unique()
  expect_equal(results@workflow$`read_tickers_catalog_2023-09-15`$row_removal_summary %>% dplyr::filter(tickers %in% delisted_not_only_NA) %>%
                 dplyr::pull(out_trd_rg_not_only_NA),
               rep(3, length(delisted_not_only_NA)))

  #Check for correct countr of untraded
  untraded_features_m_df <- raw_features_m_df@data %>% dplyr::filter(tickers %in% tickers_catalog@untraded)
  untraded_only_NA <- untraded_features_m_df$tickers[untraded_features_m_df[,-c(1:3)] %>% apply(1, function(x) all(is.na(x)))] %>% unique()
  expect_equal(results@workflow$`read_tickers_catalog_2023-09-15`$row_removal_summary %>% dplyr::filter(tickers %in% untraded_only_NA) %>%
                 dplyr::pull(untrd_only_NA),
               rep(3, length(untraded_only_NA)))

  untraded_not_only_NA <- untraded_features_m_df$tickers[untraded_features_m_df[,-c(1:3)] %>% apply(1, function(x) !all(is.na(x)))] %>% unique()
  expect_equal(results@workflow$`read_tickers_catalog_2023-09-15`$row_removal_summary %>% dplyr::filter(tickers %in% untraded_not_only_NA) %>%
                 dplyr::pull(untrd_not_only_NA),
               rep(3, length(untraded_not_only_NA)))

  #Check that the number of columns with more than 0 is always 1
  expect_equal(
  results@workflow$`read_tickers_catalog_2023-09-15`$row_removal_summary[,-1] %>% apply(1, function(x) length(which(x != 0))) %>% unique(),
  1)

  #Check that stocks with last_quote is before current_date are not present
  expect_equal(nrow(
    results@data %>%
      dplyr::filter(tickers %in% tickers_catalog@catalog$tickers[which(
        tickers_catalog@catalog$date_last_quote < (tickers_catalog@current_date) - tickers_catalog@n_days_tolerance)] #Filter tickers with last_quote before current_date
      )),
    0)

  #Check that stocks with first_quote after current_date are not present
  expect_equal(nrow(
    results@data %>%
      dplyr::filter(tickers %in% tickers_catalog@catalog$tickers[which(
        tickers_catalog@catalog$date_first_quote > tickers_catalog@current_date)] #Filter tickers with last_quote before current_date
      )),
    0)

  #Let's suppose that ABCB4 first quote were on 2023-08-15
  abcb4_original <- results@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-07-15")
  #Change catalog
  tickers_catalog@catalog$tickers_first_quote[3] <- "2023-08-15" %>% as.Date()

  results2 <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog)
  abcb4_madeup <- results2@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-07-15")

  expect_equal(nrow(abcb4_madeup), 0)
  expect_false(identical(abcb4_madeup, abcb4_original))

  #Now suppose ABCB4 last quote were on 2023-08-15
  abcb4_original <- results@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-09-15")

  #Change catalog
  tickers_catalog@catalog$tickers_last_quote[3] <- "2023-08-15" %>% as.Date()
  results2 <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog)
  abcb4_madeup <- results2@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-09-15")

  expect_equal(nrow(abcb4_madeup), 0)
  expect_false(identical(abcb4_madeup, abcb4_original))




})

test_that("read_tickers_catalog correctly responds to n_days_tolerance", {
  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote,
                                            n_days_tolerance = 10)

  #Apply function
  results <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog)

  #Change tickers catalog
  tickers_catalog2 <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote,
                                            n_days_tolerance = 1)
  results2 <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog2)

  #Check that EALT3 is now out
  expect_equal(nrow(results@data %>% dplyr::filter(tickers == "9ac40e33b6", dates == "2023-09-15")), 1)
  expect_equal(nrow(results2@data %>% dplyr::filter(tickers == "9ac40e33b6", dates == "2023-09-15")), 0)

})

test_that("read_tickers_catalog throws an error when versions do not match", {

  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:E22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:E1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  #Now one updates raw_features_m-df
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #But uses an outdated catalog
  expect_error(
    read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog),
    "The current_date of raw_features_m_df does not match the one in tickers_catalog"
  )

  #The same would happen if one uses a different name mdf
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:E22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:E1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))

  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names,
                                             meta_dataframe_name = "other_name")

  expect_error(
    read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog),
    "The meta_dataframe_name of raw_features_m_df does not match the one in tickers_catalog"
  )



})

test_that("read_tickers_catalog throws an error when there are tickers not in catalog or when there is old ticker", {

  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  #Now change a ticker
  raw_features_m_df@data$tickers[1] <- "TIMS3"
  #Apply function
  expect_error(
  read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog),
  "Some tickers in raw_features_m_df are not present in tickers_catalog"
  )

  #Recreate
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)


  #For existence of old ticker
  tickers_catalog@old <- "ABCB4"

  expect_error(
  read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog),
  "raw_features_m_df should not have 'old' tickers."
  )



})


