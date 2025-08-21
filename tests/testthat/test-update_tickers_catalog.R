test_that("update_tickers_catalog works for a ticker change scenario (and a change in order between date_first_quote and date_last_quote)", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15"))
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
    c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3"),
    as.Date(c("2001-06-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3"),
    old_tickers = c("RRRP3"),
    change_date = as.Date("2001-06-02")
  )

  # Update catalog
  results <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  # Check that perm_id for RRRP3 and BRAV3 match
  expect_equal(
    results@perm_id["BRAV3"] %>% unname(),
    old_tickers_catalog@perm_id["RRRP3"] %>% unname()
  )
  expect_equal(
    results@catalog[3, "perm_id"], old_tickers_catalog@catalog[3, "perm_id"]
  )
  # Check that remaining perm_ids are the same
  expect_equal(
    results@perm_id[c("VALE3", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname(),
    old_tickers_catalog@perm_id[c("VALE3", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname()
  )
  expect_equal(
    results@catalog[which(results@catalog$tickers %in% c("VALE3", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")), "perm_id"],
    old_tickers_catalog@catalog[which(old_tickers_catalog@catalog$tickers %in% c("VALE3", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")), "perm_id"]
  )
  # Check that ENAT3 was listed and now it isn't anymore
  expect_false("ENAT3" %in% results@listed)
  expect_true("ENAT3" %in% results@delisted)
  expect_true("ENAT3" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[6, "listed"])
  expect_true(old_tickers_catalog@catalog[5, "listed"])

  # Check that BRAV3 is now listed
  expect_true("BRAV3" %in% results@listed)
  expect_true(results@catalog[3, "listed"])

  # Check that RRRP3 date of last quote and BRAV3 date of first quote are equal to change_date
  expect_equal(results@catalog[4, "tickers_last_quote"], ticker_changes$change_date)
  expect_equal(results@catalog[3, "tickers_first_quote"], ticker_changes$change_date)
  expect_equal(results@catalog[4, "tickers_last_quote"], results@catalog[3, "tickers_first_quote"])

  # Check that RRRP3 is old
  expect_true("RRRP3" %in% results@old)

  # Test: ticker change history is updated
  expect_true(nrow(results@ticker_change_history) >= 1)

  # Check that untraded keeps untraded
  expect_equal(results@untraded, old_tickers_catalog@untraded)
  # Check listed
  expect_true(all(c("BRAV3", "PETR4", "VALE3", "ABEV3") %in% results@listed))
  # Check untraded
  expect_true(all(c("CAFE3") %in% results@untraded))
})

test_that("update_tickers_catalog works for a ticker change scenario (two changes + a new one)", {
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
  results <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  # Check that perm_id for RRRP3 and BRAV3 match
  expect_equal(
    results@perm_id["BRAV3"] %>% unname(),
    old_tickers_catalog@perm_id["RRRP3"] %>% unname()
  )
  expect_equal(results@catalog[6, "perm_id"], old_tickers_catalog@catalog[4, "perm_id"])
  expect_equal(results@catalog[6, "perm_id"], results@catalog[7, "perm_id"])

  # Check that perm_id for TRPL4 and ISAE4 match
  expect_equal(
    results@perm_id["ISAE4"] %>% unname(),
    old_tickers_catalog@perm_id["TRPL4"] %>% unname()
  )
  expect_equal(results@catalog[1, "perm_id"], old_tickers_catalog@catalog[1, "perm_id"])
  expect_equal(results@catalog[1, "perm_id"], results@catalog[2, "perm_id"])

  # Check that CMED3 is a new listed ticker
  expect_true("CMED3" %in% results@listed)
  expect_false("CMED3" %in% old_tickers_catalog@catalog$tickers)


  # Check that remaining perm_ids are the same
  expect_equal(
    results@perm_id[c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname(),
    old_tickers_catalog@perm_id[c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname()
  )
  expect_equal(
    results@catalog[which(results@catalog$tickers %in% c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")), "perm_id"],
    old_tickers_catalog@catalog[which(old_tickers_catalog@catalog$tickers %in% c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")), "perm_id"]
  )
  # Check that ENAT3 and TRPL4 were listed and now it isn't anymore
  expect_false("ENAT3" %in% results@listed)
  expect_true("ENAT3" %in% results@delisted)
  expect_true("ENAT3" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[9, "listed"])
  expect_true(results@catalog[9, "delisted"])
  expect_true(old_tickers_catalog@catalog[6, "listed"])

  expect_false("TRPL4" %in% results@listed)
  expect_true("TRPL4" %in% results@old)
  expect_true("TRPL4" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[2, "listed"])
  expect_true(results@catalog[2, "old"])
  expect_true(old_tickers_catalog@catalog[1, "listed"])

  # Check that BRAV3, CMED3 and ISAE4 are now listed
  expect_true("BRAV3" %in% results@listed)
  expect_true(results@catalog[6, "listed"])
  expect_true("CMED3" %in% results@listed)
  expect_true(results@catalog[3, "listed"])
  expect_true("ISAE4" %in% results@listed)
  expect_true(results@catalog[1, "listed"])

  # Check that RRRP3 and TRPL4 last-quote is equal to old ticker's first_quote
  expect_equal(results@catalog[2, "tickers_last_quote"], results@catalog[1, "tickers_first_quote"])
  expect_equal(results@catalog[7, "tickers_last_quote"], results@catalog[6, "tickers_first_quote"])

  expect_equal(results@catalog[7, "tickers_last_quote"], ticker_changes$change_date[1])
  expect_equal(results@catalog[2, "tickers_last_quote"], ticker_changes$change_date[2])

  # Test: ticker change history is updated
  expect_true(nrow(results@ticker_change_history) >= 1)

  # Check that untraded keeps untraded
  expect_equal(results@untraded, old_tickers_catalog@untraded)

  # Check delisted
  expect_true(all(c("ENAT3") %in% results@delisted))
  # Check old
  expect_true(all(c("TRPL4", "RRRP3") %in% results@old))
  # Check listed
  expect_true(all(c("BRAV3", "CMED3", "ISAE4", "PETR4", "VALE3", "ABEV3") %in% results@listed))
  # Check untraded
  expect_true(all(c("CAFE3") %in% results@untraded))
})

test_that("update_tickers_catalog works for a ticker being changed twice consecutively", {
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
    tickers = c("ABEV3", "VALE3", "PETR4", "BRAV3", "ENAT3", "CAFE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c(
      "1995-03-15", "1995-04-15", "1995-05-15", "1999-03-15",
      "1999-05-15", NA, "2000-02-15", "2001-05-18"
    ))
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
    tickers = c("VALE3", "ABEV3", "PETR4", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3"),
    date_first_quote = as.Date(c(
      "1995-04-15", "1995-03-15", "1995-05-15", "1999-03-15",
      "1999-05-15", NA, "2000-02-15", "2001-05-18"
    ))
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
    change_date = as.Date(c("2001-07-09", "2001-06-19"))
  )

  # Update catalog
  results <- update_tickers_catalog(
    old_tickers_catalog = first_update,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = new_ticker_changes
  )

  # Check that perm_id for RRRP3, BRAV3 and CALM3 match
  expect_equal(
    results@perm_id["BRAV3"] %>% unname(),
    old_tickers_catalog@perm_id["RRRP3"] %>% unname()
  )
  expect_equal(
    results@perm_id["CALM3"] %>% unname(),
    old_tickers_catalog@perm_id["RRRP3"] %>% unname()
  )

  expect_equal(results@catalog[7, "perm_id"], old_tickers_catalog@catalog[4, "perm_id"])
  expect_equal(results@catalog[8, "perm_id"], old_tickers_catalog@catalog[4, "perm_id"])

  expect_equal(results@catalog[8, "perm_id"], results@catalog[7, "perm_id"])
  expect_equal(results@catalog[8, "perm_id"], results@catalog[9, "perm_id"])

  # Check that perm_id for TRPL4 and ISAE4 match
  expect_equal(
    results@perm_id["ISAE4"] %>% unname(),
    old_tickers_catalog@perm_id["TRPL4"] %>% unname()
  )
  expect_equal(results@catalog[1, "perm_id"], old_tickers_catalog@catalog[1, "perm_id"])
  expect_equal(results@catalog[1, "perm_id"], results@catalog[2, "perm_id"])

  # Check that perm_id for CMED3 and DMED3 match
  expect_equal(
    results@perm_id["DMED3"] %>% unname(),
    first_update@perm_id["CMED3"] %>% unname()
  )
  expect_equal(results@catalog[3, "perm_id"], first_update@catalog[3, "perm_id"])
  expect_equal(results@catalog[3, "perm_id"], results@catalog[4, "perm_id"])

  # Check that remaining perm_ids are the same
  expect_equal(
    results@perm_id[c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname(),
    old_tickers_catalog@perm_id[c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname()
  )
  expect_equal(
    results@perm_id[c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname(),
    old_tickers_catalog@perm_id[c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname()
  )
  expect_equal(
    results@perm_id[c("ISAE4", "TRPL4", "CMED3", "VALE3", "PETR4", "BRAV3", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname(),
    first_update@perm_id[c("ISAE4", "TRPL4", "CMED3", "VALE3", "PETR4", "BRAV3", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname()
  )

  # Check that ENAT3, TRPL4 and CMED3 were listed and now aren't anymore
  expect_false("ENAT3" %in% results@listed)
  expect_true("ENAT3" %in% results@delisted)
  expect_true("ENAT3" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[9, "listed"])
  expect_true(results@catalog[9, "old"])
  expect_true(old_tickers_catalog@catalog[6, "listed"])

  expect_false("TRPL4" %in% results@listed)
  expect_true("TRPL4" %in% results@old)
  expect_true("TRPL4" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[2, "listed"])
  expect_true(results@catalog[2, "old"])
  expect_true(old_tickers_catalog@catalog[1, "listed"])

  expect_false("CMED3" %in% results@listed)
  expect_true("CMED3" %in% results@old)
  expect_true("CMED3" %in% first_update@listed)
  expect_false(results@catalog[4, "listed"])
  expect_true(results@catalog[4, "old"])
  expect_true(first_update@catalog[3, "listed"])

  # Check that CALM3, DMED3 and ISAE4 are now listed
  expect_true("CALM3" %in% results@listed)
  expect_true(results@catalog[7, "listed"])
  expect_true("DMED3" %in% results@listed)
  expect_true(results@catalog[3, "listed"])
  expect_true("ISAE4" %in% results@listed)
  expect_true(results@catalog[1, "listed"])

  # Check that RRRP3, TRPL4, CMED3 and BRAV3 date of last quote is correct
  ## TRPL4/ISAE4
  expect_equal(results@catalog[2, "tickers_last_quote"], results@catalog[1, "tickers_first_quote"])
  ## RRRP3/BRAV3
  expect_equal(results@catalog[9, "tickers_last_quote"], results@catalog[8, "tickers_first_quote"])
  ## BRAV3/CALM3
  expect_equal(results@catalog[8, "tickers_last_quote"], results@catalog[7, "tickers_first_quote"])
  ## CMED3/DMED3
  expect_equal(results@catalog[4, "tickers_last_quote"], results@catalog[3, "tickers_first_quote"])

  expect_equal(results@catalog[9, "tickers_last_quote"], ticker_changes$change_date[1])
  expect_equal(results@catalog[2, "tickers_last_quote"], ticker_changes$change_date[2])

  expect_equal(results@catalog[8, "tickers_last_quote"], new_ticker_changes$change_date[1])
  expect_equal(results@catalog[4, "tickers_last_quote"], new_ticker_changes$change_date[2])

  expect_equal(results@catalog[8, "tickers_first_quote"], ticker_changes$change_date[1])
  expect_equal(results@catalog[1, "tickers_first_quote"], ticker_changes$change_date[2])
  expect_equal(results@catalog[7, "tickers_first_quote"], new_ticker_changes$change_date[1])
  expect_equal(results@catalog[3, "tickers_first_quote"], new_ticker_changes$change_date[2])


  # Test: ticker change history is updated
  expect_equal(nrow(results@ticker_change_history), 4)

  # Check that untraded keeps untraded
  expect_equal(results@untraded, old_tickers_catalog@untraded)

  # Check delisted
  expect_true(all(c("ENAT3") %in% results@delisted))
  # Check old
  expect_true(all(c("TRPL4", "RRRP3", "CMED3", "BRAV3") %in% results@old))
  # Check listed
  expect_true(all(c("CALM3", "DMED3", "ISAE4", "PETR4", "VALE3", "ABEV3") %in% results@listed))
  # Check untraded
  expect_true(all(c("CAFE3") %in% results@untraded))
})

test_that("update_tickers_catalog works for a date without ANY changes (no new ticker and no ticker change)", {
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
    tickers = c("ABEV3", "VALE3", "PETR4", "ENAT3", "BRAV3", "CAFE3", "ISAE4", "CMED3"),
    date_first_quote = as.Date(c("1995-03-15", "1995-04-15", "1995-05-15", "1999-05-15", "1999-03-15", NA, "2000-02-15", "2001-05-18"))
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
    change_date = as.Date(c("2001-07-09", "2001-06-19"))
  )

  # Update catalog
  second_update <- update_tickers_catalog(
    old_tickers_catalog = first_update,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = new_ticker_changes
  )

  # A new bacht once again arrives
  once_again_new_features_m_df <- create_meta_dataframe(
    data = list(
      matrix(c(12, -20, 30, 0, NA, NA, 9, 12), nrow = 8, ncol = 1),
      matrix(c(4, NA, -6, NA, NA, NA, 250, 4), nrow = 8, ncol = 1),
      matrix(c(40, NA, 6, 9, NA, NA, 2, NA), nrow = 8, ncol = 1) # A new column should not change anything
    ),
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3"),
    dates = as.Date(c("2001-08-15")),
    features_names = c("Alpha", "Beta", "Gamma"),
    meta_dataframe_name = "bronze_20010815"
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "CALM3", "ENAT3", "CAFE3", "VALE3", "ISAE4", "DMED3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c("2001-08-15", "2001-08-15", "2001-08-15", "2001-08-15", "2001-05-15", NA, "2001-08-15", "2001-08-14"))
  )

  # Create another catalog
  another_new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = once_again_new_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Update catalog
  results <- update_tickers_catalog(
    old_tickers_catalog = second_update,
    new_tickers_catalog = another_new_tickers_catalog,
    ticker_changes = NULL
  )

  # Check that listed is the same
  expect_equal(results@listed, second_update@listed)
  expect_equal(results@catalog$listed, second_update@catalog$listed)

  # Check that delisted is the same
  expect_equal(results@delisted, second_update@delisted)
  expect_equal(results@catalog$delisted, second_update@catalog$delisted)

  # Check that untraded is the same
  expect_equal(results@untraded, second_update@untraded)
  expect_equal(results@catalog$untraded, second_update@catalog$untraded)

  # Check that old is the same
  expect_equal(results@old, second_update@old)
  expect_equal(results@catalog$old, second_update@catalog$old)

  # Check that first quote is the same
  expect_equal(results@catalog$tickers_first_quote, second_update@catalog$tickers_first_quote)

  # Check that last quote is different
  expect_false(all(results@catalog$tickers_last_quote == second_update@catalog$tickers_last_quote))
  expect_true(mean(results@catalog$tickers_last_quote, na.rm = TRUE) >= mean(second_update@catalog$tickers_last_quote, na.rm = TRUE))

  # Check that tickers are the same
  expect_equal(results@catalog$tickers, second_update@catalog$tickers)

  # Check that perm_id is the same
  expect_equal(results@catalog$perm_id, second_update@catalog$perm_id)
})

test_that("update_tickers_catalog works for a date without ticker change but new tickers", {
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
  third_update <- update_tickers_catalog(
    old_tickers_catalog = second_update,
    new_tickers_catalog = another_new_tickers_catalog,
    ticker_changes = NULL
  )

  # Check that listed is NOT the same
  expect_false(all(third_update@listed %in% second_update@listed))
  expect_true(all(c("AGRO3", "KEPL3", second_update@listed) %in% third_update@listed))

  # Check that delisted is the same
  expect_equal(third_update@delisted, second_update@delisted)

  # Check that untraded is the same
  expect_equal(third_update@untraded, second_update@untraded)

  # Check that old is the same
  expect_equal(third_update@old, second_update@old)

  # Check that tickers are NOT the same
  expect_false(all(third_update@catalog$tickers %in% second_update@catalog$tickers))
  expect_true(all(c("AGRO3", "KEPL3", second_update@catalog$tickers) %in% third_update@catalog$tickers))

  # Check that perm_id is the same for old tickers
  expect_equal(
    second_update@catalog$perm_id,
    third_update@catalog %>% dplyr::filter(!tickers %in% c("AGRO3", "KEPL3")) %>% dplyr::pull(perm_id)
  )
})

test_that("update_tickers_catalog works for a ticker changing ticker and being simultaneously delisted", {
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
  third_update <- update_tickers_catalog(
    old_tickers_catalog = second_update,
    new_tickers_catalog = another_new_tickers_catalog,
    ticker_changes = NULL
  )


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
    results <- update_tickers_catalog(
      old_tickers_catalog = third_update,
      new_tickers_catalog = the_last_tickers_catalog,
      ticker_changes = ticker_changes
    )
  )

  # Check that PRFG is delisted
  expect_true("PRFG3" %in% results@delisted)
  expect_true("ISAE4" %in% results@old)
  # Check that first_date of PRF3 matches ISAE
  expect_equal(results@catalog[1, "tickers_first_quote"], results@catalog[2, "tickers_last_quote"])

  # Check that perm_id is the same
  expect_equal(results@catalog$perm_id["PRFG3"] %>% unname(), third_update@catalog$perm_id["ISAE4"] %>% unname())
  expect_equal(results@catalog$perm_id["PRFG3"] %>% unname(), results@catalog$perm_id["ISAE4"] %>% unname())

  # Check that delisted is NOT the same
  expect_false(identical(third_update@delisted, results@delisted))

  # Check that DMED3 was delisted
  expect_true("DMED3" %in% results@delisted)

  # Check that history was updated
  expect_equal(results@ticker_change_history %>% tail(1) %>% as.vector(), ticker_changes %>% as.vector())
})

test_that("update_tickers_catalog works for an untraded changing ticker (no new perm_id assigned)", {

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
    results <- update_tickers_catalog(
      old_tickers_catalog = third_update,
      new_tickers_catalog = once_again_new_tickers_catalog,
      ticker_changes = ticker_changes
    )
  )

  # Check that LEIT4 kept perm_id
  expect_true("LEIT4" %in% results@untraded)
  expect_true("CAFE3" %in% results@old)
  expect_true(results@perm_id["LEIT4"] %>% unname() == results@perm_id["CAFE3"] %>% unname())

  expect_true(identical(results@perm_id["CAFE3"], third_update@perm_id["CAFE3"]))
  expect_true(results@perm_id["LEIT4"] %>% unname() == third_update@perm_id["CAFE3"] %>% unname())

  # Check for last quote
  expect_equal(results@catalog[12, "tickers_first_quote"], results@catalog[13, "tickers_last_quote"])
  expect_equal(results@catalog[12, "tickers_last_quote"], as.Date(NA))
  expect_equal(results@catalog[13, "tickers_first_quote"], as.Date(NA))


  # Check that ticker history is updated
  results@ticker_change_history %>%
    tail(1) %>%
    {
      expect_true(.$old_tickers == "CAFE3")
      expect_true(.$new_tickers == "LEIT4")
      expect_true(.$change_date == as.Date("2001-08-29"))
    }
})

test_that("update_tickers_catalog works for an untraded IPO ticker", {
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
  expect_warning(
  expect_no_error(
    results <- update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = ticker_changes
    ))
  )

  expect_true("CAFE3" %in% results@untraded)

})

test_that("update_tickers_catalog works for an delisted IPO ticker", {
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
      results <- update_tickers_catalog(
        old_tickers_catalog = old_tickers_catalog,
        new_tickers_catalog = new_tickers_catalog,
        ticker_changes = ticker_changes
      ))
  )

  expect_true("CAFE3" %in% results@delisted)

})


#Test for wrong structure in tickers_change
test_that("update_tickers_catalog fails for wrong structure in ticker_changes", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15"))
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
    c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3"),
    as.Date(c("2001-06-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create wrong_ticker_change
  wrong_ticker_changes <- data.frame(
    new_ticker = c("BRAV3"),
    old_tickers = c("RRRP3"),
    change_date = as.Date("2001-06-02")
  )

  # Update catalog
  expect_error(
  update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = wrong_ticker_changes
  ),
  "ticker_changes must contain columns: 'new_tickers', 'old_tickers', and 'change_date'."
  )

  # Create wrong_ticker_change
  wrong_ticker_changes <- data.frame(
    new_tickers = 22,
    old_tickers = c("RRRP3"),
    change_date = as.Date("2001-06-02")
  )

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = wrong_ticker_changes
    ),
    "Columns 'new_tickers' and 'old_tickers' in ticker_changes must be character type."
  )

  # Create wrong_ticker_change
  wrong_ticker_changes <- data.frame(
    new_tickers = "BRAV3",
    old_tickers = c("RRRP3"),
    change_date = NA
  )

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = wrong_ticker_changes
    ),
    "Column 'change_date' in ticker_changes must be of Date type."
  )

  # Create wrong_ticker_change
  wrong_ticker_changes <- data.frame(
    new_tickers = "BRAV3",
    old_tickers = c("RRRP3"),
    change_date = NA
  )

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = wrong_ticker_changes
    ),
    "Column 'change_date' in ticker_changes must be of Date type."
  )

  # Create wrong_ticker_change
  wrong_ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ECOR3"),
    old_tickers = c("RRRP3", "ECOR4"),
    change_date = as.Date(NA, "2002-02-15")
  )

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = wrong_ticker_changes
    ),
    "ticker_changes can't have NAs."
  )

  # Create wrong_ticker_change
  wrong_ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "BRAV3"),
    old_tickers = c("RRRP3", "RRRP4"),
    change_date = as.Date("2002-02-15", "2002-02-15")
  )

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = wrong_ticker_changes
    ),
    "ticker_changes can't have duplicate columns."
  )

  # Create wrong_ticker_change
  wrong_ticker_changes <- data.frame(
    new_tickers = c("BRAV4"),
    old_tickers = c("RRRP3"),
    change_date = as.Date("2002-02-15")
  )

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = wrong_ticker_changes
    ),
    "Newly added tickers can't be decomposed into IPOs and missing old tickers"
  )

  # Create wrong_ticker_change
  wrong_ticker_changes <- data.frame(
    new_tickers = c("BRAV3"),
    old_tickers = c("RRRP4"),
    change_date = as.Date("2002-02-15")
  )

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = wrong_ticker_changes
    ),
    "Newly added tickers can't be decomposed into IPOs and missing old tickers"
  )




})

#Test for errors (tickers dynamics)
test_that("update_tickers_catalog fails when a listed ticker becomes untraded in an update", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15"))
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
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", NA, "1999-05-15", NA, "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", NA, "2001-05-15", NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- NULL

  # Update catalog
  expect_error(
    update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
    ),
    "Listed tickers from old_tickers_catalog are now untraded in new_tickers_catalog."
  )
})

test_that("update_tickers_catalog fails when a listed ticker changes ticker and becomes untraded", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15"))
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
    c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3"),
    as.Date(c("2001-06-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", NA, "1999-05-15", NA, "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", NA, "2001-05-15", NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3"),
    old_tickers = c("RRRP3"),
    change_date = as.Date("2001-06-02")
  )

  # Update catalog
  expect_warning(
  expect_error(
    update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
    ),
    "tickers_first_quote and tickers_last_quote should not be NA for tickers that were listed in old_tickers_catalog."
    )
  )


})

test_that("update_tickers_catalog fails when a delisted ticker changes ticker", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-02"))
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
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT4", "CAFE3"),
    as.Date(c("2001-06-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "RRRP3", "ENAT4", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", NA, "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT4", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("ENAT4"),
    old_tickers = c("ENAT3"),
    change_date = as.Date(c("2001-06-10"))
  )

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = ticker_changes
    ),
    "Delisted tickers are changing ticker in ticker_changes. For relistings, treat it as a completely new ticker"
  )
})

test_that("update_tickers_catalog fails when a delisted ticker becomes untraded in an update", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-02"))
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
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", NA, NA, "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", NA, NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- NULL

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = ticker_changes
    ),
    "Delisted tickers from old_tickers_catalog are now untraded in new_tickers_catalog."
  )
})

test_that("update_tickers_catalog fails when a delisted ticker becomes listed in an update", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-02"))
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
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-03-15", NA, "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- NULL

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = ticker_changes
    ),
    "Delisted tickers from old_tickers_catalog are now listed in new_tickers_catalog."
  )
})

test_that("update_tickers_catalog fails for an untraded being listed", {

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
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3", "AGRO3", "KEPL3"),
    dates = as.Date(c("2001-09-15")),
    features_names = c("Alpha", "Beta", "Gamma"),
    meta_dataframe_name = "bronze_20010915"
  )

  date_first_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "ABEV3", "CALM3", "ENAT3", "CAFE3", "KEPL3", "VALE3", "ISAE4", "DMED3"),
    date_first_quote = as.Date(c("2001-08-10", "1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-08-29", "2001-08-10", "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "KEPL3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c(
      "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-05-15",
      "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-14"
    ))
  )

  # Create another catalog
  once_again_new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = once_again_new_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )


  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = third_update,
      new_tickers_catalog = once_again_new_tickers_catalog,
      ticker_changes = NULL
    ),
    "Untraded tickers from old_tickers_catalog are now listed in new_tickers_catalog."
  )

})

test_that("update_tickers_catalog fails for an untraded being delisted", {

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
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3", "AGRO3", "KEPL3"),
    dates = as.Date(c("2001-09-15")),
    features_names = c("Alpha", "Beta", "Gamma"),
    meta_dataframe_name = "bronze_20010915"
  )

  date_first_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "ABEV3", "CALM3", "ENAT3", "CAFE3", "KEPL3", "VALE3", "ISAE4", "DMED3"),
    date_first_quote = as.Date(c("2001-08-10", "1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-08-29", "2001-08-10", "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "KEPL3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c(
      "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-05-15",
      "2001-09-02", "2001-09-15", "2001-09-15", "2001-09-14"
    ))
  )

  # Create another catalog
  once_again_new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = once_again_new_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )


  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = third_update,
      new_tickers_catalog = once_again_new_tickers_catalog,
      ticker_changes = NULL
    ),
    "Untraded tickers from old_tickers_catalog are now delisted in new_tickers_catalog."
  )

})

test_that("update_tickers_catalog fails for an untraded changing ticker and being listed", {

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
    date_first_quote = as.Date(c("2001-08-10", "1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-08-29", "2001-08-10", "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "LEIT4", "KEPL3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c("2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-05-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-14"))
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
  expect_error(
  update_tickers_catalog(
      old_tickers_catalog = third_update,
      new_tickers_catalog = once_again_new_tickers_catalog,
      ticker_changes = ticker_changes
    ),
  "tickers_first_quote and tickers_last_quote should be NA for tickers that were untraded in old_tickers_catalog."
  )

})

test_that("update_tickers_catalog fails for an untraded changing ticker and being delisted", {

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
    date_first_quote = as.Date(c("2001-08-10", "1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-08-29", "2001-08-10", "1995-04-15", "2000-02-15", "2001-05-18"))
  )

  date_last_quote <- data.frame(
    tickers = c("AGRO3", "PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "LEIT4", "KEPL3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c("2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-09-15", "2001-05-15", "2001-09-01", "2001-09-15", "2001-09-15", "2001-09-14"))
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
  expect_warning(
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = third_update,
      new_tickers_catalog = once_again_new_tickers_catalog,
      ticker_changes = ticker_changes
    ),
    "tickers_first_quote and tickers_last_quote should be NA for tickers that were untraded in old_tickers_catalog."
  )
  )

})

test_that("update_tickers_catalog fails when a delisted ticker changes date_last_quote", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-02"))
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
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-01", NA))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- NULL

  # Update catalog
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = ticker_changes
    ),
    "Mismatch in tickers_last_quote between delisted tickers in old and new catalogs.For relistings, use 'old_ticker'_n, where n is relisting_number"
  )
})

test_that("update_tickers_catalog fails when there is a mismatch in tickers_first_quote", {
  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
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
    date_last_quote = as.Date(c(NA, "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15", "2001-05-15"))
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
    date_first_quote = as.Date(c("1995-03-15", "1995-03-15", "1995-03-15", "1999-05-15", NA, "1995-04-15"))
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
  expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = ticker_changes
    ),
    "Mismatch in tickers_first_quote between common tickers in old and new catalogs.For relistings, use 'old_ticker'_n, where n is relisting_number"
  )
})

test_that("update_tickers_catalog fails when IPO ticker first quote is < current date", {
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
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-05-10", "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", "2001-05-15"))
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
    expect_error(
    update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = ticker_changes
    ),
    "tickers_first_quote of IPOs in new_tickers_catalog should be either >= old_current_date or NA."
    )
  )
})

test_that("update_tickers_catalog fails when tickers_last_quote in new catalog is < than old_tickers", {
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
    c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3"),
    as.Date(c("2001-06-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "BRAV3", "ENAT3", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-05-18", "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "BRAV3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3"),
    old_tickers = "RRRP3",
    change_date = as.Date("2001-06-15")
  )

  # Update catalog
  first_update <- update_tickers_catalog(
      old_tickers_catalog = old_tickers_catalog,
      new_tickers_catalog = new_tickers_catalog,
      ticker_changes = ticker_changes
  )

  # A new batch of data arrives
  another_raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(1, 2, 3, 4, NA, NA), nrow = 6, ncol = 1),
      matrix(c(4, NA, 6, 7, NA, NA), nrow = 6, ncol = 1)
    ),
    c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3"),
    as.Date(c("2001-07-15")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "CALM3", "ENAT3", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "2001-07-14", "1999-05-15", "2001-05-18", "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = another_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("CALM3"),
    old_tickers = "BRAV3",
    change_date = as.Date("2001-07-14")
  )

  expect_error(
  update_tickers_catalog(
    old_tickers_catalog = first_update,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  ),
  "tickers_first_quote in new_tickers_catalog should be <= tickers_first_quote in old_tickers_catalog for new tickers."
  )


})

#distinct obj

test_that("update_tickers_catalog fails when objects do not match update requirements", {
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
    as.Date(c("2001-06-12")),
    c("Alpha", "Beta")
  )

  date_first_quote <- data.frame(
    tickers = c("PETR4", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "VALE3"),
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-06-05", "1995-04-15"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    date_last_quote = as.Date(c("2001-06-12", "2001-06-12", "2001-06-12", "2001-06-12", "2001-05-12", "2001-06-12"))
  )

  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  # Create ticker_change
  ticker_changes <- NULL

  # Update catalog
    expect_error(
      update_tickers_catalog(
        old_tickers_catalog = old_tickers_catalog,
        new_tickers_catalog = new_tickers_catalog,
        ticker_changes = ticker_changes
      ),
      "current_date in new_tickers_catalog should be equal to current_date in old_tickers_catalog plus 1"
    )


    # A new batch of data arrives
    new_raw_features_m_df <- create_meta_dataframe(
      data = list(
        matrix(c(1, 2, 3, 4, NA, NA), nrow = 6, ncol = 1),
        matrix(c(4, NA, 6, 7, NA, NA), nrow = 6, ncol = 1)
      ),
      tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
      dates = as.Date(c("2001-06-15")),
      features_names = c("Alpha", "Beta"),
      meta_dataframe_name = "random"
    )

    date_first_quote <- data.frame(
      tickers = c("PETR4", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "VALE3"),
      date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-06-05", "1995-04-15"))
    )

    date_last_quote <- data.frame(
      tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
      date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", "2001-06-15"))
    )

    new_tickers_catalog <- create_tickers_catalog(
      raw_features_m_df = new_raw_features_m_df,
      date_first_quote = date_first_quote,
      date_last_quote = date_last_quote
    )

    # Create ticker_change
    ticker_changes <- NULL

    # Update catalog
    expect_error(
      update_tickers_catalog(
        old_tickers_catalog = old_tickers_catalog,
        new_tickers_catalog = new_tickers_catalog,
        ticker_changes = ticker_changes
      ),
      "meta_dataframe_name in new_tickers_catalog should contain meta_dataframe_name in old_tickers_catalog."
    )


    # A new batch of data arrives
    new_raw_features_m_df <- create_meta_dataframe(
      data = list(
        matrix(c(1, 2, 3, 4, NA, NA), nrow = 6, ncol = 1),
        matrix(c(4, NA, 6, 7, NA, NA), nrow = 6, ncol = 1)
      ),
      tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
      dates = as.Date(c("2001-06-15")),
      features_names = c("Alpha", "Beta")
    )

    date_first_quote <- data.frame(
      tickers = c("PETR4", "ABEV3", "RRRP3", "ENAT3", "CAFE3", "VALE3"),
      date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "1999-03-15", "1999-05-15", "2001-06-05", "1995-04-15"))
    )

    date_last_quote <- data.frame(
      tickers = c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
      date_last_quote = as.Date(c("2001-06-15", "2001-06-15", "2001-06-15", "2001-06-15", "2001-05-15", "2001-06-15"))
    )

    new_tickers_catalog <- create_tickers_catalog(
      raw_features_m_df = new_raw_features_m_df,
      date_first_quote = date_first_quote,
      date_last_quote = date_last_quote,
      n_days_tolerance = 2
    )

    # Create ticker_change
    ticker_changes <- NULL

    # Update catalog
    expect_warning(
      update_tickers_catalog(
        old_tickers_catalog = old_tickers_catalog,
        new_tickers_catalog = new_tickers_catalog,
        ticker_changes = ticker_changes
      ),
      "n_days_tolerance has changed from old_tickers_catalog and new_tickers_catalog."
    )


})

