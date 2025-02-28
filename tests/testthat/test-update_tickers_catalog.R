test_that("update_tickers_catalog works for a ticker change scenario (and a change in order between date_first_quote and date_last_quote)", {

  # Create a catalog
  old_raw_features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 1, 2, NA, 3, NA, 9, 4, -1, 0, 3, NA, 1, NA, -4, 3, NA, NA), nrow = 6, ncol = 3),
      matrix(c(0, -1, 2, NA, 4, NA, 19, 5, 1, 0, 30, NA, 1, -1, NA, NA, NA, NA), nrow = 6, ncol = 3)
    ),
    c("PETR4", "VALE3", "ABEV3", "RRRP3", "ENAT3", "CAFE3"),
    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    c("Alpha", "Beta"),

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

  #A new batch of data arrives
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
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "2001-06-02", "1999-05-15", NA, "1995-04-15"))
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

  #Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3"),
    old_tickers = c("RRRP3"),
    change_date = as.Date("2001-06-02")
  )

  #Update catalog
  results <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Check that perm_id for RRRP3 and BRAV3 match
  expect_equal(
    results@perm_id["BRAV3"] %>% unname(),
    old_tickers_catalog@perm_id["RRRP3"] %>% unname()
  )
  expect_equal(
    results@catalog[3,"perm_id"], old_tickers_catalog@catalog[3, "perm_id"]
  )
  #Check that remaining perm_ids are the same
  expect_equal(
  results@perm_id[c("VALE3", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname(),
  old_tickers_catalog@perm_id[c("VALE3", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname()
  )
  expect_equal(
    results@catalog[which(results@catalog$tickers %in% c("VALE3", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")),"perm_id"],
    old_tickers_catalog@catalog[which(old_tickers_catalog@catalog$tickers %in% c("VALE3", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")), "perm_id"]
  )
  #Check that ENAT3 was listed and now it isn't anymore
  expect_false("ENAT3" %in% results@listed)
  expect_true("ENAT3" %in% results@delisted)
  expect_true("ENAT3" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[6, "listed"])
  expect_true(old_tickers_catalog@catalog[5, "listed"])

  #Check that BRAV3 is now listed
  expect_true("BRAV3" %in% results@listed)
  expect_true(results@catalog[3, "listed"])

  #Check that RRRP3 date of last quote is now equal to date change
  expect_equal(results@catalog[4, "date_last_quote"], ticker_changes$change_date)

  # Test: ticker change history is updated
  expect_true(nrow(results@ticker_change_history) >= 1)

  #Check that untraded keeps untraded
  expect_equal(results@untraded, old_tickers_catalog@untraded)
  #Check that RRRP3 and ENAT3 are now delisted
  expect_true(all(c("RRRP3", "ENAT3") %in% results@delisted))
  #Check listed
  expect_true(all(c("BRAV3", "PETR4", "VALE3", "ABEV3") %in% results@listed))
  #Check untraded
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

  #A new batch of data arrives
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
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "2001-06-02", "1999-05-15", NA, "1995-04-15", "2001-06-09", "2001-05-18"))
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

  #Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  #Update catalog
  results <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Check that perm_id for RRRP3 and BRAV3 match
  expect_equal(
    results@perm_id["BRAV3"] %>% unname(),
    old_tickers_catalog@perm_id["RRRP3"] %>% unname()
  )
  expect_equal(results@catalog[6,"perm_id"], old_tickers_catalog@catalog[4, "perm_id"])
  expect_equal(results@catalog[6,"perm_id"], results@catalog[7,"perm_id"])

  #Check that perm_id for TRPL4 and ISAE4 match
  expect_equal(
    results@perm_id["ISAE4"] %>% unname(),
    old_tickers_catalog@perm_id["TRPL4"] %>% unname()
  )
  expect_equal(results@catalog[1,"perm_id"], old_tickers_catalog@catalog[1, "perm_id"])
  expect_equal(results@catalog[1,"perm_id"], results@catalog[2,"perm_id"])

  #Check that CMED3 is a new listed ticker
  expect_true("CMED3" %in% results@listed)
  expect_false("CMED3" %in% old_tickers_catalog@catalog$tickers)


  #Check that remaining perm_ids are the same
  expect_equal(
    results@perm_id[c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname(),
    old_tickers_catalog@perm_id[c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")] %>% unname()
  )
  expect_equal(
    results@catalog[which(results@catalog$tickers %in% c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")),"perm_id"],
    old_tickers_catalog@catalog[which(old_tickers_catalog@catalog$tickers %in% c("VALE3", "TRPL4", "PETR4", "RRRP3", "CAFE3", "ENAT3", "ABEV3")), "perm_id"]
  )
  #Check that ENAT3 and TRPL4 were listed and now it isn't anymore
  expect_false("ENAT3" %in% results@listed)
  expect_true("ENAT3" %in% results@delisted)
  expect_true("ENAT3" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[9, "listed"])
  expect_true(results@catalog[9, "delisted"])
  expect_true(old_tickers_catalog@catalog[6, "listed"])

  expect_false("TRPL4" %in% results@listed)
  expect_true("TRPL4" %in% results@delisted)
  expect_true("TRPL4" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[2, "listed"])
  expect_true(results@catalog[2, "delisted"])
  expect_true(old_tickers_catalog@catalog[1, "listed"])

  #Check that BRAV3, CMED3 and ISAE4 are now listed
  expect_true("BRAV3" %in% results@listed)
  expect_true(results@catalog[6, "listed"])
  expect_true("CMED3" %in% results@listed)
  expect_true(results@catalog[3, "listed"])
  expect_true("ISAE4" %in% results@listed)
  expect_true(results@catalog[1, "listed"])

  #Check that RRRP3 and TRPL4 date of last quote is now equal to date change
  expect_equal(results@catalog[2, "date_last_quote"], ticker_changes$change_date[2])
  expect_equal(results@catalog[7, "date_last_quote"], ticker_changes$change_date[1])

  # Test: ticker change history is updated
  expect_true(nrow(results@ticker_change_history) >= 1)

  #Check that untraded keeps untraded
  expect_equal(results@untraded, old_tickers_catalog@untraded)

  #Check delisted
  expect_true(all(c("TRPL4", "ENAT3", "RRRP3") %in% results@delisted))
  #Check listed
  expect_true(all(c("BRAV3", "CMED3", "ISAE4", "PETR4", "VALE3", "ABEV3") %in% results@listed))
  #Check untraded
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

  #A new batch of data arrives
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
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "2001-06-02", "1999-05-15", NA, "1995-04-15", "2001-06-09", "2001-05-18"))
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

  #Create ticker_change
  ticker_changes <- data.frame(
    new_tickers = c("BRAV3", "ISAE4"),
    old_tickers = c("RRRP3", "TRPL4"),
    change_date = as.Date(c("2001-06-02", "2001-06-09"))
  )

  #Update catalog
  first_update <- update_tickers_catalog(
    old_tickers_catalog = old_tickers_catalog,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = ticker_changes
  )

  #Another batch of data arrives and BRAV3 is now CALM3
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
    date_first_quote = as.Date(c("1995-05-15", "1995-03-15", "2001-07-09", "1999-05-15", NA, "1995-04-15", "2001-06-09", "2001-06-09"))
  )

  date_last_quote <- data.frame(
    tickers = c("PETR4", "VALE3", "ABEV3", "CALM3", "ENAT3", "CAFE3", "ISAE4", "DMED3"),
    date_last_quote = as.Date(c("2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15", "2001-05-15", NA, "2001-07-15", "2001-07-14"))
  )

  #Another one
  new_tickers_catalog <- create_tickers_catalog(
    raw_features_m_df = another_new_raw_features_m_df,
    date_first_quote = date_first_quote,
    date_last_quote = date_last_quote
  )

  #Create a second ticker_change
  new_ticker_changes <- data.frame(
    new_tickers = c("CALM3", "DMED3"),
    old_tickers = c("BRAV3", "CMED3"),
    change_date = as.Date(c("2001-07-09", "2001-06-09"))
  )

  #Update catalog
  results <- update_tickers_catalog(
    old_tickers_catalog = first_update,
    new_tickers_catalog = new_tickers_catalog,
    ticker_changes = new_ticker_changes
  )

  #Check that perm_id for RRRP3, BRAV3 and CALM3 match
  expect_equal(
    results@perm_id["BRAV3"] %>% unname(),
    old_tickers_catalog@perm_id["RRRP3"] %>% unname()
  )
  expect_equal(
    results@perm_id["CALM3"] %>% unname(),
    old_tickers_catalog@perm_id["RRRP3"] %>% unname()
  )

  expect_equal(results@catalog[7,"perm_id"], old_tickers_catalog@catalog[4, "perm_id"])
  expect_equal(results@catalog[8,"perm_id"], old_tickers_catalog@catalog[4, "perm_id"])

  expect_equal(results@catalog[8,"perm_id"], results@catalog[7,"perm_id"])
  expect_equal(results@catalog[8,"perm_id"], results@catalog[9,"perm_id"])

  #Check that perm_id for TRPL4 and ISAE4 match
  expect_equal(
    results@perm_id["ISAE4"] %>% unname(),
    old_tickers_catalog@perm_id["TRPL4"] %>% unname()
  )
  expect_equal(results@catalog[1,"perm_id"], old_tickers_catalog@catalog[1, "perm_id"])
  expect_equal(results@catalog[1,"perm_id"], results@catalog[2,"perm_id"])

  #Check that perm_id for CMED3 and DMED3 match
  expect_equal(
    results@perm_id["DMED3"] %>% unname(),
    first_update@perm_id["CMED3"] %>% unname()
  )
  expect_equal(results@catalog[3,"perm_id"], first_update@catalog[3, "perm_id"])
  expect_equal(results@catalog[3,"perm_id"], results@catalog[4,"perm_id"])

  #Check that remaining perm_ids are the same
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

  #Check that ENAT3, TRPL4 and CMED3 were listed and now aren't anymore
  expect_false("ENAT3" %in% results@listed)
  expect_true("ENAT3" %in% results@delisted)
  expect_true("ENAT3" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[9, "listed"])
  expect_true(results@catalog[9, "delisted"])
  expect_true(old_tickers_catalog@catalog[6, "listed"])

  expect_false("TRPL4" %in% results@listed)
  expect_true("TRPL4" %in% results@delisted)
  expect_true("TRPL4" %in% old_tickers_catalog@listed)
  expect_false(results@catalog[2, "listed"])
  expect_true(results@catalog[2, "delisted"])
  expect_true(old_tickers_catalog@catalog[1, "listed"])

  expect_false("CMED3" %in% results@listed)
  expect_true("CMED3" %in% results@delisted)
  expect_true("CMED3" %in% first_update@listed)
  expect_false(results@catalog[4, "listed"])
  expect_true(results@catalog[4, "delisted"])
  expect_true(first_update@catalog[3, "listed"])


  #Check that CALM3, DMED3 and ISAE4 are now listed
  expect_true("CALM3" %in% results@listed)
  expect_true(results@catalog[7, "listed"])
  expect_true("DMED3" %in% results@listed)
  expect_true(results@catalog[3, "listed"])
  expect_true("ISAE4" %in% results@listed)
  expect_true(results@catalog[1, "listed"])

  #Check that RRRP3, TRPL4, CMED3 and BRAV3 date of last quote is now equal to date change
  expect_equal(results@catalog[2, "date_last_quote"], ticker_changes$change_date[2])
  expect_equal(results@catalog[9, "date_last_quote"], ticker_changes$change_date[1])
  expect_equal(results@catalog[8, "date_last_quote"], new_ticker_changes$change_date[1])
  expect_equal(results@catalog[4, "date_last_quote"], new_ticker_changes$change_date[2])

  # Test: ticker change history is updated
  expect_equal(nrow(results@ticker_change_history),  4)

  #Check that untraded keeps untraded
  expect_equal(results@untraded, old_tickers_catalog@untraded)

  #Check delisted
  expect_true(all(c("TRPL4", "ENAT3", "RRRP3", "CMED3", "BRAV3") %in% results@delisted))
  #Check listed
  expect_true(all(c("CALM3", "DMED3", "ISAE4", "PETR4", "VALE3", "ABEV3") %in% results@listed))
  #Check untraded
  expect_true(all(c("CAFE3") %in% results@untraded))


})

#TEST FOR UNTRADED CHANGE
#TEST FOR TICKER_CHANGE NULL

test_that("update_tickers_catalog throws an error when ticker_changes is not right", {





})


test_that("update_tickers_catalog errors if ticker_changes is missing required columns", {
  # Create minimal old and new catalogs (with one ticker) for testing
  old_raw_features_m_df <- create_meta_dataframe(
    list(matrix(1, nrow = 1)),
    "AAA",
    as.Date("2001-05-15"),
    "F1"
  )
  old_date_first_quote <- data.frame(
    tickers = "AAA",
    date_first_quote = as.Date("2000-01-01")
  )
  old_date_last_quote <- data.frame(
    tickers = "AAA",
    date_last_quote = as.Date("2001-05-15")
  )
  old_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = old_date_first_quote,
    date_last_quote = old_date_last_quote
  )

  new_raw_features_m_df <- create_meta_dataframe(
    list(matrix(1, nrow = 1)),
    "AAA",
    as.Date("2001-06-15"),
    "F1"
  )
  new_date_first_quote <- data.frame(
    tickers = "AAA",
    date_first_quote = as.Date("2000-01-01")
  )
  new_date_last_quote <- data.frame(
    tickers = "AAA",
    date_last_quote = as.Date("2001-06-15")
  )
  new_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = new_date_first_quote,
    date_last_quote = new_date_last_quote
  )

  # ticker_changes missing required columns (e.g. only new_tickers provided)
  bad_ticker_changes <- data.frame(new_tickers = "AAA")

  expect_error(
    update_tickers_catalog(old_catalog, new_catalog, bad_ticker_changes, n_update = 1),
    "ticker_changes must contain columns: 'new_tickers', 'old_tickers', and 'change_date'."
  )
})

test_that("update_tickers_catalog errors if new_tickers in ticker_changes do not match new catalog tickers", {
  # Create an old catalog with one ticker "AAA"
  old_raw_features_m_df <- create_meta_dataframe(
    list(matrix(1, nrow = 1)),
    "AAA",
    as.Date("2001-05-15"),
    "F1"
  )
  old_date_first_quote <- data.frame(
    tickers = "AAA",
    date_first_quote = as.Date("2000-01-01")
  )
  old_date_last_quote <- data.frame(
    tickers = "AAA",
    date_last_quote = as.Date("2001-05-15")
  )
  old_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = old_date_first_quote,
    date_last_quote = old_date_last_quote
  )

  # New catalog has ticker "AAA_new" (which is not mapped in ticker_changes)
  new_raw_features_m_df <- create_meta_dataframe(
    list(matrix(1, nrow = 1)),
    "AAA_new",
    as.Date("2001-06-15"),
    "F1"
  )
  new_date_first_quote <- data.frame(
    tickers = "AAA_new",
    date_first_quote = as.Date("2000-01-01")
  )
  new_date_last_quote <- data.frame(
    tickers = "AAA_new",
    date_last_quote = as.Date("2001-06-15")
  )
  new_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = new_date_first_quote,
    date_last_quote = new_date_last_quote
  )

  # ticker_changes maps "BBB" which is not in new_catalog
  ticker_changes <- data.frame(
    new_tickers = "BBB",
    old_tickers = "AAA",
    change_date = as.Date("2001-06-01")
  )

  expect_error(
    update_tickers_catalog(old_catalog, new_catalog, ticker_changes, n_update = 1),
    "Mismatch between new tickers in ticker_changes and new tickers present in raw_features_m_df"
  )
})

test_that("update_tickers_catalog errors if new current_date is not old current_date + n_update", {
  # Create old catalog with current_date = "2001-05-15"
  old_raw_features_m_df <- create_meta_dataframe(
    list(matrix(1, nrow = 1)),
    "AAA",
    as.Date("2001-05-15"),
    "F1"
  )
  old_date_first_quote <- data.frame(
    tickers = "AAA",
    date_first_quote = as.Date("2000-01-01")
  )
  old_date_last_quote <- data.frame(
    tickers = "AAA",
    date_last_quote = as.Date("2001-05-15")
  )
  old_catalog <- create_tickers_catalog(
    raw_features_m_df = old_raw_features_m_df,
    date_first_quote = old_date_first_quote,
    date_last_quote = old_date_last_quote
  )

  # New catalog with current_date not equal to old_catalog@current_date + 1 month.
  new_raw_features_m_df <- create_meta_dataframe(
    list(matrix(1, nrow = 1)),
    "AAA",
    as.Date("2001-07-15"),  # Should be "2001-06-15" if n_update = 1
    "F1"
  )
  new_date_first_quote <- data.frame(
    tickers = "AAA",
    date_first_quote = as.Date("2000-01-01")
  )
  new_date_last_quote <- data.frame(
    tickers = "AAA",
    date_last_quote = as.Date("2001-07-15")
  )
  new_catalog <- create_tickers_catalog(
    raw_features_m_df = new_raw_features_m_df,
    date_first_quote = new_date_first_quote,
    date_last_quote = new_date_last_quote
  )

  # No ticker changes needed
  ticker_changes <- data.frame(
    new_tickers = character(0),
    old_tickers = character(0),
    change_date = as.Date(character(0))
  )

  expect_error(
    update_tickers_catalog(old_catalog, new_catalog, ticker_changes, n_update = 1),
    "current_date in new_tickers_catalog should be equal to current_date in old_tickers_catalog + n_update."
  )
})
