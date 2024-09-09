test_that("apply_buffer_rule adequately classifies stocks based on buffer_zone", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create signals_m_d_ref_test
  signals_m_d_ref_test <- data.frame(
    tickers = c("Stock A", "Stock B", "Stock C"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    final_signal = c(0.5, 0.25, 0.4)
  )

  #Create old port weights test
  portfolio_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    old_portfolio_weights = c(0, 0.25, 0)
  )

  expected_results <- signals_m_d_ref_test %>% dplyr::select(tickers)
  top_quantile_buffer <- quantile(signals_m_d_ref_test$final_signal, 0.66)
  expected_results$is_in_top_quantile_buffer <- c(1,0,0)
  expected_results$old_portfolio_weights <- c(0,0.25,0)
  expected_results$was_in_old_portfolio <- c(0, 1, 0)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
                              liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test, liquidity_floor_rule = "mid_caps",
                              apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_buffer_rule <- c(0, 1, 1)
  expected_results$buffer_rule <- c(0,0,0)

  expect_equal(
  apply_buffer_rule(signals_m_d_ref = signals_m_d_ref_test, top_assets_quantile_buffer = 0.66,  portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df_test,
                    liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, buffer_rule = "small_caps"),
  expected_results)


})

test_that("apply_buffer_rule adequately classifies stocks based on buffer_zone", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create signals_m_d_ref_test
  signals_m_d_ref_test <- data.frame(
    tickers = c("Stock A", "Stock B", "Stock C"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    final_signal = c(0.8, 0.8, -0.4)
  )

  #Create old port weights test
  portfolio_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    old_portfolio_weights = c(0.02, 0.25, 0)
  )

  expected_results <- signals_m_d_ref_test %>% dplyr::select(tickers)
  top_quantile_buffer <- quantile(signals_m_d_ref_test$final_signal, 0.66)
  expected_results$is_in_top_quantile_buffer <- c(1,1,0)
  expected_results$old_portfolio_weights <- c(0.02,0.25,0)
  expected_results$was_in_old_portfolio <- c(1, 1, 0)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test, liquidity_floor_rule = "small_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_buffer_rule <- c(0, 1, 1)
  expected_results$buffer_rule <- c(0,1,0)

  expect_equal(
    apply_buffer_rule(signals_m_d_ref = signals_m_d_ref_test, top_assets_quantile_buffer = 0.66,  portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df_test,
                      liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, buffer_rule = "small_caps"),
    expected_results)


})

test_that("apply_buffer_rule adequately classifies stocks when there are delisted stocks in portfolio_weights_m_lstd_ref", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create signals_m_d_ref_test
  signals_m_d_ref_test <- data.frame(
    tickers = c("Stock A", "Stock B", "Stock C"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    final_signal = c(0.8, 0.8, -0.4)
  )

  #Create old port weights test
  portfolio_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-04-15", "Stock A1-2020-04-15", "Stock B-2020-04-15", "Stock C-2020-04-15"),
    tickers = c("Stock A","Stock A1", "Stock B", "Stock C"),
    dates = as.Date(c("2020-04-15", "2020-04-15", "2020-04-15", "2020-04-15"), format = "%Y-%m-%d"),
    old_portfolio_weights = c(0.02, 0.15, 0.25, 0)
  )

  expected_results <- signals_m_d_ref_test %>% dplyr::select(tickers)
  top_quantile_buffer <- quantile(signals_m_d_ref_test$final_signal, 0.66)
  expected_results$is_in_top_quantile_buffer <- c(1,1,0)
  expected_results$old_portfolio_weights <- c(0.02,0.25,0)
  expected_results$was_in_old_portfolio <- c(1, 1, 0)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test, liquidity_floor_rule = "small_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_buffer_rule <- c(0, 1, 1)
  expected_results$buffer_rule <- c(0,1,0)

  expect_equal(
    apply_buffer_rule(signals_m_d_ref = signals_m_d_ref_test, top_assets_quantile_buffer = 0.66,  portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df_test,
                      liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, buffer_rule = "small_caps"),
    expected_results)


})

test_that("apply_buffer_rule adequately classifies stocks when there are new stocks in signals_m_d_ref", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create signals_m_d_ref_test
  signals_m_d_ref_test <- data.frame(
    tickers = c("Stock A", "Stock B", "Stock C"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    final_signal = c(0.8, 0.8, 0.4)
  )

  #Create old port weights test
  portfolio_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-04-15", "Stock C-2020-04-15"),
    tickers = c("Stock A", "Stock C"),
    dates = as.Date(c("2020-04-15", "2020-04-15"), format = "%Y-%m-%d"),
    old_portfolio_weights = c(0.02, 0.15)
  )

  expected_results <- signals_m_d_ref_test %>% dplyr::select(tickers)
  top_quantile_buffer <- quantile(signals_m_d_ref_test$final_signal, 0.66)
  expected_results$is_in_top_quantile_buffer <- c(1,1,0)
  expected_results$old_portfolio_weights <- c(0.02,NA,0.15)
  expected_results$was_in_old_portfolio <- c(1, 0, 1)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test, liquidity_floor_rule = "small_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_buffer_rule <- c(0, 1, 1)
  expected_results$buffer_rule <- c(0,0,0)

  expect_equal(
    apply_buffer_rule(signals_m_d_ref = signals_m_d_ref_test, top_assets_quantile_buffer = 0.66,  portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df_test,
                      liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, buffer_rule = "small_caps"),
    expected_results)


})

test_that("apply_buffer_rule adequately classifies stocks based on buffer_zone when there are less classifications", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    mid_caps = c(mean_volfin_3m = 23000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create signals_m_d_ref_test
  signals_m_d_ref_test <- data.frame(
    tickers = c("Stock A", "Stock B", "Stock C"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    final_signal = c(0.8, 0.8, -0.4)
  )

  #Create old port weights test
  portfolio_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    old_portfolio_weights = c(0.02, 0.25, 0)
  )

  expected_results <- signals_m_d_ref_test %>% select(tickers)
  top_quantile_buffer <- quantile(signals_m_d_ref_test$final_signal, 0.66)
  expected_results$is_in_top_quantile_buffer <- c(1,1,0)
  expected_results$old_portfolio_weights <- c(0.02,0.25,0)
  expected_results$was_in_old_portfolio <- c(1, 1, 0)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test, liquidity_floor_rule = "mid_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_buffer_rule <- c(0, 0, 1)
  expected_results$buffer_rule <- c(0,0,0)

  expect_equal(
    apply_buffer_rule(signals_m_d_ref = signals_m_d_ref_test, top_assets_quantile_buffer = 0.66,  portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df_test,
                      liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, buffer_rule = "mid_caps"),
    expected_results)


})




