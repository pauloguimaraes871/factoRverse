test_that("apply_turnover_cap_rule adequately classifies stocks based on buffer_zone - only 1 stock", {

  #Create cutoff
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_df
  liquidity_m_df <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create stock_universe_m_d_ref
  stock_universe_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    exp_ret_score = c(0.5, 0.25, 0.4)
  )

  #Create old port weights test
  updated_port_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-04-15", "Stock B-2020-04-15", "Stock C-2020-04-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-04-15", "2020-04-15", "2020-04-15"), format = "%Y-%m-%d"),
    bop_port_weights = c(0, 0.25, 0)
  )

  expected_results <- stock_universe_m_d_ref %>% dplyr::select(id, tickers, dates)
  eligibility_quantile_range <- c(0.75, 1)
  quantile_range_buffer <- 0.09
  lower_bound_quantile_buffer <- quantile(stock_universe_m_d_ref$exp_ret_score, 0.66)

  expected_results$is_in_buffered_quantile_range <- c(1,0,0)
  expected_results$bop_port_weights <- c(0,0.25,0)
  expected_results$was_in_old_portfolio <- c(0, 1, 0)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_m_df = liquidity_m_df, liquidity_floor_rule = "mid_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_turnover_cap_rule <- c(0, 1, 1)
  expected_results$turnover_cap_rule <- c(0,0,0)

  expect_equal(
    apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                            eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = quantile_range_buffer,
                            updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df,
                            liquidity_floor_cutoffs = liquidity_floor_cutoffs, turnover_cap_rule = "small_caps"),
    expected_results)


})

test_that("apply_turnover_cap_rule adequately classifies stocks based on buffer_zone - 2 stocks", {

  #Create cutoff
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_df
  liquidity_m_df <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create stock_universe_m_d_ref
  stock_universe_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    exp_ret_score = c(0.8, 0.8, -0.4)
  )


  #Create old port weights test
  updated_port_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-04-15", "Stock B-2020-04-15", "Stock C-2020-04-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-04-15", "2020-04-15", "2020-04-15"), format = "%Y-%m-%d"),
    bop_port_weights = c(0.02, 0.25, 0)
  )

  expected_results <- stock_universe_m_d_ref %>% dplyr::select(id, tickers, dates)
  eligibility_quantile_range <- c(0.75, 1)
  quantile_range_buffer <- 0.09
  lower_bound_quantile_buffer <- quantile(stock_universe_m_d_ref$exp_ret_score, 0.66)

  expected_results$is_in_buffered_quantile_range <- c(1,1,0)
  expected_results$bop_port_weights <- c(0.02,0.25,0)
  expected_results$was_in_old_portfolio <- c(1, 1, 0)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_m_df = liquidity_m_df, liquidity_floor_rule = "small_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_turnover_cap_rule <- c(0, 1, 1)
  expected_results$turnover_cap_rule <- c(0,1,0)

  expect_equal(
    apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                            eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = quantile_range_buffer,
                            updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df,
                            liquidity_floor_cutoffs = liquidity_floor_cutoffs, turnover_cap_rule = "small_caps"),
    expected_results)


})

test_that("apply_turnover_cap_rule adequately classifies stocks when there are delisted stocks in updated_port_weights_m_lstd_ref", {

  #Create cutoff
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_df
  liquidity_m_df <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create stock_universe_m_d_ref
  stock_universe_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    exp_ret_score = c(0.8, 0.8, -0.4)
  )

  #Create old port weights test
  updated_port_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-04-15", "Stock A1-2020-04-15", "Stock B-2020-04-15", "Stock C-2020-04-15"),
    tickers = c("Stock A","Stock A1", "Stock B", "Stock C"),
    dates = as.Date(c("2020-04-15", "2020-04-15", "2020-04-15", "2020-04-15"), format = "%Y-%m-%d"),
    bop_port_weights = c(0.02, 0.15, 0.25, 0)
  )

  expected_results <- stock_universe_m_d_ref %>% dplyr::select(id, tickers, dates)
  eligibility_quantile_range <- c(0.75, 1)
  quantile_range_buffer <- 0.09
  lower_bound_quantile_buffer <- quantile(stock_universe_m_d_ref$exp_ret_score, 0.66)

  expected_results$is_in_buffered_quantile_range <- c(1,1,0)
  expected_results$bop_port_weights <- c(0.02,0.25,0)
  expected_results$was_in_old_portfolio <- c(1, 1, 0)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_m_df = liquidity_m_df, liquidity_floor_rule = "small_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_turnover_cap_rule <- c(0, 1, 1)
  expected_results$turnover_cap_rule <- c(0,1,0)

  expect_equal(
    apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                            eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = quantile_range_buffer,
                            updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df,
                            liquidity_floor_cutoffs = liquidity_floor_cutoffs, turnover_cap_rule = "small_caps"),
    expected_results)


})

test_that("apply_turnover_cap_rule adequately classifies stocks when there are new stocks in stock_universe_m_d_ref", {

  #Create cutoff
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_df
  liquidity_m_df <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create stock_universe_m_d_ref
  stock_universe_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    exp_ret_score = c(0.8, 0.8, 0.4)
  )

  #Create old port weights test
  updated_port_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-04-15", "Stock C-2020-04-15"),
    tickers = c("Stock A", "Stock C"),
    dates = as.Date(c("2020-04-15", "2020-04-15"), format = "%Y-%m-%d"),
    bop_port_weights = c(0.02, 0.15)
  )

  expected_results <- stock_universe_m_d_ref %>% dplyr::select(id, tickers, dates)
  eligibility_quantile_range <- c(0.75, 1)
  quantile_range_buffer <- 0.09
  lower_bound_quantile_buffer <- quantile(stock_universe_m_d_ref$exp_ret_score, 0.66)

  expected_results$is_in_buffered_quantile_range <- c(1,1,0)
  expected_results$bop_port_weights <- c(0.02,0,0.15)
  expected_results$was_in_old_portfolio <- c(1, 0, 1)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_m_df = liquidity_m_df, liquidity_floor_rule = "small_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_turnover_cap_rule <- c(0, 1, 1)
  expected_results$turnover_cap_rule <- c(0,0,0)

  expect_equal(
    apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                            eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = quantile_range_buffer,
                            updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df,
                            liquidity_floor_cutoffs = liquidity_floor_cutoffs, turnover_cap_rule = "small_caps"),
    expected_results)


})

test_that("apply_turnover_cap_rule adequately classifies stocks based on buffer_zone when there are less classifications", {

  #Create cutoff
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("mid_caps", "large_caps"),
    mean_volfin_3m = c(23000, 100000),
    presence = c(100, 100)
  )

  #Create liquidity_m_df
  liquidity_m_df <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create stock_universe_m_d_ref
  stock_universe_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    exp_ret_score = c(0.8, 0.8, -0.4)
  )

  #Create old port weights test
  updated_port_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-04-15", "Stock B-2020-04-15", "Stock C-2020-04-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-04-15", "2020-04-15", "2020-04-15"), format = "%Y-%m-%d"),
    bop_port_weights = c(0.02, 0.25, 0)
  )

  expected_results <- stock_universe_m_d_ref %>% dplyr::select(id, tickers, dates)
  eligibility_quantile_range <- c(0.75, 1)
  quantile_range_buffer <- 0.09
  lower_bound_quantile_buffer <- quantile(stock_universe_m_d_ref$exp_ret_score, 0.66)

  expected_results$is_in_buffered_quantile_range <- c(1,1,0)
  expected_results$bop_port_weights <- c(0.02,0.25,0)
  expected_results$was_in_old_portfolio <- c(1, 1, 0)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_m_df = liquidity_m_df, liquidity_floor_rule = "mid_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_turnover_cap_rule <- c(0, 0, 1)
  expected_results$turnover_cap_rule <- c(0,0,0)

  expect_equal(
    apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                            eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = quantile_range_buffer,
                            updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df,
                            liquidity_floor_cutoffs = liquidity_floor_cutoffs, turnover_cap_rule = "mid_caps"),
    expected_results)


})

test_that("apply_turnover_cap_rule works for a lower quantile_range that is not in edge", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Create stock_universe_m_d_ref
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == as.Date("2001-04-15"))
  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, Alpha) %>%
    dplyr::rename(exp_ret_score = Alpha) %>%
    dplyr::mutate(exp_ret_score = signal_transform(exp_ret_score))

  #Create old port weights
  updated_port_weights_m_lstd_ref <- signals_m_df %>% dplyr::filter(dates == as.Date("2001-03-15")) %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(bop_port_weights = c(0.34, 0.33, 0.33))

  #Expected results
  expected_results <- stock_universe_m_d_ref %>% dplyr::select(id, tickers, dates)
  eligibility_quantile_range <- c(0.50, 0.75)
  quantile_range_buffer <- 0.20
  lower_bound_quantile_buffer <- quantile(stock_universe_m_d_ref$exp_ret_score, 0.30)
  upper_bound_quantile_buffer <- quantile(stock_universe_m_d_ref$exp_ret_score, 0.95)

  expected_results$is_in_buffered_quantile_range <- c(0,1,0,0,1)
  expected_results$bop_port_weights <- c(0,0.34,0,0.33,0.33)
  expected_results$was_in_old_portfolio <- c(0,1,0,1,1)

  liquidity_classification <- classify_stock_liquidity( #Apply liquidity classification
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, liquidity_m_df = liquidity_m_df %>% dplyr::filter(dates == "2001-04-15"), liquidity_floor_rule = "mid_caps",
    apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = FALSE)

  expected_results$does_liquidity_meets_turnover_cap_rule <- c(0, 0, 1, 1, 1)
  expected_results$turnover_cap_rule <- c(0,0,0,0,1)

  expect_equal(
    apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                            eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = quantile_range_buffer,
                            updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_df %>% dplyr::filter(dates == "2001-04-15"),
                            liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, turnover_cap_rule = "micro_caps"),
    expected_results)


})




