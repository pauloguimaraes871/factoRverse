test_that("check_inputs_port_backtest throws an error when chosen_score_metric_and_position is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Wrong chosen_score_metric_and_position
  wrong_chosen_score_metric_and_position <- c(roe_3m = "long", vol_36m = "short")

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = wrong_chosen_score_metric_and_position,
    min_eligible_assets_fallback = NULL,
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    eligibility_quantile_range = c(0.5, 0.75),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "chosen_score_metric_and_position should be a single element."
  )

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Wrong chosen_score_metric_and_position
  wrong_chosen_score_metric_and_position <- c(low_vol_36m = "long")

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = wrong_chosen_score_metric_and_position,
    min_eligible_assets_fallback = NULL,
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    eligibility_quantile_range = c(0.5, 0.75),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "chosen_score_metric_and_position should not contain 'low_'."
  )

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Wrong chosen_score_metric_and_position not present in signals_m_df
  wrong_chosen_score_metric_and_position <- c(not_in_signals_m_df = "long")

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = wrong_chosen_score_metric_and_position,
    min_eligible_assets_fallback = NULL,
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    eligibility_quantile_range = c(0.5, 0.75),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "chosen score metric selection not avaiable in signals_m_df"
  )

})

test_that("check_inputs_port_backtest throws an error when eligibility_quantile_range/min_eligibility_fallback/rebal/buffer are not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Wrong eligibility_quantile_range
  wrong_eligibility_quantile_range <- c(0.5)

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    min_eligible_assets_fallback = NULL,
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    eligibility_quantile_range = wrong_eligibility_quantile_range,
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "eligibility_quantile_range should have length 2."
  )

  #Wrong eligibility_quantile_range
  wrong_eligibility_quantile_range <- c(1, 0.9)

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    min_eligible_assets_fallback = NULL,
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    eligibility_quantile_range = wrong_eligibility_quantile_range,
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "eligibility_quantile_range should be in increasing order."
  )

  #Wrong eligibility_quantile_range
  wrong_eligibility_quantile_range <- c(0.75, 1.9)

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    min_eligible_assets_fallback = NULL,
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    eligibility_quantile_range = wrong_eligibility_quantile_range,
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "eligibility_quantile_range should be between 0 and 1."
  )

  #Wrong min_eligible_assets_fallback
  wrong_min_fallback <- 3500

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    min_eligible_assets_fallback = wrong_min_fallback,
    eligibility_quantile_range = c(0.67, 1),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "min_eligible_assets_fallback should be less than the average number of assets."
  )


  #Wrong min_eligible_assets_fallback
  wrong_min_fallback <- -2

  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    rebalancing_months = 7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    min_eligible_assets_fallback = wrong_min_fallback,
    eligibility_quantile_range = c(0.67, 1),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "min_eligible_assets_fallback should be a positive integer."
  )

  #Wrong rebal months
  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    rebalancing_months = -7,
    initial_buffer_period = 12,
    port_construction_method = "sw",
    min_eligible_assets_fallback = NULL,
    eligibility_quantile_range = c(0.67, 1),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "rebalancing_months should be between 1 and 12."
  )


  #Wrong rebal months
  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    rebalancing_months = "seven",
    initial_buffer_period = 12,
    port_construction_method = "sw",
    min_eligible_assets_fallback = NULL,
    eligibility_quantile_range = c(0.67, 1),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "rebalancing_months should be numeric."
  )

  #Wrong buffer
  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    rebalancing_months = 7,
    initial_buffer_period = -12,
    port_construction_method = "sw",
    min_eligible_assets_fallback = NULL,
    eligibility_quantile_range = c(0.67, 1),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "initial_buffer_period must be higher than 0"
  )


  #Wrong rebal months
  expect_error(check_inputs_port_backtest(
    signals_m_df = signals_m_df, oos_predictions_m_df = NULL,
    chosen_score_metric_and_position = c(Alpha = "long"),
    rebalancing_months = 7,
    initial_buffer_period = "two",
    port_construction_method = "sw",
    min_eligible_assets_fallback = NULL,
    eligibility_quantile_range = c(0.67, 1),
    daily_stock_returns_m_xts = daily_stock_returns_m_xts,
    daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
  ), "initial_buffer_period must be numeric"
  )


})

test_that("check_inputs_port_backtest throws an error when oos_predictions_m_df is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Create a wrong mocked oos_predictions_m_df
  oos_predictions_m_df <- signals_m_df %>% dplyr::filter(dates >= "2001-08-15") %>%
    dplyr::select(-Beta, -Gamma)
  colnames(oos_predictions_m_df) <- c("id", "tickers", "dates", "pred")
  wrong_oos_predictions_m_df <- oos_predictions_m_df[-1,]

  #IDs not match
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = wrong_oos_predictions_m_df,
      chosen_score_metric_and_position = NULL,
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      custom_stock_weights_m_df = NULL,
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
    ), "all id's from signals_m_df after initial_buffer_period must have a correspondence in oos_predictions_m_df"
  )

  #Create a wrong mocked oos_predictions_m_df wwithout pred
  wrong_oos_predictions_m_df <- signals_m_df %>% dplyr::filter(dates >= "2001-08-15") %>%
    dplyr::select(-Beta, -Gamma)
  colnames(wrong_oos_predictions_m_df) <- c("id", "tickers", "dates", "preds")

  #wrong column names
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = wrong_oos_predictions_m_df,
      chosen_score_metric_and_position = NULL,
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      custom_stock_weights_m_df = NULL,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
    ), "oos_predictions_m_df should contain columns 'id', 'tickers', 'dates', 'pred'"
  )


  #Both oos_predictions and chosen_score_metric are provided
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = oos_predictions_m_df,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      custom_stock_weights_m_df = NULL,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
    ), "either chosen_score_metric_and_position, oos_predictions_m_df or custom_stock_weights_m_df should be provided."
  )

  #Neither is provided
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = NULL,
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      custom_stock_weights_m_df = NULL,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
    ), "either chosen_score_metric_and_position or oos_predictions_m_df should be provided."
  )


})

test_that("check_inputs_port_backtest throws an error when daily_stock_returns_m_xts is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))


  #wrong format
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      cov_matrix_sample_size = 252,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = as.data.frame(daily_stock_returns_m_xts),
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts
    ), "daily_stock_returns_m_xts must be a xts object"
  )

  #Absence of cov_matrix_sample_size
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = NULL
    ), "cov_matrix_sample_size must be provided together with daily_stock_returns_m_xts"
  )

  #cov_matrix_sample_size > nrows of daily_stock_returns_m_xts
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 400
    ), "daily_stock_returns_m_xts must have at least cov_matrix_sample_size rows"
  )

  #absence of tickers
  wrong_daily_stock_returns_m_xts <- daily_stock_returns_m_xts[,-4]
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = wrong_daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 100
    ), "all tickers derived from signals_m_df must be present in daily_stock_returns_m_xts"
  )

  #absence of dates
  wrong_daily_stock_returns_m_xts <- daily_stock_returns_m_xts[-c(1:180),]
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = wrong_daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 150
    ), "There is not enought cov_matrix_sample_size dates in daily_stock_returns_m_xts at backtesting date 2001-04-15"
  )

  #absence of days
  wrong_daily_stock_returns_m_xts <- daily_stock_returns_m_xts[-c(100:120),]
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = wrong_daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 100
    ), "daily_stock_returns_m_xts structure is wrong. It should have unique consecutive days and there should not be less than 15 trading days in any month."
  )


  #consecutive days
  wrong_daily_stock_returns_m_xts <- daily_stock_returns_m_xts
  wrong_dates <- zoo::index(daily_stock_returns_m_xts)
  wrong_dates[5] <- "1999-05-15"
  wrong_daily_stock_returns_m_xts <-
    xts::xts(as.data.frame(wrong_daily_stock_returns_m_xts),
             order.by = wrong_dates)

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = wrong_daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 100
    ), "daily_stock_returns_m_xts structure is wrong. It should have unique consecutive days and there should not be less than 15 trading days in any month."
  )

})

test_that("check_inputs_port_backtest throws an error when daily_bench_returns_m_xts is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Change daily_bench_returns_m_xts
  wrong_daily_benchmark_returns_m_xts <- daily_benchmark_returns_m_xts[-c(1:5),]

  #Unmatch
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = wrong_daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 100
    ), "dates in daily_bench_returns_m_xts and daily_stock_returns_m_xts must be the same"
  )

  #Change daily_bench_returns_m_xts
  wrong_daily_benchmark_returns_m_xts <- daily_benchmark_returns_m_xts
  wrong_daily_benchmark_returns_m_xts[2, ] <- NA

  #NAs
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = wrong_daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 100
    ), "daily_bench_returns_m_xts must not have any NA values"
  )

  #Wrong benchmark
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 100,
      cov_matrix_benchmark = "isus"
    ), "cov_matrix_benchmark must be present in daily_bench_returns_m_xts"
  )

  #No daily xts returns
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_sample_size = 100,
      cov_matrix_benchmark = "ibov"
    ), "daily_stock_returns_m_xts must be provided together with daily_bench_returns_m_xts"
  )


  #df
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = as.data.frame(daily_benchmark_returns_m_xts),
      cov_matrix_sample_size = 100,
      cov_matrix_benchmark = "ibov"
    ), "daily_bench_returns_m_xts must be a xts object"
  )

})

test_that("check_inputs_port_backtest throws an error when benchmark_returns_m_xts is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Wrong benchmark
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "isus",
      benchmark_returns_m_xts = benchmark_returns_m_xts
    ), "selected_benchmark should be present in benchmark_returns_m_xts"
  )

  #No NAs
  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts
  wrong_benchmark_returns_m_xts[2, ] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = wrong_benchmark_returns_m_xts
    ), "benchmark_returns_m_xts must not have any NA"
  )

  #Absence of dates
  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts
  wrong_benchmark_returns_m_xts <- wrong_benchmark_returns_m_xts[-2, ]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = wrong_benchmark_returns_m_xts
    ), "all dates in signals_m_df must be present in benchmark_returns_m_xts"
  )

  #Not enough dates
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 6,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts
    ), "There must be at least one date in benchmark_returns_m_xts after initial_buffer_period"
  )

  #Not consecutive
  wrong_benchmark_returns_m_xts <- rbind(benchmark_returns_m_xts, benchmark_returns_m_xts[2,])

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = wrong_benchmark_returns_m_xts
    ), "benchmark_returns_m_xts must have sequential unique monthly dates"
  )

  #Does not contemplate last month of fwd_return_m_df
  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts["2001-03-15/2001-07-15"]
  wrong_signals_m_df <- signals_m_df %>% dplyr::filter(!dates == "2001-08-15")
  wrong_liquidity_m_df <- liquidity_m_df %>% dplyr::filter(!dates == "2001-08-15")
  wrong_volatility_m_df <- volatility_m_df %>% dplyr::filter(!dates == "2001-08-15")
  wrong_target_m_df <- target_m_df %>% dplyr::filter(!dates == "2001-08-15")

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = wrong_signals_m_df,
      stock_groups = NULL,
      oos_predictions_m_df = NULL,
      fwd_return_m_df = wrong_target_m_df,
      liquidity_m_df = wrong_liquidity_m_df,
      volatility_m_df = wrong_volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights = NULL,
      liquidity_floor_cutoffs = NULL,
      verbose = TRUE,
      main_liquidity_metric = "mean_volfin_3m",
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = wrong_benchmark_returns_m_xts
    ), "last date of fwd_return_m_df should be covered by benchmark_returns_m_xts"
  )



})

test_that("check_inputs_port_backtest throws an error when stock_groups_m_df is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #wrong_stock_groups_m_df
  wrong_stock_groups_m_df <- stock_groups_m_df
  wrong_stock_groups_m_df$Subsector <- as.factor(wrong_stock_groups_m_df$Subsector)


  #Wrong benchmark
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = wrong_stock_groups_m_df
    ), "all group columns in stock_groups_m_df must be character"
  )


  #wrong_stock_groups_m_df
  wrong_stock_groups_m_df <- stock_groups_m_df
  wrong_stock_groups_m_df <- wrong_stock_groups_m_df[-3,]


  #Wrong benchmark
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = wrong_stock_groups_m_df
    ), "all ids from signals_m_df should be present in stock_groups_m_df"
  )

  #wrong_stock_groups_m_df
  wrong_stock_groups_m_df <- stock_groups_m_df
  wrong_stock_groups_m_df[2,4] <- NA


  #Wrong benchmark
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = wrong_stock_groups_m_df
    ), "stock_groups_m_df should not have NAs"
  )

})

test_that("check_inputs_port_backtest throws an error when liquidity_m_df is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Absence of ids
  wrong_liquidity_m_df <- liquidity_m_df
  wrong_liquidity_m_df <- wrong_liquidity_m_df[-3,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = wrong_liquidity_m_df
    ), "all ids from signals_m_df should be present in liquidity_m_df"
  )

  #NAs
  wrong_liquidity_m_df <- liquidity_m_df
  wrong_liquidity_m_df[4,5] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = wrong_liquidity_m_df
    ), "liquidity_m_df should contain only numeric columns with non-NAs."
  )

  #main liq metric not present
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_6m"
    ), "main_liquidity_metric must be present in liquidity_m_df"
  )

  #not adequate mean vol fin string
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "meanvolfin"
    ), "main_liquidity_metric must contain the string 'mean_volfin'"
  )

  #Inadequately normalized
  wrong_liquidity_m_df <- liquidity_m_df
  wrong_liquidity_m_df$mean_volfin_3m <-
    runif(n = length(wrong_liquidity_m_df$mean_volfin_3m),
          min = -1, max = 1)

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      liquidity_floor_cutoffs = NULL,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = wrong_liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m"
    ), "values in liquidity_m_df should not be normalized"
  )

})

test_that("check_inputs_port_backtest throws an error when volatility_m_df is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Absence of ids
  wrong_volatility_m_df <- volatility_m_df
  wrong_volatility_m_df <- wrong_volatility_m_df[-3,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      liquidity_floor_cutoffs = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = wrong_volatility_m_df
    ), "all ids from signals_m_df should be present in volatility_m_df"
  )

  #NAs
  wrong_volatility_m_df <- volatility_m_df
  wrong_volatility_m_df[4,5] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = wrong_volatility_m_df
    ), "volatility_m_df should contain only numeric columns with non-NAs."
  )

  #daily_vol metric not present
  wrong_volatility_m_df <- volatility_m_df
  colnames(wrong_volatility_m_df)[4] <- "vol"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = wrong_volatility_m_df
    ), "daily_vol must be present in volatility_m_df in order to calculate indirect costs"
  )


  #Inadequately normalized
  wrong_volatility_m_df <- volatility_m_df
  wrong_volatility_m_df$daily_vol <-
    runif(n = length(wrong_volatility_m_df$daily_vol),
          min = -1, max = 1)

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = wrong_volatility_m_df
    ), "values in volatility_m_df should not be normalized"
  )

})

test_that("check_inputs_port_backtest throws an error when benchmark_weights_m_df is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Absence of ids
  wrong_benchmark_weights_m_df <- benchmark_weights_m_df
  wrong_benchmark_weights_m_df <- wrong_benchmark_weights_m_df[-3,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      liquidity_floor_cutoffs = NULL,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = wrong_benchmark_weights_m_df
    ), "all ids from signals_m_df should be present in benchmark_weights_m_df"
  )


  #NAs
  wrong_benchmark_weights_m_df <- benchmark_weights_m_df
  wrong_benchmark_weights_m_df[4,5] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = wrong_benchmark_weights_m_df
    ), "benchmark_weights_m_df should contain only numeric columns with non-NAs."
  )

  #Wrong bench
  wrong_benchmark_weights_m_df <- benchmark_weights_m_df
  colnames(wrong_benchmark_weights_m_df)[4] <- "IBOV"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = wrong_benchmark_weights_m_df
    ), "selected_benchmark should be present in benchmark_weights_m_df"
  )

  #Wrong w
  wrong_benchmark_weights_m_df <- benchmark_weights_m_df
  wrong_benchmark_weights_m_df[5,4] <- 1.2

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = wrong_benchmark_weights_m_df
    ), "values in benchmark_weights_m_df should be between 0 and 1"
  )

  #Wrong w
  wrong_benchmark_weights_m_df <- benchmark_weights_m_df
  wrong_benchmark_weights_m_df[5,4] <- 0.001

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = wrong_benchmark_weights_m_df
    ), "weights in benchmark_weights_m_df should sum to 1 in every date."
  )

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = NULL
    ), "benchmark_weights_m_df must be provided when selected_benchmark is provided"
  )

})

test_that("check_inputs_port_backtest throws an error when custom_stock_weights_m_df is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))
  custom_stock_weights_m_df <- benchmark_weights_m_df
  colnames(custom_stock_weights_m_df)[4] <- "weights"
  custom_stock_weights_m_df[,5] <- NULL

  #Absence of ids
  wrong_custom_stock_weights_m_df <- custom_stock_weights_m_df
  wrong_custom_stock_weights_m_df <- wrong_custom_stock_weights_m_df[-3,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      liquidity_floor_cutoffs = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = wrong_custom_stock_weights_m_df
    ), "all ids from signals_m_df should be present in custom_stock_weights_m_df"
  )


  #NAs
  wrong_custom_stock_weights_m_df <- custom_stock_weights_m_df
  wrong_custom_stock_weights_m_df[4,5] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = wrong_custom_stock_weights_m_df
    ), "custom_stock_weights_m_df should contain only numeric columns with non-NAs."
  )

  #Wrong colname
  wrong_custom_stock_weights_m_df <- custom_stock_weights_m_df
  colnames(wrong_custom_stock_weights_m_df)[4] <- "IBOV"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = wrong_custom_stock_weights_m_df
    ), "custom_stock_weights_m_df should have a column named weights"
  )

  #Wrong w
  wrong_custom_stock_weights_m_df <- custom_stock_weights_m_df
  wrong_custom_stock_weights_m_df[5,4] <- 1.2

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = wrong_custom_stock_weights_m_df
    ), "weights in custom_stock_weights_m_df should be between 0 and 1"
  )

  #Wrong w
  wrong_custom_stock_weights_m_df <- custom_stock_weights_m_df
  wrong_custom_stock_weights_m_df[5,4] <- 0.001

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = wrong_custom_stock_weights_m_df
    ), "weights in custom_stock_weights_m_df should sum to 1 in every date."
  )

})

test_that("check_inputs_port_backtest throws an error when fwd_return_m_df is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Wrong colname
  wrong_fwd_return_m_df <- target_m_df
  colnames(wrong_fwd_return_m_df)[4] <- "ret_1m"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      liquidity_floor_cutoffs = NULL,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df
    ), "fwd_return_1m should be present in fwd_return_m_df"
  )

  #NAs
  wrong_fwd_return_m_df <- target_m_df
  wrong_fwd_return_m_df[7,4] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df
    ), "fwd_return_m_df before last period should contain only numeric columns with non-NAs."
  )


  #NAs in first backtest period
  wrong_fwd_return_m_df <- target_m_df
  wrong_fwd_return_m_df <- wrong_fwd_return_m_df %>% dplyr::filter(dates == "2001-08-15")
  adj_benchmark_returns_m_xts <- xts::xts(rbind(as.data.frame(benchmark_returns_m_xts), data.frame(ibov = c(0.1), idiv = 0.3)),
                                          order.by = as.Date(c(zoo::index(benchmark_returns_m_xts), "2001-09-15"))) #Avoid triggering a first stop

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 7,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 25,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = adj_benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df
    ), "There must be at least one date in benchmark_returns_m_xts after initial_buffer_period"
  )

  #Message for no NAs found
  wrong_fwd_return_m_df <- target_m_df %>% dplyr::mutate(fwd_return_1m = dplyr::if_else(is.na(fwd_return_1m), 1, fwd_return_1m)) %>%
    dplyr::select(id, tickers, dates, fwd_return_1m)

  expect_message(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 25,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      turnover_constraint_policy = NULL,
      liquidity_floor_cutoffs = NULL,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      transaction_costs_parameters = list(strategy_aum = 10, direct_transaction_cost = 0.07, alpha = 0.5, lambda = "dynamic"),
      verbose = TRUE
    ), "The following final dates from fwd_return_m_df are expected to be NA in an up-to-date backtest, but are not: 2001-08-15"
  )

  #NAs before last period
  wrong_fwd_return_m_df <- target_m_df %>% dplyr::select(-fwd_return_3m, -fwd_return_6m)
  wrong_fwd_return_m_df[3,4] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 25,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      turnover_constraint_policy = NULL,
      liquidity_floor_cutoffs = NULL,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      transaction_costs_parameters = list(strategy_aum = 10, direct_transaction_cost = 0.07, alpha = 0.5, lambda = "dynamic"),
      verbose = TRUE
    ),"fwd_return_m_df before last period should contain only numeric columns with non-NAs.")

  #Number of dates with NAs only 1
  wrong_fwd_return_m_df <- target_m_df %>% dplyr::select(-fwd_return_3m, -fwd_return_6m)
  wrong_fwd_return_m_df$fwd_return_1m[which(wrong_fwd_return_m_df$dates == as.Date("2001-06-15"))] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 25,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      turnover_constraint_policy = NULL,
      liquidity_floor_cutoffs = NULL,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      transaction_costs_parameters = list(strategy_aum = 10, direct_transaction_cost = 0.07, alpha = 0.5, lambda = "dynamic"),
      verbose = TRUE
    ),"fwd_return_m_df can't have NAs in the first backtesting period")



  #IDs do not match
  wrong_fwd_return_m_df <- target_m_df %>% dplyr::select(-fwd_return_3m, -fwd_return_6m)
  wrong_fwd_return_m_df <- wrong_fwd_return_m_df[-4,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 25,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      turnover_constraint_policy = NULL,
      liquidity_floor_cutoffs = NULL,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      transaction_costs_parameters = list(strategy_aum = 10, direct_transaction_cost = 0.07, alpha = 0.5, lambda = "dynamic"),
      verbose = TRUE
    ),"signals_m_df and fwd_return_m_df must possess same number of rows.")


  #IDs do not match
  wrong_fwd_return_m_df <- target_m_df %>% dplyr::select(-fwd_return_3m, -fwd_return_6m)
  wrong_fwd_return_m_df[4 ,1] <- c("Stock AA-2001-08-15")
  wrong_fwd_return_m_df[4 ,2] <- c("Stock AA")
  wrong_fwd_return_m_df[4 ,3] <- c("2001-08-15") %>% as.Date()

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 25,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      turnover_constraint_policy = NULL,
      liquidity_floor_cutoffs = NULL,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      transaction_costs_parameters = list(strategy_aum = 10, direct_transaction_cost = 0.07, alpha = 0.5, lambda = "dynamic"),
      verbose = TRUE
    ),"id in signals_m_df and in fwd_return_m_df must match.")

  #Normalization
  wrong_fwd_return_m_df <- target_m_df %>% dplyr::select(-fwd_return_3m, -fwd_return_6m)
  wrong_fwd_return_m_df$fwd_return_1m <- runif(length(wrong_fwd_return_m_df$fwd_return_1m), -1, 1)


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 4,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 25,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = wrong_fwd_return_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      turnover_constraint_policy = NULL,
      liquidity_floor_cutoffs = NULL,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      transaction_costs_parameters = list(strategy_aum = 10, direct_transaction_cost = 0.07, alpha = 0.5, lambda = "dynamic"),
      verbose = TRUE
    ),"values in fwd_return_m_df should not be normalized")


})

test_that("check_inputs_port_backtest throws an error when custom_stock_metrics_m_df is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Ids no match
  wrong_custom_stock_metrics_m_df <- signals_m_df %>% dplyr::filter(!id == "Stock A-2001-06-15")

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      liquidity_floor_cutoffs = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = wrong_custom_stock_metrics_m_df,
      verbose = TRUE
    ), "all id's from signals_m_df after initial_buffer_period must have a correspondence in custom_stock_metrics_m_df"
  )

  #NA
  wrong_custom_stock_metrics_m_df <- signals_m_df
  wrong_custom_stock_metrics_m_df[4,4] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = wrong_custom_stock_metrics_m_df,
      verbose = TRUE
    ), "custom_stock_metrics_m_df should not have NAs"
  )


})

test_that("check_inputs_port_backtest throws an error when concentration_constraint_policy is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #wrong names
  wrong_concentration_constraint_policy <- concentration_constraint_policy
  names(wrong_concentration_constraint_policy)[1] <- "Benchmark"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      liquidity_floor_cutoffs = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = wrong_concentration_constraint_policy,
      verbose = TRUE
    ), "Error in concentration_constraint_policy: elements of concentration_constraint_policy should be one of benchmark, max_abs_active_individual_weight or max_abs_active_group_weight."
  )

  #bench not set
  wrong_concentration_constraint_policy <- concentration_constraint_policy
  wrong_concentration_constraint_policy$benchmark <- NULL

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      min_eligible_assets_fallback = NULL,
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = wrong_concentration_constraint_policy,
      verbose = TRUE
    ), "Error in concentration_constraint_policy: benchmark must be set"
  )

  #bench not set
  wrong_concentration_constraint_policy <- concentration_constraint_policy
  names(wrong_concentration_constraint_policy$max_abs_active_group_weight)[1] <- "Subsector"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = wrong_concentration_constraint_policy,
      verbose = TRUE
    ), "Error in concentration_constraint_policy: max_abs_active_group_weight can't contain duplicated names"
  )

  #benchmark do not match
  wrong_concentration_constraint_policy <- concentration_constraint_policy
  wrong_concentration_constraint_policy$benchmark <- "IBOV"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = wrong_concentration_constraint_policy,
      verbose = TRUE
    ), "Error in concentration_constraint_policy: benchmark must match selected_benchmark"
  )

  #bench weights missing
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = NULL,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = concentration_constraint_policy,
      verbose = TRUE
    ), "selected_benchmark must be provided when benchmark_weights_m_df is provided"
  )

  #stock groups missing
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = NULL,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = concentration_constraint_policy,
      verbose = TRUE
    ), "Error in concentration_constraint_policy: stock_groups_m_df can't be missing if max_abs_active_group_weight of concentration_constraint_policy is set"
  )

  #not numeric
  wrong_concentration_constraint_policy <- concentration_constraint_policy
  wrong_concentration_constraint_policy$max_abs_active_group_weight[1] <- "Sectors"


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = wrong_concentration_constraint_policy,
      verbose = TRUE
    ), "Error in concentration_constraint_policy: max_abs_active_group_weight must be numeric"
  )



  #do not match stock groups names
  wrong_concentration_constraint_policy <- concentration_constraint_policy
  names(wrong_concentration_constraint_policy$max_abs_active_group_weight)[1] <- "Sectors"


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = wrong_concentration_constraint_policy,
      verbose = TRUE
    ), "Error in concentration_constraint_policy: names of group constraints must match groups in stock_groups_m_df"
  )

  #not mvo and concentration
  wrong_concentration_constraint_policy <- concentration_constraint_policy

  expect_error(
    expect_message(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = wrong_concentration_constraint_policy,
      verbose = TRUE
    ), "concentration_constraint_policy is only available for port_construction_method = 'mvo'. Ignoring concentration_constraint_policy"
  )
  )

  #benchmark not present
  wrong_concentration_constraint_policy <- concentration_constraint_policy
  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts
  colnames(wrong_benchmark_returns_m_xts)[1] <- "abeb"

  expect_error(
      check_inputs_port_backtest(
        signals_m_df = signals_m_df,
        oos_predictions_m_df = NULL,
        min_eligible_assets_fallback = NULL,
        chosen_score_metric_and_position = c(Alpha = "long"),
        rebalancing_months = 7,
        initial_buffer_period = 2,
        port_construction_method = "sw",
        eligibility_quantile_range = c(0.5,0.75),
        daily_stock_returns_m_xts = daily_stock_returns_m_xts,
        daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
        cov_matrix_benchmark = "ibov",
        cov_matrix_sample_size = 100,
        selected_benchmark = "abeb",
        benchmark_returns_m_xts = benchmark_returns_m_xts,
        stock_groups_m_df = stock_groups_m_df,
        liquidity_m_df = liquidity_m_df,
        main_liquidity_metric = "mean_volfin_3m",
        volatility_m_df = volatility_m_df,
        benchmark_weights_m_df = benchmark_weights_m_df,
        custom_stock_weights_m_df = NULL,
        fwd_return_m_df = target_m_df,
        custom_stock_metrics_m_df = NULL,
        concentration_constraint_policy = wrong_concentration_constraint_policy,
        verbose = TRUE
      ), "selected_benchmark should be present in benchmark_returns_m_xts"
    )




})

test_that("check_inputs_port_backtest throws an error when liquidity_constraint_policy is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))


  #no liq m df
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      min_eligible_assets_fallback = NULL,
      oos_predictions_m_df = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = NULL,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      verbose = TRUE
    ), "liquidity_m_df must be coercible to a meta dataframe"
  )



  #at least liquidity_floor_rule or cap
  wrong_liquidity_constraint_policy <- liquidity_constraint_policy
  wrong_liquidity_constraint_policy$liquidity_floor_rule <- NULL
  wrong_liquidity_constraint_policy$liquidity_cap_rules <- NULL


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      min_eligible_assets_fallback = NULL,
      oos_predictions_m_df = NULL,
      liquidity_floor_cutoffs = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = wrong_liquidity_constraint_policy,
      verbose = TRUE
    ), "Error in liquidity_constraint_policy: either liquidity_floor_rule or liquidity_cap_rules must be set"
  )


  #liquidity caps must have names
  wrong_liquidity_constraint_policy <- liquidity_constraint_policy
  names(wrong_liquidity_constraint_policy$liquidity_cap_rules) <- NULL


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = wrong_liquidity_constraint_policy,
      verbose = TRUE
    ), "Error in liquidity_constraint_policy: liquidity_cap_rules must have names"
  )

  #wrong liquidity_floor_rule
  wrong_liquidity_constraint_policy <- liquidity_constraint_policy
  wrong_liquidity_constraint_policy$liquidity_floor_rule <- "pico_caps"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = wrong_liquidity_constraint_policy,
      verbose = TRUE
    ), "Error in liquidity_constraint_policy: liquidity_floor_rule must be one of 'micro_caps', 'small_caps', 'mid_caps', 'large_caps' or 'mega_caps'"
  )

  #duplicated caps
  wrong_liquidity_constraint_policy <- liquidity_constraint_policy
  wrong_liquidity_constraint_policy$liquidity_cap_rules <- c(wrong_liquidity_constraint_policy$liquidity_cap_rules[2],
                                                             wrong_liquidity_constraint_policy$liquidity_cap_rules[2])

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = wrong_liquidity_constraint_policy,
      verbose = TRUE
    ), "Error in liquidity_constraint_policy: liquidity_cap_rules can't have duplicated names"
  )

  #caps for stocks less liquidir than floor
  wrong_liquidity_constraint_policy <- liquidity_constraint_policy
  wrong_liquidity_constraint_policy$liquidity_floor_rule <- "small_caps"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = wrong_liquidity_constraint_policy,
      verbose = TRUE
    ), "Error in liquidity_constraint_policy: Liquidity cap rule for 'micro_caps' is less liquid than the liquidity_floor_rule 'small_caps'"
  )

  #liq caps not in right order
  wrong_liquidity_constraint_policy <- liquidity_constraint_policy
  wrong_liquidity_constraint_policy$liquidity_cap_rules[2] <- 0.0001


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = wrong_liquidity_constraint_policy,
      verbose = TRUE
    )
  )

  #liquidity_floor_cutoffs missing
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_floor_cutoffs = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      verbose = TRUE
    ), "Error in liquidity_constraint_policy: liquidity_floor_cutoffs can't be missing if liquidity_constraint_policy is set"
  )

  #liquidity_floor_cutoffs and rule do not match
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df <- wrong_liquidity_floor_cutoffs_df[-1,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      liquidity_constraint_policy = liquidity_constraint_policy,
      verbose = TRUE
    ), "Error in liquidity_constraint_policy: liquidity_floor_rule not present in liquidity_floor_cutoffs"
  )


})

test_that("check_inputs_port_backtest throws an error when liquidity_floor_cutoffs is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #liquidity
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df <- wrong_liquidity_floor_cutoffs_df[,1 ,drop = FALSE]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ), "liquidity_floor_cutoffs must have at least 2 columns"
  )

  #first col is 'liquidity_classification'
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  colnames(wrong_liquidity_floor_cutoffs_df)[1] <- "liq_class"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ) #Will trigger another error first
  )

  #first col is 'liquidity_classification'
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df$liquidity_classification[1] <- "nano_caps"


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ),  "liquidity_classification must be one of micro_caps, small_caps, mid_caps, large_caps or mega_caps"
  )

  #duplicated row
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df[2,] <- wrong_liquidity_floor_cutoffs_df[1,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ),  "liquidity_classification must not have duplicates"
  )


  #no NA
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df[2,3] <- NA

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ),  "liquidity_floor_cutoffs must not contain NAs"
  )

  #main liq metric no present
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df$mean_volfin_3m <- NULL

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ),  "main_liquidity_metric must be present in liquidity_floor_cutoffs"
  )

  #wrong order
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df[2,] <- liquidity_floor_cutoffs_df[1,]
  wrong_liquidity_floor_cutoffs_df[1,] <- liquidity_floor_cutoffs_df[2,]


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ),  "liquidity_floor_cutoffs is not in ascending order according to main_liquidity_metric"
  )


  #wrong order for not main metric
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df$presence[2] <- liquidity_floor_cutoffs_df$presence[1]
  wrong_liquidity_floor_cutoffs_df$presence[1] <- liquidity_floor_cutoffs_df$presence[2]


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ),  "liquidity metrics orders in liquidity_floor_cutoffs are conflicting"
  )

  #wrong order for not main metric
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  colnames(wrong_liquidity_floor_cutoffs_df)[2] <- "negotiability"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ),  "all liquidity_floor_cutoffs must be present in liquidity_m_df"
  )

  #not normalized
  wrong_liquidity_floor_cutoffs_df <- liquidity_floor_cutoffs_df
  wrong_liquidity_floor_cutoffs_df$mean_volfin_3m <- c(-1, -0.75, 0 , 0.5, 1)

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = NULL,
      liquidity_floor_cutoffs = wrong_liquidity_floor_cutoffs_df,
      verbose = TRUE
    ),  "liquidity_floor_cutoffs values must not be normalized"
  )


})

test_that("check_inputs_port_backtest throws an error when turnover_constraint_policy is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #both elements needed
  wrong_turnover_constraint_policy <- turnover_constraint_policy
  wrong_turnover_constraint_policy$turnover_cap_rules <- NULL


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = wrong_turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      verbose = TRUE
    ), "Error in turnover_constraint_policy: turnover_cap_rules can't be missing"
  )


  #both elements needed
  wrong_turnover_constraint_policy <- turnover_constraint_policy
  wrong_turnover_constraint_policy$quantile_range_buffer <- 2


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = wrong_turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      verbose = TRUE
    ), "Error in turnover_constraint_policy: quantile_range_buffer must be a number"
  )

  #liquidity caps must have names
  wrong_turnover_constraint_policy <- turnover_constraint_policy
  names(wrong_turnover_constraint_policy$turnover_cap_rules) <- NULL


  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = wrong_turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      verbose = TRUE
    ), "Error in turnover_constraint_policy: turnover_cap_rules must have names"
  )

  #wrong category
  wrong_turnover_constraint_policy <- turnover_constraint_policy
  names(wrong_turnover_constraint_policy$turnover_cap_rules)[1] <- "nano_caps"

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = wrong_turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      verbose = TRUE
    ), "Error in turnover_constraint_policy: names of turnover_cap_rules must be in accordance to micro_caps, small_caps, mid_caps, large_caps, mega_caps"
  )


  #duplicated caps
  wrong_turnover_constraint_policy <- turnover_constraint_policy
  wrong_turnover_constraint_policy$turnover_cap_rules <- c(wrong_turnover_constraint_policy$turnover_cap_rules[2],
                                                           wrong_turnover_constraint_policy$turnover_cap_rules[2])
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = wrong_turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      verbose = TRUE
    ), "Error in turnover_constraint_policy: names of turnover_cap_rules must not be duplicated"
  )


  #liq caps not in right order
  wrong_turnover_constraint_policy <- turnover_constraint_policy
  wrong_turnover_constraint_policy$turnover_cap_rules[2] <- 0.0001

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = wrong_turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      verbose = TRUE
    )
  )


  #liquidity_floor_cutoffs missing
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_floor_cutoffs = NULL,
      liquidity_constraint_policy = NULL,
      turnover_constraint_policy = turnover_constraint_policy,
      verbose = TRUE
    ), "liquidity_floor_cutoffs and liquidity_m_df are needed if turnover_constraint_policy is set"
  )


})

test_that("check_inputs_port_backtest throws an error when user_defined_OR_rules_m_df is not right", {
  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #classification col missing
  user_defined_OR_rules_m_df <- signals_m_df %>% dplyr::mutate(size = Alpha) %>% dplyr::select(id, tickers, dates, size) %>%
    dplyr::mutate(is_small = dplyr::if_else(size < 0, 1, 0))
  wrong_user_defined_OR_rules_m_df <- user_defined_OR_rules_m_df
  wrong_user_defined_OR_rules_m_df[,5] <- NULL

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = wrong_user_defined_OR_rules_m_df,
      verbose = TRUE
    ), "user_defined_OR_rules_m_df should have 5 columns"
  )

  #classification should be binary
  wrong_user_defined_OR_rules_m_df <- signals_m_df %>% dplyr::mutate(size = Alpha) %>% dplyr::select(id, tickers, dates, size) %>%
    dplyr::mutate(is_small = dplyr::if_else(size < 0, 10, 2))

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = wrong_user_defined_OR_rules_m_df,
      verbose = TRUE
    ), "fifth column of user_defined_OR_rules_m_df should be 0 or 1"
  )

  #ids not match
  wrong_user_defined_OR_rules_m_df <- signals_m_df %>% dplyr::mutate(size = Alpha) %>% dplyr::select(id, tickers, dates, size) %>%
    dplyr::mutate(is_small = dplyr::if_else(size < 0, 1, 0))
  wrong_user_defined_OR_rules_m_df <- wrong_user_defined_OR_rules_m_df[-3,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = wrong_user_defined_OR_rules_m_df,
      verbose = TRUE
    ), "user_defined_OR_rules_m_df should contemplate all signals_m_df id's after initial_buffer_period"
  )




})

test_that("check_inputs_port_backtest throws an error when user_defined_AND_rules_m_df is not right", {
  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #classification col missing
  user_defined_AND_rules_m_df <- signals_m_df %>% dplyr::mutate(size = Alpha) %>% dplyr::select(id, tickers, dates, size) %>%
    dplyr::mutate(is_small = dplyr::if_else(size < 0, 1, 0))
  wrong_user_defined_AND_rules_m_df <- user_defined_AND_rules_m_df
  wrong_user_defined_AND_rules_m_df[,5] <- NULL

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = wrong_user_defined_AND_rules_m_df,
      verbose = TRUE
    ), "user_defined_AND_rules_m_df should have 5 columns"
  )

  #classification should be binary
  wrong_user_defined_AND_rules_m_df <- signals_m_df %>% dplyr::mutate(size = Alpha) %>% dplyr::select(id, tickers, dates, size) %>%
    dplyr::mutate(is_small = dplyr::if_else(size < 0, 10, 2))

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = wrong_user_defined_AND_rules_m_df,
      verbose = TRUE
    ), "fifth column of user_defined_AND_rules_m_df should be 0 or 1"
  )

  #ids not match
  wrong_user_defined_AND_rules_m_df <- signals_m_df %>% dplyr::mutate(size = Alpha) %>% dplyr::select(id, tickers, dates, size) %>%
    dplyr::mutate(is_small = dplyr::if_else(size < 0, 1, 0))
  wrong_user_defined_AND_rules_m_df <- wrong_user_defined_AND_rules_m_df[-3,]

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      port_construction_method = "sw",
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = wrong_user_defined_AND_rules_m_df,
      verbose = TRUE
    ), "user_defined_AND_rules_m_df should contemplate all signals_m_df id's after initial_buffer_period"
  )

})

test_that("check_inputs_port_backtest throws an error when port_construction_method is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #RP, not rp
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "RP",
      verbose = TRUE
    ), "port_construction_method must be one of 'ew', 'sw', 'cw', 'cs', 'rp', 'mvo' or 'custom_weights'"
  )

  #cov_est_method missing
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "rp",
      cov_estimation_method = NULL,
      verbose = TRUE
    ), "cov_estimation_method can't be missing if port_construction_method is 'rp' or 'mvo'"
  )

  #daily_stock_returns missing
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = NULL,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "rp",
      cov_estimation_method = "sample",
      verbose = TRUE
    ), "daily_stock_returns_m_xts can't be missing if port_construction_method is 'rp' or 'mvo'"
  )

  #daily_bench_ret missing with active_returns
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "rp",
      cov_estimation_method = "sample",
      active_returns = TRUE,
      verbose = TRUE
    ), "daily_bench_returns_m_xts can't be NULL if active_returns is TRUE"
  )


  #daily_bench_ret missing with active_returns
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = NULL,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "custom_weights",
      verbose = TRUE
    ), "custom_stock_weights_m_df must be provided when port_construction_method is 'custom_weights'"
  )

})

test_that("check_inputs_port_backtest throws an error when transaction_cost_pars is not right", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #multiple arguments for strategy_aum
  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "ew",
      transaction_costs_parameters = NULL,
      verbose = TRUE
    ), "transaction_costs_parameters can't be missing"
  )

  #multiple arguments for strategy_aum
  wrong_transaction_costs_parameters <- transaction_costs_parameters
  wrong_transaction_costs_parameters$strategy_aum <- c(1,2,3)

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "ew",
      transaction_costs_parameters = wrong_transaction_costs_parameters,
      verbose = TRUE
    ), "strategy_aum should be a single positive numeric"
  )

  #direct_transaction_cost too high or too low
  wrong_transaction_costs_parameters <- transaction_costs_parameters
  wrong_transaction_costs_parameters$direct_transaction_cost <- c(1)

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "ew",
      transaction_costs_parameters = wrong_transaction_costs_parameters,
      verbose = TRUE
    ), "direct_transaction_cost should be a single numeric between 0.0001 and 0.1"
  )

  #alpha too high or too low
  wrong_transaction_costs_parameters <- transaction_costs_parameters
  wrong_transaction_costs_parameters$alpha <- 2

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "ew",
      transaction_costs_parameters = wrong_transaction_costs_parameters,
      verbose = TRUE
    ), "alpha should be a single numeric between 0 and 1"
  )

  #lambda too high or too low
  wrong_transaction_costs_parameters <- transaction_costs_parameters
  wrong_transaction_costs_parameters$lambda <- 2

  expect_error(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "ew",
      transaction_costs_parameters = wrong_transaction_costs_parameters,
      verbose = TRUE
    ), "lambda should be a single numeric value between 0 and 1"
  )

  #Strategy aum does not match liquidity_m_df units
  wrong_transaction_costs_parameters <- transaction_costs_parameters
  wrong_transaction_costs_parameters$strategy_aum <- 10000000

  expect_warning(
    check_inputs_port_backtest(
      signals_m_df = signals_m_df,
      oos_predictions_m_df = NULL,
      min_eligible_assets_fallback = NULL,
      chosen_score_metric_and_position = c(Alpha = "long"),
      rebalancing_months = 7,
      initial_buffer_period = 2,
      eligibility_quantile_range = c(0.5,0.75),
      daily_stock_returns_m_xts = daily_stock_returns_m_xts,
      daily_bench_returns_m_xts = daily_benchmark_returns_m_xts,
      cov_matrix_benchmark = "ibov",
      cov_matrix_sample_size = 100,
      selected_benchmark = "ibov",
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      stock_groups_m_df = stock_groups_m_df,
      liquidity_m_df = liquidity_m_df,
      main_liquidity_metric = "mean_volfin_3m",
      volatility_m_df = volatility_m_df,
      benchmark_weights_m_df = benchmark_weights_m_df,
      custom_stock_weights_m_df = NULL,
      fwd_return_m_df = target_m_df,
      custom_stock_metrics_m_df = NULL,
      concentration_constraint_policy = NULL,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
      user_defined_OR_rules_m_df = NULL,
      user_defined_AND_rules_m_df = NULL,
      port_construction_method = "ew",
      transaction_costs_parameters = wrong_transaction_costs_parameters,
      verbose = TRUE
    ), "Please be sure that strategy_aum is in same units as main_liquidity_metric"
  )

})






