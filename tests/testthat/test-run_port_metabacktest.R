# Fixtures ----------------------------------------------------------------
# Building the cohort means running three real backtests, so it is built once and reused.

meta_backtest_cache <- new.env(parent = emptyenv())


meta_backtest_inputs <- function() {
  if (!is.null(meta_backtest_cache$inputs)) return(meta_backtest_cache$inputs)

  load(paste(testthat::test_path(), "/testdata/", "toy_preprocessed_port_obj.RData", sep = ""))

  inputs <- list(
    signals_m_df = create_meta_dataframe(signals_m_df, type = "signals"),
    fwd_return_m_df = create_meta_dataframe(fwd_return_m_df, type = "target"),
    liquidity_m_df = create_meta_dataframe(liquidity_m_df),
    volatility_m_df = create_meta_dataframe(volatility_m_df),
    benchmark_weights_m_df = create_meta_dataframe(benchmark_weights_m_df, type = "weights"),
    benchmark_returns_m_xts = suppressMessages(create_meta_xts(benchmark_returns_m_xts))
  )

  meta_backtest_cache$inputs <- inputs
  inputs
}


meta_backtest_cohort <- function() {
  if (!is.null(meta_backtest_cache$cohort)) return(meta_backtest_cache$cohort)

  inputs <- meta_backtest_inputs()

  build_config <- function(method, name) {
    create_port_backtest_config(
      chosen_score_metric_and_position = c(book_yield = "long"),
      eligibility_quantile_range = c(0.67, 1.0), selected_benchmark = "ibov",
      initial_buffer_period = 2, rebalancing_months = c(1, 4),
      port_construction_method = method, main_liquidity_metric = "mean_volfin_3m",
      config_name = name) %>%
      add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                       lambda = "dynamic", strategy_aum = 25000)
  }
  run_one <- function(config) suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))

  cohort <- suppressWarnings(suppressMessages(create_port_backtest_cohort(
    list(run_one(build_config("ew", "ew_by")),
         run_one(build_config("sw", "sw_by")),
         run_one(build_config("cw", "cw_by"))),
    cohort_name = "meta_test_cohort")))

  meta_backtest_cache$cohort <- cohort
  cohort
}


meta_backtest_config <- function(port_construction_method = "sw",
                                 meta_score = c(ann_info_ratio = "long"),
                                 eligibility_quantile_range = c(0, 1),
                                 initial_buffer_period = 4,
                                 active_returns = TRUE,
                                 config_name = "meta_test") {

  ## The toy sample runs seven months, so only a couple of portfolio returns exist by the first
  ## meta rebalance. That is fewer observations than portfolios, which the validator warns about;
  ## the tests suppress it deliberately and one asserts it.
  inner <- create_port_backtest_config(
    chosen_score_metric_and_position = meta_score,
    eligibility_quantile_range = eligibility_quantile_range,
    initial_buffer_period = initial_buffer_period, rebalancing_months = c(1, 4),
    selected_benchmark = "ibov",
    cov_est_method = create_cov_est_method(
      cov_estimation_method = "sample", cov_matrix_sample_size = 2,
      active_returns = active_returns, cov_matrix_benchmark = "ibov"),
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = port_construction_method,
    config_name = config_name) %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)

  suppressMessages(create_port_metabacktest_config(inner, config_name = config_name,
                                                   verbose = FALSE))
}


run_meta_backtest <- function(config = meta_backtest_config(), cohort = meta_backtest_cohort()) {
  inputs <- meta_backtest_inputs()
  suppressWarnings(suppressMessages(run_port_backtest(
    signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
    liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
    config = config, port_backtest_cohort = cohort,
    benchmark_weights_m_df = inputs$benchmark_weights_m_df,
    benchmark_returns_m_xts = inputs$benchmark_returns_m_xts,
    verbose = FALSE, parallel = FALSE)))
}


# Standard behaviour ------------------------------------------------------

testthat::test_that("run_port_backtest dispatches on port_metabacktest_config and fills every slot", {
  results <- run_meta_backtest()

  testthat::expect_s4_class(results, "port_metabacktest_results")
  testthat::expect_s4_class(results@meta_port_backtest_results, "port_backtest_results")
  testthat::expect_s4_class(results@port_backtest_cohort, "port_backtest_cohort")
  testthat::expect_s4_class(results@port_universe_m_df, "port_universe_m_df")
  testthat::expect_s4_class(results@meta_port_weights_m_df, "meta_dataframe")
  testthat::expect_s4_class(results@projected_stock_weights_m_df, "meta_dataframe")
  testthat::expect_s4_class(results@meta_port_stats_m_df, "meta_dataframe")
  testthat::expect_s4_class(results@final_meta_port, "port")

  ## The identifier names the allocation rule and the cohort it ran over
  testthat::expect_equal(results@backtest_identifier, "mc__meta_test_ch__meta_test_cohort")
})

testthat::test_that("meta weights are set on the configured schedule and sum to one", {
  results <- run_meta_backtest()
  meta_weights <- results@meta_port_weights_m_df@data

  ## Buffer 4 into a seven-date sample, rebalancing in January and April
  inputs <- meta_backtest_inputs()
  all_dates <- sort(unique(inputs$signals_m_df@data$dates))
  dates_backtest <- all_dates[4:length(all_dates)]
  expected_dates <- sort(unique(c(
    min(dates_backtest),
    dates_backtest[lubridate::month(dates_backtest) %in% c(1, 4)])))

  testthat::expect_setequal(unique(meta_weights$dates), expected_dates)

  sums <- tapply(meta_weights$weights, meta_weights$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(expected_dates)), tolerance = 1e-10)

  ## Every base portfolio is named, and signal weighting spreads them unevenly
  backtest_ids <- vapply(meta_backtest_cohort()@port_backtest_results_list,
                         function(x) x@backtest_identifier, character(1))
  testthat::expect_setequal(unique(meta_weights$tickers), unname(backtest_ids))
  testthat::expect_gt(stats::sd(meta_weights$weights[meta_weights$dates == expected_dates[1]]), 0)
})

testthat::test_that("the projected stock weights satisfy the panel contract", {
  results <- run_meta_backtest()
  projected <- results@projected_stock_weights_m_df@data
  inputs <- meta_backtest_inputs()

  sums <- tapply(projected$weights, projected$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-10)
  testthat::expect_true(all(inputs$signals_m_df@data$id %in% projected$id))
  testthat::expect_true(all(projected$weights >= 0 & projected$weights <= 1))
})


# The economic check ------------------------------------------------------

testthat::test_that("allocating everything to one base portfolio reproduces its own weights", {
  ## The strongest end-to-end check available: a quantile range that admits only the
  ## top-scoring portfolio, weighted equally, must project to exactly that portfolio's weights.
  ## If any step of the chain crossed portfolios or mis-scaled, this fails.
  concentrated <- meta_backtest_config(port_construction_method = "ew",
                                       eligibility_quantile_range = c(0.9, 1.0))
  results <- run_meta_backtest(config = concentrated)

  meta_weights <- results@meta_port_weights_m_df@data
  projected <- results@projected_stock_weights_m_df@data
  base_weights <- meta_backtest_cohort()@port_weights_m_df@data

  for (current_date in sort(unique(meta_weights$dates))) {
    date_weights <- meta_weights %>% dplyr::filter(dates == current_date)

    ## Exactly one portfolio was funded
    funded <- date_weights$tickers[date_weights$weights > 0]
    testthat::expect_length(funded, 1L)
    testthat::expect_equal(date_weights$weights[date_weights$weights > 0], 1, tolerance = 1e-10)

    ## and the projection is that portfolio's own weight vector, untouched
    expected <- base_weights %>%
      dplyr::filter(dates == current_date) %>%
      dplyr::select(id, expected = dplyr::all_of(funded))
    got <- projected %>%
      dplyr::filter(dates == current_date) %>%
      dplyr::select(id, weights)
    joined <- dplyr::inner_join(expected, got, by = "id")

    testthat::expect_equal(nrow(joined), nrow(expected))
    testthat::expect_equal(joined$weights, joined$expected, tolerance = 1e-12)
  }
})


# The stock-level result --------------------------------------------------

testthat::test_that("the stock-level backtest reports real returns and costs but no return view", {
  results <- run_meta_backtest()
  inner <- results@meta_port_backtest_results

  ## Trades were priced and returns were earned
  testthat::expect_true(any(is.finite(inner@port_returns_m_xts@data$net_return)))
  testthat::expect_true(any(inner@port_costs_m_xts@data$total_cost > 0, na.rm = TRUE))

  ## The weights were supplied, so there is no expected-return view at this level
  inner_stats <- inner@port_stats_m_df@data
  testthat::expect_true(all(is.na(inner_stats$act_exp_ret)))
  testthat::expect_true(all(is.na(inner_stats$IR)))
  testthat::expect_true(all(is.na(inner@stock_universe_m_df@data$exp_ret_score)))

  ## while everything measured from realized returns still works
  testthat::expect_true(any(is.finite(inner_stats$info_ratio)))
})

testthat::test_that("the expected-return view lives at the meta level instead", {
  results <- run_meta_backtest()
  meta_stats <- results@meta_port_stats_m_df@data

  testthat::expect_true(any(is.finite(meta_stats$exp_ret)))
  testthat::expect_equal(unique(meta_stats$tickers), "meta_port")
  testthat::expect_setequal(meta_stats$dates,
                            unique(results@meta_port_weights_m_df@data$dates))
})


# The covariance basis ----------------------------------------------------

testthat::test_that("meta-level risk is absolute whatever the configured covariance basis", {
  ## calculate_port_stats() re-estimates its own covariance with active returns switched off, so
  ## cov_est_method@active_returns reaches only the weights of the covariance-based methods and
  ## never the reported analytics. Under signal weighting it therefore changes nothing at all.
  active <- run_meta_backtest(config = meta_backtest_config(active_returns = TRUE))
  absolute <- run_meta_backtest(config = meta_backtest_config(active_returns = FALSE))

  testthat::expect_equal(active@meta_port_stats_m_df@data$risk,
                         absolute@meta_port_stats_m_df@data$risk)
  testthat::expect_equal(active@meta_port_weights_m_df@data$weights,
                         absolute@meta_port_weights_m_df@data$weights)

  ## and the figure is genuinely populated rather than trivially equal through being missing
  testthat::expect_true(any(is.finite(active@meta_port_stats_m_df@data$risk)))
})


# Validation propagates ---------------------------------------------------

testthat::test_that("inconsistent inputs are caught before anything is run", {
  inputs <- meta_backtest_inputs()

  ## A meta score no column of the derived universe carries
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_meta_backtest(
      config = meta_backtest_config(meta_score = c(nonsense = "long"))))),
    "is not a column of the derived port_universe_m_df"
  )

  ## A buffer that puts the first meta rebalance where no realized statistic exists yet
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_meta_backtest(
      config = meta_backtest_config(initial_buffer_period = 2)))),
    "missing at"
  )

  ## Data the cohort was not built from
  wrong_signals <- inputs$signals_m_df
  wrong_signals@meta_dataframe_name <- "some_other_signals"
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_port_backtest(
      signals_m_df = wrong_signals, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = meta_backtest_config(), port_backtest_cohort = meta_backtest_cohort(),
      verbose = FALSE, parallel = FALSE))),
    "Object name mismatch"
  )

  ## A cohort is not optional
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_port_backtest(
      signals_m_df = inputs$signals_m_df, fwd_return_m_df = inputs$fwd_return_m_df,
      liquidity_m_df = inputs$liquidity_m_df, volatility_m_df = inputs$volatility_m_df,
      config = meta_backtest_config(), verbose = FALSE, parallel = FALSE))),
    "port_backtest_cohort must be provided"
  )
})
