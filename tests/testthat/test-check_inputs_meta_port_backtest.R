# Fixtures ----------------------------------------------------------------
# The cohort fixture mirrors the one in test-derive_port_universe_m_df.R: the validator reads
# port_backtest_results_list, the cost and metric lists and backtest_workflow_common, and
# port_backtest_cohort validity only constrains cohort_name.

meta_check_stats_m_df <- function(rebalance_dates, seed_value) {

  stats_df <- data.frame(
    tickers = rep(c("raw_return", "net_return"), each = length(rebalance_dates)),
    dates   = rep(rebalance_dates, times = 2),
    stringsAsFactors = FALSE
  )
  n <- nrow(stats_df)

  ## Realized block, undefined at the first rebalance exactly as a real backtest leaves it
  stats_df$ann_info_ratio <- seq_len(n) * seed_value
  stats_df$ann_info_ratio[stats_df$dates == min(rebalance_dates)] <- NA_real_
  stats_df$track_err <- seq_len(n) * seed_value * 2
  stats_df$track_err[stats_df$dates == min(rebalance_dates)] <- NA_real_

  ## Position-derived block, computable from the first rebalance onwards
  stats_df$act_exp_ret <- rep(seq_along(rebalance_dates) * seed_value * 3, times = 2)
  stats_df$act_risk <- rep(seq_along(rebalance_dates) * seed_value * 4, times = 2)
  stats_df$IR <- rep(seq_along(rebalance_dates) * seed_value * 5, times = 2)

  stats_df$id <- paste0(stats_df$tickers, "-", stats_df$dates)
  stats_df <- stats_df[order(stats_df$id), ]
  rownames(stats_df) <- NULL
  stats_df <- stats_df[, c("id", "tickers", "dates",
                           setdiff(names(stats_df), c("id", "tickers", "dates")))]

  suppressWarnings(suppressMessages(
    create_meta_dataframe(stats_df, meta_dataframe_name = "meta_check_stats")))
}


meta_check_result <- function(backtest_identifier, rebalance_dates, seed_value) {
  methods::new(
    "port_backtest_results",
    port_backtest_config = NULL,
    port_weights_m_df = suppressWarnings(suppressMessages(create_meta_dataframe(
      data.frame(id = paste0("AAA-", rebalance_dates), tickers = "AAA",
                 dates = rebalance_dates, eop_port_weights = 1, stringsAsFactors = FALSE)))),
    transactions_log = methods::new("transactions_log"),
    port_costs_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      xts::xts(data.frame(total_cost = rep(0.1, length(rebalance_dates))),
               order.by = rebalance_dates), type = "metrics", meta_xts_name = "toy"))),
    port_metrics_m_xts = NULL,
    port_returns_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      xts::xts(data.frame(net_return = rep(1, length(rebalance_dates))),
               order.by = rebalance_dates), type = "metrics", meta_xts_name = "toy"))),
    final_stock_port = NULL,
    port_construction_method = "ew",
    stock_universe_m_df = NULL,
    final_stock_universe_m_d_ref = NULL,
    port_stats_m_df = meta_check_stats_m_df(rebalance_dates, seed_value),
    final_port_stats_m_d_ref = NULL,
    port_backtest_workflow = list(list(selected_benchmark = "ibov")),
    backtest_identifier = backtest_identifier,
    update = TRUE
  )
}


meta_check_cohort <- function(n_ports = 4L, n_dates = 24L, rebalance_every = 6L,
                              base_initial_buffer_period = 2,
                              all_na_exante = FALSE) {

  dates_backtest <- seq(as.Date("2022-01-15"), by = "month", length.out = n_dates)
  rebalance_dates <- dates_backtest[seq(1L, n_dates, by = rebalance_every)]
  port_names <- c("bt_alpha", "bt_beta", "bt_gamma", "bt_delta")[seq_len(n_ports)]

  results_list <- lapply(seq_along(port_names), function(i) {
    result <- meta_check_result(port_names[i], rebalance_dates, seed_value = i)
    if (all_na_exante) {
      ## Reproduces a cohort whose backtests ran without daily returns: no covariance matrix,
      ## so the whole position-derived block is missing
      stats_obj <- result@port_stats_m_df
      stats_obj@data$act_risk <- NA_real_
      stats_obj@data$IR <- NA_real_
      result@port_stats_m_df <- stats_obj
    }
    result
  })
  names(results_list) <- port_names

  cost_dates <- dates_backtest + 1
  cost_mat <- matrix(0.1, nrow = length(cost_dates), ncol = n_ports,
                     dimnames = list(NULL, port_names))

  methods::new(
    "port_backtest_cohort",
    cohort_name = "meta_check_cohort",
    port_backtest_results_list = results_list,
    port_weights_m_df = suppressWarnings(suppressMessages(create_meta_dataframe(
      data.frame(id = paste0("AAA-", dates_backtest), tickers = "AAA",
                 dates = dates_backtest, bt_alpha = 1, stringsAsFactors = FALSE)))),
    port_costs_m_xts_list = list(total_cost_m_xts = suppressWarnings(suppressMessages(
      create_meta_xts(xts::xts(cost_mat, order.by = cost_dates), type = "metrics",
                      meta_xts_name = "meta_check_cohort", source = port_names)))),
    port_returns_m_xts_list = list(),
    port_metrics_m_xts_list = list(),
    port_stats_m_xts_nested_list = list(),
    backtest_workflow_common = list(
      selected_benchmark = "ibov",
      dates_backtest = dates_backtest,
      initial_buffer_period = base_initial_buffer_period,
      signals_object_name = "toy_signals",
      fwd_return_object_name = "toy_fwd_return",
      liquidity_object_name = "toy_liquidity",
      volatility_object_name = "toy_volatility"
    )
  )
}


meta_check_universe <- function(cohort) {
  suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)))
}


meta_check_base_objects <- function(cohort, names_override = list()) {

  ## signals_m_df must span the full sample, since the meta buffer indexes into it
  all_dates <- seq(as.Date("2021-12-15"), by = "month", length.out = 25L)
  build <- function(object_name) {
    df <- data.frame(
      id = paste0("AAA-", all_dates), tickers = "AAA", dates = all_dates,
      value = seq_along(all_dates), stringsAsFactors = FALSE)
    suppressWarnings(suppressMessages(
      create_meta_dataframe(df, meta_dataframe_name = object_name)))
  }

  objects <- list(
    signals_m_df = build(names_override$signals_object_name %||% "toy_signals"),
    fwd_return_m_df = build(names_override$fwd_return_object_name %||% "toy_fwd_return"),
    liquidity_m_df = build(names_override$liquidity_object_name %||% "toy_liquidity"),
    volatility_m_df = build(names_override$volatility_object_name %||% "toy_volatility")
  )
  objects
}


meta_check_config <- function(port_construction_method = "ew",
                              meta_score = c(ann_info_ratio = "long"),
                              initial_buffer_period = 8,
                              rebalancing_months = c(1, 7),
                              selected_benchmark = "ibov") {

  cov_est_method <- create_cov_est_method(
    cov_estimation_method = "sample", cov_matrix_sample_size = 36,
    active_returns = !is.null(selected_benchmark), cov_matrix_benchmark = selected_benchmark)

  inner <- create_port_backtest_config(
    chosen_score_metric_and_position = meta_score,
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = initial_buffer_period,
    rebalancing_months = rebalancing_months,
    selected_benchmark = selected_benchmark,
    cov_est_method = cov_est_method,
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = port_construction_method,
    config_name = "meta_check"
  )

  suppressMessages(create_port_metabacktest_config(inner, verbose = FALSE))
}


run_check <- function(cohort = NULL, config = NULL, universe = NULL, base = NULL, ...) {
  if (is.null(cohort)) cohort <- meta_check_cohort()
  if (is.null(config)) config <- meta_check_config()
  if (is.null(universe)) universe <- meta_check_universe(cohort)
  if (is.null(base)) base <- meta_check_base_objects(cohort)

  check_inputs_meta_port_backtest(
    config = config, port_backtest_cohort = cohort, port_universe_m_df = universe,
    signals_m_df = base$signals_m_df, fwd_return_m_df = base$fwd_return_m_df,
    liquidity_m_df = base$liquidity_m_df, volatility_m_df = base$volatility_m_df,
    ...
  )
}


# Standard behaviour ------------------------------------------------------

testthat::test_that("a consistent set of inputs passes and returns the resolved schedule", {
  result <- suppressMessages(run_check(verbose = FALSE))

  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("meta_rebalance_dates", "max_stats_age_months"))
  testthat::expect_s3_class(result$meta_rebalance_dates, "Date")
  testthat::expect_gt(length(result$meta_rebalance_dates), 0L)
})

testthat::test_that("the resolved schedule matches how run_port_backtest_internal resolves it", {
  result <- suppressMessages(run_check(verbose = FALSE))

  ## Buffer 8 into a 25-date sample, rebalancing in January and July
  all_dates <- seq(as.Date("2021-12-15"), by = "month", length.out = 25L)
  dates_backtest <- all_dates[8:25]
  expected <- sort(unique(c(
    min(dates_backtest),
    dates_backtest[lubridate::month(dates_backtest) %in% c(1, 7)]
  )))

  testthat::expect_equal(result$meta_rebalance_dates, expected)
})

testthat::test_that("the meta score's basis is reported", {
  testthat::expect_message(run_check(), "REALIZED")
  testthat::expect_message(
    run_check(config = meta_check_config(meta_score = c(IR = "long"))), "EX-ANTE")
})


# Cohort size -------------------------------------------------------------

testthat::test_that("a cohort with fewer than two base portfolios is refused", {
  cohort <- meta_check_cohort(n_ports = 1L)
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_check(cohort = cohort, verbose = FALSE))),
    "at least two base portfolios"
  )
})

testthat::test_that("signal weighting over exactly two base portfolios warns but proceeds", {
  ## A pair such as a risky and a defensive sleeve is a normal allocation, so this must not block
  cohort <- meta_check_cohort(n_ports = 2L)

  testthat::expect_warning(
    result <- suppressMessages(run_check(
      cohort = cohort, config = meta_check_config(port_construction_method = "sw"),
      verbose = FALSE)),
    "ordering of the meta score, not its magnitude"
  )
  testthat::expect_gt(length(result$meta_rebalance_dates), 0L)

  ## The generic small-cohort caveat is about sensitivity to small changes, which is the opposite
  ## of what this case does, so it is not raised alongside the specific one
  warnings_raised <- testthat::capture_warnings(suppressMessages(run_check(
    cohort = cohort, config = meta_check_config(port_construction_method = "sw"),
    verbose = FALSE)))
  testthat::expect_length(warnings_raised, 1L)

  ## Other methods over the same pair get the generic caveat instead
  testthat::expect_warning(
    suppressMessages(run_check(cohort = cohort, verbose = FALSE)),
    "only 2 base portfolios"
  )
})

testthat::test_that("two-portfolio signal weighting is ordinal, which is what the warning says", {
  ## Pinning the behaviour the warning describes, so the message cannot drift from the code
  weights_for <- function(scores) {
    transformed <- signal_transform(scores, lower_quantile_winsorization = 0.025,
                                    upper_quantile_winsorization = 0.975)
    transformed / sum(transformed)
  }

  ## The gap between the scores does not change the split
  testthat::expect_equal(weights_for(c(1.0, 0.9)), weights_for(c(10, -50)), tolerance = 1e-9)
  testthat::expect_equal(weights_for(c(1.0, 0.9)), c(0.744521, 0.255479), tolerance = 1e-5)

  ## but the ordering does, and a tie splits evenly
  testthat::expect_equal(weights_for(c(0.9, 1.0)), rev(weights_for(c(1.0, 0.9))))
  testthat::expect_equal(weights_for(c(1.0, 1.0)), c(0.5, 0.5))

  ## Three portfolios do respond to the size of the gaps
  testthat::expect_false(isTRUE(all.equal(weights_for(c(1.0, 0.9, 0.8)),
                                          weights_for(c(1.0, 0.1, 0.05)))))
})

testthat::test_that("a small cross-section warns without blocking", {
  cohort <- meta_check_cohort(n_ports = 3L)
  testthat::expect_warning(
    result <- suppressMessages(run_check(cohort = cohort, verbose = FALSE)),
    "sensitive to small changes"
  )
  testthat::expect_gt(length(result$meta_rebalance_dates), 0L)

  ## Four or more passes quietly
  testthat::expect_silent(suppressMessages(run_check(verbose = FALSE)))
})


# Meta score --------------------------------------------------------------

testthat::test_that("a meta score that names no column is refused, listing what is available", {
  testthat::expect_error(
    suppressMessages(run_check(config = meta_check_config(meta_score = c(nonsense = "long")),
                               verbose = FALSE)),
    "is not a column of the derived port_universe_m_df"
  )
  testthat::expect_error(
    suppressMessages(run_check(config = meta_check_config(meta_score = c(nonsense = "long")),
                               verbose = FALSE)),
    "ann_info_ratio"
  )
})

testthat::test_that("a meta score that is missing everywhere is refused, naming the likely cause", {
  ## A cohort whose backtests ran without daily returns has no covariance matrix, so the whole
  ## position-derived block is NA even though the columns exist
  cohort <- meta_check_cohort(all_na_exante = TRUE)
  testthat::expect_error(
    suppressMessages(run_check(cohort = cohort,
                               config = meta_check_config(meta_score = c(IR = "long")),
                               verbose = FALSE)),
    "daily_stock_returns_m_xts"
  )

  ## while a realized score on the same cohort is fine
  testthat::expect_silent(suppressMessages(run_check(cohort = cohort, verbose = FALSE)))
})

testthat::test_that("a meta score missing on the rebalance schedule is refused", {
  ## Buffer 2 puts the first meta rebalance on the cohort's own first rebalance date, where no
  ## realized statistic exists yet
  testthat::expect_error(
    suppressWarnings(suppressMessages(run_check(
      config = meta_check_config(initial_buffer_period = 2, rebalancing_months = c(1, 7)),
      verbose = FALSE))),
    "missing at"
  )

  ## A position-derived score is available from the first rebalance, so it passes there
  testthat::expect_silent(suppressMessages(run_check(
    config = meta_check_config(initial_buffer_period = 2, meta_score = c(IR = "long")),
    verbose = FALSE)))
})


# Schedule and buffers ----------------------------------------------------

testthat::test_that("a meta buffer shorter than the base one is refused", {
  cohort <- meta_check_cohort(base_initial_buffer_period = 12)
  testthat::expect_error(
    suppressMessages(run_check(cohort = cohort,
                               config = meta_check_config(initial_buffer_period = 8),
                               verbose = FALSE)),
    "shorter than the base"
  )
})

testthat::test_that("a rebalance schedule reaching outside the cohort's dates is refused", {
  ## The cohort covers 2022-01-15 onward; a buffer of 1 puts the first meta rebalance on
  ## 2021-12-15, which the cohort never saw
  cohort <- meta_check_cohort(base_initial_buffer_period = 1)
  testthat::expect_error(
    suppressMessages(run_check(cohort = cohort,
                               config = meta_check_config(initial_buffer_period = 1),
                               verbose = FALSE)),
    "does not cover"
  )
})

testthat::test_that("a buffer longer than the sample is refused", {
  testthat::expect_error(
    suppressMessages(run_check(config = meta_check_config(initial_buffer_period = 99),
                               verbose = FALSE)),
    "exceeds the number of dates"
  )
})


# Staleness warns but never blocks ----------------------------------------

testthat::test_that("statistics are fresh when the two schedules agree", {
  ## The base portfolios rebalance every six months from January, so a meta schedule of January
  ## and July lands exactly on them and reads nothing stale
  result <- suppressMessages(run_check(max_stats_age_months = 0, verbose = FALSE))
  testthat::expect_equal(result$max_stats_age_months, 0)
})

testthat::test_that("stale statistics warn above the tolerance and never block", {
  ## March and September fall two months after each base rebalance, so every meta date reads
  ## statistics formed earlier
  misaligned <- meta_check_config(rebalancing_months = c(3, 9))

  testthat::expect_warning(
    result <- suppressMessages(
      run_check(config = misaligned, max_stats_age_months = 0, verbose = FALSE)),
    "above the 0-month tolerance"
  )
  ## Blocking would have prevented a return value
  testthat::expect_gt(length(result$meta_rebalance_dates), 0L)
  testthat::expect_equal(result$max_stats_age_months, 2)

  ## A generous tolerance stays quiet
  testthat::expect_silent(
    suppressMessages(run_check(config = misaligned, max_stats_age_months = 60, verbose = FALSE)))

  ## No tolerance reports the observed age without judging it
  testthat::expect_message(
    run_check(config = misaligned, max_stats_age_months = NULL), "month\\(s\\) old")
})

testthat::test_that("max_stats_age_months is validated", {
  testthat::expect_error(
    suppressMessages(run_check(max_stats_age_months = -1, verbose = FALSE)),
    "max_stats_age_months")
  testthat::expect_error(
    suppressMessages(run_check(max_stats_age_months = 1.5, verbose = FALSE)),
    "max_stats_age_months")
})


# Consistency with the cohort ---------------------------------------------

testthat::test_that("base data objects must be the ones the cohort was built from", {
  wrong_signals <- meta_check_base_objects(
    meta_check_cohort(), names_override = list(signals_object_name = "other_signals"))

  testthat::expect_error(
    suppressMessages(run_check(base = wrong_signals, verbose = FALSE)),
    "Object name mismatch for signals_object_name"
  )

  wrong_liquidity <- meta_check_base_objects(
    meta_check_cohort(), names_override = list(liquidity_object_name = "other_liquidity"))
  testthat::expect_error(
    suppressMessages(run_check(base = wrong_liquidity, verbose = FALSE)),
    "Object name mismatch for liquidity_object_name"
  )
})

testthat::test_that("the benchmark must agree between config and cohort", {
  testthat::expect_error(
    suppressMessages(run_check(
      config = meta_check_config(selected_benchmark = NULL), verbose = FALSE)),
    "selected_benchmark differs"
  )
})


# Argument classes --------------------------------------------------------

testthat::test_that("every argument is class-checked", {
  cohort <- meta_check_cohort()
  universe <- meta_check_universe(cohort)
  base <- meta_check_base_objects(cohort)
  config <- meta_check_config()

  testthat::expect_error(
    check_inputs_meta_port_backtest(
      config = list(), port_backtest_cohort = cohort, port_universe_m_df = universe,
      signals_m_df = base$signals_m_df, fwd_return_m_df = base$fwd_return_m_df,
      liquidity_m_df = base$liquidity_m_df, volatility_m_df = base$volatility_m_df,
      verbose = FALSE),
    "port_metabacktest_config")

  testthat::expect_error(
    check_inputs_meta_port_backtest(
      config = config, port_backtest_cohort = list(), port_universe_m_df = universe,
      signals_m_df = base$signals_m_df, fwd_return_m_df = base$fwd_return_m_df,
      liquidity_m_df = base$liquidity_m_df, volatility_m_df = base$volatility_m_df,
      verbose = FALSE),
    "port_backtest_cohort")

  testthat::expect_error(
    check_inputs_meta_port_backtest(
      config = config, port_backtest_cohort = cohort, port_universe_m_df = universe@data,
      signals_m_df = base$signals_m_df, fwd_return_m_df = base$fwd_return_m_df,
      liquidity_m_df = base$liquidity_m_df, volatility_m_df = base$volatility_m_df,
      verbose = FALSE),
    "port_universe_m_df")

  testthat::expect_error(
    suppressMessages(run_check(base = utils::modifyList(base, list(signals_m_df = "nope")),
                               verbose = FALSE)),
    "signals_m_df must be a meta_dataframe")

  testthat::expect_error(
    suppressMessages(run_check(benchmark_returns_m_xts = "nope", verbose = FALSE)),
    "benchmark_returns_m_xts must be a meta_xts")
})
