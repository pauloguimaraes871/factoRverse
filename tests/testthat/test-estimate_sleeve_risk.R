# Fixtures ----------------------------------------------------------------
# A synthetic sleeve, so the expected risk can be computed independently rather than compared
# against the function's own output. Returns are in percentage points, as everywhere else.

sleeve_stocks <- c("AAA", "BBB", "CCC")
sleeve_monthly_dates <- seq(as.Date("2023-01-15"), by = "month", length.out = 4L)
sleeve_daily_dates <- seq(as.Date("2022-06-01"), by = "day", length.out = 300L)


make_sleeve_daily_returns <- function(seed = 11) {
  set.seed(seed)
  returns <- cbind(
    AAA = stats::rnorm(length(sleeve_daily_dates), 0.05, 1.4),
    BBB = stats::rnorm(length(sleeve_daily_dates), 0.04, 2.1),
    CCC = stats::rnorm(length(sleeve_daily_dates), 0.03, 0.9)
  )
  xts::xts(returns, order.by = sleeve_daily_dates)
}


make_sleeve_results <- function(port_weights = c(0.5, 0.5, 0),
                                bench_weights = c(0.2, 0.3, 0.5),
                                monthly_active = NULL) {

  weights_df <- expand.grid(tickers = sleeve_stocks, dates = sleeve_monthly_dates,
                            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  weights_df$eop_port_weights <- rep(port_weights, times = length(sleeve_monthly_dates))
  weights_df$bench_weights <- rep(bench_weights, times = length(sleeve_monthly_dates))
  weights_df$id <- paste0(weights_df$tickers, "-", weights_df$dates)
  weights_df <- weights_df[order(weights_df$id),
                           c("id", "tickers", "dates", "eop_port_weights", "bench_weights")]
  rownames(weights_df) <- NULL

  if (is.null(monthly_active)) monthly_active <- c(1.2, -0.8, 2.1, -1.5)
  returns_xts <- xts::xts(
    data.frame(raw_return = monthly_active + 1,
               net_return = monthly_active + 0.9,
               raw_active_return = monthly_active,
               net_active_return = monthly_active - 0.1),
    order.by = sleeve_monthly_dates)

  methods::new(
    "port_backtest_results",
    port_backtest_config = NULL,
    port_weights_m_df = suppressWarnings(suppressMessages(
      create_meta_dataframe(weights_df, meta_dataframe_name = "sleeve", type = "weights"))),
    transactions_log = methods::new("transactions_log"),
    port_costs_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      xts::xts(data.frame(total_cost = rep(0.1, length(sleeve_monthly_dates))),
               order.by = sleeve_monthly_dates), type = "metrics", meta_xts_name = "sleeve"))),
    port_metrics_m_xts = NULL,
    port_returns_m_xts = suppressWarnings(suppressMessages(
      create_meta_xts(returns_xts, type = "metrics", meta_xts_name = "sleeve"))),
    final_stock_port = NULL,
    port_construction_method = "sw",
    stock_universe_m_df = NULL,
    final_stock_universe_m_d_ref = NULL,
    port_stats_m_df = NULL,
    final_port_stats_m_d_ref = NULL,
    port_backtest_workflow = list(list(selected_benchmark = "ibov")),
    backtest_identifier = "sleeve",
    update = TRUE
  )
}


## The expected figure, computed here rather than taken from the function under test.
## estimate_covariance_matrix() reads cov_matrix_sample_size as a lookback offset rather than an
## observation count, so a size of 60 samples 61 rows: the 60 periods back, plus the date itself.
## That is the package's convention throughout, so it is mirrored here rather than corrected.
COV_SAMPLE_OFFSET <- 1L

expected_ex_ante_risk <- function(daily_returns, weights, current_date, window) {
  sample_xts <- daily_returns[zoo::index(daily_returns) <= current_date, , drop = FALSE]
  sample_xts <- utils::tail(sample_xts, window + COV_SAMPLE_OFFSET)
  covariance <- stats::cov(as.matrix(sample_xts))
  weights <- weights[colnames(covariance)]
  sqrt(as.numeric(t(weights) %*% covariance %*% weights)) * sqrt(252)
}


ex_ante_params <- function(target_metric = "tracking_error", window = 60, ...) {
  create_cml_parameters(
    residual_ticker = "BOVA11", target = 4, target_metric = target_metric,
    vol_cov_est_method = create_cov_est_method("sample", window, FALSE, NULL), ...)
}


# Ex-ante estimation ------------------------------------------------------

testthat::test_that("a volatility target uses the sleeve's own weights", {
  daily_returns <- make_sleeve_daily_returns()
  sleeve <- make_sleeve_results(port_weights = c(0.5, 0.5, 0))
  current_date <- sleeve_monthly_dates[3]

  risk <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date,
    cml_params = ex_ante_params(target_metric = "volatility"),
    risky_port_backtest_results = sleeve,
    daily_stock_returns_m_xts = daily_returns))

  ## Only the two held names carry weight, so only they enter the covariance
  expected <- expected_ex_ante_risk(
    daily_returns[, c("AAA", "BBB")], c(AAA = 0.5, BBB = 0.5), current_date, 60)

  testthat::expect_equal(risk, expected, tolerance = 1e-8)
})

testthat::test_that("a tracking-error target uses active weights instead", {
  daily_returns <- make_sleeve_daily_returns()
  port_weights <- c(0.5, 0.5, 0)
  bench_weights <- c(0.2, 0.3, 0.5)
  sleeve <- make_sleeve_results(port_weights, bench_weights)
  current_date <- sleeve_monthly_dates[3]

  risk <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date,
    cml_params = ex_ante_params(target_metric = "tracking_error"),
    risky_port_backtest_results = sleeve,
    daily_stock_returns_m_xts = daily_returns))

  ## Active weights: portfolio minus benchmark, so CCC enters at -0.5 despite being unheld
  active <- stats::setNames(port_weights - bench_weights, sleeve_stocks)
  expected <- expected_ex_ante_risk(daily_returns, active, current_date, 60)

  testthat::expect_equal(risk, expected, tolerance = 1e-8)

  ## and the two metrics genuinely differ, so the comparison above is not vacuous
  volatility <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date,
    cml_params = ex_ante_params(target_metric = "volatility"),
    risky_port_backtest_results = sleeve,
    daily_stock_returns_m_xts = daily_returns))
  testthat::expect_false(isTRUE(all.equal(risk, volatility)))
})

testthat::test_that("the estimate is annualised from daily observations", {
  daily_returns <- make_sleeve_daily_returns()
  sleeve <- make_sleeve_results(port_weights = c(0.5, 0.5, 0))
  current_date <- sleeve_monthly_dates[3]

  risk <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date,
    cml_params = ex_ante_params(target_metric = "volatility"),
    risky_port_backtest_results = sleeve,
    daily_stock_returns_m_xts = daily_returns))

  ## The same quadratic form without the annualisation factor
  sample_xts <- utils::tail(
    daily_returns[zoo::index(daily_returns) <= current_date, c("AAA", "BBB"), drop = FALSE],
    60 + COV_SAMPLE_OFFSET)
  weights <- c(0.5, 0.5)
  daily_risk <- sqrt(as.numeric(t(weights) %*% stats::cov(as.matrix(sample_xts)) %*% weights))

  testthat::expect_equal(risk, daily_risk * sqrt(252), tolerance = 1e-8)
  testthat::expect_equal(risk / daily_risk, sqrt(252), tolerance = 1e-10)
})

testthat::test_that("a single-name sleeve is measured over the configured window", {
  ## estimate_covariance_matrix() short-circuits to stats::var() when exactly one ticker is
  ## passed. That branch used to ignore cov_matrix_sample_size and take the whole series, so a
  ## one-name sleeve was measured over a longer window than the same sleeve holding two, and the
  ## two figures were not comparable. Both paths now select the window before the branch.
  daily_returns <- make_sleeve_daily_returns()
  sleeve <- make_sleeve_results(port_weights = c(1, 0, 0))
  current_date <- sleeve_monthly_dates[3]

  risk <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date,
    cml_params = ex_ante_params(target_metric = "volatility"),
    risky_port_backtest_results = sleeve,
    daily_stock_returns_m_xts = daily_returns))

  full_history <- daily_returns[zoo::index(daily_returns) <= current_date, "AAA", drop = FALSE]
  windowed <- utils::tail(full_history, 60 + COV_SAMPLE_OFFSET)

  testthat::expect_equal(risk, stats::sd(as.numeric(windowed)) * sqrt(252), tolerance = 1e-8)

  ## The window has to bind here, or the assertion above would hold whether or not it is applied
  testthat::expect_lt(nrow(windowed), nrow(full_history))
  testthat::expect_false(isTRUE(all.equal(
    risk, stats::sd(as.numeric(full_history)) * sqrt(252))))

  ## and the one-name figure now matches the same expectation helper every multi-name test uses,
  ## which is the point of the fix: breadth no longer changes the period being measured
  testthat::expect_equal(
    risk,
    expected_ex_ante_risk(daily_returns[, "AAA", drop = FALSE], c(AAA = 1), current_date, 60),
    tolerance = 1e-8)
})

testthat::test_that("only data up to the date is used", {
  daily_returns <- make_sleeve_daily_returns()
  sleeve <- make_sleeve_results(port_weights = c(0.5, 0.5, 0))
  current_date <- sleeve_monthly_dates[2]

  baseline <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date, cml_params = ex_ante_params("volatility"),
    risky_port_backtest_results = sleeve, daily_stock_returns_m_xts = daily_returns))

  ## Corrupting everything after the date must leave the estimate untouched
  perturbed <- daily_returns
  later <- zoo::index(perturbed) > current_date
  perturbed[later, ] <- perturbed[later, ] * 50

  after <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date, cml_params = ex_ante_params("volatility"),
    risky_port_backtest_results = sleeve, daily_stock_returns_m_xts = perturbed))

  testthat::expect_equal(after, baseline)
})

testthat::test_that("the window length is respected", {
  daily_returns <- make_sleeve_daily_returns()
  sleeve <- make_sleeve_results(port_weights = c(0.5, 0.5, 0))
  current_date <- sleeve_monthly_dates[3]

  short <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date, cml_params = ex_ante_params("volatility", window = 30),
    risky_port_backtest_results = sleeve, daily_stock_returns_m_xts = daily_returns))
  long <- suppressMessages(estimate_sleeve_risk(
    current_date = current_date, cml_params = ex_ante_params("volatility", window = 200),
    risky_port_backtest_results = sleeve, daily_stock_returns_m_xts = daily_returns))

  testthat::expect_equal(short, expected_ex_ante_risk(
    daily_returns[, c("AAA", "BBB")], c(AAA = 0.5, BBB = 0.5), current_date, 30),
    tolerance = 1e-8)
  testthat::expect_false(isTRUE(all.equal(short, long)))
})

testthat::test_that("too little history returns nothing rather than a short-window estimate", {
  daily_returns <- make_sleeve_daily_returns()
  sleeve <- make_sleeve_results()

  ## An early date has fewer observations behind it than the window asks for
  early <- zoo::index(daily_returns)[10]
  risk <- suppressMessages(estimate_sleeve_risk(
    current_date = early, cml_params = ex_ante_params("volatility", window = 60),
    risky_port_backtest_results = sleeve, daily_stock_returns_m_xts = daily_returns))

  testthat::expect_true(is.na(risk))
})

testthat::test_that("ex-ante estimation refuses to proceed without daily returns", {
  testthat::expect_error(
    estimate_sleeve_risk(current_date = sleeve_monthly_dates[3],
                         cml_params = ex_ante_params("volatility"),
                         risky_port_backtest_results = make_sleeve_results(),
                         daily_stock_returns_m_xts = NULL),
    "daily_stock_returns_m_xts is required"
  )
})

testthat::test_that("a tracking-error target needs the sleeve to carry benchmark weights", {
  sleeve <- make_sleeve_results()
  stripped <- sleeve@port_weights_m_df
  stripped@data$bench_weights <- NULL
  sleeve@port_weights_m_df <- stripped

  testthat::expect_error(
    suppressMessages(estimate_sleeve_risk(
      current_date = sleeve_monthly_dates[3], cml_params = ex_ante_params("tracking_error"),
      risky_port_backtest_results = sleeve,
      daily_stock_returns_m_xts = make_sleeve_daily_returns())),
    "no bench_weights"
  )
})


# Realized rolling --------------------------------------------------------

testthat::test_that("the rolling estimate is the sleeve's own return volatility, annualised", {
  monthly_active <- c(1.2, -0.8, 2.1, -1.5)
  sleeve <- make_sleeve_results(monthly_active = monthly_active)
  params <- create_cml_parameters("BOVA11", target = 4, vol_source = "realized_rolling",
                                  vol_window = 3)

  risk <- suppressMessages(estimate_sleeve_risk(
    current_date = sleeve_monthly_dates[4], cml_params = params,
    risky_port_backtest_results = sleeve, return_basis = "net"))

  ## A tracking-error target reads the net active series, and monthly data annualises by sqrt(12)
  expected <- stats::sd(utils::tail(monthly_active - 0.1, 3)) * sqrt(12)
  testthat::expect_equal(risk, expected, tolerance = 1e-10)
})

testthat::test_that("a volatility target reads the total return series instead", {
  monthly_active <- c(1.2, -0.8, 2.1, -1.5)
  sleeve <- make_sleeve_results(monthly_active = monthly_active)
  params <- create_cml_parameters("CASH", target = 10, target_metric = "volatility",
                                  vol_source = "realized_rolling", vol_window = 3)

  risk <- suppressMessages(estimate_sleeve_risk(
    current_date = sleeve_monthly_dates[4], cml_params = params,
    risky_port_backtest_results = sleeve, return_basis = "net"))

  testthat::expect_equal(risk, stats::sd(utils::tail(monthly_active + 0.9, 3)) * sqrt(12),
                         tolerance = 1e-10)
})

testthat::test_that("the rolling window uses only returns up to the date", {
  sleeve <- make_sleeve_results(monthly_active = c(1.2, -0.8, 2.1, -1.5))
  params <- create_cml_parameters("BOVA11", target = 4, vol_source = "realized_rolling",
                                  vol_window = 3)

  at_third <- suppressMessages(estimate_sleeve_risk(
    current_date = sleeve_monthly_dates[3], cml_params = params,
    risky_port_backtest_results = sleeve))
  at_fourth <- suppressMessages(estimate_sleeve_risk(
    current_date = sleeve_monthly_dates[4], cml_params = params,
    risky_port_backtest_results = sleeve))

  testthat::expect_equal(at_third, stats::sd(c(1.2, -0.8, 2.1) - 0.1) * sqrt(12),
                         tolerance = 1e-10)
  testthat::expect_false(isTRUE(all.equal(at_third, at_fourth)))
})

testthat::test_that("a window longer than the history returns nothing", {
  sleeve <- make_sleeve_results()
  params <- create_cml_parameters("BOVA11", target = 4, vol_source = "realized_rolling",
                                  vol_window = 12)

  testthat::expect_true(is.na(suppressMessages(estimate_sleeve_risk(
    current_date = sleeve_monthly_dates[4], cml_params = params,
    risky_port_backtest_results = sleeve))))
})


# Supplied ----------------------------------------------------------------

testthat::test_that("a supplied series is read at the date and taken as annualised", {
  params <- create_cml_parameters("BOVA11", target = 4, vol_source = "supplied")
  vol_m_df <- data.frame(
    id = paste0("sleeve-", sleeve_monthly_dates), tickers = "sleeve",
    dates = sleeve_monthly_dates, ann_track_err = c(8, 6, 4, 12),
    stringsAsFactors = FALSE)

  for (i in seq_along(sleeve_monthly_dates)) {
    risk <- suppressMessages(estimate_sleeve_risk(
      current_date = sleeve_monthly_dates[i], cml_params = params,
      risky_port_backtest_results = make_sleeve_results(), vol_m_df = vol_m_df))
    testthat::expect_equal(risk, vol_m_df$ann_track_err[i])
  }

  ## A date the series does not cover has no estimate
  testthat::expect_true(is.na(suppressMessages(estimate_sleeve_risk(
    current_date = as.Date("2030-01-15"), cml_params = params,
    risky_port_backtest_results = make_sleeve_results(), vol_m_df = vol_m_df))))
})

testthat::test_that("a supplied source needs a series, and an unambiguous one", {
  params <- create_cml_parameters("BOVA11", target = 4, vol_source = "supplied")

  testthat::expect_error(
    estimate_sleeve_risk(current_date = sleeve_monthly_dates[1], cml_params = params,
                         risky_port_backtest_results = make_sleeve_results()),
    "vol_m_df must be supplied")

  two_columns <- data.frame(
    id = paste0("sleeve-", sleeve_monthly_dates), tickers = "sleeve",
    dates = sleeve_monthly_dates, a = 1, b = 2, stringsAsFactors = FALSE)
  testthat::expect_error(
    estimate_sleeve_risk(current_date = sleeve_monthly_dates[1], cml_params = params,
                         risky_port_backtest_results = make_sleeve_results(),
                         vol_m_df = two_columns),
    "exactly one risk column")
})


# risk_to_weight ----------------------------------------------------------

testthat::test_that("the rule is target over risk, raised to the exponent", {
  linear <- create_cml_parameters("BOVA11", target = 4, p = 1)
  quadratic <- create_cml_parameters("BOVA11", target = 4, p = 2)

  ## Risk at the target gives full exposure whatever the exponent
  testthat::expect_equal(risk_to_weight(4, linear), 1)
  testthat::expect_equal(risk_to_weight(4, quadratic), 1)

  ## Twice the target halves it at p = 1 and quarters it at p = 2
  testthat::expect_equal(risk_to_weight(8, linear), 0.5)
  testthat::expect_equal(risk_to_weight(8, quadratic), 0.25)

  ## Risk below the target would ask for leverage, which the default cap refuses
  testthat::expect_equal(risk_to_weight(2, linear), 1)
})

testthat::test_that("the bounds clip the rule in both directions", {
  bounded <- create_cml_parameters("BOVA11", target = 4, min_weight = 0.5, max_weight = 0.9)

  testthat::expect_equal(risk_to_weight(40, bounded), 0.5)
  testthat::expect_equal(risk_to_weight(1, bounded), 0.9)
  ## and leave an interior value alone
  testthat::expect_equal(risk_to_weight(5, bounded), 0.8)
})

testthat::test_that("a risk that cannot be used yields no weight rather than a guess", {
  params <- create_cml_parameters("BOVA11", target = 4)

  testthat::expect_true(is.na(risk_to_weight(NA_real_, params)))
  testthat::expect_true(is.na(risk_to_weight(0, params)))
  testthat::expect_true(is.na(risk_to_weight(-1, params)))
  testthat::expect_true(is.na(risk_to_weight(Inf, params)))
})
