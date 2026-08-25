# Fixtures ----------------------------------------------------------------

timing_dates <- seq(as.Date("2022-01-15"), by = "month", length.out = 12L)


make_metric_m_df <- function(values, tickers = "risky", dates = timing_dates,
                             metric_name = "ann_track_err") {
  df <- expand.grid(tickers = tickers, dates = dates,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df[[metric_name]] <- values
  df$id <- paste0(df$tickers, "-", df$dates)
  df <- df[order(df$id), c("id", "tickers", "dates", metric_name)]
  rownames(df) <- NULL
  df
}


make_two_asset_metric <- function(risky_values, dates = timing_dates) {
  ## The cash line carries a metric only so the panel is complete; it is never mapped
  risky <- make_metric_m_df(risky_values, tickers = "risky", dates = dates)
  cash <- make_metric_m_df(rep(1, length(dates)), tickers = "cash", dates = dates)
  out <- rbind(risky, cash)
  out[order(out$id), ]
}


# The inverse mapping -----------------------------------------------------

testthat::test_that("inverse targeting halves exposure when the metric doubles the target", {
  ## Tracking error of 8 against a budget of 4 gives half the risky sleeve, and 4 gives all of it
  metric <- make_two_asset_metric(rep(c(8, 4), length.out = length(timing_dates)))

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4,
    residual_asset = "cash", verbose = FALSE))@data

  risky <- result %>% dplyr::filter(tickers == "risky") %>% dplyr::arrange(dates)
  cash <- result %>% dplyr::filter(tickers == "cash") %>% dplyr::arrange(dates)

  testthat::expect_equal(risky$weights, rep(c(0.5, 1.0), length.out = nrow(risky)))
  testthat::expect_equal(cash$weights, rep(c(0.5, 0.0), length.out = nrow(cash)))
})

testthat::test_that("a metric below the target does not lever the portfolio past fully invested", {
  ## A tracking error of 2 against a budget of 4 would ask for twice the exposure; the box stops it
  metric <- make_two_asset_metric(rep(2, length(timing_dates)))

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4,
    residual_asset = "cash", verbose = FALSE))@data

  testthat::expect_true(all(result$weights[result$tickers == "risky"] == 1))
  testthat::expect_true(all(result$weights[result$tickers == "cash"] == 0))
})

testthat::test_that("the exponent sets how hard the response is", {
  metric <- make_two_asset_metric(rep(8, length(timing_dates)))

  linear <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4, exponent = 1,
    residual_asset = "cash", verbose = FALSE))@data
  variance <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4, exponent = 2,
    residual_asset = "cash", verbose = FALSE))@data

  ## Volatility targeting gives 4/8; the inverse-variance response gives (4/8)^2
  testthat::expect_true(all(linear$weights[linear$tickers == "risky"] == 0.5))
  testthat::expect_true(all(variance$weights[variance$tickers == "risky"] == 0.25))
})

testthat::test_that("inverse targeting refuses a metric that cannot be a risk measure", {
  testthat::expect_error(
    derive_timing_weights(make_two_asset_metric(c(rep(4, 11), 0)), metric = "ann_track_err",
                          method = "inverse", target = 4, residual_asset = "cash", verbose = FALSE),
    "positive metric"
  )
  testthat::expect_error(
    derive_timing_weights(make_two_asset_metric(rep(4, 12)), metric = "ann_track_err",
                          method = "inverse", target = -1, residual_asset = "cash", verbose = FALSE),
    "target must be positive"
  )
})

testthat::test_that("inverse targeting will not calibrate its own target", {
  ## Setting the constant from the whole sample is the look-ahead this refuses to offer
  testthat::expect_error(
    derive_timing_weights(make_two_asset_metric(rep(4, 12)), metric = "ann_track_err",
                          method = "inverse", residual_asset = "cash", verbose = FALSE),
    "target, which is required"
  )
})


# The time-series adjusted mapping ----------------------------------------

testthat::test_that("ts_adjusted scores the metric against its own history, not a cross-section", {
  ## A metric that is flat and then jumps: the jump is what the z-score sees
  values <- c(rep(10, 8), 20, 20, 20, 20)
  metric <- make_two_asset_metric(values)

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "ts_adjusted", window = 4,
    center = 0.75, sensitivity = -0.25, residual_asset = "cash", verbose = FALSE))@data

  risky <- result %>% dplyr::filter(tickers == "risky") %>% dplyr::arrange(dates)

  ## The first three dates lack a full window and are dropped
  testthat::expect_equal(nrow(risky), length(values) - 3L)

  ## While the metric is flat the window has no dispersion, so the rule sits at its centre
  testthat::expect_equal(risky$weights[1], 0.75)

  ## A negative sensitivity means a metric high against its own history cuts exposure
  jump_row <- risky %>% dplyr::filter(dates == timing_dates[9])
  testthat::expect_lt(jump_row$weights, 0.75)
})

testthat::test_that("the sign of sensitivity reverses the rule", {
  values <- c(rep(10, 8), 20, 20, 20, 20)
  metric <- make_two_asset_metric(values)

  defensive <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "ts_adjusted", window = 4,
    center = 0.5, sensitivity = -0.25, residual_asset = "cash", verbose = FALSE))@data
  aggressive <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "ts_adjusted", window = 4,
    center = 0.5, sensitivity = 0.25, residual_asset = "cash", verbose = FALSE))@data

  jump_date <- timing_dates[9]
  defensive_weight <- defensive$weights[defensive$tickers == "risky" & defensive$dates == jump_date]
  aggressive_weight <- aggressive$weights[aggressive$tickers == "risky" & aggressive$dates == jump_date]

  testthat::expect_lt(defensive_weight, 0.5)
  testthat::expect_gt(aggressive_weight, 0.5)
})

testthat::test_that("ts_adjusted requires a sensitivity and a window rather than assuming them", {
  metric <- make_two_asset_metric(rep(c(10, 20), 6))

  testthat::expect_error(
    derive_timing_weights(metric, metric = "ann_track_err", method = "ts_adjusted",
                          window = 4, residual_asset = "cash", verbose = FALSE),
    "sensitivity, which is required"
  )
  testthat::expect_error(
    derive_timing_weights(metric, metric = "ann_track_err", method = "ts_adjusted",
                          sensitivity = -0.25, residual_asset = "cash", verbose = FALSE),
    "window must be"
  )
})

testthat::test_that("a history shorter than the window is refused rather than shortened", {
  short <- make_two_asset_metric(rep(10, 3), dates = timing_dates[1:3])
  testthat::expect_error(
    suppressMessages(derive_timing_weights(
      short, metric = "ann_track_err", method = "ts_adjusted", window = 6,
      sensitivity = -0.25, residual_asset = "cash", verbose = FALSE)),
    "shorter than the window"
  )
})


# The trend mapping -------------------------------------------------------

testthat::test_that("trend follows the sign of the metric and ignores its size", {
  metric <- make_two_asset_metric(c(5, 50, -5, -50, rep(1, 8)), )

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "trend", center = 0.6, sensitivity = 0.4,
    residual_asset = "cash", verbose = FALSE))@data

  risky <- result %>% dplyr::filter(tickers == "risky") %>% dplyr::arrange(dates)

  ## A positive trailing return gives centre plus sensitivity, whatever its magnitude
  testthat::expect_equal(risky$weights[1], 1.0)
  testthat::expect_equal(risky$weights[2], 1.0)
  ## and a negative one gives centre minus sensitivity, again regardless of size
  testthat::expect_equal(risky$weights[3], 0.2)
  testthat::expect_equal(risky$weights[4], 0.2)
})


# Pass-through ------------------------------------------------------------

testthat::test_that("as_is treats the metric as a weight already", {
  metric <- make_two_asset_metric(rep(c(0.7, 0.3), 6))

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "as_is",
    residual_asset = "cash", verbose = FALSE))@data

  risky <- result %>% dplyr::filter(tickers == "risky") %>% dplyr::arrange(dates)
  cash <- result %>% dplyr::filter(tickers == "cash") %>% dplyr::arrange(dates)

  testthat::expect_equal(risky$weights, rep(c(0.7, 0.3), length.out = nrow(risky)))
  testthat::expect_equal(cash$weights, rep(c(0.3, 0.7), length.out = nrow(cash)))
})


# Box constraints ---------------------------------------------------------

testthat::test_that("a floor on the risky sleeve caps the residual asset", {
  ## The stated use: never hold less than half in the risky portfolio
  metric <- make_two_asset_metric(rep(40, length(timing_dates)))

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4,
    min_weight = 0.5, residual_asset = "cash", verbose = FALSE))@data

  ## Unbounded, a tracking error ten times the budget would leave a tenth in the risky sleeve
  testthat::expect_true(all(result$weights[result$tickers == "risky"] == 0.5))
  testthat::expect_true(all(result$weights[result$tickers == "cash"] == 0.5))
})

testthat::test_that("the box holds exactly when a residual asset absorbs the remainder", {
  metric <- make_two_asset_metric(seq(2, 24, length.out = length(timing_dates)))

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4,
    min_weight = 0.4, max_weight = 0.8, residual_asset = "cash", verbose = FALSE))@data

  risky <- result$weights[result$tickers == "risky"]
  testthat::expect_true(all(risky >= 0.4 - 1e-12 & risky <= 0.8 + 1e-12))
  testthat::expect_true(any(risky == 0.8))
  testthat::expect_true(any(risky == 0.4))
})

testthat::test_that("timed assets claiming more than the whole portfolio are refused", {
  ## Two risky sleeves, each floored at 0.6, cannot both be honoured alongside a cash line
  metric <- rbind(
    make_metric_m_df(rep(4, length(timing_dates)), tickers = "risky_a"),
    make_metric_m_df(rep(4, length(timing_dates)), tickers = "risky_b"),
    make_metric_m_df(rep(1, length(timing_dates)), tickers = "cash")
  )
  metric <- metric[order(metric$id), ]

  testthat::expect_error(
    suppressMessages(derive_timing_weights(
      metric, metric = "ann_track_err", method = "inverse", target = 4,
      min_weight = 0.6, residual_asset = "cash", verbose = FALSE)),
    "more than the whole portfolio"
  )
})

testthat::test_that("without a residual asset the weights are rescaled to sum to one", {
  metric <- rbind(
    make_metric_m_df(rep(4, length(timing_dates)), tickers = "port_a"),
    make_metric_m_df(rep(8, length(timing_dates)), tickers = "port_b")
  )
  metric <- metric[order(metric$id), ]

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4, verbose = FALSE))@data

  ## Raw weights of 1 and 0.5 rescale to two thirds and one third
  first <- result %>% dplyr::filter(dates == timing_dates[1]) %>% dplyr::arrange(tickers)
  testthat::expect_equal(first$weights, c(2 / 3, 1 / 3), tolerance = 1e-12)

  sums <- tapply(result$weights, result$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(timing_dates)), tolerance = 1e-12)
})

testthat::test_that("a non-default box without a residual asset says what it actually bounds", {
  metric <- rbind(
    make_metric_m_df(rep(4, length(timing_dates)), tickers = "port_a"),
    make_metric_m_df(rep(8, length(timing_dates)), tickers = "port_b")
  )
  metric <- metric[order(metric$id), ]

  testthat::expect_message(
    derive_timing_weights(metric, metric = "ann_track_err", method = "inverse", target = 4,
                          max_weight = 0.9, verbose = TRUE),
    "rather than to the final weights"
  )
})


# Contract ----------------------------------------------------------------

testthat::test_that("the result is a weights_m_df that sums to one on every date", {
  metric <- make_two_asset_metric(seq(2, 24, length.out = length(timing_dates)))

  result <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4,
    residual_asset = "cash", verbose = FALSE))

  testthat::expect_s4_class(result, "weights_m_df")
  testthat::expect_equal(names(result@data), c("id", "tickers", "dates", "weights"))
  testthat::expect_equal(result@data$id, paste0(result@data$tickers, "-", result@data$dates))
  testthat::expect_false(is.unsorted(result@data$id))

  sums <- tapply(result@data$weights, result@data$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(timing_dates)), tolerance = 1e-12)
})

testthat::test_that("the output feeds project_meta_weights_to_stocks unchanged", {
  ## The point of producing weights outside the backtest is that they go straight in
  metric <- rbind(
    make_metric_m_df(rep(4, length(timing_dates)), tickers = "bt_alpha"),
    make_metric_m_df(rep(8, length(timing_dates)), tickers = "bt_beta")
  )
  metric <- metric[order(metric$id), ]

  weights <- suppressMessages(derive_timing_weights(
    metric, metric = "ann_track_err", method = "inverse", target = 4, verbose = FALSE))

  testthat::expect_true(all(c("id", "tickers", "dates", "weights") %in% names(weights@data)))
  testthat::expect_setequal(unique(weights@data$tickers), c("bt_alpha", "bt_beta"))
})


# Validation --------------------------------------------------------------

testthat::test_that("inputs are validated", {
  metric <- make_two_asset_metric(rep(4, length(timing_dates)))

  testthat::expect_error(derive_timing_weights("nope", method = "as_is", verbose = FALSE),
                         "data.frame or meta_dataframe")

  testthat::expect_error(
    derive_timing_weights(metric, metric = "absent", method = "as_is", verbose = FALSE),
    "is not a column")

  na_metric <- metric
  na_metric$ann_track_err[1] <- NA_real_
  testthat::expect_error(
    derive_timing_weights(na_metric, metric = "ann_track_err", method = "as_is", verbose = FALSE),
    "must not contain NA")

  testthat::expect_error(
    derive_timing_weights(metric, metric = "ann_track_err", method = "as_is",
                          min_weight = 0.8, max_weight = 0.2, verbose = FALSE),
    "must not exceed max_weight")

  testthat::expect_error(
    derive_timing_weights(metric, metric = "ann_track_err", method = "as_is",
                          min_weight = -0.1, verbose = FALSE),
    "must not be negative")

  testthat::expect_error(
    derive_timing_weights(metric, metric = "ann_track_err", method = "as_is",
                          residual_asset = "absent_asset", verbose = FALSE),
    "is not among the tickers")

  testthat::expect_error(
    derive_timing_weights(metric, metric = "ann_track_err", method = "nonsense", verbose = FALSE))
})

testthat::test_that("the metric column is inferred only when there is no ambiguity", {
  single <- make_two_asset_metric(rep(4, length(timing_dates)))
  testthat::expect_s4_class(
    suppressMessages(derive_timing_weights(single, method = "inverse", target = 4,
                                           residual_asset = "cash", verbose = FALSE)),
    "weights_m_df")

  two_metrics <- single
  two_metrics$other_metric <- 1
  testthat::expect_error(
    derive_timing_weights(two_metrics, method = "as_is", residual_asset = "cash", verbose = FALSE),
    "must be named when metric_m_df carries more than one")
})
