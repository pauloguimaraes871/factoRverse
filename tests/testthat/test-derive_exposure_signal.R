# Fixtures ----------------------------------------------------------------

exposure_dates <- seq(as.Date("2022-01-15"), by = "month", length.out = 12L)


make_exposure_metric <- function(values, metric_name = "trailing_return",
                                 tickers = "sleeve", dates = exposure_dates) {
  df <- expand.grid(tickers = tickers, dates = dates,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df[[metric_name]] <- values
  df$id <- paste0(df$tickers, "-", df$dates)
  df <- df[order(df$id), c("id", "tickers", "dates", metric_name)]
  rownames(df) <- NULL
  df
}


# Trend -------------------------------------------------------------------

testthat::test_that("trend follows the direction of the metric and ignores its size", {
  signal <- suppressMessages(derive_exposure_signal(
    make_exposure_metric(c(5, 50, -5, -50, 0, rep(1, 7))),
    method = "trend", center = 0.75, sensitivity = 0.25, verbose = FALSE))

  ## A positive trailing return gives centre plus sensitivity, whatever its magnitude
  testthat::expect_equal(signal$exposure[1:2], c(1, 1))
  ## and a negative one gives centre minus sensitivity, again regardless of size
  testthat::expect_equal(signal$exposure[3:4], c(0.5, 0.5))
  ## A flat return picks no side
  testthat::expect_equal(signal$exposure[5], 0.75)
})

testthat::test_that("the sign of sensitivity reverses the rule", {
  metric <- make_exposure_metric(c(rep(1, 6), rep(-1, 6)))

  with_positive <- suppressMessages(derive_exposure_signal(
    metric, method = "trend", center = 0.5, sensitivity = 0.5, verbose = FALSE))
  with_negative <- suppressMessages(derive_exposure_signal(
    metric, method = "trend", center = 0.5, sensitivity = -0.5, verbose = FALSE))

  testthat::expect_equal(with_positive$exposure[1], 1)
  testthat::expect_equal(with_negative$exposure[1], 0)
})


# Time-series adjusted ----------------------------------------------------

testthat::test_that("ts_adjusted scores the metric against its own history", {
  ## Flat, then a step up
  signal <- suppressMessages(derive_exposure_signal(
    make_exposure_metric(c(rep(10, 8), rep(20, 4))),
    method = "ts_adjusted", window = 4, center = 0.75, sensitivity = -0.25, verbose = FALSE))

  ## The first three dates lack a full window and carry no exposure
  testthat::expect_equal(nrow(signal), length(exposure_dates) - 3L)

  ## A flat window says nothing about where the metric stands, so the rule sits at its centre
  testthat::expect_equal(signal$exposure[1], 0.75)

  ## A metric high against its own history cuts exposure under a negative sensitivity
  testthat::expect_lt(signal$exposure[signal$dates == exposure_dates[9]], 0.75)
})

testthat::test_that("ts_adjusted refuses a history shorter than its window", {
  testthat::expect_error(
    suppressMessages(derive_exposure_signal(
      make_exposure_metric(rep(10, 3), dates = exposure_dates[1:3]),
      method = "ts_adjusted", window = 6, sensitivity = -0.25, verbose = FALSE)),
    "shorter than the configured window")
})


# Pass-through and bounds -------------------------------------------------

testthat::test_that("as_is passes the metric through as the multiplier", {
  signal <- suppressMessages(derive_exposure_signal(
    make_exposure_metric(c(rep(0.6, 6), rep(0.9, 6))), method = "as_is", verbose = FALSE))
  testthat::expect_setequal(unique(signal$exposure), c(0.6, 0.9))
})

testthat::test_that("the bounds clip the exposure before the risk ratio ever sees it", {
  signal <- suppressMessages(derive_exposure_signal(
    make_exposure_metric(c(rep(-0.5, 6), rep(1.5, 6))), method = "as_is", verbose = FALSE))
  testthat::expect_setequal(unique(signal$exposure), c(0, 1))

  ## and a narrower box binds harder
  narrow <- suppressMessages(derive_exposure_signal(
    make_exposure_metric(c(rep(-0.5, 6), rep(1.5, 6))), method = "as_is",
    min_exposure = 0.25, max_exposure = 0.75, verbose = FALSE))
  testthat::expect_setequal(unique(narrow$exposure), c(0.25, 0.75))
})


# The mapping that is deliberately absent ---------------------------------

testthat::test_that("there is no inverse-of-risk mapping here", {
  ## The (target / risk)^p term owns that. Offering it in both places would let the volatility
  ## scaling be applied twice without it showing up in the output.
  testthat::expect_error(
    derive_exposure_signal(make_exposure_metric(rep(4, 12)), method = "inverse", verbose = FALSE))
  testthat::expect_error(
    create_risk_target_parameters("BOVA11", target = 4, exposure_method = "inverse"))
})


# Validation --------------------------------------------------------------

testthat::test_that("an exposure signal describes exactly one sleeve", {
  two_sleeves <- rbind(
    make_exposure_metric(rep(1, 12), tickers = "sleeve_a"),
    make_exposure_metric(rep(1, 12), tickers = "sleeve_b"))
  testthat::expect_error(
    derive_exposure_signal(two_sleeves, method = "as_is", verbose = FALSE),
    "exactly one ticker")
})

testthat::test_that("the direction-carrying arguments have no defaults", {
  metric <- make_exposure_metric(rep(1, 12))

  testthat::expect_error(
    derive_exposure_signal(metric, method = "trend", verbose = FALSE),
    "sensitivity, which is required")
  testthat::expect_error(
    derive_exposure_signal(metric, method = "ts_adjusted", sensitivity = 0.2, verbose = FALSE),
    "window must be")
})

testthat::test_that("malformed metrics are rejected", {
  testthat::expect_error(derive_exposure_signal("nope", method = "as_is", verbose = FALSE),
                         "data.frame or meta_dataframe")

  metric <- make_exposure_metric(rep(1, 12))
  testthat::expect_error(
    derive_exposure_signal(metric, metric = "absent", method = "as_is", verbose = FALSE),
    "is not a column")

  with_na <- metric
  with_na$trailing_return[1] <- NA_real_
  testthat::expect_error(
    derive_exposure_signal(with_na, method = "as_is", verbose = FALSE), "must not contain NA")

  testthat::expect_error(
    derive_exposure_signal(metric, method = "as_is", min_exposure = -0.1, verbose = FALSE),
    "must not be negative")
  testthat::expect_error(
    derive_exposure_signal(metric, method = "as_is", min_exposure = 0.8, max_exposure = 0.2,
                           verbose = FALSE),
    "must not exceed max_exposure")

  two_metrics <- metric
  two_metrics$other <- 1
  testthat::expect_error(
    derive_exposure_signal(two_metrics, method = "as_is", verbose = FALSE),
    "must be named when metric_m_df carries more than one")
})


# Composition with the risk term ------------------------------------------

testthat::test_that("the weight is the exposure times the risk ratio", {
  params <- create_risk_target_parameters("BOVA11", target = 4, p = 1)

  ## Risk at twice the target halves the position; the exposure then scales that
  testthat::expect_equal(risk_to_weight(8, params, exposure = 1), 0.5)
  testthat::expect_equal(risk_to_weight(8, params, exposure = 0.5), 0.25)
  testthat::expect_equal(risk_to_weight(8, params, exposure = 0), 0)

  ## The exponent applies to the ratio, not to the exposure
  quadratic <- create_risk_target_parameters("BOVA11", target = 4, p = 2)
  testthat::expect_equal(risk_to_weight(8, quadratic, exposure = 0.5), 0.5 * 0.25)
})

testthat::test_that("an exposure that could not be computed leaves the weight undefined", {
  ## Defaulting to full exposure would silently ignore the signal the caller asked to follow
  params <- create_risk_target_parameters("BOVA11", target = 4)
  testthat::expect_true(is.na(risk_to_weight(8, params, exposure = NA_real_)))
  testthat::expect_true(is.na(risk_to_weight(8, params, exposure = Inf)))
})

testthat::test_that("a constant exposure is redundant with the target, a varying one is not", {
  ## The reason exposure is a signal rather than a setting: any constant s can be folded into
  ## the target by rescaling it to target * s^(1/p), so only a time-varying s adds anything.
  folded <- create_risk_target_parameters("BOVA11", target = 4 * 0.5, p = 1)
  scaled <- create_risk_target_parameters("BOVA11", target = 4, p = 1)

  for (risk in c(2, 4, 8, 16)) {
    testthat::expect_equal(risk_to_weight(risk, folded, exposure = 1),
                           risk_to_weight(risk, scaled, exposure = 0.5))
  }

  ## A signal that changes over time cannot be reproduced by any single target
  varying <- c(1, 0.5, 1, 0.5)
  weights <- vapply(varying, function(s) risk_to_weight(8, scaled, exposure = s), numeric(1))
  testthat::expect_false(length(unique(weights)) == 1L)
})


testthat::test_that("an exposure metric for the wrong asset is refused", {

  ## Carrying one ticker is not the same as carrying the right one. Without this the multiplier
  ## could be driven by a metric computed for something else entirely, and nothing downstream
  ## would show it.
  metric <- make_exposure_metric(rep(1, 12), tickers = "some_other_asset")

  testthat::expect_error(
    derive_exposure_signal(metric, method = "as_is", expected_risky_ticker = "the_sleeve",
                           verbose = FALSE),
    "the risky sleeve is")

  ## and the same metric is accepted once it names the sleeve it leans on
  right_metric <- make_exposure_metric(rep(1, 12), tickers = "the_sleeve")
  testthat::expect_no_error(suppressMessages(
    derive_exposure_signal(right_metric, method = "as_is", expected_risky_ticker = "the_sleeve",
                           verbose = FALSE)))
})

testthat::test_that("two readings on one date are refused", {
  ## The signal would otherwise depend on which row happened to come first
  metric <- make_exposure_metric(rep(1, 12))
  duplicated_dates <- rbind(metric, metric[1, , drop = FALSE])

  testthat::expect_error(
    derive_exposure_signal(duplicated_dates, method = "as_is", verbose = FALSE),
    "at most one row per date")
})
