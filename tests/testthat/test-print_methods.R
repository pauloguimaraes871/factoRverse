# Fixtures ----------------------------------------------------------------
# Objects are built directly rather than by running a backtest. A show method is pure formatting
# over slots, so what can break is a slot name that does not exist, a NULL that is not guarded, or
# a column read from the wrong path. Constructing the objects here exercises all three without
# paying for a run.

meta_show_dates <- seq(as.Date("2023-01-15"), by = "month", length.out = 4L)
meta_show_sleeves <- c("value_port", "momentum_port")


## A port_backtest_results standing in for a base portfolio or for the stock-level object. The
## update flag relaxes validity, which is what lets a results object exist without a full run.
make_show_backtest_results <- function(identifier = "base_one", tickers = c("AAA", "BBB")) {

  weights_df <- expand.grid(tickers = tickers, dates = meta_show_dates,
                            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  weights_df$eop_port_weights <- 1 / length(tickers)
  weights_df$id <- paste0(weights_df$tickers, "-", weights_df$dates)
  weights_df <- weights_df[order(weights_df$id),
                           c("id", "tickers", "dates", "eop_port_weights")]
  rownames(weights_df) <- NULL

  returns_xts <- xts::xts(
    data.frame(raw_return = c(1.2, -0.8, 2.1, -1.5),
               net_return = c(1.1, -0.9, 2.0, -1.6),
               raw_active_return = c(0.4, -0.3, 0.7, -0.5),
               net_active_return = c(0.3, -0.4, 0.6, -0.6)),
    order.by = meta_show_dates)

  costs_xts <- xts::xts(
    data.frame(total_cost = rep(0.1, length(meta_show_dates)),
               turnover = c(0.5, 0.2, 0.3, 0.25)),
    order.by = meta_show_dates)

  methods::new(
    "port_backtest_results",
    port_backtest_config = NULL,
    port_weights_m_df = suppressWarnings(suppressMessages(create_meta_dataframe(
      weights_df, meta_dataframe_name = identifier, type = "weights"))),
    transactions_log = methods::new("transactions_log"),
    port_costs_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      costs_xts, type = "metrics", meta_xts_name = identifier))),
    port_metrics_m_xts = NULL,
    port_returns_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      returns_xts, type = "metrics", meta_xts_name = identifier))),
    final_stock_port = NULL,
    port_construction_method = "sw",
    stock_universe_m_df = NULL,
    final_stock_universe_m_d_ref = NULL,
    port_stats_m_df = NULL,
    final_port_stats_m_d_ref = NULL,
    port_backtest_workflow = list(list(selected_benchmark = "ibov")),
    backtest_identifier = identifier,
    update = TRUE
  )
}


make_show_cohort <- function() {

  weights_df <- expand.grid(tickers = c("AAA", "BBB"), dates = meta_show_dates,
                            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  weights_df$eop_port_weights <- 0.5
  weights_df$id <- paste0(weights_df$tickers, "-", weights_df$dates)
  weights_df <- weights_df[order(weights_df$id),
                           c("id", "tickers", "dates", "eop_port_weights")]
  rownames(weights_df) <- NULL

  methods::new(
    "port_backtest_cohort",
    cohort_name = "show_cohort",
    port_backtest_results_list = list(
      make_show_backtest_results("value_port"),
      make_show_backtest_results("momentum_port")),
    port_weights_m_df = suppressWarnings(suppressMessages(create_meta_dataframe(
      weights_df, meta_dataframe_name = "show_cohort", type = "weights"))),
    port_costs_m_xts_list = list(),
    port_returns_m_xts_list = list(),
    port_metrics_m_xts_list = list(),
    port_stats_m_xts_nested_list = list(),
    backtest_workflow_common = list(selected_benchmark = "ibov")
  )
}


## Meta weights over two sleeves, so the mean weight per sleeve has something to report
make_meta_weights <- function(tickers = meta_show_sleeves, weights = c(0.7, 0.3)) {
  df <- expand.grid(tickers = tickers, dates = meta_show_dates,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$weights <- rep(weights, times = length(meta_show_dates))
  df$id <- paste0(df$tickers, "-", df$dates)
  df <- df[order(df$id), c("id", "tickers", "dates", "weights")]
  rownames(df) <- NULL
  suppressWarnings(suppressMessages(create_meta_dataframe(
    df, meta_dataframe_name = "meta_weights", type = "weights")))
}


make_projected_weights <- function() {
  df <- expand.grid(tickers = c("AAA", "BBB", "CCC"), dates = meta_show_dates,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  ## CCC carries no weight, so the count of held stocks has to exclude it
  df$weights <- rep(c(0.6, 0.4, 0), times = length(meta_show_dates))
  df$id <- paste0(df$tickers, "-", df$dates)
  df <- df[order(df$id), c("id", "tickers", "dates", "weights")]
  rownames(df) <- NULL
  suppressWarnings(suppressMessages(create_meta_dataframe(
    df, meta_dataframe_name = "projected_weights", type = "weights")))
}


make_meta_stats <- function(columns) {
  df <- data.frame(id = paste0("meta_port-", meta_show_dates),
                   tickers = "meta_port",
                   dates = meta_show_dates,
                   stringsAsFactors = FALSE)
  df <- cbind(df, as.data.frame(columns))
  df <- df[order(df$id), , drop = FALSE]
  rownames(df) <- NULL
  suppressWarnings(suppressMessages(create_meta_dataframe(
    df, meta_dataframe_name = "meta_stats")))
}


show_inner_config <- function(score = c(exp_ret = "long"), method = "sw") {
  create_port_backtest_config(
    chosen_score_metric_and_position = score,
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = 2, rebalancing_months = c(1, 4),
    selected_benchmark = "ibov",
    cov_est_method = create_cov_est_method("sample", 2, TRUE, "ibov"),
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = method, config_name = "inner_cfg") %>%
    add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                     lambda = "dynamic", strategy_aum = 25000)
}


show_multi_port_config <- function() {
  suppressMessages(create_port_metabacktest_config(
    show_inner_config(), type = "multi_port", return_basis = "net",
    cost_lookback = 12, config_name = "multi_cfg", verbose = FALSE))
}


show_risk_targeted_config <- function(with_parameters = TRUE, ...) {
  config <- suppressMessages(create_port_metabacktest_config(
    show_inner_config(score = NULL, method = "ew"), type = "risk_targeted",
    return_basis = "net", config_name = "cml_cfg", verbose = FALSE))
  if (!with_parameters) return(config)
  add_cml_parameters(config, residual_ticker = "BOVA11", target = 4,
                     target_metric = "tracking_error", min_weight = 0.2, max_weight = 1,
                     vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL), ...)
}


show_multi_port_results <- function() {
  methods::new(
    "port_metabacktest_results",
    port_metabacktest_config = show_multi_port_config(),
    meta_port_backtest_results = make_show_backtest_results("meta_stock_level"),
    port_backtest_cohort = make_show_cohort(),
    port_universe_m_df = NULL,
    meta_port_weights_m_df = make_meta_weights(),
    projected_stock_weights_m_df = make_projected_weights(),
    meta_port_stats_m_df = make_meta_stats(list(
      exp_ret = c(0.4, 0.5, 0.45, 0.6), risk = c(12, 13, 11.5, 14),
      sharpe = c(0.033, 0.038, 0.039, 0.043),
      diversification_ratio = c(1.1, 1.2, 1.15, 1.18))),
    final_meta_port = NULL,
    backtest_identifier = "meta_multi"
  )
}


show_cml_results <- function(risky_weight = c(0.4, 0.35, 0.2, 1),
                             exposure = rep(1, 4),
                             cml_parameters = show_risk_targeted_config()@cml_parameters) {
  sleeve_risk <- c(10, 11.4, 20, 3)
  methods::new(
    "cml_metabacktest_results",
    port_metabacktest_config = show_risk_targeted_config(),
    meta_port_backtest_results = make_show_backtest_results("meta_stock_level"),
    port_backtest_cohort = make_show_cohort(),
    port_universe_m_df = NULL,
    meta_port_weights_m_df = make_meta_weights(
      tickers = c("sleeve", "BOVA11"), weights = c(0.4, 0.6)),
    projected_stock_weights_m_df = make_projected_weights(),
    meta_port_stats_m_df = make_meta_stats(list(
      sleeve_risk = sleeve_risk, exposure = exposure, risky_weight = risky_weight,
      target = rep(4, 4), implied_risk = sleeve_risk * risky_weight)),
    final_meta_port = NULL,
    backtest_identifier = "meta_risk_target",
    residual_ticker = "BOVA11",
    cml_parameters = cml_parameters
  )
}


# cml_parameters ----------------------------------------------------------

testthat::test_that("the risk-targeting parameters name the pairing they assume", {
  params <- create_cml_parameters("BOVA11", target = 4, target_metric = "tracking_error",
                                  vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL))

  output <- testthat::capture_output(methods::show(params))

  testthat::expect_match(output, "BOVA11")
  testthat::expect_match(output, "tracking_error")
  ## The residual and the target metric have to agree and nothing errors when they do not, so the
  ## assumption being made is stated rather than left to be inferred from the ticker
  testthat::expect_match(output, "residual tracks the benchmark")

  volatility <- create_cml_parameters("CASH", target = 10, target_metric = "volatility",
                                      vol_source = "realized_rolling", vol_window = 6)
  testthat::expect_match(testthat::capture_output(methods::show(volatility)),
                         "residual is riskless")
})

testthat::test_that("the response exponent is named rather than left as a number", {
  ## p is the whole shape of the response, and 1 against 2 is not self-explanatory
  targeting <- create_cml_parameters("BOVA11", target = 4, p = 1,
                                     vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL))
  managed <- create_cml_parameters("BOVA11", target = 4, p = 2,
                                   vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL))

  testthat::expect_match(testthat::capture_output(methods::show(targeting)), "risk targeting")
  testthat::expect_match(testthat::capture_output(methods::show(managed)), "inverse variance")
})

testthat::test_that("the risk source and the exposure signal are both reported", {
  without <- create_cml_parameters("BOVA11", target = 4,
                                   vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL))
  output <- testthat::capture_output(methods::show(without))
  testthat::expect_match(output, "ex_ante")
  testthat::expect_match(output, "ewma")
  ## An absent signal is stated, not omitted, so the weight rule can be read in full
  testthat::expect_match(output, "risk ratio alone")

  with_signal <- create_cml_parameters(
    "BOVA11", target = 4, exposure_method = "trend", exposure_center = 0.75,
    exposure_sensitivity = 0.25, exposure_bounds = c(0.1, 0.9),
    vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL))
  signal_output <- testthat::capture_output(methods::show(with_signal))
  testthat::expect_match(signal_output, "trend")
  testthat::expect_match(signal_output, "0\\.1 to 0\\.9")
})


# port_metabacktest_config ------------------------------------------------

testthat::test_that("a multi_port config reports the meta score and the wrapped config", {
  output <- testthat::capture_output(methods::show(show_multi_port_config()))

  testthat::expect_match(output, "multi_port")
  testthat::expect_match(output, "multi_cfg")
  testthat::expect_match(output, "exp_ret")
  testthat::expect_match(output, "12 months")
  ## The wrapped config is delegated to its own show rather than reprinted here
  testthat::expect_match(output, "Wrapped port_backtest_config")
  testthat::expect_match(output, "inner_cfg")
})

testthat::test_that("a risk_targeted config says the construction method is unused", {
  ## The wrapped config still carries a port_construction_method, and printing it without comment
  ## would suggest it drives the allocation when the targeting rule does
  output <- testthat::capture_output(methods::show(show_risk_targeted_config()))

  testthat::expect_match(output, "risk_targeted")
  testthat::expect_match(output, "port_construction_method is unused")
  testthat::expect_match(output, "BOVA11")
})

testthat::test_that("an incomplete risk_targeted config says so and names the fix", {
  ## cml_parameters may be NULL at construction, so the config can exist in a state that cannot be
  ## run. Showing it must make that visible rather than printing a config that looks complete.
  output <- testthat::capture_output(
    methods::show(show_risk_targeted_config(with_parameters = FALSE)))

  testthat::expect_match(output, "not set")
  testthat::expect_match(output, "add_cml_parameters")
})

testthat::test_that("an expanding cost average is named rather than shown as NULL", {
  config <- suppressMessages(create_port_metabacktest_config(
    show_inner_config(), type = "multi_port", return_basis = "raw",
    cost_lookback = NULL, config_name = "expanding_cfg", verbose = FALSE))

  output <- testthat::capture_output(methods::show(config))
  testthat::expect_match(output, "expanding average")
  testthat::expect_match(output, "raw")
})


# port_metabacktest_results -----------------------------------------------

testthat::test_that("the multi-portfolio summary reports both levels", {
  output <- testthat::capture_output(methods::show(show_multi_port_results()))

  ## Meta level: which sleeves, and how much each was given on average
  testthat::expect_match(output, "meta_multi")
  testthat::expect_match(output, "value_port")
  testthat::expect_match(output, "momentum_port")
  testthat::expect_match(output, "0\\.7")

  ## Stock level: the object whose returns and costs are the real ones
  testthat::expect_match(output, "Stock-Level Portfolio")
  testthat::expect_match(output, "meta_stock_level")
  testthat::expect_match(output, "net_return")
  testthat::expect_match(output, "turnover")
})

testthat::test_that("the summary counts only stocks that carry weight", {
  ## The projected panel spans every stock in the universe, including those the meta weights give
  ## nothing to, so a plain count of tickers would overstate the portfolio
  output <- testthat::capture_output(methods::show(show_multi_port_results()))
  testthat::expect_match(output, "Number of Stocks Held:\\s+2")
})

testthat::test_that("the meta-level analytics are reported with their reading", {
  output <- testthat::capture_output(methods::show(show_multi_port_results()))

  testthat::expect_match(output, "exp_ret")
  testthat::expect_match(output, "diversification_ratio")
  ## exp_ret here is a transformed score rather than a return, and risk is absolute rather than
  ## benchmark-relative. Both are easy to misread as their stock-level namesakes.
  testthat::expect_match(output, "dimensionless")
  testthat::expect_match(output, "absolute")
})

testthat::test_that("the rebalance schedule and the cohort are reported", {
  output <- testthat::capture_output(methods::show(show_multi_port_results()))

  testthat::expect_match(output, "show_cohort")
  testthat::expect_match(output, "Number of Base Portfolios:\\s+2")
  testthat::expect_match(output, "2023-01-15")
  testthat::expect_match(output, "2023-04-15")
})


# cml_metabacktest_results ------------------------------------------------

testthat::test_that("the risk-targeted summary reports the rule rather than a cross-section", {
  output <- testthat::capture_output(methods::show(show_cml_results()))

  testthat::expect_match(output, "meta_risk_target")
  testthat::expect_match(output, "Risk-Targeting Rule")
  testthat::expect_match(output, "The Rule At Work")
  testthat::expect_match(output, "Estimated Sleeve Risk")
  testthat::expect_match(output, "Risky Weight")
  ## The residual is named at the results level too, since it decides what the target means
  testthat::expect_match(output, "BOVA11")
})

testthat::test_that("dates at a bound are counted, since that is what a missed target means", {
  ## implied_risk equals the target whenever the weight is unclipped, so a gap between them says a
  ## bound was binding rather than that the targeting failed. The count is what tells them apart.
  output <- testthat::capture_output(methods::show(
    show_cml_results(risky_weight = c(0.4, 0.35, 0.2, 1))))

  ## One date sits at the 0.2 floor and one at the 1.0 cap
  testthat::expect_match(output, "1 at the floor, 1 at the cap, of 4")
})

testthat::test_that("a flat exposure is reported as absent rather than as a number", {
  ## An exposure of exactly one every date means no signal was used, and saying so is clearer than
  ## printing a mean of 1 that the reader has to interpret
  flat <- testthat::capture_output(methods::show(show_cml_results(exposure = rep(1, 4))))
  testthat::expect_match(flat, "Exposure Signal: none")

  varying <- testthat::capture_output(methods::show(
    show_cml_results(exposure = c(1, 0.5, 1, 0.5))))
  testthat::expect_match(varying, "Exposure Signal: mean")
  testthat::expect_no_match(varying, "Exposure Signal: none")
})

testthat::test_that("the realised risk is reported against the target", {
  ## The intended risk is an identity when nothing binds, so on its own it says nothing about
  ## whether the rule worked. Only the realised figure does, and it comes from the stock-level run.
  output <- testthat::capture_output(methods::show(show_cml_results()))

  testthat::expect_match(output, "Intended Risk")
  testthat::expect_match(output, "Realised tracking_error")
  testthat::expect_match(output, "net_active_return")
})

testthat::test_that("a volatility target reads the total return series instead", {
  volatility_params <- create_cml_parameters(
    "CASH", target = 10, target_metric = "volatility", vol_source = "realized_rolling",
    vol_window = 6, min_weight = 0.2)

  output <- testthat::capture_output(methods::show(
    show_cml_results(cml_parameters = volatility_params)))

  testthat::expect_match(output, "Realised volatility")
  testthat::expect_match(output, "\\(net_return,")
})

testthat::test_that("a results object with missing optional slots still shows", {
  ## port_universe_m_df, final_meta_port and cml_parameters are all allowed to be NULL, and a show
  ## method that assumed otherwise would fail exactly where a diagnostic is most wanted
  results <- show_cml_results()
  results@cml_parameters <- NULL
  results@port_metabacktest_config <- NULL

  output <- testthat::capture_output(testthat::expect_no_error(methods::show(results)))
  testthat::expect_match(output, "not available")
})
