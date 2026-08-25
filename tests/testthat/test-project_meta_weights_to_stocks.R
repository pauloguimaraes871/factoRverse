# Fixtures ----------------------------------------------------------------
# Three stocks over four dates, two base portfolios with deliberately different holdings so a
# crossed or averaged projection would not reproduce the hand calculation.

projection_stocks <- c("AAA", "BBB", "CCC")
projection_dates <- seq(as.Date("2022-02-15"), by = "month", length.out = 4L)
projection_ports <- c("bt_alpha", "bt_beta")

## bt_alpha holds AAA and BBB evenly; bt_beta holds BBB and CCC one to three. Both sum to one.
projection_base_weights <- c(bt_alpha = list(c(0.5, 0.5, 0)),
                             bt_beta = list(c(0, 0.25, 0.75)))


make_projection_cohort <- function(base_weights = projection_base_weights,
                                   dates = projection_dates) {

  weights_df <- expand.grid(tickers = projection_stocks, dates = dates,
                            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (port in names(base_weights)) {
    weights_df[[port]] <- rep(base_weights[[port]], times = length(dates))
  }
  weights_df$id <- paste0(weights_df$tickers, "-", weights_df$dates)
  weights_df <- weights_df[order(weights_df$id), c("id", "tickers", "dates", names(base_weights))]
  rownames(weights_df) <- NULL

  methods::new(
    "port_backtest_cohort",
    cohort_name = "projection_cohort",
    port_backtest_results_list = list(),
    port_weights_m_df = suppressWarnings(suppressMessages(
      create_meta_dataframe(weights_df, meta_dataframe_name = "projection_cohort",
                            type = "weights"))),
    port_costs_m_xts_list = list(),
    port_returns_m_xts_list = list(),
    port_metrics_m_xts_list = list(),
    port_stats_m_xts_nested_list = list(),
    backtest_workflow_common = list(selected_benchmark = "ibov", dates_backtest = dates)
  )
}


make_meta_weights <- function(weights_by_date = list(c(0.6, 0.4), c(0.2, 0.8)),
                              dates = projection_dates[c(1, 3)],
                              tickers = projection_ports) {

  rows <- purrr::map_dfr(seq_along(dates), function(i) {
    data.frame(tickers = tickers, dates = dates[i], weights = weights_by_date[[i]],
               stringsAsFactors = FALSE)
  })
  rows$id <- paste0(rows$tickers, "-", rows$dates)
  rows <- rows[order(rows$id), c("id", "tickers", "dates", "weights")]
  rownames(rows) <- NULL
  rows
}


make_projection_signals <- function(dates = projection_dates) {
  df <- expand.grid(tickers = projection_stocks, dates = dates,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$id <- paste0(df$tickers, "-", df$dates)
  df$book_yield <- seq_len(nrow(df)) / 100
  df <- df[order(df$id), c("id", "tickers", "dates", "book_yield")]
  rownames(df) <- NULL
  suppressWarnings(suppressMessages(
    create_meta_dataframe(df, meta_dataframe_name = "projection_signals")))
}


project <- function(meta_weights = make_meta_weights(), cohort = make_projection_cohort(), ...) {
  suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = meta_weights, port_backtest_cohort = cohort, verbose = FALSE, ...))
}


# The arithmetic ----------------------------------------------------------

testthat::test_that("weights are the meta weight times the base weight, summed over portfolios", {
  result <- project()

  testthat::expect_s4_class(result, "weights_m_df")
  testthat::expect_equal(names(result@data), c("id", "tickers", "dates", "weights"))

  ## At the first meta rebalance the split is 60/40:
  ##   AAA = 0.6 * 0.50                = 0.30
  ##   BBB = 0.6 * 0.50 + 0.4 * 0.25   = 0.40
  ##   CCC =              0.4 * 0.75   = 0.30
  first <- result@data %>%
    dplyr::filter(dates == projection_dates[1]) %>%
    dplyr::arrange(tickers)
  testthat::expect_equal(first$tickers, projection_stocks)
  testthat::expect_equal(first$weights, c(0.30, 0.40, 0.30), tolerance = 1e-12)

  ## At the second the split is 20/80:
  ##   AAA = 0.2 * 0.50                = 0.10
  ##   BBB = 0.2 * 0.50 + 0.8 * 0.25   = 0.30
  ##   CCC =              0.8 * 0.75   = 0.60
  third <- result@data %>%
    dplyr::filter(dates == projection_dates[3]) %>%
    dplyr::arrange(tickers)
  testthat::expect_equal(third$weights, c(0.10, 0.30, 0.60), tolerance = 1e-12)
})

testthat::test_that("projected weights sum to one on every date and stay inside the unit interval", {
  result <- project()@data

  sums <- tapply(result$weights, result$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(projection_dates)), tolerance = 1e-12)
  testthat::expect_true(all(result$weights >= 0 & result$weights <= 1))
})

testthat::test_that("a portfolio holding nothing in a stock contributes nothing to it", {
  ## bt_alpha holds no CCC, so at a 100/0 split CCC must be empty
  result <- project(meta_weights = make_meta_weights(
    weights_by_date = list(c(1, 0)), dates = projection_dates[1]))@data

  first <- result %>% dplyr::filter(dates == projection_dates[1]) %>% dplyr::arrange(tickers)
  testthat::expect_equal(first$weights, c(0.5, 0.5, 0), tolerance = 1e-12)
})


# Holding meta weights forward --------------------------------------------

testthat::test_that("dates between meta rebalances reuse the last meta weights", {
  result <- project()@data

  ## Base weights are constant here, so a held-forward date must reproduce its source date
  for (pair in list(c(1, 2), c(3, 4))) {
    source_row <- result %>% dplyr::filter(dates == projection_dates[pair[1]]) %>%
      dplyr::arrange(tickers)
    held_row <- result %>% dplyr::filter(dates == projection_dates[pair[2]]) %>%
      dplyr::arrange(tickers)
    testthat::expect_equal(held_row$weights, source_row$weights, tolerance = 1e-12)
  }

  ## and the two blocks genuinely differ, so the comparison above is not vacuous
  block_one <- result$weights[result$dates == projection_dates[1]]
  block_two <- result$weights[result$dates == projection_dates[3]]
  testthat::expect_false(isTRUE(all.equal(block_one, block_two)))
})

testthat::test_that("every date the cohort covers gets a weight", {
  result <- project()@data
  testthat::expect_setequal(unique(result$dates), projection_dates)
  testthat::expect_equal(nrow(result), length(projection_stocks) * length(projection_dates))
})


# Extending to the signals panel ------------------------------------------

testthat::test_that("dates before the first meta weight are filled with equal weights", {
  ## The signals panel reaches one month further back than the cohort's first meta weight
  earlier_dates <- c(seq(projection_dates[1], by = "-1 month", length.out = 2L)[2],
                     projection_dates)
  signals <- make_projection_signals(dates = earlier_dates)

  testthat::expect_message(
    result <- project_meta_weights_to_stocks(
      meta_weights_m_df = make_meta_weights(),
      port_backtest_cohort = make_projection_cohort(),
      signals_m_df = signals, verbose = TRUE),
    "precede the first meta weight"
  )
  result <- result@data

  ## Every signals id is covered, which is what check_inputs_port_backtest() demands
  testthat::expect_true(all(signals@data$id %in% result$id))

  ## The filled rows are an equal split and carry nothing from any later date
  filled <- result %>% dplyr::filter(dates == earlier_dates[1])
  testthat::expect_equal(filled$weights, rep(1 / length(projection_stocks),
                                             length(projection_stocks)), tolerance = 1e-12)

  ## and still sum to one, so the panel contract holds throughout
  sums <- tapply(result$weights, result$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(earlier_dates)), tolerance = 1e-12)
})

testthat::test_that("a signals panel the cohort cannot describe is refused", {
  ## An extra stock the cohort never held would leave a hole in the weight panel
  signals <- make_projection_signals()
  extra <- signals@data[1, ]
  extra$tickers <- "DDD"
  extra$id <- paste0("DDD-", extra$dates)
  signals@data <- rbind(signals@data, extra)
  signals@data <- signals@data[order(signals@data$id), ]

  testthat::expect_error(
    project(signals_m_df = signals),
    "no projected weight"
  )
})


# Validation --------------------------------------------------------------

testthat::test_that("meta weights are validated before anything is projected", {
  testthat::expect_error(
    project(meta_weights = make_meta_weights(weights_by_date = list(c(0.6, 0.6)),
                                             dates = projection_dates[1])),
    "do not sum to one"
  )

  na_weights <- make_meta_weights()
  na_weights$weights[1] <- NA_real_
  testthat::expect_error(project(meta_weights = na_weights), "must not contain NA weights")

  missing_column <- make_meta_weights()
  missing_column$weights <- NULL
  testthat::expect_error(project(meta_weights = missing_column), "must contain")

  ## Meta weights named after something the cohort does not hold
  unknown_port <- make_meta_weights(tickers = c("bt_alpha", "bt_unknown"))
  testthat::expect_error(project(meta_weights = unknown_port), "no column for")

  ## Meta weights set on a date the cohort never saw
  future_dates <- make_meta_weights(weights_by_date = list(c(0.6, 0.4)),
                                    dates = as.Date("2030-01-15"))
  testthat::expect_error(project(meta_weights = future_dates), "does not cover")
})

testthat::test_that("a base portfolio that is not fully invested is refused", {
  ## The projection inherits any shortfall, so it is caught at source rather than renormalized
  underinvested <- make_projection_cohort(
    base_weights = c(bt_alpha = list(c(0.5, 0.3, 0)), bt_beta = list(c(0, 0.25, 0.75))))

  testthat::expect_error(
    project(cohort = underinvested),
    "do not sum to one"
  )
})

testthat::test_that("the arguments are class-checked", {
  testthat::expect_error(
    project_meta_weights_to_stocks(make_meta_weights(), list(), verbose = FALSE),
    "port_backtest_cohort"
  )
  testthat::expect_error(
    project_meta_weights_to_stocks("nope", make_projection_cohort(), verbose = FALSE),
    "data.frame or a meta_dataframe"
  )
  testthat::expect_error(project(tolerance = 0), "tolerance")
  testthat::expect_error(project(tolerance = -1), "tolerance")
})

testthat::test_that("meta weights are accepted as a meta_dataframe as well as a data.frame", {
  as_frame <- project()@data
  as_meta <- project(meta_weights = suppressWarnings(suppressMessages(
    create_meta_dataframe(make_meta_weights(), meta_dataframe_name = "meta_weights"))))@data

  testthat::expect_equal(as_meta, as_frame)
})


# Against a real cohort ---------------------------------------------------

testthat::test_that("a real cohort projects to weights that reproduce a hand calculation", {
  load(paste(testthat::test_path(), "/testdata/", "toy_preprocessed_port_obj.RData", sep = ""))

  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- suppressMessages(create_meta_xts(benchmark_returns_m_xts))

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
    signals_m_df = signals_m_df, fwd_return_m_df = fwd_return_m_df,
    liquidity_m_df = liquidity_m_df, volatility_m_df = volatility_m_df, config = config,
    benchmark_weights_m_df = benchmark_weights_m_df,
    benchmark_returns_m_xts = benchmark_returns_m_xts, verbose = FALSE, parallel = FALSE)))

  cohort <- suppressWarnings(suppressMessages(create_port_backtest_cohort(
    list(run_one(build_config("ew", "ew_by")), run_one(build_config("sw", "sw_by"))),
    cohort_name = "real_projection_cohort")))

  backtest_ids <- vapply(cohort@port_backtest_results_list,
                         function(x) x@backtest_identifier, character(1))
  rebalance_dates <- sort(unique(
    cohort@port_backtest_results_list[[1]]@port_stats_m_df@data$dates))

  meta_weights <- expand.grid(tickers = backtest_ids, dates = rebalance_dates,
                              KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  meta_weights$weights <- ifelse(meta_weights$tickers == backtest_ids[1], 0.6, 0.4)
  meta_weights$id <- paste0(meta_weights$tickers, "-", meta_weights$dates)
  meta_weights <- meta_weights[order(meta_weights$id), c("id", "tickers", "dates", "weights")]

  result <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = meta_weights, port_backtest_cohort = cohort,
    signals_m_df = signals_m_df, verbose = FALSE))@data

  ## The panel contract downstream
  sums <- tapply(result$weights, result$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(unique(result$dates))),
                         tolerance = 1e-10)
  testthat::expect_true(all(signals_m_df@data$id %in% result$id))
  testthat::expect_true(all(result$weights >= 0 & result$weights <= 1))

  ## Hand calculation against the cohort's own weights on a rebalance date
  check_date <- rebalance_dates[2]
  base_weights <- cohort@port_weights_m_df@data %>%
    dplyr::filter(dates == check_date) %>%
    dplyr::select(tickers, dplyr::all_of(unname(backtest_ids)))
  expected <- 0.6 * base_weights[[backtest_ids[1]]] + 0.4 * base_weights[[backtest_ids[2]]]
  got <- result %>% dplyr::filter(dates == check_date) %>%
    dplyr::arrange(match(tickers, base_weights$tickers))

  testthat::expect_equal(got$weights, expected, tolerance = 1e-12)
})
