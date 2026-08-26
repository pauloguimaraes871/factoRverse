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

testthat::test_that("projecting to stocks nets off, so implied turnover is at most the weighted average", {

  ## The economic reason for projecting rather than allocating at portfolio level. When two base
  ## portfolios hold the same name and move it in opposite directions, the combined book trades
  ## less than the two books separately. Formally, with meta weights m held fixed,
  ##   |sum_p m_p * dv(p,s)| <= sum_p m_p * |dv(p,s)|
  ## so summing over stocks and halving gives meta turnover <= sum_p m_p * turnover_p.
  ## This is a property of the projection itself, kept clear of the backtest's drift between
  ## rebalances and of the meta reallocation, which are separate sources of trading.
  load(paste(testthat::test_path(), "/testdata/", "toy_preprocessed_port_obj.RData", sep = ""))

  signals_m_df <- create_meta_dataframe(signals_m_df, type = "signals")
  fwd_return_m_df <- create_meta_dataframe(fwd_return_m_df, type = "target")
  liquidity_m_df <- create_meta_dataframe(liquidity_m_df)
  volatility_m_df <- create_meta_dataframe(volatility_m_df)
  benchmark_weights_m_df <- create_meta_dataframe(benchmark_weights_m_df, type = "weights")
  benchmark_returns_m_xts <- suppressMessages(create_meta_xts(benchmark_returns_m_xts))

  ## Two sleeves driven by different signals, so they hold different names and genuinely trade
  ## against each other. Two sleeves on the same signal satisfy the inequality almost trivially,
  ## since they rarely move a stock in opposite directions: measured on such a cohort the saving
  ## was around a fifth of a percent, against up to about two percent here. Neither is large, and
  ## the test asserts the inequality rather than any particular magnitude.
  build_config <- function(score, name) {
    create_port_backtest_config(
      chosen_score_metric_and_position = score,
      eligibility_quantile_range = c(0.67, 1.0), selected_benchmark = "ibov",
      initial_buffer_period = 2, rebalancing_months = c(1, 4),
      port_construction_method = "sw", main_liquidity_metric = "mean_volfin_3m",
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
    list(run_one(build_config(c(book_yield = "long"), "value_by")),
         run_one(build_config(c(vol_36m = "short"), "lowvol_by"))),
    cohort_name = "netting_cohort")))

  backtest_ids <- unname(vapply(cohort@port_backtest_results_list,
                                function(x) x@backtest_identifier, character(1)))
  base_weights <- cohort@port_weights_m_df@data
  cohort_dates <- sort(unique(base_weights$dates))

  ## Meta weights held fixed, so the only trading compared is the base portfolios' own
  meta_split <- c(0.6, 0.4)
  meta_weights <- expand.grid(tickers = backtest_ids, dates = cohort_dates,
                              KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  meta_weights$weights <- ifelse(meta_weights$tickers == backtest_ids[1],
                                 meta_split[1], meta_split[2])
  meta_weights$id <- paste0(meta_weights$tickers, "-", meta_weights$dates)
  meta_weights <- meta_weights[order(meta_weights$id), c("id", "tickers", "dates", "weights")]

  projected <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = meta_weights, port_backtest_cohort = cohort, verbose = FALSE))@data

  ## Implied turnover between consecutive dates, the same 0.5 * sum |delta| the cost engine uses
  implied_turnover <- function(df, weight_column, from_date, to_date) {
    from <- df[df$dates == from_date, c("tickers", weight_column)]
    to <- df[df$dates == to_date, c("tickers", weight_column)]
    joined <- dplyr::full_join(from, to, by = "tickers", suffix = c("_from", "_to"))
    joined[is.na(joined)] <- 0
    sum(abs(joined[[paste0(weight_column, "_to")]] -
              joined[[paste0(weight_column, "_from")]])) / 2
  }

  strictly_less <- 0L

  for (i in seq(2, length(cohort_dates))) {
    from_date <- cohort_dates[i - 1]
    to_date <- cohort_dates[i]

    meta_turnover <- implied_turnover(projected, "weights", from_date, to_date)

    weighted_base_turnover <- sum(vapply(seq_along(backtest_ids), function(p) {
      meta_split[p] * implied_turnover(base_weights, backtest_ids[p], from_date, to_date)
    }, numeric(1)))

    testthat::expect_lte(meta_turnover, weighted_base_turnover + 1e-10)
    if (meta_turnover < weighted_base_turnover - 1e-8) strictly_less <- strictly_less + 1L
  }

  ## The inequality must bite somewhere, or the base portfolios never offset and the test
  ## would hold trivially for portfolios that share nothing
  testthat::expect_gt(strictly_less, 0L)
})


# A residual sleeve -------------------------------------------------------

projection_residual <- "CASH"


make_residual_signals <- function(dates = projection_dates) {
  ## The residual is a tradable row of the stock universe, not a portfolio
  df <- expand.grid(tickers = c(projection_stocks, projection_residual), dates = dates,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$id <- paste0(df$tickers, "-", df$dates)
  df$book_yield <- seq_len(nrow(df)) / 100
  df <- df[order(df$id), c("id", "tickers", "dates", "book_yield")]
  rownames(df) <- NULL
  suppressWarnings(suppressMessages(
    create_meta_dataframe(df, meta_dataframe_name = "residual_signals")))
}


make_residual_meta_weights <- function(portfolio_weights = c(0.3, 0.3), residual_weight = 0.4,
                                       dates = projection_dates[1]) {
  rows <- purrr::map_dfr(dates, function(d) {
    data.frame(tickers = c(projection_ports, projection_residual), dates = d,
               weights = c(portfolio_weights, residual_weight), stringsAsFactors = FALSE)
  })
  rows$id <- paste0(rows$tickers, "-", rows$dates)
  rows <- rows[order(rows$id), c("id", "tickers", "dates", "weights")]
  rownames(rows) <- NULL
  rows
}


testthat::test_that("a residual sleeve becomes a stock weight directly, with no portfolio behind it", {
  result <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = make_residual_meta_weights(),
    port_backtest_cohort = make_projection_cohort(),
    signals_m_df = make_residual_signals(),
    residual_ticker = projection_residual, verbose = FALSE))@data

  first <- result %>%
    dplyr::filter(dates == projection_dates[1]) %>%
    dplyr::arrange(tickers)

  ## bt_alpha holds AAA and BBB evenly, bt_beta holds BBB and CCC one to three, at 0.3 each:
  ##   AAA  = 0.3 * 0.50                = 0.150
  ##   BBB  = 0.3 * 0.50 + 0.3 * 0.25   = 0.225
  ##   CASH =                             0.400   (its meta weight, untouched)
  ##   CCC  =              0.3 * 0.75   = 0.225
  testthat::expect_equal(first$tickers, c("AAA", "BBB", projection_residual, "CCC"))
  testthat::expect_equal(first$weights, c(0.150, 0.225, 0.400, 0.225), tolerance = 1e-12)

  ## and the whole panel is still fully invested
  sums <- tapply(result$weights, result$dates, sum)
  testthat::expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-12)
})

testthat::test_that("the residual weight passes through unscaled at any level", {
  for (residual_weight in c(0, 0.25, 0.75)) {
    portfolio_weight <- (1 - residual_weight) / 2
    result <- suppressMessages(project_meta_weights_to_stocks(
      meta_weights_m_df = make_residual_meta_weights(
        portfolio_weights = rep(portfolio_weight, 2), residual_weight = residual_weight),
      port_backtest_cohort = make_projection_cohort(),
      signals_m_df = make_residual_signals(),
      residual_ticker = projection_residual, verbose = FALSE))@data

    residual_row <- result %>%
      dplyr::filter(dates == projection_dates[1], tickers == projection_residual)
    testthat::expect_equal(residual_row$weights, residual_weight, tolerance = 1e-12)
  }
})

testthat::test_that("a residual that a sleeve also holds has the two contributions summed", {
  ## The cohort's weight panel carries a row for every stock, so the residual would otherwise
  ## appear twice: once at whatever the sleeves hold, once at its own meta weight
  cohort <- make_projection_cohort(
    base_weights = c(bt_alpha = list(c(0.5, 0.5, 0)), bt_beta = list(c(0, 0.25, 0.75))))

  ## Give the cohort a CASH column of its own by adding it as a fourth stock the sleeves hold
  weights_df <- cohort@port_weights_m_df@data
  extra <- weights_df[weights_df$tickers == "AAA", ]
  extra$tickers <- projection_residual
  extra$id <- paste0(projection_residual, "-", extra$dates)
  extra$bt_alpha <- 0
  extra$bt_beta <- 0
  combined <- rbind(weights_df, extra)
  combined <- combined[order(combined$id), ]
  cohort@port_weights_m_df@data <- combined

  result <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = make_residual_meta_weights(),
    port_backtest_cohort = cohort,
    signals_m_df = make_residual_signals(),
    residual_ticker = projection_residual, verbose = FALSE))@data

  ## One row per stock per date, not two
  testthat::expect_equal(anyDuplicated(result$id), 0L)
  residual_row <- result %>%
    dplyr::filter(dates == projection_dates[1], tickers == projection_residual)
  testthat::expect_equal(nrow(residual_row), 1L)
  testthat::expect_equal(residual_row$weights, 0.4, tolerance = 1e-12)
})

testthat::test_that("the residual is held forward on the same schedule as the portfolios", {
  ## Meta weights set on the first and third dates only
  meta_weights <- rbind(
    make_residual_meta_weights(c(0.3, 0.3), 0.4, projection_dates[1]),
    make_residual_meta_weights(c(0.1, 0.1), 0.8, projection_dates[3])
  )
  meta_weights <- meta_weights[order(meta_weights$id), ]

  result <- suppressMessages(project_meta_weights_to_stocks(
    meta_weights_m_df = meta_weights,
    port_backtest_cohort = make_projection_cohort(),
    signals_m_df = make_residual_signals(),
    residual_ticker = projection_residual, verbose = FALSE))@data

  residual_by_date <- result %>%
    dplyr::filter(tickers == projection_residual) %>%
    dplyr::arrange(dates)

  ## The second date carries the first's weight, the fourth carries the third's
  testthat::expect_equal(residual_by_date$weights, c(0.4, 0.4, 0.8, 0.8), tolerance = 1e-12)
})

testthat::test_that("a residual sleeve is validated before anything is projected", {
  cohort <- make_projection_cohort()
  signals <- make_residual_signals()

  ## Not a single name
  testthat::expect_error(
    suppressMessages(project_meta_weights_to_stocks(
      make_residual_meta_weights(), cohort, signals,
      residual_ticker = c("A", "B"), verbose = FALSE)),
    "single character value")

  ## Naming a base portfolio, which is a portfolio rather than a holding
  testthat::expect_error(
    suppressMessages(project_meta_weights_to_stocks(
      make_meta_weights(), cohort, signals,
      residual_ticker = "bt_alpha", verbose = FALSE)),
    "also a base portfolio")

  ## Carrying no meta weight
  testthat::expect_error(
    suppressMessages(project_meta_weights_to_stocks(
      make_meta_weights(), cohort, signals,
      residual_ticker = projection_residual, verbose = FALSE)),
    "carries no meta weight")

  ## Not tradable in the stock universe
  testthat::expect_error(
    suppressMessages(project_meta_weights_to_stocks(
      make_residual_meta_weights(), cohort, make_projection_signals(),
      residual_ticker = projection_residual, verbose = FALSE)),
    "not a row of signals_m_df")

  ## Everything in the residual, leaving no portfolio to project
  all_residual <- make_residual_meta_weights(portfolio_weights = c(0, 0), residual_weight = 1)
  all_residual <- all_residual[all_residual$tickers == projection_residual, ]
  testthat::expect_error(
    suppressMessages(project_meta_weights_to_stocks(
      all_residual, cohort, signals,
      residual_ticker = projection_residual, verbose = FALSE)),
    "no portfolio to project")
})

testthat::test_that("the meta weights must still sum to one across sleeves and residual together", {
  short <- make_residual_meta_weights(portfolio_weights = c(0.3, 0.3), residual_weight = 0.1)
  testthat::expect_error(
    suppressMessages(project_meta_weights_to_stocks(
      short, make_projection_cohort(), make_residual_signals(),
      residual_ticker = projection_residual, verbose = FALSE)),
    "do not sum to one")
})
