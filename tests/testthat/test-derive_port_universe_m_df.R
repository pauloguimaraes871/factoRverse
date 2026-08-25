# Fixture -----------------------------------------------------------------
# A minimal but structurally faithful cohort. The deriver reads port_backtest_results_list,
# port_costs_m_xts_list, port_metrics_m_xts_list and backtest_workflow_common, and
# port_backtest_cohort validity only constrains cohort_name, so the object is built directly
# rather than by running two full backtests. Statistics are in percentage points.

make_toy_stats_m_df <- function(rebalance_dates, seed_value, active_returns) {

  ## Two rows per rebalance date, exactly as run_port_backtest builds them
  stats_df <- data.frame(
    tickers = rep(c("raw_return", "net_return"), each = length(rebalance_dates)),
    dates   = rep(rebalance_dates, times = 2),
    stringsAsFactors = FALSE
  )

  ## Realized figures differ between the raw and net bases
  basis_offset <- ifelse(stats_df$tickers == "raw_return", 0.5, 0)
  n <- nrow(stats_df)

  if (active_returns) {
    ## Realized block
    stats_df$act_ann_ret   <- seq_len(n) * seed_value + basis_offset
    stats_df$track_err     <- seq_len(n) * seed_value * 2 + basis_offset
    stats_df$ann_track_err <- seq_len(n) * seed_value * 3 + basis_offset
    stats_df$info_ratio    <- seq_len(n) * seed_value * 4 + basis_offset
    stats_df$alpha         <- seq_len(n) * seed_value * 5 + basis_offset
    ## Ex-ante block, joined to both bases and therefore identical across them
    stats_df$act_exp_ret   <- rep(seq_along(rebalance_dates) * seed_value * 6, times = 2)
    stats_df$act_risk      <- rep(seq_along(rebalance_dates) * seed_value * 7, times = 2)
    stats_df$IR            <- rep(seq_along(rebalance_dates) * seed_value * 8, times = 2)
  } else {
    stats_df$ann_ret       <- seq_len(n) * seed_value + basis_offset
    stats_df$std_dev       <- seq_len(n) * seed_value * 2 + basis_offset
    stats_df$sharpe_ratio  <- seq_len(n) * seed_value * 4 + basis_offset
    stats_df$exp_ret       <- rep(seq_along(rebalance_dates) * seed_value * 6, times = 2)
    stats_df$risk          <- rep(seq_along(rebalance_dates) * seed_value * 7, times = 2)
    stats_df$SR            <- rep(seq_along(rebalance_dates) * seed_value * 8, times = 2)
  }

  stats_df$id <- paste0(stats_df$tickers, "-", stats_df$dates)
  stats_df <- stats_df[order(stats_df$id), ]
  rownames(stats_df) <- NULL
  stats_df <- stats_df[, c("id", "tickers", "dates",
                           setdiff(names(stats_df), c("id", "tickers", "dates")))]

  suppressWarnings(suppressMessages(
    create_meta_dataframe(stats_df, meta_dataframe_name = "toy_stats")
  ))
}


make_toy_result <- function(backtest_identifier, rebalance_dates, seed_value, active_returns) {
  ## Only port_stats_m_df and backtest_identifier are read by the deriver; the remaining
  ## slots are filled with the minimum the class requires.
  methods::new(
    "port_backtest_results",
    port_backtest_config = NULL,
    port_weights_m_df = suppressWarnings(suppressMessages(create_meta_dataframe(
      data.frame(id = paste0("AAA-", rebalance_dates), tickers = "AAA",
                 dates = rebalance_dates, eop_port_weights = 1, stringsAsFactors = FALSE)
    ))),
    transactions_log = methods::new("transactions_log"),
    port_costs_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      xts::xts(data.frame(total_cost = rep(0.1, length(rebalance_dates))),
               order.by = rebalance_dates),
      type = "metrics", meta_xts_name = "toy"))),
    port_metrics_m_xts = NULL,
    port_returns_m_xts = suppressWarnings(suppressMessages(create_meta_xts(
      xts::xts(data.frame(net_return = rep(1, length(rebalance_dates))),
               order.by = rebalance_dates),
      type = "metrics", meta_xts_name = "toy"))),
    final_stock_port = NULL,
    port_construction_method = "ew",
    stock_universe_m_df = NULL,
    final_stock_universe_m_d_ref = NULL,
    port_stats_m_df = make_toy_stats_m_df(rebalance_dates, seed_value, active_returns),
    final_port_stats_m_d_ref = NULL,
    port_backtest_workflow = list(list(selected_benchmark = if (active_returns) "ibov" else NULL)),
    backtest_identifier = backtest_identifier,
    update = TRUE
  )
}


make_toy_cohort <- function(n_dates = 24L, active_returns = TRUE, rebalance_every = 6L,
                            n_ports = 2L, cohort_name = "toy_cohort") {

  dates_backtest <- seq(as.Date("2022-01-15"), by = "month", length.out = n_dates)
  rebalance_dates <- dates_backtest[seq(1L, n_dates, by = rebalance_every)]
  port_names <- c("bt_alpha", "bt_beta")[seq_len(n_ports)]

  results_list <- lapply(seq_along(port_names), function(i) {
    make_toy_result(port_names[i], rebalance_dates, seed_value = i, active_returns = active_returns)
  })
  names(results_list) <- port_names

  ## Costs are stamped one day after the rebalance they pay for, on every backtest date
  cost_dates <- dates_backtest + 1
  make_cost <- function(alpha_value, beta_value) {
    mat <- cbind(bt_alpha = rep(alpha_value, length(cost_dates)),
                 bt_beta  = rep(beta_value, length(cost_dates)))[, seq_len(n_ports), drop = FALSE]
    suppressWarnings(suppressMessages(create_meta_xts(
      xts::xts(mat, order.by = cost_dates), type = "metrics",
      meta_xts_name = cohort_name, source = port_names)))
  }

  metric_mat <- cbind(bt_alpha = seq_along(dates_backtest) * 1.0,
                      bt_beta  = seq_along(dates_backtest) * 2.0)[, seq_len(n_ports), drop = FALSE]

  methods::new(
    "port_backtest_cohort",
    cohort_name = cohort_name,
    port_backtest_results_list = results_list,
    port_weights_m_df = suppressWarnings(suppressMessages(create_meta_dataframe(
      data.frame(id = paste0("AAA-", dates_backtest), tickers = "AAA",
                 dates = dates_backtest, bt_alpha = 1, stringsAsFactors = FALSE)
    ))),
    port_costs_m_xts_list = list(total_cost_m_xts = make_cost(0.1, 0.2),
                                 turnover_m_xts   = make_cost(5, 10)),
    port_returns_m_xts_list = list(),
    port_metrics_m_xts_list = list(roe_3m_m_xts = suppressWarnings(suppressMessages(
      create_meta_xts(xts::xts(metric_mat, order.by = dates_backtest), type = "metrics",
                      meta_xts_name = cohort_name, source = port_names)))),
    port_stats_m_xts_nested_list = list(),
    backtest_workflow_common = list(
      selected_benchmark = if (active_returns) "ibov" else NULL,
      dates_backtest = dates_backtest
    )
  )
}


# Standard behaviour ------------------------------------------------------

testthat::test_that("derive_port_universe_m_df returns a port_universe_m_df covering every date and portfolio", {
  cohort <- make_toy_cohort()

  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)
  ))

  testthat::expect_s4_class(universe, "port_universe_m_df")
  testthat::expect_equal(nrow(universe@data), 24L * 2L)
  testthat::expect_setequal(unique(universe@data$tickers), c("bt_alpha", "bt_beta"))
  testthat::expect_equal(universe@data$id, paste0(universe@data$tickers, "-", universe@data$dates))
  testthat::expect_false(is.unsorted(universe@data$id))
  testthat::expect_equal(names(universe@data)[1:3], c("id", "tickers", "dates"))
  testthat::expect_equal(utils::tail(names(universe@data), 1), "stats_age_months")
})

testthat::test_that("cost and metric blocks are prefixed and joined per portfolio", {
  cohort <- make_toy_cohort()
  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)
  ))
  nms <- colnames(universe@data)

  testthat::expect_true(all(c("avg_total_cost", "avg_turnover", "metric_roe_3m") %in% nms))
  ## Base statistic names are carried over unchanged
  testthat::expect_true(all(c("track_err", "info_ratio", "act_risk", "IR") %in% nms))
})

testthat::test_that("the meta score column is not created here", {
  cohort <- make_toy_cohort()
  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)
  ))
  testthat::expect_false("exp_ret_score" %in% colnames(universe@data))
})


# Carry-forward -----------------------------------------------------------

testthat::test_that("statistics are carried forward and their staleness is reported", {
  cohort <- make_toy_cohort(rebalance_every = 6L)

  testthat::expect_warning(
    universe <- suppressMessages(derive_port_universe_m_df(cohort, verbose = FALSE)),
    "carried forward"
  )

  dates_backtest <- cohort@backtest_workflow_common$dates_backtest
  d <- universe@data

  ## On a base rebalance date the value is fresh
  on_source <- d[d$dates == dates_backtest[7] & d$tickers == "bt_alpha", ]
  testthat::expect_equal(on_source$stats_age_months, 0)

  ## Two months later the same value is carried, and the age says so
  carried <- d[d$dates == dates_backtest[9] & d$tickers == "bt_alpha", ]
  testthat::expect_equal(carried$stats_age_months, 2)
  testthat::expect_equal(carried$IR, on_source$IR)
  testthat::expect_equal(carried$track_err, on_source$track_err)
})

testthat::test_that("dates before the first base rebalance carry no statistics", {
  cohort <- make_toy_cohort()
  ## Push the first rebalance out so early decision dates have nothing to carry
  for (i in seq_along(cohort@port_backtest_results_list)) {
    stats_obj <- cohort@port_backtest_results_list[[i]]@port_stats_m_df
    keep_dates <- sort(unique(stats_obj@data$dates))[-1]
    stats_obj@data <- stats_obj@data[stats_obj@data$dates %in% keep_dates, ]
    cohort@port_backtest_results_list[[i]]@port_stats_m_df <- stats_obj
  }

  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)))@data
  first_date <- min(cohort@backtest_workflow_common$dates_backtest)

  first_rows <- universe[universe$dates == first_date, ]
  testthat::expect_true(all(is.na(first_rows$stats_age_months)))
  testthat::expect_true(all(is.na(first_rows$IR)))
})

testthat::test_that("ex-ante figures are identical across return bases while realized ones are not", {
  cohort <- make_toy_cohort()
  net_u <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, return_basis = "net", verbose = FALSE)))@data
  raw_u <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, return_basis = "raw", verbose = FALSE)))@data

  ## Position-derived figures do not depend on the return basis
  testthat::expect_equal(net_u$IR, raw_u$IR)
  testthat::expect_equal(net_u$act_risk, raw_u$act_risk)

  ## Realized figures do
  testthat::expect_false(isTRUE(all.equal(net_u$track_err, raw_u$track_err)))
  testthat::expect_false(isTRUE(all.equal(net_u$act_ann_ret, raw_u$act_ann_ret)))
})


# Costs -------------------------------------------------------------------

testthat::test_that("cost averages exclude the cost of the rebalance being decided", {
  cohort <- make_toy_cohort()
  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)))@data
  dates_backtest <- cohort@backtest_workflow_common$dates_backtest

  ## Costs are stamped at date + 1 day, so the first decision date has no prior cost at all
  first_rows <- universe[universe$dates == dates_backtest[1], ]
  testthat::expect_true(all(is.na(first_rows$avg_total_cost)))

  ## Constant costs make the running average equal to the level itself thereafter,
  ## and the two portfolios keep their own figures
  later_alpha <- universe[universe$dates == dates_backtest[10] &
                            universe$tickers == "bt_alpha", ]
  later_beta <- universe[universe$dates == dates_backtest[10] &
                           universe$tickers == "bt_beta", ]
  testthat::expect_equal(later_alpha$avg_total_cost, 0.1, tolerance = 1e-12)
  testthat::expect_equal(later_beta$avg_total_cost, 0.2, tolerance = 1e-12)
  testthat::expect_equal(later_alpha$avg_turnover, 5, tolerance = 1e-12)
})

testthat::test_that("a trailing cost lookback averages only the requested observations", {
  cohort <- make_toy_cohort()
  ## Make costs vary so the window length is observable
  cost_xts <- cohort@port_costs_m_xts_list$total_cost_m_xts@data
  cost_xts[, "bt_alpha"] <- seq_len(nrow(cost_xts))
  cohort@port_costs_m_xts_list$total_cost_m_xts@data <- cost_xts

  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, cost_lookback = 3L, verbose = FALSE)))@data
  dates_backtest <- cohort@backtest_workflow_common$dates_backtest

  ## At decision date 10, prior costs are observations 1 to 9; the last three are 7, 8 and 9
  row <- universe[universe$dates == dates_backtest[10] & universe$tickers == "bt_alpha", ]
  testthat::expect_equal(row$avg_total_cost, mean(c(7, 8, 9)), tolerance = 1e-12)
})


# Custom metrics ----------------------------------------------------------

testthat::test_that("custom metrics are taken at the decision date itself", {
  cohort <- make_toy_cohort()
  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)))@data
  dates_backtest <- cohort@backtest_workflow_common$dates_backtest

  row <- universe[universe$dates == dates_backtest[10] & universe$tickers == "bt_beta", ]
  testthat::expect_equal(row$metric_roe_3m, 20, tolerance = 1e-12)
})


# Basis messaging ---------------------------------------------------------

testthat::test_that("message_meta_score_basis names the basis and the counterpart", {
  testthat::expect_message(message_meta_score_basis("IR"), "EX-ANTE")
  testthat::expect_message(message_meta_score_basis("IR"), "ann_info_ratio")
  testthat::expect_message(message_meta_score_basis("ann_info_ratio"), "REALIZED")
  testthat::expect_message(message_meta_score_basis("ann_info_ratio"), "'IR'")

  testthat::expect_message(message_meta_score_basis("act_risk"), "EX-ANTE")
  testthat::expect_message(message_meta_score_basis("act_risk"), "ann_track_err")
  testthat::expect_message(message_meta_score_basis("track_err"), "REALIZED")
  testthat::expect_message(message_meta_score_basis("track_err"), "act_risk")

  ## The expected-return figures also warn that they are not returns
  testthat::expect_message(message_meta_score_basis("act_exp_ret"), "dimensionless")
})

testthat::test_that("message_meta_score_basis returns the basis invisibly and stays quiet otherwise", {
  testthat::expect_equal(
    suppressMessages(message_meta_score_basis("IR")), "ex-ante")
  testthat::expect_equal(
    suppressMessages(message_meta_score_basis("track_err")), "realized")

  ## A statistic with no counterpart of the other basis is unambiguous
  testthat::expect_silent(message_meta_score_basis("max_dd"))
  testthat::expect_null(message_meta_score_basis("max_dd"))

  ## Silenced on request
  testthat::expect_silent(message_meta_score_basis("IR", verbose = FALSE))

  testthat::expect_error(message_meta_score_basis(c("IR", "track_err")), "single")
})

testthat::test_that("every counterpart named in the basis table is itself in the table", {
  basis_table <- meta_score_basis_table()
  testthat::expect_true(all(basis_table$counterpart %in% basis_table$stat))
  testthat::expect_false(any(duplicated(basis_table$stat)))
})


# Edge cases and validation ----------------------------------------------

testthat::test_that("a cohort without a benchmark carries the raw statistic family only", {
  cohort <- make_toy_cohort(active_returns = FALSE)
  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)))@data
  nms <- colnames(universe)

  testthat::expect_true(all(c("sharpe_ratio", "SR", "exp_ret", "risk") %in% nms))
  testthat::expect_false(any(c("info_ratio", "track_err", "IR", "act_risk") %in% nms))
})

testthat::test_that("a single-portfolio cohort warns that it cannot be allocated across", {
  cohort <- make_toy_cohort(n_ports = 1L)
  ## The carry-forward warning also fires here, so both are asserted rather than leaked
  testthat::expect_warning(
    testthat::expect_warning(
      suppressMessages(derive_port_universe_m_df(cohort, verbose = FALSE)),
      "at least"
    ),
    "carried forward"
  )
})

testthat::test_that("invalid arguments are rejected with informative errors", {
  cohort <- make_toy_cohort()

  testthat::expect_error(
    derive_port_universe_m_df(list(), verbose = FALSE),
    "port_backtest_cohort"
  )
  testthat::expect_error(
    derive_port_universe_m_df(cohort, return_basis = "gross", verbose = FALSE)
  )
  testthat::expect_error(
    derive_port_universe_m_df(cohort, cost_lookback = 0, verbose = FALSE),
    "cost_lookback"
  )
  testthat::expect_error(
    derive_port_universe_m_df(cohort, cost_lookback = 2.5, verbose = FALSE),
    "cost_lookback"
  )
})

testthat::test_that("a cohort missing its date grid or its results is rejected", {
  cohort <- make_toy_cohort()

  no_dates <- cohort
  no_dates@backtest_workflow_common$dates_backtest <- NULL
  testthat::expect_error(
    derive_port_universe_m_df(no_dates, verbose = FALSE), "dates_backtest")

  no_results <- cohort
  no_results@port_backtest_results_list <- list()
  testthat::expect_error(
    derive_port_universe_m_df(no_results, verbose = FALSE), "no port_backtest_results")

  no_stats <- cohort
  no_stats@port_backtest_results_list[[1]]@port_stats_m_df <- NULL
  testthat::expect_error(
    suppressWarnings(derive_port_universe_m_df(no_stats, verbose = FALSE)), "port_stats_m_df")
})


# Class contract ---------------------------------------------------------

testthat::test_that("the class rejects an exp_ret_score column and a missing age column", {
  cohort <- make_toy_cohort()
  universe <- suppressWarnings(suppressMessages(
    derive_port_universe_m_df(cohort, verbose = FALSE)))

  build <- function(df) {
    methods::new("port_universe_m_df", data = df, workflow = NULL,
                 signals = names(df)[-c(1:3)],
                 unique_dates = length(unique(df$dates)),
                 unique_tickers = length(unique(df$tickers)),
                 n_obs = nrow(df), current_date = max(df$dates),
                 meta_dataframe_name = "bad", port_metabacktest_workflow = list())
  }

  with_score <- universe@data
  with_score$exp_ret_score <- 1
  testthat::expect_error(build(with_score), "exp_ret_score")

  without_age <- universe@data
  without_age$stats_age_months <- NULL
  testthat::expect_error(build(without_age), "stats_age_months")

  non_numeric <- universe@data
  non_numeric$IR <- as.character(non_numeric$IR)
  testthat::expect_error(build(non_numeric), "numeric")
})
