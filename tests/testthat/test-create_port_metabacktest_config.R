# Fixture -----------------------------------------------------------------
# A meta-level port_backtest_config. main_liquidity_metric and
# transaction_costs_parameters are real here rather than dummies: they price the stock-level
# trades that the meta allocation implies once its weights are pushed through to individual
# stocks.

make_meta_port_config <- function(port_construction_method = "sw",
                                  chosen_score_metric_and_position = c(ann_info_ratio = "long"),
                                  selected_benchmark = "ibov",
                                  cov_matrix_sample_size = 36,
                                  ...) {

  cov_est_method <- create_cov_est_method(
    cov_estimation_method = "sample",
    cov_matrix_sample_size = cov_matrix_sample_size,
    active_returns = !is.null(selected_benchmark),
    cov_matrix_benchmark = selected_benchmark
  )

  create_port_backtest_config(
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = 24,
    rebalancing_months = c(6, 12),
    selected_benchmark = selected_benchmark,
    cov_est_method = cov_est_method,
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = port_construction_method,
    config_name = "meta_config",
    ...
  )
}


# Standard behaviour ------------------------------------------------------

testthat::test_that("create_port_metabacktest_config builds a valid object with sensible defaults", {
  meta_config <- suppressMessages(
    create_port_metabacktest_config(make_meta_port_config(), verbose = FALSE))

  testthat::expect_s4_class(meta_config, "port_metabacktest_config")
  testthat::expect_s4_class(meta_config@meta_port_backtest_config, "port_backtest_config")
  testthat::expect_equal(meta_config@return_basis, "net")
  testthat::expect_null(meta_config@cost_lookback)
  testthat::expect_equal(meta_config@config_name, "not_identified")
})

testthat::test_that("the meta score is read from the wrapped config rather than a slot of its own", {
  meta_config <- suppressMessages(create_port_metabacktest_config(
    make_meta_port_config(chosen_score_metric_and_position = c(ann_info_ratio = "long")),
    verbose = FALSE))

  testthat::expect_equal(
    meta_config@meta_port_backtest_config@chosen_score_metric_and_position,
    c(ann_info_ratio = "long")
  )
  testthat::expect_false("meta_score" %in% methods::slotNames(meta_config))
})

testthat::test_that("arguments are carried through", {
  meta_config <- suppressMessages(create_port_metabacktest_config(
    make_meta_port_config(),
    return_basis = "raw", cost_lookback = 12L, config_name = "meta_sw_ir", verbose = FALSE))

  testthat::expect_equal(meta_config@return_basis, "raw")
  testthat::expect_equal(meta_config@cost_lookback, 12L)
  testthat::expect_equal(meta_config@config_name, "meta_sw_ir")
})


# Score basis reporting ---------------------------------------------------

testthat::test_that("the constructor reports whether the chosen meta score is ex-ante or realized", {
  testthat::expect_message(
    create_port_metabacktest_config(
      make_meta_port_config(chosen_score_metric_and_position = c(IR = "long"))),
    "EX-ANTE"
  )
  testthat::expect_message(
    create_port_metabacktest_config(
      make_meta_port_config(chosen_score_metric_and_position = c(ann_info_ratio = "long"))),
    "REALIZED"
  )
  ## An unambiguous score has no counterpart to warn about
  testthat::expect_silent(
    create_port_metabacktest_config(
      make_meta_port_config(chosen_score_metric_and_position = c(max_dd = "short")))
  )
  ## Silenced on request
  testthat::expect_silent(
    create_port_metabacktest_config(
      make_meta_port_config(chosen_score_metric_and_position = c(IR = "long")),
      verbose = FALSE)
  )
})


# Rejected construction methods -------------------------------------------

testthat::test_that("methods that do not carry over to a set of portfolios are rejected by name", {
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(
      make_meta_port_config(port_construction_method = "cw"), verbose = FALSE)),
    "liquidity metric"
  )
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(
      make_meta_port_config(port_construction_method = "cs"), verbose = FALSE)),
    "liquidity metric"
  )
})

testthat::test_that("the supported methods are all accepted", {
  for (method in c("ew", "sw", "rp", "hrp", "mvo")) {
    meta_config <- suppressMessages(suppressWarnings(create_port_metabacktest_config(
      make_meta_port_config(port_construction_method = method), verbose = FALSE)))
    testthat::expect_s4_class(meta_config, "port_metabacktest_config")
    testthat::expect_equal(
      meta_config@meta_port_backtest_config@port_construction_method, method)
  }
})


# Required and forbidden inner settings -----------------------------------

testthat::test_that("a meta score is required, because the universe must carry an exp_ret_score", {
  no_score <- make_meta_port_config(port_construction_method = "ew",
                                    chosen_score_metric_and_position = NULL)
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(no_score, verbose = FALSE)),
    "chosen_score_metric_and_position"
  )
})

testthat::test_that("constraint policies that would be inert are rejected rather than ignored", {
  with_liquidity <- make_meta_port_config() %>%
    add_liquidity_floor_cutoffs(
      metric_name = "mean_volfin_3m",
      metric_cutoffs = list(c(micro_caps = 1, small_caps = 50000, mid_caps = 100000,
                              large_caps = 200000, mega_caps = 500000))) %>%
    add_liquidity_constraint_policy(liquidity_floor_rule = "small_caps")

  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(with_liquidity, verbose = FALSE)),
    "liquidity_constraint_policy"
  )

  ## add_concentration_constraint_policy() only accepts covariance-based methods, so the
  ## rejection is exercised through 'rp'
  with_concentration <- make_meta_port_config(port_construction_method = "rp") %>%
    add_concentration_constraint_policy(benchmark = "ibov",
                                        max_abs_active_individual_weight = 0.05)
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(with_concentration, verbose = FALSE)),
    "concentration_constraint_policy"
  )
})


# Covariance sample size --------------------------------------------------

testthat::test_that("a daily-sized covariance window warns that meta-level counts months", {
  ## 252 is the create_port_backtest_config default, which is a trading-day figure
  testthat::expect_warning(
    suppressMessages(create_port_metabacktest_config(
      make_meta_port_config(port_construction_method = "rp", cov_matrix_sample_size = 252),
      verbose = FALSE)),
    "months"
  )
  ## A plausible monthly window is accepted quietly
  testthat::expect_silent(
    suppressMessages(create_port_metabacktest_config(
      make_meta_port_config(port_construction_method = "rp", cov_matrix_sample_size = 36),
      verbose = FALSE))
  )
  ## Methods that do not use a covariance matrix are not second-guessed
  testthat::expect_silent(
    suppressMessages(create_port_metabacktest_config(
      make_meta_port_config(port_construction_method = "sw", cov_matrix_sample_size = 252),
      verbose = FALSE))
  )
})


# Argument validation -----------------------------------------------------

testthat::test_that("return_basis, cost_lookback and config_name are validated", {
  base_config <- make_meta_port_config()

  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(base_config, return_basis = "gross",
                                                     verbose = FALSE)),
    "return_basis"
  )
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(base_config,
                                                     return_basis = c("net", "raw"),
                                                     verbose = FALSE)),
    "return_basis"
  )
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(base_config, cost_lookback = 0,
                                                     verbose = FALSE)),
    "cost_lookback"
  )
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(base_config, cost_lookback = 2.5,
                                                     verbose = FALSE)),
    "cost_lookback"
  )
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(base_config,
                                                     config_name = c("a", "b"),
                                                     verbose = FALSE)),
    "config_name"
  )
})

testthat::test_that("the generic refuses anything that is not a port_backtest_config", {
  testthat::expect_error(create_port_metabacktest_config(list()))
  testthat::expect_error(create_port_metabacktest_config("sw"))
})
