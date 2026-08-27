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


# The risk_targeted type --------------------------------------------------

make_risk_targeted_inner <- function(selected_benchmark = "ibov") {
  create_port_backtest_config(
    chosen_score_metric_and_position = NULL,
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = 24, rebalancing_months = c(6, 12),
    selected_benchmark = selected_benchmark,
    cov_est_method = create_cov_est_method(
      cov_estimation_method = "sample", cov_matrix_sample_size = 36,
      active_returns = !is.null(selected_benchmark), cov_matrix_benchmark = selected_benchmark),
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "custom_weights", config_name = "inner")
}


testthat::test_that("the type selector routes to the right parameter block", {
  multi <- suppressMessages(create_port_metabacktest_config(
    make_meta_port_config(), verbose = FALSE))
  testthat::expect_equal(multi@type, "multi_port")
  testthat::expect_null(multi@risk_target_parameters)

  targeted <- suppressMessages(create_port_metabacktest_config(
    make_risk_targeted_inner(), type = "risk_targeted",
    risk_target_parameters = create_risk_target_parameters("BOVA11", target = 4), verbose = FALSE))
  testthat::expect_equal(targeted@type, "risk_targeted")
  testthat::expect_s4_class(targeted@risk_target_parameters, "risk_target_parameters")

  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(
      make_meta_port_config(), type = "nonsense", verbose = FALSE)))
})

testthat::test_that("each type refuses the other's settings", {
  ## The construction method is what marks the path. On the risk-targeted path the weight comes
  ## from the targeting rule, so custom_weights is the only value that describes it honestly.
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(
      make_meta_port_config(), type = "risk_targeted",
      risk_target_parameters = create_risk_target_parameters("BOVA11", target = 4), verbose = FALSE)),
    "must be 'custom_weights' when type is 'risk_targeted'"
  )

  ## and the reverse, since custom_weights supplies no cross-section for multi_port to rank
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(
      make_risk_targeted_inner(), verbose = FALSE)),
    "must be one of 'ew', 'sw', 'rp', 'hrp' or 'mvo'"
  )

  ## risk_target_parameters would be silently ignored on the multi-portfolio path
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(
      make_meta_port_config(),
      risk_target_parameters = create_risk_target_parameters("BOVA11", target = 4), verbose = FALSE)),
    "only used when type is 'risk_targeted'"
  )

  ## The meta score being absent on the risk-targeted path no longer needs its own check here: the
  ## wrapped config refuses a score alongside custom_weights, so the two cannot disagree. Pinned so
  ## that requirement moving upstream stays visible rather than looking like lost coverage.
  testthat::expect_error(
    create_port_backtest_config(
      chosen_score_metric_and_position = c(exp_ret = "long"),
      eligibility_quantile_range = c(0, 1), initial_buffer_period = 24,
      rebalancing_months = c(6, 12), selected_benchmark = "ibov",
      cov_est_method = create_cov_est_method("sample", 36, TRUE, "ibov"),
      main_liquidity_metric = "mean_volfin_3m",
      port_construction_method = "custom_weights", config_name = "bad"),
    "must be NULL when port_construction_method is custom_weights"
  )
})

testthat::test_that("a tracking-error target requires a benchmark to track against", {
  testthat::expect_error(
    suppressMessages(create_port_metabacktest_config(
      make_risk_targeted_inner(selected_benchmark = NULL), type = "risk_targeted",
      risk_target_parameters = create_risk_target_parameters("BOVA11", target = 4,
                                             target_metric = "tracking_error"),
      verbose = FALSE)),
    "needs a selected_benchmark"
  )

  ## A volatility target is measured against nothing, so it does not
  testthat::expect_s4_class(
    suppressMessages(create_port_metabacktest_config(
      make_risk_targeted_inner(selected_benchmark = NULL), type = "risk_targeted",
      risk_target_parameters = create_risk_target_parameters("CASH", target = 10,
                                             target_metric = "volatility"),
      verbose = FALSE)),
    "port_metabacktest_config")
})

testthat::test_that("the score basis is only reported when a score was chosen", {
  ## Nothing to report when the weights come from the risk-targeting rule
  testthat::expect_silent(
    create_port_metabacktest_config(
      make_risk_targeted_inner(), type = "risk_targeted",
      risk_target_parameters = create_risk_target_parameters("BOVA11", target = 4)))
})


# risk_target_parameters ----------------------------------------------------------

testthat::test_that("create_risk_target_parameters defaults to a short daily estimator on raw returns", {
  params <- create_risk_target_parameters(residual_ticker = "BOVA11", target = 4)

  testthat::expect_s4_class(params, "risk_target_parameters")
  testthat::expect_equal(params@target_metric, "tracking_error")
  testthat::expect_equal(params@p, 1)
  testthat::expect_equal(params@vol_source, "ex_ante")
  testthat::expect_equal(params@min_weight, 0)
  testthat::expect_equal(params@max_weight, 1)

  ## Short and responsive, on daily data, which is the frequency realised risk is estimated at
  testthat::expect_s4_class(params@vol_cov_est_method, "cov_est_method")
  testthat::expect_equal(params@vol_cov_est_method@cov_estimation_method, "ewma")
  testthat::expect_equal(params@vol_cov_est_method@cov_matrix_sample_size, 60)

  ## Raw returns, because a tracking-error target is expressed through active weights
  testthat::expect_false(params@vol_cov_est_method@active_returns)
})

testthat::test_that("an estimator on active returns is refused, since it would double count", {
  testthat::expect_error(
    create_risk_target_parameters("BOVA11", target = 4,
                          vol_cov_est_method = create_cov_est_method("ewma", 60, TRUE, "ibov")),
    "subtract the benchmark twice"
  )
})

testthat::test_that("risk_target_parameters validates its own arguments", {
  testthat::expect_error(create_risk_target_parameters("", target = 4), "residual_ticker")
  testthat::expect_error(create_risk_target_parameters("BOVA11", target = 0),
                         "target must be a single positive")
  testthat::expect_error(create_risk_target_parameters("BOVA11", target = -4),
                         "target must be a single positive")
  testthat::expect_error(create_risk_target_parameters("BOVA11", target = 4, p = 0), "p must be")
  testthat::expect_error(
    create_risk_target_parameters("BOVA11", target = 4, target_metric = "nonsense"))
  testthat::expect_error(
    create_risk_target_parameters("BOVA11", target = 4, vol_source = "nonsense"))

  ## Bounds are long-only weights on the risky sleeve
  testthat::expect_error(create_risk_target_parameters("BOVA11", target = 4, min_weight = -0.1),
                         "long-only weights")
  testthat::expect_error(create_risk_target_parameters("BOVA11", target = 4, max_weight = 1.5),
                         "long-only weights")
  testthat::expect_error(
    create_risk_target_parameters("BOVA11", target = 4, min_weight = 0.8, max_weight = 0.2),
    "must not exceed max_weight")

  ## A rolling window needs enough observations to have a standard deviation
  testthat::expect_error(
    create_risk_target_parameters("BOVA11", target = 4, vol_source = "realized_rolling", vol_window = 1),
    "at least 2 months")
})

testthat::test_that("a supplied risk series needs no estimator", {
  params <- create_risk_target_parameters("BOVA11", target = 4, vol_source = "supplied")
  testthat::expect_equal(params@vol_source, "supplied")
  testthat::expect_null(params@vol_cov_est_method)
})


# add_risk_target_parameters ------------------------------------------------------

testthat::test_that("add_risk_target_parameters completes a configuration built without them", {
  ## residual_ticker and target have no sensible defaults, so unlike the other parameter blocks
  ## this one cannot be defaulted into place at construction. The configuration is therefore
  ## allowed to be incomplete and completed afterwards.
  bare <- suppressMessages(create_port_metabacktest_config(
    make_risk_targeted_inner(), type = "risk_targeted", verbose = FALSE))
  testthat::expect_null(bare@risk_target_parameters)

  completed <- bare %>%
    add_risk_target_parameters(residual_ticker = "BOVA11", target = 4, min_weight = 0.5)

  testthat::expect_s4_class(completed@risk_target_parameters, "risk_target_parameters")
  testthat::expect_equal(completed@risk_target_parameters@residual_ticker, "BOVA11")
  testthat::expect_equal(completed@risk_target_parameters@min_weight, 0.5)

  ## and an already-built parameters object attaches just as well
  attached <- bare %>% add_risk_target_parameters(create_risk_target_parameters("BOVA11", target = 6, p = 2))
  testthat::expect_equal(attached@risk_target_parameters@target, 6)
  testthat::expect_equal(attached@risk_target_parameters@p, 2)
})

testthat::test_that("add_risk_target_parameters refuses a multi-portfolio configuration", {
  multi <- suppressMessages(create_port_metabacktest_config(
    make_meta_port_config(), verbose = FALSE))

  testthat::expect_error(
    multi %>% add_risk_target_parameters(residual_ticker = "BOVA11", target = 4),
    "only available when type is 'risk_targeted'")
  testthat::expect_error(
    multi %>% add_risk_target_parameters(create_risk_target_parameters("BOVA11", target = 4)),
    "only available when type is 'risk_targeted'")
})
