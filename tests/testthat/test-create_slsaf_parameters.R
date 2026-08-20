#Construction
test_that("create_slsaf_parameters builds a valid object with documented defaults", {

  params <- create_slsaf_parameters()

  expect_s4_class(params, "slsaf_parameters")
  expect_true(methods::validObject(params))

  #The benchmark-proportional anchor and a unit conviction tilt
  expect_equal(params@bench_weight_tilt_eta, 1)
  expect_equal(params@badness_tilt_eta, 1)

  #The active budget is endogenous unless a ceiling is asked for
  expect_null(params@max_short_budget)

  #Only the long leg is configurable: the short leg is always signal weighted
  expect_s4_class(params@long_port_config, "sub_port_config")
  expect_equal(params@long_port_config@port_construction_method, "sw")
})

test_that("create_slsaf_parameters accepts a long leg by name or by configuration", {

  by_name <- create_slsaf_parameters(long_port_construction_method = "rp")
  expect_equal(by_name@long_port_config@port_construction_method, "rp")
  expect_s4_class(by_name@long_port_config@rp_parameters, "rp_parameters")

  by_config <- create_slsaf_parameters(
    long_port_config = create_sub_port_config(
      port_construction_method = "hrp",
      hrp_parameters = create_hrp_parameters(linkage = "ward.D2")
    )
  )
  expect_equal(by_config@long_port_config@port_construction_method, "hrp")
  expect_equal(by_config@long_port_config@hrp_parameters@linkage, "ward.D2")
})

test_that("create_slsaf_parameters carries the tilt exponents and the ceiling", {

  params <- create_slsaf_parameters(bench_weight_tilt_eta = 0,
                                    badness_tilt_eta = 2.5,
                                    max_short_budget = 0.3)

  expect_equal(params@bench_weight_tilt_eta, 0)
  expect_equal(params@badness_tilt_eta, 2.5)
  expect_equal(params@max_short_budget, 0.3)
})

#Validation
test_that("validate_slsaf_parameters requires a usable long leg", {

  expect_error(
    methods::new("slsaf_parameters", long_port_config = NULL),
    "long_port_config is required"
  )
  expect_error(
    methods::new("slsaf_parameters", long_port_config = list(port_construction_method = "sw")),
    "must be of class 'sub_port_config'"
  )
  #A layered method may not be nested inside another layered method
  expect_error(
    create_slsaf_parameters(long_port_construction_method = "mmaf"),
    "port_construction_method must be one of"
  )
  expect_error(
    create_slsaf_parameters(long_port_construction_method = "slsaf"),
    "port_construction_method must be one of"
  )
})

test_that("validate_slsaf_parameters rejects malformed exponents", {

  #A negative badness exponent would grant the largest underweight to the best-scoring
  #names, inverting the meaning of the leg
  expect_error(
    create_slsaf_parameters(badness_tilt_eta = -0.5),
    "badness_tilt_eta must be non-negative"
  )
  expect_error(
    create_slsaf_parameters(badness_tilt_eta = c(1, 2)),
    "badness_tilt_eta must be a single finite numeric"
  )
  expect_error(
    create_slsaf_parameters(bench_weight_tilt_eta = Inf),
    "bench_weight_tilt_eta must be a single finite numeric"
  )

  #A negative benchmark exponent is coherent: it protects large index names from
  #underweight, at the cost of budget
  expect_true(methods::validObject(create_slsaf_parameters(bench_weight_tilt_eta = -1)))
})

test_that("validate_slsaf_parameters rejects a malformed budget ceiling", {

  expect_error(create_slsaf_parameters(max_short_budget = 0),
               "max_short_budget must be NULL or a single numeric value")
  expect_error(create_slsaf_parameters(max_short_budget = 1.5),
               "max_short_budget must be NULL or a single numeric value")
  expect_error(create_slsaf_parameters(max_short_budget = c(0.1, 0.2)),
               "max_short_budget must be NULL or a single numeric value")
})

#Integration with the backtest configuration
test_that("create_port_backtest_config creates default slsaf parameters", {

  config <- create_port_backtest_config(
    chosen_score_metric_and_position = c(book_yield = "long"),
    selected_benchmark = "ibov",
    initial_buffer_period = 12,
    rebalancing_months = 12,
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "slsaf",
    config_name = "slsaf_default"
  )

  expect_s4_class(config@slsaf_parameters, "slsaf_parameters")
  expect_equal(config@slsaf_parameters@long_port_config@port_construction_method, "sw")
})

test_that("a slsaf configuration requires a benchmark and rejects overlapping policies", {

  base_args <- list(
    chosen_score_metric_and_position = c(book_yield = "long"),
    initial_buffer_period = 12,
    rebalancing_months = 12,
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "slsaf"
  )

  #The whole construction is an overlay on benchmark positions
  expect_error(
    do.call(create_port_backtest_config, base_args),
    "selected_benchmark must be provided when port_construction_method is 'slsaf'"
  )

  config <- do.call(create_port_backtest_config,
                    c(base_args, list(selected_benchmark = "ibov")))

  #The overlay already sets every active weight, so a concentration policy would either
  #duplicate it or silently fight it
  config_with_policy <- config
  config_with_policy@concentration_constraint_policy <- create_concentration_constraint_policy(
    benchmark = "ibov", max_abs_active_individual_weight = 0.02
  )
  expect_error(methods::validObject(config_with_policy),
               "concentration_constraint_policy is not supported for 'slsaf'")

  #The turnover buffer would promote the whole short block into the long block
  config_with_turnover <- config
  config_with_turnover@turnover_constraint_policy <- create_turnover_constraint_policy(
    quantile_range_buffer = 0.05, turnover_cap_rules = c(micro_caps = 0.01)
  )
  expect_error(methods::validObject(config_with_turnover),
               "turnover_constraint_policy is not supported for 'slsaf'")
})

test_that("add_slsaf_parameters attaches parameters and refuses other methods", {

  config <- create_port_backtest_config(
    chosen_score_metric_and_position = c(book_yield = "long"),
    selected_benchmark = "ibov",
    initial_buffer_period = 12,
    rebalancing_months = 12,
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "slsaf",
    config_name = "slsaf_tuned"
  )

  updated <- add_slsaf_parameters(config,
                                  long_port_construction_method = "rp",
                                  badness_tilt_eta = 2,
                                  max_short_budget = 0.25)

  expect_equal(updated@slsaf_parameters@long_port_config@port_construction_method, "rp")
  expect_equal(updated@slsaf_parameters@badness_tilt_eta, 2)
  expect_equal(updated@slsaf_parameters@max_short_budget, 0.25)

  #An existing object can be attached directly
  params <- create_slsaf_parameters(long_port_construction_method = "ew")
  expect_equal(add_slsaf_parameters(config, params)@slsaf_parameters@long_port_config@port_construction_method,
               "ew")

  #And it only makes sense for the method it configures
  ew_config <- create_port_backtest_config(
    chosen_score_metric_and_position = c(book_yield = "long"),
    selected_benchmark = "ibov",
    initial_buffer_period = 12,
    rebalancing_months = 12,
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "ew",
    config_name = "ew_config"
  )
  expect_error(add_slsaf_parameters(ew_config, params),
               "can only be added when port_construction_method = 'slsaf'")
})
