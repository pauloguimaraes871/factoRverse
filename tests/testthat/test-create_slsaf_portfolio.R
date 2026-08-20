#Helper to build a split universe from the artificial fixture
build_slsaf_universe <- function(exp_ret_score_raw = c(1.2, 0.2, 0.05, 0.04, 3),
                                 eligibility_quantile_range = c(0.6, 1),
                                 benchmark_weights_m_d_ref = NULL,
                                 ...){

  load(paste(test_path(), "/testdata/", "artificial_port_obj.RData", sep = ""))

  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]

  universe_m_d_ref <- signals_m_d_ref[, c("id", "tickers", "dates")]
  universe_m_d_ref$exp_ret_score_raw <- exp_ret_score_raw
  universe_m_d_ref$exp_ret_score <- exp_ret_score_raw

  if (is.null(benchmark_weights_m_d_ref)){
    benchmark_weights_m_d_ref <- benchmark_weights_m_df[
      which(benchmark_weights_m_df$dates == current_date), ]
  }

  classify_investment_universe(
    universe_m_d_ref = universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    selected_benchmark = "ibov",
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    include_benchmark_in_universe = TRUE,
    verbose = FALSE,
    ...
  )
}

#Structural invariants
test_that("create_slsaf_portfolio satisfies its four construction invariants", {

  universe_m_d_ref <- build_slsaf_universe()

  results <- create_slsaf_portfolio(
    universe_m_d_ref = universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("sw"),
    verbose = FALSE
  )

  bench_weights <- universe_m_d_ref$ibov_bench_weights
  weights <- results$weights
  active_weights <- weights - bench_weights

  long  <- which(universe_m_d_ref$is_long_candidate == 1L)
  short <- which(universe_m_d_ref$is_short_candidate == 1L)

  #The overlay is self-financing, so the portfolio is fully invested with no rescaling
  expect_equal(sum(active_weights), 0, tolerance = 1e-10)
  expect_equal(sum(weights), 1, tolerance = 1e-10)

  #Long-only throughout
  expect_true(all(weights >= 0))

  #A disliked constituent is never overweighted and never shorted
  expect_true(all(weights[short] <= bench_weights[short] + 1e-10))
  expect_true(all(weights[short] >= 0))

  #An eligible name is never structurally underweighted, which is the point of the method
  expect_true(all(weights[long] >= bench_weights[long] - 1e-10))

  #The released budget is exactly what the long leg receives
  expect_equal(sum(active_weights[long]), results$active_budget, tolerance = 1e-10)
  expect_equal(-sum(active_weights[short]), results$active_budget, tolerance = 1e-10)
})

test_that("the long leg receives exactly the budget the short leg releases", {

  universe_m_d_ref <- build_slsaf_universe()

  results <- create_slsaf_portfolio(
    universe_m_d_ref = universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("ew"),
    verbose = FALSE
  )

  long_tickers <- universe_m_d_ref$tickers[universe_m_d_ref$is_long_candidate == 1L]
  bench_weights <- stats::setNames(universe_m_d_ref$ibov_bench_weights, universe_m_d_ref$tickers)
  weights <- stats::setNames(results$weights, universe_m_d_ref$tickers)

  #An equal-weighted long leg spreads the budget evenly, so every long name gets the
  #same active weight
  active_long <- weights[long_tickers] - bench_weights[long_tickers]
  expect_equal(unname(active_long),
               rep(results$active_budget / length(long_tickers), length(long_tickers)),
               tolerance = 1e-10)
})

#Worked case
test_that("create_slsaf_portfolio reproduces the VALE3 and BRAP4 worked case", {

  #An index of realistic breadth: a mega cap far from its cap, a small disliked name
  #that cannot absorb its desired underweight, a residual block of small constituents,
  #and one eligible name to receive the released budget.
  n_rest <- 57

  #Scores chosen so VALE3 takes exactly 1.75% of the normalized short weights, which
  #puts BRAP4 at 0.99%, matching the worked example
  badness_vale <- 1 / 0.67
  badness_brap <- 1 / 1.16
  total_badness <- badness_vale / 0.0175
  rest_badness_each <- (total_badness - badness_vale - badness_brap) / n_rest

  tickers <- c("VALE3", "BRAP4", paste0("REST", seq_len(n_rest)), "GOOD")
  exp_ret_score_raw <- c(0.67, 1.16, rep(1 / rest_badness_each, n_rest), 2.50)
  bench_weights <- c(0.110, 0.002, rep(0.604 / n_rest, n_rest), 0.284)

  universe_m_d_ref <- data.frame(
    id      = paste0(tickers, "-2020-01-15"),
    tickers = tickers,
    dates   = rep(as.Date("2020-01-15"), length(tickers)),
    exp_ret_score_raw  = exp_ret_score_raw,
    exp_ret_score      = exp_ret_score_raw,
    ibov_bench_weights = bench_weights,
    is_long_candidate  = c(rep(0L, n_rest + 2), 1L),
    is_short_candidate = c(rep(1L, n_rest + 2), 0L),
    is_eligible        = rep(1, length(tickers)),
    stringsAsFactors = FALSE
  )

  results <- create_slsaf_portfolio(
    universe_m_d_ref = universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("sw"),
    #Pure badness, matching the worked example
    bench_weight_tilt_eta = 0,
    verbose = FALSE
  )

  #The short budget is the benchmark mass outside the eligible set
  expect_equal(results$short_budget, 0.716)

  #VALE3 is far from its cap, so it takes its full share of the budget: 71.6% * 1.75%
  expect_equal(unname(results$underweights[["VALE3"]]), 0.716 * 0.0175)
  expect_equal(round(unname(results$underweights[["VALE3"]]), 4), 0.0125)

  #BRAP4 wants 71.6% * 0.99% but holds only 0.20%, so it is sold in full
  expect_gt(0.716 * 0.0099, 0.002)
  expect_equal(unname(results$underweights[["BRAP4"]]), 0.002)
  expect_equal(results$weights[2], 0)

  #The mega cap keeps almost all of its index position, which is the structural
  #underweight this construction exists to avoid
  expect_gt(results$weights[1], 0.09)
  expect_lt(results$underweights[["VALE3"]] / 0.110, 0.12)

  #The single long name absorbs the whole released budget
  expect_equal(results$weights[length(tickers)], 0.284 + results$active_budget,
               tolerance = 1e-10)
  expect_equal(sum(results$weights), 1, tolerance = 1e-10)
})

#Tilts
test_that("badness_tilt_eta trades active budget for grading", {

  universe_m_d_ref <- build_slsaf_universe(
    exp_ret_score_raw = c(1.2, 0.2, 0.05, 0.04, 3),
    eligibility_quantile_range = c(0.8, 1)
  )

  budget_for <- function(badness_eta){
    create_slsaf_portfolio(
      universe_m_d_ref = universe_m_d_ref,
      selected_benchmark = "ibov",
      long_port_config = create_sub_port_config("ew"),
      badness_tilt_eta = badness_eta,
      verbose = FALSE
    )
  }

  #At the benchmark-proportional anchor the whole budget is spent, which is also the
  #ungraded case: every ineligible constituent is sold in full
  anchor <- budget_for(0)
  expect_equal(anchor$active_budget, anchor$short_budget, tolerance = 1e-10)
  expect_equal(anchor$n_zeroed, anchor$n_short)

  #Raising the tilt gives up budget in exchange for keeping index positions
  tilted <- budget_for(2)
  expect_lt(tilted$active_budget, anchor$active_budget)
})

test_that("max_short_budget caps the realized active budget", {

  universe_m_d_ref <- build_slsaf_universe()

  uncapped <- create_slsaf_portfolio(
    universe_m_d_ref = universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("sw"),
    verbose = FALSE
  )

  capped <- create_slsaf_portfolio(
    universe_m_d_ref = universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("sw"),
    max_short_budget = uncapped$active_budget / 2,
    verbose = FALSE
  )

  expect_equal(capped$active_budget, uncapped$active_budget / 2, tolerance = 1e-10)

  #Capping the budget shrinks every active position but keeps the construction valid
  expect_equal(sum(capped$weights), 1, tolerance = 1e-10)
  expect_true(all(capped$weights >= 0))
})

#Degenerate cases
test_that("create_slsaf_portfolio returns the benchmark when there is no short block", {

  #Every name eligible leaves nothing to underweight
  universe_m_d_ref <- build_slsaf_universe(eligibility_quantile_range = c(0, 1))

  expect_equal(sum(universe_m_d_ref$is_short_candidate), 0L)

  results <- create_slsaf_portfolio(
    universe_m_d_ref = universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("sw"),
    verbose = FALSE
  )

  expect_equal(results$short_budget, 0)
  expect_equal(results$active_budget, 0)
  expect_equal(results$weights, universe_m_d_ref$ibov_bench_weights, tolerance = 1e-10)
  expect_null(results$micro$short)
  expect_null(results$micro$long)
})

test_that("create_slsaf_portfolio renormalizes and warns when constituents are absent", {

  universe_m_d_ref <- build_slsaf_universe()

  #Drop a constituent from the universe: its benchmark weight is then unallocated
  truncated_universe_m_d_ref <- universe_m_d_ref[universe_m_d_ref$tickers != "Stock B", ]

  expect_warning(
    results <- create_slsaf_portfolio(
      universe_m_d_ref = truncated_universe_m_d_ref,
      selected_benchmark = "ibov",
      long_port_config = create_sub_port_config("sw"),
      verbose = FALSE
    ),
    "Renormalizing"
  )

  #The identity sum(w) = 1 must survive the missing constituent
  expect_equal(sum(results$weights), 1, tolerance = 1e-10)
})

#Validation
test_that("create_slsaf_portfolio validates its inputs", {

  universe_m_d_ref <- build_slsaf_universe()

  #The block split must have been produced upstream
  expect_error(
    create_slsaf_portfolio(
      universe_m_d_ref = universe_m_d_ref[, setdiff(names(universe_m_d_ref), "is_short_candidate")],
      selected_benchmark = "ibov",
      long_port_config = create_sub_port_config("sw"),
      verbose = FALSE
    ),
    "include_benchmark_in_universe = TRUE"
  )

  #A benchmark is what the whole construction is relative to
  expect_error(
    create_slsaf_portfolio(universe_m_d_ref = universe_m_d_ref,
                           selected_benchmark = NULL,
                           long_port_config = create_sub_port_config("sw"),
                           verbose = FALSE),
    "selected_benchmark must be a single character string"
  )
  expect_error(
    create_slsaf_portfolio(universe_m_d_ref = universe_m_d_ref,
                           selected_benchmark = "smll",
                           long_port_config = create_sub_port_config("sw"),
                           verbose = FALSE),
    "not found in universe_m_d_ref"
  )

  #The long leg must be configured through the shared contract
  expect_error(
    create_slsaf_portfolio(universe_m_d_ref = universe_m_d_ref,
                           selected_benchmark = "ibov",
                           long_port_config = list(port_construction_method = "sw"),
                           verbose = FALSE),
    "must be an object of class 'sub_port_config'"
  )

  #The short leg is always built from the unscaled score, so its absence is an error
  #rather than a fallback: a universe that was scaled and then had its scaler column
  #dropped would otherwise invert the scaler silently
  unscaled_missing_m_d_ref <- universe_m_d_ref
  unscaled_missing_m_d_ref$exp_ret_score_raw <- NULL
  expect_error(
    create_slsaf_portfolio(universe_m_d_ref = unscaled_missing_m_d_ref,
                           selected_benchmark = "ibov",
                           long_port_config = create_sub_port_config("sw"),
                           verbose = FALSE),
    "exp_ret_score_raw is required for slsaf"
  )
})

test_that("the short leg uses the unscaled score, never the scaled one", {

  universe_m_d_ref <- build_slsaf_universe()

  #Apply a scaler that is itself return-predictive. Inverting it into the short leg
  #would underweight exactly the names the scaler says are attractive.
  scaled_universe_m_d_ref <- universe_m_d_ref
  scaled_universe_m_d_ref$scaler <- c(2.0, 0.5, 1.5, 0.4, 1.0)
  scaled_universe_m_d_ref$exp_ret_score <-
    scaled_universe_m_d_ref$exp_ret_score_raw * scaled_universe_m_d_ref$scaler

  scaled <- create_slsaf_portfolio(
    universe_m_d_ref = scaled_universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("ew"),
    verbose = FALSE
  )
  unscaled <- create_slsaf_portfolio(
    universe_m_d_ref = universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("ew"),
    verbose = FALSE
  )

  #The scaler changes the long leg (through eligibility and scores) but must leave the
  #underweights untouched, since they are built from the raw score alone
  expect_equal(scaled$underweights, unscaled$underweights)
  expect_equal(scaled$active_budget, unscaled$active_budget)
})

#Integration with set_portfolio_weights
test_that("set_portfolio_weights builds a valid slsaf port object", {

  universe_m_d_ref <- build_slsaf_universe()

  port <- set_portfolio_weights(
    universe_m_d_ref = universe_m_d_ref,
    port_construction_method = "slsaf",
    sub_port_configs = list(long = create_sub_port_config("sw")),
    selected_benchmark = "ibov",
    verbose = FALSE
  )

  expect_s4_class(port, "port")
  expect_true(methods::validObject(port))
  expect_equal(port@port_construction_method, "slsaf")
  expect_equal(sum(port@weights), 1, tolerance = 1e-10)

  #Both legs are reported as sub-portfolios
  expect_named(port@micro, c("long", "short"))
  expect_s4_class(port@micro$long, "port")
  expect_s4_class(port@micro$short, "port")

  #The diagnostics travel with the portfolio statistics, so they reach port_stats_m_df
  expect_true(all(c("slsaf_short_budget", "slsaf_active_budget", "slsaf_n_long",
                    "slsaf_n_short", "slsaf_n_zeroed") %in% names(port@port_stats)))
  expect_equal(port@port_stats$slsaf_n_long, sum(universe_m_d_ref$is_long_candidate))
  expect_equal(port@port_stats$slsaf_n_short, sum(universe_m_d_ref$is_short_candidate))

  #The whole benchmark is represented, which is what makes an underweight expressible
  expect_setequal(port@eligible_assets,
                  universe_m_d_ref$tickers[universe_m_d_ref$is_eligible == 1])
})

test_that("set_portfolio_weights requires returns when the slsaf long leg needs a covariance matrix", {

  universe_m_d_ref <- build_slsaf_universe()

  #A score-based long leg needs no risk model
  expect_s4_class(
    set_portfolio_weights(universe_m_d_ref = universe_m_d_ref,
                          port_construction_method = "slsaf",
                          sub_port_configs = list(long = create_sub_port_config("ew")),
                          selected_benchmark = "ibov",
                          verbose = FALSE),
    "port"
  )

  #A covariance-based one does
  expect_error(
    set_portfolio_weights(universe_m_d_ref = universe_m_d_ref,
                          port_construction_method = "slsaf",
                          sub_port_configs = list(long = create_sub_port_config("rp")),
                          selected_benchmark = "ibov",
                          verbose = FALSE),
    "Covariance matrix estimation requires returns data"
  )
})
