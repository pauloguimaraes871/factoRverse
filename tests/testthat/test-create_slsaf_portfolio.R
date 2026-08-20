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

#Helper: a controlled universe wide enough that the cap does not dominate the short leg
build_slsaf_controlled_universe <- function(n_short = 40, n_long = 15,
                                            short_score_range = c(0.40, 1.20),
                                            long_score_range = c(1.60, 3.00),
                                            short_bench_range = c(0.030, 0.004),
                                            long_bench_weight = 0.010){

  tickers <- c(paste0("S", seq_len(n_short)), paste0("L", seq_len(n_long)))

  ##Scores are ordered within each block so monotonicity is directly checkable
  exp_ret_score_raw <- c(
    seq(short_score_range[1], short_score_range[2], length.out = n_short),
    seq(long_score_range[1], long_score_range[2], length.out = n_long)
  )

  bench_weights <- c(
    seq(short_bench_range[1], short_bench_range[2], length.out = n_short),
    rep(long_bench_weight, n_long)
  )
  bench_weights <- bench_weights / sum(bench_weights)

  data.frame(
    id      = paste0(tickers, "-2020-01-15"),
    tickers = tickers,
    dates   = rep(as.Date("2020-01-15"), length(tickers)),
    exp_ret_score_raw  = exp_ret_score_raw,
    exp_ret_score      = exp_ret_score_raw,
    ibov_bench_weights = bench_weights,
    is_long_candidate  = c(rep(0L, n_short), rep(1L, n_long)),
    is_short_candidate = c(rep(1L, n_short), rep(0L, n_long)),
    is_eligible        = rep(1, length(tickers)),
    stringsAsFactors = FALSE
  )
}

#Leg separation by score
test_that("the short leg holds strictly worse scores than the long leg under a plain cascade", {

  universe_m_d_ref <- build_slsaf_universe()

  long_scores  <- universe_m_d_ref$exp_ret_score[universe_m_d_ref$is_long_candidate == 1L]
  short_scores <- universe_m_d_ref$exp_ret_score[universe_m_d_ref$is_short_candidate == 1L]

  expect_gt(length(long_scores), 0)
  expect_gt(length(short_scores), 0)

  #With only the eligibility quantile rule in play, the blocks are a clean cut of the
  #score distribution: every long name outranks every short name
  expect_gte(min(long_scores), max(short_scores))
  expect_gt(mean(long_scores), mean(short_scores))
})

test_that("a strict liquidity floor can invert the leg score ordering", {

  #The floor governs what may be bought, not what may be underweighted, so a
  #high-scoring but illiquid constituent is excluded from the long block and lands in
  #the short block. On a narrow universe that can invert the ordering outright: the
  #short leg then holds names scoring better than the long leg, which means
  #underweighting names the signal likes. This is a real configuration hazard rather
  #than a defect, and it is pinned here so it cannot regress silently.
  load(paste(test_path(), "/testdata/", "artificial_port_obj.RData", sep = ""))

  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]
  universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(-Alpha, -Beta, -Gamma)
  universe_m_d_ref$exp_ret_score_raw <- c(1.2, 0.2, 0.05, 0.04, 3)
  universe_m_d_ref$exp_ret_score <- universe_m_d_ref$exp_ret_score_raw

  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date), ]
  benchmark_weights_m_d_ref <- benchmark_weights_m_df[
    which(benchmark_weights_m_df$dates == current_date), ]

  strict_liquidity_policy <- liquidity_constraint_policy
  strict_liquidity_policy$liquidity_floor_rule <- "small_caps"

  universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = universe_m_d_ref,
    eligibility_quantile_range = c(0.2, 1),
    liquidity_constraint_policy = strict_liquidity_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    liquidity_m_d_ref = liquidity_m_d_ref,
    selected_benchmark = "ibov",
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    include_benchmark_in_universe = TRUE,
    verbose = FALSE
  )

  long_scores  <- universe_m_d_ref$exp_ret_score[universe_m_d_ref$is_long_candidate == 1L]
  short_scores <- universe_m_d_ref$exp_ret_score[universe_m_d_ref$is_short_candidate == 1L]

  #The best-scoring name in the whole universe sits in the short block
  best_ticker <- universe_m_d_ref$tickers[which.max(universe_m_d_ref$exp_ret_score)]
  expect_equal(best_ticker, "Stock E")
  expect_equal(universe_m_d_ref$is_short_candidate[universe_m_d_ref$tickers == "Stock E"], 1L)

  #So separation fails, and here it fails even on the mean
  expect_lt(min(long_scores), max(short_scores))
  expect_lt(mean(long_scores), mean(short_scores))

  #The portfolio is still well formed: the invariants never depended on score ordering
  results <- create_slsaf_portfolio(
    universe_m_d_ref = universe_m_d_ref,
    selected_benchmark = "ibov",
    long_port_config = create_sub_port_config("sw"),
    verbose = FALSE
  )
  expect_equal(sum(results$weights), 1, tolerance = 1e-10)
  expect_true(all(results$weights >= 0))
})

#What the exponents actually do
test_that("the benchmark exponent maximizes the budget at proportionality, not monotonically", {

  universe_m_d_ref <- build_slsaf_controlled_universe()

  budget_for <- function(bench_eta, badness_eta = 0){
    create_slsaf_portfolio(universe_m_d_ref, "ibov", create_sub_port_config("ew"),
                           bench_weight_tilt_eta = bench_eta,
                           badness_tilt_eta = badness_eta,
                           verbose = FALSE)
  }

  #With the conviction tilt switched off, the benchmark term alone is proportional to
  #benchmark weight exactly at an exponent of 1, which is where the whole available
  #budget converts into underweight
  anchor <- budget_for(1)
  expect_equal(anchor$active_budget, anchor$short_budget, tolerance = 1e-10)

  #Moving the exponent in either direction loses budget
  expect_lt(budget_for(0.5)$active_budget, anchor$active_budget)
  expect_lt(budget_for(2)$active_budget, anchor$active_budget)
  expect_lt(budget_for(0)$active_budget, anchor$active_budget)

  #Once the conviction tilt is active the peak moves, because it depends on how badness
  #correlates with index weight. The exponent is therefore a basis, not a budget dial:
  #on this fixture badness and benchmark weight are positively correlated, so an
  #exponent of 1 overshoots proportionality and yields LESS budget than 0.
  expect_lt(budget_for(1, badness_eta = 1)$active_budget,
            budget_for(0, badness_eta = 1)$active_budget)
})

test_that("the badness exponent dislocates underweights toward conviction", {

  universe_m_d_ref <- build_slsaf_controlled_universe()
  n_short <- sum(universe_m_d_ref$is_short_candidate)
  short_bench <- universe_m_d_ref$ibov_bench_weights[universe_m_d_ref$is_short_candidate == 1L]
  short_scores <- universe_m_d_ref$exp_ret_score_raw[universe_m_d_ref$is_short_candidate == 1L]

  profile_for <- function(badness_eta){
    results <- create_slsaf_portfolio(universe_m_d_ref, "ibov", create_sub_port_config("ew"),
                                      badness_tilt_eta = badness_eta, verbose = FALSE)
    relative_trim <- results$underweights / short_bench
    list(spread = stats::sd(relative_trim),
         trim = relative_trim,
         active_budget = results$active_budget,
         n_zeroed = results$n_zeroed)
  }

  profiles <- lapply(c(0, 0.5, 1, 2, 3), profile_for)

  #Dislocation: the dispersion of relative trims widens monotonically, so underweight
  #concentrates on the names actually disliked rather than being spread evenly
  spreads <- vapply(profiles, function(p) p$spread, numeric(1))
  expect_true(all(diff(spreads) > 0))
  expect_equal(spreads[1], 0, tolerance = 1e-10)

  #At the anchor every constituent is sold in full, which is the ungraded corner
  expect_equal(profiles[[1]]$n_zeroed, n_short)

  #Raising the tilt keeps more index positions alive while spending less budget
  budgets <- vapply(profiles, function(p) p$active_budget, numeric(1))
  zeroed  <- vapply(profiles, function(p) p$n_zeroed, numeric(1))
  expect_true(all(diff(budgets) < 0))
  expect_lt(zeroed[length(zeroed)], zeroed[1])

  #And the trim is ordered by conviction: worse names give up more of their position
  uncapped <- profiles[[3]]$trim < 1 - 1e-9
  expect_gt(sum(uncapped), 5)
  expect_equal(
    stats::cor(profiles[[3]]$trim[uncapped], (1 / short_scores)[uncapped], method = "spearman"),
    1
  )
})

#Weight monotonicity in the score
test_that("within each leg, a better name is never treated worse", {

  #No scaler is applied, so exp_ret_score and exp_ret_score_raw coincide and the
  #comparison is purely about the signal
  universe_m_d_ref <- build_slsaf_controlled_universe()
  expect_equal(universe_m_d_ref$exp_ret_score, universe_m_d_ref$exp_ret_score_raw)

  results <- create_slsaf_portfolio(universe_m_d_ref, "ibov", create_sub_port_config("sw"),
                                    verbose = FALSE)

  bench_weights <- universe_m_d_ref$ibov_bench_weights
  weights <- results$weights
  active_weights <- weights - bench_weights

  long  <- which(universe_m_d_ref$is_long_candidate == 1L)
  short <- which(universe_m_d_ref$is_short_candidate == 1L)

  #Scores are increasing within each block by construction
  expect_true(all(diff(universe_m_d_ref$exp_ret_score[long]) > 0))
  expect_true(all(diff(universe_m_d_ref$exp_ret_score[short]) > 0))

  #Long leg: a better name always receives at least as much ACTIVE weight
  expect_true(all(diff(active_weights[long]) > -1e-12))

  #Short leg: a better name always retains at least as large a share of its benchmark
  #position, wherever the cap has not already taken the whole position
  retained_share <- weights[short] / bench_weights[short]
  uncapped <- retained_share > 1e-9
  expect_gt(sum(uncapped), 5)
  expect_true(all(diff(retained_share[uncapped]) > -1e-12))

  #The same statement is FALSE for total weight, because a worse name can simply hold a
  #larger index position to begin with. This is why the invariant is about active
  #weight, and it is worth pinning so the distinction is not lost later.
  expect_false(all(diff(weights[short]) > -1e-12))
})
