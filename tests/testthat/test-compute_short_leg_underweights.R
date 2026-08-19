#Budget distribution
test_that("the whole budget is spent only when short weights are benchmark-proportional", {

  #Since sum(desired) = T = sum(bench_weights) by construction, the cap can only
  #fail to bind anywhere if every desired underweight equals its benchmark weight.
  #That happens exactly when the short weights are proportional to benchmark weights.
  bench_weights <- c(A = 0.11, B = 0.002, C = 0.05, D = 0.03)

  proportional <- compute_short_leg_underweights(short_scores = bench_weights * 7,
                                                 bench_weights = bench_weights)

  expect_equal(proportional$short_budget, sum(bench_weights))
  expect_equal(proportional$active_budget, sum(bench_weights))
  expect_equal(proportional$underweights, bench_weights)

  #That maximum-budget point is also the ungraded one: every name is fully sold
  expect_equal(proportional$n_zeroed, 4L)
})

test_that("any departure from benchmark proportionality strictly loses budget", {

  bench_weights <- c(A = 0.11, B = 0.002, C = 0.05, D = 0.03)

  proportional <- compute_short_leg_underweights(bench_weights, bench_weights)
  distorted    <- compute_short_leg_underweights(c(A = 1, B = 1, C = 1, D = 1),
                                                 bench_weights)

  #U = T is an upper bound attained only at proportionality
  expect_lt(distorted$active_budget, proportional$active_budget)
  expect_lte(distorted$active_budget, distorted$short_budget)

  #And the budget lost is exactly the excess discarded by the binding caps
  desired <- distorted$short_budget * rep(0.25, 4)
  expect_equal(distorted$short_budget - distorted$active_budget,
               sum(pmax(desired - bench_weights, 0)))
})

test_that("compute_short_leg_underweights caps each underweight at the benchmark position", {

  #B is tiny, so its desired underweight cannot be delivered in full
  short_scores  <- c(A = 1, B = 1)
  bench_weights <- c(A = 0.30, B = 0.002)

  res <- compute_short_leg_underweights(short_scores, bench_weights)

  #Desired is 0.151 each, so B binds at its 0.002 benchmark weight
  expect_equal(unname(res$underweights[["A"]]), 0.302 * 0.5)
  expect_equal(unname(res$underweights[["B"]]), 0.002)

  #The excess is discarded rather than redistributed, so U falls short of T
  expect_lt(res$active_budget, res$short_budget)
  expect_equal(res$active_budget, 0.151 + 0.002)
  expect_equal(res$n_zeroed, 1L)
})

test_that("compute_short_leg_underweights reproduces the VALE3 and BRAP4 worked case", {

  #Scores chosen so the normalized short weights are exactly 1.75% and 0.99%,
  #with a residual block carrying the rest of the 71.6% benchmark mass
  short_scores  <- c(VALE3 = 1.75, BRAP4 = 0.99, REST = 97.26)
  bench_weights <- c(VALE3 = 0.110, BRAP4 = 0.002, REST = 0.604)

  res <- compute_short_leg_underweights(short_scores, bench_weights)

  expect_equal(res$short_budget, 0.716)

  #VALE3 is far from its cap, so it takes its full share of the budget
  expect_equal(unname(res$underweights[["VALE3"]]), 0.716 * 0.0175)
  expect_equal(round(unname(res$underweights[["VALE3"]]), 4), 0.0125)

  #BRAP4 wanted more than it holds, so it is zeroed at its benchmark weight
  expect_gt(0.716 * 0.0099, 0.002)
  expect_equal(unname(res$underweights[["BRAP4"]]), 0.002)

  #A mega cap keeps most of its benchmark position, a small disliked name loses all of it
  expect_gt(0.110 - res$underweights[["VALE3"]], 0.09)
  expect_equal(unname(0.002 - res$underweights[["BRAP4"]]), 0)
})

#Feasibility invariants
test_that("compute_short_leg_underweights always returns feasible underweights", {

  set.seed(42)
  short_scores  <- stats::setNames(stats::runif(40, 0.2, 4), paste0("T", 1:40))
  bench_weights <- stats::setNames(stats::runif(40, 0.001, 0.12), paste0("T", 1:40))

  res <- compute_short_leg_underweights(short_scores, bench_weights)

  #The invariants the rest of the construction depends on
  expect_true(all(res$underweights >= 0))
  expect_true(all(res$underweights <= bench_weights + 1e-10))
  expect_lte(res$active_budget, res$short_budget + 1e-10)
  expect_equal(res$active_budget, sum(res$underweights))
  expect_named(res$underweights, names(short_scores))
})

#Ceiling
test_that("max_short_budget rescales proportionally and lands exactly on the ceiling", {

  short_scores  <- c(A = 1, B = 1, C = 2)
  bench_weights <- c(A = 0.20, B = 0.20, C = 0.20)

  uncapped <- compute_short_leg_underweights(short_scores, bench_weights)
  capped   <- compute_short_leg_underweights(short_scores, bench_weights,
                                             max_short_budget = 0.30)

  #The ceiling is reached exactly, in one step
  expect_equal(capped$active_budget, 0.30)

  #Every underweight shrinks by the same factor
  expect_equal(capped$underweights, uncapped$underweights * (0.30 / uncapped$active_budget))

  #Feasibility survives the rescaling
  expect_true(all(capped$underweights <= bench_weights + 1e-10))

  #And the reported available budget is untouched
  expect_equal(capped$short_budget, uncapped$short_budget)
})

test_that("max_short_budget is inert when the realized budget is already below it", {

  short_scores  <- c(A = 1, B = 1)
  bench_weights <- c(A = 0.05, B = 0.05)

  uncapped <- compute_short_leg_underweights(short_scores, bench_weights)
  capped   <- compute_short_leg_underweights(short_scores, bench_weights,
                                             max_short_budget = 0.90)

  expect_equal(capped$underweights, uncapped$underweights)
  expect_equal(capped$active_budget, uncapped$active_budget)
})

test_that("max_short_budget keeps capped names feasible after rescaling", {

  #A mix of binding and non-binding names before the ceiling is applied
  short_scores  <- c(A = 1, B = 1, C = 1)
  bench_weights <- c(A = 0.30, B = 0.002, C = 0.05)

  res <- compute_short_leg_underweights(short_scores, bench_weights,
                                        max_short_budget = 0.05)

  expect_equal(res$active_budget, 0.05)
  expect_true(all(res$underweights <= bench_weights + 1e-10))

  #Rescaling releases previously zeroed names, so they are no longer fully sold
  expect_lt(res$underweights[["B"]], 0.002)
  expect_equal(res$n_zeroed, 0L)
})

#Degenerate inputs
test_that("compute_short_leg_underweights handles a single-name short block", {

  res <- compute_short_leg_underweights(short_scores = c(A = 3),
                                        bench_weights = c(A = 0.11))

  #The whole budget is this name's own benchmark weight, so it is fully zeroed
  expect_equal(unname(res$underweights), 0.11)
  expect_equal(res$short_budget, 0.11)
  expect_equal(res$active_budget, 0.11)
  expect_equal(res$n_zeroed, 1L)
})

test_that("compute_short_leg_underweights handles an empty short block", {

  res <- compute_short_leg_underweights(short_scores = numeric(0),
                                        bench_weights = numeric(0))

  expect_length(res$underweights, 0)
  expect_equal(res$short_budget, 0)
  expect_equal(res$active_budget, 0)
  expect_equal(res$n_zeroed, 0L)
})

#Validation
test_that("compute_short_leg_underweights rejects malformed scores and weights", {

  bench_weights <- c(A = 0.02, B = 0.02)

  expect_error(compute_short_leg_underweights(c(A = "x", B = "y"), bench_weights),
               "short_scores must be numeric")
  expect_error(compute_short_leg_underweights(c(A = 1, B = NA_real_), bench_weights),
               "must not contain NAs")
  #A zero or negative score would break the normalization
  expect_error(compute_short_leg_underweights(c(A = 1, B = 0), bench_weights),
               "short_scores must be strictly positive and finite")
  expect_error(compute_short_leg_underweights(c(A = 1, B = -2), bench_weights),
               "short_scores must be strictly positive and finite")
  expect_error(compute_short_leg_underweights(c(A = 1, B = 1), c(A = 0.02)),
               "same length as short_scores")
  expect_error(compute_short_leg_underweights(c(A = 1, B = 1), c(A = 0.02, B = 0)),
               "strictly positive, finite and not greater than 1")
})

test_that("compute_short_leg_underweights rejects a malformed ceiling", {

  short_scores  <- c(A = 1, B = 1)
  bench_weights <- c(A = 0.02, B = 0.02)

  expect_error(compute_short_leg_underweights(short_scores, bench_weights,
                                              max_short_budget = 0),
               "max_short_budget must be NULL or a single numeric value")
  expect_error(compute_short_leg_underweights(short_scores, bench_weights,
                                              max_short_budget = 1.2),
               "max_short_budget must be NULL or a single numeric value")
  expect_error(compute_short_leg_underweights(short_scores, bench_weights,
                                              max_short_budget = c(0.1, 0.2)),
               "max_short_budget must be NULL or a single numeric value")
})

#Interaction with the score tilts
test_that("bench_weight_tilt_eta moves the realized budget toward its proportional maximum, then away", {

  #A realistic block: a few large constituents whose caps do not bind, and many
  #small ones whose caps do
  set.seed(7)
  n <- 60
  bench_weights <- stats::setNames(
    c(0.11, 0.08, 0.06, 0.05, stats::runif(n - 4, 0.001, 0.012)),
    paste0("T", 1:n)
  )
  exp_ret_score_raw <- stats::setNames(stats::runif(n, 0.35, 2.5), paste0("T", 1:n))

  budget_for <- function(badness_eta, bench_eta){
    scores <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                       badness_tilt_eta = badness_eta,
                                       bench_weight_tilt_eta = bench_eta)
    compute_short_leg_underweights(scores, bench_weights)$active_budget
  }

  base <- budget_for(1, 0)

  #Raising the tilt from zero moves score mass toward the large names, i.e. toward
  #benchmark proportionality, so more of the budget converts into underweight
  expect_gt(budget_for(1, 1), base)

  #But the relationship is NOT monotone. The transformed benchmark weight is not
  #proportional to the benchmark weight, so a large exponent overshoots
  #proportionality and concentrates the underweight on a handful of mega caps,
  #whose caps then bind and discard the rest of the budget.
  expect_lt(budget_for(1, 5), budget_for(1, 1))
  expect_lt(budget_for(1, 5), base)

  #The badness tilt concentrates the distribution instead, pushing more names past
  #their cap, so it monotonically gives up budget
  expect_lt(budget_for(2, 0), base)
  expect_lt(budget_for(3, 0), budget_for(2, 0))
})

test_that("the realized budget can never exceed the short budget under any tilt", {

  set.seed(11)
  n <- 40
  bench_weights <- stats::setNames(
    c(0.13, 0.07, stats::runif(n - 2, 0.001, 0.02)), paste0("T", 1:n)
  )
  exp_ret_score_raw <- stats::setNames(stats::runif(n, 0.35, 2.5), paste0("T", 1:n))

  for (bench_eta in c(0, 0.5, 1, 2, 4)){
    for (badness_eta in c(0, 1, 2)){
      scores <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                         badness_tilt_eta = badness_eta,
                                         bench_weight_tilt_eta = bench_eta)
      res <- compute_short_leg_underweights(scores, bench_weights)

      expect_lte(res$active_budget, res$short_budget + 1e-10)
      expect_true(all(res$underweights <= bench_weights + 1e-10))
    }
  }
})
