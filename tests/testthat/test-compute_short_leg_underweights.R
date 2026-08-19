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
test_that("badness_tilt_eta walks the budget-versus-grading frontier monotonically", {

  #A realistic block: a few large constituents whose caps do not bind, and many
  #small ones whose caps do
  set.seed(7)
  n <- 60
  bench_weights <- stats::setNames(
    c(0.11, 0.08, 0.06, 0.05, stats::runif(n - 4, 0.001, 0.012)),
    paste0("T", 1:n)
  )
  exp_ret_score_raw <- stats::setNames(stats::runif(n, 0.35, 2.5), paste0("T", 1:n))
  short_budget <- sum(bench_weights)

  result_for <- function(badness_eta, bench_eta = 1){
    scores <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                       badness_tilt_eta = badness_eta,
                                       bench_weight_tilt_eta = bench_eta)
    compute_short_leg_underweights(scores, bench_weights)
  }

  #At the benchmark-proportional anchor the whole budget is spent, which is also
  #the ungraded case: every constituent is sold in full
  anchor <- result_for(badness_eta = 0)
  expect_equal(anchor$active_budget, short_budget)
  expect_equal(anchor$n_zeroed, n)

  #Raising the conviction tilt monotonically gives up budget in exchange for grading
  budgets <- vapply(c(0, 0.25, 0.5, 1, 2), function(e) result_for(e)$active_budget,
                    numeric(1))
  expect_true(all(diff(budgets) < 0))

  #And grading is what is bought: far fewer names are sold in full
  expect_lt(result_for(1)$n_zeroed, anchor$n_zeroed)
})

test_that("moving the basis away from benchmark proportionality loses budget in either direction", {

  set.seed(7)
  n <- 60
  bench_weights <- stats::setNames(
    c(0.11, 0.08, 0.06, 0.05, stats::runif(n - 4, 0.001, 0.012)),
    paste0("T", 1:n)
  )
  exp_ret_score_raw <- stats::setNames(stats::runif(n, 0.35, 2.5), paste0("T", 1:n))

  budget_for <- function(bench_eta){
    scores <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                       badness_tilt_eta = 0,
                                       bench_weight_tilt_eta = bench_eta)
    compute_short_leg_underweights(scores, bench_weights)$active_budget
  }

  #U = T is attained only at proportionality, so any exponent other than 1 loses
  peak <- budget_for(1)
  expect_equal(peak, sum(bench_weights))
  expect_lt(budget_for(0.5), peak)
  expect_lt(budget_for(2), peak)
  expect_lt(budget_for(0), peak)
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
