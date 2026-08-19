#Baseline behaviour
test_that("compute_short_leg_scores recovers the plain reciprocal under default etas", {

  exp_ret_score_raw <- c(A = 0.67, B = 1.16, C = 2.40, D = 0.85)
  bench_weights     <- c(A = 0.110, B = 0.002, C = 0.045, D = 0.008)

  scores <- compute_short_leg_scores(exp_ret_score_raw = exp_ret_score_raw,
                                     bench_weights = bench_weights)

  #Defaults are badness_tilt_eta = 1 and bench_weight_tilt_eta = 0, so g^0 = 1
  expect_equal(scores, 1 / exp_ret_score_raw)
  expect_named(scores, c("A", "B", "C", "D"))
  expect_true(all(scores > 0))
  expect_true(all(is.finite(scores)))
})

test_that("compute_short_leg_scores is monotone decreasing in the expected return score", {

  exp_ret_score_raw <- c(worst = 0.4, bad = 0.8, middling = 1.0, decent = 1.9)
  bench_weights     <- c(worst = 0.02, bad = 0.02, middling = 0.02, decent = 0.02)

  scores <- compute_short_leg_scores(exp_ret_score_raw = exp_ret_score_raw,
                                     bench_weights = bench_weights)

  #A worse stock must always earn a larger underweight score
  expect_true(all(diff(scores) < 0))

  #The reciprocal is exact, so the score ratio is the inverse score ratio
  expect_equal(unname(scores[["worst"]] / scores[["decent"]]), 1.9 / 0.4)
})

#Badness tilt
test_that("badness_tilt_eta adds convexity in the bad tail and softens borderline names", {

  #One clearly bad name, one borderline name scoring just better than average
  exp_ret_score_raw <- c(bad = 0.4, borderline = 1.1)
  bench_weights     <- c(bad = 0.02, borderline = 0.02)

  linear <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                     badness_tilt_eta = 1)
  convex <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                     badness_tilt_eta = 2)

  #The bad name is punished disproportionately more
  expect_gt(convex[["bad"]] / linear[["bad"]],
            convex[["borderline"]] / linear[["borderline"]])

  #And a borderline name (badness below 1) is punished disproportionately less
  expect_lt(convex[["borderline"]], linear[["borderline"]])

  #The gap between the two widens, which is the whole point of the tilt
  expect_gt(convex[["bad"]] / convex[["borderline"]],
            linear[["bad"]] / linear[["borderline"]])
})

test_that("badness_tilt_eta below 1 flattens the score distribution", {

  exp_ret_score_raw <- c(bad = 0.4, good = 2.0)
  bench_weights     <- c(bad = 0.02, good = 0.02)

  linear <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                     badness_tilt_eta = 1)
  flat   <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                     badness_tilt_eta = 0.5)

  expect_lt(flat[["bad"]] / flat[["good"]], linear[["bad"]] / linear[["good"]])

  #At eta = 0 every name carries the same badness contribution
  neutral <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                      badness_tilt_eta = 0)
  expect_equal(unname(neutral), c(1, 1))
})

#Benchmark weight tilt
test_that("bench_weight_tilt_eta shifts score mass toward large index names", {

  #Identical scores isolate the benchmark tilt
  exp_ret_score_raw <- c(mega = 1.0, mid = 1.0, small = 1.0, tiny = 1.0)
  bench_weights     <- c(mega = 0.12, mid = 0.03, small = 0.01, tiny = 0.002)

  neutral <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                      bench_weight_tilt_eta = 0)
  tilted  <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                      bench_weight_tilt_eta = 1)

  #With no tilt, equal scores stay equal
  expect_equal(unname(neutral), rep(1, 4))

  #With a positive tilt, the ordering follows benchmark weight
  expect_true(all(diff(tilted) < 0))
  expect_gt(tilted[["mega"]], tilted[["tiny"]])

  #A negative tilt reverses it, protecting mega caps from underweight
  protective <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                         bench_weight_tilt_eta = -1)
  expect_lt(protective[["mega"]], protective[["tiny"]])
})

test_that("bench_weight_tilt_eta is inert when benchmark weights carry no information", {

  exp_ret_score_raw <- c(A = 0.5, B = 1.5, C = 2.0)

  #Identical benchmark weights give a zero-variance transform, so g is all ones
  identical_weights <- c(A = 0.02, B = 0.02, C = 0.02)
  expect_equal(
    compute_short_leg_scores(exp_ret_score_raw, identical_weights,
                             bench_weight_tilt_eta = 2),
    compute_short_leg_scores(exp_ret_score_raw, identical_weights,
                             bench_weight_tilt_eta = 0)
  )

  #A single name transforms to 1 as well
  single <- compute_short_leg_scores(exp_ret_score_raw = c(A = 0.5),
                                     bench_weights = c(A = 0.11),
                                     bench_weight_tilt_eta = 3)
  expect_equal(unname(single), 1 / 0.5)
})

test_that("the two tilts combine multiplicatively", {

  exp_ret_score_raw <- c(A = 0.5, B = 1.5, C = 2.0, D = 0.9)
  bench_weights     <- c(A = 0.12, B = 0.03, C = 0.01, D = 0.002)

  badness_only <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                           badness_tilt_eta = 1.7,
                                           bench_weight_tilt_eta = 0)
  bench_only   <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                           badness_tilt_eta = 0,
                                           bench_weight_tilt_eta = 1.3)
  both         <- compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                           badness_tilt_eta = 1.7,
                                           bench_weight_tilt_eta = 1.3)

  expect_equal(both, badness_only * bench_only)
})

#Edge cases and validation
test_that("compute_short_leg_scores rejects malformed scores", {

  bench_weights <- c(A = 0.02, B = 0.02)

  expect_error(compute_short_leg_scores(c(A = "x", B = "y"), bench_weights),
               "exp_ret_score_raw must be numeric")
  expect_error(compute_short_leg_scores(c(A = 0.5, B = NA_real_), bench_weights),
               "must not contain NAs")
  #signal_transform() guarantees positivity, so a non-positive value is a broken input
  expect_error(compute_short_leg_scores(c(A = 0.5, B = 0), bench_weights),
               "strictly positive and finite")
  expect_error(compute_short_leg_scores(c(A = 0.5, B = -1), bench_weights),
               "strictly positive and finite")
  expect_error(compute_short_leg_scores(c(A = 0.5, B = Inf), bench_weights),
               "strictly positive and finite")
  expect_error(compute_short_leg_scores(numeric(0), numeric(0)),
               "at least one element")
})

test_that("compute_short_leg_scores rejects malformed benchmark weights", {

  exp_ret_score_raw <- c(A = 0.5, B = 1.5)

  expect_error(compute_short_leg_scores(exp_ret_score_raw, c(A = 0.02)),
               "same length as exp_ret_score_raw")
  expect_error(compute_short_leg_scores(exp_ret_score_raw, c(A = 0.02, B = NA_real_)),
               "must not contain NAs")
  #Every short-block asset is a benchmark constituent by construction
  expect_error(compute_short_leg_scores(exp_ret_score_raw, c(A = 0.02, B = 0)),
               "strictly positive, finite and not greater than 1")
  expect_error(compute_short_leg_scores(exp_ret_score_raw, c(A = 0.02, B = 1.4)),
               "strictly positive, finite and not greater than 1")
})

test_that("compute_short_leg_scores rejects malformed exponents", {

  exp_ret_score_raw <- c(A = 0.5, B = 1.5)
  bench_weights     <- c(A = 0.02, B = 0.02)

  expect_error(compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                        badness_tilt_eta = c(1, 2)),
               "badness_tilt_eta must be a single finite numeric")
  expect_error(compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                        badness_tilt_eta = NA_real_),
               "badness_tilt_eta must be a single finite numeric")
  expect_error(compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                        bench_weight_tilt_eta = Inf),
               "bench_weight_tilt_eta must be a single finite numeric")
  expect_error(compute_short_leg_scores(exp_ret_score_raw, bench_weights,
                                        bench_weight_tilt_eta = "1"),
               "bench_weight_tilt_eta must be a single finite numeric")
})

test_that("compute_short_leg_scores tolerates unnamed input", {

  scores <- compute_short_leg_scores(exp_ret_score_raw = c(0.5, 1.5),
                                     bench_weights = c(0.02, 0.03))

  expect_null(names(scores))
  expect_equal(scores, c(2, 1 / 1.5))
})
