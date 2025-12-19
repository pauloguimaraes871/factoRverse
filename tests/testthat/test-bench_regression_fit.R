test_that("bench_regression_fit returns expected structure", {
  out <- bench_regression_fit(c(1, 2, 3), c(2, 4, 6))

  expect_type(out, "list")
  expect_named(out, c("alpha", "beta", "residuals", "residual_sd", "n_obs"))
})


test_that("bench_regression_fit handles perfect linear relationship", {
  ret   <- c(1, 2, 3, 4)
  bench <- c(2, 4, 6, 8)

  out <- bench_regression_fit(ret, bench)

  expect_equal(out$alpha, 0, tolerance = 1e-12)
  expect_equal(out$beta, 0.5, tolerance = 1e-12)
  expect_equal(out$residual_sd, 0)
  expect_equal(out$n_obs, 4)
})


test_that("bench_regression_fit works without intercept", {
  ret   <- c(1, 2, 3)
  bench <- c(2, 4, 6)

  out <- bench_regression_fit(ret, bench, include_intercept = FALSE)

  expect_equal(out$alpha, 0)
  expect_equal(out$beta, 0.5, tolerance = 1e-12)
})

test_that("bench_regression_fit returns NA when ret_values are all NA", {
  out <- bench_regression_fit(c(NA, NA, NA), c(1, 2, 3))
  expect_true(is.na(out))
})

test_that("bench_regression_fit returns NA when ret_values contain Inf", {
  out <- bench_regression_fit(c(1, Inf, 3), c(2, 4, 6))
  expect_true(is.na(out))
})

test_that("bench_regression_fit errors on NA or Inf in benchmark", {
  expect_error(bench_regression_fit(c(1,2,3), c(1, NA, 3)))
  expect_error(bench_regression_fit(c(1,2,3), c(1, Inf, 3)))
})

test_that("bench_regression_fit errors on length mismatch", {
  expect_error(bench_regression_fit(c(1,2), c(1,2,3)))
})

test_that("bench_regression_fit handles small samples safely", {
  out1 <- bench_regression_fit(1, 2)
  out2 <- bench_regression_fit(c(1,2), c(2,4))

  expect_equal(out1$n_obs, 1)
  expect_true(is.na(out1$beta))

  expect_equal(out2$n_obs, 2)
  expect_false(is.na(out2$beta))
})

test_that("bench_regression_fit applies mult_last_n correctly", {
  ret   <- c(1, 2, 3)
  bench <- c(1, 2, 3)

  out1 <- bench_regression_fit(ret, bench)
  out2 <- bench_regression_fit(ret, bench, mult_last_n = 1, mult_by = -1)

  expect_false(isTRUE(all.equal(out1$beta, out2$beta)))

  out3 <- bench_regression_fit(c(1,2,-3), c(1,2,3))

  expect_equal(out2, out3)

})


test_that("alpha_bench matches lm intercept", {
  ret   <- c(1, 2, 4)
  bench <- c(1, 2, 3)

  lm_fit <- lm(ret ~ bench)

  expect_equal(
    alpha_bench(ret, bench),
    unname(coef(lm_fit)[1]),
    tolerance = 1e-8
  )
})

test_that("alpha_bench returns zero for perfect fit", {
  expect_equal(alpha_bench(c(1,2,3), c(1,2,3)), 0)
})

test_that("alpha_bench handles NA with na.rm = TRUE", {
  expect_equal(
    alpha_bench(c(NA,2,3), c(1,2,3), na.rm = TRUE),
    alpha_bench(c(2,3), c(2,3)),
    tolerance = 1e-8
  )
})

test_that("alpha_bench errors on invalid benchmark", {
  expect_error(alpha_bench(c(1,2,3), c(1, NA, 3)))
  expect_error(alpha_bench(c(1,2), c(1,2,3)))
})


test_that("beta_bench matches lm slope", {
  ret   <- c(1, 2, 4)
  bench <- c(1, 2, 3)

  lm_fit <- lm(ret ~ bench)

  expect_equal(
    beta_bench(ret, bench),
    unname(coef(lm_fit)[2]),
    tolerance = 1e-8
  )
})


test_that("beta_bench returns correct slope for collinear data", {
  expect_equal(beta_bench(c(1,2,3), c(2,4,6)), 0.5)
})

test_that("beta_bench handles NA with na.rm = TRUE", {
  expect_equal(
    beta_bench(c(NA,2,3), c(1,2,3), na.rm = TRUE),
    beta_bench(c(2,3), c(2,3)),
    tolerance = 1e-8
  )
})

test_that("beta_bench returns NA for insufficient data", {
  expect_true(is.na(beta_bench(1, 2)))
})
