test_that("res_mom returns NA when returns equal bench returns", {
  # When ret_values and bench_ret_values are equal, return zero.
  expect_equal(res_mom(c(1,2,3), c(1,2,3)), NA_real_)
  expect_equal(res_mom(c(1,2,3,4,5), c(1,2,3,4,5)), NA_real_)
})

test_that("res_mom returns NA when returns are perfectly collinear", {
  # When ret_values and bench_ret_values are perfectly collinear, residuals are zero.
  expect_equal(res_mom(c(1,2,3,4,5), c(2,4,6,8,10)), NA_real_)
  expect_equal(res_mom(c(1,2,3,4), c(2,4,6,8)), NA_real_)
  expect_equal(res_mom(c(1,2,3,4,5,6,7), c(2,4,6,8,10,12,14)), NA_real_)
})

test_that("res_mom computes residual momentum correctly", {
  ret <- c(1,2,-3,4,-5)
  bench <- c(-1,7,9,0,50)

  reg <- lm(ret ~ bench - 1)

  expect_equal(res_mom(ret, bench),
               sum(residuals(reg))/sd(residuals(reg)))
})

test_that("res_mom handles NA with na.rm = TRUE", {
  expect_equal(res_mom(c(NA,2,3,NA,5), c(1,2,3,4,5), na.rm = TRUE),
               res_mom(c(2,3,5), c(2,3,5)), tolerance = 1e-8)
})

test_that("res_mom handles olny NAs with na.rm = TRUE", {
  expect_equal(res_mom(c(NA,NA,NA,NA,NA), c(1,2,3,4,5), na.rm = TRUE), NA_real_)
})

test_that("res_mom handles olny NAs with na.rm = FALSE", {
  expect_equal(res_mom(c(NA,NA,NA,NA,NA), c(1,2,3,4,5), na.rm = TRUE), NA_real_)
})

test_that("res_mom works when only one value is passed", {
  expect_equal(res_mom(1, 2), NA_real_)
  expect_false(is.na(res_mom(c(-1,15), c(2,-3))))
  expect_false(is.na(res_mom(c(-1,15,3), c(2,-3,5))))
})

test_that("res_mom works with repeated values", {
  expect_true(is.na(res_mom(c(0,0,0), c(2,-3,5))))
  expect_no_error(is.na(res_mom(c(1,1,1), c(2,-3,5))))
  expect_no_error(is.na(res_mom(c(2,2,2,2), c(2,-3,5,9))))
})

test_that("res_mom throws errors when Inf or NA in bench", {

  expect_error(res_mom(c(0,0,0), c(2,NA,5)))
  expect_error(res_mom(c(0,0,0), c(2,Inf,5)))
})

test_that("res_mom throws errors when length does not match", {
  expect_error(res_mom(c(0,2), c(2,5,1)))
})


test_that("res_mom handles Inf", {
  expect_true(is.na(res_mom(c(Inf,2,3,Inf,5), c(1,2,3,4,5))))
  expect_true(is.na(res_mom(c(Inf,-Inf,-Inf,Inf,Inf), c(1,2,3,4,5))))
})

