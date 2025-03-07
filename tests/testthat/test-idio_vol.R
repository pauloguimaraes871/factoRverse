test_that("idio_vol returns 0 for perfect fit", {
  # When returns exactly match benchmark returns, the idiosyncratic volatility is zero.
  expect_equal(idio_vol(c(1,2,3), c(1,2,3)), 0, tolerance = 1e-5)
  expect_equal(idio_vol(c(1,2,3,4,5), c(1,2,3,4,5)), 0, tolerance = 1e-5)
  expect_equal(idio_vol(c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)), 0, tolerance = 1e-5)
})

test_that("idio_vol returns 0 when bench is perfectly collinear", {
  expect_equal(idio_vol(c(1,2,3), c(2,4,6)), 0)
  expect_equal(idio_vol(c(2,4,5,8), c(4,8,10,16)), 0)
})

test_that("idio_vol computes correct value for non-perfect fit", {
  # For ret_values = c(1,2,4) and bench_ret_values = c(1,2,3),
  # our manual calculation gives idio_vol ~ 0.288675.
  expect_equal(idio_vol(c(1,2,4), c(1,2,3)), 0.288675, tolerance = 1e-6)
})

test_that("idio_vol handles NA with na.rm = TRUE", {
  expect_equal(idio_vol(c(NA,2,3,NA,5), c(3,4,8,6,7), na.rm = TRUE),
               idio_vol(c(2,3,5), c(4,8,7)), tolerance = 1e-8)

})

test_that("idio_vol handles olny NAs with na.rm = TRUE", {
  expect_equal(idio_vol(c(NA,NA,NA,NA,NA), c(1,2,3,4,5), na.rm = TRUE), NA_real_)
})

test_that("idio_vol handles olny NAs with na.rm = FALSE", {
  expect_equal(idio_vol(c(NA,NA,NA,NA,NA), c(1,2,3,4,5), na.rm = TRUE), NA_real_)
})

test_that("idio_vol works when only one or two single values is passed", {
  expect_equal(idio_vol(1, 2), NA_real_)
  expect_equal(idio_vol(c(-1,15), c(2,-3)), NA_real_)
  expect_false(is.na(idio_vol(c(-1,15,3), c(2,-3,5))))
})

test_that("idio_vol works with repeated values", {
  expect_equal(idio_vol(c(0,0,0), c(2,-3,5)),0)
  expect_equal(idio_vol(c(1,1,1), c(2,-3,5)), 0)
  expect_equal(idio_vol(c(2,2,2,2), c(2,-3,5,6)), 0)
})


test_that("idio_vol throws errors when Inf or NA in bench", {

  expect_error(idio_vol(c(0,0,0), c(2,NA,5)))
  expect_error(idio_vol(c(0,0,0), c(2,Inf,5)))
})

test_that("idio_vol throws errors when length does not match", {
  expect_error(idio_vol(c(0,2), c(2,5,1)))
})


test_that("idio_vol handles Inf", {
  expect_true(is.na(idio_vol(c(Inf,2,3,Inf,5), c(1,2,3,4,5))))
  expect_true(is.na(idio_vol(c(Inf,-Inf,-Inf,Inf,Inf), c(1,2,3,4,5))))
})

