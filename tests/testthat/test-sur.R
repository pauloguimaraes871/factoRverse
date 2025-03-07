test_that("sur computes correct value for simple data", {
  # For c(1,2,3,4,5): mean = 3, sd ~ 1.581139, final value = 5.
  expected_sur <- (5 - 3) / 1.581139
  expect_equal(sur(c(1,2,3,4,5)), expected_sur, tolerance = 1e-6)
})

test_that("sur results in 0 surprise when most recent value equals mean", {
  vec <- c(-2,-4,0,2,4,0)
  expect_equal(sur(vec), 0)
})

test_that("sur returns NA when standard deviation is zero", {
  expect_true(is.na(sur(c(2,2,2,2))))
})

test_that("sur returns NA when only 0s are provided", {
  expect_true(is.na(sur(c(0,0,0,0))))
})

test_that("sur handles NA with na.rm = TRUE", {
  expect_equal(sur(c(1,2,NA,4,5), na.rm = TRUE), sur(c(1,2,4,5)), tolerance = 1e-8)
})

test_that("sur handles NA with na.rm = FALSE", {
  expect_equal(sur(c(1,2,NA,4,5), na.rm = FALSE), NA_real_)
})

test_that("sur handles only NAs", {
  expect_true(is.na(sur(c(NA,NA,NA,NA,NA))))
})

test_that("sur returns NA when final value is NA", {
  expect_true(is.na(sur(c(1,2,3,NA,NA))))
})

test_that("sur works when small number of values is passed", {
  expect_equal(sur(1), NA_real_)
  expect_equal(sur(c(1,2)), (2 - mean(c(1,2)))/sd(c(1,2)))
})

test_that("sur handles Infs", {
  expect_equal(sur(c(-2,-Inf,Inf,1,2)), NA_real_)
})

test_that("sur handles only Inf", {
  expect_equal(sur(c(-Inf,-Inf,Inf,Inf,Inf)), NA_real_)
})






