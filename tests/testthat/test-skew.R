test_that("skew is running correctly", {
  vec <- c(1,2,1,1,2)
  expect_equal(skew(vec),
               sqrt(5*4)/(3)*(mean((vec - mean(vec))^3)/(sd(vec)^3))
  )
})

test_that("skew returns 0 for symmetric data", {
  # For an equally spaced symmetric sequence, skewness should be 0.
  expect_equal(skew(c(1,2,3,4,5)), 0, tolerance = 1e-8)
})

test_that("skew returns NA when standard deviation is zero", {
  # When all values are identical, the standard deviation is zero.
  expect_true(is.na(skew(c(2,2,2,2))))
})

test_that("skew handles only 0s", {
  expect_true(is.na(skew(c(0,0,0,0,0))))
})

test_that("skew handles NA with na.rm = TRUE", {
  expect_equal(skew(c(1,2,3,NA,5), na.rm = TRUE), skew(c(1,2,3,5)), tolerance = 1e-8)
})

test_that("skew handles olny NAs with na.rm = TRUE", {
  expect_equal(skew(c(NA,NA,NA,NA,NA), na.rm = TRUE), NA_real_)
})

test_that("skew handles olny NAs with na.rm = FALSE", {
  expect_equal(skew(c(NA,NA,NA,NA,NA), na.rm = FALSE), NA_real_)
})

test_that("skew returns NA when NA present and na.rm = FALSE", {
  expect_true(is.na(skew(c(1,2,3,NA,5), na.rm = FALSE)))
})

test_that("skew works when only little number of values is passed", {
  expect_equal(skew(1), NA_real_)
  expect_equal(skew(c(1,2)), NA_real_)
  expect_equal(skew(c(1,2,3)), 0)
})


test_that("skew is running correctly for Inf", {
  expect_equal(skew(c(1,2,Inf,1,2)), NA_real_, tolerance = 1e-8)
})
test_that("skew is running correctly for only Infs", {
  expect_equal(skew(c(-Inf,Inf,Inf,Inf,-Inf)), NA_real_)
})




