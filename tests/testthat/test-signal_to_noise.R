test_that("signal_to_noise is running correctly", {
  vec <- c(1,2,1,1,2)
  expect_equal(signal_to_noise(vec),
               mean(vec)/sd(vec)
  )
})

test_that("signal_to_noise returns NA when standard deviation is zero", {
  # When all values are identical, the standard deviation is zero.
  expect_true(is.na(signal_to_noise(c(2,2,2,2))))
})

test_that("signal_to_noise handles only 0s", {
  expect_true(is.na(signal_to_noise(c(0,0,0,0,0))))
})

test_that("signal_to_noise handles NA with na.rm = TRUE", {
  expect_equal(signal_to_noise(c(1,2,3,NA,5), na.rm = TRUE), signal_to_noise(c(1,2,3,5)), tolerance = 1e-8)
})

test_that("signal_to_noise handles olny NAs with na.rm = TRUE", {
  expect_equal(signal_to_noise(c(NA_real_,NA_real_,NA_real_,NA_real_,NA_real_), na.rm = TRUE), NA_real_)
})

test_that("signal_to_noise handles olny NAs with na.rm = FALSE", {
  expect_equal(signal_to_noise(c(NA_real_,NA_real_,NA_real_,NA_real_,NA_real_), na.rm = FALSE), NA_real_)
})

test_that("signal_to_noise returns NA when NA present and na.rm = FALSE", {
  expect_true(is.na(signal_to_noise(c(1,2,3,NA,5), na.rm = FALSE)))
})

test_that("signal_to_noise works when only little number of values is passed", {
  expect_equal(signal_to_noise(1), NA_real_)
  expect_equal(signal_to_noise(c(1,2)), mean(c(1,2))/sd(c(1,2)))
})

test_that("signal_to_noise is running correctly for Inf", {
  expect_equal(signal_to_noise(c(1,2,Inf,1,2)), NA_real_, tolerance = 1e-8)
})
test_that("signal_to_noise is running correctly for only Infs", {
  expect_equal(signal_to_noise(c(-Inf,Inf,Inf,Inf,-Inf)), NA_real_)
})

