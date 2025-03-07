test_that("mean_std is running correctly", {
  vec <- c(1,2,1,1,2)
  expect_equal(mean_std(vec),
               mean(vec)/sd(vec)
  )
})

test_that("mean_std returns NA when standard deviation is zero", {
  # When all values are identical, the standard deviation is zero.
  expect_true(is.na(mean_std(c(2,2,2,2))))
})

test_that("mean_std handles only 0s", {
  expect_true(is.na(mean_std(c(0,0,0,0,0))))
})

test_that("mean_std handles NA with na.rm = TRUE", {
  expect_equal(mean_std(c(1,2,3,NA,5), na.rm = TRUE), mean_std(c(1,2,3,5)), tolerance = 1e-8)
})

test_that("mean_std handles olny NAs with na.rm = TRUE", {
  expect_equal(mean_std(c(NA,NA,NA,NA,NA), na.rm = TRUE), NA_real_)
})

test_that("mean_std handles olny NAs with na.rm = FALSE", {
  expect_equal(mean_std(c(NA,NA,NA,NA,NA), na.rm = FALSE), NA_real_)
})

test_that("mean_std returns NA when NA present and na.rm = FALSE", {
  expect_true(is.na(mean_std(c(1,2,3,NA,5), na.rm = FALSE)))
})

test_that("mean_std works when only little number of values is passed", {
  expect_equal(mean_std(1), NA_real_)
  expect_equal(mean_std(c(1,2)), mean(c(1,2))/sd(c(1,2)))
})

test_that("mean_std is running correctly for Inf", {
  expect_equal(mean_std(c(1,2,Inf,1,2)), NA_real_, tolerance = 1e-8)
})
test_that("mean_std is running correctly for only Infs", {
  expect_equal(mean_std(c(-Inf,Inf,Inf,Inf,-Inf)), NA_real_)
})

