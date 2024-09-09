test_that("signal_transform handles simple numeric vectors correctly", {
  vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  upper_quantile <- 0.9
  lower_quantile <- 0.1

  result <- signal_transform(vector, upper_quantile, lower_quantile)
  expected_result <- c(0.4328, 0.4397, 0.5235, 0.6468, 0.846, 1.182, 1.546, 1.91,
                       2.2741, 2.3105)
  expect_equal(result, expected_result, tolerance = 1e-3)
})

test_that("signal_transform handles vectors with NA values", {
  vector <- c(1, 2, 3, NA, 5, NA, 7, 8, 9, 10)
  upper_quantile <- 0.9
  lower_quantile <- 0.1
  result_without_NAs <- signal_transform(c(1, 2, 3, 5, 7, 8, 9, 10), upper_quantile, lower_quantile)


  expect_equal(signal_transform(c(1, 2, 3, NA, 5, NA, 7, 8, 9, 10), upper_quantile, lower_quantile),
               c(result_without_NAs[1], result_without_NAs[2], result_without_NAs[3], NA, result_without_NAs[4], NA, result_without_NAs[5], result_without_NAs[6], result_without_NAs[7], result_without_NAs[8]))


  vector <- c(NA,NA,NA)
  upper_quantile <- 0.9
  lower_quantile <- 0.1

  expect_equal(signal_transform(vector, upper_quantile, lower_quantile),
               c(NA,NA,NA))


  })

test_that("signal_transform handles edge cases", {
  vector <- c(1, 1, 1, 1, 1) # All values are the same
  upper_quantile <- 0.9
  lower_quantile <- 0.1

  result <- signal_transform(vector, upper_quantile, lower_quantile)
  expected_result <- vector # All values are the same
  expect_equal(result, expected_result)
})

test_that("signal_transform handles single-element vectors", {
  vector <- c(5)
  upper_quantile <- 0.9
  lower_quantile <- 0.1

  # Single value case
  expect_equal(signal_transform(vector, upper_quantile, lower_quantile), 1)
})
