test_that("count_if works correctly", {

  # Basic test cases
  expect_equal(count_if(c(1, 0, 3, 0, 5), function(x) x == 0), 2)
  expect_equal(count_if(c(1, 2, 3, 4, 5), function(x) x > 2), 3)

  # Count negative numbers
  expect_equal(count_if(c(-1, -2, 0, 3, 4), function(x) x < 0), 2)

  # Count even numbers
  expect_equal(count_if(c(1, 2, 3, 4, 5, 6), function(x) x %% 2 == 0), 3)

  # Handling NA values
  expect_equal(count_if(c(1, NA, 3, 0, 5), function(x) x == 0, na.rm = TRUE), 1)
  expect_equal(count_if(c(1, NA, 3, 0, 5), function(x) x == 0, na.rm = FALSE), NA_integer_)

  # Count with NA values present
  expect_equal(count_if(c(1, 2, NA, 4, 5, NA), function(x) x > 2, na.rm = TRUE), 2)
  expect_equal(count_if(c(1, 2, NA, 4, 5, NA), function(x) x > 2, na.rm = FALSE), NA_integer_)

  # Error handling
  expect_error(count_if(c("a", "b", "c"), function(x) x == "a"), "values must be numeric.")
  expect_error(count_if(c(1, 2, 3), "not_a_function"), "count_condition_fun must be a function.")
  expect_error(count_if(c(1, 2, 3), function(x) x + 1), "count_condition_fun must return a logical vector.")  # Condition must return logical
})
