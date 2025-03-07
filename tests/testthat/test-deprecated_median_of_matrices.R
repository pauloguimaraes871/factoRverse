testthat::skip("Deprecated function .median_of_matrices; tests skipped.")
# Define your test
test_that("median_of_matrices is running correctly.", {
  expect_equal(
    median_of_matrices(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2)),
    matrix(c(stats::median(c(1,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE), stats::median(c(3,7,1), na.rm = TRUE),
             stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})

# Define your test
test_that("median_of_matrices is running correctly with Data Frames.", {
  expect_equal(
    median_of_matrices(
      as.data.frame(matrix(c(1,2,3,4), nrow=2, ncol=2)),
      as.data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2)),
      as.data.frame(matrix(c(1,1,1,1), nrow=2, ncol=2))),
    matrix(c(stats::median(c(1,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE),
             stats::median(c(3,7,1), na.rm = TRUE), stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})

# Define your test
test_that("median_of_matrices is running correctly with tibbles.", {
  expect_equal(
    median_of_matrices(
      tibble::as_tibble(matrix(c(1,2,3,4), nrow=2, ncol=2), .name_repair = "unique"),
      tibble::as_tibble(matrix(c(5,6,7,8), nrow=2, ncol=2), .name_repair = "unique"),
      tibble::as_tibble(matrix(c(1,1,1,1), nrow=2, ncol=2), .name_repair = "unique")),
    matrix(c(stats::median(c(1,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE),
             stats::median(c(3,7,1), na.rm = TRUE), stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})

# Define your test
test_that("median_of_matrices works with multiple types and number of inputs.", {
  result <- median_of_matrices(
    tibble::as_tibble(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2), .name_repair = "unique"),
    tibble::as_tibble(matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2), .name_repair = "unique"),
    as.data.frame(matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2)),
    as.data.frame(matrix(c(9, 10, 11, 12), nrow = 2, ncol = 2))
  )

  expected_result <- matrix(c(
    stats::median(c(1, 5, 1, 9), na.rm = TRUE),
    stats::median(c(2, 6, 1, 10), na.rm = TRUE),
    stats::median(c(3, 7, 1, 11), na.rm = TRUE),
    stats::median(c(4, 8, 1, 12), na.rm = TRUE)
  ), nrow = 2, ncol = 2)

  expect_equal(result, expected_result)
})

# Define your test
test_that("median_of_matrices ignores NAs when there are Random NAs", {
  expect_equal(
    median_of_matrices(
      matrix(c(NA,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,NA,1), nrow=2, ncol=2)),
    matrix(c(stats::median(c(NA,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE),
             stats::median(c(3,7,NA), na.rm = TRUE), stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})


# Define your test
test_that("median_of_matrices is running correctly with Random Characters", {
  expect_equal(
    median_of_matrices(
      matrix(c("A",2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,"B",1), nrow=2, ncol=2)),
    matrix(c(stats::median(c("A",5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE),
             stats::median(c(3,7,"B"), na.rm = TRUE), stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})


# Define your test
test_that("median_of_matrices returns matrix os NAs  when every element is NA", {
  expect_equal(
    median_of_matrices(
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2)),
    matrix(c(stats::median(c(NA,NA,NA), na.rm = TRUE), stats::median(c(NA,NA,NA), na.rm = TRUE),
             stats::median(c(NA,NA,NA), na.rm = TRUE), stats::median(c(NA,NA,NA), na.rm = TRUE)), nrow=2, ncol=2)
  )
})


# Define your test
test_that("median_of_matrices throws an error when one of elements is NULL", {
  expect_error(
    median_of_matrices(
      NULL,
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2))
  )
})


# Define your test
test_that("median_of_matrices throws an error when inputs have different dimensions", {
  expect_error(
    median_of_matrices(
      matrix(c(2,3,4), nrow=3, ncol=1),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2)),
    "All input matrices must have the same dimensions."
  )
})

test_that("median_of_matrices rejects unsupported input types", {
  # Define a supported input: matrix, data.frame, tibble
  valid_matrix <- matrix(c(1, 2, 3, 4), nrow = 2)
  valid_dataframe <- data.frame(a = c(1, 2), b = c(3, 4))
  valid_tibble <- tibble::tibble(a = c(5, 6), b = c(7, 8))

  # Expect the function to run without errors for valid inputs
  expect_silent(median_of_matrices(valid_matrix, valid_dataframe, valid_tibble))

  # Define unsupported inputs
  invalid_list <- list(1, 2, 3)
  invalid_character <- "Not a matrix or data frame"

  # Expect the function to throw an error for invalid inputs
  expect_error(median_of_matrices(valid_matrix, invalid_list),
               "All inputs must be matrices, data.frames, or tibbles.")

  expect_error(median_of_matrices(valid_matrix, invalid_character),
               "All inputs must be matrices, data.frames, or tibbles.")

  expect_error(median_of_matrices(invalid_list, invalid_character),
               "All inputs must be matrices, data.frames, or tibbles.")
})

test_that("median_of_matrices is running correctly in the presence of Infs.", {

  expect_equal(
    median_of_matrices(
      matrix(c(Inf,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2)),
    matrix(c(stats::median(c(Inf,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE), stats::median(c(3,7,1), na.rm = TRUE),
             stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )


})

test_that("median_of_matrices is running correctly in the presence of equal matrices.", {
  expect_equal(
    median_of_matrices(
      matrix(c(1,1,1,1), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2)),
    matrix(c(stats::median(c(1,1,1), na.rm = TRUE), stats::median(c(1,1,1), na.rm = TRUE),
             stats::median(c(1,1,1), na.rm = TRUE), stats::median(c(1,1,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})
