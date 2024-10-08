# Define your test
test_that("sd_rolling is running correctly with small matrices", {
  expect_equal(
    sd_rolling(
      matrix(c(5,3,7,8), nrow = 2, ncol = 2),
      matrix(c(1,2,6,4), nrow = 2, ncol = 2)
      ),
    matrix(c(stats::sd(c(5,6,1)),
             stats::sd(c(3,2,4)),
             stats::sd(c(7,5,6)),
             stats::sd(c(4,3,8))), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sd_rolling is running correctly with a data frame", {
  expect_equal(
    sd_rolling(
      data.frame(matrix(c(5,3,7,8), nrow = 2, ncol = 2)),
      data.frame(matrix(c(1,2,6,4), nrow = 2, ncol = 2))),
    matrix(c(stats::sd(c(5,6,1)),
             stats::sd(c(3,2,4)),
             stats::sd(c(7,5,6)),
             stats::sd(c(4,3,8))), nrow=2, ncol=2)
  )
})

test_that("sd_rolling works with data frames and tibbles", {
  # Sample data for valid tests
  main_matrix_valid <- matrix(c(5, 3, 7, 8), nrow = 2)
  complementary_matrix_valid <- matrix(c(1, 2, 6, 4), nrow = 2)

  df_main <- data.frame(matrix(c(5, 3, 7, 8), nrow = 2))
  df_complementary <- data.frame(matrix(c(1, 2, 6, 4), nrow = 2))

  tibble_main <- tibble::as_tibble(matrix(c(5, 3, 7, 8), nrow = 2), .name_repair = "unique")
  tibble_complementary <- tibble::as_tibble(matrix(c(1, 2, 6, 4), nrow = 2), .name_repair = "unique")

  # Check that data frames work correctly
  expect_equal(sd_rolling(df_main, df_complementary), sd_rolling(main_matrix_valid, complementary_matrix_valid))

  # Check that tibbles work correctly
  expect_equal(sd_rolling(tibble_main, tibble_complementary), sd_rolling(main_matrix_valid, complementary_matrix_valid))
})


# Define your test
test_that("sd_rolling is running correctly when complementary matrix has only one column", {
  expect_equal(
    sd_rolling(
      matrix(c(5,3,7,8), nrow = 2, ncol = 2),
      matrix(c(1,2), nrow = 2, ncol = 1)),
    matrix(c(stats::sd(c(5,1)),
             stats::sd(c(3,2)),
             stats::sd(c(7,5)),
             stats::sd(c(8,3))
           ), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sd_rolling is running correctly when there are repeated values in data frame", {
  expect_equal(
    sd_rolling(
      data.frame(matrix(c(10,10,15,20,10,20), nrow =2, ncol=3)),
      data.frame(matrix(c(10,2,5,10,25,5), nrow=2, ncol=3))),

    matrix(c(stats::sd(c(10,25,5)),
             stats::sd(c(10,5,2)),
             stats::sd(c(15,10,25,5)),
             stats::sd(c(20,5,10)),
             stats::sd(c(15,10,25)),
             stats::sd(c(20,10,5))

    ), nrow=2, ncol=3)

  )
})



# Define your test
test_that("sd_rolling is running correctly when there are NAs", {
  expect_equal(
    sd_rolling(
      matrix(c(10,10,NA,NA,10,20), nrow=2, ncol=3),
      matrix(c(10,NA,5,10,25,5), nrow=2, ncol=3)),

    matrix(c(stats::sd(c(10,25,5), na.rm = TRUE),
             stats::sd(c(10,5,NA), na.rm = TRUE),
             stats::sd(c(NA,10,25,5), na.rm = TRUE),
             stats::sd(c(NA,5,10,NA), na.rm = TRUE),
             stats::sd(c(10,NA,25), na.rm = TRUE),
             stats::sd(c(20,NA,10,5), na.rm = TRUE)),
           nrow = 2, ncol = 3
  )
  )
})

# Define your test
test_that("sd_rolling throws an error when number of rows differ", {
  expect_error(
    sd_rolling(
      matrix(c(10,10,1,-4,10,20), nrow=2, ncol=3),
      matrix(c(10,3,5,10), nrow=1, ncol=4)), "Main matrix and complementary_matrix should have the same number of rows."
  )
})

# Define the tests
test_that("sd_rolling rejects unsupported input types", {

  # Sample data for valid tests
  main_matrix_valid <- matrix(c(5, 3, 7, 8), nrow = 2)
  complementary_matrix_valid <- matrix(c(1, 2, 6, 4), nrow = 2)

  # Test with valid matrices
  expect_silent(sd_rolling(main_matrix_valid, complementary_matrix_valid))

  # Define unsupported inputs
  invalid_list <- list(1, 2, 3)
  invalid_character <- "Not a matrix or data frame"

  # Expect the function to throw an error for invalid inputs
  expect_error(sd_rolling(main_matrix_valid, invalid_list),
               "Both main_matrix and complementary_matrix must be matrices, data.frames, or tibbles.")

  expect_error(sd_rolling(main_matrix_valid, invalid_character),
               "Both main_matrix and complementary_matrix must be matrices, data.frames, or tibbles.")

  expect_error(sd_rolling(invalid_list, invalid_character),
               "Both main_matrix and complementary_matrix must be matrices, data.frames, or tibbles.")
})



# Define your test
test_that("sd_rolling throws an error when one matrix is missing", {
  expect_error(
    sd_rolling(
      matrix(c(10,10,1,-4,10,20), nrow=2, ncol=3)
    ))
})


# Define your test
test_that("sd_rolling is running correctly when there are Infs", {
  expect_equal(
    sd_rolling(
      matrix(c(5,3,Inf,8), nrow = 2, ncol = 2),
      matrix(c(1,Inf), nrow = 2, ncol = 1)),
    matrix(c(stats::sd(c(5,1)),
             stats::sd(c(3,Inf)),
             stats::sd(c(5,Inf)),
             stats::sd(c(8,3))),
           nrow = 2, ncol =2))

})
