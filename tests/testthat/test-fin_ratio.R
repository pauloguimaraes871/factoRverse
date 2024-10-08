# Define your test
test_that("Fin Ratio is running correctly.", {
  expect_equal(
    fin_ratio(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2)),
    matrix(c(1/5, 2/6, 3/7, 4/8), nrow=2, ncol=2)
  )

  expect_equal(
    fin_ratio(
      matrix(c(1,0,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2)),
    matrix(c(1/5, 0, 3/7, 4/8), nrow=2, ncol=2)
  )

  expect_equal(
    fin_ratio(
      matrix(c(1,0,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,0), nrow=2, ncol=2)),
    matrix(c(1/5, 0, 3/7, Inf), nrow=2, ncol=2)
  )

  expect_equal(
    fin_ratio(
      matrix(c(-1,0,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,0), nrow=2, ncol=2)),
    matrix(c(-1/5, 0, 3/7, Inf), nrow=2, ncol=2)
  )

  expect_equal(
    fin_ratio(
      matrix(c(-1,0,3,-4), nrow=2, ncol=2),
      matrix(c(5,6,7,0), nrow=2, ncol=2)),
    matrix(c(-1/5, 0, 3/7, -Inf), nrow=2, ncol=2)
  )

})

test_that("fin_ratio works with data frames and tibbles", {
  # Sample data for valid tests
  income_statement_valid <- matrix(c(100, 200, -50, 300, NA, 150), nrow = 3)
  balance_sheet_valid <- matrix(c(1000, 1500, -200, -300, 500, 700), nrow = 3)

  df_income <- data.frame(matrix(c(100, 200, -50, 300, NA, 150), nrow = 3))
  df_balance <- data.frame(matrix(c(1000, 1500, -200, -300, 500, 700), nrow = 3))

  tibble_income <- tibble::as_tibble(matrix(c(100, 200, -50, 300, NA, 150), nrow = 3), .name_repair = "unique")
  tibble_balance <- tibble::as_tibble(matrix(c(1000, 1500, -200, -300, 500, 700), nrow = 3), .name_repair = "unique")

  # Check that data frames work correctly
  expect_equal(fin_ratio(df_income, df_balance), fin_ratio(income_statement_valid, balance_sheet_valid))

  # Check that tibbles work correctly
  expect_equal(fin_ratio(tibble_income, tibble_balance), fin_ratio(income_statement_valid, balance_sheet_valid))
})

# Define your test
test_that("Fin Ratio is running correctly - DFs.", {
  expect_equal(
    fin_ratio(
      data.frame(matrix(c(1,2,3,4), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2))),
    matrix(c(1/5, 2/6, 3/7, 4/8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Fin Ratio is running correctly with non-squared DFs.", {
  expect_equal(
    fin_ratio(
      data.frame(matrix(c(1,2,3,4,1,-5), nrow=2, ncol=3)),
      data.frame(matrix(c(5,6,7,8,9,1), nrow=2, ncol=3))),
    matrix(c(1/5, 2/6, 3/7, 4/8,1/9,-5/1), nrow=2, ncol=3)
  )
})

# Define your test
test_that("Fin Ratio is running correctly with different classes.", {
  expect_equal(
    fin_ratio(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2))),
    matrix(c(1/5, 2/6, 3/7, 4/8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Fin Ratio is running correctly with different dimensions", {
  expect_error(
    fin_ratio(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8,9,0), nrow=3, ncol=2))
,"Input matrices must have the same dimensions.")
})




# Define your test
test_that("Fin Ratio is running correctly - Random NAs.", {
  expect_equal(
    fin_ratio(
      data.frame(matrix(c(NA,2,3,4), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,NA,8), nrow=2, ncol=2))),
    matrix(c(NA/5, 2/6, 3/NA, 4/8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Fin Ratio is running correctly - All NAs.", {
  expect_equal(
    fin_ratio(
      data.frame(matrix(c(NA,NA,NA,NA), nrow=2, ncol=2)),
      data.frame(matrix(c(NA,NA,NA,NA), nrow=2, ncol=2))),
    matrix(c(NA, NA, NA, NA), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Fin Ratio is running correctly - Negative Values.", {
  expect_equal(
    fin_ratio(
      data.frame(matrix(c(1,-2,-3,4), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,-7,-8), nrow=2, ncol=2))),
    matrix(c(1/5, -2/6, NA, -4/8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Fin Ratio is running correctly with Infs", {
  expect_equal(
    fin_ratio(
      data.frame(matrix(c(1,-2,-Inf,4), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,-Inf,-8), nrow=2, ncol=2))),
    matrix(c(1/5, -2/6, NA, -4/8), nrow=2, ncol=2)
  )

  expect_equal(
    fin_ratio(
      data.frame(matrix(c(1,-2,-Inf,Inf), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,-Inf,-8), nrow=2, ncol=2))),
    matrix(c(1/5, -2/6, NA, -Inf), nrow=2, ncol=2)
  )


  expect_equal(
    fin_ratio(
      data.frame(matrix(c(Inf,-2,-Inf,Inf), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,-Inf,-8), nrow=2, ncol=2))),
    matrix(c(Inf, -2/6, NA, -Inf), nrow=2, ncol=2)
  )

})

# Define the tests
test_that("fin_ratio rejects unsupported input types", {

  # Sample data for valid tests
  income_statement_valid <- matrix(c(100, 200, -50, 300, NA, 150), nrow = 3)
  balance_sheet_valid <- matrix(c(1000, 1500, -200, -300, 500, 700), nrow = 3)

  # Test with valid matrices
  expect_silent(fin_ratio(income_statement_valid, balance_sheet_valid))

  # Define unsupported inputs
  invalid_list <- list(1, 2, 3)
  invalid_character <- "Not a matrix or data frame"

  # Expect the function to throw an error for invalid inputs
  expect_error(fin_ratio(income_statement_valid, invalid_list),
               "Both inc_statement_item and bs_item must be matrices, data.frames, or tibbles.")

  expect_error(fin_ratio(income_statement_valid, invalid_character),
               "Both inc_statement_item and bs_item must be matrices, data.frames, or tibbles.")

  expect_error(fin_ratio(invalid_list, invalid_character),
               "Both inc_statement_item and bs_item must be matrices, data.frames, or tibbles.")
})



