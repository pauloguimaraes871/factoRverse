# Define your test
test_that("sum_matrix_onena is running correctly.", {
  expect_equal(
    sum_matrix_onena(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2)),
    matrix(c(1+5, 2+6, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_onena is running correctly with Data Frames.", {
  expect_equal(
    sum_matrix_onena(
      data.frame(matrix(c(1,2,3,4), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2))),
    matrix(c(1+5, 2+6, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_onena is running correctly with NA in matrix 1.", {
  expect_equal(
    sum_matrix_onena(
      matrix(c(NA,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2)),
    matrix(c(5, 2+6, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_onena is running correctly with NA in matrix 2.", {
  expect_equal(
    sum_matrix_onena(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,NA,7,8), nrow=2, ncol=2)),
    matrix(c(1+5, 2, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_onena is running correctly with NA in both matrices.", {
  expect_equal(
    sum_matrix_onena(
      matrix(c(1,NA,3,4), nrow=2, ncol=2),
      matrix(c(5,NA,7,8), nrow=2, ncol=2)),
    matrix(c(1+5, NA, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_onena throws error when dims differ", {
  expect_error(
    sum_matrix_onena(
      matrix(c(1,NA,3,4), nrow=2, ncol=2),
      matrix(c(5,NA), nrow=1, ncol=2)), "Input matrices must have the same dimensions."
  )
})

# Define your test
test_that("sum_matrix_onena is running correctly with all NAs", {
  expect_equal(
    sum_matrix_onena(
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2)),
    matrix(c(NA, NA, NA, NA), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_onena is running correctly with all NAs in matrix 1", {
  expect_equal(
    sum_matrix_onena(
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      matrix(c(1,2,3,4), nrow=2, ncol=2)),
    matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_onena is running correctly with both positive and negative values", {
  expect_equal(
    sum_matrix_onena(
      matrix(c(-1,2,-3,4), nrow=2, ncol=2),
      matrix(c(1,-2,3,-4), nrow=2, ncol=2)),
    matrix(c(0, 0, 0, 0), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_onena is running correctly with extreme values", {
expect_equal(
  sum_matrix_onena(
    matrix(c(Inf, -Inf, NaN, 1), nrow=2, ncol=2),
    matrix(c(-Inf, NaN, 1, Inf), nrow=2, ncol=2)),
  matrix(c(NaN, -Inf, 1, Inf), nrow=2, ncol=2)
)
})

