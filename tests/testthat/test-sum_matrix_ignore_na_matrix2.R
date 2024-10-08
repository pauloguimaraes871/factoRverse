# Define your test
test_that("sum_matrix_ignore_na_matrix2 is running correctly.", {
  expect_equal(
    sum_matrix_ignore_na_matrix2(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2)),
    matrix(c(1+5, 2+6, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_ignore_na_matrix2 is running correctly with Data Frame.", {
  expect_equal(
    sum_matrix_ignore_na_matrix2(
      data.frame(matrix(c(1,2,3,4), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2))),
    matrix(c(1+5, 2+6, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_ignore_na_matrix2 is running correctly with tibble.", {
  expect_equal(
    sum_matrix_ignore_na_matrix2(
      tibble::as_tibble(matrix(c(1,2,3,4), nrow=2, ncol=2), .name_repair = "unique"),
      data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2))),
    matrix(c(1+5, 2+6, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_ignore_na_matrix2 is running correctly when only one NA Matrix 1", {
  expect_equal(
    sum_matrix_ignore_na_matrix2(
      matrix(c(NA,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2)),
    matrix(c(NA, 2+6, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_ignore_na_matrix2 is running correctly when only one NA Matrix 2", {
  expect_equal(
    sum_matrix_ignore_na_matrix2(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,NA,7,8), nrow=2, ncol=2)),
    matrix(c(1+5, 2, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_ignore_na_matrix2 is running correctly with NA in Matrices 1 and 2", {
  expect_equal(
    sum_matrix_ignore_na_matrix2(
      matrix(c(1,NA,3,4), nrow=2, ncol=2),
      matrix(c(5,NA,7,8), nrow=2, ncol=2)),
    matrix(c(1+5, NA, 3+7, 4+8), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sum_matrix_ignore_na_matrix2 is running correctly with Infs in Matrices 1 and 2", {
  expect_equal(
    sum_matrix_ignore_na_matrix2(
      matrix(c(1,NA,3,4), nrow=2, ncol=2),
      matrix(c(5,NA,7,8), nrow=2, ncol=2)),
    matrix(c(1+5, NA, 3+7, 4+8), nrow=2, ncol=2)
  )
})


# Define your test
test_that("sum_matrix_ignore_na_matrix2 throws an error when dims are different", {
  expect_error(
    sum_matrix_ignore_na_matrix2(
      matrix(c(1,NA,NA,-Inf), nrow=2, ncol=2),
      matrix(c(Inf,-Inf), nrow=2, ncol=1)),
    "Input matrices must have the same dimensions.")
})


# Define your test
test_that("sum_matrix_ignore_na_matrix2 throws an error when dims are different", {
  expect_equal(
    sum_matrix_ignore_na_matrix2(
      matrix(c(1,NA,NA,-Inf), nrow=2, ncol=2),
      matrix(c(Inf,-Inf,1,NA), nrow=2, ncol=2)),
    matrix(c(Inf,NA,NA,-Inf), nrow=2, ncol=2)
  )
})



