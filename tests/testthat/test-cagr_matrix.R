# Define your test
test_that("CAGR Matrix is running correctly.", {
  expect_equal(
    cagr_matrix(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      1),
    matrix(c(cagr(1,5,1), cagr(2,6,1), cagr(3,7,1), cagr(4,8,1)), nrow=2, ncol=2)
  )
})

test_that("CAGR Matrix is running correctly with data frames.", {
  expect_equal(
    cagr_matrix(
      data.frame(matrix(c(1,2,3,4), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2)),
      1),
    matrix(c(cagr(1,5,1), cagr(2,6,1), cagr(3,7,1), cagr(4,8,1)), nrow=2, ncol=2)
  )
})

# Define your test
test_that("CAGR Matrix is running correctly with random NAs", {
  expect_equal(
    cagr_matrix(
      matrix(c(1,NA,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,NA), nrow=2, ncol=2),
      1),
    matrix(c(cagr(1,5,1), cagr(NA,6,1), cagr(3,7,1), cagr(4,NA,1)), nrow=2, ncol=2)
  )
})

# Define your test
test_that("CAGR Matrix is running correctly with Infs", {
  expect_equal(
    cagr_matrix(
      matrix(c(1,Inf,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,Inf), nrow=2, ncol=2),
      1),
    matrix(c(cagr(1,5,1), cagr(Inf,6,1), cagr(3,7,1), cagr(4,Inf,1)), nrow=2, ncol=2)
  )
})


# Define your test
test_that("CAGR Matrix is running correctly when there are only NAs", {
  expect_equal(
    cagr_matrix(
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      1),
    matrix(c(cagr(NA,NA,1), cagr(NA,NA,1), cagr(NA,NA,1), cagr(NA,NA,1)), nrow=2, ncol=2)
  )
})

# Define your test
test_that("period is adequately being applied to individual cagr functions", {
  expect_lt(
    cagr_matrix(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      3)[1,1],
    matrix(c(cagr(1,5,1), cagr(2,6,1), cagr(3,7,1), cagr(4,8,1)), nrow=2, ncol=2)[1,1]
  )
  
  expect_lt(
    cagr_matrix(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      3)[2,2],
    matrix(c(cagr(1,5,1), cagr(2,6,1), cagr(3,7,1), cagr(4,8,1)), nrow=2, ncol=2)[2,2]
  )
  
  expect_gt(
    cagr_matrix(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      1)[1,1],
    matrix(c(cagr(1,5,3), cagr(2,6,3), cagr(3,7,1), cagr(4,8,1)), nrow=2, ncol=2)[1,1]
  )
  
  expect_gt(
    cagr_matrix(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      1)[2,2],
    matrix(c(cagr(1,5,1), cagr(2,6,1), cagr(3,7,1), cagr(4,8,3)), nrow=2, ncol=2)[2,2]
  )
})


test_that("CAGR Matrix is running correctly with edge cases", {
  # Test with extremely large numbers
  expect_equal(
    cagr_matrix(
      matrix(1e10, nrow=2, ncol=2),
      matrix(1e11, nrow=2, ncol=2),
      1),
    matrix(cagr(1e10, 1e11, 1), nrow=2, ncol=2)
  )
  
  # Test with extremely small numbers
  expect_equal(
    cagr_matrix(
      matrix(1e-10, nrow=2, ncol=2),
      matrix(1e-9, nrow=2, ncol=2),
      1),
    matrix(cagr(1e-10, 1e-9, 1), nrow=2, ncol=2)
  )
}
)

test_that("different dimension lead to error", {
  
  # Test with matrices of different dimensions
  expect_error(
    cagr_matrix(
      matrix(1, nrow=2, ncol=2),
      matrix(2, nrow=3, ncol=3),
      1),
    "Input matrices must have the same dimensions."
  )
})

