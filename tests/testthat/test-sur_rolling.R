# Define your test
test_that("sur_rolling is running correctly with small matrices", {
  expect_equal(
    sur_rolling(
      matrix(c(5,3,7,8), nrow = 2, ncol = 2),
      matrix(c(1,2,6,4), nrow = 2, ncol = 2)),
    matrix(c(0.37796447301,0,1,1.13389341903), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sur_rolling is running correctly with a data frame", {
  expect_equal(
    sur_rolling(
      data.frame(matrix(c(5,3,7,8), nrow = 2, ncol = 2)),
      data.frame(matrix(c(1,2,6,4), nrow = 2, ncol = 2))),
    matrix(c(0.37796447301,0,1,1.13389341903), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sur_rolling is running correctly with a tibble", {
  expect_equal(
    sur_rolling(
      data.frame(matrix(c(5,3,7,8), nrow = 2, ncol = 2)),
      tibble::as_tibble(matrix(c(1,2,6,4), nrow = 2, ncol = 2), .name_repair = "unique")
      ),
    matrix(c(0.37796447301,0,1,1.13389341903), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sur_rolling is running correctly when complementary matrix has only one column", {
  expect_equal(
    sur_rolling(
      matrix(c(5,3,7,8), nrow = 2, ncol = 2),
      matrix(c(1,2), nrow = 2, ncol = 1)),
    matrix(c((5-mean(c(5,1)))/sd(c(5,1)),
             (3-mean(c(3,2)))/sd(c(3,2)),
             (7-mean(c(7,5)))/sd(c(7,5)),
             (8-mean(c(8,3)))/sd(c(8,3))), nrow=2, ncol=2)
  )
})

# Define your test
test_that("sur_rolling is running correctly when there are repeated values in data frame", {
  expect_equal(
    sur_rolling(
      data.frame(matrix(c(10,10,15,20,10,20), nrow =2, ncol=3)),
      data.frame(matrix(c(10,2,5,10,25,5), nrow=2, ncol=3))),
    matrix(c(-0.32025630761, 1.07222192850, 0.14638501094, 1.09108945118, -0.87287156094, 1.09108945118
    ), nrow=2, ncol=3)
  )
})


# Define your test
test_that("sur_rolling is running correctly when there are non unique values in matrix", {
  expect_equal(
    sur_rolling(
      matrix(c(10,10,15,20,10,20), nrow =2, ncol=3),
      matrix(c(10,2,5,10,25,5), nrow=2, ncol=3)),
    matrix(c(-0.32025630761, 1.07222192850, 0.14638501094, 1.09108945118, -0.87287156094, 1.09108945118
    ), nrow=2, ncol=3)
  )
})

# Define your test
test_that("sur_rolling is running correctly when there are NAs", {
  expect_equal(
    sur_rolling(
      matrix(c(10,10,NA,NA,10,20), nrow=2, ncol=3),
      matrix(c(10,NA,5,10,25,5), nrow=2, ncol=3)),
    matrix(c(-0.3202563076, 0.7071067812, NA, NA, -0.7071067812, 1.09108945118), nrow=2, ncol=3)
  )
})

# Define your test
test_that("sur_rolling throws an error when number of rows differ", {
  expect_error(
    sur_rolling(
      matrix(c(10,10,1,-4,10,20), nrow=2, ncol=3),
      matrix(c(10,3,5,10), nrow=1, ncol=4)), "Main matrix and complementary_matrix should have same number of rows."
  )
})

# Define your test
test_that("sur_rolling throws an error when one matrix is missing", {
  expect_error(
    sur_rolling(
      matrix(c(10,10,1,-4,10,20), nrow=2, ncol=3)
  ))
})


# Define your test
test_that("sur_rolling is running correctly when there are Infs", {
  expect_equal(
    sur_rolling(
      matrix(c(5,3,Inf,8), nrow = 2, ncol = 2),
      matrix(c(1,Inf), nrow = 2, ncol = 1)),
    matrix(c((5-mean(c(5,1)))/sd(c(5,1)),
             (3-mean(c(3,Inf)))/sd(c(3,Inf)),
             (Inf-mean(c(Inf,5)))/sd(c(Inf,5)),
             (8-mean(c(8,3)))/sd(c(8,3))), nrow=2, ncol=2)
  )
})
