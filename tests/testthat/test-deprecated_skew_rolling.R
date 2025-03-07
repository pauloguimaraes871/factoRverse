# Define your test
test_that("skew_rolling is running correctly with small matrices", {
  expect_equal(
    skew_rolling(
      main_matrix = matrix(c(5,3,7,8), nrow = 2, ncol = 2),
      complementary_matrix = matrix(c(1,2,6,4), nrow = 2, ncol = 2)),
    matrix(c(moments::skewness(c(1,6,5)),
             moments::skewness(c(3,2,4)),
             moments::skewness(c(6,5,7)),
             moments::skewness(c(8,3,4))), nrow=2, ncol=2)
  )
})

# Define your test
test_that("skew_rolling is running correctly with a data frame", {
  expect_equal(
    skew_rolling(
      data.frame(matrix(c(5,3,7,8), nrow = 2, ncol = 2)),
      data.frame(matrix(c(1,2,6,4), nrow = 2, ncol = 2))),
    matrix(c(moments::skewness(c(1,6,5)),
             moments::skewness(c(3,2,4)),
             moments::skewness(c(6,5,7)),
             moments::skewness(c(8,3,4))), nrow=2, ncol=2)
  )
})


# Define your test
test_that("skew_rolling is running correctly with a tibble", {
  expect_equal(
    skew_rolling(
      tibble::as_tibble(matrix(c(5,3,7,8), nrow = 2, ncol = 2), .name_repair = "unique"),
      data.frame(matrix(c(1,2,6,4), nrow = 2, ncol = 2))),
    matrix(c(moments::skewness(c(1,6,5)),
             moments::skewness(c(3,2,4)),
             moments::skewness(c(6,5,7)),
             moments::skewness(c(8,3,4))), nrow=2, ncol=2)
  )
})


# Define your test
test_that("skew_rolling is running correctly when complementary matrix has only one column", {
  expect_equal(
    skew_rolling(
      matrix(c(5,3,7,8), nrow = 2, ncol = 2),
      matrix(c(1,2), nrow = 2, ncol = 1)),
    matrix(c(moments::skewness(c(5,1)),
             moments::skewness(c(3,2)),
             moments::skewness(c(7,5)),
             moments::skewness(c(8,3))), nrow=2, ncol=2)
  )
})

# Define your test
test_that("skew_rolling is running correctly when there are repeated values in data frame", {
  expect_equal(
    skew_rolling(
      data.frame(matrix(c(10,10,15,20,10,20), nrow =2, ncol=3)),
      data.frame(matrix(c(10,2,5,10,25,5), nrow=2, ncol=3))
      ),
    matrix(c(moments::skewness(c(10,5,25,10)),
             moments::skewness(c(10,5,10,2)),
             moments::skewness(c(15,25,5,10)),
             moments::skewness(c(20,10,5,10)),
             moments::skewness(c(10,15,10,25)),
             moments::skewness(c(20,20,10,5)))
    , nrow=2, ncol=3)
  )
})

# Define your test
test_that("skew_rolling is running correctly when there are NAs", {
  expect_equal(
    skew_rolling(
      data.frame(matrix(c(10,10,NA,20,10,20), nrow =2, ncol=3)),
      data.frame(matrix(c(10,2,5,10,NA,5), nrow=2, ncol=3))
    ),
    matrix(c(moments::skewness(c(10,5,NA,10), na.rm = TRUE),
             moments::skewness(c(10,5,10,2), na.rm = TRUE),
             moments::skewness(c(NA,NA,5,10), na.rm = TRUE),
             moments::skewness(c(20,10,5,10), na.rm = TRUE),
             moments::skewness(c(10,NA,10,NA), na.rm = TRUE),
             moments::skewness(c(20,20,10,5), na.rm = TRUE))
           , nrow=2, ncol=3)
  )
})


# Define your test
test_that("skew_rolling throws an error when number of rows differ", {
  expect_error(
    skew_rolling(
      matrix(c(10,10,1,-4,10,20), nrow=2, ncol=3),
      matrix(c(10,3,5,10), nrow=1, ncol=4)), "Main matrix and complementary_matrix should have the same number of rows."
  )
})

# Define your test
test_that("skew_rolling throws an error when one matrix is missing", {
  expect_error(
    skew_rolling(
      matrix(c(10,10,1,-4,10,20), nrow=2, ncol=3)
    ))
})



# Define your test
test_that("skew_rolling is running correctly when there are Infs", {
  expect_equal(
    skew_rolling(
      matrix(c(5,3,Inf,8), nrow = 2, ncol = 2),
      matrix(c(1,Inf), nrow = 2, ncol = 1)),
    matrix(c(moments::skewness(c(5,1)),
              moments::skewness(c(3,Inf)),
              moments::skewness(c(Inf,5)),
              moments::skewness(c(8,3))), nrow = 2, ncol = 2)

  )
})

# Define your test
test_that("skew_rolling is running correctly when there are only infs", {
  expect_equal(
    skew_rolling(
      matrix(c(Inf,Inf,Inf,Inf), nrow = 2, ncol = 2),
      matrix(c(Inf,-Inf), nrow = 2, ncol = 1)),
    matrix(c(moments::skewness(c(Inf,Inf)),
             moments::skewness(c(Inf,-Inf)),
             moments::skewness(c(Inf,Inf)),
             moments::skewness(c(Inf,-Inf))), nrow = 2, ncol = 2)

  )
})

