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
      matrix(c(10,3,5,10), nrow=1, ncol=4)), "Main matrix and complementary_matrix should have same number of rows."
  )
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