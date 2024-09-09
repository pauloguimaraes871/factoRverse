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



