# Define your test
test_that("Median Matrix is running correctly.", {
  expect_equal(
    median_matrix_36m(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2)),
    matrix(c(stats::median(c(1,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE), stats::median(c(3,7,1), na.rm = TRUE),
             stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Median Matrix is running correctly with Data Frames.", {
  expect_equal(
    median_matrix_36m(
      data.frame(matrix(c(1,2,3,4), nrow=2, ncol=2)),
      data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2)),
      data.frame(matrix(c(1,1,1,1), nrow=2, ncol=2))),
    matrix(c(stats::median(c(1,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE), 
             stats::median(c(3,7,1), na.rm = TRUE), stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})


# Define your test
test_that("Median Matrix ignores NAs when there are Random NAs", {
  expect_equal(
    median_matrix_36m(
      matrix(c(NA,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,NA,1), nrow=2, ncol=2)),
    matrix(c(stats::median(c(NA,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE), 
             stats::median(c(3,7,NA), na.rm = TRUE), stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})


# Define your test
test_that("Median Matrix is running correctly with Random Characters", {
  expect_equal(
    median_matrix_36m(
      matrix(c("A",2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,"B",1), nrow=2, ncol=2)),
    matrix(c(stats::median(c("A",5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE),
             stats::median(c(3,7,"B"), na.rm = TRUE), stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})


# Define your test
test_that("Median Matrix returns matrix os NAs  when every element is NA", {
  expect_equal(
    median_matrix_36m(
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2)),
    matrix(c(stats::median(c(NA,NA,NA), na.rm = TRUE), stats::median(c(NA,NA,NA), na.rm = TRUE), 
             stats::median(c(NA,NA,NA), na.rm = TRUE), stats::median(c(NA,NA,NA), na.rm = TRUE)), nrow=2, ncol=2)
  )
})


# Define your test
test_that("Median Matrix throws an error when one of elements is NULL", {
  expect_error(
    median_matrix_36m(
      NULL,
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2))
  )
})

# Define your test
test_that("Median Matrix throws an error when one of elements is missing", {
  expect_error(
    median_matrix_36m(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2))
  )
})

# Define your test
test_that("Median Matrix throws an error when inputs have different dimensions", {
  expect_error(
    median_matrix_36m(
      matrix(c(2,3,4), nrow=3, ncol=1),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2)),
    "Input matrices must have the same dimensions."
  )
})


test_that("Median Matrix is running correctly in the presence of Infs.", {
  
  expect_equal(
    median_matrix_36m(
      matrix(c(Inf,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2)),
    matrix(c(stats::median(c(Inf,5,1), na.rm = TRUE), stats::median(c(2,6,1), na.rm = TRUE), stats::median(c(3,7,1), na.rm = TRUE),
             stats::median(c(4,8,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
  
  
})

test_that("Median Matrix is running correctly in the presence of equal matrices.", {
  expect_equal(
    median_matrix_36m(
      matrix(c(1,1,1,1), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2)),
    matrix(c(stats::median(c(1,1,1), na.rm = TRUE), stats::median(c(1,1,1), na.rm = TRUE),
             stats::median(c(1,1,1), na.rm = TRUE), stats::median(c(1,1,1), na.rm = TRUE)), nrow=2, ncol=2)
  )
})
