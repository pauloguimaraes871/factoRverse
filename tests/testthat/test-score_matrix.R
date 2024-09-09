
# Define your test
test_that("Score Matrix is running correctly.", {
  expect_equal(
    score_matrix(list(matrix(c(0,1,1,3), nrow=2, ncol=2), matrix(c(-4,3,-6,70), nrow=2, ncol=2), matrix(c(1,-9,5,11), nrow=2, ncol=2))),
    matrix(c(
      0,
      1,
      1,
      3
    )
    , nrow=2, ncol=2)
  )
}
)


test_that("Score Matrix is running correctly with non-squared matrix.", {
  expect_equal(
    score_matrix(list(matrix(c(0,1,1,3,1,-4), nrow=2, ncol=3), matrix(c(-4,3,-6,70,4,-9), nrow=2, ncol=3), matrix(c(1,-9,5,11,0,0), nrow=2, ncol=3))),
    matrix(c(
      0,
      1,
      1,
      3,
      2,
      -2
    )
    , nrow=2, ncol=3)
  )
}
)



# Define your test
test_that("Score Matrix is running correctly with Infs.", {
  expect_equal(
    score_matrix(list(matrix(c(0,Inf,1,3), nrow=2, ncol=2), matrix(c(-Inf,3,-6,70), nrow=2, ncol=2), matrix(c(1,-9,5,11), nrow=2, ncol=2))),
    matrix(c(
      0,
      1,
      1,
      3
    )
    , nrow=2, ncol=2)
  )
}
)


# Define your test
test_that("Score Matrix is running correctly only zero and 1", {
  expect_equal(
    score_matrix(list(matrix(c(0,0,-1,1), nrow=2, ncol=2), matrix(c(0,-1,1,-1), nrow=2, ncol=2), matrix(c(1,0,1,-1), nrow=2, ncol=2))),
    matrix(c(
      1,
      -1,
      1,
      -1
    )
    , nrow=2, ncol=2)
  )
}
)

# Define your test
test_that("Score Matrix is running correctly - DFs.", {
  expect_equal(
    score_matrix(list(data.frame(matrix(c(0,1,1,3), nrow=2, ncol=2)), data.frame(matrix(c(-4,3,-6,70), nrow=2, ncol=2)), data.frame(matrix(c(1,-9,5,11), nrow=2, ncol=2)))),
    matrix(c(0, 1, 1, 3)
           , nrow=2, ncol=2)
  )
}
)

# Define your test
test_that("Score Matrix is running correctly - Some NAs.", {
  expect_equal(
    score_matrix(list(data.frame(matrix(c(0,1,1,3), nrow=2, ncol=2)),
                      data.frame(matrix(c(-4,3,-6,NA), nrow=2, ncol=2)),
                      data.frame(matrix(c(1,NA,5,11), nrow=2, ncol=2)))),
    matrix(c(0,2,1,2)
           , nrow=2, ncol=2)
  )
  
  expect_equal(
    score_matrix(list(data.frame(matrix(c(0,1,1,3), nrow=2, ncol=2)),
                      data.frame(matrix(c(-4,3,NA,NA), nrow=2, ncol=2)),
                      data.frame(matrix(c(1,NA,5,11), nrow=2, ncol=2)))),
    matrix(c(0,2,2,2)
           , nrow=2, ncol=2)
  )
  
  
}
)

# Define your test
test_that("Score Matrix is running correctly - All NAs.", {
  expect_equal(
    score_matrix(list(data.frame(matrix(c(NA,NA,NA,NA), nrow=2, ncol=2)), 
                      data.frame(matrix(c(NA,NA,NA,NA), nrow=2, ncol=2)), 
                      data.frame(matrix(c(NA,NA,NA,NA), nrow=2, ncol=2)))),
    matrix(c(NA,NA,NA,NA)
           , nrow=2, ncol=2)
  )
}
)



# Define your test
test_that("Score Matrix is running correctly - Many NAs.", {
  expect_equal(
    score_matrix(list(data.frame(matrix(c(NA,1,1,3), nrow=2, ncol=2)), 
                      data.frame(matrix(c(NA,3,NA,NA), nrow=2, ncol=2)), 
                      data.frame(matrix(c(1,NA,5,11), nrow=2, ncol=2)))),
    matrix(c(1,2, 2, 2)
           , nrow=2, ncol=2)
  )
}
)


# Define your test
test_that("Score Matrix is running correctly - Many Matrices.", {
  expect_equal(
    score_matrix(list(matrix(c(0,1,1,3), nrow=2, ncol=2), matrix(c(-4,3,-6,70), nrow=2, ncol=2), matrix(c(1,-9,5,11), nrow=2, ncol=2),
                      matrix(c(10,5,2,0), nrow=2, ncol=2), matrix(c(-2,13,50,-29), nrow=2, ncol=2))),
    matrix(c(0, 3, 3, 2)
           , nrow=2, ncol=2)
  )
}
)

# Define your test
test_that("Score Matrix is running correctly -Many columns and rows.", {
  expect_equal(
    score_matrix(list(matrix(c(0,1,1,3,5,6,7,8,-2), nrow=3, ncol=3), 
                      matrix(c(-4,3,-6,70,25,2,-5,50,9), nrow=3, ncol=3),
                      matrix(c(1,-9,5,11,-2,5,-3,0,12), nrow=3, ncol=3))),
    matrix(c(0, 1, 1, 3, 1, 3, -1, 2, 1) 
           , nrow=3, ncol=3)
  )
}
)



# Define your test
test_that("Score Matrix is running correctly - One of elements is a data.frame.", {
  expect_equal(
    score_matrix(list(data.frame(matrix(c(0,1,1,3), nrow=2, ncol=2)), matrix(c(-4,3,-6,70), nrow=2, ncol=2), matrix(c(1,-9,5,11), nrow=2, ncol=2))),
    matrix(c(0, 1, 1, 3
             
    )
    , nrow=2, ncol=2)
  )
}
)

# Define your test
test_that("Score Matrix is running correctly - One of elements is a vector.", {
  expect_error(
    score_matrix(list(matrix(c(0,1,2,3,4,5,6,7,8), nrow=3, ncol=3), as.vector(c(9,10,11,12)), matrix(c(18,19,20,21,22,23,24,25,26), nrow=3, ncol=3))),
    "Input must be a list of matrices/data.frame with same dimension"
  )
}
)

# Define your test
test_that("Score Matrix is running correctly - One of elements is diff dimension", {
  expect_error(
    score_matrix(list(matrix(c(0,1,2,3,4,5,6,7,8), nrow=3, ncol=3), matrix(c(9,10,11,12), nrow=2, ncol=2),
                      matrix(c(18,19,20,21,22,23,24,25,26), nrow=3, ncol=3)))
  ,  "Input must be a list of matrices/data.frame with same dimension")
}
)