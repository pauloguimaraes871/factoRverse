# Define your test
test_that("Sharpe Matrix is running correctly.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2))),
    matrix(c(mean(c(0,4,8), na.rm = TRUE)/sd(c(0,4,8), na.rm = TRUE),
             mean(c(1,5,9), na.rm = TRUE)/sd(c(1,5,9), na.rm = TRUE),
             mean(c(6,10,2), na.rm = TRUE)/sd(c(6,10,2), na.rm = TRUE),
             mean(c(7,11,3), na.rm = TRUE)/sd(c(7,11,3), na.rm = TRUE))
           , nrow=2, ncol=2)
  )

  expect_equal(
  sharpe_matrix(list(matrix(c(0,-1,2,3), nrow=2, ncol=2), matrix(c(4,5,-6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2))),
  matrix(c(1,0.86094603,0.25,1.75)
         , nrow=2, ncol=2)
  )


})

# Define your test
test_that("Sharpe Matrix is running correctly - Data Frames.", {
  expect_equal(
    sharpe_matrix(list(data.frame(matrix(c(0,1,2,3), nrow=2, ncol=2)), data.frame(matrix(c(4,5,6,7), nrow=2, ncol=2)),
                       data.frame(matrix(c(8,9,10,11), nrow=2, ncol=2)))),
    matrix(c(mean(c(0,4,8), na.rm = TRUE)/sd(c(0,4,8), na.rm = TRUE),
             mean(c(1,5,9), na.rm = TRUE)/sd(c(1,5,9), na.rm = TRUE),
             mean(c(6,10,2), na.rm = TRUE)/sd(c(6,10,2), na.rm = TRUE),
             mean(c(7,11,3), na.rm = TRUE)/sd(c(7,11,3), na.rm = TRUE))
           , nrow=2, ncol=2)
  )
}
)

# Define your test
test_that("Sharpe Matrix is running correctly with Infs.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(-Inf,5,6,7), nrow=2, ncol=2), matrix(c(8,9,Inf,11), nrow=2, ncol=2))),
    matrix(c(NaN,
             mean(c(1,5,9), na.rm = TRUE)/sd(c(1,5,9), na.rm = TRUE),
             NaN,
             mean(c(7,11,3), na.rm = TRUE)/sd(c(7,11,3), na.rm = TRUE))
           , nrow=2, ncol=2)
  )


})

# Define your test
test_that("Sharpe Matrix is running correctly - Some NA's.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(NA,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,NA,11), nrow=2, ncol=2))),
    matrix(c(mean(c(NA,4,8), na.rm = TRUE)/sd(c(NA,4,8), na.rm = TRUE),
             mean(c(1,5,9), na.rm = TRUE)/sd(c(1,5,9), na.rm = TRUE),
             mean(c(6,NA,2), na.rm = TRUE)/sd(c(6,NA,2), na.rm = TRUE),
             mean(c(7,11,3), na.rm = TRUE)/sd(c(7,11,3), na.rm = TRUE))
           , nrow=2, ncol=2)
  )
}
)

# Define your test
test_that("Sharpe Matrix is running correctly - Many NA's.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(NA,1,2,3), nrow=2, ncol=2),  matrix(c(4,NA,6,7), nrow=2, ncol=2), matrix(c(8,NA,NA,11), nrow=2, ncol=2))),
    matrix(c(mean(c(NA,4,8), na.rm = TRUE)/sd(c(NA,4,8), na.rm = TRUE),
             mean(c(1,NA,NA), na.rm = TRUE)/sd(c(1,NA,NA), na.rm = TRUE),
             mean(c(6,NA,2), na.rm = TRUE)/sd(c(6,NA,2), na.rm = TRUE),
             mean(c(7,11,3), na.rm = TRUE)/sd(c(7,11,3), na.rm = TRUE))
           , nrow=2, ncol=2)
  )

  expect_equal(
    sharpe_matrix(list(matrix(c(NA,NA,2,3), nrow=2, ncol=2),  matrix(c(4,NA,6,7), nrow=2, ncol=2), matrix(c(NA,NA,NA,NA), nrow=2, ncol=2))),
    matrix(c(mean(c(NA,4,NA), na.rm = TRUE)/sd(c(NA,4,NA), na.rm = TRUE),
             mean(c(NA,NA,NA), na.rm = TRUE)/sd(c(1,NA,NA), na.rm = TRUE),
             mean(c(6,NA,2), na.rm = TRUE)/sd(c(6,NA,2), na.rm = TRUE),
             mean(c(7,NA,3), na.rm = TRUE)/sd(c(7,NA,3), na.rm = TRUE))
           , nrow=2, ncol=2)
  )

}
)




# Define your test
test_that("Sharpe Matrix is running correctly - All NA's.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),  matrix(c(NA,NA,NA,NA), nrow=2, ncol=2), matrix(c(NA,NA,NA,NA), nrow=2, ncol=2))),
    matrix(c(NaN,NaN,NaN,NaN)
           , nrow=2, ncol=2)
  )
}
)



# Define your test
test_that("Sharpe Matrix is running correctly - Many Matrices.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2),
                       matrix(c(12,13,14,15), nrow=2, ncol=2), matrix(c(16,17,18,19), nrow=2, ncol=2))),
    matrix(c(mean(c(0,4,8,12,16), na.rm = TRUE)/sd(c(0,4,8,12,16), na.rm = TRUE),
             mean(c(1,5,9,13,17), na.rm = TRUE)/sd(c(1,5,9,13,17), na.rm = TRUE),
             mean(c(6,10,2,14,18), na.rm = TRUE)/sd(c(6,10,2,14,18), na.rm = TRUE),
             mean(c(7,11,3,15,19), na.rm = TRUE)/sd(c(7,11,3,15,19), na.rm = TRUE))
           , nrow=2, ncol=2)
  )
}
)

# Define your test
test_that("Sharpe Matrix is running correctly - Many columns and rows.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(0,1,2,3,4,5,6,7,8), nrow=3, ncol=3),
                       matrix(c(9,10,11,12,13,14,15,16,17), nrow=3, ncol=3),
                       matrix(c(18,19,20,21,22,23,24,25,26), nrow=3, ncol=3))),
    matrix(c(mean(c(0,9,18), na.rm = TRUE)/sd(c(0,9,18), na.rm = TRUE),
             mean(c(1,10,19), na.rm = TRUE)/sd(c(1,10,19), na.rm = TRUE),
             mean(c(2,11,20), na.rm = TRUE)/sd(c(2,11,20), na.rm = TRUE),
             mean(c(3,12,21), na.rm = TRUE)/sd(c(3,12,21), na.rm = TRUE),
             mean(c(4,13,22), na.rm = TRUE)/sd(c(4,13,22), na.rm = TRUE),
             mean(c(5,14,23), na.rm = TRUE)/sd(c(5,14,23), na.rm = TRUE),
             mean(c(6,15,24), na.rm = TRUE)/sd(c(6,15,24), na.rm = TRUE),
             mean(c(7,16,25), na.rm = TRUE)/sd(c(7,16,25), na.rm = TRUE),
             mean(c(8,17,26), na.rm = TRUE)/sd(c(8,17,26), na.rm = TRUE))
           , nrow=3, ncol=3)
  )
}
)

# Define your test
test_that("Sharpe Matrix is running correctly with non-squared matrices", {
  expect_equal(
    sharpe_matrix(list(matrix(c(0,1,2,3,4,5), nrow=3, ncol=2),
                       matrix(c(9,10,11,12,13,14), nrow=3, ncol=2),
                       matrix(c(18,19,20,21,22,23), nrow=3, ncol=2))),
    matrix(c(mean(c(0,9,18), na.rm = TRUE)/sd(c(0,9,18), na.rm = TRUE),
             mean(c(1,10,19), na.rm = TRUE)/sd(c(1,10,19), na.rm = TRUE),
             mean(c(2,11,20), na.rm = TRUE)/sd(c(2,11,20), na.rm = TRUE),
             mean(c(3,12,21), na.rm = TRUE)/sd(c(3,12,21), na.rm = TRUE),
             mean(c(4,13,22), na.rm = TRUE)/sd(c(4,13,22), na.rm = TRUE),
             mean(c(5,14,23), na.rm = TRUE)/sd(c(5,14,23), na.rm = TRUE)
             )
           , nrow=3, ncol=2)
  )
}
)

# Define your test
test_that("Sharpe Matrix is running correctly - One of elements is a data.frame.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(0,1,2,3), nrow=2, ncol=2), data.frame(matrix(c(4,5,6,7), nrow=2, ncol=2)), matrix(c(8,9,10,11), nrow=2, ncol=2),
                       matrix(c(12,13,14,15), nrow=2, ncol=2), matrix(c(16,17,18,19), nrow=2, ncol=2))),
    matrix(c(mean(c(0,4,8,12,16), na.rm = TRUE)/sd(c(0,4,8,12,16), na.rm = TRUE),
             mean(c(1,5,9,13,17), na.rm = TRUE)/sd(c(1,5,9,13,17), na.rm = TRUE),
             mean(c(6,10,2,14,18), na.rm = TRUE)/sd(c(6,10,2,14,18), na.rm = TRUE),
             mean(c(7,11,3,15,19), na.rm = TRUE)/sd(c(7,11,3,15,19), na.rm = TRUE))
           , nrow=2, ncol=2)
  )
}
)

# Define your test
test_that("Sharpe Matrix is running correctly - One of elements is a tibble.", {
  expect_equal(
    sharpe_matrix(list(matrix(c(0,1,2,3), nrow=2, ncol=2), tibble::as_tibble(matrix(c(4,5,6,7), nrow=2, ncol=2), .name_repair = "unique"), matrix(c(8,9,10,11), nrow=2, ncol=2),
                       matrix(c(12,13,14,15), nrow=2, ncol=2), matrix(c(16,17,18,19), nrow=2, ncol=2))),
    matrix(c(mean(c(0,4,8,12,16), na.rm = TRUE)/sd(c(0,4,8,12,16), na.rm = TRUE),
             mean(c(1,5,9,13,17), na.rm = TRUE)/sd(c(1,5,9,13,17), na.rm = TRUE),
             mean(c(6,10,2,14,18), na.rm = TRUE)/sd(c(6,10,2,14,18), na.rm = TRUE),
             mean(c(7,11,3,15,19), na.rm = TRUE)/sd(c(7,11,3,15,19), na.rm = TRUE))
           , nrow=2, ncol=2)
  )
}
)


# Define your test
test_that("Sharpe Matrix is running correctly - One of elements is a vector.", {
  expect_error(
    sharpe_matrix(list(matrix(c(0,1,2,3,4,5,6,7,8), nrow=3, ncol=3), as.vector(c(9,10,11,12)), matrix(c(18,19,20,21,22,23,24,25,26), nrow=3, ncol=3)))
  , "Input must be a list of matrices, data.frames, or tibbles with the same dimensions")
}
)

# Define your test
test_that("Sharpe Matrix is running correctly - One of elements is diff dimension", {
  expect_error(
    sharpe_matrix(list(matrix(c(0,1,2,3,4,5,6,7,8), nrow=3, ncol=3), matrix(c(9,10,11,12), nrow=2, ncol=2), matrix(c(18,19,20,21,22,23,24,25,26), nrow=3, ncol=3)))
  , "Input must be a list of matrices, data.frames, or tibbles with the same dimensions")
}
)
