# Define your test
test_that("Idio Vol is running correctly.", {
  expect_equal(
    idio_vol(
      matrix(c(4,5,6,7), nrow=2, ncol=2), #Vol Assets
      matrix(c(3,2), nrow=1, ncol=2), #Vol bench
      matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2)), #Beta
    matrix(c(sqrt((4^2) - ((1.1^2)*(3^2))),
             sqrt((5^2) - ((1.2^2)*(3^2))),
             sqrt((6^2) - ((0.9^2)*(2^2))),
             sqrt((7^2) - ((1.0^2)*(2^2)))
    ), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Idio Vol is running correctly with non-square matrix.", {
  expect_equal(
    idio_vol(
      matrix(c(4,5,6,7,1,4), nrow=2, ncol=3), #Vol Assets
      matrix(c(3,2,3), nrow=1, ncol=3), #Vol bench
      matrix(c(1.1,1.2,0.9,1,-1,0.5), nrow=2, ncol=3)), #Beta
    matrix(c(sqrt((4^2) - ((1.1^2)*(3^2))),
             sqrt((5^2) - ((1.2^2)*(3^2))),
             sqrt((6^2) - ((0.9^2)*(2^2))),
             sqrt((7^2) - ((1.0^2)*(2^2))),
             NA,
             sqrt((4^2) - ((0.5^2)*(3^2)))
    ), nrow=2, ncol=3)
  )
})


# Define your test
test_that("Idio Vol is running correctly with negative Beta.", {
  expect_equal(
    idio_vol(
      matrix(c(4,5,6,7), nrow=2, ncol=2), #Vol Assets
      matrix(c(3,2), nrow=1, ncol=2), #Vol bench
      matrix(c(1.1,-1.2,0.9,1), nrow=2, ncol=2)), #Beta
    matrix(c(sqrt((4^2) - ((1.1^2)*(3^2))),
             sqrt((5^2) - ((1.2^2)*(3^2))),
             sqrt((6^2) - ((0.9^2)*(2^2))),
             sqrt((7^2) - ((1.0^2)*(2^2)))
    ), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Idio Vol is running correctly with Data Frame.", {
  expect_equal(
    idio_vol(
      data.frame(matrix(c(4,5,6,7), nrow=2, ncol=2)),
      data.frame(matrix(c(3,2), nrow=1, ncol=2)),
      data.frame(matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2))),
    matrix(c(sqrt((4^2) - ((1.1^2)*(3^2))),
             sqrt((5^2) - ((1.2^2)*(3^2))),
             sqrt((6^2) - ((0.9^2)*(2^2))),
             sqrt((7^2) - ((1.0^2)*(2^2)))
    ), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Idio Vol is running correctly with NA", {
  expect_equal(
    idio_vol(
      matrix(c(4,NA,6,7), nrow=2, ncol=2),
      matrix(c(3,2), nrow=1, ncol=2),
      matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(sqrt((4^2) - ((1.1^2)*(3^2))),
             sqrt((NA^2) - ((1.2^2)*(3^2))),
             sqrt((6^2) - ((0.9^2)*(2^2))),
             sqrt((7^2) - ((1.0^2)*(2^2)))
    ), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Idio Vol is running correctly with negative sqrt", {
  expect_equal(
    idio_vol(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(3,2), nrow=1, ncol=2),
      matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(ifelse((1^2) - ((1.1^2)*(3^2)) < 0, NA, sqrt((1^2) - ((1.1^2)*(3^2)))),
             ifelse((2^2) - ((1.2^2)*(3^2)) < 0, NA, sqrt((2^2) - ((1.2^2)*(3^2)))),
             ifelse((3^2) - ((0.9^2)*(2^2)) < 0, NA, sqrt((3^2) - ((0.9^2)*(2^2)))),
             ifelse((4^2) - ((1.0^2)*(2^2)) < 0, NA, sqrt((4^2) - ((1.0^2)*(2^2))))
    ), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Idio Vol is running correctly with Infs", {
  expect_equal(
    idio_vol(
      vol_assets = matrix(c(1,2,3,Inf), nrow=2, ncol=2),
      vol_bench  = matrix(c(2,2), nrow=1, ncol=2),
      beta_bench = matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(ifelse((1^2) - ((1.1^2)*(2^2)) < 0, NA, sqrt((1^2) - ((1.1^2)*(2^2)))),
             ifelse((2^2) - ((1.2^2)*(2^2)) < 0, NA, sqrt((2^2) - ((1.2^2)*(2^2)))),
             ifelse((3^2) - ((0.9^2)*(2^2)) < 0, NA, sqrt((3^2) - ((0.9^2)*(2^2)))),
             ifelse((Inf^2) - ((1^2)*(2^2)) < 0, NA, sqrt((Inf^2) - ((1^2)*(2^2))))
    ), nrow=2, ncol=2)
  )
  
  expect_equal(
    idio_vol(
      vol_assets = matrix(c(1,2,3,Inf), nrow=2, ncol=2),
      vol_bench  = matrix(c(2,2), nrow=1, ncol=2),
      beta_bench = matrix(c(-Inf,1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(ifelse((1^2) - (((-Inf)^2)*(2^2)) < 0, NA, sqrt((1^2) - (((-Inf)^2)*(2^2)))),
             ifelse((2^2) - ((1.2^2)*(2^2)) < 0, NA, sqrt((2^2) - ((1.2^2)*(2^2)))),
             ifelse((3^2) - ((0.9^2)*(2^2)) < 0, NA, sqrt((3^2) - ((0.9^2)*(2^2)))),
             ifelse((Inf^2) - ((1^2)*(2^2)) < 0, NA, sqrt((Inf^2) - ((1^2)*(2^2))))
    ), nrow=2, ncol=2)
  )
  
  expect_equal(
    idio_vol(
      vol_assets = matrix(c(1,2,3,Inf), nrow=2, ncol=2),
      vol_bench  = matrix(c(2,Inf), nrow=1, ncol=2),
      beta_bench = matrix(c(-Inf,1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(ifelse((1^2) - (((-Inf)^2)*(2^2)) < 0, NA, sqrt((1^2) - (((-Inf)^2)*(2^2)))),
             ifelse((2^2) - ((1.2^2)*(2^2)) < 0, NA, sqrt((2^2) - ((1.2^2)*(2^2)))),
             ifelse((3^2) - ((0.9^2)*(Inf^2)) < 0, NA, sqrt((3^2) - ((0.9^2)*(Inf^2)))),
             ifelse((Inf^2) - ((1^2)*(Inf^2)) < 0, NA, sqrt((Inf^2) - ((1^2)*(Inf^2))))
    ), nrow=2, ncol=2)
  )
  
  expect_equal(
    idio_vol(
      vol_assets = matrix(c(1,2,3,Inf), nrow=2, ncol=2),
      vol_bench  = matrix(c(2,Inf), nrow=1, ncol=2),
      beta_bench = matrix(c((-Inf+1),1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(ifelse((1^2) - (((-Inf+1)^2)*(2^2)) < 0, NA, sqrt((1^2) - (((-Inf+1)^2)*(2^2)))),
             ifelse((2^2) - ((1.2^2)*(2^2)) < 0, NA, sqrt((2^2) - ((1.2^2)*(2^2)))),
             ifelse((3^2) - ((0.9^2)*(Inf^2)) < 0, NA, sqrt((3^2) - ((0.9^2)*(Inf^2)))),
             ifelse((Inf^2) - ((1^2)*(Inf^2)) < 0, NA, sqrt((Inf^2) - ((1^2)*(Inf^2))))
    ), nrow=2, ncol=2)
  )
  
  expect_equal(
    idio_vol(
      vol_assets = matrix(c(1,2,3,Inf), nrow=2, ncol=2),
      vol_bench  = matrix(c(2,(Inf-1)), nrow=1, ncol=2),
      beta_bench = matrix(c((-Inf+1),1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(ifelse((1^2) - (((-Inf+1)^2)*(2^2)) < 0, NA, sqrt((1^2) - (((-Inf+1)^2)*(2^2)))),
             ifelse((2^2) - ((1.2^2)*(2^2)) < 0, NA, sqrt((2^2) - ((1.2^2)*(2^2)))),
             ifelse((3^2) - ((0.9^2)*((Inf-1)^2)) < 0, NA, sqrt((3^2) - ((0.9^2)*((Inf-1)^2)))),
             ifelse((Inf^2) - ((1^2)*((Inf-1)^2)) < 0, NA, sqrt((Inf^2) - ((1^2)*((Inf-1)^2))))
    ), nrow=2, ncol=2)
  )
  
  
})

 

# Define your test
test_that("Idio Vol is running correctly with negative benchmark volatility", {
  expect_error(
    idio_vol(
      matrix(c(1,2,3,Inf), nrow=2, ncol=2),
      matrix(c(2,-Inf), nrow=1, ncol=2),
      matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2)),
    "Benchmark volatility is probably wrong"
  )
  
})

# Define your test
test_that("Idio Vol is running correctly with Zero Volatility", {
  expect_equal(
    idio_vol(
      matrix(c(0,0,0,0), nrow=2, ncol=2),
      matrix(c(3,2), nrow=1, ncol=2),
      matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(NA,NA,NA,NA
    ), nrow=2, ncol=2)
  )
})

# Define your test
test_that("Idio Vol is running correctly with Negative Volatility", {
  expect_equal(
    idio_vol(
      matrix(c(-2,-3,4,0), nrow=2, ncol=2),
      matrix(c(3,2), nrow=1, ncol=2),
      matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2)),
    matrix(c(NA,NA,
             sqrt(4^2 - 0.9^2*2^2),
             NA
    ), nrow=2, ncol=2)
  )
})


# Define your test
test_that("Idio Vol is running correctly with Zero Beta", {
  expect_equal(
    idio_vol(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(3,2), nrow=1, ncol=2),
      matrix(c(0,0,0,0), nrow=2, ncol=2)),
    matrix(c(ifelse((1^2) - ((0^2)*(3^2)) < 0, NA, sqrt((1^2) - ((0^2)*(3^2)))),
             ifelse((2^2) - ((0^2)*(3^2)) < 0, NA, sqrt((2^2) - ((0^2)*(3^2)))),
             ifelse((3^2) - ((0^2)*(2^2)) < 0, NA, sqrt((3^2) - ((0^2)*(2^2)))),
             ifelse((4^2) - ((0^2)*(2^2)) < 0, NA, sqrt((4^2) - ((0^2)*(2^2))))
    ), nrow=2, ncol=2)
  )
})


# Define your test
test_that("Idio Vol is running correctly with large numbers", {
  expect_equal(
    idio_vol(
      matrix(c(1e6,2e6,3e6,4), nrow=2, ncol=2),
      matrix(c(3,2), nrow=1, ncol=2),
      matrix(c(1,1,2,0), nrow=2, ncol=2)),
    matrix(c(ifelse((1e6^2) - ((1^2)*(3^2)) < 0, NA, sqrt((1e6^2) - ((1^2)*(3^2)))),
             ifelse((2e6^2) - ((1^2)*(3^2)) < 0, NA, sqrt((2e6^2) - ((1^2)*(3^2)))),
             ifelse((3e6^2) - ((2^2)*(2^2)) < 0, NA, sqrt((3e6^2) - ((2^2)*(2^2)))),
             ifelse((4^2) - ((0^2)*(2^2)) < 0, NA, sqrt((4^2) - ((0^2)*(2^2))))
    ), nrow=2, ncol=2)
  )
})


# Define your test
test_that("Idio Vol is running correctly when there are only NAs", {
  expect_equal(
    idio_vol(
      matrix(c(NA,NA,NA,NA), nrow=2, ncol=2),
      matrix(c(3,2), nrow=1, ncol=2),
      matrix(c(1,1,2,0), nrow=2, ncol=2)),
    matrix(c(NA,NA,NA,NA
    ), nrow=2, ncol=2)
  )
})


# Define your test
test_that("Idio Vol throws an error with Zero Vol for Bench", {
  expect_error(
    idio_vol(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(0,0), nrow=1, ncol=2),
      matrix(c(1.1,1.2,0.9,1), nrow=2, ncol=2)), "Benchmark volatility is probably wrong")

})


# Define your test
test_that("Idio Vol throws and error when asset's vol and beta matrices dim differ", {
  expect_error(
    idio_vol(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(0,0), nrow=1, ncol=2),
      matrix(c(1.1,1), nrow=2, ncol=1)), "Objects don't have compatible dimensions.")
  
})


# Define your test
test_that("Idio Vol throws and error when Vol Matrix and Vol Bench dim differ", {
  expect_error(
    idio_vol(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(0,1,3), nrow=1, ncol=3),
      matrix(c(1.1,1.2,3,4), nrow=2, ncol=2)), "Objects don't have compatible dimensions.")
  
})


# Define your test
test_that("Idio Vol throws and error when nrow(vol_bench) > 1", {
  expect_error(
    idio_vol(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(0,1,3,4), nrow=2, ncol=2),
      matrix(c(1.1,1.2,3,4), nrow=2, ncol=2)), "vol_bench nrow > 1")
  
})


# Define your test
test_that("Idio Vol throws and error when Vol Matrix and Vol Bench dim differ", {
  expect_error(
    idio_vol(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(0,1), nrow=2, ncol=1),
      matrix(c(1.1,1.2,3,4), nrow=2, ncol=2)
      ), "Objects don't have compatible dimensions.")
  
})


    


