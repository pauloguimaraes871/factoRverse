testthat::test_that("geometric_mean_return matches manual calc", {
  r <- c(2, 0.5, -1)              # 2 %, 0.5 %, -1 %
  expected <- {
    gm_dec <- prod(1 + r/100)^(1/3) - 1
    gm_dec * 100                  # back to % points
  }
  testthat::expect_equal(geometric_mean_return(r), expected)
})

testthat::test_that("geometric_mean_return handles NA with na.rm = TRUE", {
  r <- c(2, NA, 1)
  expected <- {
    gm_dec <- prod(1 + c(2,1)/100)^(1/2) - 1
    gm_dec * 100
  }
  testthat::expect_equal(geometric_mean_return(r, na.rm = TRUE), expected)
})

testthat::test_that("geometric_mean_return works with single return", {
  r <- 2
  expected <- 2
  testthat::expect_equal(geometric_mean_return(r), expected)
})

testthat::test_that("geometric_mean_return errors on <= -100", {
  testthat::expect_error(geometric_mean_return(c(1, -101)), "greater than -100")
})

