# Define your test
test_that("Ind Weighted Mean is running correctly - CW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      0),
    matrix(c(1*1/(1+3) + 3*3/(1+3),
             2*2/(2+3) + 4*3/(2+3),
             1*1/(1+3) + 3*3/(1+3),
             2*2/(2+3) + 4*3/(2+3),
             5*2/(2+2) + 1*2/(2+2),
             6*1/(1+2) + 1*2/(1+2),
             5*2/(2+2) + 1*2/(2+2),
             6*1/(1+2) + 1*2/(1+2)), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,-4,5,-6,1,1,0,-4), nrow=5, ncol=2),
      matrix(c("A","B","A","B", "C", "A","B", "A", "B", "C"), nrow=5, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2,4,1), nrow=5, ncol=2),
      0),
    matrix(c(2.5, -1.6, 2.5, -1.6, 5,
             -1.33333333, 0.33333333, -1.33333333, 0.33333333, -4), nrow=5, ncol=2)
  )


})

# Define your test
test_that("Ind Weighted Mean is running correctly - EW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      1),
    matrix(c(2,3,2,3,
             3,3.5,3,3.5), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,-4,5,-6,1,1,0,-4), nrow=5, ncol=2),
      matrix(c("A","B","A","B", "C", "A","B", "A", "B", "C"), nrow=5, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2,4,1), nrow=5, ncol=2),
      1),
    matrix(c(2, -1, 2, -1, 5,
             -2.5,0.5,-2.5,0.5,-4), nrow=5, ncol=2)
  )

})


# Define your test
test_that("Ind Weighted Mean is running correctly - CW - One Sector is NA.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c(NA,"B","A","B",NA,"B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      0),
    matrix(c(NA,3.20,3.00,3.20,
             NA,2.66666667,1.00,2.6666667), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,-4,5,-6,1,1,0,-4), nrow=5, ncol=2),
      matrix(c("A","B","A","B", NA, "A","B", "A", NA, "C"), nrow=5, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2,4,1), nrow=5, ncol=2),
      0),
    matrix(c(2.5, -1.6, 2.5, -1.6, NaN,
             -1.33333333, 1, -1.33333333, NaN, -4), nrow=5, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c(NA,"B",NA,"B",NA,"B", NA, "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      0),
    matrix(c(NA,
             3.2,
             NA,
             3.2,
             NA,
             2.6666667,
             NA,
             2.6666667), nrow=4, ncol=2)
  )

})


# Define your test
test_that("Ind Weighted Mean is running correctly - EW - One Sector is NA.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c(NA,"B","A","B",NA,"B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      1),
    matrix(c(NA,
             mean(c(2,4)),
             3,
             mean(c(2,4)),
             NA,
             mean(c(6,1)),
             1,
             mean(c(6,1))), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,-4,5,-6,1,1,0,-4), nrow=5, ncol=2),
      matrix(c("A","B","A","B", NA, "A","B", "A", NA, "C"), nrow=5, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2,4,1), nrow=5, ncol=2),
      1),
    matrix(c(2, -1, 2, -1, NaN,
             -2.5,1,-2.5,NaN,-4), nrow=5, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c(NA,"B",NA,"B",NA,"B", NA, "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      1),
    matrix(c(NA,
             mean(c(2,4)),
             NA,
             mean(c(2,4)),
             NA,
             mean(c(6,1)),
             NA,
             mean(c(6,1))), nrow=4, ncol=2)
  )


})

# Define your test
test_that("Ind Weighted Mean is running correctly - EW - Two Sectors are NA.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c(NA,NA,NA,NA,NA,NA, NA, NA), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      1),
    matrix(c(NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,-4,5,-6,1,1,0,-4), nrow=5, ncol=2),
      matrix(c("A",NA,"A",NA, NA, "A",NA, "A", NA, NA), nrow=5, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2,4,1), nrow=5, ncol=2),
      1),
    matrix(c(2, NaN, 2, NaN, NaN,
             -2.5,NaN,-2.5,NaN,NaN), nrow=5, ncol=2)
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly - CW - Two Sectors are NA.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c(NA,"B",NA,"B",NA,"B", NA, "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      0),
    matrix(c(NA,
             3.2,
             NA,
             3.2,
             NA,
             2.6666667,
             NA,
             2.6666667), nrow=4, ncol=2)
  )




})


# Define your test
test_that("Ind Weighted Mean is running correctly - EW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      1),
    matrix(c(mean(c(1,3)),
             mean(c(2,4)),
             mean(c(1,3)),
             mean(c(2,4)),
             mean(c(5,1)),
             mean(c(6,1)),
             mean(c(5,1)),
             mean(c(6,1))), nrow=4, ncol=2)
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly - DFs.", {
  expect_equal(
    industry_weighted_mean(
      data.frame(matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2)),
      data.frame(matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2)),
      data.frame(matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2)),
      0),
    matrix(c(1*1/(1+3) + 3*3/(1+3),
             2*2/(2+3) + 4*3/(2+3),
             1*1/(1+3) + 3*3/(1+3),
             2*2/(2+3) + 4*3/(2+3),
             5*2/(2+2) + 1*2/(2+2),
             6*1/(1+2) + 1*2/(1+2),
             5*2/(2+2) + 1*2/(2+2),
             6*1/(1+2) + 1*2/(1+2)), nrow=4, ncol=2)
  )
})

  # Define your test
  test_that("Ind Weighted Mean is running correctly - tibbles", {
    expect_equal(
      industry_weighted_mean(
        tibble::as_tibble(matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2), .name_repair = "unique"),
        tibble::as_tibble(matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2), .name_repair = "unique"),
        tibble::as_tibble(matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2), .name_repair = "unique"),
        0),
      matrix(c(1*1/(1+3) + 3*3/(1+3),
               2*2/(2+3) + 4*3/(2+3),
               1*1/(1+3) + 3*3/(1+3),
               2*2/(2+3) + 4*3/(2+3),
               5*2/(2+2) + 1*2/(2+2),
               6*1/(1+2) + 1*2/(1+2),
               5*2/(2+2) + 1*2/(2+2),
               6*1/(1+2) + 1*2/(1+2)), nrow=4, ncol=2))

  expect_equal(
    industry_weighted_mean(
      data.frame(matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2)),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      data.frame(matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2)),
      0),
    matrix(c(1*1/(1+3) + 3*3/(1+3),
             2*2/(2+3) + 4*3/(2+3),
             1*1/(1+3) + 3*3/(1+3),
             2*2/(2+3) + 4*3/(2+3),
             5*2/(2+2) + 1*2/(2+2),
             6*1/(1+2) + 1*2/(1+2),
             5*2/(2+2) + 1*2/(2+2),
             6*1/(1+2) + 1*2/(1+2)), nrow=4, ncol=2)
  )

  })


# Define your test
test_that("Ind Weighted Mean is running correctly - Only one B - CW", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,5,6,1), nrow=3, ncol=2),
      matrix(c("A","B","A","A","B", "A"), nrow=3, ncol=2),
      matrix(c(1,2,3,2,1,2), nrow=3, ncol=2),
      0),
    matrix(c(1*1/(1+3) + 3*3/(1+3),
             2,
             1*1/(1+3) + 3*3/(1+3),
             5*2/(2+2) + 1*2/(2+2),
             6,
             5*2/(2+2) + 1*2/(2+2)), nrow=3, ncol=2)
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly - Only one B - EW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,3,5,6,1), nrow=3, ncol=2),
      matrix(c("A","B","A","A","B", "A"), nrow=3, ncol=2),
      matrix(c(1,2,3,2,1,2), nrow=3, ncol=2),
      1),
    matrix(c(mean(c(1,3)),
             2,
             mean(c(1,3)),
             mean(c(5,1)),
             6,
             mean(c(5,1))), nrow=3, ncol=2)
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly - NAs - CW", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      0),
    matrix(c(3,
             3.2,
             3,
             3.2,
             3,
             6,
             3,
             6), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,NA,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      0),
    matrix(c(3,
             2,
             3,
             2,
             5*2/(2+2) + 1*2/(2+2),
             6,
             5*2/(2+2) + 1*2/(2+2),
             6), nrow=4, ncol=2)
  )


  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,NA,5,NA,1,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,NA,3,NA,2,1,2,NA), nrow=4, ncol=2),
      0),
    matrix(c(3,
             NaN,
             3,
             NaN,
             3,
             NaN,
             3,
             NaN), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,-4,5,NA,1,1,0,NA), nrow=5, ncol=2),
      matrix(c("A","B","A",NA, "C", NA,"B", "A", "B", "C"), nrow=5, ncol=2),
      matrix(c(1,NA,3,3,2,1,2,2,4,NA), nrow=5, ncol=2),
      0),
    matrix(c(3, NaN, 3, NaN, 5,
             NaN,0.33333333,1,0.33333333,NaN), nrow=5, ncol=2)
  )


})

# Define your test
test_that("Ind Weighted Mean is running correctly - NAs - EW", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      1),
    matrix(c(3,
             3,
             3,
             3,
             3,
             3.5,
             3,
             3.5), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,NA,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      1),
    matrix(c(3,
             2,
             3,
             2,
             3,
             3.5,
             3,
             3.5), nrow=4, ncol=2)
  )


  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,NA,5,NA,1,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,NA,3,NA,2,1,2,NA), nrow=4, ncol=2),
      1),
    matrix(c(3,
             2,
             3,
             2,
             3,
             NaN,
             3,
             NaN), nrow=4, ncol=2)
  )



  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,-4,5,NA,1,1,0,NA), nrow=5, ncol=2),
      matrix(c("A","B","A",NA, "C", NA,"B", "A", "B", "C"), nrow=5, ncol=2),
      matrix(c(1,NA,3,3,2,1,2,2,4,NA), nrow=5, ncol=2),
      1),
    matrix(c(3, 2, 3, NA, 5,
             NA,0.5,1,0.5,NA), nrow=5, ncol=2)
  )

})



# Define your test
test_that("Ind Weighted Mean is running correctly - Only NA IN Characteristics - CW", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      0),
    matrix(c(NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B",NA,"B","A",NA, "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      0),
    matrix(c(NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN), nrow=4, ncol=2)
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly - Only NA in Characteristics - EW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      1),
    matrix(c(NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B",NA,"B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,NA,1,2,NA), nrow=4, ncol=2),
      1),
    matrix(c(NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN,
             NaN), nrow=4, ncol=2)
  )
})


# Define your test
test_that("Ind Weighted Mean is running correctly - Only NA in Market Cap - CW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,2,3,NA,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(NA,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      0),
    matrix(rep(NaN,8), nrow=4, ncol=2)
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly - Only NAs in Characteristics and Market Cap - CW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(NA,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(NA,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      0),
    matrix(rep(NaN,8), nrow=4, ncol=2)
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly - Only one Observation without missing values - CW", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(5,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      0),
    matrix(c(5, NaN, 5, NaN, NaN, NaN, NaN, NaN), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(5,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,NA,3,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      0),
    matrix(c(5, NaN, 5, NaN, NaN, NaN, NaN, NaN), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(5,NA,3,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      0),
    matrix(c(5, NaN, 5, NaN, NaN, NaN, NaN, NaN), nrow=4, ncol=2)
  )

})

# Define your test
test_that("Ind Weighted Mean is running correctly - Only one Observation without missing values - EW", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(5,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      1),
    matrix(c(5, NaN, 5, NaN, NaN, NaN, NaN, NaN), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(5,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,NA,3,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      1),
    matrix(c(5, NaN, 5, NaN, NaN, NaN, NaN, NaN), nrow=4, ncol=2)
  )

  expect_equal(
    industry_weighted_mean(
      matrix(c(5,NA,3,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,NA,NA,NA,NA,NA,NA,NA), nrow=4, ncol=2),
      1),
    matrix(c(4, NaN, 4, NaN, NaN, NaN, NaN, NaN), nrow=4, ncol=2)
  )

})


# Define your test
test_that("Ind Weighted Mean is running correctly. Only one corresponding - CW", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,NA,4,5,6,1,1,5,6), nrow=5, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B", "A", "A"), nrow=5, ncol=2),
      matrix(c(NA,2,3,3,2,1,2,2,3,2), nrow=5, ncol=2),
      0),
    matrix(c(5,
             2*2/(2+3) + 4*3/(2+3),
             5,
             2*2/(2+3) + 4*3/(2+3),
             5,
             6*1/(1+2) + 1*2/(1+2),
             1*2/(2+3+2) + 5*3/(2+3+2) + 6*2/(2+3+2),
             6*1/(1+2) + 1*2/(1+2),
             1*2/(2+3+2) + 5*3/(2+3+2) + 6*2/(2+3+2),
             1*2/(2+3+2) + 5*3/(2+3+2) + 6*2/(2+3+2)), nrow=5, ncol=2)
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly. Only one corresponding - EW", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,2,NA,4,5,6,1,1,5,6), nrow=5, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B", "A", "A"), nrow=5, ncol=2),
      matrix(c(NA,2,3,3,2,1,2,2,3,2), nrow=5, ncol=2),
      1),
    matrix(c(3,
             3,
             3,
             3,
             3,

             3.5,
             4,
             3.5,
             4,
             4), nrow=5, ncol=2)
  )
})

# Define your test
test_that("ind_weighted_mean throws an error when dims differ", {
  expect_error(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B"), nrow=2, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      1),
    "Input matrices must have the same dimensions"
  )

  expect_error(
    industry_weighted_mean(
      matrix(c(1,2,3,4), nrow=4, ncol=1),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      1),
    "Input matrices must have the same dimensions"
  )

})

# Define your test
test_that("ind_weighted_mean throws an error when weighting matrix has negative values.", {
  expect_error(
    industry_weighted_mean(
      matrix(c(1,2,3,4,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,-1,3,2,1,2,2), nrow=4, ncol=2),
      0),
      "Weighting matrix should be strictly positive"
  )
})

# Define your test
test_that("Ind Weighted Mean is running correctly with Infs - CW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(Inf,2,3,4,-Inf,6,1,1), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      0),
    matrix(c(Inf*1/(1+3) + 3*3/(1+3),
             2*2/(2+3) + 4*3/(2+3),
             Inf*1/(1+3) + 3*3/(1+3),
             2*2/(2+3) + 4*3/(2+3),
             -Inf*2/(2+2) + 1*2/(2+2),
             6*1/(1+2) + 1*2/(1+2),
             -Inf*2/(2+2) + 1*2/(2+2),
             6*1/(1+2) + 1*2/(1+2)), nrow=4, ncol=2)
  )


expect_equal(
  industry_weighted_mean(
    matrix(c(Inf,2,3,4,-Inf,6,1,1), nrow=4, ncol=2),
    matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
    matrix(c(1,2,Inf,Inf,2,1,2,2), nrow=4, ncol=2),
    0),
  matrix(c(Inf*1/(1+Inf) + 3*Inf/(1+Inf),
           2*2/(2+Inf) + 4*Inf/(2+Inf),
           Inf*1/(1+Inf) + 3*Inf/(1+Inf),
           2*2/(2+Inf) + 4*Inf/(2+Inf),
           -Inf*2/(2+2) + 1*2/(2+2),
           6*1/(1+2) + 1*2/(1+2),
           -Inf*2/(2+2) + 1*2/(2+2),
           6*1/(1+2) + 1*2/(1+2)), nrow=4, ncol=2)
)

})

# Define your test
test_that("Ind Weighted Mean is running correctly with Infs - EW.", {
  expect_equal(
    industry_weighted_mean(
      matrix(c(1,Inf,3,4,5,6,1,-Inf), nrow=4, ncol=2),
      matrix(c("A","B","A","B","A","B", "A", "B"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,2), nrow=4, ncol=2),
      1),
    matrix(c(2,Inf,2,Inf,
             3,-Inf,3,-Inf), nrow=4, ncol=2)
  )


})

# Define your test
test_that("industry_weighted_mean rejects unsupported input types", {
  # Example data for testing
  characteristic_matrix <- matrix(c(10, 20, 30, 40), nrow = 2)
  sector_classification <- matrix(c("A", "B", "A", "B"), nrow = 2)
  weighting_matrix <- matrix(c(1, 2, 1, 2), nrow = 2)

  expect_error(industry_weighted_mean(characteristic_matrix, list(1, 2), weighting_matrix, ew = 0),
               "All inputs must be matrices, data.frames, or tibbles.")
  expect_error(industry_weighted_mean(1, 2, 3, ew = 0),
               "All inputs must be matrices, data.frames, or tibbles.")
})
