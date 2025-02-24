# Define your test
test_that(
  "CAGR is running correctly for base case.",
  {
    expect_equal(cagr(1,2,1), 1)
  }
)

# Define your test
test_that(
  "CAGR is running correctly for cases in which we have same values.",
  {
    expect_equal(cagr(1,1,1), 0)
  }
)

# Define your test
test_that(
  "CAGR returns an error when one of inputs is character .",
  {
    expect_error(cagr("A",1,1), "Inputs are not numeric")
  }
)


# Define your test
test_that(
  "CAGR is running correctly when we have two negative inputs",
  {
    expect_equal(cagr(-2,-1,1), 0.5)
  }
)

# Define your test
test_that(
  "CAGR is running correctly when second element is negative input",
  {
    expect_equal(cagr(1,-2,1), -3)
  }
)

# Define your test
test_that(
  "CAGR is running correctly when second element is negative input and we have even periods",
  {
    expect_equal(cagr(1,-2,3), ((3+1)^(1/3)-1)*-1)
  }
)

# Define your test
test_that(
  "CAGR is running correctly when first element is negative input",
  {
    expect_equal(cagr(-1,2,1), 3)
  }
)

# Define your test
test_that(
  "CAGR is running correctly when first element is negative input and we have even periods",
  {
    expect_equal(cagr(-1,2,3), (3+1)^(1/3)-1)
  }
)

# Define your test
test_that(
  "CAGR returns NA when first element is NA.",
  {
    expect_equal(cagr(NA,2,1), NA_real_)
  }
)

# Define your test
test_that(
  "CAGR returns NA when second element is NA.",
  {
    expect_equal(cagr(-1,NA,1), NA_real_)
  }
)

# Define your test
test_that(
  "CAGR returns NA both elements are NA.",
  {
    expect_equal(cagr(NA,NA,1), NA_real_)
  }
)

# Define your test
test_that("CAGR returns Inf when one of elements is 0 and second is positive.", {
  expect_true(is.infinite(cagr(0, 1, 1)) && cagr(0, 1, 1) > 0)
})


# Define your test
test_that("CAGR returns -Inf when one of elements is 0 and second is negative.", {
  expect_true(is.infinite(cagr(0, -1, 1)) && cagr(0, -1, 1) < 0)
})

# Define your test
test_that("CAGR returns NaN when both elements are zero.", {
  expect_true(is.nan(cagr(0, 0, 1)))
})


# Define your test
test_that(
  "CAGR is running correctly when we have zero periods",
  {
    expect_error(cagr(0,-1,0), "Period must be greater than zero.")
  }
)

# Define your test
test_that(
  "CAGR is running correctly when we have less than zero periods",
  {
    expect_error(cagr(0,-1,-2), "Period must be greater than zero.")
  }
)

# Define your test
test_that("CAGR is running correctly with extremely large values.", {
  expect_equal(cagr(1e20, 2e20, 1), 1)
})

# Define your test
test_that("CAGR is running correctly with extremely small values.", {
  expect_equal(cagr(1e-20, 2e-20, 1), 1)
})


