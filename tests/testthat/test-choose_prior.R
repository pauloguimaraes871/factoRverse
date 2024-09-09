test_that("choose_prior works for location (normal)", {

  set.seed(123)
  vector <- rnorm(n = 10000, mean = 5, sd = 10)

  results <- choose_prior(vector, "location")

  expect_equal(results$distribution, "norm")
  expect_equal(results$estimated_parameters, c(mean = 5, sd = 10), tolerance = 1e-1)
  expect_equal(results$bic[3:5], c(NA_real_,NA_real_,NA_real_))

})


test_that("choose_prior works for location (t)", {

  set.seed(123)
  vector <- rt(n = 10000, df = 5)

  results <- choose_prior(vector, "location")

  expect_equal(results$distribution, "t")
  expect_equal(results$estimated_parameters, c(df = 5), tolerance = 1e-1)
  expect_equal(results$bic[3:5], c(NA_real_,NA_real_,NA_real_))

})


test_that("choose_prior works for scale (lognormal)", {

  set.seed(123)
  vector <- rlnorm(n = 10000, meanlog = 5, sdlog = 3)

  results <- choose_prior(vector, "scale")

  expect_equal(results$distribution, "lnorm")
  expect_equal(results$estimated_parameters, c(meanlog = 5, sdlog = 3), tolerance = 1e-1)

})

test_that("choose_prior works for scale (cauchy)", {

  set.seed(123)
  vector <- rcauchy(n = 10000, location = 2, scale = 1)

  results <- choose_prior(vector, "scale")

  expect_equal(results$distribution, "cauchy")
  expect_equal(results$estimated_parameters, c(location = 2, scale = 1), tolerance = 1e-1)

})

test_that("choose_prior works for scale (invgamma)", {

  set.seed(123)
  vector <- extraDistr::rinvgamma(n = 10000, alpha = 1, beta = 1)

  results <- choose_prior(vector, "scale")

  expect_equal(results$distribution, "invgamma")
  expect_equal(results$estimated_parameters, c(alpha = 1, beta = 1), tolerance = 1e-1)

})






