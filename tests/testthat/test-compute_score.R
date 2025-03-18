test_that("compute_score correctly computes scores with default min_non_na", {

  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  # Define conditions
  conditions <- list(
    "Alpha" = function(x) x > 5,
    "Beta" = function(x) x < 3
  )

  # Run function
  result <- compute_score(features_m_df, conditions, feature_name = "test_score")

  expect_true("test_score" %in% names(result@data))

  # Check results
  expect_equal(result@data$test_score,
               c(0 + 0, 0 + 0, 0 + 0, 1 + 1, 0 + 0, 0 + 0, 0 + 0, 1 + 1, 1 + 0, 1 + 1, 0 + 0, 0 + 0))


})

test_that("compute_score correctly applies min_non_na threshold", {

  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, NA, 3,
               1, 7, 4, 4,
               2, NA, 9, 5),
             nrow = 3, ncol = 4),
      matrix(c(4, 7, 5, NA,
               5, 2, 4, 7,
               6, NA, -2, NA),
             nrow = 3, ncol = 4),
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  conditions <- list(
    "Alpha" = function(x) x > 5,
    "Beta" = function(x) x < 3
  )

  result <- compute_score(features_m_df, conditions, feature_name = "test_score", min_non_na = 2)

  expect_true("test_score" %in% names(result@data))

  # Check results
  expect_equal(result@data$test_score,
               c(0 + 0, 0 + NA, 0 + 0, NA + NA, 0 + 0, 0 + 0, 0 + 0, 1 + 1, NA + 0, 1 + 1, 0 + 0, 0 + NA))


  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, NA, 3,
               1, 7, 4, 4,
               2, NA, 9, 5),
             nrow = 3, ncol = 4),
      matrix(c(4, 7, 5, NA,
               5, 2, 4, 7,
               6, NA, -2, NA),
             nrow = 3, ncol = 4),
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  conditions <- list(
    "Alpha" = function(x) x > 5,
    "Beta" = function(x) x < 3,
    "Gamma" = function(x) x > 1
  )

  result <- compute_score(features_m_df, conditions, feature_name = "test_score", min_non_na = 2)

  expect_true("test_score" %in% names(result@data))

  # Check results
  expect_equal(result@data$test_score,
               c(0 + 0 + 1, 0 + 0 + 1, 0 + 0 + 1, NA + NA + 0, 0 + 0 + 1, 0 + 0 + 1, 0 + 0 + 1, 1 + 1 + 1, 0 + 0 + 1, 1 + 1 + 0, 0 + 0 + 1, 0 + 0 + 1))


})

test_that("compute_score handles missing columns", {

  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha")
  )

  missing_conditions <- list(
    "Beta" = function(x) x < 3
  )

  expect_error(compute_score(features_m_df, missing_conditions, feature_name = "test_score"),
               "The following condition names do not exist in the data: Beta")
})

test_that("compute_score handles empty conditions", {

  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha")
  )

  empty_conditions <- list()

  expect_error(compute_score(features_m_df, empty_conditions, feature_name = "test_score"),
               "Conditions must be a named list.")
})

test_that("compute_score returns all zeros when no conditions are met", {

  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 1, 2,
               -1, -2, -3, -4,
               5, 6, 7, 8),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha")
  )

  conditions <- list(
    "Alpha" = function(x) x > 100 # Unlikely condition
  )

  result <- compute_score(features_m_df, conditions, feature_name = "test_score")

  expect_true("test_score" %in% names(result@data))

  # Expect all zeroes since no row satisfies x > 100
  expect_equal(result@data$test_score, rep(0, nrow(result@data)))
})

test_that("compute_score returns all zeros when conditions is not list of fun", {

  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 1, 2,
               -1, -2, -3, -4,
               5, 6, 7, 8),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha")
  )

  conditions <- list(
    "Alpha" = "x > 2"
  )

  expect_error(
    compute_score(features_m_df, conditions, feature_name = "test_score"),
    "Each condition in the list must be a function.")

})

test_that("compute_score returns NA when min_non_na is too high", {

  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta")
  )

  conditions <- list(
    "Alpha" = function(x) x > 0,
    "Beta" = function(x) x < 10
  )

  # Setting min_non_na to a value greater than number of features (Alpha + Beta = 2 features)
  result <- compute_score(features_m_df, conditions, feature_name = "test_score", min_non_na = 3)

  expect_true("test_score" %in% names(result@data))

  # Expect all NA because min_non_na > number of available features
  expect_true(all(is.na(result@data$test_score)))
})

test_that("compute_score updates workflow", {

  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta")
  )

  conditions <- list(
    "Alpha" = function(x) x > 5,
    "Beta" = function(x) x < 3
  )

  result <- compute_score(features_m_df, conditions, feature_name = "test_score")

  last_entry <- tail(result@workflow, 1)[[1]]
  expect_equal(last_entry$feature_name, "test_score")
  expect_equal(names(last_entry$conditions), names(conditions))
})
