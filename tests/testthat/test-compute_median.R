test_that("compute_median computes correct median values for period = 1 (Alpha)", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix (not used in this test)
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix (not used in this test)
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Delta" matrix (not used in this test)
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  # Compute median for "Alpha" with period = 1; new column name will be "Alpha_median_1"
  features_m_df <- compute_median(features_m_df, period = 1, signal = "Alpha")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_A[1], 0)  # No previous record for first date
  expect_equal(alpha_A[2], median(c(3,0)))
  expect_equal(alpha_A[3], median(c(4,3)))
  expect_equal(alpha_A[4], median(c(9,4)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_B[1], 3)
  expect_equal(alpha_B[2], median(c(3,1)))
  expect_equal(alpha_B[3], median(c(4,1)))
  expect_equal(alpha_B[4], median(c(9,4)))

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_C[1], 10)
  expect_equal(alpha_C[2], median(c(7,10)))
  expect_equal(alpha_C[3], median(c(2,7)))
  expect_equal(alpha_C[4], median(c(5,2)))
})

test_that("compute_median applies period correctly for period = 3 (Alpha)", {
  # Create meta_dataframe with the same structure as Test 1
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

  # Compute median for "Alpha" with period = 3; new column will be "Alpha_median_3"
  features_m_df <- compute_median(features_m_df, period = 3, signal = "Alpha")

  # For Stock A: Values: 0, 3, 10, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_3)

  expect_equal(alpha_A[1], 0)
  expect_equal(alpha_A[2], median(c(0, 3)))
  expect_equal(alpha_A[3], median(c(4,3,0)))
  expect_equal(alpha_A[4], median(c(4,3,0,9)))

  # For Stock B: Values: 1, 7, 4, 4.
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_3)

  expect_equal(alpha_B[1], 3)
  expect_equal(alpha_B[2], median(c(1,3)))
  expect_equal(alpha_B[3], median(c(1,3,4)))
  expect_equal(alpha_B[4], median(c(1,3,4,9)))

  # For Stock C: Values: 2, 9, 9, 5.
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_3)

  expect_equal(alpha_C[1], 10)
  expect_equal(alpha_C[2], median(c(10,7)))
  expect_equal(alpha_C[3], median(c(2, 7, 10), na.rm = TRUE))
  expect_equal(alpha_C[4], median(c(2, 7, 10, 5), na.rm = TRUE))
})

test_that("compute_median computes correct median values for random NAs", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, NA, 3,    # "Alpha" matrix with random NAs for Stock A
               1, NA, 4, 4,     # Stock B
               2, NA, NA, 5),    # Stock C
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

  features_m_df <- compute_median(features_m_df, period = 1, signal = "Alpha")

  # For Stock A: Values: 0, 3, NA, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_A[1], 0)
  expect_equal(alpha_A[2], median(c(0,3)))
  expect_equal(alpha_A[3], median(c(3,4)))
  expect_equal(alpha_A[4], median(c(NA,4), na.rm = TRUE))


  # For Stock B: Values: 1, NA, 4, 4.
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_B[1], 3)
  expect_equal(alpha_B[2], median(c(1,3)))
  expect_equal(alpha_B[3], median(c(1,4)))
  expect_equal(alpha_B[4], median(c(NA,4), na.rm = TRUE))

  # For Stock C: Values: 2, NA, NA, 5.
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_C[1], NA_real_)
  expect_equal(alpha_C[2], NA_real_)
  expect_equal(alpha_C[3], median(c(2,NA), na.rm = TRUE))
  expect_equal(alpha_C[4], median(c(2,5)))

})

test_that("compute_median computes correct median values for Infs", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, Inf, 3,    # "Alpha" matrix with Infs for Stock A
               1, Inf, 4, 4,     # Stock B
               2, Inf, Inf, 5),   # Stock C
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

  features_m_df <- compute_median(features_m_df, period = 1, signal = "Alpha")

  # For Stock A: Values: 0, 3, Inf, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_A[1], 0)
  expect_equal(alpha_A[2], median(c(0,3)))
  expect_equal(alpha_A[3], median(c(3,4)))
  expect_equal(alpha_A[4], median(c(Inf,4), na.rm = TRUE))

  # For Stock B: Values: 1, Inf, 4, 4.
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_B[1], 3)
  expect_equal(alpha_B[2], median(c(3,1)))
  expect_equal(alpha_B[3], median(c(1,4)))
  expect_equal(alpha_B[4], median(c(Inf,4), na.rm = TRUE))

  # For Stock C: Values: 2, Inf, Inf, 5.
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_1)

  expect_equal(alpha_C[1], Inf)
  expect_equal(alpha_C[2], median(c(Inf,Inf)))
  expect_equal(alpha_C[3], median(c(2,Inf)))
  expect_equal(alpha_C[4], median(c(2,5), na.rm = TRUE))

})

test_that("compute_median throws error when all NA values", {
  expect_error(
    # Create meta_dataframe with all NAs for "Alpha"
    create_meta_dataframe(
      list(
        matrix(c(NA, NA, NA, NA,
                 NA, NA, NA, NA,
                 NA, NA, NA, NA),
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
    ),
    "One or more datasets contain only NA values."
  )
})

# Test 6: compute_median correctly computes median for different periods and signals
test_that("compute_median correctly computes median for different periods and signals", {
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

  # Compute median for "Alpha" with period = 1, 2, 3; new columns: "Alpha_median_1", "Alpha_median_2", "Alpha_median_3"
  features_m_df <- compute_median(features_m_df, period = 1, signal = "Alpha") %>%
    compute_median(period = 2, signal = "Alpha") %>%
    compute_median(period = 3, signal = "Alpha")

  # Expect same length
  expect_equal(
    length(which(is.na(features_m_df@data$Alpha_median_3))),
    length(which(is.na(features_m_df@data$Alpha_median_1)))
  )
  expect_equal(
    length(which(is.na(features_m_df@data$Alpha_median_2))),
    length(which(is.na(features_m_df@data$Alpha_median_1)))
  )
  expect_equal(
    length(which(is.na(features_m_df@data$Alpha_median_3))),
    length(which(is.na(features_m_df@data$Alpha_median_2)))
  )

  # Compute median for "Alpha" and "Beta"
  features_m_df <- compute_median(features_m_df, period = 1, signal = "Beta") %>%
    compute_median(period = 2, signal = "Beta")

  # Workflow names should reflect all compute_median steps
  expect_equal(names(features_m_df@workflow),
               c("compute_median_1_Alpha_2001-06-15", "compute_median_2_Alpha_2001-06-15", "compute_median_3_Alpha_2001-06-15",
                 "compute_median_1_Beta_2001-06-15", "compute_median_2_Beta_2001-06-15")
  )
})

# Test 7: compute_median errors if the signal column is absent
test_that("compute_median errors if the signal column is absent", {
  expect_error(
    compute_median(features_m_df, period = 1, signal = "NonExistingSignal"),
    "The signal column does not exist in the data frame."
  )
})

# Test 8: compute_median throws an error when input is not numeric
test_that("compute_median throws an error when input is not numeric", {
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, "A", 10, 3,    # "Alpha" matrix with non-numeric input for Stock A
               1, 7, 4, 4,       # Stock B
               2, 9, 9, 5),      # Stock C
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

  expect_error(
    compute_median(features_m_df, period = 1, signal = "Alpha"),
    "The signal column must be numeric."
  )
})


