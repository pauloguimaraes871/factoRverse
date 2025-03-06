test_that("compute_cagr computes correct CAGR values for period = 1 (Alpha)", {
  # Create meta_dataframe
  meta_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,    # Column 1: Stock A (dates in order: 2001-03-15, 2001-04-15, 2001-05-15, 2001-06-15)
               1, 7, 4, 4,     # Column 2: Stock B
               2, 9, 9, 5),    # Column 3: Stock C
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix:
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Delta" matrix:
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

   meta_df <- compute_cagr(meta_df, period = 1, signal = "Alpha")

  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], cagr(0, 3, 1))
  expect_equal(alpha_A[3], cagr(3, 4, 1))
  expect_equal(alpha_A[4], cagr(4, 9, 1))

  # For Stock B:
  beta_B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)  # Using signal "Alpha", so values come from the "Alpha" matrix.

  expect_true(is.na(beta_B[1]))
  expect_equal(beta_B[2], cagr(3, 1, 1))
  expect_equal(beta_B[3], cagr(1, 4, 1))
  expect_equal(beta_B[4], cagr(4, 9, 1))

  # For Stock C:
  gamma_C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)
  expect_true(is.na(gamma_C[1]))
  expect_equal(gamma_C[2], cagr(10, 7, 1))
  expect_equal(gamma_C[3], cagr(7, 2, 1))
  expect_equal(gamma_C[4], cagr(2, 5, 1))
})

test_that("compute_cagr applies period correctly for period = 3 (Alpha)", {
  # Create meta_dataframe with the same structure as Test 1
  meta_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,
               3, 1, 7, 4,
               4, 4, 2, 9),
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

  # Compute CAGR for "Alpha" with period = 3; new column should be "Alpha_cagr_3"
  meta_df <- compute_cagr(meta_df, period = 3, signal = "Alpha")

  # For period = 3, only the last date (2001-06-15) should have a valid CAGR value
  # For Stock A.
  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_3)

  expect_true(is.na(alpha_A[1]))  # 2001-03-15: no previous record
  expect_true(is.na(alpha_A[2]))  # 2001-04-15: no record 3 months earlier
  expect_true(is.na(alpha_A[3]))  # 2001-05-15: no record 3 months earlier
  expect_equal(alpha_A[4], cagr(0, 4, 3))  # 2001-06-15: matches with 2001-03-15

  # For Stock B:
  Alpha_B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_3)

  expect_true(all(is.na(Alpha_B[1:3])))
  expect_equal(Alpha_B[4], cagr(begin = 3, final = 2, period = 3))

  # For Stock C
  Alpha_C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_3)

  expect_true(all(is.na(Alpha_C[1:3])))
  expect_equal(Alpha_C[4], cagr(10, 9, 3))
})

test_that("compute_cagr computes correct CAGR values for random NAs", {
  # Create meta_dataframe
  meta_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, NA, 3,    # Column 1: Stock A (dates in order: 2001-03-15, 2001-04-15, 2001-05-15, 2001-06-15)
               1, NA, 4, 4,     # Column 2: Stock B
               2, NA, NA, 5),    # Column 3: Stock C
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix:
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Delta" matrix:
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  meta_df <- compute_cagr(meta_df, period = 1, signal = "Alpha")

  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], cagr(0, 3, 1))
  expect_equal(alpha_A[3], cagr(3, 4, 1))
  expect_equal(alpha_A[4], cagr(4, NA, 1))

  # For Stock B:
  beta_B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)  # Using signal "Alpha", so values come from the "Alpha" matrix.

  expect_true(is.na(beta_B[1]))
  expect_equal(beta_B[2], cagr(3, 1, 1))
  expect_equal(beta_B[3], cagr(1, 4, 1))
  expect_equal(beta_B[4], cagr(4, NA, 1))

  # For Stock C:
  gamma_C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)
  expect_true(is.na(gamma_C[1]))
  expect_equal(gamma_C[2], cagr(NA, NA, 1))
  expect_equal(gamma_C[3], cagr(NA, 2, 1))
  expect_equal(gamma_C[4], cagr(2, 5, 1))
})

test_that("compute_cagr computes correct CAGR values for Infs", {
  # Create meta_dataframe
  meta_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, Inf, 3,    # Column 1: Stock A (dates in order: 2001-03-15, 2001-04-15, 2001-05-15, 2001-06-15)
               1, Inf, 4, 4,     # Column 2: Stock B
               2, Inf, Inf, 5),    # Column 3: Stock C
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix:
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Delta" matrix:
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  meta_df <- compute_cagr(meta_df, period = 1, signal = "Alpha")

  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], cagr(0, 3, 1))
  expect_equal(alpha_A[3], cagr(3, 4, 1))
  expect_equal(alpha_A[4], cagr(4, Inf, 1))

  # For Stock B:
  beta_B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)  # Using signal "Alpha", so values come from the "Alpha" matrix.

  expect_true(is.na(beta_B[1]))
  expect_equal(beta_B[2], cagr(3, 1, 1))
  expect_equal(beta_B[3], cagr(1, 4, 1))
  expect_equal(beta_B[4], cagr(4, Inf, 1))

  # For Stock C:
  gamma_C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_1)
  expect_true(is.na(gamma_C[1]))
  expect_equal(gamma_C[2], cagr(Inf, Inf, 1))
  expect_equal(gamma_C[3], cagr(Inf, 2, 1))
  expect_equal(gamma_C[4], cagr(2, 5, 1))
})

test_that("compute_cagr throws error when all NAs", {

  expect_error(
  # Create meta_dataframe
  meta_df <- create_meta_dataframe(
    list(
      matrix(c(NA, NA, NA, NA,    # Column 1: Stock A (dates in order: 2001-03-15, 2001-04-15, 2001-05-15, 2001-06-15)
               NA, NA, NA, NA,     # Column 2: Stock B
               NA, NA, NA, NA),    # Column 3: Stock C
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix:
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Delta" matrix:
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  ), "One or more datasets contain only NA values.")
})

test_that("compute_cagr correctly computes cagr for different periods and signals", {

  meta_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,
               3, 1, 7, 4,
               4, 4, 2, 9),
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

  # Compute CAGR for "Alpha" with period = 1 to 3; new column should be "Alpha_cagr_3"
  meta_df <- compute_cagr(meta_df, period = 1, signal = "Alpha") %>%
    compute_cagr(period = 2, signal = "Alpha") %>%
    compute_cagr(period = 3, signal = "Alpha")

  # Cagr 1 contains non NA values
  expect_gt(
    length(which(is.na(meta_df@data$Alpha_cagr_3))),
    length(which(is.na(meta_df@data$Alpha_cagr_1)))
    )

  expect_gt(
    length(which(is.na(meta_df@data$Alpha_cagr_2))),
    length(which(is.na(meta_df@data$Alpha_cagr_1)))
  )

  expect_gt(
    length(which(is.na(meta_df@data$Alpha_cagr_3))),
    length(which(is.na(meta_df@data$Alpha_cagr_2)))
  )

  #Compute CAGR for Alpha and Beta
  meta_df <- compute_cagr(meta_df, period = 1, signal = c("Beta")) %>%
                compute_cagr(period = 2, signal = "Beta")


  #Workflow
  expect_equal(names(meta_df@workflow),
               c("compute_cagr_1_Alpha_2001-06-15", "compute_cagr_2_Alpha_2001-06-15", "compute_cagr_3_Alpha_2001-06-15",
                 "compute_cagr_1_Beta_2001-06-15", "compute_cagr_2_Beta_2001-06-15"
                 )
               )


})

test_that("compute_cagr errors if the signal column is absent", {
  expect_error(
    compute_cagr(meta_df, period = 1, signal = "NonExistingSignal"),
    "The signal column does not exist in the data frame."
  )
})

test_that("compute_cagr throws an error when input is not numeric", {
  # Create meta_dataframe
  meta_df <- create_meta_dataframe(
    list(
      matrix(c(0, "A", 10, 3,    # Column 1: Stock A (dates in order: 2001-03-15, 2001-04-15, 2001-05-15, 2001-06-15)
               1, 7, 4, 4,     # Column 2: Stock B
               2, 9, 9, 5),    # Column 3: Stock C
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix:
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Delta" matrix:
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
  compute_cagr(meta_df, period = 1, signal = "Alpha"),
  "Inputs are not numeric"
  )


})
