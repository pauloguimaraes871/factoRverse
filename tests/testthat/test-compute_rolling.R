test_that("compute_rolling computes correct median values for period = 1 (Alpha)", {
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

  # Compute median for "Alpha" with period = 1; new column name will be "Alpha_median_roll_1_m"
  features_m_df <- compute_rolling(features_m_df, period = 1, signal = "Alpha", FUN = "median")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_roll_1m)

  expect_equal(alpha_A[1], 0)  # No previous record for first date
  expect_equal(alpha_A[2], median(c(3,0)))
  expect_equal(alpha_A[3], median(c(4,3)))
  expect_equal(alpha_A[4], median(c(9,4)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_roll_1m)

  expect_equal(alpha_B[1], 3)
  expect_equal(alpha_B[2], median(c(3,1)))
  expect_equal(alpha_B[3], median(c(4,1)))
  expect_equal(alpha_B[4], median(c(9,4)))

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_median_roll_1m)

  expect_equal(alpha_C[1], 10)
  expect_equal(alpha_C[2], median(c(7,10)))
  expect_equal(alpha_C[3], median(c(2,7)))
  expect_equal(alpha_C[4], median(c(5,2)))
})

test_that("compute_rolling computes correct CAGR values for period = 1 (Alpha)", {
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

  meta_df <- compute_rolling(meta_df, period = 1, signal = "Alpha", FUN = "cagr")

  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_1m)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], cagr(0, 3, 1))
  expect_equal(alpha_A[3], cagr(3, 4, 1))
  expect_equal(alpha_A[4], cagr(4, 9, 1))

  # For Stock B:
  beta_B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_1m)  # Using signal "Alpha", so values come from the "Alpha" matrix.

  expect_true(is.na(beta_B[1]))
  expect_equal(beta_B[2], cagr(3, 1, 1))
  expect_equal(beta_B[3], cagr(1, 4, 1))
  expect_equal(beta_B[4], cagr(4, 9, 1))

  # For Stock C:
  gamma_C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_1m)

  expect_true(is.na(gamma_C[1]))
  expect_equal(gamma_C[2], cagr(10, 7, 1))
  expect_equal(gamma_C[3], cagr(7, 2, 1))
  expect_equal(gamma_C[4], cagr(2, 5, 1))
})

test_that("compute_rolling computes correct sd for period = 3 (Alpha)", {
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

  # Compute sd for "Alpha" with period = 3;
  features_m_df <- compute_rolling(features_m_df, period = 3, signal = "Alpha", FUN = "sd")

  # For Stock A: Values: 0, 3, 10, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sd_roll_3m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], sd(c(0, 3)))
  expect_equal(alpha_A[3], sd(c(4,3,0)))
  expect_equal(alpha_A[4], sd(c(4,3,0,9)))

  # For Stock B: Values: 1, 7, 4, 4.
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sd_roll_3m)

  expect_equal(alpha_B[1], NA_real_)
  expect_equal(alpha_B[2], sd(c(1,3)))
  expect_equal(alpha_B[3], sd(c(1,3,4)))
  expect_equal(alpha_B[4], sd(c(1,3,4,9)))

  # For Stock C: Values: 2, 9, 9, 5.
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sd_roll_3m)

  expect_equal(alpha_C[1], NA_real_)
  expect_equal(alpha_C[2], sd(c(10,7)))
  expect_equal(alpha_C[3], sd(c(2, 7, 10), na.rm = TRUE))
  expect_equal(alpha_C[4], sd(c(2, 7, 10, 5), na.rm = TRUE))
})

test_that("compute_rolling computes correct CAGR for period = 3 (Alpha) and min_non_na Default", {
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
  meta_df <- compute_rolling(meta_df, period = 3, signal = "Alpha", FUN = "cagr")

  # For period = 3, only the last date (2001-06-15) should have a valid CAGR value
  # For Stock A.
  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_3m)

  expect_true(is.na(alpha_A[1]))  # 2001-03-15: no previous record
  expect_true(is.na(alpha_A[2]))  # 2001-04-15: no record 3 months earlier
  expect_true(is.na(alpha_A[3]))  # 2001-05-15: no record 3 months earlier
  expect_equal(alpha_A[4], cagr(0, 4, 3))  # 2001-06-15: matches with 2001-03-15

  # For Stock B:
  Alpha_B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_3m)

  expect_true(all(is.na(Alpha_B[1:3])))
  expect_equal(Alpha_B[4], cagr(begin = 3, final = 2, period = 3))

  # For Stock C
  Alpha_C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_3m)

  expect_true(all(is.na(Alpha_C[1:3])))
  expect_equal(Alpha_C[4], cagr(10, 9, 3))
})

test_that("compute_rolling computes correct skew values for random NAs and min_non_na > 2", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, NA, 3,    # "Alpha" matrix with random NAs for Stock A
               1, 9, 4, 4,     # Stock B
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

  features_m_df <- compute_rolling(features_m_df, period = 3, signal = "Alpha", min_non_na = 2, FUN = "skew")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_skew_roll_3m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], NA_real_)
  expect_equal(alpha_A[3], skew(c(3,4,0)))
  expect_equal(alpha_A[4], skew(c(4,3,NA,0), na.rm = TRUE))


  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_skew_roll_3m)

  expect_equal(alpha_B[1], NA_real_)
  expect_equal(alpha_B[2], NA_real_)
  expect_equal(alpha_B[3], skew(c(4,1,3)))
  expect_equal(alpha_B[4], skew(c(NA,4,1,3), na.rm = TRUE))

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_skew_roll_3m)

  expect_equal(alpha_C[1], NA_real_)
  expect_equal(alpha_C[2], NA_real_)
  expect_equal(alpha_C[3], NA_real_)
  expect_equal(alpha_C[4], skew(c(5,2,9,NA)))

})

test_that("compute_rolling computes correct CAGR values for random NAs and min_non_na != period (permits non NAs at beggining)", {
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

  meta_df <- compute_rolling(meta_df, period = 2, signal = "Alpha", min_non_na = 1, FUN = "cagr")

  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_2m)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], cagr(0, 3, 1))
  expect_equal(alpha_A[3], cagr(0, 4, 2))
  expect_equal(alpha_A[4], cagr(0, NA, 2))

  # For Stock B:
  alpha_B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_2m)  # Using signal "Alpha", so values come from the "Alpha" matrix.

  expect_true(is.na(alpha_B[1]))
  expect_equal(alpha_B[2], cagr(3, 1, 1))
  expect_equal(alpha_B[3], cagr(3, 4, 2))
  expect_equal(alpha_B[4], cagr(1, NA, 2))

  # For Stock C:
  Alpha_C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_2m)

  expect_true(is.na(Alpha_C[1]))
  expect_equal(Alpha_C[2], cagr(NA, NA, 1))
  expect_equal(Alpha_C[3], cagr(NA, 2, 2))
  expect_equal(Alpha_C[4], cagr(NA, 5, 2))
})

test_that("compute_rolling computes correct mean_std values for Infs", {
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

  features_m_df <- compute_rolling(features_m_df, period = 1, signal = "Alpha", FUN = "mean_std")

  # For Stock A: Values: 0, 3, Inf, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_mean_std_roll_1m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], mean_std(c(0,3)))
  expect_equal(alpha_A[3], mean_std(c(3,4)))
  expect_equal(alpha_A[4], mean_std(c(Inf,4), na.rm = TRUE))

  # For Stock B: Values: 1, Inf, 4, 4.
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_mean_std_roll_1m)

  expect_equal(alpha_B[1], NA_real_)
  expect_equal(alpha_B[2], mean_std(c(3,1)))
  expect_equal(alpha_B[3], mean_std(c(1,4)))
  expect_equal(alpha_B[4], mean_std(c(Inf,4), na.rm = TRUE))

  # For Stock C: Values: 2, Inf, Inf, 5.
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_mean_std_roll_1m)

  expect_equal(alpha_C[1], NA_real_)
  expect_equal(alpha_C[2], mean_std(c(Inf,Inf)))
  expect_equal(alpha_C[3], mean_std(c(2,Inf)))
  expect_equal(alpha_C[4], mean_std(c(2,5), na.rm = TRUE))

})

test_that("compute_rolling correctly computes CAGR for different periods and signals", {

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
  meta_df <- compute_rolling(meta_df, period = 1, signal = "Alpha", FUN = "cagr") %>%
    compute_rolling(period = 2, signal = "Alpha", FUN = "cagr") %>%
    compute_rolling(period = 3, signal = "Alpha", FUN = "cagr")

  # Cagr 1 contains non NA values
  expect_gt(
    length(which(is.na(meta_df@data$Alpha_cagr_roll_3m))),
    length(which(is.na(meta_df@data$Alpha_cagr_roll_1m)))
  )

  expect_gt(
    length(which(is.na(meta_df@data$Alpha_cagr_roll_2m))),
    length(which(is.na(meta_df@data$Alpha_cagr_roll_1m)))
  )

  expect_gt(
    length(which(is.na(meta_df@data$Alpha_cagr_roll_3m))),
    length(which(is.na(meta_df@data$Alpha_cagr_roll_2m)))
  )

  #Compute CAGR for Alpha and Beta
  meta_df <- compute_rolling(meta_df, period = 1, signal = c("Beta"), FUN = "cagr") %>%
    compute_rolling(period = 2, signal = "Beta", FUN = "cagr")


  #Workflow
  expect_equal(names(meta_df@workflow),
               c("compute_Alpha_cagr_roll_1m_2001-06-15", "compute_Alpha_cagr_roll_2m_2001-06-15", "compute_Alpha_cagr_roll_3m_2001-06-15",
                 "compute_Beta_cagr_roll_1m_2001-06-15", "compute_Beta_cagr_roll_2m_2001-06-15"
               )
  )


})

test_that("compute_rolling correctly sensibilizes period when min_non_na is not period + 1", {

    # Create meta_dataframe
    meta_df <- create_meta_dataframe(
      list(
        matrix(c(3, 3, 9, 3,    # Column 1: Stock A (dates in order: 2001-03-15, 2001-04-15, 2001-05-15, 2001-06-15)
                 1, -2, 4, 4,     # Column 2: Stock B
                 2, 5, 5, 5),    # Column 3: Stock C
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

    meta_df <- compute_rolling(meta_df, period = 2, signal = "Alpha", FUN = "cagr", min_non_na = 1)

    alpha_A <- meta_df@data %>%
      dplyr::filter(tickers == "Stock A") %>%
      dplyr::arrange(dates) %>%
      dplyr::pull(Alpha_cagr_roll_2m)

    expect_true(is.na(alpha_A[1]))  # No previous record for first date
    expect_equal(alpha_A[2], cagr(3, 3, 1))
    expect_equal(alpha_A[3], cagr(3, 4, 2))
    expect_equal(alpha_A[4], cagr(3, 5, 2))

    meta_df <- compute_rolling(meta_df, period = 3, signal = "Alpha", FUN = "cagr", min_non_na = 1)

    alpha_A <- meta_df@data %>%
      dplyr::filter(tickers == "Stock A") %>%
      dplyr::arrange(dates) %>%
      dplyr::pull(Alpha_cagr_roll_3m)

    expect_true(is.na(alpha_A[1]))  # No previous record for first date
    expect_equal(alpha_A[2], cagr(3, 3, 1))
    expect_equal(alpha_A[3], cagr(3, 4, 2))
    expect_equal(alpha_A[4], cagr(3, 5, 3))

    meta_df <- compute_rolling(meta_df, period = 1, signal = "Alpha", FUN = "cagr")

    alpha_A <- meta_df@data %>%
      dplyr::filter(tickers == "Stock A") %>%
      dplyr::arrange(dates) %>%
      dplyr::pull(Alpha_cagr_roll_1m)

    expect_true(is.na(alpha_A[1]))  # No previous record for first date
    expect_equal(alpha_A[2], cagr(3, 3, 1))
    expect_equal(alpha_A[3], cagr(3, 4, 1))
    expect_equal(alpha_A[4], cagr(4, 5, 1))

})

test_that("compute_rolling throws error when all NA values", {
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

test_that("compute_rolling correctly computes sur using unique_values = TRUE and repeated value is at final", {
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(10, 3, 10, 3,
               1, 5, 3, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      matrix(c(8, 11, 4, 11,
               11, 13, 13, 11,
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

  #Compute sur
  features_m_df <- compute_rolling(features_m_df, period = 2, signal = "Alpha", FUN = "sur", only_unique = TRUE)

  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sur_roll_2m)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], sur(c(10, 3)))
  expect_equal(alpha_A[3], sur(c(10, 3)))
  expect_equal(alpha_A[4], (9 - mean(c(9,3)))/sd(c(9,3))) #Makes sure it takes 9 as most recent

  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sur_roll_2m)

  expect_true(is.na(alpha_B[1]))  # No previous record for first date
  expect_equal(alpha_B[2], sur(c(3, 1)))
  expect_equal(alpha_B[3], sur(c(3,1,4)))
  expect_equal(alpha_B[4], (9 - mean(c(9,4,1)))/sd(c(9,4,1))) #Makes sure it takes 9 as most recent

  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sur_roll_2m)

  expect_true(is.na(alpha_C[1]))  # No previous record for first date
  expect_equal(alpha_C[2], sur(c(10, 5)))
  expect_equal(alpha_C[3], sur(c(10,5,2)))
  expect_equal(alpha_C[4], sur(c(2,5))) #Makes sure it takes 5 as most recent

  #Compute sur
  features_m_df <- compute_rolling(features_m_df, period = 3, signal = "Gamma", FUN = "sur", only_unique = TRUE)

  Gamma_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_sur_roll_3m)

  expect_true(is.na(Gamma_A[1]))  # No previous record for first date
  expect_equal(Gamma_A[2], (11 - mean(c(11,8)))/sd(c(11,8)))
  expect_equal(Gamma_A[3], (13 - mean(c(11,8,13)))/sd(c(11,8,13)))
  expect_equal(Gamma_A[4], (-3 - mean(c(-3,13,11,8)))/sd(c(-3,11,13,8))) #Makes sure it takes 9 as most recent

  Gamma_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_sur_roll_3m)

  expect_true(is.na(Gamma_B[1]))  # No previous record for first date
  expect_true(is.na(Gamma_B[2]))
  expect_true(is.na(Gamma_B[3]))
  expect_equal(Gamma_B[4], (2 - mean(c(11,2)))/sd(c(11,2))) #Makes sure it takes 9 as most recent


  Gamma_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_sur_roll_3m)

  expect_true(is.na(Gamma_C[1]))  # No previous record for first date
  expect_equal(Gamma_C[2], (13 - mean(c(13,4)))/sd(c(13,4)))
  expect_equal(Gamma_C[3], (10 - mean(c(10,13,4)))/sd(c(10,13,4)))
  expect_equal(Gamma_C[4], (13 - mean(c(13,10,4)))/sd(c(13,10,4))) #Makes sure it takes 9 as most recent


})

test_that("compute_rolling correctly computes cagr using unique_values = TRUE - real data", {

  #Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                              features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                              features_sheet_range = c("D4:F22"),
                                              tickers_sheet_range = c("C4:C22"),
                                              dates_sheet_range = c("D1:F1"),
                                              output_sheet_name = c("panel"),
                                              output_sheet_range = c("B1:I58"),
                                              industry_classification_column_name = c("sector_c1"))
  #Apply function
  panel <- create_meta_dataframe(data = results$inputs$feature_list,
                                 tickers = results$inputs$tickers$...1,
                                 dates  = results$inputs$dates,
                                 features_names = results$inputs$features_names)


  #Compute sur
  features_m_df <- compute_rolling(panel, period = 2, signal = "ebit_12m", FUN = "cagr", only_unique = TRUE, feature_name = "ebit_12m_cagr_2m", min_non_na = 1)

  RRRP3 <- features_m_df@data %>%
    dplyr::filter(tickers == "RRRP3") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ebit_12m_cagr_2m)

  expect_true(is.na(RRRP3[1]))  # No previous record for first date
  expect_equal(RRRP3[2], cagr(205648000, 113912000, 1))
  expect_equal(RRRP3[3], cagr(205648000, 113912000, 1))

  AERI3 <- features_m_df@data %>%
    dplyr::filter(tickers == "AERI3") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ebit_12m_cagr_2m)

  expect_true(is.na(AERI3[1]))  # No previous record for first date
  expect_equal(AERI3[2], cagr(254680000, 277859000, 1))
  expect_equal(AERI3[3], cagr(254680000, 277859000, 1))



})

test_that("compute_rolling errors if the signal column is absent", {
  expect_error(
    compute_rolling(features_m_df, period = 1, signal = "NonExistingSignal", FUN = "cagr"),
    "The signal column does not exist in the data frame."
  )
})

test_that("compute_rolling throws an error when input is not numeric", {
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
    compute_rolling(features_m_df, period = 1, signal = "Alpha", FUN = "median"),
    "The signal column must be numeric."
  )
})

test_that("compute_rolling computes correct res_mom for period = 3 (Alpha)", {

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

  bench_returns_m_xts <- create_meta_xts(
    xts::xts(data.frame(IBOV = c(-4,-2,4,5),
                        SMLL = c(1,5,10,5)),
             order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
             ))

  # Compute sd for "Alpha" with period = 3;
  features_m_df <- compute_rolling(features_m_df, period = 3, signal = "Alpha", FUN = "res_mom",
                                   benchmark_returns_m_xts = bench_returns_m_xts, selected_bench = "IBOV")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_res_mom_roll_3m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], res_mom(c(0,3), c(-4,-2)))
  expect_equal(alpha_A[3], res_mom(c(0,3,4), c(-4,-2,4)))
  expect_equal(alpha_A[4], res_mom(c(0,3,4,9), c(-4,-2,4,5)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_res_mom_roll_3m)

  expect_equal(alpha_B[1], NA_real_)
  expect_equal(alpha_B[2], res_mom(c(3,1), c(-4,-2)))
  expect_equal(alpha_B[3], res_mom(c(3,1,4), c(-4,-2,4)))
  expect_equal(alpha_B[4], res_mom(c(3,1,4,9), c(-4,-2,4,5)))

  # For Stock C: Values: 2, 9, 9, 5.
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_res_mom_roll_3m)

  expect_equal(alpha_C[1], NA_real_)
  expect_equal(alpha_C[2], res_mom(c(10,7), c(-4,-2)))
  expect_equal(alpha_C[3], res_mom(c(10,7,2), c(-4,-2,4)))
  expect_equal(alpha_C[4], res_mom(c(10,7,2,5), c(-4,-2,4,5)))

})


test_that("compute_rolling computes correct idio_vol for period = 3 (Alpha)", {

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

  bench_returns_m_xts <- create_meta_xts(
    xts::xts(data.frame(IBOV = c(-4,-2,4,5),
                        SMLL = c(1,5,10,5)),
             order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ))

  # Compute sd for "Alpha" with period = 3;
  features_m_df <- compute_rolling(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol",
                                   benchmark_returns_m_xts = bench_returns_m_xts, selected_bench = "IBOV")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_idio_vol_roll_3m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], idio_vol(c(0,3), c(-4,-2)))
  expect_equal(alpha_A[3], idio_vol(c(0,3,4), c(-4,-2,4)))
  expect_equal(alpha_A[4], idio_vol(c(0,3,4,9), c(-4,-2,4,5)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_idio_vol_roll_3m)

  expect_equal(alpha_B[1], NA_real_)
  expect_equal(alpha_B[2], idio_vol(c(3,1), c(-4,-2)))
  expect_equal(alpha_B[3], idio_vol(c(3,1,4), c(-4,-2,4)))
  expect_equal(alpha_B[4], idio_vol(c(3,1,4,9), c(-4,-2,4,5)))

  # For Stock C: Values: 2, 9, 9, 5.
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_idio_vol_roll_3m)

  expect_equal(alpha_C[1], NA_real_)
  expect_equal(alpha_C[2], idio_vol(c(10,7), c(-4,-2)))
  expect_equal(alpha_C[3], idio_vol(c(10,7,2), c(-4,-2,4)))
  expect_equal(alpha_C[4], idio_vol(c(10,7,2,5), c(-4,-2,4,5)))

})


test_that("compute_rolling throws an error when bench length is different from features_m_df", {

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

  bench_returns_m_xts <- create_meta_xts(
    xts::xts(data.frame(IBOV = c(-2,4,5),
                        SMLL = c(5,10,5)),
             order.by = as.Date(c("2001-04-15", "2001-05-15", "2001-06-15"))
    ))

  # Compute
  expect_error(compute_rolling(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol",
                               benchmark_returns_m_xts = bench_returns_m_xts, selected_bench = "IBOV"),
               "Lengths of returns and benchmark returns differ")

})

