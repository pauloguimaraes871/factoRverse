#meta_dataframe
test_that("compute_window computes correct median values for period = 1 (Alpha)", {
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
  features_m_df <- compute_window(features_m_df, period = 1, signal = "Alpha", FUN = "median", window = "rolling")

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

test_that("compute_window computes correct CAGR values for period = 1 (Alpha)", {
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

  meta_df <- compute_window(meta_df, period = 1, signal = "Alpha", FUN = "cagr")

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

test_that("compute_window computes correct sd for period = 3 (Alpha)", {
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
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "sd")

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

test_that("compute_window computes correct sum for period = 3 (Alpha)", {
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
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "sum")

  # For Stock A: Values: 0, 3, 10, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sum_roll_3m)

  expect_equal(alpha_A[1], 0)
  expect_equal(alpha_A[2], sum(c(0, 3)))
  expect_equal(alpha_A[3], sum(c(4,3,0)))
  expect_equal(alpha_A[4], sum(c(4,3,0,9)))

  # For Stock B: Values: 1, 7, 4, 4.
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sum_roll_3m)

  expect_equal(alpha_B[1], 3)
  expect_equal(alpha_B[2], sum(c(1,3)))
  expect_equal(alpha_B[3], sum(c(1,3,4)))
  expect_equal(alpha_B[4], sum(c(1,3,4,9)))

  # For Stock C: Values: 2, 9, 9, 5.
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sum_roll_3m)

  expect_equal(alpha_C[1], 10)
  expect_equal(alpha_C[2], sum(c(10,7)))
  expect_equal(alpha_C[3], sum(c(2, 7, 10), na.rm = TRUE))
  expect_equal(alpha_C[4], sum(c(2, 7, 10, 5), na.rm = TRUE))
})

test_that("compute_window computes correct max/min for period = 3 (Alpha)", {
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

  # Compute max for "Alpha" with period = 3;
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "max")

  # For Stock A: Values: 0, 3, 10, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_max_roll_3m)

  expect_equal(alpha_A[1], 0)
  expect_equal(alpha_A[2], max(c(0, 3)))
  expect_equal(alpha_A[3], max(c(4,3,0)))
  expect_equal(alpha_A[4], max(c(4,3,0,9)))

  # Compute min for "Alpha" with period = 3;
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "min")

  # For Stock A: Values: 0, 3, 10, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_min_roll_3m)

  expect_equal(alpha_A[1], 0)
  expect_equal(alpha_A[2], min(c(0, 3)))
  expect_equal(alpha_A[3], min(c(4,3,0)))
  expect_equal(alpha_A[4], min(c(4,3,0,9)))

})

test_that("compute_window computes correct lag for period = 3 (Alpha)", {
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

  # Compute lag for "Alpha" with period = 3;
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "lag")

  # For Stock A: Values: 0, 3, 10, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_lag_roll_3m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], NA_real_)
  expect_equal(alpha_A[3], NA_real_)
  expect_equal(alpha_A[4], 0)

  # Compute lag for "Alpha" with period = 1;
  features_m_df <- compute_window(features_m_df, period = 1, signal = "Alpha", FUN = "lag")

  # For Stock A: Values: 0, 3, 10, 3.
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_lag_roll_1m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], 0)
  expect_equal(alpha_A[3], 3)
  expect_equal(alpha_A[4], 4)

})

test_that("compute_window computes correct CAGR for period = 3 (Alpha)", {
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
  meta_df <- compute_window(meta_df, period = 3, signal = "Alpha", FUN = "cagr")

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

test_that("compute_window computes correct count_if (0) for period = 3 (Alpha)", {
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

  #compute count
  meta_df <- compute_window(meta_df, period = 3, signal = "Alpha", FUN = "count_if", count_condition_fun = function(x) x == 0)

  # For period = 3, only the last date (2001-06-15) should have a valid CAGR value
  # For Stock A.
  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_count_if_roll_3m)

  expect_equal(alpha_A[1], 1)
  expect_equal(alpha_A[2], 1)
  expect_equal(alpha_A[3], 1)
  expect_equal(alpha_A[4], 1)

  # For Stock B:
  Alpha_B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_count_if_roll_3m)

  expect_equal(Alpha_B[1], 0)
  expect_equal(Alpha_B[2], 0)
  expect_equal(Alpha_B[3], 0)
  expect_equal(Alpha_B[4], 0)

  # For Stock C
  Alpha_C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_count_if_roll_3m)

  expect_equal(Alpha_C[1], 0)
  expect_equal(Alpha_C[2], 0)
  expect_equal(Alpha_C[3], 0)
  expect_equal(Alpha_C[4], 0)

})

test_that("compute_window computes correct skew values for random NAs and min_non_na > 2", {
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

  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", min_non_na = 2, FUN = "skew")

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

test_that("compute_window computes correct sd values for random NAs, only unique and min_non_na > 2", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, NA, 3,    # "Alpha" matrix with random NAs for Stock A
               1, 2, 4, 4,     # Stock B
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

  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", min_non_na = 2, FUN = "sd", only_unique = TRUE)

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sd_roll_3m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], sd(c(0,3)))
  expect_equal(alpha_A[3], sd(c(3,4,0)))
  expect_equal(alpha_A[4], sd(c(4,3,NA,0), na.rm = TRUE))


  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sd_roll_3m)

  expect_equal(alpha_B[1], NA_real_)
  expect_equal(alpha_B[2], sd(c(1,3)))
  expect_equal(alpha_B[3], sd(c(4,1,3)))
  expect_equal(alpha_B[4], sd(c(NA,4,1,3), na.rm = TRUE))

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sd_roll_3m)

  expect_equal(alpha_C[1], NA_real_)
  expect_equal(alpha_C[2], NA_real_)
  expect_equal(alpha_C[3], NA_real_) #Repeated
  expect_equal(alpha_C[4], sd(c(5,2)))

  #For Stock A - Beta (note that min_non_na is appleid after only_unique)
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Beta", min_non_na = 3, FUN = "sd", only_unique = TRUE)

  beta_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Beta_sd_roll_3m)


  expect_equal(beta_A[1], NA_real_)
  expect_equal(beta_A[2], NA_real_)
  expect_equal(beta_A[3], NA_real_) #Repeated
  expect_equal(beta_A[4], sd(c(-3,4,6)))
})

test_that("compute_window computes correct mean_std values for Infs", {
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

  features_m_df <- compute_window(features_m_df, period = 1, signal = "Alpha", FUN = "mean_std")

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

test_that("compute_window works correctly window = SEASONAL", {

  num_tickers <- 3
  num_dates <- 35
  dates <- seq(as.Date("2001-03-15"), by = "month", length.out = 35)

  set.seed(123) # Ensure reproducibility
  feature_matrices <- lapply(1:4, function(i) {
    matrix(sample(-5:15, num_tickers * num_dates, replace = TRUE),
           nrow = num_tickers, ncol = num_dates)
  })

  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    feature_matrices,
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = dates,
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  #Period = 24
  features_m_df <- compute_window(features_m_df, period = 24, signal = "Alpha", FUN = "mean_std", window = "seasonal",
                                  offset_months = c(1:3))

  # For Stock A
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_mean_std_seas_24m)

  #Expect all NAs until there are at least two past obs
  expect_true(all(is.na(alpha_A[1:10])))
  expect_equal(alpha_A[11], mean_std(c(9,-3)))
  expect_equal(alpha_A[12], mean_std(c(9,-3, 5)))
  expect_equal(alpha_A[13], mean_std(c(-3, 5, 8)))
  expect_equal(alpha_A[25], mean_std(c(-3,5,8,2,12,15)))
  expect_equal(alpha_A[35], mean_std(c(15,0,2,6,8,9))) #Only 24 months

  #Period = 36m
  features_m_df <- compute_window(features_m_df, period = 36, signal = "Beta", FUN = "sd", window = "seasonal",
                                  offset_months = c(0)
                                  )

  # For Stock A
  Beta_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Beta_sd_seas_36m)

  #Expect all NAs until there are at least two past obs
  expect_true(all(is.na(Beta_A[1:12])))
  expect_equal(Beta_A[13], sd(c(8,0)))
  expect_equal(Beta_A[20], sd(c(15,14)))
  expect_equal(Beta_A[35], sd(c(6,15,2)))

  #Period = 12
  features_m_df <- compute_window(features_m_df, period = 12, signal = "Gamma", FUN = "median", window = "seasonal",
                                  offset_months = c(1))

  # For Stock A
  Gamma_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_median_seas_12m)

  #NAs until 1 complete year
  expect_true(all(is.na(Gamma_A[1:11])))
  expect_equal(Gamma_A[12], 12)
  expect_equal(Gamma_A[20], -4)
  expect_equal(Gamma_A[35], median(c(0)))

  #Period = 12
  features_m_df <- compute_window(features_m_df, period = 12, signal = "Gamma", FUN = "median", window = "seasonal",
                                  offset_months = c(1), min_non_na = 2)

  # For Stock A
  Gamma_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_median_seas_12m)

  #NAs until 1 complete year
  expect_true(all(is.na(Gamma_A)))


})

test_that("compute_window works when small meta_dataframe is being used", {

  num_tickers <- 3
  num_dates <- 10
  dates <- seq(as.Date("2001-03-15"), by = "month", length.out = 10)

  set.seed(123) # Ensure reproducibility
  feature_matrices <- lapply(1:4, function(i) {
    matrix(sample(-5:15, num_tickers * num_dates, replace = TRUE),
           nrow = num_tickers, ncol = num_dates)
  })

  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    feature_matrices,
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = dates,
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  #Period = 1
  features_m_df <- compute_window(features_m_df, period = 1, signal = "Alpha", FUN = "mean_std", window = "seasonal")

  # For Stock A
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_mean_std_seas_1m)

  #Expect all NAs until there are at least two past obs
  expect_true(all(is.na(alpha_A)))

})

test_that("compute_window correctly computes CAGR for different periods and signals", {

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
  meta_df <- compute_window(meta_df, period = 1, signal = "Alpha", FUN = "cagr") %>%
    compute_window(period = 2, signal = "Alpha", FUN = "cagr") %>%
    compute_window(period = 3, signal = "Alpha", FUN = "cagr")

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
  meta_df <- compute_window(meta_df, period = 1, signal = c("Beta"), FUN = "cagr") %>%
    compute_window(period = 2, signal = "Beta", FUN = "cagr")


  #Workflow
  expect_equal(names(meta_df@workflow),
               c("compute_Alpha_cagr_roll_1m_2001-06-15", "compute_Alpha_cagr_roll_2m_2001-06-15", "compute_Alpha_cagr_roll_3m_2001-06-15",
                 "compute_Beta_cagr_roll_1m_2001-06-15", "compute_Beta_cagr_roll_2m_2001-06-15"
               )
  )


})

test_that("compute_window correctly sensibilizes periods for CAGR, assigning NA when last value not available", {

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

  meta_df <- compute_window(meta_df, period = 2, signal = "Alpha", FUN = "cagr", min_non_na = 0)

  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_2m)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_true(is.na(alpha_A[2]))
  expect_equal(alpha_A[3], cagr(3, 4, 2))
  expect_equal(alpha_A[4], cagr(3, 5, 2))


  meta_df <- compute_window(meta_df, period = 1, signal = "Alpha", FUN = "cagr")

  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_1m)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], cagr(3, 3, 1))
  expect_equal(alpha_A[3], cagr(3, 4, 1))
  expect_equal(alpha_A[4], cagr(4, 5, 1))


  #Remove 2001-04-15 only for Ticker A
  meta_df@data <- meta_df@data %>%
    dplyr::filter(!id == "Stock A-2001-04-15")

  meta_df <- compute_window(meta_df, period = 2, signal = "Alpha", FUN = "cagr", min_non_na = 0)

  alpha_A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_cagr_roll_2m)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], cagr(begin = 3, final = 4, 2)) #For month 5, use month 3 as reference
  expect_true(is.na(alpha_A[3]))

})

test_that("compute_window throws error when all NA values", {
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

test_that("compute_window correctly computes sur using unique_values = TRUE and repeated value is at final", {
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
  features_m_df <- compute_window(features_m_df, period = 2, signal = "Alpha", FUN = "sur", only_unique = TRUE)

  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sur_roll_2m)

  expect_true(is.na(alpha_A[1]))  # No previous record for first date
  expect_equal(alpha_A[2], sur(3, c(10, 3)))
  expect_equal(alpha_A[3], sur(3, c(10, 3)))
  expect_equal(alpha_A[4], (9 - mean(c(3,9)))/sd(c(3,9))) #Makes sure it takes 9 as most recent

  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sur_roll_2m)

  expect_true(is.na(alpha_B[1]))  # No previous record for first date
  expect_equal(alpha_B[2], sur(1, c(3,1)))
  expect_equal(alpha_B[3], sur(4, c(1,3,4)))
  expect_equal(alpha_B[4], (9 - mean(c(4,1,9)))/sd(c(4,1,9))) #Makes sure it takes 9 as most recent

  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sur_roll_2m)

  expect_true(is.na(alpha_C[1]))  # No previous record for first date
  expect_equal(alpha_C[2], sur(5, c(10,5)))
  expect_equal(alpha_C[3], sur(2, c(10,5,2)))
  expect_equal(alpha_C[4], sur(5, c(2,5))) #Makes sure it takes 5 as most recent

  #Compute sur
  features_m_df@data[1,6] <- NA
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Gamma", FUN = "sur", only_unique = TRUE,
                                  min_non_na = 2)

  Gamma_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_sur_roll_3m)

  expect_true(is.na(Gamma_A[1]))  # No previous record for first date
  expect_equal(Gamma_A[2], NA_real_)
  expect_equal(Gamma_A[3], (13 - mean(c(11,13)))/sd(c(11,13)))
  expect_equal(Gamma_A[4], (-3 - mean(c(13,11,-3)))/sd(c(11,13,-3))) #Makes sure it takes -3 as most recent

  Gamma_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_sur_roll_3m)

  expect_true(is.na(Gamma_B[1]))  # No previous record for first date
  expect_true(is.na(Gamma_B[2]))
  expect_true(is.na(Gamma_B[3]))
  expect_equal(Gamma_B[4], (2 - mean(c(2,11)))/sd(c(2,11))) #Makes sure it takes 2 as most recent


  Gamma_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_sur_roll_3m)

  expect_true(is.na(Gamma_C[1]))  # No previous record for first date
  expect_equal(Gamma_C[2], (13 - mean(c(4,13)))/sd(c(4,13)))
  expect_equal(Gamma_C[3], (10 - mean(c(13,4,10)))/sd(c(13,4,10)))
  expect_equal(Gamma_C[4], (13 - mean(c(10,13,4)))/sd(c(10,13,4))) #Makes sure it takes 9 as most recent


})

test_that("compute_window correctly computes sur using unique_values = TRUE and min_non_na > 3", {

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
      matrix(c(8, 11, 4, NA,
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
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Gamma", FUN = "sur", only_unique = TRUE,
                                  min_non_na = 3)

  Gamma_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Gamma_sur_roll_3m)

  expect_true(is.na(Gamma_A[1]))  # No previous record for first date
  expect_equal(Gamma_A[2], NA_real_)
  expect_equal(Gamma_A[3], (13 - mean(c(NA,8)))/sd(c(NA,8))) #Only 2 non-NAs
  expect_equal(Gamma_A[4], (-3 - mean(c(13,8,-3)))/sd(c(13,8,-3))) #Makes sure it takes -3 as most recent



})

test_that("compute_window correctly computes cagr - real data", {

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
  features_m_df <- compute_window(panel, period = 2, signal = "ebit_12m", FUN = "cagr", only_unique = FALSE, feature_name = "ebit_12m_cagr_2m", min_non_na = 0)

  RRRP3 <- features_m_df@data %>%
    dplyr::filter(tickers == "RRRP3") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ebit_12m_cagr_2m)

  expect_true(is.na(RRRP3[1]))  # No previous record for first date
  expect_equal(RRRP3[2], NA_real_)
  expect_equal(RRRP3[3], cagr(205648000, 113912000, 2))

  AERI3 <- features_m_df@data %>%
    dplyr::filter(tickers == "AERI3") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ebit_12m_cagr_2m)

  expect_true(is.na(AERI3[1]))  # No previous record for first date
  expect_true(is.na(AERI3[2]))
  expect_equal(AERI3[3], cagr(254680000, 277859000, 2))



})

test_that("compute_window errors if the signal column is absent", {

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


  expect_error(
    compute_window(features_m_df, period = 1, signal = "NonExistingSignal", FUN = "cagr"),
    "The signal column does not exist in the data frame."
  )
})

test_that("compute_window errors if the window is wrong", {

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


  expect_error(
    compute_window(features_m_df, period = 1, signal = "Alpha", window = "organized", FUN = "cagr"),
    "Invalid window type. Must be either 'rolling' or 'seasonal'."
  )
})

test_that("compute_window errors if the period is neg", {

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


  expect_error(
    compute_window(features_m_df, period = -1, signal = "Alpha", FUN = "cagr"),
    "The period must be greater or equal to 0."
  )
})

test_that("compute_window throws an error when input is not numeric", {
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
    compute_window(features_m_df, period = 1, signal = "Alpha", FUN = "median"),
    "The signal column must be numeric."
  )
})

test_that("compute_window computes correct res_mom for period = 3 (Alpha)", {

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
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "res_mom",
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

test_that("compute_window computes correct idio_vol for period = 3 (Alpha)", {

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
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol",
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

test_that("compute_window computes correct idio_vol for period = 3 (Alpha) when there are holes in dates", {

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

  features_m_df@data <- features_m_df@data[-2,]

  bench_returns_m_xts <- create_meta_xts(
    xts::xts(data.frame(IBOV = c(-4,-2,4,5),
                        SMLL = c(1,5,10,5)),
             order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ))

  # Compute sd for "Alpha" with period = 3;
  features_m_df <- compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol",
                                  benchmark_returns_m_xts = bench_returns_m_xts, selected_bench = "IBOV")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_idio_vol_roll_3m)

  expect_equal(alpha_A[1], NA_real_)
  expect_equal(alpha_A[2], idio_vol(c(0,4), c(-4,-2)))
  expect_equal(alpha_A[3], idio_vol(c(9,4,0), c(5,4,-4)))


  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_idio_vol_roll_3m)

  expect_equal(alpha_B[1], NA_real_)
  expect_equal(alpha_B[2], idio_vol(c(3,1), c(-4,-2)))
  expect_equal(alpha_B[3], idio_vol(c(3,1,4), c(-4,-2,4)))
  expect_equal(alpha_B[4], idio_vol(c(3,1,4,9), c(-4,-2,4,5)))


})

test_that("compute_window throws an error when bench is wrong is different from features_m_df", {

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

  # Bench length
  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol",
                              benchmark_returns_m_xts = bench_returns_m_xts, selected_bench = "IBOV"),
               "The dates in 'benchmark_returns_m_xts' must match those in 'pre_silver_features_m_df")


  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol",
                              benchmark_returns_m_xts = bench_returns_m_xts),
               "The 'selected_bench' argument must be provided for FUN idio_vol")


  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol",
                              benchmark_returns_m_xts = NULL, selected_bench = "IBOV"),
               "benchmark_returns_m_xts must be provided for FUN idio_vol")

  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol",
                              benchmark_returns_m_xts = bench_returns_m_xts@data, selected_bench = "IBOV"),
               "benchmark_returns_m_xts must be provided for FUN idio_vol")


})

test_that("compute_window throws an error when only unique is used for wrong FUN", {

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

  # Compute
  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "cagr", only_unique = TRUE),
               "The 'only_unique' is not supported for FUN cagr")

})

test_that("compute_window throws an error when window seasonal is used for wrong FUN", {

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

  # Compute
  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "idio_vol", window = "seasonal", only_unique = FALSE),
               "The 'window' argument must be 'rolling' for FUN idio_vol")

})

test_that("compute_window throws an error for wrong FUN", {

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

  # Compute
  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "CAGR"),
               "Unsupported function type")

  # Compute
  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "count_if"),
               "The 'count_condition_fun' argument must be a function.")

  # Compute
  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "cagr", count_condition_fun = function(x) x > 2),
               "The 'count_condition_fun' argument is only supported for FUN 'count_if'.")

})

test_that("compute_window throws an error for wrong data type", {

  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(0, 3, 10, 3,
               1, "7", 4, 4,
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

  # Compute
  expect_error(compute_window(features_m_df, period = 3, signal = "Alpha", FUN = "cagr"),
               "The signal column must be numeric.")

  expect_error(compute_window(features_m_df, period = 3, min_non_na = -2, signal = "Beta", FUN = "cagr"),
               "The 'min_non_na' argument must be greater or equal to 0.")

  expect_error(compute_window(features_m_df, period = 3, min_non_na = 2, signal = "Beta", FUN = "cagr"),
               "The 'min_non_na' argument is not supported for FUN cagr")


  expect_error(compute_window(features_m_df, period = 3, window = "seasonal", signal = "Beta", FUN = "cagr",
                              offset_months = NULL),
               "The 'offset_months' argument must be provided when window is 'seasonal'.")

})

#meta_xts
test_that("compute_window computes correct median values for period = 1 (Alpha) in meta_xts", {

  # Create meta_xts
  metrics_m_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  # Compute median for "Alpha" with period = 1; new column name will be "Alpha_median_roll_1_m"
  metrics_m_xts <- compute_window(metrics_m_xts, period = 1, col_name = "A", FUN = "median", window = "rolling")

  # Extract computed values
  metric_values <- metrics_m_xts@data[, "A_median_roll_1m"] %>% as.numeric()

  expect_equal(metric_values[1], 0)  # No previous record for first date
  expect_equal(metric_values[2], median(c(1,0)))
  expect_equal(metric_values[3], median(c(2,1)))

})

test_that("compute_window computes correct sd values for period = 1 (A) in meta_xts and min_non_na = 2", {
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  metrics_xts <- compute_window(metrics_xts, period = 1, col_name = "A", FUN = "sd", min_non_na = 2)

  metric_values <- metrics_xts@data[, "A_sd_roll_1m"] %>% as.numeric()

  expect_true(is.na(metric_values[1]))  # No previous record for first date
  expect_equal(metric_values[2], sd(c(0, 1)))
  expect_equal(metric_values[3], sd(c(1, 2)))
})

test_that("compute_window computes correct CAGR values for period = 3 (A) in meta_xts", {
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 6, 4, -2
      ),
      nrow = 4, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  metrics_xts <- compute_window(metrics_xts, period = 3, col_name = "A", FUN = "cagr")

  metric_values <- metrics_xts@data[, "A_cagr_roll_3m"] %>% as.numeric()

  expect_true(is.na(metric_values[1]))  # No previous record for first date
  expect_true(is.na(metric_values[2]))
  expect_true(is.na(metric_values[3]))

  expect_equal(metric_values[4], cagr(2, 1, 3))
})

test_that("compute_window computes correct CAGR values for period = 3 (A) in meta_xts even when there are missing dates", {
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 6, 4, -2
      ),
      nrow = 4, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  metrics_xts@data <- metrics_xts@data[-2,]

  metrics_xts <- compute_window(metrics_xts, period = 2, col_name = "A", FUN = "cagr")

  metric_values <- metrics_xts@data[, "A_cagr_roll_2m"] %>% as.numeric()

  expect_true(is.na(metric_values[1]))  # No previous record for first date
  expect_equal(metric_values[2], cagr(2,2,2))
  expect_true(is.na(metric_values[3]))

})

test_that("compute_window computes correct sur values for period = 3 (A) in meta_xts and NAs", {
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, NA, 4,
               NA, 9, 9, 5,
               1, 6, 4, -2,
               9, -2, 0, 4,
               2, 5, 4, 9
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  metrics_xts <- compute_window(metrics_xts, period = 3, col_name = "A", FUN = "sur")

  metric_values <- metrics_xts@data[, "A_sur_roll_3m"] %>% as.numeric()

  expect_equal(metric_values[1], sur(2, numeric(0)))
  expect_equal(metric_values[2], sur(1, c(1,2)))
  expect_equal(metric_values[3], NA_real_)
  expect_equal(metric_values[4], sur(1, c(NA, 1, 2, 1)))
  expect_equal(metric_values[5], sur(9, c(1, NA, 1,9)))
  expect_equal(metric_values[6], sur(2, c(9, 1, NA,2)))

  expect_equal(zoo::index(metrics_xts@data) %>% as.character(), as.character(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15")))


})

test_that("compute_window computes correct sur values for period = 3 (A) in meta_xts and NAs, with min_non_na > 0", {
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, NA, 4,
               NA, 9, 9, 5,
               1, 6, 4, -2,
               9, -2, 0, 4,
               2, 5, 4, 9
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  metrics_xts <- compute_window(metrics_xts, period = 3, col_name = "A", FUN = "sur", min_non_na = 3)

  metric_values <- metrics_xts@data[, "A_sur_roll_3m"] %>% as.numeric()

  expect_equal(metric_values[1], NA_real_)
  expect_equal(metric_values[2], NA_real_)
  expect_equal(metric_values[3], NA_real_)
  expect_equal(metric_values[4], sur(1, c(1, NA, 1, 2)))
  expect_equal(metric_values[5], sur(9, c(1, NA, 1, 9)))
  expect_equal(metric_values[6], sur(2, c(9, 1, NA, 2)))

  expect_equal(zoo::index(metrics_xts@data) %>% as.character(), as.character(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15")))


})

test_that("compute_window computes correct idio_vol values for period = 3 (A) in meta_xts, with min_non_na > 0", {

  returns_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 0, 4,
               0, 9, 9, 5,
               1, 6, 4, -2,
               9, -2, 0, 4,
               2, 5, 4, 9
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "returns", asset_type = "stocks"
  )

  benchmark_ret_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 0, 4,
               0, 9, 9, 5
      ),
      nrow = 6, ncol = 2, byrow = TRUE,
      dimnames = list(NULL, c("ibov", "smll"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "returns", asset_type = "benchmarks"
  )

  expect_warning(
  returns_xts <- compute_window(returns_xts, period = 3, col_name = "A",
                                FUN = "idio_vol", benchmark_returns_m_xts = benchmark_ret_xts, selected_bench = "ibov",
                                min_non_na = 3, specific_dates = as.Date(c("2001-04-15", "2001-06-15", "2001-07-15"))),
  "There are NA values in the time series.")

  metric_values <- returns_xts@data[, "A_idio_vol_roll_3m"] %>% as.numeric()

  expect_equal(metric_values[1], NA_real_)
  expect_equal(metric_values[2], NA_real_)
  expect_equal(metric_values[3], NA_real_)
  expect_equal(metric_values[4], idio_vol(ret_values = c(1,0,1,2), bench_ret_values = c(0,1,10,2)))
  expect_equal(metric_values[5], idio_vol(ret_values = c(9,1,0,1), bench_ret_values = c(0,0,1,10)))
  expect_equal(metric_values[6], NA_real_)

  expect_equal(zoo::index(returns_xts@data) %>% as.character(), as.character(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15")))


})

test_that("compute_window computes correct mean_std values for period = 2 (A) in meta_xts", {

  dates <- seq.Date(as.Date("2001-03-15"), as.Date("2003-08-15"), by = "month")
  #Generate matrix
  set.seed(123)
  metrics_matrix <- matrix(rnorm(30), nrow = length(dates), ncol = 4, dimnames = list(NULL, c("A", "B", "C", "D")))

  metrics_xts <- create_meta_xts(
    xts::xts(metrics_matrix,
             order.by = dates),
    meta_xts_name = "test_xts", type = "metrics"
  )

  metrics_xts <- compute_window(metrics_xts, period = 24, col_name = "A", FUN = "mean_std", window = "rolling",
                                min_non_na = 23)

  metric_values <- metrics_xts@data[, "A_mean_std_roll_24m"] %>% as.numeric()

  expect_true(all(is.na(metric_values[1:22])))
  expect_equal(metric_values[23], mean_std(metrics_xts@data[c(1:23), "A"]))
  expect_equal(metric_values[28], mean_std(metrics_xts@data[c(4:28), "A"]))
  expect_equal(metric_values[30], mean_std(metrics_xts@data[c(6:30), "A"]))


})

test_that("compute_window computes correct skew values for period = 3 (A) in meta_xts and unique_values = TRUE", {
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               6, 6, 4, -2,
               6, 2, 1, 4,
               2, 3, 4, -3
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  metrics_xts <- compute_window(metrics_xts, period = 3, col_name = "A", FUN = "skew", only_unique = TRUE,
                                specific_dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15", "2001-08-15")))

  metric_values <- metrics_xts@data[, "A_skew_roll_3m"] %>% as.numeric()

  expect_equal(metric_values[1], skew(c(2)))
  expect_equal(metric_values[2], skew(c(2, 1)))
  expect_equal(metric_values[3], skew(c(2, 1)))
  expect_equal(metric_values[4], skew(c(6, 2, 1)))
  expect_equal(metric_values[5], NA_real_)
  expect_equal(metric_values[6], skew(c(6, 2)))


})

test_that("compute_window throws errors when metric col is wrong ", {
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               6, 6, 4, -2,
               6, 2, 1, 4,
               2, 3, 4, -3
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  expect_error(
    compute_window(metrics_xts, period = 3, col_name = "A2", FUN = "skew", only_unique = TRUE),
    "The col_name column does not exist in the xts object."
  )

  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, "3", 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               6, 6, 4, -2,
               6, 2, 1, 4,
               2, 3, 4, -3
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  expect_error(
    compute_window(metrics_xts, period = 3, col_name = "A", FUN = "skew", only_unique = TRUE),
    "The col_name column must be numeric."
  )


})

test_that("compute_window throws errors when FUN or period is wrong ", {
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               6, 6, 4, -2,
               6, 2, 1, 4,
               2, 3, 4, -3
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  expect_error(
    compute_window(metrics_xts, period = 3, col_name = "A", FUN = "cagr", only_unique = TRUE),
    "The 'only_unique' is not supported for FUN cagr"
  )

  expect_error(
    compute_window(metrics_xts, period = 3, col_name = "A", FUN = "emb", only_unique = TRUE),
    "Unsupported function type"
  )


  expect_error(
    compute_window(metrics_xts, period = -3, col_name = "A", FUN = "cagr", only_unique = FALSE)
  )

  expect_error(
    compute_window(metrics_xts, period = 3, col_name = "A", FUN = "cagr", window = "exp"),
    "The window argument must be rolling for meta_xts method"
  )

  returns_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               6, 6, 4, -2,
               6, 2, 1, 4,
               2, 3, 4, -3
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "returns"
  )

  expect_error(
    compute_window(returns_xts, period = 3, col_name = "A", FUN = "cagr"),
    "The FUN cagr is not supported for returns_meta_xts. Use metrics_meta_xts instead."
  )

  expect_error(
    compute_window(metrics_xts, period = 3, col_name = "A", FUN = "idio_vol", benchmark_returns_m_xts = returns_xts, selected_bench = "A"),
    "The FUN idio_vol is not supported for metrics_meta_xts. Use returns_meta_xts instead."
  )

  expect_error(
    compute_window(metrics_xts, period = 3, col_name = "A", FUN = "idio_vol", benchmark_returns_m_xts = NULL, selected_bench = "A"),
    "benchmark_returns_m_xts must be provided for FUN idio_vol"
  )




})

test_that("compute_window throws errors when benchmark_m_xts does not match", {

  returns_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 0, 4,
               0, 9, 9, 5,
               1, 6, 4, -2,
               9, -2, 0, 4,
               2, 5, 4, 9
      ),
      nrow = 6, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15", "2001-07-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "returns", asset_type = "stocks"
  )

  benchmark_ret_xts <- create_meta_xts(
    xts::xts(
      matrix(c(2, 3, 10, 3,
               1, 7, 0, 4
      ),
      nrow = 4, ncol = 2, byrow = TRUE,
      dimnames = list(NULL, c("ibov", "smll"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",  "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "returns", asset_type = "benchmarks"
  )

  expect_error(
    returns_xts <- compute_window(returns_xts, period = 3, col_name = "A",
                                  FUN = "idio_vol", benchmark_returns_m_xts = benchmark_ret_xts, selected_bench = "ibov",
                                  min_non_na = 3),
    "The dates in pre_silver_m_xts do not match the dates in benchmark_returns_m_xts.")

})







