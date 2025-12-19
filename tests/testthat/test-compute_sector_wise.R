test_that("compute_sector_wise works for median", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  features_m_df <- compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha", FUN = "median")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_median)

  expect_equal(alpha_A[1], median(c(0,10)))  # No previous record for first date
  expect_equal(alpha_A[2], median(c(3,7)))
  expect_equal(alpha_A[3], median(c(4,2)))
  expect_equal(alpha_A[4], median(c(9,5)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_median)

  expect_equal(alpha_B[1], 3)  # No previous record for first date
  expect_equal(alpha_B[2], 1)
  expect_equal(alpha_B[3], 4)
  expect_equal(alpha_B[4], 9)

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_median)

  expect_equal(alpha_C[1], median(c(0,10)))  # No previous record for first date
  expect_equal(alpha_C[2], median(c(3,7)))
  expect_equal(alpha_C[3], median(c(4,2)))
  expect_equal(alpha_C[4], median(c(9,5)))


})

test_that("compute_sector_wise works for mean", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  features_m_df <- compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha", FUN = "mean")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_A[1], mean(c(0,10)))  # No previous record for first date
  expect_equal(alpha_A[2], mean(c(3,7)))
  expect_equal(alpha_A[3], mean(c(4,2)))
  expect_equal(alpha_A[4], mean(c(9,5)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_B[1], 3)  # No previous record for first date
  expect_equal(alpha_B[2], 1)
  expect_equal(alpha_B[3], 4)
  expect_equal(alpha_B[4], 9)

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_C[1], mean(c(0,10)))  # No previous record for first date
  expect_equal(alpha_C[2], mean(c(3,7)))
  expect_equal(alpha_C[3], mean(c(4,2)))
  expect_equal(alpha_C[4], mean(c(9,5)))


})

test_that("compute_sector_wise works for sd", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  features_m_df <- compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha", FUN = "sd")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_sd)

  expect_equal(alpha_A[1], sd(c(0,10)))  # No previous record for first date
  expect_equal(alpha_A[2], sd(c(3,7)))
  expect_equal(alpha_A[3], sd(c(4,2)))
  expect_equal(alpha_A[4], sd(c(9,5)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_sd)

  expect_equal(alpha_B[1], NA_real_)  # No previous record for first date
  expect_equal(alpha_B[2], NA_real_)
  expect_equal(alpha_B[3], NA_real_)
  expect_equal(alpha_B[4], NA_real_)

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_sd)

  expect_equal(alpha_C[1], sd(c(0,10)))  # No previous record for first date
  expect_equal(alpha_C[2], sd(c(3,7)))
  expect_equal(alpha_C[3], sd(c(4,2)))
  expect_equal(alpha_C[4], sd(c(9,5)))


})

test_that("compute_sector_wise works for signal_to_noise", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  features_m_df <- compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha", FUN = "signal_to_noise")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_signal_to_noise)

  expect_equal(alpha_A[1], signal_to_noise(c(0,10)))  # No previous record for first date
  expect_equal(alpha_A[2], signal_to_noise(c(3,7)))
  expect_equal(alpha_A[3], signal_to_noise(c(4,2)))
  expect_equal(alpha_A[4], signal_to_noise(c(9,5)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_signal_to_noise)

  expect_equal(alpha_B[1], NA_real_)  # No previous record for first date
  expect_equal(alpha_B[2], NA_real_)
  expect_equal(alpha_B[3], NA_real_)
  expect_equal(alpha_B[4], NA_real_)

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_signal_to_noise)

  expect_equal(alpha_C[1], signal_to_noise(c(0,10)))  # No previous record for first date
  expect_equal(alpha_C[2], signal_to_noise(c(3,7)))
  expect_equal(alpha_C[3], signal_to_noise(c(4,2)))
  expect_equal(alpha_C[4], signal_to_noise(c(9,5)))


})

test_that("compute_sector_wise handles NAs in signal", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, NA, 10, NA,
               1, NA, 4, NA,
               2, NA, NA, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, NA, 4, 7,
               6, -3, -2, NA),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  features_m_df <- compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha", FUN = "median")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_median)

  expect_equal(alpha_A[1], median(c(0,10)))  # No previous record for first date
  expect_equal(alpha_A[2], NA_real_)
  expect_equal(alpha_A[3], median(c(4,2)))
  expect_equal(alpha_A[4], median(c(NA,5), na.rm = TRUE))



})

test_that("compute_sector_wise works for min_non_NA above 0", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  features_m_df <- compute_sector_wise(features_m_df, sector_column = "sector",
                                       signal = "Alpha", FUN = "mean", min_non_na = 2)

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_A[1], mean(c(0,10)))  # No previous record for first date
  expect_equal(alpha_A[2], mean(c(3,7)))
  expect_equal(alpha_A[3], mean(c(4,2)))
  expect_equal(alpha_A[4], mean(c(9,5)))

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_B[1], NA_real_)  # No previous record for first date
  expect_equal(alpha_B[2], NA_real_)
  expect_equal(alpha_B[3], NA_real_)
  expect_equal(alpha_B[4], NA_real_)

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_C[1], mean(c(0,10)))  # No previous record for first date
  expect_equal(alpha_C[2], mean(c(3,7)))
  expect_equal(alpha_C[3], mean(c(4,2)))
  expect_equal(alpha_C[4], mean(c(9,5)))

  # Compute median
  features_m_df <- compute_sector_wise(features_m_df, sector_column = "sector",
                                       signal = "Alpha", FUN = "mean", min_non_na = 3)

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_A[1], NA_real_)  # No previous record for first date
  expect_equal(alpha_A[2], NA_real_)
  expect_equal(alpha_A[3], NA_real_)
  expect_equal(alpha_A[4], NA_real_)

  # For Stock B:
  alpha_B <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_B[1], NA_real_)  # No previous record for first date
  expect_equal(alpha_B[2], NA_real_)
  expect_equal(alpha_B[3], NA_real_)
  expect_equal(alpha_B[4], NA_real_)

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(Alpha_sector_mean)

  expect_equal(alpha_C[1], NA_real_)  # No previous record for first date
  expect_equal(alpha_C[2], NA_real_)
  expect_equal(alpha_C[3], NA_real_)
  expect_equal(alpha_C[4], NA_real_)


})

test_that("compute_sector_wise fails for NAs in sector", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, NA, 10, NA,
               1, NA, 4, NA,
               2, NA, NA, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, NA, 4, 7,
               6, -3, -2, NA),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c(NA, "Utilities", NA,
               NA, "Utilities", NA,
               NA, "Utilities", NA,
               NA, "Utilities", NA),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  expect_error(
    compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha", FUN = "median"),
    "The sector column contains NAs."
    )


})

test_that("compute_sector_wise fails for wrong col specification", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  expect_error(
    compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha2", FUN = "mean", min_non_na = 2),
    "The signal column does not exist in the data frame."
  )

  # Compute median
  expect_error(
    compute_sector_wise(features_m_df, sector_column = "sector2", signal = "Alpha", FUN = "mean", min_non_na = 2),
    "The sector column does not exist in the data frame."
  )



})

test_that("compute_sector_wise fails for wrong col type", {
  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, "3", 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, 5, 6,
               5, 2, 4, 7,
               6, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Sector" matrix
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "sector")
  )

  # Compute median
  expect_error(
    compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha", FUN = "mean", min_non_na = 2),
    "The signal column must be numeric."
  )

  # Compute median
  expect_error(
    compute_sector_wise(features_m_df, sector_column = "Beta", signal = "Gamma", FUN = "mean", min_non_na = 2),
    "The sector_column column must be character."
  )




})

test_that("compute_sector_wise correctly computes median - real data", {

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

  #Adjust NAs in sector
  panel@data$sector_c1[which(is.na(panel@data$sector_c1))] <- "Unknown"


  #Compute sur
  features_m_df <- compute_sector_wise(panel, signal = "ir_3m", FUN = "mean", feature_name = "ir_3m_sector",
                                       sector_column = "sector_c1")

  ABCB4 <- features_m_df@data %>%
    dplyr::filter(tickers == "ABCB4") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ir_3m_sector)

  expect_equal(ABCB4, features_m_df@data %>% dplyr::filter(tickers == "ABCB4") %>% dplyr::pull(ir_3m))

  ABCB11 <- features_m_df@data %>%
    dplyr::filter(tickers == "ABCB11") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ir_3m_sector)

  expect_equal(ABCB11, features_m_df@data %>% dplyr::filter(tickers == "ABCB4") %>% dplyr::pull(ir_3m))

  ABCB3 <- features_m_df@data %>%
    dplyr::filter(tickers == "ABCB3") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ir_3m_sector)

  expect_equal(ABCB3, features_m_df@data %>% dplyr::filter(tickers == "ABCB4") %>% dplyr::pull(ir_3m))


})

test_that("compute_sector_wise fails for unsupported FUN", {
  features_m_df <- create_meta_dataframe(
    list(
      matrix(c(1, 2, 3, 4,
               2, 3, 4, 5,
               3, 4, 5, 6),
             nrow = 3, ncol = 4),
      matrix(c("Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro",
               "Agro", "Utilities", "Agro"),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "sector")
  )

  expect_error(
    compute_sector_wise(features_m_df, sector_column = "sector", signal = "Alpha", FUN = "wrong_fun"),
    "Unsupported function type"
  )
})
