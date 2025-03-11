test_that("compute_formula works for simple operations and ignore_na NULL", {

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

  # Compute 'sum'
  features_m_df <- compute_formula(features_m_df, formula = "Alpha + Beta", ignore_NA = NULL, feature_name = "albeta")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_A, c(4, 9, 8, 6))

  # Compute 'subtraction'
  features_m_df <- compute_formula(features_m_df, formula = "Alpha-Beta", ignore_NA = NULL, feature_name = "alfata")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(alfata)

  expect_equal(alpha_A, c(-4, -3, 0, 12))

  # Compute ratio
  features_m_df <- compute_formula(features_m_df, formula = "Alpha/Gamma", ignore_NA = NULL, feature_name = "alfamma")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(alfamma)

  expect_equal(alpha_A, c(0, 3/11, 1, -3))

  # Compute 'product'
  features_m_df <- compute_formula(features_m_df, formula = "Alpha*Beta", ignore_NA = NULL, feature_name = "albeta")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_A, c(0, 18, 16, -27))

  #Compute log
  features_m_df <- compute_formula(features_m_df, formula = "log(Alpha)", ignore_NA = NULL, feature_name = "albeta")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_A, c(log(0), log(3), log(4), log(9)))

  #Compute sqrt
  features_m_df <- compute_formula(features_m_df, formula = "sqrt(Alpha)", ignore_NA = NULL, feature_name = "albeta")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_A, c(sqrt(0), sqrt(3), sqrt(4), sqrt(9)))

  #Compute exp
  features_m_df <- compute_formula(features_m_df, formula = "exp(Alpha)", ignore_NA = NULL, feature_name = "albeta")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_A, c(exp(0), exp(3), exp(4), exp(9)))

  #Compute exponential
  features_m_df <- compute_formula(features_m_df, formula = "Alpha^2", ignore_NA = NULL, feature_name = "albeta")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_A, c(0, 9, 16, 81))

  #Compute exponential
  features_m_df <- compute_formula(features_m_df, formula = "1/Alpha", ignore_NA = NULL, feature_name = "al")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(al)

  expect_equal(alpha_A, c(1/0, 1/3, 1/4, 1/9))

})

test_that("compute_formula works for simple operations and ignore_na not NULL", {

  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, NA, 10, 3,
               1, NA, 4, 4,
               2, 9, 9, 2),
             nrow = 3, ncol = 4),
      # "Beta" matrix
      matrix(c(4, 7, NA, 6,
               5, 1, 4, 7,
               NA, -3, -2, 8),
             nrow = 3, ncol = 4),
      # "Gamma" matrix
      matrix(c(8, 11, 4, 11,
               9, -2, 4, 12,
               10, -3, 2, 13),
             nrow = 3, ncol = 4),
      # "Delta" matrix
      matrix(c(3, 8, 5, 9,
               7, -1, -2, 8,
               9, 0, 0, 7),
             nrow = 3, ncol = 4)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15")),
    features_names = c("Alpha", "Beta", "Gamma", "Delta")
  )

  # Compute 'sum'
  features_m_df <- compute_formula(features_m_df, formula = "Alpha + Beta", ignore_NA = "Beta", feature_name = "albeta")

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_C, c(10, NA, 2, 10))

  # Compute 'subtract'
  features_m_df <- compute_formula(features_m_df, formula = "Alpha - Beta", ignore_NA = "Beta", feature_name = "albeta")

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_C, c(10, NA, 2, -6))

  # Compute 'ratio'
  features_m_df <- compute_formula(features_m_df, formula = "Alpha/Beta", ignore_NA = "Beta", feature_name = "albeta")

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_C, c(10, NA, 2, 2/8))

  # Compute 'product'
  features_m_df <- compute_formula(features_m_df, formula = "Alpha*Beta", ignore_NA = "Beta", feature_name = "albeta")

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_C, c(10, NA, 2, 2*8))


  # Compute 'product' when two are NA
  features_m_df@data$Gamma[12] <- NA
  features_m_df <- compute_formula(features_m_df, formula = "Alpha*Beta*Gamma", ignore_NA = c("Alpha", "Beta"), feature_name = "albeta")

  # For Stock C:
  alpha_C <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_C, c(10*4, -2, 20, NA))


})

test_that("compute_formula works for more complex operations and ignore_na NULL", {

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

  # (Alpha + Beta)/Beta
  features_m_df <- compute_formula(features_m_df, formula = "(Alpha + Beta)/Beta", ignore_NA = NULL, feature_name = "albeta")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_A, c(1, 9/6, 2, -2))

  # Alpha + Beta/Beta
  features_m_df <- compute_formula(features_m_df, formula = "Alpha+Beta/Beta", ignore_NA = NULL, feature_name = "alfata")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(alfata)

  expect_equal(alpha_A, c(1, 4, 5, 10))

  # Alpha*Gamma + Beta/(Gamma*Delta)
  features_m_df <- compute_formula(features_m_df, formula = "Alpha*Gamma + Beta/(Gamma*Delta)", ignore_NA = NULL, feature_name = "alfamma")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(alfamma)

  expect_equal(alpha_A, c(0 + 4/(8*3), (3*11) + 6/(11*9), 4*4 + 4/(4*-2), 9*(-3) + -3/(-3*0)))

  # (log(Alpha) + sqrt(Beta))/(Gamma + Delta)
  expect_warning(
  expect_warning(
  features_m_df <- compute_formula(features_m_df, formula = "(log(Alpha) + sqrt(Beta))/(Gamma + Delta)", ignore_NA = NULL, feature_name = "albraba")
  )
  )

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albraba)

  suppressWarnings(
  expect_equal(alpha_A, c(
    (log(0) + sqrt(4))/(8+3),   (log(3) + sqrt(6))/(11+9),   (log(4) + sqrt(4))/(4-2),   (log(9) + sqrt(-3))/(-3+0))
    )
  )

  #Compute log
  features_m_df <- compute_formula(features_m_df, formula = "log(Alpha) + Beta^3", ignore_NA = NULL, feature_name = "ronaldo")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ronaldo)

  expect_equal(alpha_A, c(log(0) + 4^3, log(3) + 6^3, log(4) + 4^3, log(9) + (-3)^3))


})

test_that("compute_formula works for multiple NAs", {

  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(NA, NA, NA, NA,
               NA, NA, NA, NA,
               NA, NA, NA, 4),
             nrow = 3, ncol = 4),
      # "Beta" matrix (not used in this test)
      matrix(c(NA, NA, NA, NA,
               NA, 1, NA, NA,
               NA, NA, NA, NA),
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

  # (Alpha + Beta)/Beta
  features_m_df <- compute_formula(features_m_df, formula = "Alpha + Beta", ignore_NA = NULL, feature_name = "albeta")

  # For Stock A:
  alpha_A <- features_m_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(albeta)

  expect_equal(alpha_A, c(NA_real_,NA_real_,NA_real_,NA_real_))


})

test_that("compute_formula works throws an error when trying complex ops and ignore_NA not NULL", {

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

  #Mult/Div
  expect_error(
    compute_formula(features_m_df, formula = "(Alpha + Beta)/Beta", ignore_NA = "Beta", feature_name = "albeta"),
    "When ignore_NA is specified, only basic arithmethic operations that do not mix addition/subtraction with multiplication/division are allowed."
  )

  #sqrt
  expect_error(
    compute_formula(features_m_df, formula = "sqrt(Alpha)", ignore_NA = "Beta", feature_name = "albeta"),
    "When ignore_NA is specified, only basic arithmethic operations that do not mix addition/subtraction with multiplication/division are allowed."
  )

})

test_that("compute_formula works throws an error when columns are missing", {

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

  #Mult/Div
  expect_error(
    compute_formula(features_m_df, formula = "(Alpha + Iota)/Beta", ignore_NA = NULL, feature_name = "albeta"),
    "The following columns are missing in the data: Iota"
  )

  #Mult/Div
  expect_error(
    compute_formula(features_m_df, formula = "Alpha/Beta", ignore_NA = "Iota", feature_name = "albeta"),
    "The following columns are missing in the data: Iota"
  )

})

test_that("compute_formula works throws an error when ignore_NA matches all vars in formula", {

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

  #Mult/Div
  expect_error(
    compute_formula(features_m_df, formula = "Alpha/Beta", ignore_NA = c("Alpha", "Beta"), feature_name = "albeta"),
    "The ignore_NA columns and formula columns are the same."
  )


})

test_that("compute_formula works throws an error when formula is wrong", {

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

  #Mult/Div
  expect_error(
    compute_formula(features_m_df, formula = "(Alpha/Beta", feature_name = "albeta")
    )


})

test_that("compute_formula fails for character arg", {

  # Create meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # "Alpha" matrix:
      matrix(c(0, "3", 10, 3,
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

  # Invalid formula case: Using an undefined variable
  expect_error(
    compute_formula(features_m_df, formula = "Alpha + Beta", feature_name = "invalid_feature"),
  )


})

