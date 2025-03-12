test_that("compute_sector_map_across works for single mapping", {

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

  # metrics meta xts
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 5, 6, 4),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #mapper
  mapper <- list(
    Agro = ~ -A,
    Utilities = ~ B
  )

  meta_df <- compute_sector_map_across(features_m_df, metrics_xts, "sector", mapper)

  #Check if sector_mapped is correct
  A <- meta_df@data %>% dplyr::filter(tickers == "Stock A") %>% dplyr::pull(sector_mapped)
  expect_equal(A, c(0, -1, -2, -1))

  B <- meta_df@data %>% dplyr::filter(tickers == "Stock B") %>% dplyr::pull(sector_mapped)
  expect_equal(B, c(3, 7, 9, 5))

  C <- meta_df@data %>% dplyr::filter(tickers == "Stock C") %>% dplyr::pull(sector_mapped)
  expect_equal(C, c(0, -1, -2, -1))

})

test_that("compute_sector_map_across works for arithmetic mapping", {

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

  # metrics meta xts
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 5, 6, 4),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #mapper
  mapper <- list(
    Agro = ~ -A + B,
    Utilities = ~ -B/A
  )

  meta_df <- compute_sector_map_across(features_m_df, metrics_xts, "sector", mapper)

  #Check if sector_mapped is correct
  A <- meta_df@data %>% dplyr::filter(tickers == "Stock A") %>% dplyr::pull(sector_mapped)
  expect_equal(A, c(-0 + 3, -1 + 7, -2 + 9, -1 + 5))

  B <- meta_df@data %>% dplyr::filter(tickers == "Stock B") %>% dplyr::pull(sector_mapped)
  expect_equal(B, c(-3/0, -7/1, -9/2, -5))

  C <- meta_df@data %>% dplyr::filter(tickers == "Stock C") %>% dplyr::pull(sector_mapped)
  expect_equal(C, c(-0 + 3, -1 + 7, -2 + 9, -1 + 5))

})

test_that("compute_sector_map_across works for more complex mapping", {

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

  # metrics meta xts
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 5, 6, 4),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #mapper
  mapper <- list(
    Agro = ~ -sqrt(A) + log(B),
    Utilities = ~ -exp(B)/(A)^2
  )

  meta_df <- compute_sector_map_across(features_m_df, metrics_xts, "sector", mapper, feature_name = "mapped_comm")

  #Check if sector_mapped is correct
  A <- meta_df@data %>% dplyr::filter(tickers == "Stock A") %>% dplyr::pull(mapped_comm)
  expect_equal(A, c(-sqrt(0) + log(3), -sqrt(1) + log(7), -sqrt(2) + log(9), -sqrt(1) + log(5)))

  B <- meta_df@data %>% dplyr::filter(tickers == "Stock B") %>% dplyr::pull(mapped_comm)
  expect_equal(B, c(-exp(3)/0, -exp(7)/(1^2), -exp(9)/(2^2), -exp(5)/(1^2)))

  C <- meta_df@data %>% dplyr::filter(tickers == "Stock C") %>% dplyr::pull(mapped_comm)
  expect_equal(C, c(-sqrt(0) + log(3), -sqrt(1) + log(7), -sqrt(2) + log(9), -sqrt(1) + log(5)))

})

test_that("compute_sector_map_across throws error for NAs in sectors", {

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

  # metrics meta xts
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 5, 6, 4),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #mapper
  mapper <- list(
    Agro = ~ -A + B,
    Utilities = ~ -B/A
  )

  expect_error(
    compute_sector_map_across(features_m_df, metrics_xts, "sector", mapper),
    "The sector column contains NAs.")

})

test_that("compute_sector_map_across throws error for wrong objs", {

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

  # metrics meta xts
  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 5, 6, 4),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #mapper
  mapper <- list(
    Agro = ~ -A + B,
    Utilities = ~ -B/A
  )

  expect_error(
    compute_sector_map_across(features_m_df, metrics_xts, "subsector", mapper),
    "The specified sector column does not exist in the meta_dataframe.")


  wrong_metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, NA, 9, 5,
               1, 5, 6, 4),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )


  expect_error(
    compute_sector_map_across(features_m_df, wrong_metrics_xts, "sector", mapper),
    "The meta_xts contains NAs.")

  wrong_mapper <- list(
    Agro = "A",
    Utilities = "B"
  )

  expect_error(
    compute_sector_map_across(features_m_df, metrics_xts, "sector", wrong_mapper),
    "The mapper object must be a list of formulas.")


  wrong_mapper <- list(
    Petro ~ C,
    Utilities ~ B
  )

  expect_error(
    compute_sector_map_across(features_m_df, metrics_xts, "sector", wrong_mapper),
    "The following sectors in meta_dataframe are missing in the mapper: Agro, Utilities")


  wrong_metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 4, 9, 5,
               1, 5, 6, 4),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-07-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )


  expect_error(
    compute_sector_map_across(features_m_df, wrong_metrics_xts, "sector", mapper),
    "Dates in meta_dataframe and meta_xts do not match.")

  wrong_features_m_df <- features_m_df
  wrong_features_m_df@current_date <- as.Date("2001-09-15")


  expect_error(
    compute_sector_map_across(wrong_features_m_df, metrics_xts, "sector", mapper),
    "Current dates do not match between meta_dataframe and meta_xts.")

  wrong_mapper <- list(
    Agro ~ E,
    Utilities ~ B
  )


  expect_error(
    compute_sector_map_across(features_m_df, metrics_xts, "sector", wrong_mapper),
    "The following sectors in meta_dataframe are missing in the mapper: Agro, Utilities")


  wrong_mapper <- list(
    Agro = ~ E,
    Utilities = ~ B
  )

  expect_error(
    compute_sector_map_across(features_m_df, metrics_xts, "sector", wrong_mapper),
    "One or more variables in formula '~E' are missing in metrics_xts at date 2001-03-15")

  mixed_mapper <- list(
    Agro = ~ A + B,
    Utilities = "B"  # Incorrect
  )


  expect_error(
    compute_sector_map_across(features_m_df, metrics_xts, "sector", mixed_mapper),
    "The mapper object must be a list of formulas."
  )

})

