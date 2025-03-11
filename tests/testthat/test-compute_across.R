test_that("compute_across works for basic operations", {

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

  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 3, 4, 2
               ),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #Compute product
  meta_df <- compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B", FUN = "product")

  #For Stock A
  A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::pull(Alpha_across_B_product)

  expect_equal(A, c(3*3, 3*7, 4*9, 5*3))

  #For Stock B
  B <- meta_df@data %>%
    dplyr::filter(tickers == "Stock B") %>%
    dplyr::pull(Alpha_across_B_product)

  expect_equal(B, c(3*3, 1*7, 4*9, 5*3))

  #For Stock C
  C <- meta_df@data %>%
    dplyr::filter(tickers == "Stock C") %>%
    dplyr::pull(Alpha_across_B_product)

  expect_equal(C, c(9*3, -2*7, 2*9, 5*3))

  #Compute sum
  meta_df <- compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B", FUN = "sum")

  #For Stock A
  A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::pull(Alpha_across_B_sum)

  expect_equal(A, c(3+3, 3+7, 4+9, 5+3))

  #Compute sub
  meta_df <- compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B", FUN = "subtract")

  #For Stock A
  A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::pull(Alpha_across_B_subtract)

  expect_equal(A, c(3-3, 3-7, 4-9, 5-3))

  #Compute ratio
  meta_df <- compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B", FUN = "ratio")

  #For Stock A
  A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::pull(Alpha_across_B_ratio)

  expect_equal(A, c(3/3, 3/7, 4/9, 5/3))


})

test_that("compute_across works for basic operations with NAs", {

  # Create meta_dataframe
  meta_df <- create_meta_dataframe(
    list(
      matrix(c(3, NA, 9, NA,    # Column 1: Stock A (dates in order: 2001-03-15, 2001-04-15, 2001-05-15, 2001-06-15)
               1, -2, 4, 4,     # Column 2: Stock B
               2, 5, NA, 5),    # Column 3: Stock C
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

  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, NA, 10, 3,
               1, 3, 4, 4,
               2, NA, 9, 5,
               1, 4, 4, 2
      ),
      nrow = 4, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #Compute product
  meta_df <- compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B", FUN = "product")

  #For Stock A
  A <- meta_df@data %>%
    dplyr::filter(tickers == "Stock A") %>%
    dplyr::pull(Alpha_across_B_product)

  expect_equal(A, c(3*NA, NA*7, 4*NA, 5*4))



})

test_that("compute_across correctly computes real data", {

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

  metrics_xts <- xts::xts(
    data.frame(ipca = c(1,2,3)),
  order.by = as.Date(c("2023-07-15", "2023-08-15", "2023-09-15"))) %>% create_meta_xts(type = "metrics")

  #Compute
  features_m_df <- compute_across(panel, metrics_xts, signal = "ir_3m", FUN = "product", metric = "ipca", feature_name = "ir_3m_ipca")

  RRRP3 <- features_m_df@data %>%
    dplyr::filter(tickers == "RRRP3") %>%
    dplyr::arrange(dates) %>%
    dplyr::pull(ir_3m_ipca)

  expect_equal(RRRP3, c(-0.4302905*1, -0.6096270*2, 0.6918965*3), tolerance = 1e-7)


})

test_that("compute_across throws an error for wrong signal", {

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

  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 3, 4, 2
      ),
      nrow = 4, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #Compute product
  expect_error(
    compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha2", metric = "B", FUN = "product"),
    "The specified signal does not exist in the meta_dataframe."
  )

  expect_error(
    compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B2", FUN = "product"),
    "The specified metric does not exist in the meta_xts."
  )




})

test_that("compute_across throws an error for wrong objects", {

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

  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 3, 4, 2
      ),
      nrow = 4, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-08-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #Compute product
  expect_error(
    compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B", FUN = "product"),
    "Current dates do not match between meta_dataframe and meta_xts."
  )


  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 3, 4, 2
      ),
      nrow = 4, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-02-15", "2001-03-15", "2001-04-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  expect_error(
    compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B", FUN = "product"),
    "Dates in meta_dataframe and meta_xts do not match."
  )




})

test_that("compute_across throws an error for wrong FUN", {

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

  metrics_xts <- create_meta_xts(
    xts::xts(
      matrix(c(0, 3, 10, 3,
               1, 7, 4, 4,
               2, 9, 9, 5,
               1, 3, 4, 2
      ),
      nrow = 4, ncol = 4, byrow = TRUE,
      dimnames = list(NULL, c("A", "B", "C", "D"))),
      order.by = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))
    ),
    meta_xts_name = "test_xts", type = "metrics"
  )

  #Compute product
  expect_error(
    compute_across(meta_dataframe = meta_df, meta_xts = metrics_xts, signal = "Alpha", metric = "B", FUN = "prod"),
    "Invalid FUN specified. Must be one of: 'product', 'ratio', 'subtract', 'sum'."
  )

})
