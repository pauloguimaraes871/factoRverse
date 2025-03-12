test_that("compute_sector_map works for simple operations", {

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

  mapper <- list(
    Agro = ~ -Alpha,
    Utilities = ~ Beta + Alpha
  )

  feature_name <- c("new_var")

  # Call function
  features_m_df <- compute_sector_map(features_m_df, sector_column = "sector", feature_name = feature_name, mapper = mapper)


  #Expected behavior
  A <- features_m_df@data %>% dplyr::filter(tickers == "Stock A") %>% dplyr::pull(new_var)
  expect_equal(A, c(0, NA, -4, NA))

  B <- features_m_df@data %>% dplyr::filter(tickers == "Stock B") %>% dplyr::pull(new_var)
  expect_equal(B, c(NA, 6, NA, NA))

  C <- features_m_df@data %>% dplyr::filter(tickers == "Stock C") %>% dplyr::pull(new_var)
  expect_equal(C, c(-10, NA, -2, -5))

})

test_that("compute_sector_map works for more complex operations", {

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

  mapper <- list(
    Agro = ~ -sqrt(Alpha),
    Utilities = ~ Beta^2 + log(Alpha)
  )


  # Call function
  features_m_df <- compute_sector_map(features_m_df, sector_column = "sector", mapper = mapper)


  #Expected behavior
  A <- features_m_df@data %>% dplyr::filter(tickers == "Stock A") %>% dplyr::pull(sector_mapped)
  expect_equal(A, c(-sqrt(0), NA, -sqrt(4), NA))

  B <- features_m_df@data %>% dplyr::filter(tickers == "Stock B") %>% dplyr::pull(sector_mapped)
  expect_equal(B, c(NA, 5^2 + log(1), NA, NA))

  C <- features_m_df@data %>% dplyr::filter(tickers == "Stock C") %>% dplyr::pull(sector_mapped)
  expect_equal(C, c(-sqrt(10), NA, -sqrt(2), -sqrt(5)))

})

test_that("compute_sector_map works for recreating variables", {

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

  mapper <- list(
    Agro = ~ -sqrt(Alpha),
    Utilities = ~ Beta^2 + log(Alpha)
  )


  # Call function
  features_m_df <- compute_sector_map(features_m_df, sector_column = "sector", mapper = mapper, feature_name = "Alpha")


  #Expected behavior
  A <- features_m_df@data %>% dplyr::filter(tickers == "Stock A") %>% dplyr::pull(Alpha)
  expect_equal(A, c(-sqrt(0), NA, -sqrt(4), NA))

  B <- features_m_df@data %>% dplyr::filter(tickers == "Stock B") %>% dplyr::pull(Alpha)
  expect_equal(B, c(NA, 5^2 + log(1), NA, NA))

  C <- features_m_df@data %>% dplyr::filter(tickers == "Stock C") %>% dplyr::pull(Alpha)
  expect_equal(C, c(-sqrt(10), NA, -sqrt(2), -sqrt(5)))

})

test_that("compute_sector_map works for real data", {

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

  #Correct NA in sector_c1
  panel@data$sector_c1[which(is.na(panel@data$sector_c1))] <- "others"

  mapper <- list(
    `Bancos e Serviços Financeiros` = ~ sharpe*(1-0.34),
    `Consumo não Cíclico` = ~ sharpe,
    `Indústria` = ~ sharpe,
    `Consumo Não-Cíclico` = ~ sharpe,
    others = ~ sharpe,
    `Petróleo gás e biocombustíveis` = ~ sharpe,
    Agro = ~ sharpe,
    `Utilidade Pública` = ~ sharpe
  )


  # Call function
  features_m_df <- compute_sector_map(panel, sector_column = "sector_c1", mapper = mapper, feature_name = "sharpe_adj")

  #For banks
  expect_equal(
    features_m_df@data %>% dplyr::filter(sector_c1 == "Bancos e Serviços Financeiros") %>% dplyr::pull(sharpe_adj),
    panel@data %>% dplyr::filter(sector_c1 == "Bancos e Serviços Financeiros") %>% dplyr::pull(sharpe)*(1-0.34)
  )

  #For others
  expect_equal(
    features_m_df@data %>% dplyr::filter(!sector_c1 == "Bancos e Serviços Financeiros") %>% dplyr::pull(sharpe_adj),
    panel@data %>% dplyr::filter(!sector_c1 == "Bancos e Serviços Financeiros") %>% dplyr::pull(sharpe)
  )



})

test_that("compute_sector_map throws error for NAs in sectors", {

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

  #mapper
  mapper <- list(
    Agro = ~ -Alpha + Beta,
    Utilities = ~ -Beta/Alpha
  )

  expect_error(
    compute_sector_map(features_m_df, "sector", mapper),
    "The sector column contains NAs.")

})

test_that("compute_sector_map throws error for wrong objs", {

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

  #mapper
  mapper <- list(
    Agro = ~ -Alpha + Beta,
    Utilities = ~ -Beta/Alpha
  )

  expect_error(
    compute_sector_map(features_m_df, "sector2", mapper),
    "The specified sector column does not exist in the meta_dataframe.")


  #mapper
  wrong_mapper <- list(
    Agro = "Alpha",
    Utilities = "Beta"
  )

  expect_error(
    compute_sector_map(features_m_df, "sector", wrong_mapper),
    "The mapper object must be a list of formulas.")


  #mapper
  wrong_mapper <- list(
    Utilities = ~ -Beta/Alpha
  )

  expect_error(
    compute_sector_map(features_m_df, "sector", wrong_mapper),
    "The following sectors in meta_dataframe are missing in the mapper: Agro")

  wrong_mapper <- list(
    Agro = ~ Zeta,
    Utilities = ~ -Beta/Alpha
  )

  expect_error(
    compute_sector_map(features_m_df, "sector", wrong_mapper),
    "One or more variables in formula '~Zeta' are missing in meta_dataframe at date 2001-03-15")



})
