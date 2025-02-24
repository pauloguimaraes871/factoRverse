test_that("read_tickers_catalog works for remove_untrade = FALSE", {

  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  #Apply function
  results <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = FALSE)

  #Check that untraded are still present
  expect_true(all(tickers_catalog@untraded %in% lookup_catalog(tickers_catalog, perm_id_to_lookup = results@data$ticker)))

  #Check that stocks for which last_quote is before current_date are not present
  expect_equal(nrow(
    results@data %>%
    dplyr::filter(tickers %in% tickers_catalog@catalog$tickers[which(
      tickers_catalog@catalog$date_last_quote < (tickers_catalog@current_date) + tickers_catalog@n_days_tolerance)] #Filter tickers with last_quote before current_date
    )),
    0)

  #Check that stocks with first_quote after current_date are not present
  expect_equal(nrow(
    results@data %>%
      dplyr::filter(tickers %in% tickers_catalog@catalog$tickers[which(
        tickers_catalog@catalog$date_first_quote > tickers_catalog@current_date)] #Filter tickers with last_quote before current_date
      )),
    0)

  #Let's suppose that ABCB4 first quote were on 2023-08-15
  abcb4_original <- results@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-07-15")
  #Change catalog
  tickers_catalog2 <- tickers_catalog
  tickers_catalog2@catalog$date_first_quote[3] <- "2023-08-15" %>% as.Date()

  results2 <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog2, remove_untraded = FALSE)
  abcb4_madeup <- results2@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-07-15")

  expect_equal(nrow(abcb4_madeup), 0)
  expect_false(identical(abcb4_madeup, abcb4_original))

  #Now suppose ABCB4 last quote were on 2023-08-15
  abcb4_original <- results@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-09-15")

  #Change catalog
  tickers_catalog@catalog$date_last_quote[3] <- "2023-08-15" %>% as.Date()
  results2 <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = FALSE)
  abcb4_madeup <- results2@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-09-15")

  expect_equal(nrow(abcb4_madeup), 0)
  expect_false(identical(abcb4_madeup, abcb4_original))


})

test_that("read_tickers_catalog works for remove_untrade = TRUE", {

  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  #Apply function
  results <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = TRUE)

  #Check that untraded are NOT present
  expect_true(all(!tickers_catalog@untraded %in% lookup_catalog(tickers_catalog, perm_id_to_lookup = results@data$ticker)))
  expect_true(all(!tickers_catalog@catalog$tickers[which(is.na(tickers_catalog@catalog$date_last_quote))]  %in% names(results@data$ticker)))
  expect_true(all(!tickers_catalog@catalog$tickers[which(is.na(tickers_catalog@catalog$date_first_quote))]  %in% names(results@data$ticker)))


  #Check that stocks with last_quote is before current_date are not present
  expect_equal(nrow(
    results@data %>%
      dplyr::filter(tickers %in% tickers_catalog@catalog$tickers[which(
        tickers_catalog@catalog$date_last_quote < (tickers_catalog@current_date) - tickers_catalog@n_days_tolerance)] #Filter tickers with last_quote before current_date
      )),
    0)

  #Check that stocks with first_quote after current_date are not present
  expect_equal(nrow(
    results@data %>%
      dplyr::filter(tickers %in% tickers_catalog@catalog$tickers[which(
        tickers_catalog@catalog$date_first_quote > tickers_catalog@current_date)] #Filter tickers with last_quote before current_date
      )),
    0)
  #Let's suppose that ABCB4 first quote were on 2023-08-15
  abcb4_original <- results@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-07-15")
  #Change catalog
  tickers_catalog@catalog$date_first_quote[3] <- "2023-08-15" %>% as.Date()

  results2 <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = FALSE)
  abcb4_madeup <- results2@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-07-15")

  expect_equal(nrow(abcb4_madeup), 0)
  expect_false(identical(abcb4_madeup, abcb4_original))

  #Now suppose ABCB4 last quote were on 2023-08-15
  abcb4_original <- results@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-09-15")

  #Change catalog
  tickers_catalog@catalog$date_last_quote[3] <- "2023-08-15" %>% as.Date()
  results2 <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = FALSE)
  abcb4_madeup <- results2@data %>% dplyr::filter(tickers == "0855dfb2d5", dates == "2023-09-15")

  expect_equal(nrow(abcb4_madeup), 0)
  expect_false(identical(abcb4_madeup, abcb4_original))


})

test_that("read_tickers_catalog correctly responds to n_days_tolerance", {
  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote,
                                            n_days_tolerance = 10)

  #Apply function
  results <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = TRUE)

  #Change tickers catalog
  tickers_catalog2 <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote,
                                            n_days_tolerance = 1)
  results2 <- read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog2, remove_untraded = TRUE)

  #Check that EALT3 is now out
  expect_equal(nrow(results@data %>% dplyr::filter(tickers == "9ac40e33b6", dates == "2023-09-15")), 1)
  expect_equal(nrow(results2@data %>% dplyr::filter(tickers == "9ac40e33b6", dates == "2023-09-15")), 0)

})

test_that("read_tickers_catalog throws an error when versions do not match", {

  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:E22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:E1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  #Now one updates raw_features_m-df
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #But uses an outdated catalog
  expect_error(
    read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = TRUE),
    "The current_date of raw_features_m_df does not match the one in tickers_catalog"
  )

  #The same would happen if one uses a different name mdf
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:E22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:E1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))

  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names,
                                             meta_dataframe_name = "other_name")

  expect_error(
    read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = TRUE),
    "The meta_dataframe_name of raw_features_m_df does not match the one in tickers_catalog"
  )



})

test_that("read_tickers_catalog throws an error when there are tickers not in catalog", {

  #Load excel and set inputs and outputs
  raw_features_input <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                                         features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                                         features_sheet_range = c("D4:F22"),
                                                         tickers_sheet_range = c("C4:C22"),
                                                         dates_sheet_range = c("D1:F1"),
                                                         output_sheet_name = c("panel"),
                                                         output_sheet_range = c("B1:I58"),
                                                         industry_classification_column_name = c("sector_c1"))
  #Apply function
  raw_features_m_df <- create_meta_dataframe(data = raw_features_input$inputs$feature_list,
                                             tickers = raw_features_input$inputs$tickers$...1,
                                             dates  = raw_features_input$inputs$dates,
                                             features_names = raw_features_input$inputs$features_names)

  #Get real date_first_quote and date_last_quote
  date_first_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                         sheet = "date_first_quote",
                                         range = "A1:B20",
                                         col_names = TRUE
  ) %>% as.data.frame()

  date_last_quote <- readxl::read_excel(test_path("testdata", "toy_features.xlsx"),
                                        sheet = "date_last_quote",
                                        range = "A1:B20",
                                        col_names = TRUE
  ) %>% as.data.frame()

  #Get tickers catalog
  tickers_catalog <- create_tickers_catalog(raw_features_m_df = raw_features_m_df, date_first_quote = date_first_quote, date_last_quote = date_last_quote)

  #Now change a ticker
  raw_features_m_df@data$tickers[1] <- "TIMS3"
  #Apply function
  expect_error(
  read_tickers_catalog(raw_features_m_df = raw_features_m_df, tickers_catalog = tickers_catalog, remove_untraded = TRUE),
  "Some tickers in raw_features_m_df are not present in tickers_catalog"
  )



})


