test_that("run_pp_backtest works for step_impute_mean + mode - using config + recipe", {

  #Load excel and set inputs and outputs
  data <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                              features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                              features_sheet_range = c("D4:F22"),
                                              tickers_sheet_range = c("C4:C22"),
                                              dates_sheet_range = c("D1:F1"),
                                              output_sheet_name = c("panel"),
                                              output_sheet_range = c("B1:I58"),
                                              industry_classification_column_name = c("sector_c1"))
  #Apply function
  panel <- create_meta_dataframe(data = data$inputs$feature_list,
                                 tickers = data$inputs$tickers$...1,
                                 dates  = data$inputs$dates,
                                 features_names = data$inputs$features_names)

  #Create recipe
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_mean(recipes::all_numeric_predictors()) %>%
    recipes::step_impute_mode(sector_c1)

  #Create pp_config for median imputation
  pp_config <- create_pp_backtest_config(panel, rec_obj = recipe)

  #Run
  set.seed(123)
  pp_panel <- run_pp_backtest(panel, pp_config, verbose = TRUE, parallel = FALSE)

  # Extract processed data
  processed_data <- pp_panel@data

  # Check that NAs are replaced with column-wise mean per date
  for (date in unique(panel@data$dates)) {
    test_subset <- dplyr::filter(panel@data, dates == date)
    processed_subset <- dplyr::filter(processed_data, dates == date)

    for (feature in colnames(panel@data %>% dplyr::select(-id, -tickers, -dates, -sector_c1))) {
      if (anyNA(test_subset[[feature]])) {
        expected_mean <- mean(test_subset[[feature]], na.rm = TRUE)
        expect_equal(processed_subset[[feature]],
                     ifelse(is.na(test_subset[[feature]]), expected_mean, test_subset[[feature]]),
                     tolerance = 1e-5)
      }
    }
  }

  # Ensure no NA values remain
  expect_false(anyNA(processed_data %>% dplyr::select(-sector_c1)))



})

test_that("run_pp_backtest works for step_impute_bag - using config + recipe", {

  # Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                              features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                              features_sheet_range = c("D4:F22"),
                                              tickers_sheet_range = c("C4:C22"),
                                              dates_sheet_range = c("D1:F1"),
                                              output_sheet_name = c("panel"),
                                              output_sheet_range = c("B1:I58"),
                                              industry_classification_column_name = c("sector_c1"))

  # Apply function
  panel <- create_meta_dataframe(data = results$inputs$feature_list,
                                 tickers = results$inputs$tickers$...1,
                                 dates  = results$inputs$dates,
                                 features_names = results$inputs$features_names)

  # Create recipe with step_impute_bag
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_mode(sector_c1) %>%  # Impute mode for sector_c1
    recipes::step_impute_bag(recipes::all_numeric_predictors(), -sector_c1)


  # Create pp_config for bagged imputation
  pp_config <- create_pp_backtest_config(panel, rec_obj = recipe)

  # Run
  set.seed(123)
  pp_panel <- run_pp_backtest(panel, pp_config, verbose = TRUE, parallel = FALSE)

  # Extract processed data
  processed_data <- pp_panel@data

  # Check that NAs are replaced using step_impute_bag per date by applying the recipe manually per date
  for (date in unique(panel@data$dates)) {
    test_subset <- dplyr::filter(panel@data, dates == date)
    processed_subset <- dplyr::filter(processed_data, dates == date)

    # Apply recipe manually for comparison
    set.seed(123)  # Ensure reproducibility
    rec_prepped <- recipes::prep(recipe, training = test_subset, retain = TRUE, verbose = FALSE)
    expected_subset <- recipes::bake(rec_prepped, new_data = test_subset)

    for (feature in colnames(panel@data %>% dplyr::select(-id, -tickers, -dates, -sector_c1))) {
      if (anyNA(test_subset[[feature]])) {
        expect_equal(processed_subset[[feature]], expected_subset[[feature]],
                     tolerance = 1e-5,
                     info = paste("Mismatch in", feature, "for date", date))
      }
    }
  }

  # Ensure no NA values remain in numeric columns and sector_c1
  expect_false(anyNA(processed_data))



})

test_that("run_pp_backtest works for step_impute_knn - using config + recipe", {

  # Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                              features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                              features_sheet_range = c("D4:F22"),
                                              tickers_sheet_range = c("C4:C22"),
                                              dates_sheet_range = c("D1:F1"),
                                              output_sheet_name = c("panel"),
                                              output_sheet_range = c("B1:I58"),
                                              industry_classification_column_name = c("sector_c1"))

  # Apply function
  panel <- create_meta_dataframe(data = results$inputs$feature_list,
                                 tickers = results$inputs$tickers$...1,
                                 dates  = results$inputs$dates,
                                 features_names = results$inputs$features_names)

  # Create recipe with step_impute_knn and step_impute_mode for sector_c1
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_mode(sector_c1) %>%  # Impute sector_c1 first
    recipes::step_impute_knn(recipes::all_numeric_predictors(), neighbors = 5)  # Use KNN imputation

  # Create pp_config for KNN imputation
  pp_config <- create_pp_backtest_config(panel, rec_obj = recipe)

  # Run
  set.seed(123)
  pp_panel <- run_pp_backtest(panel, pp_config, verbose = TRUE, parallel = FALSE)

  # Extract processed data
  processed_data <- pp_panel@data

  # Check that NAs are replaced using step_impute_knn per date by applying the recipe manually per date
  for (date in unique(panel@data$dates)) {
    test_subset <- dplyr::filter(panel@data, dates == date)
    processed_subset <- dplyr::filter(processed_data, dates == date)

    # Apply recipe manually for comparison
    set.seed(123)  # Ensure reproducibility
    rec_prepped <- recipes::prep(recipe, training = test_subset, retain = TRUE, verbose = FALSE)
    expected_subset <- recipes::bake(rec_prepped, new_data = test_subset)

    for (feature in colnames(panel@data %>% dplyr::select(-id, -tickers, -dates, -sector_c1))) {
      if (anyNA(test_subset[[feature]])) {
        expect_equal(processed_subset[[feature]], expected_subset[[feature]],
                     tolerance = 1e-5,
                     info = paste("Mismatch in", feature, "for date", date))
      }
    }
  }

  # Ensure no NA values remain in numeric columns and sector_c1
  expect_false(anyNA(processed_data))

})

test_that("run_pp_backtest works for handling factors - using config + recipe", {

  # Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                              features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                              features_sheet_range = c("D4:F22"),
                                              tickers_sheet_range = c("C4:C22"),
                                              dates_sheet_range = c("D1:F1"),
                                              output_sheet_name = c("panel"),
                                              output_sheet_range = c("B1:I58"),
                                              industry_classification_column_name = c("sector_c1"))

  # Apply function
  panel <- create_meta_dataframe(data = results$inputs$feature_list,
                                 tickers = results$inputs$tickers$...1,
                                 dates  = results$inputs$dates,
                                 features_names = results$inputs$features_names)

  # Introduce a missing factor level for a random date
  set.seed(123)
  random_date <- sample(unique(panel@data$dates), 1)
  panel@data <- panel@data %>%
    dplyr::mutate(sector_c1 = ifelse(dates == random_date & sector_c1 == "Indústria", NA, sector_c1))


  # Create recipe with step_impute_knn and step_impute_mode for sector_c1
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_mode(sector_c1) %>%  # Impute sector_c1 first)
    recipes::step_impute_knn(recipes::all_numeric_predictors(), neighbors = 5) %>%  # Use KNN imputation
    recipes::step_unknown(sector_c1) %>%
    recipes::step_dummy(sector_c1, one_hot = TRUE)  # Convert sector_c1 to dummy variables

  # Create pp_config for KNN imputation
  pp_config <- create_pp_backtest_config(panel, rec_obj = recipe)

  # Run
  set.seed(123)
  pp_panel <- run_pp_backtest(panel, pp_config, verbose = TRUE, parallel = FALSE)

  # Extract processed data
  processed_data <- pp_panel@data

  # Check that the missing factor level for the selected date has been assigned as 0s
  missing_dummy_columns <- "sector_c1_Indústria"
  processed_subset <- dplyr::filter(processed_data, dates == random_date)
  expect_true(all(processed_subset %>% dplyr::select(dplyr::all_of(missing_dummy_columns)) == 0),
              info = paste("Dummy variables for missing factor level were not set to 0 on date", random_date))

  # Check that one-hot encoding was applied correctly across all dates
  for (date in unique(panel@data$dates)) {
    test_subset <- dplyr::filter(panel@data, dates == date)
    processed_subset <- dplyr::filter(processed_data, dates == date)

    # Apply recipe manually for comparison
    set.seed(123)  # Ensure reproducibility
    rec_prepped <- recipes::prep(recipe, training = test_subset, retain = TRUE, verbose = FALSE)
    expected_subset <- recipes::bake(rec_prepped, new_data = test_subset)

    for (feature in colnames(expected_subset %>% dplyr::select(dplyr::starts_with("sector_c1_")))) {
      expect_equal(processed_subset[[feature]], expected_subset[[feature]],
                   tolerance = 1e-5,
                   info = paste("Mismatch in", feature, "for date", date))
    }
  }

  # Ensure no NA values remain in numeric columns and sector_c1 dummy variables
  expect_false(anyNA(processed_data))

  #Or ensure characters become factors
  panel@data$sector_c1 <- as.factor(panel@data$sector_c1)

  # Create recipe with step_impute_knn and step_impute_mode for sector_c1
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_mode(sector_c1) %>%  # Impute sector_c1 first)
    recipes::step_impute_knn(recipes::all_numeric_predictors(), neighbors = 5) %>%  # Use KNN imputation
    recipes::step_unknown(sector_c1) %>%
    recipes::step_dummy(sector_c1, one_hot = TRUE)  # Convert sector_c1 to dummy variables

  # Create pp_config for KNN imputation
  pp_config <- create_pp_backtest_config(panel, rec_obj = recipe)

  # Run
  set.seed(123)
  pp_panel <- run_pp_backtest(panel, pp_config, verbose = TRUE, parallel = FALSE)

  # Ensure no NA values remain in numeric columns and sector_c1 dummy variables
  expect_false(anyNA(processed_data))


})

test_that("run_pp_backtest works for handling factors when not imputing with mode - using config + recipe", {

  # Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                              features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                              features_sheet_range = c("D4:F22"),
                                              tickers_sheet_range = c("C4:C22"),
                                              dates_sheet_range = c("D1:F1"),
                                              output_sheet_name = c("panel"),
                                              output_sheet_range = c("B1:I58"),
                                              industry_classification_column_name = c("sector_c1"))

  # Apply function
  panel <- create_meta_dataframe(data = results$inputs$feature_list,
                                 tickers = results$inputs$tickers$...1,
                                 dates  = results$inputs$dates,
                                 features_names = results$inputs$features_names)

  # Introduce a missing factor level for a random date
  set.seed(123)
  random_date <- sample(unique(panel@data$dates), 1)
  panel@data <- panel@data %>%
    dplyr::mutate(sector_c1 = ifelse(dates == random_date & sector_c1 == "Indústria", NA, sector_c1))


  # Create recipe with step_impute_knn and step_impute_mode for sector_c1
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_knn(
      recipes::all_numeric_predictors(),
      neighbors = 5,
      impute_with = recipes::imp_vars(recipes::all_numeric_predictors()) # Exclude sector_c1 from imputation
    )%>%  # Use KNN imputation
    recipes::step_unknown(sector_c1) %>%
    recipes::step_dummy(sector_c1, one_hot = TRUE)  # Convert sector_c1 to dummy variables

  # Create pp_config for KNN imputation
  pp_config <- create_pp_backtest_config(panel, rec_obj = recipe)

  # Run and check that will not work bco of NAs in KNN (Some rows will have NAs because of only NAs in rows)
  expect_error(
  run_pp_backtest(panel, pp_config, verbose = TRUE, parallel = FALSE),
  "Data contains missing values")


  ##Some rows will have NAs because of only NAs in rolls
  # Create recipe with step_impute_knn and step_impute_mode for sector_c1
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()
    )%>%  # Use median imputation
    recipes::step_unknown(sector_c1) %>%
    recipes::step_dummy(sector_c1, one_hot = TRUE)  # Convert sector_c1 to dummy variables

  # Create pp_config
  pp_config <- create_pp_backtest_config(panel, rec_obj = recipe)

  # Run
  set.seed(123)
  pp_panel <- run_pp_backtest(panel, pp_config, verbose = TRUE, parallel = FALSE)

  # Extract processed data
  processed_data <- pp_panel@data

  # Checks that sector_c1_unknown contains 1
  expect_true(1 %in% processed_data$sector_c1_unknown)

  #No NAs
  expect_false(anyNA(processed_data))



})

test_that("run_pp_backtest works for step_range - using config + recipe", {

  # Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                              features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
                                              features_sheet_range = c("D4:F22"),
                                              tickers_sheet_range = c("C4:C22"),
                                              dates_sheet_range = c("D1:F1"),
                                              output_sheet_name = c("panel"),
                                              output_sheet_range = c("B1:I58"),
                                              industry_classification_column_name = c("sector_c1"))

  # Apply function
  panel <- create_meta_dataframe(data = results$inputs$feature_list,
                                 tickers = results$inputs$tickers$...1,
                                 dates  = results$inputs$dates,
                                 features_names = results$inputs$features_names)

  # Create recipe with step_range
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_unknown(sector_c1) %>%
    recipes::step_dummy(sector_c1, one_hot = TRUE) %>%  # Convert sector_c1 to dummy variables
    recipes::step_range(recipes::all_numeric_predictors(), min = -1, max = 1)  # Apply min-max scaling

  # Create pp_config for step_range
  pp_config <- create_pp_backtest_config(panel, rec_obj = recipe)

  # Run
  set.seed(123)
  pp_panel <- run_pp_backtest(panel, pp_config, verbose = TRUE, parallel = FALSE)

  # Extract processed data
  processed_data <- pp_panel@data

  # Ensure step_range is correctly applied in a time-wise manner
  for (date in unique(panel@data$dates)) {
    test_subset <- dplyr::filter(panel@data, dates == date)
    processed_subset <- dplyr::filter(processed_data, dates == date)

    # Apply recipe manually for comparison
    set.seed(123)  # Ensure reproducibility
    rec_prepped <- recipes::prep(recipe, training = test_subset, retain = TRUE, verbose = FALSE)
    expected_subset <- recipes::bake(rec_prepped, new_data = test_subset)

    # Check that all numeric predictors have been transformed using step_range
    for (feature in colnames(expected_subset %>% dplyr::select(recipes::all_numeric_predictors()))) {
      expect_equal(processed_subset[[feature]], expected_subset[[feature]],
                   tolerance = 1e-5,
                   info = paste("Mismatch in", feature, "for date", date))
      #Check that processed_subset[[feature]] values are between -1 and 1
      expect_true(all(processed_subset[[feature]] >= -1 & processed_subset[[feature]] <= 1))

    }
  }

  # Ensure no NA values remain in numeric columns and sector_c1 dummy variables
  expect_false(anyNA(processed_data))

})



