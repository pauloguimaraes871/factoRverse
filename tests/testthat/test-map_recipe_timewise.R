#winsorize----------------
test_that("step_winsorize prep computes correct limits", {
  # Sample dataset
  df <- data.frame(
    x = c(-10, -5, 0, 5, 10, 100),
    y = c(1, 2, 3, 4, 5, 6)
  )

  # Define a recipe with the custom step
  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, probs = c(0.05, 0.95))

  # Prepare the recipe
  prep_rec <- recipes::prep(rec, training = df)

  winsor_limits <- recipes::tidy(prep_rec, number = 1)
  expect_equal(winsor_limits$lower[1], quantile(df$x, 0.05, na.rm = TRUE))
  expect_equal(winsor_limits$upper[1], quantile(df$x, 0.95, na.rm = TRUE))
})

test_that("step_winsorize tidy method returns correct values for multiple columns", {
  df <- data.frame(
    x = c(-10, -5, 0, 5, 10, 100),
    y = c(1, 2, 3, 4, 5, 6)
  )

  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, y, probs = c(0.05, 0.95))

  prep_rec <- recipes::prep(rec, training = df)
  winsor_limits <- recipes::tidy(prep_rec, number = 1)

  expect_equal(winsor_limits$lower[winsor_limits$column == "x"], quantile(df$x, 0.05, na.rm = TRUE))
  expect_equal(winsor_limits$upper[winsor_limits$column == "x"], quantile(df$x, 0.95, na.rm = TRUE))

  expect_equal(winsor_limits$lower[winsor_limits$column == "y"], quantile(df$y, 0.05, na.rm = TRUE))
  expect_equal(winsor_limits$upper[winsor_limits$column == "y"], quantile(df$y, 0.95, na.rm = TRUE))
})

test_that("step_winsorize works as expected for simple toy data, with correct selection of variables", {

  # Sample dataset
  df <- data.frame(
    x = c(-10, -5, 0, 5, 10, 100),
    y = c(1, 2, 3, 4, 5, 6)
  )

  # Define a recipe with the custom step
  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, probs = c(0.05, 0.95))

  # Prepare the recipe
  prep_rec <- recipes::prep(rec, training = df)

  # Bake the data
  baked_df <- bake(prep_rec, new_data = df)

  # Compute expected thresholds
  expected_lower <- quantile(df$x, 0.05, na.rm = TRUE)
  expected_upper <- quantile(df$x, 0.95, na.rm = TRUE)

  expect_true(all(baked_df$x >= expected_lower))
  expect_true(all(baked_df$x <= expected_upper))
  expect_equal(baked_df$x[1], as.numeric(expected_lower))
  expect_equal(baked_df$x[6], as.numeric(expected_upper))

  #y no change
  expect_equal(baked_df$y, df$y)


  # Check that this is the same as using all_numerical and changing probs
  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(recipes::all_numeric_predictors(), probs = c(0.025, 0.975))

  # Prepare the recipe
  prep_rec <- recipes::prep(rec, training = df)

  # Bake the data
  baked_df <- bake(prep_rec, new_data = df)

  # Compute expected thresholds
  expected_lower <- quantile(df$x, 0.025, na.rm = TRUE)
  expected_upper <- quantile(df$x, 0.975, na.rm = TRUE)
  expect_true(all(baked_df$x >= expected_lower))
  expect_true(all(baked_df$x <= expected_upper))
  expect_equal(baked_df$x[1], as.numeric(expected_lower))
  expect_equal(baked_df$x[6], as.numeric(expected_upper))

  # Compute expected thresholds
  expected_lower <- quantile(df$y, 0.025, na.rm = TRUE)
  expected_upper <- quantile(df$y, 0.975, na.rm = TRUE)
  expect_true(all(baked_df$y >= expected_lower))
  expect_true(all(baked_df$y <= expected_upper))
  expect_equal(baked_df$y[1], as.numeric(expected_lower))
  expect_equal(baked_df$y[6], as.numeric(expected_upper))


})

test_that("step_winsorize does nothing when all values are identical", {
  df <- data.frame(x = rep(5, 10))  # All values are 5

  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, probs = c(0.05, 0.95))

  prep_rec <- recipes::prep(rec, training = df)
  baked_df <- bake(prep_rec, new_data = df)

  expect_equal(baked_df$x, df$x)  # Values should remain the same
})

test_that("step_winsorize with extreme probs (0, 1) does nothing", {
  df <- data.frame(x = c(-10, -5, 0, 5, 10, 100))

  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, probs = c(0, 1))  # No winsorization should occur

  prep_rec <- recipes::prep(rec, training = df)
  baked_df <- bake(prep_rec, new_data = df)

  expect_equal(baked_df$x, df$x)  # Values should remain unchanged
})

test_that("step_winsorize handles missing values correctly", {
  df <- data.frame(x = c(NA, -10, -5, 0, 5, 10, 100, NA))  # Some NA values

  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, probs = c(0.05, 0.95))

  prep_rec <- recipes::prep(rec, training = df)
  baked_df <- bake(prep_rec, new_data = df)

  expect_true(all(is.na(baked_df$x[is.na(df$x)])))  # NA values should remain NA
})

test_that("step_winsorize correctly replaces -Inf lower bounds with min finite value", {

  # Sample dataset with extreme values
  df <- data.frame(
    x = c(-Inf, -100, -50, 0, 50, 100, Inf),
    y = c(1, 2, 3, 4, 5, 6, 7)
  )

  # Define a recipe with extreme probs
  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, probs = c(0, 0.95))  # 0% will return -Inf

  # Expect a warning about -Inf being replaced
  expect_warning(
    prep_rec <- recipes::prep(rec, training = df),
    regexp = "Winsorization for column 'x' resulted in an infinite lower bound"
  )

  # Extract winsor limits
  winsor_limits <- recipes::tidy(prep_rec, number = 1)

  # Check that -Inf was replaced with the minimum finite value
  expect_equal(winsor_limits$lower[1], min(df$x[is.finite(df$x)], na.rm = TRUE))
})

test_that("step_winsorize correctly replaces Inf upper bounds with max finite value", {

  # Sample dataset with extreme values
  df <- data.frame(
    x = c(-100, -50, 0, 50, 100, Inf),
    y = c(1, 2, 3, 4, 5, 6)
  )

  # Define a recipe with extreme probs
  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, probs = c(0.05, 1))  # 100% will return Inf

  # Expect a warning about Inf being replaced
  expect_warning(
    prep_rec <- recipes::prep(rec, training = df),
    regexp = "Winsorization for column 'x' resulted in an infinite upper bound"
  )

  # Extract winsor limits
  winsor_limits <- recipes::tidy(prep_rec, number = 1)

  # Check that Inf was replaced with the maximum finite value
  expect_equal(winsor_limits$upper[1], max(df$x[is.finite(df$x)], na.rm = TRUE))
})

test_that("step_winsorize correctly replaces both -Inf and Inf with finite values", {

  # Sample dataset with both -Inf and Inf
  df <- data.frame(
    x = c(-Inf, -100, -50, 0, 50, 100, Inf),
    y = c(1, 2, 3, 4, 5, 6, 7)
  )

  # Define a recipe with extreme probs
  rec <- recipes::recipe(~ ., data = df) %>%
    step_winsorize(x, probs = c(0, 1))  # Forces both -Inf and Inf

  # Expect warnings for both bounds
  expect_warning(
    prep_rec <- recipes::prep(rec, training = df),
    regexp = "Winsorization for column 'x' resulted in an infinite lower bound.*infinite upper bound"
  )

  # Extract winsor limits
  winsor_limits <- recipes::tidy(prep_rec, number = 1)

  # Check that -Inf was replaced with the minimum finite value
  expect_equal(winsor_limits$lower[1], min(df$x[is.finite(df$x)], na.rm = TRUE))

  # Check that Inf was replaced with the maximum finite value
  expect_equal(winsor_limits$upper[1], max(df$x[is.finite(df$x)], na.rm = TRUE))
})

test_that("step_winsorize works with a single-column dataframe", {
  df <- data.frame(x = c(-10, -5, 0, 5, 10, 100))

  rec <- recipes::recipe(~ x, data = df) %>%
    step_winsorize(x, probs = c(0.05, 0.95))

  prep_rec <- recipes::prep(rec, training = df)
  baked_df <- bake(prep_rec, new_data = df)

  expected_lower <- quantile(df$x, 0.05, na.rm = TRUE)
  expected_upper <- quantile(df$x, 0.95, na.rm = TRUE)

  expect_true(all(baked_df$x >= expected_lower))
  expect_true(all(baked_df$x <= expected_upper))
})

test_that("step_winsorize correctly works inside map_recipe_timewise", {

  # Load Excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(
    csv_file_name = "toy_features.xlsx",
    features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
    features_sheet_range = c("D4:F22"),
    tickers_sheet_range = c("C4:C22"),
    dates_sheet_range = c("D1:F1"),
    output_sheet_name = c("panel"),
    output_sheet_range = c("B1:I58"),
    industry_classification_column_name = c("sector_c1")
  )

  # Apply function
  panel <- create_meta_dataframe(
    data = results$inputs$feature_list,
    tickers = results$inputs$tickers$...1,
    dates  = results$inputs$dates,
    features_names = results$inputs$features_names
  )

  # Create a recipe with step_winsorize
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    step_winsorize(recipes::all_numeric_predictors(), probs = c(0.05, 0.95))

  # Run
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE, type = "generic")

  # Extract processed data
  processed_data <- pp_panel@data

  # Check that winsorization was applied correctly across all dates
  for (date in unique(panel@data$dates)) {
    test_subset <- dplyr::filter(panel@data, dates == date)
    processed_subset <- dplyr::filter(processed_data, dates == date)

    # Manually compute winsorized values for each feature
    expected_subset <- test_subset
    for (feature in results$inputs$features_names[c(1:4)]) {

      # Compute lower and upper winsorization thresholds
      lower_threshold <- quantile(test_subset[[feature]], 0.05, na.rm = TRUE)
      upper_threshold <- quantile(test_subset[[feature]], 0.95, na.rm = TRUE)

      # Winsorize the data manually
      expected_subset[[feature]] <- pmax(lower_threshold, pmin(expected_subset[[feature]], upper_threshold))
    }

    for (feature in results$inputs$features_names[c(1:4)]) {
      expect_equal(processed_subset[[feature]], expected_subset[[feature]],
                   tolerance = 1e-5,
                   info = paste("Mismatch in", feature, "for date", date))
    }
  }

})

test_that("step_winsorize throws an error when trying to winsorize categorical variable", {

  # Load Excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(
    csv_file_name = "toy_features.xlsx",
    features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
    features_sheet_range = c("D4:F22"),
    tickers_sheet_range = c("C4:C22"),
    dates_sheet_range = c("D1:F1"),
    output_sheet_name = c("panel"),
    output_sheet_range = c("B1:I58"),
    industry_classification_column_name = c("sector_c1")
  )

  # Apply function
  panel <- create_meta_dataframe(
    data = results$inputs$feature_list,
    tickers = results$inputs$tickers$...1,
    dates  = results$inputs$dates,
    features_names = results$inputs$features_names
  )

  # Create a recipe with step_winsorize
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    step_winsorize(recipes::all_predictors(), probs = c(0.05, 0.95))

  # Run
  set.seed(123)
  expect_error(
  map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE, type = "generic")
  )

})

#sector_impute_sector----------------

test_that("step_impute_sector correctly computes imputation values", {
  # Sample dataset
  df <- data.frame(
    sector = c("A", "A", "B", "B", "C", "C"),
    x = c(1, 2, NA, 4, 5, NA),
    y = c(10, NA, 30, NA, 50, 60)
  )

  # Define a recipe with step_impute_sector
  rec <- recipes::recipe(~ ., data = df) %>%
    step_impute_sector(x, y, sector = "sector", method = "mean")

  # Prepare the recipe
  prep_rec <- recipes::prep(rec, training = df)

  # Get imputation values
  impute_values <- recipes::tidy(prep_rec, number = 1)

  # Expected imputation values
  expected_x <- df %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(x = mean(x, na.rm = TRUE), .groups = "drop")

  expected_y <- df %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(y = mean(y, na.rm = TRUE), .groups = "drop")

  # Check imputation values
  for (i in seq_len(nrow(expected_x))) {
    expect_equal(impute_values$imputed_value[impute_values$column == "x" & impute_values$sector == expected_x$sector[i]], expected_x$x[i])
  }

  for (i in seq_len(nrow(expected_y))) {
    expect_equal(impute_values$imputed_value[impute_values$column == "y" & impute_values$sector == expected_y$sector[i]], expected_y$y[i])
  }
})

test_that("step_impute_sector correctly imputes missing values", {
  # Sample dataset
  df <- data.frame(
    sector = c("A", "A", "B", "B", "C", "C"),
    x = c(1, 2, NA, 4, 5, NA),
    y = c(10, NA, 30, NA, 50, 60)
  )

  # Define a recipe with step_impute_sector
  rec <- recipes::recipe(~ ., data = df) %>%
    step_impute_sector(x, y, sector = "sector", method = "mean")

  # Prepare the recipe
  prep_rec <- recipes::prep(rec, training = df)

  # Bake the data
  baked_df <- bake(prep_rec, new_data = df)

  # Expected imputed values
  expected_x <- df %>%
    dplyr::group_by(sector) %>%
    dplyr::mutate(x = ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>%
    dplyr::ungroup()

  expected_y <- df %>%
    dplyr::group_by(sector) %>%
    dplyr::mutate(y = ifelse(is.na(y), mean(y, na.rm = TRUE), y)) %>%
    dplyr::ungroup()

  # Check imputed values
  expect_equal(baked_df$x, expected_x$x)
  expect_equal(baked_df$y, expected_y$y)
})

test_that("step_impute_sector correctly handles median imputation", {
  # Sample dataset
  df <- data.frame(
    sector = c("A", "A", "B", "B", "C", "C"),
    x = c(1, 2, NA, 4, 5, NA),
    y = c(10, NA, 30, NA, 50, 60)
  )

  # Define a recipe with step_impute_sector
  rec <- recipes::recipe(~ ., data = df) %>%
    step_impute_sector(x, y, sector = "sector", method = "median")

  # Prepare the recipe
  prep_rec <- recipes::prep(rec, training = df)

  # Bake the data
  baked_df <- bake(prep_rec, new_data = df)

  # Expected imputed values
  expected_x <- df %>%
    dplyr::group_by(sector) %>%
    dplyr::mutate(x = ifelse(is.na(x), median(x, na.rm = TRUE), x)) %>%
    dplyr::ungroup()

  expected_y <- df %>%
    dplyr::group_by(sector) %>%
    dplyr::mutate(y = ifelse(is.na(y), median(y, na.rm = TRUE), y)) %>%
    dplyr::ungroup()

  # Check imputed values
  expect_equal(baked_df$x, expected_x$x)
  expect_equal(baked_df$y, expected_y$y)
})

test_that("step_impute_sector handles cases where all values in a sector are NA", {
  # Sample dataset with one sector having only NA values
  df <- data.frame(
    sector = c("A", "A", "B", "B", "C", "C"),
    x = c(1, 2, NA, NA, NA, NA), # All NA in sector C
    y = c(10, 20, 30, 40, NA, NA) # All NA in sector C
  )

  # Define a recipe with step_impute_sector
  rec <- recipes::recipe(~ ., data = df) %>%
    step_impute_sector(x, y, sector = "sector", method = "mean")

  # Prepare the recipe
  prep_rec <- recipes::prep(rec, training = df)

  # Bake the data
  baked_df <- recipes::bake(prep_rec, new_data = df)

  # Expect sector C to still have NA (since no values exist to compute mean)
  expect_true(all(is.na(baked_df$x[baked_df$sector == "C"])))
  expect_true(all(is.na(baked_df$y[baked_df$sector == "C"])))
})

test_that("step_impute_sector handles completely missing dataset", {
  # Sample dataset with only NA values
  df <- data.frame(
    sector = c(NA, NA, NA, NA, NA, NA),
    x = c(NA, NA, NA, NA, NA, NA),
    y = c(NA, NA, NA, NA, NA, NA)
  )

  # Define a recipe with step_impute_sector
  rec <- recipes::recipe(~ ., data = df) %>%
    step_impute_sector(x, y, sector = "sector", method = "mean")

  # Expect an error when trying to prep the recipe
  expect_error(recipes::prep(rec, training = df))
})

test_that("step_impute_sector does nothing when there are no missing values", {
  # Sample dataset with no missing values
  df <- data.frame(
    sector = c("A", "A", "B", "B", "C", "C"),
    x = c(1, 2, 3, 4, 5, 6),
    y = c(10, 20, 30, 40, 50, 60)
  )

  # Define a recipe with step_impute_sector
  rec <- recipes::recipe(~ ., data = df) %>%
    step_impute_sector(x, y, sector = "sector", method = "mean") %>%
    recipes::step_dummy(sector, one_hot = FALSE) # Ensures it's handled properly

  # Prepare the recipe
  prep_rec <- recipes::prep(rec, training = df)

  # Bake the data
  baked_df <- recipes::bake(prep_rec, new_data = df) %>% as.data.frame()

  # Expect no changes
  expect_equal(baked_df$x, df$x)
  expect_equal(baked_df$y, df$y)
})

test_that("step_impute_sector throws an error when sector column is missing", {
  # Sample dataset
  df <- data.frame(
    x = c(1, 2, NA, 4, 5, NA),
    y = c(10, NA, 30, NA, 50, 60)
  )

  # Define a recipe with step_impute_sector
  rec <- recipes::recipe(~ ., data = df) %>%
    step_impute_sector(x, y, sector = "sector", method = "mean")

  # Expect an error because "sector" column is missing
  expect_error(recipes::prep(rec, training = df), "Sector column sector is not in the training data.")
})

test_that("step_impute_sector throws an error when trying to impute non-numeric columns", {
  # Sample dataset
  df <- data.frame(
    sector = c("A", "A", "B", "B", "C", "C"),
    x = c(1, 2, NA, 4, 5, NA),
    y = c("a", "b", "c", "d", "e", "f") # Non-numeric column
  )

  # Define a recipe with step_impute_sector
  rec <- recipes::recipe(~ ., data = df) %>%
    step_impute_sector(x, y, sector = "sector", method = "mean")

  # Expect an error because y is non-numeric
  expect_error(recipes::prep(rec, training = df))
})

test_that("step_impute_sector correctly works inside map_recipe_timewise", {
  # Load Excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(
    csv_file_name = "toy_features.xlsx",
    features_sheet_names = c("ebit_12m", "ir_3m", "sharpe", "mkt_cap", "sector_c1"),
    features_sheet_range = c("D4:F22"),
    tickers_sheet_range = c("C4:C22"),
    dates_sheet_range = c("D1:F1"),
    output_sheet_name = c("panel"),
    output_sheet_range = c("B1:I58"),
    industry_classification_column_name = c("sector_c1")
  )

  # Apply function
  panel <- create_meta_dataframe(
    data = results$inputs$feature_list,
    tickers = results$inputs$tickers$...1,
    dates  = results$inputs$dates,
    features_names = results$inputs$features_names
  )

  # Create a recipe with step_impute_sector
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    step_impute_sector(recipes::all_numeric_predictors(), sector = "sector_c1", method = "mean")

  # Run
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE, type = "generic")

  # Extract processed data
  processed_data <- pp_panel@data

  # Check that imputation was applied correctly across all dates
  for (date in unique(panel@data$dates)) {
    test_subset <- dplyr::filter(panel@data, dates == date)
    processed_subset <- dplyr::filter(processed_data, dates == date)

    # Apply imputation using extracted impute values
    expected_subset <- test_subset
    for (feature in results$inputs$features_names[c(1:4)]) {

      # Compute sector-level means
      group_means <- test_subset %>%
        dplyr::group_by(sector_c1) %>%
        dplyr::summarize(mean_sector = mean(!!rlang::sym(feature), na.rm = TRUE), .groups = "drop")

      # Join imputed means back to the dataset
      expected_subset <- dplyr::left_join(expected_subset, group_means, by = "sector_c1")

      # Replace NAs with the sector mean
      expected_subset <- expected_subset %>%
        dplyr::mutate(!!rlang::sym(feature) := dplyr::if_else(is.na(!!rlang::sym(feature)), mean_sector, !!rlang::sym(feature))) %>%
        dplyr::select(-mean_sector)  # Remove the helper column

    }

    for (feature in results$inputs$features_names[c(1:4)]) {
      expect_equal(processed_subset[[feature]], expected_subset[[feature]],
                   tolerance = 1e-5,
                   info = paste("Mismatch in", feature, "for date", date))
    }
  }
})


#map_recipe_timewise----------------------
test_that("map_recipe_timewise works for step_impute_mean + mode - Excel Files", {

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

  #Run
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE)

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

test_that("map_recipe_timewise works for step_impute_bag - Excel Files", {

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


  # Run
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE)

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

test_that("map_recipe_timewise works for step_impute_knn - Excel Files", {

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

  # Run
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE)

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

test_that("map_recipe_timewise works for handling factors - Excel Files", {

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

  # Run
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE)

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

  # Run
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE)

  # Ensure no NA values remain in numeric columns and sector_c1 dummy variables
  expect_false(anyNA(processed_data))


})

test_that("map_recipe_timewise works for handling factors when not imputing with mode - Excel Files", {

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


  ##Some rows will have NAs because of only NAs in rolls
  # Create recipe with step_impute_knn and step_impute_mode for sector_c1
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "predictor") %>%
    recipes::step_impute_median(recipes::all_numeric_predictors())%>%  # Use median imputation
    recipes::step_unknown(sector_c1) %>%
    recipes::step_dummy(sector_c1, one_hot = TRUE)  # Convert sector_c1 to dummy variables

  # Run
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE)

  # Extract processed data
  processed_data <- pp_panel@data

  # Checks that sector_c1_unknown contains 1
  expect_true(1 %in% processed_data$sector_c1_unknown)

  #No NAs
  expect_false(anyNA(processed_data))



})

test_that("map_recipe_timewise works for step_range (in parallel) - Excel Files", {

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
    step_winsorize(recipes::all_numeric_predictors()) %>% # Apply winsorization
    recipes::step_range(recipes::all_numeric_predictors(), min = -1, max = 1)  # Apply min-max scaling

  # Run
  future::plan("multisession")
  set.seed(123)
  pp_panel <- map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = TRUE)

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
  future::plan("sequential")

})

test_that("map_recipe_timewise throws an error when rows with only NAs are not correctly handled", {

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

  # Run and check that will not work bco of NAs in KNN (Some rows will have NAs because of only NAs in rows)
  expect_error(
    map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE),
    "Data contains missing values")


})

test_that("map_recipe_timewise throws error when roles are not correctly defined", {

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
    recipes::step_impute_knn(
      recipes::all_numeric_predictors(),
      neighbors = 5,
      impute_with = recipes::imp_vars(recipes::all_numeric_predictors())
      )

  # Run and check that will not work bco of NAs in KNN (Some rows will have NAs because of only NAs in rows)
  expect_error(
    map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE),
    "Variable id must have the role 'id_vars'.")



  # Create recipe with step_impute_knn and step_impute_mode for sector_c1
  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::step_impute_knn(
      recipes::all_numeric_predictors(),
      neighbors = 5,
      impute_with = recipes::imp_vars(recipes::all_numeric_predictors())
    )

  expect_error(
    map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE),
    "The following columns do not have an assigned role in the recipe: ebit_12m, ir_3m, sharpe, mkt_cap, sector_c1")

  recipe <- recipes::recipe(panel@data) %>%
    recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
    recipes::update_role(sector_c1, new_role = "outcome") %>%
    recipes::step_impute_knn(
      recipes::all_numeric_predictors(),
      neighbors = 5,
      impute_with = recipes::imp_vars(recipes::all_numeric_predictors())
    )

  expect_error(
    map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE),
    "Please create a specific meta_dataframe with appropriate type to manage targets separately.")



})

test_that("map_recipe_timewise throws an error when there is only one observation in a row", {

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

  #Leave only one data
  panel@data <- panel@data[-c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,49,52,55),]


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

  # Run and check that will not work bco of NAs in KNN (Some rows will have NAs because of only NAs in rows)
  expect_error(
  expect_warning(
    map_recipe_timewise(panel, recipe, verbose = TRUE, parallel = FALSE),
    "Not enough data to prep the recipe for date: 2023-07-15")
  )


})


#integration with other functions
test_that("map_recipe_timewise integrates with tickers_catalog and compute FUNs", {
skip()
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

  #tickers catalog
  tickers_catalog <- create_tickers_catalog(panel, date_first_quote, date_last_quote)

  #read
  pre_silver_panel <- read_tickers_catalog(panel, tickers_catalog)

  #compute FUNs
  pre_silver_panel <- pre_silver_panel %>% compute_formula(formula = "ebit_12m/mkt_cap", feature_name = "ebit_y")


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



})











