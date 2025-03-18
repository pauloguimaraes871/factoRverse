test_that("screen_by_conditions filters correctly while preserving metadata", {

  # Create a sample meta_dataframe object
  sample_data <- data.frame(
    id = c("A-2024-01-01", "B-2024-02-01", "C-2024-03-01"),
    tickers = c("A", "B", "C"),
    dates = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    value = c(10, 20, 30)
  )

  meta_df <- create_meta_dataframe(
    sample_data,
    meta_dataframe_name = "test_meta_df"
  )

  # Apply screening
  screened_meta_df <- screen_by_conditions(meta_df, value > 15)

  # Expected data
  expected_data <- sample_data %>% dplyr::filter(value > 15)

  # Check that filtered data is correct
  expect_equal(screened_meta_df@data, expected_data)

  # Check that metadata is preserved
  expect_equal(screened_meta_df@meta_dataframe_name, meta_df@meta_dataframe_name)
  expect_equal(screened_meta_df@current_date, meta_df@current_date)
  expect_true(length(screened_meta_df@workflow) > length(meta_df@workflow))

})


test_that("screen_by_conditions throws an error when filtering on a non-existent column", {

  sample_data <- data.frame(
    id = c("A-2024-01-01", "B-2024-02-01"),
    tickers = c("A", "B"),
    dates = as.Date(c("2024-01-01", "2024-02-01")),
    value = c(10, 20)
  )

  meta_df <- create_meta_dataframe(
    sample_data,
    meta_dataframe_name = "test_meta_df"
  )

  expect_error(
    screen_by_conditions(meta_df, nonexistent_column > 10),
    "object 'nonexistent_column' not found"
  )
})


test_that("screen_by_conditions throws error when filtering removes all rows", {

  sample_data <- data.frame(
    id = c("A-2024-01-01", "B-2024-02-01"),
    tickers = c("A", "B"),
    dates = as.Date(c("2024-01-01", "2024-02-01")),
    value = c(10, 20)
  )

  meta_df <- create_meta_dataframe(
    sample_data,
    meta_dataframe_name = "test_meta_df"
  )

  expect_error(
    screen_by_conditions(meta_df, value > 1000),  # No row meets this condition
    "All stocks were filtered out. Please check expression.")

})

test_that("screen_by_conditions does not modify the dataset when all rows match the condition", {

  sample_data <- data.frame(
    id = c("A-2024-01-01", "B-2024-02-01"),
    tickers = c("A", "B"),
    dates = as.Date(c("2024-01-01", "2024-02-01")),
    value = c(10, 20)
  )

  meta_df <- create_meta_dataframe(
    sample_data,
    meta_dataframe_name = "test_meta_df"
  )

  screened_meta_df <- screen_by_conditions(meta_df, value > 5)  # All rows meet this condition

  expect_equal(screened_meta_df@data, meta_df@data)  # Expect no change in data
  expect_equal(screened_meta_df@meta_dataframe_name, meta_df@meta_dataframe_name)
})

test_that("screen_by_conditions correctly filters using multiple conditions", {

  sample_data <- data.frame(
    id = c("A-2024-01-01", "B-2024-02-01", "C-2024-03-01"),
    tickers = c("A", "B", "C"),
    dates = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    value = c(10, 20, 30)
  )

  meta_df <- create_meta_dataframe(
    sample_data,
    meta_dataframe_name = "test_meta_df"
  )

  screened_meta_df <- screen_by_conditions(meta_df, value > 15, tickers != "C")

  expected_data <- sample_data %>% dplyr::filter(value > 15 & tickers != "C")

  expect_equal(screened_meta_df@data, expected_data)
})

test_that("screen_by_conditions correctly filters based on date range", {

  sample_data <- data.frame(
    id = c("A-2024-01-01", "B-2024-02-01", "C-2024-03-01"),
    tickers = c("A", "B", "C"),
    dates = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    value = c(10, 20, 30)
  )

  meta_df <- create_meta_dataframe(
    sample_data,
    meta_dataframe_name = "test_meta_df"
  )

  screened_meta_df <- screen_by_conditions(meta_df, dates >= as.Date("2024-02-01"))

  expected_data <- sample_data %>% dplyr::filter(dates >= as.Date("2024-02-01"))

  expect_equal(screened_meta_df@data, expected_data)
})

