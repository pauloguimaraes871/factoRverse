test_that("screen_by_feature always keeps id, tickers, and dates", {

  sample_data <- data.frame(
    id = c("A-2024-01-01", "B-2024-02-01"),
    tickers = c("A", "B"),
    dates = as.Date(c("2024-01-01", "2024-02-01")),
    mom_1m = c(0.01, 0.03),
    mom_3m = c(0.02, 0.05),
    val = c(1.2, 1.5)
  )

  meta_df <- create_meta_dataframe(sample_data, meta_dataframe_name = "test_meta_df")

  screened_meta_df <- screen_by_feature(meta_df, val)

  expected_cols <- c("id", "tickers", "dates", "val")

  expect_equal(colnames(screened_meta_df@data), expected_cols)
})

test_that("screen_by_feature throws an error when selecting non-existent column", {

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
    screen_by_feature(meta_df, nonexistent_column),
    "Can't select columns that don't exist"
  )
})

test_that("screen_by_feature throws an error when nothing is selected", {

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
    screen_by_feature(meta_df),
    "No features \\(beyond id, tickers, and dates\\) were selected. Please check expression."
  )
})

test_that("screen_by_feature throws error when all columns are removed", {

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
    screen_by_feature(meta_df, -id, -tickers, -dates, -value),
    "No features \\(beyond id, tickers, and dates\\) were selected. Please check expression."
  )

  expect_error(
    screen_by_feature(meta_df, -value),
    "No features \\(beyond id, tickers, and dates\\) were selected. Please check expression."
  )

})

test_that("screen_by_feature preserves all columns when selecting everything", {

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

  screened_meta_df <- screen_by_feature(meta_df, dplyr::everything())

  expect_equal(screened_meta_df@data, meta_df@data)
  expect_equal(screened_meta_df@meta_dataframe_name, meta_df@meta_dataframe_name)
})

test_that("screen_by_feature supports tidyselect helpers like starts_with", {

  sample_data <- data.frame(
    id = c("A-2024-01-01", "B-2024-02-01"),
    tickers = c("A", "B"),
    dates = as.Date(c("2024-01-01", "2024-02-01")),
    mom_1m = c(0.01, 0.03),
    mom_3m = c(0.02, 0.05),
    val = c(1.2, 1.5)
  )

  meta_df <- create_meta_dataframe(
    sample_data,
    meta_dataframe_name = "test_meta_df"
  )

  screened_meta_df <- screen_by_feature(meta_df, dplyr::starts_with("mom"))

  expected_data <- sample_data %>% dplyr::select(id, tickers, dates, dplyr::starts_with("mom"))

  expect_equal(screened_meta_df@data, expected_data)
})

test_that("screen_by_feature throws error if only core columns are retained", {

  sample_data <- data.frame(
    id = c("X-2024-01-01", "Y-2024-01-01"),
    tickers = c("X", "Y"),
    dates = as.Date(c("2024-01-01", "2024-01-01")),
    feature1 = c(1, 2)
  )

  meta_df <- create_meta_dataframe(sample_data, meta_dataframe_name = "test_meta_df")

  expect_error(
    screen_by_feature(meta_df, id, tickers, dates),
    "No features \\(beyond id, tickers, and dates\\) were selected"
  )
})
