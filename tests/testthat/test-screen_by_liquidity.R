test_that("screen_by_liquidity correctly removes stocks below liquidity floor rule", {

  # Create liquidity floor cutoffs
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  # Create liquidity_m_df as a meta_dataframe
  liquidity_m_df_data <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15", "Stock A-2020-06-15", "Stock B-2020-06-15", "Stock C-2020-06-15"),
    tickers = c("Stock A", "Stock B", "Stock C", "Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15", "2020-06-15", "2020-06-15", "2020-06-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 25500, 1000, 6000, 25500),
    presence = c(95, 98, 100, 100, 100, 100)
  ) %>% dplyr::arrange(id)

  liquidity_m_df <- create_meta_dataframe(
    liquidity_m_df_data,
    meta_dataframe_name = "liquidity_meta_df"
  )

  # Create meta_dataframe
  meta_dataframe_data <- liquidity_m_df_data %>% dplyr::select(id, tickers, dates)
  meta_dataframe <- create_meta_dataframe(
    meta_dataframe_data,
    meta_dataframe_name = "test_meta_df"
  )

  # Define liquidity floor rule
  liquidity_floor_rule <- "small_caps"

  # Expected results after screening
  expected_results <- liquidity_m_df_data %>%
    classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_floor_rule = liquidity_floor_rule,
                             apply_liquidity_floor_rule = TRUE, filter_out_liquidity_floor_rule = TRUE
    )

  # Expected filtered meta_dataframe
  expected_meta_df <- meta_dataframe@data %>% dplyr::filter(id %in% expected_results$id)

  # Apply screening
  screened_meta_df <- screen_by_liquidity(
    meta_dataframe = meta_dataframe,
    liquidity_m_df = liquidity_m_df,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs,
    liquidity_floor_rule = liquidity_floor_rule,
    verbose = TRUE
  )

  # Check that filtered data is correct
  expect_equal(screened_meta_df@data, expected_meta_df)

  # Check that metadata is preserved
  expect_equal(screened_meta_df@meta_dataframe_name, meta_dataframe@meta_dataframe_name)
  expect_equal(screened_meta_df@current_date, meta_dataframe@current_date)
  expect_true(length(screened_meta_df@workflow) > length(meta_dataframe@workflow))

})

test_that("screen_by_liquidity throws an error if liquidity_m_df has NAs", {

  liquidity_m_df_data <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15"),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2020-05-15", "2020-05-15")),
    mean_volfin_3m = c(1000, 5000),
    presence = c(90, NA)  # NA values should trigger an error
  )

  liquidity_m_df <- create_meta_dataframe(
    liquidity_m_df_data,
    meta_dataframe_name = "liquidity_meta_df"
  )

  meta_dataframe <- create_meta_dataframe(
    liquidity_m_df_data %>% dplyr::select(id, tickers, dates),
    meta_dataframe_name = "test_meta_df"
  )

  # Create liquidity floor cutoffs
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  expect_error(
    screen_by_liquidity(
      meta_dataframe = meta_dataframe,
      liquidity_m_df = liquidity_m_df,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs,
      liquidity_floor_rule = "small_caps"
    ),
    "liquidity_m_df should contain only numeric columns with non-NAs."
  )
})

test_that("screen_by_liquidity throws an error if meta_dataframe has IDs missing from liquidity_m_df", {

  # Create meta_dataframe with an extra ID that is missing in liquidity_m_df
  meta_dataframe_data <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock X-2020-05-15"),  # Stock X is missing in liquidity_m_df
    tickers = c("Stock A", "Stock B", "Stock X"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"))
  )

  meta_dataframe <- create_meta_dataframe(
    meta_dataframe_data,
    meta_dataframe_name = "test_meta_df"
  )

  # Create liquidity floor cutoffs
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  liquidity_m_df_data <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15"),  # Stock X is missing
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2020-05-15", "2020-05-15")),
    mean_volfin_3m = c(1000, 5000),
    presence = c(97.5, 99)
  )

  liquidity_m_df <- create_meta_dataframe(
    liquidity_m_df_data,
    meta_dataframe_name = "liquidity_meta_df"
  )

  expect_error(
    screen_by_liquidity(
      meta_dataframe = meta_dataframe,
      liquidity_m_df = liquidity_m_df,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs,
      liquidity_floor_rule = "small_caps"
    ),
    "all ids from meta_dataframe should be present in liquidity_m_df"
  )
})

test_that("screen_by_liquidity throws an error if liquidity_m_df contains normalized values", {

  liquidity_m_df_data <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15"),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2020-05-15", "2020-05-15")),
    mean_volfin_3m = c(0.5, 0.7),  # Normalized values should trigger an error
    presence = c(0.9, 1.0)
  )

  liquidity_m_df <- create_meta_dataframe(
    liquidity_m_df_data,
    meta_dataframe_name = "liquidity_meta_df"
  )

  meta_dataframe <- create_meta_dataframe(
    liquidity_m_df_data %>% dplyr::select(id, tickers, dates),
    meta_dataframe_name = "test_meta_df"
  )

  # Create liquidity floor cutoffs
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  expect_error(
    screen_by_liquidity(
      meta_dataframe = meta_dataframe,
      liquidity_m_df = liquidity_m_df,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs,
      liquidity_floor_rule = "small_caps"
    ),
    "values in liquidity_m_df should not be normalized"
  )
})

test_that("screen_by_liquidity throws error when all stocks are filtered out", {

  liquidity_m_df_data <- data.frame(
    id = c("Stock A-2020-05-15"),
    tickers = c("Stock A"),
    dates = as.Date(c("2020-05-15")),
    mean_volfin_3m = c(500),  # Below the lowest cutoff
    presence = c(95)
  )

  liquidity_m_df <- create_meta_dataframe(
    liquidity_m_df_data,
    meta_dataframe_name = "liquidity_meta_df"
  )

  meta_dataframe <- create_meta_dataframe(
    liquidity_m_df_data %>% dplyr::select(id, tickers, dates),
    meta_dataframe_name = "test_meta_df"
  )

  # Create liquidity floor cutoffs
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  expect_error(
    screen_by_liquidity(
    meta_dataframe = meta_dataframe,
    liquidity_m_df = liquidity_m_df,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs,
    liquidity_floor_rule = "micro_caps",
    verbose = TRUE
  ), "All stocks were filtered out. Please check liquidity_floor_cutoffs and liquidity_floor_rule."
  )

})


test_that("screen_by_liquidity throws an error if liquidity_floor_rule is not in liquidity_floor_cutoffs", {

  liquidity_m_df_data <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15"),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2020-05-15", "2020-05-15")),
    mean_volfin_3m = c(1000, 5000),
    presence = c(97.5, 99)
  )

  liquidity_m_df <- create_meta_dataframe(
    liquidity_m_df_data,
    meta_dataframe_name = "liquidity_meta_df"
  )

  meta_dataframe <- create_meta_dataframe(
    liquidity_m_df_data %>% dplyr::select(id, tickers, dates),
    meta_dataframe_name = "test_meta_df"
  )

  # Create liquidity floor cutoffs
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  expect_error(
    screen_by_liquidity(
      meta_dataframe = meta_dataframe,
      liquidity_m_df = liquidity_m_df,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs,
      liquidity_floor_rule = "nano_caps"  # Not present in liquidity_floor_cutoffs
    ),
    "liquidity_floor_rule not present in liquidity_floor_cutoffs"
  )
})




test_that("screen_by_liquidity throws an current_date does not match", {

  liquidity_m_df_data <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15"),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2020-05-15", "2020-05-15")),
    mean_volfin_3m = c(1000, 5000),
    presence = c(97.5, 99)
  )

  liquidity_m_df <- create_meta_dataframe(
    liquidity_m_df_data,
    meta_dataframe_name = "liquidity_meta_df"
  )

  meta_dataframe <- create_meta_dataframe(
    liquidity_m_df_data %>% dplyr::select(id, tickers, dates),
    meta_dataframe_name = "test_meta_df"
  )

  # Create liquidity floor cutoffs
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  liquidity_m_df@current_date <- as.Date("2023-02-15")

  expect_error(
    screen_by_liquidity(
      meta_dataframe = meta_dataframe,
      liquidity_m_df = liquidity_m_df,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs,
      liquidity_floor_rule = "micro_caps"
    ),
    "current_date of meta_dataframe and liquidity_m_df must match"
  )
})
