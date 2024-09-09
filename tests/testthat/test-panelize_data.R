# Define your test
test_that("Panelize Data is running correctly.", {
  expect_equal(
    panelize_data(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, 1, 3)),
      Beta = (c(4, 6 , 5, 7)),
      Gamma = (c(8, 10, 9, 11)))
  )
}
)

# Define your test
test_that("Panelize Data is running correctly with data frames.", {
  expect_equal(
    panelize_data(list(as.data.frame(matrix(c(0,1,2,3), nrow=2, ncol=2)), as.data.frame(matrix(c(4,5,6,7), nrow=2, ncol=2)),
                       as.data.frame(matrix(c(8,9,10,11), nrow=2, ncol=2))),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, 1, 3)),
      Beta = (c(4, 6 , 5, 7)),
      Gamma = (c(8, 10, 9, 11)))
  )
  
  expect_equal(
    panelize_data(list(matrix(c(0,1,2,3), nrow=2, ncol=2), as.data.frame(matrix(c(4,5,6,7), nrow=2, ncol=2)),
                       as.data.frame(matrix(c(8,9,10,11), nrow=2, ncol=2))),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, 1, 3)),
      Beta = (c(4, 6 , 5, 7)),
      Gamma = (c(8, 10, 9, 11)))
  )
  
}
)


# Define your test
test_that("Panelize Data is running correctly - Some NAs.", {
  expect_equal(
    panelize_data(list(matrix(c(0,NA,2,3), nrow=2, ncol=2), matrix(c(4,5,NA,7), nrow=2, ncol=2), matrix(c(8,9,10,NA), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, NA, 3)),
      Beta = (c(4, NA , 5, 7)),
      Gamma = (c(8, 10, 9, NA)))
  )
}
)

# Define your test
test_that("Panelize Data is running correctly - Many Characteristics and Stocks", {
  expect_equal(
    panelize_data(list(matrix(c(0,1,2,3,7,9,10,4,9), nrow=3, ncol=3),
                       matrix(c(4,5,6,7,2,-3,5,4,-2), nrow=3, ncol=3),
                       matrix(c(8,9,10,11,-2,-3,4,4,2), nrow=3, ncol=3),
                       matrix(c(3,7,9,8,-1,0,5,-2,0), nrow=3, ncol=3)),
                  c("Stock A", "Stock B", "Stock C"),
                  as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
                  c("Alpha", "Beta", "Gamma", "Delta")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(8, 11, 4, 9, -2, 4, 10, -3, 2)),
      Delta = (c(3, 8, 5, 7, -1, -2, 9, 0, 0)))
  )
}
)


# Define your test
test_that("Panelize Data throws an error when dimensions differ", {
  expect_error(
    panelize_data(list(matrix(c(0,NA,2,3,4,5), nrow=2, ncol=3), matrix(c(4,5,NA,7), nrow=2, ncol=2), matrix(c(8,9,10,NA), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    "Input must be a list of matrices/data.frame with same dimension"
  )
}
)


# Define your test
test_that("Panelize Data throws an error when features_list is not a list", {
  expect_error(
    panelize_data(matrix(c(4,5,NA,7), nrow=2, ncol=2),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    "Input must be a list of matrices/data.frame with same dimension"
  )
}
)

# Define your test
test_that("Panelize Data throws an error when one of list objects is not DF or Matrix", {
  expect_error(
    panelize_data(list(c(0,NA,2,3,4,5), matrix(c(4,5,NA,7), nrow=2, ncol=2), matrix(c(8,9,10,NA), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    "Input must be a list of matrices/data.frame with same dimension"
  )
}
)

# Define your test
test_that("Panelize Data throws an error when rownames length does not match number of rows in each matrix of list", {
expect_error(
  panelize_data(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                c("Stock A", "Stock B", "Ronaldo"),
                as.Date(c("2001-03-15", "2001-04-15")),
                c("Alpha", "Beta", "Gamma")),
  "Input must be a list of matrices/data.frame with same dimension"
)
})

# Define your test
test_that("Panelize Data throws an error when colnames length does not match number of columns in each matrix of list", {
  expect_error(
    panelize_data(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15", "Ronaldo")),
                  c("Alpha", "Beta", "Gamma")),
    "Input must be a list of matrices/data.frame with same dimension"
  )
})

# Define your test
test_that("Panelize Data throws an error when length of features_names does not match number of elements in features_list", {
  expect_error(
    panelize_data(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15", "Ronaldo")),
                  c("Alpha", "Beta", "Gamma")),
    "Input must be a list of matrices/data.frame with same dimension"
  )
})


# Define your test
test_that("panelize_data throws an error when there are different number of rows in matrices",{
expect_error(
  panelize_data(list(matrix(1:4, nrow = 2), matrix(5:7, nrow = 3)),
                c("Stock A", "Stock B"), as.Date(c("2001-03-15", "2001-04-15")), c("Alpha", "Beta")),
  "Input must be a list of matrices/data.frame with same dimension"
)
})

# Define your test
test_that("panelize_data works with external toy data - Excel Files", {
  
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
  panel <- panelize_data(features_list = results$inputs$feature_list,
                         row_names = results$inputs$tickers$...1,
                         column_names  = results$inputs$dates,
                         features_names = results$inputs$features_names)
  
                         
    

  # Apply the function to the test data
  expect_equal(panel,
               results$outputs
  )
  
})




