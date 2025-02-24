# Define your test
test_that("create_meta_dataframe is running correctly.", {
  expect_equal(
    create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                  tickers = c("Stock A", "Stock B"),
                  dates = as.Date(c("2001-03-15", "2001-04-15")),
                  features_names = c("Alpha", "Beta", "Gamma"))@data,
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, 1, 3)),
      Beta = (c(4, 6 , 5, 7)),
      Gamma = (c(8, 10, 9, 11)))
  )
}
)

# Define your test
test_that("create_meta_dataframe is running correctly for a single date.", {
  expect_equal(
    create_meta_dataframe(list(matrix(c(0,1), nrow=2, ncol=1), matrix(c(NA,7), nrow=2, ncol=1), matrix(c(10,11), nrow=2, ncol=1)),
                          tickers = c("Stock A", "Stock B"),
                          dates = as.Date(c("2001-03-15")),
                          features_names = c("Alpha", "Beta", "Gamma"))@data,
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock B-2001-03-15")),
      tickers = (c("Stock A", "Stock B")),
      dates = as.Date(c("2001-03-15")),
      Alpha = (c(0, 1)),
      Beta = (c(NA, 7)),
      Gamma = (c( 10, 11)))
  )
}
)

test_that("create_meta_dataframe is running correctly with character data.frame.", {

  features_m_df <- create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), data.frame(c("e","c"),c("d","a")), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                                             c("Stock A", "Stock B"),
                                             as.Date(c("2001-03-15", "2001-04-15")),
                                             c("Alpha", "Beta", "Gamma"))@data

  expect_equal(features_m_df,
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, 1, 3)),
      Beta = (c("e", "d" , "c", "a")),
      Gamma = (c(8, 10, 9, 11)))
  )

  #Check for classes
  expect_equal(class(features_m_df$Alpha), "numeric")
  expect_equal(class(features_m_df$Beta), "character")
  expect_equal(class(features_m_df$Gamma), "numeric")

}
)

# Define your test
test_that("create_meta_dataframe is running correctly with data frames and tibbles.", {
  expect_equal(
    create_meta_dataframe(list(as.data.frame(matrix(c(0,1,2,3), nrow=2, ncol=2)), tibble::as_tibble(matrix(c(4,5,6,7), nrow=2, ncol=2), .name_repair = "unique"),
                       as.data.frame(matrix(c(8,9,10,11), nrow=2, ncol=2))),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma"))@data,
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, 1, 3)),
      Beta = (c(4, 6 , 5, 7)),
      Gamma = (c(8, 10, 9, 11)))
  )

  expect_equal(
    create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), as.data.frame(matrix(c(4,5,6,7), nrow=2, ncol=2)),
                       as.data.frame(matrix(c(8,9,10,11), nrow=2, ncol=2))),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma"))@data,
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, 1, 3)),
      Beta = (c(4, 6 , 5, 7)),
      Gamma = (c(8, 10, 9, 11)))
  )

}
)


# Define your test
test_that("create_meta_dataframe is running correctly - Some NAs.", {
  expect_equal(
    create_meta_dataframe(list(matrix(c(0,NA,2,3), nrow=2, ncol=2), matrix(c(4,5,NA,7), nrow=2, ncol=2), matrix(c(8,9,10,NA), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma"))@data,
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock B-2001-03-15", "Stock B-2001-04-15")),
      tickers = (c("Stock A", "Stock A", "Stock B", "Stock B")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-03-15", "2001-04-15")),
      Alpha = (c(0, 2, NA, 3)),
      Beta = (c(4, NA , 5, 7)),
      Gamma = (c(8, 10, 9, NA)))
  )
}
)

# Define your test
test_that("create_meta_dataframe is running correctly - Many Characteristics and Stocks", {
  expect_equal(
    create_meta_dataframe(list(matrix(c(0,1,2,3,7,9,10,4,9), nrow=3, ncol=3),
                       matrix(c(4,5,6,7,2,-3,5,4,-2), nrow=3, ncol=3),
                       matrix(c(8,9,10,11,-2,-3,4,4,2), nrow=3, ncol=3),
                       matrix(c(3,7,9,8,-1,0,5,-2,0), nrow=3, ncol=3)),
                  c("Stock A", "Stock B", "Stock C"),
                  as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
                  c("Alpha", "Beta", "Gamma", "Delta"))@data,
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
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
test_that("create_meta_dataframe throws an error when dimensions differ", {
  expect_error(
    create_meta_dataframe(list(matrix(c(0,NA,2,3,4,5), nrow=2, ncol=3), matrix(c(4,5,NA,7), nrow=2, ncol=2), matrix(c(8,9,10,NA), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    "All elements in the list must have the same number of columns."
  )
}
)


# Define your test
test_that("create_meta_dataframe throws an error when features_list is not a list", {
  expect_error(
    create_meta_dataframe(matrix(c(4,5,NA,7), nrow=2, ncol=2),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma"))
  )
}
)

# Define your test
test_that("create_meta_dataframe throws an error when one of list objects is not DF or Matrix", {
  expect_error(
    create_meta_dataframe(list(c(0,NA,2,3,4,5), matrix(c(4,5,NA,7), nrow=2, ncol=2), matrix(c(8,9,10,NA), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta", "Gamma")),
    "All elements of the list must be matrices, data frames, or tibbles."
  )
}
)

# Define your test
test_that("create_meta_dataframe throws an error when tickers length does not match number of rows in each matrix of list", {
expect_error(
  create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                c("Stock A", "Stock B", "Ronaldo"),
                as.Date(c("2001-03-15", "2001-04-15")),
                c("Alpha", "Beta", "Gamma")),
  "The length of tickers must equal the number of rows in each element of the list."
)
})

# Define your test
test_that("create_meta_dataframe throws an error when colnames length does not match number of columns in each matrix of list", {
  expect_error(
    create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
                  c("Alpha", "Beta", "Gamma")),
    "The length of dates must equal the number of columns in each element of the list."
  )
})

# Define your test
test_that("create_meta_dataframe throws an error when length of features_names does not match number of elements in features_list", {
  expect_error(
    create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                  c("Stock A", "Stock B"),
                  as.Date(c("2001-03-15", "2001-04-15")),
                  c("Alpha", "Beta")),
    "The length of features_names must equal the number of elements in the list."
  )
})


# Define your test
test_that("create_meta_dataframe throws an error when there are different number of rows in matrices",{
expect_error(
  create_meta_dataframe(list(matrix(1:4, nrow = 2), matrix(5:7, nrow = 3)),
                c("Stock A", "Stock B"), as.Date(c("2001-03-15", "2001-04-15")), c("Alpha", "Beta")),
  "All elements in the list must have the same number of rows."
)
})

# Define your test
test_that("create_meta_dataframe throws an error when there is a problem with tickers column",{
  expect_error(
    create_meta_dataframe(list(matrix(1:6, nrow = 3), data.frame(tickers = c("Stock A", "Stock B", "Stock C"), Alpha = c(1,2,3))),
                          c("Stock A", "Stock B", "Stock C"), as.Date(c("2001-03-15", "2001-04-15")), c("Alpha", "Beta")),
    "One or more datasets already contain a column named 'tickers' or 'dates'."
  )

  expect_error(
    create_meta_dataframe(list(matrix(1:6, nrow = 3), data.frame(random_col = c("Stock A", "Stock B", "Stock C"), Alpha = c(1,2,3))),
                          c("Stock A", "Stock B", "Stock C"), as.Date(c("2001-03-15", "2001-04-15")), c("Alpha", "Beta")),
    "One or more datasets contain values in their columns that match provided tickers or dates."
  )

  expect_error(
    create_meta_dataframe(list(matrix(1:6, nrow = 3), data.frame(Zeta = c(1,2,3), Alpha = c(1,2,3))),
                          c(1, 2, 3), as.Date(c("2001-03-15", "2001-04-15")), c("Alpha", "Beta")),
    "tickers must be a character vector."
  )

  expect_error(
    create_meta_dataframe(list(matrix(1:6, nrow = 3), data.frame(Zeta = c(1,2,3), Alpha = c(1,2,3))),
                          c("Stock A", "Stock A", "Stock B"), as.Date(c("2001-03-15", "2001-04-15")), c("Alpha", "Beta")),
    "tickers must be unique."
  )

})


test_that("create_meta_dataframe throws an error when dates are wrong", {
  expect_error(
    create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                          tickers = c("Stock A", "Stock B"),
                          dates = as.Date(c("2001-03-15", "2001-06-15")),
                          features_names = c("Alpha", "Beta", "Gamma")),
    "Dates must be consecutive by month."
  )

  expect_error(
    create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                          tickers = c("Stock A", "Stock B"),
                          dates = as.Date(c("2001-03-15", "2001-04-16")),
                          features_names = c("Alpha", "Beta", "Gamma")),
    "All dates must have the same day."
  )

  expect_error(
    create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                          tickers = c("Stock A", "Stock B"),
                          dates = as.Date(c("2001-03-15", "2001-03-15")),
                          features_names = c("Alpha", "Beta", "Gamma")),
    "dates must be unique."
  )

  expect_error(
    create_meta_dataframe(list(matrix(c(0,1,2,3), nrow=2, ncol=2), matrix(c(4,5,6,7), nrow=2, ncol=2), matrix(c(8,9,10,11), nrow=2, ncol=2)),
                          tickers = c("Stock A", "Stock B"),
                          dates = c("2001-03-15", "2001-03-15"),
                          features_names = c("Alpha", "Beta", "Gamma")),
    "dates must be in Date format."
  )

})

test_that("create_meta_dataframe throws an error when df contains only NA", {
  expect_error(
    create_meta_dataframe(list(matrix(c(NA,NA,NA,NA), nrow=2, ncol=2), matrix(c(4,5,NA,7), nrow=2, ncol=2), matrix(c(8,9,10,NA), nrow=2, ncol=2)),
                          c("Stock A", "Stock B"),
                          as.Date(c("2001-03-15", "2001-04-15")),
                          c("Alpha", "Beta", "Gamma")),
    "One or more datasets contain only NA values."
    )

  expect_error(
    create_meta_dataframe(list(matrix(c(NA,NA,NA,NA), nrow=2, ncol=2), matrix(c(NA,NA,NA,NA), nrow=2, ncol=2), matrix(c(8,9,10,NA), nrow=2, ncol=2)),
                          c("Stock A", "Stock B"),
                          as.Date(c("2001-03-15", "2001-04-15")),
                          c("Alpha", "Beta", "Gamma")),
    "One or more datasets contain only NA values."
  )

})

# Define your test
test_that("create_meta_dataframe works with external toy data - Excel Files", {

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
                         features_names = results$inputs$features_names)@data


  results$outputs$dates <- as.Date(results$outputs$dates)

  # Apply the function to the test data
  expect_equal(panel,
               results$outputs
  )

})




