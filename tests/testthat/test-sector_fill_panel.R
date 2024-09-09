# Define your test
test_that("sector_fill_panel is running correctly with 2 sectors", {
  expect_equal(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1, 7, 4, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)

# Define your test
test_that("sector_fill_panel is running correctly with Infs", {
  expect_equal(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 0, NA, Inf, 7, 4, 2, 9, 9)),
      Beta = (c(-Inf, 7, Inf, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(Inf, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(Inf, 0, 4, Inf, 7, 4, 2, 9, 9)),
      Beta = (c(-Inf, 7, Inf, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(Inf, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)


# Define your test
test_that("sector_fill_panel is running correctly with different date format", {
  expect_equal(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1, 7, 4, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
  
  expect_equal(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                        "2001-03-15", "2001-04-15", "2001-05-15",
                        "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                        "2001-03-15", "2001-04-15", "2001-05-15",
                        "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1, 7, 4, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
  
  
  
}
)

test_that("sector_fill_panel is running correctly given 1 Sector and NA", {
  expect_equal(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c(NA, NA, NA,
                     NA, NA, NA,
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c(NA, NA, NA,
                     NA, NA, NA,
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NaN, NaN, NaN, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)



# Define your test
test_that("sector_fill_panel is running correctly with a big frame", {
  expect_equal(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15",
              "Stock D-2001-03-15", "Stock D-2001-04-15", "Stock D-2001-05-15",
              "Stock E-2001-03-15", "Stock E-2001-04-15", "Stock E-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C",
                   "Stock D", "Stock D", "Stock D",
                   "Stock E", "Stock E", "Stock E")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9, 5, -2, NA, NA, 3,-1)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2, NA, NA, 5, 2, -9, 3)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2,-9, 5, 2, NA, 1, -500))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15",
              "Stock D-2001-03-15", "Stock D-2001-04-15", "Stock D-2001-05-15",
              "Stock E-2001-03-15", "Stock E-2001-04-15", "Stock E-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C",
                   "Stock D", "Stock D", "Stock D",
                   "Stock E", "Stock E", "Stock E")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1, 7, 4, 1, 7, 4, 2, 9, 9, 5, -2, 4, 3.5, 3,-1)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2, 4, -6, 5, 2, -9, 3)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2,-9, 5, 2, 0.5, 1, -500)))
  )
}
)


# Define your test
test_that("sector_fill_panel correctly preserves Alpha", {
  expect_equal(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1"),
      features_to_preserve = c("Alpha")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)


# Define your test
test_that("sector_fill_panel is running correctly - All NAs .", {
  expect_equal(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)

# Define your test
test_that("sector_fill integrates with all other panel functions.", {
  expect_equal(
    sector_fill_panel(industry_unavaiable_feature_fill(normalize_panel_data(winsorize_panel_data(
      panelize_data(list(matrix(c(0,NA,2,3,7,9,10,4,9), nrow=3, ncol=3),
                         matrix(c(4,5,6,7,2,-3,5,4,-2), nrow=3, ncol=3),
                         matrix(c(8,9,10,NA,-2,-3,4,4,2), nrow=3, ncol=3),
                         matrix(c(NA,7,9,NA,-1,0,NA,-2,0), nrow=3, ncol=3),
                         matrix(c("Intermediários financeiros","Setor 1","Previdência e Seguros",
                                  "Intermediários financeiros","Setor 1","Previdência e Seguros",
                                  "Intermediários financeiros","Setor 1","Previdência e Seguros"), nrow=3, ncol=3)),
                    c("Stock A", "Stock B", "Stock C"),
                    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
                    c("Alpha", "Beta", "Gamma", "Delta", "segment")), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15"))),
      unavaiable_feature = c("Delta"),
      similar_features = c("Alpha", "Beta"),
      industry_classification_column_name = c("segment"),
      selected_industries = c("Intermediários financeiros")),
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c("segment")),
    
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
      Alpha = (c(-1.0000000, -1.0000000, 1.0000000, NA, 0.33333333, -1.0000000, 1.0000000, 1.0000000, 0.66666667)),
      Beta = (c(-1.0000000, 1.0000000, 1.0000000, 0.0000000, 0.0000000, 0.71428571, 1.0000000, -1.0000000, -1.0000000)),
      Gamma = (c(-1, NA, 1, 0, 1, 1, 1, -1, -1)),
      Delta = (c(-1, 0, 1, -1, -1, -1, 1, 1, 1)),
      segment = c("Intermediários financeiros","Intermediários financeiros", "Intermediários financeiros",
                  "Setor 1","Setor 1", "Setor 1", 
                  "Previdência e Seguros", "Previdência e Seguros", "Previdência e Seguros"))
  )
  
  
  expect_equal(
    sector_fill_panel(
      industry_unavaiable_feature_fill(
      normalize_panel_data(
      winsorize_panel_data(
      panelize_data(list(matrix(c(0,NA,2,3,7,9,10,4,9), nrow=3, ncol=3),
                         matrix(c(4,5,6,7,2,-3,5,4,-2), nrow=3, ncol=3),
                         matrix(c(8,9,10,NA,-2,-3,4,4,2), nrow=3, ncol=3),
                         matrix(c(NA,7,NA,NA,-1,NA,NA,-2,NA), nrow=3, ncol=3),
                         matrix(c("Intermediários financeiros","Setor 1","Intermediários financeiros",
                                  "Intermediários financeiros","Setor 1","Intermediários financeiros",
                                  "Intermediários financeiros","Setor 1","Intermediários financeiros"), nrow=3, ncol=3)),
                    c("Stock A", "Stock B", "Stock C"),
                    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
                    c("Alpha", "Beta", "Gamma", "Delta", "segment")), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15"))),
      unavaiable_feature = c("Delta"),
      similar_features = c("Alpha", "Beta"),
      industry_classification_column_name = c("segment"),
      selected_industries = c("Intermediários financeiros")),
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c("segment")),
    
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
      Alpha = (c(-1.0000000, -1.0000000, 1.0000000, NaN, 0.33333333, -1.0000000, 1.0000000, 1.0000000, 0.66666667)),
      Beta = (c(-1.0000000, 1.0000000, 1.0000000, 0.0000000, 0.0000000, 2*((4-min(c(4,4.95,-1.70)))/(max(c(4,4.95,-1.70))-min(c(4,4.95,-1.70))))-1 ,
                1.0000000, -1.0000000, -1.0000000)),
      Gamma = (c(-1, -1, 1, 0, 1, 1, 1, -1, -1)),
      Delta = (c(-1, 0, 1, NaN, NaN, NaN, 1, 0, -0.16666667)),
      segment = c("Intermediários financeiros","Intermediários financeiros", "Intermediários financeiros",
                  "Setor 1","Setor 1", "Setor 1", 
                  "Intermediários financeiros","Intermediários financeiros", "Intermediários financeiros"))
  ,
  tolerance = 1e-7)
  
})

# Define your test
test_that("sector_fill_panel throws an error when when features_df is not in right format", {
  expect_error(
    sector_fill_panel(as.matrix(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    "features_df must be a data frame."
  )
  
  expect_error(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      ticker = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    "features_df should have id, tickers and dates columns."
  )
  
}
)

# Define your test
test_that("sector_fill_panel throws an error when dates_vector not in right format.", {
  expect_error(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.character(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    "dates_vector must be factor or date."
  )
  
  
  expect_error(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03", "2001-04", "2001-05",
                          "2001-03", "2001-04", "2001-05",
                          "2001-03", "2001-04", "2001-05")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03", "2001-04", "2001-05"), format = "%Y-%m-%d"),
      industry_classification_column_name = c("sectors_c1")),
    "dates_vector must be a date object with format %Y-%m-%d"
  )
}
)

# Define your test
test_that("sector_fill_panel throws an error when industry_classification_column is not in features_df", {
  expect_error(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c3 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    "industry_classification_column is not present in features_df"
  )
}
)

# Define your test
test_that("sector_fill_panel throws an error when industry_classification_column is not in right format", {
  expect_error(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c(0.25)),
    "industry_classification_column_name must be a character."
  )
}
)

# Define your test
test_that("sector_fill_panel throws an error when dates in dates_vector do not match features_df$dates", {
  expect_error(
    sector_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors_c1 = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-09-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      industry_classification_column_name = c("sectors_c1")),
    "all dates in dates_vector must have a correspondence in features_df"
  )
})


# Define your test
test_that("sector_fill integrates with external toy data - Excel Files", {
  
  #Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                 features_sheet_names = c("ebit_12m","ir_3m", "sharpe", "mkt_cap","sector_c1"),
                                 features_sheet_range = c("D4:F22"),
                                 tickers_sheet_range = c("C4:C22"),
                                 dates_sheet_range = c("D1:F1"),
                                 output_sheet_name = c("sector_filled_panel"),
                                 output_sheet_range = c("B1:I58"),
                                 industry_classification_column_name = c("sector_c1"))
  #Apply functions
  panel <- panelize_data(features_list = results$inputs$feature_list,
                         row_names = results$inputs$tickers$...1,
                         column_names  = results$inputs$dates,
                         features_names = results$inputs$features_names)
  
  winsorized_panel <- winsorize_panel_data(features_df = panel,
                                           dates_vector = results$inputs$dates,
                                           probs = c(0.975,0.025))
  
  normalized_panel <- normalize_panel_data(features_df = winsorized_panel,
                                           dates_vector = results$inputs$dates)
  
  
  normalized_panel_banks_filled <- industry_unavaiable_feature_fill(features_df = normalized_panel,
                                                                    unavaiable_feature = c("ebit_12m"),
                                                                    similar_features = c("ir_3m", "sharpe"),
                                                                    industry_classification_column_name = c("sector_c1"),
                                                                    selected_industries = c("Bancos e Serviços Financeiros"))
  
  sector_filled_panel <- sector_fill_panel(features_df = normalized_panel_banks_filled,
                                           dates_vector = results$inputs$dates,
                                           industry_classification_column_name = c("sector_c1"))
  #Change NaN to NA
  sector_filled_panel[,4][which(is.nan(sector_filled_panel[,4]))] <- NA
  sector_filled_panel[,5][which(is.nan(sector_filled_panel[,5]))] <- NA
  sector_filled_panel[,6][which(is.nan(sector_filled_panel[,6]))] <- NA
  sector_filled_panel[,7][which(is.nan(sector_filled_panel[,7]))] <- NA
  
  
  
  
  # Apply the function to the test data
  expect_equal(sector_filled_panel,
               results$outputs
  )
  
})


