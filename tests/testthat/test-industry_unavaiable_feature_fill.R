# Define your test
test_that("Unavaiable feature is running correctly with 2 similar features", {
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(402, 9, 4.5, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)

# Define your test
test_that("Unavaiable feature is running correctly with dates_vector as Date", {
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(402, 9, 4.5, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)


# Define your test
test_that("Unavaiable feature is running correctly with 1 similar features", {
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(4, 7, 5, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)



# Define your test
test_that("Unavaiable feature is running correctly with 2 similar features and many NAs on them", {
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta","Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(800, 7, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2)))
  )
}
)

# Define your test
test_that("Unavaiable feature is running correctly when there are NAs for other sectors too", {
  #One similar feature
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, NA, 4, 2, 9, NA)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(800, NA, NA, 1, NA, 4, 2, 9, NA)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2)))
  )
  
  #Two similar features
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, NA, 4, 2, 9, NA)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta","Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(800, 7, NaN, 1, NA, 4, 2, 9, NA)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2)))
  )
  
  
}
)

test_that("Unavaiable feature is running correctly when there are 2 selected industries", {
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, 2, 9, NA)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1", "Setor 2")
    ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(800, NA, NA, 9, -2, 4, 2, 9, NA)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2)))
  )
})  

# Define your test
test_that("Unavaiable feature is running correctly in a big frame", {
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
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
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, 2, 9, 9, 5, -2, NA, NA, 3,-1)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2, NA, NA, 5, 2, -9, 3)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2,-9, 5, 2, NA, 1, -500))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
      ),
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
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 2", "Setor 2", "Setor 2",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(402, 9, 4.5, 7, 0, 4, 2, 9, 9, 5, -2, NA, NA, 3,-1)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2, NA, NA, 5, 2, -9, 3)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2,-9, 5, 2, NA, 1, -500)))
  )
}
)


# Define your test
test_that("unavaiable_feature is running correctly - All NAs .", {
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
      ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(402, 9, 4.5, 7, 0, 4, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)

# Define your test
test_that("unavaiable_feature is running correctly - All NAs, but one .", {
  expect_equal(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, 7, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
      ),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                     "Setor 1", "Setor 1", "Setor 1",
                     "Setor 2", "Setor 2", "Setor 2"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(402, 9, 4.5, 7, 0, 4, 7, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)

# Define your test
test_that("unavaiable_feature throws an error when there are non-NAs inputs in a given sector", {
  #For 1 sector
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 2, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "unavaiable_feature is not unavaiable across all entries in selected industry"
  )
  
  #For 2 sectors
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, 4, 2, 9, NA)),
      Beta = (c(NA, 7, NA, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, NA, NA, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1", "Setor 2")
    ), "unavaiable_feature is not unavaiable across all entries in selected industry")
})


# Define your test
test_that("unavaiable_feature throws an error when features_df does not have right format", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      ticker = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 2, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "features_df should have id, tickers and dates columns."
  )
}
)



# Define your test
test_that("unavaiable_feature throws an error when features_df does not have industry_classification col", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                  "Stock B", "Stock B", "Stock B",
                  "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 2, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "industry_classification_column is not present in features_df"
  )
}
)

# Define your test
test_that("unavaiable_feature throws an error when features_df does not contain unavaiable_feature", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                  "Stock B", "Stock B", "Stock B",
                  "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 2, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Iota"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "unavaiable features must be present in features_df"
  )
}
)


# Define your test
test_that("unavaiable_feature throws an error when features_df does not contain similar_features", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                  "Stock B", "Stock B", "Stock B",
                  "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 2, NA, 1, 7, 4, 2, 9, 9)),
      Iota = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "similar features must be present in features_df"
  )
}
)

# Define your test
test_that("unavaiable_feature throws an error when features_df does not contain similar_features", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1, 2, 0, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "No NA to fill"
  )
}
)





# Define your test
test_that("Unavaiable feature throws an error when features_df is not in right format", {
  expect_error(
    industry_unavaiable_feature_fill(as.matrix(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "features_df must be a data frame.")
}
)


# Define your test
test_that("Unavaiable feature throws an error when unavaiable_feature is not in right format", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c(0.75),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "unavaiable_feature must be a character.")
}
)

# Define your test
test_that("Unavaiable feature throws an error when industry_classification_column_name is not in right format", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c(0.2),
      selected_industries = c("Setor 1")
    ),
    "industry_classification_column_name must be a character.")
}
)


# Define your test
test_that("Unavaiable feature throws an error when similar_features is not in right format", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c(0.10),
      c("sectors"),
      selected_industries = c("Setor 1")
    ),
    "similar_features must be a character.")
}
)



# Define your test
test_that("Unavaiable feature throws an error when selected_industries is not in right format", {
  expect_error(
    industry_unavaiable_feature_fill(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      sectors = c("Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2",
                  "Setor 3", "Setor 3", "Setor 3"),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      c("Alpha"),
      c("Beta", "Gamma"),
      c("sectors"),
      selected_industries = c(0.25)
    ),
    "selected_industries must be a character.")
}
)


# Define your test
test_that("ind_unavaible_feature integrates with external toy data - Excel Files", {
  
  #Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                 features_sheet_names = c("ebit_12m","ir_3m", "sharpe", "mkt_cap","sector_c1"),
                                 features_sheet_range = c("D4:F22"),
                                 tickers_sheet_range = c("C4:C22"),
                                 dates_sheet_range = c("D1:F1"),
                                 output_sheet_name = c("normalized_panel_banks_filled"),
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
  
  
  
  
  # Apply the function to the test data
  expect_equal(normalized_panel_banks_filled,
               results$outputs
  )
  
})


