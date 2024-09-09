# Define your test
test_that("Winsorize Data is running correctly.", {
  expect_equal(
    winsorize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
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
      Alpha = (c(0.05, 3.2, 9.95, 1, 7, 4.25, 1.95, 8.9, 9)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(760.5, 10.35, 4, 9.05, -2, 4, 10, -2.95, 2.1)))
  )
}
)

# Define your test
test_that("Winsorize Data is running correctly with dates_vector as Date.", {
  expect_equal(
    winsorize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
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
      Alpha = (c(0.05, 3.2, 9.95, 1, 7, 4.25, 1.95, 8.9, 9)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(760.5, 10.35, 4, 9.05, -2, 4, 10, -2.95, 2.1)))
  )
  
  
  expect_equal(
    winsorize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
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
      Alpha = (c(0.05, 3.2, 9.95, 1, 7, 4.25, 1.95, 8.9, 9)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(760.5, 10.35, 4, 9.05, -2, 4, 10, -2.95, 2.1)))
  )
}
)

test_that("Winsorize Data is running correctly when Infs to preserve is NULL.", {
  expect_equal(
    winsorize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025))
    ,
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
      Alpha = (c(0.05, 3.2, 9.95, 1, 7, 4.25, 1.95, 8.9, 9)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(760.5, 10.35, 4, 9.05, -2, 4, 10, -2.95, 2.1)))
  )
}
)

# Define your test
test_that("Winsorize Data is running correctly with different date format.", {
  expect_equal(
    winsorize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Beta")),
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
      Alpha = (c(0.05, 3.2, 9.95, 1, 7, 4.25, 1.95, 8.9, 9)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(760.5, 10.35, 4, 9.05, -2, 4, 10, -2.95, 2.1)))
  )
}
)


# Define your test
test_that("Winsorize Data is running correctly - Some NAs.", {
  expect_equal(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Delta")),
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
      Alpha = (c(0.05, 3.2, NA, 1, 7, 4.125, 1.95, 8.9, 8.875)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(NA, 10.35, 4, 9.025, -2, 4, 9.975, -2.95, 2.1)))
  )
}
)


# Define your test
test_that("Winsorize Data is running correctly - Some Infs - No preservation", {
  expect_equal(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, Inf, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(-Inf, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Delta")),
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
      Alpha = (c(0.05, 3.2, NA, 1, 7, 4.125, 1.95, 8.9, 8.875)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(NA, 10.35, 4, 9.025, -2, 4, 9.975, -2.95, 2.1)))
  )
}
)

# Define your test
test_that("Winsorize Data is running correctly - Some Infs - Preserve Alpha", {
  expect_equal(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, Inf, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(-Inf, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
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
      Alpha = (c(0.05, 3.2, 9, 1, 7, 4.25, 1.95, 8.9, 9)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(NA, 10.35, 4, 9.025, -2, 4, 9.975, -2.95, 2.1)))
  )
}
)

test_that("Winsorize Data is running correctly - Some Infs - Preserve Alpha and Gamma", {
  expect_equal(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, Inf, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(-Inf, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha", "Gamma")),
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
      Alpha = (c(0.05, 3.2, 9, 1, 7, 4.25, 1.95, 8.9, 9)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(9, 10.35, 4, 9, -2, 4, 9.95, -2.95, 2.1)))
  )
}
)


test_that("Winsorize Data is running correctly - Some Infs - Alpha only NA", {
  expect_equal(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(-Inf, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha", "Gamma")),
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
      Alpha = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA)),
      Beta = (c(4.05, 6.75, 4.95, 5, 2, 4, 5.95, -2.75, -1.7)),
      Gamma = (c(9, 10.35, 4, 9, -2, 4, 9.95, -2.95, 2.1)))
  )
}
)


# Define your test
test_that("Winsorize Data integrates correctly with panelize data.", {
  expect_equal(
    winsorize_panel_data(
      panelize_data(list(matrix(c(0,1,2,3,7,9,10,4,9), nrow=3, ncol=3),
                         matrix(c(4,5,6,7,2,-3,5,4,-2), nrow=3, ncol=3),
                         matrix(c(8,9,10,11,-2,-3,4,4,2), nrow=3, ncol=3),
                         matrix(c(3,7,9,8,-1,0,5,-2,0), nrow=3, ncol=3)),
                    c("Stock A", "Stock B", "Stock C"),
                    as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
                    c("Alpha", "Beta", "Gamma", "Delta")), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
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
      Alpha = (c(0.05, 3.20, 9.95, 1.00, 7.00, 4.25, 1.95, 8.90, 9.00)),
      Beta = (c(4.05, 6.75, 4.95, 5.00, 2.00, 4.00, 5.95, -2.75, -1.70)),
      Gamma = (c(8.05, 10.35, 4.00, 9.00, -2.00, 4.00, 9.95, -2.95, 2.10)),
      Delta = c(3.20,7.60,4.75,7.00,-0.95,-1.90,8.90,0.00,0.00))
  )
}
)


# Define your test
test_that("Winsorize Data throws an error when there columns are different.", {
  expect_error(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      ticker = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
    "features_df should have id, tickers and dates columns."
  )
}
)


# Define your test
test_that("Winsorize Data throws an error when there is an uncorrespondence in features_df$dates and dates_vector", {
  expect_error(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                  "Stock B", "Stock B", "Stock B",
                  "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2003-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
    "all dates in dates_vector must have a correspondence in features_df"
  )
  
  expect_error(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2001-04-16", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
    "all dates in dates_vector must have a correspondence in features_df"
  )
  
  expect_error(
    winsorize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.factor(c("2001-03-15", "2001-04-17", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
    "all dates in dates_vector must have a correspondence in features_df"
  )
  
}
)

# Define your test
test_that("Winsorize Data throws an error when features_df is not in right format", {
  expect_error(
    winsorize_panel_data(as.matrix(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2003-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))), 
      as.factor(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
    "features_df must be a data frame."
  )
})

# Define your test
test_that("Winsorize Data throws an error when dates_vector is not in right format", {
  expect_error(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2003-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.character(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975, 0.025),
      c("Alpha")),
    "dates_vector must be factor or date."
  )
  
  expect_error(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03", "2003-04", "2001-05",
                          "2001-03", "2001-04", "2001-05",
                          "2001-03", "2001-04", "2001-05")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03", "2001-04", "2001-05"), format = "%Y-%m"),
      c(0.975, 0.025),
      c("Alpha")),
    "dates_vector must be a date object with format %Y-%m-%d"
  )
  
  
})


# Define your test
test_that("Winsorize Data throws an error when probs is not in right format", {
  expect_error(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2003-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      as.character(c(0.975, 0.025)),
      c("Alpha")),
    "probs must be a numeric vector of length 2"
  )
  
  expect_error(
    winsorize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.factor(c("2001-03-15", "2003-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))), 
      as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
      c(0.975),
      c("Alpha")),
    "probs must be a numeric vector of length 2"
  )
  
})


# Define your test
test_that("winsorize_data integrates with external toy data - Excel Files", {
  
  #Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                 features_sheet_names = c("ebit_12m","ir_3m", "sharpe", "mkt_cap","sector_c1"),
                                 features_sheet_range = c("D4:F22"),
                                 tickers_sheet_range = c("C4:C22"),
                                 dates_sheet_range = c("D1:F1"),
                                 output_sheet_name = c("winsorized_panel"),
                                 output_sheet_range = c("B1:I58"),
                                 industry_classification_column_name = c("sector_c1"))
  #Apply function
  panel <- panelize_data(features_list = results$inputs$feature_list,
                         row_names = results$inputs$tickers$...1,
                         column_names  = results$inputs$dates,
                         features_names = results$inputs$features_names)
  
  winsorized_panel <- winsorize_panel_data(features_df = panel,
                                           dates_vector = results$inputs$dates,
                                           probs = c(0.975,0.025))
  
  
  
  
  # Apply the function to the test data
  expect_equal(winsorized_panel,
               results$outputs
  )
  
})

