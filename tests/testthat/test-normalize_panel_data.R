# Define your test
test_that("Normalize Data is running correctly.", {
  expect_equal(
    normalize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))))@data,
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
      Alpha = (c(
        2*((0-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((3-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((10-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((1-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((7-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((4-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((2-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((9-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((9-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1)),

      Beta = (c(
        2*((4-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((7-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((5-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((5-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((2-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((6-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((-3-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((-2-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1)),


      Gamma = (c(
        2*((800-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((11-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((9-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((-2-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((10-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((-3-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((2-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1))
    )
  )
}
)

# Define your test
test_that("Normalize Data is running correctly with dates_vector as Date.", {
  expect_equal(
    normalize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))))@data,
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
      Alpha = (c(
        2*((0-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((3-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((10-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((1-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((7-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((4-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((2-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((9-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((9-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1)),

      Beta = (c(
        2*((4-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((7-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((5-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((5-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((2-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((6-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((-3-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((-2-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1)),


      Gamma = (c(
        2*((800-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((11-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((9-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((-2-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((10-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((-3-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((2-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1))
    )
  )

  expect_equal(
    normalize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))))@data,
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
      Alpha = (c(
        2*((0-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((3-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((10-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((1-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((7-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((4-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((2-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((9-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((9-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1)),

      Beta = (c(
        2*((4-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((7-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((5-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((5-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((2-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((6-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((-3-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((-2-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1)),


      Gamma = (c(
        2*((800-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((11-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((9-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((-2-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((10-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((-3-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((2-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1))
    )
  )

}
)

# Define your test
test_that("Normalize Data is running correctly - different date format.", {
  expect_equal(
    normalize_panel_data(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))))@data,
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
      Alpha = (c(
        2*((0-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((3-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((10-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((1-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((7-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((4-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((2-min(c(0,1,2), na.rm = TRUE))/(max(c(0,1,2), na.rm = TRUE)-min(c(0,1,2), na.rm = TRUE)))-1,
        2*((9-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((9-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1)),

      Beta = (c(
        2*((4-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((7-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((5-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((5-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((2-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((6-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((-3-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((-2-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1)),


      Gamma = (c(
        2*((800-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((11-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((9-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((-2-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((10-min(c(800,9,10), na.rm = TRUE))/(max(c(800,9,10), na.rm = TRUE)-min(c(800,9,10), na.rm = TRUE)))-1,
        2*((-3-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((2-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1))
    )
  )
}
)

# Define your test
test_that("Normalize Data is running correctly - Some NAs.", {
  expect_equal(
    suppressWarnings(
    normalize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                        "2001-03-15", "2001-04-15", "2001-05-15",
                        "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 3, 10, NA, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, 11, 4, 9, -2, 4, 10, -3, 2))))@data),
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
      Alpha = (c(
        2*((NA-min(c(NA,NA,2), na.rm = TRUE))/(max(c(NA,NA,2), na.rm = TRUE)-min(c(NA,NA,2), na.rm = TRUE)))-1,
        2*((3-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((10-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((NA-min(c(NA,NA,2), na.rm = TRUE))/(max(c(NA,NA,2), na.rm = TRUE)-min(c(NA,NA,2), na.rm = TRUE)))-1,
        2*((7-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((4-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((2-min(c(NA,NA,2), na.rm = TRUE))/(max(c(NA,NA,2), na.rm = TRUE)-min(c(NA,NA,2), na.rm = TRUE)))-1,
        2*((9-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((9-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1)),

      Beta = (c(
        2*((4-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((7-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((5-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((5-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((2-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((6-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((-3-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((-2-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1)),


      Gamma = (c(
        2*((NA-min(c(NA,9,10), na.rm = TRUE))/(max(c(NA,9,10), na.rm = TRUE)-min(c(NA,9,10), na.rm = TRUE)))-1,
        2*((11-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((9-min(c(NA,9,10), na.rm = TRUE))/(max(c(NA,9,10), na.rm = TRUE)-min(c(NA,9,10), na.rm = TRUE)))-1,
        2*((-2-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((10-min(c(NA,9,10), na.rm = TRUE))/(max(c(NA,9,10), na.rm = TRUE)-min(c(NA,9,10), na.rm = TRUE)))-1,
        2*((-3-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((2-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1))
    )
  )
}
)

# Define your test
test_that("Normalize Data is running correctly - Some Infs.", {
  expect_equal(
    normalize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                        "2001-03-15", "2001-04-15", "2001-05-15",
                        "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(Inf, 3, 10, Inf, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(-Inf, 11, 4, 9, -2, 4, 10, -3, 2))))@data,
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
      Alpha = (c(
        2*((Inf-min(c(Inf,Inf,2), na.rm = TRUE))/(max(c(Inf,Inf,2), na.rm = TRUE)-min(c(Inf,Inf,2), na.rm = TRUE)))-1,
        2*((3-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((10-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((Inf-min(c(Inf,Inf,2), na.rm = TRUE))/(max(c(Inf,Inf,2), na.rm = TRUE)-min(c(Inf,Inf,2), na.rm = TRUE)))-1,
        2*((7-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((4-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1,

        2*((2-min(c(Inf,Inf,2), na.rm = TRUE))/(max(c(Inf,Inf,2), na.rm = TRUE)-min(c(Inf,Inf,2), na.rm = TRUE)))-1,
        2*((9-min(c(3,7,9), na.rm = TRUE))/(max(c(3,7,9), na.rm = TRUE)-min(c(3,7,9), na.rm = TRUE)))-1,
        2*((9-min(c(10,4,9), na.rm = TRUE))/(max(c(10,4,9), na.rm = TRUE)-min(c(10,4,9), na.rm = TRUE)))-1)),

      Beta = (c(
        2*((4-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((7-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((5-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((5-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((2-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1,

        2*((6-min(c(4,5,6), na.rm = TRUE))/(max(c(4,5,6), na.rm = TRUE)-min(c(4,5,6), na.rm = TRUE)))-1,
        2*((-3-min(c(7,2,-3), na.rm = TRUE))/(max(c(7,2,-3), na.rm = TRUE)-min(c(7,2,-3), na.rm = TRUE)))-1,
        2*((-2-min(c(5,4,-2), na.rm = TRUE))/(max(c(5,4,-2), na.rm = TRUE)-min(c(5,4,-2), na.rm = TRUE)))-1)),


      Gamma = (c(
        2*((-Inf-min(c(-Inf,9,10), na.rm = TRUE))/(max(c(-Inf,9,10), na.rm = TRUE)-min(c(-Inf,9,10), na.rm = TRUE)))-1,
        2*((11-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((9-min(c(-Inf,9,10), na.rm = TRUE))/(max(c(-Inf,9,10), na.rm = TRUE)-min(c(-Inf,9,10), na.rm = TRUE)))-1,
        2*((-2-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((4-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1,

        2*((10-min(c(-Inf,9,10), na.rm = TRUE))/(max(c(-Inf,9,10), na.rm = TRUE)-min(c(-Inf,9,10), na.rm = TRUE)))-1,
        2*((-3-min(c(11,-2,-3), na.rm = TRUE))/(max(c(11,-2,-3), na.rm = TRUE)-min(c(11,-2,-3), na.rm = TRUE)))-1,
        2*((2-min(c(4,4,2), na.rm = TRUE))/(max(c(4,4,2), na.rm = TRUE)-min(c(4,4, 2), na.rm = TRUE)))-1))
    )
  )
}
)

test_that("normalize_panel_data sets column to zero when all values are equal", {
  # Create a test data frame
  features_m_df <- create_meta_dataframe(
    data.frame(
      id = c("Stock A-2022-01-01", "Stock A-2022-01-02", "Stock B-2022-01-01", "Stock B-2022-01-02"),
      tickers = c("Stock A", "Stock A", "Stock B", "Stock B"),
      dates = as.Date(c("2022-01-01", "2022-01-02", "2022-01-01", "2022-01-02")),
      feature_constant = c(5, 1, 5, 3),  # This column has the same value for each date
      feature_variable = c(1, 2, 3, 4)   # This column has different values
    ),
    "jil"
  )

  # Run the normalization function
  normalized_data <- normalize_panel_data(features_m_df)

  # Verify that feature_constant has been set to 0 for all rows
  expect_equal(normalized_data@data$feature_constant, c(0, -1, 0, 1))

  # Verify that feature_variable is normalized between -1 and 1
  # For this simple test data, it should normalize as intended
  expect_true(all(normalized_data@data$feature_variable >= -1))
  expect_true(all(normalized_data@data$feature_variable <= 1))

  # Verify metadata integrity
  expect_equal(normalized_data@unique_dates, 2)
  expect_equal(normalized_data@unique_tickers, 2)
  expect_equal(normalized_data@n_obs, 4)
})

# Define your test
test_that("Normalize Data throws an error when columns differ.", {
  expect_error(
    normalize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      ticker = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))),
   "features_m_df should be coercible to meta_dataframe object")
})

# Define your test
test_that("Normalize Data throws an error when there is an uncorrespondence in features_m_df$dates and dates_vector", {


  expect_error(
    normalize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-01-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))),
    "features_m_df should be coercible to meta_dataframe object")


})


# Define your test
test_that("Normalize Data throws an error when features_m_df not in right format.", {
  expect_error(
    normalize_panel_data(as.matrix(data.frame(
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
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))))),
    "features_m_df should be coercible to meta_dataframe object")
})


# Define your test
test_that("Normalize Data throws an error when dates_vector not in right format.", {
  expect_error(
    normalize_panel_data(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.character(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(0, 3, 10, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))),
    "features_m_df should be coercible to meta_dataframe object")
})

# Define your test
test_that("Normalize Data is integrating correctly with normalize and winsorize.", {

  panel_data <- create_meta_dataframe(
    data = list(matrix(c(0,1,2,3,7,9,10,4,9), nrow=3, ncol=3),
                matrix(c(4,5,6,7,2,-3,5,4,-2), nrow=3, ncol=3),
                matrix(c(8,9,10,11,-2,-3,4,4,2), nrow=3, ncol=3),
                matrix(c(3,7,9,8,-1,0,5,-2,0), nrow=3, ncol=3)),
                row_names = c("Stock A", "Stock B", "Stock C"),
                column_names = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
                features_names = c("Alpha", "Beta", "Gamma", "Delta"),
                meta_dataframe_name = "okay")

  winsorized_data <- winsorize_panel_data(panel_data, c(0.975, 0.025), c("Alpha"))

  actual_results <- normalize_panel_data(winsorized_data)

  expected_results <- new("meta_dataframe",
                          data = data.frame(
                            id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
                                    "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
                                    "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
                            tickers = (c("Stock A", "Stock A", "Stock A",
                                         "Stock B", "Stock B", "Stock B",
                                         "Stock C", "Stock C", "Stock C")),
                            dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                                              "2001-03-15", "2001-04-15", "2001-05-15",
                                              "2001-03-15", "2001-04-15", "2001-05-15")),
                            Alpha = (c(
                              2*((0.05-min(c(0.05,1,1.95), na.rm = TRUE))/(max(c(0.05,1,1.95), na.rm = TRUE)-min(c(0.05,1,1.95), na.rm = TRUE)))-1,
                              2*((3.20-min(c(3.20,7,8.90), na.rm = TRUE))/(max(c(3.20,7,8.90), na.rm = TRUE)-min(c(3.20,7,8.90), na.rm = TRUE)))-1,
                              2*((9.95-min(c(9.95,4.25,9), na.rm = TRUE))/(max(c(9.95,4.25,9), na.rm = TRUE)-min(c(9.95,4.25,9), na.rm = TRUE)))-1,

                              2*((1-min(c(0.05,1,1.95), na.rm = TRUE))/(max(c(0.05,1,1.95), na.rm = TRUE)-min(c(0.05,1,1.95), na.rm = TRUE)))-1,
                              2*((7-min(c(3.20,7,8.90), na.rm = TRUE))/(max(c(3.20,7,8.90), na.rm = TRUE)-min(c(3.20,7,8.90), na.rm = TRUE)))-1,
                              2*((4.25-min(c(9.95,4.25,9), na.rm = TRUE))/(max(c(9.95,4.25,9), na.rm = TRUE)-min(c(9.95,4.25,9), na.rm = TRUE)))-1,

                              2*((1.95-min(c(0.05,1,1.95), na.rm = TRUE))/(max(c(0.05,1,1.95), na.rm = TRUE)-min(c(0.05,1,1.95), na.rm = TRUE)))-1,
                              2*((8.90-min(c(3.20,7,8.90), na.rm = TRUE))/(max(c(3.20,7,8.90), na.rm = TRUE)-min(c(3.20,7,8.90), na.rm = TRUE)))-1,
                              2*((9-min(c(9.95,4.25,9), na.rm = TRUE))/(max(c(9.95,4.25,9), na.rm = TRUE)-min(c(9.95,4.25,9), na.rm = TRUE)))-1)),

                            Beta = (c(
                              2*((4.05-min(c(4.05,5.00,5.95), na.rm = TRUE))/(max(c(4.05,5.00,5.95), na.rm = TRUE)-min(c(4.05,5.00,5.95), na.rm = TRUE)))-1,
                              2*((6.75-min(c(6.75,2,-2.75), na.rm = TRUE))/(max(c(6.75,2,-2.75), na.rm = TRUE)-min(c(6.75,2,-2.75), na.rm = TRUE)))-1,
                              2*((4.95-min(c(4.95,4,-1.70), na.rm = TRUE))/(max(c(4.95,4,-1.70), na.rm = TRUE)-min(c(4.95,4,-1.70), na.rm = TRUE)))-1,

                              2*((5-min(c(4.05,5.00,5.95), na.rm = TRUE))/(max(c(4.05,5.00,5.95), na.rm = TRUE)-min(c(4.05,5.00,5.95), na.rm = TRUE)))-1,
                              2*((2-min(c(6.75,2,-2.75), na.rm = TRUE))/(max(c(6.75,2,-2.75), na.rm = TRUE)-min(c(6.75,2,-2.75), na.rm = TRUE)))-1,
                              2*((4-min(c(4.95,4,-1.70), na.rm = TRUE))/(max(c(4.95,4,-1.70), na.rm = TRUE)-min(c(4.95,4,-1.70), na.rm = TRUE)))-1,

                              2*((5.95-min(c(4.05,5.00,5.95), na.rm = TRUE))/(max(c(4.05,5.00,5.95), na.rm = TRUE)-min(c(4.05,5.00,5.95), na.rm = TRUE)))-1,
                              2*((-2.75-min(c(6.75,2,-2.75), na.rm = TRUE))/(max(c(6.75,2,-2.75), na.rm = TRUE)-min(c(6.75,2,-2.75), na.rm = TRUE)))-1,
                              2*((-1.70-min(c(4.95,4,-1.70), na.rm = TRUE))/(max(c(4.95,4,-1.70), na.rm = TRUE)-min(c(4.95,4,-1.70), na.rm = TRUE)))-1)),


                            Gamma = (c(
                              2*((8.05-min(c(8.05,9,9.95), na.rm = TRUE))/(max(c(8.05,9,9.95), na.rm = TRUE)-min(c(8.05,9,9.95), na.rm = TRUE)))-1,
                              2*((10.35-min(c(10.35,-2,-2.95), na.rm = TRUE))/(max(c(10.35,-2,-2.95), na.rm = TRUE)-min(c(10.35,-2,-2.95), na.rm = TRUE)))-1,
                              2*((4-min(c(4,4,2.10), na.rm = TRUE))/(max(c(4,4,2.10), na.rm = TRUE)-min(c(4,4,2.10), na.rm = TRUE)))-1,

                              2*((9-min(c(8.05,9,9.95), na.rm = TRUE))/(max(c(8.05,9,9.95), na.rm = TRUE)-min(c(8.05,9,9.95), na.rm = TRUE)))-1,
                              2*((-2-min(c(10.35,-2,-2.95), na.rm = TRUE))/(max(c(10.35,-2,-2.95), na.rm = TRUE)-min(c(10.35,-2,-2.95), na.rm = TRUE)))-1,
                              2*((4-min(c(4,4,2.10), na.rm = TRUE))/(max(c(4,4,2.10), na.rm = TRUE)-min(c(4,4,2.10), na.rm = TRUE)))-1,

                              2*((9.95-min(c(8.05,9,9.95), na.rm = TRUE))/(max(c(8.05,9,9.95), na.rm = TRUE)-min(c(8.05,9,9.95), na.rm = TRUE)))-1,
                              2*((-2.95-min(c(10.35,-2,-2.95), na.rm = TRUE))/(max(c(10.35,-2,-2.95), na.rm = TRUE)-min(c(10.35,-2,-2.95), na.rm = TRUE)))-1,
                              2*((2.1-min(c(4,4,2.1), na.rm = TRUE))/(max(c(4,4,2.1), na.rm = TRUE)-min(c(4,4,2.1), na.rm = TRUE)))-1)),

                            Delta = (c(
                              2*((3.20-min(c(3.20,7,8.90), na.rm = TRUE))/(max(c(3.20,7,8.90), na.rm = TRUE)-min(c(3.20,7,8.90), na.rm = TRUE)))-1,
                              2*((7.60-min(c(7.60,-0.95,0), na.rm = TRUE))/(max(c(7.60,-0.95,0), na.rm = TRUE)-min(c(7.60,-0.95,0), na.rm = TRUE)))-1,
                              2*((4.75-min(c(4.75,-1.90,0), na.rm = TRUE))/(max(c(4.75,-1.90,0), na.rm = TRUE)-min(c(4.75,-1.90,0), na.rm = TRUE)))-1,

                              2*((7-min(c(3.20,7,8.90), na.rm = TRUE))/(max(c(3.20,7,8.90), na.rm = TRUE)-min(c(3.20,7,8.90), na.rm = TRUE)))-1,
                              2*((-0.95-min(c(7.60,-0.95,0), na.rm = TRUE))/(max(c(7.60,-0.95,0), na.rm = TRUE)-min(c(7.60,-0.95,0), na.rm = TRUE)))-1,
                              2*((-1.90-min(c(4.75,-1.90,0), na.rm = TRUE))/(max(c(4.75,-1.90,0), na.rm = TRUE)-min(c(4.75,-1.90,0), na.rm = TRUE)))-1,

                              2*((8.90-min(c(3.20,7,8.90), na.rm = TRUE))/(max(c(3.20,7,8.90), na.rm = TRUE)-min(c(3.20,7,8.90), na.rm = TRUE)))-1,
                              2*((0-min(c(7.60,-0.95,0), na.rm = TRUE))/(max(c(7.60,-0.95,0), na.rm = TRUE)-min(c(7.60,-0.95,0), na.rm = TRUE)))-1,
                              2*((0-min(c(4.75,-1.90,0), na.rm = TRUE))/(max(c(4.75,-1.90,0), na.rm = TRUE)-min(c(4.75,-1.90,0), na.rm = TRUE)))-1))
                          ),
                          workflow = actual_results@workflow,
                          signals = actual_results@signals,
                          unique_dates = actual_results@unique_dates,
                          unique_tickers = actual_results@unique_tickers,
                          n_obs = actual_results@n_obs,
                          meta_dataframe_name = "okay"
  )

  expect_equal(actual_results@data, expected_results@data)


})



# Define your test
test_that("normalize_data integrates with external toy data - Excel Files", {

  #Load excel and set inputs and outputs
  results <- load_inputs_outputs_panels_excel(csv_file_name = "toy_features.xlsx",
                                 features_sheet_names = c("ebit_12m","ir_3m", "sharpe", "mkt_cap","sector_c1"),
                                 features_sheet_range = c("D4:F22"),
                                 tickers_sheet_range = c("C4:C22"),
                                 dates_sheet_range = c("D1:F1"),
                                 output_sheet_name = c("normalized_panel"),
                                 output_sheet_range = c("B1:I58"),
                                 industry_classification_column_name = c("sector_c1"))
  #Apply functions
  panel <- create_meta_dataframe(data = results$inputs$feature_list,
                         row_names = results$inputs$tickers$...1,
                         column_names  = results$inputs$dates,
                         features_names = results$inputs$features_names,
                         "okay")

  winsorized_panel <- winsorize_panel_data(features_m_df = panel,
                                           probs = c(0.975,0.025))

  normalized_panel <- normalize_panel_data(features_m_df = winsorized_panel)

  results$outputs$dates <- as.Date(results$outputs$dates)

  # Apply the function to the test data
  expect_equal(normalized_panel@data,
               results$outputs
  )

})

