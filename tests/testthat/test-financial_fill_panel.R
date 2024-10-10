# Define your test
test_that("Banks Fill is running correctly.", {
  expect_equal(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  "Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 4, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros"))@data),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  "Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1.5, 4, 6.5, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
})

# Define your test
test_that("Banks Fill is running correctly for banks and insurance cias.", {
  expect_equal(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  "Previdência e seguros", "Previdência e seguros", "Previdência e seguros",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, NA, 2, NA, NA, NA, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros", "Previdência e seguros")))@data,
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  "Previdência e seguros", "Previdência e seguros", "Previdência e seguros",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1.5, 8, 6.5, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(10, -3, 2, 10, -3, 2, 10, -3, 2)))
  )
}
)


# Define your test
test_that("Banks Fill is running correctly when there are only NAs", {
  expect_equal(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  "Previdência e seguros", "Previdência e seguros", "Previdência e seguros",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, NA, NA, NA, NA, NA, NA, NA, NA))),
      segment_column = "segment",
      c("Intermediários financeiros", "Previdência e seguros"))@data),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  "Previdência e seguros", "Previdência e seguros", "Previdência e seguros",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1.5, 8, 6.5, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NaN, NaN, NaN, NaN, NaN, NaN, NA, NA, NA)))
  )
}
)


# Define your test
test_that("Banks Fill is running correctly when one segment is NA.", {
  expect_equal(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  NA, NA, NA,
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 0, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, 3, NA, NA, NA, NA, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros", "Previdência e seguros"))@data),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  NA, NA, NA,
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1.5, 0, 6.5, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(10, 3, 2, NA, NA, NA, 10, -3, 2)))
  )

  expect_equal(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  NA, NA, NA,
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 0, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, 3, NA, NA, NA, -4, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros", "Previdência e seguros"))@data),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  NA, NA, NA,
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(1.5, 0, 6.5, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(10, 3, -1, NA, NA, -4, 10, -3, 2)))
  )


}
)

# Define your test
test_that("Banks Fill is running correctly when some characteristics are chosen to be unchanged.", {
  expect_equal(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  NA, NA, NA,
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, -2, NA, 3, NA, NA, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros", "Previdência e seguros"),
      c("Alpha"))@data),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  NA, NA, NA,
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(6.5, -2, 2, 3, NA, NA, 10, -3, 2)))
  )
}
)


# Define your test
test_that("Banks Fill is running correctly when some characteristic is NA for all banks.", {
  expect_equal(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  NA, NA, NA,
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, NA, NA, 3, NA, NA, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros", "Previdência e seguros"),
      c("Alpha"))@data),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  NA, NA, NA,
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, NA, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(NA, NA, NA, 3, NA, NA, 10, -3, 2)))
  )
}
)

# Define your test
test_that("Banks Fill is running correctly when there are no banks.", {
  expect_equal(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Ronaldo", "Ronaldinho", "Pelé",
                  "Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 4, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros"))@data),
    data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Ronaldo", "Ronaldinho", "Pelé",
                  "Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 4, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2)))
  )
}
)

# Define your test
test_that("Banks Fill integrates with all other panel functions.", {

  panel_data <- panelize_data(list(matrix(c(0,NA,2,3,7,9,10,4,9), nrow=3, ncol=3),
                                     matrix(c(4,5,6,7,2,-3,5,4,-2), nrow=3, ncol=3),
                                     matrix(c(8,9,10,NA,-2,-3,4,4,2), nrow=3, ncol=3),
                                     matrix(c(NA,7,9,NA,-1,0,NA,-2,0), nrow=3, ncol=3),
                                     matrix(c("Intermediários financeiros","Setor 1","Previdência e Seguros",
                                              "Intermediários financeiros","Setor 1","Previdência e Seguros",
                                              "Intermediários financeiros","Setor 1","Previdência e Seguros"), nrow=3, ncol=3)),
                                c("Stock A", "Stock B", "Stock C"),
                                as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
                                c("Alpha", "Beta", "Gamma", "Delta", "segment"))

  winsorized_panel <- winsorize_panel_data(panel_data, c(0.975, 0.025), c("Alpha"))

  normalized_panel <- normalize_panel_data(winsorized_panel)

  expected_results <- financialcia_fill_panel(normalized_panel, segment_column = "segment", c("Intermediários financeiros"))

  expect_equal(
  new("meta_dataframe",
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
    Alpha = (c(-1.0000000, -1.0000000, 1.0000000, NA, 0.33333333, -1.0000000, 1.0000000, 1.0000000, 0.66666667)),
    Beta = (c(-1.0000000, 1.0000000, 1.0000000, 0.0000000, 0.0000000, 0.71428571, 1.0000000, -1.0000000, -1.0000000)),
    Gamma = (c(-1, mean(c(1,-1)), 1, 0, 1, 1, 1, -1, -1)),
    Delta = (c(NA, NA, NA, -1, -1, -1, 1, 1, 1)),
    segment = c("Intermediários financeiros","Intermediários financeiros", "Intermediários financeiros",
                "Setor 1","Setor 1", "Setor 1",
                "Previdência e Seguros", "Previdência e Seguros", "Previdência e Seguros")),
  workflow = expected_results@workflow,
  signals = expected_results@signals,
  unique_dates = expected_results@unique_dates,
  unique_tickers = expected_results@unique_tickers,
  n_obs = expected_results@n_obs
  ),
  expected_results)

})



test_that("Banks Fill throws an error  when columns differ.", {
  expect_error(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      ticker = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      segment = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  "Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 4, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros"))@data),
    "features_m_df should be coercible to meta_dataframe object"
  )
}
)

test_that("Banks Fill throws an error when segment column is missing.", {

  expect_error(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                  "Stock B", "Stock B", "Stock B",
                  "Stock C", "Stock C", "Stock C")),
      segments = c("Intermediários financeiros", "Intermediários financeiros", "Intermediários financeiros",
                  "Setor 1", "Setor 1", "Setor 1",
                  "Setor 2", "Setor 2", "Setor 2"),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 4, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros"))@data),
    "there must be a segment_column in features_m_df"
  )

  expect_error(
    suppressWarnings(
    financialcia_fill_panel(data.frame(
      id = (c("Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
              "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15")),
      tickers = (c("Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B",
                   "Stock C", "Stock C", "Stock C")),
      dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15",
                          "2001-03-15", "2001-04-15", "2001-05-15")),
      Alpha = (c(NA, 4, NA, 1, 7, 4, 2, 9, 9)),
      Beta = (c(4, 7, 5, 5, 2, 4, 6, -3, -2)),
      Gamma = (c(800, 11, 4, 9, -2, 4, 10, -3, 2))),
      segment_column = "segment",
      c("Intermediários financeiros"))@data),
    "there must be a segment_column in features_m_df"
  )

})
