#Define your test
test_that("time_series_split works with a 1m target, no validation, current_date matches first rebalancing", {
  features_m_df = data.frame(
    stringsAsFactors = FALSE,
    id = c("Stock A-2001-03-15",
           "Stock A-2001-04-15","Stock A-2001-05-15",
           "Stock A-2001-06-15","Stock A-2001-07-15",
           "Stock A-2001-08-15",
           "Stock B-2001-03-15","Stock B-2001-04-15",
           "Stock B-2001-05-15","Stock B-2001-06-15",
           "Stock B-2001-07-15","Stock B-2001-08-15",
           "Stock C-2001-03-15","Stock C-2001-04-15",
           "Stock C-2001-05-15",
           "Stock C-2001-06-15","Stock C-2001-07-15",
           "Stock C-2001-08-15","Stock D-2001-03-15",
           "Stock D-2001-04-15","Stock D-2001-05-15",
           "Stock D-2001-06-15",
           "Stock D-2001-07-15","Stock D-2001-08-15",
           "Stock E-2001-03-15","Stock E-2001-04-15",
           "Stock E-2001-05-15","Stock E-2001-06-15",
           "Stock E-2001-07-15","Stock E-2001-08-15"),
    tickers = c("Stock A","Stock A","Stock A",
                "Stock A","Stock A","Stock A",
                "Stock B","Stock B","Stock B","Stock B",
                "Stock B","Stock B","Stock C",
                "Stock C","Stock C","Stock C","Stock C",
                "Stock C","Stock D","Stock D","Stock D",
                "Stock D","Stock D","Stock D",
                "Stock E","Stock E","Stock E","Stock E",
                "Stock E","Stock E"),
    dates = c("2001-03-15","2001-04-15",
              "2001-05-15","2001-06-15","2001-07-15",
              "2001-08-15","2001-03-15","2001-04-15",
              "2001-05-15","2001-06-15",
              "2001-07-15","2001-08-15","2001-03-15",
              "2001-04-15","2001-05-15","2001-06-15",
              "2001-07-15","2001-08-15","2001-03-15",
              "2001-04-15","2001-05-15","2001-06-15",
              "2001-07-15","2001-08-15",
              "2001-03-15","2001-04-15","2001-05-15",
              "2001-06-15","2001-07-15","2001-08-15"),
    Alpha = c(3,-20,-450,5,-2,1,1,7,4,2,
              20,1,2,9,9,-20,-150,-20,5,-2,
              2,-1,-50,-25,5,3,-1,2,-1,-20),
    Beta = c(4,7,5,3,13,10,5,2,4,1,
             -12,-10,6,-3,-2,1,1,4,0,-2,5,2,
             5,1,2,-9,3,1,2,1),
    Gamma = c(800,11,4,20,0,-523,9,-2,4,
              -15,3,4,10,-3,2,6,20,12,-9,5,
              2,3,3,-10,3,1,-500,6,4,405))

  target_m_df <- data.frame(
    id = c(
      "Stock A-2001-03-15", "Stock A-2001-04-15", "Stock A-2001-05-15",
      "Stock A-2001-06-15", "Stock A-2001-07-15", "Stock A-2001-08-15",
      "Stock B-2001-03-15", "Stock B-2001-04-15", "Stock B-2001-05-15",
      "Stock B-2001-06-15", "Stock B-2001-07-15", "Stock B-2001-08-15",
      "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15",
      "Stock C-2001-06-15", "Stock C-2001-07-15", "Stock C-2001-08-15",
      "Stock D-2001-03-15", "Stock D-2001-04-15", "Stock D-2001-05-15",
      "Stock D-2001-06-15", "Stock D-2001-07-15", "Stock D-2001-08-15",
      "Stock E-2001-03-15", "Stock E-2001-04-15", "Stock E-2001-05-15",
      "Stock E-2001-06-15", "Stock E-2001-07-15", "Stock E-2001-08-15"
    ),
    tickers = c(
      "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
      "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
      "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
      "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
      "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
    ),
    dates = as.Date(c(
      "2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15",
      "2001-07-15", "2001-08-15", "2001-03-15", "2001-04-15",
      "2001-05-15", "2001-06-15", "2001-07-15", "2001-08-15",
      "2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15",
      "2001-07-15", "2001-08-15", "2001-03-15", "2001-04-15",
      "2001-05-15", "2001-06-15", "2001-07-15", "2001-08-15",
      "2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15",
      "2001-07-15", "2001-08-15"
    )),
    fwd_premium_1m = c(
      0, 6, 7, 1, 2, 1, 1, 8, 2, 3, 5, -1, 2, 3, 7, 5, 1, -9, 8,
      8, 8, 7, 2, -2, 5, 1, 8, 1, 2, 0
    ),
    fwd_premium_3m = c(
      4, 4, 2, 0, 6, 5, 5, 3, 7, 3, 8, 2, 0, 5, 2, 8, 3, 5, 1,
      3, 8, 3, 1, 1, 9, 9, 1, 2, 3, -9
    ),
    fwd_sharpe_1m = c(
      7,7,3,1,1,3,
      4,2,8,5,4,1,2,
      6,4,6,5,1,4,
      9,0,10,1,4,7,1,
      3,3,0,1))


  #Apply function
  suppressMessages(suppressWarnings({
    split <- time_series_split(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      current_date = as.Date(c("2001-06-15"), format = "%Y-%m-%d"),

      dates_m_vector = as.Date(c("2001-03-15","2001-04-15","2001-05-15","2001-06-15",
                                 "2001-07-15", "2001-08-15"), format = "%Y-%m-%d"),
      training_sample_size = 4,
      target_fwd = 1,
      target_fwd_name = "fwd_premium_1m",
      split_method = "expanding")
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  results[[2]] <- list()
  names(results) <- c("training", "refit")

  #Fill Training
  results$training <- list()
  results$training[[1]] <- features_m_df[which(features_m_df$dates %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15"), format = "%Y-%m-%d")),]
  results$training[[2]] <- target_m_df[which(target_m_df$dates %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15"), format = "%Y-%m-%d")),
                                       which(colnames(target_m_df) == "fwd_premium_1m")]

  results$training[[3]] <- cbind(fwd_premium_1m = results$training[[2]], results$training[[1]][,-c(1:3)])
  names(results$training) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

  #Fill refit
  results$refit <- list()
  results$refit[[1]] <- results$training[[1]]
  results$refit[[2]] <- results$training[[2]]
  results$refit[[3]] <- results$training[[3]]

  names(results$refit) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")


  expect_equal(
    split,
    results
  )

})


#Define your test
test_that("time_series_split works with a 1m target, no validation, current_date matches second rebalancing", {
  features_m_df =
    data.frame(
      stringsAsFactors = FALSE,
      id = c("Stock A-2001-03-15",
             "Stock A-2001-04-15","Stock A-2001-05-15","Stock A-2001-06-15",
             "Stock A-2001-07-15","Stock A-2001-08-15",
             "Stock A-2001-09-15","Stock A-2001-10-15","Stock A-2001-11-15",
             "Stock B-2001-03-15","Stock B-2001-04-15",
             "Stock B-2001-05-15","Stock B-2001-06-15","Stock B-2001-07-15",
             "Stock B-2001-08-15","Stock B-2001-09-15","Stock B-2001-10-15",
             "Stock B-2001-11-15","Stock C-2001-03-15",
             "Stock C-2001-04-15","Stock C-2001-05-15","Stock C-2001-06-15",
             "Stock C-2001-07-15","Stock C-2001-08-15",
             "Stock C-2001-09-15","Stock C-2001-10-15","Stock C-2001-11-15",
             "Stock D-2001-03-15","Stock D-2001-04-15","Stock D-2001-05-15",
             "Stock D-2001-06-15","Stock D-2001-07-15",
             "Stock D-2001-08-15","Stock D-2001-09-15","Stock D-2001-10-15",
             "Stock D-2001-11-15","Stock E-2001-03-15",
             "Stock E-2001-04-15","Stock E-2001-05-15","Stock E-2001-06-15",
             "Stock E-2001-07-15","Stock E-2001-08-15",
             "Stock E-2001-09-15","Stock E-2001-10-15","Stock E-2001-11-15"),
      tickers = c("Stock A","Stock A","Stock A",
                  "Stock A","Stock A","Stock A","Stock A","Stock A",
                  "Stock A","Stock B","Stock B","Stock B","Stock B",
                  "Stock B","Stock B","Stock B","Stock B","Stock B",
                  "Stock C","Stock C","Stock C","Stock C","Stock C",
                  "Stock C","Stock C","Stock C","Stock C","Stock D",
                  "Stock D","Stock D","Stock D","Stock D","Stock D","Stock D",
                  "Stock D","Stock D","Stock E","Stock E","Stock E",
                  "Stock E","Stock E","Stock E","Stock E","Stock E",
                  "Stock E"),
      dates = c("2001-03-15","2001-04-15",
                "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                "2001-09-15","2001-10-15","2001-11-15","2001-03-15",
                "2001-04-15","2001-05-15","2001-06-15","2001-07-15",
                "2001-08-15","2001-09-15","2001-10-15","2001-11-15",
                "2001-03-15","2001-04-15","2001-05-15","2001-06-15",
                "2001-07-15","2001-08-15","2001-09-15","2001-10-15",
                "2001-11-15","2001-03-15","2001-04-15","2001-05-15",
                "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
                "2001-10-15","2001-11-15","2001-03-15","2001-04-15","2001-05-15",
                "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
                "2001-10-15","2001-11-15"),
      Alpha = c(3,-20,-450,5,-2,1,6,1,
                -9,1,7,4,2,20,1,1,-2,-2,2,9,9,-20,-150,-20,
                8,17,1,5,-2,2,-1,-50,-25,1,4,2,5,3,-1,2,
                -1,-20,-1,4,4),
      Beta = c(4,7,5,3,13,10,4,-5,1,5,
               2,4,1,-12,-10,3,4,1,6,-3,-2,1,1,4,24,19,
               -1,0,-2,5,2,5,1,2,5,3,2,-9,3,1,2,1,-1,
               -20,2),
      Gamma = c(800,11,4,20,0,-523,2,3,
                27,9,-2,4,-15,3,4,4,3,7,10,-3,2,6,20,12,
                13,-4,105,-9,5,2,3,3,-10,0,-1,4,3,1,-500,6,
                4,405,0,1,31)
    )
  target_m_df <-   data.frame(
    stringsAsFactors = FALSE,
    id = c("Stock A-2001-03-15",
           "Stock A-2001-04-15","Stock A-2001-05-15","Stock A-2001-06-15",
           "Stock A-2001-07-15","Stock A-2001-08-15",
           "Stock A-2001-09-15","Stock A-2001-10-15","Stock A-2001-11-15",
           "Stock B-2001-03-15","Stock B-2001-04-15",
           "Stock B-2001-05-15","Stock B-2001-06-15","Stock B-2001-07-15",
           "Stock B-2001-08-15","Stock B-2001-09-15","Stock B-2001-10-15",
           "Stock B-2001-11-15","Stock C-2001-03-15",
           "Stock C-2001-04-15","Stock C-2001-05-15","Stock C-2001-06-15",
           "Stock C-2001-07-15","Stock C-2001-08-15",
           "Stock C-2001-09-15","Stock C-2001-10-15","Stock C-2001-11-15",
           "Stock D-2001-03-15","Stock D-2001-04-15","Stock D-2001-05-15",
           "Stock D-2001-06-15","Stock D-2001-07-15",
           "Stock D-2001-08-15","Stock D-2001-09-15","Stock D-2001-10-15",
           "Stock D-2001-11-15","Stock E-2001-03-15",
           "Stock E-2001-04-15","Stock E-2001-05-15","Stock E-2001-06-15",
           "Stock E-2001-07-15","Stock E-2001-08-15",
           "Stock E-2001-09-15","Stock E-2001-10-15","Stock E-2001-11-15"),
    tickers = c("Stock A","Stock A","Stock A",
                "Stock A","Stock A","Stock A","Stock A","Stock A",
                "Stock A","Stock B","Stock B","Stock B","Stock B",
                "Stock B","Stock B","Stock B","Stock B","Stock B",
                "Stock C","Stock C","Stock C","Stock C","Stock C",
                "Stock C","Stock C","Stock C","Stock C","Stock D",
                "Stock D","Stock D","Stock D","Stock D","Stock D","Stock D",
                "Stock D","Stock D","Stock E","Stock E","Stock E",
                "Stock E","Stock E","Stock E","Stock E","Stock E",
                "Stock E"),
    dates = c("2001-03-15","2001-04-15",
              "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
              "2001-09-15","2001-10-15","2001-11-15","2001-03-15",
              "2001-04-15","2001-05-15","2001-06-15","2001-07-15",
              "2001-08-15","2001-09-15","2001-10-15","2001-11-15",
              "2001-03-15","2001-04-15","2001-05-15","2001-06-15",
              "2001-07-15","2001-08-15","2001-09-15","2001-10-15",
              "2001-11-15","2001-03-15","2001-04-15","2001-05-15",
              "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
              "2001-10-15","2001-11-15","2001-03-15","2001-04-15","2001-05-15",
              "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
              "2001-10-15","2001-11-15"),
    fwd_premium_1m = c(0,6,7,1,2,1,10,3,1,1,
                       8,2,3,5,-1,35,-152,3,2,3,7,5,1,-9,2,4,-20,
                       8,8,8,7,2,-2,-10,-45,-3,5,1,8,1,2,1,4,
                       -5,0),
    fwd_premium_3m = c(4,4,2,0,6,5,-5,-1,4,5,
                       3,7,3,8,2,5,1,2,0,5,2,8,3,5,3,40,2,1,3,
                       8,3,1,1,11,4,2,9,9,1,2,3,-9,-4,4,3),
    fwd_sharpe_1m = c(7,7,3,1,1,3,1,0,10,4,
                      2,8,5,4,1,1,4,-5,2,6,4,6,5,1,1,5,3,4,9,
                      0,10,1,4,12,1,92,7,1,3,3,0,1,3,1,9)
  )

  #Apply function
  suppressMessages(suppressWarnings({
    split <- time_series_split(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      current_date = as.Date(c("2001-09-15"), format = "%Y-%m-%d"),
      dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                 "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                 "2001-09-15","2001-10-15","2001-11-15"), format = "%Y-%m-%d"),
      training_sample_size = 4,
      target_fwd = 1,
      target_fwd_name = "fwd_premium_1m",
      split_method = "expanding")
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  results[[2]] <- list()
  names(results) <- c("training", "refit")

  #Fill Training
  results$training <- list()
  results$training[[1]] <- features_m_df[which(features_m_df$dates %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15",
                                                                                  "2001-06-15", "2001-07-15","2001-08-15"), format = "%Y-%m-%d")),]
  results$training[[2]] <- target_m_df[which(target_m_df$dates %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15",
                                                                              "2001-06-15", "2001-07-15","2001-08-15"), format = "%Y-%m-%d")),
                                       which(colnames(target_m_df) == "fwd_premium_1m")]

  results$training[[3]] <- cbind(fwd_premium_1m = results$training[[2]], results$training[[1]][,-c(1:3)])
  names(results$training) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

  #Fill refit
  results$refit <- list()
  results$refit[[1]] <- results$training[[1]]
  results$refit[[2]] <- results$training[[2]]
  results$refit[[3]] <- results$training[[3]]

  names(results$refit) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")


  expect_equal(
    split,
    results
  )

})

#Define your test
test_that("time_series_split works with a 3m target, no validation, current_date matches second rebalancing (which is final date)", {
  features_m_df =
    data.frame(
      stringsAsFactors = FALSE,
      id = c("Stock A-2001-03-15",
             "Stock A-2001-04-15","Stock A-2001-05-15","Stock A-2001-06-15",
             "Stock A-2001-07-15","Stock A-2001-08-15",
             "Stock A-2001-09-15","Stock A-2001-10-15","Stock A-2001-11-15",
             "Stock B-2001-03-15","Stock B-2001-04-15",
             "Stock B-2001-05-15","Stock B-2001-06-15","Stock B-2001-07-15",
             "Stock B-2001-08-15","Stock B-2001-09-15","Stock B-2001-10-15",
             "Stock B-2001-11-15","Stock C-2001-03-15",
             "Stock C-2001-04-15","Stock C-2001-05-15","Stock C-2001-06-15",
             "Stock C-2001-07-15","Stock C-2001-08-15",
             "Stock C-2001-09-15","Stock C-2001-10-15","Stock C-2001-11-15",
             "Stock D-2001-03-15","Stock D-2001-04-15","Stock D-2001-05-15",
             "Stock D-2001-06-15","Stock D-2001-07-15",
             "Stock D-2001-08-15","Stock D-2001-09-15","Stock D-2001-10-15",
             "Stock D-2001-11-15","Stock E-2001-03-15",
             "Stock E-2001-04-15","Stock E-2001-05-15","Stock E-2001-06-15",
             "Stock E-2001-07-15","Stock E-2001-08-15",
             "Stock E-2001-09-15","Stock E-2001-10-15","Stock E-2001-11-15"),
      tickers = c("Stock A","Stock A","Stock A",
                  "Stock A","Stock A","Stock A","Stock A","Stock A",
                  "Stock A","Stock B","Stock B","Stock B","Stock B",
                  "Stock B","Stock B","Stock B","Stock B","Stock B",
                  "Stock C","Stock C","Stock C","Stock C","Stock C",
                  "Stock C","Stock C","Stock C","Stock C","Stock D",
                  "Stock D","Stock D","Stock D","Stock D","Stock D","Stock D",
                  "Stock D","Stock D","Stock E","Stock E","Stock E",
                  "Stock E","Stock E","Stock E","Stock E","Stock E",
                  "Stock E"),
      dates = c("2001-03-15","2001-04-15",
                "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                "2001-09-15","2001-10-15","2001-11-15","2001-03-15",
                "2001-04-15","2001-05-15","2001-06-15","2001-07-15",
                "2001-08-15","2001-09-15","2001-10-15","2001-11-15",
                "2001-03-15","2001-04-15","2001-05-15","2001-06-15",
                "2001-07-15","2001-08-15","2001-09-15","2001-10-15",
                "2001-11-15","2001-03-15","2001-04-15","2001-05-15",
                "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
                "2001-10-15","2001-11-15","2001-03-15","2001-04-15","2001-05-15",
                "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
                "2001-10-15","2001-11-15"),
      Alpha = c(3,-20,-450,5,-2,1,6,1,
                -9,1,7,4,2,20,1,1,-2,-2,2,9,9,-20,-150,-20,
                8,17,1,5,-2,2,-1,-50,-25,1,4,2,5,3,-1,2,
                -1,-20,-1,4,4),
      Beta = c(4,7,5,3,13,10,4,-5,1,5,
               2,4,1,-12,-10,3,4,1,6,-3,-2,1,1,4,24,19,
               -1,0,-2,5,2,5,1,2,5,3,2,-9,3,1,2,1,-1,
               -20,2),
      Gamma = c(800,11,4,20,0,-523,2,3,
                27,9,-2,4,-15,3,4,4,3,7,10,-3,2,6,20,12,
                13,-4,105,-9,5,2,3,3,-10,0,-1,4,3,1,-500,6,
                4,405,0,1,31)
    )
  target_m_df =
    data.frame(
      stringsAsFactors = FALSE,
      id = c("Stock A-2001-03-15",
             "Stock A-2001-04-15","Stock A-2001-05-15","Stock A-2001-06-15",
             "Stock A-2001-07-15","Stock A-2001-08-15",
             "Stock A-2001-09-15","Stock A-2001-10-15","Stock A-2001-11-15",
             "Stock B-2001-03-15","Stock B-2001-04-15",
             "Stock B-2001-05-15","Stock B-2001-06-15","Stock B-2001-07-15",
             "Stock B-2001-08-15","Stock B-2001-09-15","Stock B-2001-10-15",
             "Stock B-2001-11-15","Stock C-2001-03-15",
             "Stock C-2001-04-15","Stock C-2001-05-15","Stock C-2001-06-15",
             "Stock C-2001-07-15","Stock C-2001-08-15",
             "Stock C-2001-09-15","Stock C-2001-10-15","Stock C-2001-11-15",
             "Stock D-2001-03-15","Stock D-2001-04-15","Stock D-2001-05-15",
             "Stock D-2001-06-15","Stock D-2001-07-15",
             "Stock D-2001-08-15","Stock D-2001-09-15","Stock D-2001-10-15",
             "Stock D-2001-11-15","Stock E-2001-03-15",
             "Stock E-2001-04-15","Stock E-2001-05-15","Stock E-2001-06-15",
             "Stock E-2001-07-15","Stock E-2001-08-15",
             "Stock E-2001-09-15","Stock E-2001-10-15","Stock E-2001-11-15"),
      tickers = c("Stock A","Stock A","Stock A",
                  "Stock A","Stock A","Stock A","Stock A","Stock A",
                  "Stock A","Stock B","Stock B","Stock B","Stock B",
                  "Stock B","Stock B","Stock B","Stock B","Stock B",
                  "Stock C","Stock C","Stock C","Stock C","Stock C",
                  "Stock C","Stock C","Stock C","Stock C","Stock D",
                  "Stock D","Stock D","Stock D","Stock D","Stock D","Stock D",
                  "Stock D","Stock D","Stock E","Stock E","Stock E",
                  "Stock E","Stock E","Stock E","Stock E","Stock E",
                  "Stock E"),
      dates = c("2001-03-15","2001-04-15",
                "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                "2001-09-15","2001-10-15","2001-11-15","2001-03-15",
                "2001-04-15","2001-05-15","2001-06-15","2001-07-15",
                "2001-08-15","2001-09-15","2001-10-15","2001-11-15",
                "2001-03-15","2001-04-15","2001-05-15","2001-06-15",
                "2001-07-15","2001-08-15","2001-09-15","2001-10-15",
                "2001-11-15","2001-03-15","2001-04-15","2001-05-15",
                "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
                "2001-10-15","2001-11-15","2001-03-15","2001-04-15","2001-05-15",
                "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
                "2001-10-15","2001-11-15"),
      fwd_premium_1m = c(0,6,7,1,2,1,10,3,1,1,
                         8,2,3,5,-1,35,-152,3,2,3,7,5,1,-9,2,4,-20,
                         8,8,8,7,2,-2,-10,-45,-3,5,1,8,1,2,1,4,
                         -5,0),
      fwd_premium_3m = c(4,4,2,0,6,5,-5,-1,4,5,
                         3,7,3,8,2,5,1,2,0,5,2,8,3,5,3,40,2,1,3,
                         8,3,1,1,11,4,2,9,9,1,2,3,-9,-4,4,3),
      fwd_sharpe_1m = c(7,7,3,1,1,3,1,0,10,4,
                        2,8,5,4,1,1,4,-5,2,6,4,6,5,1,1,5,3,4,9,
                        0,10,1,4,12,1,92,7,1,3,3,0,1,3,1,9)
    )
  #Apply function
  suppressMessages(suppressWarnings({
    split <- time_series_split(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      current_date = as.Date(c("2001-09-15"), format = "%Y-%m-%d"),
      dates_m_vector =  as.Date(c("2001-03-15", "2001-04-15", "2001-05-15","2001-06-15",
                                  "2001-07-15", "2001-08-15", "2001-09-15", "2001-10-15", "2001-11-15")),
      training_sample_size = 7,
      target_fwd = 3,
      target_fwd_name = "fwd_premium_3m",
      split_method = "expanding")
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  results[[2]] <- list()
  names(results) <- c("training", "refit")

  #Fill Training
  results$training <- list()
  results$training[[1]] <- features_m_df[which(features_m_df$dates %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15", "2001-06-15"), format = "%Y-%m-%d")),]
  results$training[[2]] <- target_m_df[which(target_m_df$dates %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15", "2001-06-15"), format = "%Y-%m-%d")),
                                       which(colnames(target_m_df) == "fwd_premium_3m")]

  results$training[[3]] <- cbind(fwd_premium_3m = results$training[[2]], results$training[[1]][,-c(1:3)])
  names(results$training) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")



  #Fill refit
  results$refit <- list()
  results$refit[[1]] <- results$training[[1]]
  results$refit[[2]] <- results$training[[2]]
  results$refit[[3]] <- results$training[[3]]

  names(results$refit) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")


  expect_equal(
    split,
    results
  )

})

#Define your test
test_that("time_series_split works with a 1m target, validation, current_date matches second rebalancing", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                     "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                     "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                     "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                     "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                     "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                     "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                     "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  suppressMessages(suppressWarnings({
    split <- time_series_split(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
      dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                 "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                 "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
      training_sample_size = 4,
      validation_sample_size = 3,
      target_fwd = 1,
      target_fwd_name = "fwd_premium_1m",
      split_method = "expanding")
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  results[[2]] <- list()
  results[[3]] <- list()
  names(results) <- c("training","validation", "refit")

  #Fill Training
  results$training <- list()
  results$training[[1]] <- features_m_df[which(as.Date(features_m_df$dates) %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15",
                                                                                           "2001-06-15", "2001-07-15"), format = "%Y-%m-%d")),]
  results$training[[2]] <- target_m_df[which(as.Date(target_m_df$dates) %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15",
                                                                                       "2001-06-15", "2001-07-15"), format = "%Y-%m-%d")),
                                       which(colnames(target_m_df) == "fwd_premium_1m")]

  results$training[[3]] <- cbind(fwd_premium_1m = results$training[[2]], results$training[[1]][,-c(1:3)])
  names(results$training) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")


  #Fill Validation
  results$validation <- list()
  results$validation[[1]] <- features_m_df[which(as.Date(features_m_df$dates) %in% as.Date(c("2001-08-15","2001-09-15","2001-10-15"), format = "%Y-%m-%d")),]
  results$validation[[2]] <- target_m_df[which(as.Date(target_m_df$dates) %in% as.Date(c("2001-08-15","2001-09-15", "2001-10-15"), format = "%Y-%m-%d")),
                                         which(colnames(target_m_df) == "fwd_premium_1m")]

  names(results$validation) <- c("features_validation_sample", "target_validation_sample")


  #Fill refit
  results$refit <- list()
  results$refit[[1]] <- features_m_df[which(as.Date(features_m_df$dates) %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15",
                                                                                        "2001-06-15", "2001-07-15", "2001-08-15",
                                                                                        "2001-09-15", "2001-10-15"), format = "%Y-%m-%d")),]
  results$refit[[2]] <- target_m_df[which(as.Date(target_m_df$dates) %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15",
                                                                                    "2001-06-15", "2001-07-15", "2001-08-15",
                                                                                    "2001-09-15", "2001-10-15"), format = "%Y-%m-%d")),
                                    which(colnames(target_m_df) == "fwd_premium_1m")]

  results$refit[[3]] <- cbind(fwd_premium_1m = results$refit[[2]], results$refit[[1]][,-c(1:3)])
  names(results$refit) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

  expect_equal(
    split,
    results
  )

})


#Define your test
test_that("time_series_split works with a 3m target, validation, current_date matches second rebalancing", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                     "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                     "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                     "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                     "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                     "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                     "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                     "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  suppressMessages(suppressWarnings({
    split <- time_series_split(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
      dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                 "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                 "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
      training_sample_size = 4,
      validation_sample_size = 3,
      target_fwd = 3,
      target_fwd_name = "fwd_premium_3m",
      split_method = "expanding")
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  results[[2]] <- list()
  results[[3]] <- list()
  names(results) <- c("training","validation", "refit")

  #Fill Training
  results$training <- list()
  results$training[[1]] <- features_m_df[which(as.Date(features_m_df$dates) %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15"), format = "%Y-%m-%d")),]
  results$training[[2]] <- target_m_df[which(as.Date(target_m_df$dates) %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15"), format = "%Y-%m-%d")),
                                       which(colnames(target_m_df) == "fwd_premium_3m")]

  results$training[[3]] <- cbind(fwd_premium_3m = results$training[[2]], results$training[[1]][,-c(1:3)])
  names(results$training) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")


  #Fill Validation
  results$validation <- list()
  results$validation[[1]] <- features_m_df[which(as.Date(features_m_df$dates) %in% as.Date(c("2001-08-15"), format = "%Y-%m-%d")),]
  results$validation[[2]] <- target_m_df[which(as.Date(target_m_df$dates) %in% as.Date(c("2001-08-15"), format = "%Y-%m-%d")),
                                         which(colnames(target_m_df) == "fwd_premium_3m")]

  names(results$validation) <- c("features_validation_sample", "target_validation_sample")


  #Fill refit
  results$refit <- list()
  results$refit[[1]] <- features_m_df[which(as.Date(features_m_df$dates) %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15",
                                                                                        "2001-06-15", "2001-07-15", "2001-08-15"), format = "%Y-%m-%d")),]
  results$refit[[2]] <- target_m_df[which(as.Date(target_m_df$dates) %in% as.Date(c("2001-03-15","2001-04-15","2001-05-15",
                                                                                    "2001-06-15", "2001-07-15", "2001-08-15"), format = "%Y-%m-%d")),
                                    which(colnames(target_m_df) == "fwd_premium_3m")]

  results$refit[[3]] <- cbind(fwd_premium_3m = results$refit[[2]], results$refit[[1]][,-c(1:3)])
  names(results$refit) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

  expect_equal(
    split,
    results
  )

})


#Define your test
test_that("dates in validation sample are higher than dates in training sample", {


            load(paste(test_path(),"/testdata/","toy_fulldates_features_and_targets.RData", sep =""))

            #Sample sizes
            training_sample_size <- 60
            validation_sample_size <- 36
            #fwd
            target_fwd <- 3
            target_fwd_name <- "fwd_premium_3m"
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(lubridate::month(toy_dates_full_dates) == 6 &
                                                              toy_dates_full_dates > first_rebalancing_date)]

            rebalancing_dates <- c(first_rebalancing_date, rebalancing_dates)

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check1 <- list()
            check2 <- list()


            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check1[[l]] <- all(outer(dates_training[[l]], dates_validation[[l]], "<"))
              check2[[l]] <- max(dates_training[[l]]) < min(dates_validation[[l]])
            }

            #Get checks
            check_if_dates_validation_greater_dates_training <- all(unlist(check1), unlist(check2))

            #Test
            expect_true(check_if_dates_validation_greater_dates_training, label = "check if dates in val are greater than dates in training")

            #All periods test
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(toy_dates_full_dates >= first_rebalancing_date)]

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")


            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check1 <- list()
            check2 <- list()


            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check1[[l]] <- all(outer(dates_training[[l]], dates_validation[[l]], "<"))
              check2[[l]] <- max(dates_training[[l]]) < min(dates_validation[[l]])
            }

            #Get checks
            check_if_dates_validation_greater_dates_training <- all(unlist(check1), unlist(check2))

            #Test
            expect_true(check_if_dates_validation_greater_dates_training, label = "check if dates in val are greater than dates in training")




          })

#Define your test
test_that("there is a target_fwd difference between greatest training date and lowest validation date", {


            load(paste(test_path(),"/testdata/","toy_fulldates_features_and_targets.RData", sep =""))

            #Sample sizes
            training_sample_size <- 60
            validation_sample_size <- 36
            #fwd
            target_fwd <- 3
            target_fwd_name <- "fwd_premium_3m"
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(lubridate::month(toy_dates_full_dates) == 6 &
                                                              toy_dates_full_dates > first_rebalancing_date)]

            rebalancing_dates <- c(first_rebalancing_date, rebalancing_dates)

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check <- list()


            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check[[l]] <- as.numeric(difftime(max(dates_training[[l]]), min(dates_validation[[l]]), units = "days"))
            }

            #Get checks
            check_if_dates_validation_greater_dates_training_by_target_fwd <- all(unlist(check) <= -90)


            #Test
            expect_true(check_if_dates_validation_greater_dates_training_by_target_fwd,
                        label = "check if there is target_fwd difference between greatest training date and lowest validation date")


            #Complete dates
            #Sample sizes
            training_sample_size <- 60
            validation_sample_size <- 36
            #fwd
            target_fwd <- 3
            target_fwd_name <- "fwd_premium_3m"
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(toy_dates_full_dates >= first_rebalancing_date)]

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check <- list()


            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check[[l]] <- as.numeric(difftime(max(dates_training[[l]]), min(dates_validation[[l]]), units = "days"))
            }

            #Get checks
            check_if_dates_validation_greater_dates_training_by_target_fwd <- all(unlist(check) <= -89)


            #Test
            expect_true(check_if_dates_validation_greater_dates_training_by_target_fwd,
                        label = "check if there is target_fwd difference between greatest training date and lowest validation date")


          })

#Define your test
test_that("there is no date overlap between training and validation samples", {


            load(paste(test_path(),"/testdata/","toy_fulldates_features_and_targets.RData", sep =""))

            #Sample sizes
            training_sample_size <- 60
            validation_sample_size <- 36
            #fwd
            target_fwd <- 3
            target_fwd_name <- "fwd_premium_3m"
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(lubridate::month(toy_dates_full_dates) == 6 &
                                                              toy_dates_full_dates > first_rebalancing_date)]

            rebalancing_dates <- c(first_rebalancing_date, rebalancing_dates)

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check <- list()

            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check[[l]] <- !any(dates_training[[l]] %in% dates_validation[[l]])
            }

            #Get checks
            check_if_there_is_no_date_overlap <- all(unlist(check))


            #Test
            expect_true(check_if_there_is_no_date_overlap, label = "check if there is no date overlap")

            #2nd test all dates
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(toy_dates_full_dates >= first_rebalancing_date)]


            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check <- list()

            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check[[l]] <- !any(dates_training[[l]] %in% dates_validation[[l]])
            }

            #Get checks
            check_if_there_is_no_date_overlap <- all(unlist(check))


            #Test
            expect_true(check_if_there_is_no_date_overlap, label = "check if there is no date overlap")


          })

#Define your test
test_that("there is a target_fwd difference between greatest validation date and current rebalancing date", {


            load(paste(test_path(),"/testdata/","toy_fulldates_features_and_targets.RData", sep =""))

            #Sample sizes
            training_sample_size <- 60
            validation_sample_size <- 36
            #fwd
            target_fwd <- 3
            target_fwd_name <- "fwd_premium_3m"
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(lubridate::month(toy_dates_full_dates) == 6 &
                                                              toy_dates_full_dates > first_rebalancing_date)]

            rebalancing_dates <- c(first_rebalancing_date, rebalancing_dates)

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check <- list()

            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check[[l]] <- as.numeric(difftime(max(dates_validation[[l]]), rebalancing_dates[l]))
            }

            #Get checks
            check_if_current_date_higher_validation_date_by_target_fwd <- all(unlist(check) <= -90)

            #Test
            expect_true(check_if_current_date_higher_validation_date_by_target_fwd,
                        label = "check if there is target_fwd different between greatest validation date and current_rebalancing_date")

            #2nd rebal
            rebalancing_dates <- toy_dates_full_dates[which(lubridate::month(toy_dates_full_dates) == 6 &
                                                              toy_dates_full_dates > first_rebalancing_date)]

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check <- list()

            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check[[l]] <- as.numeric(difftime(max(dates_validation[[l]]), rebalancing_dates[l]))
            }

            #Get checks
            check_if_current_date_higher_validation_date_by_target_fwd <- all(unlist(check) <= -90)

            #Test
            expect_true(check_if_current_date_higher_validation_date_by_target_fwd,
                        label = "check if there is target_fwd different between greatest validation date and current_rebalancing_date")


          })

#Define your test
test_that("amount of dates in refit is equal to training plus val minus the target_fwd block between both", {


            load(paste(test_path(),"/testdata/","toy_fulldates_features_and_targets.RData", sep =""))

            #Sample sizes
            training_sample_size <- 60
            validation_sample_size <- 36
            #fwd
            target_fwd <- 3
            target_fwd_name <- "fwd_premium_3m"
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(lubridate::month(toy_dates_full_dates) == 6 &
                                                              toy_dates_full_dates > first_rebalancing_date)]

            rebalancing_dates <- c(first_rebalancing_date, rebalancing_dates)

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check <- list()

            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check[[l]] <- length(dates_refit[[l]]) - length(c(dates_training[[l]], dates_validation[[l]]))
            }

            #Get checks
            check_if_amount_dates_refit_equal_val_plus_training_adj_by_target_fwd <- all(unlist(check) == target_fwd - 1)

            #Test
            expect_true(check_if_amount_dates_refit_equal_val_plus_training_adj_by_target_fwd,
                        label = "check if amount of dates in refit is equal to training plus val minus the block between both")

            #2nd test with all dates
            #Rebalancing dates
            first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
            rebalancing_dates <- toy_dates_full_dates[which(toy_dates_full_dates >= first_rebalancing_date)]

            #Store elements
            training_list <- list()
            training_list[[1]] <- list()
            training_list[[2]] <- list()
            training_list[[3]] <- list()
            names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

            validation_list <- list()
            validation_list[[1]] <- list()
            validation_list[[2]] <- list()
            validation_list[[3]] <- list()
            names(validation_list) <- c("features_validation_sample", "target_validation_sample", "full_data_validation_sample_clean")

            refit_list <- list()
            refit_list[[1]] <- list()
            refit_list[[2]] <- list()
            refit_list[[3]] <- list()
            names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

            #Run loop
            for(d in 1:length(rebalancing_dates)){
              #Run for all rebalancing dates
              output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                                          dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                                          target_fwd = target_fwd, target_fwd_name = target_fwd_name)

              #Store
              training_list$features_training_sample[[d]] <- output$training$features_training_sample
              training_list$target_training_sample[[d]] <- output$training$target_training_sample
              training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

              validation_list$features_validation_sample[[d]] <- output$validation$features_validation_sample
              validation_list$target_validation_sample[[d]] <- output$validation$target_validation_sample
              validation_list$full_data_validation_sample_clean[[d]] <- output$validation$full_data_validation_sample_clean

              refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
              refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
              refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

            }
            #Get dates of each partition
            dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_validation <- lapply(validation_list$features_validation_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
            dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

            #Create lists
            check <- list()

            # Check if each element in dates_training is greater than corresponding element in dates_validation
            for(l in 1:length(dates_training)){
              check[[l]] <- length(dates_refit[[l]]) - length(c(dates_training[[l]], dates_validation[[l]]))
            }

            #Get checks
            check_if_amount_dates_refit_equal_val_plus_training_adj_by_target_fwd <- all(unlist(check) == target_fwd - 1)

            #Test
            expect_true(check_if_amount_dates_refit_equal_val_plus_training_adj_by_target_fwd,
                        label = "check if amount of dates in refit is equal to training plus val minus the block between both")





          })

#Define your test
test_that("training and refit samples are the same in case of OLS", {

#third test: try with validation = 0
load(paste(test_path(),"/testdata/","toy_fulldates_features_and_targets.RData", sep =""))

#Sample sizes
training_sample_size <- 60
validation_sample_size <- 0
#fwd
target_fwd <- 3
target_fwd_name <- "fwd_premium_3m"
#Rebalancing dates
first_rebalancing_date <- toy_dates_full_dates[training_sample_size + validation_sample_size]
rebalancing_dates <- toy_dates_full_dates[which(toy_dates_full_dates >= first_rebalancing_date)]


#Store elements
training_list <- list()
training_list[[1]] <- list()
training_list[[2]] <- list()
training_list[[3]] <- list()
names(training_list) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")


refit_list <- list()
refit_list[[1]] <- list()
refit_list[[2]] <- list()
refit_list[[3]] <- list()
names(refit_list) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

#Run loop
for(d in 1:length(rebalancing_dates)){
  #Run for all rebalancing dates
  output <- time_series_split(current_date = rebalancing_dates[d], features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                              dates_m_vector = toy_dates_full_dates, training_sample_size = training_sample_size, validation_sample_size = validation_sample_size,
                              target_fwd = target_fwd, target_fwd_name = target_fwd_name)

  #Store
  training_list$features_training_sample[[d]] <- output$training$features_training_sample
  training_list$target_training_sample[[d]] <- output$training$target_training_sample
  training_list$full_data_training_sample_clean[[d]] <- output$training$full_data_training_sample_clean

  refit_list$features_m_refit[[d]] <- output$refit$features_m_refit
  refit_list$target_m_refit[[d]] <- output$refit$target_m_refit
  refit_list$full_data_m_refit_clean[[d]] <- output$refit$full_data_m_refit_clean

}
#Get dates of each partition
dates_training <- lapply(training_list$features_training_sample, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))
dates_refit <- lapply(refit_list$features_m_refit, function(x) as.Date(unique(x$dates), format = "%Y-%m-%d"))

#Create lists
check <- list()

# Check if each element in dates_training is greater than corresponding element in dates_validation
for(l in 1:length(dates_training)){
  check[[l]] <- dates_refit[[l]] == (dates_training[[l]])
}

#Get checks
check_if_refit_equals_training <- all(unlist(check))

#Test
expect_true(check_if_refit_equals_training)
})

#Define your test
test_that("time_series_split throws an error when features_m_df not in correct format", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         #tickers = c("Stock A", "Stock A", "Stock A",
         #           "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
         #          "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
         #           "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
         #           "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
         #           "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
         #           "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
         #           "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })),
    "features_m_df should have id, tickers and dates columns.")

})

#Define your test
test_that("time_series_split throws an error when an object is not in correct class", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                     "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                     "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                     "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                     "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                     "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                     "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                     "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = "three",
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })),
    "Objects not in correct class.")



  #Apply function
  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "ro")
    })),
    "split_method should be expanding or rolling.")



})



#Define your test
test_that("time_series_split throws an error when current_date not in correct format", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                    "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                    "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                    "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                    "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                    "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                    "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = 3,
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })),
    "current_date must be a single date object with format %Y-%m-%d")

})


#Define your test
test_that("time_series_split throws an error when dates_m_vector not in correct format", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                    "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                   "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                    "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                    "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                   "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                    "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                    "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })),
    "dates_m_vector must be a date object with format %Y-%m-%d")


  #Apply function
  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-11-15","2001-10-15",
                                   "2001-09-15","2001-08-15","2001-07-15","2001-06-15",
                                   "2001-05-15", "2001-04-15", "2001-03-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })),
    "dates_m_vector should be in ascending chronological order")



  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15"
             ),
         tickers = c("Stock A", "Stock B"),
         dates = structure(c(984614400, 987292800),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20),
         Beta = c(4, 7),
         Gamma = c(800, 11)), row.names = c(NA, -45L), class = "data.frame")


  target_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15"
    ),
    tickers = c("Stock A", "Stock B"),
    dates = structure(c(984614400, 987292800),
                      class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    fwd_premium_1m = c(3, -20),
    fwd_premium_3m = c(4, 7),
    fwd_sharpe_1m = c(800, 11)), row.names = c(NA, -45L), class = "data.frame")



  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })),
    "dates_m_vector should have more dates than target_fwd")

})



#Define your test
test_that("time_series_split throws an error when dates_m_vector do not correspond to features_m_df or target_m_df dates", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                     "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                     "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                     "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                     "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                     "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                     "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                     "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-12-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })),
    "all dates in dates_m_vector must have a correspondence in features_m_df")



  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 987292100, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 987292100, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 987292100), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")


  expect_error(
    suppressMessages(suppressWarnings({
      split <- time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 3,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })),
    "all dates in dates_m_vector must have a correspondence in target_m_df")


  })

#Define your test
test_that("time_series_split throws an error then traning_sample_size <= target_fwd", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                     "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                     "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                     "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                     "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                     "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                     "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                     "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  expect_error(
  suppressMessages(suppressWarnings({
    time_series_split(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
      dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                 "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                 "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
      training_sample_size = 2,
      validation_sample_size = 3,
      target_fwd = 3,
      target_fwd_name = "fwd_premium_3m",
      split_method = "expanding")
  })), "training_sample_size should be higher than target_fwd")





})


#Define your test
test_that("time_series_split throws an error then validation_sample_size < target_fwd", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                     "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                     "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                     "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                     "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                     "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                     "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                     "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  expect_error(
    suppressMessages(suppressWarnings({
      time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
        training_sample_size = 4,
        validation_sample_size = 2,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding")
    })))





})


#Define your test
test_that("time_series_split works throws an error then validation_sample_size + training_sample_size > target_fwd", {
  features_m_df = structure(
    list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                "Stock E-2001-11-15"),
         tickers = c("Stock A", "Stock A", "Stock A",
                     "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                     "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                     "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                     "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                     "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                     "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                     "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"),
         dates = structure(c(984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                             1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                             995155200, 997833600, 1000512000, 1003104000, 1005782400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, -20, -450, 5, -2, 1,
                   6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                   8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                   -1, 4, 4),
         Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                  -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                  2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
         Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                   -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                   3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame")

  target_m_df =
    structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                          "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                          "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                          "Stock A-2001-11-15", "Stock B-2001-03-15", "Stock B-2001-04-15",
                          "Stock B-2001-05-15", "Stock B-2001-06-15", "Stock B-2001-07-15",
                          "Stock B-2001-08-15", "Stock B-2001-09-15", "Stock B-2001-10-15",
                          "Stock B-2001-11-15", "Stock C-2001-03-15", "Stock C-2001-04-15",
                          "Stock C-2001-05-15", "Stock C-2001-06-15", "Stock C-2001-07-15",
                          "Stock C-2001-08-15", "Stock C-2001-09-15", "Stock C-2001-10-15",
                          "Stock C-2001-11-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                          "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                          "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                          "Stock D-2001-11-15", "Stock E-2001-03-15", "Stock E-2001-04-15",
                          "Stock E-2001-05-15", "Stock E-2001-06-15", "Stock E-2001-07-15",
                          "Stock E-2001-08-15", "Stock E-2001-09-15", "Stock E-2001-10-15",
                          "Stock E-2001-11-15"),
                   tickers = c("Stock A", "Stock A", "Stock A",
                               "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                               "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                               "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                               "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                               "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                               "Stock D", "Stock D", "Stock D", "Stock E", "Stock E", "Stock E",
                               "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E"
                   ),
                   dates = structure(c(984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                       987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                       1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                       995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                   fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                      2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                   fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                      3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                   fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                     10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame")
  #Apply function
  suppressMessages(suppressWarnings({expect_error(

      time_series_split(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        current_date = as.Date(c("2001-11-15"), format = "%Y-%m-%d"),
        dates_m_vector = as.Date(c("2001-03-15","2001-04-15",
                                   "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                                   "2001-09-15", "2001-10-15", "2001-11-15"), format = "%Y-%m-%d"),
        training_sample_size = 6,
        validation_sample_size = 6,
        target_fwd = 3,
        target_fwd_name = "fwd_premium_3m",
        split_method = "expanding"))}))


})

